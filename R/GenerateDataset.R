# CREATED   3 Sep 2014
# MODIFIED 21 Feb 2020

# AUTHOR marco.kienzle@gmail.com;

# This is a wrapper function


GenerateData <-  function(max.age = 10, nb.of.cohort = 20, catchability.range = c(3,10), effort.range = c(3e3, 5e3), nat.mort.range = c(0.1, 0.8), recruitment.range=c(1e6, 1e7), log.para.range = c(8,12), log.parb.range = c(1,3), verbose = FALSE){

## Currently the best function to generate synthetic data is
GenerateData3(max.age = max.age,
              nb.of.cohort = nb.of.cohort,
	      catchability.range = catchability.range,
	      effort.range = effort.range,
	      nat.mort.range = nat.mort.range,
	      recruitment.range = recruitment.range,
	      log.para.range = log.para.range,
	      log.parb.range = log.parb.range)

}

############################################################################################################################################
# Generate data 3
############################################################################################################################################

GenerateData3 <-  function(max.age = 10, nb.of.cohort = 20, catchability.range = c(3,10), effort.range = c(3e3, 5e3), nat.mort.range = c(0.1, 0.8), recruitment.range=c(1e6, 1e7), log.para.range = c(8,12), log.parb.range = c(1,3), verbose = FALSE){


# Some parameters
#max.age <- 10; nb.of.cohort <- 20 # be aware that nb.of.cohort / max.age >= 3
                               # for some algorithm below to work
#age <- matrix(seq(0, max.age), nrow = nb.of.cohort, ncol = max.age+1, byrow=T)

birth.year.oldest.cohort = 1970

# Fishing mortality as the product of catchability and yearly effort
catchability <- runif(1, min = catchability.range[1], max = catchability.range[2])
#catchability <- 1
catchability.mf <- 1e-4

if(verbose) print(paste("Simulated catchability is ", round(catchability,2), "x 10^-4"))

yearly.effort <- runif(nb.of.cohort, min = effort.range[1], max = effort.range[2])
#yearly.effort <- rep(1e3, nb.of.cohort)
effort.faced.by.each.cohort <- matrix(nrow = nb.of.cohort, ncol = max.age)
for(i in 1:nb.of.cohort) effort.faced.by.each.cohort[i,] <- yearly.effort[seq(i, i + max.age - 1)]

n <- max.age-1
#selectivity.at.age <- c(0,0,seq(1/(max.age - 3), (max.age - 4)/(max.age - 3), length = max.age - 4), 1,1)
#selectivity.at.age <- c(0,0,seq(1/(max.age - 5), (max.age - 6)/(max.age - 5), length = max.age - 8), rep(1,6))
log.para <- runif(1, min = log.para.range[1], max=log.para.range[2])
if(verbose) print(paste("Logistic parameter a is", log.para))
log.parb <- runif(1, min = log.parb.range[1], max=log.parb.range[2])
if(verbose) print(paste("Logistic parameter b is", log.parb))

selectivity.at.age <- logistic(log.para,log.parb,seq(1,max.age))

F <- catchability * catchability.mf * effort.faced.by.each.cohort * outer(rep(1, nb.of.cohort), selectivity.at.age)

M <- runif(1, min = nat.mort.range[1], max = nat.mort.range[2])
#M <- 0.2
if(verbose) print(paste("Simulated natural mortality is", round(M,3)))

# Recruitment: N(0)
Recruitment <- runif(nb.of.cohort, min = recruitment.range[1], max = recruitment.range[2])

# Nb at age in cohorts
cohort <- matrix(nrow = nb.of.cohort, ncol = max.age + 1)
cohort[,1] <- Recruitment
for(i in 1:nrow(cohort)) cohort[i,] <- cohort[i,1] * exp(-c(0, cumsum(M + F[i,])))

# Number that died
cohort.nb.dead <- cohort[,1:max.age] - cohort[,seq(2, max.age+1)]

# Catch
cohort.nb.caught <- F / (M+F) * cohort.nb.dead

# Average number alive in the interval
cohort.avg.alive <- cohort[,1:max.age] * (1 - exp(-M-F))/(M+F)
#print(cohort.avg.alive)


# Number alive at the end of the interval
cohort.alive.endInterval <- cohort[,1:max.age] - cohort.nb.dead

#
#nb.at.age <- cbind(age, Recruitment * exp(-c(0, cumsum(M + F))))

# suppose there are 1000 recruits, 50% survive per year
#vec.survival = rep(0.5, nb.of.cohort) #

# suppose there are 1000 recruits, survival increase with cohort number
#vec.survival = seq(0.2, 0.8, length = nb.of.cohort)

# suppose there are 1000 recruits, survival is random
# vec.survival <- runif(nb.of.cohort, min = 0.1, max = 0.9)

### Calculate the number alive at age
# using constant recruitment
#cohort <- data.frame(1000 * vec.survival ^ age)
# using variable recruitment
#cohort <- runif(nb.of.cohort, min = 1e2, max = 1e4) * vec.survival ^ age

# Name nicely you data with arbitrary cohort birth-date
dimnames(cohort)[[1]] = seq( from  = birth.year.oldest.cohort, by = 1, length = nb.of.cohort)
dimnames(cohort)[[2]] = paste("age-group", seq(0,max.age), sep="")  # give age names

# Re-format data into catch at age matrix
catch.at.age <- matrix(ncol = max.age, nrow = nb.of.cohort)
effort.at.age <- matrix(ncol = max.age, nrow = nb.of.cohort)
avg.alive.at.age <- matrix(ncol = max.age, nrow = nb.of.cohort)
alive.endInterval <- matrix(ncol = max.age, nrow = nb.of.cohort)
F.at.age <- matrix(ncol = max.age, nrow = nb.of.cohort)

for(i in seq(1,nb.of.cohort - 1)) {

  diag(catch.at.age[seq(i, nb.of.cohort),seq(1, min(nb.of.cohort - i +1, max.age))]) <-
    as.numeric(cohort.nb.caught[i,seq(1, min(nb.of.cohort - i +1, max.age))])

  diag(effort.at.age[seq(i, nb.of.cohort),seq(1, min(nb.of.cohort - i +1, max.age))]) <-
    as.numeric(effort.faced.by.each.cohort[i,seq(1, min(nb.of.cohort - i +1, max.age))])

  diag(avg.alive.at.age[seq(i, nb.of.cohort),seq(1, min(nb.of.cohort - i +1, max.age))]) <-
    as.numeric(cohort.avg.alive[i,seq(1, min(nb.of.cohort - i +1, max.age))])

diag(alive.endInterval[seq(i, nb.of.cohort),seq(1, min(nb.of.cohort - i +1, max.age))]) <-
    as.numeric(cohort.alive.endInterval[i,seq(1, min(nb.of.cohort - i +1, max.age))])


diag(F.at.age[seq(i, nb.of.cohort),seq(1, min(nb.of.cohort - i +1, max.age))]) <-
    as.numeric(F[i,seq(1, min(nb.of.cohort - i +1, max.age))])

}

catch.at.age[nb.of.cohort,1] = cohort.nb.caught[nb.of.cohort,1] # diag deals on with matrix
effort.at.age[nb.of.cohort,1] = effort.faced.by.each.cohort[nb.of.cohort,1] # diag deals on with matrix
avg.alive.at.age[nb.of.cohort,1] = cohort.avg.alive[nb.of.cohort,1] # diag deals on with matrix
alive.endInterval[nb.of.cohort,1] = cohort.alive.endInterval[nb.of.cohort,1] # diag deals on with matrix
F.at.age[nb.of.cohort,1] = F[nb.of.cohort,1] # diag deals on with matrix

# Get a complete dataset
c.catch.at.age = catch.at.age[complete.cases(catch.at.age),]
effort.at.age = effort.at.age[complete.cases(effort.at.age),]
c.avg.alive.at.age = avg.alive.at.age[complete.cases(catch.at.age),] # WARNING: here I take the complete cases of catch to get
                                                                     # c.avg.alive.at.age of the same dimension as c.catch.at.age
                                                                     # because c.avg.alive.at.age = avg.alive.at.age[complete.cases(c.avg.alive.at.age),] did not provide the same dimensions
c.alive.endInterval = alive.endInterval[complete.cases(alive.endInterval),]
F.at.age = F.at.age[complete.cases(F.at.age),]

# Give dimensions names
dimnames(c.avg.alive.at.age)[[1]] = seq(birth.year.oldest.cohort + max.age - 1, birth.year.oldest.cohort + max.age - 1 + nb.of.cohort - max.age )
dimnames(c.avg.alive.at.age)[[2]] = paste(seq(0, max.age-1), seq(1, max.age), sep="-")

dimnames(c.alive.endInterval)[[1]] = seq(birth.year.oldest.cohort + max.age - 1, birth.year.oldest.cohort + max.age - 1 + nb.of.cohort - max.age )
dimnames(c.alive.endInterval)[[2]] = paste(seq(0, max.age-1), seq(1, max.age), sep="-")

dimnames(c.catch.at.age)[[1]] = seq(birth.year.oldest.cohort + max.age - 1, birth.year.oldest.cohort + max.age - 1 + nb.of.cohort - max.age )
dimnames(c.catch.at.age)[[2]] = paste(seq(0, max.age-1), seq(1, max.age), sep="-")

dimnames(effort.at.age)[[1]] = seq(birth.year.oldest.cohort + max.age - 1, birth.year.oldest.cohort + max.age - 1 + nb.of.cohort - max.age )
dimnames(effort.at.age)[[2]] = paste(seq(0, max.age-1), seq(1, max.age), sep="-")

dimnames(F.at.age)[[1]] = seq(birth.year.oldest.cohort + max.age - 1, birth.year.oldest.cohort + max.age - 1 + nb.of.cohort - max.age )
dimnames(F.at.age)[[2]] = paste(seq(0, max.age-1), seq(1, max.age), sep="-")

return(list(abundance = c.avg.alive.at.age, abundance.EndInterval = c.alive.endInterval, catch = c.catch.at.age, effort = effort.at.age, catchability = catchability * catchability.mf, Nat.Mort = M, logistic.par = c(log.para, log.parb), Recruitment = Recruitment, M = M, F = F.at.age))
}

