# CREATED   3 Sep 2014
# MODIFIED 21 Feb 2020

# AUTHOR marco.kienzle@gmail.com;

# logistic function
logistic <- function(a,b,x){
1 / (1  + exp( a - b * x));
}
