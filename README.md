# Fishery simulator
The purpose of this repository is to create code to simulate fishery data. The fishery simulator is an essential component of a wider project to enhance understanding of stock assessment method: our work follows the principle that if a (stock assessment) method does not provide the correct answer using synthetic data, it is unlikely to work with real data. The fishery simulator will provide the synthetic datasets to test stock assessment methods. The fishery simulator belongs to the deductive area of the scientific process (represented below) while the stock assessment is an inductive method.

![alt_text](https://github.com/mkienzle/FisherySimulator/blob/master/Figures/Induction_deduction_diagram.png)

The first objective of this collaboration is to create synthetic datasets for a single species, single area, single fleet fishery.

A description of the models used in the fishery simulator is provided [here][https://www.overleaf.com/7424895544mdzrytychpvy].

## Usage

The fishery simulator comes as an R package. You can install it using the following command in R
install_github("mkienzle/FisherySimulator")