# R Code
# 14/04/2015
# A. Rivolli, A. C.P.L.F. Carvalho, 2015
# Configuration file

# Packages
library(mldr);
library(pROC);
library(ROCR);
library(kernlab);
library(FNN);
library(MASS);
library(randomForest);
library(parallel);

# directory
DIR = getwd();

#methods
METHODS = c('datasetextractor');

#datasets
FILES = list.files(
  paste(DIR, "/dataset", sep=""), pattern="*-train.arff", recursive=TRUE, full.names=TRUE
);

#number of cores
CORES = 15;

#number of folds
FOLDS = 10;

# Local to save the results
OUTPUT = paste(DIR, '/results/', sep='');
