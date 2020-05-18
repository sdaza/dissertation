
library(survey)
library(data.table)
library(texreg)
library(mitools)
library(MASS)
library(mice)
library(miceadds)
library(ggplot2)
source("src/utils.R")

# read imputed data
imp = readRDS('output/data/nlsy97_z_relative_mob_imputation.rds')

simp = extractImputations(imp)
densityplot(simp, ~ bmi)