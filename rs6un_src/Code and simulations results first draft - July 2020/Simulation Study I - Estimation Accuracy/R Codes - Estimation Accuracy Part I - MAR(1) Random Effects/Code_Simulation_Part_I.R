### clear workspace

ls()

rm(list=ls())

library(lme4)
library(optimx)
library(glmnet)
library(data.table)
library(tidyr)
library(dplyr)
library(lmerTest)
library(tidyverse)
library(rlist)
library(ggplot2)
library(gridExtra)
library(portes)
library(MASS)
library(propagate)
library(kableExtra)
library(parallel)
library(future.apply)
library(reshape2) 
library(ggplot2)

######################################################################################
######################################################################################
######################################################################################

# Upload the functions
source(file="data.prep.R")
source(file="Data.AR.R")
source(file="MVAR.Model.R")
source(file="MSE.MVAR.fit.R")
source(file="MSE.MAR.fit.R")
source(file="MSE.VAR.fit.R")
source(file="MSE.AR.fit.R")
source(file="MSE.Sys.fit.R")

######################################################################################
######################################################################################
######################################################################################

# Set the values to simulate data assuming a VAR(1) process
set.seed(123) # Set random seed
N = 60 # Number of participants
T = c(50,100,200) # Number of time points
P = c(2,4,6,8) # Number of variables in VAR(1) models
var.Psi = 0.10 # Set the variance of the random effects of the regression weights (i.e., all variances are assumed to be equal)
b.ar.min = 0.2 
b.ar.max = 0.6

# Number of replicates
R = 10

######################################################################################
######################################################################################
######################################################################################

# Simulate dataset

for (n in 1:length(N)){
for (t in 1:length(T)){
for (p in 1:length(P)){
for (r in 1:R){

# Simulate Psi

Psi.list = runif(P[p],b.ar.min,b.ar.max)
# Save Psi matrix
save(Psi.list, file = paste("Psi_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))

# Monte Carlo replicates

Sim.Var.Data = Data.AR(N[n],T[t],Psi.list,var.Psi)

# Save datasets
save(Sim.Var.Data, file = paste("Data_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))
}}}}

######################################################################################
######################################################################################
######################################################################################

# Estimate models

for (n in 1:length(N)){
for (t in 1:length(T)){
for (p in 1:length(P)){
for (r in 1:R){

# Load training and testing set
load(file = paste("Data_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))

data = Sim.Var.Data$data
Psi.true.i = lapply(1:N[n], function(i) diag(Sim.Var.Data[[3]][[i]]))

# Estimate multilevel VAR(1) Model

MSE.VAR.Sys = MSE.Sys.fit(data,P[p],Psi.true.i)

save(MSE.VAR.Sys, file = paste("MSE_MVAR_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))

# Estimate person-specific VAR(1) Model

MSE.VAR.PS.Sys = MSE.VAR.fit(data,P[p],Psi.true.i)

save(MSE.VAR.PS.Sys, file = paste("MSE_VAR_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))

# Estimate person-specific AR(1) Model

MSE.AR.PS.Sys = MSE.AR.fit(data,P[p],Psi.true.i)

save(MSE.AR.PS.Sys, file = paste("MSE_AR_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))
}}}}

######################################################################################
######################################################################################
######################################################################################
