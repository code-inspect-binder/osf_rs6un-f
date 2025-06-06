#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

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

######################################################################################
######################################################################################
######################################################################################

# Upload the functions
source(file="data.block.R")
source(file="data.prep.R")
source(file="Data.Ar.R")
source(file="MVAR.Model.R")
source(file="MSE.MVAR.Model.R")
source(file="MSE.MAR.Model.R")
source(file="MSE.MVAR.fold.R")
source(file="MSE.MAR.fold.R")
source(file="MSE.MVAR.Sys.R")
source(file="MSE.MAR.Sys.R")
source(file="MSE.VAR.fold.R")
source(file="MSE.VAR.Sys.R")
source(file="MSE.AR.fold.R")
source(file="MSE.AR.Sys.R")
source(file="MSE.Sys.R")

######################################################################################
######################################################################################
######################################################################################

# Set the values to simulate data assuming a VAR(1) process
set.seed(123) # Set random seed
N = c(20,60,120) # Number of participants
T = c(50,100,200) # Number of time points
P = c(2,4,6,8) # Number of variables in VAR(1) models
var.Psi = 0.10 # Set the variance of the random effects of the regression weights (i.e., all variances are assumed to be equal)
b.ar.min = 0.2 
b.ar.max = 0.6

# Set the number of replicates
R = 10 # Number of replicates

# Set the number of blocks in block cross-validation
fold = 10

######################################################################################
######################################################################################
######################################################################################

# Simulate dataset

for (n in 1:length(N)){
for (t in 1:length(T)){
for (p in 1:length(P)){
for (r in 1:R){

Psi.list = runif(P[p],b.ar.min,b.ar.max)
# Save Psi matrix
save(Psi.list, file = paste("Psi_MAR_RE_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))

# Monte Carlo replicates

Sim.Var.Data = Data.AR(N[n],T[t],Psi.list,var.Psi)

# Save datasets
save(Sim.Var.Data, file = paste("Data_MAR_RE_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))
}}}}

######################################################################################
######################################################################################
######################################################################################

# Generate training and testing set

for (n in 1:length(N)){
for (t in 1:length(T)){
for (p in 1:length(P)){
for (r in 1:R){

# Generate training and testing set
load(file = paste("Data_MAR_RE_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))

data.list = data.block(Sim.Var.Data$data,fold)

# Save datasets
save(data.list, file = paste("Data_Block_MAR_RE_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))
}}}}

######################################################################################
######################################################################################
######################################################################################

# Perform K-block cross-validation

for (n in 1:length(N)){
for (t in 1:length(T)){
for (p in 1:length(P)){
for (r in 1:R){

# Load training and testing set
load(file = paste("Data_Block_MAR_RE_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))

# Estimate multilevel VAR(1) Model

MSE.MVAR.PS.Sys = MSE.Sys(data.list,P[p])

save(MSE.MVAR.PS.Sys, file = paste("MSE_MVAR_MAR_RE_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))

# Estimate person-specific VAR(1) Model

MSE.VAR.PS.Sys = MSE.VAR.Sys(data.list,P[p])

save(MSE.VAR.PS.Sys, file = paste("MSE_VAR_MAR_RE_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))

# Estimate person-specific AR(1) Model

MSE.AR.PS.Sys = MSE.AR.Sys(data.list,P[p])

save(MSE.AR.PS.Sys, file = paste("MSE_AR_MAR_RE_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))
}}}}

######################################################################################
######################################################################################
######################################################################################







