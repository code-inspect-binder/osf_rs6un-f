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
library(psych)
library(pracma)

######################################################################################
######################################################################################
######################################################################################

# Upload the functions
source(file="data.block.R")
source(file="data.block.PC.R")
source(file="data.prep.R")
source(file="data.mean.R")
source(file="Data.PC.Var.Fixed.R")
source(file="MVAR.Model.R")
source(file="Psi.Matrix.R")
source(file="MSE.MVAR.Model.R")
source(file="MSE.MAR.Model.R")
source(file="MSE.MVAR.fold.R")
source(file="MSE.MAR.fold.R")
source(file="MSE.MVAR.Sys.R")
source(file="MSE.MAR.Sys.R")
source(file="MSE.Sys.R")
source(file="MSE.VAR.fold.R")
source(file="MSE.PC.VAR.fold.R")
source(file="MSE.PC.MVAR.fold.R")
source(file="MSE.PC.MAR.fold.R")
source(file="MSE.PC.Sys.R") 
source(file="MSE.VAR.Sys.R") 
source(file="MSE.AR.fold.R")
source(file="MSE.AR.Sys.R") 

######################################################################################
######################################################################################
######################################################################################

# Set the values to simulate data assuming a VAR(1) process
set.seed(123) # Set random seed
N = c(20,60) # Number of participants
T = c(50,100) # Number of time points
P = 6 # Number of variables 
Q = c(1,2,3) # Number of components in VAR(1) models
cor.Sigma = 0.2 # Set the covariance of the within-individuals errors (i.e., all covariances are assumed to be equal)
var.E = c(0.05,0.5) # variance of the error to rescale the expected noise variance of the observed variables
b.ar.min = 0.2 
b.ar.max = 0.6
b.cr.min = 0.05
b.cr.max = 0.2

# Set the number of replicates
R = 10 # Number of replicates

# Set the number of blocks in block cross-validation
fold = 10

######################################################################################
######################################################################################
######################################################################################

# Simulate dataset

for (n in 1:length(N)){
for (q in 1:length(Q)){
for (t in 1:length(T)){

for (r in 1:R){
if (Q[q]==1){
Psi.list = as.matrix(runif(Q[q],b.ar.min,b.ar.max))
}
if (Q[q]>1){
Psi.list = Psi.Matrix(Q[q],b.ar.min,b.ar.max,b.cr.min,b.cr.max)
}
# Save Psi matrix
save(Psi.list, file = paste("Psi_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_R_",r,".RData",sep = ""))

for (e in 1:length(var.E)){
# Monte Carlo replicates
Sim.Var.Data = Data.PC.VAR.Fixed(N[n],T[t],P,Psi.list,cor.Sigma,var.E[e])
 
# Save datasets
save(Sim.Var.Data, file = paste("Data_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_R_",r,".RData",sep = ""))
}}}}}

######################################################################################
######################################################################################
######################################################################################

# Generate training and testing set

for (n in 1:length(N)){
for (q in 1:length(Q)){
for (t in 1:length(T)){
for (e in 1:length(var.E)){
for (r in 1:R){

# Generate training and testing set
load(file = paste("Data_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_R_",r,".RData",sep = ""))

data.list = data.block(Sim.Var.Data$data,fold)
save(data.list, file = paste("Data_Block_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_R_",r,".RData",sep = ""))

for (p in 1:P){
data.list.PC = data.block.PC(data.list,nComp=p,fold)

# Save datasets
save(data.list.PC, file = paste("Data_Block_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",p,"_R_",r,".RData",sep = ""))
}}}}}}

######################################################################################
######################################################################################
######################################################################################

# Perform K-block cross-validation using Multilevel VAR and VAR on the original data

for (n in 1:length(N)){
for (q in 1:length(Q)){
for (t in 1:length(T)){
for (e in 1:length(var.E)){
for (r in 1:R){

p = 1
# Load training and testing set
load(file = paste("Data_Block_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",p,"_R_",r,".RData",sep = ""))

MSE.MVAR.Sys.PC = MSE.Sys(data.list.PC,P)
MSE.VAR.Sys.PC = MSE.VAR.Sys(data.list.PC,P)
MSE.AR.Sys.PC = MSE.AR.Sys(data.list.PC,P)

save(MSE.MVAR.Sys.PC, file = paste("MSE_MVAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_R_",r,".RData",sep = ""))
save(MSE.VAR.Sys.PC, file = paste("MSE_VAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_R_",r,".RData",sep = ""))
save(MSE.AR.Sys.PC, file = paste("MSE_AR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_R_",r,".RData",sep = ""))
}}}}}

######################################################################################
######################################################################################
######################################################################################

# Perform K-block cross-validation using PC-VAR

for (n in 1:length(N)){
for (q in 1:length(Q)){
for (t in 1:length(T)){
for (e in 1:length(var.E)){
for (r in 1:R){
for (p in 1:P){


# Load training and testing set
load(file = paste("Data_Block_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",p,"_R_",r,".RData",sep = ""))

MSE.PC.VAR.Sys = MSE.PC.Sys(data.list.PC,nComp=p,Method.type=1)
MSE.PC.MAR.Sys = MSE.PC.Sys(data.list.PC,nComp=p,Method.type=2)
MSE.PC.MAR.RE.Sys = MSE.PC.Sys(data.list.PC,nComp=p,Method.type=3)

save(MSE.PC.VAR.Sys, file = paste("MSE_PC_VAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",p,"_R_",r,".RData",sep = ""))
save(MSE.PC.MAR.Sys, file = paste("MSE_PC_MAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",p,"_R_",r,".RData",sep = ""))
save(MSE.PC.MAR.RE.Sys, file = paste("MSE_PC_MAR_RE_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",p,"_R_",r,".RData",sep = ""))

if (p>1){
MSE.PC.MVAR.Sys = MSE.PC.Sys(data.list.PC,nComp=p,Method.type=4)
MSE.PC.MVAR.RE.Sys = MSE.PC.Sys(data.list.PC,nComp=p,Method.type=5)
save(MSE.PC.MVAR.Sys, file = paste("MSE_PC_MVAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",p,"_R_",r,".RData",sep = ""))
save(MSE.PC.MVAR.RE.Sys, file = paste("MSE_PC_MVAR_RE_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",p,"_R_",r,".RData",sep = ""))
}}}}}}}

######################################################################################
######################################################################################
######################################################################################





