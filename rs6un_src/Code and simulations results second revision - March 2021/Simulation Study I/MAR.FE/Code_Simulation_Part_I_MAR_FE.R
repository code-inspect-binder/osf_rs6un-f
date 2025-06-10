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
library(tsDyn)
library(MASS)
library(propagate)
library(kableExtra)
library(JuliaCall)
library(future.apply)

######################################################################################
######################################################################################
######################################################################################

# Upload the functions
source(file="data.block.R")
source(file="data.prep.R")
source(file="Data.AR.FE.R")
source(file="MVAR.Model.R")
source(file="MSE.MVAR.Model.R")
source(file="MSE.MAR.Model.R")
source(file="MSE.MVAR.fold.R")
source(file="MSE.MAR.fold.R")
source(file="MSE.MVAR.Sys.R")
source(file="MSE.MAR.Sys.R")
source(file="MSE.Sys.R")
source(file="MSE.VAR.fold.R")
source(file="MSE.VAR.Sys.R")
source(file="MSE.AR.fold.R")
source(file="MSE.AR.Sys.R")
source(file="Psi.AR.Matrix.R")

source(file="MVAR.Model.fit.R")
source(file="MVAR.Model.fit.R")
source(file="MSE.MVAR.Sys.fit.R")
source(file="MSE.Sys.fit.R")
source(file="MSE.VAR.Sys.fit.R")
source(file="MSE.AR.Sys.fit.R")

######################################################################################
######################################################################################
######################################################################################

# Set the values to simulate data assuming a VAR(1) process
set.seed(123) # Set random seed
N = c(20,60,120) # Number of participants
T = c(50,100,200) # Number of time points
P = c(2,4,8) # Number of variables in VAR(1) models
cor.Sigma = 0.2 # Set the covariance of the within-individuals errors (i.e., all covariances are assumed to be equal)
b.ar.min = 0.3 
b.ar.max = 0.4

# Set the number of replicates
R = 100 # Number of replicates

# Set the number of blocks in block cross-validation
fold = 10

######################################################################################
######################################################################################
######################################################################################

# Simulate transition matrix

#for (p in 1:length(P)){
#Psi.list = Psi.AR.Matrix(P[p],b.ar.min,b.ar.max)
## Save Psi matrix
#save(Psi.list, file = paste("Psi_P_",P[p],".RData",sep = ""))
#}

######################################################################################
######################################################################################
######################################################################################

# Simulate dataset

#for (n in 1:length(N)){
#for (t in 1:length(T)){
#for (p in 1:length(P)){
#for (r in 1:R){

## Load Psi matrix
#load(file = paste("Psi_P_",P[p],".RData",sep = ""))

## Monte Carlo replicates

#Sim.Var.Data = Data.AR.FE(N[n],T[t],Psi.list,cor.Sigma)

## Save datasets
#save(Sim.Var.Data, file = paste("Data_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))
#}}}}

######################################################################################
######################################################################################
######################################################################################

# Generate training and testing set

#for (n in 1:length(N)){
#for (t in 1:length(T)){
#for (p in 1:length(P)){
#for (r in 1:R){

# Generate training and testing set
#load(file = paste("Data_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))

#data.list = data.block(Sim.Var.Data$data,fold)

# Save datasets
#save(data.list, file = paste("Data_Block_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))
#}}}}

######################################################################################
######################################################################################
######################################################################################

# Perform K-block cross-validation

# Estimate Multilevel VAR(1) Model

plan(multisession)
julia.dir = julia_setup(JULIA_HOME = "C:\\Users\\u0119584\\AppData\\Local\\Programs\\Julia 1.5.3\\bin")
julia.dir$library("MixedModels")

list.sim = expand.grid(Rr=1:R,Tt=T,Nn=N,Pp=P)

for (k in 1:nrow(list.sim)){
n = list.sim$Nn[k]
t = list.sim$Tt[k]
p = list.sim$Pp[k]
r = list.sim$Rr[k]

# Load training and testing set
load(file = paste("Data_Block_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))

MSE.MVAR.PS.Sys = MSE.Sys(data.list,p)
MSE.VAR.PS.Sys = MSE.VAR.Sys(data.list,p)
MSE.AR.PS.Sys = MSE.AR.Sys(data.list,p) 

save(MSE.MVAR.PS.Sys, file = paste("MSE_MVAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
save(MSE.VAR.PS.Sys, file = paste("MSE_VAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
save(MSE.AR.PS.Sys, file = paste("MSE_AR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
}

######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################

# Estimation accuracy
# Estimate Multilevel VAR(1) Model

julia.dir = julia_setup(JULIA_HOME = "C:\\Users\\u0119584\\AppData\\Local\\Programs\\Julia 1.5.3\\bin")
julia.dir$library("MixedModels")
julia.dir$library("JellyMe4")

list.sim = expand.grid(Rr=1:R,Tt=T,Nn=N,Pp=P)

for (k in 1:nrow(list.sim)){
n = list.sim$Nn[k]
t = list.sim$Tt[k]
p = list.sim$Pp[k]
r = list.sim$Rr[k]

# Load data set
load(file = paste("Data_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))

data = data.prep(Sim.Var.Data$data)
MSE.Fit.MVAR.PS.Sys = MSE.Sys.fit(data,p)
MSE.Fit.VAR.PS.Sys = MSE.VAR.Sys.fit(data,p)  
MSE.Fit.AR.PS.Sys = MSE.AR.Sys.fit(data,p)

save(MSE.Fit.MVAR.PS.Sys, file = paste("MSE_Fit_MVAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
save(MSE.Fit.VAR.PS.Sys, file = paste("MSE_Fit_VAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
save(MSE.Fit.AR.PS.Sys, file = paste("MSE_Fit_AR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
}

######################################################################################
######################################################################################
######################################################################################

