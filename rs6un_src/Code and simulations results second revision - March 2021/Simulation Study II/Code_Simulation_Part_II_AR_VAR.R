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
library(psych)
library(pracma)

######################################################################################
######################################################################################
######################################################################################

# Upload the functions
source(file="data.block.R")
source(file="data.block.PC.R")
source(file="data.block.Raw.R")
source(file="data.prep.R")
source(file="data.mean.R")
source(file="Data.PC.Var.Fixed.R")
source(file="Data.PC.Var.R")
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
source(file="MSE.VAR.Sys.R")
source(file="MSE.AR.fold.R")
source(file="MSE.AR.Sys.R")
source(file="MSE.PC.VAR.fold.R")
source(file="MSE.PC.MVAR.fold.R")
source(file="MSE.PC.MAR.fold.R")
source(file="MSE.PC.Sys.R") 

######################################################################################
######################################################################################
######################################################################################

# Set the values to simulate data assuming a VAR(1) process
set.seed(123) # Set random seed
Eff = c(1,2) # Effects fixed (1) or random (2)
N = 60 # Number of participants
T = c(50,100) # Number of time points
P = 6 # Number of variables 
Q = c(1,2,3) # Number of components in VAR(1) models
var.Psi = 0.025 # Set the variance of the random effects of the regression weights (i.e., all variances are assumed to be equal)var.E = c(0.05,0.5) # variance of the error to rescale the expected noise variance of the observed variables
cor.Sigma = 0.2 # Set the covariance of the within-individuals errors (i.e., all covariances are assumed to be equal)
var.E = c(0.05,0.5) # variance of the error to rescale the expected noise variance of the observed variables
b.ar.min = 0.2 
b.ar.max = 0.6
b.cr.min = 0.05
b.cr.max = 0.2

# Set the number of replicates
R = 100 # Number of replicates

# Set the number of blocks in block cross-validation
fold = 10

######################################################################################
######################################################################################
######################################################################################

######################################################################################
######################################################################################
######################################################################################

# Perform K-block cross-validation using Multilevel VAR and VAR on the original data
# Perform K-block cross-validation using PC-VAR

plan(multisession)
julia.dir = julia_setup(JULIA_HOME = "C:\\Users\\u0119584\\AppData\\Local\\Programs\\Julia 1.5.3\\bin")
julia.dir$library("MixedModels")

list.sim = expand.grid(Rr=1:R,Eff=c(1,2),Ee=c(1,2),Qq=Q,Tt=T,Nn=N)

for (k in 1:nrow(list.sim)){
n = list.sim$Nn[k]
t = list.sim$Tt[k]
r = list.sim$Rr[k]
e = list.sim$Ee[k]
q = list.sim$Qq[k]
f = list.sim$Eff[k]

# Load training and testing set
load(file = paste("Data_Block_Raw_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_nComp_",q,"_R_",r,".RData",sep = ""))
load(file = paste("Data_Block_PC_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_nComp_",q,"_R_",r,".RData",sep = ""))

MSE.MVAR.Sys.PC = MSE.Sys(data.list.Raw,P)
MSE.VAR.Sys.PC = MSE.VAR.Sys(data.list.Raw,P)
MSE.AR.Sys.PC = MSE.AR.Sys(data.list.Raw,P)

MSE.PC.VAR.Sys = MSE.PC.Sys(data.list.PC,nComp=q,Method.type=1)
MSE.PC.MAR.Sys = MSE.PC.Sys(data.list.PC,nComp=q,Method.type=2)
MSE.PC.MAR.RE.Sys = MSE.PC.Sys(data.list.PC,nComp=q,Method.type=3)

save(MSE.MVAR.Sys.PC, file = paste("MSE_MVAR_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_R_",r,".RData",sep = ""))
save(MSE.VAR.Sys.PC, file = paste("MSE_VAR_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_R_",r,".RData",sep = ""))
save(MSE.AR.Sys.PC, file = paste("MSE_AR_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_R_",r,".RData",sep = ""))

save(MSE.PC.VAR.Sys, file = paste("MSE_PC_VAR_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_nComp_",q,"_R_",r,".RData",sep = ""))
save(MSE.PC.MAR.Sys, file = paste("MSE_PC_MAR_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_nComp_",q,"_R_",r,".RData",sep = ""))
save(MSE.PC.MAR.RE.Sys, file = paste("MSE_PC_MAR_RE_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_nComp_",q,"_R_",r,".RData",sep = ""))

if (q>1){
MSE.PC.MVAR.Sys = MSE.PC.Sys(data.list.PC,nComp=q,Method.type=4)
MSE.PC.MVAR.RE.Sys = MSE.PC.Sys(data.list.PC,nComp=q,Method.type=5)
save(MSE.PC.MVAR.Sys, file = paste("MSE_PC_MVAR_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_nComp_",q,"_R_",r,".RData",sep = ""))
save(MSE.PC.MVAR.RE.Sys, file = paste("MSE_PC_MVAR_RE_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_nComp_",q,"_R_",r,".RData",sep = ""))
}}

######################################################################################
######################################################################################
######################################################################################


