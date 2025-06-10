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
source(file="Data.Cluster.VAR.R")
source(file="Data.Cluster.VAR.Fixed.R")
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
source(file="Psi.Matrix.Diff.R")

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
Eff = c(1,2) # Type of effects (1=fixed) and (2=random)
N = c(20,60) # Number of participants
T = c(50,100,200) # Number of time points
P = 4 # Number of variables in VAR(1) models
var.Psi = 0.025 # Set the variance of the random effects of the regression weights (i.e., all variances are assumed to be equal)
cor.Sigma = 0.2 # Set the covariance of the within-individuals errors (i.e., all covariances are assumed to be equal)
b.ar.min = 0.2 
b.ar.max = 0.6
b.cr.min = 0.05
b.cr.max = 0.2
K = c(2,4)
diff = 1
size = c(1,2,3)

# Set the number of replicates
R = 100 # Number of replicates

# Set the number of blocks in block cross-validation
fold = 10

######################################################################################
######################################################################################
######################################################################################

# Generate Psi matrix for each cluster

list.sim = expand.grid(Rr=1:R,Tt=T,Nn=N,Pp=P,Kk=K,Df=diff,Ss=size,Ef=Eff)

for (w in 1:nrow(list.sim)){
n = list.sim$Nn[w]
t = list.sim$Tt[w]
p = list.sim$Pp[w]
r = list.sim$Rr[w]
k = list.sim$Kk[w]
d = list.sim$Df[w]
s = list.sim$Ss[w]
e = list.sim$Ef[w]

Psi.list = lapply(1:k, function(kl) 
Psi.Matrix.Diff(p,b.ar.min,b.ar.max,b.cr.min,b.cr.max,d))
# Save Psi matrix
save(Psi.list, file = paste("Psi_",e,"_N_",n,"_T_",t,"_P_",p,"_K_",k,
"_Diff_",d,"_size_",s,"_R_",r,".RData",sep = ""))

# Simulate dataset
# Monte Carlo replicates

if (e == 1){
Sim.Var.Data = Data.Cluster.VAR.Fixed(n,t,k,s,Psi.list,cor.Sigma)
# Save datasets
save(Sim.Var.Data, file = paste("Data_Effect_",e,"_N_",n,"_T_",t,"_P_",p,"_K_",k,
"_Diff_",d,"_size_",s,"_R_",r,".RData",sep = ""))
}

if (e == 2){
Sim.Var.Data = Data.Cluster.VAR(n,t,k,s,Psi.list,cor.Sigma,var.Psi)
# Save datasets
save(Sim.Var.Data, file = paste("Data_Effect_",e,"_N_",n,"_T_",t,"_P_",p,"_K_",k,
"_Diff_",d,"_size_",s,"_R_",r,".RData",sep = ""))
}}

######################################################################################
######################################################################################
###################################################################################### 

# Generate training and testing set

list.sim = expand.grid(Rr=1:R,Tt=T,Nn=N,Pp=P,Kk=K,Df=diff,Ss=size,Ef=Eff)

for (w in 1:nrow(list.sim)){
n = list.sim$Nn[w]
t = list.sim$Tt[w]
p = list.sim$Pp[w]
r = list.sim$Rr[w]
k = list.sim$Kk[w]
d = list.sim$Df[w]
s = list.sim$Ss[w]
e = list.sim$Ef[w]

# Generate training and testing set
load(file = paste("Data_Effect_",e,"_N_",n,"_T_",t,"_P_",p,"_K_",k,
"_Diff_",d,"_size_",s,"_R_",r,".RData",sep = ""))

data.list = data.block(Sim.Var.Data$data,fold)

# Save datasets
save(data.list, file = paste("Data_Block_Effect_",e,"_N_",n,"_T_",t,"_P_",p,"_K_",k,
"_Diff_",d,"_size_",s,"_R_",r,".RData",sep = ""))
}

######################################################################################
######################################################################################
######################################################################################

# Perform K-block cross-validation
# Estimate Multilevel VAR(1) model

plan(multisession)
julia.dir = julia_setup(JULIA_HOME = "C:\\Users\\u0119584\\AppData\\Local\\Programs\\Julia 1.5.3\\bin")
julia.dir$library("MixedModels")

list.sim = expand.grid(Rr=1:R,Tt=T,Nn=N,Pp=P,Kk=K,Df=diff,Ss=size,Ef=Eff)

for (w in 1:nrow(list.sim)){
n = list.sim$Nn[w]
t = list.sim$Tt[w]
p = list.sim$Pp[w]
r = list.sim$Rr[w]
k = list.sim$Kk[w]
d = list.sim$Df[w]
s = list.sim$Ss[w]
e = list.sim$Ef[w]

# Load training and testing set
load(file = paste("Data_Block_Effect_",e,"_N_",n,"_T_",t,"_P_",p,"_K_",k,
"_Diff_",d,"_size_",s,"_R_",r,".RData",sep = ""))

MSE.MVAR.PS.Sys = MSE.Sys(data.list,p)
MSE.VAR.PS.Sys = MSE.VAR.Sys(data.list,p)
MSE.AR.PS.Sys = MSE.AR.Sys(data.list,p)

save(MSE.MVAR.PS.Sys, file = paste("MSE_MVAR_Effect_",Eff[e],"_N_",n,"_T_",t,"_P_",p,"_K_",k,
"_Diff_",d,"_size_",s,"_R_",r,".RData",sep = ""))
save(MSE.VAR.PS.Sys, file = paste("MSE_VAR_Effect_",Eff[e],"_N_",n,"_T_",t,"_P_",p,"_K_",k,
"_Diff_",d,"_size_",s,"_R_",r,".RData",sep = ""))
save(MSE.AR.PS.Sys, file = paste("MSE_AR_Effect_",Eff[e],"_N_",n,"_T_",t,"_P_",p,"_K_",k,
"_Diff_",d,"_size_",s,"_R_",r,".RData",sep = ""))
}

######################################################################################
######################################################################################
######################################################################################





