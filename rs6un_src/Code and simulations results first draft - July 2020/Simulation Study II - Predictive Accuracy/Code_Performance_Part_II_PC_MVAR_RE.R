### clear workspace

ls()

rm(list=ls())

######################################################################################
######################################################################################
######################################################################################

# Upload the functions
source(file="Performance.PC.VAR.R")

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

# Set the number of replicates
R = 10 # Number of replicates

# Set the number of blocks in block cross-validation
fold = 10

######################################################################################
######################################################################################
######################################################################################

# N = 20, Q = 2, Var.E = 5%
rbind(Performance.PC.VAR(N,n=1,T,t=1,Q,q=1,e=1,R,P)[[1]],
Performance.PC.VAR(N,n=1,T,t=2,Q,q=1,e=1,R,P)[[1]],
Performance.PC.VAR(N,n=2,T,t=1,Q,q=1,e=1,R,P)[[1]],
Performance.PC.VAR(N,n=2,T,t=2,Q,q=1,e=1,R,P)[[1]])

rbind(Performance.PC.VAR(N,n=1,T,t=1,Q,q=1,e=1,R,P)[[2]],
Performance.PC.VAR(N,n=1,T,t=2,Q,q=1,e=1,R,P)[[2]],
Performance.PC.VAR(N,n=2,T,t=1,Q,q=1,e=1,R,P)[[2]],
Performance.PC.VAR(N,n=2,T,t=2,Q,q=1,e=1,R,P)[[2]])

# N = 20, Q = 2, Var.E = 50%
rbind(Performance.PC.VAR(N,n=1,T,t=1,Q,q=1,e=2,R,P)[[1]],
Performance.PC.VAR(N,n=1,T,t=2,Q,q=1,e=2,R,P)[[1]],
Performance.PC.VAR(N,n=2,T,t=1,Q,q=1,e=2,R,P)[[1]],
Performance.PC.VAR(N,n=2,T,t=2,Q,q=1,e=2,R,P)[[1]])

rbind(Performance.PC.VAR(N,n=1,T,t=1,Q,q=1,e=2,R,P)[[2]],
Performance.PC.VAR(N,n=1,T,t=2,Q,q=1,e=2,R,P)[[2]],
Performance.PC.VAR(N,n=2,T,t=1,Q,q=1,e=2,R,P)[[2]],
Performance.PC.VAR(N,n=2,T,t=2,Q,q=1,e=2,R,P)[[2]])

########################################
########################################
########################################

# N = 20, Q = 2, Var.E = 5%
rbind(Performance.PC.VAR(N,n=1,T,t=1,Q,q=2,e=1,R,P)[[1]],
Performance.PC.VAR(N,n=1,T,t=2,Q,q=2,e=1,R,P)[[1]],
Performance.PC.VAR(N,n=2,T,t=1,Q,q=2,e=1,R,P)[[1]],
Performance.PC.VAR(N,n=2,T,t=2,Q,q=2,e=1,R,P)[[1]])

rbind(Performance.PC.VAR(N,n=1,T,t=1,Q,q=2,e=1,R,P)[[2]],
Performance.PC.VAR(N,n=1,T,t=2,Q,q=2,e=1,R,P)[[2]],
Performance.PC.VAR(N,n=2,T,t=1,Q,q=2,e=1,R,P)[[2]],
Performance.PC.VAR(N,n=2,T,t=2,Q,q=2,e=1,R,P)[[2]])

# N = 20, Q = 2, Var.E = 50%
rbind(Performance.PC.VAR(N,n=1,T,t=1,Q,q=2,e=2,R,P)[[1]],
Performance.PC.VAR(N,n=1,T,t=2,Q,q=2,e=2,R,P)[[1]],
Performance.PC.VAR(N,n=2,T,t=1,Q,q=2,e=2,R,P)[[1]],
Performance.PC.VAR(N,n=2,T,t=2,Q,q=2,e=2,R,P)[[1]])

rbind(Performance.PC.VAR(N,n=1,T,t=1,Q,q=2,e=2,R,P)[[2]],
Performance.PC.VAR(N,n=1,T,t=2,Q,q=2,e=2,R,P)[[2]],
Performance.PC.VAR(N,n=2,T,t=1,Q,q=2,e=2,R,P)[[2]],
Performance.PC.VAR(N,n=2,T,t=2,Q,q=2,e=2,R,P)[[2]])

########################################
########################################
########################################

# N = 20, Q = 3, Var.E = 5%
rbind(Performance.PC.VAR(N,n=1,T,t=1,Q,q=3,e=1,R,P)[[1]],
Performance.PC.VAR(N,n=1,T,t=2,Q,q=3,e=1,R,P)[[1]],
Performance.PC.VAR(N,n=2,T,t=1,Q,q=3,e=1,R,P)[[1]],
Performance.PC.VAR(N,n=2,T,t=2,Q,q=3,e=1,R,P)[[1]])

rbind(Performance.PC.VAR(N,n=1,T,t=1,Q,q=3,e=1,R,P)[[2]],
Performance.PC.VAR(N,n=1,T,t=2,Q,q=3,e=1,R,P)[[2]],
Performance.PC.VAR(N,n=2,T,t=1,Q,q=3,e=1,R,P)[[2]],
Performance.PC.VAR(N,n=2,T,t=2,Q,q=3,e=1,R,P)[[2]])

# N = 20, Q = 3, Var.E = 50%
rbind(Performance.PC.VAR(N,n=1,T,t=1,Q,q=3,e=2,R,P)[[1]],
Performance.PC.VAR(N,n=1,T,t=2,Q,q=3,e=2,R,P)[[1]],
Performance.PC.VAR(N,n=2,T,t=1,Q,q=3,e=2,R,P)[[1]],
Performance.PC.VAR(N,n=2,T,t=2,Q,q=3,e=2,R,P)[[1]])

rbind(Performance.PC.VAR(N,n=1,T,t=1,Q,q=3,e=2,R,P)[[2]],
Performance.PC.VAR(N,n=1,T,t=2,Q,q=3,e=2,R,P)[[2]],
Performance.PC.VAR(N,n=2,T,t=1,Q,q=3,e=2,R,P)[[2]],
Performance.PC.VAR(N,n=2,T,t=2,Q,q=3,e=2,R,P)[[2]])

######################################################################################
######################################################################################
######################################################################################





