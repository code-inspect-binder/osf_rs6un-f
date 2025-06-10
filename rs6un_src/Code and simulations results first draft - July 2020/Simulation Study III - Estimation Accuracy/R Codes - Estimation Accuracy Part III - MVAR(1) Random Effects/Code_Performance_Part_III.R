### clear workspace

ls()

rm(list=ls())

######################################################################################
######################################################################################
######################################################################################

# Upload the functions
source(file="Performance.VAR.R")

######################################################################################
######################################################################################
######################################################################################

# Set the values to simulate data assuming a VAR(1) process
set.seed(123) # Set random seed
N = 60 # Number of participants
T = c(50,100) # Number of time points
P = 4 # Number of variables in VAR(1) models
var.Psi = 0.025 # Set the variance of the random effects of the regression weights (i.e., all variances are assumed to be equal)
cor.Sigma = 0.2 # Set the covariance of the within-individuals errors (i.e., all covariances are assumed to be equal)
b.ar.min = 0.2 
b.ar.max = 0.6
b.cr.min = 0.05
b.cr.max = 0.2
K = c(2,4)
diff = c(1,2)
size = 1

# Set the number of replicates
R = 10 # Number of replicates

# Set the number of blocks in block cross-validation
fold = 10

######################################################################################
######################################################################################
######################################################################################

######################################################################################
######################################################################################
######################################################################################

cbind(Performance.VAR(N,n=1,T,t=1,P,p=1,K,k=1,diff,d=1,size,s=1,R)[[1]],
Performance.VAR(N,n=1,T,t=2,P,p=1,K,k=1,diff,d=1,size,s=1,R)[[1]],

Performance.VAR(N,n=1,T,t=1,P,p=1,K,k=1,diff,d=2,size,s=1,R)[[1]],
Performance.VAR(N,n=1,T,t=2,P,p=1,K,k=1,diff,d=2,size,s=1,R)[[1]],

Performance.VAR(N,n=1,T,t=1,P,p=1,K,k=2,diff,d=1,size,s=1,R)[[1]],
Performance.VAR(N,n=1,T,t=2,P,p=1,K,k=2,diff,d=1,size,s=1,R)[[1]],

Performance.VAR(N,n=1,T,t=1,P,p=1,K,k=2,diff,d=2,size,s=1,R)[[1]],
Performance.VAR(N,n=1,T,t=2,P,p=1,K,k=2,diff,d=2,size,s=1,R)[[1]])

########################################





