#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

### clear workspace

ls()

rm(list=ls())

######################################################################################
######################################################################################
######################################################################################

# Upload the functions
source(file="Performance.MVAR.FE.R")

######################################################################################
######################################################################################
######################################################################################

# Set the values to simulate data assuming a VAR(1) process
set.seed(123) # Set random seed
N = c(20,60,120) # Number of participants
T = c(50,100,200) # Number of time points
P = c(2,4,6,8) # Number of variables in VAR(1) models

# Set the number of replicates
R = 10 # Number of replicates

# Set the number of blocks in block cross-validation
fold = 10

######################################################################################
######################################################################################
######################################################################################

# P=2

rbind(Performance.MVAR.FE(N,n=1,T,t=1,P,p=1,R)[[1]],
Performance.MVAR.FE(N,n=1,T,t=2,P,p=1,R)[[1]],
Performance.MVAR.FE(N,n=1,T,t=3,P,p=1,R)[[1]],

Performance.MVAR.FE(N,n=2,T,t=1,P,p=1,R)[[1]],
Performance.MVAR.FE(N,n=2,T,t=2,P,p=1,R)[[1]],
Performance.MVAR.FE(N,n=2,T,t=3,P,p=1,R)[[1]],

Performance.MVAR.FE(N,n=3,T,t=1,P,p=1,R)[[1]],
Performance.MVAR.FE(N,n=3,T,t=2,P,p=1,R)[[1]],
Performance.MVAR.FE(N,n=3,T,t=3,P,p=1,R)[[1]])

rbind(Performance.MVAR.FE(N,n=1,T,t=1,P,p=1,R)[[2]],
Performance.MVAR.FE(N,n=1,T,t=2,P,p=1,R)[[2]],
Performance.MVAR.FE(N,n=1,T,t=3,P,p=1,R)[[2]],

Performance.MVAR.FE(N,n=2,T,t=1,P,p=1,R)[[2]],
Performance.MVAR.FE(N,n=2,T,t=2,P,p=1,R)[[2]],
Performance.MVAR.FE(N,n=2,T,t=3,P,p=1,R)[[2]],

Performance.MVAR.FE(N,n=3,T,t=1,P,p=1,R)[[2]],
Performance.MVAR.FE(N,n=3,T,t=2,P,p=1,R)[[2]],
Performance.MVAR.FE(N,n=3,T,t=3,P,p=1,R)[[2]])

# P=4

rbind(Performance.MVAR.FE(N,n=1,T,t=1,P,p=2,R)[[1]],
Performance.MVAR.FE(N,n=1,T,t=2,P,p=2,R)[[1]],
Performance.MVAR.FE(N,n=1,T,t=3,P,p=2,R)[[1]],

Performance.MVAR.FE(N,n=2,T,t=1,P,p=2,R)[[1]],
Performance.MVAR.FE(N,n=2,T,t=2,P,p=2,R)[[1]],
Performance.MVAR.FE(N,n=2,T,t=3,P,p=2,R)[[1]],

Performance.MVAR.FE(N,n=3,T,t=1,P,p=2,R)[[1]],
Performance.MVAR.FE(N,n=3,T,t=2,P,p=2,R)[[1]],
Performance.MVAR.FE(N,n=3,T,t=3,P,p=2,R)[[1]])

rbind(Performance.MVAR.FE(N,n=1,T,t=1,P,p=2,R)[[2]],
Performance.MVAR.FE(N,n=1,T,t=2,P,p=2,R)[[2]],
Performance.MVAR.FE(N,n=1,T,t=3,P,p=2,R)[[2]],

Performance.MVAR.FE(N,n=2,T,t=1,P,p=2,R)[[2]],
Performance.MVAR.FE(N,n=2,T,t=2,P,p=2,R)[[2]],
Performance.MVAR.FE(N,n=2,T,t=3,P,p=2,R)[[2]],

Performance.MVAR.FE(N,n=3,T,t=1,P,p=2,R)[[2]],
Performance.MVAR.FE(N,n=3,T,t=2,P,p=2,R)[[2]],
Performance.MVAR.FE(N,n=3,T,t=3,P,p=2,R)[[2]])

# P=6

rbind(Performance.MVAR.FE(N,n=1,T,t=1,P,p=3,R)[[1]],
Performance.MVAR.FE(N,n=1,T,t=2,P,p=3,R)[[1]],
Performance.MVAR.FE(N,n=1,T,t=3,P,p=3,R)[[1]],

Performance.MVAR.FE(N,n=2,T,t=1,P,p=3,R)[[1]],
Performance.MVAR.FE(N,n=2,T,t=2,P,p=3,R)[[1]],
Performance.MVAR.FE(N,n=2,T,t=3,P,p=3,R)[[1]],

Performance.MVAR.FE(N,n=3,T,t=1,P,p=3,R)[[1]],
Performance.MVAR.FE(N,n=3,T,t=2,P,p=3,R)[[1]],
Performance.MVAR.FE(N,n=3,T,t=3,P,p=3,R)[[1]])

rbind(Performance.MVAR.FE(N,n=1,T,t=1,P,p=3,R)[[2]],
Performance.MVAR.FE(N,n=1,T,t=2,P,p=3,R)[[2]],
Performance.MVAR.FE(N,n=1,T,t=3,P,p=3,R)[[2]],

Performance.MVAR.FE(N,n=2,T,t=1,P,p=3,R)[[2]],
Performance.MVAR.FE(N,n=2,T,t=2,P,p=3,R)[[2]],
Performance.MVAR.FE(N,n=2,T,t=3,P,p=3,R)[[2]],

Performance.MVAR.FE(N,n=3,T,t=1,P,p=3,R)[[2]],
Performance.MVAR.FE(N,n=3,T,t=2,P,p=3,R)[[2]],
Performance.MVAR.FE(N,n=3,T,t=3,P,p=3,R)[[2]])

# P=8

rbind(Performance.MVAR.FE(N,n=1,T,t=1,P,p=4,R)[[1]],
Performance.MVAR.FE(N,n=1,T,t=2,P,p=4,R)[[1]],
Performance.MVAR.FE(N,n=1,T,t=3,P,p=4,R)[[1]],

Performance.MVAR.FE(N,n=2,T,t=1,P,p=4,R)[[1]],
Performance.MVAR.FE(N,n=2,T,t=2,P,p=4,R)[[1]],
Performance.MVAR.FE(N,n=2,T,t=3,P,p=4,R)[[1]],

Performance.MVAR.FE(N,n=3,T,t=1,P,p=4,R)[[1]],
Performance.MVAR.FE(N,n=3,T,t=2,P,p=4,R)[[1]],
Performance.MVAR.FE(N,n=3,T,t=3,P,p=4,R)[[1]])

rbind(Performance.MVAR.FE(N,n=1,T,t=1,P,p=4,R)[[2]],
Performance.MVAR.FE(N,n=1,T,t=2,P,p=4,R)[[2]],
Performance.MVAR.FE(N,n=1,T,t=3,P,p=4,R)[[2]],

Performance.MVAR.FE(N,n=2,T,t=1,P,p=4,R)[[2]],
Performance.MVAR.FE(N,n=2,T,t=2,P,p=4,R)[[2]],
Performance.MVAR.FE(N,n=2,T,t=3,P,p=4,R)[[2]],

Performance.MVAR.FE(N,n=3,T,t=1,P,p=4,R)[[2]],
Performance.MVAR.FE(N,n=3,T,t=2,P,p=4,R)[[2]],
Performance.MVAR.FE(N,n=3,T,t=3,P,p=4,R)[[2]])

########################################









