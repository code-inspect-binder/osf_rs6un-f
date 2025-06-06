#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

#################################################################
#################################################################
#################################################################
##### Generate data assuming an underlying VAR(1) process #######
#################################################################
#################################################################
#################################################################

# Function to generate data assuming an underlying VAR(1) process

# the input is N the number of samples, T.days and T.beeps are the number of days and the number of beeps ESM is conducted (Total number of assessments is T.days x T.beeps)
# Psi is a matrix with the fixed regression weights, mu is a vector with the fixed intercepts, var.Psi is variance of the random regression weights (i.e., assumed to be equal), 

# For each participant, it is checked whether the matrix Psi conforms the assumption of stationary time series 
# with the absolute value of the maximum eigenvalue smaller than 1 
# The distribution of the random effects resembles a truncated multivariate normal distribution.

# This function simulates data from a VAR(1) model

Data.Cluster.VAR = function(N,T,K,size,Psi,cor.Sigma,var.Psi){

# Number of subjects in each group
# Create a group variable according to each cluster condition
if (size == 1){
N.size = N/K
n.group = unlist(lapply(1:K, function(k) rep(k,N.size)))
}

if (size == 2){
if (K == 2){
N.1 = 0.10*N
N.2 = N - N.1
n.group = c(rep(1,N.1),rep(2,N.2))
}
if (K == 4){
N.1 = 0.10*N
N.rest = N - N.1
N.size = N.rest/(K-1)
n.group = c(rep(1,N.1),rep(2,N.size),rep(3,N.size),rep(4,N.size))
}}

if (size == 3){
if (K == 2){
N.1 = 0.6*N
N.2 = N - N.1
n.group = c(rep(1,N.1),rep(2,N.2))
}
if (K == 4){
if (N == 20){
N.1 = 0.6*N
N.rest = N - N.1
N.size = floor(N.rest/(K-1))
n.group = c(rep(1,N.1),rep(2,N.size),rep(3,N.size+1),rep(4,N.size+1))
}
else{
N.1 = 0.6*N
N.rest = N - N.1
N.size = N.rest/(K-1)
n.group = c(rep(1,N.1),rep(2,N.size),rep(3,N.size),rep(4,N.size))
}}}

# Create variables days, beeps per day and Z
data = expand.grid(Time=1:T,Z=n.group)

# Create variable subjno
# Scramble data so people appear unordered in the final dataset
ID = expand.grid(1:T,1:N)[,2]
data = cbind(data,ID)

p = ncol(Psi[[1]])

Y = matrix(0,nrow(data),p)
colnames(Y) = sprintf("Y%d",seq(1:p))

# Covariance matrix of the random effects
Sigma.Psi = var.Psi*diag(length(as.vector(Psi[[1]])))

# Simulate multilevel VAR(1) model

Psi.list = list()
lambda.max.list = rep(0,N)

for (i in 1:N){
n.i = which(data$ID==i)
Sigma = matrix(cor.Sigma,p,p)
diag(Sigma) = 1
z.i = unique(data$Z[n.i])

lambda.max = 1 
iter = 1
while(abs(lambda.max)>=1){
nu = mvrnorm(n = 1, mu = as.vector(Psi[[z.i]]),Sigma = Sigma.Psi,empirical = FALSE)
Psi.i = matrix(nu,p,p) 
lambda.max = max(abs(eigen(Psi.i)$values))
iter = iter + 1
}
Psi.list[[i]] = Psi.i
lambda.max.list[i] = max(abs(eigen(Psi.i)$values))

Y[n.i,] = VAR.sim(B=Psi.list[[i]], n=length(n.i), lag=1, include = "none", varcov=Sigma)
}

Cluster = cbind(Time=data$Time,ID=data$ID,Group=data$Z)
data = data.frame(cbind(Time=data$Time,ID=data$ID,Y))

return(list(Sigma.Psi=Sigma.Psi,Psi=Psi,Psi.i=Psi.list,lambda.max.list=lambda.max.list,data=data,Cluster=Cluster))
}