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

Data.AR = function(N,T,Psi,cor.Sigma,var.Psi){

p = ncol(Psi)

# Create variables days, beeps per day and ID
data = expand.grid(Time=1:T,ID=1:N)

Y = matrix(0,nrow(data),p)
colnames(Y) = sprintf("Y%d",seq(1:p))

# Covariance matrix of the random effects
Sigma.Psi = diag(var.Psi*rep(1,length(diag(Psi))))

# Simulate multilevel VAR(1) model

Psi.list = list()
lambda.max.list = rep(0,N)

for (i in 1:N){
n.i = which(data$ID==i)
Sigma = matrix(cor.Sigma,p,p)
diag(Sigma) = 1

lambda.max = 1 
iter = 1
while(abs(lambda.max)>=1){
nu = mvrnorm(n = 1, mu = as.vector(diag(Psi)),Sigma = Sigma.Psi,empirical = FALSE)
Psi.i = diag(nu) 
lambda.max = max(abs(eigen(Psi.i)$values))
iter = iter + 1
}
Psi.list[[i]] = Psi.i
lambda.max.list[i] = max(abs(eigen(Psi.i)$values))
Y[n.i,] = VAR.sim(B=Psi.list[[i]], n=length(n.i), lag=1, include = "none", varcov=Sigma)
}

data = cbind(data,Y)

return(list(Sigma.Psi=Sigma.Psi,Psi=Psi,Psi.i=Psi.list,lambda.max.list=lambda.max.list,data=data))
}