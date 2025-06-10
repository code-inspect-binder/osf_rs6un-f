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

# P is the number of variables
# sigma.e is the error of the observed variables

# This function simulates data from a VAR(1) model

Data.PC.VAR = function(N,T,P,Psi,cor.Sigma,var.Psi,var.E){

Q = ncol(Psi)

# Create variables days, beeps per day and ID
data = expand.grid(Time=1:T,ID=1:N)

F = matrix(0,nrow(data),Q)
colnames(F) = sprintf("F%d",seq(1:Q))

# Covariance matrix of the random effects
Sigma.Psi = var.Psi*diag(length(as.vector(Psi)))

Psi.list = list()
lambda.max.list = rep(0,N)

if (Q == 1){
# Simulate multilevel PC-AR(1) model
for (i in 1:N){
n.i = which(data$ID==i)
Psi.list[[i]] = 1
Psi.max = 1 

iter = 1
while(abs(Psi.max)>=1){
Psi.i  = rnorm(1,Psi,var.Psi)
Psi.max = abs(Psi.i)
iter = iter + 1
}
Psi.list[[i]] = Psi.i

F[n.i] = arima.sim(list(order = c(1,0,0), ar = Psi.i), n = length(n.i)) 
}}

if (Q > 1){
# Simulate multilevel PC-VAR(1) model
for (i in 1:N){
n.i = which(data$ID==i)
Sigma = matrix(cor.Sigma,Q,Q)
diag(Sigma) = 1

lambda.max = 1 
iter = 1
while(abs(lambda.max)>=1){
nu = mvrnorm(n = 1, mu = as.vector(Psi),Sigma = Sigma.Psi,empirical = FALSE)
Psi.i = matrix(nu,Q,Q) 
lambda.max = max(abs(eigen(Psi.i)$values))
iter = iter + 1
}
Psi.list[[i]] = Psi.i
lambda.max.list[i] = max(abs(eigen(Psi.i)$values))

Psi.i = array(Psi.i, dim = c(Q, Q, 1))
F[n.i,] = varima.sim(list(ar = Psi.i, ma = NULL),n = length(n.i),k = Q,sigma = Sigma) 
}}

# Create the loading matrix
eye = diag(1,Q)
B = eye[unlist(lapply(1:Q, function(q) rep(q,P/Q))),]

# For each person combine component scores F, with loading matrix B and error values E to obtain latent structure
# For each person standardized the data using the within-person mean and standard deviation

Y = matrix(0,nrow(data),P)
colnames(Y) = sprintf("Y%d",seq(1:P))

for (i in 1:N){
n.i = which(data$ID==i)
E = sqrt(var.E)*mvrnorm(length(n.i), rep(0,P), diag(P))
Y[n.i,] = F[n.i,] %*% t(B) + E 
Y[n.i,] = apply(Y[n.i,],2,scale)
}

data = cbind(data,Y)

return(list(Sigma.Psi=Sigma.Psi,Psi=Psi,Psi.i=Psi.list,lambda.max.list=lambda.max.list,Components=F,data=data))
}
