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

Data.AR = function(N,T,Psi,var.Psi){

p = length(Psi)

# Create variables days, beeps per day and ID
data = expand.grid(Time=1:T,ID=1:N)

Y = matrix(0,nrow(data),p)
colnames(Y) = sprintf("Y%d",seq(1:p))

# Simulate multilevel VAR(1) model

Psi.list = list()

for (i in 1:N){
n.i = which(data$ID==i)
Psi.list[[i]] = rep(1,p)
Psi.max = rep(1,p) 
for (j in 1:p){
iter = 1
while(abs(Psi.max[j])>=1){
Psi.i  = rnorm(1,Psi[j],var.Psi)
Psi.max[j] = abs(Psi.i)
iter = iter + 1
}
Psi.list[[i]][j] = Psi.i

Y[n.i,j] = arima.sim(list(order = c(1,0,0), ar = Psi.i), n = length(n.i)) 
}}

data = cbind(data,Y)

data = data.prep(data)

return(list(Sigma.Psi=NULL,Psi=Psi,Psi.i=Psi.list,lambda.max.list=NULL,data=data))
}
