#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

#################################################################
#################################################################
#################################################################
###                      MSE PC-VAR(1)                        ###
#################################################################
#################################################################
#################################################################

# Function to estimate the predictive accuracy of multilevel AR(1) by performing k-block cross-validation
# k-block CV devides the data set in 10 parts and then perform cross-validation

# This function input is the trainning and testing set, and the names of
# the outcome variable, lagged-outcome variable and lagged predictors
# The output are the predictive accuracy measures

MSE.VAR.Sys.fit = function(data,P){

n.ID = unique(data$ID)

names.Y = sprintf("Y%d",seq(1:P))
names.Y.lag = sprintf("Y%d_1",seq(1:P))

MSE.i = list()
MSE.Mean.i = list()
Psi.hat.i = list()
Psi.CI.hat.i = list()
eps.sd.i = list()
resid.i = list()

for (j in 1:P){

MSE.i[[j]] = list()
MSE.Mean.i[[j]] = list()
Psi.hat.i[[j]] = list()
Psi.CI.hat.i[[j]] = list()
eps.sd.i[[j]] = list()
resid.i[[j]] = list()

for (i in 1:length(n.ID)){

n.i = which(data$ID==i)
data.i = data[n.i,]

# AR(1) analysis on original dataset

Model = as.formula(paste(names.Y[j],paste(names.Y.lag, collapse = " + "),sep = " ~ "))
fit = lm(Model,data.i)

Psi.hat.i[[j]][[i]] = coefficients(fit)
Psi.CI.hat.i[[j]][[i]] = confint(fit)

Psi.hat.ij = coefficients(fit)
Psi.CI.hat.ij = confint(fit)

eps.sd.i[[j]][[i]] = sigma(fit)
resid.i[[j]][[i]] = data.i[,names.Y[j]] - Psi.hat.ij[1]- 
as.matrix(data.i[,c(names.Y.lag)]) %*% as.matrix(Psi.hat.ij[-1])

MSE.i[[j]][[i]] = (data.i[,names.Y[j]] - Psi.hat.ij[1]- 
as.matrix(data.i[,c(names.Y.lag)]) %*% as.matrix(Psi.hat.ij[-1]))^2

MSE.Mean.i[[j]][[i]] = (data.i[,names.Y[j]] - mean(data.i[,names.Y[j]]))^2
}}

# Compute MSE 

MSE.VAR = lapply(1:P, function(j) mean(unlist(lapply(n.ID, function(i) mean(MSE.i[[j]][[i]],na.rm=TRUE)))))

# Compute R square
R2.Sys = lapply(1:P, function(j) sum(unlist(MSE.i[[j]]),na.rm=T))
R2.Sys.Mean = lapply(1:P, function(j) sum(unlist(MSE.Mean.i[[j]])))
R2.Sys.VAR = lapply(1:P, function(j) 1 - (R2.Sys[[j]]/R2.Sys.Mean[[j]]))

return(list(Psi.hat.i=Psi.hat.i,Psi.CI.hat.i=Psi.CI.hat.i,MSE.VAR=MSE.VAR,
R2.Sys.VAR=R2.Sys.VAR,eps.sd.i=eps.sd.i,resid.i=resid.i))}
