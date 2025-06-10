#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

#################################################################
#################################################################
#################################################################
###          Predictive Accuracy PC-VAR(1) (System)           ###
#################################################################
#################################################################
#################################################################

# Function to estimate the predictive accuracy of multilevel AR(1) by performing k-block cross-validation
# k-block CV devides the data set in 10 parts and then perform cross-validation

# This function input is the trainning and testing set, and the names of
# the outcome variable, lagged-outcome variable and lagged predictors
# The output are the predictive accuracy measures

MSE.VAR.Sys = function(data.list,P){

n.ID = unique(data.list[[3]])

fold = data.list[[4]]

# Estimate Person specific VAR(1) Model
MSE.k = lapply(1:fold, function(k) MSE.VAR.fold(data.list,P,k))

# Estimated regression coefficients 
beta.hat = lapply(1:fold, function(k) MSE.k[[k]]$beta.i)

# Compute MSE
MSE.Sys.k = list()
MSE.Sys.VAR = list()
MSE.Sys.VAR.sd = list()
MSE.Sys.VAR.se = list()
MSE.Sys.VAR.1se = list()
for (p in 1:P){
MSE.Sys.k[[p]] = unlist(lapply(1:fold, function(k) MSE.k[[k]]$MSE.k[[p]]))
MSE.Sys.VAR[[p]] = mean(MSE.Sys.k[[p]])
MSE.Sys.VAR.sd[[p]] = sd(MSE.Sys.k[[p]])
MSE.Sys.VAR.se[[p]] = sd(MSE.Sys.k[[p]])/sqrt(fold)
MSE.Sys.VAR.1se[[p]] = MSE.Sys.VAR[[p]] + MSE.Sys.VAR.se[[p]]
}
names(MSE.Sys.k) = sprintf("Y%d",seq(1:P))
names(MSE.Sys.VAR) = sprintf("Y%d",seq(1:P))
names(MSE.Sys.VAR.sd) = sprintf("Y%d",seq(1:P))
names(MSE.Sys.VAR.se) = sprintf("Y%d",seq(1:P))
names(MSE.Sys.VAR.1se) = sprintf("Y%d",seq(1:P))

# MSE for each participant
MSE.Sys.VAR.i = lapply(1:fold, function(k) MSE.k[[k]]$MSE.ki)

# Compute R square
R2.Sys.k = list()
R2.Sys.Mean.k = list()
R2.Sys.VAR = list()
R2.Sys.VAR.sd = list()
for (p in 1:P){
R2.Sys.k[[p]] = sum(unlist(lapply(1:fold, function(k) sum(unlist(MSE.k[[k]]$MSE.ki[[p]]),na.rm=T))))
R2.Sys.Mean.k[[p]] = sum(unlist(lapply(1:fold, function(k) sum(unlist(MSE.k[[k]]$MSE.Mean.ki[[p]])))))
R2.Sys.VAR[[p]] = 1 - (R2.Sys.k[[p]]/R2.Sys.Mean.k[[p]])
}
names(R2.Sys.VAR) = sprintf("Y%d",seq(1:P))

# Compute Bias
Bias.Sys.k = list()
Bias.Sys.VAR = list()
for (p in 1:P){
Bias.Sys.k[[p]] = unlist(lapply(1:fold, function(k) MSE.k[[k]]$Bias.k[[p]]))
Bias.Sys.VAR[[p]] = mean(Bias.Sys.k[[p]])
}
names(Bias.Sys.k) = sprintf("Y%d",seq(1:P))
names(Bias.Sys.VAR) = sprintf("Y%d",seq(1:P))

# Compute Variance
Var.Sys.k = list()
Var.Sys.VAR = list()
for (p in 1:P){
Var.Sys.k[[p]] = unlist(lapply(1:fold, function(k) MSE.k[[k]]$Var.k[[p]]))
Var.Sys.VAR[[p]] = mean(Var.Sys.k[[p]])
}
names(Var.Sys.k) = sprintf("Y%d",seq(1:P))
names(Var.Sys.VAR) = sprintf("Y%d",seq(1:P))

return(list(MSE.Sys.k = MSE.Sys.k,
MSE.Sys.VAR = MSE.Sys.VAR,
MSE.Sys.VAR.sd = MSE.Sys.VAR.sd,
MSE.Sys.VAR.se = MSE.Sys.VAR.se,
MSE.Sys.VAR.1se = MSE.Sys.VAR.1se,
beta.hat = beta.hat,
MSE.Sys.VAR.i = MSE.Sys.VAR.i,
R2.Sys.VAR = R2.Sys.VAR,
Bias.Sys.VAR = Bias.Sys.VAR,
Var.Sys.VAR = Var.Sys.VAR))}