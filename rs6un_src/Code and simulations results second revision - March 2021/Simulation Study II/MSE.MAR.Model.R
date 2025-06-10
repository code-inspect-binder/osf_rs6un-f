#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

#################################################################
#################################################################
#################################################################
###           Predictive Accuracy Multilvel AR(1)             ###
#################################################################
#################################################################
#################################################################

# Function to estimate the predictive accuracy of multilevel AR(1) by performing k-block cross-validation
# k-block CV devides the data set in 10 parts and then perform cross-validation

# This function input is the trainning and testing set, and the names of
# the outcome variable, lagged-outcome variable and lagged predictors
# The output are the predictive accuracy measures

MSE.MAR.Model = function(data.list,Model.type,y,y.lag,X.lag){

n.ID = unique(data.list[[3]])

fold = data.list[[4]]

# Estimate Multilevel VAR(1) Model

MSE.k = lapply(1:fold, function(k) MSE.MAR.fold(data.list,k,Model.type,y,y.lag,X.lag))

# Estimated regression coefficients 
beta.hat = lapply(1:fold, function(k) MSE.k[[k]]$beta.i)

# Compute MSE
MSE.list = unlist(lapply(1:fold, function(k) MSE.k[[k]]$MSE.k))
MSE.MAR = mean(MSE.list)
MSE.MAR.sd = sd(MSE.list)
MSE.MAR.se = sd(MSE.list)/sqrt(fold)
MSE.MAR.1se = MSE.MAR + MSE.MAR.se

# MSE for each participant
MSE.Sys.MAR.i = lapply(1:fold, function(k) MSE.k[[k]]$MSE.ki)

# Compute R square
R2.Sys.k = sum(unlist(lapply(1:fold, function(k) sum(unlist(MSE.k[[k]]$MSE.ki),na.rm=T))))
R2.Sys.Mean.k = sum(unlist(lapply(1:fold, function(k) sum(unlist(MSE.k[[k]]$MSE.Mean.ki)))))
R2.Sys.MAR = 1 - (R2.Sys.k/R2.Sys.Mean.k)

# Compute Bias
Bias.list = unlist(lapply(1:fold, function(k) MSE.k[[k]]$Bias.k))
Bias.MAR = mean(Bias.list)

# Compute Variance
Var.list = unlist(lapply(1:fold, function(k) MSE.k[[k]]$Var.k))
Var.MAR = mean(Var.list)

return(list(MSE.k=MSE.k,
MSE.MAR=MSE.MAR,
MSE.MAR.1se=MSE.MAR.1se,
MSE.MAR.sd=MSE.MAR.sd,
MSE.MAR.se=MSE.MAR.se,
beta.hat = beta.hat,
MSE.Sys.MAR.i = MSE.Sys.MAR.i,
R2.Sys.MAR = R2.Sys.MAR,
Bias.MAR = Bias.MAR,
Var.MAR = Var.MAR))}