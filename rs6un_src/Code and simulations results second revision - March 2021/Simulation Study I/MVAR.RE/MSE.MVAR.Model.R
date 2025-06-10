#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

#################################################################
#################################################################
#################################################################
###           Predictive Accuracy Multilvel VAR(1)            ###
#################################################################
#################################################################
#################################################################

# Function to estimate the predictive accuracy of multilevel VAR(1) by performing k-block cross-validation
# k-block CV devides the data set in 10 parts and then perform cross-validation

# This function input is the trainning and testing set, and the names of
# the outcome variable, lagged-outcome variable and lagged predictors
# The output are the predictive accuracy measures

MSE.MVAR.Model = function(data.list,Model.type,y,y.lag,X.lag){

n.ID = unique(data.list[[3]])

fold = data.list[[4]]

# Estimate Multilevel VAR(1) Model

MSE.k = lapply(1:fold, function(k) MSE.MVAR.fold(data.list,k,Model.type,y,y.lag,X.lag))

# Estimated regression coefficients 
beta.hat = lapply(1:fold, function(k) MSE.k[[k]]$beta.i)

# Compute MSE
MSE.list = unlist(lapply(1:fold, function(k) MSE.k[[k]]$MSE.k))
MSE.MVAR = mean(MSE.list)
MSE.MVAR.sd = sd(MSE.list)
MSE.MVAR.se = sd(MSE.list)/sqrt(fold)
MSE.MVAR.1se = MSE.MVAR + MSE.MVAR.se

# MSE for each participant
MSE.Sys.MVAR.i = lapply(1:fold, function(k) MSE.k[[k]]$MSE.ki)

# Compute R square
R2.Sys.k = sum(unlist(lapply(1:fold, function(k) sum(unlist(MSE.k[[k]]$MSE.ki),na.rm=T))))
R2.Sys.Mean.k = sum(unlist(lapply(1:fold, function(k) sum(unlist(MSE.k[[k]]$MSE.Mean.ki)))))
R2.Sys.MVAR = 1 - (R2.Sys.k/R2.Sys.Mean.k)

# Compute Bias
Bias.list = unlist(lapply(1:fold, function(k) MSE.k[[k]]$Bias.k))
Bias.MVAR = mean(Bias.list)

# Compute Variance
Var.list = unlist(lapply(1:fold, function(k) MSE.k[[k]]$Var.k))
Var.MVAR = mean(Var.list)

return(list(MSE.k=MSE.k,
MSE.MVAR=MSE.MVAR,
MSE.MVAR.1se=MSE.MVAR.1se,
MSE.MVAR.sd=MSE.MVAR.sd,
MSE.MVAR.se=MSE.MVAR.se,
beta.hat = beta.hat,
MSE.Sys.MVAR.i = MSE.Sys.MVAR.i,
R2.Sys.MVAR = R2.Sys.MVAR,
Bias.MVAR = Bias.MVAR,
Var.MVAR = Var.MVAR))}

