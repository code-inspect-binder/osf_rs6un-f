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

#plan(multiprocess)
MSE.k = lapply(1:fold, function(k) MSE.MVAR.fold(data.list,k,Model.type,y,y.lag,X.lag))

MSE.k = unlist(MSE.k)
MSE.MVAR = mean(MSE.k)
MSE.MVAR.sd = sd(MSE.k)
MSE.MVAR.se = sd(MSE.k)/sqrt(fold)
MSE.MVAR.1se = MSE.MVAR + MSE.MVAR.se

return(list(MSE.k=MSE.k,MSE.MVAR=MSE.MVAR,MSE.MVAR.1se=MSE.MVAR.1se,MSE.MVAR.sd=MSE.MVAR.sd,MSE.MVAR.se=MSE.MVAR.se))}

