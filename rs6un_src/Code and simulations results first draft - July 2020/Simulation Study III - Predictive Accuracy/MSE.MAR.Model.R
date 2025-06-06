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

#plan(multiprocess)
MSE.k = lapply(1:fold, function(k) MSE.MAR.fold(data.list,k,Model.type,y,y.lag,X.lag))

MSE.k = unlist(MSE.k)
MSE.MAR = mean(MSE.k)
MSE.MAR.sd = sd(MSE.k)
MSE.MAR.se = sd(MSE.k)/sqrt(fold)
MSE.MAR.1se = MSE.MAR + MSE.MAR.se

return(list(MSE.k=MSE.k,MSE.MAR=MSE.MAR,MSE.MAR.1se=MSE.MAR.1se,MSE.MAR.sd=MSE.MAR.sd,MSE.MAR.se=MSE.MAR.se))}