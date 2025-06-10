#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

#################################################################
#################################################################
#################################################################
###     Predictive Accuracy Multilvel AR(1) (System)          ###
#################################################################
#################################################################
#################################################################

# Function to estimate the predictive accuracy of multilevel AR(1) by performing k-block cross-validation
# k-block CV devides the data set in 10 parts and then perform cross-validation

# This function input is the trainning and testing set, and the names of
# the outcome variable, lagged-outcome variable and lagged predictors
# The output are the predictive accuracy measures

MSE.MAR.Sys = function(data, p, Model.type){

# List of predictors
y.list = lapply(1:p, function(j) sprintf("Y%d",j))
y.lag.list = lapply(1:p, function(j) sprintf("Y%d_1",j))
X.lag.list = lapply(1:p, function(j) sprintf("Y%d_1",seq(1:p)[-j]))

MSE.MAR.Sys = lapply(1:p, function(j) MSE.MAR.Model(data,Model.type,y.list[[j]],y.lag.list[[j]],X.lag.list[[j]]))

return(MSE.MAR.Sys)}