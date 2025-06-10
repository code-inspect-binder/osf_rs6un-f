#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

#################################################################
#################################################################
#################################################################
###     Predictive Accuracy Multilvel VAR(1) & AR(1)          ###
#################################################################
#################################################################
#################################################################

# Function to estimate the predictive accuracy of multilevel AR(1) by performing k-block cross-validation
# k-block CV devides the data set in 10 parts and then perform cross-validation

# This function input is the trainning and testing set, and the names of
# the outcome variable, lagged-outcome variable and lagged predictors
# The output are the predictive accuracy measures

MSE.Sys = function(data, p){

MSE.MAR.Fixed = MSE.MAR.Sys(data,p,Model.type=1)
MSE.MAR.Random = MSE.MAR.Sys(data,p,Model.type=2)
MSE.MVAR.Fixed = MSE.MVAR.Sys(data,p,Model.type=3)
MSE.MVAR.Random = MSE.MVAR.Sys(data,p,Model.type=4)

return(list(MSE.MAR.Fixed=MSE.MAR.Fixed,MSE.MAR.Random=MSE.MAR.Random,
MSE.MVAR.Fixed=MSE.MVAR.Fixed,MSE.MVAR.Random=MSE.MVAR.Random))}