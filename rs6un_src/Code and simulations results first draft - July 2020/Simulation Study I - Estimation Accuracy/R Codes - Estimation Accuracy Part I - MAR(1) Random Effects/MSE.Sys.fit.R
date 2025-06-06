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

MSE.Sys.fit = function(data,P,Psi.true.i){

MSE.MAR.Fixed = MSE.MAR.fit(data,P,Model.type=1,Psi.true.i)
MSE.MAR.Random = MSE.MAR.fit(data,P,Model.type=2,Psi.true.i)
MSE.MVAR.Fixed = MSE.MVAR.fit(data,P,Model.type=3,Psi.true.i)
MSE.MVAR.Random = MSE.MVAR.fit(data,P,Model.type=4,Psi.true.i)

return(list(MSE.MAR.Fixed=MSE.MAR.Fixed,MSE.MAR.Random=MSE.MAR.Random,
MSE.MVAR.Fixed=MSE.MVAR.Fixed,MSE.MVAR.Random=MSE.MVAR.Random))}