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

MSE.Sys.k = list()
MSE.Sys.MVAR = list()
MSE.Sys.MVAR.sd = list()
MSE.Sys.MVAR.se = list()
MSE.Sys.MVAR.1se = list()
for (p in 1:P){
MSE.Sys.k[[p]] = unlist(lapply(1:fold, function(k) MSE.k[[k]]$MSE.k[[p]]))
MSE.Sys.MVAR[[p]] = mean(MSE.Sys.k[[p]])
MSE.Sys.MVAR.sd[[p]] = sd(MSE.Sys.k[[p]])
MSE.Sys.MVAR.se[[p]] = sd(MSE.Sys.k[[p]])/sqrt(fold)
MSE.Sys.MVAR.1se[[p]] = MSE.Sys.MVAR[[p]] + MSE.Sys.MVAR.se[[p]]
}
names(MSE.Sys.k) = sprintf("Y%d",seq(1:P))
names(MSE.Sys.MVAR) = sprintf("Y%d",seq(1:P))
names(MSE.Sys.MVAR.sd) = sprintf("Y%d",seq(1:P))
names(MSE.Sys.MVAR.se) = sprintf("Y%d",seq(1:P))
names(MSE.Sys.MVAR.1se) = sprintf("Y%d",seq(1:P))

return(list(MSE.Sys.k = MSE.Sys.k,
MSE.Sys.MVAR = MSE.Sys.MVAR,
MSE.Sys.MVAR.sd = MSE.Sys.MVAR.sd,
MSE.Sys.MVAR.se = MSE.Sys.MVAR.se,
MSE.Sys.MVAR.1se = MSE.Sys.MVAR.1se))}