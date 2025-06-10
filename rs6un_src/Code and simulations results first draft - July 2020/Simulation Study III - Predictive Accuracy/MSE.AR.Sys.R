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

MSE.AR.Sys = function(data.list,P){

n.ID = unique(data.list[[3]])

fold = data.list[[4]]

# Estimate Person specific VAR(1) Model
MSE.k = lapply(1:fold, function(k) MSE.AR.fold(data.list,P,k))

MSE.Sys.k = list()
MSE.Sys.AR = list()
MSE.Sys.AR.sd = list()
MSE.Sys.AR.se = list()
MSE.Sys.AR.1se = list()
for (p in 1:P){
MSE.Sys.k[[p]] = unlist(lapply(1:fold, function(k) MSE.k[[k]]$MSE.k[[p]]))
MSE.Sys.AR[[p]] = mean(MSE.Sys.k[[p]])
MSE.Sys.AR.sd[[p]] = sd(MSE.Sys.k[[p]])
MSE.Sys.AR.se[[p]] = sd(MSE.Sys.k[[p]])/sqrt(fold)
MSE.Sys.AR.1se[[p]] = MSE.Sys.AR[[p]] + MSE.Sys.AR.se[[p]]
}
names(MSE.Sys.k) = sprintf("Y%d",seq(1:P))
names(MSE.Sys.AR) = sprintf("Y%d",seq(1:P))
names(MSE.Sys.AR.sd) = sprintf("Y%d",seq(1:P))
names(MSE.Sys.AR.se) = sprintf("Y%d",seq(1:P))
names(MSE.Sys.AR.1se) = sprintf("Y%d",seq(1:P))

return(list(MSE.Sys.k = MSE.Sys.k,
MSE.Sys.AR = MSE.Sys.AR,
MSE.Sys.AR.sd = MSE.Sys.AR.sd,
MSE.Sys.AR.se = MSE.Sys.AR.se,
MSE.Sys.AR.1se = MSE.Sys.AR.1se))}