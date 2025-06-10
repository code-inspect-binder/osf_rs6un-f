#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

#################################################################
#################################################################
#################################################################
###                  MSE Multilvel AR(1)                      ###
#################################################################
#################################################################
#################################################################

# Function to estimate the predictive accuracy of multilevel AR(1) by performing k-block cross-validation
# k-block CV devides the data set in 10 parts and then perform cross-validation

# This function input is the trainning and testing set, and the names of
# the outcome variable, lagged-outcome variable and lagged predictors
# The output are the predictive accuracy measures

MSE.MAR.fit = function(data,P,Model.type,Psi.true.i){

n.ID = unique(data$ID)

names.Y = sprintf("Y%d",seq(1:P))
names.Y.lag = sprintf("Y%d_1",seq(1:P))

beta1.i = list()

for (p in 1:P){
fit = MVAR.Model(data,Model.type,names.Y[p],names.Y.lag[p],X.lag)

beta1.i[[p]] = coef(fit[[2]])[[1]][,2]
}

MSE.i = list()
Psi.hat.i = list()

for (i in 1:length(n.ID)){

n.i = n.ID[i]

Psi.hat.i[[i]] = matrix(0,P,P)

for (p in 1:P){
Psi.hat.i[[i]][p,p] = beta1.i[[p]][i] 
}

MSE.i[[i]] = (Psi.hat.i[[i]]-Psi.true.i[[i]])^2
}

MSE = mean(unlist(lapply(n.ID, function(i) mean(MSE.i[[i]],na.rm=TRUE))))

return(MSE)}