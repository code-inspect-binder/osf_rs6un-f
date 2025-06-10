#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

#################################################################
#################################################################
#################################################################
###                      MSE PC-VAR(1)                        ###
#################################################################
#################################################################
#################################################################

# Function to estimate the predictive accuracy of multilevel AR(1) by performing k-block cross-validation
# k-block CV devides the data set in 10 parts and then perform cross-validation

# This function input is the trainning and testing set, and the names of
# the outcome variable, lagged-outcome variable and lagged predictors
# The output are the predictive accuracy measures

MSE.VAR.fit = function(data,P,Psi.true.i){

n.ID = unique(data$ID)

names.Y = sprintf("Y%d",seq(1:P))
names.Y.lag = sprintf("Y%d_1",seq(1:P))

MSE.i = list()
Psi.hat.i = list()

for (i in 1:length(n.ID)){

Psi.hat.i[[i]] = matrix(0,P,P)

for (p in 1:P){

n.i = which(data$ID==i)

data.i = data[n.i,]

# AR(1) analysis on original dataset

Model = as.formula(paste(names.Y[p],paste(names.Y.lag, collapse = " + "),sep = " ~ "))

fit = coefficients(lm(Model,data.i))

Psi.hat.i[[i]][p,] = as.matrix(fit[names.Y.lag])
}

MSE.i[[i]] = (Psi.hat.i[[i]]-Psi.true.i[[i]])^2
}

MSE = mean(unlist(lapply(n.ID, function(i) mean(MSE.i[[i]],na.rm=TRUE))))

return(list(MSE=MSE))}
