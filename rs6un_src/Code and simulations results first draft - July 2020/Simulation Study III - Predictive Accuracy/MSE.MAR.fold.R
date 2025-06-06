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

MSE.MAR.fold = function(data.list,k,Model.type,y,y.lag,X.lag){

n.ID = unique(data.list[[3]])

data.train = data.list[[1]][[k]]
data.test = data.list[[2]][[k]]

fit = MVAR.Model(data.train,Model.type,y,y.lag,X.lag)

beta0.i = coef(fit[[2]])[[1]][,1]
beta1.i = coef(fit[[2]])[[1]][,2]

MSE.ki = list()

for (i in 1:length(n.ID)){

n.i = n.ID[i]

data.test.i = data.test[which(data.test$ID==n.i),]

MSE.ki[[i]] = (data.test.i[,y] - beta0.i[i]
- beta1.i[i]*data.test.i[,y.lag])^2
}

MSE.k = mean(unlist(MSE.ki),na.rm=TRUE)

return(MSE.k)}