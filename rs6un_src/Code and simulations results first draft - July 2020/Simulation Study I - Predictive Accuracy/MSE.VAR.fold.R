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

MSE.VAR.fold = function(data.list,P,k){

n.ID = unique(data.list[[3]])

data.train = data.list[[1]][[k]]
data.test = data.list[[2]][[k]]

names.Y = sprintf("Y%d",seq(1:P))
names.Y.lag = sprintf("Y%d_1",seq(1:P))

MSE.ki = list()
beta1.i = list()

for (p in 1:P){

MSE.ki[[p]] = list()

for (i in 1:length(n.ID)){

n.i = which(data.train$ID==i)

data.train.i = data.train[n.i,]

# AR(1) analysis on original dataset

Model = as.formula(paste(names.Y[p],paste(names.Y.lag, collapse = " + "),sep = " ~ "))

fit = coefficients(lm(Model,data.train.i))

b0 = fit[1]
b1 = as.matrix(fit[2:(length(names.Y.lag)+1)])

MSE.ki[[p]][[i]] = (data.test[which(data.test$ID==i),names.Y[p]] - b0 - as.matrix(data.test[which(data.test$ID==i),names.Y.lag]) %*% b1)^2
}}

MSE.k = lapply(1:P, function(p) mean(unlist(MSE.ki[[p]]),na.rm=TRUE))
names(MSE.k) = sprintf("Y%d",seq(1:P))

return(list(MSE.k=MSE.k))}