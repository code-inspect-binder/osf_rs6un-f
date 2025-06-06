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

MSE.AR.fold = function(data.list,P,k){

n.ID = unique(data.list[[3]])

data.train = data.list[[1]][[k]]
data.test = data.list[[2]][[k]]

names.Y = sprintf("Y%d",seq(1:P))
names.Y.lag = sprintf("Y%d_1",seq(1:P))

MSE.ki = list()
MSE.Mean.ki = list()
beta.i = list()
Bias.ki = list()
Var.ki = list()
yhat.ki = list()

for (p in 1:P){

MSE.ki[[p]] = list()
MSE.Mean.ki[[p]] = list()
beta.i[[p]] = list()
Bias.ki[[p]] = list()
Var.ki[[p]] = list()
yhat.ki[[p]] = list()

for (i in 1:length(n.ID)){

n.i = which(data.train$ID==i)

data.train.i = data.train[n.i,]

# AR(1) analysis on original dataset

Model = as.formula(paste(names.Y[p],"~",names.Y.lag[p],sep=""))

fit = coefficients(lm(Model,data.train.i))

b0 = fit[1]
b1 = fit[2]

beta.i[[p]][[i]] = rbind(b0,b1)

yhat.ki[[p]][[i]] = b0 + as.matrix(data.test[which(data.test$ID==i),names.Y.lag[p]]) %*% b1

MSE.ki[[p]][[i]] = (data.test[which(data.test$ID==i),names.Y[p]] - b0 - as.matrix(data.test[which(data.test$ID==i),names.Y.lag[p]]) %*% b1)^2
MSE.Mean.ki[[p]][[i]] = (data.test[which(data.test$ID==i),names.Y[p]] - mean(data.test[which(data.test$ID==i),names.Y[p]]))^2

Bias.ki[[p]][[i]] = (data.test[which(data.test$ID==i),names.Y[p]] - mean(yhat.ki[[p]][[i]],na.rm=TRUE))^2

Var.ki[[p]][[i]] = (yhat.ki[[p]][[i]] - mean(yhat.ki[[p]][[i]],na.rm=TRUE))^2
}}

MSE.k = lapply(1:P, function(p) mean(unlist(MSE.ki[[p]]),na.rm=TRUE))
names(MSE.k) = sprintf("Y%d",seq(1:P))
Var.k = lapply(1:P, function(p) mean(unlist(Var.ki[[p]]),na.rm=TRUE))
names(Var.k) = sprintf("Y%d",seq(1:P))
Bias.k = lapply(1:P, function(p) unlist(MSE.k[p]) - unlist(Var.k[p]))
names(Bias.k) = sprintf("Y%d",seq(1:P))

return(list(MSE.k=MSE.k,beta.i=beta.i,MSE.ki=MSE.ki,MSE.Mean.ki=MSE.Mean.ki,yhat.ki=yhat.ki,
Var.k=Var.k,Bias.k=Bias.k))}




