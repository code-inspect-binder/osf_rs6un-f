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

MSE.PC.VAR.fold = function(data.list,k,nComp){

n.ID = unique(data.list[[3]])

data.train = data.list[[1]][[k]]
data.test = data.list[[2]][[k]]

# Loading matrix
B = data.list[[5]][[k]]
P = nrow(B[[1]])

names.Y = sprintf("Y%d",seq(1:P))
names.F = sprintf("F%d",seq(1:nComp))
names.F.lag = sprintf("F%d_1",seq(1:nComp))

MSE.PC.ki = list()
MSE.ki = list()
F.hat.test = list()
beta1.i = list()

for (i in 1:length(n.ID)){

n.i = which(data.train$ID==i)

data.train.i = data.train[n.i,]

# VAR(1) analysis on rotated scores

fit = lsfit(head(data.train.i[,names.F],-1),tail(data.train.i[,names.F],-1),intercept = FALSE)

beta1.i[[i]] = fit$coefficients

data.test.i = data.test[which(data.test$ID==i),]

F.hat.test[[i]] = as.matrix(data.test.i[,names.F.lag]) %*% t(beta1.i[[i]])
}

MSE.PC.ki = list()
if (nComp == 1){
MSE.PC.ki[[1]] = lapply(1:length(n.ID), function(i)
(data.test[which(data.test$ID==i),names.F[1]] - as.matrix(data.test[which(data.test$ID==i),names.F.lag]) %*% beta1.i[[i]])^2)
}
if (nComp > 1){
for (q in 1:nComp){
MSE.PC.ki[[q]] = lapply(1:length(n.ID), function(i)
(data.test[which(data.test$ID==i),names.F[q]] - as.matrix(data.test[which(data.test$ID==i),names.F.lag]) %*% beta1.i[[i]][q,])^2)
}}

MSE.PC.k = lapply(1:nComp, function(q) mean(unlist(MSE.PC.ki[[q]]),na.rm=TRUE))
names(MSE.PC.k) = sprintf("F%d",seq(1:nComp))

MSE.ki = list()
for (p in 1:P){
MSE.ki[[p]] = lapply(1:length(n.ID), function(i)
(data.test[which(data.test$ID==i),names.Y[p]] - F.hat.test[[i]]%*%B[[i]][p,])^2)
}

MSE.k = lapply(1:P, function(p) mean(unlist(MSE.ki[[p]]),na.rm=TRUE))
names(MSE.k) = sprintf("Y%d",seq(1:P))

return(list(MSE.PC.k=MSE.PC.k,MSE.k=MSE.k))}