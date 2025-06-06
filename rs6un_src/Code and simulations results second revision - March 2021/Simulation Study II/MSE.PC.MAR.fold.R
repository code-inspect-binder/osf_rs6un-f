#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

#################################################################
#################################################################
#################################################################
###                      MSE PC-AR(1)                        ###
#################################################################
#################################################################
#################################################################

# Function to estimate the predictive accuracy of multilevel AR(1) by performing k-block cross-validation
# k-block CV devides the data set in 10 parts and then perform cross-validation

# This function input is the trainning and testing set, and the names of
# the outcome variable, lagged-outcome variable and lagged predictors
# The output are the predictive accuracy measures

MSE.PC.MAR.fold = function(data.list,k,nComp,Model.type){

n.ID = unique(data.list[[3]])

data.train = data.list[[1]][[k]]
data.test = data.list[[2]][[k]]

# Loading matrix
B = data.list[[5]][[k]]
P = nrow(B[[1]])

names.Y = sprintf("Y%d",seq(1:P))
names.F = sprintf("F%d",seq(1:nComp))
names.F.lag = sprintf("F%d_1",seq(1:nComp))

beta.i = list()
MSE.PC.ki = list()
F.hat.test = data.frame(cbind(data.test$ID,matrix(0,nrow(data.test),nComp)))
colnames(F.hat.test) = c("ID",names.F)

# Multilvel VAR(1) analysis on rotated scores

fit = future_lapply(1:nComp, function(q)
MVAR.Model(data.train,Model.type,names.F[q],names.F.lag[q],names.F.lag[-q]),
future.seed = 0xBEEF)

for (q in 1:nComp){

MSE.PC.ki[[q]] = list()

if (Model.type == 1){
beta0.i = fit[[q]][[2]][1] + fit[[q]][[3]]
beta1.i = matrix(fit[[q]][[2]][2],nrow(fit[[q]][[3]]),1)
}

if (Model.type == 2){
beta0.i = fit[[q]][[2]][1] + fit[[q]][[3]][,1]
beta1.i = fit[[q]][[2]][2] + fit[[q]][[3]][,2]
}

beta.i[[q]] = cbind(beta0.i,beta1.i)

for (i in 1:length(n.ID)){

n.i = n.ID[i]

data.test.i = data.test[which(data.test$ID==n.i),]

MSE.PC.ki[[q]][[i]] = (data.test.i[,names.F[q]] - beta0.i[i]
- as.matrix(data.test.i[,names.F.lag[q]]) %*% t(as.matrix(beta1.i[i])))^2

F.hat.test[which(F.hat.test$ID==n.i),names.F[q]] = as.matrix(data.test.i[,names.F.lag[q]]) %*% t(as.matrix(beta1.i[i]))
}}

MSE.PC.k = lapply(1:nComp, function(q) mean(unlist(MSE.PC.ki[[q]]),na.rm=TRUE))
names(MSE.PC.k) = sprintf("F%d",seq(1:nComp))

MSE.ki = list()
MSE.Mean.ki = list()
Bias.ki = list()
Var.ki = list()
yhat.ki = list()

for (p in 1:P){

yhat.ki[[p]] = lapply(1:length(n.ID), function(i)
as.matrix(F.hat.test[which(F.hat.test$ID==i),names.F])%*%B[[i]][p,])

MSE.ki[[p]] = lapply(1:length(n.ID), function(i)
(data.test[which(data.test$ID==i),names.Y[p]] - 
as.matrix(F.hat.test[which(F.hat.test$ID==i),names.F])%*%B[[i]][p,])^2)

Bias.ki[[p]] = lapply(1:length(n.ID), function(i)
(data.test[which(data.test$ID==i),names.Y[p]] - 
mean(as.matrix(F.hat.test[which(F.hat.test$ID==i),names.F])%*%B[[i]][p,],na.rm=TRUE))^2)

Var.ki[[p]] = lapply(1:length(n.ID), function(i)
(as.matrix(F.hat.test[which(F.hat.test$ID==i),names.F])%*%B[[i]][p,] -
mean(as.matrix(F.hat.test[which(F.hat.test$ID==i),names.F])%*%B[[i]][p,],na.rm=TRUE))^2)
 
MSE.Mean.ki[[p]] = lapply(1:length(n.ID), function(i)
(data.test[which(data.test$ID==i),names.Y[p]] - mean(data.test[which(data.test$ID==i),names.Y[p]],na.rm=TRUE))^2)
}

MSE.k = lapply(1:P, function(p) mean(unlist(MSE.ki[[p]]),na.rm=TRUE))
names(MSE.k) = sprintf("Y%d",seq(1:P))
Var.k = lapply(1:P, function(p) mean(unlist(Var.ki[[p]]),na.rm=TRUE))
names(MSE.k) = sprintf("Y%d",seq(1:P))
Bias.k = lapply(1:P, function(p) MSE.k[[p]] - Var.k[[p]])
names(MSE.k) = sprintf("Y%d",seq(1:P))

return(list(MSE.PC.k=MSE.PC.k,MSE.k=MSE.k,beta.i=beta.i,MSE.ki=MSE.ki,MSE.Mean.ki=MSE.Mean.ki,yhat.ki=yhat.ki,
Var.k=Var.k,Bias.k=Bias.k))}
