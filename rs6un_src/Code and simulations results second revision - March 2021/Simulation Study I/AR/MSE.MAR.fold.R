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

if (Model.type == 1){
beta0.i = fit[[2]][1] + fit[[3]]
beta1.i = matrix(fit[[2]][2],nrow(fit[[3]]),1)
}

if (Model.type == 2){
beta0.i = fit[[2]][1] + fit[[3]][,1]
beta1.i = fit[[2]][2] + fit[[3]][,2]
}

beta.i = cbind(beta0.i,beta1.i)

MSE.ki = list()
MSE.Mean.ki = list()
Bias.ki = list()
Var.ki = list()
yhat.ki = list()

for (i in 1:length(n.ID)){

n.i = n.ID[i]

data.test.i = data.test[which(data.test$ID==n.i),]

yhat.ki[[i]] = beta0.i[i] + beta1.i[i]*data.test.i[,y.lag]

MSE.ki[[i]] = (data.test.i[,y] - yhat.ki[[i]])^2
   
Bias.ki[[i]] = (data.test.i[,y] - mean(yhat.ki[[i]],na.rm=TRUE))^2

Var.ki[[i]] = (yhat.ki[[i]] - mean(yhat.ki[[i]],na.rm=TRUE))^2
 
MSE.Mean.ki[[i]] = (data.test.i[,y] - mean(data.test.i[,y]))^2
}
  
MSE.k = mean(unlist(MSE.ki),na.rm=TRUE)
Var.k = mean(unlist(Var.ki),na.rm=TRUE)
Bias.k = MSE.k - Var.k

return(list(MSE.k=MSE.k,beta.i=beta.i,MSE.ki=MSE.ki,MSE.Mean.ki=MSE.Mean.ki,yhat.ki=yhat.ki,
Var.k=Var.k,Bias.k=Bias.k))}



