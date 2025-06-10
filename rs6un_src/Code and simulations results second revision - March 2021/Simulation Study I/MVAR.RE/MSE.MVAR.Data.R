#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

#################################################################
#################################################################
#################################################################
###                 MSE Multilvel VAR(1)                      ###
#################################################################
#################################################################
#################################################################

# Function to estimate the predictive accuracy of multilevel AR(1) by performing k-block cross-validation
# k-block CV devides the data set in 10 parts and then perform cross-validation

# This function input is the trainning and testing set, and the names of
# the outcome variable, lagged-outcome variable and lagged predictors
# The output are the predictive accuracy measures

MSE.MVAR.Data = function(data.list,k,Model.type,y,y.lag,X.lag){
  
n.ID = unique(data.list[[3]])
  
data.train = data.list[[1]][[k]]
data.test = data.list[[2]][[k]]
  
fit = MVAR.Model(data.train,Model.type,y,y.lag,X.lag)
  
if (Model.type == 3){
beta0.i = as.matrix(fit[[2]][1] + fit[[3]])
beta1.i = matrix(rep(fit[[2]][2:length(fit[[2]])],
          each=nrow(fit[[3]])), nrow=nrow(fit[[3]]), byrow=FALSE)
}
  
if (Model.type == 4){
beta0.i = as.matrix(fit[[2]][1] + fit[[3]][,1])
beta1.fixed = matrix(rep(fit[[2]][2:length(fit[[2]])],
             each=nrow(fit[[3]])), nrow=nrow(fit[[3]]), byrow=FALSE)
beta1.i = as.matrix(beta1.fixed + fit[[3]][,2:length(fit[[2]])])
}
  
beta.i = cbind(beta0.i,beta1.i)
 
MSE.ki = list()
MSE.Mean.ki = list()
  
for (i in 1:length(n.ID)){
    
n.i = n.ID[i]
    
data.test.i = data.test[which(data.test$ID==n.i),]
    
MSE.ki[[i]] = (data.test.i[,y] - beta0.i[i]
              - as.matrix(data.test.i[,c(y.lag,X.lag)]) %*% as.matrix(beta1.i[i,]))^2
    
MSE.Mean.ki[[i]] = (data.test.i[,y] - mean(data.test.i[,y]))^2
}
  
MSE.k = mean(unlist(MSE.ki),na.rm=TRUE)
  
return(list(MSE.k=MSE.k,beta.i=beta.i,MSE.ki=MSE.ki,MSE.Mean.ki=MSE.Mean.ki))}