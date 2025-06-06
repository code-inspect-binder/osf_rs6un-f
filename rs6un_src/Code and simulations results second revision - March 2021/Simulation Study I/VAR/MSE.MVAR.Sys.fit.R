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


MSE.MVAR.Sys.fit = function(data,P,Model.type){
  
n.ID = unique(data$ID)
  
# List of predictors
y.list = lapply(1:P, function(j) sprintf("Y%d",j))
y.lag.list = lapply(1:P, function(j) sprintf("Y%d_1",j))
X.lag.list = lapply(1:P, function(j) sprintf("Y%d_1",seq(1:P)[-j]))

MVAR.fit.Sys = lapply(1:P, function(j) 
MVAR.Model.fit(data,Model.type,y.list[[j]],y.lag.list[[j]],X.lag.list[[j]]))

# Psi hat fixed effects
Psi.hat = lapply(1:P, function(j) coef(summary(MVAR.fit.Sys[[j]][[2]]))[,"Estimate"])

# Psi confidence interval
Psi.CI.hat = lapply(1:P, function(j) confint(MVAR.fit.Sys[[j]][[2]],
par="beta_",method='Wald'))

# Beta person-specific effects
beta0.i = lapply(1:P, function(j) coef(MVAR.fit.Sys[[j]][[2]])[[1]][,1])
beta1.i = lapply(1:P, function(j) coef(MVAR.fit.Sys[[j]][[2]])[[1]][,-1])

# Variance of level-1 errors
eps.sd.i = lapply(1:P, function(j) VarCorr(MVAR.fit.Sys[[j]][[2]]))

resid.i = lapply(1:P, function(j) residuals(MVAR.fit.Sys[[j]][[2]]))
  
MSE.i = list()
MSE.Mean.i = list()

for (j in 1:P){  

MSE.i[[j]] = list()
MSE.Mean.i[[j]] = list()

for (i in 1:length(n.ID)){
    
n.i = n.ID[i]
    
data.i = data[which(data$ID==n.i),]

if (Model.type == 1 || Model.type == 2){  
MSE.i[[j]][[i]] = (data.i[,y.list[[j]]] - beta0.i[[j]][i]- 
as.matrix(data.i[,c(y.lag.list[[j]])]) %*% as.matrix(beta1.i[[j]])[i,])^2
}

if (Model.type == 3 || Model.type == 4){  
MSE.i[[j]][[i]] = (data.i[,y.list[[j]]] - beta0.i[[j]][i]- 
as.matrix(data.i[,c(y.lag.list[[j]],X.lag.list[[j]])]) %*% as.matrix(beta1.i[[j]])[i,])^2
}
    
MSE.Mean.i[[j]][[i]] = (data.i[,y.list[[j]]] - mean(data.i[,y.list[[j]]]))^2
}}

# Compute MSE 

MSE.MVAR = lapply(1:P, function(j) mean(unlist(MSE.i[[j]]),na.rm=TRUE))

# Compute R square
R2.Sys = lapply(1:P, function(j) sum(unlist(MSE.i[[j]]),na.rm=T))
R2.Sys.Mean = lapply(1:P, function(j) sum(unlist(MSE.Mean.i[[j]])))
R2.Sys.MVAR = lapply(1:P, function(j) 1 - (R2.Sys[[j]]/R2.Sys.Mean[[j]]))
  
return(list(Psi.hat=Psi.hat,Psi.CI.hat=Psi.CI.hat,beta0.i=beta0.i,beta1.i=beta1.i,
MSE.MVAR=MSE.MVAR,R2.Sys.MVAR=R2.Sys.MVAR,eps.sd.i=eps.sd.i,resid.i=resid.i))}



