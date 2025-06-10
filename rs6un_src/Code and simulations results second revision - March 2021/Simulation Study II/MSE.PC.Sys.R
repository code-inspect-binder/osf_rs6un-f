#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

#################################################################
#################################################################
#################################################################
###          Predictive Accuracy PC-VAR(1) (System)           ###
#################################################################
#################################################################
#################################################################

# Function to estimate the predictive accuracy of multilevel AR(1) by performing k-block cross-validation
# k-block CV devides the data set in 10 parts and then perform cross-validation

# This function input is the trainning and testing set, and the names of
# the outcome variable, lagged-outcome variable and lagged predictors
# The output are the predictive accuracy measures

MSE.PC.Sys = function(data.list,nComp,Method.type){

n.ID = unique(data.list[[3]])

fold = data.list[[4]]

# Loading matrix
P = nrow(data.list[[5]][[1]][[1]])

if (Method.type==1){
# Estimate Person Specific VAR(1) Model
MSE.k = lapply(1:fold, function(k) MSE.PC.VAR.fold(data.list,k,nComp))
}

if (Method.type==2){
# Estimate Multilevel AR(1) Model with fixed effects
MSE.k = lapply(1:fold, function(k) MSE.PC.MAR.fold(data.list,k,nComp,Model.type=1))
}

if (Method.type==3){
# Estimate Multilevel AR(1) Model with random effects
MSE.k = lapply(1:fold, function(k) MSE.PC.MAR.fold(data.list,k,nComp,Model.type=2))
}

if (Method.type==4){
# Estimate Multilevel VAR(1) Model with fixed effects 
MSE.k = lapply(1:fold, function(k) MSE.PC.MVAR.fold(data.list,k,nComp,Model.type=3))
}

if (Method.type==5){
# Estimate Multilevel VAR(1) Model with random effects
MSE.k = lapply(1:fold, function(k) MSE.PC.MVAR.fold(data.list,k,nComp,Model.type=4))
}

MSE.PC.k = list()
MSE.PC.MVAR = list()
MSE.PC.MVAR.sd = list()
MSE.PC.MVAR.se = list()
MSE.PC.MVAR.1se = list()
for (q in 1:nComp){
MSE.PC.k[[q]] = unlist(lapply(1:fold, function(k) MSE.k[[k]]$MSE.PC.k[[q]]))
MSE.PC.MVAR[[q]] = mean(MSE.PC.k[[q]])
MSE.PC.MVAR.sd[[q]] = sd(MSE.PC.k[[q]])
MSE.PC.MVAR.se[[q]] = sd(MSE.PC.k[[q]])/sqrt(fold)
MSE.PC.MVAR.1se[[q]] = MSE.PC.MVAR[[q]] + MSE.PC.MVAR.se[[q]]
}

names(MSE.PC.k) = sprintf("F%d",seq(1:nComp))
names(MSE.PC.MVAR) = sprintf("F%d",seq(1:nComp))
names(MSE.PC.MVAR.sd) = sprintf("F%d",seq(1:nComp))
names(MSE.PC.MVAR.se) = sprintf("F%d",seq(1:nComp))
names(MSE.PC.MVAR.1se) = sprintf("F%d",seq(1:nComp))

MSE.Sys.k = list()
MSE.Sys.MVAR = list()
MSE.Sys.MVAR.sd = list()
MSE.Sys.MVAR.se = list()
MSE.Sys.MVAR.1se = list()
for (p in 1:P){
MSE.Sys.k[[p]] = unlist(lapply(1:fold, function(k) MSE.k[[k]]$MSE.k[[p]]))
MSE.Sys.MVAR[[p]] = mean(MSE.Sys.k[[p]])
MSE.Sys.MVAR.sd[[p]] = sd(MSE.Sys.k[[p]])
MSE.Sys.MVAR.se[[p]] = sd(MSE.Sys.k[[p]])/sqrt(fold)
MSE.Sys.MVAR.1se[[p]] = MSE.Sys.MVAR[[p]] + MSE.Sys.MVAR.se[[p]]
}
names(MSE.Sys.k) = sprintf("Y%d",seq(1:P))
names(MSE.Sys.MVAR) = sprintf("Y%d",seq(1:P))
names(MSE.Sys.MVAR.sd) = sprintf("Y%d",seq(1:P))
names(MSE.Sys.MVAR.se) = sprintf("Y%d",seq(1:P))
names(MSE.Sys.MVAR.1se) = sprintf("Y%d",seq(1:P))

# Compute MSE
MSE.list = unlist(lapply(1:fold, function(k) MSE.k[[k]]$MSE.k))
MSE.MVAR = mean(MSE.list)
MSE.MVAR.sd = sd(MSE.list)
MSE.MVAR.se = sd(MSE.list)/sqrt(fold)
MSE.MVAR.1se = MSE.MVAR + MSE.MVAR.se

# Compute R square
R2.Sys.k = sum(unlist(lapply(1:fold, function(k) sum(unlist(MSE.k[[k]]$MSE.ki),na.rm=T))))
R2.Sys.Mean.k = sum(unlist(lapply(1:fold, function(k) sum(unlist(MSE.k[[k]]$MSE.Mean.ki)))))
R2.Sys.MVAR = 1 - (R2.Sys.k/R2.Sys.Mean.k)

# Compute Bias
Bias.list = unlist(lapply(1:fold, function(k) MSE.k[[k]]$Bias.k))
Bias.MVAR = mean(Bias.list)

# Compute Variance
Var.list = unlist(lapply(1:fold, function(k) MSE.k[[k]]$Var.k))
Var.MVAR = mean(Var.list)

return(list(MSE.PC.k = MSE.PC.k,
MSE.PC.MVAR = MSE.PC.MVAR,
MSE.PC.MVAR.sd = MSE.PC.MVAR.sd,
MSE.PC.MVAR.se = MSE.PC.MVAR.se,
MSE.PC.MVAR.1se = MSE.PC.MVAR.1se,
MSE.Sys.k = MSE.Sys.k,
MSE.Sys.MVAR = MSE.Sys.MVAR,
MSE.Sys.MVAR.sd = MSE.Sys.MVAR.sd,
MSE.Sys.MVAR.se = MSE.Sys.MVAR.se,
MSE.Sys.MVAR.1se = MSE.Sys.MVAR.1se,
R2.Sys.MVAR = R2.Sys.MVAR,
Bias.MVAR = Bias.MVAR,
Var.MVAR = Var.MVAR))}