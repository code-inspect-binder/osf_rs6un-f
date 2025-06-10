#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

#################################################################
#################################################################
#################################################################
##########     k-block cross-validation for PCA      ############
#################################################################
#################################################################
#################################################################

# Function Perform k-block cross-validation
# k-block CV devides the data set in 10 parts and then perform cross-validation

# This function creates (fold-1) data sets to perform k-block CV
# The output is divided in trainning and testing set for each (fold-1) data-blocks

data.block.PC = function(data,nComp,fold){ 

data.train = data[[3]]

data.test = data[[4]]

data.ID = data[[5]]

n.ID = unique(data.ID)

# Estimate simulateneous principal components for the training set 
Brot = list()
PCA.varimax = list()

for (k in 1:fold){
Brot[[k]] = list()
data.i = data.train[[k]][,3:ncol(data.train[[k]])]
PCA.varimax[[k]] = principal(data.i, nfactors = nComp, rotate="varimax")
F = PCA.varimax[[k]]$scores
F = as.matrix(F)
colnames(F) = sprintf("F%d",seq(1:nComp))
Bload = PCA.varimax[[k]]$loadings
Brot[[k]] = lapply(1:length(n.ID), function(i) Bload)
data.train[[k]] = cbind(data.train[[k]],F)
}

# Estimate the principal components for the testing set 

for (k in 1:fold){
data.i = data.test[[k]][,3:ncol(data.test[[k]])]
invLoadings = as.matrix(t(pracma::pinv(Brot[[k]][[1]])))
F = as.matrix(data.i) %*% invLoadings
F = as.matrix(F)
colnames(F) = sprintf("F%d",seq(1:nComp))
data.test[[k]] = cbind(data.test[[k]],F)
}

# Compute lagged predictors

for (k in 1:fold){
data.test[[k]] = data.prep(data.test[[k]])
data.train[[k]] = data.prep(data.train[[k]])
}
 
return(list(data.train,data.test,data.ID,fold,Brot,PCA.varimax))}
