#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

#################################################################
#################################################################
#################################################################
##########         k-block cross-validation          ############
#################################################################
#################################################################
#################################################################

# Function Perform k-block cross-validation
# k-block CV devides the data set in 10 parts and then perform cross-validation

# This function creates (fold-1) data sets to perform k-block CV
# The output is divided in trainning and testing set for each (fold-1) data-blocks

data.block = function(data,fold){ 

n.ID = unique(data$ID)

data.ID = data$ID

P = ncol(data)-2

X.test = list()
X.train = list()

data.test = list()
data.train = list()

for (k in 1:fold){
X.test[[k]] = list()
X.train[[k]] = list()
for (i in 1:length(n.ID)){
i.id = n.ID[i]
T.i = which(data$ID == i.id)
part = 1/fold
L = length(T.i)
X.test[[k]][[i]] = T.i[round((k-1)*part*L+1):round(k*part*L)]
X.train[[k]][[i]] = T.i[-(round((k-1)*part*L+1):round(k*part*L))]
}}

for (k in 1:fold){
data.test[[k]] = data[unlist(X.test[[k]]),]
data.train[[k]] = data[unlist(X.train[[k]]),]
}

# Scale using the person's mean and the group variance computed from the training set

mean.list.train = list()
mean.list.test = list()
sd.list = list()

data.test.c = data.test
data.train.c = data.train

for (k in 1:fold){
mean.list.train[[k]] = matrix(0,nrow(data.train[[k]]),P)
colnames(mean.list.train[[k]]) = sprintf("Y%d_mean",1:P)
mean.list.test[[k]] = matrix(0,nrow(data.test[[k]]),P)
colnames(mean.list.test[[k]]) = sprintf("Y%d_mean",1:P)
sd.list[[k]] = rep(0,P)
names(sd.list[[k]]) = sprintf("Y%d_sd",1:P)
for (p in 1:P){
y = sprintf("Y%d",p)
y.stats = data.mean(data.train[[k]],data.test[[k]],sprintf("Y%d",p))
mean.list.train[[k]][,p] = y.stats[[1]]
mean.list.test[[k]][,p] = y.stats[[2]]
sd.list[[k]][p] = y.stats[[3]]
data.train.c[[k]][,y] = (data.train[[k]][,y]-mean.list.train[[k]][,p])/sd.list[[k]][p] 
data.test.c[[k]][,y] = (data.test[[k]][,y]-mean.list.test[[k]][,p])/sd.list[[k]][p] 
}}

# Compute lagged predictors

for (k in 1:fold){
data.test[[k]] = data.prep(data.test[[k]])
data.train[[k]] = data.prep(data.train[[k]])
}

return(list(data.train,data.test,data.train.c,data.test.c,data.ID,fold,mean.list.train,mean.list.test,sd.list))}
