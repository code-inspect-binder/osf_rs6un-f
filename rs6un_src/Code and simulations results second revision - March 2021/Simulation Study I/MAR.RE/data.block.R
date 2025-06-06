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

for (k in 1:fold){
data.test[[k]] = data.prep(data.test[[k]])
data.train[[k]] = data.prep(data.train[[k]])
}
 
return(list(data.train,data.test,data.ID,fold))}
