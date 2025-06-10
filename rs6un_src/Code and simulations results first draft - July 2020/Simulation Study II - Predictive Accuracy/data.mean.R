#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

#################################################################
#################################################################
#################################################################
##### Create a mean centered variable using persons' means #####
#################################################################
#################################################################
#################################################################

# Function to create mean centered variable using persons' means

# the input is the (n x p) data matrix (i.e., data) and a (n x 1) vector with variable y
# n is the number of rows of x
# p is the number of columns of x

data.mean = function(data.train,data.test,y){ 

subjno.i = unique(data.train$ID)

# Compute the individual's mean
y_person_mean = aggregate(data.train[,y], list(data.train$ID), FUN = mean, data=data.train, na.rm=TRUE)

# Compute the individual's mean
y_mean_train = unlist(lapply(1:length(unique(data.train$ID)), function(i) 
  rep(y_person_mean[i,2],length(which(data.train$ID==y_person_mean[i,1])))))

y_mean_test = unlist(lapply(1:length(unique(data.test$ID)), function(i) 
  rep(y_person_mean[i,2],length(which(data.test$ID==y_person_mean[i,1])))))

# Compute the grand standard deviation

y_sd = sd(data.train[,y])

return(list(y_mean_train,y_mean_test,y_sd))
}