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

data.block.Raw = function(data,fold){ 

data.train = data[[1]]

data.test = data[[2]]

data.ID = data[[5]]
 
return(list(data.train,data.test,data.ID,fold))}
