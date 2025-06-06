#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

#################################################################
#################################################################
#################################################################
##########               Data preparation            ############
#################################################################
#################################################################
#################################################################

# Function to create a lag variable
# the data is lag within person and within days

data.prep = function(data){ 

subjno.i = unique(data$ID)

Y_lag = matrix(0,nrow(data),ncol(data)-2)
for (i in subjno.i){
n.i = which(data$ID==i)
Y_lag[n.i,] = apply(data[n.i,3:ncol(data)],2,shift)
}

colnames(Y_lag) = paste0(colnames(data[,3:ncol(data)]),'_1')

data = cbind(data,Y_lag)

return(data)
}