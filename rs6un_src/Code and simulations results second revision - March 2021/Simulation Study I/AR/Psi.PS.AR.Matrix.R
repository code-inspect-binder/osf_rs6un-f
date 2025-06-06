############################################################################
#################         R Code by Ginette Lafit        ################### 
#################        ginette.lafit@kuleuven.be       ###################
############################################################################

############################################################################
############################################################################
############################################################################
##### Function to generate the matrix of VAR(1) regression weights Psi #####
############################################################################
############################################################################
############################################################################

# Function to generate the matrix of VAR(1) regression weights Psi

# the input is M the number of variables and diff the differences between the regression weights Psi
# diff is a three level factor (1,2,3)
# The autoregressive effects in Psi are drawn from a Uniform[0.7,0.9]. The cross-regressive effects are sampled 
# from Uniform[0.3,0.5], and half of the random coefficients were multiplied by (-1)

# To ensure the regression weights are less than one in modulus, the elements of the matrix Psi were multiplied by 
# a factor 0.99/abs(lambda.max). Where abs(lambda.max) is the absolute value of lambda.max, which denotes the maximum eiggenvalue of Psi 
# rescaling took place after half of the regression weights were multiplied by (-1)


Psi.PS.AR.Matrix = function(p,b.ar.min,b.ar.max){

# Generate Psi of an AR(1) model

Psi = diag(p)

diag(Psi) = runif(p,b.ar.min,b.ar.max)  

return(Psi)
}

