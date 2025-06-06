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
# The autoregressive effects in Psi are drawn from a Uniform[b.ar.min,b.ar.max]. The cross-regressive effects are sampled 
# from Uniform[b.cr.min,b.cr.max], and half of the random coefficients were multiplied by (-1)

# To ensure the regression weights are less than one in modulus, the elements of the matrix Psi were multiplied by 
# a factor 0.99/abs(lambda.max). Where abs(lambda.max) is the absolute value of lambda.max, which denotes the maximum eiggenvalue of Psi 
# rescaling took place after half of the regression weights were multiplied by (-1)


Psi.Matrix = function(p,b.ar.min,b.ar.max,b.cr.min,b.cr.max){

p.sampled = sample(1:p,p/2)

Psi = diag(p) 

diag(Psi)[p.sampled] = rep(b.ar.min,p/2)
diag(Psi)[-p.sampled] = rep(b.ar.max,p/2)

# Generate Psi 

Off.diagonal = which(c(upper.tri(Psi) | lower.tri(Psi))==TRUE)

Psi[Off.diagonal] = runif(length(Off.diagonal),b.cr.min,b.cr.max)

lambda.max = max(abs(eigen(Psi)$values))
if (abs(lambda.max)>1){Psi = (0.99/lambda.max)*Psi}

return(Psi)
}


