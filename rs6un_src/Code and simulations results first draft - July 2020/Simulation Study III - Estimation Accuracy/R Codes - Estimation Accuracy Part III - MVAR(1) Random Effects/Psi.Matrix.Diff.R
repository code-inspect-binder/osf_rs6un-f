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
# The autoregressive effects in Psi are drawn from a Uniform[0.2,0.6]

# diff = 1 implies small differences in the cross-regressive effects. Half of the cross-regressive effects are sampled 
# from Uniform[0.05,0.2], and the rest are sample from a Uniform[-0.10,-0.05]

# diff = 2 implies medium differences in the cross-regressive effects. The cross-regressive effects are sampled 
# from Uniform[0.05,0.2], and half of the random coefficients were multiplied by (-1)

# To ensure the regression weights are less than one in modulus, the elements of the matrix Psi were multiplied by 
# a factor 0.99/abs(lambda.max). Where abs(lambda.max) is the absolute value of lambda.max, which denotes the maximum eiggenvalue of Psi 
# In the medium and large condition (i.e., diff=2 and diff=3), rescaling took place after half of the regression weights were multiplied by (-1)


Psi.Matrix.Diff = function(p,b.ar.min,b.ar.max,b.cr.min,b.cr.max,diff){

Psi = diag(runif(p,b.ar.min,b.ar.max)) 

# Generate Psi for the 'Small differences' condition

if (diff==1){

Off.diagonal = which(c(upper.tri(Psi) | lower.tri(Psi))==TRUE)

psi.cross.pos = sample(Off.diagonal,length(Off.diagonal)/2)
psi.cross.neg = Off.diagonal[-which(Off.diagonal %in% psi.cross.pos)]

Psi[psi.cross.pos] = runif(length(psi.cross.pos),b.cr.min,b.cr.max)
Psi[psi.cross.neg] = runif(length(psi.cross.neg),-b.cr.max/2,-b.cr.min) 

lambda.max = max(abs(eigen(Psi)$values))
if (abs(lambda.max)>1){Psi = (0.99/lambda.max)*Psi}
}

# Generate Psi for the 'Large differences' 

if (diff==2){

Off.diagonal = which(c(upper.tri(Psi) | lower.tri(Psi))==TRUE)

Psi[Off.diagonal] = c(runif(length(Off.diagonal),b.cr.min,b.cr.max))

lambda.max = max(abs(eigen(Psi)$values))
if (abs(lambda.max)>1){Psi = (0.99/lambda.max)*Psi}

psi.cross.neg = sample(Off.diagonal,length(Off.diagonal)/2)
Psi[psi.cross.neg] = -1*Psi[psi.cross.neg]
}

return(Psi)
}

