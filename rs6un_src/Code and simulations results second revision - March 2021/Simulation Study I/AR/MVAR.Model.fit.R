#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

#################################################################
#################################################################
#################################################################
########        Multielvel VAR(1) Model estimation      #########
#################################################################
#################################################################
#################################################################

# Function to estimate Multielvel VAR(1) Model

# The input is (n x p) data matrix data
# Model.Type determine the model for the random effect structure
# If Model.Type = 1, multielvel AR(1) model with random intercept
# If Model.Type = 2, multielvel AR(1) model with random intercept and random slope
# If Model.Type = 3, multielvel VAR(1) model with random intercept
# If Model.Type = 4, multielvel VAR(1) model with random intercept and random slope

# y is the name of the outcome variable
# y.lag is the name of the lagged outcome variable
# X.lag is the name of the lagged predictors (excluding the lagged outcome variable) 

# Multielvel VAR(1) Model estimation

MVAR.Model.fit = function(data,Model.type,y,y.lag,X.lag){

# Specify the formula from the corresponding model type

# AR(1) Random Intercept model
if (Model.type==1){
# Specify the fixed part of the model
# Predictors are fixed
Model.fixed = paste(y,paste(y.lag),sep = " ~ ")
Model.formula = paste(Model.fixed,"+","(","1",paste("|","ID",sep=""),")") 
Model = as.formula(paste(Model.fixed,"+","(","1",paste("|","ID",sep=""),")")) 
}

# AR(1) Random Slope model for the lagged predictor 
# Predictors are fixed
if (Model.type==2){
# Specify the fixed part of the model
Model.fixed = paste(y,paste(y.lag),sep = " ~ ")
# Specify the random part of the model
Model.random = paste(y.lag,sep = " + ")
Model.formula = paste(Model.fixed,"+","(","1","+",paste(Model.random,"|","ID",sep=""),")") 
Model = as.formula(paste(Model.fixed,"+","(","1","+",paste(Model.random,"|","ID",sep=""),")")) 
}

# VAR(1) Random Intercept model 
# Some Predictors are fixed
if (Model.type==3){
# Specify the fixed part of the model
Model.fixed = paste(y,paste(y.lag,paste(X.lag, collapse = " + "), sep = " + "),sep = " ~ ")
Model.formula = paste(Model.fixed,"+","(","1",paste("|","ID",sep=""),")") 
Model = as.formula(paste(Model.fixed,"+","(","1",paste("|","ID",sep=""),")")) 
}

# VAR(1) Random Slope model 
# Some Predictors are fixed
if (Model.type==4){
# Specify the fixed part of the model
Model.fixed = paste(y,paste(y.lag,paste(X.lag, collapse = " + "), sep = " + "),sep = " ~ ")
# Specify the random part of the model
Model.random = paste(y.lag,paste(X.lag, collapse = " + "), sep = " + ")
Model.formula = paste(Model.fixed,"+","(","1","+",paste(Model.random,"|","ID",sep=""),")")
Model = as.formula(paste(Model.fixed,"+","(","1","+",paste(Model.random,"|","ID",sep=""),")")) 
}

# Linear mixed effect model

jmer <- function(formula, data, REML=TRUE){
    # to simplify maintainence here (in the hopes of turning this into a real
    # package), I'm depending on JellyMe4, which copies the dataframe back with
    # the model this is of course what you want if you're primarily working in
    # Julia and just using RCall for the the R ecosystem of extras for
    # MixedModels, but it does create an unnecessary copy if you're starting
    # with your data in R.
    #
    # Also, this means we suffer/benefit from the same level of compatibility in
    # the formula as in JellyMe4, i.e. currently no support for the ||

    jf <- deparse(formula,width = 500)
    jreml = ifelse(REML, "true", "false")

    julia_assign("jmerdat",data)
    julia_command(sprintf("jmermod = fit!(LinearMixedModel(@formula(%s),jmerdat),REML=%s);",jf,jreml))

    julia_eval("robject(:lmerMod, Tuple([jmermod,jmerdat]));",need_return="R")
}

data$ID = as.factor(data$ID)

#julia.dir = julia_setup(JULIA_HOME = "C:\\Users\\u0119584\\AppData\\Local\\Programs\\Julia 1.5.3\\bin")
#julia.dir$library("MixedModels")
#julia.dir$library("JellyMe4")

fit = jmer(Model, data, REML=TRUE)

return(list(Model.formula,fit))}

