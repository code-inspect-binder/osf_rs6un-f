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

MVAR.Model = function(data,Model.type,y,y.lag,X.lag){

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

# For convergence issues check: https://cran.r-project.org/web/packages/lme4/vignettes/lmerperf.html

#nlopt <- function(par, fn, lower, upper, control) {
#    .nloptr <<- res <- nloptr(par, fn, lb = lower, ub = upper, 
#        opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
#        maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
#    list(par = res$solution,
#         fval = res$objective,
#         conv = if (res$status > 0) 0 else res$status,
#         message = res$message
#    )
#}

#ctrl = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
#optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))

#ctrl = lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)

#fit = try(lmer(Model, data = data,control=ctrl,REML=TRUE),silent = TRUE)

data$ID = as.factor(data$ID)

julia.dir = julia_setup(JULIA_HOME = "C:\\Users\\u0119584\\AppData\\Local\\Programs\\Julia 1.5.3\\bin")
julia.dir$library("MixedModels")
julia.dir$assign("data", data)
julia.dir$assign("form", Model)
results = julia.dir$eval("res = fit(LinearMixedModel, form, data)",need_return = c("Julia"))

beta.fix = julia_eval("coef(res)")
beta.random = t(julia_eval("ranef(res)")[1])

return(list(Model.formula,beta.fix,beta.random))}

