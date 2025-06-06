#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

Performance.VAR.MSPE.Anova = function(N,T,P,R){

# Performance results

MSPE.list = expand.grid(id=1:R,Model=c('MAR.FE','MAR.RE','MVAR.FE','MVAR.RE','AR','VAR'),
                       Variables=P,Persons=N,Time=T)

MSPE.list = cbind(MSPE.list,MSPE=rep(0,nrow(MSPE.list)),MSPE.Var=rep(0,nrow(MSPE.list)),
                  MSPE.Bias=rep(0,nrow(MSPE.list)),MSPE.1se=rep(0,nrow(MSPE.list)))
  
# Load block CV results

for (w in 1:nrow(MSPE.list)){

n = MSPE.list$Persons[w]
t = MSPE.list$Time[w]
p = MSPE.list$Variables[w]
r = MSPE.list$id[w]
m = MSPE.list$Model[w]

# Multilevel models on original data 
if (m=='MAR.FE'){
load(file = paste("MSE_MVAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
MSPE.list[w,'MSPE'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[1]][[q]]$MSE.MAR)))
MSPE.list[w,'MSPE.Var'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[1]][[q]]$Var.MAR)))
MSPE.list[w,'MSPE.Bias'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[1]][[q]]$Bias.MAR)))
MSPE.list[w,'MSPE.1se'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[1]][[q]]$MSE.MAR.1se)))
}

if (m=='MAR.RE'){
load(file = paste("MSE_MVAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
MSPE.list[w,'MSPE'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[2]][[q]]$MSE.MAR)))
MSPE.list[w,'MSPE.Var'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[2]][[q]]$Var.MAR)))
MSPE.list[w,'MSPE.Bias'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[2]][[q]]$Bias.MAR)))
MSPE.list[w,'MSPE.1se'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[2]][[q]]$MSE.MAR.1se)))
}

if (m=='MVAR.FE'){
load(file = paste("MSE_MVAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
MSPE.list[w,'MSPE'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[3]][[q]]$MSE.MVAR)))
MSPE.list[w,'MSPE.Var'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[3]][[q]]$Var.MVAR)))
MSPE.list[w,'MSPE.Bias'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[3]][[q]]$Bias.MVAR)))
MSPE.list[w,'MSPE.1se'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[3]][[q]]$MSE.MVAR.1se)))
}

if (m=='MVAR.RE'){
load(file = paste("MSE_MVAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
MSPE.list[w,'MSPE'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[4]][[q]]$MSE.MVAR)))
MSPE.list[w,'MSPE.Var'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[4]][[q]]$Var.MVAR)))
MSPE.list[w,'MSPE.Bias'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[4]][[q]]$Bias.MVAR)))
MSPE.list[w,'MSPE.1se'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[4]][[q]]$MSE.MVAR.1se)))
}


# VAR and AR models on original data
if (m=='AR'){
load(file = paste("MSE_AR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
MSPE.list[w,'MSPE'] = mean(unlist(MSE.AR.PS.Sys$MSE.Sys.AR))
MSPE.list[w,'MSPE.Var'] = mean(unlist(MSE.AR.PS.Sys$Var.Sys.AR))
MSPE.list[w,'MSPE.Bias'] = mean(unlist(MSE.AR.PS.Sys$Bias.Sys.AR))
MSPE.list[w,'MSPE.1se'] = mean(unlist(MSE.AR.PS.Sys$MSE.Sys.AR.1se))
}

if (m=='VAR'){
load(file = paste("MSE_VAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
MSPE.list[w,'MSPE'] = mean(unlist(MSE.VAR.PS.Sys$MSE.Sys.VAR))
MSPE.list[w,'MSPE.Var'] = mean(unlist(MSE.VAR.PS.Sys$Var.Sys.VAR))
MSPE.list[w,'MSPE.Bias'] = mean(unlist(MSE.VAR.PS.Sys$Bias.Sys.VAR))
MSPE.list[w,'MSPE.1se'] = mean(unlist(MSE.VAR.PS.Sys$MSE.Sys.VAR.1se))
}}

######################################################################################
######################################################################################
######################################################################################

return(MSPE.list=MSPE.list)
}