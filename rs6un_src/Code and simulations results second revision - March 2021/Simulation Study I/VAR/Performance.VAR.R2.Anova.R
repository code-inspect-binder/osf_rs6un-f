#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

Performance.VAR.R2.Anova = function(N,T,P,R){

# Performance results

R2.list = expand.grid(id=1:R,Model=c('MAR.FE','MAR.RE','MVAR.FE','MVAR.RE','AR','VAR'),
                       Variables=P,Persons=N,Time=T)

R2.list = cbind(R2.list,R2=rep(0,nrow(R2.list)))
  
# Load block CV results

for (w in 1:nrow(R2.list)){

n = R2.list$Persons[w]
t = R2.list$Time[w]
p = R2.list$Variables[w]
r = R2.list$id[w]
m = R2.list$Model[w]

# Multilevel models on original data 
if (m=='MAR.FE'){
load(file = paste("MSE_MVAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
R2.list[w,'R2'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[1]][[q]]$R2.Sys.MAR)))
}

if (m=='MAR.RE'){
load(file = paste("MSE_MVAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
R2.list[w,'R2'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[2]][[q]]$R2.Sys.MAR)))
}

if (m=='MVAR.FE'){
load(file = paste("MSE_MVAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
R2.list[w,'R2'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[3]][[q]]$R2.Sys.MVAR)))
}

if (m=='MVAR.RE'){
load(file = paste("MSE_MVAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
R2.list[w,'R2'] = mean(unlist(lapply(1:p, function(q) MSE.MVAR.PS.Sys[[4]][[q]]$R2.Sys.MVAR)))
}


# VAR and AR models on original data
if (m=='AR'){
load(file = paste("MSE_AR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
R2.list[w,'R2'] = mean(unlist(MSE.AR.PS.Sys$R2.Sys.AR))
}

if (m=='VAR'){
load(file = paste("MSE_VAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
R2.list[w,'R2'] = mean(unlist(MSE.VAR.PS.Sys$R2.Sys.VAR))
}}

######################################################################################
######################################################################################
######################################################################################

return(R2.list=R2.list)
}