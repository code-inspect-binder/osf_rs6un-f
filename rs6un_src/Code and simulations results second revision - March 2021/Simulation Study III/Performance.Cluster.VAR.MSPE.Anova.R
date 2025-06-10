#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

Performance.Cluster.VAR.MSPE.Anova = function(N,T,P,K,S,D,Effect,R){

# Performance results

MSPE.list = expand.grid(id=1:R,Model=c('MAR.FE','MAR.RE','MVAR.FE','MVAR.RE','AR','VAR'),
                       Variables=P,Clusters=K,Persons=N,Time=T,Size=S,Diff=D,Effect=Effect)

MSPE.list = cbind(MSPE.list,MSPE=rep(0,nrow(MSPE.list)),MSPE.1se=rep(0,nrow(MSPE.list)))
  
# Load block CV results

for (w in 1:nrow(MSPE.list)){

n = MSPE.list$Persons[w]
t = MSPE.list$Time[w]
p = MSPE.list$Variables[w]
r = MSPE.list$id[w]
k = MSPE.list$Clusters[w]
s = MSPE.list$Size[w]
d = MSPE.list$Diff[w]
m = MSPE.list$Model[w]
e = MSPE.list$Effect[w]

# Multilevel models on original data 
if (m=='MAR.FE'){
load(file = paste("MSE_MVAR_Effect_",e,"_N_",n,"_T_",t,"_P_",p,"_K_",k,
"_Diff_",d,"_size_",s,"_R_",r,".RData",sep = ""))
MSPE.list[w,'MSPE'] = mean(unlist(lapply(1:p, function(h) MSE.MVAR.PS.Sys[[1]][[h]]$MSE.MAR)))
MSPE.list[w,'MSPE.1se'] = mean(unlist(lapply(1:p, function(h) MSE.MVAR.PS.Sys[[1]][[h]]$MSE.MAR.1se)))
}

if (m=='MAR.RE'){
load(file = paste("MSE_MVAR_Effect_",e,"_N_",n,"_T_",t,"_P_",p,"_K_",k,
"_Diff_",d,"_size_",s,"_R_",r,".RData",sep = ""))
MSPE.list[w,'MSPE'] = mean(unlist(lapply(1:p, function(h) MSE.MVAR.PS.Sys[[2]][[h]]$MSE.MAR)))
MSPE.list[w,'MSPE.1se'] = mean(unlist(lapply(1:p, function(h) MSE.MVAR.PS.Sys[[2]][[h]]$MSE.MAR.1se)))
}

if (m=='MVAR.FE'){
load(file = paste("MSE_MVAR_Effect_",e,"_N_",n,"_T_",t,"_P_",p,"_K_",k,
"_Diff_",d,"_size_",s,"_R_",r,".RData",sep = ""))
MSPE.list[w,'MSPE'] = mean(unlist(lapply(1:p, function(h) MSE.MVAR.PS.Sys[[3]][[h]]$MSE.MVAR)))
MSPE.list[w,'MSPE.1se'] = mean(unlist(lapply(1:p, function(h) MSE.MVAR.PS.Sys[[3]][[h]]$MSE.MVAR.1se)))
}

if (m=='MVAR.RE'){
load(file = paste("MSE_MVAR_Effect_",e,"_N_",n,"_T_",t,"_P_",p,"_K_",k,
"_Diff_",d,"_size_",s,"_R_",r,".RData",sep = ""))
MSPE.list[w,'MSPE'] = mean(unlist(lapply(1:p, function(h) MSE.MVAR.PS.Sys[[4]][[h]]$MSE.MVAR)))
MSPE.list[w,'MSPE.1se'] = mean(unlist(lapply(1:p, function(h) MSE.MVAR.PS.Sys[[4]][[h]]$MSE.MVAR.1se)))
}

# VAR and AR models on original data
if (m=='AR'){
load(file = paste("MSE_AR_Effect_",e,"_N_",n,"_T_",t,"_P_",p,"_K_",k,
"_Diff_",d,"_size_",s,"_R_",r,".RData",sep = ""))
MSPE.list[w,'MSPE'] = mean(unlist(MSE.AR.PS.Sys$MSE.Sys.AR))
MSPE.list[w,'MSPE.1se'] = mean(unlist(MSE.AR.PS.Sys$MSE.Sys.AR.1se))
}

if (m=='VAR'){
load(file = paste("MSE_VAR_Effect_",e,"_N_",n,"_T_",t,"_P_",p,"_K_",k,
"_Diff_",d,"_size_",s,"_R_",r,".RData",sep = ""))
MSPE.list[w,'MSPE'] = mean(unlist(MSE.VAR.PS.Sys$MSE.Sys.VAR))
MSPE.list[w,'MSPE.1se'] = mean(unlist(MSE.VAR.PS.Sys$MSE.Sys.VAR.1se))
}}

######################################################################################
######################################################################################
######################################################################################

return(MSPE.list=MSPE.list)
}