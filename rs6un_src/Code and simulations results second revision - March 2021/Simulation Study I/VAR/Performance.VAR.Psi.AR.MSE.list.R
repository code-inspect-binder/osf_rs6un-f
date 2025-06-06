#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

Performance.VAR.Psi.AR.MSE.list = function(N,T,P,R){

# Performance results
  
MSE.Psi.list = expand.grid(id=1:R,Model=c('MAR.FE','MAR.RE','MVAR.FE','MVAR.RE','AR','VAR'),
                        Variables=P,Persons=N,Time=T) 

MSE.Psi.list = cbind(MSE.Psi.list,MSE.Psi=rep(0,nrow(MSE.Psi.list)))

# Load results

for (w in 1:nrow(MSE.Psi.list)){
  
n = MSE.Psi.list$Persons[w]
t = MSE.Psi.list$Time[w]
p = MSE.Psi.list$Variables[w]
r = MSE.Psi.list$id[w]
m = MSE.Psi.list$Model[w]

# Load data set
load(file = paste("Data_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
# Multilevel models on original data 
Psi.i = Sim.Var.Data$Psi.i
  
if (m=='MAR.FE'){
load(file = paste("MSE_Fit_MVAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
MSE.Psi.list[w,'MSE.Psi'] = mean(unlist(lapply(1:p, function(q)
  ((colMeans(matrix(unlist(lapply(1:n, function(s) 
  MSE.Fit.MVAR.PS.Sys[[1]]$beta1.i[[q]][[s]]-Psi.i[[s]][q,q])), ncol=1, byrow=T))))^2)))
}  

if (m=='MAR.RE'){
load(file = paste("MSE_Fit_MVAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
MSE.Psi.list[w,'MSE.Psi'] = mean(unlist(lapply(1:p, function(q)
  ((colMeans(matrix(unlist(lapply(1:n, function(s) 
  MSE.Fit.MVAR.PS.Sys[[2]]$beta1.i[[q]][[s]]-Psi.i[[s]][q,q])), ncol=1, byrow=T))))^2)))
}

if (m=='MVAR.FE'){
load(file = paste("MSE_Fit_MVAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
MSE.Psi.list[w,'MSE.Psi'] = mean(unlist(lapply(1:p, function(q)
  ((colMeans(matrix(unlist(lapply(1:n, function(s) 
  MSE.Fit.MVAR.PS.Sys[[3]]$beta1.i[[q]][s,1]-Psi.i[[s]][q,q])), ncol=1, byrow=T))))^2)))
}
    
if (m=='MVAR.RE'){
load(file = paste("MSE_Fit_MVAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
MSE.Psi.list[w,'MSE.Psi'] = mean(unlist(lapply(1:p, function(q)
  ((colMeans(matrix(unlist(lapply(1:n, function(s) 
  MSE.Fit.MVAR.PS.Sys[[4]]$beta1.i[[q]][s,1]-Psi.i[[s]][q,q])), ncol=1, byrow=T))))^2)))
}  

# VAR and AR models on original data

if (m=='AR'){
load(file = paste("MSE_Fit_AR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
MSE.Psi.list[w,'MSE.Psi'] = mean(unlist(lapply(1:p, function(q)
  ((colMeans(matrix(unlist(lapply(1:n, function(s) 
c(MSE.Fit.AR.PS.Sys$Psi.hat.i[[q]][[s]][-1])-Psi.i[[s]][q,q])), ncol=1, byrow=T))))^2)))
}

if (m=='VAR'){
load(file = paste("MSE_Fit_VAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
MSE.Psi.list[w,'MSE.Psi'] = mean(unlist(lapply(1:p, function(q)
  ((colMeans(matrix(unlist(lapply(1:n, function(s) 
MSE.Fit.VAR.PS.Sys$Psi.hat.i[[q]][[s]][-1][q]-Psi.i[[s]][q,q])), ncol=1, byrow=T))))^2)))
}}

######################################################################################
######################################################################################
######################################################################################

return(MSE.Psi.list=MSE.Psi.list)
}

