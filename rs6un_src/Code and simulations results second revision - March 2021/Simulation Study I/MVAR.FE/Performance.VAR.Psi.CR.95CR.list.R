#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

Performance.VAR.Psi.CR.95CR.list = function(N,T,P,R){

# Performance results
  
CR.Psi.list = expand.grid(id=1:R,Model=c('MVAR.FE','MVAR.RE','VAR'),
                           Variables=P,Persons=N,Time=T)
  
CR.Psi.list = cbind(CR.Psi.list,CR.Psi=rep(0,nrow(CR.Psi.list)))  

  
# Load results
  
for (w in 1:nrow(CR.Psi.list)){
    
n = CR.Psi.list$Persons[w]
t = CR.Psi.list$Time[w]
p = CR.Psi.list$Variables[w]
r = CR.Psi.list$id[w]
m = CR.Psi.list$Model[w]

# Load data set
load(file = paste("Data_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
# Multilevel models on original data 
Psi = Sim.Var.Data$Psi
Psi.i = lapply(1:n, function(i) Psi)

if (m=='MVAR.FE'){
load(file = paste("MSE_Fit_MVAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
CR.Psi.list[w,'CR.Psi'] = mean(unlist(lapply(1:p, function(q) 
  c(MSE.Fit.MVAR.PS.Sys[[3]]$Psi.CI.hat[[q]][-1,1][-1] < Psi[q,c(q,seq(1:p)[-q])][-1] 
& MSE.Fit.MVAR.PS.Sys[[3]]$Psi.CI.hat[[q]][-1,2][-1] > Psi[q,c(q,seq(1:p)[-q])][-1]))))
}

if (m=='MVAR.RE'){
load(file = paste("MSE_Fit_MVAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
CR.Psi.list[w,'CR.Psi'] = mean(unlist(lapply(1:p, function(q) 
  c(MSE.Fit.MVAR.PS.Sys[[4]]$Psi.CI.hat[[q]][-1,1][-1] < Psi[q,c(q,seq(1:p)[-q])][-1] 
& MSE.Fit.MVAR.PS.Sys[[4]]$Psi.CI.hat[[q]][-1,2][-1] > Psi[q,c(q,seq(1:p)[-q])][-1]))))
}

# VAR models on original data

if (m=='VAR'){
load(file = paste("MSE_Fit_VAR_N_",n,"_T_",t,"_P_",p,"_R_",r,".RData",sep = ""))
CR.Psi.list[w,'CR.Psi'] = mean(unlist(lapply(1:p, function(q)
colMeans(matrix(unlist(lapply(1:n, function(s)
c(MSE.Fit.VAR.PS.Sys$Psi.CI.hat.i[[q]][[s]][-1,1][-1] < Psi.i[[s]][q,c(q,seq(1:p)[-q])][-1] 
& MSE.Fit.VAR.PS.Sys$Psi.CI.hat.i[[q]][[s]][-1,2][-1] > Psi.i[[s]][q,c(q,seq(1:p)[-q])][-1]))), ncol=(p-1), byrow=T)))))
}}
######################################################################################
######################################################################################
######################################################################################

return(CR.Psi.list=CR.Psi.list)
}
