Performance.VAR = function(N,n,T,t,P,p,R){

# Performance resutls

MSE.MAR.FE.list = rep(0,R)
MSE.MAR.RE.list = rep(0,R)
MSE.MVAR.FE.list = rep(0,R)
MSE.MVAR.RE.list = rep(0,R)

MSE.AR.list = rep(0,R)
MSE.VAR.list = rep(0,R)

# Load block CV resutls
for (r in 1:R){

# Multilevel models on original data 
load(file = paste("MSE_MVAR_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))

MSE.MAR.FE.list[r] = MSE.VAR.Sys$MSE.MAR.Fixed
MSE.MAR.RE.list[r] = MSE.VAR.Sys$MSE.MAR.Random
MSE.MVAR.FE.list[r] = MSE.VAR.Sys$MSE.MVAR.Fixed
MSE.MVAR.RE.list[r] = MSE.VAR.Sys$MSE.MVAR.Random

# VAR and AR models on original data
load(file = paste("MSE_AR_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))
MSE.AR.list[r] = unlist(MSE.AR.PS.Sys$MSE)

load(file = paste("MSE_VAR_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))
MSE.VAR.list[r] = unlist(MSE.VAR.PS.Sys$MSE)
}

# Table MSE (with std. errors) 

MSE.sd = matrix(0,2*6,1)

for (w in 1:P[p]){
MSE.sd[1] = mean(MSE.MAR.FE.list)
MSE.sd[2] = sd(MSE.MAR.FE.list)/sqrt(R)
MSE.sd[3] = mean(MSE.MAR.RE.list)
MSE.sd[4] = sd(MSE.MAR.RE.list)/sqrt(R)
MSE.sd[5] = mean(MSE.MVAR.FE.list)
MSE.sd[6] = sd(MSE.MVAR.FE.list)/sqrt(R)
MSE.sd[7] = mean(MSE.MVAR.RE.list)
MSE.sd[8] = sd(MSE.MVAR.RE.list)/sqrt(R)
MSE.sd[9] = mean(MSE.AR.list)
MSE.sd[10] = sd(MSE.AR.list)/sqrt(R)
MSE.sd[11] = mean(MSE.VAR.list)
MSE.sd[12] = sd(MSE.VAR.list)/sqrt(R)
}

colnames(MSE.sd) = c('MSE')
rownames(MSE.sd) = c('MAR.FE','','MAR.RE','','MVAR.FE','','MVAR.RE','','AR','','VAR','')

######################################################################################
######################################################################################
######################################################################################

return(list(MSE.sd))
}