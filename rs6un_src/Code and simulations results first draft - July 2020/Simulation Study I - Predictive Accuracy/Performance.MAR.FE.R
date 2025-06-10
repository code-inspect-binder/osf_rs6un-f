#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

Performance.MAR.FE = function(N,n,T,t,P,p,R){

# Performance resutls

MSE.MAR.FE.list = list()
MSE.MAR.RE.list = list()
MSE.MVAR.FE.list = list()
MSE.MVAR.RE.list = list()

MSE.AR.list = list()
MSE.VAR.list = list()

for (w in 1:P[p]){

MSE.MAR.FE.list[[w]] = rep(0,R)
MSE.MAR.RE.list[[w]] = rep(0,R)
MSE.MVAR.FE.list[[w]] = rep(0,R)
MSE.MVAR.RE.list[[w]] = rep(0,R)

MSE.AR.list[[w]] = rep(0,R)
MSE.VAR.list[[w]] = rep(0,R)

# Load block CV resutls
for (r in 1:R){

# Multilevel models on original data 
load(file = paste("MSE_MVAR_MAR_FE_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))

MSE.MAR.FE.list[[w]][r] = MSE.MVAR.PS.Sys[[1]][[w]]$MSE.MAR
MSE.MAR.RE.list[[w]][r] = MSE.MVAR.PS.Sys[[2]][[w]]$MSE.MAR
MSE.MVAR.FE.list[[w]][r] = MSE.MVAR.PS.Sys[[3]][[w]]$MSE.MVAR
MSE.MVAR.RE.list[[w]][r] = MSE.MVAR.PS.Sys[[4]][[w]]$MSE.MVAR

# VAR and AR models on original data
load(file = paste("MSE_AR_MAR_FE_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))
MSE.AR.list[[w]][r] = unlist(MSE.AR.PS.Sys$MSE.Sys.AR[w])

load(file = paste("MSE_VAR_MAR_FE_N_",N[n],"_T_",T[t],"_P_",P[p],"_R_",r,".RData",sep = ""))
MSE.VAR.list[[w]][r] = unlist(MSE.VAR.PS.Sys$MSE.Sys.MVAR[w])
}}

# Table MSE (with std. errors) 

MSE.sd.CV = matrix(0,2*6,P[p])

for (w in 1:P[p]){
MSE.sd.CV[1,w] = mean(cbind(MSE.MAR.FE.list[[w]]))
MSE.sd.CV[2,w] = sd(cbind(MSE.MAR.FE.list[[w]]))/sqrt(R)
MSE.sd.CV[3,w] = mean(cbind(MSE.MAR.RE.list[[w]]))
MSE.sd.CV[4,w] = sd(cbind(MSE.MAR.RE.list[[w]]))/sqrt(R)
MSE.sd.CV[5,w] = mean(cbind(MSE.MVAR.FE.list[[w]]))
MSE.sd.CV[6,w] = sd(cbind(MSE.MVAR.FE.list[[w]]))/sqrt(R)
MSE.sd.CV[7,w] = mean(cbind(MSE.MVAR.RE.list[[w]]))
MSE.sd.CV[8,w] = sd(cbind(MSE.MVAR.RE.list[[w]]))/sqrt(R)
MSE.sd.CV[9,w] = mean(cbind(MSE.AR.list[[w]]))
MSE.sd.CV[10,w] = sd(cbind(MSE.AR.list[[w]]))/sqrt(R)
MSE.sd.CV[11,w] = mean(cbind(MSE.VAR.list[[w]]))
MSE.sd.CV[12,w] = sd(cbind(MSE.VAR.list[[w]]))/sqrt(R)
}

colnames(MSE.sd.CV) = sprintf("Y%d",seq(1:P[p]))
rownames(MSE.sd.CV) = c('MAR.FE','','MAR.RE','','MVAR.FE','','MVAR.RE','','AR','','VAR','')

# Table for overall MSE

MSE.sd.CV.Mean = matrix(0,2*6,1)

MSE.sd.CV.Mean[1,] = mean(rowMeans(matrix(unlist(MSE.MAR.FE.list), ncol=P[p], byrow=T)))
MSE.sd.CV.Mean[2,] = sd(rowMeans(matrix(unlist(MSE.MAR.FE.list), ncol=P[p], byrow=T)))/sqrt(R)
MSE.sd.CV.Mean[3,] = mean(rowMeans(matrix(unlist(MSE.MAR.RE.list), ncol=P[p], byrow=T)))
MSE.sd.CV.Mean[4,] = sd(rowMeans(matrix(unlist(MSE.MAR.RE.list), ncol=P[p], byrow=T)))/sqrt(R)
MSE.sd.CV.Mean[5,] = mean(rowMeans(matrix(unlist(MSE.MVAR.FE.list), ncol=P[p], byrow=T)))
MSE.sd.CV.Mean[6,] = sd(rowMeans(matrix(unlist(MSE.MVAR.FE.list), ncol=P[p], byrow=T)))/sqrt(R)
MSE.sd.CV.Mean[7,] = mean(rowMeans(matrix(unlist(MSE.MVAR.RE.list), ncol=P[p], byrow=T)))
MSE.sd.CV.Mean[8,] = sd(rowMeans(matrix(unlist(MSE.MVAR.RE.list), ncol=P[p], byrow=T)))/sqrt(R)
MSE.sd.CV.Mean[9,] = mean(rowMeans(matrix(unlist(MSE.AR.list), ncol=P[p], byrow=T)))
MSE.sd.CV.Mean[10,] = sd(rowMeans(matrix(unlist(MSE.AR.list), ncol=P[p], byrow=T)))/sqrt(R)
MSE.sd.CV.Mean[11,] = mean(rowMeans(matrix(unlist(MSE.VAR.list), ncol=P[p], byrow=T)))
MSE.sd.CV.Mean[12,] = sd(rowMeans(matrix(unlist(MSE.VAR.list), ncol=P[p], byrow=T)))/sqrt(R)

colnames(MSE.sd.CV.Mean) = "Mean"
rownames(MSE.sd.CV.Mean) = c('MAR.FE','','MAR.RE','','MVAR.FE','','MVAR.RE','','AR','','VAR','')

MSE.sd = cbind(MSE.sd.CV,MSE.sd.CV.Mean)

# Table MSE

MSE = MSE.sd[-c(2,4,6,8,10,12),]

######################################################################################
######################################################################################
######################################################################################

return(list(MSE.sd=MSE.sd,MSE=MSE))
}