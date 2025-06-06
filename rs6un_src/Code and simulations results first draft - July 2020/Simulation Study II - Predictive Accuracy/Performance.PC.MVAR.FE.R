Performance.PC.MVAR.FE = function(N,n,T,t,Q,q,e,R,P){

# Tables for PC-VAR(1)

MSE.PC.VAR.list.nComp.1 = matrix(0,R,P)
MSE.PC.VAR.list.nComp.2 = matrix(0,R,P)
MSE.PC.VAR.list.nComp.3 = matrix(0,R,P)
MSE.PC.VAR.list.nComp.4 = matrix(0,R,P)
MSE.PC.VAR.list.nComp.5 = matrix(0,R,P)
MSE.PC.VAR.list.nComp.6 = matrix(0,R,P)

for (p in 1:P){

# Load block CV resutls
for (r in 1:R){

# Load PC-VAR with 1 component
load(file = paste("MSE_PC_VAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",1,"_R_",r,".RData",sep = ""))

MSE.PC.VAR.list.nComp.1[r,p] = unlist(MSE.PC.VAR.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 2 component
load(file = paste("MSE_PC_VAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",2,"_R_",r,".RData",sep = ""))

MSE.PC.VAR.list.nComp.2[r,p] = unlist(MSE.PC.VAR.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 3 component
load(file = paste("MSE_PC_VAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",3,"_R_",r,".RData",sep = ""))

MSE.PC.VAR.list.nComp.3[r,p] = unlist(MSE.PC.VAR.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 4 component
load(file = paste("MSE_PC_VAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",4,"_R_",r,".RData",sep = ""))

MSE.PC.VAR.list.nComp.4[r,p] = unlist(MSE.PC.VAR.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 5 component
load(file = paste("MSE_PC_VAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",5,"_R_",r,".RData",sep = ""))

MSE.PC.VAR.list.nComp.5[r,p] = unlist(MSE.PC.VAR.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 6 component
load(file = paste("MSE_PC_VAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",6,"_R_",r,".RData",sep = ""))

MSE.PC.VAR.list.nComp.6[r,p] = unlist(MSE.PC.VAR.Sys$MSE.Sys.MVAR[p])
}}

# Table MSE PC-VAR

MSE.PC.VAR = matrix(0,2*P,P)

MSE.PC.VAR[1,] = colMeans(MSE.PC.VAR.list.nComp.1)
MSE.PC.VAR[2,] = apply(MSE.PC.VAR.list.nComp.1,2,sd)/sqrt(R)

MSE.PC.VAR[3,] = colMeans(MSE.PC.VAR.list.nComp.2)
MSE.PC.VAR[4,] = apply(MSE.PC.VAR.list.nComp.2,2,sd)/sqrt(R)

MSE.PC.VAR[5,] = colMeans(MSE.PC.VAR.list.nComp.3)
MSE.PC.VAR[6,] = apply(MSE.PC.VAR.list.nComp.3,2,sd)/sqrt(R)

MSE.PC.VAR[7,] = colMeans(MSE.PC.VAR.list.nComp.4)
MSE.PC.VAR[8,] = apply(MSE.PC.VAR.list.nComp.4,2,sd)/sqrt(R)

MSE.PC.VAR[9,] = colMeans(MSE.PC.VAR.list.nComp.5)
MSE.PC.VAR[10,] = apply(MSE.PC.VAR.list.nComp.5,2,sd)/sqrt(R)

MSE.PC.VAR[11,] = colMeans(MSE.PC.VAR.list.nComp.6)
MSE.PC.VAR[12,] = apply(MSE.PC.VAR.list.nComp.6,2,sd)/sqrt(R)

colnames(MSE.PC.VAR) = c("Y1","Y2","Y3","Y4","Y5","Y6")
rownames(MSE.PC.VAR) = c("PC.VAR.Comp.1","","PC.VAR.Comp.2","","PC.VAR.Comp.3","",
"PC.VAR.Comp.4","","PC.VAR.Comp.5","","PC.VAR.Comp.6","")

# Table for overall MSE

# Table MSE PC-VAR Mean

MSE.PC.VAR.Mean = matrix(0,2*P,1)

MSE.PC.VAR.Mean[1,1] = mean(rowMeans(MSE.PC.VAR.list.nComp.1))
MSE.PC.VAR.Mean[2,1] = sd(rowMeans(MSE.PC.VAR.list.nComp.1))/sqrt(R)

MSE.PC.VAR.Mean[3,1] = mean(rowMeans(MSE.PC.VAR.list.nComp.2))
MSE.PC.VAR.Mean[4,1] = sd(rowMeans(MSE.PC.VAR.list.nComp.2))/sqrt(R)

MSE.PC.VAR.Mean[5,1] = mean(rowMeans(MSE.PC.VAR.list.nComp.3))
MSE.PC.VAR.Mean[6,1] = sd(rowMeans(MSE.PC.VAR.list.nComp.3))/sqrt(R)

MSE.PC.VAR.Mean[7,1] = mean(rowMeans(MSE.PC.VAR.list.nComp.4))
MSE.PC.VAR.Mean[8,1] = sd(rowMeans(MSE.PC.VAR.list.nComp.4))/sqrt(R)

MSE.PC.VAR.Mean[9,1] = mean(rowMeans(MSE.PC.VAR.list.nComp.5))
MSE.PC.VAR.Mean[10,1] = sd(rowMeans(MSE.PC.VAR.list.nComp.5))/sqrt(R)

MSE.PC.VAR.Mean[11,1] = mean(rowMeans(MSE.PC.VAR.list.nComp.6))
MSE.PC.VAR.Mean[12,1] = sd(rowMeans(MSE.PC.VAR.list.nComp.6))/sqrt(R)

colnames(MSE.PC.VAR.Mean) = c("Mean")
rownames(MSE.PC.VAR.Mean) = c("PC.VAR.Comp.1","","PC.VAR.Comp.2","","PC.VAR.Comp.3","",
"PC.VAR.Comp.4","","PC.VAR.Comp.5","","PC.VAR.Comp.6","")

MSE.PC.VAR = cbind(MSE.PC.VAR,MSE.PC.VAR.Mean)

######################################################################################
######################################################################################
######################################################################################

# Tables for PC-MAR(1) Fixed Effects

MSE.PC.MAR.FE.list.nComp.1 = matrix(0,R,P)
MSE.PC.MAR.FE.list.nComp.2 = matrix(0,R,P)
MSE.PC.MAR.FE.list.nComp.3 = matrix(0,R,P)
MSE.PC.MAR.FE.list.nComp.4 = matrix(0,R,P)
MSE.PC.MAR.FE.list.nComp.5 = matrix(0,R,P)
MSE.PC.MAR.FE.list.nComp.6 = matrix(0,R,P)

for (p in 1:P){

# Load block CV resutls
for (r in 1:R){

# Load PC-VAR with 1 component
load(file = paste("MSE_PC_MAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",1,"_R_",r,".RData",sep = ""))

MSE.PC.MAR.FE.list.nComp.1[r,p] = unlist(MSE.PC.MAR.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 2 component
load(file = paste("MSE_PC_MAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",2,"_R_",r,".RData",sep = ""))

MSE.PC.MAR.FE.list.nComp.2[r,p] = unlist(MSE.PC.MAR.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 3 component
load(file = paste("MSE_PC_MAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",3,"_R_",r,".RData",sep = ""))

MSE.PC.MAR.FE.list.nComp.3[r,p] = unlist(MSE.PC.MAR.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 4 component
load(file = paste("MSE_PC_MAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",4,"_R_",r,".RData",sep = ""))

MSE.PC.MAR.FE.list.nComp.4[r,p] = unlist(MSE.PC.MAR.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 5 component
load(file = paste("MSE_PC_MAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",5,"_R_",r,".RData",sep = ""))

MSE.PC.MAR.FE.list.nComp.5[r,p] = unlist(MSE.PC.MAR.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 6 component
load(file = paste("MSE_PC_MAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",6,"_R_",r,".RData",sep = ""))

MSE.PC.MAR.FE.list.nComp.6[r,p] = unlist(MSE.PC.MAR.Sys$MSE.Sys.MVAR[p])
}}

# Table MSE PC-MAR

MSE.PC.MAR.FE = matrix(0,2*P,P)

MSE.PC.MAR.FE[1,] = colMeans(MSE.PC.MAR.FE.list.nComp.1)
MSE.PC.MAR.FE[2,] = apply(MSE.PC.MAR.FE.list.nComp.1,2,sd)/sqrt(R)

MSE.PC.MAR.FE[3,] = colMeans(MSE.PC.MAR.FE.list.nComp.2)
MSE.PC.MAR.FE[4,] = apply(MSE.PC.MAR.FE.list.nComp.2,2,sd)/sqrt(R)

MSE.PC.MAR.FE[5,] = colMeans(MSE.PC.MAR.FE.list.nComp.3)
MSE.PC.MAR.FE[6,] = apply(MSE.PC.MAR.FE.list.nComp.3,2,sd)/sqrt(R)

MSE.PC.MAR.FE[7,] = colMeans(MSE.PC.MAR.FE.list.nComp.4)
MSE.PC.MAR.FE[8,] = apply(MSE.PC.MAR.FE.list.nComp.4,2,sd)/sqrt(R)

MSE.PC.MAR.FE[9,] = colMeans(MSE.PC.MAR.FE.list.nComp.5)
MSE.PC.MAR.FE[10,] = apply(MSE.PC.MAR.FE.list.nComp.5,2,sd)/sqrt(R)

MSE.PC.MAR.FE[11,] = colMeans(MSE.PC.MAR.FE.list.nComp.6)
MSE.PC.MAR.FE[12,] = apply(MSE.PC.MAR.FE.list.nComp.6,2,sd)/sqrt(R)

colnames(MSE.PC.MAR.FE) = c("Y1","Y2","Y3","Y4","Y5","Y6")
rownames(MSE.PC.MAR.FE) = c("PC.MAR.FE.Comp.1","","PC.MAR.FE.Comp.2","","PC.MAR.FE.Comp.3","",
"PC.MAR.FE.Comp.4","","PC.MAR.FE.Comp.5","","PC.MAR.FE.Comp.6","")

# Table for overall MSE

# Table MSE PC-MAR Mean

MSE.PC.MAR.FE.Mean = matrix(0,2*P,1)

MSE.PC.MAR.FE.Mean[1,1] = mean(rowMeans(MSE.PC.MAR.FE.list.nComp.1))
MSE.PC.MAR.FE.Mean[2,1] = sd(rowMeans(MSE.PC.MAR.FE.list.nComp.1))/sqrt(R)

MSE.PC.MAR.FE.Mean[3,1] = mean(rowMeans(MSE.PC.MAR.FE.list.nComp.2))
MSE.PC.MAR.FE.Mean[4,1] = sd(rowMeans(MSE.PC.MAR.FE.list.nComp.2))/sqrt(R)

MSE.PC.MAR.FE.Mean[5,1] = mean(rowMeans(MSE.PC.MAR.FE.list.nComp.3))
MSE.PC.MAR.FE.Mean[6,1] = sd(rowMeans(MSE.PC.MAR.FE.list.nComp.3))/sqrt(R)

MSE.PC.MAR.FE.Mean[7,1] = mean(rowMeans(MSE.PC.MAR.FE.list.nComp.4))
MSE.PC.MAR.FE.Mean[8,1] = sd(rowMeans(MSE.PC.MAR.FE.list.nComp.4))/sqrt(R)

MSE.PC.MAR.FE.Mean[9,1] = mean(rowMeans(MSE.PC.MAR.FE.list.nComp.5))
MSE.PC.MAR.FE.Mean[10,1] = sd(rowMeans(MSE.PC.MAR.FE.list.nComp.5))/sqrt(R)

MSE.PC.MAR.FE.Mean[11,1] = mean(rowMeans(MSE.PC.MAR.FE.list.nComp.6))
MSE.PC.MAR.FE.Mean[12,1] = sd(rowMeans(MSE.PC.MAR.FE.list.nComp.6))/sqrt(R)

colnames(MSE.PC.MAR.FE.Mean) = c("Mean")
rownames(MSE.PC.MAR.FE.Mean) = c("PC.MAR.FE.Comp.1","","PC.MAR.FE.Comp.2","","PC.MAR.FE.Comp.3","",
"PC.MAR.FE.Comp.4","","PC.MAR.FE.Comp.5","","PC.MAR.FE.Comp.6","")

MSE.PC.MAR.FE = cbind(MSE.PC.MAR.FE,MSE.PC.MAR.FE.Mean)

######################################################################################
######################################################################################
######################################################################################

# Tables for PC-MAR(1) Random Effects

MSE.PC.MAR.RE.list.nComp.1 = matrix(0,R,P)
MSE.PC.MAR.RE.list.nComp.2 = matrix(0,R,P)
MSE.PC.MAR.RE.list.nComp.3 = matrix(0,R,P)
MSE.PC.MAR.RE.list.nComp.4 = matrix(0,R,P)
MSE.PC.MAR.RE.list.nComp.5 = matrix(0,R,P)
MSE.PC.MAR.RE.list.nComp.6 = matrix(0,R,P)

for (p in 1:P){

# Load block CV resutls
for (r in 1:R){

# Load PC-VAR with 1 component
load(file = paste("MSE_PC_MAR_RE_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",1,"_R_",r,".RData",sep = ""))

MSE.PC.MAR.RE.list.nComp.1[r,p] = unlist(MSE.PC.MAR.RE.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 2 component
load(file = paste("MSE_PC_MAR_RE_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",2,"_R_",r,".RData",sep = ""))

MSE.PC.MAR.RE.list.nComp.2[r,p] = unlist(MSE.PC.MAR.RE.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 3 component
load(file = paste("MSE_PC_MAR_RE_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",3,"_R_",r,".RData",sep = ""))

MSE.PC.MAR.RE.list.nComp.3[r,p] = unlist(MSE.PC.MAR.RE.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 4 component
load(file = paste("MSE_PC_MAR_RE_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",4,"_R_",r,".RData",sep = ""))

MSE.PC.MAR.RE.list.nComp.4[r,p] = unlist(MSE.PC.MAR.RE.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 5 component
load(file = paste("MSE_PC_MAR_RE_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",5,"_R_",r,".RData",sep = ""))

MSE.PC.MAR.RE.list.nComp.5[r,p] = unlist(MSE.PC.MAR.RE.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 6 component
load(file = paste("MSE_PC_MAR_RE_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",6,"_R_",r,".RData",sep = ""))

MSE.PC.MAR.RE.list.nComp.6[r,p] = unlist(MSE.PC.MAR.RE.Sys$MSE.Sys.MVAR[p])
}}

# Table MSE PC-MAR

MSE.PC.MAR.RE = matrix(0,2*P,P)

MSE.PC.MAR.RE[1,] = colMeans(MSE.PC.MAR.RE.list.nComp.1)
MSE.PC.MAR.RE[2,] = apply(MSE.PC.MAR.RE.list.nComp.1,2,sd)/sqrt(R)

MSE.PC.MAR.RE[3,] = colMeans(MSE.PC.MAR.RE.list.nComp.2)
MSE.PC.MAR.RE[4,] = apply(MSE.PC.MAR.RE.list.nComp.2,2,sd)/sqrt(R)

MSE.PC.MAR.RE[5,] = colMeans(MSE.PC.MAR.RE.list.nComp.3)
MSE.PC.MAR.RE[6,] = apply(MSE.PC.MAR.RE.list.nComp.3,2,sd)/sqrt(R)

MSE.PC.MAR.RE[7,] = colMeans(MSE.PC.MAR.RE.list.nComp.4)
MSE.PC.MAR.RE[8,] = apply(MSE.PC.MAR.RE.list.nComp.4,2,sd)/sqrt(R)

MSE.PC.MAR.RE[9,] = colMeans(MSE.PC.MAR.RE.list.nComp.5)
MSE.PC.MAR.RE[10,] = apply(MSE.PC.MAR.RE.list.nComp.5,2,sd)/sqrt(R)

MSE.PC.MAR.RE[11,] = colMeans(MSE.PC.MAR.RE.list.nComp.6)
MSE.PC.MAR.RE[12,] = apply(MSE.PC.MAR.RE.list.nComp.6,2,sd)/sqrt(R)

colnames(MSE.PC.MAR.RE) = c("Y1","Y2","Y3","Y4","Y5","Y6")
rownames(MSE.PC.MAR.RE) = c("PC.MAR.RE.Comp.1","","PC.MAR.RE.Comp.2","","PC.MAR.RE.Comp.3","",
"PC.MAR.RE.Comp.4","","PC.MAR.RE.Comp.5","","PC.MAR.RE.Comp.6","")

# Table for overall MSE

# Table MSE PC-MAR Mean

MSE.PC.MAR.RE.Mean = matrix(0,2*P,1)

MSE.PC.MAR.RE.Mean[1,1] = mean(rowMeans(MSE.PC.MAR.RE.list.nComp.1))
MSE.PC.MAR.RE.Mean[2,1] = sd(rowMeans(MSE.PC.MAR.RE.list.nComp.1))/sqrt(R)

MSE.PC.MAR.RE.Mean[3,1] = mean(rowMeans(MSE.PC.MAR.RE.list.nComp.2))
MSE.PC.MAR.RE.Mean[4,1] = sd(rowMeans(MSE.PC.MAR.RE.list.nComp.2))/sqrt(R)

MSE.PC.MAR.RE.Mean[5,1] = mean(rowMeans(MSE.PC.MAR.RE.list.nComp.3))
MSE.PC.MAR.RE.Mean[6,1] = sd(rowMeans(MSE.PC.MAR.RE.list.nComp.3))/sqrt(R)

MSE.PC.MAR.RE.Mean[7,1] = mean(rowMeans(MSE.PC.MAR.RE.list.nComp.4))
MSE.PC.MAR.RE.Mean[8,1] = sd(rowMeans(MSE.PC.MAR.RE.list.nComp.4))/sqrt(R)

MSE.PC.MAR.RE.Mean[9,1] = mean(rowMeans(MSE.PC.MAR.RE.list.nComp.5))
MSE.PC.MAR.RE.Mean[10,1] = sd(rowMeans(MSE.PC.MAR.RE.list.nComp.5))/sqrt(R)

MSE.PC.MAR.RE.Mean[11,1] = mean(rowMeans(MSE.PC.MAR.RE.list.nComp.6))
MSE.PC.MAR.RE.Mean[12,1] = sd(rowMeans(MSE.PC.MAR.RE.list.nComp.6))/sqrt(R)

colnames(MSE.PC.MAR.RE.Mean) = c("Mean")
rownames(MSE.PC.MAR.RE.Mean) = c("PC.MAR.RE.Comp.1","","PC.MAR.RE.Comp.2","","PC.MAR.RE.Comp.3","",
"PC.MAR.RE.Comp.4","","PC.MAR.RE.Comp.5","","PC.MAR.RE.Comp.6","")

MSE.PC.MAR.RE = cbind(MSE.PC.MAR.RE,MSE.PC.MAR.RE.Mean)

######################################################################################
######################################################################################
######################################################################################

# Tables for PC-MVAR(1) - Fixed Effects

MSE.PC.MVAR.FE.list.nComp.2 = matrix(0,R,P)
MSE.PC.MVAR.FE.list.nComp.3 = matrix(0,R,P)
MSE.PC.MVAR.FE.list.nComp.4 = matrix(0,R,P)
MSE.PC.MVAR.FE.list.nComp.5 = matrix(0,R,P)
MSE.PC.MVAR.FE.list.nComp.6 = matrix(0,R,P)

for (p in 1:P){

# Load block CV resutls
for (r in 1:R){

# Load PC-VAR with 2 component
load(file = paste("MSE_PC_MVAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",2,"_R_",r,".RData",sep = ""))

MSE.PC.MVAR.FE.list.nComp.2[r,p] = unlist(MSE.PC.MVAR.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 3 component
load(file = paste("MSE_PC_MVAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",3,"_R_",r,".RData",sep = ""))

MSE.PC.MVAR.FE.list.nComp.3[r,p] = unlist(MSE.PC.MVAR.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 4 component
load(file = paste("MSE_PC_MVAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",4,"_R_",r,".RData",sep = ""))

MSE.PC.MVAR.FE.list.nComp.4[r,p] = unlist(MSE.PC.MVAR.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 5 component
load(file = paste("MSE_PC_MVAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",5,"_R_",r,".RData",sep = ""))

MSE.PC.MVAR.FE.list.nComp.5[r,p] = unlist(MSE.PC.MVAR.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 6 component
load(file = paste("MSE_PC_MVAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",6,"_R_",r,".RData",sep = ""))

MSE.PC.MVAR.FE.list.nComp.6[r,p] = unlist(MSE.PC.MVAR.Sys$MSE.Sys.MVAR[p])
}}

# Table MSE PC-MVAR

MSE.PC.MVAR.FE = matrix(0,2*(P-1),P)

MSE.PC.MVAR.FE[1,] = colMeans(MSE.PC.MVAR.FE.list.nComp.2)
MSE.PC.MVAR.FE[2,] = apply(MSE.PC.MVAR.FE.list.nComp.2,2,sd)/sqrt(R)

MSE.PC.MVAR.FE[3,] = colMeans(MSE.PC.MVAR.FE.list.nComp.3)
MSE.PC.MVAR.FE[4,] = apply(MSE.PC.MVAR.FE.list.nComp.3,2,sd)/sqrt(R)

MSE.PC.MVAR.FE[5,] = colMeans(MSE.PC.MVAR.FE.list.nComp.4)
MSE.PC.MVAR.FE[6,] = apply(MSE.PC.MVAR.FE.list.nComp.4,2,sd)/sqrt(R)

MSE.PC.MVAR.FE[7,] = colMeans(MSE.PC.MVAR.FE.list.nComp.5)
MSE.PC.MVAR.FE[8,] = apply(MSE.PC.MVAR.FE.list.nComp.5,2,sd)/sqrt(R)

MSE.PC.MVAR.FE[9,] = colMeans(MSE.PC.MVAR.FE.list.nComp.6)
MSE.PC.MVAR.FE[10,] = apply(MSE.PC.MVAR.FE.list.nComp.6,2,sd)/sqrt(R)

colnames(MSE.PC.MVAR.FE) = c("Y1","Y2","Y3","Y4","Y5","Y6")
rownames(MSE.PC.MVAR.FE) = c("PC.MVAR.FE.Comp.2","","PC.MVAR.FE.Comp.3","",
"PC.MVAR.FE.Comp.4","","PC.MVAR.FE.Comp.5","","PC.MVAR.FE.Comp.6","")

# Table for overall MSE

# Table MSE PC-MVAR Mean

MSE.PC.MVAR.FE.Mean = matrix(0,2*(P-1),1)

MSE.PC.MVAR.FE.Mean[1,1] = mean(rowMeans(MSE.PC.MVAR.FE.list.nComp.2))
MSE.PC.MVAR.FE.Mean[2,1] = sd(rowMeans(MSE.PC.MVAR.FE.list.nComp.2))/sqrt(R)

MSE.PC.MVAR.FE.Mean[3,1] = mean(rowMeans(MSE.PC.MVAR.FE.list.nComp.3))
MSE.PC.MVAR.FE.Mean[4,1] = sd(rowMeans(MSE.PC.MVAR.FE.list.nComp.3))/sqrt(R)

MSE.PC.MVAR.FE.Mean[5,1] = mean(rowMeans(MSE.PC.MVAR.FE.list.nComp.4))
MSE.PC.MVAR.FE.Mean[6,1] = sd(rowMeans(MSE.PC.MVAR.FE.list.nComp.4))/sqrt(R)

MSE.PC.MVAR.FE.Mean[7,1] = mean(rowMeans(MSE.PC.MVAR.FE.list.nComp.5))
MSE.PC.MVAR.FE.Mean[8,1] = sd(rowMeans(MSE.PC.MVAR.FE.list.nComp.5))/sqrt(R)

MSE.PC.MVAR.FE.Mean[9,1] = mean(rowMeans(MSE.PC.MVAR.FE.list.nComp.6))
MSE.PC.MVAR.FE.Mean[10,1] = sd(rowMeans(MSE.PC.MVAR.FE.list.nComp.6))/sqrt(R)

colnames(MSE.PC.MVAR.FE.Mean) = c("Mean")
rownames(MSE.PC.MVAR.FE.Mean) = c("PC.MVAR.FE.Comp.2","","PC.MVAR.FE.Comp.3","",
"PC.MVAR.FE.Comp.4","","PC.MVAR.FE.Comp.5","","PC.MVAR.FE.Comp.6","")

MSE.PC.MVAR.FE = cbind(MSE.PC.MVAR.FE,MSE.PC.MVAR.FE.Mean)

######################################################################################
######################################################################################
######################################################################################

# Tables for PC-MVAR(1) - Random Effects

MSE.PC.MVAR.RE.list.nComp.2 = matrix(0,R,P)
MSE.PC.MVAR.RE.list.nComp.3 = matrix(0,R,P)
MSE.PC.MVAR.RE.list.nComp.4 = matrix(0,R,P)
MSE.PC.MVAR.RE.list.nComp.5 = matrix(0,R,P)
MSE.PC.MVAR.RE.list.nComp.6 = matrix(0,R,P)

for (p in 1:P){

# Load block CV resutls
for (r in 1:R){

# Load PC-VAR with 2 component
load(file = paste("MSE_PC_MVAR_RE_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",2,"_R_",r,".RData",sep = ""))

MSE.PC.MVAR.RE.list.nComp.2[r,p] = unlist(MSE.PC.MVAR.RE.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 3 component
load(file = paste("MSE_PC_MVAR_RE_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",3,"_R_",r,".RData",sep = ""))

MSE.PC.MVAR.RE.list.nComp.3[r,p] = unlist(MSE.PC.MVAR.RE.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 4 component
load(file = paste("MSE_PC_MVAR_RE_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",4,"_R_",r,".RData",sep = ""))

MSE.PC.MVAR.RE.list.nComp.4[r,p] = unlist(MSE.PC.MVAR.RE.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 5 component
load(file = paste("MSE_PC_MVAR_RE_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",5,"_R_",r,".RData",sep = ""))

MSE.PC.MVAR.RE.list.nComp.5[r,p] = unlist(MSE.PC.MVAR.RE.Sys$MSE.Sys.MVAR[p])

# Load PC-VAR with 6 component
load(file = paste("MSE_PC_MVAR_RE_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_nComp_",6,"_R_",r,".RData",sep = ""))

MSE.PC.MVAR.RE.list.nComp.6[r,p] = unlist(MSE.PC.MVAR.RE.Sys$MSE.Sys.MVAR[p])
}}

# Table MSE PC-MVAR

MSE.PC.MVAR.RE = matrix(0,2*(P-1),P)

MSE.PC.MVAR.RE[1,] = colMeans(MSE.PC.MVAR.RE.list.nComp.2)
MSE.PC.MVAR.RE[2,] = apply(MSE.PC.MVAR.RE.list.nComp.2,2,sd)/sqrt(R)

MSE.PC.MVAR.RE[3,] = colMeans(MSE.PC.MVAR.RE.list.nComp.3)
MSE.PC.MVAR.RE[4,] = apply(MSE.PC.MVAR.RE.list.nComp.3,2,sd)/sqrt(R)

MSE.PC.MVAR.RE[5,] = colMeans(MSE.PC.MVAR.RE.list.nComp.4)
MSE.PC.MVAR.RE[6,] = apply(MSE.PC.MVAR.RE.list.nComp.4,2,sd)/sqrt(R)

MSE.PC.MVAR.RE[7,] = colMeans(MSE.PC.MVAR.RE.list.nComp.5)
MSE.PC.MVAR.RE[8,] = apply(MSE.PC.MVAR.RE.list.nComp.5,2,sd)/sqrt(R)

MSE.PC.MVAR.RE[9,] = colMeans(MSE.PC.MVAR.RE.list.nComp.6)
MSE.PC.MVAR.RE[10,] = apply(MSE.PC.MVAR.RE.list.nComp.6,2,sd)/sqrt(R)

colnames(MSE.PC.MVAR.RE) = c("Y1","Y2","Y3","Y4","Y5","Y6")
rownames(MSE.PC.MVAR.RE) = c("PC.MVAR.RE.Comp.2","","PC.MVAR.RE.Comp.3","",
"PC.MVAR.RE.Comp.4","","PC.MVAR.RE.Comp.5","","PC.MVAR.RE.Comp.6","")

# Table for overall MSE

# Table MSE PC-MVAR Mean

MSE.PC.MVAR.RE.Mean = matrix(0,2*(P-1),1)

MSE.PC.MVAR.RE.Mean[1,1] = mean(rowMeans(MSE.PC.MVAR.RE.list.nComp.2))
MSE.PC.MVAR.RE.Mean[2,1] = sd(rowMeans(MSE.PC.MVAR.RE.list.nComp.2))/sqrt(R)

MSE.PC.MVAR.RE.Mean[3,1] = mean(rowMeans(MSE.PC.MVAR.RE.list.nComp.3))
MSE.PC.MVAR.RE.Mean[4,1] = sd(rowMeans(MSE.PC.MVAR.RE.list.nComp.3))/sqrt(R)

MSE.PC.MVAR.RE.Mean[5,1] = mean(rowMeans(MSE.PC.MVAR.RE.list.nComp.4))
MSE.PC.MVAR.RE.Mean[6,1] = sd(rowMeans(MSE.PC.MVAR.RE.list.nComp.4))/sqrt(R)

MSE.PC.MVAR.RE.Mean[7,1] = mean(rowMeans(MSE.PC.MVAR.RE.list.nComp.5))
MSE.PC.MVAR.RE.Mean[8,1] = sd(rowMeans(MSE.PC.MVAR.RE.list.nComp.5))/sqrt(R)

MSE.PC.MVAR.RE.Mean[9,1] = mean(rowMeans(MSE.PC.MVAR.RE.list.nComp.6))
MSE.PC.MVAR.RE.Mean[10,1] = sd(rowMeans(MSE.PC.MVAR.RE.list.nComp.6))/sqrt(R)

colnames(MSE.PC.MVAR.RE.Mean) = c("Mean")
rownames(MSE.PC.MVAR.RE.Mean) = c("PC.MVAR.RE.Comp.2","","PC.MVAR.RE.Comp.3","",
"PC.MVAR.RE.Comp.4","","PC.MVAR.RE.Comp.5","","PC.MVAR.RE.Comp.6","")

MSE.PC.MVAR.RE = cbind(MSE.PC.MVAR.RE,MSE.PC.MVAR.RE.Mean)

######################################################################################
######################################################################################
######################################################################################

# Performance resutls

MSE.MAR.FE.list = matrix(0,R,P)
MSE.MAR.RE.list = matrix(0,R,P)
MSE.MVAR.FE.list = matrix(0,R,P)
MSE.MVAR.RE.list = matrix(0,R,P)
MSE.AR.list = matrix(0,R,P)
MSE.VAR.list = matrix(0,R,P)

for (p in 1:P){

# Load block CV resutls
for (r in 1:R){

# Multilevel models on original data (scale)
load(file = paste("MSE_MVAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_R_",r,".RData",sep = ""))

MSE.MAR.FE.list[r,p] = MSE.MVAR.Sys.PC[[1]][[p]]$MSE.MAR
MSE.MAR.RE.list[r,p] = MSE.MVAR.Sys.PC[[2]][[p]]$MSE.MAR
MSE.MVAR.FE.list[r,p] = MSE.MVAR.Sys.PC[[3]][[p]]$MSE.MVAR
MSE.MVAR.RE.list[r,p] = MSE.MVAR.Sys.PC[[4]][[p]]$MSE.MVAR

# AR model on scale original data
load(file = paste("MSE_AR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_R_",r,".RData",sep = ""))

MSE.AR.list[r,p] = unlist(MSE.AR.Sys.PC$MSE.Sys.AR[p])

# VAR model on scale original data 
load(file = paste("MSE_VAR_PC_MVAR_FE_N_",N[n],"_T_",T[t],"_Q_",Q[q],"_E_",e,"_R_",r,".RData",sep = ""))

MSE.VAR.list[r,p] = unlist(MSE.VAR.Sys.PC$MSE.Sys.MVAR[p])
}}

# Table MSE 

MSE.PC = matrix(0,2*6,P)

MSE.PC[1,] = colMeans(MSE.MAR.FE.list)
MSE.PC[2,] = apply(MSE.MAR.FE.list,2,sd)/sqrt(R)

MSE.PC[3,] = colMeans(MSE.MAR.RE.list)
MSE.PC[4,] = apply(MSE.MAR.RE.list,2,sd)/sqrt(R)

MSE.PC[5,] = colMeans(MSE.MVAR.FE.list)
MSE.PC[6,] = apply(MSE.MVAR.FE.list,2,sd)/sqrt(R)

MSE.PC[7,] = colMeans(MSE.MVAR.RE.list)
MSE.PC[8,] = apply(MSE.MVAR.RE.list,2,sd)/sqrt(R)

MSE.PC[9,] = colMeans(MSE.AR.list)
MSE.PC[10,] = apply(MSE.AR.list,2,sd)/sqrt(R)

MSE.PC[11,] = colMeans(MSE.VAR.list)
MSE.PC[12,] = apply(MSE.VAR.list,2,sd)/sqrt(R)

colnames(MSE.PC) = c("Y1","Y2","Y3","Y4","Y5","Y6")
rownames(MSE.PC) = c('MAR.FE','','MAR.RE','','MVAR.FE','','MVAR.RE','','AR','',
'VAR','')

# Table for overall MSE

# Table MSE prediction error

MSE.Mean = matrix(0,2*6,1)

MSE.Mean[1,1] = mean(rowMeans(MSE.MAR.FE.list))
MSE.Mean[2,1] = sd(rowMeans(MSE.MAR.FE.list))/sqrt(R)

MSE.Mean[3,1] = mean(rowMeans(MSE.MAR.RE.list))
MSE.Mean[4,1] = sd(rowMeans(MSE.MAR.RE.list))/sqrt(R)

MSE.Mean[5,1] = mean(rowMeans(MSE.MVAR.FE.list))
MSE.Mean[6,1] = sd(rowMeans(MSE.MVAR.FE.list))/sqrt(R)

MSE.Mean[7,1] = mean(rowMeans(MSE.MVAR.RE.list))
MSE.Mean[8,1] = sd(rowMeans(MSE.MVAR.RE.list))/sqrt(R)

MSE.Mean[9,1] = mean(rowMeans(MSE.AR.list))
MSE.Mean[10,1] = sd(rowMeans(MSE.AR.list))/sqrt(R)

MSE.Mean[11,1] = mean(rowMeans(MSE.VAR.list))
MSE.Mean[12,1] = sd(rowMeans(MSE.VAR.list))/sqrt(R)


colnames(MSE.Mean) = c("Mean")
rownames(MSE.Mean) = c('MAR.FE','','MAR.RE','','MVAR.FE','','MVAR.RE','','AR','',
'VAR','')

MSE.PC = cbind(MSE.PC,MSE.Mean)

######################################################################################
######################################################################################
######################################################################################

MSE.sd = rbind(MSE.PC.VAR,MSE.PC.MAR.FE,MSE.PC.MAR.RE,MSE.PC.MVAR.FE,MSE.PC.MVAR.RE,MSE.PC)
nrow.MSE = c(1:68)
MSE = MSE.sd[which((nrow.MSE%%2)==1),]

return(list(MSE.sd=MSE.sd,MSE=MSE))
}