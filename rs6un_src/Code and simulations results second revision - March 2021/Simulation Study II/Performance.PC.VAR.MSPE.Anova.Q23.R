#################################################################
##########         R Code by Ginette Lafit        ###############
##########         ginette.lafit@kuleuven.be      ###############
#################################################################

Performance.PC.VAR.MSPE.Anova.Q23 = function(N,T,P,Q,E,Effect,R){

# Function to compute MSPE on scale data for MVAR
MSPE.Scale = function(fit.cv,data.list.Raw,p){

n.ID = unique(data.list.Raw[[3]])
fold = length(data.list.Raw[[1]])
data.train = data.list.Raw[[1]]
data.test = data.list.Raw[[2]]

MSE.k = list()

for (k in 1:fold){
names.Y = sprintf("Y%d",p)
y = data.test[[k]][,names.Y]
y.hat = matrix(unlist(fit.cv$MSE.k[[k]]$yhat.ki), ncol=1, byrow=F)
eps = ((y - y.hat)/sd(data.train[[k]][,names.Y]))^2
MSE.k[[k]] = mean(unlist(eps),na.rm=TRUE)
}
MSE = mean(unlist(MSE.k))
MSE.se = sd(unlist(MSE.k))/sqrt(fold)
MSE.1se = MSE + MSE.se

return(list(MSE=MSE,MSE.1se=MSE.1se))}

# Function MSPE on scaled data for VAR
MSPE.VAR.Scale = function(fit.cv,data.list.Raw,p){

n.ID = unique(data.list.Raw[[3]])
fold = length(data.list.Raw[[1]])
data.train = data.list.Raw[[1]]
data.test = data.list.Raw[[2]]

MSE.k = list()

for (k in 1:fold){
names.Y = sprintf("Y%d",p)
MSE.k[[k]] = fit.cv[[p]]/sd(data.train[[k]][,names.Y])^2
}
MSE = mean(unlist(MSE.k))
MSE.se = sd(unlist(MSE.k))/sqrt(fold)
MSE.1se = MSE + MSE.se

return(list(MSE=MSE,MSE.1se=MSE.1se))}

# Performance results

MSPE.list = expand.grid(id=1:R,Model=c('MAR.FE','MAR.RE','MVAR.FE','MVAR.RE','AR','VAR',
                       'PC.VAR','PC.MAR.FE','PC.MAR.RE','PC.MVAR.FE','PC.MVAR.RE'),
                       Variables=P,Factors=Q,Persons=N,Time=T,Error=E,Effect=Effect)

MSPE.list = cbind(MSPE.list,MSPE=rep(0,nrow(MSPE.list)),MSPE.1se=rep(0,nrow(MSPE.list)))
  
# Load block CV results

for (w in 1:nrow(MSPE.list)){

n = MSPE.list$Persons[w]
t = MSPE.list$Time[w]
p = MSPE.list$Variables[w]
r = MSPE.list$id[w]
e = MSPE.list$Error[w]
q = MSPE.list$Factors[w]
f = MSPE.list$Effect[w]
m = MSPE.list$Model[w]

# Multilevel models on original data 
if (m=='MAR.FE'){
# Load training and testing set
load(file = paste("Data_Block_Raw_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_nComp_",q,"_R_",r,".RData",sep = ""))
load(file = paste("MSE_MVAR_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_R_",r,".RData",sep = ""))
MSPE = lapply(1:p, function(h) MSPE.Scale(MSE.MVAR.Sys.PC[[1]][[h]],data.list.Raw,h))
MSPE.list[w,'MSPE'] = mean(unlist(lapply(1:p, function(h) MSPE[[h]]$MSE)))
MSPE.list[w,'MSPE.1se'] = mean(unlist(lapply(1:p, function(h) MSPE[[h]]$MSE.1se)))
}

if (m=='MAR.RE'){
load(file = paste("Data_Block_Raw_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_nComp_",q,"_R_",r,".RData",sep = ""))
load(file = paste("MSE_MVAR_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_R_",r,".RData",sep = ""))
MSPE = lapply(1:p, function(h) MSPE.Scale(MSE.MVAR.Sys.PC[[2]][[h]],data.list.Raw,h))
MSPE.list[w,'MSPE'] = mean(unlist(lapply(1:p, function(h) MSPE[[h]]$MSE)))
MSPE.list[w,'MSPE.1se'] = mean(unlist(lapply(1:p, function(h) MSPE[[h]]$MSE.1se)))
}

if (m=='MVAR.FE'){
load(file = paste("Data_Block_Raw_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_nComp_",q,"_R_",r,".RData",sep = ""))
load(file = paste("MSE_MVAR_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_R_",r,".RData",sep = ""))
MSPE = lapply(1:p, function(h) MSPE.Scale(MSE.MVAR.Sys.PC[[3]][[h]],data.list.Raw,h))
MSPE.list[w,'MSPE'] = mean(unlist(lapply(1:p, function(h) MSPE[[h]]$MSE)))
MSPE.list[w,'MSPE.1se'] = mean(unlist(lapply(1:p, function(h) MSPE[[h]]$MSE.1se)))
}

if (m=='MVAR.RE'){
load(file = paste("Data_Block_Raw_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_nComp_",q,"_R_",r,".RData",sep = ""))
load(file = paste("MSE_MVAR_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_R_",r,".RData",sep = ""))
MSPE = lapply(1:p, function(h) MSPE.Scale(MSE.MVAR.Sys.PC[[4]][[h]],data.list.Raw,h))
MSPE.list[w,'MSPE'] = mean(unlist(lapply(1:p, function(h) MSPE[[h]]$MSE)))
MSPE.list[w,'MSPE.1se'] = mean(unlist(lapply(1:p, function(h) MSPE[[h]]$MSE.1se)))
}

# VAR and AR models on original data
if (m=='AR'){
load(file = paste("Data_Block_Raw_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_nComp_",q,"_R_",r,".RData",sep = ""))
load(file = paste("MSE_AR_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_R_",r,".RData",sep = ""))
MSPE = lapply(1:p, function(h) MSPE.VAR.Scale(MSE.AR.Sys.PC$MSE.Sys.k,data.list.Raw,h))
MSPE.list[w,'MSPE'] = mean(unlist(lapply(1:p, function(h) MSPE[[h]]$MSE)))
MSPE.list[w,'MSPE.1se'] = mean(unlist(lapply(1:p, function(h) MSPE[[h]]$MSE.1se)))
}

if (m=='VAR'){
load(file = paste("Data_Block_Raw_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_nComp_",q,"_R_",r,".RData",sep = ""))
load(file = paste("MSE_VAR_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_R_",r,".RData",sep = ""))
MSPE = lapply(1:p, function(h) MSPE.VAR.Scale(MSE.VAR.Sys.PC$MSE.Sys.k,data.list.Raw,h))
MSPE.list[w,'MSPE'] = mean(unlist(lapply(1:p, function(h) MSPE[[h]]$MSE)))
MSPE.list[w,'MSPE.1se'] = mean(unlist(lapply(1:p, function(h) MSPE[[h]]$MSE.1se)))
}

if (m=='PC.VAR'){
load(file = paste("MSE_PC_VAR_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_nComp_",q,"_R_",r,".RData",sep = ""))
MSPE.list[w,'MSPE'] = mean(unlist(MSE.PC.VAR.Sys$MSE.Sys.MVAR))
MSPE.list[w,'MSPE.1se'] = mean(unlist(MSE.PC.VAR.Sys$MSE.Sys.MVAR.1se))
}

if (m=='PC.MAR.FE'){
load(file = paste("MSE_PC_MAR_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_nComp_",q,"_R_",r,".RData",sep = ""))
MSPE.list[w,'MSPE'] = mean(unlist(MSE.PC.MAR.Sys$MSE.Sys.MVAR))
MSPE.list[w,'MSPE.1se'] = mean(unlist(MSE.PC.MAR.Sys$MSE.Sys.MVAR.1se))
}

if (m=='PC.MAR.RE'){
load(file = paste("MSE_PC_MAR_RE_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_nComp_",q,"_R_",r,".RData",sep = ""))
MSPE.list[w,'MSPE'] = mean(unlist(MSE.PC.MAR.RE.Sys$MSE.Sys.MVAR))
MSPE.list[w,'MSPE.1se'] = mean(unlist(MSE.PC.MAR.RE.Sys$MSE.Sys.MVAR.1se))
}

if (m=='PC.MVAR.FE'){
load(file = paste("MSE_PC_MVAR_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_nComp_",q,"_R_",r,".RData",sep = ""))
MSPE.list[w,'MSPE'] = mean(unlist(MSE.PC.MVAR.Sys$MSE.Sys.MVAR))
MSPE.list[w,'MSPE.1se'] = mean(unlist(MSE.PC.MVAR.Sys$MSE.Sys.MVAR.1se))
}

if (m=='PC.MVAR.RE'){
load(file = paste("MSE_PC_MVAR_RE_Effect_",f,"_N_",n,"_T_",t,"_Q_",q,"_E_",e,"_nComp_",q,"_R_",r,".RData",sep = ""))
MSPE.list[w,'MSPE'] = mean(unlist(MSE.PC.MVAR.RE.Sys$MSE.Sys.MVAR))
MSPE.list[w,'MSPE.1se'] = mean(unlist(MSE.PC.MVAR.RE.Sys$MSE.Sys.MVAR.1se))
}}

######################################################################################
######################################################################################
######################################################################################

return(MSPE.list=MSPE.list)
}