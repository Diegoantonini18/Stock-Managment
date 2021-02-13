############################ DIEGO MARTIN ANTONINI ##################################
############################ PADRON: 100.013 ########################################
 
############################# LIBRERIA UTILIZADA ##################################
library(dplyr)

############################## LECTURA DE ARCHIVOS ###################################################
partsPerPadron=read.csv("partsPerPadron.csv",T,",")
partData=read.csv("partData.csv",T,",")
partDemand=read.csv("partDemand.csv",T,",")

solution=function(padron){
  #Filtro mis piezas por mi padrón
  nro_padron=paste("ends.",padron,sep="")
  mypartsid= partsPerPadron %>%
    filter(partsPerPadron[nro_padron] ==1)%>% 
    select(id)
  
  #Filtro la data de mis piezas
  mypartsdata=partData %>%
    filter(id %in% mypartsid$id)
  
  #Filtro la demanda de mis piezas
  mypartsdemand=partDemand %>%
    filter(id %in% mypartsid$id)
  
  #Volumen total de mi almacen
  partial_volume=data.frame(x=c(mypartsdata$unitVolume*mypartsdata$initialStock))
  total_volume=colSums(partial_volume)
  
  #Rearmo el data frame para poder trabajar mas facil
  mypartsdemand=as.data.frame(t(as.matrix(mypartsdemand[,-1])),row.names = 1:500)
  names(mypartsdemand)=mypartsdata$id
  
  #Realizo un modelo de regresion lineal para predecir la demanda de los proximos 100 dias
  demand_sd_predict=matrix(nrow=1000,ncol=2)
  for (id in 1:1000) {
    days=c(1:500)
    demand_for_each_ID=c(mypartsdemand[,id])
    model = lm(demand_for_each_ID ~ days)
    demand_predict=predict(model,newdata = data.frame(days=501:600))
    demand_predict_1=as.data.frame(demand_predict)
    total_demand = colSums(demand_predict_1)*3
    sd=sigma(model)
    demand_sd_predict[id,2]=sd
    demand_sd_predict[id,1]=total_demand
  }
  
  ### UTILIZO EL MODELO DE REVISION CONTINUA Y COSTOS DE FALTANTE ###
  model_data_frame=data.frame(matrix(ncol=8,nrow=1000))
  names(model_data_frame)=c("ID","q","FSR","Z","ROP","L(z)","n(Sr)","Q")
  model_data_frame$ID=mypartsdata$id
  for (ids in 1:1000) {
    model_data_frame[ids,2]=sqrt(2*demand_sd_predict[ids,1]*mypartsdata[ids,2]/mypartsdata[ids,3])
    model_data_frame[ids,3]=(1-(mypartsdata[ids,3]*model_data_frame[ids,2])/(mypartsdata[ids,4]*demand_sd_predict[ids,1]))
    model_data_frame[ids,4]=qnorm(model_data_frame[ids,3])
    model_data_frame[ids,5]=round((demand_sd_predict[ids,1]/300)*mypartsdata[ids,5]+demand_sd_predict[ids,2]*model_data_frame[ids,4])
    model_data_frame[ids,6]=dnorm(model_data_frame[ids,4])+model_data_frame[ids,4]*(1-pnorm(model_data_frame[ids,4]))
    model_data_frame[ids,7]=model_data_frame[ids,6]*demand_sd_predict[ids,2]
    model_data_frame[ids,8]=sqrt(2*demand_sd_predict[ids,1]*(mypartsdata[ids,2]+mypartsdata[ids,4]*model_data_frame[ids,7])/mypartsdata[ids,3])
    
  }
  Stock_policy=data.frame(id=mypartsdata$id,ROP=c(model_data_frame[,5]),Q=c(round(model_data_frame[,8])))
  return(Stock_policy)
}

################### RESUELVO EL PROBLEMA INTRODUCIENDO MI PADRON #################
solution(13)

####### EXPORTO ARCHIVO CSV ########
write.csv(solution(13),paste(getwd(),"\\TP-STOCKS-ANTONINI.csv",sep=""), row.names = FALSE)



