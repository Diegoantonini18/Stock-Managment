library(dplyr)
library(tibble)
library(ggplot2)
library(shiny)
partsPerPadron=read.csv("partsPerPadron.csv",T,",")
partData=read.csv("partData.csv",T,",")
partDemand=read.csv("partDemand.csv",T,",")

#Filtro mis piezas por mi padrón

mypartsid= partsPerPadron %>%
  filter(ends.13 ==1)%>% 
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
mypartsdemandtrans=as.data.frame(t(as.matrix(mypartsdemand[,-1])),row.names = 1:500)
names(mypartsdemandtrans)=mypartsdata$id

#Realizo un modelo de regresion lineal para predecir la demanda de los proximos 100 dias
demand_sd_predict=matrix(nrow=1000,ncol=2)
daily_predict_demand=data.frame(days=501:600)

for (id in 1:1000) {
  days=c(1:500)
  demand_for_each_ID=c(mypartsdemandtrans[,id])
  model = lm(demand_for_each_ID ~ days)
  demand_predict=predict(model,newdata = data.frame(days=501:600))
  demand_predict_1=as.data.frame(demand_predict)
  total_demand = colSums(demand_predict_1)*3
  sd=sigma(model)
  daily_predict_demand=daily_predict_demand %>%
    add_column("dia"=c(demand_predict))
  demand_sd_predict[id,2]=sd
  demand_sd_predict[id,1]=total_demand
}
daily_predict_demand=daily_predict_demand[,-1]


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

##1
qalmacen=matrix(nrow=100,ncol=1000)
qalmacen=data.frame(rbind(mypartsdata$initialStock,qalmacen))
names(qalmacen)=mypartsdata$id
deliver_counter=matrix(rep(0),nrow=1,ncol=1000)
daily_cost=matrix(nrow=100,ncol=1000)

for (day_simulation in 1:100) {
  for (ids in 1:1000) {
    qrestante=qalmacen[day_simulation,ids]-daily_predict_demand[day_simulation,ids]
    qalmacen[day_simulation+1,ids]=qrestante
    if (qrestante <= model_data_frame[ids,5]) {
      deliver_counter[1,ids]=deliver_counter[1,ids] + 1
      if (deliver_counter[1,ids]==mypartsdata[ids,5]+1) {
        deliver_counter[1,ids]=0
        qalmacen[day_simulation+1,ids]=qalmacen[day_simulation,ids]+model_data_frame[ids,8]
      }
    }
  }
}

costos_diarios=matrix(nrow=101,ncol=1000)
for (z in 1:101) {
  for (y in 1:1000) {
    if (qalmacen[z,y]>=0) {
      costos_diarios[z,y]=qalmacen[z,y]*mypartsdata[y,3]/300
    }else{
      costos_diarios[z,y]=qalmacen[z,y]*(-mypartsdata[y,4]/300)
    }
  }
}
costos_totales2=data.frame(rowSums(costos_diarios))





ui=fluidPage(
  titlePanel("TP STOCKS - ANTONINI"),
  selectInput(inputId="ID",label="Select an id",choices = colnames(mypartsdemandtrans)),
  splitLayout(
    plotOutput("plot_demanda"),
    plotOutput("histogram"),
    plotOutput("plot_stock")
)
)
server=function(input,output){
  output$plot_demanda=renderPlot({
    data= mypartsdemandtrans %>%
      select(input$ID)
    data=unlist(data)
    ggplot(mypartsdemandtrans,aes(x=1:500, y = data))+
      geom_line(color="red")+
      geom_smooth(formula=y ~ x, method = "lm",color="blue")+
      labs(title="Demanda Historica",x="Dias",y="Demanda")+
      theme(plot.title = element_text(size=18))
    
  })
  output$histogram=renderPlot({
    data= mypartsdemandtrans %>%
      select(input$ID)
    data=as.data.frame(data)
    ggplot(mypartsdemandtrans,aes (x = data[,1]))+
      geom_histogram(fill="red", col="blue",binwidth = 1)+
      labs(title="Histograma Demanda Historica",x="Dias",y="Frecuencia")+
      theme(plot.title = element_text(size=18))
  })
  output$plot_stock=renderPlot({
    data= qalmacen %>%
      select(input$ID)
    data=unlist(data)
    ggplot(data=qalmacen,aes(x=500:600,y=data))+geom_line(color="blue")+theme_bw()+
      labs(title="Simulacion Stocks Próximos 100 Dias",x="Dias",y="Stock")+
      theme(plot.title = element_text(size=18))
    
  })
  
  
  
}

shinyApp(ui=ui, server=server)

