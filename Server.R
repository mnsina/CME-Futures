library(shiny)
library(shinydashboard)
library(ggplot2)
library(readxl)
library(sqldf)
library(DT)
library(rsconnect)
library(plotly)
library(data.table)
library(lubridate)
library(ggpubr)
library(TSA)

library(pdftools)
library(tm)
#library(git2r)

#1) Definir directorio

#setwd("C:/Users/wzm03j/Desktop/AppGM2")
#tokem: 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

#2) Tabular documentos seleccionados

function(input, output) {

  
file1_data<-reactive({
    
 req(input$file1)
 file1_data<-input$file1 
  
})

output$table1<-renderDT({

 file1_data()  
    
}, filter="top")    
  

output$download1 <- downloadHandler(
  filename = function() {
    paste("Files", ".csv", sep="")
  },
  content = function(file) {
    write.csv(file1_data(), file)
  }
)

#3) Consolidar contratos 

db_1<-reactive({
  
 req(input$file1)

 filenames<-input$file1
 filenames<-filenames$name
  
 db_aux<-rbindlist(lapply(input$file1$datapath, fread), idcol = "ContractName")
 setnames(db_aux, new=c("ContractName", "Date", "Open", "High", "Low", "Close", "Volume", "Interest"))
 
 db_aux<-db_aux[, ContractName := factor(ContractName, labels = basename(filenames))]
 db_aux<-db_aux[, ContractName := gsub(".csv$", "", ContractName)]
 db_aux<-db_aux[, ContractName := paste(substr(ContractName, 1, 2), substr(ContractName, nchar(ContractName)-4, nchar(ContractName)))]
 db_aux<-db_aux[, ContractName := gsub(" ", "", ContractName)]
 
 
 db_aux<-db_aux[, Date:=parse_date_time(Date, orders = "ymd")]
 db_aux<-db_aux[, Date:=as.Date(Date, "%y %m %d", origin = "1970-01-01")]
 
 db_aux<-db_aux[Date %between% input$dates1, ]
 
 
})



#4) Tabular documentos consolidados

output$table2<-renderDT({
  
 req(input$file1)
  
 db_1()
  
}, filter="top")  


output$download2 <- downloadHandler(
  filename = function() {
    paste("Files", ".csv", sep="")
  },
  content = function(file) {
    write.csv(db_1(), file)
  }
)


#5) Generar base con volume roll

db_2<-reactive({
  
 req(input$file1)
 db_aux2<-db_1()[,.SD[which.max(Volume)], by=Date]
 
 
 db_aux0<-db_1()
 
 
 for(j in 1:10){
 for(i in 1:25){
 
 db_aux2<-db_aux2[, ContractNameLag1:=shift(ContractName, i, type="lag")] 
 db_aux2<-db_aux2[, ContractNameLead1:=shift(ContractName, i, type="lead")]
 
 db_aux2<-db_aux2[, ContractName_x:=ifelse(ContractNameLag1!=ContractNameLead1 | is.na(ContractNameLag1)| is.na(ContractNameLead1), ContractName, ContractNameLead1)]
 db_aux2<-db_aux2[, ContractName:=ContractName_x]
 db_aux2<-db_aux2[, ContractName_x:=NULL]

 }
 }
 
 
 db_aux2<-db_aux0[db_aux2, on=.(ContractName, Date)]
 db_aux2<-db_aux2[, c(1, 2, 3:8)]
 
 colnames(db_aux2)<-c("ContractName", "Date", "Open", "High", "Low", "Close", "Volume", "Interest")
 db_aux2<-db_aux2[,Contract:=paste(substr(ContractName,1,2), substr(ContractName,nchar(ContractName),nchar(ContractName)), sep="_")]
 
 
 db_aux2
 
})


output$download3 <- downloadHandler(
  filename = function() {
    paste("Files", ".csv", sep="")
  },
  content = function(file) {
    write.csv(db_2()[,1:8], file)
  }
)


#6) Tabular documentos volume roll

output$table3<-renderDT({
  
  req(input$file1)
  
  db_2()[, 1:8]

  
}, filter="top")  



#6) Graficar documentos volume roll

output$graph1<-renderPlotly({
  
  req(input$file1)
  up_unit<-input$Code1
  
  db_2()[,Contract:=paste(substr(ContractName,1,2), substr(ContractName,nchar(ContractName),nchar(ContractName)), sep="_")]
  
  ggplotly(
  ggplot(data=db_2(), aes(x=Date, y=Close, 
  group=ContractName))
  +geom_line(aes(color=Contract))
  +theme(legend.position = "Top")
  +xlab("Date")+ylab(paste0("Settlement Price"," ", up_unit))
  +theme_classic()
  #+theme_minimal()
  
  )
  
})  



#7) Retorno diario log y var precio

db_3<-reactive({

 req(input$file1)
 
 db_2()[, ContractNameLag1:=shift(ContractName, 1, type="lag")]
 db_2()[, LogReturnLag1:=ifelse(ContractNameLag1==ContractName, log(Close/shift(Close, 1, type="lag")), NA)]
 db_2()[, PriceDifferenceLag1:=ifelse(ContractNameLag1==ContractName, Close-shift(Close, 1, type="lag"), NA)]
  
    
})



#8) Graficar Retorno diario log

output$graph2<-renderPlotly({
  
  req(input$file1)
  
  db_3()[,Contract:=paste(substr(ContractName,1,2), substr(ContractName,nchar(ContractName),nchar(ContractName)), sep="_")]
  
  ggplotly(
    ggplot(data=db_3(), aes(x=Date, y=LogReturnLag1, 
                            group=ContractName))
    +geom_line(aes(color=Contract))
    +theme(legend.position = "Top")
    +xlab("Date")+ylab("Daily Log Returns")
    +scale_y_continuous(breaks = seq(from=-0.1, to=0.1, by=0.02), limits = c(-0.1, 0.1))
    +theme_classic()
    #+theme_minimal()
    
  )
  
})  


#9) Graficar Retorno diario log boxplot

output$graph3a<-renderPlotly({
  
  req(input$file1)
  
  db_3()[,Contract:=paste(substr(ContractName,1,2), substr(ContractName,nchar(ContractName),nchar(ContractName)), sep="_")]
  
  #plot_ly(data=db_3(), x=~Contract, y=~LogReturnLag1, 
          #color=~Contract, type = "box") %>% 
          #layout(showlegend=FALSE)
  
  ggplotly(
    ggplot(data=db_3(), aes(x=Contract, y=LogReturnLag1, color=Contract,
                            group=ContractName))
    +geom_boxplot(outlier.colour = "red")
    +theme(legend.position = "none")
    +xlab("Contract")+ylab("Daily Log Returns")
    +scale_y_continuous(breaks = seq(from=-0.1, to=0.1, by=0.02), limits = c(-0.1, 0.1))
    +theme_classic()
    #+theme_minimal()
    
  )
  
})


#10) Graficar Retorno diario log densidad

output$graph3b<-renderPlotly({
  
  req(input$file1)
  
  db_3()[,Contract:=paste(substr(ContractName,1,2), substr(ContractName,nchar(ContractName),nchar(ContractName)), sep="_")]
  
  ggplotly(
    ggdensity(data=db_3(), x="LogReturnLag1", fill="Contract")
    +stat_overlay_normal_density(aes(color=Contract), linetype="dashed")
    +scale_x_continuous(breaks = seq(from=-0.2, to=0.2, by=0.05), limits = c(-0.2, 0.2))
    +scale_y_continuous(breaks = seq(from=0, to=80, by=10), limits = c(0, 80))
    +xlab("Daily Log Return")+ylab("Density Function (%)")
    +theme_classic()
    #+theme_minimal()
    
  )
  
})  



#11) Tabular retorno log

output$table4<-renderDT({
  
  req(input$file1)
  
  datatable(db_3()[, c(1:8, 11)], filter="top") %>% formatPercentage("LogReturnLag1", 1)

})


output$download4 <- downloadHandler(
  filename = function() {
    paste("Files", ".csv", sep="")
  },
  content = function(file) {
    write.csv(db_3()[, c(1:8, 11)], file)
  }
)


#12) Test de Jarque-Bera retornos diarios

db_4<-reactive({
  
  req(input$file1)
  
  aux4<-db_3()[, .(Kurtosis=kurtosis(na.omit(LogReturnLag1)), Skewness=skewness(na.omit(LogReturnLag1)), N1=.N), by = .(Contract, ContractName)]
 
  aux4[, Jarque_Bera:=ifelse(1-pchisq(N1*((1/24)*(Kurtosis^2)+(1/6)*(Skewness^2)),df=2)>=0.05, "NORMAL", "OTHER")]
  
  #db3()[, .N, by=.(Contract, Jarque_Bera)]
  
})


#13) Tabular Test JB

output$table5<-renderDT({
  
  req(input$file1)
  
  datatable(db_4(), filter="top") %>% formatRound(c("Skewness", "Kurtosis"), 1)
  
})


output$download5 <- downloadHandler(
  filename = function() {
    paste("Files", ".csv", sep="")
  },
  content = function(file) {
    write.csv(db_4(), file)
  }
)


#14) Tabular retornos contratos

db_5<-reactive({
  
  req(input$file1)
  
  aux5<-db_3()[, c("Index1", "Index2"):=.(1:.N-1, .N:1-1), by = .(ContractName)]
  aux5<-aux5[Index1==input$Number1 | Index2==input$Number2]
  
  aux5<-aux5[, ContractNameLag1:=shift(ContractName, 1, type="lag")]
  aux5<-aux5[, ContractReturn:=ifelse(ContractNameLag1==ContractName, Close/shift(Close, 1, type="lag")-1, NA)]
  
})


output$table6<-renderDT({
  
  req(input$file1)
  
  datatable(db_5()[,c(1:8,15)], filter="top")  %>% formatPercentage("ContractReturn", 1)
  
})


output$download6 <- downloadHandler(
  filename = function() {
    paste("Files", ".csv", sep="")
  },
  content = function(file) {
    write.csv(db_5()[,c(1:8,15)], file)
  }
)


#15) Graficar Retorno contratos

output$graph4a<-renderPlotly({
  
  req(input$file1)
  
  db_5()[!is.na(ContractReturn),]
  
  ggplotly(
    ggplot(data=db_5(), aes(x=Contract, y=ContractReturn, color=Contract,
                            group=ContractName))
    +geom_boxplot(outlier.colour = "red")
    +theme(legend.position = "none")
    +xlab("Contract")+ylab("Daily Log Returns")
    +scale_y_continuous(breaks = seq(from=-0.1, to=0.1, by=0.02), limits = c(-0.1, 0.1))
    +theme_classic()
    #+theme_minimal()
    
  )
  
})

 
} 