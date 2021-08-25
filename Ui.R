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
library(git2r)

dashboardPage(
dashboardHeader(title = "Futures Contracts CME"  #,tags$li(div(img(src = 'chevy.jpg',
                #height = "30px"), style = "padding-top:10px; padding-bottom:10px;"), 
                #class = "dropdown")
                ),

dashboardSidebar(
sidebarMenu(
menuItem("File Inputs & Filters", tabName = "dashboard", icon = icon("dashboard")),


fileInput("file1", "Individual Contracts", multiple = TRUE,
buttonLabel = "Browse...", placeholder = "No File Selected"),

dateRangeInput("dates1", "Dates Range", start = "1970-01-01",
          end = "2099-01-01", format = "yyyy-mm-dd", startview = "year"),

textInput("Code1", "Contract Price Units", value = "(mu/cu)"),

menuItem("Trading Inputs & Filters", tabName = "dashboard2", icon = icon("dashboard")),

numericInput("Number1", "Buy (Days before expiration)", value = 40,
             min = 1, max = 60, step = 1),

numericInput("Number2", "Sell (Days before expiration)", value = 30,
             min = 1, max = 60, step = 1),

actionButton("RunCode", "Update")

)
),



dashboardBody(

  
tabItem(tabName = "dashboard",

    
tabsetPanel(
 
 tabPanel("JB Normal Test", 
          
 fluidRow(box(title="Split Contracts Returns Density", solidHeader = TRUE, 
 status="primary", width=12, plotlyOutput("graph3b"))
 ),
 
 fluidRow(box(title="Jarque Bera Test (5%) | Daily Log Returns", solidHeader = TRUE, 
              status="primary", width=12, DTOutput("table5"))
 ),
 
 
 fluidRow(
   
   downloadButton('download5', 'Download data')  
   
 )
 
 ),
 
 tabPanel("Log Returns",
 
 fluidRow(box(title="Split Contracts Returns", solidHeader = TRUE, 
 status="primary", width=12, plotlyOutput("graph2"))
 ),
 
 fluidRow(box(title="Split Contracts Returns Boxplot", solidHeader = TRUE, 
              status="primary", width=12, plotlyOutput("graph3a"))
 ),
 
 #fluidRow(box(title="Split Contracts Returns Histogram", solidHeader = TRUE, 
 #            status="primary", width=12, plotlyOutput("graph3b"))
 #),
 
 fluidRow(box(title="Rolled Contracts", solidHeader = TRUE, 
          status="primary", width=12, DTOutput("table4"))
 ),
 
 
 fluidRow(
   
   downloadButton('download4', 'Download data')  
   
 )                     
 
 ),
  
tabPanel("Continous Base",

fluidRow(box(title="Split Contracts Prices", solidHeader = TRUE, 
        status="primary", width=12, plotlyOutput("graph1"))
),

                  
fluidRow(box(title="Rolled Contracts", solidHeader = TRUE, 
            status="primary", width=12, DTOutput("table3"))
),


fluidRow(
  
  downloadButton('download3', 'Download data')  
  
)
 
                    
),        

                
tabPanel("Data",

fluidRow(box(title="Individual Contracts", solidHeader = TRUE, 
             status="primary", width=12, DTOutput("table2"))
),

fluidRow(
  
  downloadButton('download2', 'Download data')  
  
)

                     
),


tabPanel("Files",

fluidRow(

box(title="Uploaded Files", solidHeader = TRUE, 
    status="primary", width=12, DTOutput("table1"))
    
),

fluidRow(

downloadButton('download1', 'Download data')  
  
)



)))
))


