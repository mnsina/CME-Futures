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
library(shinyauthr)
library(rdrop2)


#0) Formula ajuste sidebar

convertMenuItem2 <- function(mi, tabName){
  mi$children[[1]]$attribs['data-toggle'] = "tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  return(mi)
}


#1) Header

Header <- dashboardHeader(title = "Futures Contracts CME",
          shiny::tags$li(shinyauthr::logoutUI("logout"),
          height = "5px", style = "padding: 8px;",
          class = "dropdown"))


#2) Sidebar 

Sidebar <- dashboardSidebar(
          sidebarMenu(id="Sidebar",
          
          convertMenuItem2(
          menuItem("File Inputs & Filters", tabName = "dashboard", icon = icon("cog", lib = "glyphicon"),
    
          fileInput("file1", "Individual Contracts", multiple = TRUE,
          buttonLabel = "Browse...", placeholder = "No File Selected"),
             
          dateRangeInput("dates1", "Dates Range", start = "1970-01-01",
          end = "2099-01-01", format = "yyyy-mm-dd", startview = "year"),
             
          textInput("Code1", "Contract Price Units", value = "(mu/cu)"),
             
          numericInput("Number1", "Buy (Days after roll)", value = 0,
          min = 0, max = 60, step = 1),
             
          numericInput("Number2", "Sell (Days before roll)", value = 0,
          min = 0, max = 60, step = 1)), tabName = "dashboard"),
    
          convertMenuItem2(
          menuItem("User Database", tabName = "dashboard2", icon = icon("user", lib = "glyphicon"),
          
          actionButton("Btn1", "Register")),tabName = "dashboard2")
    
)
)


#3.1) Tabs Dashboard (Tabset1)


Tab_1 <- tabPanel("Trading Results", fluidRow(box(title="Life of Contracts Returns Boxplot", solidHeader = TRUE, 
         status="primary", width=12, plotlyOutput("graph4a"))),
         
         fluidRow(box(title="Life of Contracts Returns", solidHeader = TRUE, 
         status="primary", width=12, DTOutput("table6"))),

         fluidRow(downloadButton('download6', 'Download data')))

Tab_2 <- tabPanel("JB Normal Test", fluidRow(box(title="Split Contracts Returns Density", solidHeader = TRUE, 
         status="primary", width=12, plotlyOutput("graph3b"))),
                
         fluidRow(box(title="Jarque Bera Test (5%) | Daily Log Returns", solidHeader = TRUE, 
         status="primary", width=12, DTOutput("table5"))),
        
         fluidRow(downloadButton('download5', 'Download data')))

Tab_3 <- tabPanel("Log Returns", fluidRow(box(title="Split Contracts Returns", solidHeader = TRUE, 
         status="primary", width=12, plotlyOutput("graph2"))),
                
         fluidRow(box(title="Split Contracts Returns Boxplot", solidHeader = TRUE, 
         status="primary", width=12, plotlyOutput("graph3a"))),
        
         fluidRow(box(title="Rolled Contracts", solidHeader = TRUE, 
         status="primary", width=12, DTOutput("table4"))),
          
         fluidRow(downloadButton('download4', 'Download data')))


Tab_4 <- tabPanel("Continous Base", fluidRow(box(title="Split Contracts Prices", solidHeader = TRUE, 
         status="primary", width=12, plotlyOutput("graph1"))),
                
         fluidRow(box(title="Rolled Contracts", solidHeader = TRUE, 
         status="primary", width=12, DTOutput("table3"))),
                
         fluidRow(downloadButton('download3', 'Download data')))


Tab_5 <- tabPanel("Data", fluidRow(box(title="Individual Contracts", solidHeader = TRUE, 
         status="primary", width=12, DTOutput("table2"))),
                
         fluidRow(downloadButton('download2', 'Download data')))


Tab_6 <- tabPanel("Files", fluidRow(box(title="Uploaded Files", solidHeader = TRUE, 
         status="primary", width=12, DTOutput("table1"))),
                
         fluidRow(downloadButton('download1', 'Download data')))


#3.2) Tabset panel1

Tabsetpanel1 <- tabsetPanel(id="tabs", Tab_1, Tab_2, Tab_3,
                Tab_4, Tab_5, Tab_6)


#3.3) Body Dashboard

Body <- dashboardBody(shinyauthr::loginUI("login"),
       tabItems(
       tabItem(tabName = "dashboard", Tabsetpanel1), 
       tabItem(tabName = "dashboard2")
        ))


#4) Orden Dashboard

dashboardPage(
  
Header,

Sidebar,

Body

)


