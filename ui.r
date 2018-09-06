library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(RMySQL)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(raster)
library(ggmap)
library(shinydashboard)
#database connection...
options(mysql = list(
  "host" = "localhost",
  "port" = 3306,
  "user" = "root",
  "password" = ""
))
databaseName <- "myshinydatabase"
table <- "responses"
db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                port = options()$mysql$port, user = options()$mysql$user,
                password = options()$mysql$password)
#function that gets the selected project name
manenoz<-function(){
  query <- sprintf("SELECT name FROM %s ", table)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}
# data2 <- reactive({
#   file1 <- input$file
#   if(is.null(file1)){return()} 
#   accdata <<- read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
#   
#   
# })
#update trigger on the project(s) selectinput
observe({
  updateSelectInput(session,"names",choices = manenoz())
})
#set data variable to use for maps
# observeEvent(
#   input$button,{
#     datamap <<- data2()
#   }
# )
shinyUI(
  dashboardPage(title="TRSD", skin = "red",
  dashboardHeader(title = "TRSD"
    ),
  dashboardSidebar(
            sidebarSearchForm("searchText","buttonSearch","Search"),
        sidebarMenu(    
    menuItem("Traffic officer Dashboard", tabName = "dashboard"),
       menuItem("Project asessement", tabName = "finance"),
    menuItem("Maps visualisation", tabName = "maps")
      )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width=12,height=7,
                fileInput("file","Upload the file"), # fileinput() function is used to get the file upload contorl option
                helpText("Default max. file size is 5MB"),
                tags$hr(),
                h5(helpText("Select the read.table parameters below")),
                checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                br(),
                radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
              # selectInput("ngear","select the comparison element",c("Time"="Time..24hr.","Road surface"="Road.Surface","Lighting conditions"="Lighting.Conditions","Accident Date"="Accident.Date","Weather Conditions"="Weather.Conditions"))
                )),
                fluidRow(
                uiOutput("tb")
              )
                    ),
      tabItem(tabName="finance",
              
                
               fluidRow(
                 column(width=12,height=7,
            
              selectInput("names", "Select the project name", choices=manenoz())
              #valueBoxOutput("getdate")
              
              )),
              
                uiOutput("pj")
              
              
                
                
                
                ),
      tabItem(tabName="maps",
              # fluidRow(
              #   actionButton("button","Generate Map visualisation")
              # ),
              fluidRow(
                
                # Global style setting #
                style = "padding-top: 10px; padding-bottom: 50px; font-family: times new roman;",
                
                # The main Menu Naviagation bar for all pages #
                # navbarPage(
                #   
                #   title = "Houston Viloent Crime Analysis Map FY 2010",
                #   position = "static-top",
                  
                  # # Menu 1 #
                  # tabPanel("HoustonMap", 
                  #          icon = icon("map-marker"),
                           div(class="outer",
                               tags$style(type = "text/css",
                                          ".outer {position: fixed; top: 50px; left: 0; right: 0;
                                          bottom: 0; overflow: hidden; padding: 0}"),
                               
                               leafletOutput("map", width="100%", height="100%"),
                               
                               # Floating panel on the right hand sdie.
                               absolutePanel(top = 30, right = 30, draggable=TRUE,
                                             wellPanel(style = "background-color: #ffffff; width: 350px",
                                                       
                                                       # Selection options
                                                       selectizeInput('casualtyfilter', 'Filter by casualty Severity:',
                                                                      #choices = c("All","Serious","Serious","Fatal")
                                                                      choices = c("All", unique(as.character(accdata$Casualty.Severity))) 
                                                                      
                                                       ),
                                                       checkboxInput('mapCluster', 'Cluster results?')
                                                             # selectizeInput('vehicleFilter', 'Select the type of vehicle:',
                                                       #                choices = c("All", unique(as.character(accdata$Type.of.Vehicle)))
                                                       # )
                                                       #2nd selectInput close
                                             )#wellPanel close
                               )#absolutePanel close
                               )#div close
                           
                           
                  )#tabPanel close
                       
                      
                       
                      ))
             
            )
          )
    )
    
  


