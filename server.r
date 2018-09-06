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
#connection to db
options(mysql = list(
  "host" = "localhost",
  "port" = 3306,
  "user" = "root",
  "password" = ""
))
databaseName <- "myshinydatabase"
table <- "responses"
# Define the fields we want to save from the form
fields <- c("name", "road", "date")
# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
 options(shiny.maxRequestSize = 9*1024^2)
 

shinyServer(function(input,output,session){
  
  
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  # When the Submit button is clicked, save the form data and update the selectinput
  observeEvent(input$submit, {
    saveData(formData())
    updateSelectInput(session,"names",choices = loadData())
  })
  #entries are loaded into responses from the loaddata function
  output$responses <- DT::renderDataTable({
    input$submit
    loadData()
  }) 
  saveData <- function(data) {
    # Connect to the database
    db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
    # Construct the update query by looping over the data fields
    query <- sprintf(
      "INSERT INTO %s (%s)  VALUES ('%s')",
      table, 
      paste(names(data), collapse = ", "),
      paste(data, collapse = "', '")
    )
    # Submit the update query and disconnect
    dbGetQuery(db, query)
    dbDisconnect(db)
  }
  
  
  loadData <- function() {
    # Connect to the database
    db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                    port = options()$mysql$port, user = options()$mysql$user,
                    password = options()$mysql$password)
    # Construct the fetching query
    query <- sprintf("SELECT name FROM %s ", table)
    # Submit the fetch query and disconnect
    data <- dbGetQuery(db, query)
    dbDisconnect(db)
    data
  }
  #data from the selected csv file is saved in variable data(comparison element)
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    accdata <- read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    # accdata[,c("Easting",input$ngear)]
    
  })
  #data from the selected csv file is saved in variable data
  data2 <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    accdata <- read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    
    
  })
  #maps visualisation
  # observeEvent(
  #   input$button,{
  #     datamap <<- data2()
  #   }
  # )
  #reactive function that get the single value element from the querry
  datayan <- reactive({
    conn <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "myshinydatabase",
      host = "localhost",
      username = "root",
      password = "")
    on.exit(dbDisconnect(conn), add = TRUE)
    sam1<<-dbGetQuery(conn, paste0(
      "SELECT Road AS '' FROM responses WHERE name='", input$names, "';"))
        returnValue(sam1)
  })
  #Trigger the value of namese after the names input
  observeEvent(input$names,{
    namese <<-datayan() 
  })
  #gets the table output from the input specific querry
  output$tbl <- renderTable({
    conn <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "myshinydatabase",
      host = "localhost",
      username = "root",
      password = "")
    on.exit(dbDisconnect(conn), add = TRUE)
    dbGetQuery(conn, paste0(
      "SELECT * FROM responses WHERE name='", input$names, "';"))
  })
  #this reactive output contains the summary of the dataset and display the summary in table format
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$file
  })
  
  #set the initial color palette
  CasualtyColor <- colorFactor(rainbow(7), accdata$Casualty.Severity)
  
  
  # Set Data based on the input selection #        
  fData <- reactive({
    data <- accdata
    
    
    if (input$casualtyfilter != "All"){
      data <- subset(data, Casualty.Severity %in% input$CasualtyFilter) 
    }
     # if (input$vehicleFilter != "All"){
     #   data <- subset(data, Type.of.Vehicle %in% input$vehicleFilter) 
     # }
    
    return(data)
    
    
  })
  
  
  # render map using the leaflet fucntion #  
  output$map <- renderLeaflet({
    
    leaflet(fData()) %>%
      addProviderTiles("CartoDB.Positron")  
    #%>%                       #setView(-95.4, 29.76, zoom = 10)
    #setView(53.79648,-1.54785)
  })              
  
  #set mapCluster variable based on input checkbox #
  mapClusterResult <- reactive({
    if(input$mapCluster){TRUE}
    else {NULL}
  })
  
  # update map based on changed inputs #
  observe({
    leafletProxy("map", data = fData()) %>%
      clearMarkers() %>%
      clearControls() %>%
      clearMarkerClusters %>%
      addCircleMarkers(
        stroke = FALSE, fillOpacity = 0.5, radius=6, color = ~CasualtyColor(Casualty.Severity),
        clusterOptions = mapClusterResult(),
        popup = ~paste("<strong>Road Surface:</strong>",Road.Surface,
                       "<br>",
                       "<strong>Weather Conditions:</strong>",Weather.Conditions,
                       "<br>",
                       "<strong>Lighting Conditions:</strong>",Lighting.Conditions)
      ) %>%
      addLegend(title = "Casualty Severity", position = "topleft",
                pal = CasualtyColor, values = ~Casualty.Severity, opacity = 1)
    
    
  })
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$sum <- renderTable({
    if(is.null(data())){return ()}
    summary(data())
    
  })
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  #accidents distribution histogram plot
  output$plotn <- renderPlot({
    if(is.null(data())){return ()}
    ggplot(data(), aes(x=Time..24hr.))+
      theme_bw()+
      geom_histogram(binwidth = NULL)+
      labs(y="accidents count",
           x="Time",
           title="Time Histogram")
    })
  #Lighting conditions factor plot
  output$plot3 <-renderPlot({
    file1 <- input$file
    if(is.null(file1)){return()} 
    accdata <- read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    ggplot(data = accdata, mapping = aes(x = Lighting.Conditions , fill = Casualty.Severity)) +
      theme_bw() +
      geom_bar() +
      labs(y = "casualty count",
           title = "Lighting Conditions")
    
  })
  #Road surface factor plot
  output$plot4 <-renderPlot({
    file1 <- input$file
    if(is.null(file1)){return()} 
    accdata <- read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    ggplot(data = accdata, mapping = aes(x = Road.Surface , fill = Casualty.Severity)) +
      theme_bw() +
      geom_bar() +
      labs(y = "casualty count",
           title = "Road Surface")
    
  })
  #Weather conditions factor plot
  output$plot5 <-renderPlot({
    file1 <- input$file
    if(is.null(file1)){return()} 
    accdata <- read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    ggplot(data = accdata, mapping = aes(x = Weather.Conditions , fill = Casualty.Severity)) +
      theme_bw() +
      geom_bar() +
      labs(y = "casualty count",
           title = "Weather Conditions")
    
  })
  #Histogram plot based on the seleted project(pre-)
  output$plothist <- renderPlot({
    file1 <- input$file
    if(is.null(file1)){return ()}
    accdata <- read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    attach(accdata)
    accdata$Accident.Date<- as.Date(accdata$Accident.Date)
    table2<- filter(accdata,(Accident.Date<input$names) & (Returned.address %in% namese))
    #ggplot(data = accdata, mapping = aes(x = Weather.Conditions , fill = Casualty.Severity)) +
    ggplot(data=table2, mapping=aes(x=Accident.Date))+
      theme_bw()+
      geom_histogram(binwidth = NULL)+
      labs(y="accidents count",
           x="Date",
           title="Accidents distribution")
  })
  #Histogram plot based on the seleted project(post-)
  output$plothist2 <- renderPlot({
    file1 <- input$file
    if(is.null(file1)){return ()}
    accdata <- read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    attach(accdata)
    accdata$Accident.Date<- as.Date(accdata$Accident.Date)
    table3<- filter(accdata,(Accident.Date>input$names) & (Returned.address%in%namese))
    #ggplot(data = accdata, mapping = aes(x = Weather.Conditions , fill = Casualty.Severity)) +
    ggplot(data=table3, mapping=aes(x=Accident.Date))+
      theme_bw()+
      geom_histogram(binwidth = NULL)+
      labs(y="accidents count",
           x="Date",
           title="Accidents distribution")
  })
  #casualty severity plot
  output$plot6 <- renderPlot({
    file1 <- input$file
    if(is.null(file1)){return()} 
    accdata <- read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    attach(accdata)
    ggplot(data=accdata, mapping=aes(x=Casualty.Severity, fill= Sex.of.Casualty))+
    theme_bw()+
    geom_bar()+  

      labs(y = "casualty count",
           title = "casualty severity level")
      })
  #Form to add a project
  output$projoadd<- renderUI({
    fixedRow(
      DT::dataTableOutput("responses", width = 300), tags$hr(),
      textInput("name", "PROJECT Name", ""),
      textInput("road", "Name of thr road", ""),
      dateInput("date", "Date of project completion",format = "mm/dd/yy"),
      actionButton("submit", "Submit")
    )
    
  })
  # the following renderUI(s) is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$try <- renderUI({
    if(is.null(data())){}
      #h5("Powered by", tags$img(src='RStudio-Ball.png', height=200, width=200))
    else
      tabsetPanel(tabPanel("File Details", valueBoxOutput("getdate"))) 
    
  })
  output$tb <- renderUI({
    if(is.null(data())){}
      #h5("Powered by", tags$img(src='RStudio-Ball.png', height=200, width=200))
    else
      tabsetPanel(tabPanel("File Details", tableOutput("filedf")),tabPanel("casualty severity",plotOutput("plot6")), tabPanel("Accident distribution",plotOutput("plotn")),tabPanel("Lighting Conditions", plotOutput("plot3")),tabPanel("Road Surface",plotOutput("plot4")),tabPanel("Weather Conditions",plotOutput("plot5")))
  })
  output$pj <- renderUI({
    if(is.null(data()))
      {}
    #h5("Powered by", tags$img(src='RStudio-Ball.png', height=200, width=200))
    else
      tabsetPanel(tabPanel("Data", tableOutput("tbl")),tabPanel("pre-project situation",plotOutput("plothist")),tabPanel("Post-project situation",plotOutput("plothist2")),tabPanel("add project",uiOutput("projoadd")))
  })
})