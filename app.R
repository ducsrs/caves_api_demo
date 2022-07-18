library(shiny)
library(dplyr)
library(readr)
library(shinydashboard)
library(plotly)
library(dashboardthemes)
library(ggrepel)
library(gridExtra)
library(tidyverse)
library(ggthemes)
library(fontawesome)
library(shinyalert)
Caves <- read_csv("~/Desktop/Conservation_Status/1tax_region_range_usesa - 1tax_region_range_usesa.csv") # Read in data 
names(Caves)[8] <- "G_status" # Change column name
names(Caves)[10] <- "R_status" #Change column name 

small_cave <- read_csv("~/Desktop/Conservation_Status/Cave Species Only - Sheet1.csv")

All_data <- read_csv("~/Desktop/Conservation_Status/Nature Serve All Species  - Sheet1.csv")


#Caves <- Caves %>% #Error in the data set quick fix 
  #mutate(G_status = ifelse(G_status == 'T1: Critically Imperiled', 'G1: Critically Imperiled', G_status))


Caves$G_status <- factor(Caves$G_status, #This color codes each categoty on the graphs 
                         levels = c("G1: Critically Imperiled",
                                    "G2: Imperiled",
                                    "G3: Vulnerable",
                                    "G4: Apparently Secure",
                                    "G5: Secure",
                                    "GH: Possibly Extinct",
                                    "GX: Presumed Extinct",
                                    "GNR: Unranked" ,
                                    "GU: Unrankable"))




# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Cave Species"),
  dashboardSidebar(
    sidebarMenu(id = "sidebarid",     
      menuItem(                     
        text = "Home",
        tabName = "home",
        icon = icon('home'),
        menuSubItem(        
          text = "About",
          tabName = "about"),
        menuSubItem(
          text = "NatureServe",
          tabName = "glance")),
      menuItem(
        text = "Global Status",
        tabName = "global",
        icon = icon("bar-chart-o")
      ),
      menuItem(
        text = "Map",
        tabName = "simplemap",
        icon = icon('map')
      ),
      menuItem(                      #This is the State tab
        text = "Conservation by State/Region",
        tabName = "state",
        icon = icon('bar-chart-o')),
      menuItem(                      #This is the map tab
        text = "Taxonomic Data",
        tabName = "map",
        icon = icon("bug"),
     menuSubItem(                    #Control panel for map/all input selections
       text = "Map Filter",
       tabName = 'filter'),
        selectInput(inputId  = 'status1',
                    label = "Select Global Status",
                    choices = sort(unique(Caves$G_status)),
                    selected = "G1: Critically Imperiled",
                    multiple = FALSE),
        selectInput(inputId = 'phylum1',
                    label = "Select Phylum",
                    choices = NULL),
        selectInput(inputId = 'class1',
                    label = "Select Class",
                    choices = NULL),
        selectInput(inputId = 'order1',
                    label = "Select Order",
                    choices = NULL),
        selectInput(inputId = 'family1',
                    label = "Select Family",
                    choices = NULL))
  
    )
      
        
      
      
      
      
        
      
    ),
 ######################### Dashboard Body ############################# 
  dashboardBody(
     tabItems(             #This is the about tab and all text/ 2 pie charts 
      tabItem(
        tabName = "about",
        fluidPage(
        fluidRow(h2("Synthesizing the Conservation Status of Cave Species"), align = 'center'),
        p("The available data on the conservation status of cave species across the United States and Canada has not been synthesized in over 20 years. This interactive dashboard provides an understanding of which species and their locations are the most vulnerable according to the current data we have collected from NatureServe, a non-profit organization dedicated to providing information on the conservation of all kinds of endangered species.", align = 'left'),
        p("*More information on NatureServe located in the next tab", align = 'left'),
        fluidRow(h3("Why do we care?"), align = 'left'),
        p("To understand the severity of cave species endangerment it is important to visualize how the safety of cave species compares to others. Shown below is a comparison of the two groups with G1-G3 representing species that are at risk. (Data collected from NatureServe 2022)", align = 'left'),
        br(),
        p("An astonishing 94% of the available cave species data collected from NatureServe lies within the G1-G3 range compared to 31% of all other species data available. This is the surface of the issue at hand, while other sections of this dashboard will highlight more specific details such as endangerment levels by region and taxonomy. For more information on terminology and data was obtained, visit the NatureServe section.", align = 'left'),
          fluidRow(column(6, plotOutput("distPlot6")),
                   column(6, plotOutput("distPlot7"))
                 
                 
      ))),
      tabItem(               #This tab is telling about nature serve along with the value boxes 
        tabName = "glance",
        fluidPage(
          tags$head(tags$style(HTML(".small-box {height: 90px}"))),
          fluidRow(h3("NatureServe - About the Data and its Meaning")),
          fluidRow(p("The number of species below under each global rank(G) has been collected from NatureServe. Not included below are several other ranks including GX: Presumed Extinct, GH: Possibly Extinct, GU: Unrankable, and GNR: Unranked. These four ranks make up a very small portion of the speices being analyzed, but are still included in all graphs and maps on the dashboard. *Definitions from NatureServe(2022)", align = 'left')),
          fluidRow(valueBox(716, "G1: Critically Imperiled",
          color = 'red', width = 7),
          fluidRow("Critically Imperiled — At very high risk of extinction or elimination due to very restricted range, very few populations or occurrences, very steep declines, very severe threats, or other factors.")),
          fluidRow(valueBox(183, "G2: Imperiled",
                            color = 'orange', width = 6),
                   p("Imperiled — At high risk of extinction or elimination due to restricted range, few populations or occurrences, steep declines, severe threats, or other factors.")),
          fluidRow(valueBox(119, "G3: Vulnerable",
                            color = 'yellow', width = 5),
                   p("Vulnerable — At moderate risk of extinction or elimination due to a fairly restricted range, relatively few populations or occurrences, recent and widespread declines, threats, or other factors.")),
          fluidRow(valueBox(37, "G4: Apparently Secure",
                            color = 'light-blue' , width = 4),
                   p("Apparently Secure — At fairly low risk of extinction or elimination due to an extensive range and/or many populations or occurrences, but with possible cause for some concern as a result of local recent declines, threats, or other factors.")),
          fluidRow(valueBox(36, "G5: Secure",
                            color = 'green', width = 3),
                   p("Secure — At very low risk of extinction or elimination due to a very extensive range, abundant populations or occurrences, and little to no concern from declines or threats.")))
      ),
      tabItem(
        tabName = "global",
        plotOutput("distPlot8"),
        br(),
      fluidRow(column(10, actionButton("add_graph", "Top 10 States"), actionButton("hexa", "Hexapods"), actionButton("myria", "Myriapods"),
                      actionButton("arach", "Arachnida"), actionButton("crust","Crustaceans"),
                      actionButton("vert","Vertebrates"), actionButton("gast","Gastropoda"),
                      actionButton("plat","Platyhelminthes"),actionButton("annel","Annelida")),
               column(8, plotOutput("plots"))
      )),
      
      tabItem(                 #This is the state bar chart select input 
        tabName = "state",
        selectInput(inputId = 'Region',
                    label = "Select Region/State",
                     choices = sort(unique(Caves$Region)),
                     selected = c('TN'),
                     multiple = FALSE),
        
          br(),
          br(),
          br(),
          br(),
                 plotlyOutput("distPlot"),
        fluidRow(h4("(Bar Plot) Use the drop down menu to select a state or region to display all cave species separated by global status.")),
       ),   #Bar plot 
      tabItem(    #Map showing only Global Status 
          tabName = "simplemap",
          selectInput(inputId  = 'Status',
                      label = "Select Global Status",
                      choices = sort(unique(Caves$G_status)),
                      selected = "G1: Critically Imperiled"),
      br(),
      br(),
      br(),
      br(),
      
            plotlyOutput("distPlot5"),
      fluidRow(h4("(Map) Use drop down menu to filter global status. Varying shades of red conveys a greater number of cave species under the chosen status."))), #Global Status Map 
                                               
      tabItem(
        tabName = "filter",
        fluidRow((plotlyOutput("distPlot4"))),    #Input for the map 
        fluidRow(column(7,h4("(Table) All species filtered by taxonomy are shown below. Filtering by global status and searching a state abbreviation will show all unique species located in that state under the respective global rank."))),
        (DT::dataTableOutput("table")))
)
)
)
   


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  shinyalert(     #This is the welcome screen 
    title = "<b>Welcome to the Cave Species Dashboard!</b>",
    text = "<b>This dashboard aims to allow you to visualize existing data on varying levels of endangerment of cave species that has been retrieved from NatureServe. No one has analyzed the existing data on endangered cave species accross the United States and Canada in approximately 20 years! For this reason, we took on the challenge in order to provide the public, and those who are interested, with the data that highlights why this is such an important topic.</b>",
    html = TRUE)
               
  ######################## This is for reactive drop-down menus ################
  status1 <- reactive({
    filter(Caves, G_status == input$status1)
  })
  observeEvent(status1(), {
    choices <- unique(status1()$Phylum)
    updateSelectInput(inputId = "phylum1", choices = choices) 
  })
  
  
  phylum1 <- reactive({
    req(input$phylum1)
    filter(status1(), Phylum == input$phylum1)
  })
  observeEvent(phylum1(), {
    choices <- unique(phylum1()$Class)
    updateSelectInput(inputId = "class1", choices = choices) 
  })
  
  class1 <- reactive({
    req(input$class1)
    filter(status1(), Class == input$class1)
  })
  observeEvent(class1(), {
    choices <- unique(class1()$Order)
    updateSelectInput(inputId = "order1", choices = choices)
  })
  
  order1 <- reactive({
    req(input$order1)
    filter(status1(), Order == input$order1)
  })
  observeEvent(order1(), {
    choices <- unique(order1()$Family)
    updateSelectInput(inputId = "family1", choices = choices)
  })
  
 ####################################################################### 
  
  
##################### All reactive values for all of the plots/maps #############
  rv <- reactiveValues()
  
  observe({
    rv$cave <-  Caves %>% filter(Region %in% input$Region) %>% 
      group_by(G_status) %>% tally
    
    rv$map <- Caves %>% filter(G_status %in% input$status1, Phylum %in% input$phylum1, Class %in% input$class1, Order %in% input$order1,
                               Family %in% input$family1) %>% 
      group_by(Region) %>% tally
    
    rv$simplemap <- Caves %>% filter(G_status %in% input$Status) %>% 
      group_by(Region) %>% tally
    
    rv$table <- Caves %>% select(c(Species,Region,Kingdom,Phylum,Class,Order,Family,G_status)) %>% 
      filter(G_status %in% input$status1, Phylum %in% input$phylum1, Class %in% input$class1, Order %in% input$order1,
                                 Family %in% input$family1) 
   
  ######################################################################  
     
  })
  
  
  output$distPlot <- renderPlotly({
    
    
    
    #stat_df <- Caves %>% group_by(G_status) %>% tally
    
    
    ggplot(data = rv$cave,
           aes(x = G_status, y = n, fill = G_status)) +
      geom_col() +
      scale_x_discrete(drop = FALSE) +
      scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                    "G2: Imperiled" = 'darkorange3',
                                    "G3: Vulnerable" = 'darkgoldenrod2',
                                    "G4: Apparently Secure" = 'chartreuse4',
                                    "G5: Secure" = 'darkgreen',
                                    "GH: Possibly Extinct" = 'black',
                                    "GX: Presumed Extinct" = 'darkslategray4',
                                    "GNR: Unranked" = 'darkslategray',
                                    "GU: Unrankable" = 'darkgrey'
      )) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      labs(x = 'Global Status', y = 'Number of Species', title = "Number of Species per Rank") +
      theme(plot.background = element_rect(fill='transparent', color=NA)) +
      theme(legend.position = 'none') 
      
    
    
    
    
  })
  
  output$distPlot4 <- renderPlotly({         #This is code for the map
    
    leg.txt <- "Number of Species"
   
     plot_geo(rv$map,
             locationmode = 'USA-states') %>% 
      add_trace(locations = ~Region,
                z = ~n, 
                color = ~n,
                colorscale = ifelse(input$status1 == "G1: Critically Imperiled" ,'Reds',     #This is how you get the colors
                              ifelse(input$status1 == "G2: Imperiled" , 'Reds',
                              ifelse(input$status1 == "G3: Vulnerable" ,'Reds',
                              ifelse(input$status1 == "G4: Apparently Secure" ,'Greens',
                              ifelse(input$status1 == "G5: Secure" ,'Greens',
                              ifelse(input$status1 == "GH: Possibly Extinct" ,'Blackbody',
                              ifelse(input$status1 == "GX: Presumed Extinct" ,'Blackbody',
                              ifelse(input$status1 == "GNR: Unranked" ,'Blues',
                              ifelse(input$status1 == "GU: Unrankable" ,'Hot'))))))))),
                colorbar = list(title = "Number of Species")
      ) %>%  
      layout(geo = list(scope = 'usa'),
             font = list(family = "DM Sans"),
             title = "Global Status in the United States \n by Phylum, Class, Order, and Family",
             plot_bgcolor='rgb(0, 0, 0)') %>%  
      config(displayModeBar = FALSE) 
    
    
    
    
  })
  
  output$table <- DT::renderDataTable(options = list(pageLength = 5,
                                                     scrollY = TRUE,
                                                     scrollX = TRUE,
                                                     autoWidth = FALSE),{  #This is the data table underneath the map 
    
    rv$table
    
  })
  
  output$distPlot5 <- renderPlotly({         #This is code for the map
    
    mytext <- paste(
      "State: ", Caves$Region,"<br/>",
      "Status: ", Caves$G_status, "<br/>",
       
      sep="") %>%
      lapply(htmltools::HTML)
    
    
    plot_geo(rv$simplemap,
             locationmode = 'USA-states') %>% 
      add_trace(locations = ~Region,
                z = ~n,
                color = ~n,
                colorscale = ifelse(input$Status == "G1: Critically Imperiled" ,'Reds',     #This is how you get the colors
                            ifelse(input$Status == "G2: Imperiled" , 'Reds',
                            ifelse(input$Status == "G3: Vulnerable" ,'Reds',
                             ifelse(input$Status == "G4: Apparently Secure" ,'Greens',
                             ifelse(input$Status == "G5: Secure" ,'Greens',
                             ifelse(input$Status == "GH: Possibly Extinct" ,'Blackbody',
                               ifelse(input$Status == "GX: Presumed Extinct" ,'Blackbody',
                               ifelse(input$Status == "GNR: Unranked" ,'Blues',
                            ifelse(input$Status == "GU: Unrankable" ,'Hot'))))))))),
                colorbar = list(title = "Number of Species") 
                
      ) %>%
      
      layout(geo = list(scope = 'usa')) %>% 
      config(displayModeBar = FALSE)
    
    
    
    
  })
  
  output$distPlot6 <- renderPlot(height = 200,{  #This is one pie chart
  
  All_data <- All_data %>% mutate(pct = round((n/37551)*100))
    
  All_data2 <- All_data %>% 
    mutate(csum = rev(cumsum(rev(pct))), 
           pos = pct/2 + lead(csum, 1),
           pos = if_else(is.na(pos), pct/2, pos))
  
  ggplot(All_data, aes(x = "" , y = pct, fill = fct_inorder(G_status))) +
    geom_col(width = 1, color = 1) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = c('darkred','darkorange3',
                                 'darkgoldenrod2',
                                 'chartreuse4',
                                 'darkgreen')) +
    geom_label_repel(data = All_data2,
                     aes(y = pos, label = paste0(pct, "%")),
                     size = 4.5, nudge_x = 1, show.legend = FALSE) +
    labs(title = "All Non-Cave Species in NatureServe") +
    guides(fill = guide_legend(title = "Global Status")) +
    theme_void()
  
 
  
  
  }, bg="transparent")
  
  output$distPlot7 <- renderPlot(height = 200,{    #This is the second pie chart
    
    small_cave <- small_cave %>% mutate(pct = round((n/1091)*100))
    
    small_cave2 <- small_cave %>% 
      mutate(csum = rev(cumsum(rev(pct))), 
             pos = pct/2 + lead(csum, 1),
             pos = if_else(is.na(pos), pct/2, pos))
    
    ggplot(small_cave, aes(x = "" , y = pct, fill = fct_inorder(G_status))) +
      geom_col(width = 1, color = 1) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c('darkred','darkorange3',
                                   'darkgoldenrod2',
                                   'chartreuse4',
                                   'darkgreen')) +
      geom_label_repel(data = small_cave2,
                       aes(y = pos, label = paste0(pct, "%")),
                       size = 4.5, nudge_x = 1, show.legend = FALSE) +
      labs(title = "Cave Species in NatureServe (G1-G5, 93%)") +
      guides(fill = guide_legend(title = "Global Status")) +
      theme_void()
    
    
    
    
  }, bg="transparent")
  
  
  output$distPlot8 <- renderPlot({  #Overall Global Status 
    
    all_status <- Caves %>% group_by(G_status) %>% 
      distinct(Species) %>% 
      tally 
    
  ggplot(data = all_status, aes(x = G_status, y = n, fill = G_status)) +
      geom_col() +
      geom_text(aes(label = n), vjust = -0.20, colour = "black", size = 2.75) +
      scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                    "G2: Imperiled" = 'darkorange3',
                                    "G3: Vulnerable" = 'darkgoldenrod2',
                                    "G4: Apparently Secure" = 'chartreuse4',
                                    "G5: Secure" = 'darkgreen',
                                    "GH: Possibly Extinct" = 'black',
                                    "GX: Presumed Extinct" = 'darkslategray4',
                                    "GNR: Unranked" = 'darkslategray',
                                    "GU: Unrankable" = 'darkgrey'
      )) +
      labs(x = 'Global Status', y = 'Number of Species', title = "Species Global Status") +
      theme_clean() +
      theme(legend.position = 'none') +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
   

    
    
    
    
  }, bg="transparent")
  
  #####################################################################
  
  observeEvent(input$add_graph,{
    crit_imperiled <- Caves %>% filter(G_status %in% "G1: Critically Imperiled") %>% 
      group_by(Region) %>% 
      tally %>% 
      arrange(desc(n)) %>% 
      head(10)
    
    plot10 <- ggplot(data = crit_imperiled, aes(x = Region, y = n)) +
      geom_col() +
      labs(x = 'Region', y = 'Number of Species', title = "Top 10 Regions with Critically Imperiled Species") +
      theme_clean() +
      theme(legend.position = 'none')
    output$plots <- renderPlot({
      plot10
    })
  })
 #######################################################################
  observeEvent(input$hexa,{
    Hexapods <- Caves %>% filter(Class %in% c("Collembola", "Insecta","Diplura")) %>%
      group_by(G_status) %>% distinct(Species) %>% 
      tally
    
    plot10 <- ggplot(data = Hexapods, aes(x = G_status, y = n, fill = G_status)) +
      geom_col() +
      scale_x_discrete(drop = FALSE) +
      geom_text(aes(label = n), vjust = -0.25, colour = "black", size = 2.75) +
      ylim(0,250) +
      scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                    "G2: Imperiled" = 'darkorange3',
                                    "G3: Vulnerable" = 'darkgoldenrod2',
                                    "G4: Apparently Secure" = 'chartreuse4',
                                    "G5: Secure" = 'darkgreen',
                                    "GH: Possibly Extinct" = 'black',
                                    "GX: Presumed Extinct" = 'darkslategray4',
                                    "GNR: Unranked" = 'darkslategray',
                                    "GU: Unrankable" = 'darkgrey' )) +
      labs(x = 'Global Status', y = 'Number of Species', title = "Global Status of Hexapods",
           subtitle = "Collembola, Insecta, Diplura") +
      theme_classic()  +
      theme(legend.position = 'none') +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    output$plots <- renderPlot({
      plot10
    })
  })
  
  #####################################################################
  observeEvent(input$myria,{
    Myriapods <- Caves %>% filter(Class %in% c("Diplopoda", "Chilopoda")) %>% 
      group_by(G_status) %>% distinct(Species) %>% 
      tally 
    
    plot10 <- ggplot(data = Myriapods, aes(x = G_status, y = n, fill = G_status)) +
      geom_col() +
      scale_x_discrete(drop = FALSE) +
      geom_text(aes(label = n), vjust = -0.25, colour = "black", size = 2.75) +
      ylim(0,250) +
      scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                    "G2: Imperiled" = 'darkorange3',
                                    "G3: Vulnerable" = 'darkgoldenrod2',
                                    "G4: Apparently Secure" = 'chartreuse4',
                                    "G5: Secure" = 'darkgreen',
                                    "GH: Possibly Extinct" = 'black',
                                    "GX: Presumed Extinct" = 'darkslategray4',
                                    "GNR: Unranked" = 'darkslategray',
                                    "GU: Unrankable" = 'darkgrey' )) +
      labs(x = 'Global Status', y = 'Number of Species', title = "Global Status of Myriapods",
           subtitle = "Diplopoda, Chilopoda") +
      theme_classic()  +
      theme(legend.position = 'none') +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    output$plots <- renderPlot({
      plot10
    })
  })
  #####################################################################
  observeEvent(input$arach,{
    
    Arachnida <- Caves %>% filter(Class %in% c("Arachnida")) %>% 
      group_by(G_status) %>% distinct(Species) %>% 
      tally 
    
    plot10 <- ggplot(data = Arachnida, aes(x = G_status, y = n, fill = G_status)) +
      geom_col() +
      scale_x_discrete(drop = FALSE) +
      geom_text(aes(label = n), vjust = -0.25, colour = "black", size = 2.75) +
      ylim(0,250) +
      scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                    "G2: Imperiled" = 'darkorange3',
                                    "G3: Vulnerable" = 'darkgoldenrod2',
                                    "G4: Apparently Secure" = 'chartreuse4',
                                    "G5: Secure" = 'darkgreen',
                                    "GH: Possibly Extinct" = 'black',
                                    "GX: Presumed Extinct" = 'darkslategray4',
                                    "GNR: Unranked" = 'darkslategray',
                                    "GU: Unrankable" = 'darkgrey' )) +
      labs(x = 'Global Status', y = 'Number of Species', title = "Global Status of Arachnida") +
      theme_classic()  +
      theme(legend.position = 'none') +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    output$plots <- renderPlot({
      plot10
    })
  })
  #####################################################################
  observeEvent(input$crust,{
    Crustaceans <- Caves %>% filter(Class %in% c("Malacostraca", "Maxillopoda", "Ostracoda")) %>% 
      group_by(G_status) %>% distinct(Species) %>% 
      tally 
    
    plot10 <- ggplot(data = Crustaceans, aes(x = G_status, y = n, fill = G_status)) +
      geom_col() +
      scale_x_discrete(drop = FALSE) +
      geom_text(aes(label = n), vjust = -0.25, colour = "black", size = 2.75) +
      ylim(0,250) +
      scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                    "G2: Imperiled" = 'darkorange3',
                                    "G3: Vulnerable" = 'darkgoldenrod2',
                                    "G4: Apparently Secure" = 'chartreuse4',
                                    "G5: Secure" = 'darkgreen',
                                    "GH: Possibly Extinct" = 'black',
                                    "GX: Presumed Extinct" = 'darkslategray4',
                                    "GNR: Unranked" = 'darkslategray',
                                    "GU: Unrankable" = 'darkgrey' )) +
      labs(x = 'Global Status', y = 'Number of Species', title = "Global Status of Crustaceans",
           subtitle = "Malacostraca, Maxillopoda, Ostracoda") +
      theme_classic()  +
      theme(legend.position = 'none') +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    output$plots <- renderPlot({
      plot10
    })
  })
  #####################################################################
  observeEvent(input$vert,{
    Vertebrates <- Caves %>% filter(Phylum %in% "Craniata") %>% 
      group_by(G_status) %>% distinct(Species) %>% 
      tally 
    
    plot10 <- ggplot(data = Vertebrates, aes(x = G_status, y = n, fill = G_status)) +
      geom_col() +
      scale_x_discrete(drop = FALSE) +
      geom_text(aes(label = n), vjust = -0.25, colour = "black", size = 2.75) +
      ylim(0,25) +
      scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                    "G2: Imperiled" = 'darkorange3',
                                    "G3: Vulnerable" = 'darkgoldenrod2',
                                    "G4: Apparently Secure" = 'chartreuse4',
                                    "G5: Secure" = 'darkgreen',
                                    "GH: Possibly Extinct" = 'black',
                                    "GX: Presumed Extinct" = 'darkslategray4',
                                    "GNR: Unranked" = 'darkslategray',
                                    "GU: Unrankable" = 'darkgrey' )) +
      labs(x = 'Global Status', y = 'Number of Species', title = "Global Status of Vertebrates") +
      theme_classic()  +
      theme(legend.position = 'none') +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    output$plots <- renderPlot({
      plot10
    })
  })
  #####################################################################
  observeEvent(input$gast,{
    
    Gastropoda <- Caves %>% filter(Class %in% "Gastropoda") %>% 
      group_by(G_status) %>% distinct(Species) %>% 
      tally 
    
    plot10 <- ggplot(data = Gastropoda, aes(x = G_status, y = n, fill = G_status)) +
      geom_col() +
      scale_x_discrete(drop = FALSE) +
      geom_text(aes(label = n), vjust = -0.25, colour = "black", size = 2.75) +
      ylim(0,25) +
      scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                    "G2: Imperiled" = 'darkorange3',
                                    "G3: Vulnerable" = 'darkgoldenrod2',
                                    "G4: Apparently Secure" = 'chartreuse4',
                                    "G5: Secure" = 'darkgreen',
                                    "GH: Possibly Extinct" = 'black',
                                    "GX: Presumed Extinct" = 'darkslategray4',
                                    "GNR: Unranked" = 'darkslategray',
                                    "GU: Unrankable" = 'darkgrey' )) +
      labs(x = 'Global Status', y = 'Number of Species', title = "Global Status of Gastropoda") +
      theme_classic()  +
      theme(legend.position = 'none') +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    output$plots <- renderPlot({
      plot10
    })
  })
  #####################################################################
  observeEvent(input$plat,{
    
    Platyhelminthes <- Caves %>% filter(Phylum %in% "Platyhelminthes") %>% 
      group_by(G_status) %>% distinct(Species) %>% 
      tally 
    
   plot10 <- ggplot(data = Platyhelminthes, aes(x = G_status, y = n, fill = G_status)) +
      geom_col() +
      scale_x_discrete(drop = FALSE) +
      geom_text(aes(label = n), vjust = -0.25, colour = "black", size = 2.75) +
      ylim(0,25) +
      scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                    "G2: Imperiled" = 'darkorange3',
                                    "G3: Vulnerable" = 'darkgoldenrod2',
                                    "G4: Apparently Secure" = 'chartreuse4',
                                    "G5: Secure" = 'darkgreen',
                                    "GH: Possibly Extinct" = 'black',
                                    "GX: Presumed Extinct" = 'darkslategray4',
                                    "GNR: Unranked" = 'darkslategray',
                                    "GU: Unrankable" = 'darkgrey' )) +
      labs(x = 'Global Status', y = 'Number of Species', title = "Global Status of Platyhelminthes") +
      theme_classic()  +
      theme(legend.position = 'none') +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    output$plots <- renderPlot({
      plot10
    })
  })
  #####################################################################
  observeEvent(input$annel,{
    Annelida <- Caves %>% filter(Phylum %in% "Annelida") %>% 
      group_by(G_status) %>% distinct(Species) %>% 
      tally 
    
    plot10 <- ggplot(data = Annelida, aes(x = G_status, y = n, fill = G_status)) +
      geom_col() +
      scale_x_discrete(drop = FALSE) +
      geom_text(aes(label = n), vjust = -0.25, colour = "black", size = 2.75) +
      ylim(0,25) +
      scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                    "G2: Imperiled" = 'darkorange3',
                                    "G3: Vulnerable" = 'darkgoldenrod2',
                                    "G4: Apparently Secure" = 'chartreuse4',
                                    "G5: Secure" = 'darkgreen',
                                    "GH: Possibly Extinct" = 'black',
                                    "GX: Presumed Extinct" = 'darkslategray4',
                                    "GNR: Unranked" = 'darkslategray',
                                    "GU: Unrankable" = 'darkgrey' )) +
      labs(x = 'Global Status', y = 'Number of Species', title = "Global Status of Annelida") +
      theme_classic()  +
      theme(legend.position = 'none') +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
    output$plots <- renderPlot({
      plot10
    })
  })
  #####################################################################
   
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
