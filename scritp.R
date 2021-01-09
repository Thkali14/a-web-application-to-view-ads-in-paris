#load libraries (make sure you have the correspond packages)
library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(tidyverse)

# user interface
ui <- fluidPage(
  fluidRow(
    theme = shinytheme("united"),
    titlePanel(HTML("<h1><center><font size=14> Paris declaration between 2018-2019</font></center></h1>")) ,   
  ),
  fluidRow(
    column(5,
           sidebarPanel(
             selectInput('Type',"Declaration type",choices = Data %>% distinct(TYPE)),
             selectInput('p_code',"Postal Code", choices = Data %>% distinct(CODE_POSTAL)),
             dateRangeInput('date','Period',
                            start = min(Data$DATEDECL),end = "2018-07-04",
                            min = min(Data$DATEDECL),max = max(Data$DATEDECL),
                            format = "yyyy-mm-dd")
           ),  plotOutput('counts')
           
    ),
    column(6,
           tabsetPanel(tabPanel('Dispersion of declarations',leafletOutput('pts_map')
           ),
           tabPanel('Density of declarations',leafletOutput('circles_map')
           )
           ), dataTableOutput('table')
    )
    
  ),
  
  
)

#server interface
server <- function(input, output,session) {
#data update with a reactive function 
  data_rue <- reactive(dansmarue[dansmarue$TYPE == input$Type,]%>% 
                         filter(as.double(DATEDECL)  >= min(input$date),
                                as.double(DATEDECL) <= max(input$date),
                                CODE_POSTAL == input$p_code
                         )
  )
  data_rue.sf <- reactive(dansmarue.sf[dansmarue.sf$TYPE == input$Type,]%>% 
                            filter(as.double(DATEDECL)  >= min(input$date),
                                   as.double(DATEDECL) <= max(input$date)  
                            )
  )
  
  densit <- reactive(sapply(st_contains( x=gr, y=data_rue.sf()$geometry), length))
  data_gr_p <- reactive(gr_p %>%  add_column(dens  = densit()))
#outputs ; interactive map ,interactive table,interactive map,bar plot 
  output$circles_map <- renderLeaflet(
    leaflet(data=data_gr_p() %>% st_transform(4326)) %>% 
      addTiles() %>% 
      addCircleMarkers(radius =~ sqrt(dens/10)*1.5,
                       fillColor = "purple",
                       stroke=FALSE,
                       fillOpacity = 0.5,
                       popup = ~paste("Nombre de plaintes:",dens))
  )
  output$table <- renderDataTable({
    data_rue() } )
  output$pts_map <- renderLeaflet(
    
    leaflet(data = data_rue()) %>% 
      addTiles() %>% 
      addCircles(fillOpacity = 0.5)
  )
  output$counts <- renderPlot(
    ggplot(data_rue() %>% count(TYPE,SOUSTYPE, DATEDECL) , aes(x=SOUSTYPE, y=n)) +
      geom_bar(stat="identity") +
      theme_minimal()+ 
      coord_flip() +labs(title = "The number of declarations by type and sub-type in the same data")
      )
}

#launch the application
shinyApp(ui=ui, server=server)