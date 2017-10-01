# Developed by Mojdeh Shirazi-Manesh, 06/06/2017
# Thanks to Dr. Anastasios Panagiotelis

#Further improvements: 
#Removing other hospitals outside Melbourne.
#including age, adding more option to select from

##Load packages
library(shiny)
library(leaflet)
library(dplyr)
library(magrittr)#
library(tidyr)#
library(rgdal)#
library(rmapshaper)

Hospitals<-read.csv('HospitalLocations.csv')
Shape<-readRDS('GreaterMelbourneShape.rds')
Incomes<-readRDS('IncomesMel.rds')#


pal <- colorNumeric("Reds",NULL)
palr <- colorNumeric("Reds",reverse=TRUE, NULL)
reverseLabelFormat = function(..., reverse_order = FALSE){ 
  if(reverse_order){ 
    function(type = "numeric", cuts){ 
      cuts <- sort(cuts, decreasing = T)
    } 
  }else{
    labelFormat(...)
  }
}



#Color palette for occupations
cols<-c("#000099","#ff9900" )

pals<-colorFactor(domain=c("PUBLIC","PRIVATE"))

##User Interface

ui <- fluidPage(
  
#tags$img("1.jpg"),
  titlePanel("Hospital locations in Melbourne"),

  sidebarLayout(
    sidebarPanel(
      # Checkbox for hospital type
            checkboxGroupInput("hospType",label = h4("Select One or Both Hospital Types"),
                               choiceNames = list(p("PUBLIC",span(icon('h-square'),style=paste0("color:",cols[1]))),
                                                  p("PRIVATE", span(icon('h-square'),style=paste0("color:",cols[2])))),
                                                   
                               choiceValues = list("PUBLIC",
                                                   "PRIVATE")),
      # Select Input for income levels
            selectInput("inc", label = h3("Select Income level"), 
                        choices = list("Negative - 42,000$" = "L42", 
                                       "42,000$ - 104,000$" = "L104",
                                       "More than 104,000$" = "Lmore"
                                        ))##,
      
      

 ##     #Slider Input for Age Range
##      sliderInput("Age", label = h3("Age Range"), min = 0, 
##                  max = 100, value = c(0, 100))
      

    ),
  
    
    mainPanel(leafletOutput("lPlot"))
  ),
  hr(),
  p("Developed by Mojdeh Shirazi-Manesh, Monash University."),
  p("Population and Income data were sourced from the Australian Bureau of Statistics, census 2011. Data on hospital locations were collected from Victorian Government Data Directory.  Coordinates for hospitals obtained using a geocoding tool from", a("www.doogal.co.uk/BatchGeocoding.php",href="https://www.doogal.co.uk/BatchGeocoding.php",target="_blank"))
  
)

server <- function(input, output) {

  ## Stuff that changes when we select different hospital types
    filteredHospitals<-reactive({
      filter(Hospitals,Type %in% input$hospType)
    })
  

    ## Stuff that changes when we select different income levels # or age
      filtereddata<-reactive({
      filter(Incomes,(Income==input$inc)) %>% #(Age>=input$Age[1])&(Age<=input$Age[2]))%>%
        group_by(.,SA2)%>%
        summarise(.,sum(Persons))%>%
        rename(.,People=`sum(Persons)`,SA2_NAME11=SA2)->out
      
      Shape<-sp::merge(Shape,out,by="SA2_NAME11",all.x=TRUE)
    })
    
  
  output$lPlot<-renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldTopoMap")%>%
      setView(lng=144.9631,lat=-37.8136,zoom=10)
  })
  ## Stuff that changes when we select different income levels
  observe({
    #Popup
    sa2_popup <- paste0("<strong>SA2: </strong>",
                        filtereddata()$SA2_NAME11, 
                        "<br><strong>People: </strong>", 
                        filtereddata()$People)
    leafletProxy("lPlot",data=filtereddata())%>%
      clearShapes()%>% #Prevent polygons simply being plotted over one another
      clearControls()%>% #Clears any previous legend
      addPolygons(fillColor = ~pal(People), 
                  fillOpacity = 0.4, 
                  color = "#BDBDC3", #color of SA2 boundary (light gray)
                  weight = 1, #Thickness of SA2 Boundary
                  popup = sa2_popup, # SA2 popup defined above
                  highlightOptions=highlightOptions(color='black',weight=5))%>% #Dark black when mouse hovers
      addLegend(.,pal = palr, 
                title = "People",
                values=~People,
                labFormat = reverseLabelFormat(reverse_order=TRUE)) #
  })
  
  ## Stuff that changes when we select different hospital types
  observe({
    #Popup
    hospitalpu <- paste0("<strong>Hospital: </strong>",
                         filteredHospitals()$OpsName,
                         "<br><strong>Campus Code: </strong>",
                         filteredHospitals()$CampusCode)
    
    #Function required to get icons having the right color
 
    getColor <- function(dat) {
      sapply(dat$Type, function(ty) {
        if(ty == "PUBLIC") {
          cols[1]
        } else {
          cols[2]
        } })
    }
    
    #Customise icon
    icons <- awesomeIcons(
      icon = 'h-square',
      iconColor = getColor(filteredHospitals()),
      library = 'fa',
      markerColor = 'lightgray')
    #Update map
    leafletProxy("lPlot")%>%
      clearMarkers()%>%
      addAwesomeMarkers(data=filteredHospitals(),
                        lng = ~X,
                        lat=~Y,
                        icon=icons,
                        popup = hospitalpu)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

