library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(DT)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

#changing the user interface
ui <- fluidPage(
	#adding sandstone theme with the "shinythemes" app
		theme = shinytheme("sandstone"),
		img(src = "BCL_Logo.png"),
  titlePanel("Prices by origin and drink type"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                  choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                  selected = "SPIRITS"),
      #adding a subtype output in the user interface
      uiOutput("subtypeOutput"),
      uiOutput("countryOutput")
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      #adding an interactive table using the DT package
      DT::dataTableOutput("results")
    )
  )
)

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  #adding the subtype, with it starting on dry gin
  output$subtypeOutput <- renderUI({
  	selectInput("subtypeInput", "Subtype",
  							sort(unique(bcl[bcl$Type == input$typeInput, ]$Subtype)),
  							selected = "DRY GIN")
  })
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput,
      			 #adding the subtype to the filter
      			 Subtype == input$subtypeInput
      )
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })

  output$results <- DT::renderDataTable({
  	filtered()
  })
}


shinyApp(ui = ui, server = server)
