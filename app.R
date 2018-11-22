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
#adding BCL image to the top of the page
		img(src = "BCL_Logo.png"),
#changing the name of the title panel
  titlePanel("Prices by origin and drink type"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                  choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
#having the buttons begin on "spirits" rather than "wine"
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
  							sort(unique(bcl$Subtype)),
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
#altering the table output with the DT package
  output$results <- DT::renderDataTable({
  	filtered()
  })
}


shinyApp(ui = ui, server = server)
