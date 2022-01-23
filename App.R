library(rsconnect)
library(shiny)
library(ggplot2)
library(dplyr)

url <- "http://deanattali.com/files/bcl-data.csv"
bc1 <- read.csv(url)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", min = 0, max = 100, value = c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type", 
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      uiOutput("countryOutput")
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      tableOutput("results")
    )
  )
)
server <- function(input, output, session){
  filtered <- reactive({
    if(is.null(input$countryInput)) {
      return(NULL)
    }
    
    bc1 %>% 
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })
  
  output$results <- renderTable({
    filtered()
  })
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bc1$Country)),
                selected = "CANADA")
  })
}
shinyApp(ui = ui, server = server)
