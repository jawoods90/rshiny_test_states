
library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tabsetPanel(
    tabPanel("Population of US states",
      fluidRow(
        column(8,
          sliderInput(inputId = "n", label = "select number of states",
                           min = 1, max = 30, value = 1, step = 1)
        )
      ),
      fluidRow(
        column(6,
          tableOutput(outputId = "listpop"),
        ),
        column(6,
          plotOutput(outputId = "plotpop")
        )
      )
    ),
    tabPanel("Area of US states",
      fluidRow(
        column(8,
          sliderInput(inputId = "n", label = "select number of states",
                      min = 1, max = 30, value = 1, step = 1)
        )
      ),
      fluidRow(
        column(6,
          tableOutput(outputId = "listarea"),
        ),
        column(6,
          plotOutput(outputId = "plotarea")
        )
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    df_population <- reactive({
      df_state %>%
        dplyr::arrange(desc(Population)) %>%
        dplyr::slice(1:input$n)
    })
    
    output$listpop <- renderTable({
      df_population()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
