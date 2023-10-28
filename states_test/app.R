
library(shiny)
library(tidyverse)
library(ggplot2)


# Define UI for application
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
      df_states %>%
        dplyr::arrange(desc(Population)) %>%
        dplyr::slice(1:input$n)
    })
    
    output$listpop <- renderTable({
      df_population() %>%
        dplyr::select(State, Population)
    })
    
    output$plotpop <- renderPlot({
      df_population() %>%
        ggplot(aes(x = State, y = Population)) +
          geom_point(size = 3, color = "red") + 
          geom_segment(aes(x = State, xend = State, y = 0, yend = Population)) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) # Rotate axis label
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
