
library(shiny)
library(tidyverse)
library(ggplot2)


# Define UI for application
ui <- fluidPage(
  
  tabsetPanel(
    tabPanel("Population of US states",
      titlePanel("Spotlight of US state populations"),
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
    tabPanel("State Spotlight",
      titlePanel("Spotlight on two US states of your choice"),
      fluidRow(
        column(4,
          selectInput(inputId = "state1", label = "select state",
                      choices = state.name)
        ),
        column(8,
          tableOutput(outputId = "info1")
        )
      ),
      fluidRow(
        column(4,
          selectInput(inputId = "state2", label = "select state",
                      choices = state.name)
        ),
        column(8,
          tableOutput(outputId = "info2")
        )
      ),
      fluidRow(
        column(2,
          plotOutput(outputId = "chartincome")
        ),
        column(2,
          plotOutput(outputId = "chartarea")
        ),
      )
    ),
    tabPanel("Income Detail",
      titlePanel("Spotlight on US state incomes")       
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

# server code for tab 1
    df_population <- reactive({
      df_states %>%
        dplyr::arrange(desc(Population)) %>%
        dplyr::slice(1:input$n)
    })
    
    output$listpop <- renderTable({
      df_population() %>%
        dplyr::select(State, StateAbb, Population, PopShare)
    })
    
    output$plotpop <- renderPlot({
      df_population() %>%
        ggplot(aes(x = State, y = Population)) +
          geom_point(size = 3, color = "red") + 
          geom_segment(aes(x = State, xend = State, y = 0, yend = Population)) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) # Rotate axis label
    })

# server code for tab 2
    output$info1 <- renderTable({
      df_states %>%
        dplyr::filter(State == input$state1) %>%
        dplyr::select(Population, Area, Income, Illiteracy, Murder)
    })
    
    output$info2 <- renderTable({
      df_states %>%
        dplyr::filter(State == input$state2) %>%
        dplyr::select(Population, Area, Income, Illiteracy, Murder)
    })
    
    df_panel2 <- reactive({
      df_states %>% 
        dplyr::filter(State == c(input$state1, input$state2))
    })
    
    output$chartincome <- renderPlot({
      df_panel2() %>%
        ggplot(aes(x = State, y = Income)) +
          geom_col(fill = "red", colour = "black") +
          theme_minimal()
    })
    
    output$chartarea <- renderPlot({
      df_panel2() %>%
        ggplot(aes(x = State, y = Area)) +
        geom_col(fill = "red", colour = "black") +
        theme_minimal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
