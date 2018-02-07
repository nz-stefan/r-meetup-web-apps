library(shiny)
library(tidyverse)

modulePlotterUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    selectizeInput(ns("var"), "Variable", list(), multiple = TRUE),
    plotOutput(ns("plot"))
  )
}

modulePlotter <- function(input, output, session, d_plot) {
  observe({
    choices <- d_plot() %>% colnames()
    updateSelectizeInput(session, "var", choices = choices, selected = choices[1])
  })
  
  output$plot <- renderPlot({
    req(input$var)
    d_plot() %>% 
      select(one_of(input$var)) %>% 
      gather(key, value) %>% 
      ggplot(aes(x = value)) +
      geom_density() +
      facet_wrap(~key, scales = "free")
  })
}

ui <- fluidPage(
  h1("Module demo"),
  fluidRow(
    column(width = 6, modulePlotterUI("mtcars")),
    column(width = 6, modulePlotterUI("faithful"))
  )
)

server <- function(input, output, session) {
  callModule(modulePlotter, "mtcars", reactive(mtcars))
  callModule(modulePlotter, "faithful", reactive(faithful))
}

shinyApp(ui, server)
