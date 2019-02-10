# VARIABLES
LHSchoices <- c("X1", "X2", "X3", "X4")

#------------------------------------------------------------------------------#
# MODULE UI ----
variablesUI <- function(id) {
  ns <- NS(id)
  
  tags$div(
    id=paste0("var", id),
    fluidRow(
      column(
        width = 6,
        uiOutput(ns('variable'))
      ),
      column(
        width = 6,
        uiOutput(ns('value'))
      )
    )
  )
}

#------------------------------------------------------------------------------#
# MODULE SERVER ----

variablesServer <- function(input, output, session){
  ns = session$ns
  
  output$variable <- renderUI({
    selectInput(
      inputId = ns("variable"),
      label = paste0("Variable ", strsplit(x = ns(""), split = "-")),
      choices = c("Choose" = "", LHSchoices)
    )
  })
  
  output$value <- renderUI({
    numericInput(
      inputId = ns('value'),
      label = paste0("Value ", strsplit(x = ns(""), split = "-")),
      value = NULL
    )
  })
}