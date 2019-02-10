source("appModule.R")

ui <- fluidPage(
  
  # PRINT DATA FRAME
  h4("Data frame"),
  verbatimTextOutput("outDF"),
  
  # MODULE UI FOR VARIABLE 1
  variablesUI(1),
  
  # MODULE UI FOR VARIABLE 2
  variablesUI(2),
  
  # Where to insert the other modules
  tags$div(id = 'placeholder'),
  
  # ADD / REMOVE BUTTONS
  fluidRow(
    column(
      width = 12, 
      
      # ADD VARIABLE BUTTON
      actionButton(inputId = 'insertVarBtn', label = "Add a new variable"),
      
      #CREATE A SPACE BETWEEN BUTTONS
      HTML("&nbsp"),
      
      # REMOVE VARIABLE BUTTON
      actionButton(inputId = 'removeVarBtn', label = "Remove last variable")
    )
  )
)
