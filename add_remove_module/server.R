library(shiny)

server <- function(input, output, session) {
  
  # CREATE EMPTY DATAFRAME
  add.variable <- reactiveValues()
  
  add.variable$df <- data.frame(
    "variable" = character(0),
    "value" = numeric(0),
    stringsAsFactors = FALSE
  )
  
  #--------------------------
  # CALL MODULE 1 
  
  callModule(variablesServer, 1)
  
  ## SAVE INPUTS FROM 1 INTO DATAFRAME
  observeEvent(input[[NS(1, "variable")]], {
    add.variable$df[1, 1] <- input[[NS(1, "variable")]]
  })
  
  observeEvent(input[[NS(1, "value")]], {
    add.variable$df[1, 2] <- input[[NS(1, "value")]]
  })
  
  #--------------------------
  # CALL MODULE 2
  callModule(variablesServer, 2)
  
  ## SAVE INPUTS FROM 2 INTO DATAFRAME
  
  observeEvent(input[[NS(2, "variable")]], {
    add.variable$df[2, 1] <- input[[NS(2, "variable")]]
  })
  
  observeEvent(input[[NS(2, "value")]], {
    add.variable$df[2, 2] <- input[[NS(2, "value")]]
  })
  
  #--------------------------
  # START BUTTON VALUE AT 2 TO ACCOUNT FOR THE FIRST 2 VALUES
  
  btn <- reactiveValues(value = 2)
  
  #--------------------------
  # ADD VARIABLES
  
  observeEvent(input$insertVarBtn, {
    
    # EACH TIME THE USER CLICKS, ADD 1 TO BUTTON VALUE
    btn$value <- btn$value + 1
    
    ## WHEN WE USE btn$value DIRECTLY WE LOSE REACTIVITY
    ## PASSING IT TO btn.temp AND USING btn.tmp WORKS (SOMEHOW)
    btn.tmp <- btn$value
    
    # CALL MODULE NUMBER params$btn
    callModule(variablesServer, btn.tmp)
    
    # INSERT MODULE UI
    insertUI(
      selector = '#placeholder',
      where = "beforeEnd",
      ui = variablesUI(btn.tmp)
    )
    
    ## SAVE INPUTS FROM NUMBER params$btn INTO DATAFRAME
    
    observeEvent(input[[NS(btn.tmp, "variable")]], {
      add.variable$df[btn.tmp, 1] <- input[[NS(btn.tmp, "variable")]]
    })
    
    observeEvent(input[[NS(btn.tmp, "value")]], {
      add.variable$df[btn.tmp, 2] <- input[[NS(btn.tmp, "value")]]
    })
    
  })
  
  #--------------------------
  # REMOVE VARIABLES
  
  observeEvent(input$removeVarBtn, {
    
    # REMOVE LAST LINE FROM DATAFRAME
    add.variable$df <- add.variable$df[-btn$value, ]
    
    # REMOVE LAST LINE MODULE UI
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#var', btn$value)
    )
    
    # SUBTRACT 1 FROM BUTTON VALUE 
    btn$value <- btn$value - 1
  })
  
  #--------------------------
  
  # OUTPUT DATAFRAME
  
  output$outDF <- renderPrint({
    print(add.variable$df)
  })
  
}