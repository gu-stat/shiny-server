
source('interactionModule.R', local=TRUE)

server <- function(input, output, session) {
  
  # \ VARIABLES ----
  
  ## I've put this as a reactive expression just in case your variables come
  ## from somewhere else
  
  final.varselect <- reactive({
    
    variablesRHS = c("X1", "X2", "X3", "X4", "X5")
    
    return(variablesRHS)
    
  })
  
  # \ OUTPUT MODAL UI ----
  
  output$modalUI <- renderUI({
    
    # TO PREVENT MODAL FROM CLOSING WHEN CLICKING OUTSIDE
    bsModalNoClose <-function(...) {
      b = bs_modal(...)
      b[[2]]$`data-backdrop` = "static"
      b[[2]]$`data-keyboard` = "false"
      return(b)
    }
    
    bsModalNoClose(
      id="modal_interactions", 
      
      title = "",
      
      body = tagList(
        div(
          
          ## FIRST CONTAINER ID ##
          ##... \__ Container ID ----
          id = "inter-1",
          
          ## DISPLAY INTERACTION NUMBER ##
          h4(paste0("Interaction ", 1)),
          
          ## INSERT MODULE UI FOR VAR 1 ##
          ##... \__ModuleUI 1 ----
          interactionModuleUi(
            id = "inter-1-var-1", 
            number = 1, 
            variable.options = final.varselect()
          ),
          
          ## INSERT MODULE UI FOR VAR 2 ##
          ##... \__ModuleUI 2 ----
          interactionModuleUi(
            id = "inter-1-var-2", 
            number = 2, 
            variable.options = final.varselect()
          ),
          
          ## PREVENT WINDOW FROM SHOWING 'X' CLOSE BUTTON ##
          tags$head(
            tags$style(
              "#window .modal-footer{display:none}
              .modal-header{display:none}"
            )
            )
          )
        ),
      
      footer = tagList(
        
        ## ADD ROW WITH ADD/REMOVE BUTTONS FOR VARIABLES ##
        ##... \__Buttons: Add-Remove Variables ----
        div(
          id="buttonVars", 
          class="row",
          div(
            class="col-sm-6 col-centered",
            actionButton("insertVarBtn", "Add another variable")
          ),
          div(
            class="col-sm-6 col-centered",
            actionButton("removeVarBtn", "Remove last variable")
          )
        ),
        
        hr(),
        
        ## ADD ROW WITH ADD/REMOVE BUTTONS FOR INTERACTIONS ##
        ##... \__Buttons: Add-Remove Interactions  ----
        div(
          id="buttonInter", 
          class="row row-centered",
          div(
            class="col-sm-6 col-centered",
            actionButton("insertInterBtn", "Add a new interaction")
          ),
          div(
            class="col-sm-6 col-centered",
            actionButton("removeInterBtn", "Remove last interaction")
          )
        ),
        
        br(),
        
        ## ADD CONTAINER FOR ADD VARIABLE/INTERACTION ERROR MESSAGE ##
        ##... \__Error Message: Add Var/Inter ----
        div(
          id="addError", 
          class="modal-footer modal-add-error"
        ),
        
        br(),
        
        ## ADD CONTAINER FOR REMOVE VARIABLE/INTERACTION ERROR MESSAGE ##
        ##... \__Error Message: Remove Var/Inter ----
        div(
          id="remError", 
          class="modal-footer modal-rem-error"
        ),
        
        br(),
        
        ## ADD CONTAINER FOR CLOSE MODAL ERROR MESSAGE ##
        ##... \__Error Message: Close Modal ----
        div(
          id="closeError", 
          class="modal-footer modal-error"
        ),
        
        ## ADD 'Clear All' BUTTON ##
        ##... \__Button: Clear All ----
        actionButton("clearAll", "Clear All"),
        br(),
        br(),
        
        ## ADD 'Close' BUTTON ##
        ##... \__Button: Close ----
        actionButton("closeModal", "Close")
      )
      )
    
  })
  
  
  
  #--------------------------------------------------------------------------#
  
  # \ INITIALIZE EMPTY OBJECTS ----
  
  ## \__ Dataframe ----
  add.interaction <- reactiveValues()
  
  add.interaction$df <- data.frame(
    "interaction.number" = numeric(0),
    "variable" = character(0),
    "lag.variable" = numeric(0),
    stringsAsFactors = FALSE
  )
  
  ## \__ Dataframe Size ----
  
  add.interaction$n <- 0
  
  ## \__ Number of Variables ----
  
  add.interaction$var.number <- 0
  
  ## \__ Interaction Number ----
  
  add.interaction$inter.number <- 0
  
  ## \__ Interaction Index 1 ----
  
  add.interaction$index.1 <- 0
  
  ## \__ Interaction Index 2 ----
  
  add.interaction$index.2 <- 0
  
  #--------------------------------------------------------------------------#
  
  # \ OBSERVE INTERACTION NUMBER ----
  
  # IF CLICKED ONCE, THEN START FIRST INTERACTION #
  # ELSE, CONSIDER LAST INTERACTION NUMBER        #
  
  observe({
    add.interaction$inter.number <- max(1, add.interaction$inter.number)
  })
  
  #--------------------------------------------------------------------------#
  
  # \ OBSERVE DATAFRAME SIZE ----
  
  # CONSIDER THAT IT HAS TWO OBSERVATIONS ALREADY #
  
  observe({
    add.interaction$n <- max(2, add.interaction$n)
  })
  
  #--------------------------------------------------------------------------#
  
  # \ OBSERVE NUMBER OF VARIABLES ----
  
  # CONSIDER THAT IT HAS TWO VARIABLES ALREADY #
  
  observe({
    add.interaction$var.number <- max(2, add.interaction$var.number)
  })
  
  #--------------------------------------------------------------------------#
  
  # \ OBSERVE INTERACTION INDEX 1 ----
  
  # CONSIDER THAT IT HAS TWO VARIABLES ALREADY #
  
  observe({
    add.interaction$index.1 <- max(3, add.interaction$index.1)
  })
  
  #--------------------------------------------------------------------------#
  
  # \ OBSERVE INTERACTION INDEX 2 ----
  
  # CONSIDER THAT IT HAS THREE VARIABLES ALREADY #
  
  observe({
    add.interaction$index.2 <- max(4, add.interaction$index.2)
  })
  
  #--------------------------------------------------------------------------#
  
  # \ GET DATA FOR INTER-1-VAR-1 ---- 
  
  callModule(
    module = interactionModule,
    id = paste0("inter-", 1, "-var-", 1),
    interaction.number = 1
  )
  
  observeEvent(input[[NS(paste0("inter-", 1, "-var-", 1), "variable")]], {
    
    if (input[[NS(paste0("inter-", 1, "-var-", 1), "variable")]] != "") {
      ## INTERACTION NUMBER ##
      add.interaction$df[1, 1] <- 1
      
      ## SELECTED VARIABLE ##
      add.interaction$df[1, 2] <- 
        input[[NS(paste0("inter-", 1, "-var-", 1), "variable")]] 
    }
    
  })
  
  observeEvent(input[[NS(paste0("inter-", 1, "-var-", 1), "lag.variable")]], {
    
    if (input[[NS(paste0("inter-", 1, "-var-", 1), "variable")]] != "") {
      
      ## SELECTED VARIABLE VALUE ##
      add.interaction$df[1, 3] <- 
        input[[NS(paste0("inter-", 1, "-var-", 1), "lag.variable")]]
      
    }
    
  })
  
  #--------------------------------------------------------------------------#
  
  # \ GET DATA FOR INTER-1-VAR-2 ---- 
  
  callModule(
    module = interactionModule,
    id = paste0("inter-", 1, "-var-", 2),
    interaction.number = 1
  )
  
  observeEvent(input[[NS(paste0("inter-", 1, "-var-", 2), "variable")]], {
    
    if (input[[NS(paste0("inter-", 1, "-var-", 2), "variable")]] != "") {
      ## INTERACTION NUMBER ##
      add.interaction$df[2, 1] <- 1
      
      ## SELECTED VARIABLE ##
      add.interaction$df[2, 2] <- 
        input[[NS(paste0("inter-", 1, "-var-", 2), "variable")]]
      
    }
    
  })
  
  observeEvent(input[[NS(paste0("inter-", 1, "-var-", 2), "lag.variable")]], {
    
    ## SELECTED VARIABLE VALUE ##
    add.interaction$df[2, 3] <- 
      input[[NS(paste0("inter-", 1, "-var-", 2),"lag.variable")]]
    
  })
  
  #--------------------------------------------------------------------------#
  
  # \ ADD VARIABLES ----
  
  observeEvent(input$insertVarBtn, {
    
    ## OBSERVE FIRST CHANGE IN DATAFRAME ##
    observeEvent(add.interaction$df, once=TRUE, {
      
      ## \__If Error ---- 
      
      if (
        ## IF THERE ARE MISSING VALUES ##
        sum(rowSums(is.na(add.interaction$df))) > 0 
        
        ## or ##
        | 
        
        ## THERE ARE EMPTY CELLS ##
        sum(as.matrix(add.interaction$df) == "") > 0 
        
      ) {
        ### \_____Error Message ----
        jscode <- 
          HTML(
            paste0(
              '$(document).ready(function() {',
              '$("#addError").html("',
              '<span style=\'color:red;font-weight:bold\'>',
              'ERROR: ',
              'There are empty fields.<br/>',
              'Assign values to them before adding another variable.',
              '</span>',
              '");',
              '});
              '
            )
            )
        
        add.interaction$jscode <- jscode
        
      } else {
        
        ## \__Otherwise ---- 
        
        ### \_____Empty Error Message ----
        jscode <- HTML(
          paste0(
            '$(document).ready(function() {',
            '$("#addError").html("',
            '");',
            '});
            '
          )
          )
        
        add.interaction$jscode <- jscode
        
        ### \_____Counters ----
        
        ### ADD 1 TO DATAFRAME SIZE ###
        
        add.interaction$n <- sum(1, add.interaction$n)
        
        ### ADD 1 TO VARIABLE NUMBER ###
        
        add.interaction$var.number <- sum(1, add.interaction$var.number)
        
        ### CREATE INDEX FOR FUTURE INTERACTIONS ###
        
        add.interaction$index.1 <- sum(add.interaction$n, 1)
        
        add.interaction$index.2 <- sum(add.interaction$n, 2)
        
        ### \_____Temp Variables ----
        
        ### CREATE TEMP DATAFRAME SIZE TO KEEP REACTIVITY ###
        
        tmp.n <- add.interaction$n
        
        ### CREATE TEMP VARIABLE NUMBER TO KEEP REACTIVITY ###
        
        tmp.var.number <- add.interaction$var.number
        
        ### CREATE TEMP INTERACTION NUMBER TO KEEP REACTIVITY ###
        
        tmp.inter.number <- add.interaction$inter.number
        
        ### \_____Insert ModuleUI ----
        
        insertUI(
          selector = paste0("#inter-", tmp.inter.number),
          where = "beforeEnd",
          ui = tagList(
            interactionModuleUi(
              id=paste0("inter-", tmp.inter.number, "-var-", tmp.var.number),
              number = tmp.var.number,
              variable.options = final.varselect()
            )
          )
        )
        
        ### \_____Call Module Server ----
        
        callModule(
          module = interactionModule,
          id = paste0("inter-", tmp.inter.number, "-var-", tmp.var.number),
          interaction.number = tmp.inter.number
        )
        
        ### \_____Populate Dataframe ----
        
        ### OBSERVE USER VARIABLE SELECTION ###
        observeEvent(
          input[[NS(
            namespace = 
              paste0("inter-", tmp.inter.number,"-var-", tmp.var.number), 
            id = "variable"
          )]], {
            
            ### INTERACTION NUMBER ###
            add.interaction$df[tmp.n, 1] <- tmp.inter.number
            
            ### SELECTED VARIABLE ###
            add.interaction$df[tmp.n, 2] <- 
              input[[NS(
                namespace = 
                  paste0("inter-", tmp.inter.number,"-var-", tmp.var.number), 
                id = "variable"
              )]]
            
          }
        )
        
        ### OBSERVE USER VALUE INPUT ###
        observeEvent(
          input[[NS(
            namespace = 
              paste0("inter-", tmp.inter.number,"-var-", tmp.var.number), 
            id = "lag.variable"
          )]], {
            
            ## SELECTED VARIABLE VALUE ##
            add.interaction$df[tmp.n, 3] <- 
              input[[NS(
                namespace = 
                  paste0("inter-", tmp.inter.number,"-var-", tmp.var.number), 
                id = "lag.variable"
              )]]
            
          }
        )
        
      }
      
    })
    
  })
  
  #--------------------------------------------------------------------------#
  
  # \ REMOVE VARIABLES ----
  
  observeEvent(input$removeVarBtn, {
    
    ## \__If var.number == 2 ----
    
    if (add.interaction$var.number == 2) {
      
      ## \_____If inter.number = 1 ----
      if (add.interaction$inter.number == 1) {
        
        ### \_______With Empty Dataframe ----
        if (sum(as.matrix(add.interaction$df) == "") == 6) {
          
          jscode <-
            HTML(
              paste0(
                '$(document).ready(function() {',
                '$("#remError").html("',
                '<span style=\'font-weight:bold\'>',
                'Warning: ',
                'Dataframe is empty. No need to remove variable.',
                '</span>',
                '");',
                '});
                '
              )
            )
          
          add.interaction$jscode <- jscode
          
        } else {
          
          ### \_______With Non-Empty Dataframe ----
          jscode <-
            HTML(
              paste0(
                '$(document).ready(function() {',
                '$("#remError").html("',
                '<span style=\'font-weight:bold\'>',
                'Warning: ',
                'Interaction needs at least two variables.<br/>',
                'To clear values, click on \'Clear All\'.',
                '</span>',
                '");',
                '});
                '
              )
            )
          
          add.interaction$jscode <- jscode
          
        }
        
      } else {
        
        ## \_____If inter.number > 1 ----
        
        jscode <-
          HTML(
            paste0(
              '$(document).ready(function() {',
              '$("#remError").html("',
              '<span style=\'font-weight:bold\'>',
              'Warning: ',
              'Interaction needs at least two variables.<br/>',
              'To remove this interaction, click on ',
              '\'Remove last interaction\'.',
              '</span>',
              '");',
              '});
              '
            )
          )
        
        add.interaction$jscode <- jscode
        
      }
      
    }
    
    ## \__If var.number > 2 ----
    
    if (add.interaction$var.number > 2 ) {
      
      tmp.inter <- add.interaction$inter.number
      
      tmp.var <- add.interaction$var.number
      
      # REMOVE LAST LINE FROM DATAFRAME
      
      last.n <- add.interaction$n
      
      add.interaction$df <- add.interaction$df[-last.n, ]
      
      # REMOVE LAST LINE MODULE UI
      
      removeUI(
        ## pass in appropriate div id
        selector = paste0("#inter-", tmp.inter, "-var-", tmp.var),
        immediate = TRUE
      )
      
      # SUBTRACT 1 FROM COUNTERS
      
      add.interaction$n <- add.interaction$n - 1
      
      add.interaction$var.number <- add.interaction$var.number - 1
      
      add.interaction$index.1 <- add.interaction$index.1 - 1
      
      add.interaction$index.2 <- add.interaction$index.2 - 1
      
      # REMOVE ERROR MESSAGE
      
      jscode <-
        HTML(
          paste0(
            '$(document).ready(function() {',
            '$("#addError").html("',
            '");',
            '});
            '
          )
        )
      
      add.interaction$jscode <- jscode
      
    } 
    
  })
  
  #--------------------------------------------------------------------------#
  
  # \ ADD INTERACTIONS ----
  
  observeEvent(input$insertInterBtn, {
    
    observeEvent(add.interaction$df, once=TRUE, {
      
      ## \__If Error ---- 
      
      if (
        ## IF THERE ARE MISSING VALUES ##
        sum(rowSums(is.na(add.interaction$df))) > 0 
        
        ## or ##
        | 
        
        ## THERE ARE EMPTY CELLS ##
        sum(as.matrix(add.interaction$df) == "") > 0 
        
      ) {
        ### \_____Error Message ----
        jscode <- 
          HTML(
            paste0(
              '$(document).ready(function() {',
              '$("#addError").html("',
              '<span style=\'color:red;font-weight:bold\'>',
              'ERROR: ',
              'There are empty fields in the last interaction.<br/>',
              'Assign values to them before adding another interaction.',
              '</span>',
              '");',
              '});
              '
            )
            )
        
        add.interaction$jscode <- jscode
        
      } else {
        
        ## \__Otherwise ---- 
        
        ### \_____Empty Error Message ----
        jscode <- HTML(
          paste0(
            '$(document).ready(function() {',
            '$("#addError").html("',
            '");',
            '});
            '
          )
          )
        
        add.interaction$jscode <- jscode
        
        ### \_____Counters ----
        
        ### \_____Temp Variables ----
        
        tmp.inter.original <- add.interaction$inter.number
        
        tmp.inter.number <- sum(1, tmp.inter.original)
        
        add.interaction$index.1 <- sum(2, add.interaction$index.1)
        
        add.interaction$index.2 <- sum(2, add.interaction$index.2)
        
        ### \_____Insert ModuleUI ----
        
        insertUI(
          selector = paste0("#inter-", tmp.inter.original),
          where = "afterEnd",
          ui = tagList(
            div(
              id = paste0("inter-", tmp.inter.number),
              
              h4(paste0("Interaction ", tmp.inter.number)),
              
              interactionModuleUi(
                paste0("inter-", tmp.inter.number, "-var-", 1),
                number = 1,
                variable.options = final.varselect()
              ),
              interactionModuleUi(
                paste0("inter-", tmp.inter.number, "-var-", 2),
                number = 2,
                variable.options = final.varselect()
              )
            )
          )
        )
        
        ### \_____Call Module Server Var 1 ----
        
        callModule(
          module = interactionModule,
          id = paste0("inter-", tmp.inter.number, "-var-", 1),
          interaction.number = tmp.inter.number
        )
        
        ### \_____Populate Dataframe Var 1 ----
        
        observeEvent(
          input[[NS(
            namespace = 
              paste0("inter-", tmp.inter.number,"-var-", 1), 
            id = "variable"
          )]], {
            
            add.interaction$df[sum(add.interaction$index.1, -2), 1] <- tmp.inter.number
            
            add.interaction$df[sum(add.interaction$index.1, -2), 2] <- 
              input[[NS(
                namespace = paste0("inter-", tmp.inter.number, "-var-", 1),
                id = "variable"
              )]]
            
          })
        
        observeEvent(
          input[[NS(
            namespace = paste0("inter-", tmp.inter.number, "-var-", 1),
            id = "lag.variable"
          )]], {
            
            add.interaction$df[sum(add.interaction$index.1, -2), 3] <- 
              input[[NS(
                namespace = paste0("inter-", tmp.inter.number, "-var-", 1),
                id = "lag.variable"
              )]]
            
          }
        )
        
        ### \_____Call Module Server Var 2 ----
        
        callModule(
          module = interactionModule,
          id = paste0("inter-", tmp.inter.number, "-var-", 2),
          interaction.number = tmp.inter.number
        )
        
        ### \_____Populate Dataframe Var 2 ----
        
        observeEvent(
          input[[NS(
            namespace = paste0("inter-", tmp.inter.number,"-var-", 2),
            id = "variable"
          )]], {
            
            add.interaction$df[sum(add.interaction$index.2, -2), 1] <- 
              add.interaction$inter.number
            
            add.interaction$df[sum(add.interaction$index.2, -2), 2] <- 
              input[[NS(
                namespace = paste0("inter-", tmp.inter.number, "-var-", 2),
                id = "variable"
            )]]
            
          }
        )
        
        observeEvent(
          input[[NS(
            namespace = paste0("inter-", tmp.inter.number, "-var-", 2),
            id = "lag.variable"
          )]], {
            
            add.interaction$df[sum(add.interaction$index.2, -2), 3] <- 
              input[[NS(
                namespace = paste0("inter-", tmp.inter.number, "-var-", 2),
                id = "lag.variable"
              
            )]]
            
          }
        )
        
        ### ADD 1 TO INTERACTION NUMBER ###
        
        add.interaction$inter.number <- sum(1, add.interaction$inter.number)
        
        ### ADD 2 TO DATAFRAME SIZE ###
        
        add.interaction$n <- sum(2, add.interaction$n)
        
        ### GET LAST VARIABLE NUMBER
        
        tmp.last.var <- add.interaction$df[
          add.interaction$df$interaction.number == tmp.inter.original, 
          ]
        
        add.interaction$var.number <- dim(tmp.last.var)[1]
        
      }
      
    })
    
  })
  
  #--------------------------------------------------------------------------#
  
  # \ REMOVE INTERACTIONS  ---- 
  
  observeEvent(input$removeInterBtn,{
    
    ## \__If inter.number = 1 ----
    
    if (add.interaction$inter.number == 1) {
      
      ### \_____Warning Message ----
      jscode <- 
        HTML(
          paste0(
            '$(document).ready(function() {',
            '$("#remError").html("',
            '<span style=\'color:red;font-weight:bold\'>',
            'Warning: ',
            'To clear first interaction, click on \'Clear all\'.',
            '</span>',
            '");',
            '});
            '
          )
        )
      
      add.interaction$jscode <- jscode
      
    } else {
      
      ## \__If inter.number > 1 ----
      
      ### \____Last Interaction Number ----
      last.interaction <- add.interaction$inter.number
      
      ### \____Previous Interaction Number ----
      prev.interaction <- sum(add.interaction$inter.number, -1)
      
      ### \____Remove Last Interaction from Dataframe ----
      
      add.interaction$df <- 
        add.interaction$df %>% filter(interaction.number != last.interaction)
      
      ### \____Remove Last Interaction UI ----
      
      removeUI(
        selector = paste0('#inter-', last.interaction),
        immediate = TRUE
      )
      
      ### \____New Counters Values ----
      
      ### SUBTRACT 1 FROM inter.number VALUE ###
      
      add.interaction$inter.number <- sum(add.interaction$inter.number, -1)
      
      add.interaction$index.1 <- add.interaction$index.1 - 2
      
      add.interaction$index.2 <- add.interaction$index.2 - 2
      
      ### GET DATAFRAME SIZE ###
      
      add.interaction$n <- dim(add.interaction$df)[1]
      
      ### GET LAST VARIABLE NUMBER
      
      tmp.last.var <- add.interaction$df[
        add.interaction$df$interaction.number == prev.interaction, 
        ]
      
      add.interaction$var.number <- dim(tmp.last.var)[1]
      
    }
    
  })
  
  #--------------------------------------------------------------------------#
  
  # \ CLEAR ALL ---- 
  observeEvent(input$clearAll,{
    
    
    ### \__Reset UI Input Inter 1 ----
    
    shinyjs::reset("inter-1")
    
    ### \__If inter.number > 1 ----
    
    ### \____Remove UI: Interactions ----
    
    ### GET INTER NUMBERS
    
    max.inter <- max(add.interaction$df$interaction.number)
    
    ### REMOVE UI FOR INTER > 1
    
    if (max.inter > 1) {
    
      for (i in 2:max.inter){
        removeUI(
          selector = paste0('#inter-', i),
          immediate = TRUE
        )
        
      }
      
    }
    
    ### \__If inter.number == 1 and var > 2 ----
    
    ### \____Remove UI: Variables ----
    
    ### GET VAR NUMBER 
    
    tmp.last.var <- add.interaction$df[
      add.interaction$df$interaction.number == 1, 
      ]
    
    tmp.var.number <- dim(tmp.last.var)[1]
    
    if (tmp.var.number > 2) {
      
      for (j in 3:tmp.var.number) {
        
        removeUI(
          selector = paste0('#inter-', 1, "-var-", j),
          immediate = TRUE
        )
        
      }
    }
    
    ### \__Clear Dataframe ----
    
    add.interaction$df <- add.interaction$df[1:2,]
    
    add.interaction$df[1, 1] <- ""
    add.interaction$df[1, 2] <- ""
    add.interaction$df[1, 3] <- ""
    add.interaction$df[2, 1] <- ""
    add.interaction$df[2, 2] <- ""
    add.interaction$df[2, 3] <- ""
    
    
    ### \__Reset Counters ----
    
    ### \____ Dataframe Size ----
    
    add.interaction$n <- 0
    
    ### \____ Number of Variables ----
    
    add.interaction$var.number <- 0
    
    ### \____ Interaction Number ----
    
    add.interaction$inter.number <- 0
    
    ### \____ Interaction Index 1 ----
    
    add.interaction$index.1 <- 0
    
    ### \____ Interaction Index 2 ----
    
    add.interaction$index.2 <- 0
    
    jscode <-
      HTML(
        paste0(
          '$(document).ready(function() {',
          '$("#addError").html("',
          '");',
          '$("#remError").html("',
          '");',
          '});
          '
      )
    )
    
    add.interaction$jscode <- jscode
    
  })
  
  #--------------------------------------------------------------------------#
  
  # \ CLOSE MODAL ---- 
  
  output$jsCodeClose <- renderUI({
    
    req(add.interaction$df)
    
    # ADD "" to the first two lines to close modal if mistakenly opened
    if (dim(add.interaction$df)[1] == 2) {
      if (sum(rowSums(is.na(add.interaction$df))) > 0) {
        add.interaction$df[1, 1] <- ""
        add.interaction$df[1, 2] <- ""
        add.interaction$df[1, 3] <- ""
        add.interaction$df[2, 1] <- ""
        add.interaction$df[2, 2] <- ""
        add.interaction$df[2, 3] <- ""
      }
    }
    
    if (sum(rowSums(is.na(add.interaction$df))) > 0 | 
        (0 < sum(as.matrix(add.interaction$df) == "") &
         sum(as.matrix(add.interaction$df) == "") != 6 ) ) {
      jscode <- 
        HTML(
          paste0(
            'var errorInt = 1;
            $(document).ready(function() {
            $("#closeModal").on("click", function() { ',
            '$("#closeError").html("',
            '<span style=\'color:red;font-weight:bold\'>',
            'ERROR: ',
            'You have an interaction with one empty variable.<br/>',
            'Either assign a value to it or delete it.',
            '</span>',
            '");',
            '});',
            '});
            '
          )
        )
      } 
    else {
      jscode <-
        HTML(
          paste0(
            'var errorInt = 0;
            $(document).ready(function() {
            $("#closeModal").on("click", function() { ',
            '$("#addError").html("',
            '");',
            '$("#remError").html("',
            '");',
            '$("#closeError").html("',
            '");',
            'if (errorInt == 0) {
            $("#modal_interactions").modal(\'hide\');',
            '}',
            '});',
            '});
            '
          )
        )
    }
    return(tags$script(jscode))
      
  })
  
  #--------------------------------------------------------------------------#
  
  # \ OUTPUTS ----
  
  ## \__ Add-Remove Variable-Interaction Error Message ----
  
  output$jsCodeAdd <- renderUI({
    
    return(tags$script(add.interaction$jscode))
    
  })
  
  ## \__Print Dataframe ----
  output$dataInfo <- renderPrint({
    if (is.null(add.interaction$df))
      "No data selected"
    else
      print(add.interaction$df)
  })
  
  ## \__Print Dataframe Size ----
  
  output$dataInfo2 <- renderPrint({
    if (is.null(add.interaction$df))
      "No data selected"
    else
      print(add.interaction$n)
  })
  
  ## \__Print Number of Variables ----
  
  output$dataInfo3 <- renderPrint({
    if (is.null(add.interaction$df))
      "No data selected"
    else
      print(add.interaction$var.number)
  })
  
  ## \__Print Interaction Number ----
  
  output$dataInfo4 <- renderPrint({
    if (is.null(add.interaction$df))
      "No data selected"
    else
      print(add.interaction$inter.number)
  })
  
  ## \__Print Number of NA's ----
  
  output$dataInfo5 <- renderPrint({
    if (is.null(add.interaction$df))
      "No data selected"
    else
      print(sum(rowSums(is.na(add.interaction$df))))
  })
  
  ## \__Print Interaction Index 1 ----
  
  output$dataInfo6 <- renderPrint({
    if (is.null(add.interaction$index.1))
      "No data selected"
    else
      print(add.interaction$index.1)
  })
  
  ## \__Print Interaction Index 2 ----
  
  output$dataInfo7 <- renderPrint({
    if (is.null(add.interaction$index.2))
      "No data selected"
    else
      print(add.interaction$index.2)
  })
  
  #--------------------------------------------------------------------------#
  
    }