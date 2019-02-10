#------------------------------------------------------------------------------#

# MODULE UI ----
interactionModuleUi <- function(id, number, variable.options) {

  ns <- NS(id)

  tagList(
    fluidRow(id = id,
      column(6,
             selectInput(ns("variable"),
                         paste0("Select Variable ", number),
                         choices = c("Choose" = "", variable.options)
                         )
             ),

      column(6,
             numericInput(ns("lag.variable"),
                          label = paste0("Lag Variable ", number),
                          value = NA, min = 0
                          )
             )
    )
  )

}

#------------------------------------------------------------------------------#

# MODULE SERVER ----

interactionModule <- function(input, output, session, interaction.number){
  reactive({

    req(input$variable, input$lag.variable)

    df <- data.frame(
      "interaction.number" = interaction.number,
      "variable" = input$variable,
      "value" = input$lag.variable,
      stringsAsFactors = FALSE
    )

    return(df)

  })
}