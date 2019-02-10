# \ PACKAGES ----

library(shiny)
library(bsplus)
library(dplyr)
library(shinyjs)

#------------------------------------------------------------------------------#

ui <- basicPage(
  
  # \ ENABLE shinyjs CODES ----
  useShinyjs(),
  
  # \ CUSTOM JS CODES ----
  
  ## \__ Close Modal ----
  uiOutput("jsCodeClose"),
  
  ## \__ Add-Remove Variable-Interaction Error Message ----
  uiOutput("jsCodeAdd"),
  
  # \ BUTTON ALIGNMENT ----
  tags$head(tags$style(
    HTML('
         .row-centered {
         text-align:center;
         }
         .col-centered {
         text-align:center;
         }
         .modal-body {
         overflow: auto;
         height: 400px;
         }
         ')
  )),
  
  # \ ADD MODAL BUTTON ----
  actionButton("show", "Add interactions?") %>%
    bs_attach_modal("modal_interactions"),
  
  # \ MODAL ----
  uiOutput("modalUI"),
  
  # \ OUTPUTS ----
  
  ## \__Dataframe ----
  h6("Data Frame"),
  verbatimTextOutput("dataInfo"),
  
  ## \__Dataframe Size ----
  h6("Data frame size"),
  verbatimTextOutput("dataInfo2"),
  
  ## \__Variable Number ----
  h6("Variable Number"),
  verbatimTextOutput("dataInfo3"),
  
  ## \__Interaction Number ----
  h6("inter.number"),
  verbatimTextOutput("dataInfo4"),
  
  ## \__Number of NA's ----
  h6("test num NA"),
  verbatimTextOutput("dataInfo5"),
  
  ## \__Interaction Index 1 ----
  h6("Interaction Index 1"),
  verbatimTextOutput("dataInfo6"),
  
  ## \__Interaction Index 2 ----
  h6("Interaction Index 2"),
  verbatimTextOutput("dataInfo7")
  
)