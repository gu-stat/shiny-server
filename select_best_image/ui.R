ui <- fluidPage(
  
  fluidRow(
    h6("Original Filenames"), 
    verbatimTextOutput("originalFilenames")
  ),
  
  fluidRow(
    h6("Remaining Filenames"), 
    verbatimTextOutput("remainingFilenames")
  ),
  
  fluidRow(
    h6("Initial Sample"), 
    verbatimTextOutput("initialSample")
  ),
  
  fluidRow(
    h6("New Sample - user choice fixed"), 
    verbatimTextOutput("newSample")
  ),
  
  fluidRow(
    uiOutput(outputId = "uiimg1"), 
    uiOutput(outputId = "uiimg2")
  ),
  
  fluidRow(uiOutput("radio")),
  
  fluidRow(uiOutput("nxt")),
  
  fluidRow(
    tags$div(
      HTML("<center>"),
      actionButton("start", "Start"),
      'id' = "strtbtn")
    )
  )