library(shiny)

server <- function(input, output) {
  
  # Generate file names
  orig.filenames <- 1:10
  
  # Create a reactive variable with filenames
  ## Reactive in the sense that we will update its values by removing the 
  ## selected ones
  filenames <- reactiveValues(names = orig.filenames)
  
  # Function to get 1 sample observation out of the remaining filenames
  mysample <- function(x){
    tmp <- sample(x,1)
    filenames$names <- setdiff(filenames$names, tmp)
    if(length(filenames$names) < 3) filenames$names <- orig.filenames
    tmp
  }
  
  # CREATE EMPTY SAMPLE SET 
  files <- reactiveValues(sample = c(NA, NA))
  
  # Get initial sample of files when user clicks 'start'
  rv.init <- eventReactive(input$start, {
    
    ## Generate 1st time LEFT value
    left <- mysample(filenames$names)
    
    ## Generate 1st time RIGHT value
    right <- mysample(filenames$names)
    
    ## Create your initial sample in files$files
    tmp <- c(left, right)
    
    return(tmp)
    
  })
  
  # UPDATE SAMPLE SET WITH INITIAL VALUES
  observeEvent(input$start,  files$sample <- rv.init())
  
  # Get new sample file, based on user choice
  ## It will only update sample after user selects 'Left' or 'Right'
  rv.cond <- eventReactive(input$start | input$nxt, {
    
    req(input$choice)
    
    # Change second value (right value), if user selects "Left"
    if (input$choice == "Left") {
      init.tmp <- files$sample
      init.tmp[2] <- mysample(filenames$names)
      tmp <- init.tmp
    }
    # Change first value (left value), if user selects "Right"
    else if (input$choice == "Right") {
      init.tmp <- files$sample
      init.tmp[1] <- mysample(filenames$names)
      tmp <- init.tmp
    }
    
    return(tmp)
    
  })
  
  # UPDATE SAMPLE SET WITH NEW VALUES
  observeEvent(input$nxt,  files$sample <- rv.cond())
  
  #----------------------------------------------------------------------------#
  
  observeEvent(input$start, {
    output$uiimg1<- renderUI(
      column(
        width = 6, 
        HTML("<center>Left Image"),
        fluidRow(imageOutput(outputId = "img1"))
      )
    )
  })
  
  observeEvent(input$start, {
    output$uiimg2<- renderUI(
      column(
        width = 6, 
        HTML("<center>Right Image"),
        fluidRow(imageOutput(outputId = "img2"))
      )
    )
  })
  
  observeEvent(input$start, {
    output$nxt <- renderUI(
      wellPanel(
        HTML("<center>"),
        actionButton("nxt","Next")
      )
    )
  })
  
  observeEvent(input$start, {
    output$radio<- renderUI(
      wellPanel(
        HTML("<center>"), 
        radioButtons(
          inputId = "choice",
          label = "Which photo do you prefer?",
          choices = c("Left", "Right"),
          inline = TRUE, 
          selected = character(0)
        )
      )
    )
  })
  
  observeEvent(input$nxt, {
    output$radio<- renderUI(
      wellPanel(
        HTML("<center>"),
        radioButtons(
          inputId = "choice",
          label = "Which photo do you prefer?",
          choices = c("Left", "Right"),
          inline = TRUE, 
          selected = character(0)
        )
      )
    )
  })
  
  observeEvent(input$start, {
    removeUI(selector = "div:has(> #strtbtn)", immediate = TRUE)
  })
  
  
  output$img1 <- renderImage({
    filename1 <- tempfile(fileext='.png')
    
    # CHANGED FROM THE ORIGINAL QUESTION --------------------------------------#
    # Set seed to filenames number from files$sample[1]
    set.seed(files$sample[1])
    
    # Generate a png
    png(filename1, width=325, height=214)
    hist(
      rnorm(50*files$sample[1]),  
      main = paste("Histogram of rnorm(50*" , files$sample[1], ")")
    )
    dev.off()
    #--------------------------------------------------------------------------#
    
    list(src = filename1, width=325, height=214)
  }, deleteFile= FALSE)
  
  output$img2 <- renderImage({
    filename2<- tempfile(fileext='.png')
    
    # CHANGED FROM THE ORIGINAL QUESTION --------------------------------------#
    # Set seed to filenames number from files$sample[2]
    set.seed(files$sample[2])
    
    # Generate a png
    png(filename2, width=325, height=214)
    hist(
      rnorm(50*files$sample[2]),  
      main = paste("Histogram of rnorm(50*" , files$sample[2], ")")
    )
    dev.off()
    #--------------------------------------------------------------------------#
    
    list(src = filename2, width=325, height=214)
  }, deleteFile= FALSE)
  
  # ADDED SERVER OUTPUTS ------------------------------------------------------#
  
  ## Print original filenames
  output$originalFilenames <- renderPrint({
    print(orig.filenames)
  })
  
  ## Print remaining filenames
  output$remainingFilenames <- renderPrint({
    print(filenames$names)
  })
  
  ## Print Initial Sample
  output$initialSample <- renderPrint({
    print(rv.init())
  })
  
  ## Print New Sample, keeping user choice fixed
  output$newSample <- renderPrint({
    req(input$start)
    print(files$sample)
  })
  
}