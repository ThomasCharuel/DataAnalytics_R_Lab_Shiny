library(shiny)
library(readxl)

shinyServer(function(input, output) {

  output$summary <- renderPrint({
    # Get input file
    inFile <- input$dataset_file
    if(is.null(inFile))
      return(NULL)

    # Load dataset
    userdata <- read_excel(inFile$datapath)

    summary(userdata[userdata$User==input$user_id, ])
  })
})