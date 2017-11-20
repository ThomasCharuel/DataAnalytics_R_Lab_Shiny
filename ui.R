library(shiny)



shinyUI(
  pageWithSidebar(

    # Application title
    headerPanel("Data Mining in R - My first app on shiny"),

    # Sidebar with controls to select a
    sidebarPanel(
      fileInput(
        'dataset_file',
        'Upload data',
        accept=c('.xlsx')
      ),
      numericInput(
        inputId = "user_id",
        label = "User id",
        value = 1
      )
    ),

    mainPanel(
      h3("Histogram of modes over a week"),
      verbatimTextOutput("summary")
    )
  )
)