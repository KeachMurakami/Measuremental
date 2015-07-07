library(shiny)
library(rCharts)


shinyUI(fluidPage(

  # Application title
  headerPanel("Let's Make Reasonable Groups!"),

  # Sidebar with a slider input for number of bins
    sidebarPanel(
      fileInput('file1', 'Choose and upload data',
                accept = c(
                  '.XLS',
                  '.XLSX',
                  '.xls',
                  '.xlsx',
                  '.csv',
                  '.CSV'
                )),
      tags$hr(),
      sliderInput("plants", label = "set a number of plants in a group", min = 1, max = 20, value = 4, step = 1),
      sliderInput("groups", label = "set a number of groups", min = 1, max = 10, value = 3, step = 1),
      submitButton(),
      tags$hr()
#      downloadButton('downloadData', 'Download the table')
      ),

  mainPanel(
    chartOutput("Grouped", "datatables"),
    chartOutput("Stats", "datatables"),
    chartOutput("Data", "datatables"),
    plotOutput("distPlot", width = 750, height = 500)
    ),
  theme = "http://bootswatch.com/cerulean/bootstrap.min.css"
))
