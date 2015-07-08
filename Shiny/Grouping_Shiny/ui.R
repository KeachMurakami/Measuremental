library(shiny)
library(rCharts)


shinyUI(fluidPage(
  headerPanel("Let's Make Reasonable Groups!"),
  fixedRow(
    column(
      width = 3,
      fileInput('file1', 'Choose and upload data',
                accept = c('.csv', '.CSV')),
      tags$hr(),
      sliderInput("plants", label = "set a number of plants in a group", min = 1, max = 20, value = 4, step = 1),
      sliderInput("groups", label = "set a number of groups", min = 1, max = 10, value = 3, step = 1),
      submitButton(),
      tags$hr(),
      tableOutput("Grouped")
#      downloadButton('downloadData', 'Download the table')
    ),
    column(
      htmlOutput("message1"),
      width = 3,
      tableOutput("Stats")
    ),
    column(
      width = 6,
  #    chartOutput("RawData", "datatables"),
      htmlOutput("message2"),
      plotOutput("barPlot", width = 500, height = 900)
    )),
  theme = "http://bootswatch.com/cerulean/bootstrap.min.css"
))