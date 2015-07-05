library(shiny)
library(rCharts)


shinyUI(fluidPage(

  # Application title
  headerPanel("Analyze leaf chlorophyll contents"),

  # Sidebar with a slider input for number of bins
    sidebarPanel(
      fileInput('file1', 'Choose and upload info.file',
                accept = c(
                  '.XLS',
                  '.XLSX',
                  '.xls',
                  '.xlsx',
                  '.csv',
                  '.CSV'
                )),
      tags$hr(),
      selectInput("Basis", label = "Display based on ...", selected = "Leaf Area",
                  choices = c("Leaf Area" = "perArea", "Fresh Weight" = "perFW")), #, "Dry Weight" = "perDW", "Chl a+b" = "perChls)),
      submitButton(),
      tags$hr(),
      downloadButton('downloadData', 'Download the table')
      ),
    # Show a plot of the generated distribution

  mainPanel(
    plotOutput("distPlot", width = 750, height = 500),
    chartOutput("contents", "datatables")
    ),
  theme = "http://bootswatch.com/cerulean/bootstrap.min.css"
))
