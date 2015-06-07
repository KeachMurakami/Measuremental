
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)



shinyUI(pageWithSidebar(

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
      selectInput("Basis", label = "Display based on ...", selected = "Leaf Area",
                  choices = c("Leaf Area" = "perArea", "Fresh Weight" = "perFW")), #, "Dry Weight" = "perDW", "Chl a+b" = "perChls)),
      submitButton()
    ),
    # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot"),
    tableOutput("contents")
    )
))
