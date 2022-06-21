#install.packages(c("shiny","rmarkdown","markdown","dplR","DT","shinyWidgets","tidyverse","shinyjs","gridExtra"))

library(shiny)
library(rmarkdown)
library(markdown)
library(dplR)
library(tidyverse)
library(shinyjs)
library(gridExtra)

ui <- tagList(
  useShinyjs(),
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  navbarPage(
    title = "iDetrend",
    id = "navbar",
    # start tabs
    # 1st tab Introduction and Upload ----
    tabPanel(title="1. Introduction and Upload",value="IntroTab",

               fluidPage(
                 h3("Introduction"),
                 includeMarkdown("text_intro.rmd"),
                 hr(),
                 includeMarkdown("text_describe.rmd"),
                 hr(),
                 h3("Upload RWL"),
                 includeMarkdown("text_upload.rmd"),
                 hr(),
                 h4("RWL File"),
                 fileInput(inputId="file1",
                           label=NULL,
                           multiple = FALSE,
                           accept = c("text/plain",
                                      ".rwl",
                                      ".raw",
                                      ".txt")),
                 checkboxInput(inputId="useDemoDated", label="Or use example data",
                               value=TRUE),
                 plotOutput("rwlPlot"),
                 selectInput(inputId="rwlPlotType", label="Plot Type",
                             choices=c("seg","spag"),
                             selected = "seg"),
                 hr(),
                 h4("RWL Report"),
                 verbatimTextOutput("rwlReport"),
                 hr(),
                 h4("Series Summary"),
                 tableOutput("rwlSummary"),
                 hr(),
                 downloadButton("rwlSummaryReport", "Generate report")
               )
    ),
    # 2nd tab Detrend  ----
    tabPanel(title="2. Detrend",value="DetrendTab",
             # choose series
             fluidRow(
               column(4,
                      selectInput(inputId = "series",
                                  label = "Choose series",
                                  choices = c("foo"))
               ),
               column(4,
                      selectInput(inputId = "differenceText",
                                  label = "Residual Method",
                                  choices = c("Division","Difference"),
                                  selected = "Ratio")
               )
             ), # end row
             # plot series
             fluidRow(
               column(10,
                      plotOutput(outputId = "seriesPlot")
               ),
               column(2,
                      # choose method
                      selectInput(inputId = "detrendMethod",label = "Choose method",
                                  choices = c("AgeDepSpline", "Spline",
                                              "ModNegExp", "Mean",
                                              "Ar", "Friedman",
                                              "ModHugershoff"),
                                  selected = "AgeDepSpline"),


                      # conditional arguments
                      conditionalPanel(condition = "input.detrendMethod == 'Spline'",
                                       numericInput(inputId = "nyrsCAPS",
                                                    label = "Spline Stiffness",
                                                    value = 20,
                                                    min = 10,
                                                    max=1e3,
                                                    step = 10)),

                      conditionalPanel(condition = "input.detrendMethod == 'AgeDepSpline'",
                                       numericInput(inputId = "nyrsADS",
                                                    label = "Initial Spline Stiffness",
                                                    value = 50,
                                                    min = 1,
                                                    max=200,
                                                    step = 1),
                                       checkboxInput(inputId = "pos.slopeADS",
                                                     label = "Allow Positive Slope",
                                                     value = FALSE)),


                      conditionalPanel(condition = "input.detrendMethod == 'ModNegExp'",
                                       checkboxInput(inputId = "pos.slopeModNegExp",
                                                     label = "Allow Positive Slope",
                                                     value = FALSE)),

                      conditionalPanel(condition = "input.detrendMethod == 'ModHugershoff'",
                                       checkboxInput(inputId = "pos.slopeModHugershoff",
                                                     label = "Allow Positive Slope",
                                                     value = FALSE)),

                      conditionalPanel(condition = "input.detrendMethod == 'Friedman'",
                                       numericInput(inputId = "bass",
                                                    label = "smoothness of the fitted curve (bass)",
                                                    value = 0,min = 0,max=10,step = 1))
               )
             ),#end row

             h5("R Call to detrend.series"),
             htmlOutput(outputId = "detrendCall",inline = TRUE),
             tags$head(tags$style("#detrendCall{
                                 font-family: courier}"
             )
             ),
             h5("Notes"),
             verbatimTextOutput(outputId = "detrendInfo")

    ) # end tab

  )# end all tabs
)

