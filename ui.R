library(shiny)
library(rmarkdown)
library(markdown)
library(dplR)
library(DT)
library(shinyWidgets)
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
             sidebarLayout(
               # Sidebar panel for inputs
               sidebarPanel(
                 h5("Upload RWL"),
                 includeMarkdown("text_upload.rmd"),
                 hr(),
                 h5("RWL File"),
                 fileInput(inputId="file1", 
                           label=NULL,
                           multiple = FALSE,
                           accept = c("text/plain",
                                      ".rwl",
                                      ".raw",
                                      ".txt")),
                 checkboxInput(inputId="useDemoDated", label="Or use example data",
                               value=TRUE),
               ),
               
               # Main panel for displaying outputs
               mainPanel(
                 includeMarkdown("text_intro.rmd")
               )
             )
    ),
    # 2nd tab Describe RWL Data ----
    tabPanel(title="2. Describe RWL Data",value="DescribeTab",
             # Sidebar layout with input and output definitions
             includeMarkdown("text_describe.rmd"),
             hr(),
             plotOutput("rwlPlot"),
             selectInput(inputId="rwlPlotType", label="Plot Type",
                         choices=c("seg","spag"),
                         selected = "seg"),
             hr(),
             h5("RWL Report"),
             verbatimTextOutput("rwlReport"),
             hr(),
             h5("Series Summary"),
             tableOutput("rwlSummary"),
             hr(),
             downloadButton("rwlSummaryReport", "Generate report")
    ),
    # 3rd tab Detrend  ----
    tabPanel(title="3. Detrend",value="DetrendTab",
             # choose series
             selectInput(inputId = "series",
                         label = "Choose series",
                         choices = c("foo")),
             
             # plot series
             plotOutput(outputId = "seriesPlot"),
             # info on method
             verbatimTextOutput(outputId = "detrendInfo"),
             
             # choose method
             selectInput(inputId = "detrendMethod",label = "Choose method",
                         choices = c("AgeDepSpline", "Spline", "ModNegExp", "Mean",
                                     "Ar", "Friedman", "ModHugershoff"),
                         selected = "AgeDepSpline"),
             
             checkboxInput(inputId = "difference", 
                           label = "Compute with difference",
                           value = FALSE),
             
             # conditional arguments
             conditionalPanel(condition = "input.detrendMethod == 'Spline'",
                              numericInput(inputId = "nyrsCAPS", 
                                           label = "Spline Stiffness",
                                           value = 20, 
                                           min = 10,
                                           max=1e3,
                                           step = 10),
                              numericInput(inputId = "f", 
                                           label = "Frequency response",
                                           value = 0.5,
                                           min = 0,
                                           max=1,
                                           step = 0.05)),
             
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
                              selectInput(inputId = "constrain.nlsModNegExp", 
                                          label = "When should the parameters of 
                                          the nls function be constrained?",
                                          choices = c("never","when.fail",
                                                      "always"),
                                          selected = "never"),
                              checkboxInput(inputId = "pos.slopeModNegExp", 
                                            label = "Allow Positive Slope",
                                            value = FALSE)),
             
             conditionalPanel(condition = "input.detrendMethod == 'ModHugershoff'",
                              selectInput(inputId = "constrain.nlsModHugershoff", 
                                          label = "When should the parameters of 
                                          the nls function be constrained?",
                                          choices = c("never","when.fail",
                                                      "always"),
                                          selected = "never")),
             
             conditionalPanel(condition = "input.detrendMethod == 'Friedman'",
                              numericInput(inputId = "wt", 
                                           label = "case weights, by default all equal",
                                           value = NULL, min = 0,max=1,step = 0.05),
                              numericInput(inputId = "span", 
                                           label = "the fraction of the observations in the span",
                                           value = NULL,min = 0,max=1,step = 0.05),
                              numericInput(inputId = "bass", 
                                           label = "smoothness of the fitted curve",
                                           value = 0,min = 0,max=10,step = 1))
    )
  )# end tabs
)
