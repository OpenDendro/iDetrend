library(shiny)
library(rmarkdown)
library(markdown)
library(dplR)
library(tidyverse)
library(shinyjs)
library(gridExtra)
library(shinyglide)

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
                                    ".csv",
                                    ".txt")),
               checkboxInput(inputId="useDemoDated", label="Use example data",
                             value=TRUE),
               hr(),
               plotOutput("rwlPlot",width = 750),
               selectInput(inputId="rwlPlotType", label="Plot Type",
                           choices=c("seg","spag"),
                           selected = "seg"),
               hr(),
               h4("RWL Report"),
               verbatimTextOutput("rwlReport"),
               hr(),
               h4("Series Summary"),
               tableOutput("rwlSummary")
             )
    ), # end tab 1

    # 2nd tab detrend screens ----
    tabPanel(title="2. Detrend",value="DetrendTab",
             htmlOutput("series_screens")
    ), # end tab 2

    # 3rd tab results ----
    tabPanel(title="3. Results",value="ResultsTab",
             tableOutput("summaryResults")
    ) # end tab 3

  ) # end the navbar

) # end the UI
