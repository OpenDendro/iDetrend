#list of packages required
list.of.packages <- c("shiny","rmarkdown","markdown","dplR",
                      "tidyverse","shinyjs","gridExtra","shinyglide","DT",
                      "plotly","shinyBS")
#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)


require(shiny)
require(rmarkdown)
require(markdown)
require(dplR)
require(tidyverse)
require(shinyjs)
require(gridExtra)
require(shinyglide)
require(DT)
require(plotly)
require(shinyBS)
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
                 checkboxInput(inputId="useDemoDated",
                               label="Use example data",
                               value=FALSE)
               ),
               mainPanel(
                 includeMarkdown("text_intro.rmd")
               )
             )
    ), # end tab 1

    # 2nd describe data ----
    tabPanel(title="2. Describe RWL Data",value="DescribeTab",
             fluidPage(
               includeMarkdown("text_describe.rmd"),
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
    ), # end tab 2


    # 3rd tab detrend screens ----
    tabPanel(title="3. Detrend",value="DetrendTab",
             htmlOutput("series_screens")
    ), # end tab 2

    # 4th tab results ----
    tabPanel(title="4. Results",value="ResultsTab",
             fluidPage(
               fluidRow(
                 plotlyOutput("plotRWI")
               ),
               hr(),
               fluidRow(
                 column(4,
                        h5("Save RWI Data"),
                        downloadButton('downloadRWI', 'Download RWI'),
                        helpText("The rwl file is writen as csv and readable
                          by standard dendro programs.(e.g.,
                          read.rwl() in dplR).")
                 ),
                 column(4,
                        h5("Generate Report"),
                        downloadButton("detrendReport", "Generate report"),
                        helpText("The report is self contained and will
                         allow reproducibility from the R prompt.")
                 ),
                 column(4,
                        h5("Save Plots"),
                        downloadButton("detrendPlots", "Download"),
                        helpText("An HTML file containing plots for the individual series.")
                 )
               ),
               hr(),
               fluidRow(
                 h5("Detrended data (RWI)"),
                 dataTableOutput("tableRWI")
               )
             )
    ) # end tab 3

  ) # end the navbar

) # end the UI
