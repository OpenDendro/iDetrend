server <- function(input, output, session) {

  ##############################################################
  #
  # Reactive Values
  #
  ##############################################################

  rwlRV <- reactiveValues()

  # Here are the elements of rwlRV. These don't need to be declared here
  # but I want to keep track of what's in the object as a matter of best
  # practices

  rwlRV$theRWL <- NULL          # the rwl object
  rwlRV$theRWL <- NULL          # the rwl object
  rwlRV$nSeries <- NULL         # the number of series
  rwlRV$theRWI <- NULL          # the stored RWI
  rwlRV$methodInfo <- NULL      # methodInfo
  rwlRV$dirtyDogs <- NULL       # ddog flags
  rwlRV$detrendParams <- NULL   # params used in detrending
  rwlRV$theCall <- NULL   # the string to pass to the report

  ##############################################################
  #
  # Observations
  #
  ##############################################################


  # When app is initiated, hide all the tabs but the first one.
  # This creates an observer so that they can be toggled when triggered
  # by an event
  observe({
    hide(selector = "#navbar li a[data-value=DescribeTab]")
    hide(selector = "#navbar li a[data-value=DetrendTab]")
    hide(selector = "#navbar li a[data-value=ResultsTab]")
  }, label = "tab hider")

  # When the dated RWL file is read in show all the tabs
  observeEvent({getRWL()},
               {
                 toggle(selector = "#navbar li a[data-value=DescribeTab]")
                 toggle(selector = "#navbar li a[data-value=DetrendTab]")
                 toggle(selector = "#navbar li a[data-value=ResultsTab]")
               }, label = "tab shower")
  ##############################################################
  #
  # Reactives
  #
  ##############################################################

  # Get the RWL file from the user at the start or use demo data
  getRWL <- reactive({
    if (input$useDemoDated) {
      data(nm046)
      dat <- nm046
      rwlRV$theRWL <- dat
      return(dat)
    }
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }
    else{
      # This might be problematic. What if the user wants a "long" tucson?
      # But we also don't want to add arguments.
      dat <- read.rwl(inFile$datapath)
      rwlRV$theRWL <- dat
      return(dat)
    }

  })
  # not 100% needed but let's dimension the RVs
  dimRVs <- reactive({
    dat <- getRWL()
    # the number of series -- vector
    rwlRV$nSeries <- ncol(dat)
    # the stored RWI -- just a copy of the RWL
    rwlRV$theRWI <- dat
    # info -- df
    rwlRV$methodInfo <- data.frame(chosen=rep(NA,ncol(dat)),
                                   actual=rep(NA,ncol(dat)))
    # the dirty dogs -- vector
    rwlRV$dirtyDogs <- ncol(dat)
    # params --
    rwlRV$detrendParams <- data.frame(method = rep(NA,ncol(dat)),
                                      nyrs = rep(NA,ncol(dat)),
                                      pos.slope =rep(NA,ncol(dat)),
                                      bass = rep(NA,ncol(dat)),
                                      span = rep(NA,ncol(dat)),
                                      difference=rep(NA,ncol(dat)))
  })
  ##############################################################
  #
  # Server logic for loading and describing the input data
  #
  ##############################################################

  # -- get the RWL report
  output$rwlReport <- renderPrint({
    req(getRWL())
    rwl.report(rwlRV$theRWL)
  })

  # -- plot rwl
  output$rwlPlot <- renderPlot({
    req(getRWL())
    plot.rwl(rwlRV$theRWL,plot.type = input$rwlPlotType)
  })

  # -- summary rwl
  output$rwlSummary <- renderTable({
    req(getRWL())
    summary(rwlRV$theRWL)
  })

  ##############################################################
  #
  # Server logic for for walking through each series and
  # detrending
  #
  ##############################################################

  # This is the server logic that will render the UI for the detrending.
  # All of the widgets will go in here. note the do.call below for `screens`
  # definitely the workhorse of the app
  output$series_screens <- renderUI({
    req(getRWL())
    # init the RVs
    dimRVs()

    # get data
    dat <- rwlRV$theRWL
    nSeries <- rwlRV$nSeries

    # loop and detrend with apply
    lapply(1:nSeries, function(i) {
      output[[paste0("series", i, "Plot")]] <- renderPlot({

        #### detrend
        mask <- is.na(dat[,i])
        seriesDF <- data.frame(y=dat[,i]) %>%
          drop_na() %>%
          mutate(x=1:length(y))

        # set detrend args here.
        method2use = input[[paste0("detrendMethod", i)]]
        # init defaults
        nyrs2use = NA
        pos.slope2use = FALSE
        bass2use = NA
        span2use = NA
        difference2use <- ifelse(input[[paste0("differenceText",i)]] == "Difference",TRUE,FALSE)


        # update args for each method
        if(method2use == "AgeDepSpline"){
          nyrs2use <- input[[paste0("nyrsADS",i)]]
          pos.slope2use <- input[[paste0("pos.slopeADS",i)]]
        }

        if(method2use == "Spline"){
          nyrs2use <- input[[paste0("nyrsCAPS",i)]]
        }

        if(method2use == "ModNegExp"){
          pos.slope2use <- input[[paste0("pos.slopeModNegExp",i)]]
        }

        if(method2use == "ModHugershoff"){
          pos.slope2use <- input[[paste0("pos.slopeModHugershoff",i)]]
        }

        if(method2use == "Friedman"){
          bass2use <- input[[paste0("bass",i)]]
          span2use <- input[[paste0("span",i)]]
        }

        res <- detrend.series(y = seriesDF$y,
                              method = method2use,
                              nyrs = nyrs2use,
                              pos.slope = pos.slope2use,
                              bass = bass2use,
                              span = span2use,
                              make.plot = FALSE,
                              verbose = FALSE,
                              return.info = TRUE,
                              difference = difference2use)
        seriesDF$Curve <- res$curve
        seriesDF$Fits <- res$series

        #print(i) # diag
        ### save output -- why does this need to be in isolate.
        ### adding observe didn't do anything with isolate
        ### and observe alone didn't work
        #observe({
        isolate({
          # save
          rwlRV$theRWI[!mask,i] <- res$series
          rwlRV$methodInfo[i,1] <- method2use
          rwlRV$dirtyDogs[i] <- res$dirtyDog
          # make these conditional on method here? above?
          rwlRV$methodInfo[i,2] <- res$model.info[[1]]$method
          rwlRV$detrendParams[i,] <- c(method2use,
                                       nyrs2use,
                                       pos.slope2use,
                                       bass2use,
                                       span2use,
                                       difference = difference2use)
        })
        #})

        # get messages to add to the plot. This is vexing.
        if(res$dirtyDog){
          capTxt <- "ARSTAN would tell you this is a dirty dog"

          if(method2use == "Ar"){
            subTxt <- paste0("Warning: detrend method (",method2use,
                             ") resulted in negative fits which were set to 0")
            if(difference2use == TRUE){
              subTxt <- paste0(subTxt, " before differencing")
            }
          }

          else {
            subTxt <- paste0("Warning: requested detrend method (",method2use,
                             ") resulted in negative fits, detrended with: ",
                             res$model.info[[1]]$method)
          }
        }
        else {
          subTxt <- paste0("Detrend method: ",res$model.info[[1]]$method)
          capTxt <- ""
        }

        ### make the plot and return it
        pSeries <- ggplot(seriesDF) +
          geom_line(aes(x=x,y=y)) +
          scale_x_continuous(name = "Index",position = "top",expand=c(0,0)) +
          labs(y="Raw",title=paste0("Series: ",names(dat)[i]),
               subtitle = subTxt)

        if(method2use != "Ar"){
          pSeries <- pSeries + geom_line(aes(x=x,y=Curve),color="darkred",size=1)
        }

        pFits <- ggplot(seriesDF) +
          geom_hline(yintercept = as.integer(round(mean(seriesDF$Fits,na.rm=TRUE))),
                     linetype="dashed") +
          geom_line(aes(x=x,y=Fits)) +
          scale_x_continuous(name = "Index",expand=c(0,0)) +
          labs(y="RWI",caption = capTxt)

        # make sure the axes are the same precision.
        pSeries <- pSeries +
          scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

        pFits <- pFits +
          scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

        pSeries <- pSeries + theme_minimal(base_size = 14) +
          theme(plot.background = element_rect(color = "grey70",size=0.5))
        pFits <- pFits + theme_minimal(base_size = 14) +
          theme(plot.background = element_rect(color = "grey70",size=0.5))

        pCombined <- grid.arrange(pSeries,pFits)

        return(pCombined)
      })
    })

    ### Set up the screens
    allScreens <- c(
      lapply(1:nSeries, function(i) {
        screen(
          fluidPage(
            ### menus
            fluidRow(
              column(3,
                     selectInput(inputId = paste0("differenceText",i),
                                 label = "Residual Method",
                                 choices = c("Division","Difference"),
                                 selected = "Ratio")),
              # add a tooltip
              bsTooltip(paste0("differenceText",i),
                        title = "Choose between standardizing by subtraction (difference) or by ratio (division)",
                        placement =  "right",
                        trigger = "hover",
                        options = list(container = "body")),
              column(3,
                     selectInput(inputId = paste0("detrendMethod",i),
                                 label = "Detrend Method",
                                 choices = c("AgeDepSpline",
                                             "Ar", "Friedman",
                                             "Mean",
                                             "ModHugershoff",
                                             "ModNegExp",
                                             "Spline"))),

              column(6,
                     # conditional arguments for specific methods
                     # note gymnastics to get pretty numbers on sliders :/
                     conditionalPanel(condition = paste0("input.detrendMethod",i," == 'Spline'"),
                                      sliderInput(inputId = paste0("nyrsCAPS",i),
                                                  label = "Spline Stiffness",
                                                  value = floor(length(na.omit(rwlRV$theRWL[,i]))/2),
                                                  min = 10,
                                                  max=(length(na.omit(rwlRV$theRWL[,i])) + 10) %/% 10 * 10,
                                                  step = 10,
                                                  ticks = FALSE),
                                      # add a tooltip
                                      bsTooltip(paste0("nyrsCAPS",i),
                                                title = "Spline stiffness in years, defaults to 1/2 the series length",
                                                placement =  "left",
                                                trigger = "hover",
                                                options = list(container = "body")),
                     ), # end cond panel

                     conditionalPanel(condition = paste0("input.detrendMethod",i," == 'AgeDepSpline'"),
                                      fluidRow(
                                        column(6,
                                               sliderInput(inputId = paste0("nyrsADS",i),
                                                           label = "Spline Stiffness",
                                                           value = 50,
                                                           min = 5,
                                                           max=(length(na.omit(rwlRV$theRWL[,i])) + 10) %/% 10 * 10,
                                                           step = 5,
                                                           ticks = FALSE),
                                               # add a tooltip
                                               bsTooltip(paste0("nyrsADS",i),
                                                         title = "Initial spline stiffness in years",
                                                         placement =  "left",
                                                         trigger = "hover",
                                                         options = list(container = "body"))
                                        ),
                                        column(6,
                                               p(strong("Allow Positive Slope")),
                                               checkboxInput(inputId = paste0("pos.slopeADS",i),
                                                             label = NULL,
                                                             value = FALSE),
                                               # add a tooltip
                                               bsTooltip(paste0("pos.slopeADS",i),
                                                         title = "Allow for a positive slope at the end of the series",
                                                         placement =  "left",
                                                         trigger = "hover",
                                                         options = list(container = "body"))
                                        )
                                      ),

                     ), # end cond panel


                     conditionalPanel(condition = paste0("input.detrendMethod",i," == 'ModNegExp'"),
                                      p(strong("Allow Positive Slope")),
                                      checkboxInput(inputId = paste0("pos.slopeModNegExp",i),
                                                    label = NULL,
                                                    value = FALSE),
                                      # add a tooltip
                                      bsTooltip(paste0("pos.slopeModNegExp",i),
                                                title = "Allow for a positive slope in case of linear model fit",
                                                placement =  "left",
                                                trigger = "hover",
                                                options = list(container = "body")),

                     ), # end cond panel

                     conditionalPanel(condition = paste0("input.detrendMethod",i," == 'ModHugershoff'"),
                                      p(strong("Allow Positive Slope")),
                                      checkboxInput(inputId = paste0("pos.slopeModHugershoff",i),
                                                    label = NULL,
                                                    value = FALSE),
                                      # add a tooltip
                                      bsTooltip(paste0("pos.slopeModHugershoff",i),
                                                title = "Allow for a positive slope in case of linear model fit",
                                                placement =  "left",
                                                trigger = "hover",
                                                options = list(container = "body")),

                     ), # end cond panel

                     conditionalPanel(condition = paste0("input.detrendMethod",i," == 'Friedman'"),

                                      fluidRow(
                                        column(6,
                                               # span
                                               sliderInput(inputId = paste0("span",i),
                                                           label = "Span",
                                                           value = 0, min = 0, max=1, step = 0.1),
                                               # add a tooltip
                                               bsTooltip(paste0("span",i),
                                                         title = "The fraction of the observations in the span of the smoother, If left at the default (0), the span will be determinied by cross-validation. Generally, either span or bass is adjusted but not both.",
                                                         placement =  "left",
                                                         trigger = "hover",
                                                         options = list(container = "body"))
                                        ),
                                        column(6,
                                               # bass
                                               sliderInput(inputId = paste0("bass",i),
                                                           label = "Bass",
                                                           value = 0, min = 0, max=10, step = 1),
                                               # add a tooltip
                                               bsTooltip(paste0("bass",i),
                                                         title = "Smoothness of bass function, Values of up to 10 indicate increasing smoothness. Generally, either span or bass is adjusted but not both.",
                                                         placement =  "left",
                                                         trigger = "hover",
                                                         options = list(container = "body"))
                                        )
                                      ),

                     ) # end cond panel
              )), # end col
            fluidRow(
              hr(),
              h5(paste0("Series ", i, " of ", nSeries))
            ),
            #####
            fluidRow(
              plotOutput(paste0("series", i, "Plot"))
            )
          )
        )
      })
    )
    do.call(glide, allScreens)
  })


  ##############################################################
  #
  # Server logic for results
  #
  ##############################################################

  output$downloadRWI <- downloadHandler(
    filename = function() {
      if(is.null(input$file1)){
        paste0("demo", "-",Sys.Date(), "RWI.csv")
      }
      else {
        paste0(input$file1, "-",Sys.Date(), "RWI.csv")
      }
    },
    content = function(file) {
      rwiOut <- rwlRV$theRWI
      rwiOut <- data.frame(Year = as.numeric(rownames(rwiOut)),rwiOut)
      write.csv(rwiOut, file, row.names = FALSE)
    }
  )


  output$detrendPlots <- downloadHandler(
    filename = "detrend_plots.html",
    content = function(file) {

      tempReport <- file.path(tempdir(), "report_savePlots.Rmd")
      file.copy("report_savePlots.Rmd", tempReport, overwrite = TRUE)

      rwlObject <- rwlRV$theRWL
      params <- list(fileName = input$file1$name, rwlObject=rwlRV$theRWL,
                     indivSeriesParam=rwlRV$detrendParams)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app). Defensive
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

  output$detrendReport <- downloadHandler(
    filename = "detrend_report.html",
    content = function(file) {

      tempReport <- file.path(tempdir(), "report_detrended_series.Rmd")
      file.copy("report_detrended_series.Rmd", tempReport, overwrite = TRUE)

      rwlObject <- rwlRV$theRWL
      params <- list(fileName = input$file1$name, rwlObject=rwlRV$theRWL,
                     indivSeriesParam=rwlRV$detrendParams)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app). Defensive
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

  output$plotRWI <- renderPlotly({
    dat <-  rwlRV$theRWI %>%
      rownames_to_column(var = "Years") %>%
      mutate(Years = as.numeric(Years)) %>%
      pivot_longer(cols = -Years,names_to = "Series",values_to = "RWI")

    plot_ly(
      dat,
      x = ~Years,
      y = ~RWI,
      type="scatter",
      split = ~Series,
      mode = "lines",
      hoverinfo = "split"
    )
  })


  output$tableRWI <- renderDataTable({
    rwiOut <-  rwlRV$theRWI
    datatable(rwiOut,
              autoHideNavigation=TRUE,
              options = list(pageLength = min(50,nrow(rwiOut)),
                             searching=FALSE,
                             lengthChange=FALSE)) %>%

      formatRound(columns = 1:ncol(rwiOut), digits = 3)

  })
}
