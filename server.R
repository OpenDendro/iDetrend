
shinyServer(function(session, input, output) {

  # Declare a few things

  # Initiate an object (empty now) that will hold the rwl data
  # (and a backup). The advantage of this is that it can be edited
  # and saved between tabs
  rwlRV <- reactiveValues()
  # Here are the elements of rwlRV. These don't need to be declared here
  # but I want to keep track of what's in the object as a matter of best
  # practices

  rwlRV$theRWL <- NULL           # the rwl object
  rwlRV$theSeriesDF <- NULL      # the series as a df
  rwlRV$Curve <- NULL            # the curve
  rwlRV$Fits <- NULL             # the fits
  rwlRV$ModelInfo <- NULL        # modelInfo

  ##############################################################
  #
  # START observations
  #
  ##############################################################


  # When app is initiated, hide all the tabs but the first one.
  # This creates an observer so that they can be toggled when triggered
  # by an event

  # get series names
  observeEvent(
    eventExpr = {
      getRWL()
    },
    handlerExpr = {
      updateSelectInput(session = session,
                        inputId = "series",
                        choices=colnames(rwlRV$theRWL),
                        selected=colnames(rwlRV$theRWL[1]))
    },
    label = "observe series being selected and update the dropdown")



  ##############################################################
  #
  # END observations
  #
  ##############################################################


  ##############################################################
  #
  # START reactives
  #
  # we use reactives for so that calculations (like corr.rwl.seg)
  # need to be done only once.
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

  # Get a series to use
  getSeries <- reactive({
    req(getRWL())
    dat <- rwlRV$theRWL
    df <- data.frame(yrs = time(dat),
                     aSeries = dat[,input$series]) %>%
      drop_na() %>%
      mutate(index = 1:length(yrs))
    rwlRV$theSeriesDF <- df
    rwlRV$nyrsInit <- floor(length(df$aSeries)/2)
  })

  # do the detrending
  detrendSelectedSeries <- reactive({
    req(getSeries())
    df <- rwlRV$theSeriesDF

    # set detrend args here. I don't understand why these can't be
    # gotten dynamically from the UI but they don't update unless explicitly
    # set here I think
    method2use = input$detrendMethod
    # init defaults
    nyrs2use = NULL
    pos.slope2use = FALSE
    difference2use <- ifelse(input$differenceText == "Difference",TRUE,FALSE)

    # update args for each method
    if(method2use == "AgeDepSpline"){
      nyrs2use <- input$nyrsADS
      pos.slope2use <- input$pos.slopeADS
    }

    if(method2use == "Spline"){
      nyrs2use <- input$nyrsCAPS
    }

    if(method2use == "ModNegExp"){
      pos.slope2use <- input$pos.slopeModNegExp
    }

    if(method2use == "ModHugershoff"){
      pos.slope2use <- input$pos.slopeModHugershoff
    }

    #    if(method2use == "Friedman"){
    #      bass2use <- input$bass
    #    }

    #wt, span = "cv", bass = 0,

    res <- detrend.series(y = df$aSeries,
                          method = method2use,
                          nyrs = nyrs2use,
                          pos.slope = pos.slope2use,
                          bass = input$bass,
                          make.plot = FALSE,
                          verbose = FALSE,
                          return.info = TRUE,
                          difference = difference2use)
    rwlRV$Curve <- res$curve
    rwlRV$Fits <- res$series
    rwlRV$ModelInfo <- res$model.info[[1]]
    rwlRV$DirtyDog <- res$dirtyDog
    rwlRV$DetrendParams <- c(seriesName = input$series,
                             method = method2use,
                             nyrs = nyrs2use,
                             pos.slope = pos.slope2use,
                             bass = input$bass,
                             make.plot = FALSE,
                             verbose = FALSE,
                             return.info = TRUE,
                             difference = difference2use)
  })



  ##############################################################
  #
  # END reactives
  #
  ##############################################################


  ##############################################################
  #
  # 2nd tab Describe RWL Data
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

  # -- summary
  output$rwlSummary <- renderTable({
    req(getRWL())
    summary(rwlRV$theRWL)
  })
  # -- make report
  output$rwlSummaryReport <- downloadHandler(
    filename = "rwl_summary_report.html",
    content = function(file) {

      tempReport <- file.path(tempdir(), "report_rwl_describe.Rmd")
      file.copy("report_rwl_describe.Rmd", tempReport, overwrite = TRUE)

      rwlObject <- rwlRV$theRWL
      params <- list(fileName = input$file1$name, rwlObject=rwlObject,
                     rwlPlotType=input$rwlPlotType)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app). Defensive
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

  ##############################################################
  #
  # 3rd tab Detrend those things
  #
  ##############################################################

  output$seriesPlot <- renderPlot({
    req(getSeries())
    req(detrendSelectedSeries())

    seriesDF <- rwlRV$theSeriesDF
    seriesDF$Curve <- rwlRV$Curve
    seriesDF$Fits <- rwlRV$Fits


    pSeries <- ggplot(seriesDF) +
      geom_line(aes(x=index,y=aSeries)) +
      scale_x_continuous(name = "Index",position = "top") +
      labs(y="Raw",title=paste0("Series: ",input$series))

    if(input$detrendMethod != "Ar"){
      pSeries <- pSeries + geom_line(aes(x=index,y=Curve),color="darkred",size=1)
    }

    pFits <- ggplot(seriesDF) +
      geom_hline(yintercept = as.integer(round(mean(seriesDF$Fits,na.rm=TRUE))),
                 linetype="dashed") +
      geom_line(aes(x=index,y=Fits)) +
      scale_x_continuous(name = "Index") +
      labs(y="RWI")

    # make sure the axes are the same precision.
    pSeries <- pSeries +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

    pFits <- pFits +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

    pSeries <- pSeries + theme_minimal()
    pFits <- pFits + theme_minimal()

    grid.arrange(pSeries,pFits)

  })

  output$detrendInfo <- renderPrint({
    req(getSeries())
    req(detrendSelectedSeries())
    methodUsed <- rwlRV$ModelInfo$method
    # add conditional here about model method vs fit
    if(input$detrendMethod == "Ar" & rwlRV$DirtyDog){
      outMsg <- cat("Fits from method=='Ar' are not all positive. Setting values <0 to 0 before rescaling. This might not be what you want. ARSTAN would tell you to plot that dirty dog at this point. Proceed with caution.")
    }
    # return some text. this should be html
    return(rwlRV$DirtyDog)

  })

  output$detrendCall <- renderText({
    req(getSeries())
    req(detrendSelectedSeries())
    tmp <- rwlRV$DetrendParams
    theCall <- paste0("rwi",tmp["seriesName"]," <- detrend.series(y = dat[,`",
                      tmp["seriesName"],"`],",
                      "make.plot = FALSE,",
                      "method = '",tmp["method"], "',",
                      "nyrs = ", tmp["nyrs"], ",",
                      "pos.slope = ", tmp["pos.slope"], ",",
                      "bass = ", tmp["bass"], ",",
                      "difference =", tmp["difference"], ")")

    theCall

  })


})

