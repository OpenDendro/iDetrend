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
  rwlRV$nSeries <- NULL         # the number of series
  rwlRV$theRWI <- NULL          # the stored RWI
  rwlRV$methodInfo <- NULL      # methodInfo
  rwlRV$dirtyDogs <- NULL       # ddog flags
  rwlRV$detrendParams <- NULL   # params used in detrending



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
  # not 100 needed but let's dimension the RVs
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
    # params -- list dim it?
    rwlRV$detrendParams <- list()
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

  # -- summary
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
        nyrs2use = NULL
        pos.slope2use = FALSE
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
        }

        res <- detrend.series(y = seriesDF$y,
                              method = method2use,
                              nyrs = nyrs2use,
                              pos.slope = pos.slope2use,
                              bass = bass2use,
                              make.plot = FALSE,
                              verbose = FALSE,
                              return.info = TRUE,
                              difference = difference2use)
        seriesDF$Curve <- res$curve
        seriesDF$Fits <- res$series

       print(i)
        ### save output -- why does this need to be in isolate.
        ### adding observe didn't do anything with isolate
        ### and observe alone didn't work
        #observe({
        isolate({
          # save
          rwlRV$theRWI[!mask,i] <- res$curve
          rwlRV$methodInfo[i,1] <- method2use
          rwlRV$dirtyDogs[i] <- res$dirtyDog
          # make these conditional on method here? above?
          rwlRV$methodInfo[i,2] <- res$model.info[[1]]$method
          rwlRV$detrendParams[[i]] <- c(seriesName = input$series,
                                        method = method2use,
                                        nyrs = nyrs2use,
                                        pos.slope = pos.slope2use,
                                        bass = input$bass,
                                        make.plot = FALSE,
                                        verbose = FALSE,
                                        return.info = TRUE,
                                        difference = difference2use)
        })
        #})

        # get messages to add to the plot. This is vexing.
        # Things the user wants to know. Does the detrend method differ
        # from what they selected? And what are the consequences? How much can
        # you explain in helptext and what should be front and center?
        # I think final fit method needs to be on there and any data
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
          scale_x_continuous(name = "Index",position = "top") +
          labs(y="Raw",title=paste0("Series: ",names(dat)[i]),
               subtitle = subTxt)

        if(method2use != "Ar"){
          pSeries <- pSeries + geom_line(aes(x=x,y=Curve),color="darkred",size=1)
        }

        pFits <- ggplot(seriesDF) +
          geom_hline(yintercept = as.integer(round(mean(seriesDF$Fits,na.rm=TRUE))),
                     linetype="dashed") +
          geom_line(aes(x=x,y=Fits)) +
          scale_x_continuous(name = "Index") +
          labs(y="RWI",caption = capTxt)

        # make sure the axes are the same precision.
        pSeries <- pSeries +
          scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

        pFits <- pFits +
          scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

        pSeries <- pSeries + theme_minimal()
        pFits <- pFits + theme_minimal()

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
              column(4,
                     selectInput(inputId = paste0("differenceText",i),
                                 label = "Residual Method",
                                 choices = c("Division","Difference"),
                                 selected = "Ratio")),
              column(4,
                     selectInput(inputId = paste0("detrendMethod",i),
                                 label = "Detrend Method",
                                 choices = c("AgeDepSpline", "Spline",
                                             "ModNegExp", "Mean",
                                             "Ar", "Friedman",
                                             "ModHugershoff"),
                                 selected = "AgeDepSpline")),
              column(4,

                     # conditional arguments for specific methods

                     conditionalPanel(condition = paste0("input.detrendMethod",i," == 'Spline'"),
                                      numericInput(inputId = paste0("nyrsCAPS",i),
                                                   label = "Spline Stiffness",
                                                   value = floor(length(na.omit(rwlRV$theRWL[,i])/2)),
                                                   #value=100,
                                                   min = 10,
                                                   max=1e3,
                                                   step = 10)),

                     conditionalPanel(condition = paste0("input.detrendMethod",i," == 'AgeDepSpline'"),
                                      numericInput(inputId = paste0("nyrsADS",i),
                                                   label = "Initial Spline Stiffness",
                                                   value = 50,
                                                   min = 1,
                                                   max=200,
                                                   step = 1),
                                      checkboxInput(inputId = paste0("pos.slopeADS",i),
                                                    label = "Allow Positive Slope",
                                                    value = FALSE)),


                     conditionalPanel(condition = paste0("input.detrendMethod",i," == 'ModNegExp'"),
                                      checkboxInput(inputId = paste0("pos.slopeModNegExp",i),
                                                    label = "Allow Positive Slope",
                                                    value = FALSE)),

                     conditionalPanel(condition = paste0("input.detrendMethod",i," == 'ModHugershoff'"),
                                      checkboxInput(inputId = paste0("pos.slopeModHugershoff",i),
                                                    label = "Allow Positive Slope",
                                                    value = FALSE)),

                     conditionalPanel(condition = paste0("input.detrendMethod",i," == 'Friedman'"),
                                      numericInput(inputId = paste0("bass",i),
                                                   label = "smoothness of the fitted curve (bass)",
                                                   value = 0,min = 0,max=10,step = 1))
              )), # end col
            fluidRow(
              hr(),
              p(paste0("Series ", i, " of ", nSeries))
            ),
            #####
            fluidRow(
              plotOutput(paste0("series", i, "Plot"))
            )

            #### Diagnostics
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

  output$summaryResults <- renderTable({
    str(rwlRV$methodInfo)
    rwlRV$theRWI
  })
}
