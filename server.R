# appHelpers.R is sourced in global.R: ui.R reads the lists of choices in it
source("plotDetrend.R")
source("svgArt.R")
source("guide.R")
source("goals.R")

shinyServer(function(session, input, output) {

  # ── State ──────────────────────────────────────────────────────────────────
  #   rwlRV$dat    — the working data: the file as read, with any gap fills
  #   rwlRV$vault  — the file as read, to undo the fills
  #   rwlRV$fills  — the fills made, in order, replayed in the report's R code
  #   rwlRV$dropped — series with no measurements, left out on loading
  #   settings()   — one row per series: how it is detrended (appHelpers.R).
  #                  The controls write to it; everything else reads it.
  #   notes()      — the user's note on each series, by name
  #   seen()       — the series that have been shown on the Detrend panel
  # Notes and seen are kept out of the settings table so that typing a note
  # does not redraw the plots.
  rwlRV <- reactiveValues(dat = NULL, vault = NULL, name = NULL, label = NULL,
                          example = FALSE, readError = NULL,
                          fills = list(), dropped = character(0),
                          goal = NULL)   # the starting point applied, if any (goals.R)
  settings    <- reactiveVal(NULL)
  notes       <- reactiveVal(list())
  seen        <- reactiveVal(character(0))
  seriesNames <- reactiveVal(NULL)   # set on loading only, so it changes only then
  noteOf <- function(series) {
    x <- notes()[[series]]
    if (is.null(x)) "" else x
  }

  # ── Unsaved work ───────────────────────────────────────────────────────────
  # Everything lives in the session, so leaving discards it. While there are
  # choices that have not been downloaded (as a report or a settings file),
  # the browser asks before the tab is closed or reloaded (script in ui.R).
  unsaved <- reactiveVal(FALSE)
  observe(session$sendCustomMessage("idUnsaved", unsaved()))

  # A download is served outside the normal input cycle, so values changed
  # in its handler would not reach the page until the next input. Ask for a
  # flush. (The test harness's mock session refuses the call.)
  afterDownload <- function() {
    tryCatch(session$requestFlush(), error = function(e) NULL)
  }

  # ── Helper: a report download that fails readably ─────────────────────────
  # A downloadHandler() whose content function stops sends the browser a
  # server error, which it saves as "<button id>.html". The report uses this
  # wrapper instead: the file the user gets says what went wrong.
  safeDownload <- function(filename, content) {
    downloadHandler(filename = filename, content = function(file) {
      tryCatch(content(file), error = function(e) {
        msg <- conditionMessage(e)
        msg <- paste0("iDetrend could not make this report: ", msg,
                      if (!grepl("[.!?]$", msg)) ".")
        writeLines(c("<html><head><meta charset='utf-8'><title>Report not made</title></head>",
                     "<body style='font-family: sans-serif; max-width: 40em; margin: 3em auto;'>",
                     "<h2>Report not made</h2>",
                     paste0("<p>", htmltools::htmlEscape(msg), "</p>"),
                     "<p>Go back to iDetrend, fix what the message describes, and generate the report again.</p>",
                     "</body></html>"), file)
      })
    })
  }


  # ════════════════════════════════════════════════════════════════════════════
  # THE FILE
  # ════════════════════════════════════════════════════════════════════════════
  # One file at a time. The example is dplR's nm046, taken from the package
  # so the report's R code needs no file.
  exampleName <- "nm046"   # the first example; examples (guide.R) lists them all
  # An example's data, made by running its R source: the same lines the
  # report prints
  exampleData <- function(id) {
    e <- new.env()
    eval(parse(text = examples[[id]]$code), e)
    e$dat
  }

  # TRUE when there is something to lose: a setting changed, a note, a fill
  hasWork <- reactive({
    S <- settings()
    !is.null(S) && (!all(sameSettings(S, defaultSettings(S$series))) ||
                      any(nzchar(unlist(notes()))) || length(rwlRV$fills) > 0)
  })

  loadFile <- function(name, path = NULL, example = FALSE) {
    res <- if (example) {
      list(data = exampleData(name), error = NULL)
    } else readRWLsafely(path)
    dat <- res$data
    # A series with no measurements cannot be detrended, and as a column of
    # NA it would be counted as a series by everything downstream. Leave it
    # out and say so (on the Overview).
    dropped <- character(0)
    if (!is.null(dat)) {
      empty   <- colSums(!is.na(dat)) == 0
      dropped <- names(dat)[empty]
      if (all(empty)) {
        res$error <- "The file was read, but no series in it has any measurements."
        dat <- NULL
      } else if (any(empty)) {
        dat <- dat[, !empty, drop = FALSE]
      }
    }
    fitCache$clear()
    statsHeld(NULL)
    pinned(NULL)
    history(list())
    rwlRV$name      <- name
    rwlRV$label     <- if (example) examples[[name]]$title else name
    rwlRV$example   <- example
    rwlRV$readError <- res$error
    rwlRV$dropped   <- dropped
    rwlRV$fills     <- list()
    rwlRV$goal      <- NULL
    rwlRV$vault     <- dat
    rwlRV$dat       <- dat
    nms <- if (is.null(dat)) NULL else names(dat)
    settings(if (is.null(dat)) NULL else defaultSettings(nms))
    notes(list())
    seen(character(0))
    seriesNames(nms)
    unsaved(FALSE)
    settingsNote(NULL)
    ctlRefresh(ctlRefresh() + 1)
    restoreOffer(NULL)
    touched(FALSE)
    if (!is.null(dat)) session$sendCustomMessage("idFetch", list(key = fileKey(name, dat)))
    updateSelectizeInput(session, "copyTo", choices = nms, selected = character(0),
                         server = length(nms) > 200)
    # a plot of every series makes a large report of a large file: leave it
    # to the user to ask for
    updateCheckboxInput(session, "reportPlots", value = length(nms) <= 60)
    nav_select("navbar", "OverviewTab", session = session)
  }

  # Loading a file replaces the one in use. Ask first when that would throw
  # work away. (The upload has already happened; this decides whether to
  # use it.)
  pending <- reactiveVal(NULL)
  askOrLoad <- function(name, path = NULL, example = FALSE) {
    if (!isTRUE(hasWork())) return(loadFile(name, path, example))
    pending(list(name = name, path = path, example = example))
    showModal(modalDialog(
      title = "Replace the file in use?",
      p("Loading ", tags$strong(name), " closes ", tags$strong(rwlRV$label),
        ". The settings and notes you have made for it would be lost."),
      p("To keep them, cancel, then save the settings file or generate the",
        "report on the Results panel."),
      footer = tagList(modalButton("Cancel"),
                       actionButton("replaceConfirm", "Load the new file",
                                    class = "btn-danger")),
      easyClose = TRUE))
  }
  observeEvent(input$replaceConfirm, {
    p <- pending()
    req(p)
    removeModal()
    pending(NULL)
    loadFile(p$name, p$path, p$example)
  })
  observeEvent(input$file1, askOrLoad(input$file1$name, input$file1$datapath))
  observeEvent(input$useDemo, askOrLoad(exampleName, example = TRUE))
  observeEvent(input$useDemo2, askOrLoad("gusPearson", example = TRUE))

  # ── The copy kept in the browser ──────────────────────────────────────────
  # The settings, notes, fills and which series have been looked at are
  # written to the browser's local storage a moment after each change (the
  # handlers are in ui.R, the format in appHelpers.R). When a file is loaded
  # the browser is asked what it holds for that file; if there is work, the
  # user is offered it back. So a session that times out, or a tab closed by
  # mistake, loses nothing. The copy never leaves the user's browser.
  storeKey     <- reactive(if (!is.null(rwlRV$vault)) fileKey(rwlRV$name, rwlRV$vault))
  restoreOffer <- reactiveVal(NULL)
  touched      <- reactiveVal(FALSE)   # work was made, or restored, in this session
  storeLog     <- new.env()            # $last: what was last sent to the browser (for the tests)
  sendStore <- function(key, value) {
    storeLog$last <- list(key = key, value = value)
    session$sendCustomMessage("idStore", list(key = key, value = value))
  }

  saveMs    <- getOption("idetrend.debounce.ms", 1500)
  saveState <- reactive(list(settings(), notes(), seen(), rwlRV$fills, rwlRV$goal))
  if (saveMs > 0) saveState <- debounce(saveState, saveMs)
  observe({
    saveState()
    isolate({
      key <- storeKey()
      # nothing to write while an offer is waiting: it would overwrite the
      # copy on offer with the fresh file's defaults
      if (is.null(key) || !is.null(restoreOffer())) return()
      if (isTRUE(hasWork())) {
        touched(TRUE)
        sendStore(key, stateToJSON(settings(), notes(), seen(), rwlRV$fills, rwlRV$goal))
      } else if (touched()) {
        sendStore(key, NULL)     # everything put back to the default
      }
    })
  })

  observeEvent(input$autosaved, {
    x <- input$autosaved
    req(identical(x$key, storeKey()), is.character(x$value), nzchar(x$value))
    if (isTRUE(hasWork())) return()
    res <- stateFromJSON(x$value, defaultSettings(seriesNames()))
    if (!is.null(res$error) || length(res$matched) == 0) return()
    nChanged <- sum(!sameSettings(res$settings, defaultSettings(res$settings$series)))
    if (nChanged == 0 && length(res$notes) == 0 && length(res$fills) == 0) return()
    restoreOffer(res)
    plural <- function(n, one, many) paste(n, if (n == 1) one else many)
    showModal(modalDialog(
      title = "Pick up where you left off?",
      p("This browser holds work on", tags$strong(rwlRV$label),
        paste0("from an earlier session",
               if (!is.null(x$saved) && nzchar(x$saved)) paste0(", saved ", x$saved), ":")),
      tags$ul(
        tags$li(plural(nChanged, "series", "series"), "with settings changed from the default"),
        if (length(res$notes)) tags$li(plural(length(res$notes), "note", "notes")),
        if (length(res$fills)) tags$li("gaps filled"),
        tags$li(plural(length(res$seen), "series", "series"), "looked at")),
      p(class = "small text-muted mb-0",
        "It is kept only in this browser, on this computer. Starting fresh deletes it."),
      footer = tagList(actionButton("restoreNo", "Start fresh", class = "id-btn-quiet"),
                       actionButton("restoreYes", "Restore", class = "btn-primary")),
      easyClose = FALSE))
  })
  observeEvent(input$restoreNo, {
    removeModal()
    restoreOffer(NULL)
    sendStore(storeKey(), NULL)
  })
  observeEvent(input$restoreYes, {
    res <- restoreOffer()
    req(res)
    removeModal()
    dat <- rwlRV$vault
    for (f in res$fills) {
      dat <- tryCatch(fillAllGaps(dat, if (f == "0") 0 else f), error = function(e) dat)
    }
    fitCache$clear()
    rwlRV$dat   <- dat
    rwlRV$fills <- res$fills
    rwlRV$goal  <- if (isTRUE(res$goal %in% goalIds)) res$goal
    settings(res$settings)
    notes(res$notes)
    seen(res$seen)
    touched(TRUE)
    unsaved(TRUE)
    restoreOffer(NULL)
    ctlRefresh(ctlRefresh() + 1)
  })

  # ── File widget ────────────────────────────────────────────────────────────
  output$fileUI <- renderUI({
    if (is.null(rwlRV$name)) {
      # Two examples, each with a guide: say what each is and what it is for
      return(div(class = "mb-2 small",
                 div("No file loaded. Load your own below, or try one of two",
                     "examples. Each comes with a guide."),
                 div(class = "mt-2", tags$strong("Start here: "),
                     actionLink("useDemo", examples$nm046$link), tags$br(),
                     span(class = "text-muted", examples$nm046$about)),
                 div(class = "mt-2", tags$strong("Then: "),
                     actionLink("useDemo2", examples$gusPearson$link), tags$br(),
                     span(class = "text-muted", examples$gusPearson$about))))
    }
    tagList(
      div(class = "fw-bold text-truncate", title = rwlRV$label,
          bs_icon("file-earmark-text"), " ", rwlRV$label),
      # the examples not in use
      {
        other <- names(examples)[!(names(examples) == rwlRV$name & rwlRV$example)]
        ids   <- c(nm046 = "useDemo", gusPearson = "useDemo2")
        div(class = "small mb-1", if (length(other) == 1) "Other example: " else "Examples: ",
            lapply(seq_along(other), function(k) {
              tagList(if (k > 1) " \u00b7 ",
                      tags$span(title = examples[[other[k]]]$about,
                                actionLink(ids[[other[k]]], examples[[other[k]]]$link)))
            }))
      })
  })

  output$fileInfo <- renderUI({
    req(rwlRV$name)
    if (!is.null(rwlRV$readError)) {
      return(div(class = "small text-danger mb-2", bs_icon("x-circle"),
                 " Could not be read: see the Overview."))
    }
    dat <- rwlRV$dat
    req(dat)
    yrs   <- range(as.numeric(rownames(dat)))
    nGaps <- nrow(gaps())
    lv    <- levels()
    nLook <- sum(lv %in% c("warning", "error"))
    nSeen <- length(seen())
    badge <- function(n, what, cls) {
      if (n > 0) span(class = paste("badge me-1", cls), n, what)
    }
    div(class = "small text-muted mb-2",
        div(ncol(dat), " series · ", yrs[1], "–", yrs[2]),
        div(badge(nGaps, if (nGaps == 1) "gap" else "gaps", "bg-warning text-dark"),
            badge(nSeen, paste("of", ncol(dat), "looked at"), "bg-secondary")),
        # The series that need a look, by name, each a link that opens it
        # on the Detrend panel: a count alone leaves the user asking which.
        if (nLook > 0) {
          bad  <- names(lv)[lv %in% c("warning", "error")]
          show <- utils::head(bad, 6)
          div(class = "mt-1 text-warning-emphasis",
              bs_icon("exclamation-triangle"),
              paste0(" ", nLook, " to check:"),
              lapply(show, function(s) {
                tagList(" ", tags$a(href = "#", class = "link-dark",
                                    onclick = sprintf("Shiny.setInputValue('openSeries', %s, {priority: 'event'}); return false;",
                                                      jsonlite::toJSON(s, auto_unbox = TRUE)), s))
              }),
              if (length(bad) > length(show)) {
                tagList(" and ", actionLink("openCheckOrder", paste(length(bad) - length(show), "more")))
              })
        })
  })
  # Open one of them
  observeEvent(input$openSeries, {
    req(input$openSeries %in% seriesNames())
    updateSelectInput(session, "series", selected = input$openSeries)
    nav_select("navbar", "DetrendTab", session = session)
  })
  # "and 12 more": list them all first on the Detrend panel
  observeEvent(input$openCheckOrder, {
    updateSelectInput(session, "seriesOrder", selected = "check")
    lv <- levels()
    updateSelectInput(session, "series", selected = names(lv)[lv %in% c("warning", "error")][1])
    nav_select("navbar", "DetrendTab", session = session)
  })

  # ── Start over ─────────────────────────────────────────────────────────────
  observe(shinyjs::toggle("divStartOver", condition = !is.null(rwlRV$name)))

  observeEvent(input$startOver, {
    if (!isTRUE(hasWork())) return(session$reload())
    showModal(modalDialog(
      title = "Start over?",
      p("This closes the file and starts a fresh session. The settings and",
        "notes you have made would be lost."),
      p("To keep them, cancel, then save the settings file or generate the",
        "report on the Results panel."),
      footer = tagList(modalButton("Cancel"),
                       actionButton("startOverConfirm", "Start over",
                                    class = "btn-danger")),
      easyClose = TRUE))
  })
  observeEvent(input$startOverConfirm, {
    # the user has just confirmed: don't let the browser ask again
    session$sendCustomMessage("idUnsaved", FALSE)
    session$reload()
  })

  # ── Panels that need a file ────────────────────────────────────────────────
  needsFile <- c("DetrendTab", "ResultsTab")
  lapply(needsFile, function(tab) {
    output[[paste0("noData", tab)]] <- renderUI({
      if (!is.null(rwlRV$dat)) return(NULL)
      div(class = "alert alert-secondary mt-3",
          bs_icon("folder2-open"), " ",
          if (!is.null(rwlRV$readError)) {
            "The file could not be read: see the Overview panel."
          } else {
            tagList(tags$strong("Load a ring-width file to begin. "),
                    "Use ", tags$strong("Load a file…"), " in the sidebar, or try",
                    " the example data.")
          })
    })
  })
  observe({
    for (tab in needsFile) {
      shinyjs::toggle(paste0("content", tab), condition = !is.null(rwlRV$dat))
    }
  })
  observe({
    shinyjs::toggle("divSeriesSelector",
                    condition = identical(input$navbar, "DetrendTab") && !is.null(rwlRV$dat))
  })


  # ════════════════════════════════════════════════════════════════════════════
  # FITS
  # ════════════════════════════════════════════════════════════════════════════
  # Every series is detrended whenever the settings change, but only the
  # series whose settings (or data) changed are refitted: the rest come
  # from fitCache, keyed by series and holding the settings each fit was
  # made with. A change to the data (a new file, a gap fill) empties it.
  fitCache <- local({
    store <- new.env()
    list(
      get = function(name, key, make) {
        hit <- store[[name]]
        if (!is.null(hit) && identical(hit$key, key)) return(hit$fit)
        fit <- make()
        store[[name]] <- list(key = key, fit = fit)
        fit
      },
      clear = function() rm(list = ls(store, all.names = TRUE), envir = store))
  })

  allFits <- reactive({
    dat <- rwlRV$dat
    S   <- settings()
    req(dat, S, identical(S$series, names(dat)))
    yrs <- as.numeric(rownames(dat))
    fits <- lapply(seq_len(nrow(S)), function(i) {
      s <- S[i, ]
      fitCache$get(s$series, do.call(paste, c(unname(as.list(s[settingFields])), sep = "|")),
                   function() detrendOne(dat[[s$series]], s, s$series, yrs))
    })
    stats::setNames(fits, S$series)
  })

  # ── What the results are compared with ───────────────────────────────────
  # The Results panel draws the chronology and gives the statistics against
  # a baseline, to show what the user's choices changed. The baseline is
  # dplR's default detrending, detrend(rwl), unless the user has pinned a
  # set of settings (a snapshot of settings() at that moment) and chosen to
  # compare with that: "did the changes since then matter?"
  pinned <- reactiveVal(NULL)   # list(settings, time)
  usePinned <- reactive(identical(input$compareWith, "pinned") && !is.null(pinned()))
  baseSettings <- reactive({
    req(rwlRV$dat)
    if (usePinned()) pinned()$settings else defaultSettings(names(rwlRV$dat))
  })
  # TRUE when the settings are the baseline's: nothing to compare
  sameAsBase <- reactive({
    S <- settings()
    req(S)
    all(sameSettings(S, baseSettings()))
  })
  # what the baseline is, for legends, and in a word, for the statistics
  baseLabel <- reactive(if (usePinned()) paste0("The settings pinned at ", pinned()$time) else baselineLabel)
  baseShort <- reactive(if (usePinned()) "pinned" else "dplR default")

  fitsFor <- function(S) {
    dat <- rwlRV$dat
    yrs <- as.numeric(rownames(dat))
    stats::setNames(lapply(seq_len(nrow(S)), function(i) detrendOne(dat[[S$series[i]]], S[i, ], S$series[i], yrs)),
                    S$series)
  }
  # dplR's default for every series, whatever the baseline: the Detrend
  # panel compares each series with its own default
  defaultFits <- reactive({
    req(rwlRV$dat)
    fitsFor(defaultSettings(names(rwlRV$dat)))
  })
  baseFits <- reactive({
    req(rwlRV$dat)
    if (usePinned()) fitsFor(pinned()$settings) else defaultFits()
  })

  statuses <- reactive({
    S <- settings()
    fits <- allFits()
    stats::setNames(lapply(seq_len(nrow(S)), function(i) fitStatus(fits[[i]], S[i, ])), S$series)
  })
  levels <- reactive(vapply(statuses(), `[[`, "", "level"))

  # The indices, their summary and the chronology, now and at the default.
  # summary() of an rwi gives rwi.stats() and each series' correlation with
  # the others; it needs two series, and chron() one.
  rwiNow  <- reactive(buildRWI(rwlRV$dat, allFits()))
  rwiBase <- reactive(buildRWI(rwlRV$dat, baseFits()))
  quiet   <- function(expr) tryCatch(suppressWarnings(suppressMessages(expr)), error = function(e) NULL)
  #
  # summary() is the slow step: its time grows with the square of the number
  # of series (10 s for 200 series and 30 s for 400 with dplR 1.8.0). Up to
  # statsAutoMax series it is computed as the settings change. Above that
  # it is computed when the user asks (a button on the Results panel) and
  # held until the indices change.
  statsAutoMax <- 120
  statsAuto <- reactive(!is.null(rwlRV$dat) && ncol(rwlRV$dat) <= statsAutoMax)

  # Which series are cores of the same tree, read from the series names
  # (treeIds() in appHelpers.R). rbar, EPS and the statistics through time
  # count trees, not cores.
  idsNow <- reactive({
    req(rwlRV$dat)
    mode <- if (is.null(input$idsMode)) "auto" else input$idsMode
    stc  <- c(input$stcSite, input$stcTree, input$stcCore)
    if (mode == "position" && (length(stc) != 3 || anyNA(stc) || any(stc < 0) || any(stc != round(stc)))) {
      return(list(ids = NULL, warn = character(0), code = NULL,
                  error = "Give three whole numbers: the characters for the site, the tree and the core."))
    }
    treeIds(rwlRV$dat, mode, if (mode == "position") as.integer(stc) else c(3, 2, 1))
  })

  # The window, in years, of the statistics through time
  runWin <- reactive({
    w <- input$runWin
    if (is.null(w) || is.na(w) || w < 10) 50 else round(w)
  })
  sumOf <- function(x, ids) {
    if (is.null(x) || ncol(x) < 2) NULL else quiet(summary(x, ids = idsFor(ids, x)))
  }
  # rwi.stats.running() result, or the reason there is none, as text
  runOf <- signalThroughTime

  # What the held statistics were computed from: they stand only while all
  # of it is unchanged
  statsHeld <- reactiveVal(NULL)   # list(key, now, run, sss, baseKey, base, baseRun, baseSss)
  nowKey  <- reactive(list(rwiNow(), idsNow()$ids, runWin()))
  baseKey <- reactive(list(rwlRV$dat, baseSettings(), idsNow()$ids, runWin()))
  held <- function(what, key, keyName) {
    h <- statsHeld()
    if (!is.null(h) && identical(h[[keyName]], key)) h[[what]] else NULL
  }
  sumNow  <- reactive(if (statsAuto()) sumOf(rwiNow(), idsNow()$ids) else held("now", nowKey(), "key"))
  sumBase <- reactive(if (statsAuto()) sumOf(rwiBase(), idsNow()$ids) else held("base", baseKey(), "baseKey"))
  runNow  <- reactive(if (statsAuto()) runOf(rwiNow(), idsNow()$ids, runWin()) else held("run", nowKey(), "key"))
  runBase <- reactive(if (statsAuto()) runOf(rwiBase(), idsNow()$ids, runWin()) else held("baseRun", baseKey(), "baseKey"))
  # SSS for each year (sssOf() in appHelpers.R), with the other statistics
  sssNow  <- reactive(if (statsAuto()) sssOf(rwiNow(), idsNow()$ids) else held("sss", nowKey(), "key"))
  sssBase <- reactive(if (statsAuto()) sssOf(rwiBase(), idsNow()$ids) else held("baseSss", baseKey(), "baseKey"))
  observeEvent(input$computeStats, {
    ids <- idsNow()$ids
    h   <- statsHeld()
    keepBase <- !is.null(h) && identical(h$baseKey, baseKey())
    statsHeld(list(key = nowKey(), now = sumOf(rwiNow(), ids), run = runOf(rwiNow(), ids, runWin()),
                   sss = sssOf(rwiNow(), ids),
                   baseKey = baseKey(),
                   base    = if (keepBase) h$base else sumOf(rwiBase(), ids),
                   baseRun = if (keepBase) h$baseRun else runOf(rwiBase(), ids, runWin()),
                   baseSss = if (keepBase) h$baseSss else sssOf(rwiBase(), ids)))
  })

  # The chronology, of the kind chosen on the Results panel (buildChron() in
  # appHelpers.R). crnTry() gives the chronology, or dplR's message when it
  # cannot be built (a window longer than the chronology, say).
  crnOpts <- reactive({
    w <- input$crnWin
    list(type     = if (isTRUE(input$crnType %in% chronTypes)) input$crnType else "std",
         biweight = !isFALSE(input$crnBiweight),
         win      = if (is.null(w) || is.na(w)) 51 else round(w))
  })
  crnTry <- function(x) {
    if (is.null(x)) return("No series could be detrended, so there is no chronology.")
    o <- crnOpts()
    tryCatch(suppressWarnings(suppressMessages(buildChron(x, o$type, o$biweight, o$win))),
             error = function(e) paste0("dplR stopped with this message: ", conditionMessage(e), "."))
  }
  crnResNow <- reactive(crnTry(rwiNow()))
  crnNow    <- reactive({ x <- crnResNow(); if (is.data.frame(x)) x else NULL })
  crnBase   <- reactive({ x <- crnTry(rwiBase()); if (is.data.frame(x)) x else NULL })
  allDefault <- reactive({
    S <- settings()
    req(S)
    all(sameSettings(S, defaultSettings(S$series)))
  })


  # ════════════════════════════════════════════════════════════════════════════
  # OVERVIEW
  # ════════════════════════════════════════════════════════════════════════════

  # Interior gaps in the working data (years with no measurement inside a
  # series). dplR >= 1.8.0 reads these as NA, and detrend.series() stops on
  # them.
  gaps <- reactive({
    req(rwlRV$dat)
    rwlGaps(rwlRV$dat)
  })

  # dplR's rwl.check(), without its crossdating checks: this app takes the
  # dating as given.
  rwlCheck <- reactive({
    req(rwlRV$dat)
    res <- tryCatch(suppressWarnings(suppressMessages(
      rwl.check(rwlRV$dat, checks = c("structure", "series", "values", "zeros")))),
      error = function(e) e)
    if (inherits(res, "error")) return(NULL)
    as.data.frame(res)
  }) |> bindCache(rwlRV$dat, cache = "session")

  # The data summary: label/value rows from summary() of the rwl
  dataSummary <- reactive({
    dat <- rwlRV$dat
    req(dat)
    st  <- summary(dat)
    v   <- unlist(dat, use.names = FALSE)
    v   <- v[!is.na(v)]
    yrs <- range(as.numeric(rownames(dat)))
    list(c("Series", ncol(dat)),
         c("Span", paste(yrs[1], "–", yrs[2])),
         c("Rings measured", format(length(v), big.mark = ",")),
         c("Series length, mean (range)",
           paste0(round(mean(st$year)), " (", min(st$year), "–", max(st$year), ")")),
         c("Absent rings (zeros)", paste0(sum(v == 0), " (", round(mean(v == 0) * 100, 2), "%)")),
         c("Mean ring width", round(mean(v), 3)),
         c("Mean first-order autocorrelation", round(mean(st$ar1, na.rm = TRUE), 3)))
  })

  output$overviewUI <- renderUI({
    if (is.null(rwlRV$name)) return(HTML(welcomeSVG))

    if (!is.null(rwlRV$readError)) {
      return(div(class = "alert alert-danger",
                 bs_icon("x-circle"), " ",
                 tags$strong("File could not be read."),
                 " dplR's ", tags$code("read.rwl()"), " returned the following error:",
                 tags$pre(class = "mt-2 mb-1", style = "font-size:0.85em;",
                          rwlRV$readError),
                 "Please open a plain R session, load dplR, and run ",
                 tags$code('read.rwl("your-file.rwl")'),
                 " to diagnose the problem. Fix the file and then reload it here."))
    }

    tagList(
      uiOutput("checkPanel"),
      # ── Where to start (goals.R) ──
      card(
        fill = FALSE,
        card_header(
          "What is the chronology for?",
          tooltip(bs_icon("question-circle"),
                  paste("The answer sets the curve every series starts with. It is a",
                        "starting point: you can change any series on the Detrend",
                        "panel, and skip this altogether to start from dplR's default."))),
        layout_columns(
          col_widths = c(5, 7),
          div(
            radioButtons("goal", NULL, width = "100%",
                         choiceNames  = lapply(goals, `[[`, "label"),
                         choiceValues = goalIds,
                         selected = { g <- isolate(rwlRV$goal); if (is.null(g)) "default" else g }),
            actionButton("goalApply", "Start every series this way", class = "btn-primary"),
            uiOutput("goalApplied")),
          uiOutput("goalDetail"))
      ),
      card(
        fill = FALSE,
        card_header(
          "Data Summary",
          tooltip(bs_icon("question-circle"),
                  paste("Check these to confirm your file was read correctly,",
                        "then go to the Detrend panel."))),
        div(lapply(dataSummary(), function(item) {
          div(class = "row mb-1",
              div(class = "col-7 text-muted small", item[1]),
              div(class = "col-5 fw-bold small text-end", item[2]))
        }))
      ),
      card(
        fill = FALSE,
        card_header(
          layout_columns(
            col_widths = c(8, 4),
            span("RWL Plot",
                 tooltip(bs_icon("question-circle"),
                         paste("Segment view: each series as a bar spanning its",
                               "dated range. Spaghetti view: all ring-width series",
                               "as lines, where the growth trends that detrending",
                               "removes can be seen. Uses plot.rwl() from dplR."))),
            div(class = "d-flex justify-content-end",
                selectInput("rwlPlotType", NULL, width = "150px",
                            choices = c("Segment" = "seg", "Spaghetti" = "spag")))
          )
        ),
        plotOutput("rwlPlot", height = "400px")
      ),
      accordion(
        open = FALSE,
        accordion_panel(
          title = "Series Summary Table",
          icon  = bs_icon("table"),
          helpText("Summary statistics for each series from summary.rwl()."),
          tableOutput("rwlSummary")
        )
      )
    )
  })

  # ── Where to start ─────────────────────────────────────────────────────────
  # What the selected goal starts every series with, why, what it does in
  # years to a series of typical length in this file, and what to watch.
  output$goalDetail <- renderUI({
    req(rwlRV$dat, input$goal %in% goalIds)
    g    <- goalById(input$goal)
    n    <- stats::median(colSums(!is.na(rwlRV$dat)))
    says <- curveSays(goalSettings(g$id, "x"), round(n))
    div(class = "small",
        p(tags$strong("Starts with: "), g$starts),
        p(tags$strong("Why: "), g$why),
        if (length(says)) p(tags$strong(sprintf("On a typical series here (%d rings): ", round(n))),
                            paste(says[-1], collapse = " ")),
        p(class = "mb-0", tags$strong("Watch for: "), g$watch))
  })

  applyGoal <- function(id) {
    if (id != "default" && isTRUE(guideHere()) && isTRUE(guideDef()$fromDefault) &&
        !is.na(guideStep())) guideRV$on <- FALSE
    pushUndo("the starting point")
    # a starting point is about the curve: the rings each series uses stay
    S   <- settings()
    new <- goalSettings(id, S$series)
    new[c("first", "last")] <- S[c("first", "last")]
    settings(new)
    rwlRV$goal <- id
    unsaved(TRUE)
    ctlRefresh(ctlRefresh() + 1)
  }
  # Ask first when it would replace settings the user has changed by hand:
  # ones that differ from where the last starting point left them
  observeEvent(input$goalApply, {
    req(settings(), input$goal %in% goalIds)
    S    <- settings()
    from <- goalSettings(if (is.null(rwlRV$goal)) "default" else rwlRV$goal, S$series)
    n    <- sum(!sameSettings(S, from, copyFields))
    # the guided example is written for the default starting point
    guiding <- isTRUE(guideRV$on) && isTRUE(guideHere()) && !is.na(guideStep()) &&
      isTRUE(guideDef()$fromDefault) && input$goal != "default"
    if (n == 0 && !guiding) return(applyGoal(input$goal))
    showModal(modalDialog(
      title = if (n > 0) "Replace the settings of every series?" else "Leave the guided example?",
      if (n > 0) p("You have changed the settings of", n, if (n == 1) "series" else "series",
        "by hand. Starting every series this way replaces them. Your notes are kept."),
      if (guiding) p("The guided example is running, and its steps are written for dplR's",
                     "default starting point. Applying another one hides the guide. You can",
                     "start it again from the sidebar; it will put the example back to the default."),
      footer = tagList(modalButton("Cancel"),
                       actionButton("goalConfirm", "Replace them", class = "btn-danger")),
      easyClose = TRUE))
  })
  observeEvent(input$goalConfirm, {
    removeModal()
    applyGoal(input$goal)
  })
  output$goalApplied <- renderUI({
    req(rwlRV$goal)
    starts <- goalById(rwlRV$goal)$starts
    div(class = "small text-success mt-2", bs_icon("check-circle"),
        paste0(" Applied: ", tolower(substr(starts, 1, 1)), substring(starts, 2)),
        actionLink("goDetrend", "Go to the Detrend panel"), "to look at each series.")
  })
  observeEvent(input$goDetrend, nav_select("navbar", "DetrendTab", session = session))

  # ── What needs attention before detrending ────────────────────────────────
  # Series left out on loading, gaps (with the controls to fill them), and
  # rwl.check()'s errors and warnings.
  output$checkPanel <- renderUI({
    req(rwlRV$dat)
    g     <- gaps()
    lines <- { f <- rwlCheck(); if (is.null(f)) character(0) else checkLines(f) }
    tagList(
      if (length(rwlRV$dropped)) {
        div(class = "alert alert-warning",
            bs_icon("exclamation-triangle"), " ",
            tags$strong(length(rwlRV$dropped),
                        if (length(rwlRV$dropped) == 1) "series has" else "series have",
                        "no measurements and",
                        if (length(rwlRV$dropped) == 1) "was" else "were", "left out: "),
            paste(rwlRV$dropped, collapse = ", "), ".")
      },
      if (nrow(g) > 0) {
        gs <- split(g, g$series)
        card(
          fill = FALSE, class = "border-warning",
          card_header(class = "bg-warning-subtle",
                      bs_icon("exclamation-triangle"),
                      sprintf(" %d series %s years with no measurement inside %s",
                              length(gs), if (length(gs) == 1) "has" else "have",
                              if (length(gs) == 1) "it" else "them")),
          p("A curve cannot be fitted across a gap, so",
            if (length(gs) == 1) "this series" else "these series",
            "cannot be detrended until the gaps are filled.",
            "The file may record a broken core or unmeasured rings this way,",
            "or use it for rings that were absent."),
          tags$ul(class = "small", lapply(names(gs), function(s) {
            tags$li(tags$strong(s), ": ", paste(formatGaps(gs[[s]]), collapse = ", "))
          })),
          layout_columns(
            col_widths = c(5, 3),
            selectInput("fillMethod", tipLabel("Fill every gap with",
              paste("Zero: the rings were absent. Linear: a straight line between",
                    "the rings on either side. Mean: the mean of the series. A",
                    "filled value is an estimate, not a measurement; the report",
                    "records the fill.")),
              choices = c("Zero (the rings were absent)" = "0",
                          "Linear interpolation" = "Linear",
                          "The series mean" = "Mean")),
            div(class = "form-group shiny-input-container w-100",
                tags$label(class = "control-label", HTML("&nbsp;")),
                actionButton("fillGapsButton", "Fill the gaps", class = "btn-primary w-100"))
          )
        )
      },
      if (length(rwlRV$fills)) {
        div(class = "alert alert-info d-flex align-items-center",
            div(class = "flex-grow-1", bs_icon("info-circle"),
                " Gaps in this file were filled (",
                paste(vapply(rwlRV$fills, function(f) if (f == "0") "zero" else tolower(f), ""),
                      collapse = ", then "),
                "). The report records it."),
            actionButton("undoFills", "Undo", class = "id-btn-quiet btn-sm"))
      },
      if (length(lines)) {
        card(
          fill = FALSE,
          card_header("Data Checks",
                      tooltip(bs_icon("question-circle"),
                              paste("From dplR's rwl.check(), without its crossdating",
                                    "checks: this app takes the dating as given. Use",
                                    "xDateR to check the dating."))),
          tags$ul(class = "mb-0", lapply(lines, tags$li))
        )
      }
    )
  })

  observeEvent(input$fillGapsButton, {
    req(rwlRV$dat, input$fillMethod)
    fill <- if (input$fillMethod == "0") 0 else input$fillMethod
    res  <- tryCatch(fillAllGaps(rwlRV$dat, fill), error = function(e) e)
    if (inherits(res, "error")) {
      showNotification(conditionMessage(res), type = "error", duration = NULL)
      return()
    }
    fitCache$clear()
    rwlRV$dat   <- res
    rwlRV$fills <- c(rwlRV$fills, list(input$fillMethod))
    unsaved(TRUE)
  })
  observeEvent(input$undoFills, {
    fitCache$clear()
    rwlRV$dat   <- rwlRV$vault
    rwlRV$fills <- list()
  })

  output$rwlPlot <- renderPlot({
    req(rwlRV$dat, input$rwlPlotType)
    # dplR 1.8.0's segment plot stops on a one-series file
    validate(need(ncol(rwlRV$dat) > 1 || input$rwlPlotType != "seg",
                  "The segment plot needs two or more series. Choose the spaghetti plot to see this one."))
    plot(rwlRV$dat, plot.type = input$rwlPlotType)
  })

  output$rwlSummary <- renderTable({
    req(rwlRV$dat)
    summary(rwlRV$dat)
  })


  # ════════════════════════════════════════════════════════════════════════════
  # DETREND: THE SERIES SELECTOR
  # ════════════════════════════════════════════════════════════════════════════

  # The selected series, or NULL while the selector still holds a name from
  # the previous file
  curSeries <- reactive({
    s <- input$series
    if (isTRUE(s %in% seriesNames())) s else NULL
  })

  # ── The order of the series ────────────────────────────────────────────────
  # The order they are listed and stepped through in: as in the file, or
  # those needing a look first, or by length (seriesOrder() in
  # appHelpers.R). The order is fixed when it is chosen and does not shift
  # as series are fixed: a series that jumped down the list the moment it
  # was put right would take "Next" with it, past the ones still waiting.
  # Choose the order again to re-sort.
  seriesList <- reactiveVal(NULL)
  sortSeries <- function() {
    nms <- seriesNames()
    if (is.null(nms)) return(seriesList(NULL))
    how <- if (is.null(input$seriesOrder)) "file" else input$seriesOrder
    lv  <- if (how == "check") tryCatch(levels(), error = function(e) NULL)
    seriesList(seriesOrder(nms, if (how == "check" && is.null(lv)) "file" else how, lv,
                           colSums(!is.na(rwlRV$dat))[nms]))
  }
  observeEvent(seriesNames(), {
    updateSelectInput(session, "seriesOrder", selected = "file")
    seriesList(seriesNames())
  }, ignoreNULL = FALSE)
  observeEvent(input$seriesOrder, sortSeries(), ignoreInit = TRUE)

  # A tick marks series that have been looked at, and a warning sign those
  # that need a look (a fit that fell back, a curve near zero, a series
  # that could not be detrended).
  observeEvent(list(seriesList(), seen(), levels()), {
    nms <- seriesList()
    if (is.null(nms)) {
      updateSelectInput(session, "series", choices = c("Load a file first" = ""))
      return()
    }
    lv  <- levels()
    lbl <- ifelse(nms %in% seen(), paste("\u2713", nms), nms)
    lbl <- ifelse(lv[nms] %in% c("warning", "error"), paste(lbl, "\u26a0"), lbl)
    sel <- if (isTRUE(input$series %in% nms)) input$series else nms[1]
    updateSelectInput(session, "series", choices = stats::setNames(nms, lbl), selected = sel)
  }, ignoreNULL = FALSE)

  # A series counts as looked at once it has been shown on the Detrend panel
  observe({
    s <- curSeries()
    req(s, identical(input$navbar, "DetrendTab"))
    if (!s %in% isolate(seen())) seen(c(isolate(seen()), s))
  })

  stepSeries <- function(by) {
    nms <- seriesList()
    i   <- match(curSeries(), nms)
    req(!is.na(i))
    j <- i + by
    if (j >= 1 && j <= length(nms)) updateSelectInput(session, "series", selected = nms[j])
  }
  observeEvent(input$prevSeries, stepSeries(-1))
  observeEvent(input$nextSeries, stepSeries(1))

  # Previous and Next are greyed out at the first and last series
  observe({
    nms <- seriesList()
    i   <- match(curSeries(), nms)
    shinyjs::toggleState("prevSeries", condition = isTRUE(i > 1))
    shinyjs::toggleState("nextSeries", condition = isTRUE(i < length(nms)))
  })

  # ── Every series looked at ─────────────────────────────────────────────────
  # Once the last series has been shown, say so and point to the Results.
  # A reactiveVal, so the message is drawn when this turns TRUE and not
  # again with every series viewed after that (it can be closed).
  allSeen <- reactiveVal(FALSE)
  observe({
    nms <- seriesNames()
    allSeen(!is.null(nms) && all(nms %in% seen()))
  })
  # how many series still need a look, held the same way: the message is
  # redrawn when the count changes, not with every change to a setting
  nToCheck <- reactiveVal(0)
  observe(nToCheck(sum(levels() %in% c("warning", "error"))))
  output$allSeenUI <- renderUI({
    req(allSeen())
    n <- length(isolate(seriesNames()))
    k <- nToCheck()
    div(class = "alert alert-success alert-dismissible fade show", role = "alert",
        bs_icon("check-circle"), " ",
        tags$strong(if (n == 1) "You have chosen a curve for the series."
                    else sprintf("You have chosen a curve for all %d series.", n)),
        " See what they give on the ",
        actionLink("goResults", "Results panel", class = "alert-link"),
        ". You can come back and change any fit whenever you like.",
        if (k > 0) paste0(" ", k, if (k == 1) " series is" else " series are",
                          " still marked \u26a0: the message under the plot says why."),
        tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "alert",
                    `aria-label` = "Close"))
  })
  observeEvent(input$goResults, nav_select("navbar", "ResultsTab", session = session))

  # The next series after this one that needs a look, wrapping round
  nextToCheck <- reactive({
    nms <- seriesList()
    i   <- match(curSeries(), nms)
    if (is.null(nms) || is.na(i)) return(NULL)
    bad <- which(levels()[nms] %in% c("warning", "error"))
    bad <- setdiff(bad, i)
    if (length(bad) == 0) return(NULL)
    nms[c(bad[bad > i], bad)[1]]
  })
  observeEvent(input$nextCheck, {
    req(nextToCheck())
    updateSelectInput(session, "series", selected = nextToCheck())
  })

  output$seriesProgress <- renderUI({
    nms <- seriesList()
    s   <- curSeries()
    req(nms, s)
    nLook <- sum(levels() %in% c("warning", "error"))
    div(class = "small text-muted mt-2",
        div(sprintf("Series %d of %d · %d looked at", match(s, nms), length(nms),
                    length(seen()))),
        if (!is.null(nextToCheck())) {
          div(actionLink("nextCheck", tagList(bs_icon("exclamation-triangle"),
                                              sprintf(" Next to check (%d)", nLook))))
        })
  })


  # ════════════════════════════════════════════════════════════════════════════
  # DETREND: THE CONTROLS
  # ════════════════════════════════════════════════════════════════════════════
  # The controls show one series' row of the settings and write changes back
  # to it. They are rebuilt, with new input ids, each time the series
  # changes or its row is changed from elsewhere (reset, a settings file).
  # The ids end in a number that goes up with every rebuild (ctlKey()$id),
  # so the server never reads a value typed for another series, or one left
  # in the browser from before the row changed: until the new controls
  # report, their inputs do not exist.
  ctlRefresh <- reactiveVal(0)
  ctlCount   <- 0
  ctlKey <- reactive({
    s <- curSeries()
    ctlRefresh()
    if (is.null(s)) return(NULL)
    ctlCount <<- ctlCount + 1
    list(id = ctlCount, series = s)
  })

  # First and last measured year of a series
  seriesSpan <- function(series) {
    dat <- isolate(rwlRV$dat)
    range(as.numeric(rownames(dat))[!is.na(dat[[series]])])
  }

  output$controlsTitle <- renderText({
    s <- curSeries()
    if (is.null(s)) "Settings" else paste("Settings for", s)
  })

  output$controls <- renderUI({
    key <- ctlKey()
    req(key)
    S   <- isolate(settings())
    s   <- S[match(key$series, S$series), ]
    n   <- sum(!is.na(isolate(rwlRV$dat)[[key$series]]))
    id  <- function(x) paste0(x, "_", key$id)
    is  <- function(x, val) sprintf("input['%s'] == '%s'", id(x), val)
    prop    <- s$spl.nyrs <= 1
    dfltYrs <- max(2, floor(0.67 * n))
    span    <- seriesSpan(key$series)
    note    <- isolate(notes())[[key$series]]

    tagList(
      selectInput(id("method"), tipLabel("Method",
        paste("The curve fitted to the series. Splines and Friedman's smoother",
              "bend to the data, as much as their settings allow. The negative",
              "exponential and Hugershoff curves are models of how a tree's",
              "rings narrow with age, and fall back to a straight line or the",
              "mean when the series does not have that shape. The",
              "autoregressive model is not a curve: it leaves only",
              "year-to-year variation.")),
        choices = methodChoices, selected = s$method),

      # ── Smoothing spline ──
      conditionalPanel(
        condition = is("method", "Spline"),
        radioButtons(id("splMode"), tipLabel("Rigidity as",
          paste("A share of the series length gives each series its own number",
                "of years, so short and long series are treated alike. A fixed",
                "number of years removes the same wavelengths from every series.")),
          choices = c("Share of the series length" = "prop", "Years" = "years"),
          selected = if (prop) "prop" else "years"),
        conditionalPanel(
          condition = is("splMode", "prop"),
          sliderInput(id("splProp"), tipLabel("Rigidity (share of length)",
            paste0("The spline keeps half the variation at a wavelength of this share",
                   " of the series' ", n, " rings, more at longer wavelengths and less",
                   " at shorter. Lower is more flexible. dplR's default is 0.67.")),
            min = 0.1, max = 1, step = 0.01, ticks = FALSE,
            value = if (prop) s$spl.nyrs else 0.67)),
        conditionalPanel(
          condition = is("splMode", "years"),
          sliderInput(id("splYears"), tipLabel("Rigidity (years)",
            paste0("The spline keeps half the variation at this wavelength, more at",
                   " longer wavelengths and less at shorter. Lower is more flexible.",
                   " This series has ", n, " rings.")),
            min = 2, max = max(20, ceiling(2 * n / 10) * 10), step = 1, ticks = FALSE,
            value = if (prop) dfltYrs else s$spl.nyrs)),
        numericInput(id("splF"), tipLabel("Frequency response (f)",
          paste("The share of variation the spline keeps at the rigidity",
                "wavelength. 0.5 is the convention; leave it unless you have a reason.")),
          value = s$spl.f, min = 0.05, max = 0.95, step = 0.05)
      ),

      # ── Age-dependent spline ──
      conditionalPanel(
        condition = is("method", "AgeDepSpline"),
        sliderInput(id("adsNyrs"), tipLabel("Starting rigidity (years)",
          paste("The spline is flexible where the tree is young and growth changes",
                "fast, and stiffens by a year with every ring. This is its rigidity",
                "at the first ring. dplR's default is 50.")),
          min = 2, max = max(100, ceiling(n / 10) * 10), step = 1, ticks = FALSE,
          value = s$ads.nyrs)
      ),

      # ── Shared by the age-dependent spline and the two growth models ──
      conditionalPanel(
        condition = sprintf("['AgeDepSpline', 'ModNegExp', 'ModHugershoff'].includes(input['%s'])",
                            id("method")),
        checkboxInput(id("posSlope"), tipLabel("Allow a positive slope",
          paste("Age-dependent spline: lets the curve rise at the end of the",
                "series; unticked, it is held level from its lowest point.",
                "Negative exponential and Hugershoff: applies to the straight",
                "line fitted when the curve cannot be; unticked, a rising line",
                "is replaced by the mean. A rising curve removes an increase in",
                "growth, which may be the signal you are after.")),
          value = s$pos.slope)
      ),
      conditionalPanel(
        condition = sprintf("['ModNegExp', 'ModHugershoff'].includes(input['%s'])", id("method")),
        selectInput(id("constrain"), tipLabel("Constrain the curve",
          paste("Whether the fit is kept to curves of the expected shape",
                "(detrend.series()'s constrain.nls). Never: fit freely, and fall",
                "back to a straight line when the result is not usable. When the",
                "fit fails: try the constrained fit before falling back. Always:",
                "use the constrained fit.")),
          choices = c("Never" = "never", "When the fit fails" = "when.fail", "Always" = "always"),
          selected = s$constrain)
      ),

      # ── Friedman ──
      conditionalPanel(
        condition = is("method", "Friedman"),
        checkboxInput(id("spanCV"), tipLabel("Choose the span by cross-validation",
          paste("The span is the share of the series the smoother looks at around",
                "each ring. Ticked, the smoother picks it, ring by ring, and bass",
                "below steers it. Unticked, you set one span for the whole series.")),
          value = is.na(s$span)),
        conditionalPanel(
          condition = sprintf("!input['%s']", id("spanCV")),
          sliderInput(id("span"), "Span (share of the series)",
                      min = 0.05, max = 1, step = 0.05, ticks = FALSE,
                      value = if (is.na(s$span)) 0.5 else s$span)),
        sliderInput(id("bass"), tipLabel("Bass",
          paste("Smoothness when the span is chosen by cross-validation: 0 is the",
                "smoother's own choice, 10 the smoothest.")),
          min = 0, max = 10, step = 1, ticks = FALSE, value = s$bass)
      ),

      conditionalPanel(
        condition = is("method", "Mean"),
        helpText("A horizontal line at the series mean. No trend is removed: the",
                 "series is only rescaled.")
      ),
      conditionalPanel(
        condition = is("method", "Ar"),
        helpText("The residuals of an autoregressive model, also called",
                 "prewhitening. There is no curve to draw, and all slow variation",
                 "is removed.")
      ),

      # what these settings remove and keep, in years (rendered below)
      uiOutput("curveSays"),

      hr(),
      radioButtons(id("index"), tipLabel("Make the index by",
        paste("Division gives a ratio centred on 1, and is the usual choice for",
              "raw ring widths: it also evens out the variance, which is larger",
              "where rings are wide. Subtraction gives a difference centred on 0,",
              "and is the usual choice after a power transform. Use the same one",
              "for every series.")),
        choices = c("Division (width / curve)" = "ratio",
                    "Subtraction (width − curve)" = "diff"),
        selected = if (s$difference) "diff" else "ratio"),
      selectInput(id("powt"), tipLabel("Power transform",
        paste("Transforms the ring widths before the curve is fitted so that their",
              "variance no longer depends on their size (dplR's powt(), Cook and",
              "Peters 1997). Rescaled: the transformed series is given back the",
              "mean and variance of the original. Usually followed by indexing by",
              "subtraction, and applied to every series or none.")),
        choices = powtChoices, selected = s$powt),

      hr(),
      sliderInput(id("trim"), tipLabel("Rings to use",
        paste("Drag the ends in to leave out the first or last years of this",
              "series: juvenile rings near the pith that no curve fits, or a",
              "damaged or doubtful end. The rings left out get no index, so the",
              "series is not in the chronology for those years. The plot shows",
              "them dotted. This belongs to the series: it is not copied to",
              "other series.")),
        min = span[1], max = span[2], step = 1, sep = "", ticks = FALSE, width = "100%",
        value = c(if (is.na(s$first)) span[1] else max(span[1], s$first),
                  if (is.na(s$last)) span[2] else min(span[2], s$last))),

      selectizeInput("compare", tipLabel("Compare with",
        paste("Draws the curves other methods would fit to this series, at their",
              "settings for this series, over the plot. The choice stays as you",
              "move between series.")),
        choices = methodChoices[methodChoices != "Ar"], multiple = TRUE,
        selected = isolate(input$compare),
        options = list(placeholder = "Other methods' curves",
                       plugins = list("remove_button"))),

      textAreaInput(id("note"), tipLabel(paste("Notes on", key$series),
        paste("Why you chose this curve. Kept for each series and printed in the",
              "report, so the choice can be defended later.")),
        value = if (is.null(note)) "" else note, width = "100%", height = "90px",
        placeholder = "Why this curve? Notes go in the report."),

      actionButton("resetSeries", "Back to dplR's default",
                   icon = bs_icon("arrow-counterclockwise"),
                   class = "id-btn-quiet btn-sm w-100")
    )
  })

  output$curveSays <- renderUI({
    s <- curRow()
    says <- curveSays(s, sum(!is.na(curFit()$x)))   # the rings used, after any trimming
    req(length(says) > 0)
    div(class = "small text-muted border-start border-3 ps-2 mb-2", lapply(says, div))
  })

  # ── Controls -> settings ───────────────────────────────────────────────────
  # Reads the current controls and writes what differs to the series' row.
  # Waits until every control has reported: they arrive together when the
  # controls are built.
  observe({
    key <- ctlKey()
    req(key)
    v <- lapply(stats::setNames(nm = c("method", "splMode", "splProp", "splYears", "splF",
                                       "adsNyrs", "posSlope", "constrain", "spanCV", "span",
                                       "bass", "index", "powt", "trim", "note")),
                function(x) input[[paste0(x, "_", key$id)]])
    if (any(vapply(v, is.null, logical(1)))) return()
    # a number box that has been cleared, or holds a value dplR would refuse
    num <- c("splProp", "splYears", "splF", "adsNyrs", "span", "bass")
    if (any(vapply(v[num], function(x) !is.numeric(x) || is.na(x), logical(1)))) return()
    if (length(v$trim) != 2 || anyNA(v$trim)) return()
    if (v$splF <= 0 || v$splF >= 1) return()
    new <- list(method     = v$method,
                spl.nyrs   = if (v$splMode == "prop") v$splProp else v$splYears,
                spl.f      = v$splF,
                ads.nyrs   = v$adsNyrs,
                pos.slope  = isTRUE(v$posSlope),
                constrain  = v$constrain,
                span       = if (isTRUE(v$spanCV)) NA_real_ else v$span,
                bass       = v$bass,
                difference = v$index == "diff",
                powt       = v$powt,
                # the slider at an end of the series means "from the first
                # ring" or "to the last": stored as NA
                first      = if (v$trim[1] <= seriesSpan(key$series)[1]) NA_real_ else v$trim[1],
                last       = if (v$trim[2] >= seriesSpan(key$series)[2]) NA_real_ else v$trim[2])
    isolate({
      S <- settings()
      i <- match(key$series, S$series)
      if (is.na(i)) return()
      row <- S[i, ]
      row[settingFields] <- new[settingFields]
      if (!sameSettings(row, S[i, ])) {
        pushUndo(paste("change to", key$series), tag = paste("edit", key$series))
        S[i, ] <- row
        settings(S)
        unsaved(TRUE)
      }
      old <- notes()[[key$series]]
      if (!identical(v$note, if (is.null(old)) "" else old)) {
        n <- notes()
        n[[key$series]] <- v$note
        notes(n)
        unsaved(TRUE)
      }
    })
  })

  # ── Undo ───────────────────────────────────────────────────────────────────
  # One step back at a time for anything that changes the settings: a
  # control, a copy to other series, a reset, a starting point, a settings
  # file. Each entry is the whole settings table as it was before the
  # change (and the starting point), with a few words saying what the
  # change was. Changes made with the controls of one series within a few
  # seconds are one entry, so dragging a slider is one step to undo, not
  # fifty. Notes are not part of it: undoing a curve should not delete what
  # was written about it.
  history <- reactiveVal(list())
  pushUndo <- function(what, tag = NULL) {
    h    <- history()
    last <- if (length(h)) h[[length(h)]]
    if (!is.null(tag) && !is.null(last) && identical(last$tag, tag) &&
        difftime(Sys.time(), last$at, units = "secs") < 4) return(invisible())
    h <- c(h, list(list(what = what, tag = tag, at = Sys.time(),
                        settings = settings(), goal = rwlRV$goal)))
    history(utils::tail(h, 25))
  }
  observeEvent(input$undo, {
    h <- history()
    req(length(h) > 0)
    last <- h[[length(h)]]
    history(h[-length(h)])
    settings(last$settings)
    rwlRV$goal <- last$goal
    unsaved(TRUE)
    ctlRefresh(ctlRefresh() + 1)
  })
  output$undoUI <- renderUI({
    h <- history()
    req(length(h) > 0, rwlRV$dat)
    actionButton("undo", paste("Undo:", h[[length(h)]]$what),
                 icon = bs_icon("arrow-counterclockwise"),
                 class = "id-btn-quiet btn-sm w-100 mb-2 text-truncate",
                 title = "Takes back the last change to the settings (Ctrl+Z)")
  })

  # Writes rows of the settings from somewhere other than the controls, and
  # rebuilds the controls so they show the new values
  setRows <- function(series, values, fields = settingFields) {
    S <- settings()
    i <- match(series, S$series)
    S[i, fields] <- values[rep_len(seq_len(nrow(values)), length(i)), fields]
    settings(S)
    unsaved(TRUE)
    ctlRefresh(ctlRefresh() + 1)
  }

  observeEvent(input$resetSeries, {
    s <- curSeries()
    req(s)
    pushUndo(paste("reset of", s))
    setRows(s, defaultSettings(s))
  })

  # ── Copy settings to other series ─────────────────────────────────────────
  observeEvent(input$copyAll, {
    updateSelectizeInput(session, "copyTo", selected = setdiff(seriesNames(), curSeries()))
  })
  observeEvent(input$copyUnseen, {
    updateSelectizeInput(session, "copyTo",
                         selected = setdiff(seriesNames(), c(seen(), curSeries())))
  })
  observeEvent(input$copyNone, updateSelectizeInput(session, "copyTo", selected = character(0)))
  observeEvent(input$copyFlagged, {
    lv <- levels()
    updateSelectizeInput(session, "copyTo",
                         selected = setdiff(names(lv)[lv %in% c("warning", "error")], curSeries()))
  })
  # By length: series with fewer than, or at least, so many rings are added
  # to those already picked
  observeEvent(input$copyRuleAdd, {
    n <- input$copyRuleN
    if (is.null(n) || is.na(n)) {
      showNotification("Give a number of rings.", type = "warning")
      return()
    }
    rings <- colSums(!is.na(rwlRV$dat))
    hit   <- names(rings)[if (identical(input$copyRuleOp, "ge")) rings >= n else rings < n]
    hit   <- setdiff(hit, curSeries())
    if (length(hit) == 0) {
      showNotification("No other series has that many rings.", type = "warning")
      return()
    }
    updateSelectizeInput(session, "copyTo", selected = union(intersect(input$copyTo, seriesNames()), hit))
  })

  copySettings <- function() {
    s  <- curSeries()
    to <- setdiff(intersect(input$copyTo, seriesNames()), s)
    S  <- settings()
    pushUndo(paste("copy to", length(to), "series"))
    # how to detrend is copied; which rings a series uses stays its own
    setRows(to, S[match(s, S$series), ], copyFields)
    updateSelectizeInput(session, "copyTo", selected = character(0))
    showNotification(sprintf("The settings of %s were copied to %d series.", s, length(to)),
                     type = "message")
  }
  # Ask first when the copy would overwrite settings the user has changed
  observeEvent(input$copyApply, {
    s  <- curSeries()
    to <- setdiff(intersect(input$copyTo, seriesNames()), s)
    req(s)
    if (length(to) == 0) {
      showNotification("Pick the series to change first.", type = "warning")
      return()
    }
    S <- settings()
    changed <- to[!sameSettings(S[match(to, S$series), ], defaultSettings(to), copyFields)]
    if (length(changed) == 0) return(copySettings())
    showModal(modalDialog(
      title = "Replace settings you have changed?",
      p(length(changed), "of the", length(to), "series you picked",
        if (length(changed) == 1) "has" else "have",
        "settings you changed from the default:",
        paste(utils::head(changed, 12), collapse = ", "),
        if (length(changed) > 12) paste("and", length(changed) - 12, "more"), "."),
      p("Copying replaces them with the settings of ", tags$strong(s), "."),
      footer = tagList(modalButton("Cancel"),
                       actionButton("copyConfirm", "Copy the settings", class = "btn-danger")),
      easyClose = TRUE))
  })
  observeEvent(input$copyConfirm, {
    removeModal()
    copySettings()
  })


  # ════════════════════════════════════════════════════════════════════════════
  # DETREND: THE PLOT
  # ════════════════════════════════════════════════════════════════════════════

  curRow <- reactive({
    s <- curSeries()
    S <- settings()
    req(s, S)
    S[match(s, S$series), ]
  })
  curFit <- reactive({
    s <- curSeries()
    req(s)
    allFits()[[s]]
  })

  # The curves the methods picked under "Compare with" would fit to this
  # series: the series' own row with the method changed, so each uses the
  # settings it would have if chosen. Labelled with what dplR fitted when
  # that is not the method itself.
  compareFits <- reactive({
    s   <- curRow()
    y   <- rwlRV$dat[[s$series]]
    out <- list()
    for (m in setdiff(input$compare, c(s$method, "Ar"))) {
      s2 <- s
      s2$method <- m
      fit <- detrendOne(y, s2, s$series, as.numeric(rownames(rwlRV$dat)))
      if (!is.null(fit$error)) next
      lbl <- methodLabel(m)
      if (!identical(fit$used, unname(usedIfAsked[m]))) {
        lbl <- paste0(lbl, " (", tolower(usedLabel(fit$used)), ")")
      }
      out[[lbl]] <- fit
    }
    out
  })

  # What was fitted, in a few words, for the plot's title line
  fitLine <- function(fit, s) {
    if (!is.null(fit$error)) return("Not detrended")
    n <- sum(!is.na(fit$x))
    paste0(usedLabel(fit$used),
           if (fit$used == "Spline") {
             yrs <- if (s$spl.nyrs <= 1) floor(s$spl.nyrs * n) else s$spl.nyrs
             paste0(", ", yrs, " yrs", if (s$spl.nyrs <= 1) paste0(" (", round(s$spl.nyrs * 100), "% of ", n, ")"))
           },
           if (fit$used == "Age-Dep Spline") paste0(", from ", s$ads.nyrs, " yrs"),
           if (fit$used == "Ar") paste0(", order ", fit$order))
  }

  # The indices of the other series, as they are detrended now: those that
  # could be detrended and are indexed the way this one is. Indices by
  # division centre on 1 and by subtraction on 0, and a power transform
  # changes the scale, so a mean across the two would mean nothing.
  othersNow <- reactive({
    s    <- curRow()
    S    <- settings()
    fits <- allFits()
    ok   <- vapply(fits, function(f) is.null(f$error), logical(1))
    use  <- S$series[ok & S$series != s$series & S$difference == s$difference &
                       (S$powt != "none") == (s$powt != "none")]
    if (length(use) == 0) return(NULL)
    as.data.frame(lapply(fits[use], `[[`, "rwi"), check.names = FALSE)
  })

  output$seriesPlot <- renderPlot({
    s   <- curRow()
    fit <- curFit()
    oth <- if (isTRUE(input$showOthers) && is.null(fit$error)) othersNow()
    plotDetrend(as.numeric(rownames(rwlRV$dat)), fit, s$series, s,
                compare = compareFits(), sub = fitLine(fit, s),
                others = if (!is.null(oth)) { m <- rowMeans(oth, na.rm = TRUE); m[is.nan(m)] <- NA; m },
                others.n = if (!is.null(oth)) ncol(oth) else 0)
  })

  # ── What dplR did, and what it means ──────────────────────────────────────
  output$fitMessage <- renderUI({
    s  <- curRow()
    st <- statuses()[[s$series]]
    if (length(st$text) == 0) {
      return(div(class = "small text-muted mt-2", bs_icon("check-circle"),
                 " Fitted as asked: ", tolower(fitLine(curFit(), s)), "."))
    }
    cls <- c(ok = "alert-secondary", note = "alert-secondary",
             warning = "alert-warning", error = "alert-danger")[[st$level]]
    icon <- if (st$level %in% c("warning", "error")) "exclamation-triangle" else "info-circle"
    div(class = paste("alert mt-2 mb-0 py-2", cls),
        if (st$level == "error") {
          tags$strong("This series is not detrended, and is left out of the results. ")
        },
        if (length(st$text) == 1) tagList(bs_icon(icon), " ", st$text) else
          tags$ul(class = "mb-0 ps-3", lapply(st$text, tags$li)))
  })

  # ── This series against the others ────────────────────────────────────────
  # Its indices correlated with the mean of the other series' indices, now
  # and with this series at the default. Immediate feedback on a choice,
  # with the caveat that higher is not the same as better.
  withOthers <- function(x, others) {
    if (is.null(others) || ncol(others) == 0) return(NA_real_)
    m  <- rowMeans(others, na.rm = TRUE)
    ok <- !is.na(x) & !is.na(m)
    if (sum(ok) < 10) return(NA_real_)
    suppressWarnings(stats::cor(x[ok], m[ok], method = "spearman"))
  }
  output$seriesEffect <- renderUI({
    s      <- curRow()
    fit    <- curFit()
    others <- othersNow()
    req(is.null(fit$error), !is.null(others))
    oth    <- names(others)
    rNow   <- withOthers(fit$rwi, others)
    req(!is.na(rNow))
    isDefault <- sameSettings(s, defaultSettings(s$series))
    base  <- defaultFits()[[s$series]]
    rBase <- if (isDefault || !is.null(base$error)) NA_real_ else withOthers(base$rwi, others)
    div(class = "small text-muted mt-2 px-1",
        bs_icon("people"), " ",
        tooltip(
          span(sprintf("Against the mean of the other %d series: r = %.2f", length(oth), rNow),
               if (!is.na(rBase)) sprintf(" (%.2f with this series at the default)", rBase),
               "."),
          paste("Spearman correlation of this series' indices with the mean of the",
                "others' as they are now. A flexible curve can raise it by removing",
                "slow variation, so higher is not by itself better: it is a check",
                "that the curve has not removed what the series share.")))
  })


  # ════════════════════════════════════════════════════════════════════════════
  # RESULTS
  # ════════════════════════════════════════════════════════════════════════════

  output$resultsAlerts <- renderUI({
    req(settings())
    w <- collectionWarnings(settings(), allFits())
    nLook <- sum(levels() == "warning")
    tagList(
      lapply(w, function(x) div(class = "alert alert-warning", bs_icon("exclamation-triangle"), " ", x)),
      if (nLook > 0) {
        div(class = "alert alert-secondary", bs_icon("info-circle"), " ",
            nLook, if (nLook == 1) "series is" else "series are",
            "marked", tags$strong("look"), "in the table below: detrended, but not as asked",
            "or not sensibly. Click the row to see why.")
      })
  })

  output$resultsPlot <- renderPlot({
    rwi <- rwiNow()
    validate(need(!is.null(rwi), "No series could be detrended, so there are no indices to show."))
    type <- input$resultsPlotType
    if (identical(type, "crn")) {
      crn <- crnResNow()
      validate(need(is.data.frame(crn), if (is.character(crn)) crn else "No chronology."))
      plotChron(crn, if (sameAsBase()) NULL else crnBase(),
                labels = c("With your settings", baseLabel()),
                ylab = c(std = "Index", res = "Residual index", ars = "ARSTAN index",
                         vsc = "Stabilised index")[[crnOpts()$type]],
                sss.from = sssFrom(sssNow()))
    } else if (identical(type, "run")) {
      run <- runNow()
      validate(need(!is.null(run), "Press \u201cCompute the statistics\u201d below: with this many series they are computed on request."))
      validate(need(is.data.frame(run), if (is.character(run)) run else ""))
      base <- if (sameAsBase()) NULL else runBase()
      plotSignal(run, if (is.data.frame(base)) base, labels = c("With your settings", baseLabel()),
                 sss = sssNow())
    } else {
      validate(need(ncol(rwi) > 1 || type != "image", "The image plot needs two or more series."))
      plot(rwi, plot.type = type)
    }
  })

  # rbar, EPS and SNR, with the all-default values under them once any
  # series has been changed
  output$statsUI <- renderUI({
    sm  <- sumNow()
    rwi <- rwiNow()
    if (is.null(sm) && !statsAuto() && !is.null(rwi) && ncol(rwi) >= 2) {
      return(div(class = "mt-3",
                 actionButton("computeStats", "Compute the statistics", class = "btn-primary btn-sm"),
                 helpText(class = "mt-2 mb-0",
                          "rbar, EPS and each series' correlation with the others. With",
                          ncol(rwi), "series this takes from several seconds to a minute or",
                          "two, so it is done when you ask, not after every change. The r",
                          "column of the table below is filled in at the same time.")))
    }
    if (is.null(sm) || is.null(sm$stats)) {
      return(helpText(class = "mt-2", "Statistics across series need two or more detrended series."))
    }
    base <- if (sameAsBase()) NULL else sumBase()
    stat <- function(label, now, was, digits, tip) {
      div(class = "id-stat",
          div(class = "small text-muted", label, tooltip(bs_icon("question-circle"), tip)),
          div(class = "id-stat-n", formatC(now, digits = digits, format = "f")),
          if (!is.null(was)) div(class = "small text-muted",
                                 paste0(baseShort(), ": ", formatC(was, digits = digits, format = "f"))))
    }
    b <- base$stats
    tagList(
      uiOutput("idsNote"),
      div(class = "d-flex flex-wrap gap-4 mt-3",
          stat("rbar", sm$stats$rbar.eff, b$rbar.eff, 3,
               "The mean correlation between trees (rwi.stats()'s rbar.eff): between cores of different trees, allowing for the cores each tree has."),
          stat("EPS", sm$stats$eps, b$eps, 3,
               paste("Expressed population signal: how well this many trees stand for the population, over the whole span.", epsNote)),
          stat("SNR", sm$stats$snr, b$snr, 2,
               "Signal-to-noise ratio."),
          {
            # how far back enough trees reach, by SSS
            from  <- sssFrom(sssNow())
            fromB <- if (!is.null(base)) sssFrom(sssBase())
            yr <- function(x) if (is.na(x)) "never" else format(x)
            div(class = "id-stat",
                div(class = "small text-muted", paste0("SSS \u2265 ", signalCut, " from"),
                    tooltip(bs_icon("question-circle"), paste(
                      "Subsample signal strength, year by year (dplR's sss()): how well the chronology",
                      "from the trees alive in a year stands for the one from all of them. This is the",
                      "first year from which it stays at or above the cut-off: before it, the chronology",
                      "rests on too few trees to stand for the whole sample. SSS is the statistic meant",
                      "for this; running EPS is often used for it in error (Buras 2017).", epsNote,
                      "See Signal through time for SSS in every year."))),
                div(class = "id-stat-n", yr(from)),
                if (!is.null(fromB)) div(class = "small text-muted", paste0(baseShort(), ": ", yr(fromB))))
          },
          stat("Mean r with the others", mean(sm$series$cor, na.rm = TRUE),
               if (!is.null(base)) mean(base$series$cor, na.rm = TRUE), 3,
               "Each series' correlation with the mean of the others (interseries.cor()), averaged.")),
      if (!is.null(base)) {
        if (usePinned()) {
          helpText(class = "mt-2 mb-0",
                   "\u201cPinned\u201d, and the orange line in the plot, is the same data with the",
                   "settings as they were when you pinned them, at", paste0(pinned()$time, "."),
                   "Flexible curves raise these numbers by removing slow variation: read",
                   "them with the chronology.")
        } else {
          helpText(class = "mt-2 mb-0",
                   "\u201cdplR default\u201d, and the orange line in the plot, is the same data",
                   "detrended by dplR with no choices made,", tags$code("detrend(rwl)"),
                   ": every series with a spline of 67% of its length. Flexible curves raise",
                   "these numbers by removing slow variation: read them with the chronology.")
        }
      },
      uiOutput("compareUI"))
  })

  # ── Pin the settings, and choose what to compare with ──────────────────────
  output$compareUI <- renderUI({
    req(settings())
    pin <- pinned()
    div(class = "d-flex flex-wrap align-items-end gap-3 mt-3 pt-3 border-top",
        if (!is.null(pin)) {
          div(style = "min-width: 17rem;",
              selectInput("compareWith", tipLabel("Compare with",
                paste("What the orange line and the second row of numbers are: dplR's",
                      "default detrending of the same data, or the settings as they",
                      "were when you pinned them.")),
                choices = stats::setNames(c("default", "pinned"),
                                          c("dplR's default detrending", paste0("The settings pinned at ", pin$time))),
                selected = if (identical(isolate(input$compareWith), "default")) "default" else "pinned",
                width = "100%"))
        },
        div(class = "mb-3",
            actionButton("pinSettings",
                         if (is.null(pin)) "Pin these settings to compare with later" else "Pin again, as they are now",
                         icon = bs_icon("pin-angle"), class = "id-btn-quiet btn-sm")),
        if (is.null(pin)) {
          helpText(class = "mb-3", style = "max-width: 34rem;",
                   "Pinning keeps a copy of every series' settings as they are now. Carry on",
                   "changing curves, then compare the chronology and the statistics with",
                   "the pinned copy to see whether the changes mattered.")
        })
  })
  observeEvent(input$pinSettings, {
    req(settings())
    pinned(list(settings = settings(), time = format(Sys.time(), "%H:%M")))
  })

  # ── The R code, as the report prints it and the script download saves it ──
  codeNow <- reactive({
    x   <- idsNow()
    o   <- crnOpts()
    detrendCode(rwlRV$name, if (rwlRV$example) examples[[rwlRV$name]]$code else FALSE,
                rwlRV$fills, settings(), allFits(),
                as.numeric(rownames(rwlRV$dat)),
                ids.code = if (!is.null(idsFor(x$ids, rwiNow()))) x$code,
                crn.code = chronCode(o$type, o$biweight, o$win))
  })

  # How the trees were counted, with dplR's warnings and a look at the result
  output$idsNote <- renderUI({
    x   <- idsNow()
    rwi <- rwiNow()
    req(rwi)
    ids <- idsFor(x$ids, rwi)
    tagList(
      if (!is.null(x$error)) {
        div(class = "alert alert-warning py-2 mt-3 mb-0", bs_icon("exclamation-triangle"),
            " The series names could not be read as trees and cores, so every series is",
            " counted as a tree: ", x$error, ".")
      },
      lapply(x$warn, function(w) {
        div(class = "alert alert-warning py-2 mt-3 mb-0", bs_icon("exclamation-triangle"),
            " dplR is not sure how the names divide into trees and cores (", w,
            "). Check the list below, or set the positions yourself.")
      }),
      div(class = "small text-muted mt-3",
          if (is.null(ids)) {
            paste0(ncol(rwi), " series, each counted as its own tree.")
          } else {
            tagList(paste0(treeCount(ids), ", read from the series names. "),
                    tags$details(
                      tags$summary("Which series are in which tree"),
                      div(class = "mt-1", lapply(split(rownames(ids), ids$tree), function(s) {
                        div(paste(s, collapse = ", "))
                      }))))
          }))
  })

  # ── The table of series ───────────────────────────────────────────────────
  seriesTableData <- reactive({
    S   <- settings()
    req(S)
    tab <- settingsTable(rwlRV$dat, S, allFits())
    rwi <- rwiNow()
    sm  <- sumNow()
    fit <- if (is.null(rwi) || is.null(sm)) {
      data.frame(cor = rep(NA_real_, nrow(tab)), p = NA_real_, trend = NA_real_)
    } else seriesFit(rwi, sm, S$series)
    tab$r     <- round(fit$cor, 3)
    tab$Trend <- round(fit$trend, 3)
    tab$Seen  <- ifelse(S$series %in% seen(), "✓", "")
    tab$Notes <- ifelse(nzchar(vapply(S$series, noteOf, "")), "✎", "")
    tab$weak  <- !is.na(fit$p) & fit$p >= 0.05
    # what to act on first, so it is in view without scrolling sideways
    first <- c("Series", "Check", "r", "Trend", "Method", "Settings", "Curve used")
    tab[, c(first, setdiff(names(tab), first))]
  })

  output$seriesTable <- renderDT({
    tab <- seriesTableData()
    hide <- which(names(tab) == "weak") - 1
    datatable(tab, rownames = FALSE, selection = "single",
              class = "compact stripe hover nowrap",
              options = list(scrollX = TRUE,
                             pageLength = 25, lengthChange = nrow(tab) > 25,
                             paging = nrow(tab) > 25, searching = nrow(tab) > 25,
                             info = nrow(tab) > 25, autoWidth = FALSE,
                             columnDefs = list(list(visible = FALSE, targets = hide)))) |>
      formatStyle("Check", fontWeight = "bold",
                  color = styleEqual(c("look", "not detrended"), c("#8a6d00", "#b02a37"))) |>
      formatStyle("r", "weak", backgroundColor = styleEqual(TRUE, "#f8d7da"))
  })

  # Click a row: open that series on the Detrend panel
  observeEvent(input$seriesTable_rows_selected, {
    s <- seriesTableData()$Series[input$seriesTable_rows_selected]
    req(length(s) == 1, s %in% seriesNames())
    updateSelectInput(session, "series", selected = s)
    nav_select("navbar", "DetrendTab", session = session)
  })

  # ── Downloads ──────────────────────────────────────────────────────────────
  output$downloadRWI <- downloadHandler(
    filename = function() downloadName(rwlRV$name, "indices", "csv"),
    content  = function(file) {
      rwi <- rwiNow()
      if (is.null(rwi)) stop("no series could be detrended")
      utils::write.csv(rwiSheet(rwi), file, row.names = FALSE, na = "")
      guideRV$savedRWI <- TRUE
      afterDownload()
    }
  )

  output$downloadCrn <- downloadHandler(
    filename = function() downloadName(rwlRV$name, "chronology", "csv"),
    content  = function(file) {
      crn <- crnNow()
      if (is.null(crn)) stop("no chronology could be built")
      out <- data.frame(Year = as.numeric(rownames(crn)), round(crn[[1]], 4), crn$samp.depth)
      names(out) <- c("Year", names(crn)[1], "samp.depth")
      # SSS beside it, when it has been computed: which years to trust
      s <- sssNow()
      if (!is.null(s)) out$sss <- round(unname(s[as.character(out$Year)]), 3)
      utils::write.csv(out, file, row.names = FALSE, na = "")
    }
  )

  # The R code on its own, to run or keep with the data
  output$downloadScript <- downloadHandler(
    filename = function() downloadName(rwlRV$name, "detrending", "R"),
    content  = function(file) {
      writeLines(scriptLines(codeNow(), if (rwlRV$example) paste(rwlRV$label, "from dplR") else rwlRV$name,
                             iDetrendVersion), file)
      unsaved(FALSE)
      afterDownload()
    }
  )

  # The chronology as a Tucson .crn, for other dendro software. The format
  # holds indices as whole numbers of thousandths and has no way to write a
  # value below zero, so a chronology with any (indices by subtraction) is
  # not offered: the csv holds it.
  crnTucsonOK <- reactive({
    crn <- crnNow()
    !is.null(crn) && !any(crn[[1]] < 0, na.rm = TRUE)
  })
  output$crnTucsonUI <- renderUI({
    req(crnNow())
    if (crnTucsonOK()) {
      downloadButton("downloadCrnTucson", "Chronology (.crn)", class = "id-btn-quiet")
    } else {
      div(class = "small text-muted", style = "max-width: 22rem;", bs_icon("info-circle"),
          " No Tucson .crn for this chronology: it has values below zero, which that",
          " format cannot hold. The .csv has them.")
    }
  })
  output$downloadCrnTucson <- downloadHandler(
    filename = function() downloadName(rwlRV$name, "chronology", "crn"),
    content  = function(file) {
      if (!crnTucsonOK()) stop("this chronology cannot be written as a Tucson .crn")
      write.crn(crnForTucson(crnNow(), rwlRV$name), file)
    }
  )

  # The settings table with the notes, to load again later (readSettings())
  output$downloadSettings <- downloadHandler(
    filename = function() downloadName(rwlRV$name, "settings", "csv"),
    content  = function(file) {
      S <- settings()
      S$note <- vapply(S$series, noteOf, "")
      utils::write.csv(S, file, row.names = FALSE, na = "")
      unsaved(FALSE)
      afterDownload()
    }
  )

  settingsNote <- reactiveVal(NULL)
  observeEvent(input$settingsFile, {
    req(settings())
    res <- readSettings(input$settingsFile$datapath, settings())
    if (!is.null(res$error)) {
      settingsNote(list(cls = "text-danger", text = res$error))
      return()
    }
    if (length(res$matched) == 0) {
      settingsNote(list(cls = "text-danger", text = paste0(
        "None of the ", length(res$unknown), " series in the settings file are in ",
        rwlRV$label, ", so nothing was changed. Is it the settings file for another data file?")))
      return()
    }
    pushUndo("loading the settings file")
    settings(res$settings)
    n <- notes()
    n[names(res$notes)] <- res$notes
    notes(n)
    ctlRefresh(ctlRefresh() + 1)
    few <- function(x) paste0(paste(utils::head(x, 8), collapse = ", "),
                              if (length(x) > 8) paste0(" and ", length(x) - 8, " more"))
    settingsNote(list(
      cls  = if (length(res$unknown) || length(res$missing)) "text-warning-emphasis" else "text-success",
      text = c(paste0("Settings loaded for ", length(res$matched), " of ",
                      nrow(res$settings), " series."),
               if (length(res$missing)) paste0(
                 length(res$missing), " series in the data are not in the settings file and",
                 " keep the settings they had: ", few(res$missing), "."),
               if (length(res$unknown)) paste0(
                 length(res$unknown), " series in the settings file are not in the data and",
                 " were ignored: ", few(res$unknown), "."))))
  })
  output$settingsLoadNote <- renderUI({
    x <- settingsNote()
    req(x)
    div(class = paste("small mt-1", x$cls), lapply(x$text, div))
  })

  # ── The report ─────────────────────────────────────────────────────────────
  output$detrendReport <- safeDownload(
    filename = function() downloadName(rwlRV$name, "detrending", "html"),
    content  = function(file) {
      req(rwlRV$dat)
      S    <- settings()
      fits <- allFits()
      tempReport <- file.path(tempdir(), "report_detrend.rmd")
      file.copy("report_detrend.rmd", tempReport, overwrite = TRUE)
      params <- list(
        fileName   = rwlRV$name,
        fileLabel  = if (rwlRV$example) paste(rwlRV$label, "from dplR") else rwlRV$name,
        example    = rwlRV$example,
        dat        = rwlRV$dat,
        settings   = S,
        fits       = fits,
        statuses   = statuses(),
        notes      = notes(),
        fills      = rwlRV$fills,
        goal       = if (!is.null(rwlRV$goal)) goalById(rwlRV$goal),
        dropped    = rwlRV$dropped,
        rwi        = rwiNow(),
        rwiSummary = sumNow(),
        baseSummary = if (sameAsBase()) NULL else sumBase(),
        crn        = crnNow(),
        baseCrn    = if (sameAsBase()) NULL else crnBase(),
        baseLabel  = baseLabel(),
        basePinned = usePinned(),
        code       = codeNow(),
        crnOpts    = crnOpts(),
        crnMessage = if (is.character(crnResNow())) crnResNow(),
        ids        = { x <- idsNow(); if (is.null(idsFor(x$ids, rwiNow()))) x$code <- NULL; x },
        running    = { r <- runNow(); if (is.data.frame(r)) r },
        sss        = sssNow(),
        baseRunning = { r <- if (sameAsBase()) NULL else runBase(); if (is.data.frame(r)) r },
        plots      = isTRUE(input$reportPlots),
        version    = iDetrendVersion,
        helpers    = normalizePath(c("appHelpers.R", "plotDetrend.R")))
      rmarkdown::render(tempReport, output_file = file, params = params,
                        envir = new.env(parent = globalenv()), quiet = TRUE)
      unsaved(FALSE)
      guideRV$savedReport <- TRUE
      afterDownload()
    }
  )


  # ════════════════════════════════════════════════════════════════════════════
  # GUIDED EXAMPLE
  # ════════════════════════════════════════════════════════════════════════════
  # A checklist in the sidebar, offered when the example data are loaded
  # (steps and text in guide.R). A step is done when the app's own state
  # says so, and stays done; the steps must be done in order.
  guideRV <- reactiveValues(on = FALSE, done = character(0), answer = FALSE,
                            savedRWI = FALSE, savedReport = FALSE)
  # the guide for the example data in use (guides, in guide.R)
  guideDef   <- function() if (isTRUE(rwlRV$example)) guides[[rwlRV$name]]
  guideSteps <- function() guideDef()$steps
  guideIds   <- function() vapply(guideSteps(), `[[`, "", "id")
  guideHere <- reactive(isTRUE(rwlRV$example) && !is.null(rwlRV$dat) && !is.null(guideDef()))
  guideStep <- reactive(match(FALSE, guideIds() %in% guideRV$done))   # NA when finished

  observe({
    req(guideRV$on, guideHere())
    k <- guideStep()
    if (is.na(k)) return()
    onDetrend <- identical(input$navbar, "DetrendTab")
    done <- switch(guideIds()[k],
      # reading steps (overview, first, results) are finished with Next
      compare = onDetrend && length(input$compare) > 0,
      fix = {
        S <- settings()
        s <- S[match("644081", S$series), ]
        levels()[["644081"]] %in% c("ok", "note") && s$method != "Mean" &&
          !sameSettings(s, defaultSettings("644081"))
      },
      all  = all(seriesNames() %in% seen()),
      save = guideRV$savedRWI && guideRV$savedReport,
      # the second example (Gus Pearson)
      g.juvenile = onDetrend && identical(input$series, "36B") && "ModNegExp" %in% input$compare,
      g.flex     = identical(rwlRV$goal, "annual"),
      g.pin      = !is.null(pinned()),
      g.stiff    = identical(rwlRV$goal, "decadal"),
      FALSE)
    if (isTRUE(done)) {
      guideRV$done   <- c(guideRV$done, guideIds()[k])
      guideRV$answer <- FALSE
    }
  })

  # The guide's steps are written for dplR's default starting point: its
  # lesson is the series the default spline fails on. So it starts from
  # there. If a starting point has been applied (goals.R) or series changed
  # by hand, starting the guide puts the example back, and asks first.
  startGuide <- function() {
    if (!allDefault()) {
      pushUndo("starting the guide")
      settings(defaultSettings(seriesNames()))
      rwlRV$goal <- NULL
      updateRadioButtons(session, "goal", selected = "default")
      ctlRefresh(ctlRefresh() + 1)
      # the steps done so far were done on other settings: begin again
      guideRV$done   <- character(0)
      guideRV$answer <- FALSE
    }
    guideRV$on <- TRUE
    # saving counts from when the guide reaches that step, not before
    guideRV$savedRWI    <- FALSE
    guideRV$savedReport <- FALSE
  }
  observeEvent(input$guideStart, {
    # the second example begins by choosing a starting point: it starts
    # from the default too, so that step is there to do
    if (allDefault()) return(startGuide())
    showModal(modalDialog(
      title = "Start the guided example?",
      p("The guide walks through the example data from dplR's default, a spline of",
        "two-thirds of each series' length. You have",
        if (!is.null(rwlRV$goal) && rwlRV$goal != "default") "applied another starting point"
        else "changed the settings of some series", "."),
      p("Starting the guide puts every series of the example back to the default.",
        "Your notes are kept. At the end the guide comes back to",
        tags$em("What is the chronology for?")),
      footer = tagList(modalButton("Cancel"),
                       actionButton("guideStartConfirm", "Start the guide", class = "btn-primary")),
      easyClose = TRUE))
  })
  observeEvent(input$guideStartConfirm, {
    removeModal()
    startGuide()
  })
  observeEvent(input$guideHide, guideRV$on <- FALSE)
  # Next: finishes a step that is about reading something
  observeEvent(input$guideNext, {
    k <- guideStep()
    req(!is.na(k), isTRUE(guideSteps()[[k]]$read))
    guideRV$done   <- c(guideRV$done, guideIds()[k])
    guideRV$answer <- FALSE
  })
  observeEvent(input$guideAnswer, guideRV$answer <- TRUE)
  observeEvent(input$guideRestart, {
    guideRV$done        <- character(0)
    guideRV$answer      <- FALSE
    guideRV$savedRWI    <- FALSE
    guideRV$savedReport <- FALSE
  })
  # A new file: the guide starts from the top next time
  observeEvent(rwlRV$name, {
    guideRV$on   <- FALSE
    guideRV$done <- character(0)
  })
  # "Take me there": the step's panel and series
  observeEvent(input$guideGo, {
    k <- guideStep()
    req(!is.na(k))
    st <- guideSteps()[[k]]
    if (!is.null(st$series)) updateSelectInput(session, "series", selected = st$series)
    nav_select("navbar", st$tab, session = session)
  })

  output$guideUI <- renderUI({
    if (!guideHere()) return(NULL)
    if (!guideRV$on) {
      return(div(class = "small mb-2",
                 actionLink("guideStart", tagList(bs_icon("signpost-2"),
                                                  paste0(" ", guideDef()$label)))))
    }
    box <- function(...) div(class = "border rounded p-2 mb-2 small",
                             style = "background: var(--bs-body-bg);", ...)
    k <- guideStep()
    if (is.na(k)) {
      return(box(div(class = "fw-bold text-success", bs_icon("check-circle"),
                     " Guided example finished"),
                 p(class = "mb-2 mt-1", HTML(guideDef()$finished)),
                 actionLink("guideRestart", "Start again"), " · ",
                 actionLink("guideHide", "Hide guide")))
    }
    st   <- guideSteps()[[k]]
    here <- identical(input$navbar, st$tab) &&
      (is.null(st$series) || identical(input$series, st$series))
    box(div(class = "text-muted", bs_icon("signpost-2"),
            sprintf(" Guided example · step %d of %d", k, length(guideSteps()))),
        div(class = "fw-bold mt-1", st$title),
        p(class = "mb-2", HTML(st$text)),
        if (!is.null(st$answer)) {
          if (guideRV$answer) div(class = "alert alert-secondary py-1 px-2 mb-2", HTML(st$answer))
          else div(class = "mb-2", actionLink("guideAnswer", "Show the answer"))
        },
        # One button, fitted to where the user is: "Take me there" when the
        # step is on another panel (or another series); "Next" when they are
        # there and the step is about reading; nothing when the step is
        # something to do there, which finishes itself.
        if (!here) {
          actionButton("guideGo", "Take me there", class = "btn-primary btn-sm")
        } else if (isTRUE(st$read)) {
          actionButton("guideNext", "Next", class = "btn-primary btn-sm")
        } else {
          span(class = "text-muted", "This step finishes when you have done it.")
        },
        div(class = "mt-1", actionLink("guideHide", "Hide guide")))
  })
})
