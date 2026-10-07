# Drives server.R headlessly through the main workflow with shiny::testServer.
# Run from the app directory, outside renv so the dev dplR is used:
#   Rscript --vanilla tests/test-server.R
# As Shiny does: global.R for everyone, ui.R on its own. Sourcing ui.R into
# the global environment would let server.R see what only ui.R defines,
# which it cannot in the running app.
suppressPackageStartupMessages(source("global.R"))
source("ui.R", local = new.env())
# the copy kept in the browser is written at once in the tests
options(idetrend.debounce.ms = 0)
appServer <- source("server.R")$value
# testServer() wants (input, output, session) in that order. Reordering the
# formals keeps the body, so the test code sees the server's reactives.
srv <- appServer
formals(srv) <- formals(function(input, output, session) NULL)
ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE); cat("\nok  ", msg) }
pdf(NULL)
plain <- function(x) as.data.frame(unclass(x), check.names = FALSE)

# The R code a report prints in its last code block
reportCode <- function(html) {
  txt <- paste(readLines(html, warn = FALSE), collapse = "\n")
  blocks <- regmatches(txt, gregexpr("<pre><code>.*?</code></pre>", txt))[[1]]
  code <- gsub("</?pre>|</?code>", "", blocks[length(blocks)])
  xml2::xml_text(xml2::read_html(paste0("<p>", code, "</p>")))
}
runCode <- function(code, env = new.env()) {
  suppressWarnings(suppressMessages(utils::capture.output(eval(parse(text = code), env))))
  env
}

# Sets the Detrend panel's controls for the current series, as the browser
# would report them: every control, with the ids of the current build.
setControls <- function(session, key, s, n, ...) {
  v <- list(method = s$method, splMode = if (s$spl.nyrs <= 1) "prop" else "years",
            splProp = if (s$spl.nyrs <= 1) s$spl.nyrs else 0.67,
            splYears = if (s$spl.nyrs <= 1) max(2, floor(0.67 * n)) else s$spl.nyrs,
            splF = s$spl.f, adsNyrs = s$ads.nyrs, posSlope = s$pos.slope,
            constrain = s$constrain, spanCV = is.na(s$span),
            span = if (is.na(s$span)) 0.5 else s$span, bass = s$bass,
            index = if (s$difference) "diff" else "ratio", powt = s$powt,
            trim = c(if (is.na(s$first)) -1e9 else s$first, if (is.na(s$last)) 1e9 else s$last), note = "")
  v[names(list(...))] <- list(...)
  names(v) <- paste0(names(v), "_", key$id)
  do.call(session$setInputs, v)
}

# ── Example data: defaults, controls, copy, results, downloads, report ───────
testServer(srv, {
  ok(grepl("try one of two", output$fileUI$html) && grepl("Start here", output$fileUI$html) &&
       grepl("Douglas-fir, New Mexico", output$fileUI$html) && grepl("Ponderosa pine, Arizona", output$fileUI$html) &&
       grepl("stand that grew crowded", output$fileUI$html) &&
       grepl("useDemo", output$fileUI$html) && grepl("useDemo2", output$fileUI$html) &&
       grepl("idetrend|iDetrend|svg", output$overviewUI$html),
     "before a file is loaded: both examples are offered, and the welcome screen draws")
  session$setInputs(useDemo = 1, navbar = "OverviewTab", rwlPlotType = "seg",
                    resultsPlotType = "crn", reportPlots = TRUE)
  ok(ncol(rwlRV$dat) == 8 && rwlRV$example, "example data loaded")
  ok(identical(settings(), defaultSettings(names(rwlRV$dat))), "every series starts at dplR's default")
  ref <- suppressWarnings(detrend(rwlRV$dat))
  ok(isTRUE(all.equal(plain(rwiNow()), plain(ref), check.attributes = FALSE)),
     "untouched, the indices are detrend(rwl)'s")
  ok(allDefault() && !hasWork() && !unsaved(), "nothing to lose yet")
  output$overviewUI; output$checkPanel; output$rwlPlot; output$rwlSummary; output$fileUI; output$fileInfo
  ok(TRUE, "Overview renders")
  info <- output$fileInfo$html
  ok(grepl("2 to check:", info) && grepl(">644041<", info) && grepl(">644081<", info) && grepl("openSeries", info),
     "the sidebar names the series to check, on every panel, as links that open them")
  session$setInputs(openSeries = "644081")
  session$setInputs(openSeries = "not a series")
  ok(TRUE, "opening one, or a name that is not there, does not error")
  ok(identical(names(levels())[levels() == "warning"], c("644041", "644081")),
     "two series to check: 644081 (the spline fails) and 644041 (the curve dives into its last rings)")

  # The Detrend panel
  session$setInputs(navbar = "DetrendTab", series = "644011")
  session$flushReact()
  ok(identical(seen(), "644011"), "a series shown on the Detrend panel is marked looked at")
  ok(!allSeen() && is.null(tryCatch(output$allSeenUI, error = function(e) NULL)),
     "no message about every series while some are still to be looked at")
  key <- ctlKey()
  n   <- sum(!is.na(rwlRV$dat[["644011"]]))
  html <- output$controls$html
  ok(grepl(paste0("method_", key$id), html, fixed = TRUE) && grepl("Notes on 644011", html),
     "controls are built for the series, with ids of this build")
  setControls(session, key, curRow(), n)
  ok(allDefault() && !unsaved(), "controls reporting the stored values change nothing")
  setControls(session, key, curRow(), n, splProp = 0.4, note = "tight spline: release at 1850")
  ok(curRow()$spl.nyrs == 0.4 && notes()[["644011"]] == "tight spline: release at 1850" && unsaved(),
     "a slider and a note reach the settings")
  ok(!allDefault() && hasWork(), "now there is work to lose")
  output$seriesPlot; output$fitMessage; output$seriesEffect; output$seriesProgress; output$controlsTitle
  ok(grepl("with this series at the default", output$seriesEffect$html),
     "the series' fit with the others is shown against the default")
  session$setInputs(showOthers = TRUE)
  ok(ncol(othersNow()) == 7 && !"644011" %in% names(othersNow()), "the other seven series are drawn behind the indices")
  ok(grepl("115-year spline", output$curveSays$html) && grepl("slower than about", output$curveSays$html),
     "the settings say in years what the spline removes (0.4 of 289 rings)")
  ok(identical(storeLog$last$key, storeKey()) && grepl("tight spline", storeLog$last$value),
     "the work in progress is sent to the browser to keep")
  setControls(session, key, curRow(), n, splMode = "years", splYears = 40, splF = NA, note = notes()[["644011"]])
  ok(curRow()$spl.nyrs == 0.4, "a cleared number box writes nothing")
  setControls(session, key, curRow(), n, splMode = "years", splYears = 40, note = notes()[["644011"]])
  ok(curRow()$spl.nyrs == 40, "rigidity in years")
  session$setInputs(compare = c("ModNegExp", "Friedman", "Spline"))
  ok(identical(names(compareFits()), c("Modified negative exponential", "Friedman's super smoother")),
     "comparison curves for the other methods, not the one in use")
  output$seriesPlot
  ok(TRUE, "plot draws with comparison curves")

  # A stale control id is ignored: values typed for a previous build
  old <- key
  session$setInputs(series = "644081")
  session$flushReact()
  key <- ctlKey()
  ok(key$id != old$id && key$series == "644081", "a new series gets new control ids")
  do.call(session$setInputs, stats::setNames(list("Mean"), paste0("method_", old$id)))
  S <- settings()
  ok(S$method[S$series == "644081"] == "Spline" && S$method[S$series == "644011"] == "Spline",
     "a value arriving for the old controls is written to no series")
  ok(grepl("no trend was removed", output$fitMessage$html) && grepl("alert-warning", output$fitMessage$html),
     "644081: the fallback to the mean is explained under the plot")
  comp <- compareFits()
  ok(any(grepl("straight line", names(comp))), "a comparison curve that fell back says what was fitted")
  n81 <- sum(!is.na(rwlRV$dat[["644081"]]))
  setControls(session, key, curRow(), n81, method = "ModNegExp")
  ok(curFit()$used == "Line" && levels()[["644081"]] == "note", "644081 fixed with a straight line")
  ok(grepl("straight line", output$fitMessage$html) && !grepl("alert-warning", output$fitMessage$html),
     "and the message is now a note")
  ok(identical(nextToCheck(), "644041"), "one left to check: 644041")

  # Next / Previous
  session$setInputs(series = "644021"); session$flushReact()
  ok(setequal(seen(), c("644011", "644081", "644021")), "seen accumulates")

  # Copy settings
  session$setInputs(series = "644081"); session$flushReact()
  session$setInputs(copyTo = c("644021", "644031", "644081"), copyApply = 1)
  S <- settings()
  ok(all(S$method[S$series %in% c("644021", "644031")] == "ModNegExp") && S$method[S$series == "644041"] == "Spline",
     "settings copied to the series picked, and only those")
  session$setInputs(copyTo = c("644011", "644041"), copyApply = 2)
  ok(settings()$method[1] == "Spline", "copying over settings the user changed waits for confirmation")
  session$setInputs(copyConfirm = 1)
  ok(settings()$method[1] == "ModNegExp" && notes()[["644011"]] == "tight spline: release at 1850",
     "confirmed: settings replaced, the note kept")

  # Reset
  before <- ctlKey()$id
  session$setInputs(resetSeries = 1)
  ok(sameSettings(curRow(), defaultSettings("644081")) && ctlKey()$id != before,
     "reset puts the series back to the default and rebuilds the controls")
  setControls(session, ctlKey(), curRow(), n81, method = "Friedman")
  ok(curFit()$used == "Friedman", "Friedman on 644081")

  # Results
  session$setInputs(navbar = "ResultsTab")
  output$resultsAlerts; output$resultsPlot; output$statsUI; output$seriesTable
  ok(grepl("dplR default:", output$statsUI$html) && grepl("detrend(rwl)", output$statsUI$html, fixed = TRUE),
     "statistics shown against dplR's default detrending, named as such")

  # Pin the settings, change more, compare with the pinned copy
  ok(grepl("Pin these settings", output$compareUI$html) && !usePinned(), "the settings can be pinned")
  atPin <- settings(); rbarPin <- sumNow()$stats$rbar.eff; crnPin <- crnNow()
  session$setInputs(pinSettings = 1)
  session$setInputs(compareWith = "pinned")
  ok(usePinned() && sameAsBase() && grepl("Pin again", output$compareUI$html), "pinned: nothing to compare yet")
  session$setInputs(navbar = "DetrendTab", series = "644041"); session$flushReact()
  setControls(session, ctlKey(), curRow(), 122, method = "Mean")
  session$setInputs(navbar = "ResultsTab")
  ok(!sameAsBase() && isTRUE(all.equal(crnBase()[[1]], crnPin[[1]])) && sumBase()$stats$rbar.eff == rbarPin &&
       grepl("pinned: ", output$statsUI$html) && grepl("settings pinned at", baseLabel()),
     "after a change, the baseline is the pinned settings' chronology and statistics")
  output$resultsPlot
  txt <- paste(readLines(output$detrendReport, warn = FALSE), collapse = " ")
  ok(grepl("settings pinned at", txt), "the report says what the comparison is with")
  session$setInputs(compareWith = "default")
  ok(!usePinned() && grepl("dplR default: ", output$statsUI$html) &&
       isTRUE(all.equal(plain(rwiBase()), plain(suppressWarnings(detrend(rwlRV$dat))), check.attributes = FALSE)),
     "and dplR's default can be chosen again")
  # put the series back for the checks that follow
  session$setInputs(navbar = "DetrendTab"); session$flushReact()
  setControls(session, ctlKey(), atPin[atPin$series == "644041", ], 122)
  session$setInputs(navbar = "ResultsTab")
  ok(all(sameSettings(settings(), atPin)), "settings restored for the rest of the checks")
  tab <- seriesTableData()
  ok(nrow(tab) == 8 && all(!is.na(tab$r)) && tab$`Curve used`[8] == "Friedman's super smoother" &&
       tab$Notes[1] == "✎" && tab$Seen[1] == "✓" && tab$Seen[5] == "",
     "series table: curve used, r, notes and seen")
  for (type in c("spag", "image")) { session$setInputs(resultsPlotType = type); output$resultsPlot }
  ok(TRUE, "spaghetti and image plots draw")
  session$setInputs(resultsPlotType = "crn")
  session$setInputs(seriesTable_rows_selected = 3)
  ok(TRUE, "clicking a row does not error")

  # Mixed indexing is called out
  session$setInputs(navbar = "DetrendTab", series = "644051"); session$flushReact()
  setControls(session, ctlKey(), curRow(), sum(!is.na(rwlRV$dat[["644051"]])), index = "diff", powt = "powt")
  ok(grepl("indexed by subtraction", output$resultsAlerts$html) && grepl("power transformed", output$resultsAlerts$html),
     "a mix of subtraction and division, and of transforms, is warned about on Results")
  ok(grepl("power 0", output$fitMessage$html), "the power of the transform is reported")
  ok(is.null(othersNow()), "a series indexed differently from all the others has no mean of the others drawn")
  output$seriesPlot

  # Downloads
  f <- output$downloadRWI
  back <- suppressMessages(suppressWarnings(read.rwl(f)))
  ok(isTRUE(all.equal(plain(back), round(plain(rwiNow()), 4), check.attributes = FALSE)),
     "indices download reads back with read.rwl()")
  crn <- read.csv(output$downloadCrn)
  ok(identical(names(crn), c("Year", "std", "samp.depth", "sss")) && nrow(crn) == nrow(rwiNow()), "chronology download")
  sc <- readLines(output$downloadScript)
  e2 <- runCode(sc)
  ok(grepl("^# Detrending of Example data \\(nm046\\) from dplR", sc[1]) && isTRUE(all.equal(plain(e2$rwi), plain(rwiNow()), check.attributes = FALSE)),
     "the R script download runs and rebuilds the indices")
  output$crnTucsonUI
  sf <- output$downloadSettings
  ok(!unsaved(), "saving the settings clears the unsaved mark")
  saved <- settings(); savedNotes <- notes()

  # The report, and its R code
  rep <- output$detrendReport
  txt <- paste(readLines(rep, warn = FALSE), collapse = "\n")
  ok(identical(sub("^.*?library", "library", paste(sc, collapse = "\n")), sub("\n$", "", reportCode(rep))),
     "the script and the report print the same code")
  ok(grepl("Report: Detrending", txt) && grepl("tight spline: release at 1850", txt) &&
       grepl("Read this first", txt) && grepl("Series plots", txt),
     "report renders with notes, warnings and series plots")
  e <- runCode(reportCode(rep))
  ok(isTRUE(all.equal(plain(e$rwi), plain(rwiNow()), check.attributes = FALSE)) &&
       identical(rownames(e$rwi), rownames(rwiNow())) && inherits(e$rwi, "rwi"),
     "the report's R code rebuilds the app's indices exactly")
  ok(isTRUE(all.equal(e$crn$std, crnNow()$std)), "and the chronology")
  ok(grepl("autoread.ids(dat)", reportCode(rep), fixed = TRUE) && nrow(e$ids) == 8 &&
       grepl("Signal through time", txt),
     "the report's code reads the tree ids, and the report shows the signal through time")
  session$setInputs(reportPlots = FALSE)
  ok(!grepl("Series plots", paste(readLines(output$detrendReport, warn = FALSE), collapse = "\n")),
     "report without series plots")

  # Settings file: change things, then load the saved file back
  session$setInputs(series = "644011"); session$flushReact()
  setControls(session, ctlKey(), curRow(), n, method = "Mean", note = "")
  ok(settings()$method[1] == "Mean", "changed after saving")
  before <- ctlKey()$id
  session$setInputs(settingsFile = list(datapath = sf, name = "s.csv"))
  ok(all(sameSettings(settings(), saved)) && identical(notes()[["644011"]], savedNotes[["644011"]]) &&
       ctlKey()$id != before,
     "loading the settings file restores every series and note, and rebuilds the controls")
  ok(grepl("Settings loaded for 8 of 8", output$settingsLoadNote$html), "and says what it did")
  bad <- tempfile(fileext = ".csv"); write.csv(data.frame(a = 1), bad)
  session$setInputs(settingsFile = list(datapath = bad, name = "bad.csv"))
  ok(grepl("not an iDetrend settings file", output$settingsLoadNote$html) && all(sameSettings(settings(), saved)),
     "a file that is not a settings file changes nothing and says so")

  # A second file while there is work: ask first
  write.csv(rwiSheet(rwlRV$dat[, 1:3]), bad, row.names = FALSE, na = "")
  session$setInputs(file1 = list(name = "three.csv", datapath = bad))
  ok(rwlRV$example && ncol(rwlRV$dat) == 8, "loading over unsaved work waits for confirmation")
  session$setInputs(navbar = "OverviewTab")
  session$setInputs(replaceConfirm = 1)
  ok(!rwlRV$example && ncol(rwlRV$dat) == 3 && rwlRV$name == "three.csv" && allDefault() &&
       length(seen()) == 0 && length(notes()) == 0,
     "confirmed: the new file replaces the old, with a clean slate")
})

# ── The copy kept in the browser: offered back, restored, discarded ──────────
testServer(srv, {
  data(nm046)
  g <- nm046
  i <- which(!is.na(g[["644021"]])); g[["644021"]][i[30:31]] <- NA
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(Year = as.numeric(rownames(g)), plain(g), check.names = FALSE), f, row.names = FALSE, na = "")
  load <- function() session$setInputs(file1 = list(name = "work.csv", datapath = f), navbar = "OverviewTab",
                                       rwlPlotType = "seg", resultsPlotType = "crn", reportPlots = FALSE)
  load()
  ok(is.null(storeLog$last), "nothing is kept for a file nobody has worked on")
  # an earlier session's work, as the browser would hand it back
  S <- defaultSettings(names(g)); S$method[8] <- "ModNegExp"; S$difference[3] <- TRUE
  old <- stateToJSON(S, list("644081" = "a line"), c("644011", "644081"), list("Linear"))
  key <- storeKey()
  session$setInputs(autosaved = list(key = "iDetrend:other.rwl:1", value = old, saved = "yesterday"))
  ok(is.null(restoreOffer()), "a copy for another file is not offered")
  session$setInputs(autosaved = list(key = key, value = NULL, saved = NULL))
  ok(is.null(restoreOffer()), "no copy, no offer")
  session$setInputs(autosaved = list(key = key, value = "{broken", saved = "x"))
  ok(is.null(restoreOffer()), "a copy that cannot be read is not offered")
  session$setInputs(autosaved = list(key = key, value = old, saved = "yesterday"))
  ok(!is.null(restoreOffer()) && allDefault() && is.null(storeLog$last),
     "a copy for this file is offered, and nothing is applied or overwritten until the user answers")
  session$setInputs(restoreYes = 1)
  ok(all(sameSettings(settings(), S)) && notes()[["644081"]] == "a line" && setequal(seen(), c("644011", "644081")) &&
       nrow(gaps()) == 0 && identical(rwlRV$fills, list("Linear")) && levels()[["644021"]] != "error",
     "Restore brings back settings, notes, series looked at, and the gap fill")
  ok(grepl("ModNegExp", storeLog$last$value) && unsaved(), "and it is kept again from there")
  code <- detrendCode("work.csv", FALSE, rwlRV$fills, settings(), allFits(), as.numeric(rownames(rwlRV$dat)))
  ok(any(grepl('fill.internal.NA(dat, fill = "Linear")', code, fixed = TRUE)), "the restored fill is in the R code")
  # the same file again, in a new session: this time start fresh
  session$setInputs(replaceConfirm = 0)
})
testServer(srv, {
  data(nm046)
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(Year = as.numeric(rownames(nm046)), plain(nm046), check.names = FALSE), f, row.names = FALSE, na = "")
  session$setInputs(file1 = list(name = "work.csv", datapath = f), navbar = "OverviewTab",
                    rwlPlotType = "seg", resultsPlotType = "crn", reportPlots = FALSE)
  S <- defaultSettings(names(nm046)); S$method[2] <- "Friedman"
  session$setInputs(autosaved = list(key = storeKey(), value = stateToJSON(S, list(), character(0), list()), saved = "x"))
  ok(!is.null(restoreOffer()), "offered")
  session$setInputs(restoreNo = 1)
  ok(is.null(restoreOffer()) && allDefault() && identical(storeLog$last, list(key = storeKey(), value = NULL)),
     "Start fresh applies nothing and deletes the copy")
  # work, then put it all back: the copy is removed, not left stale
  session$setInputs(navbar = "DetrendTab", series = "644011"); session$flushReact()
  setControls(session, ctlKey(), curRow(), 289, method = "Mean")
  ok(grepl('"Mean"', storeLog$last$value), "a change is kept")
  session$setInputs(resetSeries = 1); session$flushReact()
  ok(allDefault() && is.null(storeLog$last$value), "back at the defaults, the copy is removed")
})

# ── Starting from a goal ──────────────────────────────────────────────────────
testServer(srv, {
  session$setInputs(useDemo = 1, navbar = "OverviewTab", rwlPlotType = "seg",
                    resultsPlotType = "crn", reportPlots = FALSE, goal = "default")
  ok(grepl("What is the chronology for", output$overviewUI$html), "the Overview asks what the chronology is for")
  ok(grepl("two-thirds", output$goalDetail$html) && grepl("typical series here \\(138 rings\\)", output$goalDetail$html),
     "the default goal is explained, in years for a typical series of this file")
  session$setInputs(goal = "annual")
  ok(grepl("32-year", output$goalDetail$html) && grepl("faster than about 18 years", output$goalDetail$html),
     "choosing a goal shows what it starts with and what that keeps")
  ok(allDefault() && is.null(rwlRV$goal), "nothing changes until it is applied")
  session$setInputs(goalApply = 1)
  ok(all(settings()$spl.nyrs == 32) && rwlRV$goal == "annual" && grepl("32-year", output$goalApplied$html),
     "applied: every series starts with a 32-year spline")
  ok(grepl('"goal":\\["annual"\\]', storeLog$last$value), "and the goal is kept with the work in progress")
  ok(!allDefault() && !is.null(crnBase()), "the baseline for comparison is still dplR's default")
  # change one series by hand, then another goal: asks first
  session$setInputs(navbar = "DetrendTab", series = "644031"); session$flushReact()
  setControls(session, ctlKey(), curRow(), 154, method = "Friedman", note = "by hand")
  session$setInputs(navbar = "OverviewTab", goal = "decadal", goalApply = 2)
  ok(settings()$method[3] == "Friedman" && rwlRV$goal == "annual", "a goal that would replace hand-made settings waits for confirmation")
  session$setInputs(goalConfirm = 1)
  ok(all(settings()$method == "ModNegExp") && rwlRV$goal == "decadal" && notes()[["644031"]] == "by hand",
     "confirmed: every series restarted, notes kept")
  txt <- paste(readLines(output$detrendReport, warn = FALSE), collapse = " ")
  ok(grepl("Starting point", txt) && grepl("climate over decades and longer", txt), "the report records the starting point")
  session$setInputs(goal = "decadal", goalApply = 3)
  ok(TRUE, "applying the same goal again, with nothing changed by hand, needs no confirmation")
})

# ── Trees and cores, statistics through time, kinds of chronology ────────────
testServer(srv, {
  data(ca533)
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(Year = as.numeric(rownames(ca533)), plain(ca533), check.names = FALSE), f, row.names = FALSE, na = "")
  session$setInputs(file1 = list(name = "ca533.csv", datapath = f), navbar = "ResultsTab",
                    rwlPlotType = "seg", resultsPlotType = "crn", reportPlots = FALSE)
  ok(sumNow()$stats$n.trees == 21 && sumNow()$stats$n.cores == 34, "by default the trees are read from the names: 34 cores, 21 trees")
  ok(crnTucsonOK() && grepl("downloadCrnTucson", output$crnTucsonUI$html), "a chronology of ratios is offered as a Tucson .crn")
  back <- suppressMessages(read.crn(output$downloadCrnTucson))
  ok(names(back)[1] == "CA533" && isTRUE(all.equal(back[[1]], round(crnNow()[[1]], 3))), "which reads back with read.crn()")
  S <- settings(); S$difference <- TRUE; settings(S); session$flushReact()
  ok(any(crnNow()[[1]] < 0) && !crnTucsonOK() && grepl("values below zero", output$crnTucsonUI$html),
     "indices by subtraction go below zero: no Tucson .crn, and the panel says why")
  S$difference <- FALSE; settings(S); session$flushReact()
  ok(grepl("34 series from 21 trees", output$idsNote$html) && grepl("CAM031, CAM032", output$idsNote$html),
     "the Results panel says how the trees were counted, and lists them")
  ss <- sssNow()
  ok(length(ss) == nrow(rwiNow()) && isTRUE(all.equal(unname(ss), as.numeric(sss(rwiNow(), ids = idsFor(idsNow()$ids, rwiNow()))))),
     "SSS for every year, counting trees")
  from <- sssFrom(ss)
  ok(grepl("SSS", output$statsUI$html) && grepl(paste0(">", from, "<"), output$statsUI$html) &&
       grepl("rough guide for SSS", output$statsUI$html),
     "the Results panel gives the year the chronology becomes reliable, and says 0.85 is a convention")
  crnCsv <- read.csv(output$downloadCrn)
  ok("sss" %in% names(crnCsv) && isTRUE(all.equal(crnCsv$sss, round(unname(ss), 3))), "the chronology download carries SSS")
  epsTrees <- sumNow()$stats$eps
  session$setInputs(idsMode = "none")
  ok(sumNow()$stats$n.trees == 34 && sumNow()$stats$eps > epsTrees && grepl("each counted as its own tree", output$idsNote$html),
     "counting every core as a tree gives a higher EPS: the overstatement the ids remove")
  session$setInputs(idsMode = "position", stcSite = 3, stcTree = 2, stcCore = 1)
  ok(sumNow()$stats$n.trees == 21 && idsNow()$code == "ids <- read.ids(dat, stc = c(3, 2, 1))", "by position")
  session$setInputs(stcTree = NA)
  ok(!is.null(idsNow()$error) && sumNow()$stats$n.trees == 34 && grepl("could not be read as trees", output$idsNote$html),
     "positions that are not numbers: said so, and every series counted as a tree")
  session$setInputs(idsMode = "auto")

  # through time
  session$setInputs(resultsPlotType = "run", runWin = 50)
  run <- runNow()
  ok(is.data.frame(run) && all(c("mid.year", "eps", "rbar.eff", "n.trees") %in% names(run)) && max(run$n.trees) <= 21,
     "statistics through time, in trees")
  output$resultsPlot
  session$setInputs(runWin = 100)
  ok(runNow()$end.year[1] - runNow()$start.year[1] + 1 == 100, "the window can be changed")
  session$setInputs(runWin = 5000)
  ok(is.character(runNow()) && grepl("larger than the number of years", runNow()), "a window longer than the data gives dplR's message, not a crash")
  session$setInputs(runWin = 50, resultsPlotType = "crn")

  # kinds of chronology
  std <- crnNow()
  ok(names(std)[1] == "std", "the standard chronology by default")
  for (ty in c("res", "ars", "vsc")) {
    session$setInputs(crnType = ty, crnWin = 51, crnBiweight = TRUE)
    ok(names(crnNow())[1] == ty && !isTRUE(all.equal(crnNow()[[1]], std[[1]])), paste("chronology:", ty))
    output$resultsPlot
  }
  session$setInputs(crnType = "vsc", crnWin = 5000)
  ok(is.null(crnNow()) && grepl("shorter than the chronology", crnResNow()), "a stabilising window that is too long gives dplR's message")
  session$setInputs(crnType = "res", crnBiweight = FALSE)
  crn <- read.csv(output$downloadCrn)
  ok(identical(names(crn), c("Year", "res", "samp.depth", "sss")), "the chronology download is the kind chosen")
  rep <- output$detrendReport
  txt <- paste(readLines(rep, warn = FALSE), collapse = " ")
  ok(grepl("Residual chronology \\(arithmetic mean\\)", txt) && grepl("34 series from 21 trees", txt), "the report names the chronology and the trees")
  ok(grepl("How far back enough trees reach", txt) && grepl("rough guide for SSS", txt) &&
       grepl("signal <- sss(rwi, ids = ids)", reportCode(rep), fixed = TRUE),
     "the report gives SSS, the caution about 0.85, and the code for it")
  code <- reportCode(rep)
  ok(grepl("chron(rwi, biweight = FALSE, prewhiten = TRUE)", code, fixed = TRUE) && grepl("summary(rwi, ids = ids)", code, fixed = TRUE),
     "and its code builds that chronology, with the ids")
  old <- setwd(dirname(f)); file.copy(f, "ca533.csv", overwrite = TRUE)
  e <- runCode(code); setwd(old)
  v <- e$crn$res; v[is.nan(v)] <- NA
  ok(isTRUE(all.equal(v, crnNow()$res)) && isTRUE(all.equal(plain(e$rwi), plain(rwiNow()), check.attributes = FALSE)),
     "run on the file, the code gives the app's residual chronology")
})

# ── Part of a series, order, copy rules ───────────────────────────────────────
testServer(srv, {
  session$setInputs(useDemo = 1, navbar = "DetrendTab", series = "644011", rwlPlotType = "seg",
                    resultsPlotType = "crn", reportPlots = TRUE, seriesOrder = "file")
  session$flushReact()
  ok(grepl("Rings to use", output$controls$html) && grepl('data-min="1681"', output$controls$html) &&
       grepl('data-max="1969"', output$controls$html), "the controls offer the series' own span of years")
  setControls(session, ctlKey(), curRow(), 289, trim = c(1681, 1969))
  ok(allDefault(), "the slider at both ends of the series means every ring: nothing changes")
  setControls(session, ctlKey(), curRow(), 289, trim = c(1750, 1969))
  ok(curRow()$first == 1750 && is.na(curRow()$last) && curFit()$n.dropped == 69, "dragging the first year in leaves those rings out")
  ok(grepl("69 rings are left out", output$fitMessage$html) && grepl("series of 220 rings", output$curveSays$html),
     "the message and the spline's description follow")
  output$seriesPlot
  ok(seriesTableData()$First[1] == 1750 && min(as.numeric(rownames(rwiNow()))) == 1750, "Results show the span used; the chronology starts later")
  # copying leaves the rings each series uses alone, both ways
  setControls(session, ctlKey(), curRow(), 289, trim = c(1750, 1969), method = "Friedman")
  session$setInputs(copyTo = c("644021", "644031"), copyApply = 1)
  S <- settings()
  ok(all(S$method[2:3] == "Friedman") && all(is.na(S$first[2:3])) && S$first[1] == 1750, "copied settings do not carry the rings to use")
  session$setInputs(navbar = "OverviewTab", goal = "annual", goalApply = 1); session$setInputs(goalConfirm = 1)
  ok(all(settings()$spl.nyrs == 32) && settings()$first[1] == 1750, "a starting point keeps each series' rings to use")
  e <- runCode(reportCode(output$detrendReport))
  ok(isTRUE(all.equal(plain(e$rwi), plain(rwiNow()), check.attributes = FALSE)) && identical(rownames(e$rwi), rownames(rwiNow())),
     "the report's code rebuilds the trimmed indices")
  session$setInputs(navbar = "DetrendTab")
  session$setInputs(resetSeries = 1)
  ok(is.na(curRow()$first), "Back to the default restores every ring")

  # order
  ok(identical(seriesList(), seriesNames()), "listed as in the file to begin with")
  session$setInputs(seriesOrder = "shortest")
  ok(seriesList()[1] == "644081" && seriesList()[8] == "644011", "shortest first")
  S <- settings(); S$method <- "Spline"; S$spl.nyrs <- 0.67; settings(S); session$flushReact()
  session$setInputs(seriesOrder = "check")
  ok(identical(seriesList()[1:2], c("644041", "644081")) && levels()[["644081"]] == "warning", "needing a look first")
  session$setInputs(series = "644081"); session$flushReact()
  setControls(session, ctlKey(), curRow(), 84, method = "ModNegExp"); session$flushReact()
  ok(identical(seriesList()[1:2], c("644041", "644081")) && levels()[["644081"]] != "warning", "the order holds still when a series is fixed")
  output$seriesProgress

  # copy rules
  session$setInputs(series = "644011", copyTo = character(0)); session$flushReact()
  session$setInputs(copyRuleOp = "lt", copyRuleN = 100, copyRuleAdd = 1)
  session$setInputs(copyRuleN = NA, copyRuleAdd = 2)
  session$setInputs(copyFlagged = 1)
  ok(TRUE, "adding series by length, a missing number, and the marked series do not error")
})

# ── The guide and the starting points ────────────────────────────────────────
testServer(srv, {
  session$setInputs(useDemo = 1, navbar = "OverviewTab", rwlPlotType = "seg",
                    resultsPlotType = "crn", reportPlots = FALSE, goal = "disturbance")
  session$setInputs(goalApply = 1)
  ok(rwlRV$goal == "disturbance" && all(settings()$spl.nyrs == 50), "a starting point applied before the guide")
  session$setInputs(navbar = "DetrendTab", series = "644021"); session$flushReact()
  setControls(session, ctlKey(), curRow(), 175, note = "kept through the guide")
  session$setInputs(guideStart = 1)
  ok(!guideRV$on && rwlRV$goal == "disturbance", "starting the guide then asks before changing anything")
  session$setInputs(guideStartConfirm = 1)
  ok(guideRV$on && allDefault() && is.null(rwlRV$goal) && notes()[["644021"]] == "kept through the guide",
     "confirmed: the example is back at dplR's default, notes kept, and the guide runs")
  ok(levels()[["644081"]] == "warning", "so the series the guide teaches with is flagged again")
  session$setInputs(guideNext = 1); session$flushReact()
  # another starting point while the guide runs: asks, then hides the guide
  session$setInputs(navbar = "OverviewTab", goal = "annual", goalApply = 2)
  ok(guideRV$on && allDefault(), "a starting point applied while the guide runs waits for confirmation")
  session$setInputs(goalConfirm = 1)
  ok(!guideRV$on && rwlRV$goal == "annual" && all(settings()$spl.nyrs == 32) &&
       grepl("Show me how", output$guideUI$html),
     "confirmed: applied, and the guide is hidden, with the offer to start it again")
  session$setInputs(guideStart = 2, guideStartConfirm = 2)
  ok(guideRV$on && allDefault() && guideStep() == 1, "started again from the default, and from the first step")
  # the default goal does not disturb the guide
  session$setInputs(goal = "default", goalApply = 3)
  ok(guideRV$on && allDefault(), "applying 'I am not sure yet' leaves the guide running")
})

# ── The guide, start to finish ────────────────────────────────────────────────
testServer(srv, {
  session$setInputs(useDemo = 1, navbar = "OverviewTab", rwlPlotType = "seg",
                    resultsPlotType = "crn", reportPlots = FALSE)
  ok(grepl("Show me how", output$guideUI$html), "the guide is offered with the example data")
  session$setInputs(guideStart = 1)
  ok(guideStep() == 1 && grepl("Look at the data", output$guideUI$html), "guide starts")
  session$setInputs(guideNext = 1); session$flushReact()
  ok(guideStep() == 2 && grepl("Take me there", output$guideUI$html), "step 2 is elsewhere: offers to go")
  session$setInputs(navbar = "DetrendTab", series = "644011"); session$flushReact()
  session$setInputs(guideNext = 2); session$flushReact()
  ok(guideIds()[guideStep()] == "compare", "reading steps finish with Next")
  session$setInputs(compare = "ModNegExp"); session$flushReact()
  ok(guideIds()[guideStep()] == "fix", "choosing a comparison finishes the compare step")
  session$setInputs(series = "644081"); session$flushReact()
  setControls(session, ctlKey(), curRow(), 84, method = "Mean"); session$flushReact()
  ok(guideIds()[guideStep()] == "fix", "choosing the mean is not a fix")
  setControls(session, ctlKey(), curRow(), 84, method = "ModNegExp"); session$flushReact()
  ok(guideIds()[guideStep()] == "all", "the guide's answer finishes the fix step")
  for (s in seriesNames()) { session$setInputs(series = s); session$flushReact() }
  ok(guideIds()[guideStep()] == "results", "looking at every series finishes that step")
  ok(allSeen() && grepl("chosen a curve for all 8 series", output$allSeenUI$html) &&
       grepl("goResults", output$allSeenUI$html) && grepl("1 series is still marked", output$allSeenUI$html),
     "and the Detrend panel says every series has a curve, with a link to the Results")
  session$setInputs(navbar = "ResultsTab", guideNext = 3); session$flushReact()
  ok(guideIds()[guideStep()] == "save", "on to saving")
  output$downloadRWI; output$detrendReport; session$flushReact()
  ok(is.na(guideStep()) && grepl("Guided example finished", output$guideUI$html), "guide finishes on download")
  # the claim in the guide's Results step: the chronologies part where
  # 644081 begins, and not by much
  d <- abs(crnNow()$std - crnBase()$std)
  yr <- as.numeric(rownames(crnNow()))
  ok(all(d[yr < 1886] == 0) && max(d[yr >= 1886]) > 0.05 && max(d) < 0.2,
     "the fix changes the chronology only from 1886, and slightly, as the guide says")
})

# ── A file with gaps, an empty series and a short series ─────────────────────
testServer(srv, {
  data(nm046)
  g <- nm046
  i <- which(!is.na(g[["644021"]])); g[["644021"]][i[30:31]] <- NA
  g[["EMPTY"]] <- NA_real_
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(Year = as.numeric(rownames(g)), plain(g), check.names = FALSE), f, row.names = FALSE, na = "")
  session$setInputs(file1 = list(name = "gappy.csv", datapath = f), navbar = "OverviewTab",
                    rwlPlotType = "seg", resultsPlotType = "crn", reportPlots = TRUE)
  ok(is.null(rwlRV$readError) && ncol(rwlRV$dat) == 8 && identical(rwlRV$dropped, "EMPTY"),
     "an empty series is left out on loading, and named")
  panel <- output$checkPanel$html
  ok(grepl("no measurements", panel) && grepl("EMPTY", panel) && grepl("1 series has years with no measurement", panel),
     "the Overview says so, and shows the gap with the fill controls")
  ok(levels()[["644021"]] == "error" && ncol(rwiNow()) == 7, "the gappy series is not detrended and not in the indices")
  session$setInputs(navbar = "ResultsTab")
  ok(grepl("could not be detrended", output$resultsAlerts$html) && seriesTableData()$Check[2] == "not detrended",
     "Results names it")
  session$setInputs(navbar = "DetrendTab", series = "644021"); session$flushReact()
  output$seriesPlot
  ok(grepl("alert-danger", output$fitMessage$html) && grepl("Fill the gaps on the Overview", output$fitMessage$html),
     "the Detrend panel says why and what to do")
  code <- reportCode(output$detrendReport)
  ok(grepl('rwi[["644021"]] <- NULL', code, fixed = TRUE), "the report's code leaves it out")
  session$setInputs(fillMethod = "Linear", fillGapsButton = 1)
  ok(nrow(gaps()) == 0 && levels()[["644021"]] != "error" && ncol(rwiNow()) == 8 && unsaved(),
     "after filling, every series is detrended")
  ok(grepl("were filled", output$checkPanel$html), "the fill is shown, with an undo")
  rep <- output$detrendReport
  code <- reportCode(rep)
  ok(grepl('fill.internal.NA(dat, fill = "Linear")', code, fixed = TRUE) && grepl("Gaps filled", paste(readLines(rep), collapse = " ")),
     "the report records the fill")
  # run the code against the same file
  old <- setwd(dirname(f)); file.copy(f, "gappy.csv", overwrite = TRUE)
  e <- runCode(code); setwd(old)
  ok(isTRUE(all.equal(plain(e$rwi[, names(rwiNow())]), plain(rwiNow()), check.attributes = FALSE)),
     "and its R code, run on the file, rebuilds the indices (the empty series aside)")
  session$setInputs(undoFills = 1)
  ok(nrow(gaps()) == 1 && levels()[["644021"]] == "error", "undo brings the gap back")
})

# ── Files that cannot be used ─────────────────────────────────────────────────
testServer(srv, {
  f <- tempfile(fileext = ".rwl"); writeLines(c("this is not", "a ring width file"), f)
  session$setInputs(file1 = list(name = "junk.rwl", datapath = f), navbar = "OverviewTab")
  ok(!is.null(rwlRV$readError) && is.null(rwlRV$dat), "an unreadable file does not end the session")
  ok(grepl("File could not be read", output$overviewUI$html) && grepl("Could not be read", output$fileInfo$html),
     "and the Overview and sidebar say so")
  ok(grepl("could not be read", output$noDataDetrendTab$html), "the other panels point to the Overview")
  # one series: no statistics across series, but everything else works
  data(nm046)
  one <- nm046[, 1, drop = FALSE]
  write.csv(data.frame(Year = as.numeric(rownames(one)), plain(one), check.names = FALSE), f, row.names = FALSE, na = "")
  session$setInputs(file1 = list(name = "one.csv", datapath = f), rwlPlotType = "spag",
                    resultsPlotType = "crn", reportPlots = TRUE)
  ok(ncol(rwlRV$dat) == 1 && is.null(sumNow()) && !is.null(crnNow()), "a one-series file loads")
  session$setInputs(navbar = "DetrendTab", series = names(one)); session$flushReact()
  output$seriesPlot; output$fitMessage
  ok(is.null(tryCatch(output$seriesEffect, error = function(e) NULL)), "no comparison with others when there are none")
  session$setInputs(navbar = "ResultsTab")
  output$resultsPlot; output$seriesTable
  ok(grepl("need two or more", output$statsUI$html), "statistics say they need two series")
  ok(grepl("Report: Detrending", paste(readLines(output$detrendReport, warn = FALSE), collapse = " ")),
     "the report renders for one series")
})

# ── A large file: statistics on request ──────────────────────────────────────
testServer(srv, {
  data(ca533)
  big <- do.call(cbind, lapply(1:4, function(k) stats::setNames(plain(ca533), paste0(names(ca533), ".", k))))
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(Year = as.numeric(rownames(ca533)), big, check.names = FALSE), f, row.names = FALSE, na = "")
  session$setInputs(file1 = list(name = "big.csv", datapath = f), navbar = "ResultsTab",
                    rwlPlotType = "seg", resultsPlotType = "crn", reportPlots = FALSE)
  ok(is.null(sssNow()), "SSS waits with the other statistics on a large file")
  ok(ncol(rwlRV$dat) == 136 && !statsAuto() && is.null(sumNow()) && !is.null(crnNow()),
     "136 series: the chronology is built, the statistics wait to be asked for")
  ok(grepl("Compute the statistics", output$statsUI$html) && all(is.na(seriesTableData()$r)),
     "the Results panel offers to compute them")
  txt <- paste(readLines(output$detrendReport, warn = FALSE), collapse = " ")
  ok(grepl("had not been computed", txt), "a report made before then says the statistics are missing")
  session$setInputs(computeStats = 1)
  ok(!is.null(sssNow()) && length(sssNow()) == nrow(rwiNow()), "and is computed with them")
  ok(!is.null(sumNow()) && grepl("EPS", output$statsUI$html) && all(!is.na(seriesTableData()$r)),
     "computed on request")
  session$setInputs(navbar = "DetrendTab", series = seriesNames()[3]); session$flushReact()
  setControls(session, ctlKey(), curRow(), sum(!is.na(rwlRV$dat[[3]])), method = "Mean")
  ok(is.null(sumNow()) && !is.null(sumBase()), "a change to the settings puts them out of date; the baseline is kept")
})

# ── Undo ──────────────────────────────────────────────────────────────────────
testServer(srv, {
  session$setInputs(useDemo = 1, navbar = "DetrendTab", series = "644011", rwlPlotType = "seg",
                    resultsPlotType = "crn", reportPlots = FALSE)
  session$flushReact()
  ok(length(history()) == 0 && is.null(tryCatch(output$undoUI, error = function(e) NULL)), "nothing to undo at the start")
  S0 <- settings()
  setControls(session, ctlKey(), curRow(), 289, splProp = 0.5)
  setControls(session, ctlKey(), curRow(), 289, splProp = 0.4)
  setControls(session, ctlKey(), curRow(), 289, splProp = 0.3, note = "kept")
  ok(length(history()) == 1 && curRow()$spl.nyrs == 0.3 && grepl("Undo: change to 644011", output$undoUI$html),
     "three moves of one slider are one step to undo")
  session$setInputs(series = "644021"); session$flushReact()
  setControls(session, ctlKey(), curRow(), 175, method = "Friedman")
  S1 <- settings()
  session$setInputs(copyTo = c("644031", "644041", "644051"), copyApply = 1)
  ok(length(history()) == 3 && grepl("Undo: copy to 3 series", output$undoUI$html), "a change to another series, and a copy, are steps of their own")
  before <- ctlKey()$id
  session$setInputs(undo = 1)
  ok(all(sameSettings(settings(), S1)) && ctlKey()$id != before, "undo takes back the copy, and the controls are rebuilt")
  session$setInputs(undo = 2)
  ok(settings()$method[2] == "Spline" && settings()$spl.nyrs[1] == 0.3, "then the change to 644021")
  session$setInputs(undo = 3)
  ok(all(sameSettings(settings(), S0)) && notes()[["644011"]] == "kept" && length(history()) == 0,
     "then the slider: back to the start, with the note kept")
  session$setInputs(undo = 4)
  ok(all(sameSettings(settings(), S0)), "undo with nothing left does nothing")
  # a starting point and a reset can be undone too
  session$setInputs(navbar = "OverviewTab", goal = "annual", goalApply = 1)
  ok(rwlRV$goal == "annual" && grepl("Undo: the starting point", output$undoUI$html), "a starting point is a step")
  session$setInputs(undo = 5)
  ok(is.null(rwlRV$goal) && all(sameSettings(settings(), S0)), "undone: settings and the starting point both")
})

# ── The second example (Gus Pearson) and its guide ───────────────────────────
testServer(srv, {
  session$setInputs(useDemo2 = 1, navbar = "OverviewTab", rwlPlotType = "seg",
                    resultsPlotType = "crn", reportPlots = FALSE, goal = "default", showOthers = TRUE)
  data(gp.rwl)
  ok(rwlRV$example && rwlRV$name == "gusPearson" && ncol(rwlRV$dat) == 16 && inherits(rwlRV$dat, "rwl") &&
       rwlRV$label == "Example data (Gus Pearson, 8 trees)" && all(names(rwlRV$dat) %in% names(gp.rwl)) &&
       isTRUE(all.equal(plain(rwlRV$dat), plain(gp.rwl[, names(rwlRV$dat)]), check.attributes = FALSE)),
     "the second example loads: 16 series of dplR's gp.rwl")
  ok(sum(!is.na(gp.rwl)) == 16408, "gp.rwl has the 16,408 rings Biondi (1999) gives for the large pines")
  yr  <- as.numeric(rownames(rwlRV$dat))
  per <- function(crn) {
    y <- as.numeric(rownames(crn))
    tapply(crn[[1]], cut(y, c(1899, 1919, 1939, 1959, 1979, 1990)), mean)
  }
  ok(grepl("a crowded stand, and what the curve decides", output$guideUI$html), "and offers its own guide")
  ok(grepl("Other example:", output$fileUI$html) && grepl("Douglas-fir, New Mexico", output$fileUI$html) &&
       !grepl("useDemo2", output$fileUI$html),
     "with this example loaded, the sidebar offers the other one")
  session$setInputs(guideStart = 1)
  ok(guideRV$on && guideIds()[guideStep()] == "g.overview" && length(guideSteps()) == 9, "the guide starts")
  session$setInputs(guideNext = 1); session$flushReact()

  # the juvenile trend of 36B
  session$setInputs(navbar = "DetrendTab", series = "36B"); session$flushReact()
  x <- rwlRV$dat[["36B"]]; i <- which(!is.na(x))
  ok(yr[i[1]] == 1604 && round(mean(x[i[1:20]]), 1) == 3.8 && mean(x[yr >= 1700 & yr < 1900]) < 1 &&
       round(mean(curFit()$rwi[i[1:20]]), 2) == 1.33,
     "36B: starts 1604 near 4 mm, under 1 mm later; the default spline leaves its first rings a third too high")
  session$setInputs(compare = "ModNegExp"); session$flushReact()
  ok(guideIds()[guideStep()] == "g.end" && names(compareFits()) == "Modified negative exponential" &&
       round(mean(compareFits()[[1]]$rwi[i[1:20]]), 2) == 1.07,
     "comparing with the negative exponential, which fits it (first rings 1.07)")

  # the end of 10B
  session$setInputs(series = "10B"); session$flushReact()
  f <- curFit(); n <- max(which(!is.na(f$rwi)))
  ok(levels()[["10B"]] == "warning" && rwlRV$dat[["10B"]][n] == 0.07 && round(f$curve[n], 3) == 0.008 &&
       round(f$rwi[n], 1) == 8.7 && which.max(f$rwi) == n,
     "10B: last ring 0.07 mm over a curve of 0.008 is an index of 8.7, the highest in the series")
  session$setInputs(guideNext = 2); session$flushReact()

  # a flexible curve for every series, pinned
  session$setInputs(navbar = "OverviewTab", goal = "annual", goalApply = 1); session$flushReact()
  ok(guideRV$on && rwlRV$goal == "annual" && guideIds()[guideStep()] == "g.pin",
     "a starting point is a step of this guide, and does not hide it")
  session$setInputs(navbar = "ResultsTab")
  flat <- per(crnNow())
  ok(all(round(flat, 2) >= 0.98 & round(flat, 2) <= 1.01), "with 32-year splines the chronology is level through the 1900s (0.98 to 1.01)")
  session$setInputs(pinSettings = 1); session$flushReact()
  session$setInputs(compareWith = "pinned")
  ok(guideIds()[guideStep()] == "g.stiff", "pinning finishes that step")

  # a stiff one
  session$setInputs(navbar = "OverviewTab", goal = "decadal", goalApply = 2); session$flushReact()
  ok(rwlRV$goal == "decadal" && guideIds()[guideStep()] == "g.decline", "the stiff starting point applied")
  session$setInputs(navbar = "ResultsTab")
  stiff <- per(crnNow())
  ok(round(stiff[[1]], 2) == 1.49 && round(stiff[[5]], 2) == 0.50 && usePinned() &&
       all(round(per(crnBase()), 2) >= 0.98 & round(per(crnBase()), 2) <= 1.01),
     "now 1.49 in 1900-19 falling to 0.50 in the 1980s, against the level pinned chronology")
  output$resultsPlot
  session$setInputs(guideNext = 3); session$flushReact()
  session$setInputs(guideNext = 4); session$flushReact()
  ok(guideIds()[guideStep()] == "g.trees", "the reading steps")
  ok(grepl("16 series from 8 trees", output$idsNote$html) && round(sumNow()$stats$eps, 2) == 0.87 &&
       sssFrom(sssNow()) == 1706 &&
       length(unique(idsNow()$ids$tree[!is.na(unlist(rwlRV$dat[yr == 1705, ]))])) == 3,
     "16 series from 8 trees; EPS 0.87; SSS at or above 0.85 from 1706, before which three trees reach back")
  session$setInputs(idsMode = "none")
  ok(round(sumNow()$stats$eps, 2) == 0.91, "and EPS 0.91 if every core counted as a tree")
  session$setInputs(idsMode = "auto", guideNext = 5); session$flushReact()
  ok(is.na(guideStep()) && grepl("Guided example finished", output$guideUI$html), "the guide finishes")

  # the report: the subset is made by code, so it can be rebuilt anywhere
  rep  <- output$detrendReport
  code <- reportCode(rep)
  e <- runCode(code)
  ok(grepl("data(gp.rwl)", code, fixed = TRUE) && grepl('trees <- c("07", "10", "20", "36", "42", "47", "48", "52")', code, fixed = TRUE) &&
       isTRUE(all.equal(plain(e$dat), plain(rwlRV$dat), check.attributes = FALSE)) &&
       isTRUE(all.equal(plain(e$rwi), plain(rwiNow()), check.attributes = FALSE)) &&
       grepl("Example data \\(Gus Pearson, 8 trees\\) from dplR", paste(readLines(rep, warn = FALSE), collapse = " ")),
     "the report's code takes the same eight trees from gp.rwl and rebuilds the indices")
})
cat("\n\nAll server checks passed.\n")
