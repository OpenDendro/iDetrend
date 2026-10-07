# Smoke test on real files: drives server.R headlessly (shiny::testServer)
# over a sample of raw ITRDB .rwl files. For each file it loads the file,
# detrends every series by each of the seven methods in turn (and once with
# a power transform and indices by subtraction), builds the indices and the
# chronology, and for one file in four makes the report, runs the report's
# R code on the file, and compares the result with the app's indices.
# It records, per file and step, whether the step worked, how many series
# were not detrended or were marked to look at, and how long it took.
# tests/test-server.R checks that the numbers are right on the example
# data; this looks for crashes, series that fail, slow steps, and real
# files on which the R code does not reproduce the app.
#
# Needs the ITRDB clone next to the app (../../itrdbMeasurementsClone).
# Run from the app directory:
#   Rscript --vanilla tests/smoke-itrdb.R [n = 200] [cores = 8] [out = smoke-itrdb.rds]
args  <- commandArgs(trailingOnly = TRUE)
nFile <- if (length(args) >= 1) as.integer(args[1]) else 200L
cores <- if (length(args) >= 2) as.integer(args[2]) else 8L
out   <- if (length(args) >= 3) args[3] else "smoke-itrdb.rds"
clone <- "../../itrdbMeasurementsClone"

suppressPackageStartupMessages(source("global.R"))
source("ui.R", local = new.env())
appServer <- source("server.R")$value
srv <- appServer
formals(srv) <- formals(function(input, output, session) NULL)
plain <- function(x) as.data.frame(unclass(x), check.names = FALSE)

paths <- list.files(file.path(clone, "data_files"), pattern = "[.]rwl$",
                    recursive = TRUE, full.names = TRUE)
set.seed(20261006)
paths <- sample(paths, min(nFile, length(paths)))

oneFile <- function(i) {
  path <- paths[i]
  rows <- list()
  rec <- function(step, ok, secs, ...) {
    rows[[length(rows) + 1]] <<- data.frame(file = basename(path), step = step, ok = ok,
                                            secs = round(secs, 2), ..., stringsAsFactors = FALSE)
  }
  step <- function(name, expr, info = function(v) list()) {
    t0 <- proc.time()[["elapsed"]]
    v  <- tryCatch(expr, error = function(e) e)
    x  <- if (inherits(v, "error")) list(msg = conditionMessage(v)) else c(list(msg = ""), info(v))
    x  <- utils::modifyList(list(msg = "", series = NA, failed = NA, look = NA), x)
    rec(name, !inherits(v, "error"), proc.time()[["elapsed"]] - t0,
        msg = x$msg, series = x$series, failed = x$failed, look = x$look)
    v
  }
  pdf(NULL); on.exit(dev.off())
  tryCatch(testServer(srv, {
    counts <- function(v) list(series = length(levels()), failed = sum(levels() == "error"),
                               look = sum(levels() == "warning"))
    step("load", {
      session$setInputs(file1 = list(name = basename(path), datapath = path), navbar = "OverviewTab",
                        rwlPlotType = "seg", resultsPlotType = "crn", reportPlots = FALSE)
      if (!is.null(rwlRV$readError)) stop("not read: ", rwlRV$readError)
      output$overviewUI; output$checkPanel
    })
    if (is.null(rwlRV$dat)) return(NULL)
    # fill gaps as a user would have to
    if (nrow(gaps()) > 0) step("fill gaps", session$setInputs(fillMethod = "Linear", fillGapsButton = 1))
    session$setInputs(navbar = "DetrendTab", series = seriesNames()[1]); session$flushReact()
    setAll <- function(...) {
      S <- settings()
      new <- utils::modifyList(as.list(defaultSettings("x")), list(...))
      for (f in settingFields) S[[f]] <- new[[f]]
      settings(S)
      session$flushReact()
      allFits()
    }
    for (m in methodChoices) step(m, setAll(method = m), counts)
    step("powt + difference", setAll(method = "Spline", powt = "powt", difference = TRUE), counts)
    step("ModNegExp", setAll(method = "ModNegExp"), counts)
    step("indices and chronology", {
      if (is.null(rwiNow())) stop("no indices")
      if (is.null(crnNow())) stop("no chronology")
      output$seriesPlot; output$fitMessage
    })
    if (i %% 4 == 0) {
      rep <- step("report", output$detrendReport)
      step("report code reproduces", {
        txt <- paste(readLines(rep, warn = FALSE), collapse = "\n")
        blocks <- regmatches(txt, gregexpr("<pre><code>.*?</code></pre>", txt))[[1]]
        code <- xml2::xml_text(xml2::read_html(paste0("<p>", gsub("</?pre>|</?code>", "", blocks[length(blocks)]), "</p>")))
        code <- sub(paste0('read.rwl("', basename(path), '")'), paste0('read.rwl("', path, '")'), code, fixed = TRUE)
        e <- new.env()
        suppressWarnings(suppressMessages(utils::capture.output(eval(parse(text = code), e))))
        same <- isTRUE(all.equal(plain(e$rwi), plain(rwiNow()), check.attributes = FALSE)) &&
          identical(rownames(e$rwi), rownames(rwiNow()))
        if (!same) stop("the report's R code gives different indices")
      })
    }
  }), error = function(e) rec("session", FALSE, 0, msg = conditionMessage(e), series = NA, failed = NA, look = NA))
  do.call(rbind, rows)
}

# Separate R processes, not forks: forked R crashes on macOS once the
# graphics system has been used.
cl <- parallel::makeCluster(cores)
parallel::clusterExport(cl, c("paths", "oneFile", "plain"))
invisible(parallel::clusterEvalQ(cl, {
  suppressPackageStartupMessages(source("global.R"))
  source("ui.R", local = new.env())
  srv <- source("server.R")$value
  formals(srv) <- formals(function(input, output, session) NULL)
  NULL
}))
res <- parallel::parLapplyLB(cl, seq_along(paths), function(i) {
  tryCatch(oneFile(i), error = function(e)
    data.frame(file = basename(paths[i]), step = "harness", ok = FALSE, secs = 0,
               msg = conditionMessage(e), series = NA, failed = NA, look = NA))
})
parallel::stopCluster(cl)
res <- do.call(rbind, res)
saveRDS(res, out)

cat("\nFiles:", length(unique(res$file)), "\n\nSteps that failed:\n")
bad <- res[!res$ok, ]
print(if (nrow(bad)) as.data.frame(table(step = bad$step, msg = substr(bad$msg, 1, 90)))[
  as.data.frame(table(bad$step, substr(bad$msg, 1, 90)))$Freq > 0, ] else "none")
cat("\nSeries not detrended / to look at, by step (share of all series):\n")
agg <- aggregate(cbind(series, failed, look) ~ step, res[!is.na(res$series), ], sum)
agg$failed.pct <- round(100 * agg$failed / agg$series, 2)
agg$look.pct   <- round(100 * agg$look / agg$series, 2)
print(agg)
cat("\nSlowest steps (s):\n")
print(utils::head(res[order(-res$secs), c("file", "step", "secs", "series")], 8), row.names = FALSE)
