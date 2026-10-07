# Checks for appHelpers.R and plotDetrend.R. Run from the app directory:
#   Rscript --vanilla tests/test-helpers.R
suppressMessages(library(dplR))
source("appHelpers.R"); source("plotDetrend.R"); source("goals.R"); source("guide.R")
ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg); cat("ok  ", msg, "\n") }
plain <- function(x) as.data.frame(unclass(x), check.names = FALSE)
data(nm046)
d   <- nm046
yrs <- as.numeric(rownames(d))
fitAll <- function(dat, S) stats::setNames(lapply(seq_len(nrow(S)), function(i)
  detrendOne(dat[[S$series[i]]], S[i, ], S$series[i], as.numeric(rownames(dat)))), S$series)

# ── The defaults are dplR's ──
S    <- defaultSettings(names(d))
fits <- fitAll(d, S)
rwi  <- buildRWI(d, fits)
ref  <- suppressWarnings(detrend(d))
ok(inherits(rwi, "rwi"), "buildRWI returns class rwi")
ok(isTRUE(all.equal(plain(rwi), plain(ref), check.attributes = FALSE)),
   "untouched settings give what detrend(rwl) gives")

# ── The R code rebuilds the indices, for every method and option ──
runCode <- function(code) {
  e <- new.env()
  pdf(NULL); on.exit(dev.off())
  suppressWarnings(suppressMessages(utils::capture.output(eval(parse(text = code), e))))
  e
}
S2 <- S
S2$method     <- c("Spline", "AgeDepSpline", "ModNegExp", "ModHugershoff", "Friedman", "Mean", "Ar", "Spline")
S2$spl.nyrs   <- c(0.5, 0.67, 0.67, 0.67, 0.67, 0.67, 0.67, 30)
S2$spl.f      <- c(0.4, rep(0.5, 7))
S2$ads.nyrs   <- c(50, 30, rep(50, 6))
S2$pos.slope  <- c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
S2$constrain  <- c("never", "never", "never", "when.fail", rep("never", 4))
S2$span       <- c(NA, NA, NA, NA, 0.3, NA, NA, NA)
S2$bass       <- c(0, 0, 0, 0, 2, 0, 0, 0)
fits2 <- fitAll(d, S2)
ok(all(vapply(fits2, function(f) is.null(f$error), TRUE)), "every method fits the example data")
code <- detrendCode("nm046", TRUE, list(), S2, fits2, yrs)
ok(identical(code[2:3], c("data(nm046)", "dat <- nm046")), "an example is loaded from dplR by its name")
e <- runCode(code)
ok(isTRUE(all.equal(plain(e$rwi), plain(buildRWI(d, fits2)), check.attributes = FALSE)) &&
     identical(rownames(e$rwi), rownames(buildRWI(d, fits2))),
   "R code reproduces the indices: all seven methods, with their options")
ok(any(grepl('nyrs = 30', code)) && any(grepl('f = 0.4', code)) && any(grepl('span = 0.3, bass = 2', code)) &&
     any(grepl('constrain.nls = "when.fail"', code)) && !any(grepl("bass = 0", code)),
   "the code names the options that were set, and not those left at the default")

S3 <- S
S3$powt       <- c("powt", "rescale", rep("none", 6))
S3$difference <- c(TRUE, TRUE, TRUE, rep(FALSE, 5))
fits3 <- fitAll(d, S3)
e <- runCode(detrendCode("nm046", TRUE, list(), S3, fits3, yrs))
ok(isTRUE(all.equal(plain(e$rwi), plain(buildRWI(d, fits3)), check.attributes = FALSE)),
   "R code reproduces power transforms and indices by subtraction")
ok(!is.na(fits3[[1]]$power) && abs(mean(fits3[[1]]$rwi, na.rm = TRUE)) < 0.2,
   "power recorded; indices by subtraction centre on 0")
w <- collectionWarnings(S3, fits3)
ok(length(w) == 2 && grepl("3 of 8 series are indexed by subtraction", w[1]) &&
     grepl("2 of 8 series are power transformed", w[2]),
   "a mix of division and subtraction, or of transformed and not, is called out")
ok(length(collectionWarnings(S, fits)) == 0, "no collection warnings at the defaults")

# ── Fallbacks are reported with what they mean ──
st <- fitStatus(fits[["644081"]], S[8, ])
ok(fits[["644081"]]$used == "Mean" && fits[["644081"]]$dirtyDog && st$level == "warning" &&
     grepl("no trend was removed", st$text[1]),
   "644081: the default spline goes below zero; warned, with the consequence")
ok(fitStatus(fits[["644011"]], S[1, ])$level == "note" && length(fitStatus(fits[["644021"]], S[2, ])$text) == 0,
   "a clean fit has nothing to say (a zero ring is a note)")
s <- S[8, ]; s$method <- "ModNegExp"
f <- detrendOne(d[["644081"]], s, "644081")
st <- fitStatus(f, s)
ok(f$used == "Line" && st$level == "note" && grepl("straight line", st$text[1]),
   "negative exponential falling back to a line is a note, not a warning")
s <- S[8, ]; s$spl.nyrs <- 50
f <- detrendOne(d[["644081"]], s, "644081")
st <- fitStatus(f, s)
ok(f$used == "Spline" && st$level == "warning" && any(grepl("close to zero", st$text)),
   "a positive curve that nearly reaches zero is warned about: it inflates the indices")
s <- S[8, ]; s$difference <- TRUE
f <- detrendOne(d[["644081"]], s, "644081")
ok(f$used == "Spline" && !f$dirtyDog && fitStatus(f, s)$level == "ok",
   "by subtraction the same spline is used as fitted")
s <- S[1, ]; s$method <- "Ar"
f <- detrendOne(d[["644011"]], s, "644011")
st <- fitStatus(f, s)
ok(f$order > 0 && sum(is.na(f$rwi)) - sum(is.na(d[["644011"]])) == f$order && any(grepl("no index", st$text)),
   "Ar: the lost first years are counted and said")
s <- S[5, ]; s$method <- "Friedman"
ok(detrendOne(d[["644051"]], s, "644051")$used == "Friedman", "Friedman fits")

# ── Gaps and empty series never stop the app ──
g <- d
i <- which(!is.na(g[["644021"]]))
g[["644021"]][i[20:22]] <- NA
gp <- rwlGaps(g)
ok(nrow(gp) == 1 && gp$series == "644021" && gp$n == 3 && formatGaps(gp) == paste0(yrs[i[20]], "–", yrs[i[22]], " (3 yrs)"),
   "rwlGaps finds the gap")
fg <- fitAll(g, S)
ok(!is.null(fg[["644021"]]$error) && grepl("3 years with no measurement", fg[["644021"]]$error) &&
     fitStatus(fg[["644021"]], S[2, ])$level == "error",
   "a series with a gap is not detrended, and the message says what to do")
rg <- buildRWI(g, fg)
ok(ncol(rg) == 7 && !"644021" %in% names(rg), "it is left out of the indices: no column of NA")
ok(grepl("1 series could not be detrended", collectionWarnings(S, fg)[1]) &&
     grepl("644021", collectionWarnings(S, fg)[1]), "and named in the collection warnings")
code <- detrendCode("x.rwl", FALSE, list(), S, fg, yrs)
ok(any(grepl('rwi[["644021"]] <- NULL', code, fixed = TRUE)) && any(grepl('read.rwl("x.rwl")', code, fixed = TRUE)),
   "the R code drops it too")
for (fill in list(0, "Linear", "Mean")) {
  h <- fillAllGaps(g, fill)
  ok(inherits(h, "rwl") && identical(names(h), names(g)) && nrow(rwlGaps(h)) == 0 &&
       identical(rownames(h), rownames(g)),
     paste("fillAllGaps:", fill, "fills the gap and keeps class, names and years"))
}
h <- fillAllGaps(g, 0)
e <- new.env(); e$dat <- g
eval(parse(text = fillCode("0")), e)
ok(identical(plain(e$dat), plain(h)) && all(h[["644021"]][i[20:22]] == 0), "fillCode() is the same fill as R source")
ok(grepl('fill = "Linear"', fillCode("Linear")), "fillCode quotes a method name")
empty <- detrendOne(rep(NA_real_, 10), S[1, ], "x")
ok(grepl("no measurements", empty$error), "an empty series has an error, not a crash")

# ── Ar trims the years no series has an index for ──
SA <- S; SA$method <- "Ar"
fa <- fitAll(d, SA)
ra <- buildRWI(d, fa)
ok(min(as.numeric(rownames(ra))) == yrs[1] + fa[["644011"]]$order && all(rowSums(!is.na(ra)) > 0),
   "all-Ar: leading years with no index are trimmed")
code <- detrendCode("nm046", TRUE, list(), SA, fa, yrs)
e <- runCode(code)
ok(any(grepl("^rwi <- window", code)) && identical(rownames(e$rwi), rownames(ra)) &&
     isTRUE(all.equal(plain(e$rwi), plain(ra), check.attributes = FALSE)),
   "and the R code trims them the same way")

# ── Names that need quoting ──
q <- d[, 1:2]
names(q) <- c('A "quoted" id', "B\\C")
Sq <- defaultSettings(names(q))
fq <- fitAll(q, Sq)
tf <- tempfile(fileext = ".csv")
e <- new.env()
code <- detrendCode("q.csv", FALSE, list(), Sq, fq, as.numeric(rownames(q)))
code[2] <- "dat <- q"; e$q <- q
suppressWarnings(eval(parse(text = code[1:(grep("^rwi <- as.rwi", code))]), e))
ok(isTRUE(all.equal(plain(e$rwi), plain(buildRWI(q, fq)), check.attributes = FALSE)),
   "series names with quotes and backslashes survive the R code")

# ── Tables ──
tab <- settingsTable(d, S2, fits2)
ok(nrow(tab) == 8 && tab$Settings[1] == "50% of length, f 0.4" && tab$Settings[8] == "30 yrs" &&
     tab$`Curve used`[6] == "Mean" && tab$Rings[1] == 289 && tab$First[8] == 1886,
   "settingsTable: spans, settings in words, curve used")
ok(settingsTable(d, S, fits)$Check[8] == "look" && settingsTable(g, S, fg)$Check[2] == "not detrended",
   "settingsTable marks series to look at and series not detrended")
sm <- summary(rwi)
sf <- seriesFit(rwi, sm, S$series)
ok(nrow(sf) == 8 && all(sf$cor > 0.5) && all(is.finite(sf$trend)), "seriesFit: r and trend for each series")
sf <- seriesFit(rg, summary(rg), S$series)
ok(is.na(sf$cor[2]) && is.na(sf$trend[2]) && !is.na(sf$cor[1]), "seriesFit: NA for a series not in the indices")
sh <- rwiSheet(rwi)
write.csv(sh, tf, row.names = FALSE, na = "")
back <- suppressMessages(suppressWarnings(read.rwl(tf)))
ok(identical(names(back), names(rwi)) && isTRUE(all.equal(plain(back), round(plain(rwi), 4), check.attributes = FALSE)),
   "the indices csv is read back by read.rwl()")

# ── Settings files ──
S2n <- S2; S2n$note <- c("why, with a comma", rep("", 7))
write.csv(S2n, tf, row.names = FALSE, na = "")
r <- readSettings(tf, S)
ok(is.null(r$error) && all(sameSettings(r$settings, S2)) && length(r$matched) == 8 &&
     identical(r$notes, list("644011" = "why, with a comma")),
   "a saved settings file loads back exactly, with its notes")
write.csv(S2n[c(1, 3), ], tf, row.names = FALSE, na = "")
r <- readSettings(tf, S)
ok(length(r$matched) == 2 && length(r$missing) == 6 && sameSettings(r$settings[2, ], S[2, ]) &&
     sameSettings(r$settings[3, ], S2[3, ]),
   "series missing from the settings file keep what they had")
x <- S2n; x$series[1] <- "nope"
write.csv(x, tf, row.names = FALSE, na = "")
r <- readSettings(tf, S)
ok(identical(r$unknown, "nope") && identical(r$missing, "644011"), "unknown series are reported")
x <- S2n; x$method[2] <- "Splime"
write.csv(x, tf, row.names = FALSE, na = "")
ok(grepl("method iDetrend does not know", readSettings(tf, S)$error) && grepl("644021", readSettings(tf, S)$error),
   "a bad method stops the read and names the series")
x <- S2n; x$spl.nyrs[1] <- -1
write.csv(x, tf, row.names = FALSE, na = "")
ok(grepl("Nothing was changed", readSettings(tf, S)$error), "a bad number stops the read")
write.csv(data.frame(a = 1), tf, row.names = FALSE)
ok(grepl("not an iDetrend settings file", readSettings(tf, S)$error), "another csv is refused")

# ── What a spline removes, in years ──
# the gain of caps() on a sine wave of a given period, measured
capsGain <- function(period, nyrs, f = 0.5, n = 3000) {
  tt <- seq_len(n); m <- 500:2500
  cv <- caps(10 + sin(2 * pi * tt / period), nyrs = nyrs, f = f) - 10
  unname(coef(lm(cv[m] ~ 0 + sin(2 * pi * tt[m] / period))))
}
for (ny in c(20, 193)) for (f in c(0.5, 0.3)) for (g in c(0.1, 0.9)) {
  ok(abs(capsGain(splinePeriod(ny, f, g), ny, f) - g) < 0.005,
     sprintf("splinePeriod: caps(nyrs = %d, f = %.1f) puts %.0f%% of a wave of that period in the curve", ny, f, 100 * g))
}
ok(abs(splinePeriod(50, 0.5, 0.5) - 50) < 0.01 && abs(splinePeriod(50, 0.3, 0.3) - 50) < 0.01,
   "splinePeriod: at the frequency response the period is the rigidity")
cs <- curveSays(S[1, ], 289)
ok(grepl("193-year spline", cs[1]) && grepl("slower than about 330 years", cs[2]) &&
     grepl("longer than the series", cs[2]) && grepl("faster than about 110 years", cs[3]),
   "curveSays: the default spline on 289 rings, in years")
s <- S[1, ]; s$spl.nyrs <- 30
cs <- curveSays(s, 289)
ok(grepl("30-year spline", cs[1]) && grepl("about 52 years", cs[2]) && !grepl("longer than the series", cs[2]) &&
     grepl("about 17 years", cs[3]) && grepl("decade-to-decade", cs[3]),
   "curveSays: a 30-year spline, and what a flexible one leaves")
s <- S[1, ]; s$method <- "AgeDepSpline"
ok(grepl("50-year spline", curveSays(s, 100)[1]) && grepl("149-year spline", curveSays(s, 100)[1]),
   "curveSays: the age-dependent spline at its first and last ring")
for (m in c("ModNegExp", "ModHugershoff", "Friedman")) { s$method <- m; ok(length(curveSays(s, 100)) == 1, paste("curveSays:", m)) }
s$method <- "Mean"; ok(length(curveSays(s, 100)) == 0, "curveSays: nothing to add for the mean")

# ── The copy kept in the browser ──
j <- stateToJSON(S2, list("644011" = "a \"quoted\" note\nsecond line"), c("644011", "644031"), list("0", "Linear"))
r <- stateFromJSON(j, S)
ok(is.null(r$error) && all(sameSettings(r$settings, S2)) && identical(r$seen, c("644011", "644031")) &&
     identical(r$notes, list("644011" = "a \"quoted\" note\nsecond line")) && identical(r$fills, list("0", "Linear")),
   "the browser copy round-trips settings, notes, seen and fills exactly")
ok(is.na(r$settings$span[1]) && r$settings$span[5] == 0.3, "span by cross-validation (NA) survives the round trip")
ok(!is.null(stateFromJSON("not json", S)$error) && !is.null(stateFromJSON('{"settings":{}}', S)$error),
   "a copy that cannot be read is refused, not applied")
ok(!is.null(stateFromJSON(sub("ModNegExp", "Nope", j), S)$error), "a copy with a method the app does not know is refused")
ok(fileKey("a.rwl", d) == fileKey("a.rwl", d) && fileKey("a.rwl", d) != fileKey("b.rwl", d) &&
     fileKey("a.rwl", d) != fileKey("a.rwl", d[, 1:7]) && fileKey("a.rwl", d) != fileKey("a.rwl", d[, c(2, 1, 3:8)]),
   "fileKey: same file, same key; another name, or other series, another key")

# ── Starting points ──
ok(all(sameSettings(goalSettings("default", names(d)), S)), "the 'not sure' goal is dplR's default")
for (g in goals) {
  Sg <- goalSettings(g$id, names(d))
  fg2 <- fitAll(d, Sg)
  ok(all(names(g$set) %in% settingFields) && all(vapply(fg2, function(f) is.null(f$error), TRUE)) &&
       is.null(checkSettings(as.data.frame(lapply(Sg, as.character), stringsAsFactors = FALSE), S)$error),
     paste0("goal '", g$id, "': valid settings that detrend every example series"))
}
ok(goalSettings("annual", "a")$spl.nyrs == 32 && goalSettings("decadal", "a")$method == "ModNegExp" &&
     goalSettings("disturbance", "a")$spl.nyrs == 50, "the goals set what their text says")
r <- stateFromJSON(stateToJSON(S2, list(), character(0), list(), goal = "annual"), S)
ok(identical(r$goal, "annual") && is.null(stateFromJSON(stateToJSON(S2, list(), character(0), list()), S)$goal),
   "the goal is kept with the work in progress")

# ── Trees and cores ──
data(ca533); data(gp.rwl); data(co021)
ti <- treeIds(ca533)
ok(is.null(ti$error) && length(ti$warn) == 0 && treeCount(ti$ids) == "34 series from 21 trees" &&
     ti$code == "ids <- autoread.ids(dat)", "treeIds: ca533's 34 cores are 21 trees")
ok(treeCount(treeIds(gp.rwl)$ids) == "58 series from 29 trees", "treeIds: gp.rwl's 58 cores are 29 trees")
ok(length(treeIds(co021)$warn) == 1 && !is.null(treeIds(co021)$ids), "treeIds: an uncertain scheme comes back with dplR's warning")
tp <- treeIds(ca533, "position", c(3, 2, 1))
ok(identical(tp$ids$tree, ti$ids$tree) && tp$code == "ids <- read.ids(dat, stc = c(3, 2, 1))", "treeIds: by position, with its R code")
ok(is.null(treeIds(ca533, "none")$ids) && is.null(treeIds(ca533, "none")$code), "treeIds: none")
xi <- suppressWarnings(detrend(ca533))
ok(identical(rownames(idsFor(ti$ids, xi[, 5:9])), names(xi)[5:9]) && is.null(idsFor(NULL, xi)), "idsFor lines the ids up with the indices")
ok(summary(xi, ids = idsFor(ti$ids, xi))$stats$n.trees == 21 && summary(xi)$stats$n.trees == 34,
   "with ids, the statistics count 21 trees where they counted 34")

one <- ca533; names(one) <- sprintf("AAA01%02d", seq_along(one))
t1 <- treeIds(one, "position", c(3, 2, 2))
ok(is.null(t1$ids) && is.null(t1$code) && grepl("one tree", t1$error), "treeIds: a reading that makes every series one tree is refused, and says so")
ok(is.null(idsFor(ti$ids, xi[, c("CAM031", "CAM032")])) && !is.null(idsFor(ti$ids, xi[, c("CAM031", "CAM041")])),
   "idsFor: with one tree left among the indices, every series counts as a tree")
sg <- signalThroughTime(xi, ti$ids, 50)
ok(is.data.frame(sg) && nrow(sg) > 10 && max(sg$n.trees) <= 21, "signalThroughTime: windows along ca533, counting trees")
short <- window(xi, 1900, 1960)
ok(is.character(signalThroughTime(short, ti$ids, 50)) && grepl("too few for more than one window", signalThroughTime(short, ti$ids, 50)) &&
     is.data.frame(signalThroughTime(short, ti$ids, 20)),
   "signalThroughTime: data too short for two windows says so, and a shorter window works")
ok(grepl("larger than the number of years", signalThroughTime(xi, ti$ids, 5000)) &&
     grepl("two or more", signalThroughTime(xi[, 1, drop = FALSE], NULL, 50)),
   "signalThroughTime: dplR's error, or too few series, as a sentence")

# ── SSS ──
ss <- sssOf(xi, ti$ids)
ok(length(ss) == nrow(xi) && identical(names(ss), rownames(xi)) && all(ss > 0 & ss <= 1) &&
     isTRUE(all.equal(unname(ss), as.numeric(sss(xi, ids = ti$ids)))),
   "sssOf: dplR's sss() for every year, named by year, counting trees")
ok(is.null(sssOf(xi[, 1, drop = FALSE], NULL)) && is.null(sssOf(NULL, NULL)), "sssOf: nothing for fewer than two series")
ok(sssFrom(c("1900" = 0.5, "1901" = 0.9, "1902" = 0.7, "1903" = 0.86, "1904" = 0.9)) == 1903 &&
     sssFrom(c("1900" = 0.9, "1901" = 0.95)) == 1900 && is.na(sssFrom(c("1900" = 0.9, "1901" = 0.5))) &&
     is.na(sssFrom(NULL)),
   "sssFrom: the first year from which SSS stays at or above the cut-off; NA when it never does")
f85 <- sssFrom(ss)
ok(!is.na(f85) && f85 > min(as.numeric(names(ss))) && all(ss[as.numeric(names(ss)) >= f85] >= 0.85) &&
     ss[as.character(f85 - 1)] < 0.85, "on ca533 that year is where the early, thin end stops")
pdf(NULL)
plotChron(buildChron(xi), sss.from = f85)
plotSignal(rwi.stats.running(xi, ti$ids, window.length = 50), sss = ss)
invisible(dev.off())
ok(TRUE, "the chronology plot shades the years before it; the signal plot draws SSS")
ok(any(detrendCode("nm046", TRUE, list(), S, fits, yrs) == "signal <- sss(rwi)") &&
     any(detrendCode("nm046", TRUE, list(), S, fits, yrs, ids.code = "ids <- autoread.ids(dat)") == "signal <- sss(rwi, ids = ids)"),
   "the R code computes SSS, with the ids when there are any")

# ── Chronologies ──
for (ty in chronTypes) for (bw in c(TRUE, FALSE)) {
  cr <- buildChron(xi, ty, bw, 51)
  e  <- new.env(); e$rwi <- xi
  pdf(NULL); eval(parse(text = chronCode(ty, bw, 51)), e); invisible(dev.off())
  v <- e$crn[[ty]]; v[is.nan(v)] <- NA
  ok(identical(names(cr), c(ty, "samp.depth")) && isTRUE(all.equal(cr[[1]], v)) && identical(rownames(cr), rownames(xi)),
     paste0("buildChron and chronCode agree: ", ty, if (bw) ", robust mean" else ", arithmetic mean"))
}
ok(!isTRUE(all.equal(buildChron(xi, "std")[[1]], buildChron(xi, "res")[[1]])) &&
     !isTRUE(all.equal(buildChron(xi, "std", TRUE)[[1]], buildChron(xi, "std", FALSE)[[1]])),
   "the kinds of chronology, and the two means, differ")
ok(inherits(tryCatch(buildChron(xi, "vsc", TRUE, 5000), error = function(e) e), "error"), "a window longer than the chronology is dplR's error to report")
ok(chronLabel("res", FALSE) == "Residual chronology (arithmetic mean)", "chronLabel")
code <- detrendCode("nm046", TRUE, list(), S, fits, yrs, ids.code = "ids <- autoread.ids(dat)", crn.code = chronCode("ars"))
e <- runCode(code)
ok(any(grepl("summary(rwi, ids = ids)", code, fixed = TRUE)) && !is.null(e$ids) && !is.null(e$crn$ars),
   "the R code carries the tree ids and the kind of chronology")
code <- detrendCode("x.rwl", FALSE, list(), S, fg, yrs, ids.code = "ids <- autoread.ids(dat)")
ok(any(code == "ids <- ids[names(rwi), ]"), "and drops the ids of series that were not detrended")
pdf(NULL)
plotSignal(rwi.stats.running(xi, ti$ids, window.length = 50))
plotSignal(rwi.stats.running(xi, ti$ids, window.length = 50), rwi.stats.running(xi, window.length = 50))
plotChron(buildChron(xi, "res"), buildChron(xi, "res", FALSE), ylab = "Residual index")
invisible(dev.off())
ok(TRUE, "signal-through-time and residual chronology plots draw")

# ── Using only part of a series ──
St <- S
St$first[1] <- 1750; St$last[1] <- 1950; St$first[2] <- 1830; St$last[4] <- 1940
ft <- fitAll(d, St)
ok(ft[[1]]$n.dropped == 88 && all(is.na(ft[[1]]$rwi[yrs < 1750 | yrs > 1950])) && !anyNA(ft[[1]]$rwi[yrs >= 1750 & yrs <= 1950]) &&
     ft[[3]]$n.dropped == 0, "first and last year: the rings outside get no index")
ok(isTRUE(all.equal(ft[[1]]$rwi[yrs >= 1750 & yrs <= 1950],
                    detrend.series(d[[1]][yrs >= 1750 & yrs <= 1950], make.plot = FALSE), check.attributes = FALSE)),
   "and the curve is fitted to the rings used only")
code <- detrendCode("nm046", TRUE, list(), St, ft, yrs)
e <- runCode(code)
ok(any(code == "yrs <- time(dat)") && any(grepl("ifelse(yrs >= 1750 & yrs <= 1950,", code, fixed = TRUE)) &&
     any(grepl("ifelse(yrs <= 1940,", code, fixed = TRUE)) &&
     isTRUE(all.equal(plain(e$rwi), plain(buildRWI(d, ft)), check.attributes = FALSE)),
   "the R code trims the same rings and rebuilds the indices")
ok(!any(grepl("yrs <-|ifelse", detrendCode("nm046", TRUE, list(), S, fits, yrs))), "no trimming, no mention of it in the code")
st <- fitStatus(ft[[1]], St[1, ])
ok(any(grepl("88 rings are left out \\(before 1750 and after 1950\\)", st$text)) && any(grepl("not in the chronology", st$text)),
   "the message says how many rings are left out and what that costs")
tt <- settingsTable(d, St, ft)
ok(tt$First[1] == 1750 && tt$Last[1] == 1950 && tt$Rings[1] == 201 && grepl("rings from 1830 only", tt$Settings[2]) &&
     grepl("rings to 1940 only", tt$Settings[4]), "the table shows the span used")
s <- S[8, ]; s$first <- 3000
ok(grepl("No rings are left", detrendOne(d[[8]], s, "x", yrs)$error), "a first year after the series ends: an error to show, not a crash")
gy <- d[["644021"]]; gi <- which(!is.na(gy)); gy[gi[20:22]] <- NA
Sg <- S; Sg$first[2] <- yrs[gi[23]]
ok(!is.null(detrendOne(gy, S[2, ], "644021", yrs)$error) && is.null(detrendOne(gy, Sg[2, ], "644021", yrs)$error),
   "a gap in the rings left out no longer stops the series")
ok(all(sameSettings(St, S, copyFields)) && !all(sameSettings(St, S)), "sameSettings can leave the rings used out of the comparison")
write.csv(St, tf, row.names = FALSE, na = "")
r <- readSettings(tf, S)
ok(is.null(r$error) && all(sameSettings(r$settings, St)), "first and last survive the settings file")
old <- St[, setdiff(names(St), c("first", "last"))]
write.csv(old, tf, row.names = FALSE, na = "")
r <- readSettings(tf, S)
ok(is.null(r$error) && all(is.na(r$settings$first)) && all(sameSettings(r$settings, S)), "a settings file saved before first and last existed still loads")
x <- St; x$first[1] <- 1960
write.csv(x, tf, row.names = FALSE, na = "")
ok(grepl("first year after the last", readSettings(tf, S)$error), "first after last is refused")
r <- stateFromJSON(stateToJSON(St, list(), character(0), list()), S)
ok(all(sameSettings(r$settings, St)), "and the copy kept in the browser")
pdf(NULL); plotDetrend(yrs, ft[[1]], "644011", St[1, ]); plotDetrend(yrs, ft[[2]], "644021", St[2, ]); invisible(dev.off())
ok(TRUE, "the plot draws the rings left out")

# ── A curve that swings into its last rings ──
data(wa082)
wy <- as.numeric(rownames(wa082))
endMsg <- function(series, ...) {
  s <- utils::modifyList(as.list(defaultSettings(series)), list(...))
  s <- as.data.frame(s, stringsAsFactors = FALSE)
  f <- detrendOne(wa082[[series]], s, series, wy)
  cv <- f$curve[!is.na(f$curve)]
  list(st = fitStatus(f, s), ch = cv[length(cv)] / cv[length(cv) - 10] - 1)
}
e1 <- endMsg("712082", spl.nyrs = 15)
ok(abs(e1$ch) > 0.5 && e1$st$level == "warning" && any(grepl("Over its last 10 rings the curve (rises|falls) by", e1$st$text)) &&
     any(grepl("most recent years", e1$st$text)),
   "a flexible spline that swings by more than half over its last ten rings is flagged, with what it does")
e2 <- endMsg("712082", spl.nyrs = 0.67)
ok(abs(e2$ch) < 0.5 && !any(grepl("last 10 rings", e2$st$text)), "the same series with the default spline is not")
e3 <- endMsg("712082", spl.nyrs = 15, difference = TRUE)
ok(!any(grepl("last 10 rings", e3$st$text)), "nor by subtraction, where the curve is not divided by")
flagged <- names(d)[vapply(names(d), function(s) fitStatus(fits[[s]], S[S$series == s, ])$level == "warning", TRUE)]
ok(identical(flagged, c("644041", "644081")) &&
     any(grepl("falls by 65%", fitStatus(fits[["644041"]], S[4, ])$text)),
   "on the example data at the defaults it flags 644041, whose curve falls by 65% over its last ten rings")

# ── The order of the series ──
lv <- c(a = "ok", b = "warning", c = "note", d = "error", e = "warning"); rg <- c(a = 50, b = 300, c = 120, d = 80, e = 200)
ok(identical(seriesOrder(names(lv), "file", lv, rg), names(lv)) &&
     identical(seriesOrder(names(lv), "check", lv, rg), c("d", "b", "e", "c", "a")) &&
     identical(seriesOrder(names(lv), "longest", lv, rg), c("b", "e", "c", "d", "a")) &&
     identical(seriesOrder(names(lv), "shortest", lv, rg), c("a", "d", "c", "e", "b")),
   "seriesOrder: as in the file, needing a look first, by length")

# ── The script and the Tucson chronology ──
sl <- scriptLines(detrendCode("nm046", TRUE, list(), S2, fits2, yrs), "nm046 (example data from dplR)", "9.9")
e <- runCode(sl)
ok(grepl("^# Detrending of nm046", sl[1]) && grepl("iDetrend 9.9", sl[2]) &&
     isTRUE(all.equal(plain(e$rwi), plain(buildRWI(d, fits2)), check.attributes = FALSE)),
   "the script has a header and runs as it stands")
ct <- crnForTucson(buildChron(rwi, "std"), "nm-046 site.rwl")
ok(names(ct)[1] == "NM046S" && names(crnForTucson(buildChron(rwi), NULL))[1] == "CRN" &&
     names(crnForTucson(buildChron(rwi), "---.rwl"))[1] == "CRN", "crnForTucson: a six-character site code from the file name")
write.crn(ct, tf)
back <- suppressMessages(read.crn(tf))
ok(isTRUE(all.equal(back[[1]], round(ct[[1]], 3))) && identical(rownames(back), rownames(ct)) &&
     identical(back$samp.depth, ct$samp.depth), "the Tucson .crn reads back with read.crn() to three decimals")

# ── The examples ──
for (id in names(examples)) {
  e <- new.env(); eval(parse(text = examples[[id]]$code), e)
  code <- detrendCode(id, examples[[id]]$code, list(), defaultSettings(names(e$dat)),
                      fitAll(e$dat, defaultSettings(names(e$dat))), as.numeric(rownames(e$dat)))
  ok(inherits(e$dat, "rwl") && all(examples[[id]]$code %in% code) && !any(grepl("read.rwl", code)) &&
       length(examples[[id]]$guide$steps) > 5 && !anyDuplicated(vapply(examples[[id]]$guide$steps, `[[`, "", "id")),
     paste0("example '", id, "': its code makes an rwl and is what the report prints; its guide has steps"))
}
gp <- { e <- new.env(); eval(parse(text = examples$gusPearson$code), e); e$dat }
ok(ncol(gp) == 16 && treeCount(treeIds(gp)$ids) == "16 series from 8 trees" && all(table(treeIds(gp)$ids$tree) == 2),
   "the Gus Pearson set: 16 series, two cores for each of 8 trees")

# ── Small things ──
ok(downloadName("nm046.rwl", "indices", "csv") == paste0("nm046-indices-", Sys.Date(), ".csv") &&
     downloadName(NULL, "settings", "csv") == paste0("iDetrend-settings-", Sys.Date(), ".csv"),
   "download names")
ok(identical(sameSettings(S, S2), c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)) && all(sameSettings(S, S)),
   "sameSettings compares row by row")
chk <- as.data.frame(rwl.check(d, checks = c("structure", "series", "values", "zeros")))
ok(is.character(checkLines(chk)), "checkLines takes rwl.check() findings")
pdf(NULL)
plotDetrend(yrs, fits[["644081"]], "644081", S[8, ], compare = list("Friedman" = fits2[["644051"]]), sub = "Mean")
plotDetrend(yrs, fg[["644021"]], "644021", S[2, ], sub = "Not detrended")
plotDetrend(yrs, fa[["644011"]], "644011", SA[1, ])
plotChron(chron(rwi), chron(buildRWI(d, fits2)))
plotChron(chron(rwi))
plotDetrend(yrs, fits[["644011"]], "644011", S[1, ], others = rowMeans(plain(rwi)[, -1], na.rm = TRUE), others.n = 7)
invisible(dev.off())
ok(TRUE, "plots draw: a fallback with a comparison, a failed series, Ar, chronologies, the other series' mean")
cat("\nAll helper checks passed.\n")
