# Helpers shared by server.R and the report. Kept free of Shiny so they can
# be tested in a plain R session (see tests/test-helpers.R).

# ── readRWLsafely ─────────────────────────────────────────────────────────────
# Wraps read.rwl() so a bad file never takes the session down. Returns a list:
#   data  — the rwl object, or NULL if the file could not be read
#   error — the error message, or NULL
# Messages and warnings from the reader are swallowed here; the app reports
# what matters about the data itself (e.g. gaps, via rwlGaps()) instead of
# echoing reader chatter. No `verbose` argument is passed: read.rwl() hands
# it on to the format's reader, and read.compact(), read.fh() and
# read.tridas() don't take one.
readRWLsafely <- function(path) {
  res <- tryCatch(
    suppressMessages(suppressWarnings(
      if (isTridas(path)) readTridas(path) else read.rwl(path)
    )),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    return(list(data = NULL, error = conditionMessage(res)))
  }
  list(data = res, error = NULL)
}

# TRiDaS is detected here rather than left to read.rwl(): its auto-detection
# (dplR 1.8.0) looks for a bare "<tridas>" tag, so files whose root element
# carries a namespace, as write.tridas() writes them, are taken for Tucson
# and fail. read.tridas() also returns a list, with the ring widths in
# $measurements, rather than an rwl object.
isTridas <- function(path) {
  any(grepl("<tridas[ >]", readLines(path, n = 20, warn = FALSE)))
}

readTridas <- function(path) {
  m <- read.rwl(path, format = "tridas")$measurements
  if (is.data.frame(m)) return(as.rwl(m))
  if (is.list(m) && length(m) == 1) return(as.rwl(m[[1]]))
  stop("this TRiDaS file holds ", length(m), " sets of measurements (for ",
       "example several sites, species or variables), and iDetrend reads one ",
       "set at a time. Export the set you want to detrend (as Tucson, say) ",
       "and load that.")
}

# ── rwlGaps ───────────────────────────────────────────────────────────────────
# Interior gaps: years inside a series' span with no measurement. Since dplR
# 1.8.0, read.tucson() returns these as NA (older readers filled them with
# zero), and detrend.series() stops on them. Returns a data.frame with one
# row per gap: series, first, last, n.
rwlGaps <- function(rwl) {
  yrs <- as.numeric(rownames(rwl))
  out <- lapply(names(rwl), function(s) {
    x   <- rwl[[s]]
    idx <- which(!is.na(x))
    if (length(idx) < 2) return(NULL)
    inside <- seq(idx[1], idx[length(idx)])
    miss   <- inside[is.na(x[inside])]
    if (length(miss) == 0) return(NULL)
    runs <- split(miss, cumsum(c(1, diff(miss) != 1)))
    data.frame(series = s,
               first  = vapply(runs, function(r) yrs[r[1]], numeric(1)),
               last   = vapply(runs, function(r) yrs[r[length(r)]], numeric(1)),
               n      = vapply(runs, length, integer(1)),
               row.names = NULL, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  if (is.null(out)) {
    out <- data.frame(series = character(0), first = numeric(0),
                      last = numeric(0), n = integer(0))
  }
  out
}

# Human-readable gap description, e.g. "1689–1693 (5 yrs)".
formatGaps <- function(gaps) {
  ifelse(gaps$n == 1,
         as.character(gaps$first),
         paste0(gaps$first, "–", gaps$last, " (", gaps$n, " yrs)"))
}

# ── fillAllGaps ───────────────────────────────────────────────────────────────
# Fills every interior gap in the file with dplR's fill.internal.NA().
# `fill` is 0 (the years are absent rings), "Linear" or "Mean". "Spline" is
# not offered: it can return a negative width. fillCode() is the same call
# as R source, for the report.
# as.rwl(): fill.internal.NA() returns a plain data.frame in dplR 1.8.0.
fillAllGaps <- function(rwl, fill) {
  as.rwl(fill.internal.NA(rwl, fill = fill))
}

fillCode <- function(fill) {
  paste0("dat <- as.rwl(fill.internal.NA(dat, fill = ",
         if (identical(fill, 0) || identical(fill, "0")) "0" else paste0('"', fill, '"'),
         "))")
}

# ══════════════════════════════════════════════════════════════════════════════
# SETTINGS
# ══════════════════════════════════════════════════════════════════════════════
# One row per series says how that series is detrended. The app's controls
# write to this table and everything else (plots, indices, report, R code)
# reads from it.
#
#   method     — the detrend.series() method asked for
#   spl.nyrs   — Spline rigidity: years, or a share of the series length
#                when 1 or less (caps() reads it that way)
#   spl.f      — Spline frequency response at spl.nyrs
#   ads.nyrs   — AgeDepSpline starting rigidity in years
#   pos.slope  — AgeDepSpline, ModNegExp, ModHugershoff: allow a rise
#   constrain  — ModNegExp, ModHugershoff: constrain.nls
#   span       — Friedman span; NA means chosen by cross-validation ("cv")
#   bass       — Friedman bass
#   difference — index by subtraction (TRUE) or division (FALSE)
#   powt       — "none", "powt" (power transform) or "rescale" (power
#                transform, rescaled to the original mean and variance)
#   first,last — use only the rings from year `first` to year `last`; NA
#                means from the series' first ring, or to its last. These
#                two belong to the series itself (which of its rings to
#                trust), so they are not copied to other series or reset
#                by a starting point: see copyFields.
#
# The defaults are detrend.series()'s own, so a file nobody has touched
# gives what detrend(rwl) gives.

methodChoices <- c("Smoothing spline"                    = "Spline",
                   "Age-dependent spline"                = "AgeDepSpline",
                   "Modified negative exponential"       = "ModNegExp",
                   "Modified Hugershoff"                 = "ModHugershoff",
                   "Friedman's super smoother"           = "Friedman",
                   "Mean (horizontal line)"              = "Mean",
                   "Autoregressive model (prewhitening)" = "Ar")

methodLabel <- function(m) names(methodChoices)[match(m, methodChoices)]

powtChoices <- c("None" = "none",
                 "Power transform" = "powt",
                 "Power transform, rescaled" = "rescale")

settingFields <- c("method", "spl.nyrs", "spl.f", "ads.nyrs", "pos.slope",
                   "constrain", "span", "bass", "difference", "powt", "first", "last")
# what is copied from one series to another: how to detrend, not which rings
copyFields <- setdiff(settingFields, c("first", "last"))

defaultSettings <- function(series) {
  data.frame(series     = as.character(series),
             method     = rep("Spline", length(series)),
             spl.nyrs   = 0.67,
             spl.f      = 0.5,
             ads.nyrs   = 50,
             pos.slope  = FALSE,
             constrain  = "never",
             span       = NA_real_,
             bass       = 0,
             difference = FALSE,
             powt       = "none",
             first      = NA_real_,
             last       = NA_real_,
             stringsAsFactors = FALSE)
}

# TRUE for each row of `a` whose settings equal those of the same row of `b`.
sameSettings <- function(a, b, fields = settingFields) {
  key <- function(x) do.call(paste, c(unname(as.list(x[fields])), sep = "|"))
  key(a) == key(b)
}

# ── detrendArgs ───────────────────────────────────────────────────────────────
# The detrend.series() arguments one row of the settings asks for: the
# method and whatever that method reads. Arguments left at dplR's default
# are left out, except the spline's rigidity, which is always written so the
# R code says what it is. The app's own call and the report's R code are
# both built from this list, so they cannot disagree.
detrendArgs <- function(s) {
  a <- list(method = s$method)
  if (s$method == "Spline") {
    a$nyrs <- s$spl.nyrs
    if (!isTRUE(all.equal(s$spl.f, 0.5))) a$f <- s$spl.f
  }
  if (s$method == "AgeDepSpline") a$nyrs <- s$ads.nyrs
  if (s$method %in% c("AgeDepSpline", "ModNegExp", "ModHugershoff") && s$pos.slope) {
    a$pos.slope <- TRUE
  }
  if (s$method %in% c("ModNegExp", "ModHugershoff") && s$constrain != "never") {
    a$constrain.nls <- s$constrain
  }
  if (s$method == "Friedman") {
    if (!is.na(s$span)) a$span <- s$span
    if (s$bass != 0) a$bass <- s$bass
  }
  if (s$difference) a$difference <- TRUE
  a
}

# The same, in words, for tables: "67% of length", "bass 3", ...
paramText <- function(s) {
  a <- detrendArgs(s)
  c(if (s$method == "Spline") {
      if (a$nyrs <= 1) paste0(round(a$nyrs * 100), "% of length") else paste0(a$nyrs, " yrs")
    },
    if (s$method == "AgeDepSpline") paste0("starts at ", a$nyrs, " yrs"),
    if (!is.null(a$f)) paste("f", a$f),
    if (!is.null(a$pos.slope)) "positive slope allowed",
    if (!is.null(a$constrain.nls)) paste("constrained", sub("when.fail", "when fit fails", a$constrain.nls)),
    if (s$method == "Friedman") if (is.null(a$span)) "span by cross-validation" else paste("span", a$span),
    if (!is.null(a$bass)) paste("bass", a$bass),
    if (!is.na(s$first) || !is.na(s$last)) paste0(
      "rings ", if (is.na(s$first)) "to " else paste0("from ", s$first, if (!is.na(s$last)) " to "),
      if (!is.na(s$last)) s$last, " only")) |>
    paste(collapse = ", ")
}

# ── splinePeriod ──────────────────────────────────────────────────────────────
# The wavelength (years) at which a smoothing spline of rigidity `nyrs` and
# frequency response `f` puts the share `gain` of a wave's amplitude into
# the curve. The curve is what detrending removes, so a wave at
# splinePeriod(nyrs, f, 0.9) is nine-tenths removed from the indices, and
# one at splinePeriod(nyrs, f, 0.1) nine-tenths kept. By definition
# splinePeriod(nyrs, f, f) is nyrs.
# From the frequency response of the cubic smoothing spline (Cook and
# Peters 1981), which caps() reproduces: tests/test-helpers.R checks these
# numbers against what caps() does to sine waves.
splinePeriod <- function(nyrs, f = 0.5, gain = 0.5) {
  k <- function(w) 6 * (cos(w) - 1)^2 / (cos(w) + 2)
  H <- function(period) 1 / (1 + ((1 - f) / f) * k(2 * pi / period) / k(2 * pi / nyrs))
  stats::uniroot(function(p) H(p) - gain, c(2.0001, 1e6))$root
}

# ── curveSays ─────────────────────────────────────────────────────────────────
# What a row of the settings does to a series of n rings, in years: which
# variation the curve takes out of the indices and which it leaves in.
# Sentences for the Detrend panel, under the method's controls. For the
# spline methods the numbers are from splinePeriod(); for the others it
# says what kind of curve it is.
curveSays <- function(s, n) {
  yrs <- function(x) paste(format(signif(x, 2), big.mark = ",", scientific = FALSE), "years")
  if (s$method == "Spline") {
    ny   <- if (s$spl.nyrs <= 1) floor(s$spl.nyrs * n) else s$spl.nyrs
    if (ny < 3) return("A spline this flexible follows the series ring by ring: almost nothing is left in the indices.")
    slow <- splinePeriod(ny, s$spl.f, 0.9)
    fast <- splinePeriod(ny, s$spl.f, 0.1)
    c(paste0("This is a ", ny, "-year spline on a series of ", n, " rings."),
      paste0("Swings in growth slower than about ", yrs(slow), " are removed from the indices",
             if (slow > n) paste0(". That is longer than the series, so in effect only its",
                                  " overall level and slope are removed") , "."),
      paste0("Swings faster than about ", yrs(fast), " are kept",
             if (fast < 20) ": only the year-to-year and decade-to-decade variation is left",
             ". In between, part of each."))
  } else if (s$method == "AgeDepSpline") {
    first <- splinePeriod(s$ads.nyrs, 0.5, 0.1)
    last  <- splinePeriod(s$ads.nyrs + n - 1, 0.5, 0.1)
    c(paste0("At the first ring this acts like a ", s$ads.nyrs, "-year spline, and by the last like a ",
             s$ads.nyrs + n - 1, "-year spline."),
      paste0("So swings faster than about ", yrs(first), " are kept in the early rings, where growth",
             " changes fastest, and anything faster than about ", yrs(last), " by the end."))
  } else if (s$method %in% c("ModNegExp", "ModHugershoff")) {
    paste0("One smooth curve for the whole series",
           if (s$method == "ModNegExp") ", falling and levelling off" else ", rising, then falling and levelling off",
           ". It removes the decline in ring width as the tree ages and leaves every swing around it,",
           " slow or fast, in the indices.")
  } else if (s$method == "Friedman") {
    paste0("The smoother chooses how far to bend, ring by ring, so there is no single wavelength",
           " to give. Look at the curve: what it follows is removed.")
  } else character(0)
}

# ── detrendOne ────────────────────────────────────────────────────────────────
# Detrends one series (a column of an rwl: NA before and after the
# measurements) the way its row of the settings says. Never stops: a series
# that cannot be detrended comes back with `error` set and no indices.
#   rwi, curve — the indices and the fitted curve, the length of y
#   x          — the series the curve was fitted to (y, or y power transformed)
#   used       — the curve dplR used, which is not always the one asked for
#   dirtyDog   — dplR's flag: the fit asked for was not positive throughout
#   warnings   — dplR's warnings, as text
#   power      — the power of the transform, if one was applied
#   order      — the order of the AR model, for method "Ar"
#   n.zeros    — rings of zero width (set to 0.001 before dividing)
#   dropped    — the rings left out by the row's `first` and `last` (NA
#                elsewhere), for drawing; n.dropped counts them
# `years` are the years of y; they are needed only when the row trims the
# series.
detrendOne <- function(y, s, name = "", years = NULL) {
  out <- list(rwi = rep(NA_real_, length(y)), curve = rep(NA_real_, length(y)),
              x = y, used = NA_character_, dirtyDog = FALSE,
              warnings = character(0), error = NULL,
              power = NA_real_, order = NA_integer_, n.zeros = 0L,
              dropped = rep(NA_real_, length(y)), n.dropped = 0L)
  if (length(which(!is.na(y))) == 0) {
    out$error <- "This series has no measurements."
    return(out)
  }
  # use only part of the series: the rest is treated as not measured
  if (!is.na(s$first) || !is.na(s$last)) {
    if (is.null(years)) stop("detrendOne() needs 'years' to trim a series")
    keep <- (is.na(s$first) | years >= s$first) & (is.na(s$last) | years <= s$last)
    out$dropped   <- ifelse(keep, NA_real_, y)
    out$n.dropped <- sum(!is.na(out$dropped))
    y     <- ifelse(keep, y, NA_real_)
    out$x <- y
  }
  idx <- which(!is.na(y))
  if (length(idx) == 0) {
    out$error <- "No rings are left between the first and last year chosen for this series."
    return(out)
  }
  nGap <- sum(is.na(y[seq(idx[1], idx[length(idx)])]))
  if (nGap > 0) {
    out$error <- paste0("This series has ", nGap,
                        if (nGap == 1) " year" else " years",
                        " with no measurement inside it, and a curve cannot be",
                        " fitted across a gap. Fill the gaps on the Overview panel.")
    return(out)
  }
  warn <- character(0)
  res <- tryCatch(withCallingHandlers({
    x <- y
    if (s$powt != "none") {
      p <- powt(y, method = "cook", rescale = s$powt == "rescale", return.power = TRUE)
      x <- p$transformed.data
      out$power <- p$power
    }
    out$x <- x
    do.call(detrend.series,
            c(list(y = x, y.name = name, make.plot = FALSE, return.info = TRUE),
              detrendArgs(s)))
  }, warning = function(w) {
    warn <<- c(warn, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(e) e)
  if (inherits(res, "error")) {
    out$error <- paste0("dplR stopped with this message: ", conditionMessage(res),
                        if (!grepl("[.!?]$", conditionMessage(res))) ".")
    return(out)
  }
  info <- res$model.info[[1]]
  out$rwi      <- res$series
  # method "Ar" has no curve: detrend.series() returns one number
  out$curve    <- if (length(res$curves) == length(y)) res$curves else out$curve
  out$dirtyDog <- isTRUE(res$dirtyDog)
  out$warnings <- warn
  out$n.zeros  <- res$data.info$n.zeros
  if (!is.null(info$order)) out$order <- info$order
  # dplR 1.8.0 reports "Friedman" even when it fell back to the mean
  out$used <- if (s$method == "Friedman" && out$dirtyDog) "Mean" else info$method
  out
}

# What dplR calls the curve it fitted, as words
usedLabel <- function(used) {
  lbl <- c(Spline = "Smoothing spline", "Age-Dep Spline" = "Age-dependent spline",
           NegativeExponential = "Negative exponential", Hugershoff = "Hugershoff",
           Line = "Straight line", Mean = "Mean", Friedman = "Friedman's super smoother",
           Ar = "Autoregressive model")
  ifelse(used %in% names(lbl), lbl[used], used)
}

# The value of fit$used when dplR fitted what was asked for
usedIfAsked <- c(Spline = "Spline", AgeDepSpline = "Age-Dep Spline",
                 ModNegExp = "NegativeExponential", ModHugershoff = "Hugershoff",
                 Friedman = "Friedman", Mean = "Mean", Ar = "Ar")

# ── fitStatus ─────────────────────────────────────────────────────────────────
# What the user needs to know about one fit, and how much it matters:
#   error   — the series could not be detrended; it is left out of the indices
#   warning — the series was detrended, but not as asked or not sensibly;
#             it is marked in the series list
#   note    — worth knowing, nothing to fix
#   ok      — fitted as asked
# `text` holds one sentence per finding. Each says what it means for the
# indices, not only that it happened.
fitStatus <- function(fit, s) {
  if (!is.null(fit$error)) return(list(level = "error", text = fit$error))
  level <- "ok"
  text  <- character(0)
  add <- function(lv, ...) {
    text <<- c(text, paste0(...))
    level <<- c("ok", "note", "warning")[max(match(c(level, lv), c("ok", "note", "warning")))]
  }
  asked <- methodLabel(s$method)
  fellBack <- !identical(fit$used, unname(usedIfAsked[s$method]))

  if (s$method == "Ar") {
    if (fit$dirtyDog) {
      add("warning", "Some values from the autoregressive model were below zero and",
          " were set to zero, so those years have an index of zero. Index by",
          " subtraction to keep them.")
    }
    add("note", "An autoregressive model of order ", fit$order, " was fitted, so the first ",
        fit$order, if (fit$order == 1) " year has" else " years have", " no index.",
        " This method leaves only year-to-year variation: use it when that is what you want.")
  } else if (fit$dirtyDog) {
    add("warning", "The ", tolower(asked), " reached zero or went below it, and a series",
        " cannot be divided by a curve that is not positive. dplR used the series mean",
        " instead, so no trend was removed from this series. Try other settings, another",
        " method, or index by subtraction.")
  } else if (fellBack && fit$used == "Line") {
    add("note", "A ", if (s$method == "ModNegExp") "negative exponential" else "Hugershoff",
        " curve could not be fitted to this series (it does not rise and fall the way",
        " that model expects), so dplR fitted a straight line instead.")
  } else if (fellBack && fit$used == "Mean") {
    add("note", "Neither the ", if (s$method == "ModNegExp") "negative exponential" else "Hugershoff",
        " curve nor a falling straight line could be fitted, so dplR used the series mean:",
        " no trend was removed from this series.",
        if (!s$pos.slope) " Tick “Allow a positive slope” to let the line rise.")
  }

  # A curve that is positive but nearly zero: dividing by it blows the
  # indices up. Judged against the series' own level.
  if (!s$difference && s$method != "Ar" && !fit$dirtyDog) {
    cv  <- fit$curve[!is.na(fit$curve)]
    lvl <- mean(fit$x, na.rm = TRUE)
    if (length(cv) && min(cv) < 0.05 * lvl) {
      add("warning", "The curve comes close to zero (", signif(min(cv), 2), " at its lowest,",
          " against a series mean of ", signif(lvl, 2), "), so dividing by it inflates the",
          " indices there: the largest is ", signif(max(fit$rwi, na.rm = TRUE), 3), ".",
          " A stiffer curve, another method, or indexing by subtraction avoids this.")
    }
  }
  # A flexible curve chasing the last rings. At its end a curve has rings on
  # one side only and follows them closely, so the indices of the most
  # recent years, the ones compared with climate records, are set as much
  # by where the curve ends as by the rings. Flagged when the curve changes
  # by more than half over its last ten rings: about one default spline in
  # fifty on ITRDB series, one 32-year spline in eleven.
  if (!s$difference && s$method %in% c("Spline", "AgeDepSpline", "Friedman") && !fit$dirtyDog) {
    cv <- fit$curve[!is.na(fit$curve)]
    n  <- length(cv)
    if (n >= 30 && cv[n - 10] > 0) {
      ch <- cv[n] / cv[n - 10] - 1
      if (abs(ch) > 0.5) {
        add("warning", "Over its last 10 rings the curve ", if (ch > 0) "rises" else "falls", " by ",
            round(abs(ch) * 100), "%. A curve is least sure at its end, where it has rings on one",
            " side only, and a flexible one follows the last few rings closely. The indices of the",
            " most recent years then depend more on where the curve ends than on the rings, and",
            " those are usually the years compared with climate records. Try a stiffer curve, and",
            " check those years against the mean of the other series.")
      }
    }
  }
  if (any(grepl("greater than the length of series", fit$warnings))) {
    add("note", "The spline's rigidity is more years than the series has rings.")
  }
  if (s$powt != "none") {
    add("note", "Power transformed before fitting (power ", round(fit$power, 3), ").",
        if (!s$difference) paste0(" Power-transformed series are usually indexed by",
                                  " subtraction (Cook and Peters 1997)."))
    if (s$powt == "rescale" && !s$difference && any(fit$x < 0, na.rm = TRUE)) {
      add("warning", "Rescaling made some transformed values negative, and indexing by",
          " division gives those years a negative index. Index by subtraction instead.")
    }
  }
  if (fit$n.dropped > 0) {
    add("note", fit$n.dropped, if (fit$n.dropped == 1) " ring is" else " rings are",
        " left out (", paste(c(if (!is.na(s$first)) paste("before", s$first),
                              if (!is.na(s$last)) paste("after", s$last)), collapse = " and "),
        "): ", if (fit$n.dropped == 1) "it has" else "they have",
        " no index, and this series is not in the chronology for ",
        if (fit$n.dropped == 1) "that year." else "those years.")
  }
  if (fit$n.zeros > 0 && !s$difference) {
    add("note", fit$n.zeros, if (fit$n.zeros == 1) " ring of zero width was" else " rings of zero width were",
        " set to 0.001 before dividing, as detrend.series() does.")
  }
  list(level = level, text = text)
}

# ── buildRWI ──────────────────────────────────────────────────────────────────
# The indices for the whole file, from the fits (a list of detrendOne()
# results named by series). Series that could not be detrended are left
# out: an all-NA column would go on to be counted as a series by chron()
# and the statistics. Years no series has an index for are trimmed from the
# ends (method "Ar" drops the first years of a series). Returns NULL when
# no series could be detrended. rwiCode() writes the same steps as R source.
buildRWI <- function(dat, fits) {
  ok <- names(fits)[vapply(fits, function(f) is.null(f$error), logical(1))]
  if (length(ok) == 0) return(NULL)
  rwi <- dat
  for (s in names(fits)) rwi[[s]] <- if (s %in% ok) fits[[s]]$rwi else NULL
  rwi <- as.rwi(rwi)
  yrs <- rwiYears(rwi)
  if (!identical(yrs, range(as.numeric(rownames(rwi))))) rwi <- window(rwi, yrs[1], yrs[2])
  rwi
}

# First and last year with an index in any series
rwiYears <- function(rwi) {
  range(as.numeric(rownames(rwi))[rowSums(!is.na(rwi)) > 0])
}

# ══════════════════════════════════════════════════════════════════════════════
# TREES AND CHRONOLOGIES
# ══════════════════════════════════════════════════════════════════════════════

# ── treeIds ───────────────────────────────────────────────────────────────────
# Which series are cores of the same tree, from the series names. rbar and
# EPS depend on it: two cores of one tree agree more than two trees do, and
# counting them as two trees overstates how well the chronology stands for
# the stand.
#   mode "auto"     — dplR's autoread.ids() works the scheme out
#        "position" — read.ids() with `stc`: how many characters of each
#                     name are the site, the tree and the core
#        "none"     — every series is its own tree
# Returns list(ids, warn, error, code): the ids data.frame (tree, core; NULL
# when every series is a tree or the names could not be read), dplR's
# warnings and error as text, and the R source that makes `ids` from `dat`.
# Never stops.
treeIds <- function(dat, mode = c("auto", "position", "none"), stc = c(3, 2, 1)) {
  mode <- match.arg(mode)
  if (mode == "none") return(list(ids = NULL, warn = character(0), error = NULL, code = NULL))
  warn <- character(0)
  res <- tryCatch(withCallingHandlers(suppressMessages(
    if (mode == "auto") autoread.ids(dat) else read.ids(dat, stc = stc)),
    warning = function(w) {
      warn <<- c(warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = function(e) e)
  if (inherits(res, "error")) {
    return(list(ids = NULL, warn = warn, error = conditionMessage(res), code = NULL))
  }
  # rbar and EPS compare trees with each other, so one tree is no use, and
  # in a file of many series it is almost certainly a misreading of the names
  if (ncol(dat) > 1 && length(unique(res$tree)) < 2) {
    return(list(ids = NULL, warn = warn, code = NULL,
                error = paste0("read this way, all ", ncol(dat), " series are cores of one tree,",
                               " and rbar and EPS need at least two trees")))
  }
  list(ids = res, warn = warn, error = NULL,
       code = if (mode == "auto") "ids <- autoread.ids(dat)" else
         paste0("ids <- read.ids(dat, stc = c(", paste(stc, collapse = ", "), "))"))
}

# ── SSS ───────────────────────────────────────────────────────────────────────
# The subsample signal strength for each year (dplR's sss(), after Wigley
# et al. 1984): how well the chronology built from the trees that were
# alive that year stands for the one built from all of them. It answers
# the question running EPS is often used for: how far back do enough trees
# reach for the chronology to stand for the whole sample? Wigley et al.
# meant SSS for that, to estimate what a reconstruction loses as the trees
# thin out back in time, and their rough guide of 0.85 was for SSS, not
# EPS (Buras 2017). Neither says whether the chronology carries a climate
# signal: that is for calibration and verification against climate data.
# sssOf() returns the values named by year, or NULL when there are fewer
# than two series or dplR stops. Trees are counted by `ids`.
sssOf <- function(rwi, ids) {
  if (is.null(rwi) || ncol(rwi) < 2) return(NULL)
  res <- tryCatch(suppressWarnings(suppressMessages(sss(rwi, ids = idsFor(ids, rwi)))),
                  error = function(e) NULL)
  if (is.null(res) || length(res) != nrow(rwi)) return(NULL)
  stats::setNames(as.numeric(res), rownames(rwi))
}

# The cut-off for SSS, and for the line drawn at that EPS. Arbitrary: a
# convention and no more. Wherever the app shows the number it says so
# (cutNote in sight, epsNote in the longer text).
signalCut <- 0.85
cutNote <- paste(signalCut, "is an arbitrary cut-off")
epsNote <- paste(
  "The 0.85 is arbitrary. No test or theory sets it, and a chronology is not",
  "sound at 0.85 and unsound at 0.84. Wigley et al. (1984) offered it as a",
  "rough guide for SSS, and gave no threshold for EPS (Buras 2017). Neither",
  "number says whether a chronology suits a climate reconstruction: read them",
  "as matters of degree.")

# The statistics describe the indices, not the chronology drawn from them:
# said under the statistics and in the report.
statsOfNote <- paste(
  "These describe the detrended indices, not the chronology: they are the same",
  "whichever kind of chronology is chosen, and whether or not its mean is robust.")

# The first year from which SSS stays at or above `cut` to the end of the
# chronology: where the chronology becomes reliable, by that cut-off. NA
# when it never does (or there is no SSS).
sssFrom <- function(x, cut = signalCut) {
  if (is.null(x) || all(is.na(x))) return(NA_real_)
  yrs   <- as.numeric(names(x))
  below <- which(is.na(x) | x < cut)
  if (length(below) == 0) return(yrs[1])
  if (max(below) == length(x)) return(NA_real_)
  yrs[max(below) + 1]
}

# ── signalThroughTime ─────────────────────────────────────────────────────────
# rbar and EPS in windows of `win` years, each overlapping the last by half
# (dplR's rwi.stats.running()), counting trees by `ids`. Returns the
# data.frame, or one sentence saying why there is none: never stops. With
# data too short for two windows rwi.stats.running() returns a single row
# for the whole span, which is not "through time" and is reported as such.
signalThroughTime <- function(rwi, ids, win) {
  if (is.null(rwi) || ncol(rwi) < 2) return("Statistics through time need two or more detrended series.")
  res <- tryCatch(suppressWarnings(suppressMessages(
    rwi.stats.running(rwi, ids = idsFor(ids, rwi), window.length = win,
                      window.overlap = floor(win / 2)))),
    error = function(e) paste0("dplR stopped with this message: ", conditionMessage(e), "."))
  if (is.character(res)) return(res)
  if (!all(c("start.year", "mid.year", "end.year") %in% names(res)) || nrow(res) < 2) {
    return(paste0("The indices span ", nrow(rwi), " years, too few for more than one window of ",
                  win, " years. Shorten the window to see the signal through time."))
  }
  res
}

# The rows of `ids` for the series in `rwi`, in its order (series that could
# not be detrended are not in the indices)
idsFor <- function(ids, rwi) {
  if (is.null(ids) || is.null(rwi)) return(NULL)
  out <- ids[names(rwi), , drop = FALSE]
  # with fewer than two trees left, every series counts as a tree
  if (length(unique(out$tree)) < 2) NULL else out
}

# "34 series from 21 trees"
treeCount <- function(ids) {
  n <- nrow(ids)
  k <- length(unique(ids$tree))
  paste0(n, " series from ", k, if (k == 1) " tree" else " trees")
}

# ── Chronologies ──────────────────────────────────────────────────────────────
# The kinds of chronology offered, by the name of the column dplR returns:
#   std — the mean of the indices each year: chron()
#   res — the same after each series is prewhitened (its autocorrelation
#         removed): chron(prewhiten = TRUE)
#   ars — ARSTAN's: the residual chronology with the autocorrelation the
#         series share put back: chron.ars()
#   vsc — the mean, rescaled so that its variance does not depend on how
#         many series there are in each year: chron.stabilized()
# The mean is Tukey's biweight robust mean unless biweight is FALSE.
chronTypes <- c("Standard: the mean of the indices"           = "std",
                "Residual: series prewhitened first"          = "res",
                "ARSTAN: pooled autocorrelation put back"     = "ars",
                "Variance stabilised"                         = "vsc")

# buildChron() returns a data.frame with the chronology in its first column
# (named as above) and samp.depth; chronCode() is the same call as R source
# for the report. Errors from dplR are passed on (a window too long for the
# chronology, too few series to prewhiten).
buildChron <- function(rwi, type = "std", biweight = TRUE, win = 51) {
  crn <- switch(type,
    std = chron(rwi, biweight = biweight),
    res = chron(rwi, biweight = biweight, prewhiten = TRUE),
    ars = chron.ars(rwi, biweight = biweight, verbose = FALSE),
    vsc = chron.stabilized(rwi, winLength = win, biweight = biweight),
    stop("unknown kind of chronology: ", type))
  out <- data.frame(crn[[type]], samp.depth = crn$samp.depth, row.names = rownames(crn))
  names(out)[1] <- type
  out[[1]][is.nan(out[[1]])] <- NA
  out
}

chronCode <- function(type = "std", biweight = TRUE, win = 51) {
  bw <- if (biweight) "" else ", biweight = FALSE"
  switch(type,
    std = c(paste0("crn <- chron(rwi", bw, ")"), "plot(crn)"),
    res = c(paste0("crn <- chron(rwi", bw, ", prewhiten = TRUE)"),
            "# the residual chronology is crn$res", "plot(crn)"),
    ars = c(paste0("crn <- chron.ars(rwi", bw, ", verbose = FALSE)"),
            "# the ARSTAN chronology is crn$ars", "plot(crn)"),
    vsc = c(paste0("crn <- chron.stabilized(rwi, winLength = ", win, bw, ")"), "plot(crn)"))
}

chronLabel <- function(type, biweight = TRUE) {
  paste0(c(std = "Standard chronology", res = "Residual chronology",
           ars = "ARSTAN chronology", vsc = "Variance-stabilised chronology")[[type]],
         if (biweight) " (robust mean)" else " (arithmetic mean)")
}

# ══════════════════════════════════════════════════════════════════════════════
# R CODE FOR THE REPORT
# ══════════════════════════════════════════════════════════════════════════════

# R source for one value: "Spline", 0.67, TRUE
valCode <- function(v) {
  if (is.character(v)) encodeString(v, quote = '"') else as.character(v)
}

# R source for named arguments, e.g. 'method = "Spline", nyrs = 0.67'
argCode <- function(args) {
  paste0(names(args), " = ", vapply(args, valCode, ""), collapse = ", ")
}

# The detrend.series() call for one series
seriesCode <- function(name, s) {
  nm <- valCode(name)
  y  <- paste0("dat[[", nm, "]]")
  # only part of the series: the rest set to NA (yrs is time(dat))
  if (!is.na(s$first) || !is.na(s$last)) {
    y <- paste0("ifelse(", paste(c(if (!is.na(s$first)) paste("yrs >=", s$first),
                                   if (!is.na(s$last)) paste("yrs <=", s$last)), collapse = " & "),
                ", ", y, ", NA)")
  }
  if (s$powt != "none") {
    y <- paste0("powt(", y, ', method = "cook"',
                if (s$powt == "rescale") ", rescale = TRUE", ")")
  }
  paste0("rwi[[", nm, "]] <- detrend.series(", y, ", y.name = ", nm, ", ",
         argCode(detrendArgs(s)), ", make.plot = FALSE)")
}

# ── detrendCode ───────────────────────────────────────────────────────────────
# Everything needed to rebuild the app's indices in plain R.
#   fname    — the file's name
#   example  — FALSE for a file the user loaded. For example data, the R
#              source that makes `dat` from dplR (examples, in guide.R);
#              TRUE means fname is itself the name of a dplR data set
#   fills    — the gap fills made, in order (see fillCode())
#   settings — the settings table
#   fits     — the app's fits, to say which series are left out and whether
#              the years need trimming
#   years    — the years of the data (the row names of the rwl)
#   ids.code — the R source that makes the tree ids (treeIds()$code), or NULL
#              when every series counts as a tree
#   crn.code — the R source that builds the chronology (chronCode())
detrendCode <- function(fname, example, fills, settings, fits, years,
                        ids.code = NULL, crn.code = chronCode()) {
  failed <- names(fits)[!vapply(fits, function(f) is.null(f$error), logical(1))]
  ok     <- settings[!settings$series %in% failed, ]
  calls  <- vapply(seq_len(nrow(ok)), function(i) seriesCode(ok$series[i], ok[i, ]), "")
  trim <- NULL
  if (nrow(ok) > 0) {
    has <- Reduce(`|`, lapply(fits[ok$series], function(f) !is.na(f$rwi)))
    if (!has[1] || !has[length(has)]) {
      trim <- c("# no series has an index in the first or last years: drop them",
                paste0("rwi <- window(rwi, ", min(years[has]), ", ", max(years[has]), ")"))
    }
  }
  c("library(dplR)",
    if (is.character(example)) example else
      if (isTRUE(example)) c(paste0("data(", fname, ")"), paste0("dat <- ", fname)) else
      paste0("dat <- read.rwl(", valCode(fname), ")"),
    if (length(fills)) c("", "# years with no measurement inside a series, filled",
                         vapply(fills, fillCode, "")),
    "",
    "# one call for each series, with the settings chosen in iDetrend",
    "rwi <- dat",
    if (any(!is.na(ok$first) | !is.na(ok$last))) c(
      "# some series use only part of their rings: the rest are set to NA",
      "yrs <- time(dat)"),
    calls,
    if (length(failed)) c("",
      "# could not be detrended (see the report), so left out",
      paste0("rwi[[", vapply(failed, valCode, ""), "]] <- NULL")),
    "rwi <- as.rwi(rwi)",
    trim,
    "",
    if (!is.null(ids.code)) c(
      "# which series are cores of the same tree, read from their names",
      ids.code,
      if (length(failed)) "ids <- ids[names(rwi), ]"),
    "",
    "# the indices, and a chronology built from them",
    if (is.null(ids.code)) "summary(rwi)" else "summary(rwi, ids = ids)",
    crn.code,
    "",
    "# subsample signal strength, year by year: how far back enough trees reach",
    if (is.null(ids.code)) "signal <- sss(rwi)" else "signal <- sss(rwi, ids = ids)")
}

# ══════════════════════════════════════════════════════════════════════════════
# TABLES AND FILES
# ══════════════════════════════════════════════════════════════════════════════

# ── settingsTable ─────────────────────────────────────────────────────────────
# One row per series for the Results panel and the report: its span, what
# was asked for, what dplR used, and whether it needs a look.
settingsTable <- function(dat, settings, fits) {
  yrs <- as.numeric(rownames(dat))
  st  <- lapply(seq_len(nrow(settings)), function(i) fitStatus(fits[[settings$series[i]]], settings[i, ]))
  rows <- lapply(seq_len(nrow(settings)), function(i) {
    s   <- settings[i, ]
    fit <- fits[[s$series]]
    # the rings used: after any trimming
    idx <- which(!is.na(fit$x))
    data.frame(
      Series = s$series,
      First  = if (length(idx)) yrs[idx[1]] else NA,
      Last   = if (length(idx)) yrs[idx[length(idx)]] else NA,
      Rings  = length(idx),
      Method = methodLabel(s$method),
      Settings = paramText(s),
      "Curve used" = if (is.null(fit$error)) unname(usedLabel(fit$used)) else "none",
      Transform = if (s$powt == "none") "" else
        paste0(if (s$powt == "rescale") "power, rescaled" else "power",
               if (!is.na(fit$power)) paste0(" (", round(fit$power, 2), ")")),
      "Index by" = if (s$difference) "subtraction" else "division",
      Check = c(ok = "", note = "", warning = "look", error = "not detrended")[[st[[i]]$level]],
      check.names = FALSE, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# ── seriesFit ─────────────────────────────────────────────────────────────────
# How each series' indices sit with the rest, for the Results table and the
# report, from summary() of the indices (dplR >= 1.8.0):
#   cor, p — correlation with the mean of the other series
#            (interseries.cor()); a series that does not correlate may be
#            misdated, or detrended in a way that hides what it shares
#   trend  — the slope of a straight line through the indices, as change
#            per century: trend the curve left in (or put in)
# `sm` is summary(rwi). Series not in the indices get NA.
seriesFit <- function(rwi, sm, series) {
  yrs <- as.numeric(rownames(rwi))
  trend <- vapply(names(rwi), function(s) {
    ok <- !is.na(rwi[[s]])
    if (sum(ok) < 3) return(NA_real_)
    unname(stats::coef(stats::lm(rwi[[s]][ok] ~ yrs[ok]))[2]) * 100
  }, numeric(1))
  i <- match(series, sm$series$series)
  data.frame(series = series, cor = sm$series$cor[i], p = sm$series$p[i],
             trend = unname(trend[match(series, names(rwi))]),
             stringsAsFactors = FALSE)
}

# ── collectionWarnings ────────────────────────────────────────────────────────
# Things wrong with the indices taken together, for the Results panel and
# the report. Indices by division centre on 1 and by subtraction on 0, and a
# power transform changes the scale, so a chronology that averages a mix of
# them means nothing.
collectionWarnings <- function(settings, fits) {
  failed <- names(fits)[!vapply(fits, function(f) is.null(f$error), logical(1))]
  s <- settings[!settings$series %in% failed, ]
  few <- function(x) {
    if (length(x) <= 6) paste(x, collapse = ", ") else
      paste0(paste(x[1:6], collapse = ", "), " and ", length(x) - 6, " more")
  }
  c(if (length(failed)) paste0(
      length(failed), if (length(failed) == 1) " series could" else " series could",
      " not be detrended and ", if (length(failed) == 1) "is" else "are",
      " left out of the indices, the chronology and the downloads: ", few(failed), "."),
    if (nrow(s) && any(s$difference) && !all(s$difference)) paste0(
      sum(s$difference), " of ", nrow(s), " series are indexed by subtraction and the",
      " rest by division. Indices by subtraction centre on 0 and by division on 1, so",
      " a chronology averaging the two is not meaningful. Use one for every series.",
      " By subtraction: ", few(s$series[s$difference]), "."),
    if (nrow(s) && any(s$powt != "none") && !all(s$powt != "none")) paste0(
      sum(s$powt != "none"), " of ", nrow(s), " series are power transformed and the",
      " rest are not, so their indices are on different scales. Transform every series",
      " or none. Transformed: ", few(s$series[s$powt != "none"]), "."))
}

# ── seriesOrder ───────────────────────────────────────────────────────────────
# The order to walk the series in on the Detrend panel.
#   "file"     — as in the file
#   "check"    — those that need a look first: not detrended, then marked
#                to look at, then the rest; file order within each
#   "longest", "shortest" — by number of rings
# `levels` are the fitStatus() levels and `rings` the ring counts, both
# named by series.
seriesOrder <- function(series, how = "file", levels = NULL, rings = NULL) {
  switch(how,
    check    = series[order(match(levels[series], c("error", "warning", "note", "ok")))],
    longest  = series[order(-rings[series])],
    shortest = series[order(rings[series])],
    series)
}

# ── readSettings ──────────────────────────────────────────────────────────────
# Reads a settings file saved from the app (a csv of the settings table,
# with the notes) and lines it up with the series of the loaded file.
# Returns list(settings, notes, matched, unknown, missing) or list(error).
#   matched — series in both; their rows replace the current ones
#   unknown — series in the settings file that the data do not have
#   missing — series in the data that the settings file does not have;
#             they keep the settings they have now
# A row with a value detrend.series() would not accept stops the whole
# read: half-applied settings are worse than none.
readSettings <- function(path, current) {
  # everything as text: a series called 0012 must not become 12
  x <- tryCatch(utils::read.csv(path, colClasses = "character", check.names = FALSE),
                error = function(e) e)
  if (inherits(x, "error")) return(list(error = paste("The file could not be read as csv:", conditionMessage(x))))
  checkSettings(x, current)
}

# The checks behind readSettings(), on a data.frame of settings whose
# columns are all text. Also used for the copy kept in the browser
# (stateFromJSON()).
checkSettings <- function(x, current) {
  # first and last came later: a file saved without them means "every ring"
  for (f in c("first", "last")) if (!f %in% names(x)) x[[f]] <- ""
  need <- c("series", settingFields)
  if (!all(need %in% names(x))) {
    return(list(error = paste0("This is not an iDetrend settings file: it has no column called ",
                               paste(setdiff(need, names(x)), collapse = ", "), ".")))
  }
  x$series <- trimws(x$series)
  x[is.na(x)] <- ""
  bad <- function(what, rows) list(error = paste0(
    "The settings file has ", what, " (series ", paste(utils::head(x$series[rows], 5), collapse = ", "),
    "). Nothing was changed."))
  if (anyDuplicated(x$series)) return(bad("a series listed twice", duplicated(x$series)))
  lgl <- function(v) if (is.logical(v)) v else toupper(trimws(as.character(v))) %in% c("TRUE", "T", "1")
  num <- function(v) suppressWarnings(as.numeric(v))
  x$pos.slope  <- lgl(x$pos.slope)
  x$difference <- lgl(x$difference)
  for (f in c("spl.nyrs", "spl.f", "ads.nyrs", "span", "bass", "first", "last")) x[[f]] <- num(x[[f]])
  for (f in c("method", "constrain", "powt")) x[[f]] <- trimws(as.character(x[[f]]))
  if (any(r <- !x$method %in% methodChoices)) return(bad("a method iDetrend does not know", r))
  if (any(r <- !x$constrain %in% c("never", "when.fail", "always"))) return(bad("an unknown value of constrain", r))
  if (any(r <- !x$powt %in% powtChoices)) return(bad("an unknown value of powt", r))
  if (any(r <- is.na(x$spl.nyrs) | x$spl.nyrs <= 0)) return(bad("a spline rigidity that is not a number above 0", r))
  if (any(r <- is.na(x$spl.f) | x$spl.f <= 0 | x$spl.f >= 1)) return(bad("a spline f that is not between 0 and 1", r))
  if (any(r <- is.na(x$ads.nyrs) | x$ads.nyrs <= 1)) return(bad("an age-dependent spline rigidity that is not above 1", r))
  if (any(r <- is.na(x$bass) | x$bass < 0 | x$bass > 10)) return(bad("a bass that is not between 0 and 10", r))
  if (any(r <- !is.na(x$span) & (x$span <= 0 | x$span > 1))) return(bad("a span that is not between 0 and 1", r))
  if (any(r <- !is.na(x$first) & !is.na(x$last) & x$first > x$last)) return(bad("a first year after the last year", r))

  matched <- intersect(current$series, x$series)
  out <- current
  out[match(matched, out$series), settingFields] <- x[match(matched, x$series), settingFields]
  notes <- if ("note" %in% names(x)) {
    n <- as.character(x$note[match(matched, x$series)])
    n[is.na(n)] <- ""
    stats::setNames(as.list(n), matched)
  } else list()
  list(settings = out, notes = notes[nzchar(unlist(notes))], matched = matched,
       unknown = setdiff(x$series, current$series),
       missing = setdiff(current$series, x$series))
}

# ── The copy kept in the browser ──────────────────────────────────────────────
# The app keeps the work in progress in the browser's local storage, so a
# session that times out, or a tab closed by mistake, does not lose it.
# fileKey() names the copy: the file's name and a fingerprint of its series
# names and years, so settings are only ever offered back for the file they
# were made on. stateToJSON() / stateFromJSON() write and read it; reading
# goes through the same checks as a settings file.
fileKey <- function(name, dat) {
  txt <- paste(c(names(dat), range(as.numeric(rownames(dat))), ncol(dat)), collapse = "|")
  v   <- utf8ToInt(txt)
  paste0("iDetrend:", name, ":", sum(v * (seq_along(v) %% 9973 + 1)) %% 2147483647)
}

stateToJSON <- function(settings, notes, seen, fills, goal = NULL) {
  S <- settings
  S$note <- vapply(S$series, function(s) { x <- notes[[s]]; if (is.null(x)) "" else x }, "")
  S$seen <- S$series %in% seen
  as.character(jsonlite::toJSON(list(version = 1, fills = unlist(fills), goal = goal, settings = S),
                                dataframe = "columns", na = "null", auto_unbox = FALSE))
}

# Returns list(settings, notes, seen, fills, matched) or list(error).
stateFromJSON <- function(txt, current) {
  x <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(x) || is.null(x$settings) || is.null(x$settings$series)) {
    return(list(error = "The copy kept in this browser could not be read."))
  }
  col <- function(v) vapply(v, function(z) if (is.null(z)) "" else as.character(z), "")
  df  <- as.data.frame(lapply(x$settings, col), stringsAsFactors = FALSE, check.names = FALSE)
  res <- checkSettings(df, current)
  if (!is.null(res$error)) return(res)
  fills <- as.character(unlist(x$fills))
  if (!all(fills %in% c("0", "Linear", "Mean"))) {
    return(list(error = "The copy kept in this browser could not be read."))
  }
  res$seen  <- intersect(df$series[toupper(df$seen) == "TRUE"], current$series)
  res$fills <- as.list(fills)
  res$goal  <- if (length(x$goal)) as.character(unlist(x$goal))[1]
  res
}

# ── rwiSheet ──────────────────────────────────────────────────────────────────
# The indices as a data.frame to write as csv: a Year column, then one
# column per series, to four decimals (ring widths are measured to two or
# three, so nothing real is lost). read.rwl() reads the file back.
rwiSheet <- function(rwi) {
  data.frame(Year = as.numeric(rownames(rwi)),
             round(as.data.frame(unclass(rwi), check.names = FALSE), 4),
             check.names = FALSE)
}

# ── scriptLines ───────────────────────────────────────────────────────────────
# The R code (detrendCode()) as a script to save: the same lines the report
# prints, under a header saying what made them.
scriptLines <- function(code, fname, version) {
  c(paste0("# Detrending of ", fname),
    paste0("# Written by iDetrend ", version, " with dplR ", utils::packageVersion("dplR"),
           " on ", Sys.Date(), "."),
    "# Run it in the folder that holds the data file. It rebuilds the indices",
    "# (rwi) and the chronology (crn) made in the app.",
    "", code)
}

# ── crnForTucson ──────────────────────────────────────────────────────────────
# A chronology from buildChron() in the shape write.crn() wants. The Tucson
# format names a chronology by a site code of up to six characters, taken
# from the column name: the data file's name, letters and digits only, is
# used for it.
crnForTucson <- function(crn, fname) {
  id <- toupper(gsub("[^[:alnum:]]", "", tools::file_path_sans_ext(if (is.null(fname)) "CRN" else fname)))
  names(crn)[1] <- substr(if (nzchar(id)) id else "CRN", 1, 6)
  crn
}

# ── File names ────────────────────────────────────────────────────────────────
# "nm046-indices-2026-10-06.csv": the data file's name, what this is, the date
downloadName <- function(file, kind, ext) {
  base <- if (is.null(file)) "iDetrend" else tools::file_path_sans_ext(file)
  paste0(base, "-", kind, "-", Sys.Date(), ".", ext)
}

# ── checkLines ────────────────────────────────────────────────────────────────
# rwl.check() findings (rows of as.data.frame(rwl.check(...))) that matter
# before detrending, as sentences. Gaps are left out: the app has its own
# card for them, with the fill controls.
checkLines <- function(f) {
  f <- f[f$severity %in% c("error", "warning") & f$check != "RWL_INTERNAL_NA", , drop = FALSE]
  if (nrow(f) == 0) return(character(0))
  msg <- paste0(toupper(substr(f$message, 1, 1)), substring(f$message, 2),
                ifelse(grepl("[.!?]$", f$message), "", "."))
  ifelse(is.na(f$series) | mapply(grepl, f$series, msg, fixed = TRUE),
         msg, paste0(f$series, ": ", msg))
}
