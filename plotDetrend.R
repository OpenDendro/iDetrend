## The plot of one series being detrended. Sourced by server.R and by the
## report, which draws the same figure for every series. Base graphics, no
## packages beyond dplR's own dependencies.

# Colours: the series in near-black, the curve in use in red, and curves
# shown for comparison in colours that stay apart from both.
detrendCols <- list(series = "grey25", curve = "#B22222", ref = "grey50",
                    others = "#BBD9EE", sss = "#1F78B4",
                    compare = c("#1F78B4", "#E08A00", "#33A02C", "#7B3294",
                                "#00A0A0", "#8C6D31"))

# ── plotDetrend ───────────────────────────────────────────────────────────────
# Two panels sharing the year axis. Above: the series the curve is fitted to
# (ring widths, or power-transformed widths) and the curve. Below: the
# indices, with a line at 1 (division) or 0 (subtraction).
#   years   — the years of the rwl
#   fit     — a detrendOne() result
#   name    — the series' name, for the title
#   s       — its row of the settings
#   compare — optional named list of detrendOne() results for other methods,
#             whose curves are drawn thin in the upper panel
#   sub     — a line under the title (what dplR used)
#   others  — optional: the mean of the other series' indices each year (the
#             length of years), drawn behind this series' indices, with
#             others.n the number of series in it. A swing this series
#             shares with the others is signal; one it has alone is not.
# The axis on top counts rings from the first one measured, since rigidity
# is given in years of the series, not calendar years.
plotDetrend <- function(years, fit, name, s, compare = list(), sub = "",
                        others = NULL, others.n = 0) {
  has  <- which(!is.na(fit$x))
  # rings left out by the series' first and last year, drawn pale beside
  # the rings used (only on the scale of the widths: not after a transform)
  drop <- if (!is.null(fit$dropped) && s$powt == "none") which(!is.na(fit$dropped)) else integer(0)
  if (length(has) == 0) {
    plot.new()
    text(0.5, 0.5, "No measurements to plot.")
    return(invisible(NULL))
  }
  span <- seq(has[1], has[length(has)])
  yr   <- years[span]
  x    <- fit$x[span]
  xlim <- range(years[c(has, drop)])
  failed <- !is.null(fit$error)
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  layout(matrix(1:2, ncol = 1), heights = c(1.15, 1))
  cex <- 1.1

  # ── upper panel: the series and the curve ──
  par(mar = c(0.6, 4.4, 4.6, 1), mgp = c(2.6, 0.7, 0), tcl = -0.3, las = 1,
      cex = cex, bty = "l")
  curves <- c(list(fit$curve[span]), lapply(compare, function(f) f$curve[span]))
  ylim <- range(c(x, unlist(curves)), na.rm = TRUE)
  # with comparison curves there is a legend: leave it room above the data
  if (length(compare)) ylim[2] <- ylim[2] + diff(ylim) * 0.09 * (length(compare) + 1)
  if (length(drop)) ylim <- range(c(ylim, fit$dropped[drop]))
  plot(yr, x, type = "n", xaxt = "n", xlim = xlim, ylim = ylim, xlab = "",
       ylab = if (s$powt == "none") "Ring width" else "Power-transformed width")
  grid(col = "grey90", lty = 1)
  if (length(drop)) {
    # joined to the rings used, so the series reads as one
    for (side in list(drop[drop < has[1]], drop[drop > has[length(has)]])) {
      if (length(side)) {
        j <- sort(c(side, if (side[1] < has[1]) has[1] else has[length(has)]))
        lines(years[j], ifelse(is.na(fit$dropped[j]), fit$x[j], fit$dropped[j]), col = "grey75", lwd = 1, lty = 3)
      }
    }
    abline(v = c(if (any(drop < has[1])) yr[1], if (any(drop > has[length(has)])) yr[length(yr)]),
           col = "grey60", lty = 3)
  }
  lines(yr, x, col = detrendCols$series, lwd = 1)
  for (i in seq_along(compare)) {
    lines(yr, compare[[i]]$curve[span], lwd = 1.5,
          col = detrendCols$compare[(i - 1) %% length(detrendCols$compare) + 1])
  }
  if (!failed && s$method != "Ar") {
    lines(yr, fit$curve[span], col = detrendCols$curve, lwd = 2.5)
  }
  # ring number along the top
  at <- pretty(seq_along(yr))
  at <- at[at >= 1 & at <= length(yr)]
  axis(3, at = yr[1] + at - 1, labels = at, col.axis = "grey40", cex.axis = 0.85)
  mtext(if (length(drop)) "Ring number, of the rings used" else "Ring number",
        side = 3, line = 1.7, cex = 0.85 * cex, col = "grey40")
  mtext(name, side = 3, line = 3.2, adj = 0, font = 2, cex = 1.15 * cex)
  if (nzchar(sub)) mtext(sub, side = 3, line = 3.2, adj = 1, cex = 0.9 * cex, col = "grey30")
  if (length(compare)) {
    own <- !failed && s$method != "Ar"
    legend("topright", inset = 0.01, bty = "n", cex = 0.85,
           legend = c(if (own) "Curve in use", names(compare)),
           lwd = c(if (own) 2.5, rep(1.5, length(compare))),
           col = c(if (own) detrendCols$curve,
                   detrendCols$compare[(seq_along(compare) - 1) %% length(detrendCols$compare) + 1]))
  }

  # ── lower panel: the indices ──
  par(mar = c(3.4, 4.4, 0.6, 1))
  if (failed) {
    plot(yr, x, type = "n", xlim = xlim, yaxt = "n", xlab = "Year", ylab = "Index")
    text(mean(range(yr)), mean(range(x, na.rm = TRUE)), "No indices: see the message below.",
         col = detrendCols$curve)
    return(invisible(NULL))
  }
  rwi <- fit$rwi[span]
  ref <- if (s$difference) 0 else 1
  ylim <- range(c(rwi, ref), na.rm = TRUE)
  # room for the legend above the data
  if (!is.null(others)) ylim[2] <- ylim[2] + diff(ylim) * 0.2
  plot(yr, rwi, type = "n", xlim = xlim, ylim = ylim,
       xlab = "Year", ylab = if (s$difference) "Index (width − curve)" else "Index (width / curve)")
  grid(col = "grey90", lty = 1)
  abline(h = ref, col = detrendCols$ref, lty = 2)
  if (!is.null(others) && any(!is.na(others[span]))) {
    # a filled shape from the reference line, not a second line: two
    # jagged lines of equal weight cannot be told apart
    o  <- others[span]
    ok <- !is.na(o)
    for (r in split(which(ok), cumsum(c(1, diff(which(ok)) != 1)))) {
      polygon(c(yr[r], rev(yr[r])), c(o[r], rep(ref, length(r))),
              col = detrendCols$others, border = NA)
    }
    abline(h = ref, col = detrendCols$ref, lty = 2)
    legend("topright", inset = 0.01, bty = "n", cex = 0.85,
           lwd = c(1, NA), pch = c(NA, 15), pt.cex = 1.6,
           col = c(detrendCols$series, detrendCols$others),
           legend = c("This series", paste0("Mean of the other ", others.n, " series")))
  }
  lines(yr, rwi, col = detrendCols$series, lwd = 1)
  invisible(NULL)
}

# What the comparison line in the chronology and signal plots is: the same
# data detrended by dplR with no choices made, detrend(rwl).
baselineLabel <- "dplR's default detrending, detrend(rwl)"

# ── plotChron ─────────────────────────────────────────────────────────────────
# The chronology (the mean of the indices each year) over its sample depth,
# with an optional second chronology behind it for comparison: the one the
# same data give when every series is left at dplR's default.
#   crn, base — results of chron(); base may be NULL
#   sss.from  — optional: the year from which SSS stays at or above the
#               cut-off (sssFrom()). The years before it are shaded and the
#               year is marked: the chronology there rests on too few trees
#               to stand for the whole sample.
plotChron <- function(crn, base = NULL, labels = c("With your settings", baselineLabel),
                      ylab = "Index", sss.from = NULL) {
  yrs <- as.numeric(rownames(crn))
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mar = c(3.4, 4.4, 1, 4.4), mgp = c(2.6, 0.7, 0), tcl = -0.3, las = 1,
      cex = 1.1, bty = "u")
  # sample depth behind, on the right-hand axis
  plot(yrs, crn$samp.depth, type = "n", axes = FALSE, xlab = "", ylab = "",
       ylim = c(0, max(crn$samp.depth) * 1.02), yaxs = "i")
  polygon(c(yrs, rev(yrs)), c(crn$samp.depth, rep(0, length(yrs))),
          col = "grey92", border = NA)
  axis(4, col.axis = "grey40")
  mtext("Series", side = 4, line = 2.8, las = 0, cex = 1.1, col = "grey40")
  par(new = TRUE)
  # the chronology is the first column, whatever its kind (std, res, ars, vsc)
  ylim <- range(c(crn[[1]], if (!is.null(base)) base[[1]]), na.rm = TRUE)
  plot(yrs, crn[[1]], type = "n", ylim = ylim, xlab = "Year", ylab = ylab)
  if (!is.null(base)) {
    lines(as.numeric(rownames(base)), base[[1]], col = detrendCols$compare[2], lwd = 1.2)
  }
  lines(yrs, crn[[1]], col = detrendCols$series, lwd = 1.2)
  if (!is.null(sss.from) && !is.na(sss.from) && sss.from > yrs[1]) {
    u <- par("usr")
    rect(u[1], u[3], sss.from, u[4], col = grDevices::adjustcolor("white", 0.55), border = NA)
    abline(v = sss.from, lty = 3, col = detrendCols$sss)
    text(sss.from, u[3] + 0.04 * (u[4] - u[3]), paste0(" SSS \u2265 ", signalCut, " from ", sss.from),
         adj = 0, cex = 0.8, col = detrendCols$sss)
    box(bty = "u")
  }
  if (!is.null(base)) {
    legend("topleft", inset = 0.01, bty = "n", cex = 0.85, lwd = 1.5,
           legend = labels, col = c(detrendCols$series, detrendCols$compare[2]))
  }
  invisible(NULL)
}

# ── plotSignal ────────────────────────────────────────────────────────────────
# How strong the common signal is through time: rbar (the mean correlation
# between trees) and EPS in windows along the chronology, from
# rwi.stats.running(), over the number of trees in each window. One rbar and
# one EPS for the whole span hide that both usually fall off where there
# are few trees. The dashed line is the usual EPS threshold of 0.85.
#   run, base — results of rwi.stats.running(); base may be NULL
#   sss       — optional: SSS for each year, named by year (sssOf()), drawn
#               as a line. It is by year, not by window, and is the one to
#               read for where the chronology is reliable.
# The dashed line at 0.85 is a convention, not a test (see epsNote).
plotSignal <- function(run, base = NULL, labels = c("With your settings", baselineLabel), sss = NULL) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mar = c(3.4, 4.4, 1, 4.4), mgp = c(2.6, 0.7, 0), tcl = -0.3, las = 1,
      cex = 1.1, bty = "u")
  x <- run$mid.year
  w <- (run$end.year[1] - run$start.year[1] + 1) / 2
  plot(x, run$n.trees, type = "n", axes = FALSE, xlab = "", ylab = "",
       ylim = c(0, max(run$n.trees) * 1.02), yaxs = "i", xlim = range(run$start.year, run$end.year))
  rect(x - w / 2, 0, x + w / 2, run$n.trees, col = "grey92", border = NA)
  axis(4, col.axis = "grey40")
  mtext("Trees", side = 4, line = 2.8, las = 0, cex = 1.1, col = "grey40")
  par(new = TRUE)
  plot(x, run$eps, type = "n", ylim = c(min(0, run$rbar.eff, run$eps, na.rm = TRUE), 1),
       xlim = range(run$start.year, run$end.year), xlab = "Year (middle of each window)",
       ylab = if (is.null(sss)) "rbar and EPS" else "rbar, EPS and SSS")
  abline(h = signalCut, lty = 2, col = detrendCols$ref)
  if (!is.null(sss)) lines(as.numeric(names(sss)), sss, col = detrendCols$sss, lwd = 2)
  if (!is.null(base)) {
    lines(base$mid.year, base$eps, col = detrendCols$compare[2], lwd = 1.2, type = "b", pch = 16, cex = 0.5)
    lines(base$mid.year, base$rbar.eff, col = detrendCols$compare[2], lwd = 1.2, lty = 3, type = "b", pch = 1, cex = 0.5)
  }
  lines(x, run$eps, col = detrendCols$series, lwd = 1.5, type = "b", pch = 16, cex = 0.6)
  lines(x, run$rbar.eff, col = detrendCols$series, lwd = 1.5, lty = 3, type = "b", pch = 1, cex = 0.6)
  legend("bottomleft", inset = 0.01, bty = "n", cex = 0.85,
         legend = c("EPS", "rbar", if (!is.null(sss)) "SSS, year by year", if (!is.null(base)) labels[2]),
         lty = c(1, 3, if (!is.null(sss)) 1, if (!is.null(base)) 1),
         lwd = c(1.5, 1.5, if (!is.null(sss)) 2, if (!is.null(base)) 1.2),
         pch = c(16, 1, if (!is.null(sss)) NA, if (!is.null(base)) 16),
         col = c(detrendCols$series, detrendCols$series, if (!is.null(sss)) detrendCols$sss,
                 if (!is.null(base)) detrendCols$compare[2]))
  invisible(NULL)
}
