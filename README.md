# iDetrend

## Overview

A Shiny app for detrending tree-ring series one at a time, built on
[dplR](https://github.com/OpenDendro/dplR). Part of openDendro.

You choose a curve for each series while looking at it. The app keeps the
record: one report lists every setting and carries R code that rebuilds the
indices from the original file.

What the app is for is what is awkward at the R prompt:

- **See a curve move** as its rigidity changes, with the indices under it.
- **Compare methods** on one series, their curves laid over each other.
- **See the series against the stand.** The mean of the other series sits
  behind each series' indices, so a swing the trees share (signal) can be
  told from one this tree has alone.
- **Be told what a setting does.** Under a spline's controls the app says,
  in years, which swings in growth it removes and which it keeps.
- **Be told where to look.** Series whose curve fell back to something
  else, or comes close to zero, are marked, with the reason and what it
  does to the indices.
- **See what a choice did.** The chronology and its statistics are shown
  against the same data with every series at dplR's default.
- **Keep the record without writing it down.** Settings, notes and R code
  go in the report; a settings file picks the work up in a later session.
  Work in progress is also kept in the browser's local storage and offered
  back when the same file is loaded, so a session that times out loses
  nothing. That copy never leaves the user's browser.

It fits curves to single series with `detrend.series()`. It does not do
regional curve, signal-free or C-method standardization (`rcs()`, `ssf()`,
`cms()`).

## Running it

The app needs dplR 1.8.0 or later.

    renv::restore()     # once, to install the packages in renv.lock
    shiny::runApp()

To run it against the development dplR installed in the system library,
bypass renv (the `.Rprofile` activates it) with `--vanilla`:

    Rscript --vanilla -e 'shiny::runApp(".", port = 4816)'

## Code layout

- `global.R`: packages, the dplR version check, the app's version. It
  sources `appHelpers.R`, so both `ui.R` and `server.R` see the helpers.
- `ui.R`, `server.R`: the app. The state is one table of settings, one row
  per series (`settings()` in `server.R`). The controls on the Detrend
  panel write to it; plots, indices, the report and its R code read it.
- `appHelpers.R`: Shiny-free helpers. `detrendOne()` detrends a series from
  its row of the settings; `detrendArgs()` turns a row into
  `detrend.series()` arguments. The app's own call and the report's R code
  are both built from `detrendArgs()`, so they cannot disagree.
  `fitStatus()` says what dplR did with a series and what it means.
- `plotDetrend.R`: the series plot and the chronology plot (base graphics).
  The report draws the same figures.
- `goals.R`: the starting points offered on the Overview ("What is the
  chronology for?"): which curve each goal starts every series with, and
  the text explaining why. The rules are judgement; change them there.
- `guide.R`: the two examples and their guides. The first is dplR's
  `nm046`. The second is eight trees of dplR's `gp.rwl` (ponderosa pine,
  Gus Pearson Natural Area), with a guide built on Biondi (1999). `svgArt.R`: the welcome
  screen.
- `report_detrend.rmd`: the downloadable report.

## Testing

Run from this directory:

    Rscript --vanilla tests/test-helpers.R
    Rscript --vanilla tests/test-server.R

`test-server.R` drives the server through the whole workflow with
`shiny::testServer()`. It runs the R code printed in the report and checks
that it gives the app's indices.

`tests/smoke-itrdb.R` does the same over a sample of raw ITRDB files, with
every method. It needs the ITRDB clone next to the app
(`../../itrdbMeasurementsClone`):

    Rscript --vanilla tests/smoke-itrdb.R 200 8

To test against the CRAN dplR rather than the development one, put a
library that holds it first, for example xDateR's renv library:

    R_LIBS=/path/to/library Rscript --vanilla tests/test-server.R

## Package management and deployment

`renv.lock` pins the packages for local work. `manifest.json` is the
packing list Posit Connect reads when it deploys from this repository: the
version of R, each package with its version, and a checksum of each file
of the app. Regenerate both when a package or a file of the app changes:

    renv::snapshot()
    rsconnect::writeManifest()

rsconnect is not one of the app's packages, so it is not in the renv
library and `writeManifest()` fails with "there is no package called
'rsconnect'" until it is installed there: `renv::install("rsconnect")`.
That does not change `renv.lock`.

`.rscignore` keeps the tests and notes out of the deployed app.

## Citation

Bunn AG (2008). A dendrochronology program library in R (dplR).
*Dendrochronologia*, 26(2), 115-124. doi:10.1016/j.dendro.2008.01.002
