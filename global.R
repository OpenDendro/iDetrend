# ══════════════════════════════════════════════════════════════════════════════
# iDetrend — global.R
# Run by Shiny before ui.R and server.R, so what is set here is seen by both:
# the packages, the version, and the one helper both files build UI with.
# ══════════════════════════════════════════════════════════════════════════════

# ── Packages ──────────────────────────────────────────────────────────────────
# Installed by renv (locally) and from manifest.json (on Posit Connect); see
# the README. Nothing is installed at startup: installing on a live server
# at app launch is slow and can leave the app on untested package versions.
# The report also uses knitr and kableExtra, and loads them itself.
library(shiny)
library(rmarkdown)
library(dplR)

# iDetrend uses dplR 1.8.0 features throughout (the rwi class and its
# summary, gaps read as NA, rwl.check(), window()). An older dplR fails in
# confusing ways deep inside the app, so stop here and say why.
if (packageVersion("dplR") < "1.8.0") {
  stop("iDetrend needs dplR 1.8.0 or later, but this R session loaded dplR ",
       packageVersion("dplR"), " from ", dirname(find.package("dplR")),
       ". Install a newer dplR (or run renv::restore()) and restart R.",
       call. = FALSE)
}

# The version shown under About, so a user reporting a problem can say what
# they were running. Change it with each deployment.
iDetrendVersion <- "2026.10"

# Uploads: Shiny's limit of 5 MB is left as it is. The largest ring-width
# file in the ITRDB (chin067, 597 series) is 3.1 MB.

library(DT)
library(shinyjs)
library(bslib)
library(bsicons)

# The Shiny-free helpers. ui.R reads the lists of choices (the kinds of
# chronology); server.R uses the rest.
source("appHelpers.R")

# A label with a question-mark tooltip beside it. Used by ui.R and by the
# controls server.R builds.
tipLabel <- function(label, tip) {
  tags$label(class = "control-label", label, tooltip(bs_icon("question-circle"), tip))
}
