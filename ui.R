# ══════════════════════════════════════════════════════════════════════════════
# iDetrend — ui.R
# Shiny app for detrending tree-ring series one at a time.
# Built on bslib (Bootstrap 5 / flatly theme) with a persistent sidebar and
# three panels, in the style of xDateR.
#
# Panel structure:
#   1. Overview — welcome / data checks, gaps, data summary, RWL plot
#   2. Detrend  — one series: its settings, the curve and the indices
#   3. Results  — every series: chronology, statistics, table, downloads
#
# Key design decisions:
#   • One table of settings, one row per series, is the app's state
#     (server.R). The controls on the Detrend panel write to it; the plots,
#     the indices, the report and its R code all read from it.
#   • The app is for what is awkward at the R prompt: seeing a curve move as
#     its rigidity changes, laying candidate curves over one series, being
#     told which of 200 series need a look, seeing what a choice does to the
#     chronology, and keeping the record of every choice without writing it
#     down.
#   • Every series starts at detrend.series()'s defaults, so a file nobody
#     has touched gives what detrend(rwl) gives. The Results panel compares
#     the chronology against that baseline.
#   • The report carries R code that rebuilds the indices exactly.
#   • Scrolling within panels is expected: this is a work tool, not a
#     dashboard.
# ══════════════════════════════════════════════════════════════════════════════

# The packages, the version and tipLabel() are in global.R, which Shiny runs
# before this file and server.R.


# ══════════════════════════════════════════════════════════════════════════════
# SIDEBAR
# ══════════════════════════════════════════════════════════════════════════════
# Persistent across panels: the file, the series selector (from the Detrend
# panel on) and About.

appSidebar <- sidebar(
  width = 280,

  # ── The file ─────────────────────────────────────────────────────────────
  # The current file (name, size, span, what needs a look) is rendered in
  # server.R.
  h6("Ring-width file", class = "text-muted fw-bold mt-1"),
  uiOutput("fileUI"),
  uiOutput("fileInfo"),
  # One step back for the last change to the settings (server.R)
  uiOutput("undoUI"),
  # The guided example: offered when the example data are loaded (server.R)
  uiOutput("guideUI"),
  # A standard fileInput shown as one button: its file-name box and progress
  # bar are hidden by the .id-upload CSS below, since the current file is
  # shown above instead.
  div(
    class = "id-upload",
    fileInput(
      inputId     = "file1",
      label       = NULL,
      multiple    = FALSE,
      buttonLabel = tagList(bs_icon("folder2-open"), " Load a file…"),
      accept      = c("text/plain", "text/csv", ".rwl", ".raw", ".txt", ".csv", ".fh", ".xml")
    )
  ),
  helpText("Tucson, Heidelberg, compact, TRiDaS, or .csv spreadsheets",
           "(years down, series across). Dated ring widths."),
  # Start over: a fresh session. Shown once a file is loaded; asks before
  # discarding work (see server.R).
  shinyjs::hidden(
    div(
      id = "divStartOver",
      actionButton("startOver", "Start over",
                   icon  = bs_icon("arrow-counterclockwise"),
                   class = "id-btn-quiet btn-sm w-100 mt-1"),
      helpText(class = "mt-1", "Clears the file and every setting for a fresh start.")
    )
  ),

  hr(),

  # ── Series selector ──────────────────────────────────────────────────────
  # Hidden until a file is loaded and the user is on the Detrend panel.
  shinyjs::hidden(
    div(
      id = "divSeriesSelector",
      h6("Series", class = "text-muted fw-bold"),
      selectInput(
        inputId  = "series",
        label    = NULL,
        choices  = c("Load a file first" = ""),
        selected = NULL
      ),
      div(class = "d-flex gap-1",
          actionButton("prevSeries", tagList(bs_icon("chevron-left"), "Previous"),
                       class = "id-btn-quiet btn-sm flex-fill"),
          actionButton("nextSeries", tagList("Next", bs_icon("chevron-right")),
                       class = "id-btn-quiet btn-sm flex-fill")),
      # where the user is, how many are left, and a jump to the next series
      # that needs a look (rendered in server.R)
      uiOutput("seriesProgress"),
      div(class = "small text-muted mt-1",
          "Keys: \u2190 \u2192 step through the series, C goes to the next to check."),
      selectInput("seriesOrder", tipLabel("Order",
        paste("The order the series are listed and stepped through in. 'Needing",
              "a look first' puts the series that could not be detrended, then",
              "those marked \u26a0, at the top. The order is fixed when you choose",
              "it and does not shift as you fix series: choose it again to re-sort.")),
        choices = c("As in the file" = "file", "Needing a look first" = "check",
                    "Longest first" = "longest", "Shortest first" = "shortest")),
      hr()
    )
  ),

  # ── About ────────────────────────────────────────────────────────────────
  accordion(
    open = FALSE,
    accordion_panel(
      title = "About",
      icon  = bs_icon("info-circle"),
      p("iDetrend is a Shiny app for detrending tree-ring series one at a",
        "time, using the",
        a("dplR", href = "https://github.com/OpenDendro/dplR/", target = "_blank"),
        "package. You choose a curve for each series while looking at it,",
        "and the app keeps the record: the report's R code rebuilds your",
        "indices exactly."),
      p("It fits curves to single series. It does not do regional curve",
        "(RCS), signal-free or C-method standardization: for those use dplR's",
        tags$code("rcs()"), ",", tags$code("ssf()"), "and", tags$code("cms()"), "."),
      p(a(bs_icon("github"), " iDetrend on GitHub",
          href = "https://github.com/OpenDendro/iDetrend", target = "_blank")),
      p(tags$small(
        paste0("iDetrend ", iDetrendVersion, " · dplR ", packageVersion("dplR"), "."),
        "Something not working?",
        a("Report a problem",
          href = "https://github.com/OpenDendro/iDetrend/issues", target = "_blank"),
        "and say which versions these are and, if you can, attach the file.")),
      # What changed in this version, for people who used the last one.
      # Rewrite the list with each deployment (and change iDetrendVersion).
      tags$details(
        class = "small mb-2",
        tags$summary(tags$strong(paste0("What's new in ", iDetrendVersion))),
        p(class = "mt-2 mb-1", tags$em("The app was rewritten. What changes results:")),
        tags$ul(
          class = "ps-3",
          tags$li(tags$strong("Gaps are gaps."), "Years a file records no",
                  "measurement for used to be read as zero-width rings. A",
                  "curve cannot be fitted across them, so the Overview asks",
                  "how to fill them."),
          tags$li(tags$strong("Every series starts at dplR's default,"), "a spline",
                  "of 67% of the series length, where the old app started at",
                  "an age-dependent spline."),
          tags$li(tags$strong("Subtraction uses the curve as fitted."), "With",
                  "indices by subtraction, a curve that goes below zero is no",
                  "longer swapped for the mean.")),
        p(class = "mb-1", tags$em("Also new:")),
        tags$ul(
          class = "ps-3",
          tags$li("One series at a time, picked from the sidebar, with the plot beside its settings."),
          tags$li("Other methods' curves can be laid over a series before you choose."),
          tags$li("The Overview asks what the chronology is for, and starts every series accordingly."),
          tags$li("The mean of the other series is drawn behind each series' indices."),
          tags$li("The settings say, in years, what a spline removes and what it keeps."),
          tags$li("Series that need a look are marked, with the reason in plain language."),
          tags$li("Work in progress is kept in your browser and offered back if the session ends."),
          tags$li("The Results panel shows what your choices did to the chronology."),
          tags$li("rbar and EPS count trees, read from the series names, and can be seen through time."),
          tags$li("SSS shows how far back enough trees reach; the years before are shaded."),
          tags$li("Residual, ARSTAN and variance-stabilised chronologies, as well as the standard one."),
          tags$li("Settings can be copied to other series, and saved to pick up later."),
          tags$li("Each series can use only part of its rings, leaving out juvenile rings or a bad end."),
          tags$li("Arrow keys step through the series, which can be put in order of need."),
          tags$li("Undo takes back the last change to the settings, one step at a time."),
          tags$li("A curve that swings steeply into its last rings is flagged: it distorts the most recent years."),
          tags$li("A second guided example, on ponderosa pine from a stand that grew crowded: what the choice of curve decides."),
          tags$li("One report, with R code that rebuilds the indices; the code also downloads as a script."),
          tags$li("Settings can be pinned, to see whether later changes made a difference."),
          tags$li("The chronology downloads as a Tucson .crn as well as .csv."),
          tags$li("A guided example: look for “Show me how” in the sidebar."))
      ),
      hr(),
      p(tags$strong("Please cite dplR if you use this app:")),
      p(tags$small(
        "Bunn AG (2008). A dendrochronology program library in R (dplR).",
        tags$em("Dendrochronologia"), ", 26(2), 115-124.",
        a("doi:10.1016/j.dendro.2008.01.002",
          href = "http://doi.org/10.1016/j.dendro.2008.01.002",
          target = "_blank")
      )),
      hr(),
      p(tags$em("Remember: detrending is a dark art. There is never a",
                "perfect curve, only one you can defend."))
    )
  )
)


# ══════════════════════════════════════════════════════════════════════════════
# PANEL 1: OVERVIEW
# ══════════════════════════════════════════════════════════════════════════════
# Before data loads: a welcome screen. After: what needs attention before
# detrending (gaps, data checks), the data summary and the RWL plot. The
# switch is made in server.R (output$overviewUI).

panelOverview <- nav_panel(
  title = "Overview",
  icon  = bs_icon("house"),
  value = "OverviewTab",
  uiOutput("overviewUI")
)


# ══════════════════════════════════════════════════════════════════════════════
# PANEL 2: DETREND
# ══════════════════════════════════════════════════════════════════════════════
# The series selected in the sidebar: its settings on the left, the plot on
# the right, so the effect of a change is seen as it is made. Under the plot,
# what dplR did and what it means for the indices.
#
# The settings controls are rendered in server.R (output$controls) with
# fresh input ids each time the series changes, so a value left over from
# one series can never be written to another.

panelDetrend <- nav_panel(
  title = "Detrend",
  icon  = bs_icon("graph-down"),
  value = "DetrendTab",
  uiOutput("noDataDetrendTab"),
  shinyjs::hidden(div(
    id = "contentDetrendTab",

    accordion(
      open = FALSE,
      accordion_panel(
        title = "How to use this panel",
        icon  = bs_icon("info-circle"),
        p("Each series is detrended by fitting a curve to it and dividing the",
          "ring widths by the curve (or subtracting it), with dplR's",
          a("detrend.series()", href = "https://rdrr.io/cran/dplR/man/detrend.series.html",
            target = "_blank"), ". The curve stands for the growth trend of the tree;",
          "what is left, the index, is what the series has in common with the others."),
        tags$ol(
          tags$li("Pick a series in the sidebar, or step through them with",
                  tags$strong("Next"), ". Every series starts with dplR's default,",
                  "a smoothing spline of 67% of the series length."),
          tags$li("Behind the indices, the pale blue shape is the", tags$strong("mean of the other series"),
                  ". Swings the series shares with it are signal to keep; swings it has",
                  "alone are what the curve should take out."),
          tags$li("Change the", tags$strong("method"), "and its settings. The plot follows."),
          tags$li("Use", tags$strong("Compare with"), "to lay other methods' curves over",
                  "the series before choosing one."),
          tags$li("Read the message under the plot: it says what dplR fitted, which is",
                  "not always what was asked for, and what that means for the indices."),
          tags$li("Series marked ⚠ in the sidebar need a look.",
                  tags$strong("Next to check"), "goes to them in turn."),
          tags$li("To detrend many series the same way, set one up and use",
                  tags$strong("Copy these settings"), ". Series can be picked by",
                  "length there: short series often want a different curve."),
          tags$li("To leave out juvenile rings or a damaged end, drag the ends of",
                  tags$strong("Rings to use"), "in."),
          tags$li("On a large file, set", tags$strong("Order"), "in the sidebar to",
                  "see the series needing a look first, and use the arrow keys.")),
        p(helpText("A flexible curve follows the series closely and removes more,",
                   "including slow changes in climate you may want to keep. A stiff",
                   "one removes less. The top axis counts rings, because rigidity is",
                   "in years of the series."))
      )
    ),

    # Shown once every series has been looked at (server.R)
    uiOutput("allSeenUI"),

    layout_columns(
      col_widths = c(4, 8),
      card(
        fill = FALSE,
        card_header(textOutput("controlsTitle", inline = TRUE)),
        uiOutput("controls")
      ),
      div(
        card(
          fill = FALSE,
          card_header(
            "Series, curve and indices",
            tooltip(
              bs_icon("question-circle"),
              paste("Above: the series and the fitted curve (red). Curves chosen",
                    "under 'Compare with' are drawn thin, in colour. Below: the",
                    "indices, the series divided by the curve (or the curve",
                    "subtracted), with a dashed line at 1 (or 0)."))
          ),
          plotOutput("seriesPlot", height = "520px"),
          checkboxInput("showOthers", tipLabel(
            "Show the mean of the other series behind the indices",
            paste("The pale blue shape is the mean of the other series' indices, as they",
                  "are detrended now. A swing this series shares with the others is",
                  "probably climate or something else the whole stand felt: signal",
                  "to keep. A swing it has alone is probably this tree: a neighbour",
                  "falling, an injury. A curve that removes the shared swings is",
                  "too flexible. Only series indexed the same way (division or",
                  "subtraction, power transformed or not) are in the mean.")),
            value = TRUE, width = "100%"),
          uiOutput("fitMessage")
        ),
        # What this series' settings do to the chronology (rendered in
        # server.R)
        uiOutput("seriesEffect")
      )
    ),

    # ── Copy settings to other series ─────────────────────────────────────
    card(
      fill = FALSE,
      card_header(
        "Copy these settings to other series",
        tooltip(
          bs_icon("question-circle"),
          paste("Gives the series you pick the settings shown above: method,",
                "its parameters, power transform and how the index is made.",
                "A spline rigidity given as a share of the length stays a",
                "share, so each series gets its own number of years. Notes",
                "and 'Rings to use' are not copied: they belong to the series."))
      ),
      layout_columns(
        col_widths = c(9, 3),
        selectizeInput(
          inputId  = "copyTo",
          label    = "Series to change",
          choices  = NULL,
          multiple = TRUE,
          width    = "100%",
          options  = list(placeholder = "Pick series, or use the links below",
                          closeAfterSelect = TRUE,
                          dropdownParent = "body",
                          plugins = list("remove_button"))
        ),
        div(class = "form-group shiny-input-container w-100",
            tags$label(class = "control-label", HTML("&nbsp;")),
            actionButton("copyApply", "Copy these settings", class = "btn-primary w-100"))
      ),
      div(class = "small",
          actionLink("copyAll", "Every other series"), " \u00b7 ",
          actionLink("copyUnseen", "Series not looked at yet"), " \u00b7 ",
          actionLink("copyFlagged", "Series marked \u26a0"), " \u00b7 ",
          actionLink("copyNone", "Clear")),
      # by length: short series often want a different curve from long ones
      div(class = "d-flex flex-wrap align-items-center gap-2 small mt-2",
          span("Add series with"),
          div(style = "width: 9.5rem;",
              selectInput("copyRuleOp", NULL, choices = c("fewer than" = "lt", "at least" = "ge"),
                          width = "100%", selectize = FALSE)),
          div(style = "width: 6rem;",
              numericInput("copyRuleN", NULL, value = 100, min = 1, step = 10, width = "100%")),
          span("rings"),
          actionButton("copyRuleAdd", "Add", class = "id-btn-quiet btn-sm"))
    )
  ))
)


# ══════════════════════════════════════════════════════════════════════════════
# PANEL 3: RESULTS
# ══════════════════════════════════════════════════════════════════════════════
# Every series together: what is wrong with the set (rendered in server.R),
# the chronology against the all-default baseline, the statistics, the table
# of series (click a row to open that series), and the downloads.

panelResults <- nav_panel(
  title = "Results",
  icon  = bs_icon("bar-chart-line"),
  value = "ResultsTab",
  uiOutput("noDataResultsTab"),
  shinyjs::hidden(div(
    id = "contentResultsTab",

    accordion(
      open = FALSE,
      accordion_panel(
        title = "How to use this panel",
        icon  = bs_icon("info-circle"),
        p("The indices of every series, taken together. The chronology is their",
          "mean each year, from dplR's",
          a("chron()", href = "https://rdrr.io/cran/dplR/man/chron.html", target = "_blank"),
          "(Tukey's biweight robust mean). The orange line behind it is the",
          "chronology from dplR's default detrending of the same data,",
          tags$code("detrend(rwl)"), ", so you can see what your choices changed."),
        p("The statistics are from", tags$code("summary()"), "of the indices:",
          tags$strong("rbar"), "is the mean correlation between series,",
          tags$strong("EPS"), "says how well the chronology stands for the",
          "population it samples, and",
          tags$strong("SNR"), "is the signal-to-noise ratio. They count trees, not",
          "cores: under", tags$strong("Trees and cores"), "the app reads from the",
          "series names which series belong to one tree. Check that it read them",
          "right.", tags$strong("Signal through time"), "(in the plot's menu) shows",
          "where along the chronology the signal holds up."),
        p(tags$strong("SSS"), "(subsample signal strength) says, year by year, how well",
          "the trees alive then stand for the whole sample: how far back enough",
          "trees reach. The panel gives the year from which it stays at or above",
          "0.85, and the chronology plot shades the years before. That is the job",
          "running EPS is often given, and SSS is the statistic meant for it."),
        p(tags$strong("The 0.85 is arbitrary."), "No test or theory sets it, and a",
          "chronology is not sound at 0.85 and unsound at 0.84. Wigley et al. (1984)",
          "offered it as a rough guide for SSS and gave none for EPS (Buras 2017).",
          "Read the year the panel gives as where the trees thin out, not as where",
          "the chronology starts to be right. Neither statistic says whether a",
          "chronology suits a climate reconstruction. That is settled by",
          "calibrating and verifying it against climate data."),
        p(tags$strong("Pin these settings"), "(under the statistics) keeps a copy of",
          "every series' settings as they are. After more changes, compare with the",
          "pinned copy in place of dplR's default to see whether they mattered."),
        p("Under", tags$strong("Chronology"), "choose the kind: the standard mean,",
          "the residual chronology (year-to-year variation only), ARSTAN's, or one",
          "with its variance stabilised."),
        p("In the table, click a row to open that series on the Detrend panel.",
          "Sort by", tags$strong("r"), "to find series that do not go with the",
          "rest, and by", tags$strong("Trend"), "to find series where the curve",
          "left a trend in."),
        p(helpText("Download the report before you leave: nothing is kept on the",
                   "server. The settings file lets you pick the work up again."))
      )
    ),

    uiOutput("resultsAlerts"),

    card(
      fill = FALSE,
      card_header(
        layout_columns(
          col_widths = c(8, 4),
          span("The indices together",
               tooltip(
                 bs_icon("question-circle"),
                 paste("Chronology: the robust mean of the indices each year,",
                       "over the number of series (grey). The orange line is",
                       "the chronology from dplR's default detrending of the",
                       "same data, detrend(rwl). All series: every",
                       "series' indices as a line. Image: series down the page,",
                       "years across, coloured by index, which shows trend the",
                       "curves left in."))),
          div(class = "d-flex justify-content-end",
              selectInput("resultsPlotType", NULL, width = "210px",
                          choices = c("Chronology" = "crn", "Signal through time" = "run",
                                      "All series" = "spag", "Image" = "image")))
        )
      ),
      plotOutput("resultsPlot", height = "420px"),
      conditionalPanel(
        condition = "input.resultsPlotType == 'run'",
        div(class = "d-flex align-items-end gap-3 mt-2",
            # flex-shrink-0: the long help text beside it would squeeze the input
            div(class = "flex-shrink-0",
                numericInput("runWin", tipLabel("Window (years)",
                  paste("rbar and EPS are computed in windows of this many years, each",
                        "overlapping the last by half (rwi.stats.running()). Longer",
                        "windows are steadier and show less detail.")),
                  value = 50, min = 20, step = 10, width = "170px")),
            helpText("EPS (solid) and rbar (dotted) in each window, and SSS (blue) in each",
                     "year, over the number of trees (grey). SSS is the one to read for",
                     "how far back enough trees reach: it says how well the trees alive in",
                     "a year stand for the whole sample. The dashed line is at 0.85, which",
                     "is arbitrary: no test or theory sets it. Wigley et al. (1984)",
                     "offered it as a rough guide for SSS and gave none for EPS (Buras",
                     "2017). Read all three as matters of degree; none says the signal",
                     "is a climate signal."))
      ),
      uiOutput("statsUI")
    ),

    # ── How the chronology is built, and which series are one tree ────────
    layout_columns(
      col_widths = c(6, 6),
      card(
        fill = FALSE,
        card_header(
          "Chronology",
          tooltip(bs_icon("question-circle"),
                  paste("Standard: the mean of the indices each year, slow variation",
                        "included. Residual: each series has its autocorrelation",
                        "removed first, leaving year-to-year variation; use it when the",
                        "persistence in growth is of the tree and not of the climate.",
                        "ARSTAN: the residual chronology with the autocorrelation the",
                        "series share put back. Variance stabilised: the mean, rescaled",
                        "so its variance does not grow where there are few series."))),
        selectInput("crnType", "Kind", choices = chronTypes, width = "100%"),
        conditionalPanel(
          condition = "input.crnType == 'vsc'",
          numericInput("crnWin", tipLabel("Window (years)",
            paste("The length of the running window in which the agreement between",
                  "series is measured (chron.stabilized()'s winLength). Under 30 is",
                  "not recommended.")),
            value = 51, min = 11, step = 2, width = "170px")),
        checkboxInput("crnBiweight", tipLabel("Robust mean",
          paste("Tukey's biweight robust mean, which gives less weight to a series far",
                "from the others in a year. Unticked: the plain arithmetic mean.")),
          value = TRUE),
        helpText(class = "mb-0", "The plot, the chronology download and the report use this choice.")
      ),
      card(
        fill = FALSE,
        card_header(
          "Trees and cores",
          tooltip(bs_icon("question-circle"),
                  paste("Two cores of one tree agree more than two trees do. rbar and EPS",
                        "are about trees, so the app needs to know which series are",
                        "cores of the same tree. It reads that from the series names."))),
        radioButtons("idsMode", NULL, width = "100%",
                     choices = c("Work it out from the series names" = "auto",
                                 "By position in the name" = "position",
                                 "Every series is its own tree" = "none")),
        conditionalPanel(
          condition = "input.idsMode == 'position'",
          div(class = "d-flex gap-2",
              numericInput("stcSite", "Site", value = 3, min = 0, step = 1, width = "90px"),
              numericInput("stcTree", "Tree", value = 2, min = 1, step = 1, width = "90px"),
              numericInput("stcCore", "Core", value = 1, min = 0, step = 1, width = "90px")),
          helpText("How many characters of each name, from the left, are the site, the",
                   "tree and the core. For CAM031: site 3 (CAM), tree 2 (03), core 1 (1).")),
        helpText(class = "mb-0", "The result is shown under the statistics above.")
      )
    ),

    card(
      fill = FALSE,
      card_header(
        "Series",
        tooltip(
          bs_icon("question-circle"),
          paste("One row per series: what was asked for and what dplR used.",
                "r is the series' correlation with the mean of the others",
                "(shaded when not significant at p < 0.05). Trend is the slope",
                "of a straight line through its indices, per century. Click a",
                "row to open the series."))
      ),
      DTOutput("seriesTable", fill = FALSE)
    ),

    layout_columns(
      col_widths = c(6, 6),
      card(
        fill = FALSE,
        card_header("Save the results"),
        div(class = "d-flex flex-wrap gap-2",
            downloadButton("downloadRWI", "Indices (.csv)", class = "id-btn-quiet"),
            downloadButton("downloadCrn", "Chronology (.csv)", class = "id-btn-quiet"),
            # the Tucson .crn, when the chronology can be written as one
            uiOutput("crnTucsonUI", inline = TRUE)),
        helpText(class = "mt-2",
                 "The .csv files have years down the page. dplR reads the indices back",
                 "with", tags$code("as.rwi(read.rwl(file))"), ". The .crn is the Tucson",
                 "format other dendro software reads; it holds three decimals."),
        hr(),
        downloadButton("detrendReport", "Generate report", class = "btn-primary"),
        downloadButton("downloadScript", "R script (.R)", class = "id-btn-quiet ms-2"),
        checkboxInput("reportPlots", "Include a plot of every series in the report", value = TRUE),
        helpText("Every setting, what dplR fitted, your notes, and R code that",
                 "rebuilds the indices from the original file. The R script is that",
                 "code on its own, to run or to keep with the data. With several",
                 "hundred series the plots make a large file.")
      ),
      card(
        fill = FALSE,
        card_header(
          "Pick up later",
          tooltip(bs_icon("question-circle"),
                  paste("The settings file is a .csv with one row per series. Load",
                        "the same ring-width file in a later session, then load",
                        "the settings file here: series are matched by name."))),
        downloadButton("downloadSettings", "Save settings (.csv)", class = "id-btn-quiet"),
        div(class = "id-upload mt-3",
            fileInput("settingsFile", NULL, multiple = FALSE, accept = c("text/csv", ".csv"),
                      buttonLabel = tagList(bs_icon("folder2-open"), " Load a settings file…"))),
        uiOutput("settingsLoadNote")
      )
    )
  ))
)


# ══════════════════════════════════════════════════════════════════════════════
# APP ASSEMBLY
# ══════════════════════════════════════════════════════════════════════════════
# The same layout and type as xDateR (flatly, IBM Plex Sans), in a colour of
# its own so the two apps are not mistaken for each other in a row of
# browser tabs: slate blue here, where xDateR is forest green. The
# welcome art (svgArt.R) uses the same colour.

ui <- tagList(
  useShinyjs(),
  # Ask before the tab is closed or reloaded while there is unsaved work.
  # The server says when there is (custom message idUnsaved, see server.R).
  tags$script(HTML("
    window.idUnsaved = false;
    $(document).on('shiny:connected', function() {
      Shiny.addCustomMessageHandler('idUnsaved', function(x) { window.idUnsaved = x; });
      // The work in progress, kept in this browser's local storage so a
      // session that ends does not lose it (see 'The copy kept in the
      // browser' in server.R). idStore writes or removes it; idFetch sends
      // back what is held for a file. Storage can be unavailable (private
      // windows, a full quota): then nothing is kept, and nothing breaks.
      Shiny.addCustomMessageHandler('idStore', function(x) {
        try {
          if (x.value === null || x.value === undefined) {
            localStorage.removeItem(x.key); localStorage.removeItem(x.key + ':saved');
          } else {
            localStorage.setItem(x.key, x.value);
            localStorage.setItem(x.key + ':saved', new Date().toLocaleString());
          }
        } catch (e) {}
      });
      Shiny.addCustomMessageHandler('idFetch', function(x) {
        var v = null, s = null;
        try { v = localStorage.getItem(x.key); s = localStorage.getItem(x.key + ':saved'); } catch (e) {}
        Shiny.setInputValue('autosaved', {key: x.key, value: v, saved: s}, {priority: 'event'});
      });
    });
    // Keys on the Detrend panel: left and right arrows step through the
    // series, C goes to the next one to check. Not while typing in a box,
    // moving a slider or a set of radio buttons, or with a dialog open.
    document.addEventListener('keydown', function(e) {
      var el = document.activeElement, tag = el ? el.tagName : '';
      if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT' || (el && el.isContentEditable) ||
          (el && el.classList && (el.classList.contains('irs-handle') || el.classList.contains('irs-line')))) return;
      if (document.querySelector('.modal.show')) return;
      var tab = document.querySelector('.navbar .nav-link.active');
      // Ctrl+Z (Cmd+Z on a Mac) is Undo, on any panel
      if ((e.ctrlKey || e.metaKey) && !e.altKey && !e.shiftKey && (e.key === 'z' || e.key === 'Z')) {
        var u = document.getElementById('undo');
        if (u) { e.preventDefault(); u.click(); }
        return;
      }
      if (e.ctrlKey || e.metaKey || e.altKey) return;
      if (!tab || tab.getAttribute('data-value') !== 'DetrendTab') return;
      var id = e.key === 'ArrowRight' ? 'nextSeries' : e.key === 'ArrowLeft' ? 'prevSeries' :
               (e.key === 'c' || e.key === 'C') ? 'nextCheck' : null;
      var btn = id && document.getElementById(id);
      if (btn && !btn.disabled) { e.preventDefault(); btn.click(); }
    });
    window.addEventListener('beforeunload', function(e) {
      if (window.idUnsaved) { e.preventDefault(); e.returnValue = ''; }
    });
  ")),
  tags$head(
    tags$style(HTML("
      /* Remove bslib default card max-height so cards grow with content */
      .card { max-height: none !important; }
      /* Give each panel bottom breathing room */
      .tab-pane { padding-bottom: 3rem; }
      /* Secondary buttons in body-text colour: Flatly's dark and
         secondary outlines are a pale grey that reads as disabled */
      .btn.id-btn-quiet { color: var(--bs-body-color); border: 1px solid var(--bs-body-color);
                          background: transparent; }
      .btn.id-btn-quiet:hover { background: var(--bs-gray-200); }
      /* File upload shown as a single full-width button (see appSidebar) */
      .id-upload .shiny-input-container { margin-bottom: 0.25rem; width: 100%; }
      .id-upload .input-group > .form-control,
      .id-upload .progress { display: none; }
      .id-upload .input-group-btn,
      .id-upload .input-group-prepend,
      .id-upload .btn-file { width: 100%; }
      .id-upload .btn-file { border-radius: var(--bs-border-radius) !important;
                             background-color: var(--bs-primary);
                             border-color: var(--bs-primary); color: #fff; }
      /* The statistics under the Results plot: label over number */
      .id-stat { min-width: 6.5rem; }
      .id-stat .id-stat-n { font-size: 1.35rem; font-weight: 600; line-height: 1.2; }
    "))
  ),
  page_navbar(
    title = "iDetrend",
    id    = "navbar",
    # Panels scroll rather than fill the window. With bslib's default
    # (fillable = TRUE) each panel is a flex container that shrinks its
    # cards to fit the viewport, clipping plots and tables on short screens.
    fillable = FALSE,
    theme = bs_theme(
      version    = 5,
      bootswatch = "flatly",
      primary    = "#2F4B7C",         # slate blue
      # links in the same colour: flatly's own are a green close to xDateR's
      "link-color" = "#2F4B7C",
      base_font  = font_google("IBM Plex Sans")
    ),
    sidebar = appSidebar,
    # A spinner on any output that is recalculating, and a pulse at the top
    # of the page while the server is busy
    header = useBusyIndicators(),

    panelOverview,
    panelDetrend,
    panelResults,

    nav_spacer(),
    nav_item(
      tags$a(
        bs_icon("github"),
        href   = "https://github.com/OpenDendro/iDetrend",
        target = "_blank",
        title  = "iDetrend on GitHub",
        class  = "text-muted"
      )
    )
  )
)
