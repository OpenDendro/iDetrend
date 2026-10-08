# The guided example: one complete detrending job on the example data, as a
# checklist in the sidebar. Each step ticks itself off when the user has
# done it (the conditions are in server.R, next to the state they read).
#
# The example data are dplR's nm046: eight series of Douglas-fir from New
# Mexico, 1681-1969. Six take the default spline without trouble. 644041's
# curve falls by 65% over its last ten rings, which the app flags (a note in
# the "all" step). 644081 is the main lesson: its rings fall from about 3 mm to almost nothing, the default
# spline follows them below zero at the end, and dplR falls back to the
# mean. A straight line (what the modified negative exponential method fits
# to it) or Friedman's smoother stays positive.
#
# The steps are written for dplR's default starting point, so starting this
# guide puts the example back to it, and applying another starting point
# (goals.R) while it runs hides it (both in server.R). A second guide, on
# other data, follows this one.
#
# Each step: id, the panel it happens on (tab), the series to select when
# the user asks to be taken there, a title and the instruction (HTML).
# read = TRUE marks a step that is about reading what is on the panel: it
# is finished with the guide's Next button. The others are things to do,
# and finish when the app sees them done.
nm046Steps <- list(
  list(id = "overview", tab = "OverviewTab", series = NULL, read = TRUE,
       title = "Look at the data",
       text = paste(
         "Eight series, 1681&ndash;1969, with no gaps and nothing flagged by the",
         "data checks. The plot shows how the series overlap. Leave <b>What is",
         "the chronology for?</b> at <i>I am not sure yet</i>: this walk-through",
         "starts every series from dplR's default, and comes back to that",
         "question at the end. Next, the <b>Detrend</b> panel.")),
  list(id = "first", tab = "DetrendTab", series = "644011", read = TRUE,
       title = "One series and its curve",
       text = paste(
         "Above, the ring widths of 644011 with the fitted curve in red. Below,",
         "the indices: each width divided by the curve. Every series starts with",
         "dplR's default, a spline of 67% of the series length. Drag",
         "<b>Rigidity</b> down and watch the curve start to follow the series: a",
         "flexible curve removes more, including slow change you may want to",
         "keep. Put it back when you have seen it.")),
  list(id = "compare", tab = "DetrendTab", series = "644011",
       title = "Compare methods",
       text = paste(
         "Under <b>Compare with</b>, pick <b>Modified negative exponential</b>.",
         "Its curve is drawn over the series beside the spline, so the two can",
         "be judged on the same rings before you choose.")),
  list(id = "fix", tab = "DetrendTab", series = "644081",
       title = "A series that needs a look",
       text = paste(
         "644081 is marked &#9888;. The message under the plot says why: its",
         "rings shrink to almost nothing, the spline follows them below zero,",
         "and a series cannot be divided by a curve that is not positive. dplR",
         "used the mean instead, so no trend was removed. Choose a method",
         "whose curve stays positive."),
       answer = paste(
         "Set the method to <b>Modified negative exponential</b>. No such curve",
         "fits this series, so dplR fits a straight line, which stays above",
         "zero. <b>Friedman's super smoother</b> works too.")),
  list(id = "all", tab = "DetrendTab", series = NULL,
       title = "Look at every series",
       text = paste(
         "Step through the rest with <b>Next</b> in the sidebar. A tick marks",
         "the series you have looked at. One more is marked &#9888;: 644041,",
         "whose curve dives over its last ten rings, so its most recent indices",
         "depend on where the curve happens to end. Read the message and try a",
         "stiffer curve. Change any curve you do not like, and write why in the",
         "notes: they go in the report.")),
  list(id = "results", tab = "ResultsTab", series = NULL, read = TRUE,
       title = "What your choices did",
       text = paste(
         "The chronology is the mean of the indices each year. It is drawn over",
         "the orange line: dplR's default detrending of the same data. The two",
         "part only after 1886, where 644081 begins, and not by much: it is one",
         "series in eight, and the mean is a robust one. A chronology of many",
         "series forgives a poor curve; a series that stands alone does not.",
         "The table lists every series: click a row to go back to it.")),
  list(id = "save", tab = "ResultsTab", series = NULL,
       title = "Save your work",
       text = paste(
         "Nothing is kept on the server. Download the <b>indices</b>, and",
         "generate the <b>report</b>: its R code rebuilds the indices from the",
         "original data, so the work can be reproduced."))
)

nm046Finished <- paste(
  "You did the job: look, compare, fix, check the result, save. On your own",
  "data, begin with <b>What is the chronology for?</b> on the Overview: the",
  "answer sets a better starting curve than the default for most purposes.",
  "The app says which series need a look, but which curve is right for a",
  "tree is your call, and the notes are where you defend it.")

# ── The second example: a stand that grew crowded ────────────────────────────
# Ponderosa pine from the Gus Pearson Natural Area near Flagstaff, Arizona:
# eight of the 29 trees in dplR's gp.rwl, both cores of each, 16 series,
# 1570-1990. The eight were chosen to show different things (gpTrees
# below); it is a teaching set, not a sample.
#
# What is said about the site is from two papers by Biondi, who collected
# the data:
#   Biondi (1996) Decadal-scale dynamics at the Gus Pearson Natural Area:
#     evidence for inverse (a)symmetric competition? Canadian Journal of
#     Forest Research 26: 1397-1406. From the timber inventories of
#     1920-1990: stand density rose through the century (regeneration, fire
#     control, no cutting); growth of the stand as a whole did not change
#     but individual growth declined, and more for large pines than small,
#     from which he inferred competition.
#   Biondi (1999) Comparing tree-ring chronologies and repeated timber
#     inventories as forest monitoring tools. Ecological Applications 9(1):
#     216-227. The tree-ring data, and the points below.
#   - a permanent plot set up in 1908 in unmanaged ponderosa pine
#   - 1909: an open, park-like forest of clumps of trees; the last
#     spreading fire was in 1876; fire suppression began shortly after 1900
#     and pine regeneration "exploded"; by 1990 a dense, multistoried stand
#   - two cores from each tree, 29 large pines (50 cm diameter or more in
#     1990) and 29 small. That gp.rwl is the 29 large pines is an
#     inference, not something dplR documents: it has 58 series from 29
#     trees over 1570-1990, and its 16,408 rings are exactly the count the
#     paper gives for the large pines.
#   - individual tree growth declined over the 20th century, "attributed
#     to increased stand density"; monthly precipitation and temperature
#     showed no overall trend from 1910 to 1990; the decline of the large
#     pines is "in part, a return of growth rates to their long-term
#     average preceding the growth surge of the early 1900s"
#   - chronologies made with ARSTAN's defaults "did not show any decline
#     over time"; a stiff curve reproduced the decline the inventories
#     measured
#   - a series that ends in a low-growth period: dividing by the fitted
#     curve "could artificially inflate the final part" of the chronology
#
# The numbers in the text are from the data and checked by
# tests/test-server.R:
#   36B: first 20 rings average 3.8 mm; under 1 mm on average 1700-1899;
#     the default spline leaves its first 20 indices averaging 1.33, the
#     negative exponential 1.07
#   10B at the default: last ring 0.07 mm, curve 0.008, index 8.7
#   32-year spline for every series: the chronology averages 0.98 to 1.01
#     in every period of the 1900s
#   negative exponential (or line, or mean) for every series: 1.49 in
#     1900-19, 0.50 in 1980-90
#   with those settings EPS is 0.87 counting 8 trees, 0.91 counting 16
#     cores; SSS is at or above 0.85 from 1706, when the fourth tree begins
gpTrees <- c("07", "10", "20", "36", "42", "47", "48", "52")

gpSteps <- list(
  list(id = "g.overview", tab = "OverviewTab", series = NULL, read = TRUE,
       title = "Gus Pearson",
       text = paste(
         "Ponderosa pine from the Gus Pearson Natural Area near Flagstaff,",
         "Arizona: eight large trees, two cores each, 1570&ndash;1990, collected by",
         "Franco Biondi. In 1909 this was an open, park-like forest. Fire was",
         "kept out after 1900, young pines filled it in, and by 1990 it was a",
         "dense stand (Biondi 1999). These eight are a teaching set picked from",
         "the 29 trees in dplR's <code>gp.rwl</code>, not a sample.")),
  list(id = "g.juvenile", tab = "DetrendTab", series = "36B",
       title = "The trend of a growing tree",
       text = paste(
         "36B starts in 1604 with rings near 4 mm and is under 1 mm a century",
         "later. Nothing happened to the tree: the same wood is laid over a",
         "wider stem each year. The default spline is too stiff to turn that",
         "corner, so the first rings come out a third too high. Under",
         "<b>Compare with</b>, pick <b>Modified negative exponential</b>: a curve",
         "made for this shape.")),
  list(id = "g.end", tab = "DetrendTab", series = "10B", read = TRUE,
       title = "Dividing by almost nothing",
       text = paste(
         "10B is marked &#9888;. Its rings shrink to 0.07 mm at the end, the",
         "curve follows them down to 0.008, and 0.07 divided by 0.008 is an",
         "index of 8.7: the highest in the series, from one of its narrowest",
         "rings. Biondi (1999) warns of exactly this: where a series ends in",
         "slow growth, dividing by the curve can inflate the end of the",
         "chronology.")),
  list(id = "g.flex", tab = "OverviewTab", series = NULL,
       title = "A flexible curve for every series",
       text = paste(
         "Under <b>What is the chronology for?</b> choose <b>Year-to-year",
         "variation</b> and click <b>Start every series this way</b>. Every",
         "series gets a 32-year spline.")),
  list(id = "g.pin", tab = "ResultsTab", series = NULL,
       title = "A level chronology",
       text = paste(
         "Through the 1900s the chronology stays close to 1: about 0.98 to 1.01",
         "in every 20 years. Keep this to compare with: click <b>Pin these",
         "settings to compare with later</b>, under the statistics.")),
  list(id = "g.stiff", tab = "OverviewTab", series = NULL,
       title = "Now a stiff one",
       text = paste(
         "Back under <b>What is the chronology for?</b> choose <b>Climate over",
         "decades and longer</b> and apply it. Every series gets a negative",
         "exponential curve, or a straight line or the mean where that does",
         "not fit.")),
  list(id = "g.decline", tab = "ResultsTab", series = NULL, read = TRUE,
       title = "The same rings, another history",
       text = paste(
         "The chronology now climbs to about 1.5 in 1900&ndash;1919 and falls to 0.5",
         "by the 1980s. The orange line is what you pinned: level throughout.",
         "Not one ring changed. The flexible spline bent with each tree's",
         "decline and divided it out; the stiff curves could not, and left it",
         "in.")),
  list(id = "g.why", tab = "ResultsTab", series = NULL, read = TRUE,
       title = "Which one is right?",
       text = paste(
         "The decline is real. Repeated forest inventories at this site measured",
         "it, and Biondi (1996, 1999) attributes it to the stand growing denser,",
         "not to climate: rainfall and temperature showed no trend from 1910 to 1990.",
         "Part of it is growth settling back after a surge in the early 1900s.",
         "So every tree shares this swing, and it is not climate. What trees",
         "have in common is not always the weather. For the history of the",
         "stand, keep it. For year-to-year climate, take it out. The curve is",
         "where you make that choice.")),
  list(id = "g.trees", tab = "ResultsTab", series = NULL, read = TRUE,
       title = "Trees, not cores",
       text = paste(
         "Under the statistics: <i>16 series from 8 trees</i>, read from the",
         "names. Two cores of one tree agree more than two trees do, so EPS",
         "counts trees: 0.87 here, where counting every core as a tree would",
         "give 0.91. <b>SSS &ge; 0.85 from 1706</b>: before that year only three",
         "of the eight trees reach back, and the plot shades it. The 0.85 is",
         "arbitrary. 1706 is the year a fourth tree begins, and nothing else",
         "about the chronology changes there."))
)

gpFinished <- paste(
  "Detrending did not find the answer here; it chose the question. A curve",
  "that follows each tree closely keeps what changes from year to year and",
  "removes the rest, including a decline the whole stand shared. A stiff one",
  "keeps the decline. Say which you are after, and let the report show the",
  "curve you chose for it.",
  "<br><br><span class='text-muted'>The site's history is from Biondi (1996),",
  "<i>Can. J. For. Res.</i> 26: 1397&ndash;1406, and Biondi (1999), <i>Ecol.",
  "Appl.</i> 9: 216&ndash;227.</span>")

# ── The examples, and the guide that runs on each ────────────────────────────
#   link    — the words of the link that loads the example: what the data are
#   about   — one or two sentences on what the example teaches, shown under
#             the link before a file is loaded, so the two can be told apart
#   title   — what the file is called once loaded, and in the report
#   code    — R source that makes the data from dplR, as `dat`. The app runs
#             it to load the example and the report prints it, so the two
#             cannot differ.
#   guide   — label: the link that starts the guide; steps; finished;
#             fromDefault: TRUE when the steps are written for dplR's
#             default starting point, so that applying another starting
#             point while the guide runs hides it
examples <- list(
  nm046 = list(
    link  = "Douglas-fir, New Mexico",
    about = paste("8 series. The basics: fitting a curve to each series, and what to",
                  "do when the default curve fails."),
    title = "Example data (nm046)",
    code  = c("data(nm046)", "dat <- nm046"),
    guide = list(label = "Show me how: the basics of detrending",
                 steps = nm046Steps, finished = nm046Finished, fromDefault = TRUE)),
  gusPearson = list(
    link  = "Ponderosa pine, Arizona",
    about = paste("16 series from 8 trees, in a crowding stand. How the",
                  "curve you choose decides whether a decline the trees share",
                  "stays in the chronology."),
    title = "Example data (Gus Pearson, 8 trees)",
    code  = c("data(gp.rwl)",
              "# eight of its 29 trees, both cores of each: a teaching set",
              paste0("trees <- c(", paste0('"', gpTrees, '"', collapse = ", "), ")"),
              'dat <- gp.rwl[, as.vector(rbind(paste0(trees, "A"), paste0(trees, "B")))]'),
    guide = list(label = "Show me how: a crowded stand, and what the curve decides",
                 steps = gpSteps, finished = gpFinished, fromDefault = FALSE))
)
guides <- lapply(examples, `[[`, "guide")
