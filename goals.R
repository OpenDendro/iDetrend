# Starting points: what the chronology is for decides where to begin.
#
# A newcomer does not know what "a spline of 67% of the series length"
# implies, but does know what the chronology is for. Each goal below sets
# the same starting curve for every series and says why. It is a place to
# start, not a verdict: every series can still be changed on the Detrend
# panel, and the app goes on marking the ones that need a look.
#
# These rules are judgement, and this file is where to change them. Each
# goal has:
#   id     — kept with the work in progress and printed in the report
#   label  — the choice the user sees
#   set    — the settings that differ from dplR's default (appHelpers.R,
#            defaultSettings()); everything else stays at the default
#   starts — the starting curve, in a few words
#   why    — why that curve suits the goal
#   watch  — what it costs, or what to look for series by series
goals <- list(
  list(id = "default",
       label = "I am not sure yet",
       set = list(),
       starts = "A smoothing spline of two-thirds of each series' length (dplR's default).",
       why = paste(
         "A middle course. It bends enough to follow most growth trends and is",
         "stiff enough to leave decade-to-decade variation in the indices."),
       watch = paste(
         "Because the rigidity is a share of the length, short series get a more",
         "flexible curve than long ones, and so keep less slow variation.")),
  list(id = "annual",
       label = "Year-to-year variation: dry years, event years, checking the dating",
       set = list(method = "Spline", spl.nyrs = 32),
       starts = "A 32-year smoothing spline for every series.",
       why = paste(
         "A flexible curve of one fixed length takes the slow swings out of every",
         "series alike, whatever its length, and leaves the year-to-year signal the",
         "trees share. It is the spline COFECHA uses to check dating."),
       watch = paste(
         "Variation slower than a few decades is gone, and cannot be had back from",
         "these indices. Do not use them to say anything about trends.")),
  list(id = "decadal",
       label = "Climate over decades and longer: a reconstruction",
       set = list(method = "ModNegExp"),
       starts = paste("A modified negative exponential curve, or a straight line where",
                      "that curve does not fit."),
       why = paste(
         "The most conservative choice. One smooth curve stands for the narrowing",
         "of rings as the tree ages, and every swing around it, slow or fast, stays",
         "in the indices."),
       watch = paste(
         "It suits open-grown trees whose rings narrow steadily. It cannot follow a",
         "suppression or a release, which then stays in the indices as if it were",
         "climate: look at each series against the mean of the others. No curve",
         "fitted to single series keeps variation longer than the series themselves;",
         "that needs regional curve or signal-free methods (dplR's rcs() and ssf()).")),
  list(id = "disturbance",
       label = "A closed-canopy stand, where trees are suppressed and released",
       set = list(method = "Spline", spl.nyrs = 50),
       starts = "A 50-year smoothing spline for every series.",
       why = paste(
         "In a closed stand a tree's growth jumps when a neighbour falls and sags",
         "when it is overtopped. Those pulses last a decade or a few, differ from",
         "tree to tree, and a curve has to be flexible enough to follow them."),
       watch = paste(
         "Fifty years is a starting size. Make the spline about as long as the",
         "disturbances you see, series by series: shorter where they are brief. A",
         "swing this series shares with the mean of the others is not a disturbance",
         "and should be left in."))
)

goalIds <- vapply(goals, `[[`, "", "id")
goalById <- function(id) goals[[match(id, goalIds)]]

# The settings a goal starts every series with
goalSettings <- function(id, series) {
  S <- defaultSettings(series)
  for (f in names(goalById(id)$set)) S[[f]] <- goalById(id)$set[[f]]
  S
}
