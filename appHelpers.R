# Helpers shared by server.R. Kept free of Shiny so they can be tested in a
# plain R session (see tests/test-helpers.R).

# The Hanning filter length n arrives from a selectInput as a string.
# "NULL" means no filter.
resolveN <- function(val) {
  if (is.null(val) || identical(val, "NULL")) NULL else as.numeric(val)
}

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
       "example several sites, species or variables), and xDateR reads one ",
       "set at a time. Export the set you want to crossdate (as Tucson, say) ",
       "and load that.")
}

# ── seriesSpan ────────────────────────────────────────────────────────────────
# First and last measured year of one series in an rwl.
seriesSpan <- function(rwl, series) {
  yrs <- as.numeric(rownames(rwl))
  range(yrs[!is.na(rwl[[series]])])
}

# ── rwlGaps ───────────────────────────────────────────────────────────────────
# Interior gaps: years inside a series' span with no measurement. Since dplR
# 1.8.0, read.tucson() returns these as NA (older readers filled them with
# zero). Returns a data.frame with one row per gap: series, first, last, n.
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

# editRing() lives in editRing.R: the edit report prints that file verbatim.

# ── tucsonPrec ────────────────────────────────────────────────────────────────
# Precision to write a Tucson file at: 0.001 if any value would lose a digit
# at 0.01, otherwise 0.01. write.tucson() defaults to 0.01 regardless, which
# silently rounds 0.001 mm data.
tucsonPrec <- function(rwl) {
  v <- unlist(rwl, use.names = FALSE)
  v <- v[!is.na(v)]
  if (any(abs(v * 100 - round(v * 100)) > 1e-6)) 0.001 else 0.01
}

# ── downloadName ──────────────────────────────────────────────────────────────
# File name for a downloaded .rwl: original name (sans extension) plus a tag
# and the date. `name` may be NULL (e.g. no undated file uploaded).
downloadName <- function(name, tag, fallback = "xDateR") {
  base <- if (is.null(name)) fallback else tools::file_path_sans_ext(name)
  paste0(base, "-", tag, "-", Sys.Date(), ".rwl")
}

# ── argCode ───────────────────────────────────────────────────────────────────
# R source for named arguments, for the "R Code" section of the reports,
# e.g. argCode(p, c("n", "method")) gives 'n = NULL,\n    method = "spearman"'.
argCode <- function(p, which, sep = ",\n    ") {
  fmt <- function(v) {
    if (is.null(v)) "NULL"
    else if (is.character(v)) paste0('"', v, '"')
    else as.character(v)
  }
  paste0(which, " = ", vapply(p[which], fmt, ""), collapse = sep)
}

# R source for a character vector, e.g. c("A", "B").
vecCode <- function(x) {
  if (length(x) == 0) return("character(0)")
  paste0("c(", paste0('"', x, '"', collapse = ", "), ")")
}

# Analysis arguments passed to every dplR crossdating call.
normArgs <- c("n", "nyrs", "prewhiten", "ar.order.max", "biweight")

# ── crsFlagged ────────────────────────────────────────────────────────────────
# COFECHA-style A and B flags from a corr.rwl.seg() result run with lag.max,
# one row per flagged segment. Mirrors dplR's internal report.flagged() (used
# by xdate.report()) so the app and the COFECHA-style report agree:
#   A — correlation under pcrit, but the dated position is the best tested:
#       weak, not misdated.
#   B — some other position (best.lag != 0) correlates better. A dating
#       hypothesis to check on the wood: negative lag = probably missing
#       rings, positive = probably false rings.
#   weak — a B whose best correlation is itself under the critical value:
#       the segment crossdates nowhere in the window, so read it as a low
#       correlation, not a dating error.
# The critical r is from the t distribution: exact for Pearson, an
# approximation for Spearman and Kendall.
rCrit <- function(n, pcrit) {
  if (n < 4) return(NA_real_)
  tq <- qt(1 - pcrit, n - 2)
  tq / sqrt(n - 2 + tq^2)
}

crsFlagged <- function(crs) {
  rho  <- crs$spearman.rho
  lag  <- crs$best.lag
  flag <- ifelse(lag != 0, "B", ifelse(crs$p.val >= crs$pcrit, "A", ""))
  flag[is.na(flag)] <- ""
  idx  <- which(flag != "", arr.ind = TRUE)
  idx  <- idx[order(idx[, 1], idx[, 2]), , drop = FALSE]
  isB  <- flag[idx] == "B"
  weak <- isB & crs$best.rho[idx] < rCrit(crs$seg.length, crs$pcrit)
  data.frame(series   = rownames(rho)[idx[, 1]],
             from     = crs$bins[idx[, 2], 1],
             to       = crs$bins[idx[, 2], 2],
             flag     = flag[idx],
             r.dated  = rho[idx],
             best.lag = lag[idx],
             r.lag    = as.numeric(ifelse(isB, crs$best.rho[idx], NA_real_)),
             gain     = as.numeric(ifelse(isB, crs$best.rho[idx] - rho[idx], NA_real_)),
             weak     = weak,
             row.names = NULL, stringsAsFactors = FALSE)
}

# ── windowBounds ──────────────────────────────────────────────────────────────
# Bounds for the Edit panel's skeleton-plot window on a series spanning
# `span` (first and last year). Window centre and width constrain each other:
#   width  <= min(100, 2 * distance from centre to the nearer end), in steps
#             of 10, so the window never runs off the series (and the
#             skeleton plot stays readable); and >= 20, because
#             xskel.ccf.plot() correlates the two halves of the window and
#             fails on 5-year halves
#   centre within [start + width/2, end - width/2], in steps of 5
# NULL/NA centre or width start the window mid-series, 40 years wide.
# Values that no longer fit are clamped.
windowBounds <- function(span, center = NULL, width = NULL) {
  if (is.null(center) || is.na(center)) center <- round(mean(span), -1)
  if (is.null(width)  || is.na(width))  width  <- 40
  center   <- min(max(center, span[1]), span[2])
  halfRoom <- min(center - span[1], span[2] - center)
  maxWidth <- max(20, min(100, floor(halfRoom / 10) * 10 * 2))
  width    <- max(20, min(width, maxWidth))
  # Round inward to the slider's 5-year steps, so the window can't hang
  # off either end (rounding to the nearest 10 could overshoot by 5)
  minCenter <- ceiling((span[1] + width / 2) / 5) * 5
  maxCenter <- floor((span[2] - width / 2) / 5) * 5
  if (minCenter > maxCenter) {           # series shorter than the window
    minCenter <- maxCenter <- round(mean(span))
  }
  list(center    = max(minCenter, min(maxCenter, center)),
       minCenter = minCenter, maxCenter = maxCenter,
       width     = width, maxWidth = maxWidth)
}

# editRing.R without its developer notes (lines starting with ##)
userCode <- function(code) {
  code <- unlist(strsplit(paste(code, collapse = "\n"), "\n"))
  code <- code[!grepl("^\\s*##", code)]
  while (length(code) && !nzchar(trimws(code[1]))) code <- code[-1]
  code
}

# ── editStepsCode ─────────────────────────────────────────────────────────────
# R source that replays the app's edits (rows of editDF, in order) on an rwl
# called `dat`, using editRing() and fillGaps() from editRing.R. Shared by
# the reports, so every report that shows edited data reproduces it.
editStepsCode <- function(ed) {
  fmtFill <- function(f) if (f == "0") "0" else paste0('"', f, '"')
  vapply(seq_len(nrow(ed)), function(i) {
    r <- ed[i, ]
    if (r$action == "fill") {
      paste0("dat <- fillGaps(dat, ", vecCode(strsplit(r$series, ",")[[1]]),
             ", fill = ", fmtFill(r$fill), ")")
    } else if (r$action == "delete") {
      paste0('dat <- editRing(dat, "', r$series, '", "delete", year = ', r$year,
             ", fix.last = ", r$fixLast, ")")
    } else {
      paste0('dat <- editRing(dat, "', r$series, '", "insert", year = ', r$year,
             ", value = ", r$value, ", fix.last = ", r$fixLast, ")")
    }
  }, character(1))
}

# Everything needed to rebuild the app's data in plain R: read the file, then
# (if there are edits) define editRing()/fillGaps() and replay them.
# editCode is the text of editRing.R; its ## developer notes are dropped.
dataCode <- function(fname, ed, editCode) {
  editCode <- userCode(editCode)
  c("library(dplR)",
    paste0('dat <- read.rwl("', fname, '")'),
    if (nrow(ed) > 0) c("",
                        "# The edits made in xDateR, replayed in order.",
                        "# First the functions xDateR uses to make them:",
                        editCode, "",
                        editStepsCode(ed)))
}

# ── checkTitle ────────────────────────────────────────────────────────────────
# A plain-language title for one rwl.check() finding (a row of
# as.data.frame(rwl.check(...))). The checks users meet most often get a
# sentence that names the series and what it means; the rest fall back to
# the description in dplR's rwl.check.catalogue(), passed as `catalogue`.
checkTitle <- function(f, catalogue) {
  s <- if (is.na(f$series)) "" else f$series
  v <- f$value
  switch(f$check,
    RWL_DATING_LAG = paste0(s, " may be misdated by ", abs(v),
                            if (abs(v) == 1) " year" else " years", ": probably ",
                            if (abs(v) == 1) "a " else "",
                            if (v < 0) "missing ring" else "false ring",
                            if (abs(v) > 1) "s" else ""),
    RWL_SERIES_OUTLIER   = paste0(s, " doesn't fit the rest of the collection"),
    RWL_INTERNAL_NA      = paste0(s, " has years with no measurement"),
    RWL_EMPTY_SERIES     = paste0(s, " has no measurements"),
    RWL_DUP_SERIES       = "Two or more series hold identical measurements",
    RWL_ZERO_VARIANCE    = paste0(s, " has the same value throughout"),
    RWL_NEGATIVE         = paste0(s, " has a negative ring width"),
    RWL_REPEATED_VALUE   = paste0(s, " repeats the same width in consecutive rings"),
    RWL_IMPLAUSIBLE_MEAN = "The mean ring width looks wrong for millimetres: a units error?",
    RWL_FUTURE_YEAR      = "The data include years later than this year",
    RWL_SITE_CODE        = paste0(s, " has a different site code from the other series"),
    RWL_ID_RENAMED       = paste0(s, " was renamed on reading: its id is used twice in the file"),
    RWL_HEADER_SPAN      = "The span in the file header disagrees with the data",
    RWL_ALL_ZERO_YEAR    = "Some years are zero in every series",
    {
      d <- catalogue$description[match(f$check, catalogue$check)]
      d <- if (is.na(d)) f$check else paste0(toupper(substr(d, 1, 1)), substring(d, 2))
      if (nzchar(s)) paste0(d, " (", s, ")") else d
    })
}

# Findings as "check|series" keys, for comparing two rwl.check() runs.
checkKeys <- function(f) paste(f$check, ifelse(is.na(f$series), "", f$series), sep = "|")

# ── resolvedText ──────────────────────────────────────────────────────────────
# Sentences for findings that an edit made go away (rows of rwl.check()
# output), one per kind of check, phrased as what changed. The problem
# titles from checkTitle() describe a problem in the present tense, so they
# can't be reused here: "704071 has years with no measurement" under
# "Resolved" reads as if the gap were still there.
resolvedText <- function(fixed, catalogue) {
  andList <- function(x) {
    if (length(x) <= 1) return(x)
    paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
  }
  vapply(split(fixed, fixed$check), function(x) {
    s  <- sort(unique(x$series[!is.na(x$series)]))
    ss <- andList(s)
    one <- length(s) == 1
    switch(x$check[1],
      RWL_INTERNAL_NA    = paste0("Gaps filled in ", ss, "."),
      RWL_DATING_LAG     = paste0(ss, if (one) " now fits" else " now fit",
                                  " best where ", if (one) "it is" else "they are", " dated."),
      RWL_SERIES_OUTLIER = paste0(ss, if (one) " now fits" else " now fit",
                                  " the rest of the collection."),
      {
        d <- catalogue$description[match(x$check[1], catalogue$check)]
        if (is.na(d)) d <- x$check[1]
        paste0("No longer flagged: ", d, if (length(s)) paste0(" (", ss, ")"), ".")
      })
  }, character(1), USE.NAMES = FALSE)
}

# ── floaterRunnerUp ───────────────────────────────────────────────────────────
# The best floater position other than the best one: the highest correlation
# among positions more than `gap` years from the best. Positions one or two
# years either side are the same match off by a ring or two (the segment
# table shows those), not a different placement. Returns list(r, first,
# last), or NULL if there is no other position.
floaterRunnerUp <- function(fcs, gap = 2) {
  i <- which.max(fcs$r)
  far <- which(abs(fcs$last - fcs$last[i]) > gap & !is.na(fcs$r))
  if (length(far) == 0) return(NULL)
  j <- far[which.max(fcs$r[far])]
  list(r = fcs$r[j], first = fcs$first[j], last = fcs$last[j])
}

# ── lagRuns ───────────────────────────────────────────────────────────────────
# Sentences for the B-flagged segments of one series (rows of crsFlagged()),
# grouped into runs of consecutive segments with the same lag, saying where
# to look for the error. `tested` is every segment of the series that was
# tested (crsTested()), flagged or not.
#
# Where is the error, and what kind? Dating runs from the bark inward, so a
# missing ring makes every ring BEFORE it one year late. With correctly
# dated segments AFTER the run (the usual case in a dated series), a run at
# lag -1 means a missing ring, +1 a false ring, and the error is in the last
# segment of the run, the one next to the correct segments.
# With correctly dated segments BEFORE the run (a floater placed by its
# older part), the rings after the error are the ones that are off, and the
# reading reverses: lag -1 means a FALSE ring (one ring too many has been
# counted going forward), +1 a missing ring, and the error is in the first
# segment of the run.
# With correct segments on both sides the ring count is right but two errors
# cancel, one at each end of the run. With no correct segments the whole
# series is offset. (See "Details" in ?corr.rwl.seg. Checked against the
# example data, whose errors are known: ABC118 lost its 1797 ring, ABC104
# has a duplicate at 1425, the floater ABC110 a duplicate at 1816; each lies
# in the segment these rules name.)
lagRuns <- function(f, tested = NULL, what = "the floater") {
  b <- f[f$flag == "B" & !f$weak, ]
  if (nrow(b) == 0) return(character(0))
  b <- b[order(b$from), ]
  step <- (b$to[1] - b$from[1] + 1) / 2
  run  <- cumsum(c(1, diff(b$best.lag) != 0 | diff(b$from) > step))
  vapply(split(b, run), function(x) {
    k     <- x$best.lag[1]
    n     <- abs(k)
    yrs   <- paste0(n, if (n == 1) " year " else " years ")
    # the kind of error, read with correct segments AFTER the run (bark side)
    ring  <- if (k < 0) "a missing ring" else "a false ring"
    other <- if (k < 0) "a false ring" else "a missing ring"
    seg   <- function(i) paste0(x$from[i], "\u2013", x$to[i])
    last  <- seg(nrow(x))
    first <- seg(1)
    one   <- nrow(x) == 1
    lead  <- paste0(min(x$from), "\u2013", max(x$to), ": ",
                    if (one) "this segment fits" else "these segments fit",
                    " better ", yrs, if (k < 0) "earlier" else "later",
                    " (lag ", sprintf("%+d", as.integer(k)), "). ")
    rest   <- if (is.null(tested)) NULL else tested[!tested$from %in% x$from, , drop = FALSE]
    before <- !is.null(rest) && any(rest$from < min(x$from))
    after  <- !is.null(rest) && any(rest$from > max(x$from))
    where <- if (is.null(tested)) {
      "Look for the error where the lag changes."
    } else if (after && !before) {
      paste0("The segments after it fit where dated, so look for ", ring, " in ",
             what, if (one) " in this segment." else
               paste0(" in ", last, ", the last of these segments."))
    } else if (before && !after) {
      paste0("The segments before it fit where dated, so look for ", other, " in ",
             what, if (one) " in this segment." else
               paste0(" in ", first, ", the first of these segments."))
    } else if (before && after) {
      paste0("The segments on both sides fit where dated, so the ring count is",
             " right but two errors cancel: look for ", ring, " in ", last,
             " and ", other, " in ", first, ".")
    } else {
      paste0("Every tested segment is shifted, so the whole of ", what,
             " may be dated ", yrs, if (k < 0) "too late" else "too early", ".")
    }
    paste0(lead, where)
  }, character(1), USE.NAMES = FALSE)
}

# The segments of one series that were tested (have a correlation), from a
# corr.rwl.seg() result: a data.frame of from and to. `i` is the series' row.
crsTested <- function(crs, i = 1) {
  ok <- !is.na(crs$spearman.rho[i, ])
  data.frame(from = crs$bins[ok, 1], to = crs$bins[ok, 2])
}

# ── dplrAdvice ────────────────────────────────────────────────────────────────
# What the user can do inside xDateR about a dplR error. dplR's own advice
# is written for R users ("detrend the data yourself and pass the indices"),
# which can't be followed in the app. Returns "" when there is nothing
# specific to suggest. `series` is the names of the loaded series: any named
# in the message (as a whole word, so a series called "j" doesn't match
# every "j") can be left out of the master.
dplrAdvice <- function(msg, series) {
  named <- series[vapply(series, function(s) {
    grepl(paste0("(^|[^[:alnum:]_.])", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", s),
                 "($|[^[:alnum:]_.])"), msg)
  }, logical(1))]
  tips <- c(
    if (length(named)) paste0("leave ", paste(named, collapse = ", "),
                              " out of the master (filter on the Correlations panel)"),
    if (grepl("internal NA", msg, fixed = TRUE)) "fill the gap from the Overview panel",
    if (grepl("nyrs", msg, fixed = TRUE))
      "switch the Low-frequency filter to None or Hanning in the Analysis Parameters")
  if (length(tips) == 0) return("")
  paste0(" In xDateR you can: ", paste(tips, collapse = "; or "), ".")
}

# ── checkBody ─────────────────────────────────────────────────────────────────
# The detail line under a finding's title. dplR's message ends with its own
# reading of the numbers ("...; the series may be misdated by 1 year(s), most
# likely a missing ring"), which checkTitle() has already said. For the
# checks that have a plain-language title, keep the evidence and drop that
# closing clause; other messages are shown whole.
checkBody <- function(f) {
  msg <- f$message
  if (f$check %in% c("RWL_DATING_LAG", "RWL_SERIES_OUTLIER")) {
    msg <- sub(";.*$", "", msg)
  }
  paste0(toupper(substr(msg, 1, 1)), substring(msg, 2),
         if (!grepl("[.!?]$", msg)) ".")
}

# ── replayEdits ───────────────────────────────────────────────────────────────
# Applies the edits in `ed` (rows of the app's editDF, in order) to `rwl`
# with editRing() and fillGaps() (editRing.R): the same replay the reports'
# R code does. Used to undo the last edit: the file as loaded, replayed
# without it, is exact and needs no stored copy of the data per step.
replayEdits <- function(rwl, ed) {
  for (i in seq_len(nrow(ed))) {
    r <- ed[i, ]
    rwl <- if (r$action == "fill") {
      fillGaps(rwl, strsplit(r$series, ",")[[1]],
               if (r$fill == "0") 0 else r$fill)
    } else {
      editRing(rwl, r$series, r$action, year = r$year,
               value = if (r$action == "insert") r$value else NULL,
               fix.last = r$fixLast)
    }
  }
  rwl
}

# ── reportName ────────────────────────────────────────────────────────────────
# File name for a downloaded report: the data file's name, what the report
# is, and the date, e.g. "ak006-series-704071-2026-10-03.html".
reportName <- function(file, kind) {
  base <- if (is.null(file)) "xDateR" else tools::file_path_sans_ext(file)
  paste0(base, "-", gsub("[^[:alnum:]_.-]", "_", kind), "-", Sys.Date(), ".html")
}

# ── paramSummary ──────────────────────────────────────────────────────────────
# The analysis settings as short phrases, for the one-line summary in the
# sidebar. `p` is the app's parameter list (xdParams()).
paramSummary <- function(p) {
  c(paste0(p$seg.length, "-yr segments"),
    paste("bin floor", p$bin.floor),
    if (!is.null(p$nyrs)) paste0(p$nyrs, "-yr spline") else
      if (!is.null(p$n)) paste("Hanning", p$n) else "no filter",
    if (p$prewhiten) {
      if (is.null(p$ar.order.max)) "prewhitened" else
        paste0("prewhitened (AR \u2264 ", p$ar.order.max, ")")
    } else "not prewhitened",
    paste0(toupper(substr(p$method, 1, 1)), substring(p$method, 2)),
    paste("p <", p$pcrit),
    if (p$lag.max > 0) paste0("lags \u00b1", p$lag.max) else "no lag search",
    if (p$biweight) "robust mean" else "plain mean")
}

# ── fmtP ──────────────────────────────────────────────────────────────────────
# A p-value for display: "< 0.001" below that, three decimals otherwise.
# (Shown as a number, 1.3e-199 prints in the browser as
# 1.299999999999999e-199, and that much precision means nothing.)
fmtP <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "< 0.001", formatC(p, digits = 3, format = "f")))
}

# ── placeFloater ──────────────────────────────────────────────────────────────
# Dates an undated series by its last (outer) year, the way xdate.floater()
# does for its best fit: the measurements (NA removed) are given consecutive
# years ending at `last`. Returns rwlOut (the dated series) and rwlCombined
# (the master with it added), as xdate.floater() does. Used when the user
# chooses a position other than the best fit.
placeFloater <- function(master, series, name, last) {
  x   <- series[!is.na(series)]
  out <- data.frame(x, row.names = as.character((last - length(x) + 1):last))
  names(out) <- name
  class(out) <- c("rwl", "data.frame")
  list(rwlOut = out, rwlCombined = combine.rwl(master, out))
}

# ── floaterCandidates ─────────────────────────────────────────────────────────
# The distinct positions that fit a floater best, from xdate.floater()'s
# floaterCorStats: the highest correlation first, then the next highest that
# is more than `gap` years from every position already listed (positions a
# year or two apart are the same match off by a ring). Up to `n` rows of
# first, last, r, p and n (the years of overlap with the master).
floaterCandidates <- function(fcs, n = 5, gap = 2) {
  fcs  <- fcs[order(-fcs$r), ]
  keep <- integer(0)
  for (i in seq_len(nrow(fcs))) {
    if (all(abs(fcs$last[i] - fcs$last[keep]) > gap)) keep <- c(keep, i)
    if (length(keep) == n) break
  }
  out <- fcs[keep, ]
  rownames(out) <- NULL
  out
}
