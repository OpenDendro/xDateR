# Checks for appHelpers.R and editRing.R. Run from the app directory:
#   Rscript --vanilla tests/test-helpers.R
suppressMessages(library(dplR))
source("appHelpers.R"); source("editRing.R")
ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg); cat("ok  ", msg, "\n") }
d <- read.rwl("data/xDateRtest.rwl", verbose = FALSE)
yrs <- as.numeric(rownames(d))

# A series that starts on the rwl's first year: insert with fix.last must
# extend the rwl back a year, not drop the first measurement.
s1 <- names(d)[!is.na(d[1, ])][1]
sp <- seriesSpan(d, s1)
e <- editRing(d, s1, "insert", year = sp[1] - 1, value = 0.5, fix.last = TRUE)
ok(seriesSpan(e, s1)[1] == sp[1] - 1 && seriesSpan(e, s1)[2] == sp[2], "insert at first ring, fix.last: first year moves back, last fixed")
ok(e[[s1]][1] == 0.5 && identical(e[[s1]][-1][seq_len(sum(!is.na(d[[s1]])))], d[[s1]][!is.na(d[[s1]])]), "insert at first ring keeps every original value")
ok(inherits(e, "rwl") && !is.null(attr(e, "dplR.provenance")), "class and provenance kept")
pl <- function(x) as.data.frame(unclass(x))
ok(isTRUE(all.equal(pl(e)[-1, names(d) != s1], pl(d)[, names(d) != s1], check.attributes = FALSE)), "other series untouched")

# Delete the first ring with fix.last (old app mis-numbered this case)
e <- editRing(d, s1, "delete", year = sp[1], fix.last = TRUE)
ok(identical(seriesSpan(e, s1), c(sp[1] + 1, sp[2])), "delete first ring, fix.last: span shrinks from the start")
e <- editRing(d, s1, "delete", year = sp[1] + 10, fix.last = FALSE)
ok(identical(seriesSpan(e, s1), c(sp[1], sp[2] - 1)), "delete, fix first: span shrinks from the end")

# Gaps: edits must keep the gap in place relative to the rings around it
g <- d; s <- "ABC105"; idx <- which(!is.na(g[[s]])); g[[s]][idx[100:104]] <- NA
gp <- rwlGaps(g)
ok(nrow(gp) == 1 && gp$n == 5 && gp$first == yrs[idx[100]], "rwlGaps finds the gap")
e <- editRing(g, s, "delete", year = yrs[idx[200]], fix.last = TRUE)
gp2 <- rwlGaps(e)
ok(gp2$first == gp$first + 1 && gp2$n == 5, "delete after gap with fix.last shifts the gap one year later")
e <- editRing(g, s, "delete", year = yrs[idx[200]], fix.last = FALSE)
ok(rwlGaps(e)$first == gp$first, "delete after gap fixing first year leaves the gap where it was")
ok(sum(!is.na(e[[s]])) == sum(!is.na(g[[s]])) - 1, "one measurement removed, nothing else lost")

# Fill
f <- fillGaps(g, s, 0)
ok(nrow(rwlGaps(f)) == 0 && all(f[[s]][idx[100]:idx[104]] == 0), "fill with zero")
f <- fillGaps(g, s, "Linear")
ok(nrow(rwlGaps(f)) == 0 && inherits(f, "rwl"), "linear fill, class kept")
ok(tucsonPrec(d) == 0.01 && tucsonPrec(f) == 0.001, "precision: 0.01 data stays 0.01; off-grid fill needs 0.001")

# Errors fail loudly
ok(inherits(tryCatch(editRing(d, s1, "insert", year = sp[1] + 5, value = -1), error = identity), "error"), "negative insert value refused")
ok(is.null(readRWLsafely("tests/test-helpers.R")$data), "unreadable file returns an error, not a crash")

# Every format the upload box offers reads through readRWLsafely(). (It once
# passed verbose = FALSE, which read.compact(), read.fh() and read.tridas()
# refuse.)
sameData <- function(a, b) isTRUE(all.equal(as.data.frame(unclass(a)), as.data.frame(unclass(b)),
                                            check.attributes = FALSE, tolerance = 1e-6))
fc <- tempfile(fileext = ".rwl"); suppressMessages(write.compact(d, fc))
r <- readRWLsafely(fc); ok(is.null(r$error) && sameData(r$data, d), "compact format reads back")
ft <- tempfile(fileext = ".xml"); suppressMessages(write.tridas(d, ft))
r <- readRWLsafely(ft); ok(is.null(r$error) && ncol(r$data) == ncol(d), "TRiDaS format reads")
fs <- tempfile(fileext = ".csv"); suppressMessages(write.sheet(d, fs))
r <- readRWLsafely(fs); ok(is.null(r$error) && sameData(r$data, d), "csv spreadsheet reads back")
# Heidelberg: dplR has no writer, so write three series by hand (1/100 mm,
# ten 6-character values per line, ending with the last ring's year)
fh <- tempfile(fileext = ".fh"); sub <- d[, 1:3]; lines <- character(0)
for (s in names(sub)) {
  x <- sub[[s]]; yrs <- as.numeric(rownames(sub))[!is.na(x)]; x <- x[!is.na(x)]
  lines <- c(lines, "HEADER:", paste0("KeyCode=", s), paste0("DateEnd=", max(yrs)),
             paste0("Length=", length(x)), "Unit=1/100 mm", "DATA:Tree")
  v <- sprintf("%6d", round(x * 100))
  lines <- c(lines, vapply(split(v, ceiling(seq_along(v) / 10)), paste, "", collapse = ""))
}
writeLines(lines, fh)
r <- readRWLsafely(fh)
ok(is.null(r$error) && isTRUE(all.equal(r$data[[names(sub)[1]]][!is.na(r$data[[1]])],
                                        sub[[1]][!is.na(sub[[1]])])), "Heidelberg format reads")
ok(argCode(list(n = NULL, method = "spearman"), c("n", "method"), ", ") == 'n = NULL, method = "spearman"', "argCode")
# Edit window bounds
b <- windowBounds(c(1500, 1900))
ok(b$center == 1700 && b$width == 40 && b$maxWidth == 100, "window starts mid-series, 40 wide, max 100")
b <- windowBounds(c(1500, 1900), center = 1515, width = 80)
ok(b$width == 20 && b$center - b$width / 2 >= 1500, "near an end the width shrinks so the window stays on the series")
b <- windowBounds(c(1500, 1900), center = 1505, width = 40)
ok(b$width == 20 && b$center >= 1510, "the window is never narrower than 20 years (the skeleton plot fails at 10)")
b <- windowBounds(c(1500, 1900), center = 2000, width = 40)
ok(b$center + b$width / 2 <= 1900, "a centre past the end is clamped onto the series")
b <- windowBounds(c(1500, 1903), center = 1903, width = 20)
ok(b$maxCenter + b$width / 2 <= 1903, "centre limit never puts the window past the series end")
# Plain-language titles for rwl.check() findings
cat0 <- rwl.check.catalogue()
f1 <- data.frame(check = "RWL_DATING_LAG", series = "ABC118", value = -1, stringsAsFactors = FALSE)
ok(checkTitle(f1, cat0) == "ABC118 may be misdated by 1 year: probably a missing ring", "dating-lag title")
f1$value <- 2
ok(checkTitle(f1, cat0) == "ABC118 may be misdated by 2 years: probably false rings", "dating-lag title, plural")
f2 <- data.frame(check = "RWL_TAB", series = NA, value = NA, stringsAsFactors = FALSE)
ok(checkTitle(f2, cat0) == "File contains tab characters", "fallback title from the catalogue")
# Resolved findings are phrased as what changed, grouped by kind
fx <- data.frame(check = c(rep("RWL_INTERNAL_NA", 4), "RWL_DATING_LAG", "RWL_TAB"),
                 series = c("704071", "704091", "704092", "704111", "ABC118", NA),
                 stringsAsFactors = FALSE)
rt <- resolvedText(fx, cat0)
ok("Gaps filled in 704071, 704091, 704092 and 704111." %in% rt, "resolved gaps read as filled")
ok("ABC118 now fits best where it is dated." %in% rt, "resolved dating lag reads as fixed")
ok("No longer flagged: file contains tab characters." %in% rt, "resolved fallback wording")
# Floater: runner-up position and runs of lagged segments
fcs <- data.frame(first = 101:110, last = 151:160,
                  r = c(0.1, 0.2, 0.3, 0.6, 0.9, 0.5, 0.2, 0.1, 0.4, 0.1))
ru <- floaterRunnerUp(fcs)
ok(ru$r == 0.4 && ru$last == 159, "runner-up skips positions within 2 years of the best")
fl <- data.frame(series = "X", from = c(1780, 1805, 1830), to = c(1829, 1854, 1879),
                 flag = c("A", "B", "B"), best.lag = c(0, -1, -1), weak = FALSE,
                 stringsAsFactors = FALSE)
seg <- function(from) data.frame(from = from, to = from + 49)
# correct segments before the run (a floater placed by its older part): the
# error is in the FIRST lagged segment, and lag -1 there means a FALSE ring
r1 <- lagRuns(fl, seg(c(1730, 1755, 1780, 1805, 1830)))
ok(length(r1) == 1 && grepl("^1805\u20131879: these segments fit better 1 year earlier \\(lag -1\\)", r1) &&
     grepl("look for a false ring in the floater in 1805\u20131854, the first of these segments", r1),
   "lagged run after correct segments: a false ring, in the first segment of the run")
# correct segments after the run (the usual case in a dated series): the
# error is in the LAST lagged segment, and lag -1 means a missing ring
r2 <- lagRuns(fl[2:3, ], seg(c(1805, 1830, 1855, 1880)), what = "series X")
ok(grepl("look for a missing ring in series X in 1830\u20131879, the last of these segments", r2),
   "lagged run before correct segments: a missing ring, in the last segment of the run")
flp <- fl[2:3, ]; flp$best.lag <- 1
ok(grepl("look for a false ring in series X in 1830\u20131879", lagRuns(flp, seg(c(1805, 1830, 1855, 1880)), what = "series X")),
   "lag +1 before correct segments: a false ring")
# correct on both sides: two errors that cancel
r3 <- lagRuns(fl[2:3, ], seg(c(1755, 1805, 1830, 1880)))
ok(grepl("two errors cancel: look for a missing ring in 1830\u20131879 and a false ring in 1805\u20131854", r3),
   "lagged run between correct segments: two compensating errors")
# nothing correct: the whole series is offset
ok(grepl("the whole of the floater may be dated 1 year too late", lagRuns(fl[2:3, ], seg(c(1805, 1830)))),
   "every segment lagged: the whole series is offset")
ok(length(lagRuns(fl[1, ], seg(c(1780, 1805)))) == 0, "no sentence without B segments")

# Advice for dplR errors, in terms of what the app can do
m1 <- "the 'nyrs' spline for series TR2804 is not all positive, so dividing by it would give negative or infinite indices. Remove the series, or detrend the data yourself"
a1 <- dplrAdvice(m1, c("TR2804", "TR2821", "j"))
ok(grepl("leave TR2804 out of the master", a1) && !grepl("TR2821", a1) && !grepl("leave j", a1) &&
     grepl("None or Hanning", a1), "advice names the series in the message, and only that one")
m2 <- "series ABC105 has internal NA values, and the 'nyrs' spline needs an unbroken series."
ok(grepl("fill the gap from the Overview panel", dplrAdvice(m2, c("ABC105", "ABC104"))), "advice for a gap offers the fill")
ok(dplrAdvice("some other dplR error", c("A", "B")) == "", "no advice when there is nothing specific to say")
for (m in c("shorten 'seg.length' or adjust 'bin.floor'",
            "'seg.length' can be at most 1/2 the number of years in 'rwl'",
            "number of overlapping years is less than 'seg.length'")) {
  ok(grepl("set a shorter Segment length in the Analysis Parameters (it is 50 years)", dplrAdvice(m, c("rwl", "A"), 50), fixed = TRUE) &&
       !grepl("leave", dplrAdvice(m, c("rwl", "A"), 50)),
     paste("advice for segments that do not fit:", substr(m, 1, 30)))
}
ok(grepl("more rings than the master has years", dplrAdvice("'x' and 'y' must have the same length", "A")),
   "advice when dplR 1.8.0 cannot search a floater longer than the master")
shortest <- names(d)[which.min(colSums(!is.na(d)))]
o1 <- oneSeries(d, shortest)
ok(inherits(o1, "rwl") && identical(rownames(o1), rownames(d)) && identical(names(o1), shortest) &&
     identical(o1[[1]], d[[shortest]]) && nrow(d[, shortest, drop = FALSE]) < nrow(d),
   "oneSeries keeps every year of the file, where [ trims to the series")
ok(grepl("leave 704071 out", dplrAdvice("series 704071 has internal NA values", c("704071", "70407"))) &&
     !grepl("70407 ", dplrAdvice("series 704071 has internal NA values", c("704071", "70407"))),
   "series names match as whole words")

# Finding bodies keep the evidence and drop what the title already says
fb <- data.frame(check = "RWL_DATING_LAG", series = "ABC118", value = -1, stringsAsFactors = FALSE,
                 message = "correlates best with the master at lag -1 (r = 0.559 against r = 0.332 as dated); the series may be misdated by 1 year(s), most likely a missing ring")
ok(checkBody(fb) == "Correlates best with the master at lag -1 (r = 0.559 against r = 0.332 as dated).",
   "dating-lag body gives the evidence without repeating the title")
fb2 <- data.frame(check = "RWL_TAB", series = NA, value = NA, message = "file contains 3 tab characters; they have no defined width", stringsAsFactors = FALSE)
ok(checkBody(fb2) == "File contains 3 tab characters; they have no defined width.", "other messages are kept whole")

# Replaying an edit log gives the same data as making the edits one by one
ed <- data.frame(series = c("ABC105", "ABC104", "ABC104"), year = c(NA, 1500, 1600),
                 value = c(NA, NA, 0.3), action = c("fill", "delete", "insert"),
                 fixLast = c(NA, TRUE, FALSE), fill = c("0", NA, NA), stringsAsFactors = FALSE)
step <- editRing(editRing(fillGaps(g, "ABC105", 0), "ABC104", "delete", year = 1500, fix.last = TRUE),
                 "ABC104", "insert", year = 1600, value = 0.3, fix.last = FALSE)
ok(identical(replayEdits(g, ed), step), "replayEdits matches the edits made one at a time")
ok(identical(replayEdits(g, ed[0, ]), g), "replaying no edits returns the data unchanged")
# p-values for display
ok(identical(fmtP(c(1.3e-199, 0.0004, 0.001, 0.0312, 0.5, NA)),
             c("< 0.001", "< 0.001", "0.001", "0.031", "0.500", "")), "p-values display without false precision")
# Placing a floater by its last year, and the candidate positions
und <- suppressMessages(read.rwl("data/xDateRtestUndated.rwl", verbose = FALSE))
fo  <- xdate.floater(d, und[, "ABC119"], series.name = "ABC119", make.plot = FALSE, verbose = FALSE)
best <- fo$floaterCorStats[which.max(fo$floaterCorStats$r), ]
pl  <- placeFloater(d, und[, "ABC119"], "ABC119", best$last)
ok(identical(pl$rwlOut, fo$rwlOut) && identical(pl$rwlCombined, fo$rwlCombined),
   "placing a floater at the best last year gives what xdate.floater() gives")
pl2 <- placeFloater(d, und[, "ABC119"], "ABC119", 1500)
ok(identical(range(as.numeric(rownames(pl2$rwlOut))), c(1500 - 193, 1500)), "placing by a chosen last year")
cand <- floaterCandidates(fo$floaterCorStats)
ok(nrow(cand) == 5 && cand$last[1] == best$last && all(diff(cand$r) <= 0) &&
     min(dist(cand$last)) > 2, "candidates: best first, in order, and more than 2 years apart")
ok(isTRUE(all.equal(cand$r[2], floaterRunnerUp(fo$floaterCorStats)$r)), "the second candidate is the next best position")
# The best fit by r on a short overlap against stronger evidence elsewhere
# (the numbers are bulg001's series 653111 from the ITRDB test)
fx <- data.frame(first = c(1533, 1556, 1721, 1608, 1300), last = c(1792, 1815, 1980, 1867, 1559),
                 r = c(0.338, 0.331, 0.281, 0.195, 0.10), p = c(5.2e-3, 1.44e-3, 4.85e-6, 1.27e-2, 0.2),
                 n = c(57, 80, 243, 132, 60))
ok(floaterStrongest(fx)$last == 1980 && floaterDisagree(fx), "strongest evidence is the long overlap, not the highest r")
ok(!floaterDisagree(fo$floaterCorStats) && floaterStrongest(fo$floaterCorStats)$last == cand$last[1],
   "the example floater: best fit and strongest evidence agree")
c2 <- floaterCandidates(fx, n = 2)
ok(nrow(c2) == 3 && identical(c2$last, c(1792, 1815, 1980)),
   "the strongest position is listed even when it is not among the top n by r")
ok(nrow(floaterCandidates(fx, n = 3)) == 3, "and not listed twice when it is")
ok(identical(fmtPsmall(c(0.0052, 4.85e-6, NA, 0.2, 0, 1e-30)), c("0.005", "5e-06", "", "0.200", "< 1e-10", "< 1e-10")),
   "small p-values keep their size, down to 1e-10")

cat("all helper checks passed\n")
