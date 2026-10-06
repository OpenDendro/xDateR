# Drives server.R headlessly through the main workflow with shiny::testServer.
# Run from the app directory, outside renv so the dev dplR is used:
#   Rscript --vanilla tests/test-server.R
suppressPackageStartupMessages({
  source("ui.R")          # loads the packages the app uses
})
# Settings take effect at once in the tests; one block below checks the
# debounce itself
options(xdater.debounce.ms = 0)
appServer <- source("server.R")$value
# testServer() wants (input, output, session) in that order. Reordering the
# formals keeps the body, so the test code sees the server's reactives.
srv <- appServer
formals(srv) <- formals(function(input, output, session) NULL)
ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE); cat("\nok  ", msg) }
pdf(NULL)

# Run the R code a report prints in its last code block and return `env`.
reportCode <- function(html) {
  txt <- paste(readLines(html, warn = FALSE), collapse = "\n")
  blocks <- regmatches(txt, gregexpr("<pre><code>.*?</code></pre>", txt))[[1]]
  code <- gsub("</?pre>|</?code>", "", blocks)
  code <- vapply(code, function(x) xml2::xml_text(xml2::read_html(paste0("<p>", x, "</p>"))), "")
  unname(code)
}

params <- list(seg.length = 50, bin.floor = "10", lowFreq = "none", n = "7",
               nyrs = 32, prewhiten = TRUE, ar.order.max = "NULL",
               pcrit = 0.05, biweight = TRUE, method = "spearman", lag.max = 5,
               rwlPlotType = "seg", lagCCF = 5, fixLast = TRUE, insertValue = 0.2)

# ── Demo data: master filter, parameters, edits, downloads, reports ──────────
testServer(srv, {
  do.call(session$setInputs, c(list(useDemoDated = 1), params))
  ok(ncol(rwlRV$dated) == 18, "demo file loaded into the working copy")
  crs <- getCRS()
  ok(nrow(crs$overall) == 18, "corr.rwl.seg on all 18 series")

  session$setInputs(lowFreq = "spline", nyrs = 32, ar.order.max = "3")
  p <- xdParams()
  ok(is.null(p$n) && p$nyrs == 32 && p$ar.order.max == 3, "spline + AR order reach the parameters")
  ok(nrow(getCRS()$overall) == 18, "corr.rwl.seg with nyrs = 32, ar.order.max = 3")
  session$setInputs(lowFreq = "hanning", n = "9")
  ok(xdParams()$n == 9 && is.null(xdParams()$nyrs), "Hanning n; nyrs unset")
  session$setInputs(prewhiten = FALSE)
  ok(is.null(xdParams()$ar.order.max), "ar.order.max dropped when not prewhitening")
  session$setInputs(lowFreq = "none", prewhiten = TRUE, ar.order.max = "NULL")

  # ── Step 2: lag search, A/B flags, checks, COFECHA-style report ──
  ok(crs$lag.max == 5 && !is.null(crs$best.lag), "corr.rwl.seg searches lags to +/-5")
  fl <- crsFlags()
  ok(all(fl$best.lag[fl$series == "ABC104" & fl$flag == "B"] == 1) &&
       all(fl$best.lag[fl$series == "ABC118" & fl$flag == "B"] == -1),
     "demo's planted errors flagged B: ABC104 at +1 (false ring), ABC118 at -1 (missing ring)")
  output$crsFlags; output$crsFancyPlot; output$flaggedSeriesUI
  ok(TRUE, "flags table, tile plot and Series banner render")
  # A collection with nothing flagged: no lag search and a pcrit no segment fails
  session$setInputs(lag.max = 0, pcrit = 0.999)
  ok(nrow(crsFlags()) == 0, "settings that flag nothing")
  output$crsFlags; output$flaggedSeriesUI
  ok(TRUE, "flags table and Series banner render with no flagged segments")
  ok(file.exists(output$crsReport), "correlation report renders with no flagged segments")
  session$setInputs(pcrit = 0.05, lag.max = 5)
  session$setInputs(lag.max = 50)
  msg <- tryCatch({getCRS(); ""}, error = function(e) conditionMessage(e))
  ok(grepl("must be less than the segment length", msg), "lag search >= segment length refused with a message")
  # a report asked for while the settings are invalid explains itself
  bad <- paste(readLines(output$crsReport, warn = FALSE), collapse = " ")
  ok(grepl("Report not made", bad) && grepl("must be less than the segment length", bad),
     "a report that cannot be made says why, instead of a server error page")
  session$setInputs(lag.max = 5)
  chk <- rwlCheck()
  ok(any(chk$check == "RWL_DATING_LAG"), "rwl.check finds the dating error in the demo file")
  panel <- output$checkPanel$html
  ok(grepl("ABC118 may be misdated by 1 year: probably a missing ring", panel, fixed = TRUE),
     "data checks panel leads with the plain-language dating finding")
  ok(identical(flaggedSeries(), "ABC118"), "ABC118 is marked in the series selector")
  ok(lengths(regmatches(panel, gregexpr("RWL_DATING_LAG", panel))) == 1 &&
       grepl("Also: </strong>", panel, fixed = TRUE) &&
       !grepl("most likely a missing ring", panel),
     "ABC118's two findings share one card, without repeating the title")
  ok(grepl("flagged on the Correlations panel", panel) && grepl("ABC104 (better at lag +1)", panel, fixed = TRUE),
     "the Overview points to segment flags, naming ABC104, which rwl.check() does not flag")
  output$crsOverall; output$crsAvgCorrBin; output$crsCorrBin
  ok(TRUE, "correlation tables render with the new shading")
  for (type in c("html", "text", "markdown")) {
    session$setInputs(cofechaType = type)
    txt <- paste(readLines(output$cofechaReport, warn = FALSE), collapse = "\n")
    ok(grepl("CORRELATION OF SERIES BY SEGMENTS", txt, ignore.case = TRUE) && grepl("ABC118", txt),
       paste("COFECHA-style report downloads as", type))
  }

  nms <- colnames(rwlRV$dated)
  session$setInputs(leaveOut = "ABC104", updateMasterButton = 1)
  ok(identical(rwlRV$excluded, "ABC104"), "filter records the excluded series")
  ok(ncol(masterRWL()) == 17 && ncol(rwlRV$dated) == 18, "master has 17, working copy keeps 18")
  ok(!"ABC104" %in% rownames(getCRS()$overall), "excluded series is out of corr.rwl.seg")

  # The excluded series can still be selected and tested against the master
  session$setInputs(series = "ABC104", rangeCCF = c(1300, 1950),
                    winCenter = 1500, winWidth = 40)
  si <- seriesInputs("ABC104")
  ok(ncol(si$rwl) == 17 && !"ABC104" %in% colnames(si$rwl), "excluded series tested against 17-series master")
  output$cssPlot; output$ccfPlot; output$xskelPlot
  ok(TRUE, "Series and Edit plots render for an excluded series")

  # Edit: delete a ring, then Update Master must not lose it
  tab <- seriesTable()
  yr  <- tab$Year[10]
  session$setInputs(table1_rows_selected = 10, deleteRows = 1)
  ok(nrow(rwlRV$editDF) == 1 && sum(!is.na(rwlRV$dated$ABC104)) == nrow(tab) - 1, "ring deleted")
  session$setInputs(table1_rows_selected = 20, insertRows = 1)
  ok(nrow(rwlRV$editDF) == 2, "ring inserted")
  edited <- rwlRV$dated
  session$setInputs(leaveOut = character(0), updateMasterButton = 2)
  ok(identical(rwlRV$dated, edited), "Update Master keeps the edits")
  ok(length(rwlRV$excluded) == 0, "series back in the master")

  # Download keeps every series and the edits
  session$setInputs(leaveOut = "ABC111", updateMasterButton = 3)
  f <- output$downloadRWL
  back <- suppressMessages(read.rwl(f, verbose = FALSE))
  ok(ncol(back) == 18, "download has all 18 series although one is out of the master")
  ok(isTRUE(all.equal(back$ABC104[!is.na(back$ABC104)], edited$ABC104[!is.na(edited$ABC104)])), "download holds the edited series")

  # Reports render, and the edit report's R code reproduces the edits
  for (r in c("rwlSummaryReport", "crsReport", "cssReport", "editReport")) {
    ok(file.exists(output[[r]]), paste("report renders:", r))
  }
  code <- reportCode(output$editReport)
  env <- new.env()
  wd <- setwd("data"); on.exit(setwd(wd))
  eval(parse(text = sub('read.rwl\\("DemoData.rwl"\\)', 'read.rwl("xDateRtest.rwl")', paste(code[length(code) - 1:0], collapse = "\n"))), envir = env)
  setwd(wd)
  ok(isTRUE(all.equal(as.data.frame(unclass(env$dat)), as.data.frame(unclass(rwlRV$dated)), check.attributes = FALSE)),
     "edit report R code reproduces the app's data exactly")
  # The Overview report's R code, run on the original file, rebuilds the
  # app's edited data and the same rwl.check() findings
  ovCode <- reportCode(output$rwlSummaryReport)
  ovCode <- gsub('"xDateRtest.rwl"', '"data/xDateRtest.rwl"', ovCode[length(ovCode)], fixed = TRUE)
  ovEnv <- new.env(); pdf(NULL)
  eval(parse(text = ovCode), envir = ovEnv)
  ok(isTRUE(all.equal(as.data.frame(unclass(ovEnv$dat), check.names = FALSE),
                      as.data.frame(unclass(rwlRV$dated), check.names = FALSE), check.attributes = FALSE)),
     "Overview report R code rebuilds the edited data")
  appChk <- rwlCheck(); repChk <- as.data.frame(ovEnv$chk)
  ok(identical(sort(paste(appChk$check, appChk$series)), sort(paste(repChk$check, repChk$series))),
     "Overview report R code gives the same rwl.check() findings")
  ovHtml <- paste(readLines(output$rwlSummaryReport, warn = FALSE), collapse = "\n")
  ok(grepl("Edits applied", ovHtml) && grepl("Data checks", ovHtml) && grepl("RWL_DATING_LAG", ovHtml),
     "Overview report shows the edits and the data checks")

  crsCode <- reportCode(output$crsReport)
  ok(any(grepl("lag.max = 5", crsCode)), "correlation report code includes lag.max")
  ok(any(grepl('setdiff(names(dat), c("ABC111"))', crsCode, fixed = TRUE)) &&
       any(grepl("nyrs = NULL", crsCode)) && any(grepl("ar.order.max = NULL", crsCode)),
     "correlation report code includes the filter, nyrs and ar.order.max")

  # Each report's R code, run on the original files, gives the app's numbers:
  # the code replays the edits (delete + insert on ABC104) and the filter
  runCode <- function(code) {
    code <- gsub('"xDateRtest.rwl"', '"data/xDateRtest.rwl"', code, fixed = TRUE)
    code <- gsub('"xDateRtestUndated.rwl"', '"data/xDateRtestUndated.rwl"', code, fixed = TRUE)
    e <- new.env(); eval(parse(text = code), envir = e); e
  }
  e <- runCode(crsCode[length(crsCode)])
  ok(isTRUE(all.equal(e$crs$spearman.rho, getCRS()$spearman.rho)) &&
       isTRUE(all.equal(e$crs$best.lag, getCRS()$best.lag)),
     "correlation report R code reproduces the app's correlations (edits + filter)")
  cssCode <- reportCode(output$cssReport)
  e <- runCode(cssCode[length(cssCode)])
  p <- xdParams()
  appCss <- do.call(corr.series.seg, c(seriesInputs(input$series),
                                       p[c("seg.length", "bin.floor", normArgs, "pcrit", "method")],
                                       list(make.plot = FALSE)))
  appCcf <- seriesCCF()
  ok(isTRUE(all.equal(e$css$spearman.rho, appCss$spearman.rho)) &&
       isTRUE(all.equal(e$ccfObject, appCcf)),
     "series report R code reproduces the app's series and cross-correlations")

  # Revert
  session$setInputs(revertSeries = 1)
  ok(identical(rwlRV$dated, rwlRV$datedVault) && nrow(rwlRV$editDF) == 0, "revert restores the file")

  # Floater with the shared parameters, against an edited master
  session$setInputs(series = "ABC104", table1_rows_selected = 10, deleteRows = 2)
  ok(nrow(rwlRV$editDF) == 1, "an edit to the master before the floater")
  session$setInputs(useDemoUndated = 1, series2 = "ABC110", minOverlapUndated = 50,
                    lowFreq = "spline", nyrs = 32)
  fo <- getFloater()
  ok(!is.null(fo$floaterCorStats), "xdate.floater runs with nyrs")
  output$floaterPlot; output$ccfPlotUndated
  # ABC110 fits best overall, but its late segments fit better a year early
  runs <- lagRuns(crsFlagged(floaterSegs()), crsTested(floaterSegs()))
  ok(length(runs) >= 1 && any(grepl("lag -1", runs)) &&
       any(grepl("look for a false ring in the floater in 1805\u20131854, the first of these segments", runs)),
     "floater segments: a false ring in the first lagged segment (ABC110 has a duplicate at 1816)")
  ok(grepl("Candidate positions", output$floaterPositionUI$html) && nrow(floaterCands()) > 1 &&
       floaterCands()$r[1] == max(fo$floaterCorStats$r),
     "floater lists the candidate positions, the best fit first")
  ok(placedFloater()$isBest && identical(placedFloater()$rwlOut, fo$rwlOut),
     "the position in use starts as the best fit, the same series xdate.floater() returns")
  output$floaterSegsUI; output$floaterSegsTable
  ok(TRUE, "floater segment summary and table render")
  session$setInputs(saveDates = 1)
  ok(!is.null(rwlRV$undated2dated), "floater dates saved")
  ok(any(grepl("Note: .*lag -1", rwlRV$dateLog)), "date log notes the lagged segments")
  session$setInputs(appendMaster = TRUE)
  back <- suppressMessages(read.rwl(output$downloadUndatedRWL, verbose = FALSE))
  ok(ncol(back) == 18 && !"ABC111" %in% names(back),
     "undated download appends the 17 master series, not ABC111 (left out of the master)")
  ok(file.exists(output$undatedReport), "report renders: undatedReport")
  flCode <- reportCode(output$undatedReport)
  ok(any(grepl("min.overlap = 50", flCode)) && any(grepl("nyrs = 32", flCode)), "floater report code uses min.overlap and nyrs")
  e <- runCode(flCode[length(flCode)])
  ok(isTRUE(all.equal(e$fo$floaterCorStats, getFloater()$floaterCorStats)),
     "floater report R code reproduces the floater search (edited master + filter)")
  ok(isTRUE(all.equal(e$segs$spearman.rho, floaterSegs()$spearman.rho)) &&
       identical(e$segs$best.lag, floaterSegs()$best.lag),
     "floater report R code reproduces the segment table")

  # Choosing the position by hand: the second candidate
  best <- placedFloater()
  c2   <- floaterCands()[2, ]
  session$setInputs(floaterCandsTable_rows_selected = 2)
  pf <- placedFloater()
  ok(!pf$isBest && pf$how == "candidate" && pf$last == c2$last && pf$first == c2$first &&
       isTRUE(all.equal(pf$r, c2$r)) &&
       identical(row.names(pf$rwlOut), as.character(c2$first:c2$last)) &&
       identical(pf$rwlOut[[1]], best$rwlOut[[1]]),
     "choosing the second candidate places the same rings at its years")
  ok(grepl("chosen by you", output$floaterPositionUI$html) &&
       grepl("Back to the best fit", output$floaterPositionUI$html),
     "the panel says a chosen position is in use and offers the way back")
  ok(as.numeric(colnames(floaterSegs()$spearman.rho)[1]) < as.numeric(colnames(e$segs$spearman.rho)[1]) ||
       !identical(dim(floaterSegs()$spearman.rho), dim(e$segs$spearman.rho)) ||
       !isTRUE(all.equal(floaterSegs()$spearman.rho, e$segs$spearman.rho)),
     "the segment test follows the chosen position")
  output$floaterPlot; output$ccfPlotUndated; output$floaterSegsUI
  session$setInputs(saveDates = 2)
  ok(ncol(rwlRV$undated2dated) == 1 &&
       identical(range(as.numeric(row.names(rwlRV$undated2dated))), c(c2$first, c2$last)) &&
       any(grepl("saved again with dates .*Position chosen by the user.*the best fit was", rwlRV$dateLog)),
     "saving again replaces the dates and the log says the position was chosen")
  flCode <- reportCode(output$undatedReport)
  ok(any(grepl("The position used is not the best fit", flCode)), "floater report code says the position was chosen")
  e2 <- runCode(flCode[length(flCode)])
  ok(isTRUE(all.equal(e2$placed, pf$rwlOut, check.attributes = FALSE)) &&
       identical(row.names(e2$placed), row.names(pf$rwlOut)) &&
       isTRUE(all.equal(e2$segs$spearman.rho, floaterSegs()$spearman.rho)) &&
       identical(e2$segs$best.lag, floaterSegs()$best.lag),
     "floater report R code reproduces a chosen position and its segment table")

  # A year entered by hand, where the series does not overlap the master
  session$setInputs(floaterLast = 2500.5, floaterUseYear = 1)
  ok(placedFloater()$last == c2$last, "a last year that is not a whole number is refused")
  session$setInputs(floaterLast = 2500, floaterUseYear = 2)
  pf <- placedFloater()
  ok(pf$last == 2500 && is.na(pf$r) && pf$how == "manual" &&
       grepl("cannot be checked", output$floaterPositionUI$html),
     "a hand-entered year with no overlap is allowed, with a warning that it cannot be checked")
  ok(inherits(tryCatch(floaterSegs(), error = function(e) e), "error"),
     "no segment test where the position cannot be checked")
  output$floaterPlot
  session$setInputs(saveDates = 3)
  ok(max(as.numeric(row.names(rwlRV$undated2dated))) == 2500 &&
       any(grepl("Position entered by hand; it could not be checked", rwlRV$dateLog)),
     "the hand-entered dates are saved and the log says they could not be checked")
  ok(file.exists(output$undatedReport), "report renders for a position that cannot be checked")
  back <- suppressMessages(read.rwl(output$downloadUndatedRWL, verbose = FALSE))
  ok("ABC110" %in% names(back) && max(as.numeric(row.names(back))) == 2500,
     "the download holds the hand-dated series")

  # A hand-entered year that was searched is checked like any other
  session$setInputs(floaterLast = best$last - 1, floaterUseYear = 3)
  ok(!is.na(placedFloater()$r) && placedFloater()$how == "manual" && !placedFloater()$isBest,
     "a hand-entered year within the search has its correlation")
  session$setInputs(floaterUseBest = 1)
  ok(placedFloater()$isBest && identical(placedFloater()$rwlOut, best$rwlOut), "back to the best fit")
  # a new search drops the choice
  session$setInputs(floaterCandsTable_rows_selected = 2)
  session$setInputs(series2 = "ABC119")
  ok(placedFloater()$isBest, "choosing another series starts from its best fit")
})

# ── File with a gap: alert, skeleton plot, edits around it, fill ────────────
d <- suppressMessages(read.rwl("data/xDateRtest.rwl", verbose = FALSE))
idx <- which(!is.na(d$ABC105)); gapYrs <- time(d)[idx[100:104]]
d$ABC105[idx[100:104]] <- NA
gf <- tempfile(fileext = ".rwl")
suppressMessages(write.tucson(d, gf))

testServer(srv, {
  do.call(session$setInputs, c(list(file1 = data.frame(name = "gappy.rwl", size = 1,
                                                       type = "", datapath = gf)), params))
  ok(nrow(datedGaps()) == 1 && datedGaps()$first == gapYrs[1], "gap found on load")
  ok(nrow(getCRS()$overall) == 18, "correlations run with the gap (NA)")
  session$setInputs(lowFreq = "spline", nyrs = 32)
  msg <- tryCatch({getCRS(); ""}, error = function(e) conditionMessage(e))
  ok(grepl("ABC105", msg) && grepl("dplR stopped", msg), "spline on gapped data shows dplR's message naming the series")
  session$setInputs(lowFreq = "none")

  session$setInputs(series = "ABC105", winCenter = gapYrs[3], winWidth = 40)
  msg <- tryCatch({output$xskelPlot; ""}, error = function(e) conditionMessage(e))
  ok(grepl("no measurements for", msg), "skeleton plot explains the gap instead of crashing")

  # Delete a ring after the gap (fix last): the gap moves one year later,
  # and no measurement is lost or misdated
  tab <- seriesTable()
  ok(sum(is.na(tab$Value)) == 5, "edit table keeps the gap rows")
  # A gap row can't be deleted: nothing changes, nothing is logged
  before <- rwlRV$dated
  session$setInputs(table1_rows_selected = which(tab$Year == gapYrs[2]), deleteRows = 1)
  ok(identical(rwlRV$dated, before) && nrow(rwlRV$editDF) == 0,
     "deleting a gap row is refused and changes nothing")
  row <- which(tab$Year == gapYrs[5] + 50)
  session$setInputs(table1_rows_selected = row, deleteRows = 2)
  ok(datedGaps()$first == gapYrs[1] + 1, "after delete with fix.last the gap shifts with the rings before it")
  before <- d$ABC105[time(d) > gapYrs[5] + 50]
  after  <- rwlRV$dated$ABC105[as.numeric(rownames(rwlRV$dated)) > gapYrs[5] + 50]
  ok(identical(before, after), "rings after the deleted one keep their years")

  session$setInputs(fillSeries = "ABC105", fillMethod = "0", fillGapsButton = 1)
  ok(nrow(datedGaps()) == 0 && nrow(rwlRV$editDF) == 2, "fill with zero logged as an edit")
  ok(grepl("Gaps filled in ABC105.", output$checkPanel$html, fixed = TRUE),
     "panel says the gap was filled, not that it is still missing")
  session$setInputs(lowFreq = "spline", nyrs = 32)
  ok(nrow(getCRS()$overall) == 18, "spline works once the gap is filled")
  code <- reportCode(output$editReport)
  env <- new.env()
  eval(parse(text = sub('read.rwl\\("gappy.rwl"\\)', sprintf('read.rwl("%s")', gf),
                        paste(code[length(code) - 1:0], collapse = "\n"))), envir = env)
  ok(isTRUE(all.equal(as.data.frame(unclass(env$dat)), as.data.frame(unclass(rwlRV$dated)), check.attributes = FALSE)),
     "edit report replays delete + fill on the gapped file exactly")
})

# ── Numeric series names (common in ITRDB files, e.g. ak006) ──────────────
nd <- d; names(nd) <- as.character(704000 + seq_along(nd))   # d has the gap
nf <- tempfile(fileext = ".rwl"); suppressMessages(write.tucson(nd, nf))
testServer(srv, {
  do.call(session$setInputs, c(list(file1 = data.frame(name = "numeric.rwl", size = 1,
                                                       type = "", datapath = nf)), params))
  ok(identical(colnames(rwlRV$dated), names(nd)), "numeric series names kept on load")
  ok(datedGaps()$series == "704005", "gap found in a numerically named series")
  session$setInputs(fillSeries = "704005", fillMethod = "0", fillGapsButton = 1)
  ok(nrow(datedGaps()) == 0 && identical(colnames(rwlRV$dated), names(nd)), "fill works and keeps numeric names")
  session$setInputs(series = "704005", winCenter = 1700, winWidth = 40)
  tab <- seriesTable()
  session$setInputs(table1_rows_selected = 5, deleteRows = 1)
  ok(nrow(rwlRV$editDF) == 2, "ring edit on a numerically named series")
  ok(nrow(getCRS()$overall) == 18 && nrow(crsFlags()) >= 0, "correlations and flags with numeric names")
  back <- suppressMessages(read.rwl(output$downloadRWL, verbose = FALSE))
  ok(identical(names(back), names(nd)), "download keeps numeric names")
  code <- reportCode(output$editReport)
  env <- new.env()
  eval(parse(text = sub('read.rwl\\("numeric.rwl"\\)', sprintf('read.rwl("%s")', nf),
                        paste(code[length(code) - 1:0], collapse = "\n"))), envir = env)
  ok(isTRUE(all.equal(as.data.frame(unclass(env$dat), check.names = FALSE),
                      as.data.frame(unclass(rwlRV$dated), check.names = FALSE), check.attributes = FALSE)),
     "edit report replays fill + edit with numeric names")
})

# ── Data checks panel actions, and what an edit resolves ─────────────────
testServer(srv, {
  do.call(session$setInputs, c(list(useDemoDated = 1), params))
  session$setInputs(checkAction = list(action = "examine", series = list("ABC118"), nonce = 1))
  ok(TRUE, "Examine action runs")
  ok(!grepl("out of master", output$checkPanel$html) && length(rwlRV$excluded) == 0,
     "Overview offers no leave-out-of-master buttons; the master is untouched")
  # Fix ABC118's missing ring: a zero ring before 1791, keeping the last year
  session$setInputs(series = "ABC118", winCenter = 1700, winWidth = 40, insertValue = 0)
  row <- which(seriesTable()$Year == 1791)
  session$setInputs(table1_rows_selected = row, insertRows = 1)
  panel <- output$checkPanel$html
  ok(grepl("Resolved by your edits", panel) && grepl("ABC118 now fits best where it is dated.", panel, fixed = TRUE),
     "panel reports the dating error as resolved by the edit")
  ok(!"RWL_DATING_LAG" %in% rwlCheck()$check && length(flaggedSeries()) == 0,
     "the finding and the series marker are gone after the fix")
})

# ── Skeleton plot at the start of a series ───────────────────────────────
testServer(srv, {
  do.call(session$setInputs, c(list(useDemoDated = 1), params))
  session$setInputs(series = "ABC118", winCenter = 1505, winWidth = 20)
  msg <- tryCatch({output$xskelPlot; ""}, error = function(e) conditionMessage(e))
  ok(grepl("Prewhitening removes the first few years", msg),
     "skeleton plot near the start of a series explains prewhitening")
  session$setInputs(winCenter = 1700, winWidth = 20)
  msg <- tryCatch({output$xskelPlot; ""}, error = function(e) conditionMessage(e))
  ok(msg == "", "a 20-year window mid-series draws")
})

# ── Large files: typing waits for a pause, and results are cached ────────
options(xdater.debounce.ms = 500)
nCrs <- 0
# counts calls the app makes (it finds corr.rwl.seg on the search path)
trace("corr.rwl.seg", quote(nCrs <<- nCrs + 1), print = FALSE)
testServer(srv, {
  do.call(session$setInputs, c(list(useDemoDated = 1), params))
  session$elapse(600)
  getCRS(); n0 <- nCrs
  # typing "0.01" into P crit: "0.0" then "0.01", without a pause
  session$setInputs(pcrit = 0.0); session$setInputs(pcrit = 0.01)
  ok(xdParams()$pcrit == 0.05, "settings wait while typing")
  session$elapse(600)
  ok(xdParams()$pcrit == 0.01, "settings take effect after a pause")
  getCRS()
  ok(nCrs == n0 + 1, "one correlation run for the typed value, not one per keystroke")
  session$setInputs(pcrit = 0.05); session$elapse(600)
  getCRS()
  ok(nCrs == n0 + 1, "going back to earlier settings is served from the cache")
  # reverting edits is instant too: same data, same settings
  session$setInputs(series = "ABC104", winCenter = 1500, winWidth = 40,
                    table1_rows_selected = 10, deleteRows = 1)
  getCRS(); n1 <- nCrs
  session$setInputs(revertSeries = 1)
  getCRS()
  ok(nCrs == n1, "reverting edits is served from the cache")
  ok(!is.null(datedReport()$meanInterSeriesCor), "summary statistics computed once and shared")
})
untrace("corr.rwl.seg")
options(xdater.debounce.ms = 0)

# ── Short series: the message names the series and gives the numbers ────
sd1 <- suppressMessages(read.rwl("data/xDateRtest.rwl", verbose = FALSE))
cut <- function(x, n) { i <- which(!is.na(x)); x[i[seq_len(length(i) - n)]] <- NA; x }
sd1$ABC101 <- cut(sd1$ABC101, 40)
sf1 <- tempfile(fileext = ".rwl"); suppressMessages(write.tucson(sd1, sf1))
sd2 <- sd1; sd2$ABC102 <- cut(sd2$ABC102, 38)
sf2 <- tempfile(fileext = ".rwl"); suppressMessages(write.tucson(sd2, sf2))
testServer(srv, {
  p30 <- params; p30$seg.length <- 30
  do.call(session$setInputs, c(list(file1 = data.frame(name = "short1.rwl", size = 1, type = "", datapath = sf1)), p30))
  qa <- rwlQA()
  ok(qa$tier == 2 && qa$title == "Series ABC101 is short for 30-year segments",
     "one short series: the title names it")
  ok(grepl("^It has 40 rings; reliable crossdating at this segment length needs at least 45", qa$message),
     "one short series: the message gives its length and the length needed")
  ok(grepl("Series ABC101 is short", output$checkPanel$html) &&
       !grepl("too short for reliable crossdating at the current", output$checkPanel$html),
     "Overview card shows the new wording, not the repeated sentence")
  session$setInputs(file1 = data.frame(name = "short2.rwl", size = 1, type = "", datapath = sf2))
  qa <- rwlQA()
  ok(qa$title == "2 series are short for 30-year segments" &&
       grepl("These have fewer: ABC101 (40), ABC102 (38).", qa$message, fixed = TRUE),
     "several short series: counted in the title, listed with lengths in the message")
})

# ── Undated files: the same widget as dated files ────────────────────────
testServer(srv, {
  do.call(session$setInputs, c(list(useDemoDated = 1), params))
  session$setInputs(useDemoUndated = 1)
  ok(identical(undatedName(), "xDateRtestUndated.rwl") && ncol(getRWLUndated()) == 3,
     "example undated data loads from the link")
  uf <- tempfile(fileext = ".rwl")
  suppressMessages(write.tucson(getRWLUndated()[, 1:2], uf))
  session$setInputs(file2 = data.frame(name = "floaters.rwl", size = 1, type = "", datapath = uf))
  ok(identical(undatedName(), "floaters.rwl") && ncol(getRWLUndated()) == 2 &&
       length(ufilesRV$files) == 2, "a second undated file becomes current")
  session$setInputs(activeUndated = "xDateRtestUndated.rwl")
  ok(ncol(getRWLUndated()) == 3, "switching back to the first undated file")
})

# ── "Adjust plotted years" chooses what is drawn, not what is analysed ──
# arge016: a master series with a gap filled with zeros, cut to a window
# that starts inside the zeros, has no positive spline. Rebuilt here from
# the demo data: 15 zero rings in ABC105, window starting among them.
zd <- suppressMessages(read.rwl("data/xDateRtest.rwl", verbose = FALSE))
zi <- which(!is.na(zd$ABC105)); zyrs <- time(zd)[zi[100:114]]
zd$ABC105[zi[100:114]] <- 0
zf <- tempfile(fileext = ".rwl"); suppressMessages(write.tucson(zd, zf))
testServer(srv, {
  do.call(session$setInputs, c(list(file1 = data.frame(name = "zeros.rwl", size = 1, type = "", datapath = zf)), params))
  win <- c(zyrs[3], zyrs[3] + 150)
  session$setInputs(lowFreq = "spline", nyrs = 32, series = "ABC104", rangeCCF = win)
  si <- seriesInputs("ABC104")
  whole <- rwlRV$dated[, setdiff(colnames(rwlRV$dated), "ABC104"), drop = FALSE]
  ok(identical(si$rwl, whole) && length(si$series) == nrow(rwlRV$dated),
     "the master and the selected series are analysed whole")
  msg <- tryCatch({output$ccfPlot; ""}, error = function(e) conditionMessage(e))
  ok(msg == "", "cross-correlations draw with a window that starts inside a master series' zeros")
  # the segments are those of the whole-series analysis, and only the ones
  # inside the window are drawn
  res <- seriesCCF()
  full <- suppressMessages(ccf.series.rwl(si$rwl, series = si$series, seg.length = 50, bin.floor = 10,
                                          nyrs = 32, lag.max = 5, make.plot = FALSE))
  ok(identical(res, full), "the values are ccf.series.rwl() on the whole series")
  drawn <- plotCCF(res, from = win[1], to = win[2])$condlevels[[1]]
  yr1 <- as.numeric(sub("[.].*", "", drawn)); yr2 <- as.numeric(sub(".*[.]", "", drawn))
  ok(length(drawn) >= 1 && all(yr1 >= win[1]) && all(yr2 <= win[2]),
     "only segments wholly inside the window are drawn")
  # the series with the zeros itself, window starting inside its zeros:
  # this failed when the series was cut to the window before filtering
  session$setInputs(series = "ABC105", rangeCCF = win)
  msg <- tryCatch({output$ccfPlot; ""}, error = function(e) conditionMessage(e))
  ok(msg == "", "a window starting inside the selected series' own zeros draws")
  # a window too narrow for any segment says so
  session$setInputs(rangeCCF = c(win[1], win[1] + 20))
  msg <- tryCatch({output$ccfPlot; ""}, error = function(e) conditionMessage(e))
  ok(grepl("lies wholly inside", msg), "a window with no whole segment says to widen it")
  # cutting everything to the window, as before, fails on this data
  old <- tryCatch({ccf.series.rwl(suppressMessages(window(whole, win[1], win[2])),
                                  series = si$series[as.numeric(names(si$series)) >= win[1] & as.numeric(names(si$series)) <= win[2]],
                                  seg.length = 50, bin.floor = 10, nyrs = 32, make.plot = FALSE); ""},
                  error = function(e) conditionMessage(e))
  ok(grepl("ABC105 is not all positive", old), "cutting the data to the window is what failed")
})

# ── dplR errors come with advice the user can act on in the app ──────────
testServer(srv, {
  do.call(session$setInputs, c(list(file1 = data.frame(name = "gappy.rwl", size = 1, type = "", datapath = gf)), params))
  session$setInputs(lowFreq = "spline", nyrs = 32)
  msg <- tryCatch({getCRS(); ""}, error = function(e) conditionMessage(e))
  ok(grepl("dplR stopped", msg) && grepl("In xDateR you can: leave ABC105 out of the master", msg) &&
       grepl("fill the gap from the Overview panel", msg) && grepl("None or Hanning", msg),
     "a dplR error names what can be done in the app")
})

# ── Empty panels, and work that would be lost on leaving ─────────────────
testServer(srv, {
  do.call(session$setInputs, params)
  ok(grepl("Load a ring-width file to begin", output$noDataAllSeriesTab$html) &&
       grepl("Load a ring-width file to begin", output$noDataEditSeriesTab$html),
     "panels that need a file say so before one is loaded")
  session$setInputs(useDemoDated = 1)
  ok(is.null(output$noDataAllSeriesTab), "the prompt goes once a file is loaded")
  ok(length(unsaved$files) == 0 && !unsaved$floater, "nothing unsaved after loading")
  session$setInputs(series = "ABC104", winCenter = 1500, winWidth = 40,
                    table1_rows_selected = 10, deleteRows = 1)
  ok(identical(unsaved$files, "xDateRtest.rwl"), "an edit marks the file as unsaved")
  ok(grepl("downloadRWLside", output$datedFileInfo$html), "the sidebar offers the edited file once there are edits")
  f <- output$downloadRWLside
  ok(file.exists(f) && length(unsaved$files) == 0, "downloading the edited file clears the unsaved mark")
  session$setInputs(table1_rows_selected = 12, deleteRows = 2)
  session$setInputs(revertSeries = 1)
  ok(length(unsaved$files) == 0, "reverting clears it too")
})

# ── Undo, the Edit panel's before/after, and jumping from a flag ─────────
testServer(srv, {
  do.call(session$setInputs, c(list(useDemoDated = 1), params))
  session$setInputs(series = "ABC118", winCenter = 1700, winWidth = 40)
  # before any edit: what is wrong with the series, on the Edit panel
  e <- editEffect()
  ok(is.null(e$before) && isTRUE(all.equal(e$now$spearman.rho[1, ], getCRS()$spearman.rho["ABC118", ],
                                           check.attributes = FALSE)),
     "the Edit panel's test of a series matches the Correlations panel")
  ok(grepl("look for a missing ring in series ABC118 in 1755\u20131804, the last of these segments", output$editEffectUI$html) &&
       !grepl("As loaded", output$editEffectUI$html),
     "before editing, the Edit panel names the segment holding the missing ring (ABC118 lost 1797)")
  # two edits, then undo each
  vault <- rwlRV$dated
  session$setInputs(insertValue = 0, table1_rows_selected = which(seriesTable()$Year == 1791), insertRows = 1)
  afterFix <- rwlRV$dated
  html <- output$editEffectUI$html
  ok(grepl("As loaded", html) && grepl("Every segment now fits best where dated", html),
     "after the fix, the Edit panel compares as loaded with now and reports it resolved")
  e <- editEffect()
  ok(e$now$overall[1, 1] > e$before$overall[1, 1] + 0.3, "the correlation with the master rose after the fix")
  session$setInputs(table1_rows_selected = 5, deleteRows = 1)
  ok(nrow(rwlRV$editDF) == 2 && !identical(rwlRV$dated, afterFix), "a second edit")
  session$setInputs(undoEdit = 1)
  ok(identical(rwlRV$dated, afterFix) && nrow(rwlRV$editDF) == 1 && length(rwlRV$editLog) == 1,
     "undo takes back only the last edit, exactly")
  ok(identical(unsaved$files, "xDateRtest.rwl"), "the file is still marked unsaved after a partial undo")
  session$setInputs(undoEdit = 2)
  ok(identical(rwlRV$dated, vault) && nrow(rwlRV$editDF) == 0 && is.null(rwlRV$editLog) &&
       length(unsaved$files) == 0, "undoing the first edit returns to the file as loaded")
  # jump from a flagged row, and from a click on a tile
  centre <- function() as.numeric(sub('.*data-from="([0-9.]+)".*', "\\1", output$winCenter.ui$html))
  i  <- which(crsFlags()$series == "ABC118")[3]
  f1 <- crsFlags()[i, ]
  session$setInputs(crsFlags_rows_selected = i)
  ok(centre() == round((f1$from + f1$to) / 2 / 5) * 5,
     "clicking a flagged row puts the Edit window on that segment")
  session$setInputs(`plotly_click-crs` = '[{"curveNumber":0,"pointNumber":3,"x":1754.5,"y":16.4,"customdata":"ABC118"}]')
  ok(centre() == 1755, "clicking a tile does the same")
})

# ── Notes per series, report names, floater dates kept per dated file ────
testServer(srv, {
  do.call(session$setInputs, c(list(useDemoDated = 1), params))
  session$setInputs(series = "ABC118", datingNotes = "missing ring near 1800")
  session$setInputs(series = "ABC104", datingNotes = "false ring before 1450")
  ok(notes$dated[[noteKey("xDateRtest.rwl", "ABC118")]] == "missing ring near 1800" &&
       notes$dated[[noteKey("xDateRtest.rwl", "ABC104")]] == "false ring before 1450",
     "dating notes are kept separately for each series")
  ok(output$notesTitle == "Dating Notes for ABC104", "the notes card names the series")
  ok(grepl("^50-yr segments .* Spearman .* lags \u00b15", paste(paramSummary(xdParams()), collapse = " \u00b7 ")),
     "the settings summary gives the settings in effect")
  ok(reportName("ak006.rwl", "series-704071") == paste0("ak006-series-704071-", Sys.Date(), ".html"),
     "reports are named after the file, the report and the date")
  # series dated on the Floater panel stay with their dated file
  session$setInputs(useDemoUndated = 1, series2 = "ABC119", minOverlapUndated = 50)
  session$setInputs(saveDates = 1)
  ok(ncol(rwlRV$undated2dated) == 1, "a floater dated against the example data")
  session$setInputs(file1 = data.frame(name = "gappy.rwl", size = 1, type = "", datapath = gf))
  ok(is.null(rwlRV$undated2dated), "another dated file starts with no dated floaters")
  session$setInputs(activeFile = "xDateRtest.rwl")
  ok(!is.null(rwlRV$undated2dated) && names(rwlRV$undated2dated) == "ABC119" && length(rwlRV$dateLog) == 1,
     "switching back brings the dated floaters and their log back")
})

# ── The guided example, played through ───────────────────────────────────
testServer(srv, {
  do.call(session$setInputs, c(list(useDemoDated = 1, navbar = "OverviewTab"), params))
  ok(grepl("Show me how", output$guideUI$html), "the guide is offered with the example data, not started")
  session$setInputs(guideStart = 1)
  step <- function() guideIds[guideStep()]
  ok(step() == "checks" && grepl("step 1 of 7", output$guideUI$html), "the guide starts at step 1")
  ok(grepl("guideNext", output$guideUI$html) && !grepl("guideGo", output$guideUI$html),
     "on the step's own panel a reading step offers Next, not Take me there")
  session$setInputs(guideNext = 1)
  ok(step() == "find" && grepl("guideGo", output$guideUI$html),
     "Next finishes step 1, and step 2 (on another panel) offers Take me there")
  session$setInputs(navbar = "AllSeriesTab")
  ok(!grepl("guideGo", output$guideUI$html) && grepl("finishes when you have done it", output$guideUI$html),
     "on the panel of a doing step there is no button")
  session$setInputs(series = "ABC118", navbar = "IndividualSeriesTab")
  ok(step() == "look", "opening ABC118 on the Series panel completes step 2")
  session$setInputs(navbar = "EditSeriesTab", winCenter = 1795, winWidth = 40)
  ok(step() == "fix", "opening the Edit panel completes step 3")
  # an edit that does not fix the series does not complete the step
  session$setInputs(table1_rows_selected = 3, deleteRows = 1)
  ok(step() == "fix", "an edit that does not clear the flags leaves the step open")
  session$setInputs(undoEdit = 1)
  # the guide's answer: a 0.25 mm ring above 1798
  session$setInputs(insertValue = 0.25, table1_rows_selected = which(seriesTable()$Year == 1798), insertRows = 1)
  ok(step() == "confirm", "inserting the ring above 1798 completes step 4")
  data(co021, package = "dplR")
  vals <- function(x) x[!is.na(x)]
  ok(isTRUE(all.equal(vals(rwlRV$dated$ABC118), vals(co021[["645221"]]))),
     "the guide's answer for ABC118 gives back the original co021 series")
  session$setInputs(navbar = "OverviewTab")
  ok(step() == "confirm", "step 5 waits on the Overview until the user has read it")
  session$setInputs(guideNext = 2)
  ok(step() == "own" && grepl("Show the answer", output$guideUI$html), "Next on the Overview completes step 5")
  session$setInputs(guideAnswer = 1)
  ok(grepl("1425 is a duplicate", output$guideUI$html), "the answer for ABC104 is shown on request")
  session$setInputs(series = "ABC104", navbar = "EditSeriesTab", winCenter = 1425, winWidth = 40)
  session$setInputs(table1_rows_selected = which(seriesTable()$Year == 1425), deleteRows = 2)
  ok(step() == "save", "deleting ABC104's duplicate ring completes step 6")
  ok(isTRUE(all.equal(vals(rwlRV$dated$ABC104), vals(co021[["643143"]]))),
     "the guide's answer for ABC104 gives back the original co021 series")
  ok(nrow(crsFlags()) == 0, "with both fixed, nothing is flagged")
  f <- output$downloadRWLside
  ok(step() == "save", "the edited file alone does not finish the guide")
  r <- output$editReport
  session$flushReact()   # the app asks for this itself after a download
  ok(is.na(guideStep()) && grepl("Guided example finished", output$guideUI$html),
     "downloading the file and the edit report finishes the guide")
})
testServer(srv, {
  do.call(session$setInputs, c(list(file1 = data.frame(name = "gappy.rwl", size = 1, type = "", datapath = gf)), params))
  ok(is.null(output$guideUI), "no guide for the user's own files")
})

# ── The Floater guide, played through, and the fix it ends with ─────────
testServer(srv, {
  do.call(session$setInputs, c(list(useDemoDated = 1, navbar = "OverviewTab"), params))
  ok(grepl("dating a floater", output$guideUIF$html), "the Floater guide is offered with the example data")
  session$setInputs(guideStart = 1, guideStartF = 1)
  stepF <- function() guideIdsF[guideStepF()]
  ok(stepF() == "load" && !guideRV$on && grepl("guideGoF", output$guideUIF$html),
     "starting the Floater guide hides the first guide and offers Take me there")
  session$setInputs(navbar = "UndatedSeriesTab", useDemoUndated = 1)
  ok(stepF() == "fit", "loading the undated example completes step 1")
  session$setInputs(series2 = "ABC119", minOverlapUndated = 50)
  ok(grepl("guideNextF", output$guideUIF$html), "a reading step on the Floater panel offers Next")
  session$setInputs(guideNextF = 1); session$setInputs(guideNextF = 2)
  ok(stepF() == "save", "Next twice reaches the save step")
  session$setInputs(saveDates = 1)
  ok(stepF() == "pick", "saving ABC119's dates completes step 4")
  session$setInputs(series2 = "ABC110")
  ok(stepF() == "inside", "choosing ABC110 completes step 5")
  ok(any(grepl("look for a false ring in the floater in 1805", lagRuns(crsFlagged(floaterSegs()), crsTested(floaterSegs())))),
     "the panel says what the guide says: a false ring in 1805-1854")
  session$setInputs(guideNextF = 3)
  f <- output$downloadUndatedRWL; session$flushReact()
  ok(stepF() == "download", "a download without ABC110 saved does not finish the guide")
  session$setInputs(saveDates = 2, appendMaster = TRUE)
  f <- output$downloadUndatedRWL; session$flushReact()
  ok(is.na(guideStepF()) && grepl("Floater guide finished", output$guideUIF$html), "saving ABC110 and downloading finishes the guide")
  # what the closing message says to do next: load that file as a dated
  # file, delete ABC110's duplicate 1817 with Fix Last Year unchecked
  session$setInputs(file1 = data.frame(name = "dated-floaters.rwl", size = 1, type = "", datapath = f))
  ok(all(c("ABC110", "ABC119", "ABC118") %in% colnames(rwlRV$dated)), "the download holds the floaters and the master")
  ok(any(crsFlags()$series == "ABC110") && is.null(output$guideUIF), "ABC110 is flagged in it; no guide on the user's own file")
  session$setInputs(series = "ABC110", winCenter = 1815, winWidth = 40, fixLast = FALSE)
  tab <- seriesTable()
  ok(all(tab$Value[tab$Year %in% 1816:1817] == 0.5), "rings 1816 and 1817 of ABC110 are the duplicate pair")
  session$setInputs(table1_rows_selected = which(tab$Year == 1817), deleteRows = 1)
  ok(!any(crsFlags()$series == "ABC110"), "deleting 1817 with Fix Last Year unchecked clears ABC110's flags")
})

# ── Switching files keeps each file's work ────────────────────────────────
testServer(srv, {
  do.call(session$setInputs, c(list(useDemoDated = 1), params))
  session$setInputs(series = "ABC104", winCenter = 1500, winWidth = 40,
                    table1_rows_selected = 10, deleteRows = 1)
  session$setInputs(leaveOut = "ABC111", updateMasterButton = 1)
  edited <- rwlRV$dated
  ok(nrow(rwlRV$editDF) == 1, "edit made on the example data")
  session$setInputs(file1 = data.frame(name = "gappy.rwl", size = 1, type = "", datapath = gf))
  ok(identical(datedName(), "gappy.rwl") && nrow(rwlRV$editDF) == 0 &&
       length(rwlRV$excluded) == 0, "a new file starts with no edits and the full master")
  session$setInputs(activeFile = "xDateRtest.rwl")
  ok(identical(rwlRV$dated, edited) && nrow(rwlRV$editDF) == 1 &&
       identical(rwlRV$excluded, "ABC111"), "switching back restores the edits and the master filter")
  session$setInputs(activeFile = "gappy.rwl")
  ok(nrow(datedGaps()) == 1, "switching again shows the other file")
  session$setInputs(file1 = data.frame(name = "xDateRtest.rwl", size = 1, type = "",
                                       datapath = "data/xDateRtest.rwl"))
  ok(identical(datedName(), "xDateRtest.rwl") && nrow(rwlRV$editDF) == 0,
     "reloading a file of the same name replaces it and discards its edits")
  ok(length(filesRV$files) == 2, "two files in the switcher")
})

# ── Unreadable files ───────────────────────────────────────────────────────
testServer(srv, {
  bad <- tempfile(fileext = ".rwl"); writeLines("this is not a ring-width file", bad)
  session$setInputs(file1 = data.frame(name = "bad.rwl", size = 1, type = "", datapath = bad))
  ok(is.null(getRWL()) && !is.null(datedRead()$error), "bad dated file: error kept, no crash")
  session$setInputs(useDemoDated = 1, file2 = data.frame(name = "bad.rwl", size = 1, type = "", datapath = bad))
  ok(is.null(getRWLUndated()) && !is.null(undatedRead()$error), "bad undated file: error kept, no crash")
})

# ── A file with one series: every panel says why, none stops with an error ──
testServer(srv, {
  one <- read.rwl("data/xDateRtest.rwl", verbose = FALSE)[, "ABC104", drop = FALSE]
  f <- tempfile(fileext = ".rwl"); suppressMessages(write.tucson(one, f, prec = 0.01))
  do.call(session$setInputs, c(list(file1 = data.frame(name = "one.rwl", size = 1, type = "", datapath = f)), params))
  ok(ncol(rwlRV$dated) == 1, "a one-series file loads")
  said <- function(expr) tryCatch({expr; ""}, error = function(e) conditionMessage(e))
  ok(grepl("segment plot needs two or more series", said(output$rwlPlot)),
     "one series: the segment plot says to use the spaghetti plot")
  session$setInputs(rwlPlotType = "spag")
  output$rwlPlot; output$checkPanel; output$rwlSummary
  ok(TRUE, "one series: the Overview renders")
  session$setInputs(series = "ABC104", rangeCCF = c(1300, 1950), winCenter = 1500, winWidth = 40)
  for (o in c("cssPlot", "ccfPlot", "xskelPlot")) {
    ok(grepl("master has only 1 series", said(output[[o]])), paste("one series:", o, "says the master is too small"))
  }
  ok(grepl("master has only 1 series", said(getCRS())), "one series: Correlations says the master is too small")
  ok(grepl("master has only 1 series", paste(readLines(output$cssReport, warn = FALSE), collapse = " ")),
     "one series: the series report says why it was not made")
})

# ── An undated series with a gap inside it is not searched ──────────────────
testServer(srv, {
  und <- read.rwl("data/xDateRtestUndated.rwl", verbose = FALSE)
  und <- as.data.frame(und); mid <- which(!is.na(und$ABC119))[c(60, 61)]
  und[mid, "ABC119"] <- NA
  uf <- tempfile(fileext = ".csv")
  write.csv(cbind(year = as.numeric(rownames(und)), und), uf, row.names = FALSE, na = "")
  do.call(session$setInputs, c(list(useDemoDated = 1, minOverlapUndated = 50), params))
  session$setInputs(file2 = data.frame(name = "gap.csv", size = 1, type = "", datapath = uf))
  session$setInputs(series2 = "ABC119")
  ok(nrow(rwlGaps(getRWLUndated()[, "ABC119", drop = FALSE])) == 1, "an undated series with a two-year gap loads")
  msg <- tryCatch({getFloater(); ""}, error = function(e) conditionMessage(e))
  ok(grepl("Series ABC119 has no measurement for", msg) && grepl("can't be dated as one piece", msg) &&
       grepl("missing rings", msg),
     "a floater with a gap is refused, naming the years, not dated with the gap closed")
  session$setInputs(series2 = "ABC110")
  ok(!is.null(getFloater()$floaterCorStats), "the other series in that file are still dated")
  ok(grepl(">p<", output$floaterCandsTable) || grepl('"p"', output$floaterCandsTable), "the candidates table has a p column")
})

cat("all server checks passed\n")
