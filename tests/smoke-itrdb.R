# Smoke test on real files: drives server.R headlessly (shiny::testServer)
# over a sample of raw ITRDB .rwl files, the way a user would: load the
# file, look at each panel, make and undo an edit, download, and (for some
# files) make every report and date a floater. It records, per file and
# step, whether the step worked, what message the app showed, and how long
# it took. It does not check that the numbers are right (tests/test-server.R
# does that on the example data): it looks for crashes, unhelpful messages
# and slow steps.
#
# Needs the ITRDB clone next to the app (../../itrdbMeasurementsClone).
# Run from the app directory:
#   Rscript --vanilla tests/smoke-itrdb.R [n = 300] [cores = 8] [out = smoke-itrdb.rds] [big = yes]
# big = no leaves out the ten files with the most series and the ten with
# the most years: they take over an hour between them.
# With the CRAN dplR:
#   Rscript --vanilla -e '.libPaths(c(path.expand("~/R/cran-dplR"), .libPaths()));
#     source("tests/smoke-itrdb.R")'
args  <- commandArgs(trailingOnly = TRUE)
nFile <- if (length(args) >= 1) as.integer(args[1]) else 300L
cores <- if (length(args) >= 2) as.integer(args[2]) else 8L
out   <- if (length(args) >= 3) args[3] else "smoke-itrdb.rds"
big   <- !(length(args) >= 4 && args[4] == "no")
clone <- "../../itrdbMeasurementsClone"

suppressPackageStartupMessages(source("ui.R"))
options(xdater.debounce.ms = 0)
appServer <- source("server.R")$value
srv <- appServer
formals(srv) <- formals(function(input, output, session) NULL)

# ── The sample ───────────────────────────────────────────────────────────────
# Mostly ring-width files at random, plus the cases most likely to hurt:
# the files with the most series and the most years, the smallest, some
# files that are not ring width, and files dplR could not read.
meta  <- readRDS(file.path(clone, "Rdatafiles/rwls_meta.rds"))
rwls  <- readRDS(file.path(clone, "Rdatafiles/rwls.rds"))
paths <- list.files(file.path(clone, "data_files"), pattern = "[.]rwl$",
                    recursive = TRUE, full.names = TRUE)
names(paths) <- sub("[.]rwl$", "", basename(paths))
nser <- vapply(rwls, ncol, 1L); nyr <- vapply(rwls, nrow, 1L)
rw   <- meta$file[meta$variableShort %in% "total ring width"]
bad  <- setdiff(names(paths), names(rwls))
set.seed(4815)
pick <- unique(c(
  sample(rw, nFile),
  if (big) names(sort(nser[rw], decreasing = TRUE))[1:10],
  if (big) names(sort(nyr[rw],  decreasing = TRUE))[1:10],
  names(sort(nser[rw]))[1:10],
  sample(setdiff(meta$file, rw), 40),
  sample(bad, min(20, length(bad)))))
pick <- pick[pick %in% names(paths)]
# every fifth file also gets the reports and the floater
full <- pick[seq(1, length(pick), by = 5)]
rm(rwls)
cat(length(pick), "files;", length(full), "with reports and floater;", cores, "cores\n")

params <- list(seg.length = 50, bin.floor = "10", lowFreq = "none", n = "7",
               nyrs = 32, prewhiten = TRUE, ar.order.max = "NULL",
               pcrit = 0.05, biweight = TRUE, method = "spearman", lag.max = 5,
               rwlPlotType = "seg", lagCCF = 5, fixLast = TRUE, insertValue = 0.2,
               cofechaType = "html", minOverlapUndated = 50)

oneFile <- function(id) {
  pdf(NULL); on.exit(dev.off())
  log <- list()
  # Run one step. A validation message is what the app shows the user in
  # place of an output: recorded as "message". Anything else is an error.
  step <- function(name, expr, limit = 300) {
    t0  <- Sys.time()
    res <- tryCatch({
      setTimeLimit(elapsed = limit, transient = TRUE)
      force(expr); setTimeLimit(elapsed = Inf)
      c("ok", "")
    }, error = function(e) {
      setTimeLimit(elapsed = Inf)
      kind <- if (grepl("reached elapsed time limit", conditionMessage(e))) "TIMEOUT"
              else if (inherits(e, "shiny.silent.error") && !nzchar(conditionMessage(e))) "skipped"
              else if (inherits(e, "validation") || inherits(e, "shiny.silent.error")) "message"
              else "ERROR"
      c(kind, conditionMessage(e))
    })
    log[[length(log) + 1]] <<- data.frame(
      file = id, step = name, status = res[1], msg = substr(res[2], 1, 300),
      secs = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2))
    res[1] == "ok"
  }
  report <- function(name, output) {
    step(name, {
      f   <- output[[name]]
      txt <- paste(readLines(f, warn = FALSE), collapse = " ")
      if (grepl("Report not made", txt)) {
        stop(sub(".*could not make this report: ([^<]*)<.*", "\\1", txt))
      }
    })
  }
  size <- c(NA, NA)
  tryCatch(testServer(srv, {
    step("load", do.call(session$setInputs, c(list(
      file1 = data.frame(name = paste0(id, ".rwl"), size = 1, type = "",
                         datapath = paths[[id]])), params)))
    dat <- tryCatch(getRWL(), error = function(e) NULL)
    if (is.null(dat)) {
      # the app could not read it: what does it tell the user?
      log[[length(log) + 1]] <<- data.frame(
        file = id, step = "read", status = "unreadable",
        msg = substr(paste(tryCatch(datedRead()$error, error = conditionMessage), collapse = " "), 1, 300),
        secs = 0)
      step("unreadable: panels", { output$datedFileInfo; output$overviewUI; output$checkPanel })
    } else {
      size <<- dim(dat)
      step("overview: checks", { rwlCheck(); output$checkPanel })
      step("overview: summary", { output$overviewUI; output$rwlSummaryHeader; output$rwlPlot; output$rwlSummary })
      step("corr: corr.rwl.seg", getCRS())
      step("corr: flags", crsFlags())
      step("corr: outputs", { output$qaAlertCorr; output$crsPlotUI; output$crsFancyPlot; output$crsOverall
                              output$crsAvgCorrBin; output$crsFlags; output$crsCorrBin
                              output$cofechaNote; output$flaggedSeriesUI })
      # the series a user would open first: a flagged one if there is one
      fl <- tryCatch(crsFlags(), error = function(e) NULL)
      s  <- if (!is.null(fl) && nrow(fl) > 0) as.character(fl$series[1]) else colnames(dat)[1]
      sp <- range(as.numeric(rownames(dat))[!is.na(dat[[s]])])
      step("series: select", session$setInputs(
        series = s, rangeCCF = sp, winCenter = round(mean(sp)), winWidth = 40))
      step("series: plots", { output$rangeCCF; output$cssPlot; output$ccfPlot })
      step("edit: plots", { output$xskelPlot; output$series2edit; output$table1; output$editEffectUI })
      step("edit: insert", {
        tab <- seriesTable()
        session$setInputs(table1_rows_selected = max(1, nrow(tab) %/% 2), insertRows = 1)
        output$editEffectUI; output$editLog; getCRS()
      })
      step("edit: delete", {
        session$setInputs(table1_rows_selected = max(1, nrow(seriesTable()) %/% 3), deleteRows = 1)
        output$editEffectUI; output$xskelPlot
      })
      step("download", {
        back <- suppressMessages(read.rwl(output$downloadRWL, verbose = FALSE))
        if (ncol(back) != ncol(rwlRV$dated)) {
          stop("download has ", ncol(back), " series, the app has ", ncol(rwlRV$dated))
        }
      })
      if (id %in% full) {
        for (r in c("rwlSummaryReport", "crsReport", "cofechaReport", "cssReport", "editReport")) {
          report(r, output)
        }
      }
      step("edit: undo", { session$setInputs(undoEdit = 1); session$setInputs(undoEdit = 2) })
      step("edit: undone", if (!identical(rwlRV$dated, rwlRV$datedVault)) stop("undoing both edits did not give back the file"))
      if (id %in% full && ncol(dat) >= 6) {
        # Floater: the longest series, renumbered from year 1, dated against
        # the rest of the file
        len <- colSums(!is.na(dat)); fs <- names(len)[which.max(len)]
        x   <- dat[[fs]]; x <- x[!is.na(x)]
        und <- as.rwl(data.frame(FLOAT1 = x, row.names = seq_along(x)))
        uf  <- tempfile(fileext = ".rwl")
        suppressMessages(write.tucson(und, uf, prec = 0.001))
        step("floater: load", {
          session$setInputs(leaveOut = fs, updateMasterButton = 1)
          session$setInputs(file2 = data.frame(name = "floater.rwl", size = 1, type = "", datapath = uf))
          session$setInputs(series2 = "FLOAT1")
        })
        step("floater: search", getFloater())
        step("floater: outputs", { output$floaterUI; output$floaterControls; output$floaterText
                                   output$floaterPositionUI; output$floaterCandsTable
                                   output$floaterPlot })
        step("floater: segments", { output$floaterSegsUI; output$floaterSegsTable; output$ccfPlotUndated })
        step("floater: found", {
          pf  <- placedFloater()
          sp2 <- max(as.numeric(rownames(dat))[!is.na(dat[[fs]])])
          if (pf$last != sp2) {
            stop("best fit ends ", pf$last, ", the series really ends ", sp2,
                 " (r = ", round(pf$bestR, 2), ")")
          }
        })
        step("floater: save", { session$setInputs(saveDates = 1); output$dateLog; output$downloadUndatedRWL })
        report("undatedReport", output)
      }
    }
  }), error = function(e) {
    log[[length(log) + 1]] <<- data.frame(file = id, step = "session", status = "ERROR",
                                          msg = substr(conditionMessage(e), 1, 300), secs = NA)
  })
  res <- do.call(rbind, log)
  res$nyr <- size[1]; res$nser <- size[2]
  res
}

# Separate R processes, not forks: forked R crashes on macOS once the
# graphics and web libraries are loaded.
cl <- parallel::makeCluster(cores)
parallel::clusterExport(cl, c("paths", "full", "params", "oneFile"))
invisible(parallel::clusterEvalQ(cl, {
  suppressPackageStartupMessages(source("ui.R"))
  options(xdater.debounce.ms = 0)
  srv <- source("server.R")$value
  formals(srv) <- formals(function(input, output, session) NULL)
  NULL
}))
res <- parallel::clusterApplyLB(cl, pick, function(id) {
  tryCatch(suppressMessages(suppressWarnings(oneFile(id))), error = function(e) {
    data.frame(file = id, step = "worker", status = "ERROR",
               msg = substr(conditionMessage(e), 1, 300), secs = NA, nyr = NA, nser = NA)
  })
})
parallel::stopCluster(cl)
lost <- !vapply(res, is.data.frame, TRUE)
if (any(lost)) {
  res[lost] <- lapply(pick[lost], function(id) data.frame(
    file = id, step = "worker", status = "ERROR", msg = "the R process died", secs = NA, nyr = NA, nser = NA))
}
res <- do.call(rbind, res)
res$dplR <- as.character(packageVersion("dplR"))
saveRDS(res, out)

cat("\ndplR", res$dplR[1], "\n")
print(table(res$step, res$status)[unique(res$step), , drop = FALSE])
