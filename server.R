source("plot.floater.R")
source("plotlyCRSFunc.R")

shinyServer(function(session, input, output) {
  
  # ── Reactive values ────────────────────────────────────────────────────────
  # Central store for mutable app state. Using reactiveValues rather than
  # reactive() for data that gets modified in place (e.g. edits to rwlRV$dated).
  # datedVault holds the pre-edit snapshot so edits can be reverted.
  rwlRV <- reactiveValues(
    dated          = NULL,
    undated        = NULL,
    editLog        = NULL,
    editDF         = data.frame(series  = NULL, year    = NULL,
                                value   = NULL, action  = NULL,
                                fixLast = NULL),
    seriesDF       = NULL,
    datedNoSeries  = NULL,
    datedVault     = NULL,
    undatedVault   = NULL,
    undated2dated  = NULL,
    dateLog        = NULL,
    readError      = NULL   # set by tryCatch in getRWL(); displayed on Overview
  )
  
  # ── Helper: resolve n input ────────────────────────────────────────────────
  # The Hanning filter length n can be NULL (no filter) or a numeric.
  # Centralised here so all reactives use identical logic.
  resolveN <- function(val) {
    if (is.null(val) || val == "NULL") NULL else as.numeric(val)
  }
  
  # ── Helper: build summary card from rwl.report() return value ─────────────
  # rwl.report() returns a named list (since dplR 1.7.7 it is no longer
  # invisible). We read fields directly rather than parsing printed output.
  # Returns a div of label/value rows suitable for rendering inside a card.
  rwlSummaryCard <- function(dat) {
    rpt <- rwl.report(dat)
    
    nZerosPct      <- if (rpt$n > 0) round(rpt$nZeros / rpt$n * 100, 2) else 0
    missingStr     <- paste0(rpt$nZeros, " (", nZerosPct, "%)")
    unconnectedStr <- if (rpt$unconnected) {
      paste0("YES \u2014 years: ", paste(rpt$unconnectedYrs, collapse = ", "))
    } else {
      "None"
    }
    
    items <- list(
      list(label = "Dated series",             value = rpt$nSeries),
      list(label = "Total measurements",       value = rpt$n),
      list(label = "Missing rings (zeros)",    value = missingStr),
      list(label = "Mean series length",       value = round(rpt$meanSegLength, 0)),
      list(label = "Span",                     value = paste(rpt$firstYear,
                                                             "\u2013",
                                                             rpt$lastYear)),
      list(label = "Mean interseries cor (SD)",
           value = paste0(round(rpt$meanInterSeriesCor, 3),
                          " (", round(rpt$sdInterSeriesCor, 3), ")")),
      list(label = "Mean AR1 (SD)",
           value = paste0(round(rpt$meanAR1, 3),
                          " (", round(rpt$sdAR1, 3), ")"))
      #list(label = "Unconnected floaters",     value = unconnectedStr)
    )
    
    rows <- lapply(items, function(item) {
      div(class = "row mb-1",
          div(class = "col-7 text-muted small",         item$label),
          div(class = "col-5 fw-bold small text-end",   as.character(item$value))
      )
    })
    div(rows)
  }
  
  # ── Progressive sidebar disclosure ────────────────────────────────────────
  # Sidebar controls reveal as the user advances through the workflow:
  #   Overview:     Dated Series + About only
  #   Correlations: + Analysis Parameters
  #   Series/Edit:  + Series selector
  #   Floater:      + Undated Series (only if dated file is loaded)
  observe({
    shinyjs::hide("divSharedParams")
    shinyjs::hide("divSeriesSelector")
    shinyjs::hide("divUndated")
  })
  
  observeEvent(input$navbar, {
    tab <- input$navbar
    
    # Analysis Parameters — Correlations and beyond
    if (tab %in% c("AllSeriesTab", "IndividualSeriesTab",
                   "EditSeriesTab", "UndatedSeriesTab")) {
      shinyjs::show("divSharedParams")
    } else {
      shinyjs::hide("divSharedParams")
    }
    
    # Series selector — Series and Edit panels only
    if (tab %in% c("IndividualSeriesTab", "EditSeriesTab")) {
      shinyjs::show("divSeriesSelector")
    } else {
      shinyjs::hide("divSeriesSelector")
    }
    
    # Undated series div is handled by the observe() below so that it
    # reacts to both tab changes AND dated file state changes (e.g.
    # unchecking useDemoDated). Nothing to do here.
  })
  
  # Undated sidebar section — separate observer so it reacts to BOTH
  # tab changes and dated file state (getRWL() going NULL/non-NULL).
  # If this lived only inside observeEvent(input$navbar), unchecking
  # useDemoDated would leave divUndated visible because the navbar
  # tab didn't change.
  observe({
    if (!is.null(input$navbar) &&
        input$navbar == "UndatedSeriesTab" &&
        !is.null(getRWL())) {
      shinyjs::show("divUndated")
    } else {
      shinyjs::hide("divUndated")
    }
  })
  # ── File readers ───────────────────────────────────────────────────────────
  getRWL <- reactive({
    rwlRV$readError <- NULL   # clear any previous error on each attempt
    if (input$useDemoDated) {
      return(read.rwl("data/xDateRtest.rwl"))
    }
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    # Use try() rather than tryCatch() — more reliable for catching base R
    # errors (e.g. from scan()) that fire deep inside dplR's read.rwl().
    result <- try(read.rwl(inFile$datapath), silent = TRUE)
    if (inherits(result, "try-error")) {
      # Strip the "Error in ... :" prefix that try() prepends
      rwlRV$readError <- trimws(gsub("^Error.*?:\n", "", as.character(result)))
      return(NULL)
    }
    result
  })
  
  getRWLUndated <- reactive({
    if (input$useDemoUndated) {
      dat <- read.rwl("data/xDateRtestUndated.rwl")
      rwlRV$undated <- dat
      return(dat)
    }
    inFile <- input$file2
    if (is.null(inFile)) return(NULL)
    dat <- read.rwl(inFile$datapath)
    rwlRV$undated <- dat
    dat
  })
  
  # ── Populate sidebar controls once data loads ──────────────────────────────
  observeEvent(getRWL(), {
    updateAwesomeCheckboxGroup(
      session  = session,
      inputId  = "master",
      inline   = TRUE,
      choices  = colnames(getRWL()),
      selected = colnames(getRWL()),
      status   = "primary"
    )
  })
  
  observeEvent(filteredRWL(), {
    updateSelectInput(
      session  = session,
      inputId  = "series",
      choices  = colnames(rwlRV$dated),
      selected = colnames(rwlRV$dated)[1]
    )
  })
  
  observeEvent(getRWLUndated(), {
    updateSelectInput(
      session  = session,
      inputId  = "series2",
      choices  = colnames(rwlRV$undated),
      selected = colnames(rwlRV$undated)[1]
    )
  })
  
  # ── Update window sliders when series, data, or window controls change ───────
  # winCenter and winWidth are mutually constrained:
  #
  #   winWidth max  = min(100, 2 * min(center - seriesStart, seriesEnd - center))
  #     capped at 100 (skeleton plot is unreadable beyond that) and can never
  #     push the window off either end of the series.
  #
  #   winCenter min/max = seriesStart + winWidth/2  ..  seriesEnd - winWidth/2
  #     the centre must be far enough from both ends to fit a half-width.
  #
  # Both sliders share one observer so they stay consistent. Current values are
  # preserved where still within the new bounds, and clamped otherwise.
  # lagCCF fallback guards against NULL on first fire.
  observeEvent({
    input$series
    filteredRWL()
    input$winCenter
    input$winWidth
  }, {
    req(input$series, rwlRV$dated)
    dat    <- rwlRV$dated
    tmp    <- summary(dat)
    lag    <- if (!is.null(input$lagCCF) && !is.na(input$lagCCF)) input$lagCCF else 5
    sBnds  <- as.numeric(tmp[tmp$series == input$series, 2:3])
    sStart <- sBnds[1]
    sEnd   <- sBnds[2]
    
    # rangeCCF (Series panel) -- unaffected by winWidth
    minWin <- round(sStart + lag, -1)
    maxWin <- round(sEnd   - lag, -1)
    output$rangeCCF <- renderUI({
      sliderInput("rangeCCF", "Adjust plotted years",
                  min       = minWin, max = maxWin,
                  value     = c(minWin, maxWin),
                  step      = 5, sep = "", dragRange = TRUE,
                  ticks     = FALSE)
    })
    
    # Current slider values with sensible defaults on first load
    curCenter <- if (!is.null(input$winCenter) && !is.na(input$winCenter)) {
      input$winCenter
    } else {
      round(mean(sBnds), -1)
    }
    curWidth <- if (!is.null(input$winWidth) && !is.na(input$winWidth)) {
      input$winWidth
    } else {
      40
    }
    
    # winWidth bounds: largest window that fits around curCenter, capped at 100
    halfRoom <- min(curCenter - sStart, sEnd - curCenter)
    maxWidth <- max(10, min(100, floor(halfRoom / 10) * 10 * 2))
    curWidth <- min(curWidth, maxWidth)
    
    # winCenter bounds: centre must fit half-width from each end
    halfWidth <- curWidth / 2
    minCenter <- round(sStart + halfWidth, -1)
    maxCenter <- round(sEnd   - halfWidth, -1)
    if (minCenter >= maxCenter) {
      minCenter <- round(sStart, -1)
      maxCenter <- round(sEnd,   -1)
    }
    curCenter <- max(minCenter, min(maxCenter, curCenter))
    
    output$winCenter <- renderUI({
      sliderInput("winCenter", "Window Center",
                  min   = minCenter, max = maxCenter,
                  value = curCenter,
                  step  = 5, sep = "",
                  ticks = FALSE)
    })
  }, ignoreNULL = TRUE)
  
  # ── winWidth.ui: dynamic window width slider ───────────────────────────────
  # Registered as a top-level renderUI (not inside the observer) so that
  # outputOptions(suspendWhenHidden = FALSE) can reference it at startup.
  # Reads input$series, input$winCenter, and rwlRV$dated as reactive deps
  # so it still updates whenever any of those change.
  # max = min(100, largest even window that fits around curCenter)
  # default = min(40, max)
  output$winWidth.ui <- renderUI({
    if (is.null(rwlRV$dated) || is.null(input$series) || input$series == "") {
      sliderInput("winWidth", "Window width (years)",
                  min = 10, max = 100, value = 40, step = 10, ticks = FALSE)
    } else {
      dat    <- rwlRV$dated
      tmp    <- summary(dat)
      sBnds  <- as.numeric(tmp[tmp$series == input$series, 2:3])
      sStart <- sBnds[1]
      sEnd   <- sBnds[2]
      curCenter <- if (!is.null(input$winCenter) && !is.na(input$winCenter)) {
        input$winCenter
      } else {
        round(mean(sBnds), -1)
      }
      halfRoom <- min(curCenter - sStart, sEnd - curCenter)
      maxWidth <- max(10, min(100, floor(halfRoom / 10) * 10 * 2))
      curWidth <- if (!is.null(input$winWidth) && !is.na(input$winWidth)) {
        min(input$winWidth, maxWidth)
      } else {
        min(40, maxWidth)
      }
      sliderInput("winWidth", "Window width (years)",
                  min = 10, max = maxWidth, value = curWidth,
                  step = 10, ticks = FALSE)
    }
  })
  outputOptions(output, "winWidth.ui", suspendWhenHidden = FALSE)
  
  # ── filteredRWL ────────────────────────────────────────────────────────────
  # The master chronology used by all correlation functions. Recomputes when
  # the user clicks "Update Master" or when new data is loaded.
  # Series can be temporarily excluded via the checkbox group on the
  # Correlations panel — useful when a bad series would corrupt the master.
  filteredRWL <- eventReactive({
    input$updateMasterButton
    getRWL()
  }, {
    req(getRWL())
    res <- if (is.null(input$master) || length(input$master) == 0 || identical(input$master, "")) {
      getRWL()
    } else {
      getRWL()[, colnames(getRWL()) %in% input$master, drop = FALSE]
    }
    rwlRV$dated      <- res
    rwlRV$datedVault <- res   # snapshot for revert
    res
  })
  
  # ── rwlStats: derived series-length stats used by dynamic widgets ─────────
  # Computed once from filteredRWL() and shared by seg.length.ui, winWidth.ui,
  # and any future widgets that need to be bounded by the data dimensions.
  # minLen: min series length rounded down to nearest 10 (minimum 10).
  #   Used as the seg.length max — any series shorter than the segment length
  #   will produce zero-length bins and crash corr.rwl.seg().
  # seriesLengths: named integer vector of non-NA measurements per series.
  rwlStats <- reactive({
    req(filteredRWL())
    dat     <- rwlRV$dated
    lengths <- colSums(!is.na(dat))
    minLen  <- max(10, floor(min(lengths) / 10) * 10)
    list(minLen = minLen, seriesLengths = lengths)
  })
  
  # ── seg.length.ui: dynamic segment length slider ───────────────────────────
  # max  = min series length rounded down to nearest 10 — prevents any series
  #        being shorter than the segment, which crashes corr.rwl.seg().
  # default = min(50, minLen)
  # suspendWhenHidden = FALSE ensures the slider renders even when the
  # Analysis Parameters accordion is closed, so input$seg.length is never NULL.
  output$seg.length.ui <- renderUI({
    if (is.null(rwlRV$dated)) {
      sliderInput("seg.length", "Segment Length",
                  min = 10, max = 100, value = 50, step = 10, ticks = FALSE)
    } else {
      ml      <- rwlStats()$minLen
      maxSeg  <- max(10, ml)
      defSeg  <- min(50, maxSeg)
      sliderInput("seg.length", "Segment Length",
                  min = 10, max = maxSeg, value = defSeg, step = 10,
                  ticks = FALSE)
    }
  })
  outputOptions(output, "seg.length.ui", suspendWhenHidden = FALSE)
  
  # ── rwlQA: tiered data quality check ──────────────────────────────────────
  # Depends on rwlRV$dated and input$seg.length. Returns a list:
  #   tier    — 0 = ok, 1 = hard block, 2 = soft warning, 3 = advisory
  #   message — plain-language explanation for the user
  #   short   — character vector of series names that are too short (tier 2)
  #
  # Tier 1 (hard block): fewer than 5 series OR all series shorter than seg.length
  # Tier 2 (soft warn):  some (not all) series shorter than 1.5 * seg.length
  # Tier 3 (advisory):   mean series length < 3 * seg.length
  rwlQA <- reactive({
    req(rwlRV$dated, input$seg.length)
    dat     <- rwlRV$dated
    segLen  <- input$seg.length
    lengths <- colSums(!is.na(dat))
    nSeries <- ncol(dat)
    
    # Tier 1 checks
    if (nSeries < 5) {
      return(list(
        tier    = 1,
        message = paste0(
          "This file has only ", nSeries, " series. Statistical crossdating ",
          "requires at least 5 series to build a meaningful master chronology. ",
          "Please load a file with more series."
        ),
        short   = character(0)
      ))
    }
    if (all(lengths < segLen)) {
      return(list(
        tier    = 1,
        message = paste0(
          "All series are shorter than the current segment length (", segLen,
          " years). No correlations can be computed. Try reducing the segment ",
          "length in the Analysis Parameters, or load a file with longer series."
        ),
        short   = character(0)
      ))
    }
    
    # Tier 2 check
    shortNames <- names(lengths[lengths < 1.5 * segLen])
    if (length(shortNames) > 0) {
      return(list(
        tier    = 2,
        message = paste0(
          length(shortNames), " series ",
          if (length(shortNames) == 1) "is" else "are",
          " too short for reliable crossdating at the current segment length (",
          segLen, " years): ",
          paste(shortNames, collapse = ", "),
          ". Consider removing ",
          if (length(shortNames) == 1) "it" else "them",
          " using the filter on the Correlations panel, or reducing the segment length."
        ),
        short   = shortNames
      ))
    }
    
    # Tier 3 check
    meanLen <- mean(lengths)
    if (meanLen < 3 * segLen) {
      return(list(
        tier    = 3,
        message = paste0(
          "Mean series length (", round(meanLen, 0), " years) is short relative ",
          "to the segment length (", segLen, " years). Correlations may have ",
          "low statistical power."
        ),
        short   = character(0)
      ))
    }
    
    list(tier = 0, message = "", short = character(0))
  })
  
  # ── getCRS ─────────────────────────────────────────────────────────────────
  # Runs corr.rwl.seg() on the current master. All correlation panels read
  # from this single reactive so parameter changes propagate everywhere.
  getCRS <- reactive({
    req(rwlRV$dated, input$seg.length)
    # Hard block: do not attempt computation if data fail QA tier 1
    qa <- rwlQA()
    if (qa$tier == 1) return(NULL)
    corr.rwl.seg(
      rwlRV$dated,
      seg.length = input$seg.length,
      bin.floor  = as.numeric(input$bin.floor),
      n          = resolveN(input$n),
      prewhiten  = input$prewhiten,
      pcrit      = input$pcrit,
      biweight   = input$biweight,
      method     = input$method,
      make.plot  = FALSE
    )
  })
  
  # ── getFloater ─────────────────────────────────────────────────────────────
  # Runs dplR::xdate.floater() for the selected undated series. The floater
  # panel has its own parameter inputs (seg.lengthUndated etc.) because floater
  # analysis is typically run with different settings than the main crossdating.
  # make.plot = FALSE and verbose = FALSE because the app handles display itself.
  getFloater <- reactive({
    req(filteredRWL(), input$series2, rwlRV$undated,
        input$minOverlapUndated)
    series2date <- rwlRV$undated[, input$series2]
    fo <- dplR::xdate.floater(
      rwl         = filteredRWL(),
      series      = series2date,
      series.name = input$series2,
      min.overlap = input$minOverlapUndated,
      n           = NULL,
      prewhiten   = TRUE,
      biweight    = TRUE,
      method      = "spearman",
      make.plot   = FALSE,
      verbose     = FALSE,
      return.rwl  = TRUE
    )
    if (is.null(names(fo))) {
      names(fo) <- c("floaterCorStats", "rwlCombined")
    }
    fo$series.name <- input$series2
    fo
  })
  
  # ── getSeries4Editing ──────────────────────────────────────────────────────
  # Prepares the selected series for the edit table. Splits the dated rwl into
  # the series being edited and everything else (datedNoSeries), so edits can
  # be recombined via combine.rwl() after each insert/delete operation.
  getSeries4Editing <- reactive({
    req(filteredRWL(), input$series)
    dat         <- rwlRV$dated
    datNoSeries <- dat[, names(dat) != input$series, drop = FALSE]
    series      <- dat[, input$series]
    mask        <- is.na(series)
    seriesDF    <- data.frame(Year  = time(datNoSeries)[!mask],
                              Value = series[!mask])
    rwlRV$datedNoSeries <- datNoSeries
    rwlRV$seriesDF      <- seriesDF
  })
  
  
  # ════════════════════════════════════════════════════════════════════════════
  # OVERVIEW
  # ════════════════════════════════════════════════════════════════════════════
  
  # ── overviewUI: switches between welcome state and data state ─────────────
  # When no data is loaded: SVG tree-ring art with onboarding instructions.
  # When data is loaded: summary card + RWL plot + collapsible series table.
  #
  # IMPORTANT: uiOutput("rwlSummaryHeader") inside the data state is a
  # placeholder — it requires its own output$rwlSummaryHeader renderUI below.
  # Nested uiOutputs always need their own registered render calls.
  output$overviewUI <- renderUI({
    if (is.null(getRWL()) && is.null(rwlRV$readError)) {
      
      # ── Welcome state ────────────────────────────────────────────────────
      HTML('
        <svg width="100%" viewBox="0 0 680 420" xmlns="http://www.w3.org/2000/svg">
          <g opacity="0.18">
            <circle cx="0" cy="210" r="390" fill="none" stroke="#2C5F2E" stroke-width="18"/>
            <circle cx="0" cy="210" r="360" fill="none" stroke="#2C5F2E" stroke-width="10"/>
            <circle cx="0" cy="210" r="336" fill="none" stroke="#2C5F2E" stroke-width="20"/>
            <circle cx="0" cy="210" r="308" fill="none" stroke="#2C5F2E" stroke-width="8"/>
            <circle cx="0" cy="210" r="288" fill="none" stroke="#2C5F2E" stroke-width="22"/>
            <circle cx="0" cy="210" r="256" fill="none" stroke="#2C5F2E" stroke-width="12"/>
            <circle cx="0" cy="210" r="232" fill="none" stroke="#2C5F2E" stroke-width="16"/>
            <circle cx="0" cy="210" r="206" fill="none" stroke="#2C5F2E" stroke-width="8"/>
            <circle cx="0" cy="210" r="188" fill="none" stroke="#2C5F2E" stroke-width="24"/>
            <circle cx="0" cy="210" r="154" fill="none" stroke="#2C5F2E" stroke-width="10"/>
            <circle cx="0" cy="210" r="134" fill="none" stroke="#2C5F2E" stroke-width="18"/>
            <circle cx="0" cy="210" r="106" fill="none" stroke="#2C5F2E" stroke-width="8"/>
            <circle cx="0" cy="210" r="88"  fill="none" stroke="#2C5F2E" stroke-width="20"/>
            <circle cx="0" cy="210" r="60"  fill="none" stroke="#2C5F2E" stroke-width="14"/>
            <circle cx="0" cy="210" r="38"  fill="none" stroke="#2C5F2E" stroke-width="10"/>
            <circle cx="0" cy="210" r="20"  fill="#2C5F2E" opacity="0.5"/>
          </g>
          <line x1="0" y1="210" x2="420" y2="210" stroke="#2C5F2E" stroke-width="0.5"
                opacity="0.5" stroke-dasharray="4 4"/>
          <text x="340" y="110" text-anchor="middle" font-family="sans-serif"
                font-size="32" font-weight="500" fill="#2C5F2E" opacity="0.9">xDateR</text>
          <text x="340" y="148" text-anchor="middle" font-family="sans-serif"
                font-size="14" fill="#666">Statistical crossdating for tree-ring data</text>
          <line x1="280" y1="168" x2="400" y2="168" stroke="#ccc" stroke-width="0.5"/>
          <circle cx="252" cy="210" r="14" fill="#2C5F2E" opacity="0.15"/>
          <text x="252" y="215" text-anchor="middle" font-family="sans-serif"
                font-size="13" font-weight="500" fill="#2C5F2E">1</text>
          <text x="276" y="207" font-family="sans-serif" font-size="13"
                font-weight="500" fill="#333">Upload a ring-width file</text>
          <text x="276" y="222" font-family="sans-serif" font-size="12"
                fill="#888">Tucson, Heidelberg, compact, or TRiDaS format</text>
          <circle cx="252" cy="262" r="14" fill="#2C5F2E" opacity="0.15"/>
          <text x="252" y="267" text-anchor="middle" font-family="sans-serif"
                font-size="13" font-weight="500" fill="#2C5F2E">2</text>
          <text x="276" y="259" font-family="sans-serif" font-size="13"
                font-weight="500" fill="#333">Inspect and crossdate</text>
          <text x="276" y="274" font-family="sans-serif" font-size="12"
                fill="#888">Correlations, segment analysis, skeleton plots</text>
          <circle cx="252" cy="314" r="14" fill="#2C5F2E" opacity="0.15"/>
          <text x="252" y="319" text-anchor="middle" font-family="sans-serif"
                font-size="13" font-weight="500" fill="#2C5F2E">3</text>
          <text x="276" y="311" font-family="sans-serif" font-size="13"
                font-weight="500" fill="#333">Edit and export</text>
          <text x="276" y="326" font-family="sans-serif" font-size="12"
                fill="#888">Insert or delete rings, download corrected .rwl</text>
          <rect x="208" y="356" width="264" height="32" rx="6"
                fill="#2C5F2E" opacity="0.08"/>
          <text x="340" y="377" text-anchor="middle" font-family="sans-serif"
                font-size="12" fill="#2C5F2E">Or check "Use example data" to explore</text>
        </svg>
      ')
      
    } else {
      
      # ── Data loaded state ────────────────────────────────────────────────
      # Build alert list — only non-NULL elements are included in tagList
      readErrAlert <- if (!is.null(rwlRV$readError)) {
        div(class = "alert alert-danger",
            bs_icon("x-circle"), " ",
            tags$strong("File could not be read."),
            " dplR's ", tags$code("read.rwl()"), " returned the following error:",
            tags$pre(class = "mt-2 mb-1", style = "font-size:0.85em;",
                     rwlRV$readError),
            "Please open a plain R session, load dplR, and run ",
            tags$code('read.rwl("your-file.rwl")'),
            " to diagnose the problem. Fix the file and then reload it here."
        )
      }
      qaAlert <- if (!is.null(rwlRV$dated) && !is.null(input$seg.length)) {
        qa <- rwlQA()
        if (qa$tier == 1) {
          div(class = "alert alert-danger mb-2",
              bs_icon("x-circle"), " ",
              tags$strong("Data cannot be crossdated statistically. "),
              qa$message)
        } else if (qa$tier == 2) {
          div(class = "alert alert-warning mb-2",
              bs_icon("exclamation-triangle"), " ",
              tags$strong("Some series are too short. "),
              qa$message)
        } else if (qa$tier == 3) {
          div(class = "alert alert-info mb-2",
              bs_icon("info-circle"), " ",
              tags$strong("Note: "),
              qa$message)
        }
      }
      tagList(
        readErrAlert,
        qaAlert,
        card(
          fill = FALSE,
          card_header(
            "Data Summary",
            tooltip(
              bsicons::bs_icon("question-circle"),
              paste("Key statistics from rwl.report(). Check these to confirm",
                    "your file was read correctly — number of series, span,",
                    "mean series length, and interseries correlation. Proceed",
                    "to the Correlations tab to begin crossdating.")
            )
          ),
          uiOutput("rwlSummaryHeader")
        ),
        card(
          fill = FALSE,
          card_header(
            layout_columns(
              col_widths = c(8, 4),
              span("RWL Plot",
                   tooltip(
                     bsicons::bs_icon("question-circle"),
                     paste("Segment view: each series shown as a horizontal bar spanning",
                           "its dated range. Spaghetti view: all ring-width series",
                           "plotted as lines — useful for spotting outlier series.",
                           "Uses plot.rwl() from dplR.")
                   )
              ),
              div(
                class = "d-flex justify-content-end",
                selectInput(
                  inputId  = "rwlPlotType",
                  label    = NULL,
                  choices  = c("Segment" = "seg", "Spaghetti" = "spag"),
                  selected = "seg",
                  width    = "150px"
                )
              )
            )
          ),
          plotOutput("rwlPlot", height = "500px")
        ),
        accordion(
          open = FALSE,
          accordion_panel(
            title = "Series Summary Table",
            icon  = bsicons::bs_icon("table"),
            helpText("Summary statistics for each series from summary.rwl(). Includes start year, end year, length, mean, and AR1."),
            tableOutput("rwlSummary")
          )
        ),
        div(class = "mt-2", downloadButton("rwlSummaryReport", "Generate report"))
      )
    }
  })
  
  # ── rwlSummaryHeader: fills the nested uiOutput in the data loaded state ──
  # Must be registered as its own output — a uiOutput inside a renderUI still
  # needs its own render call in the server. Without this the card body is blank.
  output$rwlSummaryHeader <- renderUI({
    req(filteredRWL())
    rwlSummaryCard(rwlRV$dated)
  })
  
  output$rwlPlot <- renderPlot({
    req(filteredRWL())
    plot.rwl(filteredRWL(), plot.type = input$rwlPlotType)
  }, height = 400)
  
  output$rwlSummary <- renderTable({
    req(filteredRWL())
    summary(filteredRWL())
  })
  
  output$rwlSummaryReport <- downloadHandler(
    filename = "rwl_summary_report.html",
    content  = function(file) {
      tempReport <- file.path(tempdir(), "report_rwl_describe.rmd")
      file.copy("report_rwl_describe.rmd", tempReport, overwrite = TRUE)
      params <- list(fileName    = input$file1$name,
                     rwlObject   = rwlRV$dated,
                     rwlPlotType = input$rwlPlotType)
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir  = new.env(parent = globalenv()))
    }
  )
  
  
  # ── qaAlertCorr: QA banner for the Correlations panel ────────────────────
  # Shows tier 2 and tier 3 alerts at the top of the Correlations panel.
  # Tier 1 is not shown here — getCRS() is already blocked so nothing renders.
  output$qaAlertCorr <- renderUI({
    req(rwlRV$dated, input$seg.length)
    qa <- rwlQA()
    if (qa$tier == 2) {
      div(class = "alert alert-warning",
          bs_icon("exclamation-triangle"), " ",
          tags$strong("Some series are too short. "),
          qa$message)
    } else if (qa$tier == 3) {
      div(class = "alert alert-info",
          bs_icon("info-circle"), " ",
          tags$strong("Note: "),
          qa$message)
    }
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # CORRELATIONS
  # ════════════════════════════════════════════════════════════════════════════
  
  # ── crsPlotUI: dynamic height container for the correlation tile plot ────
  # plotlyOutput height must be set on the widget div itself — setting height
  # inside plotly layout() alone is not enough as Shiny clips the container.
  # We compute height from the number of series so the plot is never cramped.
  output$crsPlotUI <- renderUI({
    req(filteredRWL())
    nSeries    <- ncol(rwlRV$dated)
    # Each series gets two tile rows at ~56px each plus generous margins
    # for the x-axis and top padding. Minimum 400px.
    plotHeight <- paste0(max(300, nSeries * 22 + 200), "px")
    plotlyOutput("crsFancyPlot", height = plotHeight)
  })
  
  # Interactive plotly tile plot — see plotlyCRSFunc.R for implementation.
  # Uses pure plotly (no ggplotly) to avoid factor coercion errors.
  output$crsFancyPlot <- renderPlotly({
    req(filteredRWL())
    crsPlotly(getCRS())
  })
  
  # Overall mean correlation per series across all bins
  output$crsOverall <- renderDT({
    req(filteredRWL())
    crsObject  <- getCRS()
    overallCor <- round(crsObject$overall, 3)
    res <- data.frame(Series      = rownames(overallCor),
                      Correlation = overallCor[, 1])
    datatable(res, rownames = FALSE,
              caption = "Overall Series Correlation",
              options = list(pageLength   = nrow(res),
                             searching    = FALSE,
                             lengthChange = FALSE)) %>%
      formatStyle("Correlation",
                  fontWeight = styleInterval(crsObject$pcrit,
                                             c("normal", "bold")))
  })
  
  # Average correlation within each time bin across all series
  output$crsAvgCorrBin <- renderDT({
    req(filteredRWL())
    crsObject <- getCRS()
    binNames  <- paste(crsObject$bins[, 1], "-", crsObject$bins[, 2], sep = "")
    res <- data.frame(Bin         = binNames,
                      Correlation = round(crsObject$avg.seg.rho, 3))
    datatable(res, rownames = FALSE,
              caption = "Avg. Correlation by Bin",
              options = list(pageLength   = nrow(res),
                             searching    = FALSE,
                             lengthChange = FALSE)) %>%
      formatStyle("Correlation",
                  fontWeight = styleInterval(crsObject$pcrit,
                                             c("normal", "bold")))
  })
  
  # Series/segments with p > pcrit — these are the ones to investigate
  output$crsFlags <- renderDT({
    req(filteredRWL())
    crsObject <- getCRS()
    flags     <- crsObject$flags
    res <- if (length(flags) == 0) {
      data.frame(Series = character(0), Bins = character(0))
    } else {
      flags <- unlist(flags)
      data.frame(Series = names(flags),
                 Bins   = gsub("\\.", "-", flags))
    }
    datatable(res, rownames = FALSE,
              caption = "Flagged Series / Segments",
              options = list(pageLength   = max(5, nrow(res)),
                             searching    = FALSE,
                             lengthChange = FALSE))
  })
  
  # Full correlation matrix: series x bin
  output$crsCorrBin <- renderDT({
    req(filteredRWL())
    crsObject <- getCRS()
    binNames  <- paste(crsObject$bins[, 1], "-", crsObject$bins[, 2], sep = "")
    res       <- round(crsObject$spearman.rho, 3)
    res       <- data.frame(Series = rownames(res), res)
    colnames(res) <- c("Series", binNames)
    datatable(res, rownames = FALSE,
              caption = "Series Correlation by Bin",
              options = list(pageLength   = min(30, nrow(res)),
                             searching    = TRUE,
                             lengthChange = FALSE)) %>%
      formatStyle(columns    = -1,
                  fontWeight = styleInterval(crsObject$pcrit,
                                             c("normal", "bold")))
  })
  
  # Report: captures all parameters for reproducibility
  output$crsReport <- downloadHandler(
    filename = "rwl_correlation_report.html",
    content  = function(file) {
      tempReport <- file.path(tempdir(), "report_rwl_corr.rmd")
      file.copy("report_rwl_corr.rmd", tempReport, overwrite = TRUE)
      crsParams <- list(seg.length = input$seg.length,
                        bin.floor  = input$bin.floor,
                        n          = input$n,
                        prewhiten  = input$prewhiten,
                        pcrit      = input$pcrit,
                        biweight   = input$biweight,
                        method     = input$method)
      params <- list(fileName  = input$file1$name,
                     crsObject = getCRS(),
                     crsParams = crsParams)
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir  = new.env(parent = globalenv()))
    }
  )
  
  
  # ════════════════════════════════════════════════════════════════════════════
  # SERIES
  # ════════════════════════════════════════════════════════════════════════════
  
  # Alert banner listing any flagged series from the Correlations panel.
  # Renders NULL (no banner) if nothing is flagged.
  output$flaggedSeriesUI <- renderUI({
    req(filteredRWL())
    crsObject <- getCRS()
    flags     <- crsObject$flags
    if (length(flags) == 0) return(NULL)
    flags      <- unlist(flags)
    series2get <- names(flags)
    msg <- if (length(series2get) == 1) {
      paste("Series", series2get, "was flagged.")
    } else if (length(series2get) == 2) {
      paste("Series", paste(series2get, collapse = " and "), "were flagged.")
    } else {
      paste("These series were flagged:",
            paste(series2get, collapse = ", "), ".")
    }
    div(class = "alert alert-warning", msg)
  })
  
  # Segment correlation plot: corr.series.seg() for the selected series
  output$cssPlot <- renderPlot({
    req(filteredRWL(), input$series)
    corr.series.seg(
      rwlRV$dated,
      series     = input$series,
      seg.length = input$seg.length,
      bin.floor  = as.numeric(input$bin.floor),
      n          = resolveN(input$n),
      prewhiten  = input$prewhiten,
      pcrit      = input$pcrit,
      biweight   = input$biweight,
      method     = input$method,
      make.plot  = TRUE
    )
  }, height = 400)
  
  # Cross-correlation plot: ccf.series.rwl() for the selected series and window
  output$ccfPlot <- renderPlot({
    req(filteredRWL(), input$series, input$rangeCCF)
    dat <- rwlRV$dated
    yrs <- time(dat)
    win <- input$rangeCCF[1]:input$rangeCCF[2]
    dat <- dat[yrs %in% win, , drop = FALSE]
    ccf.series.rwl(
      dat,
      series     = input$series,
      seg.length = input$seg.length,
      bin.floor  = as.numeric(input$bin.floor),
      n          = resolveN(input$n),
      prewhiten  = input$prewhiten,
      pcrit      = input$pcrit,
      biweight   = input$biweight,
      method     = input$method,
      lag.max    = input$lagCCF,
      make.plot  = TRUE
    )
  }, height = 400)
  
  # Report: includes dating notes for the analyst's reasoning
  output$cssReport <- downloadHandler(
    filename = "series_correlation_report.html",
    content  = function(file) {
      tempReport <- file.path(tempdir(), "report_series.rmd")
      file.copy("report_series.rmd", tempReport, overwrite = TRUE)
      cssParams <- list(seg.length  = input$seg.length,
                        bin.floor   = input$bin.floor,
                        n           = input$n,
                        prewhiten   = input$prewhiten,
                        pcrit       = input$pcrit,
                        biweight    = input$biweight,
                        method      = input$method,
                        series      = input$series,
                        lagCCF      = input$lagCCF,
                        datingNotes = input$datingNotes,
                        winCCF      = input$rangeCCF[1]:input$rangeCCF[2])
      params <- list(fileName  = input$file1$name,
                     rwlObject = rwlRV$dated,
                     cssParams = cssParams)
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir  = new.env(parent = globalenv()))
    }
  )
  
  
  # ════════════════════════════════════════════════════════════════════════════
  # EDIT
  # ════════════════════════════════════════════════════════════════════════════
  
  # Skeleton / CCF plot: xskel.ccf.plot() centred on the current window
  output$xskelPlot <- renderPlot({
    req(filteredRWL(), input$series, input$winCenter, input$winWidth)
    wStart <- input$winCenter - (input$winWidth / 2)
    xskel.ccf.plot(
      rwlRV$dated,
      series    = input$series,
      win.start = wStart,
      win.width = input$winWidth,
      n         = resolveN(input$n),
      prewhiten = input$prewhiten,
      biweight  = input$biweight
    )
  }, height = 400)
  
  output$series2edit <- renderText({
    paste("Series", input$series, "selected")
  })
  
  # Scrollable measurements table — automatically scrolls to window center.
  # Click a row to select it, then use the edit controls to modify it.
  output$table1 <- renderDataTable({
    req(filteredRWL(), input$series, input$winCenter)
    getSeries4Editing()
    wStart    <- input$winCenter - 10
    wEnd      <- input$winCenter + 10
    nRows     <- length(seq(wStart, wEnd)) * 33.33
    row2start <- which(rwlRV$seriesDF[, 1] == wStart)
    
    datatable(
      rwlRV$seriesDF,
      selection  = list(mode = "single", target = "row"),
      extensions = "Scroller",
      rownames   = FALSE,
      options    = list(
        deferRender  = TRUE,
        autoWidth    = TRUE,
        scrollY      = nRows,
        scroller     = TRUE,
        searching    = FALSE,
        lengthChange = FALSE,
        columnDefs   = list(list(className = "dt-left", targets = "_all")),
        initComplete = JS('function() {this.api().table().scroller.toPosition(',
                          row2start - 1, ');}')
      )
    )
  })
  
  output$editLog <- renderPrint({
    req(filteredRWL())
    if (!is.null(rwlRV$editLog)) rwlRV$editLog
  })
  
  # Controls the visibility of the Save/Revert card — only shown after edits
  output$showSaveEdits <- reactive({
    !all(sapply(rwlRV$editDF, is.null))
  })
  outputOptions(output, "showSaveEdits", suspendWhenHidden = FALSE)
  
  # Show/hide the save card based on whether any edits exist.
  # Using shinyjs rather than renderUI because verbatimTextOutput("editLog")
  # inside a renderUI doesn't bind correctly in Shiny.
  observe({
    if (!is.null(rwlRV$editDF) && nrow(rwlRV$editDF) > 0) {
      shinyjs::show("divSaveEdits")
    } else {
      shinyjs::hide("divSaveEdits")
    }
  })
  
  # ── Delete ring ────────────────────────────────────────────────────────────
  # Removes the selected row from seriesDF, renumbers years according to
  # fix.last logic, recombines with the rest of the rwl, and updates the log.
  observeEvent(input$deleteRows, {
    req(filteredRWL())
    if (!is.null(input$table1_rows_selected)) {
      seriesDF     <- rwlRV$seriesDF
      row2delIndex <- as.numeric(input$table1_rows_selected)
      yearDeleted  <- seriesDF$Year[row2delIndex]
      seriesDF     <- seriesDF[-row2delIndex, ]
      n            <- nrow(seriesDF)
      newYrs <- if (input$deleteRingFixLast) {
        seq(seriesDF$Year[2], by = 1, length.out = n)
      } else {
        seq(seriesDF$Year[1], by = 1, length.out = n)
      }
      seriesDF$Year      <- newYrs
      tmpSeriesDF        <- data.frame(x = seriesDF[, 2])
      names(tmpSeriesDF) <- input$series
      rownames(tmpSeriesDF) <- seriesDF[, 1]
      class(tmpSeriesDF) <- c("rwl", "data.frame")
      tmpDat       <- combine.rwl(rwlRV$datedNoSeries, tmpSeriesDF)
      tmpDat       <- tmpDat[, names(rwlRV$dated)]
      rwlRV$dated  <- tmpDat
      rwlRV$editLog <- c(rwlRV$editLog,
                         paste0("Series ", input$series, ". Year ",
                                yearDeleted, " deleted. Fix Last = ",
                                input$deleteRingFixLast))
      rwlRV$editDF <- rbind(rwlRV$editDF,
                            data.frame(series  = input$series,
                                       year    = yearDeleted,
                                       value   = NA,
                                       action  = "delete.ring",
                                       fixLast = input$deleteRingFixLast))
    }
  })
  
  # ── Insert ring ────────────────────────────────────────────────────────────
  # Inserts a new row above the selected row with the user-specified value,
  # renumbers years, recombines, and updates the log.
  observeEvent(input$insertRows, {
    req(filteredRWL())
    if (!is.null(input$table1_rows_selected)) {
      seriesDF     <- rwlRV$seriesDF
      n            <- nrow(seriesDF)
      row2addIndex <- as.numeric(input$table1_rows_selected)
      yearInserted <- seriesDF$Year[row2addIndex]
      seriesDF1    <- seriesDF[seq_len(row2addIndex - 1), ]
      seriesDF2    <- data.frame(Year = row2addIndex, Value = input$insertValue)
      seriesDF3    <- seriesDF[row2addIndex:n, ]
      seriesDF     <- rbind(seriesDF1, seriesDF2, seriesDF3)
      n            <- nrow(seriesDF)
      newYrs <- if (input$insertRingFixLast) {
        seq(seriesDF$Year[1] - 1, by = 1, length.out = n)
      } else {
        seq(seriesDF$Year[1], by = 1, length.out = n)
      }
      seriesDF$Year      <- newYrs
      tmpSeriesDF        <- data.frame(x = seriesDF[, 2])
      names(tmpSeriesDF) <- input$series
      rownames(tmpSeriesDF) <- seriesDF[, 1]
      class(tmpSeriesDF) <- c("rwl", "data.frame")
      tmpDat       <- combine.rwl(rwlRV$datedNoSeries, tmpSeriesDF)
      tmpDat       <- tmpDat[, names(rwlRV$dated)]
      rwlRV$dated  <- tmpDat
      rwlRV$editLog <- c(rwlRV$editLog,
                         paste0("Series ", input$series, ". Year ",
                                yearInserted, " inserted with value ",
                                input$insertValue, ". Fix Last = ",
                                input$insertRingFixLast))
      rwlRV$editDF <- rbind(rwlRV$editDF,
                            data.frame(series  = input$series,
                                       year    = yearInserted,
                                       value   = input$insertValue,
                                       action  = "insert.ring",
                                       fixLast = input$insertRingFixLast))
    }
  })
  
  # ── Revert all edits ───────────────────────────────────────────────────────
  observeEvent(input$revertSeries, {
    req(filteredRWL())
    rwlRV$dated   <- rwlRV$datedVault
    rwlRV$editLog <- "Edits reverted. Log reset."
    rwlRV$editDF  <- data.frame(series  = NULL, year    = NULL,
                                value   = NULL, action  = NULL,
                                fixLast = NULL)
  })
  
  # Report: includes the edit log and reproducible R code
  output$editReport <- downloadHandler(
    filename = "edits_report.html",
    content  = function(file) {
      tempReport <- file.path(tempdir(), "report_edits.rmd")
      file.copy("report_edits.rmd", tempReport, overwrite = TRUE)
      params <- list(fileName = input$file1$name,
                     editLog  = rwlRV$editLog,
                     editDF   = rwlRV$editDF)
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir  = new.env(parent = globalenv()))
    }
  )
  
  output$downloadRWL <- downloadHandler(
    filename = function() paste0(input$file1, "-", Sys.Date(), ".rwl"),
    content  = function(file) {
      write.tucson(rwl.df = rwlRV$dated, fname = file)
    }
  )
  
  
  # ════════════════════════════════════════════════════════════════════════════
  # FLOATER
  # ════════════════════════════════════════════════════════════════════════════
  
  # SVG art shared between welcome states — stored as a string to avoid
  # duplicating the markup in two branches of the renderUI below.
  floaterSVG <- '
    <svg width="100%" viewBox="0 0 680 420" xmlns="http://www.w3.org/2000/svg">
      <defs>
        <marker id="arr-green" viewBox="0 0 10 10" refX="8" refY="5"
                markerWidth="5" markerHeight="5" orient="auto">
          <path d="M2 1L8 5L2 9" fill="none" stroke="#2C5F2E"
                stroke-width="1.5" opacity="0.5"/>
        </marker>
      </defs>
      <text x="340" y="50" text-anchor="middle" font-family="sans-serif"
            font-size="28" font-weight="500" fill="#2C5F2E" opacity="0.9">Floater</text>
      <text x="340" y="72" text-anchor="middle" font-family="sans-serif"
            font-size="13" fill="#888">Date an undated or misdated series against a master chronology</text>
      <text x="40" y="90" font-family="sans-serif" font-size="12" fill="#888">Master chronology (dated)</text>
      <rect x="40" y="100" width="600" height="18" rx="3" fill="#2C5F2E" opacity="0.15"/>
      <line x1="80"  y1="100" x2="80"  y2="118" stroke="#2C5F2E" stroke-width="1.5" opacity="0.4"/>
      <line x1="106" y1="100" x2="106" y2="118" stroke="#2C5F2E" stroke-width="2.5" opacity="0.5"/>
      <line x1="128" y1="100" x2="128" y2="118" stroke="#2C5F2E" stroke-width="1"   opacity="0.4"/>
      <line x1="152" y1="100" x2="152" y2="118" stroke="#2C5F2E" stroke-width="3"   opacity="0.5"/>
      <line x1="172" y1="100" x2="172" y2="118" stroke="#2C5F2E" stroke-width="1.5" opacity="0.4"/>
      <line x1="198" y1="100" x2="198" y2="118" stroke="#2C5F2E" stroke-width="2"   opacity="0.5"/>
      <line x1="220" y1="100" x2="220" y2="118" stroke="#2C5F2E" stroke-width="1"   opacity="0.4"/>
      <line x1="244" y1="100" x2="244" y2="118" stroke="#2C5F2E" stroke-width="2.5" opacity="0.5"/>
      <line x1="268" y1="100" x2="268" y2="118" stroke="#2C5F2E" stroke-width="1.5" opacity="0.4"/>
      <line x1="290" y1="100" x2="290" y2="118" stroke="#2C5F2E" stroke-width="1"   opacity="0.5"/>
      <line x1="314" y1="100" x2="314" y2="118" stroke="#2C5F2E" stroke-width="3"   opacity="0.4"/>
      <line x1="338" y1="100" x2="338" y2="118" stroke="#2C5F2E" stroke-width="1.5" opacity="0.5"/>
      <line x1="360" y1="100" x2="360" y2="118" stroke="#2C5F2E" stroke-width="2"   opacity="0.4"/>
      <line x1="384" y1="100" x2="384" y2="118" stroke="#2C5F2E" stroke-width="1"   opacity="0.5"/>
      <line x1="406" y1="100" x2="406" y2="118" stroke="#2C5F2E" stroke-width="2.5" opacity="0.4"/>
      <line x1="430" y1="100" x2="430" y2="118" stroke="#2C5F2E" stroke-width="1.5" opacity="0.5"/>
      <line x1="452" y1="100" x2="452" y2="118" stroke="#2C5F2E" stroke-width="1"   opacity="0.4"/>
      <line x1="476" y1="100" x2="476" y2="118" stroke="#2C5F2E" stroke-width="2"   opacity="0.5"/>
      <line x1="500" y1="100" x2="500" y2="118" stroke="#2C5F2E" stroke-width="3"   opacity="0.4"/>
      <line x1="522" y1="100" x2="522" y2="118" stroke="#2C5F2E" stroke-width="1.5" opacity="0.5"/>
      <line x1="548" y1="100" x2="548" y2="118" stroke="#2C5F2E" stroke-width="1"   opacity="0.4"/>
      <line x1="572" y1="100" x2="572" y2="118" stroke="#2C5F2E" stroke-width="2.5" opacity="0.5"/>
      <line x1="596" y1="100" x2="596" y2="118" stroke="#2C5F2E" stroke-width="1.5" opacity="0.4"/>
      <line x1="40" y1="122" x2="640" y2="122" stroke="#ccc" stroke-width="0.5"/>
      <text x="40"  y="134" font-family="sans-serif" font-size="11" fill="#aaa" text-anchor="middle">1200</text>
      <text x="190" y="134" font-family="sans-serif" font-size="11" fill="#aaa" text-anchor="middle">1400</text>
      <text x="340" y="134" font-family="sans-serif" font-size="11" fill="#aaa" text-anchor="middle">1600</text>
      <text x="490" y="134" font-family="sans-serif" font-size="11" fill="#aaa" text-anchor="middle">1800</text>
      <text x="640" y="134" font-family="sans-serif" font-size="11" fill="#aaa" text-anchor="middle">2000</text>
      <text x="268" y="162" font-family="sans-serif" font-size="12" fill="#888">Undated series — sliding to find best fit</text>
      <rect x="268" y="172" width="220" height="18" rx="3" fill="#2C5F2E" opacity="0.35"/>
      <line x1="290" y1="172" x2="290" y2="190" stroke="#2C5F2E" stroke-width="1"   opacity="0.7"/>
      <line x1="314" y1="172" x2="314" y2="190" stroke="#2C5F2E" stroke-width="3"   opacity="0.8"/>
      <line x1="338" y1="172" x2="338" y2="190" stroke="#2C5F2E" stroke-width="1.5" opacity="0.7"/>
      <line x1="360" y1="172" x2="360" y2="190" stroke="#2C5F2E" stroke-width="2"   opacity="0.8"/>
      <line x1="384" y1="172" x2="384" y2="190" stroke="#2C5F2E" stroke-width="1"   opacity="0.7"/>
      <line x1="406" y1="172" x2="406" y2="190" stroke="#2C5F2E" stroke-width="2.5" opacity="0.8"/>
      <line x1="430" y1="172" x2="430" y2="190" stroke="#2C5F2E" stroke-width="1.5" opacity="0.7"/>
      <line x1="452" y1="172" x2="452" y2="190" stroke="#2C5F2E" stroke-width="1"   opacity="0.8"/>
      <line x1="476" y1="172" x2="476" y2="190" stroke="#2C5F2E" stroke-width="2"   opacity="0.7"/>
      <line x1="248" y1="181" x2="264" y2="181" stroke="#2C5F2E" stroke-width="1.5"
            opacity="0.5" stroke-dasharray="3 2" marker-end="url(#arr-green)"/>
      <line x1="492" y1="181" x2="508" y2="181" stroke="#2C5F2E" stroke-width="1.5"
            opacity="0.5" stroke-dasharray="3 2" marker-end="url(#arr-green)"/>
    </svg>'
  
  # ── floaterUI: three-state panel ──────────────────────────────────────────
  output$floaterUI <- renderUI({
    
    # State 1: no dated file — full welcome screen
    if (is.null(getRWL())) {
      return(tagList(
        HTML(floaterSVG),
        div(style = "margin-top: 1rem;",
            fluidRow(
              column(6, offset = 2,
                     div(style = "display:flex; align-items:center; gap:12px; margin-bottom:12px;",
                         div(style = "min-width:28px; height:28px; border-radius:50%;
                             background:#2C5F2E22; display:flex; align-items:center;
                             justify-content:center; font-weight:500; color:#2C5F2E;
                             font-size:13px;", "1"),
                         div(
                           tags$strong("Load a dated master .rwl file", style = "font-size:13px;"),
                           tags$div("Use the Dated Series upload in the sidebar",
                                    style = "font-size:12px; color:#888;")
                         )
                     ),
                     div(style = "display:flex; align-items:center; gap:12px;",
                         div(style = "min-width:28px; height:28px; border-radius:50%;
                             background:#2C5F2E22; display:flex; align-items:center;
                             justify-content:center; font-weight:500; color:#2C5F2E;
                             font-size:13px;", "2"),
                         div(
                           tags$strong("Navigate here — then load your undated .rwl",
                                       style = "font-size:13px;"),
                           tags$div("The undated upload appears in the sidebar once you arrive",
                                    style = "font-size:12px; color:#888;")
                         )
                     )
              )
            )
        )
      ))
    }
    
    # State 2: dated file loaded but no undated file yet.
    # Show a summary of the dated file so the user can confirm it loaded
    # correctly, plus a clear prompt to load the undated file.
    # The decorative SVG is intentionally omitted here — it was designed
    # for the empty welcome state and doesn't crop cleanly in this context.
    if (is.null(getRWLUndated())) {
      return(tagList(
        card(
          fill = FALSE,
          card_header(
            "Dated File Summary",
            tooltip(
              bs_icon("question-circle"),
              "Confirm your dated file loaded correctly before proceeding."
            )
          ),
          rwlSummaryCard(rwlRV$dated)
        ),
        div(style = "padding: 0.5rem 0;",
            div(class = "alert alert-success mb-0",
                bs_icon("check-circle"), " ",
                tags$strong("Dated file loaded."),
                " Now load your undated .rwl file using the ",
                tags$strong("Undated Series"),
                " upload that has appeared in the sidebar."
            )
        )
      ))
    }
    
    # State 3: both files loaded — compact summary card + reveal plots
    shinyjs::show("divFloaterPlots")
    card(
      fill = FALSE,
      card_header(
        "Data Summary",
        tooltip(bs_icon("question-circle"),
                "Key statistics confirming your undated file was read correctly.")
      ),
      rwlSummaryCard(rwlRV$undated)
    )
  })
  
  
  output$floaterControls <- renderUI({
    req(getRWLUndated())
    # Subtract 20 years from the raw series length as a conservative approximation
    # of the detrended series length — prewhitening and the Hanning filter both
    # trim years from the ends of the series. Using the raw length as the max
    # could allow values that exceed the detrended length and trigger an error
    # in xdate.floater().
    maxOverlap <- max(10, floor((sum(!is.na(rwlRV$undated[, input$series2])) - 20) / 10) * 10)
    div(
      selectInput(
        inputId  = "series2",
        label    = "Choose undated series",
        choices  = colnames(rwlRV$undated),
        selected = input$series2  # preserve current selection to avoid circular reset
      ),
      tags$label(
        "Minimum overlap (years)",
        tooltip(
          bs_icon("question-circle"),
          paste("The minimum number of years the undated series must overlap with the",
                "master chronology at a given position for a correlation to be calculated.",
                "Positions with less overlap than this are excluded from the search.",
                "Increase for more reliable correlations; decrease if your series is short",
                "and the search range is being truncated too aggressively.")
        )
      ),
      sliderInput(
        inputId = "minOverlapUndated",
        label   = NULL,
        min     = 10,
        max     = maxOverlap,
        value   = min(50, maxOverlap),
        step    = 10,
        ticks = FALSE
      )
    )
  })
  
  # CCF parameters — segment length, bin floor, pcrit
  output$floaterCCFParams <- renderUI({
    req(getRWLUndated())
    layout_columns(
      col_widths = c(4, 4, 4),
      sliderInput(
        inputId = "seg.lengthUndated",
        label   = "Segment Length",
        min = 10, max = 100, value = 50, step = 10,
        ticks=FALSE
      ),
      selectInput(
        inputId  = "bin.floorUndated",
        label    = "Bin Floor",
        choices  = c(0, 10, 50, 100),
        selected = 10
      ),
      numericInput(
        inputId = "pcritUndated",
        label   = "P crit",
        value   = 0.05, min = 0, max = 1, step = 0.01
      )
    )
  })
  
  # Hide divFloaterPlots when no undated file is loaded
  observe({
    if (is.null(getRWLUndated())) {
      shinyjs::hide("divFloaterPlots")
    }
  })
  
  # Summary of the undated file — confirm it was read correctly
  output$rwlReport2 <- renderPrint({
    req(getRWLUndated())
    rwl.report(getRWLUndated())
  })
  
  # Best-fit dating result text
  output$floaterText <- renderText({
    req(getRWLUndated(), getFloater())
    fo    <- getFloater()
    cat("here \n")
    cat(names(fo), "\n")
    cat(str(fo), "\n")
    fcs   <- fo$floaterCorStats
    rBest <- which.max(fcs$r)
    paste0("Series: ", fo$series.name, "<br/>",
           "Years searched: ", min(fcs$first), " to ", max(fcs$last), "<br/>",
           "Best correlation: ", round(fcs$r[rBest], 2),
           " (", fcs$first[rBest], " \u2013 ", fcs$last[rBest], ")")
  })
  
  # Floater correlation surface plot
  output$floaterPlot <- renderPlot({
    req(getRWLUndated(), getFloater())
    plot.floater(getFloater())
  })
  
  # Cross-correlation for the best-fit dated series against the master
  output$ccfPlotUndated <- renderPlot({
    req(getFloater())
    fo  <- getFloater()
    dat <- fo$rwlCombined
    ccf.series.rwl(
      dat,
      series     = fo$series.name,
      seg.length = if (!is.null(input$seg.lengthUndated)) input$seg.lengthUndated else 50,
      bin.floor  = if (!is.null(input$bin.floorUndated)) as.numeric(input$bin.floorUndated) else 10,
      n          = NULL,
      prewhiten  = TRUE,
      pcrit      = if (!is.null(input$pcritUndated)) input$pcritUndated else 0.05,
      biweight   = TRUE,
      method     = "spearman",
      make.plot  = TRUE
    )
  })
  
  # Save the best-fit dates for the current undated series
  observeEvent(input$saveDates, {
    req(getRWLUndated(), getFloater())
    fo        <- getFloater()
    fcs       <- fo$floaterCorStats
    rBest     <- which.max(fcs$r)
    firstBest <- fcs$first[rBest]
    lastBest  <- fcs$last[rBest]
    
    if (is.null(rwlRV$undated2dated)) {
      rwlRV$dateLog       <- c(rwlRV$dateLog,
                               paste0("Series ", fo$series.name,
                                      " saved with dates ",
                                      firstBest, " to ", lastBest, "."))
      rwlRV$undated2dated <- fo$rwlOut
    } else {
      if (input$series2 %in% names(rwlRV$undated2dated)) {
        rwlRV$dateLog <- c(rwlRV$dateLog,
                           paste0("Series ", fo$series.name,
                                  " was already saved."))
      } else {
        rwlRV$dateLog       <- c(rwlRV$dateLog,
                                 paste0("Series ", fo$series.name,
                                        " saved with dates ",
                                        firstBest, " to ", lastBest, "."))
        rwlRV$undated2dated <- combine.rwl(rwlRV$undated2dated, fo$rwlOut)
      }
    }
  })
  
  # Revert all saved dates
  observeEvent(input$removeDates, {
    req(getRWLUndated(), getFloater())
    rwlRV$undated2dated <- NULL
    rwlRV$dateLog       <- "Dates removed for all undated series. Log reset."
  })
  
  output$dateLog <- renderPrint({
    req(getRWLUndated(), getFloater())
    if (!is.null(rwlRV$dateLog)) rwlRV$dateLog
  })
  
  output$downloadUndatedRWL <- downloadHandler(
    filename = function() paste0(input$file2, "-", Sys.Date(), ".rwl"),
    content  = function(file) {
      tmp <- if (input$appendMaster) {
        combine.rwl(rwlRV$undated2dated, filteredRWL())
      } else {
        rwlRV$undated2dated
      }
      write.tucson(rwl.df = tmp, fname = file)
    }
  )
  
  # Report: floater analysis report
  output$undatedReport <- downloadHandler(
    filename = "undated_series_report.html",
    content  = function(file) {
      tempReport <- file.path(tempdir(), "report_undated_series.rmd")
      file.copy("report_undated_series.rmd", tempReport, overwrite = TRUE)
      undatedParams <- list(
        seg.length  = if (!is.null(input$seg.lengthUndated)) input$seg.lengthUndated else 50,
        bin.floor   = if (!is.null(input$bin.floorUndated))  input$bin.floorUndated  else 10,
        n           = "NULL",
        prewhiten   = TRUE,
        pcrit       = if (!is.null(input$pcritUndated)) input$pcritUndated else 0.05,
        biweight    = TRUE,
        method      = "spearman",
        series      = input$series2,
        datingNotes = input$undatingNotes,
        dateLog     = rwlRV$dateLog
      )
      params <- list(fileName1     = input$file1$name,
                     fileName2     = input$file2$name,
                     floaterObject = getFloater(),
                     undatedParams = undatedParams)
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir  = new.env(parent = globalenv()))
    }
  )
  
})
