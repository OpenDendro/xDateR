source("appHelpers.R")
source("editRing.R")
source("svgArt.R")
source("plot.floater.R")
source("plotCCF.R")
source("guide.R")
source("plotlyCRSFunc.R")

shinyServer(function(session, input, output) {

  # ── Reactive values ────────────────────────────────────────────────────────
  # Central store for mutable app state.
  #
  #   dated      — the working copy: EVERY series in the dated file, with any
  #                edits (ring insert/delete, gap fills) applied. Downloads
  #                and the Series/Edit panels read this.
  #   datedVault — the file as read, for Revert.
  #   excluded   — series left out of the master chronology by the filter on
  #                the Correlations panel. They stay in `dated`: they can
  #                still be selected, tested against the master, edited and
  #                downloaded. Only masterRWL() drops them.
  #   editDF     — one row per edit, replayed in the edit report's R code.
  rwlRV <- reactiveValues(
    dated         = NULL,
    datedVault    = NULL,
    excluded      = character(0),
    editLog       = NULL,
    editDF        = NULL,
    undated2dated = NULL,
    dateLog       = NULL
  )

  emptyEditDF <- function() {
    data.frame(series = character(0), year = numeric(0), value = numeric(0),
               action = character(0), fixLast = logical(0), fill = character(0),
               stringsAsFactors = FALSE)
  }
  rwlRV$editDF <- emptyEditDF()

  # ── Helper: build summary card from rwl.report() return value ─────────────
  # rwl.report() returns a named list (since dplR 1.7.7 it is no longer
  # invisible). We read fields directly rather than parsing printed output.
  # Takes the report, not the data: rwl.report() is slow on large files
  # (73 s on a 597-series file), so it is computed once per data set by the
  # cached reactives datedReport() and undatedReport().
  # Returns a div of label/value rows suitable for rendering inside a card.
  rwlSummaryCard <- function(rpt) {

    nZerosPct  <- if (rpt$n > 0) round(rpt$nZeros / rpt$n * 100, 2) else 0
    missingStr <- paste0(rpt$nZeros, " (", nZerosPct, "%)")

    items <- list(
      list(label = "Series",                   value = rpt$nSeries),
      list(label = "Total measurements",       value = rpt$n),
      list(label = "Missing rings (zeros)",    value = missingStr),
      list(label = "Mean series length",       value = round(rpt$meanSegLength, 0)),
      list(label = "Span",                     value = paste(rpt$firstYear,
                                                             "–",
                                                             rpt$lastYear)),
      list(label = "Mean interseries cor (SD)",
           value = paste0(round(rpt$meanInterSeriesCor, 3),
                          " (", round(rpt$sdInterSeriesCor, 3), ")")),
      list(label = "Mean AR1 (SD)",
           value = paste0(round(rpt$meanAR1, 3),
                          " (", round(rpt$sdAR1, 3), ")"))
    )

    rows <- lapply(items, function(item) {
      div(class = "row mb-1",
          div(class = "col-7 text-muted small",         item$label),
          div(class = "col-5 fw-bold small text-end",   as.character(item$value))
      )
    })
    div(rows)
  }

  # ── Helper: let the app react to a download ───────────────────────────────
  # A download is served outside the normal input cycle, so values changed
  # in its handler (the unsaved-work mark, the guided example's progress)
  # would not reach the page until the next input. Ask for a flush.
  # (The test harness's mock session refuses the call, hence tryCatch.)
  afterDownload <- function() {
    tryCatch(session$requestFlush(), error = function(e) NULL)
  }

  # ── Helper: a report download that fails readably ─────────────────────────
  # A downloadHandler() whose content function stops (for example the
  # settings are invalid, so the correlations cannot be computed) sends the
  # browser a server error, which it saves as "<button id>.html". Reports
  # use this wrapper instead: the file the user gets says what went wrong.
  safeDownload <- function(filename, content) {
    downloadHandler(filename = filename, content = function(file) {
      tryCatch(content(file), error = function(e) {
        msg <- paste0("xDateR could not make this report: ", conditionMessage(e))
        writeLines(if (grepl("[.]html?$", file, ignore.case = TRUE)) {
          c("<html><head><meta charset='utf-8'><title>Report not made</title></head>",
            "<body style='font-family: sans-serif; max-width: 40em; margin: 3em auto;'>",
            "<h2>Report not made</h2>",
            paste0("<p>", htmltools::htmlEscape(msg), "</p>"),
            "<p>Go back to xDateR, fix what the message describes, and generate the report again.</p>",
            "</body></html>")
        } else msg, file)
      })
    })
  }

  # ── Helper: run a dplR call, showing its error in place of the output ─────
  # dplR's error messages say what is wrong and what to do (e.g. the nyrs
  # spline refusing a series with gaps). Show them to the user instead of
  # Shiny's generic red error. req()'s silent errors pass through untouched.
  tryDplR <- function(expr) {
    tryCatch(expr, error = function(e) {
      if (inherits(e, "shiny.silent.error") || inherits(e, "validation")) stop(e)
      # dplR's message, then what can be done about it inside the app
      # (dplrAdvice()): dplR's own advice is written for R users
      msg <- conditionMessage(e)
      validate(need(FALSE, paste0("dplR stopped with this message: ", msg,
                                  if (!grepl("[.!?]$", msg)) ".",
                                  dplrAdvice(msg, isolate(colnames(rwlRV$dated))))))
    })
  }

  # ── Progressive sidebar disclosure ────────────────────────────────────────
  # Sidebar controls reveal as the user advances through the workflow:
  #   Overview:     Dated Series + About only
  #   Correlations: + Analysis Parameters
  #   Series/Edit:  + Series selector
  #   Floater:      + Undated Series (only if dated file is loaded)
  observeEvent(input$navbar, {
    tab <- input$navbar
    shinyjs::toggle("divSharedParams",
                    condition = tab %in% c("AllSeriesTab", "IndividualSeriesTab",
                                           "EditSeriesTab", "UndatedSeriesTab"))
    shinyjs::toggle("divSeriesSelector",
                    condition = tab %in% c("IndividualSeriesTab", "EditSeriesTab"))
  })

  # Undated sidebar section — separate observer so it reacts to BOTH
  # tab changes and dated file state (getRWL() going NULL/non-NULL).
  observe({
    shinyjs::toggle("divUndated",
                    condition = identical(input$navbar, "UndatedSeriesTab") &&
                      !is.null(getRWL()))
  })

  # ── File readers ───────────────────────────────────────────────────────────
  # readRWLsafely() never throws: it returns list(data, error). A file that
  # cannot be read shows dplR's error on the Overview (dated) or Floater
  # (undated) panel instead of ending the session.
  # ── Loaded dated files ─────────────────────────────────────────────────────
  # Every dated file loaded this session, so the user can switch between
  # them. files[[name]] = list(path, label); `active` is the current one.
  # `store` holds the working state of files that are not current (edited
  # data, master filter, edit log), so switching away and back keeps the
  # work. Loading a file with the name of one already loaded replaces it.
  exampleName <- "xDateRtest.rwl"
  filesRV <- reactiveValues(files = list(), active = NULL, store = list())
  
  # Saves the current file's work, then makes `name` current. The observer
  # on getRWL() below restores that file's saved work, if it has any.
  activateFile <- function(name) {
    old <- filesRV$active
    if (!is.null(old) && !identical(old, name) && !is.null(rwlRV$datedVault)) {
      filesRV$store[[old]] <- list(dated = rwlRV$dated, vault = rwlRV$datedVault,
                                   excluded = rwlRV$excluded,
                                   editLog = rwlRV$editLog, editDF = rwlRV$editDF,
                                   undated2dated = rwlRV$undated2dated,
                                   dateLog = rwlRV$dateLog)
    }
    filesRV$active <- name
  }
  
  addFile <- function(name, path, label = name) {
    nEdits <- if (identical(name, filesRV$active)) nrow(rwlRV$editDF) else
      if (!is.null(filesRV$store[[name]])) nrow(filesRV$store[[name]]$editDF) else 0
    if (nEdits > 0) {
      showNotification(paste0("Reloaded ", name, ". The ", nEdits,
                              " edit(s) made to the earlier copy were discarded."),
                       type = "warning", duration = 10)
    }
    filesRV$store[[name]] <- NULL
    unsaved$files <- setdiff(unsaved$files, name)
    files <- filesRV$files
    files[[name]] <- list(path = path, label = label)
    filesRV$files <- files
    activateFile(name)
  }
  
  observeEvent(input$file1, addFile(input$file1$name, input$file1$datapath))
  observeEvent(input$useDemoDated,
               addFile(exampleName, "data/xDateRtest.rwl",
                       label = "Example data (xDateRtest.rwl)"))
  observeEvent(input$activeFile, {
    if (!identical(input$activeFile, filesRV$active)) activateFile(input$activeFile)
  })
  
  datedName <- reactive(filesRV$active)
  datedPath <- reactive({
    req(filesRV$active)
    filesRV$files[[filesRV$active]]$path
  })
  
  datedRead <- reactive({
    if (is.null(filesRV$active)) return(list(data = NULL, error = NULL))
    readRWLsafely(datedPath())
  })
  getRWL <- reactive(datedRead()$data)
  
  # ── File widget: switcher, and a summary of the current file ─────────────
  output$datedFileUI <- renderUI({
    files <- filesRV$files
    if (length(files) == 0) {
      return(div(class = "mb-2 small",
                 "No file loaded.",
                 actionLink("useDemoDated", "Try the example data to explore and see a guide.")))
    }
    labels <- vapply(files, `[[`, "", "label")
    tagList(
      if (length(files) > 1) {
        selectInput("activeFile", "Switch file",
                    choices  = setNames(rev(names(files)), rev(labels)),
                    selected = filesRV$active)
      } else {
        div(class = "fw-bold text-truncate", title = labels[[1]],
            bs_icon("file-earmark-text"), " ", labels[[1]])
      },
      if (!exampleName %in% names(files)) {
        div(class = "small mb-1", actionLink("useDemoDated", "Add the example data"))
      }
    )
  })
  
  output$datedFileInfo <- renderUI({
    req(filesRV$active)
    if (!is.null(datedRead()$error)) {
      return(div(class = "small text-danger mb-2", bs_icon("x-circle"),
                 " Could not be read: see the Overview."))
    }
    dat <- rwlRV$dated
    req(dat)
    yrs   <- range(as.numeric(rownames(dat)))
    nGaps <- nrow(datedGaps())
    nEd   <- nrow(rwlRV$editDF)
    nEx   <- length(rwlRV$excluded)
    badge <- function(n, what, cls) {
      if (n > 0) span(class = paste("badge me-1", cls), n, what)
    }
    tagList(
      div(class = "small text-muted mb-2",
          div(ncol(dat), " series \u00b7 ", yrs[1], "\u2013", yrs[2]),
          div(badge(nGaps, if (nGaps == 1) "gap" else "gaps", "bg-warning text-dark"),
              badge(nEd, if (nEd == 1) "edit" else "edits", "bg-info text-dark"),
              badge(nEx, "out of master", "bg-secondary"))),
      if (nEd > 0) downloadButton("downloadRWLside", "Download edited file",
                                  class = "xd-btn-quiet btn-sm w-100 mb-2"))
  })

  # ── Loaded undated files ───────────────────────────────────────────────────
  # The same widget as for the dated file: every undated file loaded this
  # session, one current, a switcher once there are two. There is no
  # per-file work to keep here: the series dated so far (undated2dated) are
  # kept across undated files, since they are results against the master.
  exampleUndated <- "xDateRtestUndated.rwl"
  ufilesRV <- reactiveValues(files = list(), active = NULL)
  
  addUndated <- function(name, path, label = name) {
    files <- ufilesRV$files
    files[[name]] <- list(path = path, label = label)
    ufilesRV$files  <- files
    ufilesRV$active <- name
  }
  observeEvent(input$file2, addUndated(input$file2$name, input$file2$datapath))
  observeEvent(input$useDemoUndated,
               addUndated(exampleUndated, "data/xDateRtestUndated.rwl",
                          label = "Example data (xDateRtestUndated.rwl)"))
  observeEvent(input$activeUndated, {
    if (!identical(input$activeUndated, ufilesRV$active)) ufilesRV$active <- input$activeUndated
  })
  undatedName <- reactive(ufilesRV$active)
  
  undatedRead <- reactive({
    if (is.null(ufilesRV$active)) return(list(data = NULL, error = NULL))
    readRWLsafely(ufilesRV$files[[ufilesRV$active]]$path)
  })
  getRWLUndated <- reactive(undatedRead()$data)
  
  output$undatedFileUI <- renderUI({
    files <- ufilesRV$files
    if (length(files) == 0) {
      return(div(class = "mb-2 small",
                 "No file loaded.",
                 actionLink("useDemoUndated", "Try the example data to explore and see a guide.")))
    }
    
    labels <- vapply(files, `[[`, "", "label")
    tagList(
      if (length(files) > 1) {
        selectInput("activeUndated", "Switch file",
                    choices  = setNames(rev(names(files)), rev(labels)),
                    selected = ufilesRV$active)
      } else {
        div(class = "fw-bold text-truncate", title = labels[[1]],
            bs_icon("file-earmark-text"), " ", labels[[1]])
      },
      if (!exampleUndated %in% names(files)) {
        div(class = "small mb-1", actionLink("useDemoUndated", "Add the example data"))
      }
    )
  })
  
  output$undatedFileInfo <- renderUI({
    req(ufilesRV$active)
    if (!is.null(undatedRead()$error)) {
      return(div(class = "small text-danger mb-2", bs_icon("x-circle"),
                 " Could not be read: see the Floater panel."))
    }
    und <- getRWLUndated()
    req(und)
    nSaved <- if (is.null(rwlRV$undated2dated)) 0 else ncol(rwlRV$undated2dated)
    div(class = "small text-muted mb-2",
        div(ncol(und), " series \u00b7 up to ", max(colSums(!is.na(und))), " rings"),
        if (nSaved > 0) div(span(class = "badge bg-info text-dark", nSaved, "dated")))
  })

  # ── Current dated file changed: restore its saved work, or start fresh ───
  observeEvent(getRWL(), {
    dat   <- getRWL()
    saved <- if (!is.null(filesRV$active)) filesRV$store[[filesRV$active]]
    if (!is.null(dat) && !is.null(saved)) {
      rwlRV$dated      <- saved$dated
      rwlRV$datedVault <- saved$vault
      rwlRV$excluded   <- saved$excluded
      rwlRV$editLog    <- saved$editLog
      rwlRV$editDF     <- saved$editDF
    } else {
      rwlRV$dated      <- dat
      rwlRV$datedVault <- dat
      rwlRV$excluded   <- character(0)
      rwlRV$editLog    <- NULL
      rwlRV$editDF     <- emptyEditDF()
    }
    # Series dated on the Floater panel belong to the master they were dated
    # against, so they are kept with their dated file
    rwlRV$undated2dated <- if (!is.null(dat) && !is.null(saved)) saved$undated2dated
    rwlRV$dateLog       <- if (!is.null(dat) && !is.null(saved)) saved$dateLog
    nms <- if (is.null(dat)) character(0) else colnames(dat)
    updateSelectizeInput(session, "leaveOut", choices = nms,
                         selected = rwlRV$excluded, server = length(nms) > 200)
  }, ignoreNULL = FALSE)

  # Series selector: every series in the file. Series left out of the master
  # are labelled so the user knows they are being tested against a master
  # they are not part of. Only fires on a new file or a filter change, not on
  # every edit, so the selection is not reset.
  # A warning sign marks series with an error or warning from the data
  # checks (rwl.check()), so problems are visible from every panel.
  observeEvent(list(getRWL(), rwlRV$excluded, flaggedSeries()), {
    nms <- colnames(getRWL())
    if (is.null(nms)) {
      updateSelectInput(session, "series", choices = c("Load a file first" = ""))
      return()
    }
    lbl <- ifelse(nms %in% flaggedSeries(), paste(nms, "\u26a0"), nms)
    lbl <- ifelse(nms %in% rwlRV$excluded, paste(lbl, "(not in master)"), lbl)
    sel <- if (isTRUE(input$series %in% nms)) input$series else nms[1]
    updateSelectInput(session, "series", choices = setNames(nms, lbl),
                      selected = sel)
  }, ignoreNULL = FALSE)

  observeEvent(getRWLUndated(), {
    updateSelectInput(
      session  = session,
      inputId  = "series2",
      choices  = colnames(getRWLUndated()),
      selected = colnames(getRWLUndated())[1]
    )
  })

  flaggedSeries <- reactive({
    f <- tryCatch(rwlCheck(), error = function(e) NULL)
    if (is.null(f)) return(character(0))
    sort(unique(f$series[f$severity %in% c("error", "warning") & !is.na(f$series)]))
  })
  
  # ── Start over ─────────────────────────────────────────────────────────────
  # Reloads the session: every file, edit and setting is cleared. Asks first
  # when that would throw away work (edits in any loaded file, or dates saved
  # on the Floater panel).
  observe(shinyjs::toggle("divStartOver", condition = length(filesRV$files) > 0))
  
  observeEvent(input$startOver, {
    nEdits <- nrow(rwlRV$editDF) +
      sum(vapply(filesRV$store, function(x) nrow(x$editDF), 0))
    nDated <- if (is.null(rwlRV$undated2dated)) 0 else ncol(rwlRV$undated2dated)
    if (nEdits == 0 && nDated == 0) return(session$reload())
    showModal(modalDialog(
      title = "Start over?",
      p("This closes every file and starts a fresh session. You would lose:"),
      tags$ul(
        if (nEdits > 0) tags$li(nEdits, if (nEdits == 1) "edit" else "edits",
                                "(download the edited .rwl from the Edit panel to keep them)"),
        if (nDated > 0) tags$li(nDated, if (nDated == 1) "series" else "series",
                                "dated on the Floater panel (download them there)")),
      footer = tagList(modalButton("Cancel"),
                       actionButton("startOverConfirm", "Start over",
                                    class = "btn-danger")),
      easyClose = TRUE))
  })
  observeEvent(input$startOverConfirm, {
    # the user has just confirmed: don't let the browser ask again
    session$sendCustomMessage("xdUnsaved", FALSE)
    session$reload()
  })
  
  # ── Guided example ─────────────────────────────────────────────────────────
  # A checklist in the sidebar, offered when the example data are current
  # (steps and text in guide.R). A step is done when the app's own state
  # says so, and stays done; the steps must be done in order.
  guideRV <- reactiveValues(on = FALSE, done = character(0), answer = FALSE,
                            savedFile = FALSE, savedReport = FALSE,
                            onF = FALSE, doneF = character(0), savedFloater = FALSE)
  guideIds   <- vapply(guideSteps, `[[`, "", "id")
  guideHere  <- reactive(identical(filesRV$active, exampleName) && !is.null(rwlRV$dated))
  guideStep  <- reactive(match(FALSE, guideIds %in% guideRV$done))   # NA when finished
  
  observe({
    req(guideRV$on, guideHere())
    k <- guideStep()
    if (is.na(k)) return()
    tab <- input$navbar
    s   <- input$series
    # B flags left for a series, and whether the user has edited it
    clean <- function(series) {
      f <- tryCatch(crsFlags(), error = function(e) NULL)
      !is.null(f) && !any(f$series == series & f$flag == "B") &&
        any(rwlRV$editDF$series == series)
    }
    done <- switch(guideIds[k],
      # reading steps (checks, look, confirm) are also finished with Next
      checks  = identical(tab, "AllSeriesTab"),
      find    = identical(tab, "IndividualSeriesTab") && identical(s, "ABC118"),
      look    = identical(tab, "EditSeriesTab") && identical(s, "ABC118"),
      fix     = clean("ABC118"),
      confirm = FALSE,
      own     = clean("ABC104"),
      save    = guideRV$savedFile && guideRV$savedReport)
    if (isTRUE(done)) {
      guideRV$done   <- c(guideRV$done, guideIds[k])
      guideRV$answer <- FALSE
    }
  })
  
  observeEvent(input$guideStart, {
    guideRV$on <- TRUE
    # saving counts from when the guide reaches that step, not before
    guideRV$savedFile <- FALSE
    guideRV$savedReport <- FALSE
  })
  observeEvent(input$guideHide, guideRV$on <- FALSE)
  # Next: finishes a step that is about reading something
  observeEvent(input$guideNext, {
    k <- guideStep()
    req(!is.na(k), isTRUE(guideSteps[[k]]$read))
    guideRV$done   <- c(guideRV$done, guideIds[k])
    guideRV$answer <- FALSE
  })
  observeEvent(input$guideAnswer, guideRV$answer <- TRUE)
  observeEvent(input$guideRestart, {
    guideRV$done <- character(0)
    guideRV$answer <- FALSE
    guideRV$savedFile <- FALSE
    guideRV$savedReport <- FALSE
  })
  # "Take me there": the step's panel, series and Edit window
  observeEvent(input$guideGo, {
    k <- guideStep()
    req(!is.na(k))
    st <- guideSteps[[k]]
    if (!is.null(st$series)) updateSelectInput(session, "series", selected = st$series)
    if (!is.null(st$year)) setWindowCenter(st$year)
    nav_select("navbar", st$tab, session = session)
  })
  
  # The guide card, shared by both guides. `ids` are the input ids of its
  # buttons; `here` says whether the user is where the step happens.
  guideCard <- function(label, steps, k, finished, here, answerShown, ids) {
    box <- function(...) div(class = "border rounded p-2 mb-2 small",
                             style = "background: var(--bs-body-bg);", ...)
    if (is.na(k)) {
      return(box(div(class = "fw-bold text-success", bs_icon("check-circle"),
                     paste0(" ", label, " finished")),
                 p(class = "mb-2 mt-1", HTML(finished)),
                 actionLink(ids$restart, "Start again"), " \u00b7 ",
                 actionLink(ids$hide, "Hide guide")))
    }
    st <- steps[[k]]
    box(div(class = "text-muted", bs_icon("signpost-2"),
            sprintf(" %s \u00b7 step %d of %d", label, k, length(steps))),
        div(class = "fw-bold mt-1", st$title),
        p(class = "mb-2", HTML(st$text)),
        if (!is.null(st$answer)) {
          if (answerShown) div(class = "alert alert-secondary py-1 px-2 mb-2", HTML(st$answer))
          else div(class = "mb-2", actionLink(ids$answer, "Show the answer"))
        },
        # One button, fitted to where the user is: "Take me there" when the
        # step is on another panel (or another series); "Next" when they are
        # there and the step is about reading; nothing when the step is
        # something to do there, which finishes itself.
        if (!here) {
          actionButton(ids$go, "Take me there", class = "btn-primary btn-sm")
        } else if (isTRUE(st$read)) {
          actionButton(ids$nxt, "Next", class = "btn-primary btn-sm")
        } else {
          span(class = "text-muted", "This step finishes when you have done it.")
        },
        div(class = "mt-1", actionLink(ids$hide, "Hide guide")))
  }
  
  output$guideUI <- renderUI({
    if (!guideHere()) return(NULL)
    if (!guideRV$on) {
      return(div(class = "small mb-2",
                 actionLink("guideStart", tagList(bs_icon("signpost-2"),
                                                  " Show me how: a guided crossdating example"))))
    }
    k  <- guideStep()
    st <- if (!is.na(k)) guideSteps[[k]]
    guideCard("Guided example", guideSteps, k, guideFinished,
              here = !is.na(k) && identical(input$navbar, st$tab) &&
                (is.null(st$series) || identical(input$series, st$series)),
              answerShown = guideRV$answer,
              ids = list(go = "guideGo", nxt = "guideNext", hide = "guideHide",
                         answer = "guideAnswer", restart = "guideRestart"))
  })
  
  # ── The Floater guide ──────────────────────────────────────────────────────
  # The second guide (floaterSteps in guide.R): dating the undated example
  # series against the example master. Offered under the first; starting
  # either one hides the other.
  guideIdsF  <- vapply(floaterSteps, `[[`, "", "id")
  guideStepF <- reactive(match(FALSE, guideIdsF %in% guideRV$doneF))
  
  observe({
    req(guideRV$onF, guideHere())
    k <- guideStepF()
    if (is.na(k)) return()
    done <- switch(guideIdsF[k],
      load     = identical(ufilesRV$active, exampleUndated),
      save     = "ABC119" %in% names(rwlRV$undated2dated),
      pick     = identical(input$series2, "ABC110"),
      download = guideRV$savedFloater && "ABC110" %in% names(rwlRV$undated2dated),
      FALSE)   # fit, segs, inside: finished with Next
    if (isTRUE(done)) guideRV$doneF <- c(guideRV$doneF, guideIdsF[k])
  })
  
  observeEvent(input$guideStartF, {
    guideRV$onF <- TRUE
    guideRV$on  <- FALSE
    guideRV$savedFloater <- FALSE
  })
  observeEvent(input$guideStart, guideRV$onF <- FALSE)
  observeEvent(input$guideHideF, guideRV$onF <- FALSE)
  observeEvent(input$guideNextF, {
    k <- guideStepF()
    req(!is.na(k), isTRUE(floaterSteps[[k]]$read))
    guideRV$doneF <- c(guideRV$doneF, guideIdsF[k])
  })
  observeEvent(input$guideRestartF, {
    guideRV$doneF <- character(0)
    guideRV$savedFloater <- FALSE
  })
  observeEvent(input$guideGoF, {
    k <- guideStepF()
    req(!is.na(k))
    st <- floaterSteps[[k]]
    nav_select("navbar", "UndatedSeriesTab", session = session)
    if (!is.null(st$series2) && !is.null(getRWLUndated()) &&
        st$series2 %in% colnames(getRWLUndated())) {
      updateSelectInput(session, "series2", selected = st$series2)
    }
  })
  
  output$guideUIF <- renderUI({
    if (!guideHere()) return(NULL)
    if (!guideRV$onF) {
      return(div(class = "small mb-2",
                 actionLink("guideStartF", tagList(bs_icon("signpost-2"),
                                                   " Show me how: dating a floater"))))
    }
    k  <- guideStepF()
    st <- if (!is.na(k)) floaterSteps[[k]]
    guideCard("Floater guide", floaterSteps, k, floaterFinished,
              here = !is.na(k) && identical(input$navbar, "UndatedSeriesTab") &&
                (is.null(st$series2) || identical(input$series2, st$series2)),
              answerShown = FALSE,
              ids = list(go = "guideGoF", nxt = "guideNextF", hide = "guideHideF",
                         answer = "guideAnswerF", restart = "guideRestartF"))
  })
  
  # ── Unsaved work ───────────────────────────────────────────────────────────
  # Files with edits made since they were last downloaded, and floater dates
  # saved but not downloaded. While there are any, the browser asks before
  # the tab is closed or reloaded (see the script in ui.R): everything lives
  # in the session, so leaving discards it.
  unsaved <- reactiveValues(files = character(0), floater = FALSE)
  observe({
    session$sendCustomMessage("xdUnsaved",
                              length(unsaved$files) > 0 || isTRUE(unsaved$floater))
  })
  
  # ── Panels that need a file ────────────────────────────────────────────────
  # Correlations, Series and Edit show a prompt instead of empty cards and
  # live buttons until a dated file is loaded.
  needsFile <- c("AllSeriesTab", "IndividualSeriesTab", "EditSeriesTab")
  lapply(needsFile, function(tab) {
    output[[paste0("noData", tab)]] <- renderUI({
      if (!is.null(rwlRV$dated)) return(NULL)
      div(class = "alert alert-secondary mt-3",
          bs_icon("folder2-open"), " ",
          if (!is.null(datedRead()$error)) {
            "The file could not be read: see the Overview panel."
          } else {
            tagList(tags$strong("Load a ring-width file to begin. "),
                    "Use ", tags$strong("Load a file\u2026"), " in the sidebar, or try",
                    " the example data.")
          })
    })
  })
  observe({
    for (tab in needsFile) {
      shinyjs::toggle(paste0("content", tab), condition = !is.null(rwlRV$dated))
    }
  })
  
  # ── Master filter ──────────────────────────────────────────────────────────
  # "Update Master" records which series to leave out of the master. It no
  # longer rebuilds the working data, so edits survive it.
  observeEvent(input$updateMasterButton, {
    req(rwlRV$dated)
    rwlRV$excluded <- intersect(colnames(rwlRV$dated), input$leaveOut)
  })

  # The series that build the master chronology: the working copy (with
  # edits) minus the excluded series.
  masterRWL <- reactive({
    req(rwlRV$dated)
    keep <- setdiff(colnames(rwlRV$dated), rwlRV$excluded)
    rwlRV$dated[, keep, drop = FALSE]
  })

  # TRUE when input$series names a series in the current file. Guards the
  # moment after a new file loads, before the selector has been updated.
  seriesOK <- reactive({
    !is.null(rwlRV$dated) && isTRUE(input$series %in% colnames(rwlRV$dated))
  })

  # The selected series and the master to test it against: every series in
  # the master except the selected one (leave-one-out). An excluded series
  # is tested against the master it is not part of. Both are always whole:
  # dplR filters each series it is given, so nothing is cut to a window
  # before it is analysed (see plotCCF.R).
  seriesInputs <- function(series) {
    dat <- rwlRV$dated
    m   <- dat[, setdiff(colnames(dat), c(rwlRV$excluded, series)), drop = FALSE]
    v   <- setNames(dat[[series]], rownames(dat))
    list(rwl = m, series = v)
  }

  # ── Analysis parameters ────────────────────────────────────────────────────
  # One list read by every dplR call, the floater and the reports.
  # Low-frequency filter: n (Hanning) and nyrs (spline) cannot both be set in
  # dplR >= 1.8.0, so the UI offers one choice of filter. ar.order.max only
  # applies when prewhitening.
  #
  # Debounced: the number boxes (P crit, Lag search, nyrs) change with every
  # keystroke, and on a large file each change used to start a full
  # recomputation (typing "0.01" ran corr.rwl.seg() three times, about two
  # minutes on a 597-series file). The settings now take effect once they
  # have been still for xdater.debounce.ms milliseconds (default 500; the
  # tests set 0, which turns it off).
  xdParamsNow <- reactive({
    lf <- if (is.null(input$lowFreq)) "none" else input$lowFreq
    if (lf == "spline") {
      validate(need(isTRUE(input$nyrs > 0),
                    "Spline rigidity (nyrs) must be a number greater than 0."))
    }
    lagMax <- if (is.null(input$lag.max)) 0 else input$lag.max
    validate(need(isTRUE(lagMax >= 0 && lagMax == round(lagMax)),
                  "Lag search must be a whole number of years, 0 or more."))
    if (!is.null(input$seg.length)) {
      validate(need(lagMax < input$seg.length, paste0(
        "Lag search (", lagMax, " years) must be less than the segment length (",
        input$seg.length, " years). Reduce it in the Analysis Parameters.")))
    }
    list(
      seg.length   = input$seg.length,
      bin.floor    = as.numeric(input$bin.floor),
      n            = if (lf == "hanning") as.numeric(input$n) else NULL,
      nyrs         = if (lf == "spline") input$nyrs else NULL,
      prewhiten    = isTRUE(input$prewhiten),
      ar.order.max = if (isTRUE(input$prewhiten)) resolveN(input$ar.order.max) else NULL,
      pcrit        = input$pcrit,
      biweight     = isTRUE(input$biweight),
      method       = input$method,
      lag.max      = lagMax
    )
  })
  debounceMs <- getOption("xdater.debounce.ms", 500)
  xdParams <- if (debounceMs > 0) debounce(xdParamsNow, debounceMs) else xdParamsNow
  
  # ── COFECHA preset ────────────────────────────────────────────────────────
  # The defaults of dplR's xdate.report(), which follow COFECHA.
  observeEvent(input$cofechaPreset, {
    updateSliderInput(session, "seg.length", value = 50)
    updateSelectInput(session, "bin.floor", selected = "100")
    updateRadioButtons(session, "lowFreq", selected = "spline")
    updateNumericInput(session, "nyrs", value = 32)
    updateCheckboxInput(session, "prewhiten", value = TRUE)
    updateSelectInput(session, "ar.order.max", selected = "3")
    updateNumericInput(session, "pcrit", value = 0.01)
    updateNumericInput(session, "lag.max", value = 10)
    updateSelectInput(session, "method", selected = "pearson")
    updateCheckboxInput(session, "biweight", value = TRUE)
    showNotification("COFECHA-like settings applied.", type = "message")
  })
  
  # The settings the app starts with
  observeEvent(input$resetParams, {
    updateSliderInput(session, "seg.length", value = 50)
    updateSelectInput(session, "bin.floor", selected = "10")
    updateRadioButtons(session, "lowFreq", selected = "none")
    updateSelectInput(session, "n", selected = "7")
    updateNumericInput(session, "nyrs", value = 32)
    updateCheckboxInput(session, "prewhiten", value = TRUE)
    updateSelectInput(session, "ar.order.max", selected = "NULL")
    updateNumericInput(session, "pcrit", value = 0.05)
    updateNumericInput(session, "lag.max", value = 5)
    updateSelectInput(session, "method", selected = "spearman")
    updateCheckboxInput(session, "biweight", value = TRUE)
    showNotification("Settings reset to the defaults.", type = "message")
  })
  
  # The settings in effect, in one line under the (usually closed) accordion
  output$paramSummary <- renderUI({
    p <- tryCatch(xdParamsNow(), error = function(e) NULL)
    if (is.null(p) || is.null(p$seg.length)) return(NULL)
    div(class = "small text-muted mt-2",
        paste(c(paramSummary(p)), collapse = " \u00b7 "))
  })

  # ── Edit window sliders ────────────────────────────────────────────────────
  # Window centre and width constrain each other (see windowBounds() in
  # appHelpers.R). Each slider is built once per series, and rebuilt only
  # when the data change, keeping its current value where it still fits.
  # After that the two constrain each other through updateSliderInput()
  # rather than being rebuilt, so dragging one doesn't redraw the other.
  # winWidth.ui renders even while hidden so input$winWidth is never NULL.
  editWindow <- function(center, width) {
    windowBounds(seriesSpan(rwlRV$dated, input$series), center, width)
  }
  
  # A window centre asked for by a jump (a click on the Correlations panel,
  # or the guided example). It is held in a plain variable and announced
  # with a counter: the slider is rebuilt ONCE, with that centre, and the
  # variable cleared without triggering another rebuild. (Clearing a
  # reactive value here rebuilt the slider a second time with the old
  # centre while the new one was on its way back from the browser, and the
  # two values then chased each other without end.)
  jump     <- new.env()
  jumpTick <- reactiveVal(0)
  setWindowCenter <- function(year) {
    jump$center <- round(year / 5) * 5
    jumpTick(isolate(jumpTick()) + 1)
  }
  sent <- new.env()   # the slider limits the browser currently has
  
  output$winCenter.ui <- renderUI({
    req(seriesOK())
    jumpTick()
    pc <- jump$center
    jump$center <- NULL
    b <- editWindow(if (!is.null(pc)) pc else isolate(input$winCenter),
                    isolate(input$winWidth))
    sent$center <- c(b$minCenter, b$maxCenter)
    sliderInput("winCenter", "Window Center",
                min = b$minCenter, max = b$maxCenter, value = b$center,
                step = 5, sep = "", ticks = FALSE)
  })
  
  output$winWidth.ui <- renderUI({
    if (!seriesOK()) {
      return(sliderInput("winWidth", "Window width (years)",
                         min = 20, max = 100, value = 40, step = 10, ticks = FALSE))
    }
    b <- editWindow(isolate(input$winCenter), isolate(input$winWidth))
    sent$width <- b$maxWidth
    sliderInput("winWidth", "Window width (years)",
                min = 20, max = b$maxWidth, value = b$width,
                step = 10, ticks = FALSE)
  })
  outputOptions(output, "winWidth.ui", suspendWhenHidden = FALSE)
  
  # The two sliders limit each other. Only what has changed is sent to the
  # browser: new limits when they differ from the ones it has, and a value
  # only when the current one no longer fits. Sending a slider the value it
  # already has echoes the input back to the server, and an echo can loop.
  observeEvent(list(input$winCenter, input$winWidth), {
    req(seriesOK(), input$winCenter, input$winWidth)
    b <- editWindow(input$winCenter, input$winWidth)
    cen <- list()
    if (!identical(c(b$minCenter, b$maxCenter), sent$center)) {
      cen$min <- b$minCenter
      cen$max <- b$maxCenter
      sent$center <- c(b$minCenter, b$maxCenter)
    }
    if (b$center != input$winCenter) cen$value <- b$center
    if (length(cen)) do.call(updateSliderInput, c(list(session, "winCenter"), cen))
    wid <- list()
    if (!identical(b$maxWidth, sent$width)) {
      wid$max <- b$maxWidth
      sent$width <- b$maxWidth
    }
    if (b$width != input$winWidth) wid$value <- b$width
    if (length(wid)) do.call(updateSliderInput, c(list(session, "winWidth"), wid))
  })
  
  # ── rangeCCF: plotted years for the Series panel CCF ──────────────────────
  # Its own renderUI so moving the Edit panel's window no longer resets it.
  # Keeps the user's range when it still fits the series.
  output$rangeCCF <- renderUI({
    req(seriesOK())
    lag    <- if (isTRUE(input$lagCCF > 0)) input$lagCCF else 5
    sBnds  <- seriesSpan(rwlRV$dated, input$series)
    minWin <- round(sBnds[1] + lag, -1)
    maxWin <- round(sBnds[2] - lag, -1)
    cur    <- isolate(input$rangeCCF)
    val    <- if (!is.null(cur) && cur[1] >= minWin && cur[2] <= maxWin) cur else c(minWin, maxWin)
    sliderInput("rangeCCF", "Adjust plotted years",
                min = minWin, max = maxWin, value = val,
                step = 5, sep = "", dragRange = TRUE, ticks = FALSE)
  })


  # ── rwlStats: derived series-length stats used by dynamic widgets ─────────
  # minLen: min series length in the master rounded down to nearest 10
  #   (minimum 10). Used as the seg.length max — any series shorter than the
  #   segment length will produce zero-length bins and crash corr.rwl.seg().
  rwlStats <- reactive({
    dat     <- masterRWL()
    lengths <- colSums(!is.na(dat))
    list(minLen = max(10, floor(min(lengths) / 10) * 10))
  })

  # ── seg.length.ui: dynamic segment length slider ───────────────────────────
  # suspendWhenHidden = FALSE ensures the slider renders even when the
  # Analysis Parameters accordion is closed, so input$seg.length is never NULL.
  output$seg.length.ui <- renderUI({
    if (is.null(rwlRV$dated)) {
      sliderInput("seg.length", "Segment Length",
                  min = 10, max = 100, value = 50, step = 10, ticks = FALSE)
    } else {
      maxSeg <- max(10, rwlStats()$minLen)
      cur    <- isolate(input$seg.length)
      val    <- if (!is.null(cur) && cur <= maxSeg) cur else min(50, maxSeg)
      sliderInput("seg.length", "Segment Length",
                  min = 10, max = maxSeg, value = val, step = 10,
                  ticks = FALSE)
    }
  })
  outputOptions(output, "seg.length.ui", suspendWhenHidden = FALSE)

  # ── rwlQA: tiered data quality check on the master ────────────────────────
  # Returns a list:
  #   tier    — 0 = ok, 1 = hard block, 2 = soft warning, 3 = advisory
  #   message — plain-language explanation for the user
  #   short   — character vector of series names that are too short (tier 2)
  #
  # Tier 1 (hard block): fewer than 5 series OR all series shorter than seg.length
  # Tier 2 (soft warn):  some (not all) series shorter than 1.5 * seg.length
  # Tier 3 (advisory):   mean series length < 3 * seg.length
  rwlQA <- reactive({
    req(input$seg.length)
    dat     <- masterRWL()
    segLen  <- input$seg.length
    lengths <- colSums(!is.na(dat))
    nSeries <- ncol(dat)

    if (nSeries < 5) {
      return(list(
        tier    = 1,
        message = paste0(
          "The master has only ", nSeries, " series. Statistical crossdating ",
          "requires at least 5 series to build a meaningful master chronology. ",
          if (length(rwlRV$excluded)) {
            "Put more series back in the master using the filter on the Correlations panel, or load a file with more series."
          } else {
            "Please load a file with more series."
          }
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

    shortNames <- names(lengths[lengths < 1.5 * segLen])
    if (length(shortNames) > 0) {
      # The title names the series (or counts them); the message adds what
      # the title doesn't say: how long they are and how long they need to be.
      one    <- length(shortNames) == 1
      need   <- ceiling(1.5 * segLen)
      advice <- paste0(" Consider leaving ", if (one) "it" else "them",
                       " out of the master with the filter on the Correlations",
                       " panel, or reducing the segment length.")
      return(list(
        tier    = 2,
        title   = if (one) {
          paste0("Series ", shortNames, " is short for ", segLen, "-year segments")
        } else {
          paste0(length(shortNames), " series are short for ", segLen, "-year segments")
        },
        message = if (one) {
          paste0("It has ", lengths[[shortNames]], " rings; reliable crossdating at",
                 " this segment length needs at least ", need,
                 " (1.5 \u00d7 the segment length).", advice)
        } else {
          paste0("Reliable crossdating at this segment length needs at least ", need,
                 " rings (1.5 \u00d7 the segment length). These have fewer: ",
                 paste0(shortNames, " (", lengths[shortNames], ")", collapse = ", "),
                 ".", advice)
        },
        short   = shortNames
      ))
    }

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
  # Runs corr.rwl.seg() on the master. All correlation panels read from this
  # single reactive so parameter changes propagate everywhere.
  #
  # The expensive reactives (getCRS, getFloater, rwlCheck, rwlCheckOriginal,
  # datedReport) are cached for the session with bindCache(), keyed by the
  # data and settings they use: going back to earlier settings, switching
  # back to another loaded file, or reverting edits is then instant.
  # Errors are not cached. cache = "session" keeps one user's results out of
  # another's memory on the server.
  getCRS <- reactive({
    qa <- rwlQA()
    validate(need(qa$tier != 1, qa$message))
    p <- xdParams()
    tryDplR(do.call(corr.rwl.seg, c(
      list(rwl = masterRWL()),
      p[c("seg.length", "bin.floor", normArgs, "pcrit", "method", "lag.max")],
      list(make.plot = FALSE)
    )))
  }) |> bindCache(masterRWL(), xdParams(), cache = "session")

  # ── getFloater ─────────────────────────────────────────────────────────────
  # Runs dplR::xdate.floater() for the selected undated series against the
  # master, with the shared analysis parameters. make.plot = FALSE and
  # verbose = FALSE because the app handles display itself.
  getFloater <- reactive({
    req(getRWLUndated(), input$minOverlapUndated,
        isTRUE(input$series2 %in% colnames(getRWLUndated())))
    p  <- xdParams()
    fo <- tryDplR(do.call(dplR::xdate.floater, c(
      list(rwl         = masterRWL(),
           series      = getRWLUndated()[, input$series2],
           series.name = input$series2,
           min.overlap = input$minOverlapUndated),
      p[c(normArgs, "method")],
      list(make.plot = FALSE, verbose = FALSE, return.rwl = TRUE)
    )))
    fo$series.name <- input$series2
    fo
  }) |> bindCache(masterRWL(), getRWLUndated(), input$series2,
                  input$minOverlapUndated, xdParams()[c(normArgs, "method")],
                  cache = "session")


  # ════════════════════════════════════════════════════════════════════════════
  # OVERVIEW
  # ════════════════════════════════════════════════════════════════════════════

  # Interior gaps in the working data (years with no measurement inside a
  # series). dplR >= 1.8.0 reads these as NA; older versions read them as 0.
  datedGaps <- reactive({
    req(rwlRV$dated)
    rwlGaps(rwlRV$dated)
  })

  # ── overviewUI: switches between welcome state and data state ─────────────
  # IMPORTANT: uiOutput("rwlSummaryHeader") inside the data state is a
  # placeholder — it requires its own output$rwlSummaryHeader renderUI below.
  # Nested uiOutputs always need their own registered render calls.
  output$overviewUI <- renderUI({
    readError <- datedRead()$error
    if (is.null(getRWL()) && is.null(readError)) return(HTML(welcomeSVG))

    if (!is.null(readError)) {
      return(div(class = "alert alert-danger",
                 bs_icon("x-circle"), " ",
                 tags$strong("File could not be read."),
                 " dplR's ", tags$code("read.rwl()"), " returned the following error:",
                 tags$pre(class = "mt-2 mb-1", style = "font-size:0.85em;",
                          readError),
                 "Please open a plain R session, load dplR, and run ",
                 tags$code('read.rwl("your-file.rwl")'),
                 " to diagnose the problem. Fix the file and then reload it here."
      ))
    }

    tagList(
      uiOutput("checkPanel"),
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
  })

  observeEvent(input$fillGapsButton, {
    req(rwlRV$dated, input$fillSeries, input$fillMethod)
    fill <- if (input$fillMethod == "0") 0 else input$fillMethod
    res  <- tryCatch(fillGaps(rwlRV$dated, input$fillSeries, fill),
                     error = function(e) e)
    if (inherits(res, "error")) {
      showNotification(conditionMessage(res), type = "error", duration = NULL)
      return()
    }
    rwlRV$dated <- res
    fillLbl <- if (identical(fill, 0)) "zero" else tolower(fill)
    logEdit(paste0("Gaps filled with ", fillLbl, " in ",
                   paste(input$fillSeries, collapse = ", "), "."),
            data.frame(series = paste(input$fillSeries, collapse = ","),
                       year = NA, value = NA, action = "fill",
                       fixLast = NA, fill = as.character(fill),
                       stringsAsFactors = FALSE))
  })

  # ── Data checks: dplR's rwl.check() ───────────────────────────────────────
  # Runs on the working copy, so a fixed dating error drops off the list
  # after the edit. Passing the uploaded file also runs the checks that need
  # the file itself (line endings, tabs, header span).
  rwlCheck <- reactive({
    req(rwlRV$dated)
    res <- tryCatch(suppressWarnings(suppressMessages(
      rwl.check(rwlRV$dated, file = datedPath()))), error = function(e) e)
    if (inherits(res, "error")) {
      validate(need(FALSE, paste("rwl.check() stopped:", conditionMessage(res))))
    }
    f <- as.data.frame(res)
    f[order(match(f$severity, c("error", "warning", "note")), f$series), ]
  }) |> bindCache(rwlRV$dated, datedPath(), cache = "session")
  
  # rwl.report() for the summary cards and the Overview report, computed
  # once per data set (see rwlSummaryCard())
  datedReport <- reactive({
    req(rwlRV$dated)
    suppressWarnings(rwl.report(rwlRV$dated))
  }) |> bindCache(rwlRV$dated, cache = "session")
  
  undatedReport <- reactive({
    req(getRWLUndated())
    suppressWarnings(rwl.report(getRWLUndated()))
  }) |> bindCache(getRWLUndated(), cache = "session")
  
  # ── Data checks panel ──────────────────────────────────────────────────────
  # Everything about the data that may need the user's attention, in one
  # place and in order of importance, each with the next step as a button:
  #   * cards for errors and warnings: rwl.check() findings with plain
  #     titles (checkTitle()), interior gaps with the fill controls, and the
  #     segment-length checks from rwlQA()
  #   * after edits, what they resolved and anything new they raised
  #   * notes, one collapsible line per kind
  checkCatalogue <- rwl.check.catalogue()

  # rwl.check() on the file as read, to compare with the edited data
  rwlCheckOriginal <- reactive({
    req(rwlRV$datedVault)
    tryCatch(as.data.frame(suppressWarnings(suppressMessages(
      rwl.check(rwlRV$datedVault, file = datedPath())))), error = function(e) NULL)
  }) |> bindCache(rwlRV$datedVault, datedPath(), cache = "session")

  # Card buttons report through one input, input$checkAction, as
  # {action, series}, rather than needing an observer per button.
  checkButton <- function(label, action, series, icon, cls = "btn-outline-primary") {
    js <- sprintf(paste0("Shiny.setInputValue('checkAction', {action: '%s', ",
                         "series: %s, nonce: Math.random()}, {priority: 'event'})"),
                  action, jsonlite::toJSON(as.character(series)))
    tags$button(type = "button", class = paste("btn btn-sm text-nowrap", cls),
                onclick = js, bs_icon(icon), " ", label)
  }

  findingCard <- function(level, title, body = NULL, buttons = NULL, id = NULL) {
    col  <- c(error = "danger", warning = "warning", info = "info")[[level]]
    icon <- c(error = "x-circle", warning = "exclamation-triangle",
              info = "info-circle")[[level]]
    div(class = paste0("d-flex gap-2 align-items-start p-2 mb-2 rounded border ",
                       "border-start border-4 border-", col),
        span(class = paste0("text-", col), bs_icon(icon)),
        div(class = "flex-grow-1",
            div(class = "fw-bold", title),
            if (!is.null(body)) div(class = "small", body),
            if (!is.null(id)) div(class = "small text-muted", tags$code(id))),
        if (length(buttons)) div(class = "d-flex flex-column gap-1", buttons))
  }

  gapCard <- function(gaps) {
    gapSeries <- unique(gaps$series)
    byS <- vapply(gapSeries, function(s) {
      paste0(s, ": ", paste(formatGaps(gaps[gaps$series == s, ]), collapse = ", "))
    }, character(1))
    findingCard(
      "warning",
      paste(length(gapSeries), if (length(gapSeries) == 1) "series has" else "series have",
            "years with no measurement"),
      tagList(
        tags$ul(class = "mb-1", lapply(byS, tags$li)),
        p(class = "mb-1",
          "dplR reads these as missing, not as zero-width rings. Correlations",
          "skip them, the skeleton plot can't be drawn across them, and the",
          "spline filter (nyrs) stops on them. If they are absent rings, fill",
          "them with zero. Linear and mean fills invent values: use them only",
          "if you know why the measurements are missing. A fill is logged as",
          "an edit and can be reverted from the Edit panel."),
        layout_columns(
          col_widths = c(5, 4, 3),
          selectInput("fillSeries", "Series to fill", choices = gapSeries,
                      selected = gapSeries, multiple = TRUE),
          selectInput("fillMethod", "Fill with",
                      choices = c("Zero (absent rings)" = "0",
                                  "Linear interpolation" = "Linear",
                                  "Series mean" = "Mean")),
          # Same structure as the inputs beside it (an empty label above
          # the control) so the button lines up with the two dropdowns
          div(class = "form-group shiny-input-container w-100",
              tags$label(class = "control-label", HTML("&nbsp;")),
              actionButton("fillGapsButton", "Fill gaps",
                           icon  = bs_icon("paint-bucket"),
                           class = "btn-primary w-100"))
        )
      ),
      id = "RWL_INTERNAL_NA")
  }

  output$checkPanel <- renderUI({
    f     <- rwlCheck()
    gaps  <- datedGaps()
    qa    <- if (!is.null(input$seg.length)) rwlQA() else list(tier = 0)
    nms   <- colnames(rwlRV$dated)
    rowsOf <- function(x) lapply(seq_len(nrow(x)), function(i) x[i, , drop = FALSE])

    # Only Examine here: whether to leave a series out of the master is a
    # crossdating judgement, made on the Correlations or Series panel
    findingButtons <- function(r) {
      s <- r$series
      if (is.na(s) || !s %in% nms) return(NULL)
      list(checkButton("Examine", "examine", s, "search"))
    }
    # One card per series (its findings together, the most serious first)
    # and one per finding that isn't about a series. A card takes the level
    # of its most serious finding.
    serious <- f[f$severity %in% c("error", "warning") & f$check != "RWL_INTERNAL_NA", ]
    groups  <- split(serious, ifelse(is.na(serious$series),
                                     paste0("\r", seq_len(nrow(serious))), serious$series))
    groups  <- groups[order(vapply(groups, function(g) {
      min(match(g$severity, c("error", "warning")))
    }, numeric(1)))]
    rwlCards <- function(sev) {
      lapply(Filter(function(g) g$severity[1] == sev, groups), function(g) {
        first <- g[1, , drop = FALSE]
        also  <- lapply(rowsOf(g[-1, , drop = FALSE]), function(r) {
          div(class = "mt-1", tags$strong("Also: "),
              paste0(checkTitle(r, checkCatalogue), ". ", checkBody(r)))
        })
        findingCard(sev, checkTitle(first, checkCatalogue),
                    tagList(checkBody(first), also),
                    findingButtons(first), paste(g$check, collapse = " \u00b7 "))
      })
    }

    # Segment-level flags from the Correlations panel. rwl.check() tests each
    # series as a whole, so a series misdated in only part of its length
    # (the example's ABC104) is not among its findings. On a large file the
    # flags are not computed here, so the Overview does not wait for
    # corr.rwl.seg(); the note under the cards points to the panel instead.
    bigFile <- ncol(rwlRV$dated) > 100
    segs    <- if (!bigFile) tryCatch(crsFlags(), error = function(e) NULL)
    segCard <- if (!is.null(segs) && nrow(segs) > 0) {
      nB <- sum(segs$flag == "B" & !segs$weak)
      list(findingCard(
        if (nB > 0) "warning" else "info",
        paste0(nrow(segs), if (nrow(segs) == 1) " segment is" else " segments are",
               " flagged on the Correlations panel"),
        tagList(paste0(paste(flagsBySeries(segs), collapse = "; "), "."),
                div(class = "text-muted",
                    "Each series is tested there segment by segment, which can show a",
                    " series that is misdated in only part of its length.")),
        list(checkButton("Open Correlations", "correlations", "", "grid-3x3"))))
    }

    cards <- c(
      if (qa$tier == 1) list(findingCard(
        "error", "Too little data to crossdate statistically", qa$message)),
      rwlCards("error"),
      if (nrow(gaps) > 0) list(gapCard(gaps)),
      if (qa$tier == 2) list(findingCard("warning", qa$title, qa$message)),
      rwlCards("warning"),
      segCard
    )

    # What the edits changed, judged against the file as read
    orig <- rwlCheckOriginal()
    changes <- if (nrow(rwlRV$editDF) > 0 && !is.null(orig)) {
      o   <- orig[orig$severity %in% c("error", "warning"), ]
      cur <- f[f$severity %in% c("error", "warning"), ]
      fixed  <- o[!checkKeys(o) %in% checkKeys(cur), ]
      raised <- cur[!checkKeys(cur) %in% checkKeys(o), ]
      titles <- function(x) paste(vapply(rowsOf(x), checkTitle, "", checkCatalogue),
                                  collapse = "; ")
      tagList(
        if (nrow(fixed) > 0) div(class = "alert alert-success py-2 mb-2",
                                 bs_icon("check-circle"), " ",
                                 tags$strong("Resolved by your edits: "),
                                 paste(resolvedText(fixed, checkCatalogue), collapse = " ")),
        if (nrow(raised) > 0) div(class = "alert alert-warning py-2 mb-2",
                                  bs_icon("exclamation-triangle"), " ",
                                  tags$strong("New since your edits: "),
                                  paste0(titles(raised), ". See below.")))
    }

    # Notes: one collapsible line per kind
    notes <- f[f$severity == "note", ]
    noteKinds <- split(notes, notes$check)
    notesUI <- if (length(noteKinds) > 0 || qa$tier == 3 || bigFile) {
      div(class = "mt-2",
          div(class = "small text-muted fw-bold mb-1", "For information"),
          if (qa$tier == 3) tags$details(
            tags$summary(class = "small", "Series are short relative to the segment length"),
            div(class = "small ms-3 mb-1", qa$message)),
          if (bigFile) div(class = "small",
                           "Segment-by-segment flags are on the Correlations panel."),
          lapply(noteKinds, function(x) {
            k <- x[1, , drop = FALSE]; k$series <- NA
            ser <- unique(x$series[!is.na(x$series)])
            tags$details(
              tags$summary(class = "small", checkTitle(k, checkCatalogue),
                           span(class = "text-muted",
                                paste0(" — ", nrow(x),
                                       if (length(ser)) paste0(" in ", length(ser), " series")))),
              tags$ul(class = "small mb-1",
                      lapply(head(x$message, 30), tags$li),
                      if (nrow(x) > 30) tags$li(class = "text-muted",
                                                paste("and", nrow(x) - 30, "more"))))
          }))
    }

    lead <- if (length(cards) == 0 && nrow(notes) == 0) {
      span(bs_icon("check-circle"), " Nothing to flag: the checks found no problems.")
    } else if (length(cards) == 0) {
      span(bs_icon("check-circle"), " Nothing needs your attention.")
    } else {
      span(length(cards), if (length(cards) == 1) "thing" else "things",
           "to look at, most important first.")
    }

    card(
      fill = FALSE,
      card_header(
        "Data Checks",
        tooltip(bs_icon("question-circle"),
                paste("Things worth checking, found by dplR's rwl.check() and",
                      "by xDateR's own checks against the analysis settings:",
                      "gaps, series that seem misdated or don't fit the",
                      "collection, duplicate or empty series, unit errors, file",
                      "problems. Red is very likely a mistake in the data;",
                      "orange is worth a look; notes are for information.",
                      "The checks rerun after every edit."))
      ),
      p(class = "mb-2", lead),
      changes,
      cards,
      notesUI
    )
  })

  # Card button: examine a series on the Series panel
  observeEvent(input$checkAction, {
    a <- input$checkAction
    s <- unlist(a$series)
    req(rwlRV$dated)
    if (identical(a$action, "correlations")) {
      nav_select("navbar", "AllSeriesTab", session = session)
    } else if (identical(a$action, "examine")) {
      req(length(s) > 0, all(s %in% colnames(rwlRV$dated)))
      updateSelectInput(session, "series", selected = s[1])
      nav_select("navbar", "IndividualSeriesTab", session = session)
    }
  })
  
  output$rwlSummaryHeader <- renderUI({
    req(rwlRV$dated)
    rwlSummaryCard(datedReport())
  })

  output$rwlPlot <- renderPlot({
    req(rwlRV$dated, input$rwlPlotType)
    plot.rwl(rwlRV$dated, plot.type = input$rwlPlotType)
  }, height = 400)

  output$rwlSummary <- renderTable({
    req(rwlRV$dated)
    summary(rwlRV$dated)
  })

  output$rwlSummaryReport <- safeDownload(
    filename = function() reportName(datedName(), "overview"),
    content  = function(file) {
      tempReport <- file.path(tempdir(), "report_rwl_describe.rmd")
      file.copy("report_rwl_describe.rmd", tempReport, overwrite = TRUE)
      params <- list(fileName    = datedName(),
                     rwlObject   = rwlRV$dated,
                     rwlPlotType = input$rwlPlotType,
                     editDF      = rwlRV$editDF,
                     editLog     = rwlRV$editLog,
                     editCode    = readLines("editRing.R"),
                     checks      = rwlCheck(),
                     rwlReport   = datedReport())
      params$helpers <- normalizePath("appHelpers.R")
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir  = new.env(parent = globalenv()))
    }
  )


  # ── qaAlertCorr: QA banner for the Correlations panel ────────────────────
  # Tier 1 is not shown here — getCRS() is blocked and shows the message.
  output$qaAlertCorr <- renderUI({
    req(rwlRV$dated, input$seg.length)
    qa <- rwlQA()
    if (qa$tier == 2) {
      div(class = "alert alert-warning",
          bs_icon("exclamation-triangle"), " ",
          tags$strong(paste0(qa$title, ". ")),
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
  output$crsPlotUI <- renderUI({
    nSeries    <- ncol(masterRWL())
    plotHeight <- paste0(max(300, nSeries * 22 + 200), "px")
    plotlyOutput("crsFancyPlot", height = plotHeight)
  })

  # Interactive plotly tile plot — see plotlyCRSFunc.R for implementation.
  output$crsFancyPlot <- renderPlotly({
    crsPlotly(getCRS())
  })

  # Overall mean correlation per series across all bins
  output$crsOverall <- renderDT({
    crsObject  <- getCRS()
    # Shaded where the series as a whole is not significantly correlated
    # with the master (p >= pcrit). (This table used to bold every
    # correlation larger than pcrit, i.e. it compared r with a p-value.)
    # p is shown as text (fmtP()); the number rides along in a hidden
    # column, which the shading and the sorting of the p column use.
    res <- data.frame(Series      = rownames(crsObject$overall),
                      Correlation = round(crsObject$overall[, 1], 3),
                      p           = fmtP(crsObject$overall[, 2]),
                      pnum        = crsObject$overall[, 2],
                      stringsAsFactors = FALSE)
    datatable(res, rownames = FALSE,
              options = list(paging = FALSE, scrollY = "320px",
                             searching = FALSE, info = FALSE,
                             columnDefs = list(
                               list(visible = FALSE, targets = 3),
                               list(orderData = 3, className = "dt-right", targets = 2)))) %>%
      formatStyle("pnum", target = "row",
                  backgroundColor = styleInterval(crsObject$pcrit - 1e-12,
                                                  c("transparent", "#fde4e4")))
  })

  # Average correlation within each time bin across all series
  output$crsAvgCorrBin <- renderDT({
    crsObject <- getCRS()
    binNames  <- paste(crsObject$bins[, 1], "-", crsObject$bins[, 2], sep = "")
    res <- data.frame(Bin         = binNames,
                      Correlation = round(crsObject$avg.seg.rho, 3))
    res <- res[!is.na(res$Correlation), ]   # bins no series fills completely
    datatable(res, rownames = FALSE,
              options = list(paging = FALSE, scrollY = "320px",
                             searching = FALSE, info = FALSE))
  })

  # COFECHA-style A/B flags, one row per segment (see crsFlagged())
  crsFlags <- reactive(crsFlagged(getCRS()))
  
  # ── Jump from a flag to the series and segment ──────────────────────────
  # A click on a segment of the tile plot, or on a row of Flagged Segments,
  # opens that series on the Series panel and puts the Edit panel's window
  # on that segment.
  jumpTo <- function(series, year) {
    req(rwlRV$dated, series %in% colnames(rwlRV$dated))
    setWindowCenter(year)
    updateSelectInput(session, "series", selected = series)
    nav_select("navbar", "IndividualSeriesTab", session = session)
  }
  observeEvent(suppressWarnings(event_data("plotly_click", source = "crs")), {
    ev <- suppressWarnings(event_data("plotly_click", source = "crs"))
    req(ev$customdata)
    jumpTo(as.character(ev$customdata[1]), ev$x[1])
  })
  observeEvent(input$crsFlags_rows_selected, {
    f <- crsFlags()[input$crsFlags_rows_selected, ]
    jumpTo(f$series, (f$from + f$to) / 2)
  })
  
  # One phrase per flagged series: the lags of its B segments and how many
  # of its segments are weak, e.g. "ABC104 (better at lag +1)"
  flagsBySeries <- function(f) {
    vapply(unique(f$series), function(s) {
      fs   <- f[f$series == s, ]
      lags <- sort(unique(fs$best.lag[fs$flag == "B" & !fs$weak]))
      parts <- c(
        if (length(lags)) paste0("better at lag ",
                                 paste(sprintf("%+d", as.integer(lags)), collapse = "/")),
        if (any(fs$flag == "A" | fs$weak)) paste0(sum(fs$flag == "A" | fs$weak), " weak")
      )
      paste0(s, " (", paste(parts, collapse = ", "), ")")
    }, character(1))
  }
  
  output$crsFlags <- renderDT({
    f   <- crsFlags()
    # sprintf(), not paste0(): with no flags, paste0() still returns one
    # string ("-"), and data.frame() then fails on mismatched lengths
    res <- data.frame(Series  = f$series,
                      Segment = sprintf("%g-%g", f$from, f$to),
                      Flag    = ifelse(f$weak, "B (weak)", f$flag),
                      r       = round(f$r.dated, 2),
                      Lag     = ifelse(f$flag == "B", sprintf("%+d", as.integer(f$best.lag)), ""),
                      Gain    = round(f$gain, 2),
                      stringsAsFactors = FALSE)
    datatable(res, rownames = FALSE, selection = "single",
              caption = if (nrow(res) == 0) "No flagged segments" else
                "B: better at the lag shown (\u2212 = missing ring?, + = false ring?)",
              options = list(pageLength   = 10,
                             searching    = FALSE,
                             lengthChange = FALSE)) %>%
      formatStyle("Flag", target = "row",
                  backgroundColor = styleEqual(c("B", "B (weak)", "A"),
                                               c("#ead9f5", "#f4eef8", "#fde4e4")))
  })

  # Full correlation matrix: series x bin
  output$crsCorrBin <- renderDT({
    crsObject <- getCRS()
    binNames  <- paste(crsObject$bins[, 1], "-", crsObject$bins[, 2], sep = "")
    rho       <- round(crsObject$spearman.rho, 3)
    # Cells shaded like the tile plot: red where the segment is under the
    # critical value but best where dated (A), purple where it fits better
    # at another lag (B). The flags ride along in hidden columns.
    flag <- ifelse(crsObject$best.lag != 0, "B",
                   ifelse(crsObject$p.val >= crsObject$pcrit, "A", ""))
    flag[is.na(flag)] <- ""
    nb  <- ncol(rho)
    res <- data.frame(Series = rownames(rho), rho, flag, check.names = FALSE,
                      stringsAsFactors = FALSE)
    colnames(res) <- c("Series", binNames, paste0("flag", seq_len(nb)))
    datatable(res, rownames = FALSE,
              options = list(pageLength   = min(30, nrow(res)),
                             searching    = TRUE,
                             lengthChange = FALSE,
                             scrollX      = TRUE,
                             columnDefs   = list(list(visible = FALSE,
                                                      targets = nb + seq_len(nb))))) %>%
      formatStyle(columns = binNames, valueColumns = paste0("flag", seq_len(nb)),
                  backgroundColor = styleEqual(c("A", "B"), c("#fde4e4", "#ead9f5")))
  })

  # Report: captures all parameters for reproducibility
  output$crsReport <- safeDownload(
    filename = function() reportName(datedName(), "correlations"),
    content  = function(file) {
      tempReport <- file.path(tempdir(), "report_rwl_corr.rmd")
      file.copy("report_rwl_corr.rmd", tempReport, overwrite = TRUE)
      params <- list(fileName  = datedName(),
                     crsObject = getCRS(),
                     flagged   = crsFlags(),
                     xdParams  = xdParams(),
                     excluded  = rwlRV$excluded,
                     editDF    = rwlRV$editDF,
                     editCode  = readLines("editRing.R"))
      params$helpers <- normalizePath("appHelpers.R")
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir  = new.env(parent = globalenv()))
    }
  )


  # ── COFECHA-style report: dplR's xdate.report() ─────────────────────────
  # Uses the current parameters and master. xdate.report() takes a spline
  # (nyrs) or no filter, not a Hanning filter, so the download is disabled
  # and the reason shown when Hanning is chosen.
  cofechaOK <- reactive(!identical(input$lowFreq, "hanning"))
  
  # Hidden, not disabled: a download button is a link, and browsers ignore
  # "disabled" on links, so a disabled one could still be clicked.
  observe(shinyjs::toggle("divCofechaDownload", condition = cofechaOK()))
  
  output$cofechaNote <- renderUI({
    if (cofechaOK()) {
      helpText("Uses the current Analysis Parameters",
               if (length(rwlRV$excluded)) "and master filter", ".",
               "COFECHA's own settings are one click away in the sidebar.")
    } else {
      div(class = "text-danger small",
          "The COFECHA-style report filters with a spline (nyrs) or not at",
          "all; it has no Hanning filter. Choose None or Spline in the",
          "Analysis Parameters to download it.")
    }
  })
  
  output$cofechaReport <- safeDownload(
    filename = function() {
      base <- tools::file_path_sans_ext(datedName())
      ext  <- c(html = "html", text = "txt", markdown = "md")[[input$cofechaType]]
      paste0(base, "-cofecha-", Sys.Date(), ".", ext)
    },
    content = function(file) {
      p     <- xdParams()
      title <- datedName()
      if (length(rwlRV$excluded)) {
        title <- paste0(title, " (left out of the master: ",
                        paste(rwlRV$excluded, collapse = ", "), ")")
      }
      xr <- xdate.report(masterRWL(),
                         seg.length = p$seg.length, bin.floor = p$bin.floor,
                         nyrs = p$nyrs, prewhiten = p$prewhiten,
                         ar.order.max = p$ar.order.max, pcrit = p$pcrit,
                         lag.max = p$lag.max, method = p$method,
                         biweight = p$biweight, check = TRUE, title = title)
      write.xdate.report(xr, fname = file, type = input$cofechaType)
    }
  )
  
  
  # ════════════════════════════════════════════════════════════════════════════
  # SERIES
  # ════════════════════════════════════════════════════════════════════════════

  # Alert banner listing any flagged series from the Correlations panel,
  # plus a note when the selected series is not in the master.
  output$flaggedSeriesUI <- renderUI({
    notInMaster <- if (seriesOK() && input$series %in% rwlRV$excluded) {
      div(class = "alert alert-info",
          bs_icon("info-circle"), " ",
          "Series ", tags$strong(input$series), " is left out of the master",
          " (filter on the Correlations panel). It is being tested against the",
          " master built from the other series.")
    }
    f <- tryCatch(crsFlags(), error = function(e) NULL)
    flagged <- if (is.null(f) || nrow(f) == 0) NULL else {
      div(class = "alert alert-warning",
          bs_icon("flag"), " ",
          tags$strong("Flagged on the Correlations panel: "),
          paste0(paste(flagsBySeries(f), collapse = "; "), "."))
    }
    tagList(notInMaster, flagged)
  })

  # Segment correlation plot: corr.series.seg() for the selected series
  output$cssPlot <- renderPlot({
    req(seriesOK(), input$seg.length)
    si <- seriesInputs(input$series)
    p  <- xdParams()
    tryDplR(do.call(corr.series.seg, c(
      si,
      p[c("seg.length", "bin.floor", normArgs, "pcrit", "method")],
      list(make.plot = TRUE)
    )))
  }, height = 400)

  # Cross-correlations by segment for the selected series: ccf.series.rwl()
  # on the WHOLE series and master (cached), so the segments are the same as
  # in the segment plot above and no filter is fitted to a fragment.
  # "Adjust plotted years" only chooses which segments plotCCF() draws, so
  # moving it redraws without recomputing.
  seriesCCF <- reactive({
    req(seriesOK(), input$seg.length, input$lagCCF)
    p <- xdParams()
    tryDplR(suppressMessages(do.call(ccf.series.rwl, c(
      seriesInputs(input$series),
      p[c("seg.length", "bin.floor", normArgs, "pcrit")],
      list(lag.max = input$lagCCF, make.plot = FALSE)
    ))))
  }) |> bindCache(rwlRV$dated, rwlRV$excluded, input$series, input$lagCCF,
                  xdParams()[c("seg.length", "bin.floor", normArgs, "pcrit")],
                  cache = "session")

  output$ccfPlot <- renderPlot({
    req(input$rangeCCF)
    res <- seriesCCF()
    win <- input$rangeCCF
    validate(need(any(res$bins[, 1] >= win[1] & res$bins[, 2] <= win[2] &
                        !is.na(res$ccf[1, ])),
                  paste0("No ", input$seg.length, "-year segment of ", input$series,
                         " lies wholly inside ", win[1], "\u2013", win[2],
                         ". Widen \"Adjust plotted years\".")))
    plotCCF(res, from = win[1], to = win[2], pcrit = xdParams()$pcrit)
  }, height = function() {
    # a row of panels per four segments drawn, so many segments stay readable
    res <- tryCatch(seriesCCF(), error = function(e) NULL)
    win <- input$rangeCCF
    if (is.null(res) || is.null(win)) return(400)
    n <- sum(res$bins[, 1] >= win[1] & res$bins[, 2] <= win[2] & !is.na(res$ccf[1, ]))
    max(300, 160 * ceiling(n / 4) + 90)
  })
  
  # ── Dating notes, kept per series ────────────────────────────────────────
  # One text box, but each series (of each file) has its own notes: they are
  # saved as they are typed and brought back when the series is selected.
  notes <- reactiveValues(dated = list(), undated = list())
  noteKey <- function(file, series) paste(file, series, sep = "\r")
  observeEvent(input$datingNotes, {
    req(seriesOK())
    notes$dated[[noteKey(filesRV$active, input$series)]] <- input$datingNotes
  }, ignoreInit = TRUE)
  observeEvent(list(input$series, filesRV$active), {
    req(seriesOK())
    saved <- notes$dated[[noteKey(filesRV$active, input$series)]]
    updateTextAreaInput(session, "datingNotes", value = if (is.null(saved)) "" else saved)
  })
  output$notesTitle <- renderText({
    if (seriesOK()) paste("Dating Notes for", input$series) else "Dating Notes"
  })
  observeEvent(input$undatingNotes, {
    req(undatedName(), input$series2)
    notes$undated[[noteKey(undatedName(), input$series2)]] <- input$undatingNotes
  }, ignoreInit = TRUE)
  observeEvent(list(input$series2, undatedName()), {
    req(undatedName(), input$series2)
    saved <- notes$undated[[noteKey(undatedName(), input$series2)]]
    updateTextAreaInput(session, "undatingNotes", value = if (is.null(saved)) "" else saved)
  })
  output$undatedNotesTitle <- renderText({
    if (!is.null(input$series2) && nzchar(input$series2)) {
      paste("Dating Notes for", input$series2)
    } else "Dating Notes"
  })

  # Report: includes dating notes for the analyst's reasoning
  output$cssReport <- safeDownload(
    filename = function() reportName(datedName(), paste0("series-", input$series)),
    content  = function(file) {
      tempReport <- file.path(tempdir(), "report_series.rmd")
      file.copy("report_series.rmd", tempReport, overwrite = TRUE)
      params <- list(fileName    = datedName(),
                     series      = input$series,
                     seriesFull  = seriesInputs(input$series),
                     plotCode    = readLines("plotCCF.R"),
                     xdParams    = xdParams(),
                     excluded    = rwlRV$excluded,
                     lagCCF      = input$lagCCF,
                     winCCF      = input$rangeCCF,
                     datingNotes = input$datingNotes,
                     editDF      = rwlRV$editDF,
                     editCode    = readLines("editRing.R"))
      params$helpers <- normalizePath("appHelpers.R")
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir  = new.env(parent = globalenv()))
    }
  )


  # ════════════════════════════════════════════════════════════════════════════
  # EDIT
  # ════════════════════════════════════════════════════════════════════════════

  # Skeleton / CCF plot: xskel.ccf.plot() centred on the current window.
  # xskel.ccf.plot() cannot be drawn across a gap in the selected series, so
  # say which years are missing instead of showing its internal error.
  output$xskelPlot <- renderPlot({
    req(seriesOK(), input$winCenter, input$winWidth)
    wStart <- input$winCenter - (input$winWidth / 2)
    win    <- c(wStart, wStart + input$winWidth - 1)
    gaps   <- rwlGaps(rwlRV$dated[, input$series, drop = FALSE])
    hit    <- gaps[gaps$last >= win[1] & gaps$first <= win[2], , drop = FALSE]
    validate(need(nrow(hit) == 0, paste0(
      "Series ", input$series, " has no measurements for ",
      paste(formatGaps(hit), collapse = ", "),
      ". The skeleton plot can't be drawn across a gap: move the window ",
      "past it, or fill the gap from the Overview panel.")))
    si <- seriesInputs(input$series)
    p  <- xdParams()
    res <- tryCatch(do.call(xskel.ccf.plot, c(
      si,
      list(win.start = wStart, win.width = input$winWidth),
      p[normArgs]
    )), error = function(e) e)
    if (inherits(res, "error")) {
      # Near the start of a series the usual cause is prewhitening, which
      # removes as many years from the start of each series as the AR
      # model's order. Say so rather than pass on dplR's internal error.
      sp <- seriesSpan(rwlRV$dated, input$series)
      nearStart <- win[1] - sp[1] < 30 && isTRUE(p$prewhiten)
      validate(need(FALSE, if (nearStart) {
        paste0("The skeleton plot can't be drawn for ", win[1], "\u2013", win[2],
               ". Prewhitening removes the first few years of each series (as many ",
               "as the order of its AR model), and this window starts only ",
               win[1] - sp[1], " years into ", input$series, ". Move the window ",
               "later, or set a lower Max AR order in the Analysis Parameters.")
      } else {
        paste0("The skeleton plot can't be drawn for ", win[1], "\u2013", win[2],
               ". Try moving the window or changing its width. (dplR stopped with: ",
               conditionMessage(res), ")")
      }))
    }
    # The note under the plot. dplR up to 1.8.0 words it for R users ("NB:
    # With series.x = FALSE (default), negative lags indicate missing rings
    # in series"); later versions say it plainly. xskel.ccf.plot() draws it
    # inside the plot, so paint the plain wording over it. This can go once
    # the app requires a dplR that has the plain wording.
    grid::upViewport(0)
    grid::grid.rect(x = 0.5, y = grid::unit(0.015, "npc"), width = 1,
                    height = grid::unit(0.032, "npc"),
                    gp = grid::gpar(col = NA, fill = "white"))
    grid::grid.text("Negative lags suggest a missing ring in the series",
                    x = 0.5, y = grid::unit(0.015, "npc"), just = "center")
    invisible(res)
  }, height = 400)

  output$series2edit <- renderText({
    req(seriesOK())
    paste("Series", input$series, "selected")
  })

  # The selected series from its first to its last measurement. Interior
  # gaps are kept as rows (Value NA) so the years stay right: dropping them
  # and renumbering would misdate every ring after the gap.
  seriesTable <- reactive({
    req(seriesOK())
    dat  <- rwlRV$dated
    yrs  <- as.numeric(rownames(dat))
    x    <- dat[[input$series]]
    idx  <- which(!is.na(x))
    span <- seq(idx[1], idx[length(idx)])
    data.frame(Year = yrs[span], Value = x[span])
  })

  # Scrollable measurements table — automatically scrolls to window center.
  # Click a row to select it, then use the edit controls to modify it.
  output$table1 <- renderDataTable({
    req(input$winCenter)
    tab       <- seriesTable()
    shown     <- data.frame(Year  = tab$Year,
                            Value = ifelse(is.na(tab$Value), "gap",
                                           format(tab$Value)))
    wStart    <- input$winCenter - 10
    nRows     <- 21 * 33.33
    row2start <- max(1, which(tab$Year == wStart)[1], na.rm = TRUE)

    datatable(
      shown,
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
    if (!is.null(rwlRV$editLog)) rwlRV$editLog
  })

  # Show/hide the save card based on whether any edits exist.
  # Using shinyjs rather than renderUI because verbatimTextOutput("editLog")
  # inside a renderUI doesn't bind correctly in Shiny.
  observe({
    shinyjs::toggle("divSaveEdits", condition = nrow(rwlRV$editDF) > 0)
  })

  # Records one edit in the log and in editDF (replayed by the edit report).
  logEdit <- function(msg, row) {
    rwlRV$editLog <- c(rwlRV$editLog, msg)
    rwlRV$editDF  <- rbind(rwlRV$editDF, row)
    unsaved$files <- union(unsaved$files, filesRV$active)
  }

  # Applies an insert or delete through editRing() (editRing.R), so the app
  # and the edit report's R code make exactly the same change. A refused
  # edit (e.g. a negative ring width) is reported, not applied.
  applyRingEdit <- function(action, year, value, fixLast, msg) {
    res <- tryCatch(editRing(rwlRV$dated, input$series, action,
                             year = year, value = value, fix.last = fixLast),
                    error = function(e) e)
    if (inherits(res, "error")) {
      showNotification(paste("Edit not made:", conditionMessage(res)),
                       type = "error", duration = NULL)
      return(invisible(FALSE))
    }
    rwlRV$dated <- res
    logEdit(msg, data.frame(series = input$series, year = year,
                            value = if (is.null(value)) NA else value,
                            action = action, fixLast = fixLast, fill = NA,
                            stringsAsFactors = FALSE))
    invisible(TRUE)
  }

  selectedYear <- function() {
    sel <- input$table1_rows_selected
    if (is.null(sel)) {
      showNotification("Click a row in the Measurements table first.",
                       type = "warning")
      return(NULL)
    }
    seriesTable()$Year[sel]
  }

  # ── Delete ring ────────────────────────────────────────────────────────────
  # A gap row (a year with no measurement) can't be deleted: that would say
  # the year doesn't exist and redate every ring on one side of it, on the
  # strength of no measurement at all. Absent rings are filled with zero
  # instead (Overview panel), which keeps the years where they are.
  observeEvent(input$deleteRows, {
    yr <- selectedYear()
    if (is.null(yr)) return()
    tab <- seriesTable()
    if (is.na(tab$Value[tab$Year == yr])) {
      showNotification(
        paste0(yr, " has no measurement: it is part of a gap in ", input$series,
               ". Deleting it would move every ",
               if (isTRUE(input$fixLast)) "earlier ring one year later" else
                 "later ring one year earlier",
               ". If these years are absent rings, fill the gap with zero from ",
               "the Overview panel."),
        type = "warning", duration = 15)
      return()
    }
    applyRingEdit("delete", year = yr, value = NULL,
                  fixLast = isTRUE(input$fixLast),
                  msg = paste0("Series ", input$series, ". Year ", yr,
                               " deleted. Fix Last = ", isTRUE(input$fixLast)))
  })

  # ── Insert ring ────────────────────────────────────────────────────────────
  # "Insert above the selected row" puts the new ring before the selected
  # year. dplR's insert.ring() inserts AFTER its `year`, so pass year - 1.
  observeEvent(input$insertRows, {
    yr <- selectedYear()
    if (is.null(yr)) return()
    applyRingEdit("insert", year = yr - 1, value = input$insertValue,
                  fixLast = isTRUE(input$fixLast),
                  msg = paste0("Series ", input$series, ". Ring inserted before ",
                               yr, " with value ", input$insertValue,
                               ". Fix Last = ", isTRUE(input$fixLast)))
  })

  # ── Undo the last edit ─────────────────────────────────────────────────────
  # Replays the remaining edits on the file as loaded (replayEdits()), so no
  # copy of the data is kept per step.
  observeEvent(input$undoEdit, {
    n <- nrow(rwlRV$editDF)
    req(n > 0, rwlRV$datedVault)
    ed  <- rwlRV$editDF[-n, , drop = FALSE]
    res <- tryCatch(replayEdits(rwlRV$datedVault, ed), error = function(e) e)
    if (inherits(res, "error")) {
      showNotification(paste("Could not undo:", conditionMessage(res)),
                       type = "error", duration = NULL)
      return()
    }
    undone        <- rwlRV$editLog[n]
    rwlRV$dated   <- res
    rwlRV$editDF  <- ed
    rwlRV$editLog <- if (n > 1) rwlRV$editLog[-n] else NULL
    unsaved$files <- if (nrow(ed) > 0) union(unsaved$files, filesRV$active) else
      setdiff(unsaved$files, filesRV$active)
    showNotification(paste("Undone:", undone), type = "message")
  })
  
  # ── The selected series against the master, as loaded and now ──────────
  # corr.rwl.seg() for one series against the master built from the others
  # (the master as a data.frame, so the same leave-one-out master as the
  # Correlations panel), with the lag search.
  seriesSegs <- function(dat, series, p) {
    m <- dat[, setdiff(colnames(dat), c(rwlRV$excluded, series)), drop = FALSE]
    suppressMessages(do.call(corr.rwl.seg, c(
      list(rwl = dat[, series, drop = FALSE], master = m),
      p[c("seg.length", "bin.floor", normArgs, "pcrit", "method", "lag.max")],
      list(make.plot = FALSE))))
  }
  
  editEffect <- reactive({
    req(seriesOK())
    p   <- xdParams()
    s   <- input$series
    ed  <- rwlRV$editDF
    # edits to this series: its own ring edits, or a fill that included it
    edited <- nrow(ed) > 0 && any(vapply(strsplit(ed$series, ","),
                                         function(x) s %in% x, logical(1)))
    list(now    = tryDplR(seriesSegs(rwlRV$dated, s, p)),
         before = if (edited && s %in% colnames(rwlRV$datedVault)) {
           tryCatch(seriesSegs(rwlRV$datedVault, s, p), error = function(e) NULL)
         })
  }) |> bindCache(rwlRV$dated, rwlRV$datedVault, rwlRV$excluded, input$series,
                  rwlRV$editDF, xdParams(), cache = "session")
  
  output$editEffectUI <- renderUI({
    e   <- editEffect()
    s   <- input$series
    summ <- function(crs) {
      f <- crsFlagged(crs)
      list(r = crs$overall[1, 1], nB = sum(f$flag == "B" & !f$weak),
           nA = sum(f$flag == "A" | f$weak),
           runs = lagRuns(f, crsTested(crs), what = paste("series", s)))
    }
    now <- summ(e$now)
    was <- if (!is.null(e$before)) summ(e$before)
    cell <- function(x) tags$td(class = "text-end fw-bold", x)
    flagTxt <- function(x) if (x$nB + x$nA == 0) "none" else
      paste(c(if (x$nB) paste(x$nB, "better at another lag"),
              if (x$nA) paste(x$nA, "weak")), collapse = ", ")
    verdict <- if (now$nB + now$nA == 0) {
      div(class = "text-success", bs_icon("check-circle"),
          if (!is.null(was) && was$nB + was$nA > 0) {
            " Every segment now fits best where dated."
          } else " Every segment fits best where dated.")
    } else if (length(now$runs)) {
      div(class = "alert alert-warning py-2 mb-0", bs_icon("exclamation-triangle"),
          " ", lapply(now$runs, div))
    }
    card(
      fill = FALSE,
      card_header(
        paste("Series", s, "against the master"),
        tooltip(bs_icon("question-circle"),
                paste("The selected series tested segment by segment against the",
                      "master built from the other series, with the lag search:",
                      "the same test as the Correlations panel. After an edit it",
                      "compares the series as loaded with the series now, so you",
                      "can see whether the edit helped without leaving this panel."))
      ),
      tags$table(
        class = "table table-sm mb-2", style = "max-width: 34rem;",
        tags$thead(tags$tr(tags$th(""),
                           if (!is.null(was)) tags$th(class = "text-end", "As loaded"),
                           tags$th(class = "text-end", if (!is.null(was)) "Now" else ""))),
        tags$tbody(
          tags$tr(tags$td("Correlation with the master"),
                  if (!is.null(was)) cell(round(was$r, 2)), cell(round(now$r, 2))),
          tags$tr(tags$td("Flagged segments"),
                  if (!is.null(was)) cell(flagTxt(was)), cell(flagTxt(now))))),
      verdict
    )
  })

  # ── Revert all edits ───────────────────────────────────────────────────────
  observeEvent(input$revertSeries, {
    req(rwlRV$datedVault)
    rwlRV$dated   <- rwlRV$datedVault
    rwlRV$editLog <- NULL
    rwlRV$editDF  <- emptyEditDF()
    unsaved$files <- setdiff(unsaved$files, filesRV$active)
    showNotification("All edits reverted.", type = "message")
  })

  # Report: includes the edit log and reproducible R code
  output$editReport <- safeDownload(
    filename = function() reportName(datedName(), "edits"),
    content  = function(file) {
      tempReport <- file.path(tempdir(), "report_edits.rmd")
      file.copy("report_edits.rmd", tempReport, overwrite = TRUE)
      params <- list(fileName = datedName(),
                     editLog  = rwlRV$editLog,
                     editDF   = rwlRV$editDF,
                     editCode = paste(readLines("editRing.R"), collapse = "\n"))
      params$helpers <- normalizePath("appHelpers.R")
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir  = new.env(parent = globalenv()))
      guideRV$savedReport <- TRUE
      afterDownload()
    }
  )

  # Every series in the file, with edits, including any left out of the
  # master. Written at 0.001 mm when 0.01 would round values. The same
  # download is offered on the Edit panel and, whenever the file has edits,
  # in the sidebar (a gap fill on the Overview is an edit too).
  editedDownload <- function() {
    downloadHandler(
      filename = function() downloadName(datedName(), "edited"),
      content  = function(file) {
        write.tucson(rwl.df = rwlRV$dated, fname = file,
                     prec = tucsonPrec(rwlRV$dated))
        unsaved$files <- setdiff(unsaved$files, filesRV$active)
        guideRV$savedFile <- TRUE
        afterDownload()
      }
    )
  }
  output$downloadRWL     <- editedDownload()
  output$downloadRWLside <- editedDownload()


  # ════════════════════════════════════════════════════════════════════════════
  # FLOATER
  # ════════════════════════════════════════════════════════════════════════════

  # ── floaterUI: three-state panel ──────────────────────────────────────────
  output$floaterUI <- renderUI({

    # State 1: no dated file — full welcome screen
    if (is.null(getRWL())) {
      shinyjs::hide("divFloaterPlots")
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

    # State 2: dated file loaded but no undated file yet (or it could not be
    # read). Show a summary of the dated file plus a prompt or the error.
    if (is.null(getRWLUndated())) {
      shinyjs::hide("divFloaterPlots")
      undatedErr <- undatedRead()$error
      prompt <- if (!is.null(undatedErr)) {
        div(class = "alert alert-danger mb-0",
            bs_icon("x-circle"), " ",
            tags$strong("Undated file could not be read."),
            " dplR's ", tags$code("read.rwl()"), " returned:",
            tags$pre(class = "mt-2 mb-0", style = "font-size:0.85em;", undatedErr))
      } else {
        div(class = "alert alert-success mb-0",
            bs_icon("check-circle"), " ",
            tags$strong("Dated file loaded."),
            " Now load your undated .rwl file using the ",
            tags$strong("Undated Series"),
            " upload that has appeared in the sidebar.")
      }
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
          rwlSummaryCard(datedReport())
        ),
        div(style = "padding: 0.5rem 0;", prompt)
      ))
    }

    # State 3: both files loaded — compact summary card + reveal plots
    shinyjs::show("divFloaterPlots")
    card(
      fill = FALSE,
      card_header(
        "Undated Series Summary",
        tooltip(bs_icon("question-circle"),
                "Key statistics confirming your undated file was read correctly.")
      ),
      rwlSummaryCard(undatedReport())
    )
  })


  output$floaterControls <- renderUI({
    und <- getRWLUndated()
    req(und)
    s <- if (isTRUE(input$series2 %in% colnames(und))) input$series2 else colnames(und)[1]
    # Subtract 20 years from the raw series length as a conservative approximation
    # of the detrended series length — prewhitening and the Hanning filter both
    # trim years from the ends of the series. Using the raw length as the max
    # could allow values that exceed the detrended length and trigger an error
    # in xdate.floater().
    maxOverlap <- max(10, floor((sum(!is.na(und[, s])) - 20) / 10) * 10)
    div(
      selectInput(
        inputId  = "series2",
        label    = "Choose undated series",
        choices  = colnames(und),
        selected = s  # preserve current selection to avoid circular reset
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
        ticks   = FALSE
      )
    )
  })

  # CCF parameters — segment length, bin floor, pcrit. The filter,
  # prewhitening, biweight and method come from the sidebar.
  output$floaterCCFParams <- renderUI({
    req(getRWLUndated())
    layout_columns(
      col_widths = c(4, 4, 4),
      sliderInput(
        inputId = "seg.lengthUndated",
        label   = "Segment Length",
        min = 10, max = 100, value = 50, step = 10,
        ticks = FALSE
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

  # Floater-specific CCF settings, with defaults until the inputs render.
  floaterCCFArgs <- reactive({
    list(seg.length = if (!is.null(input$seg.lengthUndated)) input$seg.lengthUndated else 50,
         bin.floor  = if (!is.null(input$bin.floorUndated)) as.numeric(input$bin.floorUndated) else 10,
         pcrit      = if (!is.null(input$pcritUndated)) input$pcritUndated else 0.05)
  })

  # ── The position in use ────────────────────────────────────────────────────
  # xdate.floater() finds the best-fitting position, and that is the
  # default. The user can instead take another candidate, or enter the last
  # year by hand (a known bark date, say, or a second match the statistics
  # cannot separate from the first). Everything below reads placedFloater():
  # the plot, the segment test, the cross-correlations, what is saved and
  # the report. The choice is dropped when the search itself changes
  # (another series, another master, other settings).
  floaterChoice <- reactiveValues(last = NULL, how = "best")
  observeEvent(getFloater(), {
    floaterChoice$last <- NULL
    floaterChoice$how  <- "best"
  })
  floaterCands <- reactive(floaterCandidates(getFloater()$floaterCorStats))
  
  placedFloater <- reactive({
    fo     <- getFloater()
    fcs    <- fo$floaterCorStats
    best   <- fcs[which.max(fcs$r), ]
    last   <- if (is.null(floaterChoice$last)) best$last else floaterChoice$last
    series <- getRWLUndated()[, fo$series.name]
    i      <- match(last, fcs$last)
    c(list(series.name = fo$series.name, floaterCorStats = fcs),
      placeFloater(masterRWL(), series, fo$series.name, last),
      list(first = last - sum(!is.na(series)) + 1, last = last,
           # NA when the position was not among those searched: too little
           # overlap with the master (or none) to compute a correlation
           r      = if (is.na(i)) NA_real_ else fcs$r[i],
           isBest = last == best$last,
           how    = if (last == best$last) "best" else floaterChoice$how,
           bestFirst = best$first, bestLast = best$last, bestR = best$r))
  })
  
  observeEvent(input$floaterCandsTable_rows_selected, {
    floaterChoice$last <- floaterCands()$last[input$floaterCandsTable_rows_selected]
    floaterChoice$how  <- "candidate"
  })
  observeEvent(input$floaterUseYear, {
    yr <- input$floaterLast
    if (is.null(yr) || is.na(yr) || yr != round(yr)) {
      showNotification("Enter the last year as a whole number.", type = "warning")
      return()
    }
    floaterChoice$last <- yr
    floaterChoice$how  <- "manual"
  })
  observeEvent(input$floaterUseBest, {
    floaterChoice$last <- NULL
    floaterChoice$how  <- "best"
  })
  
  output$floaterText <- renderText({
    pf  <- placedFloater()
    fcs <- pf$floaterCorStats
    paste0("Series: ", pf$series.name, "<br/>",
           "Years searched: ", min(fcs$first), " to ", max(fcs$last), "<br/>",
           "Best correlation: <b>", round(pf$bestR, 2),
           "</b> (", pf$bestFirst, " \u2013 ", pf$bestLast, ")")
  })
  
  # Candidate positions, the hand-entry box, and what is in use
  output$floaterPositionUI <- renderUI({
    pf   <- placedFloater()
    cand <- floaterCands()
    status <- if (pf$isBest) {
      if (nrow(cand) > 1 && cand$r[1] - cand$r[2] < 0.1) {
        div(class = "alert alert-danger py-2 small",
            tags$b("Ambiguous: "), "the two best positions correlate almost equally",
            " well. Check both before saving: click the second one below.")
      }
    } else if (!is.na(pf$r)) {
      div(class = "alert alert-light border py-2 small",
          tags$b(paste0("Using ", pf$first, "\u2013", pf$last)),
          paste0("chosen by you (r = ", round(pf$r, 2), "). The best fit is ",
                 pf$bestFirst, "\u2013", pf$bestLast, " (r = ", round(pf$bestR, 2), "). "),
          actionLink("floaterUseBest", "Back to the best fit"))
    } else {
      div(class = "alert alert-warning py-2 small",
          tags$b(paste0("Using ", pf$first, "\u2013", pf$last)),
          paste0("entered by hand. This position cannot be checked: there the",
                 " series overlaps the master by fewer than ", input$minOverlapUndated,
                 " years, or not at all, so there is no correlation and no segment",
                 " test. It will be saved as entered. "),
          actionLink("floaterUseBest", "Back to the best fit"))
    }
    tagList(
      status,
      tags$label(class = "small fw-bold mt-2", "Candidate positions",
                 tooltip(bs_icon("question-circle"),
                         paste("The best-fitting positions that are more than two",
                               "years apart (closer ones are the same match off by a",
                               "ring). If the first is far ahead of the second, the",
                               "fit is clear-cut. Click a row to use that position:",
                               "the plot, the segment test and Save These Dates all",
                               "follow it."))),
      DTOutput("floaterCandsTable", fill = FALSE),
      div(class = "mt-2",
          numericInput("floaterLast", "Or set the last year (outer ring) by hand",
                       value = pf$last, step = 1),
          actionButton("floaterUseYear", "Use this year",
                       class = "xd-btn-quiet btn-sm w-100"))
    )
  })
  
  output$floaterCandsTable <- renderDT({
    cand <- floaterCands()
    datatable(data.frame(Years   = paste0(cand$first, "\u2013", cand$last),
                         r       = round(cand$r, 2),
                         Overlap = cand$n),
              rownames = FALSE,
              # the position in use is the highlighted row
              selection = list(mode = "single",
                               selected = which(cand$last == placedFloater()$last)),
              options = list(dom = "t", ordering = FALSE))
  })
  
  # ── The floater's segments at its best-fit dates ─────────────────────────
  # The placed floater is tested segment by segment against the master
  # (corr.rwl.seg() with the master as a data.frame, so no leave-one-out),
  # with the lag search: the same A/B flags as the Correlations panel. A
  # floater can fit best overall yet have a run of segments that fit better
  # a year off, e.g. a missing ring inside it.
  floaterSegs <- reactive({
    fo <- placedFloater()
    validate(need(!is.na(fo$r), "This position cannot be checked against the master."))
    a  <- floaterCCFArgs()
    p  <- xdParams()
    validate(need(p$lag.max < a$seg.length, paste0(
      "Lag search (", p$lag.max, " years) must be less than the segment ",
      "length here (", a$seg.length, " years).")))
    tryDplR(do.call(corr.rwl.seg, c(
      list(rwl = fo$rwlOut, master = masterRWL()),
      a[c("seg.length", "bin.floor", "pcrit")],
      p[c(normArgs, "method", "lag.max")],
      list(make.plot = FALSE))))
  }) |> bindCache(placedFloater()$rwlOut, placedFloater()$r, masterRWL(), floaterCCFArgs(),
                  xdParams()[c(normArgs, "method", "lag.max")], cache = "session")

  output$floaterSegsUI <- renderUI({
    crs  <- floaterSegs()
    f    <- crsFlagged(crs)
    runs <- lagRuns(f, crsTested(crs))
    nA   <- sum(f$flag == "A" | f$weak)
    lead <- if (length(runs) == 0 && nA == 0) {
      div(class = "text-success mb-2", bs_icon("check-circle"), " ",
          "Every segment fits best where dated.")
    } else {
      div(class = "mb-2",
          if (length(runs)) div(class = "alert alert-warning py-2 mb-2",
                                bs_icon("exclamation-triangle"), " ",
                                lapply(runs, div)),
          if (nA > 0) div(class = "small text-muted",
                          nA, if (nA == 1) " segment is" else " segments are",
                          " weak (under the critical value) but fit best where dated."))
    }
    tagList(lead, DTOutput("floaterSegsTable", fill = FALSE))
  })

  output$floaterSegsTable <- renderDT({
    crs  <- floaterSegs()
    ok   <- !is.na(crs$spearman.rho[1, ])
    f    <- crsFlagged(crs)
    seg  <- sprintf("%g-%g", crs$bins[ok, 1], crs$bins[ok, 2])
    flag <- f$flag[match(crs$bins[ok, 1], f$from)]
    weak <- f$weak[match(crs$bins[ok, 1], f$from)]
    lag  <- crs$best.lag[1, ok]
    res  <- data.frame(Segment = seg,
                       r       = round(crs$spearman.rho[1, ok], 2),
                       Flag    = ifelse(is.na(flag), "", ifelse(weak, "B (weak)", flag)),
                       Lag     = ifelse(lag == 0, "", sprintf("%+d", as.integer(lag))),
                       Gain    = ifelse(lag == 0, NA, round(crs$best.rho[1, ok] - crs$spearman.rho[1, ok], 2)),
                       stringsAsFactors = FALSE)
    # Every segment in one scrolling table, in date order, opened at the
    # first flagged segment so a problem late in the series isn't on page 3
    first <- which(res$Flag != "")[1]
    datatable(res, rownames = FALSE, extensions = "Scroller",
              options = list(paging = TRUE, scroller = TRUE, scrollY = 300,
                             deferRender = TRUE, searching = FALSE, info = FALSE,
                             initComplete = JS(sprintf(
                               "function() { this.api().scroller.toPosition(%d); }",
                               if (is.na(first)) 0L else max(0L, first - 2L))))) %>%
      formatStyle("Flag", target = "row",
                  backgroundColor = styleEqual(c("B", "B (weak)", "A"),
                                               c("#ead9f5", "#f4eef8", "#fde4e4")))
  })

  # Floater correlation surface plot
  output$floaterPlot <- renderPlot({
    plot.floater(placedFloater(), params = xdParams())
  })

  # Cross-correlation for the best-fit dated series against the master
  output$ccfPlotUndated <- renderPlot({
    # Computed by dplR, drawn by plotCCF() like the Series panel's plot:
    # segments in time order, and the note under it in plain words whatever
    # the dplR version (see xskelPlot).
    res <- tryDplR(floaterCCF())
    plotCCF(res, pcrit = floaterCCFArgs()$pcrit)
  }, height = function() {
    n <- tryCatch(sum(!is.na(floaterCCF()$ccf[1, ])), error = function(e) 4)
    max(300, 160 * ceiling(n / 4) + 90)
  })
  
  # segments with a cross-correlation, for the plot's height
  floaterCCF <- reactive({
    fo <- placedFloater()
    validate(need(!is.na(fo$r), "This position cannot be checked against the master."))
    suppressMessages(do.call(ccf.series.rwl, c(
      list(rwl = fo$rwlCombined, series = fo$series.name),
      floaterCCFArgs(), xdParams()[normArgs], list(make.plot = FALSE))))
  }) |> bindCache(placedFloater()$rwlCombined, placedFloater()$series.name, placedFloater()$r,
                  floaterCCFArgs(), xdParams()[normArgs], cache = "session")

  # Download is only possible once something has been saved
  observe({
    # Hidden, not disabled: a download button is a link, and browsers ignore
    # "disabled" on links. Clicked with nothing saved, it downloaded the
    # server's error page as "downloadUndatedRWL.html".
    saved <- !is.null(rwlRV$undated2dated)
    shinyjs::toggle("divUndatedDownload", condition = saved)
    shinyjs::toggle("divUndatedNone", condition = !saved)
  })

  # Save the best-fit dates for the current undated series
  observeEvent(input$saveDates, {
    pf   <- placedFloater()
    name <- pf$series.name
    # the log says how the position was arrived at
    how <- if (pf$isBest) "" else if (is.na(pf$r)) {
      paste0(" Position entered by hand; it could not be checked against the",
             " master. The best fit was ", pf$bestFirst, " to ", pf$bestLast,
             " (r = ", round(pf$bestR, 2), ").")
    } else {
      paste0(" Position chosen by the user (r = ", round(pf$r, 2), "); the best",
             " fit was ", pf$bestFirst, " to ", pf$bestLast, " (r = ",
             round(pf$bestR, 2), ").")
    }
    runs <- tryCatch(lagRuns(crsFlagged(floaterSegs()), crsTested(floaterSegs())),
                     error = function(e) character(0))
    # saving a series again replaces its earlier dates
    again <- !is.null(rwlRV$undated2dated) && name %in% names(rwlRV$undated2dated)
    kept  <- if (again) rwlRV$undated2dated[, setdiff(names(rwlRV$undated2dated), name), drop = FALSE]
             else rwlRV$undated2dated
    rwlRV$dateLog <- c(rwlRV$dateLog,
                       paste0("Series ", name, if (again) " saved again" else " saved",
                              " with dates ", pf$first, " to ", pf$last, ".", how,
                              if (length(runs)) paste0(" Note: ", paste(runs, collapse = " "))))
    rwlRV$undated2dated <- if (is.null(kept) || ncol(kept) == 0) {
      pf$rwlOut
    } else {
      combine.rwl(kept, pf$rwlOut)
    }
    unsaved$floater <- TRUE
  })

  # Revert all saved dates
  observeEvent(input$removeDates, {
    unsaved$floater     <- FALSE
    rwlRV$undated2dated <- NULL
    rwlRV$dateLog       <- "Dates removed for all undated series. Log reset."
  })

  output$dateLog <- renderPrint({
    if (!is.null(rwlRV$dateLog)) rwlRV$dateLog
  })

  # The saved series, optionally with the master series: the ones the
  # floaters were dated against, with any edits. Series left out of the
  # master are not appended.
  output$downloadUndatedRWL <- downloadHandler(
    filename = function() downloadName(undatedName(), "dated", fallback = "xDateR-undated"),
    content  = function(file) {
      tmp <- if (isTRUE(input$appendMaster)) {
        combine.rwl(rwlRV$undated2dated, masterRWL())
      } else {
        rwlRV$undated2dated
      }
      write.tucson(rwl.df = tmp, fname = file, prec = tucsonPrec(tmp))
      unsaved$floater <- FALSE
      guideRV$savedFloater <- TRUE
      afterDownload()
    }
  )

  # Report: floater analysis report
  output$undatedReport <- safeDownload(
    filename = function() reportName(datedName(), paste0("floater-", input$series2)),
    content  = function(file) {
      tempReport <- file.path(tempdir(), "report_undated_series.rmd")
      file.copy("report_undated_series.rmd", tempReport, overwrite = TRUE)
      params <- list(fileName1     = datedName(),
                     fileName2     = undatedName(),
                     floaterObject = placedFloater(),
                     xdParams      = xdParams(),
                     ccfArgs       = floaterCCFArgs(),
                     # NULL when the position in use cannot be checked
                     floaterSegs   = tryCatch(floaterSegs(), error = function(e) NULL),
                     minOverlap    = input$minOverlapUndated,
                     excluded      = rwlRV$excluded,
                     editDF        = rwlRV$editDF,
                     editCode      = readLines("editRing.R"),
                     datingNotes   = input$undatingNotes,
                     dateLog       = rwlRV$dateLog)
      params$helpers <- normalizePath("appHelpers.R")
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir  = new.env(parent = globalenv()))
    }
  )

})
