# Builds an interactive plotly correlation plot for a corr.rwl.seg object.
# Uses pure plotly (no ggplot/ggplotly) to avoid factor coercion errors.
#
# Design notes:
#   - Coloured tiles are drawn as plotly shapes (no hover capability)
#   - Two separate invisible scatter traces carry hover text — one per course
#     (bottom and top) so the mouse can distinguish the two overlapping bin sets
#   - A third invisible trace bound to xaxis2 forces the top x-axis to render
#   - Alternating grey/white bands help the eye track individual series
#   - With lag.max > 0, segments that correlate better at another position
#     (COFECHA's "B" flag, best.lag != 0) are purple, as in dplR's plot.crs,
#     and their hover text gives the best lag and the gain in correlation

crsPlotly <- function(x){
  yr.range <- function(x, yr.vec = as.numeric(names(x))) {
    na.flag <- is.na(x)
    if (all(na.flag)) {
      res <- rep(NA, 2)
      mode(res) <- mode(yr.vec)
      res
    } else {
      range(yr.vec[!na.flag])
    }
  }
  
  rho  <- x$spearman.rho
  bins <- x$bins
  pval <- x$p.val
  # best.lag/best.rho exist from dplR 1.8.0; without them nothing is a B
  bLag <- if (is.null(x$best.lag)) rho * 0 else x$best.lag
  bRho <- if (is.null(x$best.rho)) rho else x$best.rho
  yrs  <- as.numeric(rownames(x$rwi))
  
  nseries    <- nrow(rho)
  nbins      <- nrow(bins)
  seg.length <- x$seg.length
  pcrit      <- x$pcrit
  
  seriesStartStop <- t(apply(x$rwi, 2, yr.range, yr.vec = yrs))
  
  # Order series by start date
  neworder        <- sort.int(seriesStartStop[, 1], decreasing = FALSE,
                              index.return = TRUE)$ix
  seriesStartStop <- seriesStartStop[neworder, ]
  rho             <- rho[neworder, , drop = FALSE]
  pval            <- pval[neworder, , drop = FALSE]
  bLag            <- bLag[neworder, , drop = FALSE]
  bRho            <- bRho[neworder, , drop = FALSE]
  
  dat <- data.frame(
    seriesOrder = seq_len(nseries),
    seriesName  = as.character(rownames(seriesStartStop)),
    xMin        = seriesStartStop[, 1],
    xMax        = seriesStartStop[, 2],
    stringsAsFactors = FALSE
  )
  
  # ── Build long data frames for bottom and top course ──────────────────────
  buildLong <- function(colIdx, courseAdj) {
    # no bins in this course (one bin in all), or none with a correlation
    # (a stretch of years no series covers for a whole segment)
    if (length(colIdx) == 0 || all(is.na(rho[, colIdx]))) return(NULL)
    centers <- rowMeans(bins[colIdx, , drop = FALSE])
    # One row per series x bin; rho, p, best lag and best rho side by side
    cell <- expand.grid(i = seq_len(nseries), j = seq_along(colIdx))
    m    <- cbind(cell$i, colIdx[cell$j])
    rhoLong <- data.frame(seriesOrder = dat$seriesOrder[cell$i],
                          seriesName  = dat$seriesName[cell$i],
                          binCenter   = centers[cell$j],
                          rho         = rho[m],
                          width       = seg.length,
                          pval        = pval[m],
                          lag         = bLag[m],
                          bestRho     = bRho[m],
                          stringsAsFactors = FALSE)
    rhoLong <- rhoLong[!is.na(rhoLong$rho), ]
    rhoLong$courseAdj <- courseAdj
    
    # Add incomplete edge tiles
    firstYrs <- dat %>% select(seriesOrder, seriesName, xMin)
    lastYrs  <- dat %>% select(seriesOrder, seriesName, xMax)
    
    firstBin <- inner_join(rhoLong, firstYrs, by = c("seriesOrder","seriesName")) %>%
      group_by(seriesName) %>%
      slice(which.min(binCenter)) %>%
      mutate(binEdge = binCenter - width/2,
             width   = binEdge - xMin,
             binCenter = xMin + width/2) %>%
      select(-xMin, -binEdge)
    firstBin$rho  <- NA
    firstBin$pval <- NA
    firstBin$lag  <- NA
    
    lastBin <- inner_join(rhoLong, lastYrs, by = c("seriesOrder","seriesName")) %>%
      group_by(seriesName) %>%
      slice(which.max(binCenter)) %>%
      mutate(binEdge = binCenter + width/2,
             width   = xMax - binEdge,
             binCenter = xMax - width/2) %>%
      select(-xMax, -binEdge)
    lastBin$rho  <- NA
    lastBin$pval <- NA
    lastBin$lag  <- NA
    
    bind_rows(rhoLong, firstBin, lastBin)
  }
  
  bottomIdx <- which(seq_len(ncol(rho)) %% 2 == 1)
  topIdx    <- which(seq_len(ncol(rho)) %% 2 == 0)
  
  dat2 <- bind_rows(
    buildLong(bottomIdx, courseAdj = 0.75),
    buildLong(topIdx,    courseAdj = 0.25)
  )
  
  # ── Classify rho into colour bands ────────────────────────────────────────
  breaksRho  <- c(-Inf, seq(0, 1, by = 0.1))
  labelNames <- c("<0", "0.0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4",
                  "0.4-0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8",
                  "0.8-0.9", "0.9-1.0")
  
  dat2 <- dat2 %>%
    mutate(rhoFac = cut(rho, breaks = breaksRho, labels = labelNames)) %>%
    mutate(rhoFac = case_when(
      is.na(pval)          ~ "Incomplete",
      !is.na(lag) & lag != 0 ~ "B",
      pval > pcrit         ~ "NS",
      TRUE                 ~ as.character(rhoFac)
    ))
  
  # ── Colour palette ─────────────────────────────────────────────────────────
  # A fixed ramp over all eleven correlation bands, so a colour means the
  # same correlation in every file, plus the three special classes.
  colLookup <- c(
    setNames(colorRampPalette(c("lightblue", "darkblue"))(length(labelNames)),
             labelNames),
    Incomplete = "#90EE90",   # green: segment not fully covered
    NS         = "#ffcccb",   # red: below pcrit, best as dated (A)
    B          = "#9b59b6"    # purple: better at another lag (B)
  )
  
  # ── Build rectangles as plotly shapes ─────────────────────────────────────
  
  # Alternating grey/white background bands — one per series, drawn below tiles
  bandShapes <- lapply(seq_len(nseries), function(i) {
    list(
      type      = "rect",
      x0        = min(yrs),
      x1        = max(yrs),
      y0        = i - 1,
      y1        = i,
      fillcolor = if (i %% 2 == 0) "rgba(240,240,240,0.6)" else "rgba(255,255,255,0)",
      line      = list(width = 0),
      layer     = "below"
    )
  })
  
  # Tile shapes — bottom course meets the series centerline from below,
  # top course meets it from above. Both courses share the centerline edge
  # at seriesOrder - 0.5.
  tileH      <- 0.22
  tileShapes <- vector("list", nrow(dat2))
  for (i in seq_len(nrow(dat2))) {
    row      <- dat2[i, ]
    center   <- row$seriesOrder - 0.5
    isBottom <- row$courseAdj == 0.75
    y0 <- if (isBottom) center - tileH else center
    y1 <- if (isBottom) center          else center + tileH
    lev     <- as.character(row$rhoFac)
    fillCol <- if (is.na(lev) || is.na(colLookup[lev])) {
      "rgba(200,200,200,0.4)"
    } else {
      colLookup[lev]
    }
    tileShapes[[i]] <- list(
      type      = "rect",
      x0        = row$binCenter - row$width / 2,
      x1        = row$binCenter + row$width / 2,
      y0        = y0,
      y1        = y1,
      fillcolor = fillCol,
      line      = list(color = "white", width = 0.5)
    )
  }
  
  shapes <- c(bandShapes, tileShapes)
  
  # ── Split dat2 into bottom and top for separate hover traces ──────────────
  # Two invisible scatter traces — one per course — so hovering over the bottom
  # tiles shows bottom-course bin dates and hovering over the top tiles shows
  # top-course bin dates. A single combined trace at the centerline makes both
  # courses indistinguishable to the mouse.
  dat2_bottom <- dat2[dat2$courseAdj == 0.75, ]
  dat2_top    <- dat2[dat2$courseAdj == 0.25, ]
  
  hoverText <- function(d) {
    lagTxt <- ifelse(!is.na(d$lag) & d$lag != 0,
                     paste0("<br>better at lag ", sprintf("%+d", as.integer(d$lag)),
                            ": r = ", round(d$bestRho, 3),
                            " (gain ", round(d$bestRho - d$rho, 3), ")",
                            ifelse(d$lag < 0, "<br>(missing ring?)", "<br>(false ring?)")),
                     "")
    paste0(d$seriesName, "<br>",
           round(d$binCenter - d$width / 2), "\u2013",
           round(d$binCenter + d$width / 2), "<br>",
           "r = ", round(d$rho,3), lagTxt)
  }
  
  # Map each row's rhoFac to its tile colour for the hover label background
  hoverBgColor <- function(d) {
    lev <- as.character(d$rhoFac)
    col <- colLookup[lev]
    col[is.na(col)] <- "rgba(200,200,200,0.4)"
    unname(col)
  }
  
  # ── Assemble plotly figure ─────────────────────────────────────────────────
  tickVals     <- seq_len(nseries) - 0.5
  seriesLabels <- dat$seriesName
  
  # Height: thin tiles per series plus generous margins for axes and padding
  plotHeight <- max(300, nseries * 22 + 200)
  
  # source = "crs" and customdata (the series name) let the app respond to a
  # click on a segment: see the plotly_click observer in server.R
  fig <- plot_ly(height = plotHeight, source = "crs") %>%
    
    # Bottom course hover trace — markers sit in the middle of bottom tiles
    add_trace(
      data       = dat2_bottom,
      x          = ~binCenter,
      y          = ~(seriesOrder - 0.5 - tileH / 2),
      type       = "scatter",
      mode       = "markers",
      marker     = list(opacity = 0, size = 1),
      text       = hoverText(dat2_bottom),
      customdata = ~seriesName,
      hoverinfo  = "text",
      hoverlabel = list(
        bgcolor   = hoverBgColor(dat2_bottom),
        font      = list(color = "white")
      ),
      showlegend = FALSE
    ) %>%
    
    # Top course hover trace — markers sit in the middle of top tiles
    add_trace(
      data       = dat2_top,
      x          = ~binCenter,
      y          = ~(seriesOrder - 0.5 + tileH / 2),
      type       = "scatter",
      mode       = "markers",
      marker     = list(opacity = 0, size = 1),
      text       = hoverText(dat2_top),
      customdata = ~seriesName,
      hoverinfo  = "text",
      hoverlabel = list(
        bgcolor   = hoverBgColor(dat2_top),
        font      = list(color = "white")
      ),
      showlegend = FALSE
    ) %>%
    
    # Third invisible trace bound to xaxis2 — forces the top x-axis to render.
    # Without at least one trace using xaxis2 plotly ignores the axis entirely.
    add_trace(
      x          = range(yrs),
      y          = c(nseries / 2, nseries / 2),
      type       = "scatter",
      mode       = "markers",
      marker     = list(opacity = 0, size = 1),
      xaxis      = "x2",
      hoverinfo  = "none",
      showlegend = FALSE
    ) %>%
    
    layout(
      shapes = shapes,
      margin = list(t = 60, r = 10, b = 50, l = 10),
      xaxis  = list(
        title    = "Year",
        zeroline = FALSE,
        showgrid = FALSE,
        side     = "bottom"
      ),
      xaxis2 = list(
        overlaying = "x",
        side       = "top",
        zeroline   = FALSE,
        showgrid   = FALSE,
        range      = range(yrs),
        title      = ""
      ),
      yaxis  = list(
        title    = "",
        tickmode = "array",
        tickvals = tickVals,
        ticktext = seriesLabels,
        zeroline = FALSE,
        showgrid = FALSE,
        range    = c(0, nseries)
      ),
      plot_bgcolor  = "white",
      paper_bgcolor = "white"
    ) %>%
    event_register("plotly_click") %>%
    config(
      displaylogo            = FALSE,
      modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d"),
      toImageButtonOptions   = list(format = "png")
    )
  
  fig
}