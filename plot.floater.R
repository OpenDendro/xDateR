# params: the app's analysis parameters (xdParams()), so the envelope of
# typical interseries correlation is computed the same way as the floater's
# own correlations. NULL uses interseries.cor() defaults.
#
# x may carry the position in use (x$first, x$last, x$r), set by the app
# when the user takes a position other than the best fit. The upper panel
# always shows the series where x$rwlCombined has it; the lower panel then
# marks that position in green and the best fit in grey.
plot.floater <- function(x, params = NULL, ...) {
  series.name <- x$series.name
  floaterCorStats <- x$floaterCorStats
  rwlCombined <- x$rwlCombined
  
  # Quantile envelope from the dated master (excluding the floater)
  rwlOrig <- rwlCombined
  rwlOrig[, series.name] <- NULL
  isc <- do.call(interseries.cor,
                 c(list(rwl = rwlOrig),
                   params[intersect(names(params),
                                    c("n", "nyrs", "prewhiten",
                                      "ar.order.max", "biweight", "method"))]))
  quantCor <- quantile(isc[, 1],
                       probs = c(0.05, 0.5, 0.95))
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  # Plot 1: segment plot with floater highlighted in green
  yr <- as.numeric(row.names(rwlCombined))
  first.year <- as.matrix(apply(rwlCombined, 2, dplR:::yr.range, yr.vec = yr))[1, ]
  last.year  <- as.matrix(apply(rwlCombined, 2, dplR:::yr.range, yr.vec = yr))[2, ]
  neworder <- order(first.year, decreasing = FALSE)
  segs <- rwlCombined[, neworder, drop = FALSE]
  n.col <- ncol(segs)
  seq.col <- seq_len(n.col)
  for (i in seq.col) {
    segs[[i]][!is.na(segs[[i]])] <- i
  }
  seg2col <- which(names(segs) == series.name)
  segs.axis2 <- names(segs)
  segs.axis4 <- names(segs)
  segs.axis2[seq.col %% 2 == 0] <- NA
  segs.axis4[seq.col %% 2 == 1] <- NA
  
  par(mfcol = c(2, 1))
  par(mar = c(-0.1, 5, 2, 5) + 0.1, mgp = c(1.1, 0.1, 0), tcl = 0.5,
      xaxs = "i", yaxs = "i")
  xlim <- range(floaterCorStats$first, floaterCorStats$last, x$first, x$last)
  plot(yr, segs[[1]], type = "n", ylim = c(0, n.col + 1),
       xlim = xlim,
       axes = FALSE, ylab = "", xlab = "")
  abline(h = seq.col, lwd = 1, col = "grey")
  grid(ny = NA)
  apply(segs, 2, lines, x = yr, lwd = 4, lend = 2, col = "grey60")
  abline(h = seq.col[[seg2col]], lwd = 1, col = "darkgreen")
  lines(x = yr, y = segs[[seg2col]], lwd = 4, lend = 2, col = "darkgreen")
  axis(2, at = seq.col, labels = segs.axis2, srt = 45, tick = FALSE, las = 2)
  axis(4, at = seq.col, labels = segs.axis4, srt = 45, tick = FALSE, las = 2)
  axis(3)
  box()
  
  # Plot 2: correlation by end year with interseries quantile envelope
  rBest     <- which.max(floaterCorStats$r)
  firstBest <- floaterCorStats$first[rBest]
  lastBest  <- floaterCorStats$last[rBest]
  rBestVal  <- floaterCorStats$r[rBest]
  sig <- qnorm(1 - 0.05 / 2) / sqrt(floaterCorStats$n)
  
  par(mar = c(2, 5, -0.1, 5) + 0.1, yaxs = "r")
  plot(floaterCorStats$last, floaterCorStats$r,
       type = "n", xlab = "Year", ylab = "End Year Cor.",
       xlim = xlim,
       ylim = range(quantCor, floaterCorStats$r), axes = FALSE)
  xx <- c(min(floaterCorStats$first), max(floaterCorStats$last),
          max(floaterCorStats$last), min(floaterCorStats$first))
  yy <- c(quantCor[1], quantCor[1], quantCor[3], quantCor[3])
  polygon(xx, yy, border = NA, col = "lightblue")
  abline(h = quantCor[2], col = "darkblue")
  lines(floaterCorStats$last, sig, lty = "dashed")
  lines(floaterCorStats$last, floaterCorStats$r, col = "grey")
  abline(h = 0)
  mark <- function(first, last, r, col) {
    points(c(first, last), c(r, r), col = col, pch = 20)
    segments(x0 = first, x1 = last, y0 = r, y1 = r, lty = "dashed", col = col)
    text(x = last,  y = r, labels = last,  col = col, adj = c(0, 1))
    text(x = first, y = r, labels = first, col = col, adj = c(1, 1))
  }
  other <- !is.null(x$last) && x$last != lastBest
  # the best fit: green when it is the position in use, grey when it is not
  mark(firstBest, lastBest, rBestVal, if (other) "grey40" else "darkgreen")
  if (other) {
    if (!is.na(x$r)) {
      mark(x$first, x$last, x$r, "darkgreen")
    } else {
      # a position with no correlation (too little overlap): show its span
      abline(v = c(x$first, x$last), lty = "dashed", col = "darkgreen")
    }
  }
  axis(1); axis(2); box()
  
  invisible(x)
}
