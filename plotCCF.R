## Draws the cross-correlations by segment for the Series panel. Sourced by
## server.R and printed in the series report's R code, so the plot there can
## be reproduced. Keep it self-contained. Lines starting with ## are notes
## for developers and are left out of the reports.
##
## Why not let ccf.series.rwl() draw? It filters the series, computes and
## draws in one call, and draws every segment. To show only some years the
## app used to cut the data to those years first, which refits every spline
## and AR model on a fragment (and can fail outright: a series cut so that it
## starts in a run of zero rings has no positive spline). Now the whole
## series is filtered and correlated, and only the drawing is restricted.
## The lattice code below is dplR's own, from ccf.series.rwl().

# ── plotCCF ───────────────────────────────────────────────────────────────────
# Plots the result of dplR's ccf.series.rwl(..., make.plot = FALSE) for the
# segments that lie wholly inside the years `from` to `to`: the same plot
# ccf.series.rwl() draws, for those segments only. `pcrit` sets the dashed
# significance lines, as in ccf.series.rwl(). The segments are laid out in
# time order, left to right and top to bottom, `per.row` to a row.
plotCCF <- function(x, from = -Inf, to = Inf, pcrit = 0.05, per.row = 4) {
  keep <- x$bins[, 1] >= from & x$bins[, 2] <= to & !is.na(x$ccf[1, ])
  if (!any(keep)) {
    stop("no segment lies wholly inside ", from, "-", to,
         ": widen the years to plot")
  }
  r    <- x$ccf[, keep, drop = FALSE]
  bins <- x$bins[keep, , drop = FALSE]
  seg.length <- bins[1, 2] - bins[1, 1] + 1
  lag.vec <- as.numeric(sub("lag.", "", rownames(r), fixed = TRUE))
  ccf.df <- data.frame(r   = c(r),
                       bin = factor(rep(colnames(r), each = nrow(r)),
                                    levels = colnames(r)[order(bins[, 1])]),
                       lag = rep(lag.vec, ncol(r)))
  sig <- qnorm(1 - pcrit / 2) / sqrt(seg.length)
  sig <- c(-sig, sig)
  p <- lattice::xyplot(
    r ~ lag | bin, data = ccf.df,
    ylim = range(ccf.df$r, sig, na.rm = TRUE) * 1.1,
    xlab = "Lag", ylab = "Correlation",
    col.line = NA, cex = 1.25,
    as.table = TRUE,
    layout = c(min(per.row, ncol(r)), ceiling(ncol(r) / per.row)),
    sub = "Negative lags suggest a missing ring in the series",
    panel = function(x, y, ...) {
      lattice::panel.abline(h = seq(from = -1, to = 1, by = 0.1),
                            lty = "solid", col = "gray")
      lattice::panel.abline(v = lag.vec, lty = "solid", col = "gray")
      lattice::panel.abline(h = 0, v = 0, lwd = 2)
      lattice::panel.abline(h = sig, lwd = 2, lty = "dashed")
      col <- ifelse(y > 0, "darkred", "darkblue")
      bg  <- ifelse(y > 0, "lightsalmon", "lightblue")
      lattice::panel.segments(x1 = x, y1 = 0, x2 = x, y2 = y, col = col, lwd = 2)
      lattice::panel.dotplot(x, y, col = col, fill = bg, pch = 21, ...)
    })
  lattice::trellis.par.set(strip.background = list(col = "transparent"),
                           warn = FALSE,
                           par.sub.text = list(font = 1, cex = 0.75, just = "left",
                                               x = grid::unit(5, "mm")))
  print(p)
  invisible(p)
}
