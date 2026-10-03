## Edits that change the measurements: ring insert/delete and gap filling.
## Sourced by server.R and printed in the reports' R code, so the code there
## reproduces the app's edits exactly. Keep it self-contained. Lines starting
## with ## (like these) are notes for developers and are left out of the
## reports; single-# comments are for the reports' readers.

# ── editRing ──────────────────────────────────────────────────────────────────
# Inserts or deletes one ring in one series of an rwl and returns the edited
# rwl. Works on the series' own span (first to last measurement, interior
# gaps kept as NA in place) with dplR's insert.ring()/delete.ring() and
# fix.length = FALSE, then puts the series back, adding or dropping years at
# the ends of the rwl as needed.
#
# Why not call insert.ring() on the whole rwl column? With fix.length = TRUE
# it keeps the column's length by dropping a value from one end. If the
# series starts (or ends) on the rwl's first (last) year, that dropped value
# is a real measurement.
#
# The year argument follows dplR: delete.ring() removes the ring AT `year`;
# insert.ring() inserts the new ring AFTER `year` (use first year - 1 to
# insert before the first ring).
editRing <- function(rwl, series, action = c("delete", "insert"), year,
                     value = NULL, fix.last = TRUE) {
  action <- match.arg(action)
  yrs    <- as.numeric(rownames(rwl))
  x      <- rwl[[series]]
  idx    <- which(!is.na(x))
  if (length(idx) == 0) stop("series '", series, "' has no measurements")
  span   <- seq(idx[1], idx[length(idx)])
  x      <- x[span]
  x.yrs  <- yrs[span]
  if (action == "delete" && length(x) < 2) {
    stop("cannot delete the only ring in series '", series, "'")
  }
  x2 <- if (action == "delete") {
    delete.ring(x, x.yrs, year = year, fix.last = fix.last,
                fix.length = FALSE)
  } else {
    insert.ring(x, x.yrs, year = year, ring.value = value,
                fix.last = fix.last, fix.length = FALSE)
  }
  x2.yrs <- as.numeric(names(x2))
  # Rebuild on a plain data.frame: the rwl `[` method in dplR >= 1.8.0
  # refuses row indices that are not consecutive years.
  out.yrs <- seq(min(yrs, x2.yrs), max(yrs, x2.yrs))
  out <- as.data.frame(lapply(unclass(rwl), function(col) col[match(out.yrs, yrs)]),
                       check.names = FALSE)
  out[[series]] <- unname(x2[match(out.yrs, x2.yrs)])
  # Trim leading/trailing years that no series covers any more
  covered <- which(rowSums(!is.na(out)) > 0)
  keep    <- seq(min(covered), max(covered))
  out     <- out[keep, , drop = FALSE]
  rownames(out) <- out.yrs[keep]
  attr(out, "dplR.provenance") <- attr(rwl, "dplR.provenance")
  class(out) <- class(rwl)
  out
}

# ── fillGaps ──────────────────────────────────────────────────────────────────
# Fills interior gaps (NA inside a series' span) in the named series with
# dplR's fill.internal.NA(). `fill` is 0 (the years are absent rings),
# "Linear" or "Mean". "Spline" is not offered: it can return a negative
# width, which write.tucson() writes as missing, so the gap comes back.
fillGaps <- function(rwl, series, fill) {
  # check.names = FALSE: series names like "704071" (common in ITRDB files)
  # would otherwise become "X704071" and the lookup below would fail
  filled <- fill.internal.NA(as.data.frame(unclass(rwl), check.names = FALSE)[, series, drop = FALSE],
                             fill = fill)
  for (s in series) rwl[[s]] <- filled[[s]]
  rwl
}
