# xDateR

## Overview

A shiny app for interactive crossdating. This is deployed as part of openDendro at:

https://viz.datascience.arizona.edu/xDateR/ 

## Package Management and Deployment Notes for Devs
This app is deployed on a Posit Connect server at the University of Arizona. Package management for deployment is handled via manifest.json, which must be generated on a Linux machine running the same version of R as the server.
Do not regenerate manifest.json locally on a Mac or Windows machine and commit it to the repo. Doing so will embed platform-specific binary information that is incompatible with the Linux server, causing the deployment to fail.
If packages need to be added, updated, or removed:
Update the package dependencies in the app code
Run renv::snapshot() to update renv.lock
Contact the server admin to regenerate manifest.json on the server
Commit and push both renv.lock and the new manifest.json

The renv.lock file is platform-neutral and should be kept in sync with manifest.json. It is used for local development reproducibility — run renv::restore() to recreate the package environment locally.

## Code layout
- `ui.R`, `server.R`: the app. `server.R` holds the reactive logic; one
  `xdParams()` list carries the analysis parameters to every dplR call and
  report.
- `appHelpers.R`: Shiny-free helpers (safe file reading, gaps, window
  bounds, A/B flags, R-code builders for the reports).
- `editRing.R`: ring insert/delete and gap filling. The edit report prints
  this file verbatim, so its R code reproduces the app's edits exactly. Keep
  it self-contained.
- `guide.R`: the two guided examples (steps and text): crossdating the
  dated file, and dating the floaters. They walk through the
  example data, `data/xDateRtest.rwl`, which is dplR's `co021` with two
  planted errors: ABC118 has its 1797 ring removed and ABC104 has its 1425
  ring duplicated. `data/xDateRtestUndated.rwl` holds three series taken
  from the same data for the Floater panel; ABC110 has its 1816 ring
  duplicated.
- `plotCCF.R`: draws the cross-correlations by segment for chosen years.
  Like `editRing.R`, it is printed in a report (the series report), so
  keep it self-contained.
- `plotlyCRSFunc.R`, `plot.floater.R`: the correlation tile plot and the
  floater plot. `svgArt.R`: decorative art for the welcome screens.
- `report_*.rmd`: the downloadable reports.

## Running and testing locally
The app needs dplR 1.8.0 or later. To run it against the development dplR
installed in the system library, bypass renv (the `.Rprofile` activates it)
with `--vanilla`:

    Rscript --vanilla -e 'shiny::runApp(".", port = 4815)'

Two test scripts, run from this directory, check the helpers and drive the
server through the main workflow (loading, filtering the master, edits,
gap fills, the floater, every download and report):

    Rscript --vanilla tests/test-helpers.R
    Rscript --vanilla tests/test-server.R

