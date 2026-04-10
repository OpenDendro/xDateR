# ══════════════════════════════════════════════════════════════════════════════
# xDateR — ui.R
# Shiny app for interactive crossdating of tree-ring data.
# Built on bslib (Bootstrap 5 / flatly theme) with a persistent sidebar and
# five main panels. The sidebar holds global controls (file upload, series
# selector, shared analysis parameters) that remain visible across all panels.
#
# Panel structure:
#   1. Overview    — welcome / data summary + RWL plot
#   2. Correlations — corr.rwl.seg output: plot, tables, series filter
#   3. Series       — corr.series.seg + ccf.series.rwl for one series
#   4. Edit         — skeleton plot + ring insert/delete table
#   5. Floater      — xdate.floater for undated/floating series
#
# Key design decisions:
#   • Shared analysis parameters live in the sidebar under ONE set of input IDs
#     (seg.length, bin.floor, n, pcrit, method, prewhiten, biweight). All
#     panels read from these same IDs — no duplicated inputs per panel.
#   • The Overview panel shows a welcome screen when no data is loaded and
#     switches to the data view once a file is uploaded.
#   • Scrolling within panels is intentional and expected — this is a work tool,
#     not a dashboard. The sidebar never scrolls.
#   • Reports (downloadable HTML via rmarkdown) capture all parameters so the
#     analysis is fully reproducible. This is critical for scientific use.
# ══════════════════════════════════════════════════════════════════════════════

# ── Package management ────────────────────────────────────────────────────────
# Install any missing packages on startup. This is intentional for deployment
# on shared servers where packages may not be pre-installed.

list.of.packages <- c("shiny", "rmarkdown", "markdown", "dplR",
                      "DT", "shinyjs", "shinyWidgets", "plotly",
                      "tidyverse", "RColorBrewer", "kableExtra",
                      "bslib", "bsicons")

new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)

library(shiny)
library(markdown)
library(rmarkdown)
library(dplR)
library(DT)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(RColorBrewer)
library(kableExtra)
library(bslib)
library(bsicons)


# ══════════════════════════════════════════════════════════════════════════════
# SHARED COMPONENTS
# ══════════════════════════════════════════════════════════════════════════════

# ── Shared analysis parameters ────────────────────────────────────────────────
# Rendered once in the sidebar and read by all panels via the same input IDs.
# Previously these were duplicated per-panel (seg.lengthCRS, seg.lengthCSS,
# seg.lengthUndated etc.) which caused maintenance headaches and user confusion.
# The Floater panel retains its own separate parameter inputs because floater
# analysis is often run with different settings than the main crossdating.

sharedParams <- function() {
  tagList(
    accordion(
      open = FALSE,
      accordion_panel(
        title = "Analysis Parameters",
        icon  = bs_icon("sliders"),
        
        # Segment length: length of the correlation window in years.
        # Typical values are 50 (default) or 100 years.
        sliderInput(
          inputId = "seg.length",
          label   = "Segment Length",
          min = 10, max = 200, value = 50, step = 10
        ),
        
        # Bin floor: anchor year for the first bin. 0 = start of data,
        # 10/50/100 = round to nearest decade/half-century/century.
        selectInput(
          inputId  = "bin.floor",
          label    = "Bin Floor",
          choices  = c(0, 10, 50, 100),
          selected = 10
        ),
        
        # n: filter length for the Hanning filter used to remove low-frequency
        # variation before correlation. NULL = no filter (prewhitening handles
        # this instead, which is the recommended default).
        selectInput(
          inputId  = "n",
          label    = "Hanning filter (n)",
          choices  = c("NULL", seq(5, 13, by = 2)),
          selected = "NULL"
        ),
        
        # pcrit: critical p-value for correlation significance. Segments with
        # p > pcrit are flagged red in the plot.
        numericInput(
          inputId = "pcrit",
          label   = "P crit",
          value   = 0.05, min = 0, max = 1, step = 0.01
        ),
        
        # method: correlation method passed to cor.test().
        selectInput(
          inputId  = "method",
          label    = "Method",
          choices  = c("spearman", "pearson", "kendall"),
          selected = "spearman"
        ),
        
        # prewhiten: fit an AR model to each series before correlating.
        # This removes autocorrelation (red noise) and is the default in dplR.
        checkboxInput("prewhiten", "Prewhiten", value = TRUE),
        
        # biweight: use Tukey's biweight robust mean for the master chronology.
        # Recommended when outliers may be present.
        checkboxInput("biweight", "Biweight", value = TRUE)
      )
    )
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# SIDEBAR
# ══════════════════════════════════════════════════════════════════════════════
# The sidebar is persistent across all panels. It holds file upload controls,
# the series selector, shared analysis parameters, and an About section.
# Width is 280px — wide enough for readable controls, narrow enough to leave
# the main panel breathing room.

appSidebar <- sidebar(
  width = 280,
  
  # ── Dated series upload ──────────────────────────────────────────────────
  # Always visible — this is the entry point for the app.
  h6("Dated Series", class = "text-muted fw-bold mt-1"),
  fileInput(
    inputId  = "file1",
    label    = NULL,
    multiple = FALSE,
    accept   = c("text/plain", ".rwl", ".raw", ".txt")
  ),
  helpText("Accepts Tucson, Heidelberg, compact, and TRiDaS .rwl formats."),
  checkboxInput("useDemoDated", "Use example data", value = FALSE),
  
  hr(),
  
  # ── Undated series upload ────────────────────────────────────────────────
  # Hidden until user navigates to the Floater panel AND a dated file is loaded.
  shinyjs::hidden(
    div(
      id = "divUndated",
      h6("Undated Series", class = "text-muted fw-bold"),
      fileInput(
        inputId  = "file2",
        label    = NULL,
        multiple = FALSE,
        accept   = c("text/plain", ".rwl", ".raw", ".txt")
      ),
      checkboxInput("useDemoUndated", "Use example data", value = FALSE),
      hr()
    )
  ),
  
  # ── Series selector ──────────────────────────────────────────────────────
  # Hidden until user reaches the Series or Edit panel.
  # The selected series is always excluded from the master (leave-one-out).
  shinyjs::hidden(
    div(
      id = "divSeriesSelector",
      h6("Series", class = "text-muted fw-bold"),
      selectInput(
        inputId  = "series",
        label    = NULL,
        choices  = c("Load a file first" = ""),
        selected = NULL
      ),
      hr()
    )
  ),
  
  # ── Shared analysis parameters ───────────────────────────────────────────
  # Hidden until user reaches the Correlations panel or beyond.
  shinyjs::hidden(
    div(
      id = "divSharedParams",
      sharedParams(),
      hr()
    )
  ),
  
  # ── About ────────────────────────────────────────────────────────────────
  # Always visible.
  accordion(
    open = FALSE,
    accordion_panel(
      title = "About",
      icon  = bs_icon("info-circle"),
      p("xDateR is a Shiny app for crossdating tree-ring data using the",
        a("dplR", href = "https://github.com/OpenDendro/dplR/",
          target = "_blank"), "package. It provides an interactive environment",
        "for assessing dating quality and identifying and fixing problems."),
      p(a(bs_icon("youtube"), " Video walkthrough",
          href = "https://youtu.be/TbjZvnnYCy0", target = "_blank")),
      p(a(bs_icon("github"), " xDateR on GitHub",
          href = "https://github.com/OpenDendro/xDateR", target = "_blank")),
      hr(),
      p(tags$strong("Please cite dplR if you use this app:")),
      p(tags$small(
        "Bunn AG (2008). A dendrochronology program library in R (dplR).",
        tags$em("Dendrochronologia"), ", 26(2), 115-124.",
        a("doi:10.1016/j.dendro.2008.01.002",
          href = "http://doi.org/10.1016/j.dendro.2008.01.002",
          target = "_blank")
      )),
      p(tags$small(
        "Bunn AG (2010). Statistical and visual crossdating in R using the dplR library.",
        tags$em("Dendrochronologia"), ", 28(4), 251-258.",
        a("doi:10.1016/j.dendro.2009.12.001",
          href = "http://doi.org/10.1016/j.dendro.2009.12.001",
          target = "_blank")
      )),
      hr(),
      p(tags$em("Remember: never rely purely on statistical crossdating —",
                "always go back to the wood."))
    )
  )
)


# ══════════════════════════════════════════════════════════════════════════════
# PANEL 1: OVERVIEW
# ══════════════════════════════════════════════════════════════════════════════
# The overview panel serves two roles:
#   • Before data loads: a welcome screen with tree-ring art and onboarding
#     instructions. The SVG art shows concentric growth rings emanating from
#     the left edge — a nod to the cross-section of a tree core.
#   • After data loads: a diagnostic data summary (key stats from rwl.report)
#     plus the RWL segment/spaghetti plot. The series summary table is
#     collapsed by default to keep the plot prominent.
#
# The switch between these two states is handled in server.R via renderUI
# (output$overviewUI). The panel itself just contains a single uiOutput
# placeholder.

panelOverview <- nav_panel(
  title = "Overview",
  icon  = bs_icon("house"),
  value = "OverviewTab",
  
  # Content switches between welcome and data view in server.R
  uiOutput("overviewUI")
)


# ══════════════════════════════════════════════════════════════════════════════
# PANEL 2: CORRELATIONS
# ══════════════════════════════════════════════════════════════════════════════
# Wraps dplR::corr.rwl.seg(). The primary crossdating diagnostic panel.
# Shows correlations between each series and the leave-one-out master
# chronology, broken into overlapping segments.
#
# The interactive plotly tile plot (crsPlotly) uses the shared parameters
# from the sidebar. Series can be temporarily filtered out of the master
# chronology using the checkbox group — useful when a known bad series would
# corrupt the master and give misleading correlations for the others.
#
# The "Update Master" button is intentionally explicit (not reactive to the
# checkboxes directly) so the user can make several filter decisions before
# triggering the potentially slow corr.rwl.seg() computation.

panelCorrelations <- nav_panel(
  title = "Correlations",
  icon  = bs_icon("grid-3x3"),
  value = "AllSeriesTab",
  
  # ── How to use this panel ─────────────────────────────────────────────
  accordion(
    open = FALSE,
    accordion_panel(
      title = "How to use this panel",
      icon  = bs_icon("info-circle"),
      p("This panel shows output from",
        a("corr.rwl.seg()", href = "https://rdrr.io/cran/dplR/man/corr.rwl.seg.html",
          target = "_blank"), "— the primary crossdating diagnostic in dplR.",
        "Each series is correlated against a master chronology built from all",
        "other series (leave-one-out principle). Correlations are calculated on",
        "overlapping segments (e.g., 50-year segments overlapped by 25 years).",
        "By default, series are prewhitened to remove low-frequency variation",
        "before correlation."),
      p("In the tile plot, each series appears as two rows:",
        tags$strong("blue"), "= p ≤ pcrit (good correlation);",
        tags$strong("red"), "= p > pcrit (potential dating problem);",
        tags$strong("green"), "= incomplete overlap (no correlation calculated).",
        "Hover over any tile to see the series name, bin dates, and correlation value."),
      p("If a series is flagged, investigate it in the",
        tags$strong("Series"), "panel. You can temporarily remove problem series",
        "from the master using the filter below — useful when a badly dated series",
        "would corrupt the master and give misleading correlations for the others.",
        "Proceed to the", tags$strong("Series"), "tab to investigate individual series.")
    )
  ),
  # Interactive plotly version of the classic dplR corr.rwl.seg plot.
  # Each series is shown as two rows of coloured tiles (bottom course on
  # lower axis timeline, top course on upper axis). Colour encodes
  # correlation strength: blue = good, red = flagged, green = incomplete.
  card(
    fill = FALSE,
    card_header(
      "Correlation by Series and Segment",
      tooltip(
        bs_icon("question-circle"),
        paste(
          "Output from corr.rwl.seg(). Each series is correlated against a",
          "master chronology built from all other series (leave-one-out).",
          "Each series appears as two rows of coloured tiles:",
          "blue = p \u2264 pcrit (good correlation),",
          "red = p > pcrit (potential dating problem),",
          "green = incomplete overlap with master (no correlation calculated).",
          "Adjust parameters in the sidebar and click 'Update Master' to",
          "recompute. Flagged series should be investigated in the Series tab."
        ),
        placement = "right"
      )
    ),
    # Height is computed dynamically in server.R based on number of series.
    uiOutput("crsPlotUI")
  ),
  
  # ── Series filter ──────────────────────────────────────────────────────
  card(
    fill = FALSE,
    card_header(
      "Filter Series from Master",
      tooltip(
        bs_icon("question-circle"),
        "Uncheck series to remove them from the master chronology.
         Click 'Update Master' to recompute. Filtering here does not
         permanently remove series — use the Edit panel for that."
      )
    ),
    div(
      style = "min-height: 80px;",
      awesomeCheckboxGroup(
        inputId  = "master",
        label    = NULL,
        inline   = TRUE,
        choices  = c("")
      )
    ),
    actionButton(
      inputId = "updateMasterButton",
      label   = "Update Master",
      class   = "btn-primary btn-sm"
    )
  ),
  
  # ── Summary tables ─────────────────────────────────────────────────────
  fluidRow(
    column(4,
           card(
             fill = FALSE,
             card_header(
               "Overall Correlation",
               tooltip(bs_icon("question-circle"),
                       "Mean correlation across all bins for each series.")
             ),
             DTOutput("crsOverall")
           )
    ),
    column(4,
           card(
             fill = FALSE,
             card_header(
               "Avg. Correlation by Bin",
               tooltip(bs_icon("question-circle"),
                       "Mean interseries correlation within each time bin.")
             ),
             DTOutput("crsAvgCorrBin")
           )
    ),
    column(4,
           card(
             fill = FALSE,
             card_header(
               "Flagged Series / Segments",
               tooltip(bs_icon("question-circle"),
                       "Series and segments with p > pcrit. Investigate these
                   in the Series panel.")
             ),
             DTOutput("crsFlags")
           )
    )
  ),
  
  # ── Full correlation matrix ────────────────────────────────────────────
  accordion(
    open = FALSE,
    accordion_panel(
      title = "Full Table of Correlation by Series / Segment",
      icon  = bs_icon("table"),
      tooltip(
        bs_icon("question-circle"),
        "Full table of Spearman rho values for each series by bin.
         Bold values exceed the pcrit threshold."
      ),
      DTOutput("crsCorrBin")
    )
  ),
  
  div(class = "mt-2",
      downloadButton("crsReport", "Generate report"))
)


# ══════════════════════════════════════════════════════════════════════════════
# PANEL 3: SERIES
# ══════════════════════════════════════════════════════════════════════════════
# Deep-dive into a single series selected from the sidebar. Uses three dplR
# functions to triangulate dating quality:
#
#   corr.series.seg()  — running + segment correlations against master
#   ccf.series.rwl()   — cross-correlations by segment (detect lag offsets)
#   xskel.ccf.plot()   — skeleton plot + CCF (visual + statistical combined)
#
# The series is always removed from the master (leave-one-out). Users can
# also filter additional series from the master using the Correlations panel
# filter — those choices persist here.
#
# If any series were flagged in the Correlations panel, an alert banner
# appears at the top of this panel directing the user to investigate.
#
# Dating notes entered here are saved in the generated report, providing a
# record of the analyst's reasoning — important for reproducibility.

panelSeries <- nav_panel(
  title = "Series",
  icon  = bs_icon("activity"),
  value = "IndividualSeriesTab",
  
  # Alert banner: shows which series were flagged (rendered conditionally)
  uiOutput("flaggedSeriesUI"),
  
  # ── How to use this panel ─────────────────────────────────────────────
  accordion(
    open = FALSE,
    accordion_panel(
      title = "How to use this panel",
      icon  = bs_icon("info-circle"),
      p("Select a series from the sidebar to investigate it in detail. The",
        "selected series is always removed from the master chronology (leave-one-out).",
        "You can also filter additional series from the master using the Correlations",
        "panel filter — those choices persist here."),
      p(tags$strong("Segment Correlations"), " (",
        a("corr.series.seg()", href = "https://rdrr.io/cran/dplR/man/corr.series.seg.html",
          target = "_blank"), "): horizontal bars show the correlation for each",
        "overlapping segment. A centered running correlation complements the segment",
        "bars. The dashed line shows pcrit — a dip below it suggests a dating problem."),
      p(tags$strong("Cross-Correlations"), " (",
        a("ccf.series.rwl()", href = "https://rdrr.io/cran/dplR/man/ccf.series.rwl.html",
          target = "_blank"), "): shows cross-correlations at multiple lags for each",
        "segment. A strong peak at lag ±1 or ±2 is the classic signature of a missing",
        "or false ring — the series dates are off by that many years."),
      p("Use the sliders to isolate periods of poor correlation. Any notes taken will",
        "be saved in the generated report. The selected series can be edited in the",
        tags$strong("Edit"), "panel.")
    )
  ),
  
  # ── Segment correlation plot ───────────────────────────────────────────
  card(
    fill = FALSE,
    card_header(
      "Series vs Master: Segment Correlations",
      tooltip(
        bs_icon("question-circle"),
        paste("corr.series.seg(): horizontal bars show correlation for each",
              "overlapping segment; the curve shows running correlation.",
              "Dashed line = pcrit. Segments below pcrit suggest a dating problem.",
              "Overlapping segments are lagged by half the segment length.")
      )
    ),
    plotOutput("cssPlot", height = "400px")
  ),
  
  # ── Cross-correlation plot ─────────────────────────────────────────────
  card(
    fill = FALSE,
    card_header(
      "Series vs Master: Cross-Correlations by Segment",
      tooltip(
        bs_icon("question-circle"),
        paste("ccf.series.rwl(): cross-correlations at each lag for each time segment.",
              "A peak at lag 0 means good dating. A peak at lag ±1 or ±2 means the",
              "series may be missing or have a false ring — its dates are off by that",
              "many years. Adjust the time window with the sliders to isolate the problem.")
      )
    ),
    layout_columns(
      col_widths = c(4, 8),
      numericInput("lagCCF", "Max lag (years)",
                   value = 5, min = 1, max = 100, step = 1),
      uiOutput("rangeCCF")
    ),
    plotOutput("ccfPlot", height = "400px")
  ),
  
  # ── Dating notes ───────────────────────────────────────────────────────
  card(
    fill = FALSE,
    card_header(
      "Dating Notes",
      tooltip(bs_icon("question-circle"),
              "Notes are included verbatim in the generated report. Use this to
               document your interpretation — e.g. 'series appears to be missing
               a ring near 1850, correlations improve after deletion'. This record
               is important for reproducibility.")
    ),
    textAreaInput(
      inputId     = "datingNotes",
      label       = NULL,
      value       = "",
      width       = "100%",
      height      = "120px",
      placeholder = "Document your dating interpretation here. These notes will be saved in the generated report."
    )
  ),
  
  div(class = "mt-2",
      downloadButton("cssReport", "Generate report"))
)


# ══════════════════════════════════════════════════════════════════════════════
# PANEL 4: EDIT
# ══════════════════════════════════════════════════════════════════════════════
# Ring editing for the series selected in the sidebar. Uses dplR's
# insert.ring() and delete.ring() under the hood (via the server logic).
#
# Layout: skeleton plot at the top (visual crossdating aid), then a two-column
# layout with edit controls on the left and the scrollable measurements table
# on the right. The table and skeleton plot are linked by the window center
# slider — both show the same time window simultaneously.
#
# "Fix Last Year": when deleting a ring, this keeps the outer (most recent)
# year fixed and shifts all earlier years forward by one. When unchecked,
# the first year is fixed instead. This mirrors the dplR fix.last argument.
#
# The Save/Revert section only appears after at least one edit has been made
# (conditionalPanel keyed to output$showSaveEdits). The edit log and
# reproducible R code are included in the downloadable report.
#
# Note: the edit panel intentionally does NOT scroll independently of the
# main page — the skeleton plot and table need to be visible simultaneously.

panelEdit <- nav_panel(
  title = "Edit",
  icon  = bs_icon("pencil-square"),
  value = "EditSeriesTab",
  
  # ── Instructions ──────────────────────────────────────────────────────
  accordion(
    open = FALSE,
    accordion_panel(
      title = "How to use this panel",
      icon  = bs_icon("info-circle"),
      tags$ol(
        tags$li("Select a series in the sidebar (carry over from the Series panel)."),
        tags$li("Use the", tags$strong("Window Center"), "slider to scroll to the area you want to edit."),
        tags$li("Adjust", tags$strong("Window Width"), "— the measurements table scrolls to match."),
        tags$li("Click a row in the table to select a measurement."),
        tags$li("Use", tags$strong("Delete Selected Row"), "or", tags$strong("Insert Above Selected Row"), "to make your edit."),
        tags$li("Return to the", tags$strong("Series"), "panel to assess the effect of the edit."),
        tags$li("Repeat for other series as needed."),
        tags$li("Click", tags$strong("Revert All Changes"), "to undo, or",
                tags$strong("Download edited .rwl"), "to save your work.")
      ),
      p(helpText("Fix Last Year: when checked, the outer (most recent) year is preserved",
                 "and earlier years shift to compensate. Uncheck to fix the first year instead.",
                 "This mirrors the fix.last argument in dplR's insert.ring() and delete.ring()."))
    )
  ),
  card(
    fill = FALSE,
    card_header(
      "Skeleton Plot",
      tooltip(
        bs_icon("question-circle"),
        "Combines the visual skeleton plot approach with cross-correlation
         analysis. The top panel shows normalised ring widths for the series
         (above) and master (below). The bottom panels show CCF for the first
         and second halves of the window. Adjust the window to focus on the
         area you want to edit."
      )
    ),
    textOutput("series2edit"),
    plotOutput("xskelPlot", height = "400px"),
    layout_columns(
      col_widths = c(6, 6),
      uiOutput("winCenter"),
      sliderInput(
        inputId = "winWidth",
        label   = "Window width (years)",
        value   = 40, min = 10, max = 80, step = 10
      )
    )
  ),
  
  # ── Edit controls + measurements table ────────────────────────────────
  fluidRow(
    column(8,
           card(
             fill = FALSE,
             card_header("Edit Controls"),
             layout_columns(
               col_widths = c(6, 6),
               div(
                 h6("Remove Ring"),
                 actionButton("deleteRows", "Delete Selected Row",
                              class = "btn-danger btn-sm w-100"),
                 checkboxInput("deleteRingFixLast", "Fix Last Year", value = TRUE),
                 helpText("Removes the selected measurement. If 'Fix Last Year' is
                      checked, the outer (most recent) year is preserved and
                      all earlier years shift forward by one. Uncheck to fix
                      the first year instead.")
               ),
               div(
                 h6("Insert Ring"),
                 numericInput("insertValue", "Ring width value (mm)",
                              value = 0, min = 0, step = 0.01),
                 actionButton("insertRows", "Insert Above Selected Row",
                              class = "btn-success btn-sm w-100"),
                 checkboxInput("insertRingFixLast", "Fix Last Year", value = TRUE),
                 helpText("Inserts a new row above the selected measurement with the
                      value specified. The same Fix Last Year logic applies.")
               )
             )
           )
    ),
    column(4,
           card(
             fill = FALSE,
             card_header(
               "Measurements",
               tooltip(bs_icon("question-circle"),
                       "Click a row to select it, then use the controls on the left
                   to delete or insert a ring. The table automatically scrolls
                   to the current window center.")
             ),
             dataTableOutput("table1")
           )
    )
  ),
  
  # ── Save / Revert / Log ────────────────────────────────────────────────
  # Hidden until first edit is made. shinyjs::show/hide in server.R controls
  # visibility. Static placement here ensures editLog output binding registers
  # correctly — nested outputs inside renderUI don't bind reliably.
  shinyjs::hidden(
    div(
      id = "divSaveEdits",
      card(
        fill = FALSE,
        card_header("Save or Revert"),
        layout_columns(
          col_widths = c(6, 6),
          div(
            actionButton("revertSeries", "Revert All Changes",
                         class = "btn-warning btn-sm w-100"),
            helpText("Undoes all edits and resets the edit log.")
          ),
          div(
            downloadButton("downloadRWL", "Download edited .rwl",
                           class = "w-100"),
            helpText("Written in Tucson/decadal format, readable by
                      dplR::read.rwl() and standard dendro software.")
          )
        ),
        hr(),
        h6("Edit Log"),
        verbatimTextOutput("editLog"),
        div(class = "mt-2",
            downloadButton("editReport", "Generate report"))
      )
    )
  )
)


# ══════════════════════════════════════════════════════════════════════════════
# PANEL 5: FLOATER
# ══════════════════════════════════════════════════════════════════════════════
# For dating series with unknown or uncertain dates — "floating" series that
# need to be positioned against a dated master chronology.
#
# Uses xdate.floater() (defined in xdate.floater.R — not yet in dplR CRAN
# release but will be in the next version). This function slides the undated
# series along the master chronology and computes the correlation at each
# possible position, identifying the best-fit date range.
#
# The floater panel has its own separate parameter inputs (seg.lengthUndated,
# bin.floorUndated, pcritUndated) because floater analysis is often run with
# different settings — typically longer segments and more lenient pcrit — than
# the main crossdating analysis.
#
# Workflow:
#   1. Load a dated .rwl file (the master)
#   2. Upload an undated .rwl file via the sidebar (revealed automatically)
#   3. Select a series from the undated file
#   4. Review the correlation plot and best-fit dates
#   5. Save the dated series, then download the combined .rwl
#
# Note: reproducible R code is not yet available in the generated report
# because xdate.floater() is not yet on CRAN. This will be updated when the
# function is included in the next dplR release.

panelFloater <- nav_panel(
  title = "Floater",
  icon  = bs_icon("arrow-left-right"),
  value = "UndatedSeriesTab",
  
  # ── How to use this panel ─────────────────────────────────────────────
  accordion(
    open = FALSE,
    accordion_panel(
      title = "How to use this panel",
      icon  = bs_icon("info-circle"),
      p("This panel dates undated series by sliding them against a",
        "dated master chronology using", tags$code("xdate.floater()"), "— a function",
        "currently in development in dplR. The function computes the",
        "correlation between the undated series and the master at every possible",
        "position, identifying the best-fit date range."),
      p("To get started:"),
      tags$ol(
        tags$li("Load a dated", tags$strong(".rwl file"), "using the Dated Series upload in the sidebar."),
        tags$li("Navigate to this panel — the", tags$strong("Undated Series"),
                "upload will then appear in the sidebar."),
        tags$li("Upload your undated", tags$strong(".rwl file"), "and select a series from the dropdown."),
        tags$li("Review the", tags$strong("Best-Fit Dating"), "plot: the top panel shows the undated",
                "series (green) placed at its best-fit position against the master; the bottom panel",
                "shows the correlation at each candidate end year. The light blue band is the",
                "5th\u201395th percentile of typical interseries correlation in the master \u2014",
                "ideally the series peak should fall well within or above this band."),
        tags$li("Check the", tags$strong("Cross-Correlation by Segment"), "plot: a clean peak at",
                "lag 0 supports the proposed dating. A peak at lag \u00b11 or \u00b12 suggests the",
                "dates may still be off by that many years."),
        tags$li("If satisfied, click", tags$strong("Save These Dates"), "then repeat for other",
                "series as needed."),
        tags$li("Click", tags$strong("Download dated .rwl"), "to export. Optionally append the",
                "master chronology to the output file.")
      ),
      p(helpText("The Floater panel uses its own Segment Length, Bin Floor, and P crit controls",
                 "(visible once an undated file is loaded) as a way of looking at lagged correlations",
                 "given the best matched dates for the floating series. This gives the user",
                 "an idea of not only the best match overall but helps them think about possible",
                 "dating issues within the flaoter itself."))
    )
  ),
  
  # The welcome/prompt states are rendered dynamically.
  # Plot containers must be static so Shiny can bind renderPlot to them —
  # dynamic plotOutput inside renderUI breaks the output binding.
  div(uiOutput("floaterUI")),
  
  # Static plot containers — hidden until both files are loaded.
  # Shown/hidden via shinyjs in server.R when floaterUI switches to state 3.
  shinyjs::hidden(
    div(
      id = "divFloaterPlots",
      card(
        fill = FALSE,
        card_header(
          "Best-Fit Dating",
          tooltip(
            bs_icon("question-circle"),
            paste("The selected series is slid along the master chronology to find",
                  "the best-fit position. Top panel: the series (green) shown with",
                  "as a segment at the best-fit dates. Bottom panel: correlation",
                  "(at end year) of the undated series at all possible locations.",
                  "The blue band shows the 5th-95th percentile of the",
                  "interseries correlation in the master — the undated series should",
                  "fall within this band. The dark blue line is the median interseries",
                  "correlation; the dashed black line is the significance threshold.")
          )
        ),
        layout_columns(
          col_widths = c(4, 8),
          div(
            uiOutput("floaterControls"),
            hr(),
            htmlOutput("floaterText"),
            hr(),
            actionButton("saveDates",   "Save These Dates",
                         class = "btn-primary btn-sm w-100"),
            br(), br(),
            actionButton("removeDates", "Revert Saved Dates",
                         class = "btn-warning btn-sm w-100"),
            hr(),
            h6("Date Log"),
            verbatimTextOutput("dateLog")
          ),
          plotOutput("floaterPlot", height = "500px")
        )
      ),
      card(
        fill = FALSE,
        card_header(
          "Cross-Correlation by Segment",
          tooltip(bs_icon("question-circle"),
                  paste("Cross-correlations between the undated series (placed at its",
                        "best-fit dates) and the master chronology for each segment.",
                        "A clean peak at lag 0 supports the proposed dating.",
                        "A peak at lag ±1 or ±2 suggests the dates may still be off."))
        ),
        uiOutput("floaterCCFParams"),
        plotOutput("ccfPlotUndated", height = "400px")
      ),
      card(
        fill = FALSE,
        card_header("Dating Notes"),
        textAreaInput(
          inputId     = "undatingNotes",
          label       = NULL,
          value       = "",
          width       = "100%",
          height      = "120px",
          placeholder = "Document your dating interpretation here. Notes will be saved in the generated report."
        )
      ),
      fluidRow(
        column(6,
               downloadButton("downloadUndatedRWL", "Download dated .rwl"),
               br(), br(),
               checkboxInput("appendMaster", "Append master chronology?",
                             value = FALSE),
               helpText("If checked, the dated master series are appended to the
                    output file. Written in Tucson/decadal format.")
        ),
        column(6,
               downloadButton("undatedReport", "Generate report"),
               br(), br(),
               helpText("Note: reproducible R code will be available in a future
                    release when xdate.floater() is added to dplR on CRAN.")
        )
      )
    )
  )
)


# ══════════════════════════════════════════════════════════════════════════════
# APP ASSEMBLY
# ══════════════════════════════════════════════════════════════════════════════
# page_navbar() from bslib wraps everything in a Bootstrap 5 navbar layout
# with a persistent sidebar. The theme uses the flatly bootswatch base with
# a forest green primary colour — appropriate for a dendrochronology tool.
# IBM Plex Sans is loaded from Google Fonts for clean, readable UI text.

ui <- tagList(
  useShinyjs(),   # required for sidebar show/hide of divUndated
  tags$head(
    tags$style(HTML("
      /* Remove bslib default card max-height so cards grow with content */
      .card { max-height: none !important; }
      /* Give each panel bottom breathing room */
      .tab-pane { padding-bottom: 3rem; }
    "))
  ),
  page_navbar(
    title = "xDateR",
    id    = "navbar",
    theme = bs_theme(
      version    = 5,
      bootswatch = "flatly",
      primary    = "#2C5F2E",         # forest green
      base_font  = font_google("IBM Plex Sans")
    ),
    sidebar = appSidebar,
    
    panelOverview,
    panelCorrelations,
    panelSeries,
    panelEdit,
    panelFloater,
    
    # GitHub link in the navbar — right-aligned via nav_spacer()
    nav_spacer(),
    nav_item(
      tags$a(
        bs_icon("github"),
        href   = "https://github.com/OpenDendro/xDateR",
        target = "_blank",
        title  = "xDateR on GitHub",
        class  = "text-muted"
      )
    )
  )
)