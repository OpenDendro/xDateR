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
#     (seg.length, bin.floor, lowFreq/n/nyrs, pcrit, method, prewhiten,
#     ar.order.max, biweight). All
#     panels read from these same IDs — no duplicated inputs per panel.
#   • The Overview panel shows a welcome screen when no data is loaded and
#     switches to the data view once a file is uploaded.
#   • Scrolling within panels is intentional and expected — this is a work tool,
#     not a dashboard. The sidebar never scrolls.
#   • Reports (downloadable HTML via rmarkdown) capture all parameters so the
#     analysis is fully reproducible. This is critical for scientific use.
# ══════════════════════════════════════════════════════════════════════════════

# ── Packages ──────────────────────────────────────────────────────────────────
# Installed by renv (locally) and from manifest.json (on Posit Connect); see
# the README. Nothing is installed at startup: installing on a live server
# at app launch is slow and can leave the app on untested package versions.
# The reports also use knitr and kableExtra, and load them themselves.
library(shiny)
library(rmarkdown)
library(dplR)

# xDateR uses dplR 1.8.0 features throughout (gap handling, nyrs and
# ar.order.max, lag search, rwl.check(), xdate.report()). An older dplR
# fails in confusing ways deep inside the app, so stop here and say why.
if (packageVersion("dplR") < "1.8.0") {
  stop("xDateR needs dplR 1.8.0 or later, but this R session loaded dplR ",
       packageVersion("dplR"), " from ", dirname(find.package("dplR")),
       ". Install a newer dplR (or run renv::restore()) and restart R.",
       call. = FALSE)
}
library(DT)
library(shinyjs)
library(plotly)
library(dplyr)        # tile-plot data wrangling in plotlyCRSFunc.R
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
        # Rendered dynamically in server.R so max is bounded by mean series
        # length of the loaded data, and default is min(50, mean series length).
        uiOutput("seg.length.ui"),
        
        tags$label(
          "Bin Floor",
          tooltip(
            bs_icon("question-circle"),
            paste(
              "Sets the anchor year for segment boundaries.",
              "With a bin floor of 10, segments align to years ending in 10",
              "(e.g. 1510\u20131560, 1560\u20131610). With 0, segments start from",
              "the first year of data with no rounding.",
              "Changing this shifts where bin edges fall, which can slightly",
              "affect correlation values. Use the same bin floor across analyses",
              "to keep results comparable."
            )
          )
        ),
        selectInput(
          inputId  = "bin.floor",
          label    = NULL,
          choices  = c(0, 10, 50, 100),
          selected = 10
        ),
        
        # Low-frequency filter applied before correlation. dplR >= 1.8.0
        # offers a Hanning filter (n) or a smoothing spline (nyrs), and they
        # cannot both be set, so the user picks one. "None" leaves the
        # low-frequency removal to prewhitening, the long-standing default.
        tags$label(
          "Low-frequency filter",
          tooltip(
            bs_icon("question-circle"),
            paste(
              "Removes slow growth trends before correlating.",
              "None: rely on prewhitening alone (the default).",
              "Hanning (n): a moving filter of n years; it trims years from",
              "the ends of each series.",
              "Spline (nyrs): divides each series by a smoothing spline of",
              "this rigidity in years (values of 1 or less are a proportion",
              "of each series' length), as COFECHA does with 32. It trims no",
              "years, but cannot be used if any series in the master has a gap."
            )
          )
        ),
        radioButtons(
          inputId  = "lowFreq",
          label    = NULL,
          choices  = c("None" = "none",
                       "Hanning (n)" = "hanning",
                       "Spline (nyrs)" = "spline"),
          selected = "none"
        ),
        conditionalPanel(
          condition = "input.lowFreq == 'hanning'",
          selectInput(
            inputId  = "n",
            label    = "Hanning filter length (n)",
            choices  = seq(5, 13, by = 2),
            selected = 7
          )
        ),
        conditionalPanel(
          condition = "input.lowFreq == 'spline'",
          numericInput(
            inputId = "nyrs",
            label   = "Spline rigidity (nyrs)",
            value   = 32, min = 0.01, step = 1
          )
        ),
        
        # pcrit: critical p-value for correlation significance. Segments with
        # p > pcrit are flagged red in the plot.
        numericInput(
          inputId = "pcrit",
          label   = "P crit",
          value   = 0.05, min = 0, max = 1, step = 0.01
        ),
        
        # lag.max: corr.rwl.seg() also correlates each segment with the master
        # shifted by up to this many years and reports the best lag. Segments
        # that fit better elsewhere are COFECHA's "B" flags. 0 turns it off.
        tags$label(
          "Lag search (\u00b1 years)",
          tooltip(
            bs_icon("question-circle"),
            paste(
              "Each segment is also correlated with the master shifted by up",
              "to this many years. A segment that fits better at another lag",
              "is flagged B and drawn purple: a negative lag suggests missing",
              "rings, a positive lag false rings. Read the lags along a series:",
              "the error is where the lag changes. 0 turns the search off.",
              "Must be less than the segment length."
            )
          )
        ),
        numericInput(
          inputId = "lag.max",
          label   = NULL,
          value   = 5, min = 0, max = 20, step = 1
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
        
        # ar.order.max: cap on the AR model order used to prewhiten. Only
        # meaningful when prewhitening, so shown only then. "Auto" lets ar()
        # choose by AIC, which on long series can reach 20 or more; each
        # order removes one year from the start of every series.
        conditionalPanel(
          condition = "input.prewhiten",
          tags$label(
            "Max AR order",
            tooltip(
              bs_icon("question-circle"),
              paste(
                "Upper limit on the order of the autoregressive model used to",
                "prewhiten. Auto chooses by AIC. Prewhitening removes as many",
                "years from the start of each series as the model's order.",
                "COFECHA uses a maximum of 3."
              )
            )
          ),
          selectInput(
            inputId  = "ar.order.max",
            label    = NULL,
            choices  = c("Auto (AIC)" = "NULL", 1:5),
            selected = "NULL"
          )
        ),
        
        # biweight: use Tukey's biweight robust mean for the master chronology.
        # Recommended when outliers may be present.
        checkboxInput("biweight", "Biweight", value = TRUE),
        
        # One click to the settings xdate.report() uses by default, which
        # follow COFECHA: 50-yr segments lagged 25, 32-yr spline, AR order
        # <= 3, Pearson, pcrit 0.01, lags to +/-10.
        actionButton("cofechaPreset", "Use COFECHA-like settings",
                     class = "xd-btn-quiet btn-sm w-100"),
        helpText(class = "mt-1",
                 "50-yr segments, 32-yr spline, AR order \u2264 3, Pearson,",
                 "p < 0.01, lags \u00b110: the settings of dplR's",
                 "COFECHA-style report."),
        actionButton("resetParams", "Reset to defaults",
                     icon = bs_icon("arrow-counterclockwise"),
                     class = "xd-btn-quiet btn-sm w-100")
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
  
  # ── Dated series: current file, switcher, upload ────────────────────────
  # Always visible — this is the entry point for the app. The current file
  # (name, size, span, gaps/edits) and, once more than one file has been
  # loaded, a switcher are rendered in server.R. Each file keeps its own
  # edits and master filter, so switching back finds the work where it was.
  h6("Dated Series", class = "text-muted fw-bold mt-1"),
  uiOutput("datedFileUI"),
  uiOutput("datedFileInfo"),
  # The guided example: offered when the example data are loaded (server.R)
  uiOutput("guideUI"),
  # A standard fileInput shown as one button: its file-name box and progress
  # bar are hidden by the .xd-upload CSS below, since the current file is
  # shown above instead.
  div(
    class = "xd-upload",
    fileInput(
      inputId     = "file1",
      label       = NULL,
      multiple    = FALSE,
      buttonLabel = tagList(bs_icon("folder2-open"), " Load a file\u2026"),
      accept      = c("text/plain", "text/csv", ".rwl", ".raw", ".txt", ".csv", ".fh", ".xml")
    )
  ),
  helpText("Tucson, Heidelberg, compact, TRiDaS, or .csv spreadsheets",
           "(years down, series across)."),
  # Start over: a fresh session for a user who is lost. Shown once a file is
  # loaded; asks before discarding edits (see server.R).
  shinyjs::hidden(
    div(
      id = "divStartOver",
      actionButton("startOver", "Start over",
                   icon  = bs_icon("arrow-counterclockwise"),
                   class = "xd-btn-quiet btn-sm w-100 mt-1"),
      helpText(class = "mt-1", "Clears all files and edits for a fresh start.")
    )
  ),
  
  hr(),
  
  # ── Undated series upload ────────────────────────────────────────────────
  # Hidden until user navigates to the Floater panel AND a dated file is loaded.
  shinyjs::hidden(
    div(
      id = "divUndated",
      h6("Undated Series", class = "text-muted fw-bold"),
      # Same widget as the dated file: current file and switcher (rendered
      # in server.R) above a single upload button
      uiOutput("undatedFileUI"),
      uiOutput("undatedFileInfo"),
      div(
        class = "xd-upload",
        fileInput(
          inputId     = "file2",
          label       = NULL,
          multiple    = FALSE,
          buttonLabel = tagList(bs_icon("folder2-open"), " Load a file\u2026"),
          accept      = c("text/plain", "text/csv", ".rwl", ".raw", ".txt", ".csv", ".fh", ".xml")
        )
      ),
      # The Floater guide: here, under the controls it talks about, so it is
      # seen on the Floater panel, where every one of its steps happens
      uiOutput("guideUIF"),
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
      # The settings in effect, in one line, so they are visible without
      # opening the accordion (rendered in server.R)
      uiOutput("paramSummary"),
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
  # Prompt shown until a dated file is loaded; the content is hidden till then
  uiOutput("noDataAllSeriesTab"),
  shinyjs::hidden(div(
    id = "contentAllSeriesTab",
  
  # QA alert banner — rendered in server.R, shown for tier 2/3 data issues
  uiOutput("qaAlertCorr"),
  
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
        tags$strong("red"), "= p > pcrit but best where dated (COFECHA's A flag);",
        tags$strong("purple"), "= correlates better at another lag (COFECHA's B flag);",
        tags$strong("green"), "= incomplete overlap (no correlation calculated).",
        "Hover over any tile to see the series name, bin dates, and correlation value."),
      p("If a series is flagged, investigate it in the",
        tags$strong("Series"), "panel. You can temporarily remove problem series",
        "from the master using the filter below — useful when a badly dated series",
        "would corrupt the master and give misleading correlations for the others.",
        "A removed series stays in your data: you can still test it against the",
        "master in the Series panel, edit it, and download it.",
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
          "red = p > pcrit but best where dated (A),",
          "purple = correlates better at another lag (B; hover for the lag),",
          "green = incomplete overlap with master (no correlation calculated).",
          "The plot recomputes when the Analysis Parameters change; 'Update",
          "Master' applies the series filter below. Flagged series should be",
          "investigated in the Series tab."
        ),
        placement = "right"
      )
    ),
    helpText(class = "mb-0",
             "Click a segment, or a row of Flagged Segments below, to open that",
             "series on the Series panel with the Edit window on that segment."),
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
        "Pick series to leave out of the master chronology, then click
         'Update Master'. Click the x on a name to put it back. Series
         left out stay in your data: they can still be tested, edited
         and downloaded. Type to search."
      )
    ),
    # One searchable box rather than a checkbox per series: it reads the
    # right way round (empty = every series is in the master) and works for
    # files with hundreds of series.
    #   closeAfterSelect: the list closes after each pick. Left open, it
    #     covered the content below and gave no obvious way to dismiss it.
    #   remove_button: an x on each chosen series.
    # The button sits beside the box, not under it, so the list (which
    # opens downwards) can never cover it.
    layout_columns(
      col_widths = c(9, 3),
      selectizeInput(
        inputId  = "leaveOut",
        label    = "Series left out of the master",
        choices  = NULL,
        multiple = TRUE,
        width    = "100%",
        options  = list(placeholder = "None: every series is in the master",
                        closeAfterSelect = TRUE,
                        # the list is attached to the page, or the card's
                        # layout clips it to a sliver
                        dropdownParent = "body",
                        plugins = list("remove_button"))
      ),
      div(class = "form-group shiny-input-container w-100",
          tags$label(class = "control-label", HTML("&nbsp;")),
          actionButton(
            inputId = "updateMasterButton",
            label   = "Update Master",
            class   = "btn-primary w-100"
          ))
    )
  ),
  
  # DTOutput(fill = FALSE) throughout this panel: with the default
  # (fill = TRUE) bslib gives a table in a card a fixed height and its own
  # scrollbar, so the page scrolls inside the card.
  # ── Flagged segments, full width: what to act on ───────────────────────
  # (It used to share a row with the two tables below, in a third of the
  # width, which pushed its Lag and Gain columns out of sight.)
           card(
             fill = FALSE,
             card_header(
               "Flagged Segments",
               tooltip(bs_icon("question-circle"),
                       "COFECHA-style flags. A: correlation under pcrit, but
                   the dated position is the best tested (weak, not misdated).
                   B: correlates better at another lag; a negative lag suggests
                   missing rings, positive false rings. Gain is how much the
                   correlation improves at that lag. 'Weak' B segments are
                   under the critical value even at their best lag: read them
                   as low correlations. Check B segments on the wood, using
                   the Series and Edit panels.")
             ),
             DTOutput("crsFlags", fill = FALSE)
           ),
  
  # ── Summary tables ─────────────────────────────────────────────────────
  layout_columns(
    col_widths = c(6, 6),
    card(
      fill = FALSE,
      card_header(
        "Overall Correlation",
        tooltip(bs_icon("question-circle"),
                "Each series' correlation with the master over its whole
                 length, with its p-value. Shaded rows are not significant
                 at P crit.")
      ),
      DTOutput("crsOverall", fill = FALSE)
    ),
    card(
      fill = FALSE,
      card_header(
        "Avg. Correlation by Bin",
        tooltip(bs_icon("question-circle"),
                "Mean correlation with the master within each time bin,
                 across the series that fill it.")
      ),
      DTOutput("crsAvgCorrBin", fill = FALSE)
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
        "Each series' correlation with the master in each bin, by the
         method chosen in the Analysis Parameters. Shaded as in the tile
         plot: red = under the critical value but best where dated (A),
         purple = fits better at another lag (B)."
      ),
      DTOutput("crsCorrBin", fill = FALSE)
    )
  ),
  
  layout_columns(
    col_widths = c(4, 8),
    div(class = "mt-2",
        downloadButton("crsReport", "Generate report")),
    card(
      fill = FALSE,
      card_header(
        "COFECHA-style report",
        tooltip(
          bs_icon("question-circle"),
          paste("A crossdating report laid out like COFECHA's output, from",
                "dplR's xdate.report(): series statistics, correlations by",
                "segment with A and B flags, the flagged segments with their",
                "lags and gains, and the data checks from rwl.check(). It uses",
                "the current Analysis Parameters and master filter, and",
                "records every setting.")
        )
      ),
      layout_columns(
        col_widths = c(5, 7),
        selectInput("cofechaType", NULL,
                    choices = c("HTML" = "html", "Text (ITRDB layout)" = "text",
                                "Markdown" = "markdown")),
        div(id = "divCofechaDownload",
            downloadButton("cofechaReport", "Download COFECHA-style report",
                           class = "btn-sm"))
      ),
      uiOutput("cofechaNote")
    )
  )
  ))
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
  # Prompt shown until a dated file is loaded; the content is hidden till then
  uiOutput("noDataIndividualSeriesTab"),
  shinyjs::hidden(div(
    id = "contentIndividualSeriesTab",
  
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
        "panel filter — those choices persist here. A series left out of the master",
        "can still be selected here: it is tested against the master built from the others."),
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
    # height set in server.R from the number of segments drawn
    plotOutput("ccfPlot", height = "auto")
  ),
  
  # ── Dating notes ───────────────────────────────────────────────────────
  card(
    fill = FALSE,
    card_header(
      textOutput("notesTitle", inline = TRUE),
      tooltip(bs_icon("question-circle"),
              "Notes are kept separately for each series and included verbatim
               in that series' report. Use this to document your interpretation
               — e.g. 'series appears to be missing a ring near 1850,
               correlations improve after deletion'. This record is important
               for reproducibility.")
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
  ))
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
  # Prompt shown until a dated file is loaded; the content is hidden till then
  uiOutput("noDataEditSeriesTab"),
  shinyjs::hidden(div(
    id = "contentEditSeriesTab",
  
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
        tags$li("Click a row in the table to select a measurement. Rows marked",
                tags$em("gap"), "are years with no measurement; they keep their",
                "place so the years after them stay correct. A gap row can't be",
                "deleted; fill absent rings with zero from the Overview panel."),
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
      uiOutput("winCenter.ui"),
      uiOutput("winWidth.ui")
    )
  ),
  
  # ── The selected series against the master: as loaded, and now ────────
  # What is wrong with this series before editing, and whether an edit
  # helped, without leaving the panel (rendered in server.R).
  uiOutput("editEffectUI"),
  
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
                 helpText(class = "mt-2", "Removes the selected measurement.")
               ),
               div(
                 h6("Insert Ring"),
                 numericInput("insertValue", "Ring width value (mm)",
                              value = 0, min = 0, step = 0.01),
                 actionButton("insertRows", "Insert Above Selected Row",
                              class = "btn-success btn-sm w-100"),
                 helpText(class = "mt-2", "Inserts a new row above the selected
                      measurement with the value given.")
               )
             ),
             # One setting for both actions (there used to be a checkbox each)
             checkboxInput("fixLast", "Fix Last Year", value = TRUE),
             helpText("Checked: the outer (most recent) year of the series keeps
                  its date, and the rings before the edit move by one year, as
                  for a core with a known bark date. Unchecked: the first year
                  is kept and the later rings move.")
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
          col_widths = c(4, 4, 4),
          div(
            actionButton("undoEdit", "Undo Last Edit",
                         icon  = bs_icon("arrow-counterclockwise"),
                         class = "xd-btn-quiet btn-sm w-100"),
            helpText("Takes back the most recent edit in the log below.")
          ),
          div(
            actionButton("revertSeries", "Revert All Changes",
                         class = "btn-warning btn-sm w-100"),
            helpText("Undoes all edits, including gap fills, and resets the edit log.")
          ),
          div(
            downloadButton("downloadRWL", "Download edited .rwl",
                           class = "w-100"),
            helpText("Every series in the file, including any left out of the
                      master. Written in Tucson/decadal format, readable by
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
  ))
)


# ══════════════════════════════════════════════════════════════════════════════
# PANEL 5: FLOATER
# ══════════════════════════════════════════════════════════════════════════════
# For dating series with unknown or uncertain dates — "floating" series that
# need to be positioned against a dated master chronology.
#
# Uses dplR's xdate.floater(), which slides the undated series along the
# master chronology and computes the correlation at each possible position,
# identifying the best-fit date range. It uses the sidebar's analysis
# parameters (filter, prewhitening, biweight, method) and the master as
# filtered on the Correlations panel.
#
# The cross-correlation card has its own segment length, bin floor and pcrit
# (seg.lengthUndated, bin.floorUndated, pcritUndated) because the floater is
# often checked with different segments than the main crossdating.
#
# Workflow:
#   1. Load a dated .rwl file (the master)
#   2. Upload an undated .rwl file via the sidebar (revealed automatically)
#   3. Select a series from the undated file
#   4. Review the correlation plot and best-fit dates
#   5. Save the dated series, then download the combined .rwl

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
        "dated master chronology using dplR's", tags$code("xdate.floater()"), ". It computes the",
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
        tags$li("Check", tags$strong("how clear-cut the fit is"), ": in Candidate positions the",
                "next best should correlate well below the best. If they are close, the dating is",
                "ambiguous. To use another position, click its row, or enter the last year by hand."),
        tags$li("Check the", tags$strong("Segments at These Dates"), ": every segment",
                "should fit best where dated. A run of segments that fit better a year off",
                "means a ring problem inside the series; the summary above the table says",
                "which segment to look in and whether to look for a missing or a false ring.",
                "The cross-correlation plot below shows the same thing by segment."),
        tags$li("If satisfied, click", tags$strong("Save These Dates"), "then repeat for other",
                "series as needed."),
        tags$li("Click", tags$strong("Download dated .rwl"), "to export. Optionally append the",
                "master chronology to the output file.")
      ),
      p(helpText("The search uses the Analysis Parameters in the sidebar and the master as",
                 "filtered on the Correlations panel. The Cross-Correlation card has its own",
                 "Segment Length, Bin Floor, and P crit controls for looking at lagged",
                 "correlations at the best-matched dates. This shows not only the best match",
                 "overall but also possible dating problems within the floater itself."))
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
            # candidate positions, a year entered by hand, and what is in use
            uiOutput("floaterPositionUI"),
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
          "Segments at These Dates",
          tooltip(bs_icon("question-circle"),
                  paste("The undated series, placed at the dates in use (the best fit",
                        "unless you chose another position), tested",
                        "segment by segment against the master, with the lag search",
                        "from the Analysis Parameters. A series can fit best overall",
                        "and still have segments that fit better a year off, which means",
                        "a ring problem inside it. The summary says which segment to look",
                        "in. Note the reading of the lag: a floater is placed by the part",
                        "that fits, so when the lagged run comes after correctly placed",
                        "segments, lag -1 points to a false ring and +1 to a missing one,",
                        "the reverse of a series dated from the bark. Flags as on the",
                        "Correlations panel. Below,",
                        "the cross-correlations by segment: a clean peak at lag 0",
                        "supports the dating."))
        ),
        uiOutput("floaterCCFParams"),
        uiOutput("floaterSegsUI"),
        h6(class = "mt-3", "Cross-correlations by segment"),
        plotOutput("ccfPlotUndated", height = "auto")
      ),
      card(
        fill = FALSE,
        card_header(textOutput("undatedNotesTitle", inline = TRUE)),
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
               # The download appears once a series' dates have been saved
               # (toggled in server.R); until then, say what to do.
               div(id = "divUndatedNone",
                   helpText(bs_icon("info-circle"),
                            "Nothing to download yet. Click", tags$strong("Save These Dates"),
                            "to keep this series' dates; the download will appear here.")),
               shinyjs::hidden(div(
                 id = "divUndatedDownload",
                 downloadButton("downloadUndatedRWL", "Download dated .rwl"),
                 br(), br(),
                 checkboxInput("appendMaster", "Append the master series?",
                               value = FALSE),
                 helpText("If checked, the series that build the master (with any
                      edits) are appended to the output; series left out of the
                      master are not. Written in Tucson/decadal format.")
               ))
        ),
        column(6,
               downloadButton("undatedReport", "Generate report"),
               br(), br(),
               helpText("Includes the R code to reproduce the search with dplR.")
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
  # Ask before the tab is closed or reloaded while there is unsaved work.
  # The server says when there is (custom message xdUnsaved, see server.R).
  tags$script(HTML("
    window.xdUnsaved = false;
    $(document).on('shiny:connected', function() {
      Shiny.addCustomMessageHandler('xdUnsaved', function(x) { window.xdUnsaved = x; });
    });
    window.addEventListener('beforeunload', function(e) {
      if (window.xdUnsaved) { e.preventDefault(); e.returnValue = ''; }
    });
  ")),
  tags$head(
    tags$style(HTML("
      /* Remove bslib default card max-height so cards grow with content */
      .card { max-height: none !important; }
      /* Give each panel bottom breathing room */
      .tab-pane { padding-bottom: 3rem; }
      /* Secondary buttons in body-text colour: Flatly's dark and
         secondary outlines are a pale grey that reads as disabled */
      .btn.xd-btn-quiet { color: var(--bs-body-color); border: 1px solid var(--bs-body-color);
                          background: transparent; }
      .btn.xd-btn-quiet:hover { background: var(--bs-gray-200); }
      /* File upload shown as a single full-width button (see appSidebar) */
      .xd-upload .shiny-input-container { margin-bottom: 0.25rem; width: 100%; }
      .xd-upload .input-group > .form-control,
      .xd-upload .progress { display: none; }
      .xd-upload .input-group-btn,
      .xd-upload .input-group-prepend,
      .xd-upload .btn-file { width: 100%; }
      .xd-upload .btn-file { border-radius: var(--bs-border-radius) !important;
                             background-color: var(--bs-primary);
                             border-color: var(--bs-primary); color: #fff; }
    "))
  ),
  page_navbar(
    title = "xDateR",
    id    = "navbar",
    # Panels scroll rather than fill the window. With bslib's default
    # (fillable = TRUE) each panel is a flex container that shrinks its
    # cards to fit the viewport, clipping plots and tables on short screens.
    fillable = FALSE,
    theme = bs_theme(
      version    = 5,
      bootswatch = "flatly",
      primary    = "#2C5F2E",         # forest green
      base_font  = font_google("IBM Plex Sans")
    ),
    sidebar = appSidebar,
    # A spinner on any output that is recalculating, and a pulse at the top
    # of the page while the server is busy, so a long computation on a large
    # file doesn't look like a hang
    header = useBusyIndicators(),
    
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