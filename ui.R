library(shiny)
library(dplR)
library(DT)
library(shinyjs)
library(shinyWidgets)



ui <- tagList(
  useShinyjs(),
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  navbarPage(
    title = "xDateR",
    id = "navbar",
    # 1st tab ----
    tabPanel(title="Introduction and Upload",value="tab1",
             sidebarLayout(
               # Sidebar panel for inputs
               sidebarPanel(
                 h5("Upload RWL"),
                 includeMarkdown("text_upload.rmd"),
                 hr(),
                 h5("Dated Series"),
                 fileInput(inputId="file1", 
                           label=NULL,
                           multiple = FALSE,
                           accept = c("text/plain",
                                      ".rwl",
                                      ".raw",
                                      ".txt")),
                 checkboxInput(inputId="useDemoDated", label="Or use example data",
                               value=FALSE),
                 shinyjs::hidden(div(id='divUndated', 
                                     hr(),
                                     h5("Undated Series (Optional)"),
                                     fileInput(inputId="file2", 
                                               label=NULL,
                                               multiple = FALSE,
                                               accept = c("text/plain",
                                                          ".rwl",
                                                          ".raw",
                                                          ".txt")),
                                     checkboxInput(inputId="useDemoUndated", label="Or use example data",
                                                   value=FALSE)
                 ))
               ),
               
               # Main panel for displaying outputs
               mainPanel(
                 includeMarkdown("text_intro.rmd")
               )
             )
    ),
    # 2nd tab ----
    
    tabPanel(title="Describe RWL Data",value="tab2",
             # Sidebar layout with input and output definitions
             includeMarkdown("text_describe.rmd"),
             hr(),
             plotOutput("rwlPlot"),
             selectInput(inputId="rwlPlotType", label="Plot Type", 
                         choices=c("seg","spag"),
                         selected = "seg"),
             hr(),
             h5("RWL Report"),
             verbatimTextOutput("rwlReport"),
             hr(),
             h5("Series Summary"),
             tableOutput("rwlSummary"),
             hr(),
             downloadButton("rwlSummaryReport", "Generate report")
    ),
    # 3rd tab ----
    tabPanel(title="Correlations Between Series", value="tab3",
             includeMarkdown("text_rwl_correlation.rmd"),
             h5("Correlation by Series and Segment"),
             plotOutput("crsPlot"),
             hr(),
             fluidRow(
               column(12,align="center",
                      sliderInput(inputId="seg.lengthCRS",
                                  label="Segment Length",
                                  min = 10,
                                  max = 200,
                                  value = 50,
                                  step=10))
             ),
             fluidRow(
               column(4,
                      awesomeCheckboxGroup(inputId = "master", 
                                           inline = TRUE,
                                           label = "Filter series",
                                           choices = c("")),
                      actionButton(inputId="updateMasterButton", 
                                   label="Update", 
                                   class = "btn-primary",
                                   style='padding:4px; font-size:100%')
               ),
               column(2,
                      checkboxInput(inputId="prewhitenCRS", label="Prewhiten",value=TRUE),
                      checkboxInput(inputId="biweightCRS", label="Biweight",value=TRUE)),
               column(3,
                      selectInput(inputId="nCRS", label="n", 
                                  choices=c("NULL", seq(5,13,by=2)),
                                  selected = "NULL"),
                      selectInput(inputId="bin.floorCRS", label="Bin Floor", 
                                  choices=c(0, 10, 50, 100),selected = 10)
               ),
               column(3,
                      numericInput(inputId="pcritCRS", label="P crit", 
                                   value=0.05,min=0,max=1,step=0.01),
                      selectInput(inputId="methodCRS", label="Method", 
                                  choices=c("pearson", "kendall", "spearman"),
                                  selected = "spearman")
               )
             ),
             fluidRow(
               hr(),
               h5("Correlation Overview"),
               p("Significant correlations are in bold."),
               column(4,
                      DTOutput("crsOverall")),
               column(4,
                      DTOutput("crsAvgCorrBin")),
               column(4,
                      DTOutput("crsFlags"))
             ),
             fluidRow(
               hr(),
               h5("Correlation by Series/Segment"),
               DTOutput("crsCorrBin")),
             hr(),
             downloadButton("crsReport", "Generate report")
    ),
    # 4th tab ----
    tabPanel(title="Individual Series Correlations", value="tab4",
             includeMarkdown("text_series_correlation.rmd"),
             hr(),
             h5("Flagged Series"),
             textOutput("flaggedSeries"),
             h5("Series Correlation"),
             plotOutput("cssPlot"),
             hr(),
             fluidRow(
               column(4,
                      selectInput(inputId = "series",
                                  label = "Choose series",
                                  choices = c("foo"))
               ),
               column(8,align="center",
                      sliderInput(inputId="seg.lengthCSS",
                                  label="Segment Length",
                                  min = 10,
                                  max = 200,
                                  value = 50,
                                  step=10))
             ),
             fluidRow(
               column(2),
               column(2,
                      checkboxInput(inputId="prewhitenCSS", label="Prewhiten",value=TRUE),
                      checkboxInput(inputId="biweightCSS", label="Biweight",value=TRUE)),
               column(3,
                      selectInput(inputId="nCSS", label="n", 
                                  choices=c("NULL", seq(5,13,by=2)),
                                  selected = "NULL"),
                      selectInput(inputId="bin.floorCSS", label="Bin Floor", 
                                  choices=c(0, 10, 50, 100),selected = 10)
               ),
               column(3,
                      numericInput(inputId="pcritCSS", label="P crit", 
                                   value=0.05,min=0,max=1,step=0.01),
                      selectInput(inputId="methodCSS", label="Method", 
                                  choices=c("pearson", "kendall", "spearman"),
                                  selected = "spearman")
               ),
               column(2)
             ),
             h5("Series Cross-Correlation by Segment"),
             plotOutput("ccfPlot"),
             fluidRow(
               column(2),
               column(4,
                      numericInput(inputId="lagCCF", label="Max lag", 
                                   value=5,min=1,max=100,step=1)
               ),
               column(4,
                      # Think this has to be a renderUI
                      #sliderInput(inputId="rangeCCF", 
                      #            label="Adjust plotted years", 
                      #            min = NA, 
                      #            max = NA, 
                      #            value = c(NA,NA), 
                      #            step = 5,
                      #            sep = "", 
                      #            dragRange = TRUE)
                      uiOutput("rangeCCF")
               ),
               column(2)
             ),
             hr(),
             h5("Series Cross-Correlation with Skeleton Plot"),
             plotOutput("xskelPlot"),
             fluidRow(
               column(2),
               column(4,
                      # sliderInput(inputId = "winCenter",
                      #             label="Window Center",
                      #             value=NA,
                      #             min=NA,
                      #             max=NA,
                      #             step=NA,
                      #            sep = "")
                      uiOutput("winCenter")
               ),
               column(4,
                      sliderInput(inputId="winWidth", 
                                  label="Window width",
                                  value=50,
                                  min=10,
                                  max=200,
                                  step=10)
               ),
               column(2)
             ),
             hr(),
             textAreaInput(inputId="datingNotes", 
                           label="Dating notes",
                           value="",width="600px",height = "400px",
                           placeholder="Notes will be saved if you generate a report."),
             hr(),
             downloadButton("cssReport", "Generate report")
    ),
    # 5th tab ----
    tabPanel(title="Edit Series",value="tab5",
             sidebarLayout(
               sidebarPanel(
                 includeMarkdown("text_edit.rmd"),
                 hr(),
                 h5("Remove Row"),
                 fluidRow(
                   column(6,
                          actionButton("deleteRows", "Delete Measurement")
                   ),
                   column(6,
                          checkboxInput(inputId="deleteRingFixLast", 
                                        label="Fix Last Year",value=TRUE)
                   )
                 ),
                 helpText("Click \"Delete Measurement\" to delete a year (row). 
                                     If \"Fix Last Year\" is selected the last year of 
                                    growth will stay the same."),
                 hr(),
                 h5("Add Row"),
                 numericInput(inputId="insertValue", 
                              label="Measurement Value",
                              value = 0,
                              min = 0),
                 fluidRow(
                   column(6,
                          actionButton("insertRows", "Insert Measurement")
                   ),
                   column(6,
                          checkboxInput(inputId="insertRingFixLast", 
                                        label="Fix Last Year",value=TRUE)
                   ),
                   helpText("To add a year, enter a 
                                             value and click \"Insert Measurement.\" The new row appears above 
                                      the highlight. If \"Fix Last Year\" is selected the 
                                      last year of growth will stay the same.")
                 ),
                 hr(),
                 h5("Undo Changes"),
                 actionButton("revertSeries", "Revert Changes"),
                 hr(),
                 h5("Save Edited File"),
                 downloadButton('downloadRWL', 'Download rwl object (.rwl)'),
                 helpText("The rwl file is writen in tucson/decadal format readable 
                                     by standard dendro programs.(e.g., read.rwl() in 
                                     dplR)."),
                 hr(),
                 h5("Log"),
                 verbatimTextOutput("editLog"),
                 downloadButton("editReport", "Generate report"),
                 width=5),
               
               mainPanel(
                 textOutput("series2edit"),
                 hr(),
                 dataTableOutput("table1"),
                 width=7)
             )
    ),
    # 6th tab ----
    tabPanel(title="Undated Series",value="tab6",
             includeMarkdown("text_undated.rmd"),
             hr(),
             h5("RWL Report"),
             verbatimTextOutput("rwlReport2"),
             hr(),
             h5("Best Overall Dating for Series"),
             plotOutput("floaterPlot"),
             fluidRow(
               column(1),
               column(5,
                      selectInput(inputId = "series2",
                                  label = "Choose undated series",
                                  choices = c("bar")),
                      sliderInput(inputId="minOverlapUndated", 
                                  label="Minimim Overlap", 
                                  value=50,
                                  min=10,
                                  max=200,
                                  step=10)
               ),
               column(2,
                      checkboxInput(inputId="prewhitenUndated", label="Prewhiten",value=TRUE),
                      checkboxInput(inputId="biweightUndated", label="Biweight",value=TRUE)),
               column(3,
                      selectInput(inputId="nUndated", label="n", 
                                  choices=c("NULL", seq(5,13,by=2)),
                                  selected = "NULL"),
                      selectInput(inputId="methodUndated", label="Method", 
                                  choices=c("pearson", "kendall", "spearman"),
                                  selected = "spearman")
               ),
               column(1)
             ),
             hr(),
             plotOutput("ccfPlotUndated"),
             fluidRow(
               column(2),
               column(4,align="center",
                      sliderInput(inputId="seg.lengthUndated",
                                  label="Segment Length",
                                  min = 10,
                                  max = 200,
                                  value = 50,
                                  step=10)),
               column(4,
                      selectInput(inputId="bin.floorUndated", label="Bin Floor", 
                                  choices=c(0, 10, 50, 100),selected = 10),
                      numericInput(inputId="pcritUndated", label="P crit", 
                                   value=0.05,min=0,max=1,step=0.01)
               ),
               column(2)
             ),
             hr(),
             fluidRow(
               column(1),
               column(5,
                      fluidRow(
                        h5("Floater Report"),
                        htmlOutput("floaterText")
                      ),
                      fluidRow(
                        hr(),
                        actionButton("saveDates", "Save Dates"),
                        actionButton("removeDates", "Revert")
                      ),
                      fluidRow(
                        hr(),
                        h5("Log"),
                        verbatimTextOutput("dateLog")
                      )
               ),
               column(5,
                      textAreaInput(inputId="undatingNotes", 
                                    label="Dating notes",
                                    value="",width="600px",height = "400px",
                                    placeholder="Notes will be saved if you generate a report.")
               ),
               column(1)
             ),
             hr(),
             h5("Save Dated Series to File"),
             downloadButton('downloadUndatedRWL', 'Download dated rwl object (.rwl)'),
             checkboxInput(inputId="appendMaster", label="Append dated rwl?",
                           value=FALSE),
             helpText("The rwl file contains any series where dates were assigned above. 
                       The series from the dated (master) rwl
                      can optionally be included in the output file as well.
                      The file is writen in tucson/decadal format readable by standard dendro 
                      programs.(e.g., read.rwl() in dplR)."),
             hr(),
             downloadButton("undatedReport", "Generate report")
             
    )
    # end tabs ----
  )
)

