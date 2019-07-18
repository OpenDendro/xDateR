library(shiny)
library(dplR)
library(DT)

ui <- fluidPage(
  title = "xDateR",
  tabsetPanel(type = "tabs",
              # 1st tab ----
              tabPanel("Introduction and Uplaod",
                       sidebarLayout(
                         # Sidebar panel for inputs
                         sidebarPanel(
                           h3("Upload a RWL file"),
                           fileInput(inputId="file1", 
                                     label=NULL,
                                     multiple = FALSE,
                                     accept = c("text/plain",
                                                ".rwl",
                                                ".raw",
                                                ".txt")),
                           includeMarkdown("text/upload.rmd")
                         ),
                         
                         # Main panel for displaying outputs
                         mainPanel(
                           includeMarkdown("text/intro.rmd")
                         )
                       )
              ),
              # 2nd tab ----
              tabPanel("Describe RWL Data",
                       # Sidebar layout with input and output definitions
                       includeMarkdown("text/describeRWL.rmd"),
                       hr(),
                       verbatimTextOutput("rwlReport"),
                       hr(),
                       plotOutput("rwlPlot"),
                       selectInput(inputId="rwlPlotType", label="Plot Type", 
                                   choices=c("seg","spag"),
                                   selected = "seg"),
                       hr(),
                       tableOutput("rwlSummary"),
                       hr(),
                       downloadButton("rwlSummaryReport", "Generate report")
              ),
              # 3rd tab ----
              tabPanel("Correlations between Series", 
                       includeMarkdown("text/corrRWL.rmd"),
                       plotOutput("crsPlot"),
                       hr(),
                       fluidRow(
                         column(12,align="center",
                                sliderInput(inputId="seg.lengthCRS",
                                            label="Segment Length",
                                            min = 10,
                                            max = 100,
                                            value = 50,
                                            step=10))
                       ),
                       fluidRow(
                         column(2),
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
                         ),
                         column(2)
                       ),
                       fluidRow(
                         hr(),
                         column(4,
                                DTOutput("crsOverall")),
                         column(4,
                                DTOutput("crsAvgCorrBin")),
                         column(4,
                                DTOutput("crsFlags"))
                       ),
                       fluidRow(
                         hr(),
                         DTOutput("crsCorrBin")),
                       hr(),
                       downloadButton("crsReport", "Generate report")
              ),
              # 4th tab ----
              tabPanel("Individual Series Correlations", 
                       includeMarkdown("text/corrSeries.rmd"),
                       textOutput("flaggedSeries"),
                       p("Output from corr.series.seg."),
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
                                            max = 100,
                                            value = 50,
                                            step=10))
                       ),
                       fluidRow(
                         column(4,
                                checkboxGroupInput(inputId = "master", 
                                                   inline = TRUE,
                                                   label = "Filter series from master",
                                                   choices = c("")),
                                actionButton(inputId="updateMasterButton", 
                                             label="Update master", 
                                             class = "btn-primary",
                                             style='padding:4px; font-size:100%')
                         ),
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
                         )
                       ),
                       p("Output from ccf.series.rwl"),
                       plotOutput("ccfPlot"),
                       fluidRow(
                         column(2),
                         column(4,
                                numericInput(inputId="lagCCF", label="Max lag", 
                                             value=5,min=1,max=100,step=1)
                         ),
                         column(4,
                                sliderInput(inputId="rangeCCF", 
                                            label="Adjust plotted years", 
                                            min = NA, 
                                            max = NA, 
                                            value = c(NA,NA), 
                                            step = 10,
                                            sep = "", 
                                            dragRange = TRUE)
                         ),
                         column(2)
                       ),
                       hr(),
                       p("Output from xskel.ccf.plot"),
                       plotOutput("xskelPlot"),
                       fluidRow(
                         column(2),
                         column(4,
                                sliderInput(inputId = "winCenter",
                                            label="Year to center window",
                                            value=NA,
                                            min=NA,
                                            max=NA,
                                            step=NA,
                                            sep = "")
                         ),
                         column(4,
                                sliderInput(inputId="winWidth", 
                                            label="Window width",
                                            value=50,min=20,max=100,step=10)
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
              tabPanel("Edit Series",
                       sidebarLayout(
                         sidebarPanel(
                           includeMarkdown("text/editSeries.rmd"),
                           hr(),
                           fluidRow(
                             column(6,
                                    actionButton("deleteRows", "Delete Measurement")
                             ),
                             column(6,
                                    checkboxInput(inputId="deleteRingFixLast", 
                                                  label="Fix Last Year",value=TRUE)
                             )
                           ),
                           helpText("Click \"Delete\" to delete a year (row). 
                                     If \"Fix Last Year\" is selected the last year of 
                                    growth will stay the same."),
                           hr(),
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
                                             value and click \"Insert.\" The new row appears above 
                                      the highlight. If \"Fix Last Year\" is selected the 
                                      last year of growth will stay the same.")
                           ),
                           hr(),
                           actionButton("revertSeries", "Revert Changes"),
                           hr(),
                           h4("Download"),
                           downloadButton('downloadRWL', 'Downlaod rwl object (.rwl)'),
                           helpText("The rwl file is writen in tucson/decadal format readable 
                                     by standard dendro programs.(e.g., read.rwl() in 
                                     dplR)."),
                           hr(),
                           h4("Log"),
                           verbatimTextOutput("editLog"),
                           downloadButton("editReport", "Generate report"),
                           width=5),
                         
                         mainPanel(
                           textOutput("series2edit"),
                           hr(),
                           dataTableOutput("table1"),
                           width=7)
                       )
              )
              # end tabs ----
  )
)

