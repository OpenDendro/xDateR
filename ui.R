library(shiny)
library(dplR)
library(DT)

ui <- fluidPage(
  title = "xDateR",
  tabsetPanel(type = "tabs",
              # 1st tab ----
              tabPanel("Introduction",
                       includeMarkdown("text/intro.rmd")
              ),
              # 2nd tab ----
              tabPanel("Upload RWL Data",
                       # Sidebar layout with input and output definitions
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
                           # Output: Data file
                           hr(),
                           verbatimTextOutput("rwlReport"),
                           hr(),
                           # add in box to choose plot type?
                           plotOutput("rwlPlot"),
                           hr(),
                           tableOutput("rwlSummary"),
                           hr(),
                           downloadButton("rwlSummaryReport", "Generate report")
                           
                         )
                       )
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
                                            choices=c(0, 10, 50, 100),selected = 100)
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
                       plotOutput("cssPlot"),
                       hr(),
                       fluidRow(
                         column(4,
                                selectInput(inputId = "selectSeries",
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
                                checkboxGroupInput(inputId = "selectMaster", 
                                                   inline = TRUE,
                                                   label = "Filter series from master",
                                                   choices = c("")),
                                actionButton("updateMaster", "Update master", class = "btn-primary",
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
                                            choices=c(0, 10, 50, 100),selected = 100)
                         ),
                         column(3,
                                numericInput(inputId="pcritCSS", label="P crit", 
                                             value=0.05,min=0,max=1,step=0.01),
                                selectInput(inputId="methodCSS", label="Method", 
                                            choices=c("pearson", "kendall", "spearman"),
                                            selected = "spearman")
                         )
                       ),
                       plotOutput("ccfPlot"),
                       numericInput(inputId="lagCCF", label="Max lag", 
                                    value=5,min=1,max=100,step=1),
                       hr(),
                       plotOutput("xskelPlot"),
                       fluidRow(
                         column(2),
                         column(4,
                               sliderInput(inputId = "selectWinStart",
                                           label="Year to center window",
                                           value=NA,
                                           min=NA,
                                           max=NA,
                                           step=NA,
                                           sep = "")
                         ),
                         column(4,
                                sliderInput(inputId="selectWinWidth", 
                                            label="Window width",
                                            value=50,min=20,max=100,step=10)
                         ),
                         column(2)
                       )
                       
              )
              # end tabs ----
  )
)

