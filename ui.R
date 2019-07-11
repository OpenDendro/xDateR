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
                           tableOutput("rwlSummary")
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
                                sliderInput(inputId="segLength",
                                            label="Segment Length",
                                            min = 10,
                                            max = 100,
                                            value = 50,
                                            step=10))
                       ),
                       fluidRow(
                         column(4,
                                checkboxGroupInput(inputId = "select", inline = TRUE,
                                                   label = "Filter series",choices = c("")),
                                actionButton("update", "Update", class = "btn-primary",
                                             style='padding:4px; font-size:100%')
                         ),
                         column(2,
                                checkboxInput(inputId="prewhiten", label="Prewhiten",value=TRUE),
                                checkboxInput(inputId="biweight", label="Biweight",value=TRUE)),
                         column(3,
                                selectInput(inputId="n", label="n", 
                                            choices=c("NULL", seq(5,13,by=2)),
                                            selected = "NULL"),
                                selectInput(inputId="bin.floor", label="Bin Floor", 
                                            choices=c(0, 10, 50, 100),selected = 100)
                         ),
                         column(3,
                                numericInput(inputId="pcrit", label="P crit", 
                                             value=0.05,min=0,max=1,step=0.01),
                                selectInput(inputId="method", label="Method:", 
                                            choices=c("pearson", "kendall", "spearman"),
                                            selected = "spearman")
                         )
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
                       plotOutput("cssPlot")
              )
              # end tabs ----
  )
)

