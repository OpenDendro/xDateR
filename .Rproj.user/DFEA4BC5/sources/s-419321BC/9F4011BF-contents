library(shiny)
library(dplR)
library(DT)

ui <- fluidPage(
  
  title = "xDateR",
  tabsetPanel(type = "tabs",
              # first tab ----
              tabPanel("Introduction",
                       includeMarkdown("text/intro.rmd")
              ),
              tabPanel("Upload RWL Data",
                       includeMarkdown("text/upload.rmd"),
                       hr(),
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: Select a file ----
                           fileInput(inputId="file1", 
                                     label="Choose .rwl file",
                                     multiple = FALSE,
                                     accept = c("text/plain",
                                                ".rwl",
                                                ".raw",
                                                ".txt"))
                         ),
                         
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Data file ----
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
              # second tab ----
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
                                tableOutput("crsOverall")),
                         column(4,
                                tableOutput("crsAvgCorrBin")),
                         column(4,
                                #p("A summary of series that fall below the specified 
                                #   correlation level. These might warrant further 
                                #   investigation."),
                                tableOutput("crsFlags"))
                       ),
                       fluidRow(
                         hr(),
                         tableOutput("crsCorrBin")),
                       hr(),
                       downloadButton("crsReport", "Generate report")
              )
  )
)
