list.of.packages <- c("shiny","rmarkdown","markdown","dplR",
                      "shinyWidgets","DT","shinyjs","plotly",
                      "tidyverse","RColorBrewer")
#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

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


ui <- tagList(
  useShinyjs(),
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  navbarPage(
    title = "xDateR",
    id = "navbar",
    # start tabs
    # 1st tab Introduction and Upload ----
    tabPanel(title="1. Introduction and Upload",value="IntroTab",
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
    # 2nd tab Describe RWL Data ----
    tabPanel(title="2. Describe RWL Data",value="DescribeTab",
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
    # 3rd tab Correlations Between Series ----
    tabPanel(title="3. Correlations Between Series", value="AllSeriesTab",
             includeMarkdown("text_rwl_correlation.rmd"),
             h5("Correlation by Series and Segment"),
#             plotOutput("crsPlot"),
#             actionButton("launchFancyPlot", "Make Interactive Plot"),
             plotlyOutput("crsFancyPlot"),
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
    # 4th tab Individual Series Correlations ----
    tabPanel(title="4. Individual Series Correlations", value="IndividualSeriesTab",
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
                      uiOutput("rangeCCF")
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
    # 5th (a) tab Edit Series ----
    tabPanel(title="5. Edit Series",value="EditSeriesTab",
             # start row 1 for skel ccf (might need spacer cols?)
             h5("Series Cross-Correlation with Skeleton Plot"),
             textOutput("series2edit"),
             plotOutput("xskelPlot"),
             fluidRow(
               column(2),
               column(4,
                      uiOutput("winCenter")
               ),
               column(4,
                      sliderInput(inputId="winWidth", 
                                  label="Window width",
                                  value=40,
                                  min=10,
                                  max=80,
                                  step=10)
               ),
               column(2)
             ), # end row 1
             # start row 2
             hr(),
             # end row 2
             fluidRow( # start row 3
               column(1), #space
               column(5, # col for remove on left
                      includeMarkdown("text_edit.rmd"),
                      h5("Remove Row"),
                      actionButton("deleteRows", "Delete Measurement"),
                      checkboxInput(inputId="deleteRingFixLast", 
                                    label="Fix Last Year",value=TRUE),
                      helpText("Click \"Delete Measurement\" to delete a year (row). 
                                     If \"Fix Last Year\" is selected the last year of 
                                    growth will stay the same."),
                      h5("Add Row"),
                      numericInput(inputId="insertValue", 
                                   label="Measurement Value",
                                   value = 0,
                                   min = 0),
                      actionButton("insertRows", "Insert Measurement"),
                      checkboxInput(inputId="insertRingFixLast", 
                                    label="Fix Last Year",value=TRUE),
                      helpText("To add a year, enter a 
                                             value and click \"Insert Measurement.\" The new row appears above 
                                      the highlight. If \"Fix Last Year\" is selected the 
                                      last year of growth will stay the same.")
               ), # end col for remove row
               column(5,  # start col for table
                      dataTableOutput("table1")
               ), #end col for table
               column(1) # space
             ), #end row 3
             # start row 4
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
             downloadButton("editReport", "Generate report")
             # end row 4
    ), # end tab
    # 5th (b) tab DataEditR Series ----
    # this works after a fashion but I'm not sure it will have 
    # the right functionality for keeping years fixed etc.
    # tabPanel(title="5b. Edit Series",value="DataEditRSeriesTab",
    #          sidebarLayout(
    #            sidebarPanel(
    #              includeMarkdown("text_DataEditR.rmd"),
    #              width=5),
    #            
    #            mainPanel(
    #              textOutput("series2DataEditR"),
    #              hr(),
    #              p("The data itself"),
    #              dataEditUI("edit-1"),
    #              p("was it there?"),
    #              width=7)
    #          )
    # ),
    # 6th tab Undated Series ----
    tabPanel(title="6. Undated Series",value="UndatedSeriesTab",
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
    # end tabs
  )
)


