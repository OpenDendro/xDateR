library(shiny)
library(rmarkdown)
library(dplR)
library(DT)
library(shinyWidgets)

source("xdate.floater.R")  

# Server logic
shinyServer(function(session, input, output) {
  
  # Declare a few things
  
  # Initiate an object (empty now) that will hold the rwl data
  # (and a backup). The advantage of this is that it can be edited 
  # and saved between tabs
  rwlRV <- reactiveValues()
  # Here are the elements of rwlRV. These don't need to be decalred here
  # but I want to keep track of what's in the object as a matter of best
  # practices
  
  rwlRV$dated <- NULL           # the dated rwl object
  rwlRV$undated <- NULL         # the undated rwl object
  rwlRV$editLog <- NULL         # a string of edits
  rwlRV$editDF <- NULL          # a data.frame of edits
  rwlRV$seriesDF <- NULL        # the series being edited from rwlRV$undated. 
  # it is formated as a data.frame with years
  rwlRV$datedNoSeries <- NULL   # A copy of rwlRV$dated with the series being 
  # edited removed.
  rwlRV$datedVault <- NULL      # a copy of the dated rwl object that is not edited
  rwlRV$undatedVault <- NULL    # a copy of the undated rwl object that is not edited                              
  rwlRV$undated2dated <- NULL   # the undated rwl saved with dates
  rwlRV$dateLog <- NULL         # a string of edits for undated series
  
  rwlRV$editDF <- data.frame(series=NULL,
                             year = NULL,
                             value = NULL,
                             action = NULL,
                             fixLast = NULL)
  ##############################################################
  #
  # START observations
  #
  ##############################################################
  
  
  # When app is initiated, hide all the tabs but the first one.
  # This creates an oberver so that they can be toggled when triggered
  # by an event
  observe({
    hide(selector = "#navbar li a[data-value=tab2]")
    hide(selector = "#navbar li a[data-value=tab3]")
    hide(selector = "#navbar li a[data-value=tab4]")
    hide(selector = "#navbar li a[data-value=tab5]")
    hide(selector = "#navbar li a[data-value=tab6]")
  }, label = "tab hider")
  
  # When the dated RWL file is read in: 
  # 1. show all the tabs but the floater tab, and
  # 2. show the upload box for the floaters
  observeEvent({getRWL()},
               {
                 toggle(selector = "#navbar li a[data-value=tab2]")
                 toggle(selector = "#navbar li a[data-value=tab3]")
                 toggle(selector = "#navbar li a[data-value=tab4]")
                 toggle(selector = "#navbar li a[data-value=tab5]")
                 shinyjs::show('divUndated')
               }, label = "tab shower")
  
  # When an undated RWL file is read in, show all the floater tab
  observeEvent(
    eventExpr = {
      getRWLUndated()
    },
    handlerExpr = {
      toggle(selector = "#navbar li a[data-value=tab6]")
    }, 
    label = "tab shower")
  
  # This observes the input RWL and gets the names of the series. These names 
  # get passed to the checkboxes for possible filtering later (when the action button
  # is pressed).
  observeEvent(
    eventExpr = getRWL(), 
    handlerExpr = {
      # updateCheckboxGroupInput(session = session, inline = TRUE,
      #                          inputId = "master",
      #                          choices=colnames(getRWL()),
      #                          selected=colnames(getRWL()))
      updateAwesomeCheckboxGroup(session = session, inline = TRUE,
                                 inputId = "master",
                                 choices=colnames(getRWL()),
                                 selected=colnames(getRWL()),
                                 status = "primary")
    },
    label = "updates the check boxes for filtering the master")
  
  # observe which series gets selected for the individual series correlation
  # analysis and create the variable input$series. It deafaults to the first
  # series.
  observeEvent(
    eventExpr = {
      filteredRWL()
    }, 
    handlerExpr = {
      updateSelectInput(session = session,
                        inputId = "series",
                        choices=colnames(rwlRV$dated),
                        selected=colnames(rwlRV$dated)[1])
    },
    label = "observe series being selected and update the input box")
  
  
  # observe which series gets selected from the undates file
  # series.
  observeEvent(
    eventExpr = {
      getRWLUndated()
    }, 
    handlerExpr = {
      updateSelectInput(session = session,
                        inputId = "series2",
                        choices=colnames(rwlRV$undated),
                        selected=colnames(rwlRV$undated)[1])
    },
    label = "observe series being selected and update the input box for undated")
  
  # observe which series gets selected for the individual series correlation
  # get the start and end dates for the series. These are used to
  # update the window slider for the ccf window and xskel plot
  observeEvent(
    eventExpr = {
      # this is what triggers the observation
      input$series
      filteredRWL()
    }, 
    handlerExpr = {
      dat <- rwlRV$dated
      tmp <- summary(dat)
      winBnds <- as.numeric(tmp[tmp$series==input$series,2:3])
      minWin <- round(winBnds[1] + input$lagCCF,-1)
      maxWin <- round(winBnds[2] - input$lagCCF,-1)
      # updateSliderInput(session = session,
      #                   inputId = "winCenter",
      #                   value=round(mean(winBnds),-1),
      #                   min=minWin + 20,
      #                   max=maxWin - 50,
      #                   step=5)
      # 
      # updateSliderInput(session = session,
      #                   inputId = "rangeCCF",
      #                   value=c(minWin,
      #                           maxWin),
      #                   min=minWin,
      #                   max=maxWin,
      #                   step=5)
      output$winCenter <- renderUI({
        tagList(
          sliderInput(inputId = "winCenter",
                      label="Window Center",
                      min=minWin + 20,
                      max=maxWin - 50,
                      value=round(mean(winBnds),-1),
                      step=5,
                      sep = "")
        )
      })
      output$rangeCCF <- renderUI({
        tagList(
          sliderInput(inputId = "rangeCCF",
                      label="Adjust plotted years", 
                      min=minWin,
                      max=maxWin,
                      value=c(minWin,maxWin),
                      step=5,
                      sep = "", 
                      dragRange = TRUE)
        )
      })
    },
    label = "observe series being selected and update the window sliders")
  ##############################################################
  #
  # END observations
  #
  ##############################################################
  
  
  ##############################################################
  #
  # START reactives
  #
  # we use reactives for so that calculations (like corr.rwl.seg)
  # need to be done only once.
  #
  ##############################################################
  
  
  
  # Get the RWL file from the user at the start or use demo data
  getRWL <- reactive({
    if (input$useDemoDated) {
      dat <- read.rwl("data/xDateRtest.rwl")
      return(dat)
    }
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }
    else{
      # This might be problematic. What if the user wants a "long" tucson?
      # But we also don't want to add arguments.
      dat <- read.rwl(inFile$datapath)
      return(dat)
    }
    
  })
  
  # If the user wants to upload an undated RWL file. Note: the file has dates of 
  # some sort. But they can be real, arbitrary, etc. They get stripped when the 
  # floater code is run.
  getRWLUndated <- reactive({
    if (input$useDemoUndated) {
      dat <- read.rwl("data/xDateRtestUndated.rwl")
      rwlRV$undated <- dat
      return(dat)
    }
    inFile <- input$file2
    if (is.null(inFile)) {
      return(NULL)
    }
    else{
      dat <- read.rwl(inFile$datapath)
      rwlRV$undated <- dat
      return(dat)
    }
  })
  
  
  # This is the reactive that filters that RWL object.
  # It waits for an event to occur. Here it waits for 
  # updateMasterButton to be invoked with the action button.
  
  filteredRWL <- eventReactive(
    eventExpr = {
      # both these events have to happen to trigger this
      input$updateMasterButton
      getRWL()
    },
    #handlerExpr ={},# The expression to call whenever eventExpr is invalidated.
    valueExpr = {
      # this is what happens if triggered
      req(getRWL())
      if(is.null(input$master) || input$master == ""){
        res <- getRWL()
      }
      else {
        res <- getRWL()[, colnames(getRWL()) %in% input$master]
      }
      rwlRV$dated <- res
      rwlRV$datedVault <- res
    },
    label = "filteredRWL eventReactive")
  
  
  # reactive to run corr.rwl.seg with inputs gathered from the user.
  # this is the workhorse function. We produce a plot and a bunch of
  # tables with it, so it makes sense to do this as a reactive.
  # It's computationally expensive and used in several places.
  
  getCRS <- reactive({
    dat <- rwlRV$dated
    if(input$nCRS=="NULL"){
      n <- NULL
    }
    else{
      n <- as.numeric(input$nCRS)
    }
    # init bin.floor and seg length based on mean series length (or series[1]?).
    crs <- corr.rwl.seg(dat, seg.length = input$seg.lengthCRS, 
                        bin.floor = as.numeric(input$bin.floorCRS),n = n,
                        prewhiten = input$prewhitenCRS, pcrit = input$pcritCRS,
                        biweight = input$biweightCRS, method = input$methodCRS,
                        make.plot=FALSE)
    
    crs
  })
  
  # reactive to run xdate.floater with inputs gathered from the user.
  # It's computationally expensive and used in several places.
  
  getFloater <- reactive({
    master <- filteredRWL()
    series2get <- input$series2
    if(series2get == "bar") {series2get <- 1}
    series2date <- rwlRV$undated[,series2get]
    if(input$nUndated=="NULL"){
      n <- NULL
    }
    else{
      n <- as.numeric(input$nUndated)
    }
    
    res <- xdate.floater(rwl=master, series=series2date, 
                         series.name = input$series2, 
                         min.overlap=input$minOverlapUndated, 
                         n=n, prewhiten = input$prewhitenUndated, 
                         biweight=input$biweightUndated, 
                         method = input$methodUndated,
                         return.rwl = TRUE)
    res
  })
  
  ##############################################################
  #
  # END reactives
  #
  ##############################################################
  
  
  ##############################################################
  #
  # 1st tab 
  #
  ##############################################################
  
  # -- get the RWL report
  output$rwlReport <- renderPrint({
    req(filteredRWL())
    rwl.report(filteredRWL())
  })
  
  # -- plot rwl
  output$rwlPlot <- renderPlot({
    req(filteredRWL())
    plot.rwl(filteredRWL(),plot.type = input$rwlPlotType)
  })
  
  # -- summary
  output$rwlSummary <- renderTable({
    req(filteredRWL())
    summary(filteredRWL())
  })
  # -- make report
  output$rwlSummaryReport <- downloadHandler(
    filename = "rwl_summary_report.html",
    content = function(file) {
      
      tempReport <- file.path(tempdir(), "report_rwl_describe.Rmd")
      file.copy("report_rwl_describe.Rmd", tempReport, overwrite = TRUE)
      
      rwlObject <- rwlRV$dated
      params <- list(fileName = input$file1$name, rwlObject=rwlObject,
                     rwlPlotType=input$rwlPlotType)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app). Defensive
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  ##############################################################
  #
  # 2nd tab 
  #
  ##############################################################
  
  # -- plot the results from corr.rwl.seg
  output$crsPlot <- renderPlot({
    
    req(filteredRWL())
    # need to build plot here by hand I think
    crsObject <- getCRS()
    plot(crsObject)
  })
  
  # make a table of overall correlation by series 
  output$crsOverall <- renderDT({
    
    req(filteredRWL())
    crsObject <- getCRS()
    overallCor <- round(crsObject$overall,3)
    res <- data.frame(Series=rownames(overallCor),
                      Correlation=overallCor[,1])
    datatable(res,rownames = FALSE, 
              caption = "Overall Series Correlation",
              autoHideNavigation=TRUE,
              options = list(pageLength = min(30,nrow(res)),
                             searching=FALSE,
                             lengthChange=FALSE)) %>%
      formatStyle('Correlation', 
                  fontWeight = styleInterval(input$pcritCRS, c('normal', 'bold')))
  })
  
  # -- make a table of the avg correlation by bin
  output$crsAvgCorrBin <- renderDT({
    req(filteredRWL())
    crsObject <- getCRS()
    binNames <- paste(crsObject$bins[,1], "-", crsObject$bins[,2], sep="")
    res <- data.frame(series=binNames,round(crsObject$avg.seg.rho,3))
    colnames(res) <- c("Bin","Correlation")
    datatable(res,rownames = FALSE, 
              caption = "Avg. Correlation by Bin",
              autoHideNavigation=TRUE,
              options = list(pageLength = min(30,nrow(res)),
                             searching=FALSE,
                             lengthChange=FALSE)) %>%
      formatStyle('Correlation', 
                  fontWeight = styleInterval(input$pcritCRS, c('normal', 'bold')))
  })
  
  # -- flags
  output$crsFlags <- renderDT({
    
    req(filteredRWL())
    crsObject <- getCRS()
    flags <- crsObject$flags
    if(length(flags) == 0){res <- NULL}
    else{
      flags <- unlist(flags)
      res <- data.frame(Series=names(flags),
                        Bins=gsub(pattern = "\\.",
                                  replacement = "-",
                                  x = flags))  
    }
    datatable(res,rownames = FALSE, 
              caption = "Flagged Series / Segments",
              autoHideNavigation=TRUE,
              options = list(pageLength = min(30,nrow(res)),
                             searching=FALSE,
                             lengthChange=FALSE))
  })
  
  # -- make a table of the correlation by bin
  output$crsCorrBin <- renderDT({
    
    req(filteredRWL())
    crsObject <- getCRS()
    binNames <- paste(crsObject$bins[,1], "-", crsObject$bins[,2], sep="")
    res <- round(crsObject$spearman.rho,3)
    res <- data.frame(series=rownames(res),res)
    colnames(res) <- c("Series",binNames)
    datatable(res,rownames = FALSE, 
              caption = "Series Correlation by Bin",
              autoHideNavigation=TRUE,
              options = list(pageLength = min(30,nrow(res)),
                             searching=TRUE,
                             lengthChange=FALSE)) %>%
      formatStyle(columns=-1, 
                  fontWeight = styleInterval(input$pcritCRS, c('normal', 'bold')))
  })
  
  # -- report
  output$crsReport <- downloadHandler(
    filename = "rwl_correlation_report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report_rwl_corr.Rmd")
      file.copy("report_rwl_corr.Rmd", tempReport, overwrite = TRUE)
      
      crsObject <- getCRS()
      crsParams <- list(seg.length=input$seg.lengthCRS,
                        bin.floor=input$bin.floorCRS,
                        n=input$nCRS, 
                        prewhiten=input$prewhitenCRS, 
                        pcrit=input$pcritCRS, 
                        biweight=input$biweightCRS,
                        method=input$methodCRS)
      params <- list(fileName = input$file1$name,
                     crsObject = crsObject,
                     crsParams = crsParams)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  ##############################################################
  #
  # 3rd tab 
  #
  ##############################################################
  
  # -- print the flags from last tab
  output$flaggedSeries <- renderText({
    
    req(filteredRWL())
    crsObject <- getCRS()
    flags <- crsObject$flags
    if(length(flags) == 0){
      res <- "There are no flagged series."}
    else{
      flags <- unlist(flags)
      series2get <- names(flags)
      if(length(series2get) == 1){
        res <- paste("Series ", series2get," was flagged.",sep="")  
      }
      if(length(series2get) == 2){
        res <- paste("Series ", paste(series2get,sep=" ",collapse = " and "),
                     " were flagged.",sep="")  
      }
      if(length(series2get) > 2){
        res <- paste("These series were flagged:", 
                     paste(series2get,sep=" ",collapse = " , "),
                     ".",sep="")  
      }
    }
    res
  })
  
  # -- plot the results from corr.series.seg
  output$cssPlot <- renderPlot({
    req(input$series)
    
    req(filteredRWL())
    dat <- rwlRV$dated
    
    if(input$nCSS=="NULL"){
      n <- NULL
    }
    else{
      n <- as.numeric(input$nCSS)
    }
    
    css <- corr.series.seg(dat, series = input$series, seg.length = input$seg.lengthCSS, 
                           bin.floor = as.numeric(input$bin.floorCSS),n = n,
                           prewhiten = input$prewhitenCSS, pcrit = input$pcritCSS,
                           biweight = input$biweightCSS, method = input$methodCSS,
                           make.plot=TRUE)
    
  })
  
  # -- plot the results from ccf.series.rwl
  output$ccfPlot <- renderPlot({
    
    req(filteredRWL())
    req(input$series)
    dat <- rwlRV$dated
    yrs <- time(dat)
    win <- input$rangeCCF[1]:input$rangeCCF[2]
    dat <- dat[yrs %in% win,]
    if(input$nCSS=="NULL"){
      n <- NULL
    }
    else{
      n <- as.numeric(input$nCSS)
    }
    
    ccfObject <- ccf.series.rwl(dat, series = input$series, seg.length = input$seg.lengthCSS, 
                                bin.floor = as.numeric(input$bin.floorCSS),n = n,
                                prewhiten = input$prewhitenCSS, pcrit = input$pcritCSS,
                                biweight = input$biweightCSS, method = input$methodCSS,
                                lag.max = input$lagCCF,make.plot=TRUE)
  })
  
  # -- plot the results from xskel.ccf.plot
  output$xskelPlot <- renderPlot({
    
    req(filteredRWL())
    req(input$series)
    dat <- rwlRV$dated
    
    if(input$nCSS=="NULL"){
      n <- NULL
    }
    else{
      n <- as.numeric(input$nCSS)
    }
    
    wCenter <- input$winCenter - (input$winWidth/2)
    
    xskelObject <- xskel.ccf.plot(dat, series = input$series, 
                                  win.start = wCenter,
                                  win.width = input$winWidth,
                                  n = n,
                                  prewhiten = input$prewhitenCSS,
                                  biweight = input$biweightCSS)
  })
  
  # -- edit series
  
  
  # -- report
  output$cssReport <- downloadHandler(
    filename = "series_correlation_report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report_series.Rmd")
      file.copy("report_series.Rmd", tempReport, overwrite = TRUE)
      
      cssParams <- list(seg.length=input$seg.lengthCSS,
                        bin.floor=input$bin.floorCSS,
                        n=input$nCSS, 
                        prewhiten=input$prewhitenCSS, 
                        pcrit=input$pcritCSS, 
                        biweight=input$biweightCSS,
                        method=input$methodCSS,
                        series=input$series,
                        winCenter=input$winCenter,
                        winWidth=input$winWidth,
                        lagCCF=input$lagCCF,
                        datingNotes=input$datingNotes,
                        winCCF = input$rangeCCF[1]:input$rangeCCF[2])
      params <- list(fileName = input$file1$name,
                     rwlObject = rwlRV$dated,
                     cssParams = cssParams)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  ##############################################################
  #
  # 4th tab 
  #
  ##############################################################
  # delete rows
  observeEvent(input$deleteRows,{
    req(filteredRWL())
    if (!is.null(input$table1_rows_selected)) {
      # get from RV
      seriesDF <-rwlRV$seriesDF
      # delete row      
      row2delIndex <- as.numeric(input$table1_rows_selected)
      # drop the year
      seriesDF <- seriesDF[-row2delIndex,]
      # redo yrs
      n <- nrow(seriesDF)
      
      if(input$deleteRingFixLast){
        newYrs <- seq(seriesDF$Year[2],by=1,length.out = n)
      }
      else{
        newYrs <- seq(seriesDF$Year[1],by=1,length.out = n)
      }
      seriesDF$Year <- newYrs
      
      # insert series back into master
      tmpSeriesDF <- data.frame(x=seriesDF[,2])
      names(tmpSeriesDF)[1] <- input$series
      rownames(tmpSeriesDF) <- seriesDF[,1]
      class(tmpSeriesDF) <- c("rwl", "data.frame")
      tmpDat <- combine.rwl(rwlRV$datedNoSeries,tmpSeriesDF)
      # reorder
      tmpDat <- tmpDat[,names(rwlRV$dated)]
      rwlRV$dated <- tmpDat
      # write an edit log
      rwlRV$editLog <- c(rwlRV$editLog,
                         paste("Series ", input$series, ". ", "Year ", 
                               rwlRV$seriesDF$Year[row2delIndex], " deleted. ",
                               "Last Year Fix = ",input$deleteRingFixLast,
                               sep=""))
      tmpEditDF <- data.frame(series=input$series,
                              year = rwlRV$seriesDF$Year[row2delIndex],
                              value = NA,
                              action = "delete.ring",
                              fixLast = input$deleteRingFixLast)
      rwlRV$editDF <- rbind(rwlRV$editDF,
                            tmpEditDF)
      
    }
  })
  
  # insert rows
  observeEvent(input$insertRows,{
    req(filteredRWL())
    
    if (!is.null(input$table1_rows_selected)) {
      seriesDF <- rwlRV$seriesDF
      n <- nrow(seriesDF)
      # get row index to add
      row2addIndex <- as.numeric(input$table1_rows_selected)
      
      # first part of the df
      seriesDF1 <- seriesDF[1:(row2addIndex-1),]
      # new row
      seriesDF2 <- data.frame(Year=row2addIndex,Value=input$insertValue)
      # second part of the df
      seriesDF3 <- seriesDF[(row2addIndex):n,]
      # combine
      seriesDF <- rbind(seriesDF1,seriesDF2,seriesDF3)
      n <- nrow(seriesDF)
      # redo years
      if(input$insertRingFixLast){
        newYrs <- seq(seriesDF$Year[1]-1,by=1,length.out = n)
      }
      else{
        newYrs <- seq(seriesDF$Year[1],by=1,length.out = n)
      }
      seriesDF$Year <- newYrs
      # insert series back into master
      tmpSeriesDF <- data.frame(x=seriesDF[,2])
      names(tmpSeriesDF)[1] <- input$series
      rownames(tmpSeriesDF) <- seriesDF[,1]
      class(tmpSeriesDF) <- c("rwl", "data.frame")
      tmpDat <- combine.rwl(rwlRV$datedNoSeries,tmpSeriesDF)
      # reorder
      tmpDat <- tmpDat[,names(rwlRV$dated)]
      rwlRV$dated <- tmpDat
      # write an edit log
      rwlRV$editLog <- c(rwlRV$editLog,
                         paste("Series ", input$series, ". ", "Year ", 
                               rwlRV$seriesDF$Year[row2addIndex], " inserted with value ",
                               input$insertValue,
                               ". Last Year Fix = ",input$insertRingFixLast,
                               sep=""))
      tmpEditDF <- data.frame(series=input$series,
                              year = rwlRV$seriesDF$Year[row2addIndex],
                              value = input$insertValue,
                              action = "insert.ring",
                              fixLast = input$insertRingFixLast)
      
      rwlRV$editDF <- rbind(rwlRV$editDF,
                            tmpEditDF)
    }
  })
  
  # revert rows
  observeEvent(input$revertSeries,{
    req(filteredRWL())
    rwlRV$dated <- rwlRV$datedVault
    # write an edit log
    rwlRV$editLog <- "Edits reverted. Log reset."
    rwlRV$editDF <- data.frame(series=NULL,
                               year = NULL,
                               value = NULL,
                               action = NULL,
                               fixLast = NULL)
  })
  
  output$series2edit <- renderText({
    paste("Series", input$series, "selected",sep=" ")
  })
  
  output$table1 <- renderDataTable({
    req(filteredRWL())
    dat <- rwlRV$dated
    datNoSeries <- dat
    datNoSeries[,input$series] <- NULL
    series <- dat[,input$series]
    mask <- is.na(series)
    seriesDF <- data.frame(Year=time(datNoSeries)[!mask],
                           Value=series[!mask])
    
    #write to RV for ease in editing observations
    rwlRV$datedNoSeries <- datNoSeries
    rwlRV$seriesDF <- seriesDF
    
    datatable(rwlRV$seriesDF,
              selection=list(mode="single",target="row"),
              rownames = FALSE, 
              autoHideNavigation=TRUE,
              options = list(pageLength = min(50,nrow(rwlRV$seriesDF)),
                             searching=TRUE,
                             lengthChange=FALSE,
                             columnDefs = list(list(className = 'dt-left', 
                                                    targets = "_all"))))
    
  })
  
  # -- log
  output$editLog <- renderPrint({
    req(filteredRWL())
    if(!is.null(rwlRV$editLog)){
      rwlRV$editLog  
    }
  })
  # -- report
  output$editReport <- downloadHandler(
    filename = "edits_report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report_edits.Rmd")
      file.copy("report_edits.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(fileName = input$file1$name,
                     editLog = rwlRV$editLog,
                     editDF = rwlRV$editDF)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  # -- write rwl file  
  output$downloadRWL <- downloadHandler(
    filename = function() {
      paste(input$file1, "-",Sys.Date(), ".rwl", sep="") 
    },
    content = function(file) {
      write.tucson(rwl.df=rwlRV$dated, fname=file)
    }
  )
  
  ##############################################################
  #
  # 6th tab -- floaters
  #
  ##############################################################
  
  # -- summary  
  output$floaterText <- renderText({
    req(getRWLUndated())
    req(getFloater())
    floaterObject <- getFloater()
    floaterReport <- floaterObject$floaterReport
    floaterCorStats <- floaterObject$floaterCorStats
    rBest <- which.max(floaterCorStats$r)
    firstBest <- floaterCorStats$first[rBest]
    lastBest <- floaterCorStats$last[rBest]
    rBest <- floaterCorStats$r[rBest]
    pBest <- floaterCorStats$p[rBest]
    
    #paste("Original rwl years: ", floaterReport["minTimeRWL"], " to ", 
    #floaterReport["maxTimeRWL"]," (", 
    #floaterReport["nTimeRWL"], ")<br/>",
    #"Detrended rwl years: ", floaterReport["minDetrendedTime"], " to ", 
    #floaterReport["maxDetrendedTime"], " (", 
    #floaterReport["nDetrendedTime"], ")<br/>",
    #"Original series length:", floaterReport["nSeries"], "<br/>",
    #"Detrended series length:", floaterReport["ny"], "<br/>",
    #"Minimum overlap for search:", floaterReport["min.overlap"], "<br/>",
    #"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~", "<br/>",
    paste("Series Name: ", floaterObject$series.name,"<br/>",
          "Years searched ", min(floaterCorStats$first), " to ", max(floaterCorStats$last), "<br/>",
          "Highest overall correlation for series is ", round(rBest,2), 
          " with dates ", firstBest, " to ", lastBest, "<br/>", sep="")
    
  })
  
  # -- plot
  # -- get the RWL report
  output$rwlReport2 <- renderPrint({
    req(getRWLUndated())
    rwl.report(getRWLUndated())
  })
  
  output$floaterPlot <- renderPlot({
    req(getRWLUndated())
    floaterObject <- getFloater()
    plot.floater(floaterObject)
  })
  
  # -- ccf plot
  output$ccfPlotUndated <- renderPlot({
    floaterObject <- getFloater()
    series2get <- floaterObject$series.name
    dat <- floaterObject$rwlCombined
    if(input$nUndated=="NULL"){
      n <- NULL
    }
    else{
      n <- as.numeric(input$nUndated)
    }
    
    ccfObject <- ccf.series.rwl(dat, series = input$series2,
                                seg.length = input$seg.lengthUndated,
                                bin.floor = as.numeric(input$bin.floorUndated),
                                n = n,
                                prewhiten = input$prewhitenUndated,
                                pcrit = input$pcritUndated,
                                biweight = input$biweightUndated,
                                method = input$methodUndated,
                                make.plot=TRUE)
  })
  
  # -- if dates are saved
  
  observeEvent(input$saveDates,{
    req(getRWLUndated())
    req(getFloater())
    floaterObject <- getFloater()
    floaterCorStats <- floaterObject$floaterCorStats
    rBest <- which.max(floaterCorStats$r)
    firstBest <- floaterCorStats$first[rBest]
    lastBest <- floaterCorStats$last[rBest]
    # write the dated series to rwlRV. But first check that
    # rwlRV$undated2dated is NULL.
    # if it is, then write the dated rwl object and a message in the log
    # If it's not null do the same but only if the user hasn't already tried
    # to save those dates already.
    
    if(is.null(rwlRV$undated2dated)){
      rwlRV$dateLog <- c(rwlRV$dateLog,
                         paste("Series ", floaterObject$series.name,
                               " saved with dates ", firstBest, " to ", 
                               lastBest, ".", sep=""))
      rwlRV$undated2dated <- floaterObject$rwlOut  
    }
    else{
      # it's easy for the user to save dates twice I think.
      # so this is a check that makes that harmless and warns them
      if(input$series2 %in% names(rwlRV$undated2dated)){
        rwlRV$dateLog <- c(rwlRV$dateLog,
                           paste("Series ", floaterObject$series.name,
                                 " was already saved.",sep=""))
      }
      else{
        rwlRV$dateLog <- c(rwlRV$dateLog,
                           paste("Series ", floaterObject$series.name,
                                 " saved with dates ", firstBest, " to ", 
                                 lastBest, ".", sep=""))
        rwlRV$undated2dated <- combine.rwl(rwlRV$undated2dated,
                                           floaterObject$rwlOut)
      }
    }
  })
  
  # revert dates
  observeEvent(input$removeDates,{
    req(getRWLUndated())
    req(getFloater())
    rwlRV$undated2dated <- NULL
    # write an edit log
    rwlRV$dateLog <- "Dates removed for all undated series. Log reset."
  })
  
  # -- log
  output$dateLog <- renderPrint({
    req(getRWLUndated())
    req(getFloater())
    if(!is.null(rwlRV$dateLog)){
      rwlRV$dateLog  
    }
  })
  
  # -- write rwl
  # figure out how to grey out if rwlRV$undated2dated is NULL?
  output$downloadUndatedRWL <- downloadHandler(
    filename = function() {
      paste(input$file2, "-",Sys.Date(), ".rwl", sep="") 
    },
    content = function(file) {
      if(input$appendMaster){
        tmp <- combine.rwl(rwlRV$undated2dated,filteredRWL())
      }
      else{
        tmp <- rwlRV$undated2dated
      }
      write.tucson(rwl.df=tmp, fname=file)
    }
  )
  
  
  # -- report  
  output$undatedReport <- downloadHandler(
    filename = "undated_series_report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report_undated_series.Rmd")
      file.copy("report_undated_series.Rmd", tempReport, overwrite = TRUE)
      
      undatedParams <- list(seg.length=input$seg.lengthUndated,
                            bin.floor=input$bin.floorUndated,
                            n=input$nUndated, 
                            prewhiten=input$prewhitenUndated, 
                            pcrit=input$pcritUndated, 
                            biweight=input$biweightUndated,
                            method=input$methodUndated,
                            series=input$series2,
                            datingNotes=input$undatingNotes,
                            dateLog = rwlRV$dateLog)
      params <- list(fileName1 = input$file1ci$name,
                     fileName2 = input$file2$name,
                     floaterObject = getFloater(),
                     undatedParams = undatedParams)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
})

