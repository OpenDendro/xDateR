library(shiny)
library(rmarkdown)
library(dplR)
library(DT)

# Define server logic
shinyServer(function(session, input, output) {
  
  ##############################################################
  # START reactives
  #
  # we use reactives for two reasons (at least). The first is
  # so that calculations (like corr.rwl.seg)
  # need to be done only once. The output can get passed between 
  # tabs and code chunks. The second reason is so user input can 
  # be gathered from the UI dynamically and passed. 
  # E.g., selecting series. These dynamic things are obervations 
  # and actions. At moment, I'm pretty confised about terminolgy
  #
  ##############################################################
  
  # this is an object (empty now) that will hold the rwl data
  # (and a backup) that can be edited and passed around
  rwlRV <- reactiveValues()
  rwlRV$edits <- NULL
  
  # This is a reactive to get the RWL file from the user at the start.
  getRWL <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }
    else{
      dat <- read.rwl(inFile$datapath)
    }
    dat
  })
  
  # This observes the input RWL and gets the names of the series. These names 
  # get passed to the checkboxes for possible filtering later (when the action button
  # is pressed).
  observeEvent(
    eventExpr = getRWL(), 
    handlerExpr = {
      updateCheckboxGroupInput(session = session, inline = TRUE,
                               inputId = "master",
                               choices=colnames(getRWL()),
                               selected=colnames(getRWL()))
    },
    label = "passes the col names to the UI for the master filtering I think")
  
  # This is the reactive that filters that RWL object.
  # It waits for an event to occur. Here it waits for 
  # updateMasterButton to be invoked with the action button. I think. And then it
  # filters the RWL
  
  filteredRWL <- eventReactive(
    eventExpr = {
      # both these events have to happen to trigger this
      input$updateMasterButton
      getRWL()
    },
    #handlerExpr ={},# not needed unlesd something is invalidated? Maybe
    valueExpr = {
      # this is what happens if triggered
      req(getRWL())
      if(is.null(input$master) || input$master == ""){
        res <- getRWL()
      }
      else {
        res <- getRWL()[, colnames(getRWL()) %in% input$master]
      }
      rwlRV$dat <- res
      rwlRV$datVault <- res
    },
    label = "filteredRWL eventReactive")
  
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
                        choices=colnames(rwlRV$dat),
                        selected=colnames(rwlRV$dat)[1])
    },
    label = "observe series being selected and update the input box")
  
  
  # observe which series gets selected for the individual series correlation
  # get the start and end dates for the series. These are used to
  # update the window slider for the xskel plot
  observeEvent(
    eventExpr = {
      # this is what triggers the observation
      input$series
      filteredRWL()
    }, 
    handlerExpr = {
      dat <- rwlRV$dat
      tmp <- summary(dat)
      winBnds <- as.numeric(tmp[tmp$series==input$series,2:3])
      minWin <- round(winBnds[1] + input$lagCCF,-1)
      maxWin <- round(winBnds[2] - input$lagCCF,-1)
      updateSliderInput(session = session,
                        inputId = "winCenter",
                        value=round(mean(winBnds),-1),
                        min=minWin + 20,
                        max=maxWin - 50,
                        step=5)
      
      updateSliderInput(session = session,
                        inputId = "rangeCCF",
                        value=c(minWin,
                                 maxWin),
                        min=minWin,
                        max=maxWin,
                        step=5)
    },
    label = "observe series being selected and update the window slider")
  
  # deleting and adding rings goes here


  
  # back to buisiness
  
  # reactive to run corr.rwl.seg with inputs gathered from the user.
  # this is the workhorse function. We produce a plot and a bunch of
  # tables with it, so it makes sense to do this as a reactive.
  
  getCRS <- reactive({
    dat <- rwlRV$dat
    if(input$nCRS=="NULL"){
      n <- NULL
    }
    else{
      n <- as.numeric(input$nCRS)
    }
    crs <- corr.rwl.seg(dat, seg.length = input$seg.lengthCRS, 
                        bin.floor = as.numeric(input$bin.floorCRS),n = n,
                        prewhiten = input$prewhitenCRS, pcrit = input$pcritCRS,
                        biweight = input$biweightCRS, method = input$methodCRS,
                        make.plot=FALSE)
    
    crs
  })
  
  
  ##############################################################
  #
  # END reactives
  #
  ##############################################################
  
  
  ##############################################################
  #
  # 1st tab -- get the RWL file
  #
  ##############################################################
  
  output$rwlReport <- renderPrint({
    req(input$file1)
    rwl.report(filteredRWL())
  })
  
  output$rwlPlot <- renderPlot({
    req(input$file1)
    plot.rwl(filteredRWL(),plot.type = input$rwlPlotType)
  })
  
  output$rwlSummary <- renderTable({
    req(input$file1)
    summary(filteredRWL())
  })
  ##############################################################
  #
  # 1st tab -- report
  #
  ##############################################################
  output$rwlSummaryReport <- downloadHandler(
    filename = "rwl_summary_report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_rwl_describe.Rmd")
      file.copy("report_rwl_describe.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      rwlObject <- rwlRV$dat
      params <- list(fileName = input$file1$name, rwlObject=rwlObject,
                     rwlPlotType=input$rwlPlotType)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  ##############################################################
  #
  # 2nd tab -- plot the results from corr.rwl.seg
  #
  ##############################################################
  output$crsPlot <- renderPlot({
    req(input$file1)
    # need to build plot here by hand I think
    crsObject <- getCRS()
    plot(crsObject)
  })
  
  ##############################################################
  #
  # 2nd tab -- make a table of overall correlation by series
  #
  ##############################################################
  output$crsOverall <- renderDT({
    req(input$file1)
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
  
  ##############################################################
  #
  # 2nd tab -- make a table of the avg correlation by bin
  #
  ##############################################################
  output$crsAvgCorrBin <- renderDT({
    req(input$file1)
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
  
  ##############################################################
  #
  # 2nd tab -- flags
  #
  ##############################################################
  output$crsFlags <- renderDT({
    req(input$file1)
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
  
  ##############################################################
  #
  # 2nd tab -- make a table of the correlation by bin
  #
  ##############################################################
  output$crsCorrBin <- renderDT({
    req(input$file1)
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
  
  ##############################################################
  #
  # 2nd tab -- report
  #
  ##############################################################
  output$crsReport <- downloadHandler(
    # For PDF output, change this to ".pdf"
    filename = "rwl_correlation_report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_rwl_corr.Rmd")
      file.copy("report_rwl_corr.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
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
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  ##############################################################
  #
  # 3rd tab -- print the flags from last tab
  #
  ##############################################################
  
  output$flaggedSeries <- renderText({
    req(input$file1)
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
  
  ##############################################################
  #
  # 3rd tab -- plot the results from corr.series.seg
  #
  ##############################################################
  output$cssPlot <- renderPlot({
    req(input$series)
    req(input$file1)
    dat <- rwlRV$dat
    
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
  
  ##############################################################
  #
  # 3rd tab -- plot the results from ccf.series.rwl
  #
  ##############################################################
  output$ccfPlot <- renderPlot({
    req(input$file1)
    req(input$series)
    dat <- rwlRV$dat
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
  
  ##############################################################
  #
  # 3rd tab -- plot the results from xskel.ccf.plot
  #
  ##############################################################
  output$xskelPlot <- renderPlot({
    req(input$file1)
    req(input$series)
    dat <- rwlRV$dat
    
    if(input$nCSS=="NULL"){
      n <- NULL
    }
    else{
      n <- as.numeric(input$nCSS)
    }
    
    wCenter <- input$winCenter - (input$winWidth/2)
    
    xskeObject <- xskel.ccf.plot(dat, series = input$series, 
                                 win.start = wCenter,
                                 win.width = input$winWidth,
                                 n = n,
                                 prewhiten = input$prewhitenCSS,
                                 biweight = input$biweightCSS)
  })
  
  ##############################################################
  #
  # 3rd tab -- edit series
  #
  ##############################################################
  
  
  ##############################################################
  #
  # 3rd tab -- report
  #
  ##############################################################
  output$cssReport <- downloadHandler(
    # For PDF output, change this to ".pdf"
    filename = "series_correlation_report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_series.Rmd")
      file.copy("report_series.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
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
                     rwlObject = rwlRV$dat,
                     cssParams = cssParams)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  ##############################################################
  #
  # 4th tab -- edit
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
      tmpDat <- combine.rwl(rwlRV$datNoSeries,tmpSeriesDF)
      # reorder
      tmpDat <- tmpDat[,names(rwlRV$dat)]
      rwlRV$dat <- tmpDat
      # write an edit log
      rwlRV$edits <- c(rwlRV$edits,
                       paste("Series ", input$series, ". ", "Year ", 
                             rwlRV$seriesDF$Year[row2delIndex], " deleted. ",
                             "Last Year Fix = ",input$deleteRingFixLast,
                             sep=""))
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
      tmpDat <- combine.rwl(rwlRV$datNoSeries,tmpSeriesDF)
      # reorder
      tmpDat <- tmpDat[,names(rwlRV$dat)]
      rwlRV$dat <- tmpDat
      # write an edit log
      rwlRV$edits <- c(rwlRV$edits,
                       paste("Series ", input$series, ". ", "Year ", 
                             rwlRV$seriesDF$Year[row2addIndex], " inserted with value ",
                             input$insertValue,
                             ". Last Year Fix = ",input$insertRingFixLast,
                             sep=""))
    }
  })
  
  # revert rows
  observeEvent(input$revertSeries,{
    req(filteredRWL())
    rwlRV$dat <- rwlRV$datVault
    # write an edit log
    rwlRV$edits <- "Edits reverted. Log reset."
  })
  
  output$series2edit <- renderText({
    paste("Series", input$series, "selected",sep=" ")
  })
  
  output$table1 <- renderDataTable({
    req(filteredRWL())
    dat <- rwlRV$dat
    datNoSeries <- dat
    datNoSeries[,input$series] <- NULL
    series <- dat[,input$series]
    mask <- is.na(series)
    seriesDF <- data.frame(Year=time(datNoSeries)[!mask],
                           Value=series[!mask])
    
    #write to RV fo ease in editing observations
    rwlRV$datNoSeries <- datNoSeries
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
  
  ##############################################################
  #
  # 4th tab -- log
  #
  ##############################################################
  output$editLog <- renderPrint({
    req(input$file1)
    if(!is.null(rwlRV$edits)){
      rwlRV$edits  
    }
  })
  
  output$editReport <- downloadHandler(
    # For PDF output, change this to ".pdf"
    filename = "edits_report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_edits.Rmd")
      file.copy("report_edits.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(fileName = input$file1$name,
                     rwlObject = rwlRV$dat,
                     edits = rwlRV$edits)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  ##############################################################
  #
  # 4th tab -- save stuff
  #
  ##############################################################
  
  output$downloadRWL <- downloadHandler(
    filename = function() {
      paste(input$file1, "-",Sys.Date(), ".rwl", sep="") 
    },
    content = function(file) {
      write.tucson(rwl.df=rwlRV$dat, fname=file)
    }
  )
  
  
})

