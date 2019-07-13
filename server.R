library(shiny)
library(rmarkdown)
library(dplR)
library(DT)

# Define server logic
shinyServer(function(session, input, output) {
  
  ##############################################################
  # START reactives
  #
  # we use reactives so that calculations (like corr.rwl.seg)
  # need to be done once. the output can get passed between tabs
  # and code chunks.
  #
  ##############################################################
  
  # reactive to get the RWL file from the user at the start.
  # nothing will happen until this is done
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
  
  # Because we want the user to be able to dynamically filter the series that
  # are included this is the start of a tedious path to make checkboxes that will
  # update the series included in the crs plot.
  
  # The logic is, I think that we need to update what getRWL() produces and 
  # respond to and event by oberserving it. Thisis difficult because we want the 
  # colnames of the file that was read in to be displayed. Because that comes from the 
  # getRWL() reactive, this  process is tedious.
  
  filteredRWL <- eventReactive(eventExpr = {
    input$updateMaster
    getRWL()
  },
  #handlerExpr ={},# not needed unlesd something is invalidated? Maybe
  valueExpr = {
    req(getRWL())
    if(is.null(input$master) || input$master == "")
      getRWL() else 
        getRWL()[, colnames(getRWL()) %in% input$master]
  },label = "where the getRWL gets filtered")
  
  observeEvent(eventExpr = getRWL(), 
               handlerExpr = {
                 updateCheckboxGroupInput(session = session, inline = TRUE,
                                          inputId = "master",
                                          choices=colnames(getRWL()),
                                          selected=colnames(getRWL()))
               },label = "passes the col names to the UI for the master filtering I think")
  
  observeEvent(eventExpr = {
    filteredRWL()
  }, 
  handlerExpr = {
    updateSelectInput(session = session,
                      inputId = "series",
                      choices=colnames(filteredRWL()),
                      selected=colnames(filteredRWL())[1])
  },label = "passes the col names to the UI for series selection I think")
  
  observeEvent(eventExpr = {
    # this is what triggers the observation
    input$series
    #filteredRWL()
  }, 
  handlerExpr = {
    dat <- filteredRWL()
    tmp <- summary(dat)
    winInit <- as.numeric(tmp[tmp$series==input$series,2:3])
    
    updateSliderInput(session = session,
                      inputId = "winCenter",
                      value=round(mean(winInit),-1),
                      min=round(winInit[1]+5,-1) + 50,
                      max=round(winInit[2],-1) - 50,
                      step=10)
  },label = "passes the yearsto the UI for window selection I think")
  # back to buisiness
  
  # reactive to run corr.rwl.seg with inputs gathered from the user.
  # this is the workhorse function
  getCRS <- reactive({
    dat <- getRWL()
    
    # Get user input about n for hanning
    # (btw, it is stupid to have to do this. hanning isn't
    # widely used)
    if(input$nCRS=="NULL"){
      n <- NULL
    }
    else{
      n <- as.numeric(input$nCRS)
    }
    
    # run corr.rwl.seg.
    # note inputs that are passed in via inputs$ which comes from the
    # UI side
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
    rwl.report(getRWL())
  })
  
  output$rwlPlot <- renderPlot({
    req(input$file1)
    plot.rwl(getRWL(),plot.type = input$rwlPlotType)
  })
  
  output$rwlSummary <- renderTable({
    req(input$file1)
    summary(getRWL())
  })
  ##############################################################
  #
  # 1st tab -- report
  #
  ##############################################################
  output$rwlSummaryReport <- downloadHandler(
    filename = "rwlSummaryReport.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "rwlOutputReport.Rmd")
      file.copy("reportRmd/rwlOutputReport.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      rwlObject <- getRWL()
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
    filename = "crsReport.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "crsOutputReport.Rmd")
      file.copy("reportRmd/crsOutputReport.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      crsObject <- getCRS()
      crsParams <- list(seg.length=input$seg.length,
                        bin.floor=input$bin.floor,
                        n=input$n, 
                        prewhiten=input$prewhiten, 
                        pcrit=input$pcrit, 
                        biweight=input$biweight,
                        method=input$method)
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
    dat <- filteredRWL()
    
    # Get user input about n for hanning
    # (btw, it is stupid to have to do this. hanning isn't
    # widely used)
    if(input$nCSS=="NULL"){
      n <- NULL
    }
    else{
      n <- as.numeric(input$nCSS)
    }
    
    # run corr.series.seg.
    # note inputs that are passed in via inputs$ which comes from the
    # UI side
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
    req(input$series)
    dat <- filteredRWL()
    
    # Get user input about n for hanning
    # (btw, it is stupid to have to do this. hanning isn't
    # widely used)
    if(input$nCSS=="NULL"){
      n <- NULL
    }
    else{
      n <- as.numeric(input$nCSS)
    }
    
    # run corr.series.seg.
    # note inputs that are passed in via inputs$ which comes from the
    # UI side
    ccfObject <- ccf.series.rwl(dat, series = input$series, seg.length = input$seg.lengthCSS, 
                                bin.floor = as.numeric(input$bin.floorCSS),n = n,
                                prewhiten = input$prewhitenCSS, pcrit = input$pcritCSS,
                                biweight = input$biweightCSS, method = input$methodCSS,
                                lag.max = input$lagCCF,make.plot=TRUE)
    
    #ccfObject
    
  })
  
  ##############################################################
  #
  # 3rd tab -- plot the results from xskel.ccf.plot
  #
  ##############################################################
  output$xskelPlot <- renderPlot({
    req(input$series)
    dat <- filteredRWL()
    
    # Get user input about n for hanning
    # (btw, it is stupid to have to do this. hanning isn't
    # widely used)
    if(input$nCSS=="NULL"){
      n <- NULL
    }
    else{
      n <- as.numeric(input$nCSS)
    }
    
    wCenter <- input$winCenter - (input$winWidth/2)
    # run corr.series.seg.
    # note inputs that are passed in via inputs$ which comes from the
    # UI side
    xskeObject <- xskel.ccf.plot(dat, series = input$series, 
                                 win.start = wCenter,
                                 win.width = input$winWidth,
                                 n = n,
                                 prewhiten = input$prewhitenCSS,
                                 biweight = input$biweightCSS)
    
  })
  
  ##############################################################
  #
  # 3rd tab -- report
  #
  ##############################################################
  output$cssReport <- downloadHandler(
    # For PDF output, change this to ".pdf"
    filename = "cssReport.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "cssOutputReport.Rmd")
      file.copy("reportRmd/cssOutputReport.Rmd", tempReport, overwrite = TRUE)
      
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
                        lagCCF=input$lagCCF)
      params <- list(fileName = input$file1$name,
                     rwlObject = filteredRWL(),
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
  
})

