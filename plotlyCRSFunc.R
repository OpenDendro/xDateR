# this is a freaking hell function. Because plotly doesn't allow layering of tiles
# with diff color schemes, I decided to hand build tiles which was a terrible experience.

crsPlotly <- function(x){
  yr.range <- function(x, yr.vec = as.numeric(names(x))) {
    na.flag <- is.na(x)
    if (all(na.flag)) {
      res <- rep(NA, 2)
      mode(res) <- mode(yr.vec)
      res
    } else {
      range(yr.vec[!na.flag])
    }
  }
  
  rho <- x$spearman.rho
  bins <- x$bins
  pval <- x$p.val
  yrs <- as.numeric(rownames((x$rwi)))
  min.yr <- min(yrs)
  max.yr <- max(yrs)
  
  
  nseries <- nrow(rho)
  nbins <- nrow(bins)
  nyrs <- length(yrs)
  seq.series <- seq_len(nseries)
  
  seg.length <- x$seg.length
  seg.lag <- x$seg.lag
  pcrit <- x$pcrit
  
  seriesStartStop <- t(apply(x$rwi, 2, yr.range, yr.vec = yrs))
  
  # order series by start date
  rsult <- sort.int(seriesStartStop[,1 ], decreasing = FALSE, 
                    index.return = TRUE)
  neworder <- rsult$ix
  
  # reorder starts stops
  seriesStartStop <- seriesStartStop[neworder, ]
  seriesStartStopDF <- data.frame(seriesOrder=1:nseries,
                                  seriesName=rownames(seriesStartStop),
                                  start=seriesStartStop[,1],
                                  end=seriesStartStop[,2])
  
  #reorder rho
  rho <- rho[neworder, , drop = FALSE]
  #reorder pval
  pval <- pval[neworder, , drop = FALSE]
  
  
  dat <- data.frame(seriesOrder = seq(from = 1, to = nseries),
                    seriesName = rownames(seriesStartStop))
  dat$xMin <- seriesStartStop[,1]
  dat$xMax <- seriesStartStop[,2]
  dat$xMinBnd <- min(yrs)
  dat$xMaxBnd <- max(yrs)
  
  bottomRho <- rho[,seq(1,ncol(rho),by=2)]
  bottomRho <- data.frame(dat[,1:2],bottomRho)
  bottomCenterBins <- rowMeans(bins[seq(1,ncol(rho),by=2),])
  names(bottomRho)[-c(1:2)] <- bottomCenterBins
  
  bottomRhoLong <- bottomRho %>% 
    pivot_longer(cols=-c(1:2),names_to = "binCenter", values_to = "rho") %>%
    mutate(binCenter = as.numeric(binCenter)) %>% 
    mutate(width = seg.length) %>% drop_na()
  
  # now pvals
  bottomPval <- pval[,seq(1,ncol(pval),by=2)]
  bottomPval <- data.frame(dat[,1:2],bottomPval)
  bottomCenterBins <- rowMeans(bins[seq(1,ncol(pval),by=2),])
  names(bottomPval)[-c(1:2)] <- bottomCenterBins
  
  bottomPvalLong <- bottomPval %>% 
    pivot_longer(cols=-c(1:2),names_to = "binCenter", values_to = "pval") %>%
    mutate(binCenter = as.numeric(binCenter)) %>% 
    drop_na()
  
  # add Pval to bottomRhoLong
  bottomRhoLong$pval <- bottomPvalLong$pval
  
  # tops 
  topRho <- rho[,seq(2,ncol(rho),by=2)]
  topCenterBins <- rowMeans(bins[seq(2,ncol(rho),by=2),])
  topRho <- data.frame(dat[,1:2],topRho)
  names(topRho)[-c(1:2)] <- topCenterBins
  
  topRhoLong <- topRho %>% 
    pivot_longer(cols=-c(1:2),names_to = "binCenter", values_to = "rho") %>%
    mutate(binCenter = as.numeric(binCenter)) %>% 
    mutate(width = seg.length) %>% drop_na()
  
  # now pvals
  topPval <- pval[,seq(2,ncol(pval),by=2)]
  topPval <- data.frame(dat[,1:2],topPval)
  topCenterBins <- rowMeans(bins[seq(2,ncol(pval),by=2),])
  names(topPval)[-c(1:2)] <- topCenterBins
  
  topPvalLong <- topPval %>% 
    pivot_longer(cols=-c(1:2),names_to = "binCenter", values_to = "pval") %>%
    mutate(binCenter = as.numeric(binCenter)) %>% 
    drop_na()
  
  # add Pval to bottomRhoLong
  topRhoLong$pval <- topPvalLong$pval
  
  # for bottoms
  # calc n years from first year to first bin and set to NA
  # balance all the center years and so on.
  # what a PITA
  firstYrs <- dat %>% 
    select(seriesOrder,seriesName,xMin) 
  
  firstBinBottom <- inner_join(bottomRhoLong,firstYrs) %>% 
    group_by(seriesName) %>% 
    slice(which.min(binCenter)) %>%
    mutate(binEdge = binCenter - width/2) %>%
    mutate(width = binEdge - xMin) %>%
    mutate(binCenter = xMin + width/2) %>%
    select(-xMin,-binEdge)
  firstBinBottom$rho <- NA
  firstBinBottom$pval <- NA
  
  
  # calc n years from last year to last bin and set to NA
  # balance all the center years and so on.
  # what a PITA
  lastYrs <- dat %>% 
    select(seriesOrder,seriesName,xMax) 
  
  lastBinBottom <- inner_join(bottomRhoLong,lastYrs) %>% 
    group_by(seriesName) %>% 
    slice(which.max(binCenter)) %>%
    mutate(binEdge = binCenter + width/2) %>%
    mutate(width = xMax - binEdge) %>%
    mutate(binCenter = xMax - width/2) %>%
    select(-xMax,-binEdge)
  lastBinBottom$rho <- NA
  lastBinBottom$pval <- NA
  
  bottomDat <- bind_rows(bottomRhoLong,firstBinBottom,lastBinBottom)
  
  # for tops
  firstBinTop <- inner_join(topRhoLong,firstYrs) %>% 
    group_by(seriesName) %>% 
    slice(which.min(binCenter)) %>%
    mutate(binEdge = binCenter - width/2) %>%
    mutate(width = binEdge - xMin) %>%
    mutate(binCenter = xMin + width/2) %>%
    select(-xMin,-binEdge)
  firstBinTop$rho <- NA
  firstBinTop$pval <- NA
  
  # calc n years from last year to last bin and set to NA
  lastYrs <- dat %>% 
    select(seriesOrder,seriesName,xMax) 
  
  lastBinTop <- inner_join(topRhoLong,lastYrs) %>% 
    group_by(seriesName) %>% 
    slice(which.max(binCenter)) %>%
    mutate(binEdge = binCenter + width/2) %>%
    mutate(width = xMax - binEdge) %>%
    mutate(binCenter = xMax - width/2) %>%
    select(-xMax,-binEdge)
  lastBinTop$rho <- NA
  lastBinTop$pval <- NA
  
  topDat <- bind_rows(topRhoLong,firstBinTop,lastBinTop)
  
  tail(topDat)
  
  # rejoin top and bottom to get factors the same between the two
  topDat$courseAdj <- 0.25
  bottomDat$courseAdj <- 0.75
  dat2 <- bind_rows(bottomDat,topDat)
  
  # make labels prettier for rho
  breaksRho <- c(-Inf,seq(0,1,by=0.1))
  dat2 <- dat2 %>% mutate(rhoFac = cut(x=rho,breaks=breaksRho,dig.lab=1)) 
  dat2 <-  dat2 %>% mutate(rhoFac = case_when(pval>pcrit ~ "NS",
                                              is.na(pval) ~ "Incomplete",
                                              TRUE ~ as.character(rhoFac)))
  
  # check for NS flags
  if(any(dat2$pval>pcrit,na.rm = TRUE)){
    n <- length(unique(dat2$rhoFac))
    fillPal <- colorRampPalette(c("lightblue", "darkblue"))( n-2 ) 
    # add inc and NS
    fillPal <- c(fillPal,"#90EE90","#ffcccb")
  } else{
    n <- length(unique(dat2$rhoFac))
    fillPal <- colorRampPalette(c("lightblue", "darkblue"))( n-1 ) 
    # add inc and NS
    fillPal <- c(fillPal,"#90EE90")
  }
  
  dat2$rhoFac <- as.factor(dat2$rhoFac)
  
  p1 <- ggplot() + 
    # bins bottom
    geom_tile(data = dat2,
              aes(x = binCenter, y = seriesOrder - courseAdj, 
                  width = width,fill=rhoFac),
              height = 0.5,color="white") +
    scale_fill_manual(name = "Correlation",
                      values=fillPal,na.value="grey80") +
    
    # series marks
    geom_hline(yintercept = 0:nseries,col="white") + # size=1?
    # bin marks
    geom_vline(xintercept =  c(bins[, 1], bins[c(nbins - 1, nbins), 2] + 1), 
               color = "grey", linetype = "dotted") +
    # finish up
    scale_x_continuous(name = element_blank(),expand = c(0,10)) +
    scale_y_continuous(name = element_blank(),expand = c(0,0.1),
                       breaks = seq(1,nseries,by=1)-0.5, 
                       labels = dat$seriesName[seq(1,nseries,by=1)]) +
    # no joy in plotly
    # sec.axis = dup_axis(breaks=seq(2,nseries,by=2)-0.5,
    #                     labels=dat$seriesName[seq(2,nseries,by=2)])) +
    theme_minimal()
  
  html_plot <- ggplotly(p1,tooltip="fill")
  html_plot
}
