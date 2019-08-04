xdate.floater <- function(rwl, series, series.name = "Unk", min.overlap=50, n=NULL,
                          prewhiten = TRUE, biweight=TRUE,
                          method = c("spearman", "pearson", "kendall"),
                          return.rwl = FALSE) {
  
  method2 <- match.arg(method)
  # Trim series in case it has NA (e.g., submitted stright from the rwl)
  idx.good <- !is.na(series)
  series <- series[idx.good]
  nSeries <- length(series)
  ## turn off warnings for this function
  ## The sig test for spearman's rho often produces warnings.
  w <- options(warn = -1)
  on.exit(options(w))
  
  
  ## Normalize
  tmp <- dplR:::normalize.xdate(rwl, series, n, prewhiten, biweight)
  master <- tmp$master
  
  series2 <- tmp$series
  idx.good <- !is.na(series2)
  series2 <- series2[idx.good]
  
  
  ## trim master so there are no NaN like dividing when
  ## only one series for instance.
  idx.good <- !is.nan(master)
  x <- master[idx.good]
  
  yrs <- as.numeric(names(x))
  y <- series2
  
  nx <- length(x)
  ny <- length(y)
  
  if(min.overlap > ny) {stop("min.overlap must be less than series length after detrending")}
  
  minYrsOut <- numeric()
  maxYrsOut <- numeric()
  rOut <- numeric()
  pOut <- numeric()
  nOut <- numeric()
  # need to crawl through backwards because, the start years on both the master 
  # and the series can be impacted by the nomalizing (e.g., hanning, prewhiten). 
  # The ends can't be. So crawl through backwards and calc dates that way
  crawl <- (nx+(ny-min.overlap)):(min.overlap)
  edgeCounter <- 0
  for(i in crawl){
    if(i > nx){
      xInd <- (i-ny+1):nx
      yInd <- 1:(ny-(i-nx))
      tmp <- cor.test(x[xInd],y[yInd], method = method2,alternative = "greater")
      rOut[i] <- tmp$estimate
      pOut[i] <- tmp$p.val
      # the dating here is weird. The end date is going to be the max of xInd plus the overlap off the edge.
      maxYrsOut[i] <- max(yrs[xInd]) + ny - min.overlap + edgeCounter
      edgeCounter <- edgeCounter - 1
      minYrsOut[i] <- maxYrsOut[i] - nSeries + 1
      nOut[i] <- length(x[xInd])
    }
    if(i >= ny & i <= nx){
      xInd <- (i-ny+1):i
      tmp <- cor.test(x[xInd],y, method = method2,alternative = "greater")
      rOut[i] <- tmp$estimate
      pOut[i] <- tmp$p.val
      maxYrsOut[i] <- max(yrs[xInd])
      # the end date is right, so subtract the original series length to get start date
      minYrsOut[i] <- max(yrs[xInd]) - nSeries + 1
      nOut[i] <- length(x[xInd])
    }
    if(i < ny){
      xInd <- 1:i
      yInd <- xInd + ny-length(xInd)
      tmp <- cor.test(x[xInd],y[yInd], method = method2,alternative = "greater")
      rOut[i] <- tmp$estimate
      pOut[i] <- tmp$p.val
      maxYrsOut[i] <- max(yrs[xInd])
      # the end date is right, so subtract the original series length to get start date
      minYrsOut[i] <- max(yrs[xInd]) - nSeries + 1
      nOut[i] <- length(x[xInd])
    }
  }
  
  # write an output object
  floaterCorStats <- data.frame(minYrsOut,maxYrsOut,rOut,pOut,nOut)
  names(floaterCorStats) <- c("first","last","r","p","n")
  mask <- rowSums(is.na(floaterCorStats))==0
  floaterCorStats <- floaterCorStats[mask,]
  
  # data for output report
  floaterReport <- c(minTimeRWL = min(time(rwl)),
                     maxTimeRWL = max(time(rwl)),
                     nTimeRWL = length(time(rwl)),
                     minDetrendedTime = min(yrs),
                     maxDetrendedTime = max(yrs),
                     nDetrendedTime = length(yrs),
                     nSeries = nSeries,
                     ny = ny,
                     min.overlap = min.overlap)
  
  res <- list(series.name,floaterCorStats,floaterReport)
  names(res) <- c("series.name","floaterCorStats","floaterReport")
  # best cor -- make rwl
  rBest <- which.max(floaterCorStats$r)
  firstBest <- floaterCorStats$first[rBest]
  lastBest <- floaterCorStats$last[rBest]
  #rBest <- floaterCorStats$r[rBest]
  #pBest <- floaterCorStats$p[rBest]
  names(series) <- firstBest:lastBest
  rwlOut <- as.rwl(data.frame(series))
  names(rwlOut) <- series.name 
  class(rwlOut) <- c("rwl", "data.frame")
  # add to new rwl
  rwlCombined <- combine.rwl(rwl,rwlOut)
  if(return.rwl){
    res <- list(series.name,floaterCorStats,floaterReport,rwlCombined, rwlOut)
    names(res) <- c("series.name","floaterCorStats","floaterReport",
                    "rwlCombined", "rwlOut")
  }
  res
}

# floaterReport <- function(x){
#   floaterReport <- x$floaterReport
#   print(floaterReport)
#   cat("Original rwl years: ", floaterReport["minTimeRWL"], " to ", 
#       floaterReport["maxTimeRWL"]," (", 
#       floaterReport["nTimeRWL"], ")\n",sep="")
#   cat("Detrended rwl years: ", floaterReport["minDetrendedTime"], " to ", 
#       floaterReport["maxDetrendedTime"], " (", 
#       floaterReport["nDetrendedTime"], ")\n",sep="")
#   cat("Original series length:", floaterReport["nSeries"], "\n")
#   cat("Detrended series length:", floaterReport["ny"], "\n")
#   cat("Minimum overlap for search:", floaterReport["min.overlap"], "\n")
#  cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~", "\n")
#  cat("Years searched:", min(res$first), "to", max(res$last), "\n")
#  cat("Highest overall correlation for series is", round(rBest,2), "with dates", firstBest, "to", lastBest, "\n")
#}


plot.floater <- function(x){
  series.name <- x$series.name
  floaterCorStats <- x$floaterCorStats
  rwlCombined <- x$rwlCombined
  
  
  rwlOrig <- rwlCombined
  rwlOrig[,series.name] <- NULL
  quantCor <- quantile(interseries.cor(rwlOrig)[,1],probs = c(0.05,0.5,0.95))
  
  
  op <- par(no.readonly=TRUE) # Save par
  on.exit(par(op))            # Reset par on exit
  
  # plot 1 -- seg plot with new series inserted
  yr <- as.numeric(row.names(rwlCombined))
  first.year <- as.matrix(apply(rwlCombined, 2, dplR:::yr.range, yr.vec=yr))[1, ]
  last.year <- as.matrix(apply(rwlCombined, 2, dplR:::yr.range, yr.vec=yr))[2, ]
  neworder <- order(first.year, decreasing=FALSE)
  segs <- rwlCombined[, neworder, drop=FALSE]
  n.col <- ncol(segs)
  seq.col <- seq_len(n.col)
  for (i in seq.col) {
    segs[[i]][!is.na(segs[[i]])] <- i
  }
  seg2col <- which(names(segs)==series.name)
  segs.axis2 <- names(segs)
  segs.axis4 <- names(segs)
  segs.axis2[seq(2,n.col,by=2)] <- NA
  segs.axis4[seq(1,n.col,by=2)] <- NA
  
  par(mfcol=c(2,1))
  par(mar=c(-0.1, 5, 2, 5) + 0.1, mgp=c(1.1, 0.1, 0), tcl=0.5, 
      xaxs="i", yaxs="i")
  plot(yr, segs[[1]], type="n", ylim=c(0, n.col+1), 
       xlim=range(floaterCorStats$first,floaterCorStats$last),axes=FALSE,
       ylab="", xlab="")
  abline(h=seq.col,lwd=1,col="grey")
  grid(ny = NA)
  apply(segs, 2, lines, x=yr, lwd=4,lend=2, col="grey60")
  abline(h=seq.col[[seg2col]],lwd=1,col="darkgreen")
  lines(x=yr,y = segs[[seg2col]], lwd=4,lend=2, col="darkgreen")
  axis(2, at=seq.col, labels=segs.axis2, srt=45, tick=FALSE, las=2)
  axis(4, at=seq.col, labels=segs.axis4, srt=45, tick=FALSE, las=2)
  axis(3)
  box()
  # plot 2 -- cor plot by year searched
  sig <- qnorm(1 - 0.05 / 2) / sqrt(floaterCorStats$n)
  par(mar=c(2, 5, -0.1, 5) + 0.1, yaxs="r")
  plot(floaterCorStats$last,floaterCorStats$r,
       type="n",xlab="Year", ylab="End Year Cor.",
       xlim=range(floaterCorStats$first,floaterCorStats$last),
       ylim=range(quantCor,floaterCorStats$r), axes=FALSE)
  xx <- c(min(floaterCorStats$first),max(floaterCorStats$last),
          max(floaterCorStats$last),min(floaterCorStats$first))
  yy <- c(quantCor[1],quantCor[1],quantCor[3],quantCor[3])
  polygon(xx,yy,border = NA,col="lightblue")
  abline(h=quantCor[2],col="darkblue")
  lines(floaterCorStats$last,sig, lty="dashed")
  lines(floaterCorStats$last,floaterCorStats$r,col="grey")
  abline(h=0)
  rBest <- which.max(floaterCorStats$r)
  firstBest <- floaterCorStats$first[rBest]
  lastBest <- floaterCorStats$last[rBest]
  rBest <- floaterCorStats$r[rBest]
  pBest <- floaterCorStats$p[rBest]
  
  points(lastBest,rBest,col="darkgreen",pch=20)
  segments(x0 = firstBest, x1 = lastBest,y0 = rBest, y1 = rBest, 
           lty="dashed", col="darkgreen")
  points(firstBest,rBest,col="darkgreen",pch=20)
  text(x = lastBest, y = rBest, labels = lastBest, 
       col="darkgreen", adj=c(0,1))
  text(x = firstBest, y = rBest, labels = firstBest, 
       col="darkgreen", adj = c(1,1))
  axis(1);axis(2);box()
}