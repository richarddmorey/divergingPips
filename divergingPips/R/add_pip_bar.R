#' Add a series of pip bars to a plot 
#'
#' @param x array of x axis values where to center the bars
#' @param ns array of frequencies
#' @param bar.width The width of a bar in x units
#' @param bar.width.n The number of columns of pips to break the bars into
#' @param line.col A vector of length 2 giving the colors of the pip lines for the top and bottom bars
#' @param hist Create histogram-style bars?
#' @param pct percentage to add to top of bar (for non-stacked plots)
#' @param pct.cex The magnification of the pct labels relative to par(cex.axis)
#' @param bar.col A vector or array giving the colors of the bars (see details)
#' @param line.wd Width of the pip lines
#' @param top.cex The magnification of the top labels relative to par(cex.axis)
#' @param scale.to for percentage bars, N to which to normalize
#' @param ... Unused additional arguments
#'
#' @return Returns NULL invisibly.
add.pip.bar <- function(x, ns, 
                         bar.width = .5,
                         bar.width.n = 2, 
                         line.col = c("white", "white"), 
                         bar.col = c("red", "blue"),
                         hist = FALSE, pct = NULL, 
                         pct.cex = 1, line.wd = 1, 
                         top.cex = top.cex, scale.to = NULL, ...)
{
  
  if(length(dim(ns))!=2){
    dim(ns) = c(2,length(ns)/2)
  }
  
  if(hist & dim(ns)[2]>1){
    return(add.pip.bar.hist(x, ns, 
                             bar.width,
                             bar.width.n, 
                             line.col, bar.col,
                             line.wd = line.wd,
                             pct = pct,
                             pct.cex = pct.cex,
                             top.cex = top.cex, ...))
  } 
  
  if(is.null(scale.to)) scale.to=NA
  
  if(!is.null(scale.to))
     if(!is.na(scale.to)){
    if(any( (ns > 1) | (ns < 0) )){
      stop("For percentage bars, data must be proportions.")
    }
  }
  
  if(length(dim(ns))>2){
    bar.col = matrix(bar.col,2,dim(ns)[3])
  }else{
    bar.col = matrix(bar.col,nrow=2)
  }
  
  
  bar.height.n.all = ceiling(rowSums(ns) / bar.width.n)
  bar.leftover.n = rowSums(ns) %% bar.width.n
  bar.height.n = rowSums(ns) %/% bar.width.n
  
  for(i in 1:2){
    bar.matrix = matrix(NA, bar.width.n, bar.height.n.all[i])
    for(j in 1:dim(ns)[2]){
      if(ns[i,j]==0){
        next
      }
      if(!is.null(scale.to) & !is.na(scale.to)){
        x.c = c( x - bar.width/2, x + bar.width/2 )
        y.c = c(0, ns[i] * scale.to / bar.width.n ) * 2*(2 - i - .5)
        rect(x.c[1], y.c[1], x.c[2], y.c[2], border=line.col[i],col=bar.col[i,j], lwd = line.wd )
      }else{
        idx = c(0,cumsum(ns[i,]))[j:(j+1)] + c(1,0)
        bar.matrix[idx[1]:idx[2]]=j
        y.b = apply(bar.matrix==j,1,function(v) range(which(v))) - c(1,0)
        y.b[is.infinite(y.b)]=0
        y.b = c(y.b[1,], rev(y.b[2,]))
        y.c = 2*(2 - i - .5)*rep(y.b,each=2)
        x.c = seq(x - bar.width/2, x + bar.width/2, bar.width/bar.width.n)
        x.c = c(x.c[1],  rep(x.c[-c(1,bar.width.n+1)],each=2) ,x.c[bar.width.n+1])
        x.c = c(x.c,rev(x.c))
        polygon(x.c,y.c,border=line.col[i],col=bar.col[i,j], lwd = line.wd)
      }
      # Add percentage
      if(!is.null(pct)){
        if(i == 1){
          text(mean(range(x.c)),max(y.c), paste(pct[i],"%",sep=""),adj=c(.5,-.5), cex = pct.cex)
        }else{
          text(mean(range(x.c)),min(y.c), paste(pct[i],"%",sep=""),adj=c(.5,1.5), cex = pct.cex)
        }
      }
    }
  }
  
  
  if(is.na(scale.to) | is.null(scale.to))
    invisible(NULL)
  
  for(i in 1:2){
    ## Draw segments
    for(j in 1:(bar.width.n-1)){
      segments(j*bar.width/bar.width.n + x - bar.width/2,
               0,
               j*bar.width/bar.width.n + x - bar.width/2,
               2*(2 - i - .5)*bar.height.n[i],
               col=line.col[i], lwd = line.wd)
    }
    
    if( bar.leftover.n[i] > 1){
      for(j in 1:(bar.leftover.n[i]-1)){
        segments(j*bar.width/bar.width.n + x - bar.width/2,
                 2*(2 - i - .5)*bar.height.n.all[i],
                 j*bar.width/bar.width.n + x - bar.width/2,
                 2*(2 - i - .5)*bar.height.n[i],
                 col=line.col[i], lwd = line.wd)
      }
    }
    if(bar.height.n.all[i]>1){
      for(j in 1:bar.height.n[i]){
        segments(x - bar.width/2,
                 2*(2 - i - .5)*j,
                 x + bar.width/2,
                 2*(2 - i - .5)*j,
                 col=line.col[i], lwd = line.wd)
      }
    }
  }  
  invisible(NULL)
}




#' Add a histogram-style series of pip bars to a plot 
#'
#' @param x array of x axis values where to center the bars
#' @param ns array of frequencies
#' @param bar.width The width of a bar in x units
#' @param bar.width.n The number of columns of pips to break the bars into
#' @param line.col A vector of length 2 giving the colors of the pip lines for the top and bottom bars
#' @param pct percentage to add to top of bar
#' @param pct.cex The magnification of the pct labels relative to par(cex.axis)
#' @param line.wd Width of the pip lines
#' @param bar.col A vector or array giving the colors of the bars (see details)
#' @param top.cex The magnification of the top labels relative to par(cex.axis)
#' @param ... Unused additional arguments
#'
#' @return Returns NULL invisibly.
add.pip.bar.hist <- function(x, ns, 
                              bar.width = .5,
                              bar.width.n = 2, 
                              line.col = c("white", "white"), 
                              bar.col = c("red", "blue"),
                              pct = NULL, pct.cex = 1,
                              line.wd = 1, 
                              top.cex = 1, ...)
{
  
  if(length(dim(ns))>2){
    bar.col = matrix(bar.col,2,dim(ns)[3])
  }else{
    bar.col = matrix(bar.col,nrow=2)
  }
  
  
  bar.height.n.all = ceiling(ns / bar.width.n)
  bar.leftover.n = ns %% bar.width.n
  bar.height.n = ns %/% bar.width.n
  total.width = bar.width * dim(ns)[2]
  
  for(j in 2:dim(ns)[2]){
    abline(v=x - total.width/2 + (j-1)*bar.width,col="lightgray",lty=3) 
  }
  
  for(i in 1:2){
    for(j in 1:dim(ns)[2]){
      x0 = x - total.width/2 + (j-1)*bar.width + bar.width/2
      text(x0, par()$usr[4],
           dimnames(ns)[[2]][j],cex=par()$cex.axis*top.cex,adj=c(.5,1.2))
      bar.matrix = matrix(NA, bar.width.n, bar.height.n.all[i,j])
      if(ns[i,j]==0){
        next
      }
      bar.matrix[1:ns[i,j]] = 1
      if(j<=dim(ns)[2]/2){
        bar.matrix = bar.matrix[bar.width.n:1,]
      }
      bar.matrix = matrix(bar.matrix,nrow=bar.width.n)
      y.b = apply(bar.matrix==1,1,function(v) range(which(v))) - c(1,0)
      y.b[is.infinite(y.b)]=0
      y.b = c(y.b[1,], rev(y.b[2,]))
      y.c = 2*(2 - i - .5)*rep(y.b,each=2)
      x.c = seq(x0 - bar.width/2, x0 + bar.width/2, bar.width/bar.width.n)
      x.c = c(x.c[1],  rep(x.c[-c(1,bar.width.n+1)],each=2) ,x.c[bar.width.n+1])
      x.c = c(x.c,rev(x.c))
      polygon(x.c,y.c,border=line.col[i],col=bar.col[i,j], lwd = line.wd)
      # Add percentage
      if(!is.null(pct)){
        if(i == 1){
          text(mean(range(x.c)),max(y.c), paste(pct[i,j],"%",sep=""),adj=c(.5,-.5), cex = pct.cex)
        }else{
          text(mean(range(x.c)),min(y.c), paste(pct[i,j],"%",sep=""),adj=c(.5,1.5), cex = pct.cex)
        }
      }
      
      # Draw segments
      for(k in 1:(bar.width.n-1)){
        segments(k*bar.width/bar.width.n + x0 - bar.width/2,
                 0,
                 k*bar.width/bar.width.n + x0 - bar.width/2,
                 2*(2 - i - .5)*bar.height.n[i,j],
                 col=line.col[i], lwd = line.wd)
      }
      if( bar.leftover.n[i,j] > 1){
        for(k in 1:(bar.leftover.n[i,j]-1)){
          s = 1-2*(j<=dim(ns)[2]/2)
          segments(s*k*bar.width/bar.width.n + x0 - s*bar.width/2,
                   2*(2 - i - .5)*bar.height.n.all[i,j],
                   s*k*bar.width/bar.width.n + x0 - s*bar.width/2,
                   2*(2 - i - .5)*bar.height.n[i,j],
                   col=line.col[i], lwd = line.wd)
        }
      }
      if(bar.height.n[i,j]>0){
        for(k in 1:(bar.height.n[i,j])){
          segments(x0 - bar.width/2,
                   2*(2 - i - .5)*k,
                   x0 + bar.width/2,
                   2*(2 - i - .5)*k,
                   col=line.col[i], lwd = line.wd)
        }
      }
      
    }
  }
  invisible(NULL)
}


