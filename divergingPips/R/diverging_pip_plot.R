
#' Create a diverging pip bar plot from frequencies in an array
#' 
#' 
#'
#' @param ns A 2 by N by M array (see details)
#' @param bar.width The width of a bar in x units
#' @param bar.width.n The number of columns of pips to break the bars into
#' @param line.col A vector of length 2 giving the colors of the pip lines for the top and bottom bars
#' @param line.wd Width of the pip lines
#' @param bar.col A vector or array giving the colors of the bars (see details)
#' @param xlab x axis label
#' @param box Create box around entire plot?
#' @param tick.every Make a y axis tick after this many pip rows
#' @param sym Should the y axis by symmetric around 0?
#' @param cluster.width For side-by-side plots, width of all bars together in x axis units
#' @param panel.lty The line type for dividing lines across bars/clusters
#' @param stacked If TRUE, create a stacked bar plot (see details)
#' @param hist If TRUE, create a histogram-style bar chart (see details)
#' @param top.cex The magnification of the top labels relative to par(cex.axis)
#' 
#' @return Returns NULL invisibly.
#' @export
#'
#' @examples
#' ## A basic bar plot
#' frq = array( c(10,40,20,10,30,20), dim = c(2,4))
#' 
#' rownames(frq) = c("Women","Men")
#' colnames(frq) = 1995:1998
#' 
#' diverging_pip_plot(frq, bar.width = .3, bar.width.n = 4, sym = TRUE)
#' 
#' ## Side-by-side bar plot
#' frq = array( c(10,40,20,10,
#'                30,20,15,20,
#'                15,15,20,22,
#'                11,23,54,23), dim = c(2,4,2))
#' 
#' rownames(frq) = c("Women","Men")
#' colnames(frq) = 1995:1998
#' dimnames(frq)[[3]] = c("a","b")
#' 
#' bar.col = array(c("red","blue","darkred","darkblue"),dim=c(2,2))
#' 
#' diverging_pip_plot(frq, bar.width = .2, bar.width.n = 4, bar.col = bar.col,
#'                    sym = TRUE, cluster.width = .4, panel.lty = 1)
#' 
#' ## Stacked bar plot
#' diverging_pip_plot(frq, bar.width = .5, bar.width.n = 6, bar.col = bar.col,
#'                    sym = TRUE, cluster.width = .2, panel.lty = 1, stacked=TRUE)
#' 
#' ## Histogram-style plot
#' 
#' bar.col = array(c("pink","lightblue","red","blue","darkred","darkblue"),dim=c(2,3))
#' 
#' frq = array( c(10,40,20,10,
#'                30,20,15,20,
#'                15,15,20,22,
#'                11,23,54,23,
#'                20,15,20,15,
#'                15,20,22,31), dim = c(2,4,3))
#' 
#' rownames(frq) = c("Women","Men")
#' colnames(frq) = 1995:1998
#' dimnames(frq)[[3]] = c("a","b","c")
#' 
#' diverging_pip_plot(frq, bar.width = .2, bar.width.n = 6, bar.col = bar.col,
#'                    sym = TRUE, cluster.width = .3, panel.lty = 1, hist=TRUE)
diverging_pip_plot <- function(ns, bar.width = .3,
                            bar.width.n = 2, 
                            line.col = c("white", "white"), 
                            line.wd = 1,
                            bar.col = c("red", "blue"),
                            xlab="", box=TRUE, tick.every=5, sym=FALSE,
                            cluster.width=.3, panel.lty=0,
                            stacked=FALSE, hist=FALSE,top.cex = 1)
{
  
  if(length(dim(ns))>2){
    bar.col = matrix(bar.col,2,dim(ns)[3])
  }
  
  if(length(ns)==2) ns = matrix(ns,2,1)
  if(length(dim(ns))==3){
    side.by.side=TRUE
    n.in.cluster = dim(ns)[3]
    if(stacked & hist) stop("Only one of stacked and hist may be true at once.")
  }else{
    side.by.side=FALSE
    stacked = FALSE
    hist = FALSE
  }
  
  if(sym){
    if(stacked){
      ymax = max(ceiling(apply(ns,c(1,2),sum)/bar.width.n))
    }else{
      ymax = max(ceiling(ns/bar.width.n))
    }
  }else{
    if(stacked){
      ymax = rev(apply( ceiling(apply(ns,c(1,2),sum)/bar.width.n), 1, max ))
    }else{
      ymax = rev(apply( ceiling(ns/bar.width.n), 1, max ))
    }
  }
  ylims = c(-1,1)*ymax*1.05
  
  if(side.by.side){
    xlims = c(.5, ncol(ns) + .5)
  }else{
    xlims = c(.8, ncol(ns) + .2)
  }
  
  
  par(las=1)
  plot(0,0,xlim=xlims,ylim=ylims,typ='n',axes=FALSE,ylab="",xlab=xlab)
  
  for(i in 1:ncol(ns)){
    if(side.by.side){
      if(stacked | hist){
        add.pip.bar(i,ns[,i,],bar.width,
                bar.width.n, 
                line.col, 
                bar.col,
                hist = hist, line.wd = line.wd, 
                top.cex = top.cex)
      }else{
        for(j in 0:(n.in.cluster-1)){
          add.pip.bar(i - cluster.width/2 + j*cluster.width/(n.in.cluster-1),
                  ns[,i,j+1],bar.width,
                  bar.width.n, 
                  line.col, 
                  bar.col[,j+1],
                  line.wd = line.wd, top.cex = top.cex)
          text(i - cluster.width/2 + j*cluster.width/(n.in.cluster-1),
               par()$usr[4],
               dimnames(ns)[[3]][j+1],cex=par()$cex.axis*top.cex,adj=c(.5,1.2))
        }
      }
    }else{
      add.pip.bar(i,ns[,i],bar.width,
              bar.width.n, 
              line.col, 
              bar.col,
              line.wd = line.wd, top.cex = top.cex)
    }
  } 
  axis(1,at=1:ncol(ns),labels=colnames(ns),tick = FALSE)
  ticks = seq(tick.every, ceiling(max(ymax)/tick.every)*tick.every,tick.every)
  axis(2,at=ticks,labels=ticks*bar.width.n)
  axis(2,at=-ticks,labels=ticks*bar.width.n)
  
  for(i in 1:2){
    mtext(rownames(ns)[i],2,par()$mgp[1],cex=par()$cex.lab,adj=2-i,las=0)
  }
  if(dim(ns)[2]>1 & side.by.side){
    abline(v=seq(1.5,ncol(ns)-.5,1),col="gray",lty=panel.lty)
  }
  abline(h=0,col="gray",lty=1)
  
  if(box) box()
  
  invisible(NULL)
}