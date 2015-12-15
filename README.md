# divergingPips
R package for creation of "diverging pip" plots


To install, use the `devtools` package:
    
    install.packages('devtools') # if necessary
     
    devtools::install_github("richarddmorey/divergingPips", subdir = "divergingPips")

Then run the examples:
    
    example("diverging_pip_plot", "divergingPips")

Or try this example:

    frq = array( c(10,40,20,10,
                   30,20,15,20,
                   15,15,20,22,
                   11,23,54,23), dim = c(2,4,2))
    
    rownames(frq) = c("Women","Men")
    colnames(frq) = 1981:1984
                       
    cols = matrix(c(rgb(255,164,56,maxColorValue = 255),
                rgb(120,80,155,maxColorValue = 255),
                rgb(255,164,56,maxColorValue = 255, alpha=128),
                rgb(120,80,155,maxColorValue = 255, alpha=128)),2,2)
    
    divergingPips::diverging_pip_plot(frq, bar.width = .4, bar.width.n = 5, bar.col = cols, sym = FALSE, stacked = TRUE)  

which results in this figure:
![Example figure](http://richarddmorey.org/images/div_pips.png)
