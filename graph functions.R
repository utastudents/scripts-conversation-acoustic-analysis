library(ggplot2)
library(VGAM)
library(fitdistrplus)

resqq <- function(mdl){
  resdf<-data.frame(residuals(mdl))
  colnames(resdf)[1]<-'res'
  ggplot(data=resdf, aes(sample = res)) +
    stat_qq() +
    stat_qq_line() +
    theme_bw() 
}

reshist <- function(mdl){
  resdf<-data.frame(residuals(mdl))
  colnames(resdf)[1]<-'res'
  ggplot(data=resdf, aes(x = res)) +
    geom_histogram(bins=48, closed="right", boundary=0) +
    theme_bw() 
}


hist_with_density = function(mdl, func = "norm", start = NULL){
    res <- residuals(mdl)
    # fit density to data
    fit   = fitdist(res, func, start = NULL)
    args  = as.list(fit$estimate)
    dfunc = match.fun(paste('d', func, sep = ''))

    resdf<-data.frame(res)
    colnames(resdf)[1]<-'res'

    # plot histogram, empirical and fitted densities
    ggplot(resdf, geom = 'blank', aes(x=res)) +
       geom_line(aes(y = after_stat(density),colour = 'Empirical'),stat = 'density') +
       stat_function(fun = dfunc, args = args, aes(colour = func))  +
       geom_histogram(aes(y = after_stat(density)), alpha = 0.4, bins=80) +
       scale_colour_manual(name = '', values = c('red', 'blue')) + 
       theme(legend.position = 'top', legend.direction = 'horizontal')  +
       theme_bw()
}
