pltLinePoint <- function(x) {
  
  # save name of groundwater well
  nm <- as.character(x$Omr_stn[1])
  
  # how to plot gw-series
  p <- ggplot(data = x, aes (Date, norm)) + 
        geom_point() + geom_line() +
        theme_light() +
        ylab("m.a.s.l.") +
        ylim(c(-5,5))
  
  # plot to file
  ggsave(filename = paste("./output/figs/linepl_", nm,".pdf", sep=""), 
          width = 12, height = 6, plot = p)
}
