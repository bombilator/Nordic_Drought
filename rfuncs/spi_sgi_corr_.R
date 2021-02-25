# provide data frame with sgi and spi values in one
# calculate correlation between sgi and different spi's, for different 
# delay periods
spi_sgi_corr_ = function(df, sgi = df$sgi,
                         id = df$id, station = df$Stationname, 
                         tunnus = df$Tunnus){
  library(stringr)
  library(dplyr)
  library(stats)
  
  spi.sgi <- data.frame(id = id,
                        Stationname = station,
                        Tunnus = tunnus) %>% group_by(Tunnus)
  

  for(i in 0:6){
    for(j in 1:48){
      
      label <- paste("spi", j, sep="")
      spi <- df[[label]]

      
      spi.sgi[[paste("spi", j, "_l", i, sep ="")]] = cor(lag(sgi, i), spi,
                     method = "pearson",
                     use = "pairwise.complete.obs")
      
    }
  }
  return(spi.sgi)
}