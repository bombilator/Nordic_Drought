spi_ = function(df, P, id = df$id, date = df$date#, 
                # fun = "spi"
                ){
  library(SPEI)
  # if(fun == "spi"){
  spi.df = data.frame(id = matrix(id))
  # for(j in length(id)){
    # spi.df$id <- id
    spi.df$date <- date
    spi.df <- spi.df %>% group_by(id)
    for(i in 1:48){
      label <- paste("spi_", i, sep="")
      # print(label)
      spi.df[[label]] <- as.numeric(SPEI::spi(P, i, distribution = "PearsonIII")$fitted, 3)
      # print(spi.df[[label]])
    }
  # }
  return(spi.df)
  # }
  # if(fun == "T"){
  #   sti.df = data.frame(id = matrix(id))
  #   # for(j in length(id)){
  #   # spi.df$id <- id
  #   sti.df$date <- date
  #   sti.df <- sti.df %>% group_by(id)
  #   for(i in 1:48){
  #     label <- paste("sti_", i, sep="")
  #     # print(label)
  #     sti.df[[label]] <- as.numeric(SPEI::spi(P, i)$fitted, 3)
  #     # print(spi.df[[label]])
  #   }
  #   # }
  #   return(sti.df)
  # }
}