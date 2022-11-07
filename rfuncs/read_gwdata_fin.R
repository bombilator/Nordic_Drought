read_gwdata_fin <- function(lspath) {
    library(data.table)
    require(tidyverse)
    gw <- read_excel(lspath, 1)
    
    Id_Vars <- gw %>% select(contains("Tunnus"))
    Coords_Vars <- gw %>% select(contains("Koordinaatit"))
    Date_Vars <- gw %>% select(contains("Aika"))
    Level_Vars <- gw %>% select(contains("Vedenkorkeus"))
    
    Aq_Vars <- gw %>% select(contains("Aquifer"))
    km_Vars <- gw %>% select(contains("PValueen"))
    El_Vars <- gw %>% select(contains("Maanpinta"))
    top_Vars <- gw %>% select(contains("Putken yläpää"))
    bot_Vars <- gw %>% select(contains("Putken alapää"))

    gw_srt <- list("vector", ncol(Id_Vars))
    
    for(i in 1:ncol(Id_Vars)) {
        gw_srt <- bind_cols(
            Id_Vars[, i] %>% rename(),
            Coords_Vars[, i],
            Date_Vars[, i],
            Level_Vars[, i],
            Aq_Vars[, i],
            km_Vars[, i],
            El_Vars[, i],
            top_Vars[, i],
            bot_Vars[, i]
            )
        
        
        data.table::setnames(gw_srt[[i]], 
                             old = names(gw_srt[[i]]), 
                             new = names(gw_srt[[1]]))
    }
    
    gw_df <- bind_rows(gw_srt) %>% 
        dplyr::filter(!is.na(Aika)) 
    
    return(gw_df)
}



read_metadata_fin <- function(lspath) {
  library(data.table)
  require(tidyverse)
  gw <- read_excel(lspath, 1)
  
  Id_Vars <- gw %>% select(contains("Tunnus"))
  Coords_Vars <- gw %>% select(contains("Koordinaatit"))

  Aq_Vars <- gw %>% select(contains("Aquifer"))
  km_Vars <- gw %>% select(contains("PValueen"))
  El_Vars <- gw %>% select(contains("Maanpinta"))
  top_Vars <- gw %>% select(contains("Putken yl"))
  bot_Vars <- gw %>% select(contains("Putken ala"))
  
  gw_srt <- list("vector", ncol(Id_Vars))
  
  for(i in 1:ncol(Id_Vars)) {
    gw_srt <- bind_cols(
      Id_Vars[, i] %>% rename(),
      Coords_Vars[, i],
      Aq_Vars[, i],
      km_Vars[, i],
      El_Vars[, i],
      top_Vars[, i],
      bot_Vars[, i]
  )
print(paste("after bind", (gw_srt[1])))
    data.table::setnames(gw_srt,
                         old = names(gw_srt[i]),
                         new = names(gw_srt[1]))
  }

  # gw_df <- Filter(function(x) !is.null(x), gw_srt)
  # gw_df <- bind_rows(gw_df) 

  return(gw_srt)
}