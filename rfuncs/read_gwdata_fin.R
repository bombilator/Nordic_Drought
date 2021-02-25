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

    gw_srt <- list("vector", ncol(Id_Vars))
    
    for(i in 1:ncol(Id_Vars)) {
        gw_srt[[i]] <- bind_cols(
            Id_Vars[, i] %>% rename(),
            Coords_Vars[, i],
            Date_Vars[, i],
            Level_Vars[, i],
            Aq_Vars[, i],
            km_Vars[, i],
            El_Vars[, i]
            )
        
        
        data.table::setnames(gw_srt[[i]], 
                             old = names(gw_srt[[i]]), 
                             new = names(gw_srt[[1]]))
    }
    
    gw_df <- bind_rows(gw_srt) %>% 
        dplyr::filter(!is.na(Aika)) 
    
    return(gw_df)
}