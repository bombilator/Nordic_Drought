# downloads entire databse from SGU open data, renames columns,
# groups by Omr_stn

importSGUjson <- function(...){

  library(tidyverse, quietly = T)
  require(rjson, quietly = T)
  library(lubridate)
  
  URL <- "http://resource.sgu.se/oppnadata/data/grundvatten/grundvattenniva/grundvattennivaer.json"
  
  testDB = rjson::fromJSON(readLines(URL)[1])
  
  # get meta and coordinates --------------------------------------------------
  
  # get meta data, aquifertype etc
  DB_properties = sapply(testDB$features, "[", "properties")
  
  # get geometry data, coordinates etc
  DB_coords =  sapply(testDB$features, "[", "geometry")
  # DB_coords[sapply(DB_coords, is.null)] <- NA
  
  
  DB_cords_df = dplyr::bind_rows(
    lapply(DB_coords, function (x) { 
      # take care of missing values
      if (!is.null(x)) { 
        zz = data.frame(X = x[[2]][1], Y = x[[2]][2]) } else {
        zz = data.frame(X = NA, Y = NA)
        }
      })
    )
  
  # extract meta data from SGU dataset
  DB_prop_meta = dplyr::bind_rows(
    lapply(DB_properties, 
      function (x) dplyr::bind_cols(x[1:length(x)-1])
      )
    ) %>% 
    dplyr::mutate(
      CRS = testDB$crs$properties$name
    )
  
  DB_prop_meta$X = DB_cords_df$X
  DB_prop_meta$Y = DB_cords_df$Y
  
  # rename columns
  colnames(DB_prop_meta) <- c("Omr_stn", "Namn", "Start", "Jordart", "Akvifertyp",
                      "TopoLage", "RefNivOkRor", "RorHojdOMark", "RorLangd",
                      "Matmetod", "Kommunkod", "Kvalitet", "End", "EUCD", "CRS", "N", "E")
  
  # get list elements into dataframe
  DB_meas_list = 
    lapply(DB_properties, 
      function (x) lapply(x[length(x)], dplyr::bind_rows)
    )
  
  # flatten list up one level
  DB_meas_list = lapply(DB_meas_list, function (x) x[[1]])
  
  # give names to list elements for later automatic id assignment
  names(DB_meas_list) = DB_prop_meta$Omr_stn
  
  DB_meas = DB_meas_list %>% 
    dplyr::bind_rows(., .id = "Omr_stn") %>% 
    tidyr::separate(
      Omr_stn, c("Omr", "Stn"), remove = FALSE
    ) %>% 
    dplyr::rename(
      Datum = datum_for_matning,
      UnderOkRor = grundvattenniva_cm_u._roroverkant,
      m_o_h = grundvattenniva_m_o.h.,
      UnderMark = grundvattenniva_m_under_markyta
      ) %>% 
    dplyr::group_by(Omr_stn) %>% 
    dplyr::mutate(
      Datum = as.Date(Datum),
      # Z_SCORE = (m_o_h - mean(m_o_h, na.rm = T)) / sd(m_o_h, na.rm = T),
      YEAR = as.numeric(lubridate::year(Datum)),
      MONTH = as.numeric(lubridate::month(Datum)),
      YEARWEEK = as.numeric(lubridate::week(Datum)),
      MONTHDAY = as.numeric(lubridate::day(Datum)),
      YEARDAY = as.numeric(lubridate::yday(Datum)),
      HYD_YEAR = ifelse( MONTH %in% 1:8, YEAR, YEAR + 1)
    ) 
  
  
  # save file for easy retrieval
  SGU_Meas_Meta = list("meas" = DB_meas, "meta" = DB_prop_meta)
  saveRDS(SGU_Meas_Meta, file = paste0("output/SGU_Meas_Meta_", today() ,".RDS"))
  
  return(SGU_Meas_Meta)
}