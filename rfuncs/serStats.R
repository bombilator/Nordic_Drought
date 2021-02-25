# some exploratory analysis to make a selection of the 
# data for further analysis 
# NAtest to filter files that have no reference level

serStats <- function(rawData, minMonths) {
  
  ###
  #MetaStats = rawData  %>% 
  #   dplyr::summarise(
  #     count = n(),
  #     StartD = min(Date),
  #     EndD = max(Date),
  #     iqr = IQR(m_o_h, na.rm =T),
  #     MAD = mad(m_o_h, na.rm =T)
  #   ) %>%
  #   # monthly frequency
  #   mutate(
  #     freq_m = count / ((as.yearmon(EndD) - as.yearmon(StartD)) * 12),
  #     freq_y = count / (year(EndD) - year(StartD))
  #   ) %>%
  #   # yearly frequency
  #   mutate(
  #     length_m = (as.yearmon(EndD) - as.yearmon(StartD)) * 12
  #   ) %>%
  #   #   filter values with at least 1 measurements a month, but max 3 on average
  #   #   remove non finite values and series shorter than 30 years
  #   filter(
  #     freq_m < 3 & freq_m >= 1 & is.finite(freq_m) & length_m >= minMonths &
  #       NAtest == FALSE
  #   )
   
  #MetaStats <- droplevels(MetaStats)
  ###
   
   
  SerieStatsDate = measData  
  
  # Martens inter vs intrayearly seasonality ----------------------------------
  LWHW = SerieStatsDate %>% 
    dplyr::group_by(
      Omr_stn, HYD_YEAR
    ) %>% 
    dplyr::mutate(
      mr = min_rank(desc(UnderMark)),
      hr = min_rank(UnderMark)
    ) %>% 
    dplyr::mutate(
      HW = ifelse(mr %in% 1:3, UnderMark, NA),
      LW = ifelse(hr %in% 1:3, UnderMark, NA)
    ) %>% 
    dplyr::filter(
      !is.na(HW) | !is.na(LW)
    )
  
  LW = LWHW %>% 
    dplyr::filter(
      hr %in% 1:3
    ) %>% 
    dplyr::mutate(
      POS = paste(HYD_YEAR, seq_len(n()), sep ="_")
    ) %>% 
    dplyr::select(
      -HW, -mr, -hr
    )
  
  MW = LWHW %>% 
    dplyr::filter(
      mr %in% 1:3
    ) %>% 
    dplyr::mutate(
      POS = paste(HYD_YEAR, seq_len(n()), sep ="_")
    ) %>% 
    dplyr::select(
      -LW, -mr, -hr
    )
  
  LWHW_cons = dplyr::full_join(
    LW, MW[,c(1,13,14,15)], by = c("Omr_stn", "POS", "HYD_YEAR")
    ) %>% 
    dplyr::group_by(Omr_stn, HYD_YEAR) %>% 
    dplyr::summarise(
      HW = mean(HW, na.rm = T),
      LW = mean(LW, na.rm = T))
  
  MLWMHW = LWHW_cons %>% 
    dplyr::summarise(
      MLW = mean(LW, na.rm = TRUE),
      MHW = mean(HW, na.rm = TRUE)
    ) %>% 
    dplyr::mutate(
      s = MHW - MLW
    )
  
  
  LWHW_minmax = LWHW %>% 
    ungroup() %>% 
    group_by(Omr_stn) %>% 
    dplyr::summarise(minLW = min(LW, na.rm = TRUE),
      maxLW = max(LW, na.rm = TRUE),
      minHW = min(HW, na.rm = TRUE),
      maxHW = max(HW, na.rm = TRUE)
    ) %>% 
    dplyr::mutate(
      y = ((minHW - maxHW) + (minLW - maxLW)) / 2
    )
  
  
  # other stats --------------------------------------------------------------
  SerieStats_yearly = SerieStatsDate %>% 
    group_by(Omr_stn, YEAR) %>% 
    dplyr::summarise(
      MF_MTLY = length(m_o_h) / 12,
      IQR = IQR(m_o_h, na.rm =T),
      MAD = mad(m_o_h, na.rm =T)
    ) %>% 
    # at least on average 2 measurements a month on average per year
    # dplyr::filter(MF >= .857) %>%
    dplyr::mutate(
      DIFF_Y = YEAR - lag(YEAR),
      FIRST_YEAR = min(YEAR),
      LAST_YEAR = max(YEAR),
      # YEARS_FULL_RECORD = length(YEAR),
      REM = ifelse(DIFF_Y > 1, 
              ifelse(abs(YEAR - FIRST_YEAR) > abs(YEAR - LAST_YEAR), 
                "BEFORE", "AFTER"), NA)
      ) %>% 
    dplyr::left_join(
      ., LWHW_cons, by = c("Omr_stn", "YEAR" = "HYD_YEAR")
    )
  
  SerieStats_stations = dplyr::full_join(
    MLWMHW, LWHW_minmax, by = "Omr_stn"
    ) %>% 
    dplyr::mutate(
      x = s - y)
  
  ##
  # # LongSeries = SerieStats %>% 
  #LongSeries = SerieStatsDate %>% 
  #   dplyr::filter(MF > 28) %>% 
  #   dplyr::group_by(Omr_stn) %>% 
  #   dplyr::summarize(
  #     MEASFREQ = min(MF),
  #     YEARS = length(Omr_stn), 
  #     MIN_YEAR = min(YEAR),
  #     MAX_YEAR = max(YEAR)
  #     ) %>% 
  #   arrange(desc(YEARS, MIN_YEAR))
  ##
   
  return(list("yearly" = SerieStats_yearly, "stations" = SerieStats_stations))
  
}
