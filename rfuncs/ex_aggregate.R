ex_aggregate = function (x, 
  start_TS = min(as.Date(x$DATE)), 
  end_TS = max(as.Date(x$DATE)),
  remove_W53 = FALSE,
  freq = "weekly") {
  
  require(lubridate)
  require(dplyr)
 
  meanx = mean(x$VALUE, na.rm = T)
  
  
  # weekly aggregation --------------------------------------------------------
  if (freq == "weekly") {
    
    # aggregate data
    zx = x %>% 
      group_by(
        YEAR, YEARWEEK
      ) %>% 
      summarize(
        ID = first(ID), 
        VALUE = mean(VALUE),
        VALUE_ZEROMEAN = VALUE - meanx
      ) %>% 
      ungroup()
    
    
    # weekly averages
    wats = x %>% 
      dplyr::mutate(
        VALUE_ZEROMEAN = VALUE - meanx
      ) %>% 
      group_by(
        ID, YEARWEEK
      ) %>% 
      dplyr::summarize(
        WEEK_MEAN = mean(VALUE_ZEROMEAN, na.rm = T),
        WEEK_STD = sd(VALUE_ZEROMEAN, na.rm = T)
      ) 
    
    zx = zx %>% 
      dplyr::inner_join(
        ., wats, by = c("ID", "YEARWEEK")
      ) %>% 
      dplyr::mutate(
        VALUE_ATS = VALUE_ZEROMEAN - WEEK_MEAN,
        VALUE_SATS = (VALUE_ZEROMEAN - WEEK_MEAN)/WEEK_STD
      # ) %>% 
      # dplyr::mutate(
        # DATE = as.POSIXct(paste0(YEAR, "-", YEARWEEK, "-7"), format="%Y-%U-%u")
      )
    
    
    # generate vector with all weekly dates in TS and NA
    alldates = seq(
      lubridate::ymd(start_TS), lubridate::ymd(end_TS), by = '1 week'
      )
    
    # generate empty xts with full length for later merge
    dd = data_frame(
      YEAR = lubridate::year(alldates),
      YEARWEEK = lubridate::week(alldates),
      allNA = NA
      ) %>% 
      dplyr::left_join(
      # merge dfs and remove duplicates with NA for normalized data
        ., zx, by = c("YEAR", "YEARWEEK")
      ) %>%
      dplyr::mutate(
        DATE = as.POSIXct(paste0(YEAR, "-", YEARWEEK, "-7"), format = "%Y-%U-%u"),
        ID = x$ID[1]
      ) %>%
      dplyr::select(
        # add anomaly data here if needed
        ID, DATE, VALUE, YEARWEEK
      )
    
    
    
    # get rid of week 53 in order to be able to convert to base::ts object,
    # which is necessary for certain further computation
    
    if (remove_W53 == TRUE) {
      dd = tbl_df(dd) %>%   
        dplyr::filter(YEARWEEK != 53) %>% 
        dplyr::select(-YEARWEEK)
    } else {
      dd = dd %>% dplyr::select(-YEARWEEK)
      }  
    
    # monthly aggregation -----------------------------------------------------
    
  } else if (freq == "monthly") {
    
    # aggregate data
    zx = x %>% 
      group_by(
        YEAR, MONTH
      ) %>% 
      summarize(
        ID = first(ID), 
        VALUE = mean(VALUE),
        VALUE_ZEROMEAN = VALUE - meanx
      ) %>% 
      ungroup()
    
    
    # monthly averages
    wats = x %>% 
      dplyr::mutate(
        VALUE_ZEROMEAN = VALUE - meanx
      ) %>% 
      group_by(
        ID, MONTH
      ) %>% 
      dplyr::summarize(
        MONTH_MEAN = mean(VALUE_ZEROMEAN, na.rm = T),
        MONTH_STD = sd(VALUE_ZEROMEAN, na.rm = T)
      ) 
    
    # calculate anomaly for every value
    zx = zx %>% 
      dplyr::inner_join(
        ., wats, by = c("ID", "MONTH")
      ) %>% 
      dplyr::mutate(
        VALUE_ATS = VALUE_ZEROMEAN - MONTH_MEAN,
        VALUE_SATS = (VALUE_ZEROMEAN - MONTH_MEAN) / MONTH_STD
        # ) %>% 
        # dplyr::mutate(
        # DATE = as.POSIXct(paste0(YEAR, "-", YEARWEEK, "-7"), format="%Y-%U-%u")
      )
    
    
    # generate vector with all weekly dates in TS and NA
    # extra work around for monthly aggregation (leap year problem)
    start_TS_m = as.Date(paste(zx$YEAR[1], zx$MONTH[1], 15, sep = "-"))
    
    alldates = seq(
      start_TS_m, end_TS, by = '1 month'
    )
    
    # generate empty xts with full length for later merge
    dd = data_frame(
      YEAR = lubridate::year(alldates),
      MONTH = lubridate::month(alldates),
      allNA = NA
    ) %>% 
      dplyr::left_join(
        # merge dfs and remove duplicates with NA for normalized data
        ., zx, by = c("YEAR", "MONTH")
      ) %>%
      dplyr::mutate(
        DATE = alldates,
        ID = x$ID[1]
      ) %>%
      dplyr::select(
        # add anomaly data here if needed
        ID, DATE, VALUE, MONTH
      )
    
    # get rid of week 53 in order to be able to convert to base::ts object,
    # which is necessary for certain further computation
    
    
    
  } else if (freq == "daily") {
  
    # daily aggregation -------------------------------------------------------
      
    # aggregate data
    zx = x %>% 
      group_by(
        YEAR, YEARDAY
      ) %>% 
      summarize(
        ID = first(ID), 
        VALUE = mean(VALUE),
        VALUE_ZEROMEAN = VALUE - meanx
      ) %>% 
      ungroup()
    
    
    # daily averages
    wats = x %>% 
      dplyr::mutate(
        VALUE_ZEROMEAN = VALUE - meanx
      ) %>% 
      group_by(
        # keep ID for later merging reasons
        ID, YEARDAY
      ) %>% 
      dplyr::summarize(
        DAY_MEAN = mean(VALUE_ZEROMEAN, na.rm = T),
        DAY_STD = sd(VALUE_ZEROMEAN, na.rm = T)
      ) 
    
    zx = zx %>% 
      dplyr::inner_join(
        ., wats, by = c("ID", "YEARDAY")
      ) %>% 
      dplyr::mutate(
        VALUE_ATS = VALUE_ZEROMEAN - DAY_MEAN,
        VALUE_SATS = (VALUE_ZEROMEAN - DAY_MEAN)/DAY_STD
        )
    
    # ggplot(zx) +
    #   aes(x= DATE, y = VALUE_ZEROMEAN) +
    #   geom_line(colour = "red") +
    #   geom_line(aes(y = VALUE_ATS), colour = "black")

    # generate vector with all weekly dates in TS and NA
    alldates = seq(
      lubridate::ymd(start_TS), lubridate::ymd(end_TS), by = '1 day'
    )
    
    # generate empty tbl with full length for later merge
    dd = data_frame(
      YEAR = lubridate::year(alldates),
      YEARDAY = lubridate::yday(alldates),
      allNA = NA) %>% 
      dplyr::left_join(
        # merge dfs and remove duplicates with NA for normalized data
            ., zx, by = c("YEAR", "YEARDAY")
      ) %>%
      dplyr::mutate(
        DATE = as.POSIXct(paste0(YEAR, "-", YEARDAY), format = "%Y-%j"),
        ID = x$ID[1]
      ) %>% 
      dplyr::select(
    # add anomaly data here if needed
        ID, DATE, VALUE, YEARWEEK
      )
    
    
    # get rid of week 53 in order to be able to convert to base::ts object,
    # which is necessary for certain further computation

    if (remove_W53 == TRUE) {
      dd = tbl_df(dd) %>%   
        dplyr::filter(YEARWEEK != 53) %>% 
        dplyr::select(-YEARWEEK)
    } else {
      dd = dd %>% dplyr::select(-YEARWEEK)
    }  
    
  } else {
    # throw error
    stop("No valid aggregation period selected")
  }
  
  return(dd)
}

# add scripts for anomaliyed space, so that the return argument works 
#  for all of the aggregation frequencies  

# biweekly aggregation --------------------------------------------------------  
# } else if (freq == "biweekly") {
#   
#   # aggregate data
#   zx = x %>% 
#     dplyr::mutate(
#       BIYEARWEEK = ifelse(YEARWEEK %% 2, YEARWEEK, YEARWEEK - 1)
#     ) %>% 
#     group_by(
#       YEAR, BIYEARWEEK
#     ) %>% 
#     dplyr::summarize(
#       ID = first(ID), 
#       NORM = mean(NORM), 
#       MEAS = mean(MEAS),
#       MEAS_ZEROMEAN = mean(MEAS_ZEROMEAN)
#     ) 
#   
#   
#   # generate vector with all monthly dates in TS and NA
#   alldates = seq(lubridate::ymd(start_TS), lubridate::ymd(end_TS), by = '2 weeks')
#   
#   biweekyearsNA = data.frame(YEAR = lubridate::year(alldates),
#     BIYEARWEEK = lubridate::week(alldates)) %>% 
#     group_by(YEAR) %>% 
#     dplyr::mutate(
#       BIYEARWEEK = ifelse(BIYEARWEEK %% 2, BIYEARWEEK, BIYEARWEEK - 1)
#     )
#   
#   
#   # generate empty xts with full length for later merge
#   fullNA = transform(biweekyearsNA,
#     allNA = rep(NA, length(YEAR)))
#   
#   # merge dfs and remove duplicates with NA for normalized data
#   dd = dplyr::left_join(fullNA, zx, by = c("YEAR", "BIYEARWEEK"))
#   
#   # get rid of week 53 in order to be able to convert to base::ts object,
#   # which is necessary for certain further computation
#   
#   dd = transform(dd, DATE = alldates,
#     ID = rep(x$ID[1], nrow(dd)))
#   
#   if (remove_W53 == TRUE) {
#     dd = tbl_df(dd) %>%   
#       filter(BIYEARWEEK != 53)
#   }
#   
#   
