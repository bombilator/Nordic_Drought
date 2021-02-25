# do correlation coefficient algorithm on different periods, also depending on the well
spearcorr <- function(data, diffid1, diffid2, y1_start, y1_end, y2_start, y2_end, 
                      y3_start, y3_end, dofilter){
  if(dofilter==1){
    print("doing 1 period/same for all")
    new <- data %>% select(-data) %>% unnest() %>% 
      filter(date >= y1_start & date <= y1_end) %>%
      select(-mean_mon, -year, -month) %>% group_by(id, N, E) %>% arrange(agg, date) %>% 
      filter(!is.na(index)) %>% nest(.key="stand") %>% 
      mutate(corr = purrr::map(stand, group_by, agg) %>% 
               purrr::map(summarize, corr = cor(sgi, index, use = "everything", method="spearman")))
  } else if(dofilter==2){
    print("doing 2")
    new <- data %>% select(-data) %>% unnest() %>% 
      filter(case_when(Cluster==diffid1 ~date >= y2_start & date <= y2_end,
                       Cluster != diffid1 ~date >= y1_start & date <= y1_end)) %>%
      select(-mean_mon, -year, -month) %>% group_by(id, N, E) %>% arrange(agg, date) %>% 
      filter(!is.na(index)) %>% nest(.key="stand") %>% 
      mutate(corr = purrr::map(stand, group_by, agg) %>% 
               purrr::map(summarize, corr = cor(sgi, index, use = "everything", method="spearman")))
  } else if(dofilter==3) {
    print("doing 3")
    new <- data %>% select(-data) %>% unnest() %>% 
      filter(case_when(Cluster==diffid1 ~date >= y2_start & date <= y2_end,
                       Cluster==diffid2 ~date >= y3_start & date <= y3_end,
                       Cluster != diffid1 & Cluster != diffid2 ~date >= y1_start & date <= y1_end)) %>%
      select(-mean_mon, -year, -month) %>% group_by(id, N, E) %>% arrange(agg, date) %>% 
      filter(!is.na(index)) %>% nest(.key="stand") %>% 
      mutate(corr = purrr::map(stand, group_by, agg) %>% 
               purrr::map(summarize, corr = cor(sgi, index, use = "everything", method="spearman")))
  } else{
    print("doing entire record, no filter")
    new <- data %>% select(-data) %>% unnest() %>% 
      select(-mean_mon, -year, -month) %>% group_by(id, N, E) %>% arrange(agg, date) %>% 
      filter(!is.na(index)) %>% nest(.key="stand") %>% 
      mutate(corr = purrr::map(stand, group_by, agg) %>% 
               purrr::map(summarize, corr = cor(sgi, index, use = "everything", method="spearman")))
  }
  new <- new %>% select(-stand) %>% unnest() %>% arrange(id) %>% ungroup()
  return(new)
  # df %>% select(-data) %>% unnest() %>% 
  # filter(case_when(Cluster==34 ~ year >= 2003 & year <= 2006,    # filter for different times as the 
  #                 Cluster!=34 ~ year>= 2002 & year <= 2004)) %>% # different clusters have droughts for different 
  #                                                                # periods
  # select(-mean_mon, -year, -month) %>% group_by(id, N, E) %>% arrange(agg, date) %>% 
  # filter(!is.na(index)) %>% # if I use the same period for standardisation for met as for groundwater, 
  # # NAs will be produced in the first years of the met index value calculations for all aggregation periods
  # nest(.key = "stand") %>%
  # mutate(
  #   cor_sgi_spi = 
  #     # purrr::map(stand, gather, spi_agg, spi, -date, -sgi) %>% 
  #     purrr::map(stand, group_by, agg) %>% 
  #     purrr::map(summarize, cor_sgi_spi = cor(sgi, index, use = "everything",
  #                                             method="spearman"))
  # )
}

#
#
#
# do the same, but return p.value as well as the correlation coefficient rho
spearcorr.pval <- function(data, diffid1, diffid2, y1_start, y1_end, y2_start = y1_start, 
                           y2_end = y1_end, y3_start = y1_start, y3_end = y1_end, 
                           dofilter = 4){
  library(broom)
  if(dofilter==1){
    print("doing 1 period, filtered/same for all")
    new <- data %>% 
      filter(date >= y1_start & date <= y1_end) %>% group_by(id, agg) %>% arrange(agg, date) %>% 
      filter(!is.na(index)) %>% 
      do(cortest = tidy(cor.test(~sgi+index, use = "everything", method="spearman", data=.))) %>% 
      mutate(pval = cortest$p.value, corr = cortest$estimate, 
             tresh= ifelse(pval < 0.05, "<0.05", ">0.05"))
  } else if(dofilter==2){
    print("doing 2 different filters")
    new <- data %>% 
      filter(case_when(Cluster==diffid1 ~date >= y2_start & date <= y2_end,
                       Cluster != diffid1 ~date >= y1_start & date <= y1_end)) %>%
      group_by(id, agg) %>% arrange(agg, date) %>% 
      filter(!is.na(index)) %>% 
      do(cortest = tidy(cor.test(~sgi+index, use = "everything", method="spearman", data=.))) %>% 
      mutate(pval = cortest$p.value, corr = cortest$estimate, tresh= ifelse(pval < 0.05, "<0.05", ">0.05"))
  } else if(dofilter==3) {
    print("doing 3 different filters")
    new <- data %>% 
      filter(case_when(Cluster==diffid1 ~date >= y2_start & date <= y2_end,
                       Cluster==diffid2 ~date >= y3_start & date <= y3_end,
                       Cluster != diffid1 & Cluster != diffid2 ~date >= y1_start & date <= y1_end)) %>%
      group_by(id, agg) %>% arrange(agg, date) %>% 
      filter(!is.na(index)) %>% 
      do(cortest = tidy(cor.test(~sgi+index, use = "everything", method="spearman", data=.))) %>% 
      mutate(pval = cortest$p.value, corr = cortest$estimate, tresh= ifelse(pval < 0.05, "<0.05", ">0.05"))
  } else{
    print("doing entire record, no filter")
    new <- data %>% group_by(id, agg) %>% arrange(agg, date) %>% 
      filter(!is.na(index)) %>% 
      do(cortest = tidy(cor.test(~sgi+index, use = "everything", method="spearman", data=.))) %>% 
      mutate(pval = cortest$p.value, corr = cortest$estimate, tresh= ifelse(pval < 0.05, "<0.05", ">0.05"))
  }
  new <- new %>% select(-cortest) %>% arrange(id) %>% ungroup()
  return(new)
}