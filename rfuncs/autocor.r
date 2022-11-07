autocorr <- function(data, Cluster, diffid1, diffid2, y1_start, y1_end, y2_start, y2_end, 
                     y3_start, y3_end, dofilter){
  library(lubridate)
  # filter by date
  if(dofilter==1){
    print("doing 1 period/same for all")
    new <- data %>% unnest() %>% 
      filter(date >= y1_start-years(2) & date <= y1_end) %>%
      select(-date) %>% filter(!is.na(sgi)) %>% nest(data=c(sgi))
     
  } else if(dofilter==3) {
    print(paste("doing 3", " using: ", data[Cluster]))
    new <- data %>% unnest() %>% 
      filter(case_when(Cluster==diffid1 ~date >= y2_start-years(2) & date <= y2_end,
                       Cluster==diffid2 ~date >= y3_start-years(2) & date <= y3_end,
                       Cluster != diffid1 & 
                         Cluster != diffid2 ~date >= y1_start-years(2) & date <= y1_end)) %>%
      select(-date) %>% filter(!is.na(sgi)) %>% nest(data=c(sgi)) 
  } else if(dofilter==0){
    print("doing entire record, no filter")
    new <- data %>% unnest() %>% select(-date) %>% 
      filter(!is.na(sgi)) %>% nest(data=c(sgi))
    
  } else{
    errorCondition("no filter, enter 0 for no filter, 1 for the same for all ids or 3 for 3 different filters")
  } 
  
  # after filter, calculate autocorrelation
  
  for(i in 1:length(new$id)) {
    print(i)
    new$an[[i]]$auto = acf(new$data[[i]], lag.max = 48, plot=FALSE)$acf
    # fun below <- https://stackoverflow.com/questions/14266333/extract-confidence-interval-values-from-acf-correlogram
    # confidence interval is the same as the level of significant confidence
    # for significant autocorrelation you want the p value (pval) to be higher than the highest level
    # of significance (below)
    new$an[[i]]$conf = qnorm(1-0.05/2)/sqrt(length(new$data[[i]]$sgi))
  }

  for(i in 1:length(new$id)){
    new$conf[[i]] = new$an[[i]]$conf
    new$auto[[i]] = new$an[[i]]$auto[1:length(new$an[[i]]$auto)]
    new$lag[[i]] = (1:length(new$an[[i]]$auto) -1 )
  }

  return(new)
}  
