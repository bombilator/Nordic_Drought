library(RPostgreSQL);library(tidyverse);library(lubridate);library(zoo)
library(dplyr);library(gwdyn);library(TSclust);library(dendextend)
library(DBI);library(data.table);library(sf);library(stars);library(tiff)
library(raster);library(sp);library(rgdal)

rm(list=ls())
sapply(list.files("Q:/Projects/Drought Scandinavia/Michelle/Nordic_Drought/sweden_class/gwdyn/R/", 
                  pattern="*.R", full.names=TRUE), source)

setwd("Q:/Projects/Drought Scandinavia/Michelle/Nordic_Drought/")

# import and load data  ----
# import data
# ts <- readRDS("sweden_class/input data/sgu_21_11.RDS")$meas %>%
#   dplyr::rename(pid = Omr_stn, location=Omr, well_nr=Stn, mdate=Datum, level_masl=m_o_h,
#          m = UnderMark) %>%
#   left_join(.,  readRDS("sweden_class/input data/sgu_21_11.RDS")$meta %>%
#               dplyr::rename(pid=Omr_stn) %>%
#               distinct(pid, N, E)) %>%
#   dplyr::select(-UnderOkRor, - YEAR,-MONTH,-YEARWEEK,
#                 -MONTHDAY,-YEARDAY,-HYD_YEAR)
# ts <- ts %>% filter(!is.na(N) & !is.na(E)) %>% dplyr::rename(lon=N, lat=E)
# coordinates(ts) = ~lon+lat
# proj4string(ts) = CRS("+init=epsg:3006")
# ts = spTransform(ts, CRS("+proj=longlat + datum=WGS84"))
# ts = as.data.frame(ts)
# write_rds(ts, 'sweden_class/input data/ts.rds')
ts<- read_rds('sweden_class/input data/ts.rds') %>% group_by(pid)

# meta <- readRDS("sweden_class/input data/sgu_21_11.RDS")$meta %>%
#   dplyr::rename(pid=Omr_stn, name=Namn, sed=Jordart, type=Akvifertyp,
#                 topo=TopoLage, elevation=RefNivOkRor, welldep=RorLangd) %>%
  # dplyr::select(-Start, -RorHojdOMark, -Matmetod, -Kommunkod, -Kvalitet, -EUCD, -N,-E)
# meta <- meta %>% dplyr::mutate(confinement = ifelse(grepl('öppet', type), 'unconfined', NA),
#                        confinement = ifelse(grepl('slutet', type), 'confined', confinement),
#                        type = ifelse(grepl('berg och jord', type), 'both', type),
#                        type = ifelse(grepl('berg', type), 'bedrock', type),
#                        type = ifelse(grepl('jord', type), 'sediment', type)
# )
# 
# write_rds(meta, 'sweden_class/input data/meta.rds')
meta<- read_rds('sweden_class/input data/meta.rds')


bbox = as.numeric(c(11, 55, 24, 69)) 
names(bbox) <- c('left','bottom','right','top')
source("Q:/Projects/Drought Scandinavia/Michelle/Nordic_Drought/rfuncs/get_stamenmapPNG.R")
stamen_fenn <- get_stamenmapPNG(bbox, zoom = 6, maptype = "toner-background", color="color",
                                force = TRUE); rm(bbox)
ggmap(stamen_fenn, darken = c(0.6, 'white')) + 
  geom_point(data=ts%>% distinct(pid, lon, lat), aes(lon,lat)) 
  # geom_text(data=ts%>% distinct(pid, N, E), 
  #           aes(N,E, label=ndays), size=2, vjust=2, hjust=2) + 
  # scale_colour_viridis_c()

# analyse all with NA ----
ff <- ts %>% ungroup() %>% 
  group_by(pid) %>% 
  nest() %>%
  ungroup %>% 
  dplyr::filter(row_number() %in% 1:10000) %>%
  dplyr::mutate(
    purrr::map_df(data, function (x) summarise(x, start = min(mdate))),
    purrr::map_df(data, function (x) summarise(x, end = max(mdate))),
    purrr::map_df(data, function (x) summarise(x, n_meas = length(mdate))),
    ndays = as.numeric(end-start, unit = "days"),
    meas_freq_d = round(n_meas/ndays, digits = 4),
    # len = as.numeric(len), units = "days")
  )

#Daily_data ----
d_full <- ff %>%
  dplyr::filter(meas_freq_d > 0.95) %>%
  unnest(cols = c(data)) %>%
  mutate(
    YEAR = year(mdate),
    MONTH = month(mdate),
    WEEK = week(mdate),
    DAY = mday(mdate),
  )%>%
  group_by(pid, YEAR, MONTH)

d_tmp <- d_full %>%
  dplyr::filter(row_number() == 15)

d_len <- d_tmp %>%
  ungroup(YEAR) %>%
  mutate(
    YEARDAY = yday(mdate)
  ) %>%
  group_by(pid, YEAR) %>%
  arrange(pid, YEAR) %>%
  mutate(
    data_length = length(unique(MONTH))
  )

#Daily_gaps
d_gaps <- ff %>%
  dplyr::filter(between (meas_freq_d, 0.3000, 0.9499)) %>%
  unnest(cols = c(data)) %>%
  mutate(
    YEAR = year(mdate),
    MONTH = month(mdate),
    WEEK = week(mdate),
    DAY = mday(mdate),
  )%>%
  group_by(pid, YEAR, MONTH)

d_gaps_tmp <- d_gaps %>%
  dplyr::filter(row_number() == 15)

d_gap_len <- d_gaps_tmp %>%
  ungroup(YEAR) %>%
  mutate(
    YEARDAY = yday(mdate)
  ) %>%
  group_by(pid, YEAR) %>%
  arrange(pid, YEAR) %>%
  mutate(
    data_length = length(unique(MONTH))
  )

#Daily_piece
d_piece <- ff %>%
  dplyr::filter(between (meas_freq_d, 0.1431, 0.2999)) %>%
  unnest(cols = c(data)) %>%
  mutate(
    YEAR = year(mdate),
    MONTH = month(mdate),
    WEEK = week(mdate),
    DAY = mday(mdate),
  )%>%
  group_by(pid, YEAR, MONTH)

d_piece_tmp <- d_piece %>%
  dplyr::filter(row_number() == 5)

d_piece_len <- d_piece_tmp %>%
  ungroup(YEAR) %>%
  mutate(
    YEARDAY = yday(mdate)
  ) %>%
  group_by(pid, YEAR) %>%
  arrange(pid, YEAR) %>%
  mutate(
    data_length = length(unique(MONTH))
  )

#weekly ----
w_gaps <- ff %>%
  # dplyr::filter(between (meas_freq_d, 0.0340, 0.1430)) %>%
  dplyr::filter(meas_freq_d > 0.034) %>%
  unnest(cols = c(data)) %>%
  mutate(
    YEAR = year(mdate), 
    MONTH = month(mdate), 
    WEEK = week(mdate),
  )%>%
  group_by(pid, YEAR, MONTH)

w_tmp <- w_gaps %>%
  dplyr::filter(row_number() == 3)

w_len <- w_tmp %>%
  ungroup(YEAR) %>%
  mutate(
    YEARDAY = yday(mdate)
  ) %>%
  group_by(pid, YEAR) %>% 
  arrange(pid, YEAR) %>% 
  mutate(
    data_length = length(unique(MONTH))
  )

# monthly ----
#w_m <- ff %>%
#  dplyr::filter(between (meas_freq_d, 0.0330, 0.1379))

m_gaps <- ff %>%
  # below code of line removes data with higher than monthly meas but less than weekly
  # dplyr::filter(between (meas_freq_d, 0.0200, 0.0339)) %>% 
  unnest(cols = c(data)) %>%
  dplyr::mutate(
    YEAR = year(mdate), 
    MONTH = month(mdate), 
    WEEK = week(mdate),
  )%>%
  group_by(pid, YEAR, MONTH)

m_tmp <- m_gaps %>%
  dplyr::filter(row_number() == 1)

m_len <- m_tmp %>%
  ungroup(YEAR) %>%
  mutate(
    YEARDAY = yday(mdate)
  ) %>%
  group_by(pid, YEAR) %>% 
  arrange(pid, YEAR) %>% 
  mutate(
    data_length = length(unique(MONTH))
  )

#combine ----
# rest <- ff %>%
  # dplyr::filter (meas_freq_d < 0.0199)# | meas_freq_d > 1)  
 
all_data <- bind_rows(d_tmp, d_gaps_tmp, d_piece_tmp), 
                       # w_tmp, 
                      m_tmp)
all_data_len <- bind_rows(d_len, d_gap_len, d_piece_len), 
                          # w_len, 
                          m_len)
write_rds(all_data, "sweden_class/processed/all_data_swe.rds")

rm(d_gaps, d_full, d_gap_len, d_gaps_tmp, d_len, d_piece, d_piece_len, d_piece_tmp, d_tmp, m_gaps, m_len, m_tmp, rest, w_full, w_m,  w_gaps, w_len, w_tmp, d_w)

# filter years with > 9 measurements/year -----
viable_meas <- all_data_len %>% 
  mutate(maxmeas = max(data_length)) %>% 
  dplyr::filter(maxmeas >= 9) # minimum 8 measurements/year

#Time series with more than 10 continous years ----
viable_cont_years <- viable_meas %>% 
  #because a lot of data has gaps and I'm not sure how to separate those out...
  # dplyr::filter(year(mdate) >2005 & year(mdate)<=2020) %>%
  
  dplyr::filter(row_number() == n()) %>% 
  dplyr::group_by(pid) %>% 
  dplyr::mutate(
    cont = YEAR - lag(YEAR),
    cont = ifelse(is.na(cont), 1, cont) #I had to change if_else to ifelse in order to run the code- Why?
  ) %>% 
  dplyr::arrange(mdate, .by_group = TRUE) %>% 
  dplyr::group_by(run = data.table::rleid(pid, cont), pid) %>% 
  dplyr::mutate(
    run_count = 1:n(),
    maxruncount = max(run_count, na.rm = T) 
  ) %>%
  dplyr::group_by(pid) %>% 
  dplyr::filter(maxruncount == max(maxruncount)) %>% 
  # dplyr::filter(maxruncount >= 8) #series with more than 8 years of continuous data
  dplyr::filter(maxruncount >= 20) 

#join basedata with number of continous data
data_sel_join <- right_join(all_data %>% dplyr::ungroup(), 
                            viable_cont_years %>% dplyr::ungroup(), 
                            by = c("pid", "YEAR"))

#check that the record length is realistic
data_sel_join %>% distinct(pid, data_length, start.y, end.y) %>% 
  dplyr::mutate(years=lubridate::time_length(difftime(end.y,start.y), 'years'))
#complete monthly series where values are missing ----
data_sel_mcomp <- data_sel_join %>% 
  dplyr::select(pid, mdate.x) %>% 
  dplyr::group_by(pid) %>% 
  complete(mdate.x = seq(ymd(min(mdate.x)), ymd(max(mdate.x)), by= "month")) %>% 
  dplyr::mutate(
    YEAR =year(mdate.x),
    MONTH = month(mdate.x)
  ) %>% 
  dplyr::group_by(pid, YEAR, MONTH) %>% 
  dplyr::filter(row_number() == 1)

#join and subset data ----
names(data_sel_join)[names(data_sel_join) == 'MONTH.x'] <- 'MONTH'

data_sel_mfin <- right_join(data_sel_join, data_sel_mcomp, by = c("pid", "YEAR", "MONTH"))

data_sel_mfin = subset(data_sel_mfin, select = c("pid","mdate.x.y", "m.x"))

#fill with new interpolated data, calculate normalized values and z-score. 
data_sel <- data_sel_mfin %>%
  dplyr::group_by(pid) %>%
  dplyr::mutate(VALUE_INTP = na.approx(m.x, na.rm =FALSE)) %>%
  fill(VALUE_INTP, .direction = "updown") %>%
  dplyr::select(1,2,3,4) %>%
  group_by(pid) %>%
  dplyr::mutate(
    VALUE_norm = (VALUE_INTP - min(VALUE_INTP, na.rm = T)) /
      (max(VALUE_INTP, na.rm = T) - min(VALUE_INTP, na.rm = T)),
    VALUE_open = VALUE_INTP - min(VALUE_INTP, na.rm = T),
    VALUE_meas =( VALUE_INTP - mean(VALUE_INTP, na.rm=T)) / sd(VALUE_INTP, na.rm=T)
  ) %>%
  dplyr::select(pid, mdate.x.y , everything()) %>%
  dplyr::mutate_at(
    ., vars(VALUE_norm:VALUE_open), ~round(., 2)
  )

(data_sel %>% nest())[c(1:10,100:120,200:220),] %>% 
  unnest() %>%  dplyr::filter(between(year(mdate.x.y), 1980,1990)) %>%
  ggplot(aes(mdate.x.y, VALUE_norm))+geom_line() + 
  facet_wrap(~pid, scales='free')


# write_rds(data_sel, "sweden_class/processed/data_sel_swe_monthly_newdat.rds")
write_rds(data_sel, "sweden_class/processed/data_sel_swe_monthly_0520.rds")

# Figures
# dsn = "PG:dbname='swe_1' host='localhost' port='5432' user='postgres' password='abc'"
# p1_seq <- seq(500, 83e3, by= 500)
# p1 <- lapply(p1_seq, function (x) st_read(dsn, "p", query = paste0("SELECT id , geom FROM p WHERE id <", x," AND id >", x-500)))
# p2 <- p1 %>% bind_rows()
# 
# data_sel_loc <- data_sel_join %>%
#   group_by(pid) %>%
#   mutate(
#     id = pid
#   ) %>%
#   group_by(id) %>%
#   nest() %>%
#   dplyr::inner_join(p2, data_sel_loc, by = "id") %>%
#   dplyr::relocate(id, geom, data)
# data_sel_loc2 <- st_as_sf(data_sel_loc)
# 
# poly <- st_read(dsn, "ro_export_polygons_115")
# 
# grid <- st_read(dsn, "cntr_rg_03m_2014_3035")
# grid %>% dplyr::filter(cntr_id %in% c("RO", "HU", "AT", "DE", "HR", "SK", "BG", "SI", "RS")) %>%
#   ggplot(data=.)+geom_sf()+geom_sf(data=data_sel_loc2)

#join with all_data

#skip this step and use time_series <- data_sel
# write.csv(data_sel, "complete_data.csv", row.names = FALSE)
# write_rds(ts, "processed/data_sel.rds")
# write_rds(ts, "processed/data_sel_loc2.rds")

# time_series <- dbGetQuery(con, "SELECT * FROM danube1.wts_danube")


# calculate autocorrelation ----
#do it for SGI, correlate SGI memory to indices
autocorr <- data_sel %>% dplyr::mutate(month=month(mdate.x.y)) %>%
  group_by(pid, month) %>% 
  dplyr::mutate(sgi = qqnorm(-1*VALUE_INTP, plot.it=FALSE)$x) %>% ungroup() %>%
  group_by(pid)%>% 
  dplyr::select(pid, sgi) %>%
  filter(!is.na(sgi)) %>% nest(data=c(sgi),.key="data")

for(i in 1:length(autocorr$pid)) {
  print(i)
  autocorr$an[[i]]$auto = acf(autocorr$data[[i]], type='correlation', lag.max = 80, plot=FALSE)$acf
  # fun below <- https://stackoverflow.com/questions/14266333/extract-confidence-interval-values-from-acf-correlogram
  # confidence interval is the same as the level of significant confidence
  # for significant autocorrelation you want the p value (pval) to be higher than the highest level
  # of significance (below)
  # autocorr$an[[i]]$conf = qnorm(1-0.05/2)/sqrt(length(autocorr$data[[i]]$sgi))
}

for(i in 1:length(autocorr$pid)){
  # autocorr$conf[[i]] = autocorr$an[[i]]$conf
  autocorr$auto[[i]] = autocorr$an[[i]]$auto[1:length(autocorr$an[[i]]$auto)]
  autocorr$lag[[i]] = (1:length(autocorr$an[[i]]$auto) -1 )
} 
autocorr <- autocorr %>%
  dplyr::select(pid, auto, lag) %>% unnest 

(autocorr %>% nest())[c(1:10,100:120,200:220),] %>% 
  unnest() %>% 
  ggplot(aes(lag, auto))+geom_line() + geom_hline(yintercept=0)+
  geom_smooth(formula=y~log(x)) +
  facet_wrap(~pid)

# use code to get logarithmic fit of autocorr, works v poorly
library(broom)
logest <- autocorr %>%
  group_by(pid) %>% 
  filter(lag!=0) %>%
  do(model=tidy(lm(auto~log(lag), data=.)),
     sum = summary(lm(auto~log(lag), data=.)))
logest$p=0
logest$est=0
logest$resid=0
logest$r.sq = 0
for(i in 1:length(logest$pid)){
  logest$p[i] = logest$model[[i]]$p.value[2]
  logest$est[i] = logest$model[[i]]$estimate[2]
  
  logest$resid[i] = logest$sum[[i]]$sigma
  logest$r.sq[i] = logest$sum[[i]]$r.squared
}
(autocorr %>% nest())[c(1:10,100:120,200:220),] %>% 
  unnest() %>% #dplyr::mutate(lag=ifelse(lag==0, 0.000001, lag)) %>% 
  right_join(logest %>% dplyr::select(-model, -sum), ., by='pid') %>% 
  group_by(pid) %>%
  dplyr::mutate(a0 = ifelse(lag==1, auto,NA), a0=dplyr::first(na.omit(a0))) %>%
  ggplot(aes(lag, auto))+geom_line() + geom_hline(yintercept=0)+
  geom_line(data=. %>% filter(lag!=0),
            aes(lag, est*log(lag)+a0, group=pid, colour=r.sq), linetype='dashed') +
  scale_colour_gradient(high='darkgreen', low='red') +
  facet_wrap(~pid)
memory <- autocorr %>% filter(lag==1) %>% dplyr::select(-lag) %>% 
  right_join(logest %>% dplyr::select(-model, -sum), ., by='pid') %>%
  distinct()

# playing with decomposition of data and HURST using time series -----


# attempt to detrend ts using rewriting some code from Ezra


time_series <- data_sel %>% rename(mdate=mdate.x.y, m=m.x) #%>% group_by(pid) %>%
# dplyr::mutate(m = zoo::na.approx(m, na.rm=T))
as_mon_ts = function (x, y) {
  ts(x, start = c(y[[1]], y[[2]]), frequency = 12)
}
#possible help: https://stackoverflow.com/questions/12058390/stl-decomposition-of-time-series-with-missing-values-for-anomaly-detection
x <- time_series %>% group_by(pid) %>% tidyr::nest() %>%
  dplyr::mutate(
    start = map(data %>% map("mdate"), .f = start_YEARMON),
    tsobj = map2(data %>% map("m"), start, .f = as_mon_ts))
x <- x %>% dplyr::mutate(lambda=map(na.contiguous(tsobj), .f=forecast::BoxCox.lambda),
                         x = map2(tsobj, lambda, .f=forecast::BoxCox),
                         x.stl = map(x, .f=stl, s.window='periodic', na.action = na.contiguous))

# Hurst
library(fracdiff)
x<- x %>% dplyr::mutate(hurst.frac=map(na.contiguous(x), .f=fracdiff,0,0),
                        hurst= as.double(hurst.frac %>% map('d')) + 0.5) %>%
  distinct(pid, hurst)


# for(i in 1:nrow(x)){
#   x$seasonal[[i]] = x$x.stl[[i]]$time.series[,1]
#   x$trend[[i]] = x$x.stl[[i]]$time.series[,2]
#   x$remainder[[i]] = x$x.stl[[i]]$time.series[,3]
# }
# x <- x %>% dplyr::mutate(seasonal = map(seasonal, .f= as.data.frame),
#                          trend = map(trend, .f= as.data.frame),
#                          remainder = map(remainder, .f= as.data.frame))
# x <- x %>% dplyr::select(pid, data, seasonal, trend, remainder) %>%
#   unnest('data') %>% dplyr::filter(!is.na(m)) %>%
#   dplyr::select(pid, mdate, m, seasonal, trend, remainder) %>%
#   nest(data=c(mdate,m), .key='data')
# x %>% dplyr::mutate(hurst= data %>% map(na.contiguous(m)) %>%
#                       fracdiff(.,0,0)$d + 0.5)
#   distinct(pid, hurst)
#
# x <- x %>% unnest(c('seasonal', 'trend', 'remainder'), names_repair = 'unique') %>%
#   dplyr::rename(seasonal='x...2', trend='x...3', remainder='x...4')
# p.time <- time_series %>% dplyr::select(pid, mdate) %>%
#   dplyr::mutate(month_past = 1:length(mdate)) %>% filter(!is.na(time))
# x <- x %>% group_by(pid) %>% dplyr::mutate(month_past = 1:length(pid)) %>%
#   left_join(., p.time) %>% left_join(., time_series, by=c('pid', 'mdate')) %>%
#   dplyr::select(-data,-month_past)
#
# (x %>% nest())[c(1:10,100:120,200:220),] %>% unnest() %>%
#   ggplot(., aes(mdate, m)) + geom_line() + facet_wrap(~pid, scales='free') +
#   # geom_line(aes(y=m-trend, colour='m-trend-season'))
#   geom_line(aes(colour='seasonal', y=seasonal)) +
#   geom_line(aes(colour='trend', y=trend)) +
#   geom_line(aes(colour='remainder', y=remainder))
#

# run indices: ----

# data_sel <- read_rds("sweden_class/processed/data_sel_swe_monthly_newdat.rds")
# data_sel <- read_rds("sweden_class/processed/data_sel_swe_monthly_0520.rds")

  
ind_wkl <- data_sel %>%
  dplyr::filter(!is.na(VALUE_norm)) %>%
  group_by(pid) %>%
  dplyr::summarise(
    # originally open scale - now also standardized
    iaf.s = mean(intannfluc(mdate.x.y, VALUE_norm, out = "s")),
    iaf.y = intannfluc(mdate.x.y, VALUE_norm, out = "y"),
    BLS = baseflow(mdate.x.y,VALUE_norm, out = "BFS", block.len = 3), #
    BLI = baseflow(mdate.x.y,VALUE_norm, out = "BFI", block.len = 3), #
    l1 = lmom(mdate.x.y, VALUE_norm, out = "l1"),
    l2 = lmom(mdate.x.y, VALUE_norm, out = "l2"),
    l3 = lmom(mdate.x.y, VALUE_norm, out = "l3"),
    l4 = lmom(mdate.x.y, VALUE_norm, out = "l4"),
    amp.max = max(m.x, na.rm=T)-min(m.x, na.rm=T),
    iqr = IQR(m.x, na.rm=T),
    # standardized
    bimod = bimodality(VALUE_norm),
    #Colwell- Standard value: s = 11(why?) -> changed to s = 12 
    #Colwell- Standard value: aggregation = weeks -> changed to month
    colwell.C = colwells(mdate.x.y, VALUE_norm, out = "constancy"),
    colwell.M = colwells(mdate.x.y, VALUE_norm, out = "contingency"),
    colwell.P = colwells(mdate.x.y, VALUE_norm, out = "predictability"),
    cvmon.min = cvmonminmax(mdate.x.y, VALUE_norm, out = "min"),
    cvmon.max = cvmonminmax(mdate.x.y, VALUE_norm, out = "max"),
    # dip = list(diptest::dip.test(VALUE_norm)),#
    dc.rng.01.09 = fdcindices(mdate.x.y, VALUE_norm, out = "range", y1 = 0.1, y2 = 0.9),
    dc.rng.02.08 = fdcindices(mdate.x.y, VALUE_norm, out = "range", y1 = 0.2, y2 = 0.8),
    dc.rng.025.075 = fdcindices(mdate.x.y, VALUE_norm, out = "range", y1 = 0.25, y2 = 0.75),
    dc.slp.01.09 = fdcindices(mdate.x.y, VALUE_norm, out = "slope", y1 = 0.1, y2 = 0.9),
    dc.slp.02.08 = fdcindices(mdate.x.y, VALUE_norm, out = "slope", y1 = 0.2, y2 = 0.8),
    dc.slp.025.075 = fdcindices(mdate.x.y, VALUE_norm, out = "slope", y1 = 0.25, y2 = 0.75),
    med = median(VALUE_norm, na.rm = TRUE),
    parde = parde(mdate.x.y, VALUE_norm, out = "metric"),
    # needs higher resolution data, daily
    #pulse.count.l = pulses(mdate.x.y, VALUE_norm, out = "count", threshold.type = "low"),
    #pulse.count.h = pulses(mdate.x.y, VALUE_norm, out = "count", threshold.type = "high"),
    #pulse.dur.l = pulses(mdate.x.y, VALUE_norm, out = "duration", threshold.type = "low"),
    #pulse.dur.h = pulses(mdate.x.y, VALUE_norm, out = "duration", threshold.type = "high"),
    #pulse.cv.l = pulses(mdate.x.y, VALUE_norm, out = "CV", threshold.type = "low"),
    #pulse.cv.h = pulses(mdate.x.y, VALUE_norm, out = "CV", threshold.type = "high"),
    #bandwd = silverman(VALUE_norm),
    vardoy.min = varjdminmax(mdate.x.y, VALUE_norm, out = "min"), #
    vardoy.max = varjdminmax(mdate.x.y, VALUE_norm, out = "max") #
  ) %>% left_join(., x) %>% left_join(., memory) %>%distinct()

# gives dubplicates
write_rds(ind_wkl, "sweden_class/processed/indices_monthly.rds")
# write_rds(ind_wkl, "sweden_class/processed/indices_monthly_200.rds")





# PCA ----

# ind_wkl <- read_rds("sweden_class/processed/indices_monthly.rds")
# ind_2 <- read_rds("sweden_class/processed/indices_monthly_200.rds")

ind_inf <- ind_wkl %>% dplyr::select(iaf.s, iaf.y, l4, bimod, colwell.C, colwell.M, 
                          dc.rng.01.09, dc.slp.01.09, med, parde, hurst, est, auto, 
                          vardoy.min, vardoy.max, amp.max, iqr)

ind_inf <- do.call(data.frame,
  lapply(ind_inf, # replacing (one) Inf value with the median value, should not affect PCA
         function(x) replace(x, is.infinite(x)|is.na(x), median(x, na.rm=T))))
ind_pca <- prcomp(ind_inf[,2:17], #][,2:21], 
                  center = TRUE,scale. = TRUE)


library(ggbiplot)
ggbiplot(ind_pca)
plot(cumsum(ind_pca$sdev^2 / sum(ind_pca$sdev^2)), type="b", ylim=0:1) # PCA1-5 or 6?
library(factoextra); library(FactoMineR)
fviz_eig(ind_pca)
get_eigenvalue(ind_pca) %>% mutate(eigenvalue=round(eigenvalue,3)) # PCA1-4?
ind_pca$rotation
fviz_pca_var(ind_pca, 
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(ind_pca,axes=c(3,4),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

rownames(ind_pca$x)<- ind_wkl$pid

fviz_pca_biplot(ind_pca, repel = TRUE,
                geom_ind=c("point"),
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

data_sel %>% 
  # dplyr::filter(pid=='6700_32' | pid=='11_33' | pid=="101_1" | pid=='30_8') %>%
  dplyr::filter(pid=='55_24' | pid=='50000_30' | pid=="101_1" | pid=='53_3') %>%
  ggplot(., aes(mdate.x.y, VALUE_norm, group=pid)) +
  geom_line() + facet_wrap(~pid, scales='free')
  

rot <- ind_pca$rotation %>% as.data.frame() %>% 
  rownames_to_column() %>% 
  reshape2::melt(., id.vars=c('rowname')) %>%
  dplyr::rename(index=rowname, rotation=value, pca=variable) %>% 
  mutate(type=ifelse(rotation<0, '-', '+'),
  rotation=ifelse(rotation<0, rotation*-1, rotation)) %>% 
  left_join(., stack(summary(ind_pca)$importance[2,]) %>%
              dplyr::rename(variance=values, pca=ind)) %>%
  mutate(pca=as.numeric(gsub(".*PC", "", pca))) %>% 
  dplyr::group_by(pca)
# make boxplot, fill with variance of pca
rot %>% filter(pca<6) %>%
  ggplot(., aes(y=rotation, group=index, fill=pca)) + geom_boxplot()

library(corrplot)
corrplot(round(cor(ind_inf[, 2:17]),2), 
         type='upper', order='hclust',
         tl.col='black', tl.srt=45)
library("PerformanceAnalytics")
chart.Correlation(ind_inf[, 2:17], 
                  histogram=TRUE, pch=19)

library("FactoMineR")
fviz_pca_ind(ind_pca, col.ind="cos2") +
  scale_color_gradient2(low="blue", mid="white", 
                        high="red", midpoint=0.50)
---
# DESCRIPTORS ----

#med gw-level (needed for the calculation of depth to gw)
gw_med <- data.frame(
  pid = data_sel$pid,
  VALUE_INTP = data_sel$VALUE_INTP
) %>%
  group_by(pid) %>%
  mutate(
    med = median(VALUE_INTP, na.rm = TRUE)
  ) %>%
  dplyr::relocate(pid, med, VALUE_INTP) %>%
  group_by(pid, med) %>%
  nest()

#land use/elevation
loc <- data_sel_loc2 %>% 
  dplyr::select(id, geom)
loc_buffer_s <- st_buffer(loc, 300) # change the number in order to in/decrease the radius around the well
loc_buffer_l <- st_buffer(loc, 3000) # change the number in order to in/decrease the radius around the well

corine_name <- "C:/06EU_Danube/R_EU_Danube/corine_danube.tif"
DEM_name <- "C:/06EU_Danube/R_EU_Danube/DEM_danube1.tif"
riv <- st_read("C:/06EU_Danube/R_EU_Danube/C_Danube_riv.shp")
corine <- raster(corine_name, crs ="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
DEM <- raster(DEM_name, crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#descriptors <- data.frame(
#  cor = raster::extract(corine, data_sel_loc2)) %>%
#  mutate(
#    pid = data_sel_loc2$id, cor,
#    ele = raster::extract(DEM, data_sel_loc2),
#  ) %>%
#  dplyr::relocate(pid, cor, ele)
#descriptors <- dplyr::inner_join(descriptors, gw_med, by = "pid") %>%
#  mutate(
#    dpt = round(ele - med, digits = 2)
#  )

----
LUs <- raster::extract(corine, loc_buffer_s)
names(LUs) <- as.integer(loc$id)
LUs.counts <- rapply(LUs, f=function(x) ifelse(is.na(x),-99,x), how="replace")
LUs.counts <- lapply(LUs.counts,table)
LUs.pct <- lapply(LUs.counts, FUN=function(x){ x / sum(x) })


LUl <- raster::extract(corine, loc_buffer_l)
names(LUl) <- as.integer(loc$id)
LUl.counts <- rapply(LUl, f=function(x) ifelse(is.na(x),-99,x), how="replace")
LUl.counts <- lapply(LUl.counts,table)
LUl.pct <- lapply(LUl.counts, FUN=function(x){ x / sum(x) })

LUs.ov <- LUs.pct %>% 
  lapply(., as.data.frame) %>%
  bind_rows(., .id="pid") %>%
  as_tibble() %>%
  dplyr::select(pid, Class="Var1", Frac=Freq) %>%
  complete(pid = names(LUs)) %>%
  mutate(pid = as.integer(pid)) %>%
  arrange(pid) %>%
  group_by(pid) %>%
  nest(LUs = c(Class, Frac))
#  pivot_wider(names_from = Class, values_from = Frac) overview!

LUl.ov <- LUl.pct %>% 
  lapply(., as.data.frame) %>%
  bind_rows(., .id="pid") %>%
  as_tibble() %>%
  dplyr::select(pid, Class="Var1", Frac=Freq) %>%
  complete(pid = names(LUl)) %>%
  mutate(pid = as.integer(pid)) %>%
  arrange(pid) %>%
  group_by(pid) %>%
  nest(LUl = c(Class, Frac))
#  pivot_wider(names_from = Class, values_from = Frac) overview!
  
descriptors <- data.frame(
    pid = loc$id,
    ele = raster::extract(DEM, loc)
    )
descriptors <- dplyr::inner_join(descriptors, gw_med, by = "pid") %>%
  mutate(
    dpt = round(ele - med, digits = 2)
  )
descriptors <- dplyr::inner_join(descriptors, LUs.ov, by = "pid")
descriptors <- dplyr::inner_join(descriptors, LUl.ov, by = "pid")

write_rds(descriptors, "C:/06EU_Danube/R_EU_Danube/processed/descriptors.rds")

----
#Distance to river
wd <- 1e3

data_sel_loc2_sf <- data_sel_loc2 %>%
  dplyr::select(id, geom) %>%
  st_set_crs(sf::st_crs(riv)) %>% 
  group_by(id) %>% 
  nest(data = c(geom))

coord <- data.frame(
  id = data_sel_loc2$id,
  st_coordinates(data_sel_loc2)
  ) 

   #calculate distances:

riv_bb <- coord %>%
  dplyr::select(id, X, Y) %>% 
  bind_cols(., riv %>% nest(data = everything())) %>% 
  left_join(., data_sel_loc2_sf, by ="id") %>% 
  dplyr::rename(rivers = `data.x`, wells = `data.y`) %>% 
  mutate(
    xmin = X - wd, 
    xmax = X + wd, 
    ymin = Y - wd, 
    ymax = Y + wd
  ) %>% 
  group_by(id) %>% 
  mutate(
    bbox = purrr::pmap(list(xmin, ymin, xmax, ymax), function(...) {
      c(xmin = xmin, ymin =ymin, xmax=xmax, ymax=ymax) %>% 
        st_bbox()
    } )
  ) %>% 
  mutate(
    # crop and simplify
    cropped = purrr::map2(rivers, bbox, st_crop),
      purrr::map(st_simplify, dTolerance = 50),
      purrr::map(st_cast, "MULTILINESTRING"),
    dist = purrr::map2(cropped, wells, st_distance, by_element = TRUE) 
  )

write_rds(riv_bb, "C:/06EU_Danube/R_EU_Danube/processed/riv_bb_danube.rds")  
    




    
df %>% group_by(ID, YEAR) %>%
  dplyr::filter(row_number() == 1) %>%
  count(YEAR) %>%
  ggplot(data=data_sel_join$pid)



# indices according to Wang et al 2006 ---------------------------------

# indices according to Wang et al 2006 ---------------------------------
# read data ---- loop, if errors in dataserie

# id_vector <- ind_wkl[['id2']]
# 
# 
# for (x in id_vector){
#   test = paste("SELECT * FROM danube1.wts_danube WHERE id2 =", x)
#   data_sel2 <- dbGetQuery(con, test)
# 
#   if(nrow(time_series) != 0){
# 
# 
#   print(x)
# 
#     res <- try(
time_series <- data_sel %>% rename(mdate=mdate.x.y, m=m.x) #%>% group_by(pid) %>%
  # dplyr::mutate(m = zoo::na.approx(m, na.rm=T))
as_mon_ts = function (x, y) {
  ts(x, start = c(y[[1]], y[[2]]), frequency = 12)
}


 #possible help: https://stackoverflow.com/questions/12058390/stl-decomposition-of-time-series-with-missing-values-for-anomaly-detection
ind_wkl_wang <- time_series %>% 
  group_by(pid) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(
    start = map(data %>% map("mdate"), .f = start_YEARMON),
    tsobj = map2(data %>% map("m"), start, .f = as_mon_ts))#, #.f = as_wkly_ts),
    ####
    wang = map(tsobj, .f = tschar)
  ) %>% 
  tidyr::unnest(wang) %>% 
  dplyr::rename(
    hurst = Hurst,
    lyapunov = Lyapunov,
    autocor = autocorrelation,
    freq = frequency,
    seas = seasonal,
    skew = skewness,
    kurt = kurtosis,
    autocor.dc = dc_autocorrelation,
    nonlinear.dc = dc_nonlinear,
    skew.dc = dc_skewness,
    kurt.dc = dc_kurtosis
  )
#   )
#   if(inherits(res, "try-error"))
#   {
#     print(x)
#     print("error")
#     next
#   }
# }
# 
# }

# all_indicies <- merge(ind_wkl, ind_wkl_wang, by = c("id2"))



ind_inf <- do.call(data.frame,
                   lapply(ind_wkl, # replacing (one) Inf value with the median value, should not affect PCA
                          function(x) replace(x, is.infinite(x)|is.na(x), median(x, na.rm=T))))
all_indices <- ind_inf

#optimize this when time, currently removing indices not working or unnecessery columns
all_indices = subset(all_indices, select = -c(dip, start))
all_indices = all_indices %>% BBmisc::dropNamed("tsobj")
all_indices = all_indices %>% BBmisc::dropNamed("data")
all_indices = all_indices %>% BBmisc::dropNamed("freq")
all_indices = all_indices %>% BBmisc::dropNamed("seas")
all_indices = all_indices %>% BBmisc::dropNamed("freq")

#scale and as matrix
# z_indicies <- scale(all_indicies[2:39])
z_indices <- scale(all_indices[2:21])#[c(1:150),]


# z_test <- scale(all_indicies["iaf.s"])

z_frame = as.data.frame(z_indices)

z_ind <- cbind(all_indices$pid, z_frame)
# z_ind <- cbind(all_indices[c(1:150),]$pid, z_frame)

names(z_ind)[names(z_ind) == 'all_indices$pid'] <- 'pid'

#preparing clustering

x <- z_ind

time_series_sub <- subset(ts, select =c("pid","mdate", "m"))

time_series <- time_series_sub %>% 
  mutate(id2 = as.character(pid))

dfind_to_mat = function (x) {
  x %>% 
    column_to_rownames(var= "pid") %>% 
    as.matrix
}


# index-based clustering ------------------------------------------------------

# selct indices to cluster
ind_sel <- c(
  "iaf.s",
  "vardoy.min",
  "rev.avg", "BLI",
  "autocor", "hurst",
  "recov.const", "reces.const",
  "avg.ann.max",
  "peakts.avg", 
  "bimod",
  "nonlinear")

ind_sel2 <- c(
  "iaf.y",
  "rise.cv",
  "rev.avg",
  "autocor", "hurst",
  "recov.const",
  "peakbase.cv",
  "pulse.dur.h", 
  "pulse.count.h",
  "nonlinear")

ind_sel3 <- c(
  "hurst", 
  "autocor",
  "pulse.count.l",
  "pulse.count.h",
  "iaf.y"
)

ind_series <- z_ind %>% 
  dplyr::select(pid, one_of(ind_sel)) %>% 
  nest(data = everything()) %>% 
  mutate(
    mat       = map(data, dfind_to_mat),
    man_diss  = map(mat, dist, method = "manhattan"),
    euc_diss  = map(mat, dist, method = "euclidean"),
    man_clust = map(man_diss, hclust, method = "ward.D2") %>% 
      map(as.dendrogram),
    euc_clust = map(euc_diss, hclust, method = "ward.D2") %>% 
      map(as.dendrogram),
    man_cut  = map(man_clust, cutree, k=6),
    euc_cut  = map(euc_clust, cutree, k=6),
  )

dend = as.dendrogram(ind_series$euc_clust[[1]])

dend <- color_branches(dend, k = 6)

plot(dend)

cutdend <- cutree(dend, 6)
table(cutdend)
cutdend <- stack(cutdend) %>% 
  as.data.frame() %>%
  dplyr::rename(euc_clust = values, pid=ind)

meta
  
clust_meta <- meta %>% right_join(., cutdend) %>%
  right_join(., ind_series$mat[[1]][,1] %>% stack(.) %>% as.data.frame() %>%
               dplyr::rename(iaf.s = values, pid=ind))
table(clust_meta$euc_clust, clust_meta$sed)
clust_meta %>% ungroup() %>% group_by(sed, euc_clust) %>% 
  dplyr::mutate(sum=n()) %>% ungroup() %>% group_by(sed) %>% 
  dplyr::mutate(new=(sum-min(sum))/(max(sum)-min(sum))) %>% 
  ggplot(., aes(sed, as.factor(euc_clust), fill=new)) + geom_tile() +
  geom_text(aes(label=sum)) +
  facet_grid(confinement~., space='free', scales='free') +
  scale_fill_gradient2(low="#00AFBB", mid="#E7B800", high="#FC4E07",
                       midpoint=0.5) +
  theme(axis.text.x = element_text(angle=90))

clust_meas <- ts %>% right_join(., cutdend)
clust_meas %>% ungroup() %>% group_by(pid) %>% 
  filter(year(mdate)>2010 & year(mdate)<=2011) %>%
  dplyr::mutate(new=m-median(m, na.rm=T)) %>%
  ggplot(., aes(mdate, new, group=pid)) + 
  geom_line() + # geom_point(size=1) + 
  facet_grid(rows=vars(euc_clust), scales='free') +
  scale_y_reverse() 

ggmap(stamen_fenn, darken = c(0.6, 'white')) + 
  geom_point(data=ts%>% distinct(pid, lon,lat) %>% 
               right_join(., clust_meta), 
             aes(lon,lat, colour=euc_clust), shape=21, fill=NA) +
  scale_colour_gradient2(low="#00AFBB", mid="#E7B800", high="#FC4E07",
                       midpoint=3.5)
  


# plotting and diagnosis -------------------------
# plot ts and clusters ------

ts_cluster <- ts %>% 
  ungroup %>% 
  left_join(., tibble(
    id2 = names(ind_series$man_cut[[1]]), 
    c_ind_man = as.character(ind_series$man_cut[[1]])), by = "id2"
  ) %>% 
  left_join(., tibble(
    id2 = names(ind_series$euc_cut[[1]]), 
    c_ind_euc = as.character(ind_series$euc_cut[[1]])), by = "id2"
  )

write_csv(ts_cluster, "clusters/clusters.csv")


#theme
ez_theme_pl <- theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "grey95"),
    strip.background = element_rect(fill=NA, colour = NA),
    axis.text = element_text(colour = "grey5"),
    axis.ticks.length=unit(-0.2, "cm"), 
    axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
    axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
    legend.position = "none"
    # axis.title.y = element_blank(),
    # plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

# ind man ---------
ts_cluster %>% 
  ggplot() + 
  aes(DATE, VALUE_C, colour = c_ind_man) + 
  labs(title = "index-based / manhattan distance") +
  geom_line(aes(group = ID), size = .2) + 
  stat_summary(fun.y = mean,
               geom = "line", colour = "black") +
  facet_wrap(~c_ind_man, nrow = 3) +
  ez_theme_pl

# ind eucl ---------
ts_cluster_plot %>% 
  filter(date_x_y > as.Date("1980-01-01"))%>%
  filter(date_x_y < as.Date("1995-01-01"))%>%
  ggplot() + 
  aes(date_x_y, value_meas, colour = c_ind_euc) + 
  labs(y="Standardized groundwater level", x="Date") +
  geom_line(aes(group = id2), size = .2) + 
  ylim(c(-6,6))+
  # stat_summary(fun = mean,
  #              geom = "line", colour = "black", size = 0.1) +
  facet_wrap(~c_ind_euc, nrow = 3) +
  ez_theme_pl
