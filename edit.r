# Cairo sources:
# https://blog.hasanbul.li/2018/01/14/exporting-correlation-plots/
# https://www.jumpingrivers.com/blog/r-graphics-cairo-png-pdf-saving/

library(magrittr); library(dplyr); library(readr); library(ggplot2);
library(tidyverse);library(lubridate);library(zoo); 
library(RPostgreSQL);library(gwdyn);library(TSclust);library(dendextend);
library(DBI);library(data.table);library(sf);library(stars);library(tiff);
library(raster);library(sp);library(rgdal)

rm(list=ls())
sapply(list.files("Q:/Projects/Drought Scandinavia/Michelle/Nordic_Drought/sweden_class/gwdyn/R/", 
                  pattern="*.R", full.names=TRUE), source)

setwd("Q:/Projects/Drought Scandinavia/Michelle/Nordic_Drought/")

ts<- read_rds('sweden_class/input data/ts.rds') %>% group_by(pid) %>% 
  filter(!is.na(m) & !is.na(level_masl))
meta<- read_rds('sweden_class/input data/meta.rds') %>%
  dplyr::filter(pid!='3_25') 
# meta <- read_rds('sweden_class/input data/meta_4clusts-00-15.rds') %>%
  # dplyr::filter(pid!='3_25') 


my_theme <- theme(panel.background = element_rect(colour="lightgrey", fill="white"),
                  # panel.grid = element_line(colour=alpha("lightgrey",0.5)),
                  panel.grid = element_blank(), 
                  strip.background = element_rect(fill=NA, colour=NA),
                  strip.text = element_text(size=7),
                  # strip.background = element_blank(), strip.text=element_blank(),
                  axis.title = element_text(size=7),
                  axis.ticks = element_line(colour="grey"),
                  legend.key = element_blank(), legend.key.height = unit(3, 'mm'),
                  legend.position='bottom',
                  legend.box="vertical",
                  legend.text = element_text(size=7),
                  legend.title = element_text(size=7),
                  plot.margin = unit(c(0,0,0,0), "mm"),
                  axis.text = element_text(size=7))

bbox = as.numeric(c(11, 55, 24, 69)) 
names(bbox) <- c('left','bottom','right','top')
source("Q:/Projects/Drought Scandinavia/Michelle/Nordic_Drought/rfuncs/get_stamenmapPNG.R")
stamen_fenn <- get_stamenmapPNG(bbox, zoom = 6, maptype = "toner-background", color="color",
                                force = TRUE); rm(bbox)


ts <- ts %>% dplyr::filter(pid %in% meta$pid) %>% 
  left_join(meta)
ts %>% group_by(Jordart, group) %>% dplyr::mutate(med=median(m, na.rm=T),
                                                  min = min(m, na.rm=T),
                                                  max=max(m, na.rm=T), 
                                                  sd=sd(m, na.rm=T),
                                                  iqr=IQR(m, na.rm=T)) %>% 
  distinct(Jordart, group, pid, med, min, max, sd, iqr) %>% 
  dplyr::mutate(n = n()) %>% distinct(Jordart, group, med, min, max, sd,iqr, n) %>% 
  write.csv('sweden_class/metadata.csv')

grDevices::cairo_pdf(filename='sweden_class/levels.pdf', width=2.5, height=3.5,
                     fallback_resolution = 400)
ts %>% group_by(pid) %>% dplyr::mutate(med=median(m, na.rm=T),
                                       min = min(m, na.rm=T),
                                       max=max(m, na.rm=T), 
                                       sd=sd(m, na.rm=T),
                                       iqr=IQR(m, na.rm=T)) %>% 
  distinct( pid, med, min, max, sd, iqr) %>% 
  dplyr::mutate(n = n()) %>% distinct(  med, min, max, sd,iqr, n) %>% 
  ggplot()+
  geom_histogram(data=.%>%mutate(type='median depth'), aes(med) ) + 
  geom_histogram(data=.%>%mutate(type='min depth'),aes(min))+
  geom_histogram(data=.%>%mutate(type='max depth'),aes(max))+ 
  facet_grid(rows =vars(type)) + my_theme +
  scale_x_continuous('groundwater level m below the ground surface', expand=c(0,0)) + 
  scale_y_continuous('',expand=c(0,0))
dev.off()

ts.ex <- ts %>% dplyr::filter(pid=='6_26' | pid=='37_32' | 
                                pid=='3_3' | pid=='32_16' | pid=='55_7') %>%
  left_join(., meta) %>% dplyr::filter(between(year(mdate),1980,2010)) %>%
  group_by(pid) %>%
  dplyr::mutate(m=,
    year=year(mdate), month=month(mdate),
                med=median(m, na.rm=T), 
                ym=paste(year,month,sep='-'),
    m=m-med) %>%
  group_by(pid, ym) %>%
  dplyr::mutate(m=median(m, na.rm=T)) %>% 
  group_by(pid, month) %>%
  dplyr::mutate(med=median(m, na.rm=T),
                md=as.Date(paste(month, '15', sep='-'), format='%m-%d')) %>% 
  select(-mdate) %>% distinct() %>%
  ungroup() 
      

grDevices::cairo_pdf(filename='sweden_class/sedimenttype.pdf', width=3.25, height=5.3,
                     fallback_resolution = 400)

ts.ex %>% filter(year==2000) %>%
  ggplot(., aes(md, m, group=year)) + 
  # geom_ribbon(aes(ymax=med, ymin=m, fill='level difference')) + 
  geom_line(aes(y=med, linetype='median level')) +
  # geom_line(aes(linetype='level')) +
  facet_grid(rows=vars(Jordart), cols=vars(year)) + 
  scale_linetype_manual('',values=c('solid', 'longdash')) +
  scale_fill_manual('', values=c('grey')) +
  scale_y_reverse('groundwater level (m) below ground surface',expand=c(0,0)) + 
  scale_x_date('month', expand=c(0,0)) +
  my_theme + theme(legend.position = 'bottom')
dev.off()
# import data  ----
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


# meta <- readRDS("sweden_class/input data/sgu_21_11.RDS")$meta %>%
#   dplyr::rename(pid=Omr_stn, name=Namn, type=Akvifertyp,
#                 topo=TopoLage, elevation=RefNivOkRor, welldep=RorLangd) %>%
#   dplyr::select(-Start, -RorHojdOMark, -Matmetod, -Kommunkod, Kvalitet, -EUCD, -N,-E)
# meta <- meta %>% dplyr::mutate(
#                       confinement = ifelse(grepl('öppet', type), 'unconfined', NA),
#                       confinement = ifelse(grepl('slutet', type), 'confined', confinement),
# 
# 
#                       type = ifelse(grepl('berg och jord', type), 'both', type),
#                        type = ifelse(grepl('berg', type), 'bedrock', type),
#                        type = ifelse(grepl('jord', type), 'sediment', type),
# 
#                        sed = ifelse(grepl('Moränlera', Jordart), 'clayey till', Jordart),
#                        sed = ifelse(grepl('Morän', sed), 'till', sed),
#                        sed = ifelse(grepl('Sand', sed), 'sand', sed),
#                        sed = ifelse(grepl('Lera', sed), 'clay', sed),
#                        sed = ifelse(grepl('Grus', sed), 'gravel', sed),
#                        sed = ifelse(grepl('Torv', sed), 'peat', sed),
#                        sed = ifelse(grepl('Silt', sed), 'silt', sed),
#                        sed = ifelse(grepl('saknas', sed), 'outcrop', sed),
# 
#                        confinement = ifelse(grepl('clay', sed) |
#                                             grepl('silt', sed) |
#                                             grepl('till', sed) , NA, confinement),
#                       group = ifelse((grepl('sand',sed)|grepl('gravel',sed)) &
#                                       confinement == 'confined','confined sand',
#                                     'other),
#                       group = ifelse((grepl('sand',sed)|grepl('gravel',sed)) &
#                                       (confinement != 'confined'|is.na(confinement)),
#                                     'unconfined sand',
#                                     group),
#                       group = ifelse('till' == sed,'till', group),
#                       group = ifelse((grepl('silt',sed)|grepl('clay', sed)),
#                                     'clay and silt', group),
#                       group = ifelse(is.na(type), 'other', group)
# 
# )
# write_rds(meta, 'sweden_class/input data/meta.rds')




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
  dplyr::filter(meas_freq_d>0.0200) %>%
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

all_data <- bind_rows(#d_tmp, d_gaps_tmp, d_piece_tmp), 
                        # w_tmp, 
                      m_tmp)
all_data_len <- bind_rows(#d_len, d_gap_len, d_piece_len), 
                            # w_len, 
                            m_len)
# write_rds(all_data, "sweden_class/processed/all_data_swe.rds")

rm(d_gaps, d_full, d_gap_len, d_gaps_tmp, d_len, d_piece, d_piece_len, d_piece_tmp, d_tmp, m_gaps, m_len, m_tmp, rest, w_full, w_m,  w_gaps, w_len, w_tmp, d_w)

# filter years with > 9 measurements/year -----
viable_meas <- all_data_len %>% 
  mutate(maxmeas = max(data_length)) %>% 
  dplyr::filter(maxmeas >= 9) # minimum 9 measurements/year

#Time series with more than 10 continous years ----
viable_cont_years <- viable_meas %>% 
  #because a lot of data has gaps and I'm not sure how to separate those out...
  
  # dplyr::filter(year(mdate) >1970 & year(mdate)<=2020) %>%
  
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
  dplyr::filter(maxruncount >= 15)  # minimum for SGI in MS3 

# filter based on dates:
viable_cont_years <- viable_cont_years %>% group_by(pid) %>%
  dplyr::filter(year(min(mdate))<=2000 & year(max(mdate))>=2015)

#join basedata with number of continous data
data_sel_join <- right_join(all_data %>% dplyr::ungroup(), 
                            viable_cont_years %>% dplyr::ungroup(), 
                            by = c("pid", "YEAR"))


data_sel_join %>% group_by(pid) %>%
  dplyr::mutate(start=min(mdate.x), end=max(mdate.x)) %>%  
  dplyr::mutate(years.x=lubridate::time_length(difftime(end.x,start.x), 'years'),
                years=lubridate::time_length(difftime(end,start), 'years')) %>%
  dplyr::mutate(end=year(end), start=year(start)) %>% 
  dplyr::distinct(pid, YEAR, end, start, years) %>% 
  ggplot(., aes(YEAR, group=pid)) + geom_bar() + my_theme + 
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))
#check that the record length is realistic
data_sel_join %>% group_by(pid) %>%
  dplyr::mutate(start=min(mdate.x), end=max(mdate.x)) %>%  
  distinct(pid, data_length, start, end, start.x, end.x) %>% 
  dplyr::mutate(years.x=lubridate::time_length(difftime(end.x,start.x), 'years'),
                years=lubridate::time_length(difftime(end,start), 'years'))


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
  unnest() %>%  dplyr::filter(between(year(mdate.x.y), 2000,2010)) %>%
  ggplot(aes(mdate.x.y, VALUE_norm))+geom_line() + 
  facet_wrap(~pid, scales='free')


# get rid of outliers: ----
#I don't know why but viable_meas and viable_count_years code doesn't work 
# properly, count data and years instead, then (try to) remove outliers below

data_sel %>% head()
data_sel <- data_sel %>% group_by(pid) %>%
  dplyr::mutate(start=min(mdate.x.y), end=max(mdate.x.y)) %>%  
  dplyr::mutate(years=lubridate::time_length(difftime(end,start), 'years')) %>%
  filter(years >= 15)

outlrs <- data_sel %>% group_by(pid) %>% 
  dplyr::mutate(sd_m = sd(m.x, na.rm=T),
                                 diff = m.x-lag(m.x, 1),
                                 outlier=(diff > sd_m *4), 
                decade=year(mdate.x.y) - (year(mdate.x.y) %% 5),
                diffs = (diff==0 & lead(diff,1)==0& lag(diff,1)==0))  %>% 
  group_by(pid, diffs, decade) %>% filter((n()>24 & (is.na(diffs)|diffs==T)) |
                                    outlier==T) %>% ungroup() %>%
  distinct(pid) 
med <- data_sel %>% dplyr::filter(!pid %in% outlrs$pid) %>% 
  dplyr::mutate(sd_m = sd(m.x, na.rm=T), 
                decade=year(mdate.x.y) - (year(mdate.x.y) %% 10)) %>%
  group_by(pid, decade) %>% dplyr::filter(n()/12>5) %>% 
  dplyr::mutate(median_d = median(m.x, na.rm=T),
                max_d = max(m.x, na.rm=T),
                min_d = min(m.x, na.rm=T),
                amp_d = max_d-min_d) %>% filter(decade!=2020) %>% 
  group_by(pid) %>% distinct(pid, decade, median_d, amp_d, sd_m) %>%
  dplyr::filter(((median_d - lag(median_d,1)) > amp_d )|
                ((median_d - lag(median_d,1)) < (-amp_d)) |
                ((median_d - lead(median_d,1)) > amp_d) |
                ((median_d - lead(median_d,1)) < (-amp_d)) |
                (median_d > (lag(median_d,1) + amp_d)) |
                (median_d > (lead(median_d,1) + amp_d))  
                ) %>% distinct(pid)


(data_sel %>% dplyr::filter(!pid %in% outlrs$pid & !pid %in% med$pid) %>%
    nest())[c(11:20,130:140,221:230),] %>% 
  unnest() %>%
  #dplyr::filter(between(year(mdate.x.y), 1980,1990)) %>%
  ggplot(aes(mdate.x.y, m.x))+geom_line() + 
  facet_wrap(~pid, scales='free')


#manually remove the rest?
data_sel <- data_sel %>% filter(pid != '23_31' & pid != '16_13' & 
                                  !pid %in% outlrs$pid & !pid %in% med$pid) 


ggmap(stamen_fenn, darken = c(0.6, 'white')) +
  geom_point(data=data_sel %>% left_join(., ts%>% distinct(pid, lon, lat)) %>%
               distinct(pid, lon, lat, years), 
             aes(lon,lat, colour=years), shape=1)  + my_theme + 
  scale_colour_viridis_c() + theme(legend.position='right') + ylab('') + xlab('')

out <- data_sel %>% left_join(., ts%>% distinct(pid, lon, lat)) %>%
  distinct(pid, lon, lat) %>% ungroup() %>% dplyr::filter(!is.na(lon)|!is.na(lat))
sp::coordinates(out)=~lon+lat
proj4string(out) = CRS("+proj=longlat + datum=WGS84")
raster::shapefile(out,'sweden_class/processed/data_sel_15yrs_00-15.shp', overwrite=T)
write_rds(data_sel, "sweden_class/processed/data_sel_15yrs_00-15.rds")


# calculate autocorrelation ----
# data_sel <- read_rds("sweden_class/processed/data_sel_15yrs_filters.rds") #not concurrent
data_sel <- read_rds("sweden_class/processed/data_sel_15yrs_00-15.rds") %>% # concurrent
  dplyr::filter(pid!='3_25')
#do it for SGI, correlate SGI memory to indices
autocorr <- data_sel %>% dplyr::mutate(month=month(mdate.x.y),
                                       year = year(mdate.x.y)) %>%
  dplyr::filter(between(year, 2000,2015)) %>% 
  group_by(pid, year, month) %>% dplyr::mutate(m=median(VALUE_INTP, na.rm=T)) %>%
  group_by(pid, month) %>% 
  dplyr::mutate(sgi = qqnorm(-1*m, plot.it=FALSE)$x) %>% ungroup() %>%
  group_by(pid) %>% distinct(pid, year, month, sgi) %>%
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
  facet_wrap(~pid)
# saveRDS(autocorr, 'sweden_class/processed/autocorr-00-15.rds')
# use code to get logarithmic fit of autocorr, works v poorly
library(broom)
est <- autocorr %>%
  group_by(pid) %>% 
  filter(lag!=0) %>%
  do(log_model=tidy(lm(auto~log(lag), data=.)),
     log_sum = summary(lm(auto~log(lag), data=.)),
     lm_model=tidy(lm(auto~lag, data=.)),
     lm_sum = summary(lm(auto~lag, data=.)))
est$log_p=0; est$lm_p=0
est$log_est=0; est$lm_est=0
est$log_resid=0;est$lm_resid=0
est$log_r.sq = 0;est$lm_r.sq = 0
for(i in 1:length(est$pid)){
  est$log_p[i] = est$log_model[[i]]$p.value[2];
  est$lm_p[i] = est$lm_model[[i]]$p.value[2]
  est$log_est[i] = est$log_model[[i]]$estimate[2]; 
  est$lm_est[i] = est$lm_model[[i]]$estimate[2]
  
  est$log_resid[i] = est$log_sum[[i]]$sigma; 
  est$lm_resid[i] = est$lm_sum[[i]]$sigma
  est$log_r.sq[i] = est$log_sum[[i]]$r.squared; 
  est$lm_r.sq[i] = est$lm_sum[[i]]$r.squared
}
(autocorr %>% nest())[c(2:20,50:110,180:190),] %>% 
  unnest() %>% #dplyr::mutate(lag=ifelse(lag==0, 0.000001, lag)) %>% 
  right_join(est %>% dplyr::select(-lm_model, -log_model, -lm_sum, -log_sum), ., by='pid') %>% 
  group_by(pid) %>%
  dplyr::mutate(a0 = ifelse(lag==1, auto,NA), a0=dplyr::first(na.omit(a0))) %>%
  ggplot(aes(lag, auto))+geom_line() + geom_hline(yintercept=0)+
  geom_line(data=. %>% filter(lag!=0),
            aes(lag, log_est*log(lag)+a0, group=pid, colour=log_r.sq, linetype='log')) +
  geom_line(aes(lag, lm_est*lag+a0, group=pid, colour=lm_r.sq, linetype='lm')) +
  scale_colour_gradient(high='darkgreen', low='red') +
  facet_wrap(~pid) + my_theme



###### from hydro mem, do lm on two slopes
slopes2 <- autocorr %>% group_by(pid) %>%
  mutate(memory= ifelse(auto<0.2, lag-1, NA), 
         memory= ifelse(is.na(memory) & is.na(lead(auto,1)), lag, memory), 
         memory=first(na.omit(memory)),
         dif1 = auto-lag(auto,1),
         dif1_c = ifelse(dif1 < 0.01 & dif1 >-0.01 & lag >3, 0, sign(dif1)),
         cp.auto = ifelse(dif1_c >=0, lag-1, NA), 
         cp.auto=min(first(na.omit(cp.auto)), memory))

library(broom)
slope1 <- slopes2 %>% group_by(pid) %>% 
  filter(lag<=min(memory, cp.auto, na.rm=TRUE)) %>% 
  do(model=tidy(lm(auto~lag, data=.)), sum=summary(lm(auto~lag, data=.)))
for(i in 1:length(slope1$pid)){
  slope1$slope1[i] = slope1$model[[i]]$estimate[2]
  slope1$intercept1[i] = slope1$model[[i]]$estimate[1]
  slope1$r.sq1[i] = slope1$sum[[i]]$r.squared[1]
}

slope2 <- slopes2 %>% group_by(pid) %>%
  filter(lag>=min(memory, cp.auto, na.rm=TRUE)) %>%
  do(model=tidy(lm(auto~lag, data=.)), sum=summary(lm(auto~lag, data=.)))
for(i in 1:length(slope2$pid)){
  slope2$slope2[i] = slope2$model[[i]]$estimate[2]
  slope2$intercept2[i] = slope2$model[[i]]$estimate[1]
  slope2$r.sq2[i] = slope2$sum[[i]]$r.squared[1]
}
slopes2 <- autocorr %>% left_join(., slopes2 %>% distinct(pid, cp.auto)) %>% 
  left_join(., slope1 %>% distinct(pid, slope1, intercept1, r.sq1)) %>% 
  left_join(., slope2 %>% distinct(pid, slope2, intercept2, r.sq2))

#check results in plot:
library(corrplot)
slope2_4 <- (slopes2 %>% nest)[sample(nrow(slopes2 %>% nest()), 3),] %>% unnest() %>%
  full_join(., slopes2 %>% dplyr::filter(pid=='4_2')) %>%
  right_join(est %>% 
               dplyr::select(-lm_model, -log_model, -lm_sum, -log_sum), 
             ., by='pid') %>% 
  group_by(pid) %>%
  dplyr::mutate(a0 = ifelse(lag==1, auto,NA), a0=dplyr::first(na.omit(a0))) 
ids= (slope2_4 %>% dplyr::distinct(pid))$pid %>% paste()

grDevices::cairo_pdf(filename='sweden_class/acf_ex.pdf', width=4, height=3,
                      fallback_resolution = 400)
slope2_4 %>%
  ggplot(aes(x=lag, y=auto)) + 
  geom_line(size=0.5) + #geom_hline(yintercept=0) +
  geom_vline(aes(xintercept=cp.auto, linetype="ACF'", colour="ACF'"), size=0.5) + 
  # geom_text(aes(x=cp.auto, y=Inf, label="ACF'"), vjust=1,hjust=-.5, size=2, aplha=0.5)+
  geom_line(data=. %>% filter(lag!=0),
            aes(lag, log_est*log(lag)+a0, colour='log slope', linetype='log slope'), size=0.8) +
  geom_line(data=.%>% filter(lag<cp.auto+5), 
            aes(y=slope1*lag+intercept1, colour='lm slope 1', linetype='lm slope 1'), size=0.8) +
  geom_line(data=. %>% filter(lag>cp.auto), 
            aes(y=slope2*lag+intercept2, colour='lm slope 2', linetype='lm slope 2'), size=0.8) +
  scale_y_continuous("ACF", limits=c(-0.5,1), expand=c(0.05,0)) + 
  scale_x_continuous(expand=c(0,0), breaks=c(1, 6, 12, 24, 36, 48,60, 72)
                     #trans='log',breaks=c(1, 2, 4,8,16, 32, 64)
                     )+
  
  geom_text(data= data.frame(pid=ids, lab =c('(a)', '(b)', '(c)', '(d)')),
            aes(Inf, Inf, label=lab), vjust=1, hjust=1) + 
  
  scale_linetype_manual('method', 
                        values=c('dotdash', 'dashed','longdash','longdash')) +
  scale_colour_manual('method',values=c('black', COL2('RdBu')[30],
                                        COL2('RdBu')[60],
                                        # COL2('RdBu')[117],
                                        COL2('RdBu')[160])) +
  # scale_colour_viridis_c('r squared', option='cividis', direction=-1) +
  facet_wrap(~pid) + my_theme + theme(strip.text = element_blank(),legend.key.width = unit(2,'mm'),
                                     legend.position = 'right') 
dev.off()



# for Kappa
grDevices::cairo_pdf(filename='sweden_class/memorymeasures.pdf', width=3, height=2,
                     fallback_resolution = 400)
slope2_4 %>% dplyr::filter(pid=='3_2') %>%
  ggplot(aes(x=lag, y=auto)) + 
  geom_line(size=0.5) + #geom_hline(yintercept=0) +
  geom_line(data=.%>% filter(lag<cp.auto+5), 
            aes(y=slope1*lag+intercept1, colour='slope 1', linetype='slope 1'), size=0.8) +
  geom_line(data=. %>% filter(lag>cp.auto), 
            aes(y=slope2*lag+intercept2, colour='slope 2', linetype='slope 2'), size=0.8) +
  
  geom_vline(aes(xintercept=1, linetype="k = 1", colour="k = 1"), size=0.5) + 
  geom_hline(aes(yintercept=0.2, linetype="ACF = 0.2", colour="ACF = 0.2"), size=0.5) + 
  geom_line(data=.%>% filter(lag<cp.auto+5), 
            aes(y=slope1*lag+intercept1, colour='slope 1', linetype='slope 1'), size=0.8) +
  geom_line(data=. %>% filter(lag>cp.auto), 
            aes(y=slope2*lag+intercept2, colour='slope 2', linetype='slope 2'), size=0.8) +
  
  scale_y_continuous("ACF", limits=c(-0.25,1), breaks=c(-0.25,0,0.2,0.5,1), expand=c(0.05,0)) + 
  scale_x_continuous(expand=c(0,0), breaks=c(1, 6, 12, 24, 36, 48,60, 72))+
  scale_linetype_manual('measure', 
                        values=c('longdash','longdash','dotdash', 'dashed')) +
  scale_colour_manual('measure',values=c('grey','grey', COL2('RdBu')[30],
                                        COL2('RdBu')[60],
                                        # COL2('RdBu')[117],
                                        COL2('RdBu')[160])) +
  facet_wrap(~pid) + my_theme + 
  theme(strip.text = element_blank(),legend.key.width = unit(2,'mm'),
        legend.position = 'right')
dev.off()
####

memory <- slopes2 %>% group_by(pid) %>% 
  dplyr::mutate(k02=ifelse(auto>0.2 & (lead(auto,1) < 0.2 | is.na(lead(auto,1))), lag, NA),
                k02 = first(na.omit(k02)),
                k0 = ifelse(auto>0 & (lead(auto,1) < 0 | is.na(lead(auto,1))), lag, NA),
                k0 = first(na.omit(k0))) %>% 
  filter(lag==1) %>% dplyr::select(-lag) %>% 
  distinct() 
write_rds(memory, 'sweden_class/processed/memory-00-15.rds')
# playing with decomposition of data and HURST using time series -----


# attempt to detrend ts using rewriting some code from Ezra


time_series <- data_sel %>% dplyr::rename(mdate=mdate.x.y, m=m.x) #%>% group_by(pid) %>%
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




# run indices: ----

data_sel <- read_rds("sweden_class/processed/data_sel_15yrs_00-15.rds")


ind_wkl <- data_sel %>%
  dplyr::filter(!is.na(VALUE_norm)) %>%
  group_by(pid) %>%
  dplyr::summarise(
  #   # originally open scale - now also standardized
  #   iaf.s = mean(intannfluc(mdate.x.y, VALUE_norm, out = "s")),
  #   iaf.y = intannfluc(mdate.x.y, VALUE_norm, out = "y"),
  #   BLS = baseflow(mdate.x.y,VALUE_norm, out = "BFS", block.len = 3), #
  #   BLI = baseflow(mdate.x.y,VALUE_norm, out = "BFI", block.len = 3), #
  #   l1 = lmom(mdate.x.y, VALUE_norm, out = "l1"),
  #   l2 = lmom(mdate.x.y, VALUE_norm, out = "l2"),
  #   l3 = lmom(mdate.x.y, VALUE_norm, out = "l3"),
  #   l4 = lmom(mdate.x.y, VALUE_norm, out = "l4"),
    amp.max = max(VALUE_INTP, na.rm=T)-min(m.x, na.rm=T),
    iqr = IQR(VALUE_INTP, na.rm=T),
  #   # standardized
  #   bimod = bimodality(VALUE_norm),
  #   #Colwell- Standard value: s = 11(why?) -> changed to s = 12 
  #   #Colwell- Standard value: aggregation = weeks -> changed to month
  #   colwell.C = colwells(mdate.x.y, VALUE_norm, out = "constancy"),
  #   colwell.M = colwells(mdate.x.y, VALUE_norm, out = "contingency"),
  #   colwell.P = colwells(mdate.x.y, VALUE_norm, out = "predictability"),
  #   cvmon.min = cvmonminmax(mdate.x.y, VALUE_norm, out = "min"),
  #   cvmon.max = cvmonminmax(mdate.x.y, VALUE_norm, out = "max"),
  #   # dip = list(diptest::dip.test(VALUE_norm)),#
  #   dc.rng.01.09 = fdcindices(mdate.x.y, VALUE_norm, out = "range", y1 = 0.1, y2 = 0.9),
  #   dc.rng.02.08 = fdcindices(mdate.x.y, VALUE_norm, out = "range", y1 = 0.2, y2 = 0.8),
  #   dc.rng.025.075 = fdcindices(mdate.x.y, VALUE_norm, out = "range", y1 = 0.25, y2 = 0.75),
  #   dc.slp.01.09 = fdcindices(mdate.x.y, VALUE_norm, out = "slope", y1 = 0.1, y2 = 0.9),
  #   dc.slp.02.08 = fdcindices(mdate.x.y, VALUE_norm, out = "slope", y1 = 0.2, y2 = 0.8),
  #   dc.slp.025.075 = fdcindices(mdate.x.y, VALUE_norm, out = "slope", y1 = 0.25, y2 = 0.75),
  #   # med = median(VALUE_norm, na.rm = TRUE),
    med = median(VALUE_INTP, na.rm = TRUE),
  #   parde = parde(mdate.x.y, VALUE_norm, out = "metric"),
  #   # needs higher resolution data, daily
  #   #pulse.count.l = pulses(mdate.x.y, VALUE_norm, out = "count", threshold.type = "low"),
  #   #pulse.count.h = pulses(mdate.x.y, VALUE_norm, out = "count", threshold.type = "high"),
  #   #pulse.dur.l = pulses(mdate.x.y, VALUE_norm, out = "duration", threshold.type = "low"),
  #   #pulse.dur.h = pulses(mdate.x.y, VALUE_norm, out = "duration", threshold.type = "high"),
  #   #pulse.cv.l = pulses(mdate.x.y, VALUE_norm, out = "CV", threshold.type = "low"),
  #   #pulse.cv.h = pulses(mdate.x.y, VALUE_norm, out = "CV", threshold.type = "high"),
  #   #bandwd = silverman(VALUE_norm),
  #   vardoy.min = varjdminmax(mdate.x.y, VALUE_norm, out = "min"), #
  #   vardoy.max = varjdminmax(mdate.x.y, VALUE_norm, out = "max") #
  ) %>% #left_join(., x) %>%
  distinct() %>% 
  left_join(., read_rds('sweden_class/processed/memory-00-15.rds')) %>% distinct()

write_rds(ind_wkl, "sweden_class/processed/indices_monthly_15yrs-00-15.rds")





# PCA ----
rm(list=ls())
ind_wkl <- read_rds("sweden_class/processed/indices_monthly_15yrs-00-15.rds")

# ind_wkl <- ind_wkl %>% 
#   dplyr::select(pid, iaf.s, iaf.y, l4, bimod, colwell.C, colwell.M, colwell.P,
#                dc.rng.01.09, dc.slp.01.09, med, parde, hurst, BLI, BLS, 
#                vardoy.min, vardoy.max, amp.max, iqr, k0, k02, k1, slope, r.sq)

ind_inf <- do.call(data.frame,
                   lapply(ind_wkl, # replacing (one) Inf value with the median value, should not affect PCA
                          function(x) replace(x, is.infinite(x)|is.na(x), median(x, na.rm=T))))
ind_pca <- prcomp(ind_inf[,2:10], #][,2:24], 
                  center = TRUE,scale. = TRUE)


library(ggbiplot);library(factoextra); library(FactoMineR)
ggbiplot(ind_pca)
plot(cumsum(ind_pca$sdev^2 / sum(ind_pca$sdev^2)), type="b", ylim=0:1) # PCA1-5 or 6?

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

data_sel <- read_rds("sweden_class/processed/data_sel_15yrs_filters.rds")
data_sel %>% 
  # dplyr::filter(pid=='11_12' | pid=='11_33' | pid=="11_6" | pid=='11_1') %>%
  dplyr::filter(pid=='23_3' | pid=='72_11' | pid=="53_3" | pid=='83_3') %>%
  ggplot(., aes(mdate.x.y, m.x, group=pid)) +
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
corrplot(round(cor(ind_inf[, c('slope1', 'slope2', 'k02', 'intercept1')]),2), 
         type='upper', order='hclust', method='ellipse',
         tl.col='black', tl.srt=45)
library("PerformanceAnalytics")
chart.Correlation(ind_inf[, 2:24], 
                  histogram=TRUE, pch=19)

library("FactoMineR")
fviz_pca_ind(ind_pca, col.ind="cos2") +
  scale_color_gradient2(low="blue", mid="white", 
                        high="red", midpoint=0.50)
---
  # PREPARE for clustering  ----

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



ind_wkl <- read_rds("sweden_class/processed/indices_monthly_15yrs-00-15.rds")
ind_inf <- do.call(data.frame,
                   lapply(ind_wkl, # replacing (one) Inf value with the median value, should not affect PCA
                          function(x) replace(x, is.infinite(x)|is.na(x), median(x, na.rm=T))))
all_indices <- ind_inf

#scale and as matrix
# z_indicies <- scale(all_indicies[2:39])
z_indices <- #scale(all_indices[2:37])#[c(1:150),]
  scale(all_indices[2:14])


# z_test <- scale(all_indicies["iaf.s"])

z_frame = as.data.frame(z_indices)

z_ind <- cbind(all_indices$pid, z_frame)
# z_ind <- cbind(all_indices[c(1:150),]$pid, z_frame)

names(z_ind)[names(z_ind) == 'all_indices$pid'] <- 'pid'

#preparing clustering

x <- z_ind

data_sel <- read_rds("sweden_class/processed/data_sel_15yrs_00-15.rds")

sgi <- data_sel %>% #read_rds("sweden_class/processed/data_sel_15yrs_filters.rds") %>% 
  dplyr::select(mdate.x.y, pid, VALUE_INTP) %>% dplyr::rename(mdate = mdate.x.y) %>%
  dplyr::mutate(month=lubridate::month(mdate), year=lubridate::year(mdate)) %>%
  group_by(pid, year, month) %>% dplyr::mutate(m=median(VALUE_INTP, na.rm=T)) %>%
  group_by(pid, month) %>% 
  dplyr::mutate(sgi = qqnorm(-1*m, plot.it=FALSE)$x,
                mdate=as.Date(paste(year, month, '15', sep='-'))) %>% ungroup() %>%
  group_by(pid) %>% dplyr::distinct(mdate, pid, sgi)

time_series_sub <- subset(sgi, select =c("pid","mdate", "sgi"))#"m"))

time_series <- time_series_sub %>% 
  mutate(id2 = as.character(pid))


saveRDS(z_ind, 'sweden_class/processed/z_ind_15yrs_00-15.rds')
# index-based clustering ------------------------------------------------------
dfind_to_mat = function (x) {
  library(tibble)
  x %>% 
    column_to_rownames(var= "pid") %>% 
    as.matrix
}

z_ind <- readRDS('sweden_class/processed/z_ind_15yrs_00-15.rds') %>%
  dplyr::filter(pid %in% meta$pid)

# selct indices to cluster
# ind_sel <- c(
#   "iaf.s",
#   "vardoy.min",
#   "rev.avg", "BLI",
#   "autocor", "hurst",
#   "recov.const", "reces.const",
#   "avg.ann.max",
#   "peakts.avg", 
#   "bimod",
#   "nonlinear")
# 
# ind_sel2 <- c(
#   "iaf.y",
#   "rise.cv",
#   "rev.avg",
#   "autocor", "hurst",
#   "recov.const",
#   "peakbase.cv",
#   "pulse.dur.h", 
#   "pulse.count.h",
#   "nonlinear")
# 
# ind_sel3 <- c(
#   "hurst", 
#   "autocor",
#   "pulse.count.l",
#   "pulse.count.h",
#   "iaf.y"
# )
ind_sel_mem <- c(
   "slope2",
  "slope1",
  "auto",
  "k02"
  # "k0"
)

library(dendextend)
ind_series <- z_ind %>% 
  dplyr::select(pid, one_of(ind_sel_mem))  %>%
  nest(data = everything()) %>% 
  dplyr::mutate(
    mat       = map(data, dfind_to_mat),
    man_diss  = map(mat, dist, method = "manhattan"),
    euc_diss  = map(mat, dist, method = "euclidean"),
    man_clust = map(man_diss, hclust, method = "ward.D2") %>% 
      map(as.dendrogram),
    euc_clust = map(euc_diss, hclust, method = "ward.D2") %>% 
      map(as.dendrogram),
    man_cut  = map(man_clust, cutree, k=4),
    euc_cut  = map(euc_clust, cutree, k=4),
    dend = map(euc_clust, as.dendrogram) %>%
      map(., dendextend::color_branches, k=4),
    cutdend = map(dend, cutree, 4) %>%
      
    #   man_cut  = map(man_clust, cutree, k=6),
    # euc_cut  = map(euc_clust, cutree, k=6),
    # dend = map(euc_clust, as.dendrogram) %>% 
    #   map(., dendextend::color_branches, k=6),
    # cutdend = map(dend, cutree, 6) %>%
      map(., stack)
  )

### testing the clustering : https://towardsdatascience.com/10-tips-for-choosing-the-optimal-number-of-clusters-277e93d72d92 ----
kmean_calc <- function(df, ...){
  library(factoextra)
  # kmeans(df, scaled = ..., nstart = 30)
  eclust(df, FUNcluster='hclust', hc_method='ward.D2', scaled=..., nstart=30)
}
km2 <- kmean_calc(ind_series$euc_diss[[1]], 2)
km3 <- kmean_calc(ind_series$euc_diss[[1]], 3)
km4 <- kmeans(ind_series$euc_diss[[1]], 4)
km5 <- kmeans(ind_series$euc_diss[[1]], 5)
km6 <- kmeans(ind_series$euc_diss[[1]], 6)
km7 <- kmeans(ind_series$euc_diss[[1]], 7)
km8 <- kmeans(ind_series$euc_diss[[1]], 8)
km9 <- kmeans(ind_series$euc_diss[[1]], 9)
km10 <- kmeans(ind_series$euc_diss[[1]], 10)
km11 <- kmeans(ind_series$euc_diss[[1]], 11)
p1 <- fviz_cluster(km2, data = ind_series$euc_diss[[1]], frame.type = "convex") + theme_minimal() + ggtitle("k = 2") 
p2 <- fviz_cluster(km3, data = ind_series$euc_diss[[1]], frame.type = "convex") + theme_minimal() + ggtitle("k = 3")
p3 <- fviz_cluster(km4, data = ind_series$euc_diss[[1]], frame.type = "convex") + theme_minimal() + ggtitle("k = 4")
p4 <- fviz_cluster(km5, data = ind_series$euc_diss[[1]], frame.type = "convex") + theme_minimal() + ggtitle("k = 5")
p5 <- fviz_cluster(km6, data = ind_series$euc_diss[[1]], frame.type = "convex") + theme_minimal() + ggtitle("k = 6")
p6 <- fviz_cluster(km7, data = ind_series$euc_diss[[1]], frame.type = "convex") + theme_minimal() + ggtitle("k = 7")
plot_grid(p1, p2, p3, p4, p5, p6) #, labels = c("k2", "k3", "k4", "k5", "k6", "k7"))

set.seed(31)
# function to compute total within-cluster sum of squares
fviz_nbclust(ind_series$mat[[1]], kmeans, method = "wss", k.max = 24) + 
  theme_minimal() + ggtitle("the Elbow Method")

######



dend = as.dendrogram(ind_series$dend[[1]])
labels_colors(dend) <- 'white'
#highlight_branches_col(dend)
# plot(dend)
library(ggsci); library(viridis); library(Cairo)
grDevices::cairo_pdf(filename='sweden_class/dendrogram.pdf', width=5, height=3,
                     fallback_resolution = 400)
# dend %>% highlight_branches_col(rev(pal_tron(palette = c("legacy"),alpha = 1)(4))) %>%
#   plot(horiz=T)
  # plot()
# dend %>% highlight_branches_col(rev(magma(4))) %>% 
  # plot(horiz=T)
plot(dend, horiz=T)
dend %>% rect.dendrogram(k=4, horiz = TRUE,
                           border = 9, lty = 5, lwd = 0.1)

dev.off()
# dend <- dendextend::color_branches(dend, k = 4)
# 
# cutdend <- cutree(dend, 4)
# table(cutdend)
# cutdend <- stack(cutdend) %>% 
#   as.data.frame() %>%
#   dplyr::rename(euc_clust = values, pid=ind)

cutdend <- ind_series %>% dplyr::select(cutdend) %>% unnest() %>% 
  dplyr::rename(euc_clust=values, pid=ind)


clust_meta <- meta %>% 
  right_join(., cutdend) %>% distinct() %>%
   right_join(., ind_series$mat[[1]][,1] %>% stack(.) %>% as.data.frame() %>%
                dplyr::rename(sgi = values, pid=ind)) %>%
  dplyr::mutate(rel_memory = ifelse(euc_clust==1, 3, NA),
                rel_memory = ifelse(euc_clust==2, euc_clust, rel_memory),
                rel_memory = ifelse(euc_clust==3, 1, rel_memory),
                rel_memory = ifelse(euc_clust==4, euc_clust, rel_memory),
                euc_clust=rel_memory) %>% select(-rel_memory)
# write_rds(clust_meta, 'sweden_class/input data/meta_4clusts-00-15.rds')

# clust_meta <- read_rds('sweden_class/input data/meta_4clusts-00-15.rds')
table(clust_meta$euc_clust, clust_meta$group)
library(ggsci); clust_meta %>% ungroup() %>% group_by(euc_clust, group) %>% # all
  dplyr::mutate(number=n()) %>% ungroup() %>% group_by(group) %>% # by what you want to group/compare between
  dplyr::mutate(sum_bgroup=round(number/n(), 2)) %>% 
  ungroup() %>% group_by(euc_clust) %>% 
  dplyr::mutate(sum_bclust=round(number/n(),2)) %>% 
  ggplot(., aes(x=as.factor(euc_clust), fill=group, label = sum_bgroup*100)) + 
  # geom_boxplot(aes(x=as.factor(euc_clust), y=group, colour=sum_bgroup)) +
  # geom_text(aes(x=as.factor(euc_clust), y=group, label=sum_bclust, 
  #               colour=sum_bclust), alpha=0.5, show.legend=T) +
  geom_bar(position='fill') +
  stat_count(geom = "text", size = 2.5, 
             position=position_fill(vjust=0.5)) + 
  # scale_fill_viridis_d("") +
  # scale_colour_viridis_c("fraction by cluster", option='inferno', 
                         # direction=-1, limits=c(0,1)) +
  scale_fill_tron(name='') + 
  scale_y_discrete(expand=c(0,0)) + #, position='right', title='') + 
  scale_x_discrete('cluster', expand=c(0,0)) +
  # scale_fill_gradient2("fraction by setting", low="#00AFBB", mid="#E7B800", high="#FC4E07",
                       # midpoint=0.5, limits=c(0,1)) +
  theme(axis.text.x = element_text(angle=90)) + my_theme + 
  theme(#legend.position = 'right', 
        text = element_text(size=8), 
        panel.grid = element_line(colour=NA))

clust_meas <- sgi %>% right_join(., clust_meta %>% distinct(pid, group, euc_clust)) %>% 
  ungroup() %>% group_by(euc_clust, mdate) %>%
  dplyr::filter(between(year(mdate), 2000, 2015)) %>% 
  dplyr::mutate(sgi_m=median(sgi, na.rm=T),
                sgi_min=min(sgi, na.rm=T),
                sgi_max=max(sgi, na.rm=T)) %>%
  distinct(mdate, euc_clust, sgi_m, sgi_min, sgi_max) 
drought <- clust_meas %>% group_by(euc_clust, spell = data.table::rleid(sgi_m<0)) %>% 
  dplyr::mutate(drought =  ifelse(sgi_m<0, row_number(), 0L),
                drought = last(drought)) %>% 
  arrange(euc_clust, mdate) %>% dplyr::mutate(start=first(mdate)) %>%
  ungroup() %>% 
  group_by(euc_clust, drought, start) %>% dplyr::mutate(severity=min(sgi_m))


library(ggsci); library(viridis); library(Cairo)
# grDevices::cairo_pdf(filename='sweden_class/sgi.pdf', width=5, height=4,
                     # fallback_resolution = 400)
sgi.plot <- clust_meas %>%
  ggplot(., aes(x=mdate)) +
  geom_ribbon(aes(ymin=sgi_min, ymax=sgi_max), fill='black', alpha=0.1) +
  geom_line(aes(y=sgi_m)) +
  geom_tile(data=. %>% dplyr::filter(sgi_m < 0), aes(y=-2.1, fill=sgi_m), height=0.2) +
  facet_grid(rows=vars(euc_clust), scales='free') + my_theme +
  scale_y_continuous("SGI", expand=c(0,0)) + 
  scale_x_date("", expand=c(0,0), date_breaks = '1 year', date_labels='%Y') +
  geom_hline(yintercept=0, linetype='dashed') +
  geom_text(data=data.frame(euc_clust= c(1,2,3,4),
                            lab = c('(a)', '(b)', '(c)', '(d)')),
            aes(as.Date('2015-12-15'), Inf, label=lab), vjust=1, hjust=1) +
  scale_fill_viridis_c('SGI',option='magma', end=0.7) + 
  theme(legend.position='right', legend.key.width = unit(0.2, 'cm'), 
        legend.key.height=unit(1, 'cm')) 
# dev.off()

library(ggsci); ggmap(stamen_fenn, darken = c(0.6, 'white')) + 
  geom_point(data=ts%>% distinct(pid, lon,lat) %>% 
               right_join(., clust_meta), 
               aes(lon,lat, colour=as.factor(euc_clust)), shape=21, fill=NA, size=2) +
  scale_color_viridis_d("")
  # scale_colour_gradient2(low="#00AFBB", mid="#E7B800", high="#FC4E07",
  #                        midpoint=3.5)


# connect to indices, stats --------------------------------------------
# ACF plot per cluster
autocorr <- read_rds('sweden_class/processed/autocorr-00-15.rds')
library(ggsci); library(viridis); library(Cairo)
# grDevices::cairo_pdf(filename='sweden_class/acf.pdf', width=4, height=4,
#                      fallback_resolution = 400)
acf.plot <- autocorr %>% unnest() %>% 
  right_join(., clust_meta) %>% 
  # right_join(., read_rds('sweden_class/input data/meta_4clusts-00-15.rds')) %>% 
  ungroup() %>% 
  group_by(euc_clust, 
           lag) %>% #group
  dplyr::mutate(sgi_m=median(auto, na.rm=T),
                sgi_min=min(auto, na.rm=T),
                sgi_max=max(auto, na.rm=T)) %>%
  distinct(lag, euc_clust, sgi_m, sgi_min, sgi_max) %>% 
  ggplot(., aes(x=lag)) + 
  geom_ribbon(aes(ymin=sgi_min, ymax=sgi_max), alpha=0.2) + #fill_group
  geom_line(aes(y=sgi_m)) + # colour=group
  
  geom_vline(aes(xintercept=1, linetype='k=1')) + 
  geom_hline(aes(yintercept=0.2, linetype='ACF=0.2')) + 
  geom_hline(aes(yintercept=0, linetype='ACF=0')) + 
  # geom_text(aes(x =1, y=Inf, label='k=1', colour='k=1', fontface='plain'), alpha=0.5, 
  #           vjust=1, hjust=-0.1, size=3) + 
  # geom_text(aes(x =Inf, y=0.2, label='ACF=0.2', colour='ACF=0.2', fontface='plain'), alpha=0.5, 
  #           vjust=-0.1, hjust=1, size=2) + 
  # geom_text(aes(x=Inf, y=0, label='ACF=0', colour='ACF=0', fontface='plain'), alpha=0.5, 
  #           vjust=-0.2, hjust=1, size=2) + 
  
  # scale_color_viridis_d("", option='cividis', end=0.8, na.value='lightpink') + 
  # scale_fill_viridis_d("", option='cividis', end=0.8, na.value='lightpink') +
  my_theme + #scale_fill_tron(name='') + scale_colour_tron(name='') + 
  scale_linetype_manual('', values=c('dashed','dotted','longdash')) +
  # guides(colour=guide_colorbar('none')) + 
  facet_grid(rows=vars(euc_clust)) +
  # facet_grid(rows=vars(group), cols=vars(euc_clust)) + 
  my_theme + 
  scale_y_continuous("ACF", expand=c(0,0)) + 
  scale_x_continuous("lag", expand=c(0,0), breaks=c(1, 12, 24, 36, 48, 60, 72)) +
  geom_text(data=data.frame(euc_clust= c('1', '2', '3', '4'),
                            lab = c('(a)', '(b)', '(c)', '(d)')),
            aes(Inf, Inf, label=lab), vjust=1, hjust=1) 
# dev.off()
# autocorr %>% unnest() %>% right_join(., clust_meta) %>% ungroup() %>%
#   distinct(pid, euc_clust, group) %>% group_by(euc_clust, group) %>%
#   dplyr::summarise(n=n())%>%
#   write.csv(., 'Q:/MICHELLE_PHD/Nordic Drought/Paper_swedenclass/tables_sgi2.csv')

library(cowplot)
plot.sa <- plot_grid(sgi.plot + theme(legend.position='none', strip.text = element_blank()),
          acf.plot+ theme(legend.position='none'),
          align='hv', axis='lr', ncol=2, rel_widths=c(2,0.8))
grDevices::cairo_pdf(filename='sweden_class/acf+sgi.pdf', width=7, height=4,
                     fallback_resolution = 400)
plot_grid(plot.sa, get_legend(acf.plot+theme(legend.position = 'bottom')),
          get_legend(sgi.plot+theme(legend.position='bottom', 
                                    legend.key.width = unit(1, 'cm'), 
                                    legend.key.height=unit(0.3, 'cm')) 
                       # guides(fill=guide_legend(override.aes=list(colour='black')))
                     ),
          ncol=1, rel_heights=c(2,0.2,0.2))
dev.off()
# do significance test,
# memory
ind_wkl <- read_rds("sweden_class/processed/indices_monthly_15yrs-00-15.rds")
cutdend
stats <- ind_wkl %>% right_join(., clust_meta) %>% dplyr::rename(k1=auto) %>%
  dplyr::mutate(slope1_z = as.numeric(
    (slope1 - mean(slope1, na.rm=T))/sd(slope1, na.rm=T)),
    slope2_z = as.numeric(
      (slope2 - mean(slope2, na.rm=T))/sd(slope2, na.rm=T)),
    k1_z = as.numeric(
      (k1 - mean(k1, na.rm=T))/sd(k1, na.rm=T)),
    k02_z = as.numeric(
      (k02 - mean(k02, na.rm=T))/sd(k02, na.rm=T)),
    slope2_z = as.numeric(
      (slope2 - mean(slope2, na.rm=T))/sd(slope2, na.rm=T)))
  # left_join(., cutdend) %>% dplyr::rename(k1=auto) #%>% 
  # left_join(., meta %>% distinct(pid, group))
# library(ggsci)
order=c('ACF at k = 1 month','k (months) at ACF = 0.2','slope 1 (ACF/lag)','slope 2 (ACF/lag)')

library(Cairo); library(ggplot2); library(RColorBrewer)
grDevices::cairo_pdf(filename='sweden_class/mem_est_boxplots.pdf', width=4, height=4,
                     fallback_resolution = 400)
stats %>% dplyr::mutate(euc_clust = factor(euc_clust, levels=c(1,2,3,4))) %>% 
  group_by(euc_clust) %>% 
  ggplot(., aes(x=euc_clust)) + 
  geom_boxplot(data=. %>% dplyr::mutate(mm=factor('ACF at k = 1 month', 
                                                 levels=order)),
              aes(y=k1), size=0.2) +
  geom_boxplot(data=. %>% dplyr::mutate(mm=factor('slope 1 (ACF/lag)', 
                                                  levels=order)),
               aes(y=slope1)) +
  geom_boxplot(data=. %>% dplyr::mutate(mm=factor('slope 2 (ACF/lag)', 
                                       levels=order)),
              aes(y=slope2)) + 
  geom_boxplot(data=. %>% dplyr::mutate(mm=factor('k (months) at ACF = 0.2', 
                                       levels=order)),
              aes(y=k02)) + 
  geom_text(data=data.frame(mm=factor(order, levels=order),
                            lab = c('(a)', '(b)', '(c)', '(d)')),
            aes(Inf, Inf, label=lab), vjust=1, hjust=1) + 
  my_theme + #scale_fill_tron(name='') + scale_colour_tron(name='') + 
  scale_x_discrete('cluster', expand=c(0.1,0.1)) +
  scale_y_continuous('') + #, trans='log',
  #                    breaks=c(0.1, 0.3, 0.6,0.9),
  #                    sec.axis = sec_axis('k estimate', trans=~.*100,
  #                                        breaks=c(1, 3, 6, 12, 24, 36, 48, 60, 72, 80)))  +
  facet_wrap(.~mm, scales='free')
dev.off()

# connect cluster to indices
stats <- stats %>% dplyr::select(-amp.max)

# cross-corr on indices----
library(rlang)
correlation_fun <- function(data=., x, y){
  data= data %>% distinct(pid, !! sym(x), !! sym(y))
  # data = as.matrix(data[colnames(data[2:length(data)])]) %>% scale(.) %>%
  #   as.data.frame()
  result = cor.test(data[[x]], data[[y]], method='spearman')
  data.frame(x, y, #groups, 
             result[c("estimate","p.value","statistic","method")], 
             stringsAsFactors=TRUE)
}
mem_est=c("slope1", "slope2", "k1", "k02")
# vars = c('iaf.s', 'iaf.y', 'BLS', 'BLI', 'vardoy.min', 'vardoy.max', 'hurst',
#          'iqr', 'l4', 'bimod', 'colwell.P', 'colwell.M', 'cvmon.min', 'cvmon.max',
# 'dc.rng.01.09', 'dc.slp.01.09', 'med', 'parde')

library(gridExtra); library(grid); library(ggpmisc)
# stats %>%
#   pivot_longer(cols=all_of(c('parde', 'BLI', 'BLS', 'iaf.y', 'iaf.s', 'hurst')), 
#                names_to='index', values_to='value') %>%
#     ggplot(., aes(x=as.factor(euc_clust), y=value, fill=group)) + 
#   geom_boxplot() + facet_wrap(~index, scales='free') + 
#   scale_x_discrete('cluster', expand=c(0,0)) + 
#   scale_y_continuous('index value', expand=c(0,0)) +
#   scale_fill_tron(name='') + my_theme
# stats %>%
#   # dplyr::mutate(estimate=ifelse(p.value>0.05 | estimate==0, NA, estimate)) %>% 
#   # group_by(euc_clust, group, index) %>% 
#   # dplyr::mutate(min = min(value, na.rm=T), max=max(value))
#   # pivot_longer(cols=all_of(mem_est), names_to='mem_est', values_to='est') %>%
#   ggplot(., aes(x=as.factor(euc_clust), y=value, fill=group)) + 
#   # geom_tile(data=. %>% dplyr::filter(p.value<0.05), aes(fill=est))+
#   # geom_tile(aes(), col='black')+
#   # scale_fill_gradient2(limits=c(-1,1), na.value='white', midpoint = 0,
#   #                      high="#00AFBB", mid="white", low="#FC4E07") + 
#   # scale_colour_manual(values=c("black")) +
#   # scale_x_discrete(expand=c(0,0)) +
#   # scale_y_discrete(expand=c(0,0)) +
#   # guides(colour= "none") + my_theme
#   geom_point() + geom_smooth(method='lm', level=0)+ 
#   stat_fit_glance(method = 'lm',
#                   method.args = list(formula = y~x),
#                   geom = 'text_npc',
#                   mapping = aes(label = sprintf('r^2=%.3f\np=%.2g',
#                                                 after_stat(r.squared), 
#                                                 after_stat(p.value)),
#                                 colour='black'),
#                   label.x = "right", label.y = "bottom", size = 3) + 
#   facet_grid(rows=vars(euc_clust), cols=vars(mem_est), scales='free')

gather_fun <- function(data=stats){
  result = do.call(rbind, mapply(correlation_fun, x=mem_est[1], y=vars,
                                     MoreArgs=list(data=data),
                                     SIMPLIFY=FALSE)) %>%
    full_join(., do.call(rbind, mapply(correlation_fun, x=mem_est[2], y=vars,
                                       MoreArgs=list(data=data),
                                       SIMPLIFY=FALSE))) %>%
    full_join(., do.call(rbind, mapply(correlation_fun, x=mem_est[3], y=vars,
                                       MoreArgs=list(data=data),
                                       SIMPLIFY=FALSE))) %>%
    full_join(., do.call(rbind, mapply(correlation_fun, x=mem_est[4], y=vars,
                                       MoreArgs=list(data=data),
                                       SIMPLIFY=FALSE)))
  return(result)
}
reordered_stats <- stats %>% dplyr::mutate(rel_memory=euc_clust) 
  # dplyr::mutate(rel_memory = ifelse(euc_clust==1,3,NA),
  #               rel_memory = ifelse(euc_clust==2,2,rel_memory),
  #               rel_memory = ifelse(euc_clust==3,1,rel_memory),
  #               rel_memory = ifelse(euc_clust==4,4,rel_memory))
test1 = reordered_stats %>% gather_fun(data=.) %>% as.data.frame() %>% 
  group_by(x) %>%
  dplyr::mutate(x= as.character(x), x=ifelse(x=='k02', 'ACF02', x), 
                type='all') %>%
  
  # full_join(., gather_fun(data=reordered_stats %>% filter(rel_memory==1))%>% #group=='till')
  # as.data.frame() %>% group_by(x) %>%
  # dplyr::mutate(x= as.character(x), x=ifelse(x=='k02', 'ACF02', x), 
  #               type='1')) %>% 
  # 
  # full_join(., gather_fun(data=reordered_stats %>% filter(rel_memory==2)) %>% 
  # as.data.frame() %>% group_by(x) %>% 
  # dplyr::mutate(x= as.character(x), x=ifelse(x=='k02', 'ACF02', x), 
  #               type='2')) %>% 
  # 
  # full_join(., gather_fun(data=reordered_stats %>% filter(rel_memory==3)) %>% 
  #             as.data.frame() %>% group_by(x) %>% 
  #             dplyr::mutate(x= as.character(x), x=ifelse(x=='k02', 'ACF02', x), 
  #                           type='3')) %>% 
  # 
  # full_join(., gather_fun(data=reordered_stats %>% filter(rel_memory==4)) %>% 
  #             as.data.frame() %>% group_by(x) %>% 
  #             dplyr::mutate(x= as.character(x), x=ifelse(x=='k02', 'ACF02', x), 
  #                           type='4')) %>% 
  
  dplyr::mutate(x = factor(x, levels=c('k1', 'ACF02', 'slope1', 'slope2', 'cluster')))

# test2 = do.call(rbind, mapply(correlation_fun, x='rel_memory', y=vars,  
#                                   MoreArgs=list(data=reordered_stats), 
#                                   SIMPLIFY=FALSE)) %>% as.data.frame() %>% 
#               dplyr::mutate(x=ifelse(x=='rel_memory', 'cluster', x), type='all') %>% 
#   
#   full_join(., 
#             do.call(rbind, mapply(correlation_fun, x='rel_memory', y=vars,  
#                                   MoreArgs=list(data=reordered_stats %>% filter(group=='till')), 
#                                   SIMPLIFY=FALSE)) %>% as.data.frame() %>% 
#               dplyr::mutate(x=ifelse(x=='rel_memory', 'cluster', x))%>% 
#               as.data.frame() %>% group_by(x) %>% 
#               dplyr::mutate(x= as.character(x), x=ifelse(x=='k02', 'ACF02', x),
#                             type='till')) %>% 
#   
#   full_join(., 
#             do.call(rbind, mapply(correlation_fun, x='rel_memory', y=vars,  
#                                   MoreArgs=list(data=reordered_stats %>% filter(group=='unconfined sand')), 
#                                   SIMPLIFY=FALSE)) %>% as.data.frame() %>% 
#               dplyr::mutate(x=ifelse(x=='rel_memory', 'cluster', x))%>% 
#               as.data.frame() %>% group_by(x) %>% 
#               dplyr::mutate(x= as.character(x), x=ifelse(x=='k02', 'ACF02', x),
#                             type='unconfined sand')) %>% 
#   
#   dplyr::mutate(x = factor(x, levels=c('k1', 'ACF02', 'slope1', 'slope2', 'cluster')))
# stats_test <- full_join(test1, test2)
stats_test <- test1

stats_test %>% dplyr::mutate(type=ifelse(is.na(type), 'all', type)) %>%
  ggplot(.) + 
  geom_tile(data=. %>% dplyr::mutate(estimate=ifelse(p.value>0.05 | estimate == 0,
                                                     NA, estimate)), 
            aes(x,y, fill=estimate)) +
  scale_fill_gradient2("Sparman's rho", limits=c(-1,1), na.value='white', midpoint = 0,
                       high="#00AFBB", mid="white", low="#FC4E07") + 
  scale_x_discrete('',expand=c(0,0)) + 
  scale_y_discrete('', expand=c(0,0)) +
  guides(colour= "none") + my_theme + 
  theme(panel.grid = element_line(colour='white'), #legend.position='none',
        axis.text.x = element_text(angle=90)) + facet_grid(cols=vars(type))

library(cowplot)
plot_grid(a + theme(legend.position='none'), 
          get_legend(a), rel_heights=c(2,0.15), ncol=1)
# plotting and diagnosis -------------------------
# plot ts and clusters ------

ts_cluster <- ts %>% 
  ungroup %>% 
  left_join(., tibble(
    pid = names(ind_series$man_cut[[1]]), 
    c_ind_man = as.character(ind_series$man_cut[[1]])), by = "pid"
  ) %>% 
  left_join(., tibble(
    pid = names(ind_series$euc_cut[[1]]), 
    c_ind_euc = as.character(ind_series$euc_cut[[1]])), by = "pid"
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
  aes(mdate, m, colour = c_ind_man) + 
  labs(title = "index-based / manhattan distance") +
  geom_line(aes(group = pid), size = .2) + 
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


# cross-corr environmental properties ----

comb <- read_rds('sweden_class/processed/descriptors/combined-test.rds') %>%
    dplyr::mutate(qws_ff= qws_ff/qws_yr, 
                pws_ff=pws_ff/pws_yr,
                q_ff=q_ff/q_yr, 
                p_ff=p_ff/p_yr, 
                t_ff=t_ff/t_yr,
                qws_frost=qws_frost/qws_yr, 
                pws_frost=pws_frost/pws_yr,
                q_frost=q_frost/q_yr, 
                p_frost=p_frost/pws_yr,
                t_frost=t_frost/t_yr,)
comb1 <- read_rds('sweden_class/processed/descriptors/combined-test-lu1km.rds')
ind_wkl <- read_rds("sweden_class/processed/indices_monthly_15yrs-00-15.rds")
ts<- read_rds('sweden_class/input data/ts.rds') %>% group_by(pid) %>% 
  filter(!is.na(m) & !is.na(level_masl)) %>% dplyr::filter(pid %in% ind_wkl$pid)
meta <- read_rds('sweden_class/input data/meta_4clusts-00-15.rds') %>% 
  left_join(., ts %>% dplyr::distinct(pid, lon, lat), by='pid') 
comb <- ind_wkl %>% left_join(., comb, by='pid') %>% 
  left_join(., comb1 %>% dplyr::rename(sd1 = stream_density, forest1=forest, fields1=fields, water1=water) %>% 
              dplyr::select(-dist.stream, -stream_l), by='pid') %>% 
  left_join(., meta, by='pid') %>%
  dplyr::filter(pid!='3_25')

reordered <- comb %>% dplyr::mutate(cluster=euc_clust) %>% 
  dplyr::mutate(slope1_z = as.numeric(
    (slope1 - mean(slope1, na.rm=T))/sd(slope1, na.rm=T)),
                slope2_z = as.numeric(
                  (slope2 - mean(slope2, na.rm=T))/sd(slope2, na.rm=T)),
                auto_z = as.numeric(
                  (auto - mean(auto, na.rm=T))/sd(auto, na.rm=T)),
                k02_z = as.numeric(
                  (k02 - mean(k02, na.rm=T))/sd(k02, na.rm=T)),
                cluster_z = as.numeric(
                  (cluster - mean(cluster, na.rm=T))/sd(cluster, na.rm=T)),
  # dplyr::mutate(cluster = ifelse(euc_clust==1,3,NA),
  #               cluster = ifelse(euc_clust==2,2,cluster),
  #               cluster = ifelse(euc_clust==3,4,cluster),
  #               cluster = ifelse(euc_clust==4,1,cluster),
                stream.density=as.numeric(stream_density),
                elevation=as.numeric(elevation),
                slope1 = ifelse(is.na(slope1), -1, slope1),
                aq_thick = soildepth-med,
                'T' = logK*aq_thick
  ) %>% as.data.frame()

corfun <- function(data=., x, y, use=...){
  library(rlang)
  data= data %>% distinct(pid, !! rlang::sym(x), !! rlang::sym(y))
  result = cor.test(data[[x]] %>% as.vector(mode='numeric'), 
                    data[[y]] %>% as.vector(mode='numeric'), 
                    na.action=na.rm, 
                    method=use)
  data.frame(x, y, result[c("estimate","p.value","statistic","method")], 
             stringsAsFactors=TRUE)
}
gather_fun <- function(data, vars=..., use='spearman'){
  result = do.call(rbind, mapply(corfun, x=mem_est[1], y= vars, use=use,
                                 MoreArgs=list(data=data),
                                 SIMPLIFY=FALSE)) %>%
    full_join(., do.call(rbind, mapply(corfun, x=mem_est[2], y= vars, use=use,
                                       MoreArgs=list(data=data),
                                       SIMPLIFY=FALSE))) %>%
    full_join(., do.call(rbind, mapply(corfun, x=mem_est[3], y= vars, use=use,
                                       MoreArgs=list(data=data),
                                       SIMPLIFY=FALSE))) #%>%
    # full_join(., do.call(rbind, mapply(corfun, x=mem_est[4], y= vars, use=use,
    #                                    MoreArgs=list(data=data),
    #                                    SIMPLIFY=FALSE))) #%>%
    # full_join(., do.call(rbind, mapply(corfun, x=mem_est[5], y= vars, use=use,
    #                                    MoreArgs=list(data=data),
    #                                    SIMPLIFY=FALSE)))
  return(result)
}
# # mem measures not normalised
# mem_est=c("slope1", #"slope2",
#           "auto", "k02", 'cluster')
# mem measures normalised to z-score
mem_est=c("slope1_z", #"slope2", 
          "auto_z", "k02_z"#, 'cluster_z'
          )

varsloc = c(#'elevation', #'welldep', 'lon', 'lat', 
         'iqr', 'med', 'water', 'forest', 'fields',
         'stream_density', #'aq_thick', 
         'soildepth', #'T',
         # 'KS2', 'KS3', 
         'KS4','KS5', 'KS6', 'KS7', 'logK',
         'sd1', 'forest1', 'fields1','water1'
         )
varsclim = c(#'q_DJF', 't_DJF', 'qws_DJF', 'q_SON', 't_SON', 'qws_SON',
             #'q_JJA', 't_JJA', 'qws_JJA', 'q_MAM', 't_MAM', 'qws_MAM',
              #'p_DJF', 'pws_DJF', 'p_SON', 'pws_SON', 'p_JJA', 'pws_JJA',
              #'p_MAM', 'pws_MAM',
             # 'q_yr', 'qws_yr',
  't_yr', 'p_yr', 'pws_yr',
  'q_ff', 't_ff', 'qws_ff','p_ff', 'pws_ff', 'q_frost', 't_frost', 
  'pws_frost','qws_frost', 'p_frost'
             )
# name='spearman'
name='pearson'
library(r2symbols)
test.p <- reordered  %>% 
  gather_fun(data=., vars=varsloc, use=name) %>% as.data.frame() %>% 
  group_by(x) %>%  
  dplyr::mutate(type='terrestrial') %>%
  full_join(., gather_fun(data=reordered, vars=varsclim, use=name) %>% as.data.frame() %>% 
              group_by(x) %>%  
              dplyr::mutate(type='climate')) %>%
  dplyr::mutate(y= as.character(y), y=ifelse(y=='fields', 'field area', y),
                y=ifelse(y=='forest', 'forest area', y),
                y=ifelse(y=='water', 'surface water area', y),
                y=ifelse(y=='stream_density', 'drainage density', y),
                y=ifelse(y=='med', 'median depth', y),
                y=ifelse(y=='logK', 'KR', y),
                y=ifelse(y=='aq_thick', 'thickness', y),
                
                
                y=ifelse(y=='t_yr', 'annual T', y),
                y=ifelse(y=='p_yr', 'annual P', y),
                # y=ifelse(y=='q_yr', 'mean rm', y),
                y=ifelse(y=='pws_yr', 'annual wet spell (P)', y),
                # y=ifelse(y=='qws_yr', 'mean wet spell (rm)', y),
                
                y=ifelse(y=='t_ff', 'summer T', y),
                y=ifelse(y=='p_ff', 'summer P', y),
                y=ifelse(y=='q_ff', 'summer rm', y),
                y=ifelse(y=='pws_ff', 'summer wet spell (P)', y),
                y=ifelse(y=='qws_ff', 'summer wet spell (rm)', y),
                y=ifelse(y=='t_frost', 'winter T', y),
                y=ifelse(y=='p_frost', 'winter P', y),
                y=ifelse(y=='q_frost', 'winter rm', y),
                y=ifelse(y=='pws_frost', 'winter wet spell (P)', y),
                y=ifelse(y=='qws_frost', 'winter wet spell (rm)', y),
                
                
                # y = factor(y, levels=c('drainage density', 'surface water area',
                #                        'field area', 'forest area', 'elevation',
                #                        'iqr', 'median depth', 'thickness','T',
                #                        'soildepth',
                #                        'KR', 'KS7', 'KS6', 'KS5','KS4','KS3', 'KS2',
                #                        
                #                        # 'mean wet spell (rm)',  'mean rm',
                #                        'annual wet spell (P)',
                #                        'annual P', 'annual T',
                #                        
                #                        'summer wet spell (rm)', 'summer wet spell (P)',
                #                        'summer rm', 'summer P', 'summer T',
                #                        'winter wet spell (rm)', 'winter wet spell (P)',
                #                        'winter rm', 'winter P', 'winter T'
                #                        
                # )),
                type= factor(type, levels=c('terrestrial','climate')))

library(cowplot); library(corrplot)
# other way of plotting the cross-correlation ----
test.s.new <- test.s %>% dplyr::mutate(x=as.character(x),
                         x = ifelse(x=='slope1_z', 'slope 1 (ACF/lag)', x),
                         x = ifelse(x=='auto_z', 'ACF at k = 1 month', x),
                         x = ifelse(x=='k02_z', 'k (months) at ACF = 0.2', x),
                         # x = ifelse(x=='cluster_z', 'cluster memory', x),
                         correlation=ifelse(estimate<0, 'negative','positive'),
                         correlation=paste('Spearman', correlation, sep=', '),
                         est=ifelse(estimate<0, estimate*-1,estimate),
                         p = ifelse(p.value>0.05, 'insignificant', 'significant'),
                         use='Spearman')
                         # x = factor(x, levels=order))
test.p.new <- test.p %>% dplyr::mutate(x=as.character(x),
                                   x = ifelse(x=='slope1_z', 'slope 1 (ACF/lag)', x),
                                   x = ifelse(x=='auto_z', 'ACF at k = 1 month', x),
                                   x = ifelse(x=='k02_z', 'k (months) at ACF = 0.2', x),
                                   # x = ifelse(x=='cluster_z', 'cluster memory', x),
                                   correlation=ifelse(estimate<0, 'negative','positive'),
                                   correlation=paste('Pearson', correlation, sep=', '),
                                   est=ifelse(estimate<0, estimate*-1,estimate),
                                   p = ifelse(p.value>0.05, 'insignificant', 'significant'),
                                   use='Pearson')
test <- full_join(test.s.new
                    # dplyr::filter(y=="water1" | y =="surface water area" |
                    #                       y=="sd1" | y=="drainage density" |
                    #                       y=="forest1" | y=="forest area" |
                    #                     y=="fields1" | y=="field area")
                  , 
                  test.p.new 
                    # dplyr::filter(y=='water1' | y =='surface water area' |
                    #          y=='sd1' | y=='drainage density' |
                    #          y=='forest1' | y=='forest area' |
                    #        y=='fields1' | y=='field area')
                  ) %>% 
  dplyr::mutate(correlation=factor(correlation, 
                                   levels=c('Pearson, negative',
                                            'Spearman, negative',
                                            'Pearson, positive',
                                            'Spearman, positive')),
                x=factor(x, c('ACF at k = 1 month',
                              'k (months) at ACF = 0.2',
                              'slope 1 (ACF/lag)',
                              'cluster memory')))
                # y=ifelse(y=="sd1", "ds1", y))
library(ggpattern)
a<-test %>% 
  dplyr::filter(type=='terrestrial')%>% 
  ggplot(.) + 
  geom_hline(yintercept=0.5, linetype='dashed') + 
  geom_col_pattern(aes(y, est, fill=correlation, pattern=p), position='dodge',
    color = "black", pattern_fill = "black", pattern_angle = 45, 
    pattern_density = 0.1, pattern_spacing = 0.025, pattern_key_scale_factor = 0.6) +
  # geom_text(data=. %>% filter(p=='significant'), # & grepl('positive',correlation)==T),
  #           aes(y, est, group=correlation,label=round(estimate,2)), size=2,
  #           position=position_dodge(1),
  #           hjust=-0.5) +
  
  scale_fill_manual('', 
                    values=c('Pearson, negative' = COL2('RdBu')[30],
                             'Spearman, negative' = COL2('RdBu')[10],
                             'Pearson, positive'= COL2('RdBu')[150],
                             'Spearman, positive' = COL2('RdBu')[170])) + 
  
  scale_y_continuous('cross-correlation coefficient', limits=c(0, 0.6)) +
  scale_x_discrete('', expand=c(0,0)) +
  guides(colour= "none", pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) + 
  scale_pattern_manual('p > 0.05', values = c('significant' = "stripe", 'insignificant' = "none")) +
  my_theme +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
    facet_grid(cols=vars(x), rows=vars(type)) + #, scales='free_y', space='free_y') +
  coord_flip()
b<-test %>% 
  dplyr::filter(type=='climate')%>% 
  ggplot(.) + 
  geom_hline(yintercept=0.5, linetype='dashed') + 
  geom_col_pattern(aes(y, est, fill=correlation, pattern=p), position='dodge',
                   color = "black", pattern_fill = "black", pattern_angle = 45, 
                   pattern_density = 0.1, pattern_spacing = 0.025, pattern_key_scale_factor = 0.6) +
  # geom_text(data=. %>% filter(p=='significant'), # & grepl('positive',correlation)==T),
  #           aes(y, est, group=correlation,label=round(estimate,2)), size=2,
  #           position=position_dodge(1)) +
  #
  
  scale_fill_manual('', 
                    values=c('Pearson, negative' = COL2('RdBu')[30],
                             'Spearman, negative' = COL2('RdBu')[10],
                             'Pearson, positive'= COL2('RdBu')[150],
                             'Spearman, positive' = COL2('RdBu')[170])) + 
  
  scale_y_continuous('cross-correlation coefficient', limits=c(0,0.6)) +
  scale_x_discrete('', expand=c(0,0)) +
  guides(colour= "none") + 
  my_theme +
  scale_pattern_manual('p > 0.05', values = c('significant' = "stripe", 'insignificant' = "none")) +
  
  theme(panel.grid = element_blank(), strip.text.x = element_blank()) +
  facet_grid(cols=vars(x), rows=vars(type)) + #, scales='free_y', space='free_y') +
  coord_flip()
library(Cairo); library(ggplot2); library(RColorBrewer)

# grDevices::cairo_pdf(filename='sweden_class/crosscorr.pdf', width=5, height=4,
                     # fallback_resolution = 400)
plot_grid(a + theme(legend.position = 'none'),
          b + theme(legend.position = 'none'), 
          get_legend(a),
          nrow=3, rel_heights=c(2,2.16667,0.5), align='v', axis='lr')
# dev.off()

test %>% dplyr::filter(p=='significant') %>% 
  dplyr::distinct(x,y,estimate,p.value)

test.pval <- test %>% dplyr::filter(p=='significant') %>% 
  dplyr::distinct(x,y,est,p.value,type,use)

test.pval %>% group_by(x, use, type) %>% dplyr::mutate(count=n()) %>%
  ggplot(., aes(x,est, fill=use, group=use), alpha=0.7) +
  geom_boxplot(position='dodge') + 
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  geom_text(data=. %>% distinct(x, type, est, use, count), aes(label = count)) +
  scale_fill_grey('method') + 
  facet_grid(cols=vars(x), rows=vars(type), scales='free', space='free') + my_theme + 
  theme(axis.text.x = element_blank(), axis.title=element_blank())

# show some descriptors ----
library(tidyverse); library(ggsci)
order=c('stream density [km^-1]', 'surface water area [%]',
        'field area [%]', 'forest area [%]', 'elevation [masl]',
        'iqr [m]', 'median depth [mbgs]')
# order=c('alpha0', 'k0.2', 'alpha1', 'alpha2', 'cluster')

comb[c(vars, 'group', 'cluster')] %>% 
  dplyr::mutate(cluster = as.factor(cluster),
                elevation=as.numeric(elevation),
                stream_density=round(as.numeric(stream_density)*1000,2)/((5^2)*pi),
                fields=round(fields/((5000^2)*pi),2)*100,
                forest=round(forest/((5000^2)*pi),2)*100,
                water=round(water/((5000^2)*pi),2)*100) %>%
  group_by(cluster, group) %>%
  pivot_longer(cols=all_of(vars), #mem_est[!mem_est %in% 'cluster']), 
               names_to='index', values_to='value') %>%
  
  dplyr::mutate(
    # mem_est=ifelse('auto'==mem_est, 'alpha0', mem_est),
    # mem_est=ifelse('k02'==mem_est, 'k0.2', mem_est),
    # mem_est=ifelse('slope1'==mem_est, 'alpha1', mem_est),
    # mem_est=ifelse('slope2'==mem_est, 'alpha2', mem_est),
    # mem_est= factor(mem_est, levels=c(order))
    index=ifelse(index=='fields', 'field area [%]', index),
    index=ifelse(index=='forest', 'forest area [%]', index),
    index=ifelse(index=='water', 'surface water area [%]', index),
    index=ifelse(index=='med', 'median depth [mbgs]', index),
    index=ifelse(index=='stream_density', 'stream density [km^-1]', index),
    index=ifelse(index=='elevation', 'elevation [masl]', index),
    index=ifelse(index=='iqr', 'iqr [m]', index),
    index = factor(index, levels=c(order))
    ) %>% 
  ggplot(., aes(x=cluster, y=value, fill=group)) + 
  geom_boxplot() + facet_wrap(~index, scales='free') + 
  scale_x_discrete('cluster') + #, expand=c(0.01,0.01)) + 
  scale_y_continuous('') + #, expand=c(0.01,0.01)) +
  # scale_y_continuous('',trans='log', labels = scales::number_format(accuracy = 0.01)) +
  scale_fill_tron(name='') + my_theme +
  theme(panel.grid=element_blank(), 
        panel.border=element_rect(colour='grey', fill=NA))



# are variables correlated?----
library(corrplot)
cor_reordered <- do.call(cbind, #data.frame,
                         lapply(reordered[c(varsloc, varsclim)], # replacing (one) Inf value with the median value, should not affect PCA
                                function(x) replace(x, is.infinite(x)|is.na(x), median(x, na.rm=T))))
cor_reordered <- cor_reordered[, c(varsloc, varsclim)] %>% as.data.frame() %>%
  dplyr::rename('field area'=fields,
                'forest area' = forest, 
                'surface water area' = water,
                'median depth' =med,
                'drainage density' = stream_density,
                'KR'= logK,
                'soil depth' = soildepth,
                # 'thickness'= aq_thick,
                # 'mean wet spell (rm)'= qws_yr, 
                'annual wet spell (P)'=pws_yr,
                # 'mean rm'= q_yr, 
                'annual P'=p_yr,
                'annual T'= t_yr,
                'summer wet spell (rm)'= qws_ff, 
                'summer wet spell (P)'= pws_ff,
                'summer rm'= q_ff, 
                'summer P'= p_ff, 
                'summer T'= t_ff,
                'winter wet spell (rm)'= qws_frost, 
                'winter wet spell (P)'= pws_frost,
                'winter rm'= q_frost, 
                'winter P'= p_frost, 
                'winter T'= t_frost
  )
plot <- rbind(cor_reordered %>% dplyr::mutate(test='Spearman') %>%
                group_by(test),
                  cor_reordered %>% dplyr::mutate(test='Pearson')%>%
                group_by(test))
cor.a <- cor(plot[which(plot$test=='Spearman'),1:25], method = 'spearman')
cor.b <- cor(plot[which(plot$test=='Pearson'),1:25], method = 'pearson')
cor.a[lower.tri(cor.a)] <- cor.b[lower.tri(cor.b)]
p.a <- cor.mtest(plot[which(plot$test=='Spearman'),1:25], method = 'spearman')
p.b <- cor.mtest(plot[which(plot$test=='Pearson'),1:25], method = 'pearson')
p.a[lower.tri(p.a)] <- p.b[lower.tri(p.b)]

library(Cairo); library(ggplot2); library(RColorBrewer)
grDevices::cairo_pdf(filename='sweden_class/corr_matrix.pdf', width=6, height=5.5,
                     fallback_resolution = 400)
# png(height=53, width=61, units='mm', file="sweden_class/corr_matrix.png", 
    # type = "cairo", res=400)
corrplot(cor.a, p.mat=p.a$p,
         # type='lower',
         order='hclust',
         method='ellipse',
         tl.col='black', tl.srt=90, 
         tl.cex = 0.5, cl.cex=0.5, sig.level = c(0.001, 0.01, 0.05), 
         insig = 'label_sig', pch.cex=0.5) 
# 
dev.off()
#when opening in affinity, uncheck boxes ' favour editability over fedility'


