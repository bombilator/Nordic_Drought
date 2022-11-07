rm(list = ls())

library(magrittr)
library(purrr)
library(tidyquant)
library(ggplot2)
library(psych)
library(dplyr)

# 1. group data biweekly, interpolate the water tables for missing weeks if necessary
# 2. define the recharge period and it's timing (in plots.R)

#download new data if necessary
source("rfuncs/importSGUjson.R")
processedData = importSGUjson()

# FI reading the data  ----
chars <- readRDS("output/process/gwts_chars.rds") %>% 
  # select(Tunnus, elevation) %>%
  mutate(elevation=as.numeric(elevation))
chars_manually <- list(Tunnus = c("1303p1", "1303p2","1303p3","1303p4","1303p5","1303p7",
                                  "0602p1", "0602p2", "0602p3","0602p4","0602p5",
                                  "0602p6","0602p7","0602p8","0602p9","0602p10"),
                       elevation = as.numeric(c(196.47,199.06,203.96,207.84,200.31,200.27,
                                                115.23,NA, 112.32, 114.98, 114.71,
                                                114.65, 115.43, 116.75, 116.67, 111.91)))
chars <- bind_rows(chars, chars_manually)
saveRDS(chars, "output/process/fi_meta.RDS")
# chars <- left_join(g.df %>% select(Stationname, Tunnus, id), chars, by="Tunnus") %>%
# distinct(Stationname, Tunnus, id, N, E, material, area_km, elevation)

# get ids into raw dataframe
# old
old.df <- readRDS("output/process/fin_gw_sgi.rds") %>% 
  select(Stationname, Tunnus, N, E, id) %>% #unnest() %>% 
  dplyr::rename(
    # date = Date,
    #             year = YEAR,
    #             month = MONTH,
                # sgi = SGIm, 
                Station = Stationname)
# new, all raw data
raw.df <- readRDS("R_finland_drought/output/process/fin_gw_raw.rds") %>% 
  select(Stationname, Tunnus, N, E, date2, Vedenkorkeus) %>% 
  dplyr::rename(raw = Vedenkorkeus,
              # sgi = SGIm,
              date = date2,
              Station = Stationname) %>% mutate(raw = as.numeric(raw))
g.df <- full_join(old.df, raw.df)
  
g.df <- left_join(g.df, chars, by="Tunnus") %>% filter(!is.na(elevation)) %>% 
  mutate(#mean_mon = elevation - mean_mon
    raw_dep = elevation - raw,
    week = week(date),
    week2 = ceiling(week/2),
    # month = month(date),
    year = year(date)#,
    # day = mday(date)
    #- geometric.mean(mean_mon)
  ) %>%
  # do mean_dep in the five-year period groups instead/mutate it
  group_by(Tunnus) %>%
  filter(Tunnus != "0802p5") %>%
  mutate(
    # gmean = psych::geometric.mean(raw_dep+100, na.rm=TRUE)-100,
                              # mean = mean(raw_dep, na.rm=TRUE),
         mean_dep = raw_dep - mean(raw_dep, na.rm=TRUE)) %>% #not sure if this will be correct
  # want to standardise the water levels to fluctuate around 0 before I group all wells
  #in one id together
  ungroup() %>%
  # group_by(week2) %>% #, year, month) %>% 
  # mutate(date2 = as.Date(paste(year, month, ifelse((day >= 15), 20, 10), sep="-"),
  #                              format = "%Y-%m-%d"))
  group_by(week2,year, id) %>% #before: by date and station/id
  mutate(
    # sgi_max = max(sgi),
    #      sgi_min = min(sgi),
    #      sgi_geom = psych::geometric.mean(sgi+4)-4,
         g_mean = mean(mean_dep, na.rm=TRUE),
         g_geom = psych::geometric.mean(mean_dep+100, na.rm=TRUE)-100,
         gmax=max(mean_dep, na.rm=TRUE),
         gmin =min(mean_dep, na.rm=TRUE),
         stdev = sd(mean_dep, na.rm=TRUE)#,
         
         # monthday = format(date, "%m-%d")
         ) %>% 
  ungroup() %>% #group_by(id) %>%
  # mutate(mean = mean(g_geom),
         # gmean = psych::geometric.mean(g_geom)) %>% 
  filter(id != 1685 &id != 1440 & id != 1447 & 
             id != 1496 & id != 1978& id != 1874& id != 1873 &
             id != 2042)

saveRDS(g.df, "R_finland_drought/output/process/g_df.rds")

# FI get raw data and remove data with too few observations in Mar-Jun ----
# for years and wells, find where the data sampling frequency is less than biweekly in 
# the months March to June 
g.df <- readRDS("R_finland_drought/output/process/g_df.rds")
freq.filt <- g.df %>% select(Tunnus, date, raw, year, week2) %>%  
  mutate(month = month(date)) %>% 
  dplyr::add_count(wt = raw, Tunnus, year, week2) %>%
  mutate(freq2 = ifelse(n > 0, TRUE, FALSE)) 
  
g.df2 <- full_join(g.df, freq.filt) %>%
  select(Tunnus, date, raw, year, week2, month, n, freq2)

# filter to get years when the frequency is less than biweekly between March and June
freq.filt <- freq.filt %>% filter(month >= 3 & month <=5) %>% # <=6
  mutate(Mar_Jun_freq2 = freq2) %>% distinct(Tunnus, year, Mar_Jun_freq2) %>% 
  filter(Mar_Jun_freq2 == FALSE)

g.df2 <- full_join(g.df2, freq.filt) %>% 
  mutate(Mar_Jun_freq2 = ifelse(is.na(Mar_Jun_freq2), TRUE, FALSE)) %>%
  select(-n)
g.df2 <- full_join(g.df, g.df2)

saveRDS(g.df2, "R_finland_drought/output/process/g_df_raw.rds")

# FI average for the different decades ----
g.df2 <- readRDS("output/process/g_df_raw.rds")

freq.filt <- g.df2 %>% filter(((year >= 1980 & year<= 1989) |
                                   (year >=2001 & year <= 2010))) %>%
  # distinct(id, Tunnus, year) %>%
  # mutate(eighties = ifelse(year >= 1980 & year <= 1995, TRUE, FALSE),
  #        tens = ifelse(year >= 1996 & year <= 2010, TRUE, FALSE)) %>% 
  distinct(Tunnus, id, year)

g.df2 <- inner_join(g.df2, freq.filt) 

g.df8 <- g.df2  %>% 
  filter(year >= 1980 & year <= 1989) %>%
  distinct(Station, Tunnus, N, E, id, 
           date, raw_dep, mean_dep, month, week2, year, g_mean, g_geom, gmax, gmin) %>% 
  group_by(id, Tunnus, year) %>% mutate(mean_dep = raw_dep - median(raw_dep, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(ymd = as.Date(paste(ifelse(month<=11, "1981", "1980"), 
                        month, ifelse(day(date) != 29, day(date), 28),sep="-"))) %>%
  distinct(id, Station, Tunnus, N, E, ymd, raw_dep, mean_dep, year, week2) %>% 
  
  group_by(id, Station, Tunnus, year) %>% mutate(mean_dep = ifelse(is.na(mean_dep), na.approx(mean_dep), mean_dep)) %>%
  
  group_by(id,week2) %>% 
  mutate(g_geom = median(mean_dep, na.rm=TRUE),
         gmax=max(mean_dep, na.rm=TRUE),
         gmin =min(mean_dep, na.rm=TRUE),
         stdev = sd(mean_dep, na.rm=TRUE)) %>% ungroup() %>% 
  distinct(Tunnus, id,N, E, ymd, g_geom, gmax, gmin, stdev, mean_dep,
           year)%>% group_by(id, Tunnus, year) %>% filter(!duplicated(g_geom))
  # filter(!is.na(mean_dep)) 

# saveRDS(g.df8, "R_finland_drought/output/process/g_df8.rds")
saveRDS(g.df8, "output/process/g_df8_raw.rds")

g.df1 <- g.df2  %>% 
  filter(year >= 2001 & year <= 2010) %>%
  distinct(Station, Tunnus, N, E, id, 
           date, raw_dep, mean_dep, month, week2, year, g_mean, g_geom, gmax, gmin)%>%  
  group_by(id, Tunnus, year) %>% mutate(mean_dep = raw_dep - median(raw_dep, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(ymd = as.Date(paste(ifelse(month<=11, "1981", "1980"), 
                        month, ifelse(day(date) != 29, day(date), 28),sep="-"))) %>%
  distinct(id, Station, Tunnus, N, E, ymd, raw_dep, mean_dep, year, week2) %>% 
  
  group_by(id, Station, Tunnus, year) %>% mutate(mean_dep = ifelse(is.na(mean_dep), na.approx(mean_dep), mean_dep)) %>%
  
  group_by(id, week2) %>%
  mutate(g_geom = median(mean_dep, na.rm=TRUE),
         gmax=max(mean_dep, na.rm=TRUE),
         gmin =min(mean_dep, na.rm=TRUE),
         stdev = sd(mean_dep, na.rm=TRUE)) %>% ungroup() %>% 
  distinct(Tunnus, id,N, E, ymd, g_geom, gmax, gmin, stdev, mean_dep,
           year) %>% group_by(id, Tunnus, year) %>% filter(!duplicated(g_geom))
  # filter(!is.na(mean_dep))

# saveRDS(g.df1, "R_finland_drought/output/process/g_df1.rds")
saveRDS(g.df1, "output/process/g_df1_raw.rds")

# SWE reading the data ----
g.ds <- readRDS("input/SWE_data/processed/gw_swe.RDS")
meta <- g.ds$meta %>% dplyr::select(-Namn, -TopoLage, -Kommunkod, -Matmetod) %>% 
  filter(Start < as.Date("1980-12-31") & 
                                  (End > as.Date("2010-12-31") | 
                                     is.na(End)) &
                                  (
                                    Omr_stn != "1_8" &
                                      Omr_stn != "13_5" &
                                      Omr_stn != "15_7" &
                                      Omr_stn != "16_42" &
                                      Omr_stn != "16_74" &
                                      Omr_stn != "18_3" &
                                      Omr_stn != "20_1" &
                                      Omr_stn != "20_2" &
                                      Omr_stn != "26_28" &
                                      Omr_stn != "3_4" &
                                      Omr_stn != "37_2" &
                                      Omr_stn != "37_46" &
                                      Omr_stn != "37_48" &
                                      Omr_stn != "37_50" &
                                      Omr_stn != "39_8" &
                                      Omr_stn != "4_12" &
                                      Omr_stn != "5_3" &
                                      Omr_stn != "52_11"&
                                      Omr_stn != "65_1" &
                                      Omr_stn != "65_2" &
                                      Omr_stn != "68_3" &
                                      Omr_stn != "8_2" 
                                  )) %>% 
  dplyr::select(-Start, -End, -EUCD, -Kvalitet, -CRS) %>% distinct()
saveRDS(meta, "output/process/swe_meta.RDS")
meas <- g.ds$meas %>% select(-YEARWEEK,-MONTHDAY,-YEARDAY,-HYD_YEAR) %>% filter(Omr_stn %in% meta$Omr_stn)

# add dates for every day into meas df
library(tidyr)
# tidyr complete function docs: https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-exploratory-79f2a321e6b5
new <- meas %>% select(Omr_stn, Omr, Stn, Datum, m_o_h) %>% 
  mutate(date2 = as.Date(Datum))  %>%
  group_by(Omr_stn, Omr, Stn) %>%
  complete(date2 = seq.Date(min(Datum), max(Datum), by="day")) %>% select(-Datum)
  

new <- full_join(new, meta) %>% #select(Omr_stn, Omr, Stn, date2, m_o_h) %>% 
  rename(Datum = date2)
saveRDS(new, "output/process/swe_gw_raw.rds")
# SWE computing the means ----
new <- readRDS("output/process/swe_gw_raw.rds")
id <- readRDS("output/process/sweden_ids_tunnus.rds") %>%
  rename(id=ids)
g.ds <- #left_join(new, meta, by="Omr_stn") %>% 
  new %>%
  ungroup() %>% 
  mutate(m_o_h = ifelse(Omr_stn == "6_24" & Datum == as.Date("2018-11-14"), 
                        NA, m_o_h)) %>% 
  select(-Omr_stn) %>% 
  rename(
    date = Datum, 
    E = N,
    N = E,
    material = Jordart,
    Station = Omr,
    Tunnus = Stn,
    type = Akvifertyp,
    raw = m_o_h
  ) 
g.ds <- left_join(g.ds %>% mutate(Station=as.integer(Station)), id)
g.ds <- g.ds %>% 
  mutate(
    elevation = as.numeric(RefNivOkRor)-as.numeric(RorHojdOMark),
    week2 = ceiling(week(date)/2),
    raw_dep = elevation - raw,
    year = year(date),
    month = month(date)
  ) %>% 
  group_by(id, Station, Tunnus) %>% 
  mutate(
    mean_dep =raw_dep - mean(raw_dep, na.rm = TRUE)
    ) %>% ungroup() %>%  
  distinct(id, Station, Tunnus, date, year, month, week2, 
           material, type, E, N, elevation, 
           raw, raw_dep, mean_dep
           ) %>%
  ungroup() %>%
  group_by(week2, year, id) %>% #before: date and Station
  mutate(
    g_geom = psych::geometric.mean(mean_dep+100, na.rm = TRUE)-100,
    gmax=max(mean_dep, na.rm = TRUE),
    gmin =min(mean_dep, na.rm =TRUE),
    stdev = sd(mean_dep, na.rm=TRUE)
    ) %>% 
  ungroup() 

saveRDS(g.ds, "output/process/g_ds2.rds") 
#before g.ds (grouped by Station, not id also)

# SWE get raw data and remove data with too few observations in Mar-Jun ----
# for years and wells, find where the data (well AND year) sampling frequency is less 
# than biweekly in the months March to June (could change to March-May, the more 
# crucial months according to Marcus)
g.ds <- readRDS("output/process/g_ds2.rds")
freq.filt <- g.ds %>% select(Station, Tunnus, date, raw, year,month, week2) %>%  
  dplyr::add_count(wt = raw, Station, Tunnus, year, week2) %>%
  mutate(freq2 = ifelse(n > 0, TRUE, FALSE)) 

g.ds2 <- full_join(g.ds, freq.filt) %>%
  select(Station, Tunnus, date, raw, year, week2, month, n, freq2)

# filter to get years when the frequency is less than biweekly between March and June
freq.filt <- freq.filt %>% filter(month >= 3 & month <=5) %>%
  mutate(Mar_Jun_freq2 = freq2) %>% distinct(Station, Tunnus, year, Mar_Jun_freq2) %>% 
  filter(Mar_Jun_freq2 == FALSE)

g.ds2 <- full_join(g.ds2, freq.filt) %>% 
  mutate(Mar_Jun_freq2 = ifelse(is.na(Mar_Jun_freq2), TRUE, Mar_Jun_freq2)) %>%
  select(-n) 
g.ds2 <- full_join(g.ds, g.ds2)

saveRDS(g.ds2, "R_finland_drought/output/process/g_ds_raw2.rds")
# before g_ds_raw grouped not by id but by Station and Tunnus

# SWE average for the different decades ----
# source("R_finland_drought/rfuncs/qqnorm_dpl.R")
# g.ds <- g.ds %>% 
#   mutate(type = ifelse(type=="Rör i jord, öppet magasin", "unconfined", "confined")) #%>%
#   group_by(Station, Tunnus, start, material, type, E, N, elevation) %>% nest(.key= mean_mon) %>%
#   mutate(
#     sgi = mean_mon %>%
#       map("mean_mon") %>% map(qqnorm_dpl)) # not sure if standardised per month or if all months are considered equal
g.ds2 <- readRDS("output/process/g_ds_raw2.rds")


freq.filt <- g.ds2 %>% filter(((year >= 1980 & year<= 1989) |
                              (year >=2001 & year <= 2010))) %>%
  distinct(Station, Tunnus, year) 
g.ds2 <- inner_join(g.ds2, freq.filt) 

g.ds8 <- g.ds2  %>% 
  filter(year >= 1980 & year <= 1989) %>% 
  distinct(id, Station, Tunnus, N, E, date, raw_dep, mean_dep, month, week2, year,g_geom, gmax, gmin) %>%  
  group_by(id, Tunnus, year) %>% mutate(mean_dep = raw_dep - median(raw_dep, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(ymd = as.Date(paste(ifelse(month<=11, "1981", "1980"), 
                        month, ifelse(day(date) != 29, day(date), 28),sep="-"))) %>%
  distinct(id, Station, Tunnus, N, E, ymd, raw_dep, mean_dep, year, week2) %>% 
  
  group_by(id, Station, Tunnus, year) %>% mutate(mean_dep = ifelse(is.na(mean_dep), na.approx(mean_dep), mean_dep)) %>%
  
  group_by(id, week2) %>%
  mutate(g_geom = median(mean_dep, na.rm=TRUE),
         gmax=max(mean_dep, na.rm=TRUE),
         gmin =min(mean_dep, na.rm=TRUE),
         stdev = sd(mean_dep, na.rm=TRUE)) %>% ungroup() %>%
  distinct(id, Station, Tunnus, N, E, ymd, g_geom, gmax, gmin, stdev, mean_dep,
           year) %>% group_by(id, Station, Tunnus, year) %>% filter(!duplicated(g_geom))
  # filter(!is.na(mean_dep))
# saveRDS(g.ds8, "R_finland_drought/output/process/g_ds8.rds")
saveRDS(g.ds8, "output/process/g_ds8_raw2.rds")



station=281
g.df8 %>% filter(id==station) %>% ggplot(.) +
  geom_line(data = . %>% filter(!is.na(mean_dep)),
            aes(ymd, mean_dep, linetype =  as.factor(Tunnus), colour = as.factor(year)),
            alpha= .5, show.legend = FALSE) +
  geom_point(aes(ymd, mean_dep,shape = as.factor(Tunnus), colour = as.factor(year)),
             alpha=.5) +
  geom_line(aes(ymd, g_geom, colour = "water table"), 
            colour= "black", size = 0.8, alpha=.6) + scale_y_reverse()

g.ds1 <- g.ds2  %>% 
  filter(year >= 2001 & year <= 2010) %>% 
  distinct(id, Station, Tunnus, N, E, date, raw_dep, mean_dep, month, week2, year, g_geom, gmax, gmin)  %>%  
  group_by(id, Tunnus, year) %>% mutate(mean_dep = raw_dep - median(raw_dep, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(ymd = as.Date(paste(ifelse(month<=11, "1981", "1980"),
                        month, ifelse(day(date) != 29, day(date), 28),sep="-"))) %>%
  distinct(id, Station, Tunnus, N, E, ymd, raw_dep, mean_dep, year, week2) %>% 
  
  group_by(id, Station, Tunnus, year) %>% mutate(mean_dep = ifelse(is.na(mean_dep), na.approx(mean_dep), mean_dep)) %>%
  
  group_by(id, week2) %>% 
  mutate(g_geom = median(mean_dep, na.rm=TRUE),
         gmax=max(mean_dep, na.rm=TRUE),
         gmin =min(mean_dep, na.rm=TRUE),
         stdev = sd(mean_dep, na.rm=TRUE)) %>% ungroup() %>% 
  distinct(id, Station, Tunnus, N, E, ymd, g_geom, gmax, gmin, stdev, mean_dep,
           year) %>% group_by(id, Station, Tunnus, year) %>% filter(!duplicated(g_geom))
  # filter(!is.na(mean_dep)) 
# saveRDS(g.ds1, "R_finland_drought/output/process/g_ds1.rds")
saveRDS(g.ds1, "output/process/g_ds1_raw2.rds")




# SWE get id names to connect between gw dat and met dat ----
g.ds <- readRDS("output/process/g_ds_raw.rds")
a <- readRDS("output/process/sweden_ids.rds") %>% 
  rename(Station = id) %>% mutate(E=as.character(round(E)) %>% sub("\\..*", "", .),
                                  N=as.character(round(N)) %>% sub("\\..*", "", .))
g.ds <- g.ds %>% distinct(Station, Tunnus, N, E) %>% 
  mutate(Station=as.integer((Station)), N=as.character(N),
         E=as.character(E))
a <- left_join(a %>% group_by(Station), g.ds %>% group_by(Station, Tunnus), 
               by=c("Station", "E", "N")) 
g.ds <- a %>% select(Station, Tunnus, ids)
saveRDS(g.ds, "output/process/sweden_ids_tunnus.rds")
# met data ----
tibm <- readRDS("output/process/tibm.r")
tibm <- tibm %>% ungroup() %>% 
  mutate(year = year(date),
         id = paste("f_", id, sep="")) %>% rename(RH = Hum)


swe.tibm <- readRDS("output/process/sweden_tibm2.rds") 
swe.tibm <- swe.tibm %>% ungroup() %>%
  mutate(year = year(date),
         id = paste("s_", id, sep="")) %>%
  rename(Tday = Tm, RRday = Precip) %>% 
  select(-Tmax, -Tmin)

all.tibm <- full_join(tibm, swe.tibm) %>%
  arrange(id, date) #ymd) 
saveRDS(all.tibm, "output/process/fennos_tibm_all2.rds")
all.tibm <- all.tibm %>% #readRDS("output/process/fennos_tibm_all.rds") %>% 
  filter(((year >= 1980 & year <= 1989) | (year >= 2001 & year <= 2010))) %>% # &
           # id %in% g$id) %>%  #from plots.R
  mutate(period = ifelse(year>1995, 1, 8),
         ymd = as.Date(paste(ifelse(month(date)<=11, "1981", "1980"),
                             month(date), ifelse(day(date) != 29, day(date), 28),
                             sep="-"
  ))) %>% select(-date)
saveRDS(all.tibm, "output/process/fennos_tibm_all.rds")
# ex plot, needs data from plots.R
all.tibm %>% 
  ggplot() +
  # geom_vline(data = dur1_2, aes(xintercept=ymd, colour = re_dis_type1), alpha=.2) +
  geom_line(aes(ymd, Tday)) +
  geom_col(aes(ymd, RRday)) +
  facet_wrap(~id, nrow=4) +
  scale_x_date(date_labels = "%b", date_breaks = "6 months") +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.ticks.x = element_blank()#,
        # axis.text.x = element_blank()
  )

pet <- readRDS("output/process/fennos_pet_all.r") %>% select(-date) %>% rename(date = ymd)
  # filter(id != 1685) %>% 
  # mutate(id = paste("f_", id, sep="")) 
# pet$q_pet = pet$sum_Q-pet$pet
# pet$p_pet = pet$sum_P-pet$pet
pet <- pet %>% ungroup() %>% 
  mutate(year = year(date)) %>%
  filter(((year >= 1980 &  year <= 1989) | (year >= 2001 & 
             year <= 2010))) %>% # &
           # id %in% g$id) %>%  #from plots.R
  mutate(period = ifelse(year>1995, 1, 8),
         ymd = as.Date(paste(ifelse(month(date)<=11, "1981", "1980"), 
                             month(date), ifelse(day(date) != 29, day(date), 28),
                             sep="-"
         ))) %>% select(-date)
saveRDS(pet,"output/process/fennos_pet_all2.r")
# pet data ----
pet <- readRDS("output/process/fennos_pet_all2.R") 
tibm <- readRDS("output/process/fennos_tibm_all2.rds") 

tibm.pet <- left_join(tibm %>% 
            select(id, RRday, Tday, A, M, Q, P_snow, OSQ,
                   year, period, ymd), 
          pet) %>% mutate(week2=ceiling(week(ymd)/2))
tibm.pet8 <- tibm.pet %>% 
  filter(year >= 1980 & year <= 1989) %>%
  group_by(id, ymd) %>% #before week
  mutate(pet_m = ifelse(month(ymd)==c(1, 3, 5, 7, 8, 10, 12),(pet*2)/31, (pet*2)/30),
         pet_m = ifelse(month(ymd)==2, (pet*2)/28, pet_m), #from biweekly to daily representatives
      RRday_m = mean(RRday, na.rm=TRUE),
         A_m = mean(A, na.rm=TRUE),
         M_m = mean(M, na.rm=TRUE),
         Q_m = mean(Q, na.rm=TRUE),
         P_snow_m = mean(P_snow, na.rm=TRUE),
      OSQ_m = mean(OSQ, na.rm=TRUE)) %>%
    ungroup() %>%
  distinct(id, N, E, ymd, pet_m,  RRday_m, A_m, M_m, Q_m, P_snow_m,OSQ_m,
           period, week2) 
saveRDS(tibm.pet8, "output/process/fennos_tibm_pet8.rds")
tibm.pet1 <- tibm.pet %>% 
  filter(year >= 2001 & year <= 2010) %>% 
    group_by(id, ymd) %>% #before week
    mutate(pet_m = ifelse(month(ymd)==c(1, 3, 5, 7, 8, 10, 12),(pet*2)/31, (pet*2)/30),
           pet_m = ifelse(month(ymd)==2, (pet*2)/28, pet_m), #from biweekly to daily representatives
        RRday_m = mean(RRday, na.rm=TRUE),
        A_m = mean(A, na.rm=TRUE),
        M_m = mean(M, na.rm=TRUE),
        Q_m = mean(Q, na.rm=TRUE),
    P_snow_m = mean(P_snow, na.rm=TRUE),
    OSQ_m = mean(OSQ, na.rm=TRUE)) %>%
    ungroup() %>%
    distinct(id, N, E, ymd, pet_m,  RRday_m, A_m, M_m, Q_m, P_snow_m, OSQ_m,
             period, week2) 
saveRDS(tibm.pet1, "output/process/fennos_tibm_pet1.rds")
