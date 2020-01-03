rm(list = ls())

library(magrittr)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sp)
library(ggmap)

# find appropriate wells (3) ----
g <- readRDS("output/process/g_ds_raw2.rds") %>% filter(!is.na(raw_dep)) %>%
  select(Station, Tunnus, date, raw, material, type, E, N, id, elevation, raw_dep, year, month)

coordinates(g) = ~E+N
proj4string(g) = CRS("+init=epsg:3006")
g = spTransform(g, CRS("+proj=longlat + datum=WGS84"))
g = as.data.frame(g)


# g <- g %>% add_count(wt=!is.na(raw_dep), Station, Tunnus, year) %>% arrange(Station, Tunnus, date) #%>%
#   filter(#n > 21 &  #21/12 = 1.75 measurements a year at least 
#            id != "14" & id != "104"& id != "125"& id != "15" & 
#            id != "18"& id != "46" & id != "45"& id != "38"& id != "7")
# g <- g %>% select(-n) %>%  mutate(con= c(NA,diff(year)==1 | diff(year)==0))
g <- g %>% group_by(Station, Tunnus) %>% mutate(beg = first(year), end = last(year)) 
a <- g %>% distinct(Station, Tunnus, beg, end, N, E, date, raw_dep, year) %>% 
  group_by(Station, Tunnus) %>% 
  arrange(N, beg) %>%
  filter(beg <= 1975 & (is.na(end)|end>=2018) & year >= 1975) %>% 
  add_count(wt=!is.na(raw_dep), Station, Tunnus, year) %>% 
  filter(n >= 12) %>% distinct(Station, Tunnus, N, E, year, n) %>% arrange(Station, Tunnus, year) %>%
  mutate(lag_y =ifelse(lead(year,1)!=year+1, FALSE, TRUE)) %>% distinct(Station, Tunnus, N, E, lag_y) %>%
  filter(lag_y==FALSE) %>% mutate(id = paste(Station, Tunnus, sep="_"))
g <- g %>% mutate(id=paste(Station, Tunnus, sep="_")) %>% filter(!id %in% a$id & year >= 1975 & 
                                                                   beg <= 1975 & (is.na(end)|end>=2018))

ggmap(stamen) + geom_point(data=g %>% distinct(Station, Tunnus, N, E) %>% 
                             filter(Station==37 | Station==34 | Station==26 | Station==16),
                           aes(E, N, colour=as.factor(Station)))
g %>% ggplot(.) + aes(date, raw_dep) + 
  geom_line(aes(colour=as.factor(Station), group=Tunnus), size=1) +
  # geom_point(aes(colour=as.factor(Station), group=Tunnus), size=1) + 
  facet_wrap(~Station)
  xlim(as.Date("2001-01-01"), as.Date("2010-12-30"))


g %>% distinct(Station, Tunnus, id, beg, end)
  
# get new gw data for Swe ----
a <- readRDS("output/for_sgi.rds")
a <- full_join(a$meas, a$meta) %>% group_by(Omr, Stn) %>% 
  mutate(Start = first(Datum)) %>% filter(is.na(End) & Start <= as.Date("1970-01-01"))
#%>% filter(Omr == 4) #6 | Omr == 26 | Omr == 37)
a <- a %>% ungroup() %>% select(Omr, Stn, Namn, Datum, UnderOkRor, m_o_h, YEAR, MONTH, Start, Jordart, Akvifertyp, RefNivOkRor, RorHojdOMark,
                  RorHojdOMark, RorLangd, End, N, E) 
a <- a %>% rename(Cluster = Omr, Station = Stn, date = Datum, year = YEAR, month = MONTH, 
             material = Jordart, type = Akvifertyp, level_m = m_o_h) %>% 
  mutate(elevation = as.numeric(RefNivOkRor)-as.numeric(RorHojdOMark), 
         level_cm = as.numeric(UnderOkRor) - as.numeric(RorHojdOMark)*100)
a <- a %>% select(-RefNivOkRor, -RorHojdOMark, -UnderOkRor)
a
coordinates(a) = ~N+E
proj4string(a) = CRS("+init=epsg:3006")
a = spTransform(a, CRS("+proj=longlat + datum=WGS84"))
a = as.data.frame(a) %>% rename(N=E, E=N)
a %>% filter(year>=1970 & (Cluster==55 | Cluster==37 | Cluster==14)) %>%  
  group_by(Cluster, Station) %>% mutate(medwt=median(level_cm, na.rm=TRUE)/100,
                                            medwt_masl=median(level_m, na.rm=TRUE)) %>% 
  distinct(Cluster, Station, material, type, elevation, medwt, medwt_masl) %>% View()

a <- a %>% filter(year >= 1970)

a %>% #filter(Cluster==6) %>% 
  ggplot(.) + aes(date, level_cm) + geom_line(aes(linetype=as.factor(Cluster), 
                                                  colour=as.factor(Station))) +
  geom_point(aes(fill=as.factor(Cluster), colour=as.factor(Station)), shape=16)
  

  
# standardise ----
agg = a %>% 
  group_by(Cluster, Station, year, month) %>%
  summarise(date = first(date),
    N = first(N),
    E = first(E),
    mean_mon = mean(level_m)) %>% ungroup()
g.sgi = agg %>%
  group_by(Cluster, Station, month) %>%
  dplyr::mutate(
    sgi = qqnorm(mean_mon)$x
  )
# saveRDS(g.sgi, "output/sgi.rds")
library(lubridate)
g.sgi <- readRDS("output/sgi.rds") %>% ungroup() %>% group_by(Cluster, Station)
g.sgi <- g.sgi %>% mutate(id=paste(Cluster, Station, sep="_"),
                   balance=ifelse(sgi<0, "y", "n"),
                   dry=ifelse(sgi<=-1.5 & (lead(sgi, 1) <=-1.5 | lag(sgi,1) <=-1.5), "y", "n"),
                   dry_start=ifelse(balance!=lag(balance, 1) & balance=="y", paste(date), "-"),
                   dry_start=ifelse(balance==lag(balance,1) & balance=="y", NA, dry_start),
                   dry_start= zoo::na.locf(dry_start, na.rm=FALSE),
                   dry_start=ifelse(dry_start=="-", NA, dry_start),
                   dry_dat = ifelse(!is.na(dry_start), paste(date), NA),
                   dry_end = ifelse(balance!=lead(balance, 1) & lead(balance,1)=="n", paste(date), "-"),
                   dry_end=ifelse(balance==lead(balance,1) & balance=="y", NA, dry_end),
                   dry_end= zoo::na.locf(dry_end, fromLast=TRUE, na.rm=FALSE),
                   dry_end=ifelse(dry_end=="-", NA, dry_end),
                   dry_year= year(dry_start)) %>% 
  group_by(id, dry_year) %>% mutate(dry_dur=as.Date(dry_end) - as.Date(dry_start))
g.sgi <- g.sgi %>% right_join(., g.sgi %>% group_by(id) %>% filter(any(sgi <= -1.5) &
                                                                     dry_dur >= 180) %>% distinct(Cluster)) %>%
  ungroup() %>% group_by(id)
# g.sgi <- g.sgi %>%
#   mutate(dry_end=as.Date(dry_end), dry_start = as.Date(dry_start), dry_dat=as.Date(dry_dat)) %>%
#   right_join(., g.sgi %>% group_by(id) %>% filter(dry_dur > 548) %>%
#                distinct(id))
g.sgi <- g.sgi %>% filter(id %in% (g %>% distinct(Station, Tunnus, N, E, id) %>% 
                            filter(Station!=10))$id)
# saveRDS(g.sgi %>% filter(Cluster==37 | Cluster==34 | Cluster==26 | Cluster==16), 
#         "output/process/sgi_durs.rds")
g.sgi <- g.sgi %>% filter(Cluster==37 | Cluster==34 | Cluster==26 | Cluster==16)
g.sgi %>% 
  ungroup() %>% group_by(id) %>% mutate(mean=median(mean_mon, na.rm=TRUE),
                                                type="sgi") %>%
  ggplot(.) + 
  geom_hline(yintercept = 0) +
  # geom_ribbon(data=. %>% group_by(id, dry_end) %>% filter(any(sgi <= -1.5) & dry_dur > 180) %>%
  #               mutate(dry_dur=as.numeric(dry_dur)),
  #             aes(x=date, ymin=sgi, ymax=0, group=dry_start), colour="black", alpha=.3)+
  # # geom_line(aes(date, sgi, group = id, colour=Station), alpha=.5) +
  geom_line(data=. %>% mutate(type="dat"),
            aes(date, mean_mon-mean, group = id, colour=id), alpha=.3) +
  # xlim(as.Date("1994-01-01"), as.Date("1998-12-12"))+
  facet_wrap(~Cluster, ncol=1) + theme_classic()
# saveRDS(g.sgi, "output/3.sgi.rds")

library(ggmap)
ggmap(stamen) + geom_point(data=g.sgi %>% distinct(id, N, E), aes(N, E)) +
  geom_point(data=g.sgi %>% ungroup() %>% mutate(id=paste(Cluster, Station, sep="_")) %>% 
               # filter(!id %in% g.sgi$id) %>% 
               distinct(id, Cluster, N, E),
             aes(N, E, colour=Cluster), size = 2, alpha=.5)

ggmap(stamen) + geom_point(data=g.sgi %>% filter(dry_dur > 548) %>%
                             filter(Cluster==14 | Cluster==55 | Cluster==37) %>%
                             distinct(Cluster, Station, id, N, E, dry_dur, dry_year),
                           aes(N, E, colour=as.numeric(dry_dur)), alpha=.6) + 
  # scale_colour_discrete(na.value="grey") + 
  facet_wrap(~dry_year)

# # read and standardise met
# tibm <- readRDS("output/process/fennos_tibm_all_2.R") %>% select(E, N, id, date, RRday, Tday, Q, M)
# 
# tibm %>% filter(id=="s_45") %>% 
#   ggplot(.) + aes(date, Q, group=id) %>% geom_line()

g.ds2 <- readRDS("output/process/g_ds_raw2.rds")
readRDS("output/process/g_ds_raw2.rds")
