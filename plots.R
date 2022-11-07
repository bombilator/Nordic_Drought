
# colour scales how: https://ggplot2.tidyverse.org/reference/scale_brewer.html
# pallete source: http://colorbrewer2.org/#type=qualitative&scheme=Set3&n=10
# geom_smooth: http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
# loess : https://stats.stackexchange.com/questions/30975/how-to-add-non-linear-trend-line-to-a-scatter-plot-in-r
# colours: http://sape.inf.usi.ch/quick-reference/ggplot2/colour

# LFSTAT
# https://cran.r-project.org/web/packages/lfstat/lfstat.pdf


# package 'swirl' learning platform

# double mass curves: https://www.researchgate.net/publication/263736142_Trend_analysis_of_rainfall-runoff_regimes_in_selected_headwater_areas_of_the_Czech_Republic
rm(list = ls())

library(magrittr); library(purrr); library(ggplot2); library(dplyr); library(tidyverse)
library(sp);library(lubridate); library(ggmap)

# Read data fi ----
g.df <- #readRDS("output/process/g_df.rds") %>% 
  readRDS("output/process/g_df_raw.rds") %>% 
  distinct(
  Station, Tunnus, N, E, id, year, date, elevation, g_mean, g_geom, gmax, gmin, stdev, 
  # monthday, 
  # mean, gmean, 
  # mean_mon, mean_wt
  mean_dep, freq2, #Mar_Jun_freq2, 
  week2, raw_dep) %>% mutate(country = "fi", id = paste("f_", id, sep="")) 

# there are about 21.5 measurements a year (mean), which averages to 1.79 a month. that would be 4*1.79 for how many
# mean measurements for 4 months you'd expect, which is 7.16. Therefore, we don't want to be missing more than 
# 7.16 out of mean 665.76 measurements in total (mean), in case we don't want to be missing more than four months.
# -> 666-7
# if we on the other hand don't want to be missing more than 1 percent of the desired dataset length, which is 
# 670 measurements = 12 months * 1.8 measurements a month * 31 years, then 1 % is 6.7
# -> 670-6.7
# let's just automate it to n > than mean n (=mean dataset length) - 1%
g.df <- g.df %>% #filter(year >= 1980 & year <=2010) %>% mutate(period=ifelse(year<1990, 1, 8)) %>% 
  filter(year >= 1970 & year <=2017) %>%
  add_count(wt=!is.na(raw_dep), Station, Tunnus) 
filter <- mean(g.df$n) - 0.01*mean(g.df$n)
g.df <- g.df %>% filter(n > filter)
# old filter:
# freq.filt <- g.df %>% #filter(Mar_Jun_freq2 == TRUE &
#                                 # ((year >= 1980 & year<= 1984) |
#                                    # (year >=2006 & year <= 2010))) %>%
#   distinct(id, Tunnus, year) %>%
#   mutate(eighties = ifelse(year >= 1980 & year <= 1995, TRUE, FALSE),
#          tens = ifelse(year >= 1996 & year <= 2010, TRUE, FALSE)) %>%
#   distinct(id, Tunnus, eighties, tens) %>% 
#   # mutate(dupl = id %in% id[duplicated(id)]) %>%
#   mutate(dupl = Tunnus %in% Tunnus[duplicated(Tunnus)]) %>%
#   filter(dupl == TRUE ) %>%
#   distinct(id, Tunnus)
g.df <- g.df  %>% #filter(id %in% freq.filt$id & Tunnus %in% freq.filt$Tunnus) %>% 
  filter(id != "f_382" & id != "f_590" & id != "f_99" & id != "f_2794" & id != "f_591" & 
           id != "f_281" & id != "f_2613")

g.df8 <- readRDS("output/process/g_df8_raw.rds")%>% ungroup() %>%
  mutate(country = "fi", id = paste("f_", id, sep="")) %>% 
  filter(Tunnus %in% g.df$Tunnus)
g.df1 <- readRDS("output/process/g_df1_raw.rds")%>% ungroup() %>%
  mutate(country = "fi", id = paste("f_", id, sep="")) %>% 
  filter(Tunnus %in% g.df$Tunnus)

meta.f <- readRDS("output/process/fi_meta.RDS")

# Read data swe -----
g.ds <- #readRDS("R_finland_drought/output/process/g_ds.rds") %>% 
  readRDS("output/process/g_ds_raw2.rds") %>% 
  distinct(
  Station, Tunnus, N, E, year, date, elevation, week2, mean_dep, g_mean,
  g_geom, gmax, 
  gmin, stdev, 
  #monthday, mean, gmean,
  # material, type, mean_mon, mean_wt
  freq2, Mar_Jun_freq2, raw_dep) %>% mutate(country = "swe") #%>%
  # mutate(id = as.integer(Station))


#for comments see g.df
g.ds <- g.ds %>% filter(year >= 1980 & year <=2010) %>% mutate(period=ifelse(year<1996, 1, 8)) %>% 
  add_count(wt=!is.na(raw_dep), Station, Tunnus) %>% filter(n > filter)
# freq.filt <- g.ds %>%
#   mutate(omr_stn = paste(Station, Tunnus, sep = "_")) %>%
#   filter(Mar_Jun_freq2 == TRUE & 
#            ((year >= 1980 & year<= 1984) | (year >=2006 & year <= 2010))) %>%
#   distinct(Station, omr_stn, Tunnus, year) %>%
#   mutate(eighties = ifelse(year >= 1980 & year <= 1984, TRUE, FALSE),
#          tens = ifelse(year >= 2006 & year <= 2010, TRUE, FALSE)) %>%
#   distinct(Station, omr_stn, Tunnus, eighties, tens) %>%
#   # mutate(dupl = id %in% id[duplicated(id)]) %>%
#   mutate(dupl = omr_stn %in% omr_stn[duplicated(omr_stn)]) %>%
#   filter(dupl == TRUE) %>%
#   distinct(Station, omr_stn, Tunnus)

# g.ds <- g.ds %>%
#   mutate(omr_stn = paste(Station, Tunnus, sep="_")) %>%
#   filter(omr_stn %in% freq.filt$omr_stn) %>% select(-omr_stn)
# coordinates(g.ds) = ~E+N
# proj4string(g.ds) = CRS("+init=epsg:3006")
# g.ds = spTransform(g.ds, CRS("+proj=longlat +datum=WGS84"))
# g.ds = as.data.frame(g.ds)
# swe.met <- readRDS("R_finland_drought/output/process/swe_met.rds") 
# g.ds <- left_join(g.ds, swe.met %>% select(N, E, id) %>% mutate(id = as.integer(id)))

# swedish ids for met data
id <- readRDS("output/process/sweden_ids_tunnus.rds") %>%
  ungroup() %>% 
  mutate(country="swe",
         ids=as.integer(ids)) %>% 
  rename(id = ids)
g.ds <- left_join(g.ds, id) %>% mutate(id = paste("s_", id, sep="")) %>% 
  filter(id != "s_60" & id != "s_49" &id != "s_149" &id != "s_148" &id != "s_28" &id != "s_51" &
           id != "s_127" & id != "s_140" & id != "s_23")

g.ds1 <- readRDS("output/process/g_ds1_raw2.rds") %>% ungroup() %>%
  mutate(country = "swe", id = paste("s_", id, sep="")) %>%
  # mutate(id = as.integer(Station)) %>% select(-Station) %>%
  # mutate(omr_stn = paste(Station, Tunnus, sep="_")) %>%
  filter(id %in% g.ds$id) #%>% select(-omr_stn) %>%
  left_join(., id)
g.ds8 <- readRDS("output/process/g_ds8_raw2.rds")%>% ungroup() %>%
  mutate(country = "swe", id = paste("s_", id, sep="")) %>%
  # mutate(id = as.integer(Station)) %>% select(-Station) %>%
  # mutate(omr_stn = paste(Station, Tunnus, sep="_")) %>%
  filter(id %in% g.ds$id) #%>% select(-omr_stn) #%>%
  left_join(., id)



meta.s <- readRDS("output/process/swe_meta.RDS")
# find the ids still in the dataset with wells in rock
rock <- meta.s %>% distinct(Omr_stn, Akvifertyp) %>% 
  #filter(grepl("berg",Akvifertyp) | grepl("slutet",Akvifertyp)) %>% 
  mutate(Station = sub("_(.*)", "", Omr_stn), Tunnus = sub("(.*)_", "", Omr_stn)) %>% 
  left_join(., g %>% filter(country=="swe") %>% distinct(Station, Tunnus, id)) %>% 
  filter(!is.na(id)) %>% mutate(mark = paste(id, Tunnus, sep="_")) #%>%
  # distinct(id)
rm(meta.s)

# Combine fi and swe data ----
# homogenize water level data
g <- full_join(g.df %>% ungroup(), g.ds %>% mutate(Station= as.character(Station))) 
g <- g %>% 
  mutate(En = round(E*0.001, 0),
         Nn = round(N*0.0001, 0)) 
saveRDS(g, "output/process/g_raw2.rds")

g1 <- full_join(g.df1, g.ds1)
saveRDS(g1, "output/process/g1_raw2.rds")

g8 <- full_join(g.df8, g.ds8)
saveRDS(g8, "output/process/g8_raw2.rds")

# load combined data ----
g <- readRDS("output/process/g_raw2.rds") 
g1 <- readRDS("output/process/g1_raw2.rds")
g1 <- g1 %>% # this will remove Tunnus rows where there are doubles in the 
  # id mean data, and removing stdev==NaN removes individual rows with only
  # one value, as stdev==NaN occurs where there is no standard deviation,
  # i.e. only one value
  group_by(id) %>% filter(!duplicated(g_geom)) %>% as.data.frame

g8 <- readRDS("output/process/g8_raw2.rds")
g8 <- g8 %>% group_by(id) %>% filter(!duplicated(g_geom)) %>% as.data.frame


tibm.pet8 <- readRDS("output/process/fennos_tibm_pet8.rds") %>% filter(id %in% g$id)
tibm.pet1 <- readRDS("output/process/fennos_tibm_pet1.rds") %>% filter(id %in% g$id)


test <- g %>% distinct(Station, Tunnus, id, country)
test <- test %>% mutate(place=ifelse(country=="swe", paste("SE_", Station, sep=""), 
                                     paste("FI_", Station, sep="")))
test <- test %>% filter(id== "f_2043"| id=="f_491"| id=="f_2113"| 
                          id=="f_742"| id=="f_2167" | id=="f_783" | 
                          id=="f_471"| id=="f_3453"| id=="f_3697"| id=="f_1855"| 
                          id=="f_1526"| id=="s_40"| id=="s_126"| id=="s_79"| id=="s_69")

# tibm <- readRDS("output/process/tibm.r")
# tibm <- tibm %>% ungroup() %>%
#   mutate(year = year(date),
#          id = paste("f_", id, sep="")) %>% rename(RH = Hum)
# swe.tibm <- readRDS("output/process/sweden_tibm2.rds")
# swe.tibm <- swe.tibm %>% ungroup() %>%
#   mutate(year = year(date),
#          id = paste("s_", id, sep="")) %>%
#   rename(Tday = Tm, RRday = Precip) %>%
#   select(-Tmax, -Tmin)
# all.tibm <- full_join(tibm %>% filter(date >= as.Date("1980-01-01")), swe.tibm) %>%
#   arrange(id, date)
# all.tibm <- all.tibm %>% filter(date <= as.Date("2010-12-31"))
# saveRDS(all.tibm, "output/process/fennos_tibm_all_2.R")
tibm <- readRDS("output/process/fennos_tibm_all2.rds") %>% 
  dplyr::select(E, N, id, date,Tday, RRday, Q, OSQ)
pet <- readRDS("output/process/fennos_pet_all.r") %>%
  mutate(pet = pet*2)

# g.df <- readRDS("output/process/sgi_durs.rds") %>% ungroup() %>% #readRDS("output/3.sgi.rds") %>% 
#   group_by(id) %>% mutate(date = as.Date(paste(format(date, "%Y-%m"), "-15", sep=""))) %>% 
#   as.data.frame()
# # for RT90 coordinates (download from luftweb manually)
# cors <- g.df %>% distinct(id, Station, N, E)
# coordinates(cors) = ~N+E
# proj4string(cors) = CRS("+proj=longlat + datum=WGS84")
# cors = spTransform(cors, CRS("+init=epsg:3021")) #RT90
# cors = as.data.frame(cors)

stand <- readRDS("output/process/clusters_spi_sgi1911.rds") %>% select(-data) %>% unnest() %>% 
  mutate(type="spi") %>% 
  full_join(., readRDS("output/process/clusters_spei_sgi1911.rds") %>% select(-data) %>% unnest() %>% 
              mutate(type="spei")) %>% 
  full_join(., readRDS("output/process/clusters_smri_sgi1911.rds") %>% select(-data) %>% unnest() %>% 
              mutate(type="smri")) %>% 
  full_join(., readRDS("output/process/clusters_smrei_sgi1911.rds") %>% select(-data) %>% unnest() %>% 
              mutate(type="smrei")) %>% mutate(agg=as.numeric(agg)) %>% 
  arrange(Cluster, Station, agg, date) %>% 
  left_join(., readRDS("output/process/clusters_cmax_amax_all.rds") %>% rename(typemax=type))

a # from calculate sgi.r line 57
stand
a <- a %>% mutate(id=paste(Cluster, Station, sep="_")) %>% 
  filter(id %in% stand$id) %>% group_by(id) %>% 
  mutate(median_mohm = median(level_m, na.rm=TRUE),
         median_cm = median(level_cm, na.rm=TRUE)) %>% 
  select(-date, -level_m, -level_cm, -UnderOkRor, -year, -month) %>% distinct()
a <- a %>% select(-End, -RorHojdOMark, -E, -N)
a <- left_join(a, stand %>% distinct(id, N, E))

# Background map and coordinates ----
bbox = as.numeric(c(8, 54, 33, 71)) # Sweden: c(11,55,24,69)) # Sweden and Finland: c(11, 55, 32, 70))
names(bbox) <- c('left','bottom','right','top')
source("rfuncs/get_stamenmapPNG.R")
stamen_fenn <- get_stamenmapPNG(bbox, zoom = 6, maptype = "toner-background", color="color",
                                force = TRUE) 
bbox = as.numeric(c(-123.30, 49, -121.80, 49.50)) # Fraser valley, zoom 10
stamen_bc <- get_stamenmapPNG(bbox, zoom = 10, maptype = "toner-background", color="color",
                           force = TRUE)

rm(bbox)

# library(maps)
# bg <-map('world',col="grey", fill=FALSE, 
#          bg="white", lwd=0.05, mar=rep(0,4),border=0, 
#          ylim=c(54.5,71), xlim=c(9,32.5),  plot=TRUE)

library(rgdal)
shp.file <- readOGR("C:/Users/xnygmi/Desktop/data/paper_lmtrends", 
        "sea_pol")
shp.file <- fortify(shp.file)
coordinates(shp.file) = ~long+lat
proj4string(shp.file) = CRS("+init=epsg:3067")
shp.file = spTransform(shp.file, CRS("+proj=longlat + datum=WGS84"))
shp.file <- as.data.frame(shp.file)
map <- ggplot() +
  geom_polygon(data = shp.file, aes(x = long, y = lat, group = group),
            color = NA, fill = 'grey', size = .2)
map_projected <- map +
  coord_map()

gstat <- g %>% distinct(Tunnus, Station, id, N, E, country)
plots <- gstat %>% filter(country=="swe") %>% distinct(Tunnus, Station, id, N, E)
coordinates(plots) = ~E+N
proj4string(plots) = CRS("+init=epsg:3006")
plots = spTransform(plots, CRS("+proj=longlat + datum=WGS84"))
plots = as.data.frame(plots)
plotf <- gstat %>% filter(country=="fi") %>% distinct(Tunnus, Station, id, N, E)
coordinates(plotf) = ~E+N
proj4string(plotf) = CRS("+init=epsg:3067")
plotf = spTransform(plotf, CRS("+proj=longlat + datum=WGS84"))
plotf = as.data.frame(plotf)
plot <- full_join(plots, plotf)
rm(plotf, plots, gstat)
plot <- plot %>% filter(!id %in% rock$id)
# library(rworldmap)
# newMap <- getMap(resolution="low")  # you may use resolution="low"
# plot(newMap, xlim=c(25, 27), ylim=c(54, 70), usePolypath=FALSE) # Sweden and Finland
# ggmap(newMap)



# Wavelet power analysis: ----
# source: https://www.hydrol-earth-syst-sci-discuss.net/hess-2019-119/hess-2019-119.pdf
# source: http://www.hs-stat.com/projects/WaveletComp/WaveletComp_guided_tour.pdf
library(WaveletComp)
station = "f_99"
data <- all.tibm %>% group_by(id) %>% filter(id==station &
                                               year >= 1980 &
                                               year <= 2015) %>% 
  select(id, date, Tday, RRday, year) %>% group_by(id, year) %>% 
  mutate(t = mean(Tday), # - sd(Tday), 
         p = mean(RRday)) %>% # - sd(RRday))
  distinct(id, year, t, p)
data %>% ggplot(.) + geom_line(aes(year, t))
my.w <- analyze.wavelet(data, 
                        "t",loess.span = 0.75, dt = 1, dj = 1/250,
                        lowerPeriod = 2^1, upperPeriod = 2^5,make.pval = TRUE, n.sim = 10)
wt.image(my.w, color.key = "interval", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", 
                              label.digits = 2),periodlab = "periods (days)", 
         # maximum.level = 1.4,
         # Concerning item 1 above --- plot the square root of power:
         exponent = 0.5,
         # Concerning item 2 above --- time axis:
         show.date = TRUE, date.format = "%F", timelab = "",
         spec.time.axis = list(at = c(paste(1980:2015, "-01-01", sep = "")),
                               labels = c(1980:2015)),timetcl = -0.5,  # draws outward ticks
         # Concerning item 3 above --- period axis:
         spec.period.axis =list(at = c(1, 2, 7, 16)),#32, 365, 1024, 2555, 5840)),
         periodtck = 1, periodtcl = NULL  # draws horizontal lines
         )
reconstruct(my.w, plot.waves = FALSE, lwd = c(1,2),legend.coords = "bottomleft")

my.rec <- reconstruct(my.w)
x.rec <- my.rec$series$t.r
plot(x.rec, type = "l", col = "red")

reconstruct(my.w, sel.period = 7, plot.waves = TRUE, 
            lwd = c(1,2), legend.coords = "bottomleft", ylim = c(-1, 1), 
            show.date = TRUE, date.format = "%F", timelab = "")

reconstruct(my.w, sel.period = 3000, plot.waves = TRUE,
            legend.coords = "bottomleft", siglvl = 0.5, ylim = c(-1, 1), 
            show.date = TRUE, date.format = "%F", timelab = "")

my.wx <- analyze.wavelet(data, "t", loess.span = 0.75,dt = 1, dj = 1/20,
                         lowerPeriod = 2^1, upperPeriod = 2^5,make.pval = TRUE, 
                         n.sim = 10)
wt.avg(my.wx, exponent = 0.5)

plot(1, my.w$Power.avg)




# plotting decadal trends ----

# Meteorology:
test <- tibm %>%  mutate(month = month(date),
                         season = ifelse(month == 9 | month == 11 | month == 10, "Sep-Nov", NA),
                         season = ifelse(month == 1 | month == 2 | 
                                           month == 12, "Dec-Feb", season),
                         season = ifelse(month == 4 | month == 5 | 
                                           month == 3, "Mar-May", season),
                         season = ifelse(month == 7 | month == 8 | 
                                           month == 6, "Jun-Aug", season),
                         year = year(date)) %>% group_by(id, year) %>% 
  mutate(RRyr = sum(RRday), Tyr = mean(Tday)) %>% group_by(id) %>% 
  mutate(RRmean = mean(RRyr), Tmean = mean(Tyr))
test <- test %>% select(-N, -E) %>% filter(id %in% plot$id) %>% 
  left_join(., plot %>% distinct(id, E, N) %>% rename(lat=N)) %>% filter(year>=1980 & year <= 2010) %>% 
  mutate(group = ifelse(lat < 59, "-59˚", NA),
         group = ifelse(lat >= 59, "59˚-63˚", group),
         group = ifelse(lat > 63, "63˚-", group),
         decade=ifelse(year<1990 & year >= 1980, "1980s", "2000s"),
         decade=ifelse(year<2001 & year >= 1990, "1990s", decade),
         alt=ifelse(year<1995, "1", "2")) 

test.g <- g  %>% filter(!is.na(raw_dep)) %>% #filter(country=="swe") %>%
  distinct(Station, Tunnus, id, date, g_geom, year)%>% 
  group_by(id, year) %>%
  mutate(yr_gw = median(g_geom)) %>%
  ungroup() %>%
  left_join(., plot %>% distinct(id, E, N) %>% rename(lat=N)) %>% 
  mutate(group = ifelse(lat < 59, "-59˚", NA),
         group = ifelse(lat >= 59, "59˚-63˚", group),
         group = ifelse(lat > 63, "63˚-", group)) %>%
  group_by(id, year) %>%
  mutate(yr_gw_all = median(g_geom)) %>% distinct(Station, Tunnus, id, year, yr_gw, yr_gw_all, group)

t <- test %>% distinct(group, id, RRyr, Tyr, RRmean, Tmean, year, decade, alt) %>% 
           ggplot(.) + 
  geom_line(data=. %>% group_by(group, year) %>% mutate(mean = mean(Tyr)),
            aes(year, mean, group = group, linetype="year"), colour="black",
            size=.1) +
  geom_line(data=. %>% group_by(group, decade) %>% mutate(mean = mean(Tyr)),
            aes(year, mean, group=group, colour=group, linetype="10-year"), 
            size=1) +
  geom_line(data=. %>% group_by(group, alt) %>% mutate(mean = mean(Tyr)),
            aes(year, mean, group=group, colour=group, linetype="15-year"), 
            size=1) +
  theme(axis.text =element_text(size=10), 
        panel.background=element_rect(fill="white", colour="black"),
        panel.grid.major = element_blank(),
        axis.text.x = element_blank()) +
  scale_colour_viridis_d(guides(name="Latitude"), direction=-1) +
  scale_linetype_manual("period", values=c("year"=3, "10-year"=1, 
                                           "15-year"=2),
                        breaks=c("year", "10-year", "15-year")) +
  ylab("Temperature [ºC]") 
p <- test %>% distinct(group, id, season, RRyr, Tyr, RRmean, Tmean, year, decade, alt) %>%
  ggplot(.) +
  geom_line(data=. %>% group_by(group, year) %>% mutate(mean = mean(RRyr)),
            aes(year, mean, group = group, linetype="year"), colour="black",
            size=.1)+
  geom_line(data=. %>% group_by(group, decade) %>% mutate(mean = mean(RRyr)),
            aes(year, mean, group=group, colour=group, linetype="10-year"), 
            size=1) +
  geom_line(data=. %>% group_by(group, alt) %>% mutate(mean = mean(RRyr)),
            aes(year, mean, group=group, colour=group, linetype="15-year"), 
            size=1) +
  theme(axis.text =element_text(size=10), 
        panel.background=element_rect(fill="white", colour="black"),
        panel.grid.major = element_blank()) +
  scale_colour_viridis_d(guides(name="Latitude"), direction=-1) +
  scale_linetype_manual("period", values=c("year"=3, "10-year"=1, 
                                           "15-year"=2),
                        breaks=c("year", "10-year", "15-year")) +
  ylab("Precipitation [mm]") 
  
wt <- test.g %>% ggplot(.) + 
  geom_line(aes(year, yr_gw_all, colour = group, group = id), alpha=.4, size=.1) +
  # geom_smooth(aes(year, yr_gw_all, group = group), colour = "white", method = 'lm', level = 0, size=1.5) +
  geom_smooth(aes(year, yr_gw_all, group = group, colour = group), method = 'lm', level = 0, size=.6) +
  xlim(1980, 2011) + theme(axis.text =element_text(size=10))+ 
  scale_colour_viridis_d("Latitude", direction=-1) + ylab("average, m /yr") + 
  scale_y_reverse() 

library(cowplot)
prow1 <- plot_grid(t + theme(legend.position = "none",
                             axis.title.x = element_blank()),
                   p + theme(legend.position = "none"), 
                   # wt + theme(legend.position = "none") + ggtitle("Water tables"),
                   nrow=2, align = "hv", axis = "lb", ncol = 1,
                   rel_heights = c(1,1))
legend1 <- get_legend(t)
plot_grid(prow1, legend1, rel_widths = c(5,2), ncol = 2)


test %>% ungroup() %>% 
  group_by(decade, group#, season
           ) %>% mutate(meanp = mean(RRday, na.rm=TRUE),
                                           meant = mean(Tday, na.rm=TRUE)) %>%
  # group_by(alt, group#, season
  #          ) %>% mutate(meanp = mean(RRday, na.rm=TRUE),
  #                                         meant = mean(Tday, na.rm=TRUE)) %>%
  distinct(group, meanp, meant, #alt#, 
           decade#,
           # season
           ) %>% View()

# plotting the annual mean fluctuations, all ids ----
# wb_gw <- 
g1  %>% #filter(id==station) %>%
  ungroup() %>%
# do(gw = 
     ggplot(.) +
     geom_hline(yintercept=0, alpha=.2,
                linetype = 2) +
   # geom_ribbon(aes(ymd,
   #                 ymin= (gmin - mean(g_geom)),
   #                 ymax =(gmax - mean(g_geom)),
   #                 fill = "water table"),
   #             alpha =.2) +
     geom_line(data = . %>% filter(!is.na(mean_dep)),
               aes(ymd, mean_dep,
                   colour =  as.factor(Tunnus),
                   linetype = as.factor(year)),
               alpha= .5, show.legend = FALSE) +
     geom_line(aes(ymd, g_geom), colour = "black", size=1) +
  scale_y_reverse(breaks=c(1, 0, -1), limits=c(2.5,-2.5), 
                  name="normalised biweekly mean water table [m]") +
     facet_wrap(~id, nrow=5) +
     theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
           # axis.ticks.x = element_blank(),
           axis.text.x = element_blank(),
           panel.background = element_blank(),
           # strip.text.x = element_blank()
           legend.position = "none", axis.text =element_text(size=12), 
           axis.line.x = element_blank(),
           axis.line.y = element_line(colour="black")) + 
    ggtitle("2001-2010 (P2)") +
    scale_x_date(date_labels = "%m", date_breaks = "3 months", name="December-November")

{
wb_met = swe.met %>% 
  # filter(id %in% g.df$id) %>%
  ungroup() %>%
  # group_by(id) %>%
  do(met = ggplot(.) +
       # geom_col(aes(date, Q, group = id, fill = "Snowmelt"),
       #          alpha = .7) +
       # 
       geom_col(aes(date, Precip, group = id, fill = "Rain"),
                alpha=.5) +
       geom_line(aes(date, Tm-272.15, group = id, colour = "T"), alpha=.5) +

       # geom_col(aes(date, P_snow, group = id, fill = "Snow"),
       #          alpha=.4) +
#        
#        geom_ma(aes(date, Q, colour = "Snowmelt"), alpha=.4, size = 1,
#                ma_fun=SMA, n=60) +
#        
#        geom_ma(aes(date, RRday, colour = "Rain"), alpha=.5, size =1,
#                ma_fun=SMA, n=60) +

      # geom_col(aes(date, M, group = id, fill = "Snowmelt"),
      #                    alpha=.4) +

      # geom_col(aes(date, sum_Snow, group =id, fill = "Snow"), alpha=.4) +

       xlim(as.Date("2000-10-01"),as.Date("2001-10-01")) +
       scale_colour_manual("",
                           values=c("smri"="purple",
                                    "sgi"="salmon",
                                    "spei_q"="lightskyblue",
                                    "spi"="red",
                                    "spei"="orange",
                                    "T" = "red",
                                    "Rain"="steelblue2",
                                    "Snow"="purple",
                                    "Snowmelt" = "gray50")) +
       scale_fill_manual("",
                         values=c("Rain"="steelblue2",
                                  "Snow"="purple",
                                  "Snowmelt" = "gray50")) +
       # scale_y_continuous(limits = #c(-2.5,2.5))
       #   c(0,25))+
       #   sec.axis=sec_axis(~./5+10, "m")) +
       ylab(label = "mm") + theme_classic() +
       theme(panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.line.x = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "cm")) +
       facet_wrap(~id, nrow=4)
  )
  }

library(cowplot)
# for this plot, the above dataframes need to be ungrouped (not grouped by id)
plot_grid(
  # wb_met$met[[1]],
          wb_gw$gw[[1]],
          align = "hv", axis = "lb", ncol = 1,
          rel_heights = c(1,1)
          )

# for this plot, the above dataframe need to be grouped by id (not ungrouped)
for(i in 1:length(wb_gw$id)){
  name = paste("wb-", wb_gw$id[i], ".jpeg", sep="")
  plot = plot_grid(wb_met$met[[i]], 
              wb_gw$gw[[i]], 
              align = "hv", axis = "lb", ncol = 1,
              rel_heights = c(1.5,1))
  save_plot(name, plot)
}



# general surface water balance ----
# a = 
context <- tibm %>% ungroup() %>% distinct(id, date, Tday, RRday) %>% 
  filter(date >= as.Date("1980-01-01") & date <= as.Date("1990-01-01")) %>% 
  rename(ymd=date) %>% 
  filter(id== "f_1162" | id == "s_157" | id == "f_3453" | id == "s_98" |id == "f_131" | id == "s_46" |
           id=="s_93" | id == "f_2465")  %>% distinct(id, ymd, Tday, RRday) %>% 
  # left_join(., pet %>% distinct(id, pet, ymd)) %>% 
  mutate(month=month(ymd), year = year(ymd)) %>%
  group_by(id, year, month) %>% mutate(P = sum(RRday), Temp = mean(Tday)) %>% 
  # mutate(qpet = (Q - pet/1000), osqpet =(OSQ - pet/1000)) %>% 
  left_join(., g %>% distinct(Station, Tunnus, id, date, raw_dep, elevation) %>% rename(ymd=date)) %>% 
  transform(id=factor(id, levels=c("f_3453", "s_93", "f_2465", "f_1162", "s_46", "f_131", "s_98", "s_157")))%>%
  mutate(id = as.character(id),id = replace(id, id == "f_3453", 1),
         id = replace(id, id == "s_93", 2), id = replace(id, id == "f_2465", 3), id = replace(id, id == "f_1162", 4),
         id = replace(id, id == "s_46", 5), id = replace(id, id == "f_131", 6),id = replace(id, id == "s_98", 7),
         id = replace(id, id == "s_157", 8)) %>%
  filter(!is.na(raw_dep)) 
library(cowplot); library(scales)
a <- context %>% ungroup() %>% group_by(id) %>% mutate(median=raw_dep - median(raw_dep, na.rm=TRUE),
                                                  name= paste(id," [", Tunnus, "]", sep=""),
                                                  type="h") %>% 
  # filter(Tunnus!="1204p8") %>% 
            filter(id==2) %>% #| (id==3 & Tunnus =="1204p10") | (id == 7 & Tunnus ==9) | id == 8) %>%
  ggplot(.) + geom_col(data=. %>% mutate(type="P"), aes(ymd, P), colour="black", fill="black") + 
  geom_line(data=. %>% mutate(type="T"), aes(ymd, Temp), colour="black") + 
  geom_line(aes(ymd, raw_dep, group=Tunnus), colour="black", size=.7) +
      # scale_y_continuous(sec.axis=sec_axis(~(.*10), "P [mm], T [˚C]")) +
      # ylab("depth from ground surface [m]") + xlab("year") +
  xlab("") + ylab("") +
  scale_y_continuous(breaks = pretty_breaks(n = 2)) +
      facet_grid(type~name, scales= "free_y") +
      theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), 
            axis.text.x = element_blank(),
            panel.background = element_rect(fill="white"),
            axis.text =element_text(size=10),
            axis.line = element_line(colour="black"))
b <- context %>% ungroup() %>% group_by(id) %>% mutate(median=raw_dep - median(raw_dep, na.rm=TRUE),
                                                  name= paste(id," [", Tunnus, "]", sep=""),
                                                  type="h") %>% 
  filter(id==3 & Tunnus =="1204p10") %>%
  ggplot(.) + geom_col(data=. %>% mutate(type="P"), aes(ymd, P), colour="black", fill="black") + 
  geom_line(data=. %>% mutate(type="T"), aes(ymd, Temp), colour="black") + 
  geom_line(aes(ymd, raw_dep, group=Tunnus), colour="black", size=.7) +
  ylab("") + xlab("") +  
  scale_y_continuous(breaks = pretty_breaks(n = 2)) +
  facet_grid(type~name, scales= "free_y") +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        panel.background = element_rect(fill="white"),
        axis.text.x=element_blank(),
        axis.text =element_text(size=10),
        axis.line = element_line(colour="black"))
c <- context %>% ungroup() %>% group_by(id) %>% mutate(median=raw_dep - median(raw_dep, na.rm=TRUE),
                                                  name= paste(id," [", Tunnus, "]", sep=""),
                                                  type="h") %>% 
  filter((id == 7 & Tunnus ==9)) %>%
  ggplot(.) + geom_col(data=. %>% mutate(type="P"), aes(ymd, P), 
                       colour="black", fill="black") + 
  geom_line(data=. %>% mutate(type="T"), aes(ymd, Temp), colour="black") + 
  geom_line(aes(ymd, raw_dep, group=Tunnus), colour="black", size=.7) +
  ylab("") + xlab("") +
  scale_y_continuous(breaks = pretty_breaks(n = 2)) +
  facet_grid(type~name, scales= "free_y") +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        panel.background = element_rect(fill="white"),
        axis.text.x = element_blank(),
        axis.text =element_text(size=10),
        axis.line = element_line(colour="black"))
d <- context %>% ungroup() %>% group_by(id) %>% mutate(median=raw_dep - median(raw_dep, na.rm=TRUE),
                                                  name= paste(id," [", Tunnus, "]", sep=""),
                                                  type="h") %>% 
  filter(id==8) %>%
  ggplot(.) + geom_col(data=. %>% mutate(type="P"), aes(ymd, P), colour="black", fill="black") + 
  geom_line(data=. %>% mutate(type="T"), aes(ymd, Temp), colour="black") + 
  geom_line(aes(ymd, raw_dep, group=Tunnus), colour="black", size=.7) +
  ylab("") + xlab("") + 
  scale_y_continuous(breaks = pretty_breaks(n = 2)) +
  facet_grid(type~name, scales= "free_y") +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        panel.background = element_rect(fill="white"),
        axis.text =element_text(size=10),
        axis.line = element_line(colour="black"))
library(grid); library(gridExtra)
y.grob <- textGrob("depth from ground surface [m], P [mm], T [ºC]", rot=90,
                   gp = gpar(fontsize=10))
x.grob <- textGrob("year", rot=0,gp = gpar(fontsize=10))
grid.arrange(arrangeGrob(plot_grid(a,b,c,d, ncol=1, align = "v"), 
                         left = y.grob, bottom = x.grob))

# water balance with gw, investigating ids----
require(smooth)
require(Mcomp)


stand %>% group_by(Cluster, Station) %>%# filter(Cluster==37) %>% 
  filter(agg==1 & type=="spi" & period=="allrec") %>%
  ggplot(.) +
    # GW: ----
    # geom_line(data = . %>% filter(!is.na(mean_dep)),
    #           aes(ymd, mean_dep,
    #               linetype =  as.factor(Tunnus),
    #               colour = as.factor(year)),
    #           alpha= .5, show.legend = FALSE) +
    # geom_point(aes(ymd, mean_dep,
    #           shape = as.factor(Tunnus),
    #           colour = as.factor(year)),
    #           alpha=.5) +
    # geom_line(aes(ymd, g_geom, colour = "water table"),
    #           colour= "black", size = 0.8, alpha=.6) +
    # # geom_smooth(aes(ymd, g_geom*1), level=0) +
    # geom_line(data=. %>% group_by(id) %>% mutate(mean=median(mean_mon, na.rm=TRUE)),
    #       aes(date, mean_mon-mean, group = id, colour=id), alpha=.3) +
  #   geom_vline(aes(xintercept=as.Date("2002-04-15"))) +  
  # geom_vline(aes(xintercept=as.Date("2002-07-15"))) +  
  # geom_vline(aes(xintercept=as.Date("2002-12-15"))) +  
  # geom_vline(aes(xintercept=as.Date("2003-04-15"))) +  
  # geom_vline(aes(xintercept=as.Date("2003-07-15"))) +  
  # geom_vline(aes(xintercept=as.Date("2003-12-15"))) +  
  # geom_vline(aes(xintercept=as.Date("2002-04-15"))) +  
  # geom_vline(aes(xintercept=as.Date("2002-07-15"))) +  
  # geom_vline(aes(xintercept=as.Date("2002-12-15"))) +  

    # standardised ----
  #   # geom_rect(data=.%>%filter(Cluster!=34) %>% distinct(Cluster, date),
  #   #           aes(xmin=as.Date("1995-01-15"), xmax=as.Date("1997-01-15"),
  #   #               ymin=-4, ymax=2), alpha=.1) +
  # #vlines to show which areas where used for drought corr calcs:
  geom_vline(data=.%>% filter(Cluster==16 | Cluster==26) %>% 
               distinct(Cluster,id, date),
        aes(xintercept=as.Date("2002-08-15"))) +
  geom_vline(data=.%>% filter(Cluster==16 | Cluster==26) %>% distinct(Cluster, id, date),
             aes(xintercept=as.Date("2003-06-15"))) +
  geom_vline(data=.%>%filter(Cluster==34) %>% distinct(Cluster,id, date),
             aes(xintercept=as.Date("2003-01-15"))) +
  geom_vline(data=.%>%filter(Cluster==37) %>% distinct(Cluster,id, date),
             aes(xintercept=as.Date("2004-06-15"))) +
  geom_vline(data=.%>%filter(Cluster==37) %>% distinct(Cluster,id, date),
             aes(xintercept=as.Date("2002-05-15"))) +
  # # short and long aggs for one met type:
  #   # geom_ribbon(data = . %>%
  #   #               filter(period == "03" & type==typemax) %>%
  #   #               mutate(dur=ifelse(agg<Amax, "short", "long")) %>% # customise long and short according to Amax
  #   #               group_by(id, date, dur) %>%
  #   #               mutate(mini=min(index, na.rm=TRUE), maxi=max(index, na.rm=TRUE)),
  #   #           aes(date, ymin=mini, ymax=maxi, colour=dur), #group=agg),
  #   #         alpha=.2) +
  # 
  # # all aggs and all met types:
  # geom_ribbon(data = . %>%
  #             filter(period == "allrec") %>% group_by(id, date, type) %>%
  #             mutate(mini=min(index, na.rm=TRUE), maxi=max(index, na.rm=TRUE)),
  #         aes(date, ymin=mini, ymax=maxi, group = type, fill=type, colour=type), alpha=.6) +
  # 
  # # different mets same agg period:
  # geom_line(data=. %>% filter(period=="allrec" & type==typemax & agg == Amax), 
  #           aes(date, index, group=type, colour="Cmax"), size=1) +
  # # geom_line(data=. %>% filter(period=="allrec" & type!=typemax & agg == Amax), 
  # #           aes(date, index, group=type, colour=type), size=.9, linetype=2) +
  # geom_line(data=. %>% filter(period=="95" & type==typemax & agg == Amax),
  #           aes(date, index, group=type, colour="Amax"), linetype=2, size=1) +
  # 
  geom_line(data=. %>% distinct(Cluster, Station, id, date, sgi),
    aes(date, sgi, group=Station, colour=Station), alpha = .8) +
    geom_ribbon(data=. %>% group_by(id, dry_year) %>% filter(any(sgi <= -1.5) & dry_dur > 180) %>%
                mutate(dry_dur=as.numeric(dry_dur)),
              aes(x=as.Date(dry_dat), ymin=sgi, ymax=0, group=interaction(dry_start, Station)), alpha=.2)+
    # general ----
  geom_hline(yintercept = 0, alpha=.5) +
    # xlim(as.Date("1989-01-15"),as.Date("1999-01-15")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                 limits=c(as.Date("1983-01-15"),as.Date("1990-01-15"))) +
    theme_classic() +
    facet_wrap(~Cluster, ncol=1) 
    # scale_colour_manual(values=c("sgi" = "black", "long" = "red", 
    #                              "short" = "cyan", "Cmax" = "blue",
    #                              "Amax"="cyan",
    #                              # "spei" = "red",
    #                              # "spi" = "darkgreen", "smri" = "yellow",
    #                              # "smrei" = "orange",
    #                              "smrei" = "#440154ff", 
    #                              "smri" = "#3b518bff", 
    #                              "spei" = "#42bb72ff",
    #                              "spi" = "#fde725ff"
    #                              # "all spi" = "lightgreen",
    #                              # "all spei" = "pink",
    #                              # "all smri" = "yellow",
    #                              # "all smrei" = "orange"
    #                              )) +
  scale_fill_manual(values=c("smrei" = "#440154ff", 
                             "smri" = "#3b518bff", 
                             "spei" = "#42bb72ff",
                             "spi" = "#fde725ff"))

g.ds %>%
  group_by(Station, year) %>%
  mutate(yr_gw = psych::geometric.mean(g_geom+100)-100) %>% 
  # filter(Station != ) %>% #%in% dtibm$id) %>%
  ggplot(.) + 
  geom_line(aes(year, yr_gw,
              colour = Station), alpha=.3) +
  # xlim(as.Date("2000-01-01"), as.Date("2000-12-31"))+
  xlim(1975,2014)+
  # ylim(-1,1)+
  theme_classic() 

# heatmap ----
smri_sgi_corr <- readRDS("output/process/fin_smri_sgi10_corr.rds")
smri_sgi_corr$cor_sgi_smri %>% dplyr::filter(id==2491) %>% head
spi_sgi_corr <- readRDS("output/process/fin_spi_sgi_corr.rds")

smri_sgi_corr %>% select(id, Stationname, Tunnus, cor_sgi_smri) %>% unnest %>% 
  filter(id==2491) %>%
  ggplot(.) + 
  aes(as.numeric(agg), Tunnus, fill = cor_sgi_smri, group = id) + geom_tile() + 
  scale_fill_gradient2(low="blue", mid = "yellow", high="red", midpoint = 0.4,
                       limits=c(0.05,0.65)) #+
facet_wrap(~id)
spi_sgi_corr %>% select(id, Stationname, Tunnus, cor_sgi_spi) %>% unnest %>% 
  filter(id==2491) %>% ggplot(.) + 
  aes(as.numeric(agg), Tunnus,fill = cor_sgi_spi) + geom_tile() + 
  scale_fill_gradient2(low="blue", mid = "yellow", high="red", midpoint = 0.4,
                       limits=c(0.05,0.65)) 

heatmap()

# plot the stations on a map ----
library(ggmap)
library(sp)
source("Rscripts/get_stamenmapPNG.R")

plot <- plot %>% mutate(mean_rec = (g %>% filter(!is.na(mean_dep)) %>% 
                                             group_by(Station, Tunnus, id) %>% 
                                             mutate(mean_dep = mean(raw_dep)) %>%
                                      distinct(Tunnus, Station, mean_dep))$mean_dep %>% round(.,2))
library(ggrepel)
ggmap(stamen) + geom_point(aes(x = E, y = N, colour = mean_rec), size = 1.5, alpha=.7,
                               position=position_jitter(h=0.05,w=0.05), data = plot) + 
  scale_colour_viridis_c(trans="log10", name = paste("\n", "mean water", "\n", "table depth", "\n", 
                                                     "m")) +
  geom_label_repel(data =  plot %>% group_by(id) %>% mutate(E = first(E), N = first(N)) %>% 
                     distinct(id, N, E, Station) %>% 
                     filter(id== "f_1162" | id == "s_157" |
                              id == "f_3453" | id == "s_98" | 
                              id == "f_131" | id == "s_46" |
                              id=="s_93" | id == "f_2465"),
                   aes(E, N, label=id),
                   box.padding   = 0.4, 
                   point.padding = 0.5,
                   segment.color = 'grey50', alpha = .7, size = 3)


# make shapefiles/maps ----
library(sp)

sweplot <- read_rds("input/SWE_data/processed/gw_swe.RDS")$meta %>% distinct(Omr_stn, Namn, Start, End, N, E) %>%
  rename(Stattionname = Namn, Tunnus = Omr_stn) %>% filter(!is.na(E), !is.na(N))
coordinates(sweplot) = ~N+E
proj4string(sweplot) = CRS("+init=epsg:3006") 
sweplot = spTransform(sweplot, CRS("+proj=longlat +datum=WGS84"))
raster::shapefile(sweplot, "obs_wells_sweden.shp")


finplot <- read_rds("output/process/fin_gw_sgi.rds") %>%
  # g.df %>%
  distinct(Stationname, Tunnus, id, N, E, country) #%>% filter(id %in% g.df$id)
coordinates(finplot) = ~E+N
proj4string(finplot) = CRS("+init=epsg:3067") 
finplot = spTransform(finplot, CRS("+proj=longlat +datum=WGS84"))
#spTranform enough for a shapefile? from: https://gis.stackexchange.com/questions/214062/create-a-shapefile-from-dataframe-in-r-keeping-attribute-table
raster::shapefile(finplot, "obs_wells_finland.shp")

gstat <- g %>% distinct(id, Station, Tunnus, N, E, elevation, country)
plots <- gstat %>% filter(country=="swe") 
coordinates(plots) = ~E+N
proj4string(plots) = CRS("+init=epsg:3006")
plots = spTransform(plots, CRS("+proj=longlat + datum=WGS84"))
plots = as.data.frame(plots)
plotf <- gstat %>% filter(country=="fi") 
coordinates(plotf) = ~E+N
proj4string(plotf) = CRS("+init=epsg:3067")
plotf = spTransform(plotf, CRS("+proj=longlat + datum=WGS84"))
plotf = as.data.frame(plotf)
plot <- full_join(plots, plotf)
rm(plotf, plots, gstat)

# coordinates(plot) = ~E+N
# proj4string(plot) = CRS("+proj=longlat + datum=WGS84")
# raster::shapefile(plot, "selected_obs_wells_all.shp", overwrite=TRUE)

# reclassify the geology map
library(rgdal)
geo <- readOGR("C:/Users/xnygmi/Desktop/data/fennos_geology.shp")
class(geo)
str(geo)
plot(geo)
as.data.frame(geo)
geo.dat <- geo %>% as.data.frame() %>% select(GEO, area_id, AgeName, Portr_Petr)
geo.dat <- geo.dat %>% mutate(AgeName2 = AgeName,
  AgeName2 = gsub('Early ', '', AgeName2),
  AgeName2 = gsub('Late ', '', AgeName2),
  AgeName2 = gsub('Middle ', '', AgeName2),
  AgeName2 = gsub('I', '', AgeName2),
  AgeName2 = gsub('II', '', AgeName2),
  AgeName2 = gsub(' ', '', AgeName2),
  AgeName2 = ifelse(grepl('Jurassic', AgeName2) |
                     grepl('Triassic', AgeName2) |
                     grepl('Cretaceous', AgeName2), 'Mesozoic', AgeName2),
  AgeName2 = ifelse(grepl('Cambrian', AgeName2) |
                      grepl('Palaeozoic', AgeName2)|
                     grepl('Ordovician', AgeName2) |
                     grepl('Silurian', AgeName2)|
                     grepl('Devonian', AgeName2)|
                     grepl('Carboniferous', AgeName2)|
                     grepl('Permian', AgeName2), 'Paleozoic', AgeName2),
  AgeName2 = ifelse(grepl('Archean', AgeName2) |
                     grepl('Proterozoic', AgeName2), 'Precambrian', AgeName2),
  AgeName2 = ifelse(grepl('Paleocene', AgeName2) |
                      grepl('Palaeocene', AgeName2)|
                     grepl('Eocene', AgeName2) |
                     grepl('Oligocene', AgeName2)|
                     grepl('Miocene', AgeName2)|
                     grepl('Pliocene', AgeName2)|
                     grepl('Neogene', AgeName2), 'Cenozoic', AgeName2))
geo.dat <- geo.dat %>% 
  mutate(Portr_Petr2 = as.character(Portr_Petr),
    Portr_Petr2 = ifelse(is.na(Portr_Petr) | grepl('undifferentiated', Portr_Petr), 
                             'Undifferentiated rocks', Portr_Petr2),
    Portr_Petr2 = ifelse(grepl('gypsum', Portr_Petr) | grepl('salt', Portr_Petr), 
                        'evaporite rocks', Portr_Petr2),
    Portr_Petr2 = ifelse(grepl('gravel', Portr_Petr2), 'unconsolidated clastic rocks', Portr_Petr2),
    Portr_Petr2 = ifelse(grepl('dolostone', Portr_Petr) |
                             grepl('carbonates', Portr_Petr) |
                             grepl('limestone', Portr_Petr), 'carbonate rocks', Portr_Petr2),
    Portr_Petr2 = ifelse(grepl('shale', Portr_Petr) |
                          grepl('silt', Portr_Petr) |
                          grepl('mud', Portr_Petr) |
                          grepl('sand', Portr_Petr) |
                          grepl('clay', Portr_Petr) |
                          grepl('marl', Portr_Petr), 
                        'consolidated clastic rocks', Portr_Petr2),
    Portr_Petr2 = ifelse((grepl('meta', Portr_Petr) |
                          grepl('granite', Portr_Petr) |
                          grepl('gneiss', Portr_Petr)|
                          grepl('schist', Portr_Petr)|
                          grepl('granulite', Portr_Petr)|
                          grepl('basalt', Portr_Petr)|
                          grepl('dolerite', Portr_Petr)|
                          grepl('andesite', Portr_Petr)|
                          grepl('granodiorite', Portr_Petr)|
                          grepl('rhyolite', Portr_Petr)|
                          grepl('diorite', Portr_Petr)|
                          grepl('amphibolite', Portr_Petr)|
                          grepl('migmatite', Portr_Petr)|
                          grepl('mafic', Portr_Petr)|
                          grepl('lamprophyre', Portr_Petr)|
                          grepl('phonolite', Portr_Petr)|
                          grepl('latite', Portr_Petr)|
                          grepl('gabbro', Portr_Petr)|
                          grepl('serpentinite', Portr_Petr)|
                          grepl('alkali', Portr_Petr)|
                           grepl('phyllite', Portr_Petr)|
                           grepl('quartzite', Portr_Petr)), 
                        'igenous and metamorphic rocks', Portr_Petr2))
geo.dat <- geo.dat %>% mutate(AgeName2 = ifelse(Portr_Petr2 == "Undifferentiated rocks", NA, AgeName2),
                              Portr_Petr2 = as.factor(Portr_Petr2)) %>% 
  distinct(GEO, area_id, AgeName2, Portr_Petr2)

require(sp)
geo2 <- merge(geo,geo.dat, by=c("GEO", "area_id"))
plot(geo2)

raster::shapefile(geo2, "fennos_geology_reclass.shp", overwrite=TRUE)


# plot mean water table ----
library(ggrepel)

g %>% group_by(id, Tunnus) %>% 
  mutate(mean_wt = median(raw_dep, na.rm=TRUE)) %>% 
  distinct(id,
           Tunnus, mean_wt, elevation, N, country) %>% #filter(id %in% dtibm$id) %>% 
  mutate(Nn = round(N*0.001,0)) %>%
  ggplot(.) + 
  theme_classic() +
  aes(mean_wt, N ) + #Nn) + #, colour = max(gmax)-min(gmin)) + 
  geom_point(alpha=.4, aes(colour = country)) +
  geom_label_repel(data = . %>% filter(mean_wt > 10 | mean_wt < 0) %>% 
                     distinct(id, Tunnus, mean_wt, elevation, N) %>%  
                     mutate(Nn = round(N*0.001,0)),
                                       aes(mean_wt, N, label=id),
                   box.padding   = 0.4, 
                   point.padding = 0.5,
                   segment.color = 'grey50', alpha = .7, size = 3) #+
  scale_colour_gradient2(low="blue", mid = "yellow", high="red", midpoint = 0)
                         
g %>% group_by(id, Tunnus) %>%
  # filter(id== "f_1162" | id == "s_157" |
  #          id == "f_3453" | id == "s_98" | 
  #          id == "f_131" | id == "s_23" |
  #          id=="s_93" | id == "f_2465") %>% 
  distinct(id, Tunnus, raw_dep) %>% 
  group_by(id, Tunnus) %>% mutate(mean_dep = median(raw_dep, na.rm=TRUE)) %>%
  distinct(id, Tunnus, mean_dep) %>% 
  ggplot(.) + aes(mean_dep) + geom_histogram(binwidth=3) +
  # geom_bar(stat="identity", width=0.5) + 
  xlab("depth below surface [m]") + ylab("piezometers")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=12), 
        panel.background=element_blank(),
        panel.border=element_rect(colour="black", fill=NA)) 


# manual annual fluctuations ts, classes ----
  
# g %>% select(Station, Tunnus, N, E,Nn, id, year, mean_mon, elevation, country) %>%
#     group_by(id) %>% #filter(id==48) %>%
#     # mutate(stdev  = sd(mean_mon, na.rm=TRUE),
#     #        gmean = psych::geometric.mean(mean_mon+100)-100,
#     #        mean = mean(mean_mon),
#     #        max = max(mean_mon), 
#     #        min = min(mean_mon)) %>%
#     # distinct(id, stdev, gmean, mean, max, min, country) %>% 
#     ggplot(.) +
#     geom_boxplot(aes(x=as.factor(id), y = mean_mon, outlier.shape=NA,
#                      # lower = mean - stdev, middle = gmean,
#                      # upper = mean + stdev, ymax = max, ymin = min, 
#                      group = id)) 
#   
#     facet_wrap(~id, nrow=10) +
#     theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
#           axis.ticks.x = element_blank(),
#           axis.text.x = element_blank(),
#           strip.background = element_blank(),
#           strip.text.x = element_blank()
#     )
    


g8 <- g8 %>% mutate(class= ifelse(id==1 | id==3 | id==4 | 
                                       id==7| id==41| id==53| 
                                       id==65 | id==69 | id==72, 2, 1),
                                     class= ifelse(id==14 | id==52 | id==54 | id ==58, 3, class),
                       class= ifelse(id==24 | id==30, 4, class))

gstat <- full_join(g1 %>% select(Station, N, E,Nn, id, ymd, mean_mon, g_geom, country) %>% mutate(period = 2010),
                   g8 %>% select(Station, N, E,Nn, id, ymd, mean_mon,g_geom, country) %>% mutate(period = 1980))
# write_csv(gstat %>%distinct(id, country, period,class), "R_finland_drought/output/process/classifications.csv") # input class column
class <- read_csv("R_finland_drought/output/process/classifications.csv")
gstat <- full_join(gstat, class) #no class column in gstat here


gstat %>%
  group_by(id, period) %>%
  mutate(stdev  = sd(mean_mon, na.rm=TRUE),
         gmean = psych::geometric.mean(mean_mon+100)-100,
         mean = mean(mean_mon),
         max = max(mean_mon),
         min = min(mean_mon)) %>%
  ggplot(.) + 
  # scale_y_reverse() +
  # geom_ribbon(aes(Nn, ymax = max, ymin = min,
  #                 fill = as.factor(class)), alpha = .1)+
  # geom_ribbon(aes(Nn, ymax = mean + stdev, ymin = mean - stdev,
  #               fill = as.factor(class)), alpha = .1)+
  # geom_line(aes(Nn, gmean,
  #                colour = as.factor(class)), alpha=.8) + facet_wrap(~period, nrow=2) +
  geom_point(aes(gmean, N,  group = id, colour=as.factor(period))) +
  geom_smooth(aes(gmean, N, group = id, colour = as.factor(period)), method = lm) +
  theme_classic() 

gstat %>% group_by(id, period) %>% mutate(g_geom = g_geom - mean(mean_mon)) %>%
  ggplot() +
  geom_hline(yintercept=0, alpha=.2,
             linetype = 2) +
  scale_y_reverse() + 
  # geom_line(
  #   data = gstat %>% filter(class==1),
  #           aes(ymd, g_geom, group = as.factor(id),
  #               colour = as.factor(class)),
  #           alpha =.3, size=1) +
  geom_line(
    # data = gstat %>% filter(class!=1),
    aes(ymd, g_geom, group = as.factor(id),
                colour = N), #as.factor(class)),
            alpha =.4, size=1) +
  # geom_label_repel(data = . %>% mutate(point=first(g_geom)) %>% 
  # distinct(point, id, period, class), 
  #                  aes(as.Date("1981-01-01"), point, label = id)) +
  geom_vline(xintercept = as.Date("1981-04-01"),
             colour = "red", alpha=.2,
             linetype = 2) +
  # facet_grid(vars(period),vars(class)) +
  facet_wrap(~period, ncol = 2) +
  theme_classic() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()#,
        # strip.background = element_blank(),
        # strip.text.x = element_blank()
  ) +
  scale_colour_gradient2(low="red", mid = "yellow", high="blue",
                         midpoint = 6850000,#55,
                         limits=c(6000000,7700000) #20,82 )
  )
  scale_colour_manual("",
                      values=c("1" = "blue",
                               "2" = "lightblue",
                               "3" = "brown",
                               "4" = "purple",
                               "5" = "red")) +
  scale_fill_manual("",
                    values=c("1" = "blue",
                             "2" = "lightblue",
                             "3" = "brown",
                             "4" = "purple",
                             "5" = "red"))

    
# removing trends 2019-02-22 -----
# blog: https://datascienceplus.com/time-series-analysis-in-r-part-2-time-series-transformations/
station = 1#72
test <- g.ds %>% filter(id==station & year <= 1984 & year >=1980) %>%
  as.data.frame() %>% select(id, Tunnus, date, mean_dep, week2) %>% 
  filter(!is.na(mean_dep))
plot.ts(test$mean_dep)

test.diff <- diff(test$mean_dep, lag =1)
plot.ts(test.diff)

test.means <- test %>% 
  group_by(week2) %>% 
  mutate(mean = mean(mean_dep, na.rm=TRUE),
         year = year(date)) %>% filter(year == 1982)
# 2009 is the year with the most observations, 25, according to:
# test %>% mutate(year=year(date)) %>% group_by(year) %>% summarise(rows=length(year))
plot.ts(cbind(test$mean_dep,test.diff))
plot.ts(cbind(test.means$mean))

test.diff <- test
diff <- diff(test$mean_dep, lag=1)
test.diff[(1+
  length(test.diff$date) - length(diff)):length(test.diff$date), "diff"] <- diff
test.diff <- test.diff %>% mutate(year = year(date)) %>% 
  group_by(week2) %>% 
  mutate(mean = mean(diff, na.rm=TRUE)) %>% filter(year==1982) %>% 
  mutate(ymd = as.Date(paste(ifelse(month(date)<=10, "1982", "1981"), 
                             month(date), ifelse(day(date) != 29, day(date), 28),
                             sep="-")))
plot.ts(test.diff$mean*-1)
ggplot(data = test.diff) + aes(ymd, mean) + geom_line() + scale_y_reverse() +
  theme_classic()

test.dlog <- ts(diff(log(test$mean_dep))) #is producing a lot of NaNs
plot.ts(cbind(test$mean_dep,test.dlog))

# DURATION TIMING ----
# to identify recharge and dischagre dominated periods, check the difference of 
# the mean between one sample period OR one month to the next 
# (mean depth(day) - mean depth(day - 1 day)), if the value is
# negative, there's an increase and thus a recharge period, and vice versa.
# Thereafter count weeks of recharge (duration), when it started, when it stops,
# how much the difference is between the first increase and the last peak value 
# before decrease, check for different time intervals, i.e. weeks, one month, two
# etc. 
# dur gw 2006-2010 ----
dur <- g1 %>% group_by(id) %>% dplyr::select(id, g_geom, ymd) %>%
  mutate(week2 = ceiling(week(ymd)/2))
dur1 <- dur %>% arrange(id, ymd) %>% group_by(id) %>% 
  mutate(stdev=sd(g_geom),
         re_dis_diff =  as.numeric(g_geom - lag(g_geom, 1)),
         re_dis_type = ifelse(re_dis_diff>0, "DWT", 
                              "RWT"),
         re_dis_type = ifelse(re_dis_diff > stdev & #stdev/8 &
                                 re_dis_diff < -stdev, #-stdev/8, 
                               "no change", re_dis_type))
dur1 <- dur1 %>% arrange(id, ymd) %>% group_by(id) %>%
  mutate(re_dis_type = ifelse(re_dis_diff > stdev | 
                                 re_dis_diff < -stdev, re_dis_type,
    ifelse(lead(re_dis_type, 1) == re_dis_type | 
             lag(re_dis_type, 1) == re_dis_type,
                               re_dis_type, "no change")),
         re_dis_type = ifelse(re_dis_type == "no change", NA, re_dis_type),
         re_dis_type = zoo::na.locf(re_dis_type, fromLast = FALSE, na.rm = FALSE),
         re_dis_change = ifelse((is.na(re_dis_type) |
                                   lead(re_dis_type,1) != re_dis_type) &
                                   lead(re_dis_type,1) == "RWT",
                                 "rwt-start", NA),
         re_dis_change = ifelse((is.na(re_dis_type) |
                                    lead(re_dis_type,1) != re_dis_type)  &
                                   lead(re_dis_type,1) == "DWT",
                                 "dwt-start", re_dis_change))
dur1 %>% group_by(id, re_dis_change) %>% 
  mutate(count = length(!is.na(re_dis_change)),
         count = ifelse(tail(!is.na(re_dis_change),1) ==
                                   head(!is.na(re_dis_change), 1),
                            count - 1, count)) #%>% rename(RWT=PWB, DWT=NWB)

dur1_2 <- dur1 %>% group_by(id) %>%
  complete(ymd = seq.Date(min(ymd), max(ymd), by="day")) %>%
  group_by(id) %>%
  mutate(re_dis_type = zoo::na.locf(re_dis_type, fromLast = TRUE,
                                     na.rm = FALSE)) #%>% rename(RWT=PWB, DWT=NWB)


# dur met + gw 2006-2010 ----
dur_met1 <- tibm.pet1 %>% 
  ungroup() %>%
  group_by(id) %>% mutate(q_pet = (Q_m) - (pet_m)) %>% 
  ungroup() %>%
  distinct(id, period, E, N, ymd, week2, M_m,Q_m, pet_m, q_pet, OSQ_m)
dur_met1 <- dur_met1 %>% arrange(id, ymd) %>% group_by(id) %>% 
  mutate(qpet_type = ifelse(q_pet < 0, "high PET", NA), 
         qpet_type = ifelse(q_pet > 0, "rain", qpet_type), 
         qpet_type = ifelse(q_pet == 0, "snowfall", qpet_type), 
         # qpet_type = ifelse(M_m > 0.5, "snowmelt", qpet_type))
         qpet_type = ifelse(OSQ_m > 0.5, "snowmelt", qpet_type)) #,
         # qpet_type = ifelse(OSQ_m < q_pet & OSQ_m > 0, "rain-on-snow", qpet_type))

dur_met1 <- left_join(dur_met1 %>% dplyr::select(id, ymd, E, N, pet_m, Q_m, qpet_type, OSQ_m),
               dur1 %>% dplyr::select(id, ymd, re_dis_change, re_dis_type)) %>% 
  mutate(month = month(ymd),
         season = ifelse(month == 9 | month == 11 | month == 10, "Sep-Nov", NA),
         season = ifelse(month == 1 | month == 2 | 
                           month == 12, "Dec-Feb", season),
         season = ifelse(month == 4 | month == 5 | 
                           month == 3, "Mar-May", season),
         season = ifelse(month == 7 | month == 8 | 
                           month == 6, "Jun-Aug", season)) %>% dplyr::select(-month)
  
dur_met1 <- dur_met1 %>% group_by(id) %>%
  mutate(#re_dis_qpet1 = zoo::na.locf(re_dis_qpet1, fromLast = FALSE,
          #                           na.rm = FALSE),
         re_dis_type = zoo::na.locf(re_dis_type, fromLast = TRUE,
                                     na.rm = FALSE),
         season = zoo::na.locf(season, fromLast = TRUE,
                                     na.rm = FALSE))

dur_type1 <- dur_met1 %>% group_by(id, season, re_dis_type, qpet_type) %>%
  mutate(re_dis_nr = n()) %>% ungroup() %>% 
  # group_by(id, season, re_dis_qpet1) %>% mutate(qpet_nr = n()) %>% ungroup() %>%  
  distinct(id, E, N, re_dis_type, re_dis_nr, season,qpet_type) %>% 
  arrange(id, season) %>% group_by(id, season) %>% 
  spread(key=re_dis_type, value=re_dis_nr) %>%
  rename(rech = RWT, disch = DWT,
         na = '<NA>') %>% 
  mutate_at(c(6:8), ~replace(., is.na(.), 0))

days <- dur_met1 %>% group_by(id, season, re_dis_type) %>%
  mutate(re_dis_nr = n()) %>% ungroup() %>% 
  # group_by(id, season, re_dis_qpet1) %>% mutate(qpet_nr = n()) %>% ungroup() %>%  
  distinct(id, E, N, re_dis_type, re_dis_nr, season) %>% 
  arrange(id, season) %>% group_by(id, season) %>% 
  spread(key=re_dis_type, value=re_dis_nr) %>%
  rename(rech = RWT, disch = DWT,
         na = '<NA>') %>% 
  mutate_at(c(5:7), ~replace(., is.na(.), 0)) %>% 
  mutate(days_tot = rech+disch+ifelse(!is.na(na), na, 0),
         main_gw = ifelse(rech > days_tot/2, "re", "dis")) %>% 
  distinct(id, E, N, season, days_tot, main_gw)
dur_type1 <- left_join(dur_type1, days) %>% group_by(id, season, qpet_type) %>% 
  mutate(re = round(rech/days_tot, 2),
         dis = round(disch/days_tot, 2)) %>% 
  dplyr::select(-na, -disch, -rech) %>% arrange(id, season)

dur_type1 <- dur_type1 %>% group_by(id, season) %>% #check this out
#   mutate(frac_ch = re+dis,
#          type_gw = ifelse(frac_ch > 1/3 & re > frac_ch/2, "re", "dis"),
#          type_gw = ifelse(frac_ch < 1/3 & re > frac_ch/2, "re <1/3", type_gw),
#          type_gw = ifelse(frac_ch < 1/3 & dis > frac_ch/2, "dis <1/3", type_gw),
#          type = ifelse(frac_ch > 1/3, qpet_type1, "<1/3")) %>% 
#   filter(type != "<1/3") %>% 
  mutate_all(., ~replace(., .<0.001, NA)) #%>% rename(RWT=PWB, DWT=NWB)




# dur gw 1980-1984 ----
dur <- g8 %>% group_by(id) %>% dplyr::select(id, g_geom, ymd) %>%
  mutate(week2 = ceiling(week(ymd)/2))
dur8 <- dur %>% arrange(id, ymd) %>% group_by(id) %>% 
  mutate(stdev=sd(g_geom),
         re_dis_diff =  as.numeric(g_geom - lag(g_geom, 1)),
         re_dis_type = ifelse(re_dis_diff>0, "DWT", 
                               "RWT"),
         re_dis_type = ifelse(re_dis_diff > stdev & #stdev/8 &
                                 re_dis_diff < -stdev, #-stdev/8, 
                               "no change", re_dis_type))
dur8 <- dur8 %>% arrange(id, ymd) %>% group_by(id) %>%
  mutate(re_dis_type = ifelse(re_dis_diff > stdev | 
                                 re_dis_diff < -stdev, re_dis_type,
                               ifelse(lead(re_dis_type, 1) == re_dis_type | 
                                        lag(re_dis_type, 1) == re_dis_type,
                                      re_dis_type, "no change")),
         re_dis_type = ifelse(re_dis_type == "no change", NA, re_dis_type),
         re_dis_type = zoo::na.locf(re_dis_type, fromLast = FALSE, na.rm = FALSE),
         re_dis_change = ifelse((is.na(re_dis_type) |
                                    lead(re_dis_type,1) != re_dis_type) &
                                   lead(re_dis_type,1) == "RWT",
                                 "rwt-start", NA),
         re_dis_change = ifelse((is.na(re_dis_type) |
                                    lead(re_dis_type,1) != re_dis_type)  &
                                   lead(re_dis_type,1) == "DWT",
                                 "dwt-start", re_dis_change))
dur8 %>% group_by(id, re_dis_change) %>% 
  mutate(count = length(!is.na(re_dis_change)),
         count = ifelse(tail(!is.na(re_dis_change),1) ==
                          head(!is.na(re_dis_change), 1),
                        count - 1, count)) #%>% rename(RWT=RWT, DWT=NWB)

dur8_2 <- dur8 %>% group_by(id) %>%
  complete(ymd = seq.Date(min(ymd), max(ymd), by="day")) %>%
  group_by(id) %>%
  mutate(re_dis_type = zoo::na.locf(re_dis_type, fromLast = TRUE,
                                     na.rm = FALSE))# %>% rename(RWT=RWT, DWT=DWT)


# dur met + gw 1980-1984 ----
dur_met8 <- tibm.pet8 %>% 
  ungroup() %>%
  group_by(id) %>% mutate(q_pet = Q_m - pet_m) %>% 
  ungroup() %>%
  distinct(id, period, E, N, ymd, week2, M_m,Q_m, pet_m, q_pet, OSQ_m)
dur_met8 <- dur_met8 %>% arrange(id, ymd) %>% group_by(id) %>% 
  mutate(qpet_type = ifelse(q_pet < 0, "high PET", NA), 
         qpet_type = ifelse(q_pet > 0, "rain", qpet_type), 
         qpet_type = ifelse(q_pet == 0, "snowfall", qpet_type), 
         # qpet_type = ifelse(M_m > 0.5, "snowmelt", qpet_type))
         qpet_type = ifelse(OSQ_m > 0.5, "snowmelt", qpet_type)) #,
         # qpet_type = ifelse(OSQ_m < q_pet & OSQ_m > 0, "rain-on-snow", qpet_type))

dur_met8 <- left_join(dur_met8 %>% dplyr::select(id, ymd, E, N, pet_m, Q_m, qpet_type, OSQ_m),
               dur8 %>% dplyr::select(id, ymd, re_dis_change, re_dis_type)) %>% 
  mutate(month = month(ymd),
         season = ifelse(month == 9 | month == 11 | month == 10, "Sep-Nov", NA),
         season = ifelse(month == 1 | month == 2 | 
                           month == 12, "Dec-Feb", season),
         season = ifelse(month == 4 | month == 5 | 
                           month == 3, "Mar-May", season),
         season = ifelse(month == 7 | month == 8 | 
                           month == 6, "Jun-Aug", season)) %>% dplyr::select(-month)

dur_met8 <- dur_met8 %>% group_by(id) %>%
  mutate(#re_dis_qpet1 = zoo::na.locf(re_dis_qpet1, fromLast = FALSE,
    #                           na.rm = FALSE),
    re_dis_type = zoo::na.locf(re_dis_type, fromLast = TRUE,
                                na.rm = FALSE),
    season = zoo::na.locf(season, fromLast = TRUE,
                          na.rm = FALSE))

b <- dur_met8 %>% group_by(id, season, re_dis_type, qpet_type) %>%
  mutate(re_dis_nr = n()) %>% ungroup() %>% 
  # group_by(id, season, re_dis_qpet1) %>% mutate(qpet_nr = n()) %>% ungroup() %>%  
  distinct(id, E, N, re_dis_type, re_dis_nr, season,qpet_type) %>% 
  arrange(id, season) %>% group_by(id, season) %>% 
  spread(key=re_dis_type, value=re_dis_nr) %>%
  rename(rech = RWT, disch = DWT,
         na = '<NA>') %>% 
  mutate_at(c(6:8), ~replace(., is.na(.), 0))

days <- dur_met8 %>% group_by(id, season, re_dis_type) %>%
  mutate(re_dis_nr = n()) %>% ungroup() %>% 
  # group_by(id, season, re_dis_qpet1) %>% mutate(qpet_nr = n()) %>% ungroup() %>%  
  distinct(id, E, N, re_dis_type, re_dis_nr, season) %>% 
  arrange(id, season) %>% group_by(id, season) %>% 
  spread(key=re_dis_type, value=re_dis_nr) %>%
  rename(rech = RWT, disch = DWT,
         na = '<NA>') %>% 
  mutate_at(c(5:7), ~replace(., is.na(.), 0)) %>% 
  mutate(days_tot = rech+disch+ifelse(!is.na(na), na, 0),
         main_gw = ifelse(rech > days_tot/2, "re", "dis")) %>% 
  distinct(id, E, N, season, days_tot, main_gw)
dur_type8 <- left_join(b, days) %>% group_by(id, season, qpet_type) %>% 
  mutate(re = round(rech/days_tot, 2),
         dis = round(disch/days_tot, 2)) %>% 
  dplyr::select(-na, -disch, -rech) %>% arrange(id, season)

dur_type8 <- dur_type8 %>% group_by(id, season) %>% #check this out
#   mutate(frac_ch = re+dis,
#          type_gw = ifelse(frac_ch > 1/3 & re > frac_ch/2, "re", "dis"),
#          type_gw = ifelse(frac_ch < 1/3 & re > frac_ch/2, "re <1/3", type_gw),
#          type_gw = ifelse(frac_ch < 1/3 & dis > frac_ch/2, "dis <1/3", type_gw),
#          type = ifelse(frac_ch > 1/3, qpet_type, "<1/3")) %>% 
#   filter(type != "<1/3") %>% 
  mutate_all(., ~replace(., .<0.001, NA)) #%>% rename(RWT=RWT, DWT=NWB)

# dur_type <- full_join(dur_type8 %>% mutate(period="P1"), dur_type1 %>% mutate(period="P2"))
# coordinates(dur_type) = ~E+N
# proj4string(dur_type) = CRS("+proj=longlat + datum=WGS84")
# raster::shapefile(dur_type, "dur_type2.shp")

# dur plot  ----
library(sp)
library(cowplot)
library(ggmap)
# a <- 
ggmap(stamen) +
  geom_point(data = full_join(dur_type8 %>% mutate(period="P1"), dur_type1 %>% mutate(period="P2")),
             aes(x = E, y = N, group = id, colour = qpet_type, alpha=dis, 
                 size = dis),
             position=position_jitter(h=0.2,w=0.2)) + #scale_size(range = c(.1,3)) +
  scale_colour_viridis_d() +
  facet_wrap(~season+period, nrow=2) + ggtitle("DWT") + xlab("")
ggmap(stamen) +
  geom_point(data = dur_type8,
             aes(x = E, y = N, group = id, colour = qpet_type, alpha=re, 
                 size = re),
             position=position_jitter(h=0.2,w=0.2)) + #scale_size(range = c(.1,3)) +
  scale_colour_viridis_d() +
  facet_wrap(~season, nrow=2) + ggtitle("RWT") + xlab("")

b <- ggmap(stamen) +
  geom_point(data = full_join(dur_type8 %>% mutate(period="P1"), dur_type1 %>% mutate(period="P2")),
             aes(x = E, y = N, group = id, colour = qpet_type, 
                 size =dis, alpha=dis),
             position=position_jitter(h=0.2,w=0.2)) +  #scale_size(range = c(.1,3), "days") +
  scale_alpha("days") +
  scale_colour_viridis_d("") +
  facet_wrap(~season+period, nrow=2) + ggtitle("DWT") 
prow1 <- plot_grid(a + theme(legend.position = "none"),
                   b + theme(legend.position = "none"), 
                   # labels = "2006-2010",
                   nrow=2,
                   align = "hv", axis = "lb", ncol = 1,
                   rel_heights = c(1,1))
legend1 <- get_legend(b)
plot_grid(prow1, legend1, rel_widths = c(6, 1.5), ncol = 2)


library(stringr)
library(scales)
library(viridis)
library(colormap)
scales::show_col(colormap(colormap=colormaps$inferno), labels=T)
scales::show_col(colormap(), labels=T)
dur1 <- left_join(dur1, dur_met1 %>% distinct(id, ymd, N, E, qpet_type))
dur_met8 <- dur_met8 %>% mutate(re_dis_type = ifelse(re_dis_type=='RWT', 
                                             'RPL', re_dis_type),
                        re_dis_type = ifelse(re_dis_type=='DWT', 
                                             'DPL', re_dis_type))
dur_met1 <- dur_met1 %>% mutate(re_dis_type = ifelse(re_dis_type=='RWT', 
                                             'RPL', re_dis_type),
                        re_dis_type = ifelse(re_dis_type=='DWT', 
                                             'DPL', re_dis_type))
dur1 %>%
  filter(id== "f_1162" | id == "s_157" |
           id == "f_3453" | id == "s_98" |
           id == "f_131" | id == "s_46" |
           id=="s_93" | id == "f_2465") %>%
  mutate(period="2001-2010") %>%
  transform(id=factor(id, levels=c("f_3453", "s_93", "f_2465", "f_1162",
                                   "s_46", "f_131", "s_98", "s_157")))%>%
  mutate(id = as.character(id),id = replace(id, id == "f_3453", 1),
         id = replace(id, id == "s_93", 2),
         id = replace(id, id == "f_2465", 3),
         id = replace(id, id == "f_1162", 4),
         id = replace(id, id == "s_46", 5),
         id = replace(id, id == "f_131", 6),
         id = replace(id, id == "s_98", 7),
         id = replace(id, id == "s_157", 8)) %>%
  ggplot(.) +
  geom_segment(data = dur_met1 %>%
                 filter(id== "f_1162" | id == "s_157" |
                          id == "f_3453" | id == "s_98" |
                          id == "f_131" | id == "s_46" |
                          id=="s_93" | id == "f_2465")%>%
                 transform(id=factor(id, levels=c("f_3453", "s_93", "f_2465", "f_1162",
                                                  "s_46", "f_131", "s_98", "s_157")))%>%
                 mutate(id = as.character(id),id = replace(id, id == "f_3453", 1),
                        id = replace(id, id == "s_93", 2),
                        id = replace(id, id == "f_2465", 3),
                        id = replace(id, id == "f_1162", 4),
                        id = replace(id, id == "s_46", 5),
                        id = replace(id, id == "f_131", 6),
                        id = replace(id, id == "s_98", 7),
                        id = replace(id, id == "s_157", 8)),
               aes(x = ymd,y=1.3, xend=ymd, yend=1.49, colour = qpet_type), 
               size=1, alpha=1) +
  geom_segment(data = dur_met8 %>%
                 filter(id== "f_1162" | id == "s_157" |
                          id == "f_3453" | id == "s_98" |
                          id == "f_131" | id == "s_46" |
                          id=="s_93" | id == "f_2465")%>%
                 transform(id=factor(id, levels=c("f_3453", "s_93", "f_2465", "f_1162",
                                                  "s_46", "f_131", "s_98", "s_157")))%>%
                 mutate(id = as.character(id),id = replace(id, id == "f_3453", 1),
                        id = replace(id, id == "s_93", 2),
                        id = replace(id, id == "f_2465", 3),
                        id = replace(id, id == "f_1162", 4),
                        id = replace(id, id == "s_46", 5),
                        id = replace(id, id == "f_131", 6),
                        id = replace(id, id == "s_98", 7),
                        id = replace(id, id == "s_157", 8)),
               aes(x = ymd,y=1.1, xend=ymd, yend=1.3, colour = qpet_type),
               size=1, alpha=1) +
  geom_segment(data = dur_met1 %>%
                 filter(id== "f_1162" | id == "s_157" |
                          id == "f_3453" | id == "s_98" |
                          id == "f_131" | id == "s_46" |
                          id=="s_93" | id == "f_2465")%>%
                 transform(id=factor(id, levels=c("f_3453", "s_93", "f_2465", "f_1162",
                                                  "s_46", "f_131", "s_98", "s_157")))%>%
                 mutate(id = as.character(id),id = replace(id, id == "f_3453", 1),
                        id = replace(id, id == "s_93", 2),
                        id = replace(id, id == "f_2465", 3),
                        id = replace(id, id == "f_1162", 4),
                        id = replace(id, id == "s_46", 5),
                        id = replace(id, id == "f_131", 6),
                        id = replace(id, id == "s_98", 7),
                        id = replace(id, id == "s_157", 8)),
               aes(x = ymd,y=-1.1, xend=ymd, yend=-1.3, colour = re_dis_type), 
               size=1, alpha=1) +
  geom_segment(data = dur_met8 %>%
                 filter(id== "f_1162" | id == "s_157" |
                          id == "f_3453" | id == "s_98" |
                          id == "f_131" | id == "s_46" |
                          id=="s_93" | id == "f_2465")%>%
                 transform(id=factor(id, levels=c("f_3453", "s_93", "f_2465", "f_1162",
                                                  "s_46", "f_131", "s_98", "s_157")))%>%
                 mutate(id = as.character(id),id = replace(id, id == "f_3453", 1),
                        id = replace(id, id == "s_93", 2),
                        id = replace(id, id == "f_2465", 3),
                        id = replace(id, id == "f_1162", 4),
                        id = replace(id, id == "s_46", 5),
                        id = replace(id, id == "f_131", 6),
                        id = replace(id, id == "s_98", 7),
                        id = replace(id, id == "s_157", 8)),
               aes(x = ymd,y=-1.3, xend=ymd, yend=-1.49, colour = re_dis_type),
               size=1, alpha=1) +
  geom_line(data=dur8 %>% 
              filter(id== "f_1162" | id == "s_157" |
                       id == "f_3453" | id == "s_98" |
                       id == "f_131" | id == "s_46" |
                       id=="s_93" | id == "f_2465") %>%
              transform(id=factor(id, levels=c("f_3453", "s_93", "f_2465", "f_1162",
                                               "s_46", "f_131", "s_98", "s_157")))%>%
              mutate(id = as.character(id),id = replace(id, id == "f_3453", 1),
                     id = replace(id, id == "s_93", 2),
                     id = replace(id, id == "f_2465", 3),
                     id = replace(id, id == "f_1162", 4),
                     id = replace(id, id == "s_46", 5),
                     id = replace(id, id == "f_131", 6),
                     id = replace(id, id == "s_98", 7),
                     id = replace(id, id == "s_157", 8)) %>%
              mutate(period="1980-1989"), aes(ymd, g_geom, linetype=period), size =.6)+
  geom_line(aes(ymd, g_geom, linetype=period), size=0.6) +
  scale_color_manual(values=c("high PET" = "#f6850fff", #FC6404BF", #BB3754FF",
                                "rain" = "#3c4f8aff", #39568CFF",
                                "snowfall" = "#29ae80ff",
                                "snowmelt" = "#440154FF", 
                              "RPL" = "#fde725ff",
                              "DPL" = "#440154ff"),
                     breaks=c("RPL", "DPL", "high PET", "snowfall", "rain",
                                       "snowmelt")) +
  geom_hline(aes(yintercept=1.3))+geom_hline(aes(yintercept=-1.3))+
  facet_wrap(~id, nrow=2) + 
  scale_x_date(date_labels = "%m", date_breaks = "3 months", name="December-November") +
  scale_y_reverse(breaks=c(0.5,0,-0.5), limits=c(1.5,-1.5), 
                     name="normalised biweekly median piezometric level [m]") +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        # axis.ticks.x = element_blank(),
        # axis.text.x = element_blank(),
        # panel.grid.minor=element_blank(),
        axis.line.x = element_blank(), axis.text =element_text(size=12),
        panel.grid.major=element_blank(), panel.background = element_rect(fill="white",
                                                                        colour="white"),
        legend.title = element_blank(), axis.line.y = element_line(colour="black")) + guides(size=5)




# Metrics prep ----
# blog: http://www.statsoft.com/Textbook/Time-Series-Analysis#analysis
# data from 'removing trends'
head(g8)
head(g1)
head(g)
diff <- g %>% filter(!is.na(raw_dep)) %>% 
  distinct(date, Tunnus, year, id, elevation, raw_dep) %>% group_by(id, Tunnus) %>% 
  mutate(#raw_dep = elevation - raw_dep,  # get masl of water table, gets different results as
    #the magnitude difference is smaller, e.g. masl 117.6 and mean 117.5 vs depth 0.4 mean 0.5
         stdev_rec = sd(raw_dep, na.rm =TRUE),
         mean_rec = median(raw_dep, na.rm =TRUE)) %>% 
  mutate(month = month(date),
    season = ifelse(month == 9 | month == 11 | month == 10, "Sep-Nov", NA),
    season = ifelse(month == 1 | month == 2 | 
                      month == 12, "Dec-Feb", season),
    season = ifelse(month == 4 | month == 5 | 
                      month == 3, "Mar-May", season),
    season = ifelse(month == 7 | month == 8 | 
                      month == 6, "Jun-Aug", season),
    mean_all = round(median(raw_dep, na.rm = TRUE),1)) 

diff8 <- diff %>% filter(year <= 1989 & year >= 1980) %>% 
  group_by(id,Tunnus, season) %>% 
  mutate(stdev_8 = sd(raw_dep, na.rm = TRUE),
         mean_8 = median(raw_dep, na.rm = TRUE),
         min_8 = min(raw_dep, na.rm = TRUE),
         max_8 = max(raw_dep, na.rm = TRUE)) %>%
  distinct(id, Tunnus, 
           stdev_8, mean_8, season, 
           min_8, max_8, mean_all, mean_rec)

diff1 <- diff %>% filter(year <= 2010 & year >= 2001) %>% 
  group_by(id, Tunnus, season) %>% #before Tunnus and season and year
  mutate(stdev_1 = sd(raw_dep, na.rm = TRUE),
         mean_1 = median(raw_dep, na.rm = TRUE),
         min_1 = min(raw_dep, na.rm = TRUE),
         max_1 = max(raw_dep, na.rm = TRUE)) %>%
  distinct(id, Tunnus, 
           stdev_1, mean_1, season, 
           min_1, max_1,  mean_all, mean_rec)

diff <- full_join(diff %>% distinct(id, Tunnus, 
                                    stdev_rec, mean_rec, season, mean_all), 
                  diff1) %>% filter(!is.na(stdev_1) & !is.na(stdev_rec))
diff <- full_join(diff, diff8) %>% filter(!is.na(stdev_8))
diff <- full_join(diff, plot %>% # from other section, make plot
                    rename(lat = N,lon = E) %>% group_by(id) %>% mutate(lon = mean(lon)) %>% 
                    distinct(lon, lat,Tunnus, id)) #lon lat versions of N E, from "#make shapefiles/maps ----"
diff <- diff %>% 
  mutate(group = ifelse(lat < 59, "-59?", NA),
         group = ifelse(lat >= 59, "59?-63?", group),
         group = ifelse(lat > 63, "63?-", group))


# Metrics plots ----
s <- ggplot(diff) + 
  aes((stdev_8/mean_rec), (stdev_1/mean_rec), group = Tunnus) +
        geom_point(aes(fill=as.factor(group), shape = season),shape =21, colour ="grey19",
                   alpha = .8, size = 2) +
        geom_abline(intercept = 0, slope = 1) +
  scale_fill_viridis_d(guides(name="Latitude"), direction=-1) +
  # scale_fill_manual(values=c("-59˚"= "#fde725ff",
  #                              "59˚-63˚" = "#8dd544ff",
  #                              "63˚-" = "#218f8dff"), guides(name="Latitude")) +
  geom_abline(intercept = 0, slope = 1) + 
  # scale_x_continuous(trans='log10', limits = c(0.005, 20)) +
  # scale_y_continuous(trans='log10', limits = c(0.005, 20)) +
  scale_x_continuous(trans='log10', limits = c(0.0001, 10)) +
  scale_y_continuous(trans='log10', limits = c(0.0001, 10)) +
  theme(axis.text =element_text(size=12)) +
  facet_wrap(~season) + xlab("sd. P1")+
  ylab("sd. P2") 
m <- ggplot(diff %>% group_by(id, Tunnus)) + 
  aes((mean_8/mean_rec), (mean_1/mean_rec), group = Tunnus) +
  geom_point(aes(fill=as.factor(group), shape = season),shape =21, colour ="grey19",
             alpha = .8, size = 2) +
  geom_abline(intercept = 0, slope = 1) + 
  # scale_x_continuous(trans='log10', limits = c(0.01,5))+
  # scale_y_continuous(trans='log10', limits = c(0.01,5))+
  scale_x_continuous(trans='log10', limits = c(0.8,1.2))+
  scale_y_continuous(trans='log10', limits = c(0.8,1.2))+
  # ylim(0,4) + xlim(0,4) +
  scale_fill_viridis_d(guides(name="Latitude"), direction=-1) +
  # scale_fill_manual(values=c("-59˚"= "#fde725ff",
  #                            "59˚-63˚" = "#8dd544ff",
  #                            "63˚-" = "#218f8dff"), guides(name="Latitude")) + 
  theme(axis.text =element_text(size=12)) + #theme_classic() + 
  facet_wrap(~season) + xlab("median P1")+
  ylab("median P2") 
min <- ggplot(diff%>% group_by(id, Tunnus)) + 
  aes((min_8/mean_rec), (min_1)/mean_rec, group = Tunnus) +
  geom_point(aes(fill=as.factor(group), shape = season), shape =21, colour ="grey19",
             alpha = .8, size = 2) +
  geom_abline(intercept = 0, slope = 1) +  
  # scale_x_continuous(trans='log10', limits = c(0.01,3))+
  # scale_y_continuous(trans='log10', limits = c(0.01,3))+
  scale_x_continuous(trans='log10', limits = c(0.25,1.15)) +
  scale_y_continuous(trans='log10', limits = c(0.25,1.15)) +
  scale_fill_viridis_d(guides(name="Latitude"), direction=-1) +
  # scale_fill_manual(values=c("-59˚"= "#fde725ff",
  #                            "59˚-63˚" = "#8dd544ff",
  #                            "63˚-" = "#218f8dff"), guides(name="Latitude")) + 
  theme(axis.text =element_text(size=12)) +# theme_classic() + 
  facet_wrap(~season) + xlab("min. P1")+
  ylab("min. P2")
max <- ggplot(diff%>% group_by(id, Tunnus)) + 
  aes((max_8)/mean_rec, (max_1)/mean_rec, group = group) +
  geom_point(aes(fill=as.factor(group) , shape = season), shape =21, colour ="grey19",
             alpha = .8, size = 2) +
  geom_abline(intercept = 0, slope = 1) + 
  # scale_x_continuous(trans='log10', limits = c(.1,20))+
  # scale_y_continuous(trans='log10', limits = c(.1,20))+
  scale_x_continuous(trans='log10', limits = c(.9,1.8))+
  scale_y_continuous(trans='log10', limits = c(.9,1.8))+ 
  scale_fill_viridis_d(guides(name="Latitude"), direction=-1) +
  # scale_fill_manual(values=c("-59˚"= "#fde725ff",
  #                            "59˚-63˚" = "#8dd544ff",
  #                            "63˚-" = "#218f8dff"), guides(name="Latitude")) + 
  # scale_fill_manual("Latitude",
  #                     values=c("-59˚" = "orange", "59˚-63˚" = "olivedrab3",
  #                              "63˚-"="darkgreen")) +
  theme(axis.text =element_text(size=12)) + #theme_classic() + 
  facet_wrap(~season) + xlab("max. P1")+
  ylab("max. P2") 


library(cowplot)
prow <- plot_grid(m + theme(legend.position = "none", 
                            panel.background = element_rect("white"),
                            axis.line = element_line("black")), 
          s + theme(legend.position = "none", 
                    panel.background = element_rect("white"),
                    axis.line = element_line("black")), 
          min  + theme(legend.position = "none", 
                       panel.background = element_rect("white"),
                       axis.line = element_line("black")), 
          max + theme(legend.position = "none", 
                      panel.background = element_rect("white"),
                      axis.line = element_line("black")), 
          align = "hv", axis = "lb", ncol = 2,
  rel_heights = c(1,1))
legend <- get_legend(m)
plot_grid(prow, legend, rel_widths = c(3, .5))



# Metrics stats ----
wilcox.test((diff %>% filter(season=="Dec-Feb"))$mean_8, (diff %>% filter(season=="Dec-Feb"))$mean_1)$p.value
# 0.027
wilcox.test((diff %>% filter(season=="Mar-May"))$mean_8, (diff %>% filter(season=="Mar-May"))$mean_1)$p.value
# 0.029
wilcox.test((diff %>% filter(season=="Jun-Aug"))$mean_8, (diff %>% filter(season=="Jun-Aug"))$mean_1)$p.value
# 0.009
wilcox.test((diff %>% filter(season=="Sep-Nov"))$mean_8, (diff %>% filter(season=="Sep-Nov"))$mean_1)$p.value
# 0.00097
mean(diff$mean_8) - mean(diff$mean_1)

wilcox.test((diff %>% filter(season=="Dec-Feb"))$stdev_8, (diff %>% filter(season=="Dec-Feb"))$stdev_1)$p.value
# 0.8
wilcox.test((diff %>% filter(season=="Mar-May"))$stdev_8, (diff %>% filter(season=="Mar-May"))$stdev_1)$p.value
# 0.94
wilcox.test((diff %>% filter(season=="Jun-Aug"))$stdev_8, (diff %>% filter(season=="Jun-Aug"))$stdev_1)$p.value
# 0.9
wilcox.test((diff %>% filter(season=="Sep-Nov"))$stdev_8, (diff %>% filter(season=="Sep-Nov"))$stdev_1)$p.value
# 0.97

wilcox.test((diff %>% filter(season=="Dec-Feb"))$min_8, (diff %>% filter(season=="Dec-Feb"))$min_1)$p.value
# 0.679
wilcox.test((diff %>% filter(season=="Mar-May"))$min_8, (diff %>% filter(season=="Mar-May"))$min_1)$p.value
# 0.59
wilcox.test((diff %>% filter(season=="Jun-Aug"))$min_8, (diff %>% filter(season=="Dec-Feb"))$min_1)$p.value
# 0.8
wilcox.test((diff %>% filter(season=="Sep-Nov"))$min_8, (diff %>% filter(season=="Sep-Nov"))$min_1)$p.value
# 0.3

wilcox.test((diff %>% filter(season=="Dec-Feb"))$max_8, (diff %>% filter(season=="Dec-Feb"))$max_1)$p.value

wilcox.test((diff %>% filter(season=="Mar-May" & group=="-59?"))$max_8, 
            (diff %>% filter(season=="Mar-May" & group=="-59?"))$max_1)#$p.value
wilcox.test((diff %>% filter(season=="Jun-Aug"))$max_8, 
            (diff %>% filter(season=="Jun-Aug"))$max_1, paired=TRUE)#$p.value
wilcox.test((diff %>% filter(season=="Sep-Nov"))$max_8, 
            (diff %>% filter(season=="Sep-Nov"))$max_1, paired=FALSE, alternative="l", conf.int=TRUE)#$p.value

diff
sign <- diff %>% distinct(id, Tunnus, season, mean_rec, stdev_1, 
                          stdev_8, mean_1, mean_8, min_1, min_8, max_1, max_8, group) %>% 
  group_by(id, Tunnus, season) %>%
  mutate(stdev_1 = stdev_1/mean_rec, 
         stdev_8 = stdev_8/mean_rec, 
         mean_1 = mean_1/mean_rec, 
         mean_8 = mean_8/mean_rec, 
         min_1 = min_1/mean_rec, 
         min_8 = min_8/mean_rec, 
         max_1 = max_1/mean_rec, 
         max_8 = max_8/mean_rec) %>% 
  group_by(season, group) %>%
  # mutate(sd = ((median(stdev_1, na.rm=TRUE) - median(stdev_8, na.rm=TRUE))/median(stdev_8, na.rm=TRUE)) %>% round(.,3),
  #        sd.p = wilcox.test(stdev_8, stdev_1, paired=TRUE)$p.value%>% round(.,3),
  #        median = ((median(mean_1, na.rm=TRUE) - median(mean_8, na.rm=TRUE))/median(mean_8, na.rm=TRUE)) %>% round(.,2),
  #        median.p = wilcox.test(mean_8, mean_1, paired=TRUE)$p.value%>% round(.,3),
  #        min = ((median(min_1, na.rm=TRUE) - median(min_8, na.rm=TRUE))/median(min_8, na.rm=TRUE)) %>% round(.,2),
  #        min.p = wilcox.test(min_8, min_1, paired=TRUE)$p.value%>% round(.,3),
  #        max = ((median(max_1, na.rm=TRUE) - median(max_8, na.rm=TRUE))/median(max_8, na.rm=TRUE)) %>% round(.,2),
  #        max.p = wilcox.test(max_8, max_1, paired=TRUE)$p.value%>% round(.,3)) %>%
  mutate(sd = median((stdev_1 -stdev_8)/stdev_8) %>% round(.,2),
       sd.p = wilcox.test(stdev_8, stdev_1, paired=TRUE)$p.value%>% round(.,3),
       median = median((mean_1 - mean_8)/mean_8) %>% round(.,2),
       median.p = wilcox.test(mean_8, mean_1, paired=TRUE)$p.value%>% round(.,3),
       min = median((min_1 - min_8)/min_8) %>% round(.,2),
       min.p = wilcox.test(min_8, min_1, paired=TRUE)$p.value%>% round(.,3),
       max = median((max_1 - max_8)/max_8) %>% round(.,2),
       max.p = wilcox.test(max_8, max_1, paired=TRUE)$p.value%>% round(.,3)) %>%
  distinct(season, sd, median, min, max,sd.p, median.p, min.p, max.p)

sign %>% #dplyr::select(season, sd, median, min, max) %>% 
  View()

#violin plots of mistake
diff %>% distinct(id, Tunnus, season, mean_rec, stdev_1, 
                  stdev_8, mean_1, mean_8, min_1, min_8, max_1, max_8, group) %>% 
  group_by(id, Tunnus, season) %>%
  mutate(stdev_1 = stdev_1/mean_rec, 
         stdev_8 = stdev_8/mean_rec, 
         mean_1 = mean_1/mean_rec, 
         mean_8 = mean_8/mean_rec, 
         min_1 = min_1/mean_rec, 
         min_8 = min_8/mean_rec, 
         max_1 = max_1/mean_rec, 
         max_8 = max_8/mean_rec, 
         sd = ((stdev_1 -stdev_8)/stdev_8) %>% round(.,3),
         median = ((mean_1 - mean_8)/mean_8) %>% round(.,3),
         min = ((min_1 - min_8)/min_8) %>% round(.,3),
         max = ((max_1 - max_8)/max_8) %>% round(.,3),
         group = ifelse(group=="-59?", "-59", group),
         group = ifelse(group=="63?-", "63-", group),
         group = ifelse(group=="59?-63?", "59-63", group)) %>%
  distinct(season, group, Tunnus, sd, median, min, max) %>% #group_by(season, group) %>% 
  # mutate(sd = median(sd),
  #        median = median(median),
  #        min = median(min),
  #        max = median(max)) %>%
  transform(season=factor(season, levels=c("Dec-Feb", "Mar-May", "Jun-Aug", "Sep-Nov"))) %>%
  ggplot(.) + 
  geom_violin(aes(season, max, fill=group), position="dodge",
              trim=TRUE) + ylim(-2,2) +
  geom_point(data = . %>% group_by(season, group) %>% 
               mutate(sd=median(sd),
                      median=median(median),
                      min=median(min),
                      max=median(max)) %>% distinct(season, group, sd, median, min, max), 
             aes(season, max), size=3, fill="blue", shape=24)+
  
  geom_point(data=diff %>% 
               distinct(id, Tunnus, season, mean_rec, stdev_1, stdev_8, mean_1, mean_8, min_1, min_8, max_1, max_8, group) %>% 
                group_by(id, Tunnus, season) %>%
                mutate(stdev_1 = stdev_1/mean_rec, 
                       stdev_8 = stdev_8/mean_rec, 
                       mean_1 = mean_1/mean_rec, 
                       mean_8 = mean_8/mean_rec, 
                       min_1 = min_1/mean_rec, 
                       min_8 = min_8/mean_rec, 
                       max_1 = max_1/mean_rec, 
                       max_8 = max_8/mean_rec,
                       group = ifelse(group=="-59?", "-59", group),
                       group = ifelse(group=="63?-", "63-", group),
                       group = ifelse(group=="59?-63?", "59-63", group)) %>% ungroup() %>% 
                group_by(season, group) %>%
                mutate(stdev_1 = median(stdev_1), 
                       stdev_8 = median(stdev_8), 
                       mean_1 = median(mean_1), 
                       mean_8 = median(mean_8), 
                       min_1 = median(min_1), 
                       min_8 = median(min_8), 
                       max_1 = median(max_1), 
                       max_8 = median(max_8)) %>% select(-Tunnus, -id, -mean_rec) %>%
                distinct() %>% 
                mutate(sd = ((stdev_1 -stdev_8)/stdev_8) %>% round(.,3),
                       median = ((mean_1 - mean_8)/mean_8) %>% round(.,3),
                       min = ((min_1 - min_8)/min_8) %>% round(.,3),
                       max = ((max_1 - max_8)/max_8) %>% round(.,3)) %>%
               distinct(season, group, sd, median, min, max) %>%
             transform(season=factor(season, levels=c("Dec-Feb", "Mar-May", "Jun-Aug", "Sep-Nov"))), 
             aes(season, max), size=3, shape=25, colour="black", fill="red") + 
  facet_wrap(~group, ncol=3) + theme_bw() +  
  scale_fill_viridis_d(guides(name="Latitude"), direction=-1) 


# linear regression ----
# groundwater levels lm ----
# info: http://r-statistics.co/Linear-Regression.html
trend.d <- diff %>% filter(year>=1980 & year <=2010)  %>%
  ungroup() %>% mutate(season=ifelse(month > 4 & month < 11, "summer", "winter")) %>%
    distinct(Tunnus, id, date, elevation, raw_dep, year, month, season) %>%
  mutate(raw = elevation-raw_dep) %>% 
  group_by(id, Tunnus, year) %>% # season or month
  mutate(median=median(raw, na.rm=TRUE)) %>%
         # medr = median(raw_dep, na.rm=TRUE),
         # min = min(raw, na.rm=TRUE),
         # max = max(raw, na.rm=TRUE)) %>%
  distinct(id, Tunnus, year, median)

# trend.d %>% left_join(., plot %>% distinct(id, N,E)) %>% 
#   mutate(group = ifelse(N > 60, "cold", "temperate")) %>% group_by(year, season, group) %>% 
#   mutate(level = median(median)) %>% distinct(year, season, level, group) %>% 
#   ggplot(.) + geom_line(aes(year,level, colour=season)) + 
#   geom_smooth(aes(year,level, colour=season), level=0, method="lm") + 
#   facet_grid(~group) + scale_y_reverse()

library(reshape2); library(broom); library(Kendall); library(trend)
trend <- trend.d %>%
  # group_by(id, Tunnus, year) %>%
  # summarise(median=median(median, na.rm=TRUE)) %>% 
         # max=max(max), min=min(min),
         # diff = (max - min)/max) %>%
    group_by(id, Tunnus) %>% 
  do(model= tidy(MannKendall(.$median)),
     slope = sens.slope(.$median),
     iqrange=IQR(.$median))

trend$p=0
trend$tau=0
trend$sen=0
trend$iqr=0
# trend$r.sq = 0
for(i in 1:length(trend$Tunnus)){
    trend$p[i] = trend$model[[i]]$p.value[1]
    trend$tau[i] = trend$model[[i]]$statistic[1]
    trend$sen[i] = trend$slope[[i]]$estimates[1]
    trend$iqr[i] = trend$iqrange[[i]]
    # trend$resid[i] = trend$sum[[i]]$sigma
    # trend$r.sq[i] = trend$sum[[i]]$r.squared
}

trend.c <- trend %>% mutate(sign=ifelse(p <= 0.05, "p<0.05", "not significant"),
                 # sign=ifelse(p < 0.01, "p<0.01", sign),
                 # sign=ifelse(p < 0.001, "p<0.001", sign),
                 CRI=(sen*31)/iqr,
                 a = ifelse(sen > 0, "higher", "lower"),
                 trend = ifelse(p<=0.1, "p<0.1", NA),
                 trend = ifelse(p<=0.05, "p<0.05", trend),
                 d = paste(trend, sign, sep=", "))
trend.c <- left_join(trend.c %>% distinct(id, Tunnus, p, tau, sen, sign, trend, d, iqr, CRI,a,
                                          # season
                                          ), 
                   plot %>% ungroup() %>% #na.omit() %>% 
                     distinct(id, Tunnus, N, E)) %>% 
  mutate(mark = paste(id, Tunnus, sep="_")) %>%
  filter(id != "s_21" &
           mark != "f_2167_1101p2" &
           mark != "f_2168_1101p12" & 
           mark !="f_2168_1101p5" & mark !="f_2168_1101p8" & 
           mark !="f_2168_1101p9" & mark != "f_830_0502p3" &
           mark!="f_784_0502p2")


# trend.c <- trend.c %>% filter(!mark %in% 
#                                 (rock %>% filter(grepl("berg", Akvifertyp)==TRUE $
#                                                    grepl("slutet", Akvifertyp)==TRUE))$mark)

grDevices::cairo_pdf(filename='sweden_class/gwlevel_trends.pdf', width=4, height=4,
                     fallback_resolution = 400)
ggmap(stamen_fenn, darken = c(0.6, "white")) + 
    geom_point(data=trend.c %>%
                 mutate(trend = ifelse(p<=0.05, NA, CRI)),
                 
                 # transform(season=factor(season, levels=c("winter", "summer"))),
                 # transform(month=factor(month, levels=c(12,1,2,3,4,5,6,7,8,9,10,11))),
                 aes(E, N, fill = CRI, colour= trend, shape=a), size=2,
               position=position_jitter(h=0.5,w=0.5)) +
  # scale_colour_manual("significance", values=c("lower"="orange", "higher" = "steelblue",
  #                                            "no trend" = "lightgrey",
  #                                       "no trend, p<0.05" = "black",
  #                                       "no trend, not significant" = "lightgrey",
  #                                          "not significant" = "lightgrey", "p<0.05" = "black",
  #                                          "lower, p<0.05" = "black",
  #                                          "lower, not significant" = "orange",
  #                                          "higher, p<0.05" = "black",
  #                                          "higher, not significant" = "steelblue"),
  #                     guide=FALSE) +
  scale_colour_gradient2("CRI", #low="orange", mid = "white", high="steelblue",
                         high=COL2('RdBu')[170], mid = "white", low=COL2('RdBu')[10],
                         # limits=c(min(trend.c$CRI),max(1, max(trend.c$CRI))), 
                         limits=c(-2.5, 2.5),
                         na.value="black",
                         breaks=c(-2,-1, 0, 1,2),
                         guide='none') +
  # scale_alpha_manual("", values=c("p<0.05"=1, "p<0.1"=0.8), na.value=0.5) +
  # scale_fill_manual("trend", values=c("lower" = "orange", "higher" = "steelblue",
  #                                        "no trend" = "lightgrey")) +
  scale_fill_gradient2("CRI", #low="orange", mid = "white", high="steelblue",
                       high=COL2('RdBu')[170], mid = "white", low=COL2('RdBu')[10],
                       # limits=c(min(trend.c$CRI),max(1, max(trend.c$CRI))),
                       limits=c(-2.5, 2.5),
                       breaks=c(-2,-1, 0, 1,2)) +
  # scale_alpha_manual("significance", values=c("not significant" = .7, "p<0.05" = .9),
  #                    guide=FALSE)+
  scale_shape_manual("", values=c("lower"= 25, "higher" = 24, "no trend" = 21)) +
  theme(panel.background = element_rect(colour="lightgrey", fill="white"),
        panel.grid = element_line(colour=alpha("lightgrey",0.5)),
        axis.line = element_line(colour="lightgrey"),
        axis.ticks = element_line(colour="grey"),
        panel.border = element_rect(colour="grey", fill=NA),
        plot.margin = unit(c(0,0,0,0), "mm")) +
  ylab("") + xlab("") + my_theme + theme(legend.position='right')
  # scale_size_continuous(limits = c(0, 1), breaks=c(.2,.5,.8), range(.1, 1)) +
    # facet_wrap(~season)
dev.off()
# ggsave("level spat trends.tiff", width = 14, height = 14.6, units = "cm", dpi=300)


library(sf)

df <- st_as_sf(x = trend.c,                         
               coords = c("E", "N"),
               crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
write_sf(df, 'gwlevel_trends.shp')



# investigate well characteristics ----
fin  <- readRDS("output/process/fin_gw_raw.rds") %>% 
  distinct(Stationname, Tunnus, material, area_km, elevation) %>%
  rename(Station=Stationname) %>% left_join(., g %>% distinct(Station, Tunnus, id)) %>%
  mutate(mark=paste(id, Tunnus, sep="_")) %>% filter(!is.na(id)) %>% ungroup() %>% 
  select(-N, -E)%>% rename(Akvifertyp=material)

ggmap(stamen_fenn, darken = c(0.6, "white")) + 
  geom_point(data=trend.c %>%
               mutate(trend = ifelse(p<=0.05, NA, CRI), 
                      mark = paste(id, Tunnus, sep="_"), match=ifelse(mark %in% rock$mark, "T", "F")),
             aes(E, N, fill = CRI, colour= match, shape=a), size=2,
             position=position_jitter(h=0.2,w=0.3)) + scale_colour_manual(values=c("T" = "red", "F" = "black"))+
  scale_fill_gradient2("CRI", low="orange", mid = "lightgrey", high="steelblue",
                       limits=c(min(trend.c$CRI),max(1, max(trend.c$CRI))),
                       breaks=c(-2,-1, 0, 1,2)) +
  scale_shape_manual("", values=c("lower"= 25, "higher" = 24, "no trend" = 21)) +
  theme_bw() +ylab("") + xlab("")

ggmap(stamen_fenn, darken = c(0.6, "white")) + 
  geom_point(data=trend.c %>%
               mutate(trend = ifelse(p<=0.05, NA, CRI), 
                      mark = paste(id, Tunnus, sep="_"), match=ifelse(mark %in% rock$mark, "T", "F")),
             aes(E, N, fill = CRI, colour= trend, shape=match), size=2, 
             position=position_jitter(h=0.3,w=0.3)) + 
  scale_colour_gradient2("CRI", low="orange", mid = "white", high="steelblue",
                         limits=c(min(trend.c$CRI),max(1, max(trend.c$CRI))), na.value="black",
                         breaks=c(-2,-1, 0, 1,2),
                         guide=FALSE) +
  scale_fill_gradient2("CRI", low="orange", mid = "white", high="steelblue",
                       limits=c(min(trend.c$CRI),max(1, max(trend.c$CRI))),
                       breaks=c(-2,-1, 0, 1,2)) +
  scale_shape_manual("", values=c("T"= 22, "F" = 21)) +
  theme_bw() +ylab("") + xlab("")


trend.b <- trend.c %>%
  mutate(trend = ifelse(p<=0.05, NA, CRI), 
         mark = paste(id, Tunnus, sep="_"), match=ifelse(mark %in% rock$mark, "T", "F")) %>% 
  left_join(., full_join(rock, fin)) %>% 
  mutate(material=ifelse(is.na(Akvifertyp)==TRUE, NA, "unconsolidated"),
         material=ifelse(grepl("berg", Akvifertyp)==TRUE, "consolidated", material),
         type = ifelse(grepl("slutet", Akvifertyp)==TRUE, "confined", NA),
         type = ifelse(grepl("öppet", Akvifertyp)==TRUE, "unconfined", type))
trend.b <- trend.b %>% select(-elevation) %>%
  left_join(., diff %>% filter(year>=1980 & year <=2010)  %>%
              distinct(Tunnus, id, date, elevation, raw_dep, year, month, season) %>%
              group_by(id, Tunnus) %>% # season or month
              mutate(median=median(raw_dep, na.rm=TRUE),
                     elevation=median(elevation-raw_dep, na.rm=TRUE)) %>% 
              distinct(id, Tunnus, year, median, elevation))%>% 
  mutate(mark=paste(id, Tunnus, sep="_"))  %>% 
  distinct(id, Tunnus, N, E, sign, material, elevation, type, mark, median, CRI)

trend.b  %>% mutate(country=ifelse(grepl("s", id), "Sweden", "Finland"))%>%
  ggplot(.) + geom_point(aes(median, CRI, fill=material, colour=sign, shape=type), na.rm=FALSE) + 
  scale_colour_viridis_d("significance", na.value="grey", direction=-1, end=0.8, option="magma") + 
  scale_fill_viridis_d(na.value="grey") + 
  scale_shape_manual(values=c("confined" = 22, "unconfined" = 25), na.value= 21) +
  facet_wrap(~country) + xlab("median groundwater depth") +
  
  guides(fill= guide_legend(override.aes = list(size=3, shape=22, colour=NA)),
         colour= guide_legend(override.aes = list(size=3, shape=0))) + 
  theme(panel.background = element_rect(colour="lightgrey", fill="white"),
        panel.grid = element_line(colour=alpha("lightgrey",0.5)),
        strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
        axis.ticks = element_line(colour="grey"))
        # plot.margin = unit(c(0,0,0,0), "mm"))

trend.b %>% mutate(country=ifelse(grepl("s", id), "Sweden", "Finland")) %>%
  ggplot(.) + geom_bar(aes(country, fill=material), stat="count", position="dodge", width=0.6) + 
  theme_bw() + scale_fill_grey(na.value="grey54")

ggmap(stamen_fenn, darken = c(0.6, "white")) + 
  geom_point(data= trend.b %>% distinct(id, N, E, Tunnus, sign, material, type, mark, median, CRI), 
             aes(E,N, fill=material, colour=sign, shape=type), alpha=.7, 
             position=position_jitter(width=0.2, height=0.2)) + 
  scale_colour_viridis_d(na.value="grey", direction=-1, end=0.8, option="magma") + 
  scale_fill_viridis_d(na.value="grey") + 
  scale_shape_manual(values=c("confined" = 22, "unconfined" = 25), na.value= 21)
  

# met lm ----
met <- tibm %>% distinct(date, id, Tunnus, RRday, Tday, OSQ, Q, E, N) %>% group_by(id) %>% 
  mutate(year = year(date), month = month(date),
         season = ifelse(month == 9 | month == 11 | month == 10, "Sep-Nov", NA),
         season = ifelse(month == 1 | month == 2 | 
                           month == 12, "Dec-Feb", season),
         season = ifelse(month == 4 | month == 5 | 
                           month == 3, "Mar-May", season),
         season = ifelse(month == 7 | month == 8 | 
                           month == 6, "Jun-Aug", season)) %>%
  distinct(id, Tunnus, date, RRday, Tday, OSQ, Q, year, season, month) %>% 
  left_join(., pet %>% ungroup %>% distinct(id, N, E, ymd, pet) %>% rename(date=ymd)) %>%
  mutate(ORQ = Q - OSQ) %>% filter(year(date) >= 1980)

# for snowmelt and rain trends
met <- met %>%
  group_by(id, year, season, month) %>%
  mutate(sump=sum(RRday), meant = mean(Tday), sumq = sum(Q), sumosq=sum(OSQ),
         sumorq= sum(ORQ)) %>%
  distinct(id, Tunnus, N, E, year, season, month, sump, meant, sumq, sumosq, sumorq, pet) %>%
  filter(year >=1980 & year <= 2010)

#attempt to similate ground frost
# met <- met %>% group_by(id, year, season, n=Tday <= 0) %>% 
#   mutate(frostdays=ifelse(Tday <= 0, row_number(), 0L),
#          nofrostdays=ifelse(Tday > 0, row_number(), 0L),
#          concrain = ifelse(ORQ > 0, 1, 0)) %>% 
#   ungroup() %>% group_by(id, year, season)  %>%
#   mutate(frost = max(frostdays),
#          nofrost = max(nofrostdays)) %>%  select(-n, -frostdays, -nofrostdays) %>% 
#   distinct(id, Tunnus, N, E, year, season, month, frost, nofrost)


# met <- met %>% filter(id %in% trend$id)
# met %>% 
#   mutate(group = ifelse(N < 59, "-59⸰ N", NA),
#          group = ifelse(N >= 59, "59⸰-63⸰ N", group),
#          group = ifelse(N > 63, "63⸰ N-", group)) %>% group_by(group, month) %>% 
#   mutate(meanp = mean(sump), meant = mean(meant)) %>% distinct(group, month, meanp, meant) %>% View


library(reshape2)
library(broom)
temp <- met %>%
  group_by(id, year, season, month) %>% #or season + month, do sumq-pet 
  mutate(sumpet=sum(pet), sumq = sum(sumq), sump = sum(sump),
         q_pet = sumq - sumpet) %>% distinct(id, N, E, year, season, month, 
                                                              sumq, sumpet, q_pet, frost, nofrost) %>%
  distinct(id, N, E, year, season, month, sumq) %>%
  ungroup() %>% group_by(id, N, E, season, month) %>%
  do(model=tidy(lm(sumq~year, data=.)),
     sum = summary(lm(sumq~year, data=.)))
temp$p=0
temp$est=0
temp$resid=0
temp$r.sq = 0
for(i in 1:length(temp$id)){
  temp$p[i] = temp$model[[i]]$p.value[2]
  temp$est[i] = temp$model[[i]]$estimate[2]
  
  temp$resid[i] = temp$sum[[i]]$sigma
  temp$r.sq[i] = temp$sum[[i]]$r.squared
}
temp <- temp %>% mutate(sign=ifelse(p <= 0.05, "p<0.05", "not significant"),
                        trend = ifelse(est > 0, "wetter", "drier"),
                        trend = ifelse(est == 0, "no trend", trend)) %>% 
  distinct(id, N, E, season, month, p, est, r.sq, sign, trend) %>% 
  na.omit(.)

label = "rain and melt"
ggmap(stamen) + 
  geom_point(data=temp %>%
               # transform(season=factor(season, levels=c("Dec-Feb", "Mar-May", "Jun-Aug",
               #                                          "Sep-Nov")))%>%
               transform(month=factor(month, levels=c(12,1,2,3,4,5,6,7,8,9,10,11))),
             aes(E, N, fill = trend, colour=sign, alpha=sign), size=2,  shape = 22) + 
  # scale_colour_viridis_c(direction = -1, breaks=c(-4,0,4), option="cividis") +
  scale_colour_manual("linear trend", values=c("drier"="orange", "wetter" = "steelblue",
                                      "no trend" = "lightgrey",
                      "not significant" = "lightgrey","p<0.05" = "black")) +
  # scale_fill_viridis_c(direction = -1, breaks=c(-4,0,4), option="cividis") +
  scale_fill_manual(name=label, values=c("drier" = "orange", "wetter" = "steelblue",
                                    "no trend" = "lightgrey")) +
  scale_alpha_manual("linear trend", values=c("not significant" = .3,"p<0.05" = 1),
                     na.translate = TRUE, na.value = NA) +
  facet_wrap(~month, ncol=3)

# time series analysis for lm ----
comb <- readRDS("output/process/URC_selectedgrids.rds") %>% 
  unnest_wider(monthly) %>% unnest() %>%
  filter(!is.na(temp) & lon >= min(plot$E, na.rm=TRUE) & 
                        lon <= max(plot$E, na.rm=TRUE) &
                        lat >= min(plot$N, na.rm=TRUE)) %>%
  mutate(year=year(date), month=month(date),
         group = ifelse(lat > 60, "cold", "temperate"),
         season = ifelse(month == 9 | month == 11 | month == 10, "SON", NA),
         season = ifelse(month == 1 | month == 2 | 
                           month == 12, "DJF", season),
         season = ifelse(month == 4 | month == 5 | 
                           month == 3, "MAM", season),
         season = ifelse(month == 7 | month == 8 | 
                           month == 6, "JJA", season),
         rs = ifelse(lat >60 & month >4 & month < 11, "frost-free season", "frost season"),
         rs = ifelse(lat <60 & month >3 & month < 12, "frost-free season", rs)) %>%
  transform(group = factor(group, levels=c("cold", 
                                           "temperate"))) %>% 
  
  group_by(year, lon, lat, group) %>% 
  mutate(pyr = sum(prec),
         tyr=median(temp)) %>% 
  group_by(year, group) %>%
  mutate(tyr=median(tyr), pyr = median(pyr)) %>%

  group_by(year, rs, lon, lat, group) %>% 
  mutate(meant=median(temp), sump = sum(prec)) %>% 
  group_by(year, rs, group) %>%
  mutate(meant=median(meant), sump = median(sump)) %>%
  
  distinct(year, rs, group, meant, sump, tyr, pyr)


#### met from 'met lm' ###
met.plot <- tibm %>% distinct(date, id, RRday, Tday, OSQ, Q, E, N) %>% group_by(id) %>% 
  mutate(year = year(date), month = month(date),
         season = ifelse(month == 9 | month == 11 | month == 10, "Sep-Nov", NA),
         season = ifelse(month == 1 | month == 2 | 
                           month == 12, "Dec-Feb", season),
         season = ifelse(month == 4 | month == 5 | 
                           month == 3, "Mar-May", season),
         season = ifelse(month == 7 | month == 8 | 
                           month == 6, "Jun-Aug", season)) %>%
  distinct(id, date, RRday, Tday, OSQ, Q, year, season, month) %>% 
  left_join(., pet %>% ungroup %>% distinct(id, N, E, ymd, pet) %>% rename(date=ymd)) %>%
  mutate(ORQ = Q - OSQ) %>% filter(year(date) >= 1980) %>% 
  
  ungroup() %>% filter(!is.na(N)) %>%
  mutate(group = ifelse(N > 60, "cold", "temperate"),
              season = ifelse(month == 9 | month == 11 | month == 10, "SON", NA),
              season = ifelse(month == 1 | month == 2 | 
                                month == 12, "DJF", season),
              season = ifelse(month == 4 | month == 5 | 
                                month == 3, "MAM", season),
              season = ifelse(month == 7 | month == 8 | 
                                month == 6, "JJA", season),
         country = ifelse(str_detect(id, "f"), "fi", "swe"),
         rs = ifelse(N >60 & month >4 & month < 11, "frost-free season", "frost season"),
         rs = ifelse(N <60 & month >3 & month < 12, "frost-free season", rs)) %>% 

  
  group_by(year, id, group, country) %>% 
  mutate(pyr = sum(RRday),
         tyr = median(Tday)) %>% 
  group_by(year, group, country) %>%
  mutate(tyr=median(tyr), pyr = median(pyr)) %>%
  
  group_by(year, rs, id, group, country) %>% 
  mutate(sump = sum(RRday),
         meant=median(Tday)) %>% 
  group_by(year, rs, group, country) %>%
  mutate(meant=median(meant), sump = median(sump)) %>%
  
  distinct(year, rs, group, meant, sump, country, tyr, pyr) %>% 
  transform(group = factor(group, levels=c("cold", 
                                           "temperate"))) #,
            # season = factor(season, levels=c("DJF", "MAM", "JJA", "SON"))) %>% 
  # distinct(year, season, group, meant, mint, maxt) %>% 
library(rlang)
metplot.fun <- function(met.plot, comb, xval, yfrs, yyr,  variable="T", myylab="°C",
                        labels=label.df){
  x <- met.plot %>% ungroup() %>% distinct(group)
  if(nrow(x)==2 | x[1] == "cold"){
    met.plot %>%
      ggplot(aes(x={{xval}}, y={{yfrs}})) + 
      geom_rect(data=comb %>% ungroup() %>% select(-rs) %>% distinct(year, group),
                aes(y=Inf, xmin=1980, xmax=2010, ymin=-Inf, ymax=Inf), colour="lightgrey", alpha=.01) +
      geom_line(data=comb%>% mutate(type="seasons"), 
                aes(colour= "CRU TS", group=rs, 
                    linetype={{variable}}), size=.5) + 
      geom_line(data=. %>% filter(country=="swe")%>% mutate(type="seasons"),
                aes(colour= "E-OBS RRA", group=rs, 
                    linetype={{variable}}), size=.5) +
      geom_line(data=. %>% filter(country=="fi")%>% mutate(type="seasons"),
                aes(colour= "FMI ClimGrid", group=rs,
                    linetype={{variable}}), size=.5) +
      
      # geom_point(data=comb%>% mutate(type="seasons"), 
      #            aes(fill= rs, colour= "CRU TS"), size=1, shape=21 ) + 
      # geom_point(data=. %>% filter(country=="swe")%>% mutate(type="seasons"),
      #            aes(fill= rs,colour= "E-OBS RRA"), size=1, shape=21) +
      # geom_point(data=. %>% filter(country=="fi")%>% mutate(type="seasons"),
      #            aes(fill= rs,colour= "FMI ClimGrid"), size=1, shape=21) +
      # 
      geom_smooth(data=comb%>% mutate(type="seasons"), 
                  aes(colour= "CRU TS", linetype="trend", group=rs),
                  method="lm", level=0, size=.5)+
      geom_smooth(data=. %>% filter(country=="swe")%>% mutate(type="seasons"),
                  aes(colour="E-OBS RRA", linetype="trend", group=rs),
                  method="lm", level=0, size=.5)+
      geom_smooth(data=. %>% filter(country=="fi") %>% mutate(type="seasons"),
                  aes(colour="FMI ClimGrid", linetype="trend", group=rs),
                  method="lm", level=0, size=.5)+
      
      
      geom_line(data=comb %>% distinct(year, group, tyr, pyr, rs) %>%
                  ungroup() %>%
                  mutate(rs="annual"), 
                aes(y={{yyr}}, colour= "CRU TS", group=rs, 
                    linetype={{variable}}), size=.5) + 
      geom_smooth(data=comb %>% distinct(year, group, tyr, pyr, rs) %>%
                    ungroup() %>%
                    mutate(rs="annual"), 
                  aes(y={{yyr}}, colour= "CRU TS", linetype="trend", group=rs),
                  method="lm", level=0, size=.5)+
      geom_line(data=. %>% filter(country=="swe") %>% distinct(year, group, tyr, pyr, rs) %>%
                  ungroup() %>%
                  mutate(rs="annual"),
                aes(y={{yyr}}, colour= "E-OBS RRA", group=rs, 
                    linetype={{variable}}), size=.5) +
      geom_smooth(data=. %>% filter(country=="swe") %>% distinct(year, group, tyr, pyr, rs) %>%
                    ungroup() %>%
                    mutate(rs="annual"),
                  aes(y={{yyr}}, colour="E-OBS RRA", linetype="trend", group=rs),
                  method="lm", level=0, size=.5)+
      geom_line(data=. %>% filter(country=="fi")%>% distinct(year, group, tyr, pyr, rs) %>%
                  ungroup() %>%
                  mutate(rs="annual"),
                aes(y={{yyr}}, colour= "FMI ClimGrid", group=rs,
                    linetype={{variable}}), size=.5) +
      geom_smooth(data=. %>% filter(country=="fi")%>% distinct(year, group, tyr, pyr, rs) %>%
                    ungroup() %>%
                    mutate(rs="annual"),
                  aes(y={{yyr}}, colour="FMI ClimGrid", linetype="trend", group=rs),
                  method="lm", level=0, size=.5)+
      
      scale_colour_manual("dataset", values=c("CRU TS"="#440154FF",#"#0D0887FF",
                                              "FMI ClimGrid" = "#2A788EFF",#"#FCA636FF",
                                              "E-OBS RRA"= "#2FB47CFF"))+ #"#B12A90FF")) +
      # scale_colour_viridis_d(end=0.8, direction=-1) +
      scale_fill_manual("", values=c("white", "black"), guide="none")+
      scale_linetype_manual("",values=c("solid","dashed")) +
      guides(lty = guide_legend(override.aes = list(col = 'black', fill="white"), order=1),
             colour= guide_legend(override.aes = list(size=2, fill="white"), order=2)) + 
      scale_y_continuous(myylab) + 
      scale_x_continuous("", limits=c(1960,2019), expand = c(0, 0)) +
      facet_grid(~rs~group, scales="free_y") + 
      # theme_bw() + 
      theme(panel.background = element_rect(colour="lightgrey", fill="white"),
            panel.grid = element_line(colour=alpha("lightgrey",0.5)),
            strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
            axis.ticks = element_line(colour="grey"),
            plot.margin = unit(c(0,0,0,0), "mm")) +
      geom_text(data = labels,
                mapping = aes(x = -Inf, y = Inf, label = label), hjust = -0.5, vjust  = 1.2, size=2.6)
        
  }
  else if (x[1]=="temperate"){
    met.plot %>%
      ggplot(aes(x={{xval}}, y={{yfrs}})) + 
      geom_rect(data=comb %>% ungroup() %>% select(-rs) %>% distinct(year, group),
                aes(y=Inf, xmin=1980, xmax=2010, ymin=-Inf, ymax=Inf), colour="lightgrey", alpha=.01) +
      geom_line(data=comb%>% mutate(type="seasons"), 
                aes(colour= "CRU TS", group=rs, 
                    linetype={{variable}}), size=.5) + 
      geom_line(data=. %>% filter(country=="swe")%>% mutate(type="seasons"),
                aes(colour= "E-OBS RRA", group=rs, 
                    linetype={{variable}}), size=.5) +
      
      # geom_point(data=comb%>% mutate(type="seasons"), 
      #            aes(fill= rs, colour= "CRU TS"), size=1, shape=21 ) + 
      # geom_point(data=. %>% filter(country=="swe")%>% mutate(type="seasons"),
      #            aes(fill= rs,colour= "E-OBS RRA"), size=1, shape=21) +
      # geom_point(data=. %>% filter(country=="fi")%>% mutate(type="seasons"),
      #            aes(fill= rs,colour= "FMI ClimGrid"), size=1, shape=21) +
      # 
      geom_smooth(data=comb%>% mutate(type="seasons"), 
                  aes(colour= "CRU TS", linetype="trend", group=rs),
                  method="lm", level=0, size=.5)+
      geom_smooth(data=. %>% filter(country=="swe")%>% mutate(type="seasons"),
                  aes(colour="E-OBS RRA", linetype="trend", group=rs),
                  method="lm", level=0, size=.5)+
      
      
      geom_line(data=comb %>% distinct(year, group, tyr, pyr, rs) %>%
                  ungroup() %>%
                  mutate(rs="annual"), 
                aes(y={{yyr}}, colour= "CRU TS", group=rs, 
                    linetype={{variable}}), size=.5) + 
      geom_smooth(data=comb %>% distinct(year, group, tyr, pyr, rs) %>%
                    ungroup() %>%
                    mutate(rs="annual"), 
                  aes(y={{yyr}}, colour= "CRU TS", linetype="trend", group=rs),
                  method="lm", level=0, size=.5)+
      geom_line(data=. %>% filter(country=="swe") %>% distinct(year, group, tyr, pyr, rs) %>%
                  ungroup() %>%
                  mutate(rs="annual"),
                aes(y={{yyr}}, colour= "E-OBS RRA", group=rs, 
                    linetype={{variable}}), size=.5) +
      geom_smooth(data=. %>% filter(country=="swe") %>% distinct(year, group, tyr, pyr, rs) %>%
                    ungroup() %>%
                    mutate(rs="annual"),
                  aes(y={{yyr}}, colour="E-OBS RRA", linetype="trend", group=rs),
                  method="lm", level=0, size=.5)+
      
      scale_colour_manual("dataset", values=c("CRU TS"="#440154FF",#"#0D0887FF",
                                              "FMI ClimGrid" = "#2A788EFF",#"#FCA636FF",
                                              "E-OBS RRA"= "#2FB47CFF"))+ #"#B12A90FF")) +
      # scale_colour_viridis_d(end=0.8, direction=-1) +
      scale_fill_manual("", values=c("white", "black"), guide="none")+
      scale_linetype_manual("",values=c("solid","dashed")) +
      guides(lty = guide_legend(override.aes = list(col = 'black', fill="white"), order=1),
             colour= guide_legend(override.aes = list(size=2, fill="white"), order=2)) + 
      scale_y_continuous(myylab) +
      scale_x_continuous("", limits=c(1960,2019), expand = c(0, 0)) +
      facet_grid(~rs~group, scales="free") + 
      # theme_bw() + 
      theme(panel.background = element_rect(colour="lightgrey", fill="white"),
            panel.grid = element_line(colour=alpha("lightgrey",0.5)),
            strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
            axis.ticks = element_line(colour="grey"),
            plot.margin = unit(c(0,0,0,0), "mm")) +
      geom_text(data = labels,
                mapping = aes(x = -Inf, y = Inf, label = label), hjust = -0.5, vjust  = 1.2, size=2.6)
  }
}

#save 4.8 in to 8 in
metplot.fun(met.plot, comb, xval=year, yfrs=sump, yyr=pyr, variable="P", myylab = "sum P [mm]", labels=label.df1) 
metplot.fun(met.plot, comb, xval=year, yfrs=meant, yyr=tyr, variable="T", myylab = "median T [ºC]")


# ggsave("prec trends.tiff", width = 17, height = 10.81, units = "cm", dpi=600)
# ggsave("temp trends.tiff", width = 17, height = 10.81, units = "cm", dpi=600)
# dev.off()



# load, group and visualise data for Mann-Kendall trend prep ----
urc.df <- readRDS("output/process/urcdf.rds")
library(Kendall)

#prep/test data
test <- urc.df %>% unnest() %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(year >1979 & year <=2010) %>%
  mutate(group  = ifelse(lat > 60, "cold", "temperate"),
         season = ifelse(month > 4 & month < 11, "summer", "winter"),
         pet = pet * days_in_month(date)) #%>% 
  group_by(lon, lat, year) %>% 
  mutate(sum_frs_y = sum(frs),
         med_temp_y = median(temp),
         sum_prec_y = sum(prec),
         sum_pet_y = sum(pet),
         sum_wet_y = sum(wet)) %>%
  group_by(lon, lat, year, month) %>%
  mutate(sum_prec_s = sum(prec),
         sum_pet_s = sum(pet),
         sum_wet_s = sum(wet),
         sum_frs_s=sum(frs)) %>% distinct(lon, lat, group, year, month, season, sum_prec_y, med_temp_y,
                                          sum_pet_y,sum_frs_y,sum_wet_y, sum_frs_s,
                                          sum_prec_s, sum_pet_s,sum_wet_s)

ggmap(stamen) + geom_tile(data=test %>% ungroup() %>% distinct(lon, lat, cz) %>% 
                            filter(!is.na(cz)), aes(lon, lat, fill=cz), alpha=.5) + 
  scale_fill_viridis_d()

test <- test %>% ungroup() %>% distinct(lon, lat, month, frs) %>% 
  group_by(lon, lat, month) %>% filter(!is.na(frs)) %>%
  mutate(med_frs=median(frs)/days_in_month(as.Date(paste("2000", 
                                                         as.character(month),"01",
                                                         sep="-")))*30) %>% 
  distinct(lon, lat, month, med_frs) %>% ungroup() %>%
  mutate(month = month.abb[month],
         month=factor(month, levels=c("Jan", "Feb", "Mar", "Apr",
                                      "May", "Jun", "Jul", "Aug",
                                      "Sep", "Oct", "Nov", "Dec")))

a <- ggmap(stamen_fenn, darken = c(0.6, "white")) + 
  geom_tile(data=test, aes(lon, lat, fill=med_frs), colour=NA, alpha=.75) +
  # geom_point(data=test %>% ungroup() %>% distinct (lon, lat, month, 
  #                                                  frs) %>% 
  #              group_by(lon, lat, month) %>%
  #              # transform(season=factor(season, levels=c("winter", "summer"))) %>% 
  #              mutate(sd_frs = sd(frs)) %>% 
  #              distinct(lon, lat, month, med_frs, sd_frs), 
  #           aes(lon, lat, colour=sd_frs), size=1) +
  scale_fill_viridis_c("days", direction = -1, na.value = "white") +
  scale_colour_viridis_c("days", na.value = "white", direction=-1, guide="none") +
  # scale_x_continuous("", breaks=c(10,20,30), minor_breaks = c(15, 25),
                     # labels = c(10,20,30)) + 
  ylab("") + xlab("") +
  geom_hline(aes(yintercept=60), colour="red") +
  facet_grid(.~month, cols=3) + 
  
  theme(panel.background = element_rect(colour="lightgrey", fill="white"),
        panel.grid = element_line(colour=alpha("lightgrey",0.5)),
        strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
        axis.ticks = element_line(colour="grey"),
        plot.margin = unit(c(0,0,0,0), "mm"),
        legend.margin = margin(0.2,0,0,0, "mm"),
        axis.text = element_text(size=8), 
        legend.position = "bottom",
        legend.text = element_text(size=8),
        legend.title = element_text(size=9)) 

leg <- get_legend(a + theme(legend.key.width = unit(9, "mm"),
                            legend.key.height = unit(3, "mm")))
# pdf("frost days.pdf", width=2.7, height=5)
plot_grid(a + theme(legend.position = "none"), 
          leg, rel_heights = c(2,0.1), rel_widths=c(1,0.8), nrow=2)
# dev.off()
ggsave("frost days2.tiff", width = 87.6, height = 150, units = "mm", dpi=600)

# find cells nearest the smaller gridcells
library(sp); library(rgeos)
sp.urc <- SpatialPoints(urc.df[ , c(1, 2)], proj4string=CRS("+proj=longlat + datum=WGS84"))
sp.reg <- SpatialPoints((pet %>% ungroup() %>% distinct(id, N, E))[ , c(3, 2)], 
                        proj4string=CRS("+proj=longlat + datum=WGS84"))

ids <- apply(gDistance(sp.urc, sp.reg, byid=TRUE), 1, which.min)
urc2.df <- urc.df %>% dplyr::filter(id %in% ids)
saveRDS(urc2.df, "output/process/URC_selectedgrids.rds")
pet <- pet %>% ungroup() %>% distinct(id, N, E)
pet$n <- ids
join.df <- pet %>% ungroup() %>% distinct(id, n, E,N)
join.df <- urc2.df %>% rename(n = id) %>% full_join(., join.df)

join.df <- join.df %>% left_join(., tibm %>% distinct(id, date, RRday, Q, OSQ) %>% 
                                   group_by(id) %>% nest(.key="daily"))

join.unnest <- join.df %>% unnest("monthly") %>% select(-temp,frs,-daily) %>% 
  filter(year(date) >=1980) 
join.unnest <- join.unnest %>% group_by(lon, lat, id, n, E, N) %>% 
  complete(date = seq.Date(as.Date("1980-01-01"), max(date), by="day")) %>% 
  group_by(month(date), lon, lat, id, n) %>% mutate(pet = pet[!is.na(pet)][1L]) %>% 
  ungroup() %>% select(-'month(date)')
join.unnest2 <- join.df %>% unnest("daily") %>% select(-monthly) 
join.unnest <- left_join(join.unnest, join.unnest2)
join.unnest <- join.unnest %>% group_by(lon, lat, id, n, E, N, date) %>% 
  mutate(q_pet = Q - pet, q_pet = ifelse(q_pet <0, 0, q_pet),
         p_pet = RRday - pet, p_et = ifelse(p_pet <0, 0, p_pet),
         month = month(date), year=year(date),
         group = ifelse(lat > 60, "snow dominated", "transition zone"),
         group = ifelse(lat <= 58, "temperate", group),
         rs  = ifelse(lat > 60 & month > 4 & month < 11, "MJJASO", NA),
         rs  = ifelse(lat <= 60 & lat >58 & month > 2 & month < 12,
                          "MAMJJASON", rs),
         rs  = ifelse(lat <= 58, "no frs", rs),
         season = ifelse(month == 9 | month == 11 | month == 10, "SON", NA),
         season = ifelse(month == 1 | month == 2 | 
                           month == 12, "DJF", season),
         season = ifelse(month == 4 | month == 5 | 
                           month == 3, "MAM", season),
         season = ifelse(month == 7 | month == 8 | 
                           month == 6, "JJA", season)) %>%
  filter(year <= 2010) %>% ungroup() %>% group_by(lon, lat, id, n, E, N)
saveRDS(join.unnest, "output/process/join_unnest.RDS")


# prep RM-PET and wet day data ----

join.unnest <- readRDS("output/process/join_unnest.RDS")
join.unnest2 <- join.unnest %>% #filter(!is.na(rs)) %>% 
  # mutate(season=ifelse(month > 4 & month < 11, "summer", "winter")) %>% 
  mutate(rs = ifelse(lat >60 & month >4 & month < 11, "frost-free season", "frost season"),
         rs = ifelse(lat <60 & month >3 & month < 12, "frost-free season", rs)) %>%
  group_by(lon, lat, id, n, E, N, year, rs) %>%
  mutate(qpet_rs=sum(q_pet), wet_rs=sum(wet, na.rm=TRUE),
            frs_rs = sum(frs, na.rm=TRUE), ppet_rs = sum(p_pet)) %>% 
  group_by(lon, lat, id, n, E, N, year) %>%
  mutate(qpet_yr=sum(q_pet), wet_yr=sum(wet, na.rm=TRUE),
            frs_yr = sum(frs, na.rm=TRUE), ppet_yr = sum(p_pet)) %>% 
  select(-date,-prec,-pet,-frs,-wet,-RRday,-Q,-OSQ,-q_pet,-p_pet,
         -p_et,-month,-season,-group) %>%
  distinct()


plot.ju <- join.unnest2 %>% #filter(!is.na(rs)) %>% 
  ungroup() %>%
  mutate(group  = ifelse(lat > 60, "cold", NA),
         group  = ifelse(lat <= 60, "temperate", group),
         # rs = ifelse(lat >60 & month >4 & month < 11, "frost-free season", "frost season"),
         # rs = ifelse(lat <60 & month >3 & month < 12, "frost-free season", rs),
         country=ifelse(grepl("s", id)==TRUE, "CCCS", "FMI ClimGrid")
         # season = ifelse(month > 4 & month < 11, "summer", "winter")
         ) %>%
  transform(group = factor(group, levels=c("cold", 
                                           "temperate"))) %>%
  
  # group_by(id, group, year, rs) %>%
  # mutate(sum_qpet = sum(q_pet), sum_wet = sum(wet, na.rm=TRUE)) %>% 
  
  group_by(year, group, rs) %>%
  mutate(qpet_rs = median(qpet_rs), wet_rs = median(wet_rs)) %>% #,
            # frs = median(sum_frs)) %>%
  
  # group_by(year, id, group) %>%
  # mutate(q_pet_yr = sum(q_pet), wet_yr = sum(wet, na.rm=TRUE)) %>%
  group_by(year, group) %>% 
  mutate(qpet_yr=median(qpet_yr), wet_yr = median(wet_yr)) %>%
  distinct(year, group, rs, qpet_rs, wet_rs, qpet_yr, wet_yr)

# make similar function for the r+m-pet and wet day plots ----
hcplot <- function(x=plot.ju, labels=label.df){
  x %>%
    ggplot(., aes(x=year)) + 
    geom_line(aes(y=wet_rs, linetype="value", colour="wet days"),
              size=0.5) +
    geom_line(aes(y=qpet_rs/4, linetype="value", colour="R+M-PET"),
              size=0.5) + 
    geom_smooth(aes(y=wet_rs, linetype="trend", colour="wet days"), 
                method="lm", level=0,
                size=0.5) +
    geom_smooth(aes(y=qpet_rs/4, linetype="trend", colour="R+M-PET"), 
                method="lm", level=0,
                size=0.5) +
    # 
    #   geom_point(aes(y=wet_rs, fill=rs, colour="wet days"),
    #             size=1, shape=21) +
    #   geom_point(aes(y=q_pet_rs/4, fill=rs, colour="R+M-PET"),
    #             size=1, shape=21) + 
    #   
    
    geom_line(data=. %>% ungroup() %>% mutate(rs="annual"), 
              aes(y=wet_yr, linetype="value", colour="wet days"),
              size=0.5) +
    geom_smooth(data=. %>% ungroup() %>% mutate(rs="annual"), 
                aes(y=wet_yr, linetype="trend", colour="wet days"), 
                method="lm", level=0,
                size=0.5) +
    geom_line(data=. %>% ungroup() %>% mutate(rs="annual"), 
              aes(y=qpet_yr/4, linetype="value", colour="R+M-PET"),
              size=0.5) + 
    geom_smooth(data=. %>% ungroup() %>% mutate(rs="annual"), 
                aes(y=qpet_yr/4, linetype="trend", colour="R+M-PET"), 
                method="lm", level=0,
                size=0.5) +
    
    scale_y_continuous(name= "days", sec.axis=sec_axis(trans=~.*4, name="mm")) +
    scale_x_continuous("", limits=c(1980,2010), expand = c(0, 0)) +
    scale_linetype_manual("", values=c("dashed", "solid")) + 
    scale_colour_manual("",
                        values=c("R+M-PET" = "#FCA636FF", "wet days" = "#B12A90FF")) +
    
    scale_fill_manual("", values=c("white", "black"), guide="none")+
    
    guides(lty = guide_legend(override.aes = list(col = 'black', fill="white"), order=1),
           colour= guide_legend(override.aes = list(size=2, fill="white"), order=2)) + 
    theme(panel.background = element_rect(colour="lightgrey", fill="white"),
          panel.grid = element_line(colour=alpha("lightgrey",0.5)),
          strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
          axis.ticks = element_line(colour="grey"),
          plot.margin = unit(c(0,0,0,0), "mm")) +
    
    
    facet_grid(~rs~group) +
    
    geom_text(data = labels,
              mapping = aes(x = -Inf, y = Inf, label = label), hjust = -0.5, vjust  = 1.2, size=2.6)
}

# ggsave("rmpet and wet trends.tiff", width = 17, height = 10.81, units = "cm", dpi=600)



join.unnest2 %>% ggplot(.) + geom_point(aes(sum_wet, sum_q_pet, 
                                            colour=sum_frs, group=n), alpha=.5) +
  scale_colour_viridis_c(direction=-1) + scale_y_continuous(trans="log10") + 
  scale_x_continuous(trans="log10")


# Mann-Kendall met trends ----
# join.unnest2 <- join.unnest %>% #filter(!is.na(rs)) %>% 
#   mutate(season=ifelse(month > 4 & month < 11, "summer", "winter"),
#          group  = ifelse(lat > 60, "cold", "temperate")) %>% 
#   transform(group = factor(group, levels=c("cold", 
#                                            "temperate")),
#             season = factor(season, levels=c("winter", "summer"))) %>%
#   group_by(lon, lat, id, n, E, N, year, group, season) %>%
#   summarise(sum_q_pet=sum(q_pet), sum_wet=sum(wet, na.rm=TRUE),
#             sum_frs = sum(frs, na.rm=TRUE), sum_p_pet = sum(p_pet))
library(broom); library(Kendall); library(trend)
join.trend.qr <- join.unnest2 %>% ungroup() %>% 
  mutate(type="R+M-PET") %>%
  group_by(lon, lat, id, n, E, N, rs, type) %>% 
  do(model= tidy(MannKendall(.$qpet_rs)),
     slope = sens.slope(.$qpet_rs)) 
join.trend.wr <- join.unnest2 %>% ungroup() %>% 
  mutate(type="wet days") %>%
  group_by(lon, lat, id, n, E, N, rs, type) %>% 
  do(model= tidy(MannKendall(.$wet_rs)),
     slope = sens.slope(.$wet_rs)) 
join.trend.qy <- join.unnest2 %>% ungroup() %>% 
  distinct(lon, lat, id, n, E, N, qpet_yr) %>%
  mutate(rs="annual", type="R+M-PET") %>%
  group_by(lon, lat, id, n, E, N, rs, type) %>% 
  do(model= tidy(MannKendall(.$qpet_yr)),
           slope = sens.slope(.$qpet_yr)) 
join.trend.wy <- join.unnest2 %>% ungroup() %>% 
  distinct(lon, lat, id, n, E, N, wet_yr) %>%
  mutate(rs="annual", type="wet days") %>%
  group_by(lon, lat, id, n, E, N, rs, type) %>% 
  do(model= tidy(MannKendall(.$wet_yr)),
     slope = sens.slope(.$wet_yr)) 
join.trend.qr$p = NA
join.trend.wr$p = NA
join.trend.qy$p = NA
join.trend.wy$p = NA
join.trend.qr$tau = NA
join.trend.wr$tau = NA
join.trend.qy$tau = NA
join.trend.wy$tau = NA
join.trend.qr$sen = NA
join.trend.wr$sen = NA
join.trend.qy$sen = NA
join.trend.wy$sen = NA

getvals <- function(x){
  for(i in 1:nrow(x)){
    x$p[i] = x$model[[i]]$p.value[1]
    x$tau[i] = x$model[[i]]$statistic[1]
    x$sen[i] = x$slope[[i]]$estimates[1]
  }
  return(x)
}


join.trend <- getvals(join.trend.qr) %>% select(-model,-slope) %>% 
  full_join(., getvals(join.trend.wr) %>% select(-model,-slope)) %>% 
  full_join(., getvals(join.trend.qy) %>% select(-model,-slope))%>% 
  full_join(., getvals(join.trend.wy) %>% select(-model,-slope)) %>% 
  group_by(lon, lat, id, n, E, N, rs, type) %>%
  mutate(sign=ifelse(p <= 0.05, "p<0.05", "not significant"),
                        trend = ifelse(sen > 0, "wetter", "drier"),
                        trend = ifelse(sen < 0.01 & sen > -0.01, "no trend", trend)) %>%
  mutate(group=ifelse(N > 60, "cold", "temperate"))
rm(join.trend.qr, join.trend.wr, join.trend.wy, join.trend.qy)
library(ggnewscale); library(corrplot)
join.trend.plot <- function(x, vars2 = "wet days", vars1="R+M-PET"){
  ggmap(stamen_fenn, darken = c(0.6, "white")) + 
    geom_tile(data=x %>% filter(type==vars1), 
              aes(lon, lat, fill=sen)) +
    # scale_fill_gradient2("mm/yr", high="steelblue", mid = "white", low="orange", 
    #                      breaks=c(-4,-2,0,2,4), limits=c(-5,5)) +
    scale_fill_gradient2("mm/yr", high=COL2('RdBu')[170], mid = "white", low=COL2('RdBu')[10], 
                         breaks=c(-4,-2,0,2,4), limits=c(-5,5)) +
    new_scale("fill") +
    geom_tile(data=x %>% filter(type==vars2), 
              aes(lon, lat, fill=sen)) +
    scale_fill_gradient2("days/yr", high=COL2('RdBu')[170], mid = "white", low=COL2('RdBu')[10],
                         breaks=c(-1,-0.5,0,0.5,1), limits=c(-1.2, 1.2)) +
    # scale_fill_distiller("days/yr",
                         # breaks=c(-1,-0.5,0,0.5,1), limits=c(-1.2, 1.2), palette = "Spectral")+
    
    geom_tile(data=join.trend %>%
                mutate(trend=ifelse(p<=0.05, "sign", NA)),
              aes(lon, lat, colour=trend), fill=NA)+
    scale_colour_manual(values=c("sign"="black"), na.value=NA) +
    facet_grid(rs~type) + guides(color = FALSE) +
    
    theme(panel.background = element_rect(colour="lightgrey", fill="white"),
          panel.grid = element_line(colour=alpha("lightgrey",0.5)),
          strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
          axis.ticks = element_line(colour="grey"),
          plot.margin = unit(c(0,0,0,0), "mm")) +
    
    ylab("") + xlab("") +
    geom_text(data = data.frame(label = c("A", "B", "C", "D", "E", "F"),
                                # type = c("R+M-PET", "wet days", "R+M-PET",
                                         # "wet days", "R+M-PET", "wet days"),
                                type = c("R+M-PET", "R+M-PET", "R+M-PET", 
                                         "wet days", "wet days", "wet days"),
                                # rs = c("annual", "annual", "frost-free season", "frost-free season",
                                       # "frost season", "frost season")), 
                                rs = c("annual", "frost-free season", "frost season", 
                                       "annual", "frost-free season", "frost season")), 
              mapping = aes(x = -Inf, y = Inf, label = label),  hjust = -1, vjust  = 1.2) 
}

grDevices::cairo_pdf(filename='sweden_class/met_trends.pdf', width=4, height=7,
                     fallback_resolution = 400)
join.trend.plot(x=join.trend) + my_theme + theme(legend.position='right')
dev.off()
# ggsave("met spat trends.tiff", width = 12.45, height = 15, units = "cm", dpi=300)


df <- st_as_sf(x = join.trend,                         
               coords = c("E", "N"),
               crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
write_sf(df, 'met_trends.shp')


# do MK trends on met based on graph figures, climate vs frost season ----

#met.plot and comb need to be calculated per grid cell, 
# and only averaged per group when trends are calculated?
met.trends <- comb %>% mutate(dataset="CRU TS") %>% ungroup() %>%
  full_join(., met.plot %>% rename(dataset=country) %>% 
              mutate(dataset=ifelse(dataset=="fi", "FMI ClimGrid", "E-OBS RRA")))
# plot.ju




library(broom); library(Kendall); library(trend)
trend.Trs <- met.trends %>% 
  group_by(group, rs, dataset, id, lon, lat) %>% filter(!is.na(meant)) %>%
  do(model= tidy(MannKendall(.$meant)),
     slope = sens.slope(.$meant))
trend.Tyr <- met.trends %>% ungroup() %>%
  mutate(rs="annual") %>%
  group_by(group, dataset, rs, id, lon, lat) %>% filter(!is.na(tyr)) %>%
  do(model= tidy(MannKendall(.$tyr)),
     slope = sens.slope(.$tyr))

trend.Prs <- met.trends %>% 
  group_by(group, rs, dataset, id, lon, lat) %>% 
  do(model= tidy(MannKendall(.$sump)),
     slope = sens.slope(.$sump))
trend.Pyr <- met.trends %>% ungroup() %>%
  mutate(rs="annual") %>%
  group_by(group, dataset, rs, id, lon, lat) %>% 
  do(model= tidy(MannKendall(.$pyr)),
     slope = sens.slope(.$pyr))

trend.QPETrs <- plot.ju %>% 
  group_by(group, rs) %>% 
  do(model= tidy(MannKendall(.$qpet_rs)),
     slope = sens.slope(.$qpet_rs))
trend.QPETyr <- plot.ju %>% ungroup() %>% 
  mutate(rs="annual") %>%
  group_by(group, rs) %>% 
  do(model= tidy(MannKendall(.$qpet_yr)),
     slope = sens.slope(.$qpet_yr))

trend.Wrs <- plot.ju %>% 
  group_by(group, rs) %>% 
  do(model= tidy(MannKendall(.$wet_rs)),
     slope = sens.slope(.$wet_rs))
trend.Wyr <- plot.ju %>% ungroup() %>%
  mutate(rs="annual") %>%
  group_by(group, rs) %>% 
  do(model= tidy(MannKendall(.$wet_yr)),
     slope = sens.slope(.$wet_yr))

trend.Trs$p = NA
trend.Trs$tau = NA
trend.Trs$sen = NA
trend.Tyr$p = NA
trend.Tyr$tau = NA
trend.Tyr$sen = NA

trend.Prs$p = NA
trend.Prs$tau = NA
trend.Prs$sen = NA
trend.Pyr$p = NA
trend.Pyr$tau = NA
trend.Pyr$sen = NA

trend.QPETrs$p = NA
trend.QPETrs$tau = NA
trend.QPETrs$sen = NA
trend.QPETyr$p = NA
trend.QPETyr$tau = NA
trend.QPETyr$sen = NA

trend.Wrs$p = NA
trend.Wrs$tau = NA
trend.Wrs$sen = NA
trend.Wyr$p = NA
trend.Wyr$tau = NA
trend.Wyr$sen = NA


getvals <- function(x){
  for(i in 1:nrow(x)){
    x$p[i] = x$model[[i]]$p.value[1]
    x$tau[i] = x$model[[i]]$statistic[1]
    x$sen[i] = x$slope[[i]]$estimates[1]
  }
  return(x)
}

trend.Trs <- getvals(trend.Trs) %>% select(-model, -slope) %>% mutate(type="Trs")
trend.Tyr <- getvals(trend.Tyr) %>% select(-model, -slope)%>% mutate(type="Tyr")

trend.Prs <- getvals(trend.Prs) %>% select(-model, -slope) %>% mutate(type="Prs")
trend.Pyr <- getvals(trend.Pyr) %>% select(-model, -slope) %>% mutate(type="Pyr")

trend.QPETrs <- getvals(trend.QPETrs) %>% select(-model, -slope) %>% mutate(type="QPETrs")
trend.QPETyr <- getvals(trend.QPETyr) %>% select(-model, -slope) %>% mutate(type="QPETyr")

trend.Wrs <- getvals(trend.Wrs) %>% select(-model, -slope) %>% mutate(type="Wrs")
trend.Wyr <- getvals(trend.Wyr) %>% select(-model, -slope) %>% mutate(type="Wyr")


trends <- trend.Trs %>% full_join(., trend.Tyr) %>% 
  full_join(., trend.Prs) %>% full_join(., trend.Pyr) %>% 
  full_join(., trend.QPETrs) %>% full_join(., trend.QPETyr) %>% 
  full_join(., trend.Wrs) %>% full_join(., trend.Wyr) %>%

  mutate(sign=ifelse(p <= 0.05, "p<0.05", "not significant"),
         trend = ifelse(sen > 0, "rise", "decline"),
         trend = ifelse(sen < 0.01 & sen > -0.01, "no trend", trend)) %>% 
  distinct(group, type, rs, dataset, p, tau, sen, sign, trend)
# save trend calc P and T for all grid cells
all <- trend.Trs %>% full_join(., trend.Tyr) %>% 
  full_join(., trend.Prs) %>% full_join(., trend.Pyr) %>%
  mutate(sign=ifelse(p <= 0.05, "p<0.05", "not significant"),
         trend = ifelse(sen > 0, "rise", "decline"),
         trend = ifelse(sen < 0.01 & sen > -0.01, "no trend", trend)) %>% 
  distinct(group, type, rs, dataset, p, tau, sen, sign, trend)
#redo calculation as median per group

# function trend plots with violin plots ----

violinplots <- function(alldat=join.trend, gendat=trends, version="hc", met=NULL, axtitle="˚C/yr", 
                        labels=label.df, metlimits=c(0,08)){
  if(version=="hc"){
   ggplot(data=alldat, aes(type, sen)) + 
      
      geom_violin(data=. %>% filter(type=="wet days"), aes(fill=type), colour=NA) +
      geom_violin(data=. %>% filter(type!="wet days"), aes(y=sen/4, fill=type), colour=NA) +
      
      geom_text(data=gendat %>% 
                   mutate(type=ifelse(grepl("W", type)==TRUE, "wet days", type)) %>% 
                   filter(type=="wet days"), 
                 mapping=aes(x=type, -Inf, label=paste("median:", round(sen,2)), colour=sign), vjust=-0.3, size=2.6)+
      geom_text(data=gendat %>% 
                   mutate(type=ifelse(grepl("QPET", type)==TRUE, "R+M-PET", type)) %>% 
                   filter(type=="R+M-PET"), 
                 mapping=aes(x=type, -Inf, label=paste("median:", round(sen,2)), colour=sign), vjust=-0.3, size=2.6)+
      
      # geom_label_repel(data=gendat %>% 
      #                    mutate(type=ifelse(grepl("W", type)==TRUE, "wet days", type)) %>% 
      #                    filter(type=="wet days"), 
      #                  mapping=aes(x=type, sen, label=round(sen,2), colour=sign), label.padding = 0.1,
      #                  size=2.5, vjust=2, hjust=0, fill="white", alpha=0.8) +
      # geom_label_repel(data=gendat %>% 
      #                    mutate(type=ifelse(grepl("QPET", type)==TRUE, "R+M-PET", type)) %>% 
      #                    filter(type=="R+M-PET"), 
      #                  mapping=aes(x=type, sen/4, label=round(sen,2), colour=sign), label.padding = 0.1,
      #                  size=2.5, vjust=2, hjust=0, fill="white", alpha=0.8) +
      
      scale_y_continuous(name= "days/yr", limits=c(-1.3,1.3), sec.axis=sec_axis(trans=~.*4, name="mm/yr")) +
      scale_fill_manual("variable",
                        values=c("R+M-PET" = "#FCA636FF", "wet days" = "#B12A90FF")) + 
      
      scale_colour_manual("", values=c("grey50", "black"), guide="none") +
      
      facet_grid(~rs~group, scales="free_x", space="free_x") + 
      
      guides(lty = guide_legend(override.aes = list(col = 'black', fill="white"), order=1),
             colour= guide_legend(override.aes = list(size=2, fill="white"), order=2)) + 
      theme(panel.background = element_rect(colour="lightgrey", fill="white"),
            panel.grid = element_line(colour=alpha("lightgrey",0.5)),
            strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
            axis.ticks = element_line(colour="grey"),
            plot.margin = unit(c(0,0,0,0), "mm"),
            axis.text.x = element_blank()) +
      
      xlab("") + 
      geom_text(data = labels,
                mapping = aes(x = -Inf, y = Inf, label = label), hjust = -0.5, vjust  = 1.2, size=2.6)
  } 
  else if(version=="met"){
    ggplot(data=alldat, aes(dataset, sen)) + 

    geom_violin(data= . %>%
                  mutate(type=ifelse(grepl(met, type)==TRUE & grepl("PET", type) == FALSE, met, type)) %>%
                  filter(type==met), aes(dataset, sen, fill=dataset), colour=NA) +
      
      
    geom_text(data= gendat %>% mutate(type=ifelse(grepl(met, type)==TRUE & grepl("PET", type) == FALSE,
                                          met, type)) %>% filter(type==met), 
              mapping=aes(x=dataset, -Inf, label=paste("median:", round(sen,2)), colour=sign), vjust=-0.3, size=2.6)+
    
    # geom_label_repel(data=gendat %>% mutate(type=ifelse(grepl(met, type)==TRUE & grepl("PET", type) == FALSE,
    #                                                     met, type)) %>% filter(type==met),
    #                  mapping=aes(x=dataset, sen, label=round(sen,2), colour=sign), label.padding = 0.1,
    #                  size=2.5, vjust=2, hjust=0, fill="white", alpha=0.8) +
      
    scale_y_continuous(name= axtitle, limits=metlimits) +

    scale_fill_manual("dataset", values=c("CRU TS"="#440154FF",
                                          "FMI ClimGrid" = "#2A788EFF",
                                          "E-OBS RRA"= "#2FB47CFF")) + 
      
    scale_colour_manual("", values=c("grey50", "black"), guide="none") +
    
    guides(lty = guide_legend(override.aes = list(col = 'black', fill="white"), order=1),
           colour= guide_legend(override.aes = list(size=2, fill="white"), order=2)) + 
    theme(panel.background = element_rect(colour="lightgrey", fill="white"),
          panel.grid = element_line(colour=alpha("lightgrey",0.5)),
          strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
          axis.ticks = element_line(colour="grey"),
          plot.margin = unit(c(0,0,0,0), "mm"),
          axis.text.x = element_blank()) +
      
      facet_grid(rs~group, scales="free_x", space="free_x") + 
      xlab("") + 
      geom_text(data = labels, 
                mapping = aes(x = -Inf, y = Inf, label = label), hjust = -0.5, vjust  = 1., size=2.6)
  } 
}

# label.df <- data.frame(label = c("a", "b", "c", "d", "e", "f"),
#            group = c("cold", "cold", "cold",
#                      "temperate", "temperate", "temperate"),
#            rs = c("annual", "frost-free season", "frost season",
#                   "annual","frost-free season", "frost season"))
label.df1 <- data.frame(label = c("A1", "A2", "A3", "B1", "B2", "B3"),
                        group = c("cold", "cold", "cold",
                                  "temperate", "temperate", "temperate"),
                        rs = c("annual", "frost-free season", "frost season",
                               "annual", "frost-free season", "frost season"))
label.df2 <- data.frame(label = c("A4", "A5", "A6", "B4", "B5", "B6"),
                        group = c("cold", "cold", "cold",
                                  "temperate", "temperate", "temperate"),
                        rs = c("annual", "frost-free season", "frost season",
                               "annual", "frost-free season", "frost season"))

# a <- metplot.fun(met.plot %>% filter(group=="cold"), comb %>% filter(group=="cold"),
#             labels=label.df1 %>% filter(group=="cold"),
#             xval=year, yfrs=meant, yyr=tyr, variable="T", myylab = "median T [ºC]")
# b <- violinplots(alldat = all %>% as.data.frame() %>% filter(group=="cold"),
#             gendat= trends %>% filter(group=="cold"),
#             labels = label.df2 %>% filter(group=="cold"),
#             version="met", met="T", axtitle="˚C/yr", metlimits=c(0,0.095))
# c <- metplot.fun(met.plot %>% filter(group=="temperate"), comb %>% filter(group=="temperate"),
#             labels=label.df1 %>% filter(group=="temperate"),
#             xval=year, yfrs=meant, yyr=tyr, variable="T", myylab = "median T [ºC]")
# d <- violinplots(alldat = all %>% as.data.frame() %>% filter(group=="temperate"),
#             gendat= trends %>% filter(group=="temperate"),
#             labels = label.df2 %>% filter(group=="temperate"),
#             version="met", met="T", axtitle="˚C/yr", metlimits=c(0,0.095))


# a <- metplot.fun(met.plot %>% filter(group=="cold"), comb %>% filter(group=="cold"),
#             labels=label.df1 %>% filter(group=="cold"),
#             xval=year, yfrs=sump, yyr=pyr, variable="P", myylab = "sum P [mm]")
# b <- violinplots(alldat = all %>% as.data.frame() %>% filter(group=="cold"),
#             gendat= trends %>% filter(group=="cold"),
#             labels = label.df2 %>% filter(group=="cold"),
#             version="met", met="P", axtitle="mm/yr", metlimits=c(-3.2,5.8))
# c <- metplot.fun(met.plot %>% filter(group=="temperate"), comb %>% filter(group=="temperate"),
#             labels=label.df1 %>% filter(group=="temperate"),
#             xval=year, yfrs=sump, yyr=pyr, variable="P", myylab = "sum P [mm]")
# d <- violinplots(alldat = all %>% as.data.frame() %>% filter(group=="temperate"),
#             gendat= trends %>% filter(group=="temperate"),
#             labels = label.df2 %>% filter(group=="temperate"),
#             version="met", met="P", axtitle="mm/yr", metlimits=c(-3.2,5.8))


a <- hcplot(plot.ju %>% filter(group=="cold"),
            labels=label.df1 %>% filter(group=="cold"))
b <- violinplots(alldat = join.trend %>% as.data.frame() %>% filter(group=="cold"),
            gendat= trends %>% filter(group=="cold"),
            labels = label.df2 %>% filter(group=="cold"),
            version="hc", met=NULL, axtitle=NULL)
c <- hcplot(plot.ju %>% filter(group=="temperate"),
            labels=label.df1 %>% filter(group=="temperate"))
d <- violinplots(alldat = join.trend %>% as.data.frame() %>% filter(group=="temperate"),
                 gendat= trends %>% filter(group=="temperate"),
                 labels = label.df2 %>% filter(group=="temperate"),
                 version="hc", met=NULL, axtitle=NULL)
library(cowplot)
a_leg <- get_legend(a + guides(col = guide_legend(override.aes = list(size=3, fill="white"))) +
                      theme(legend.position = "bottom"))
a_plot <- plot_grid(a + xlab("") + theme(legend.position = "none", 
                               plot.margin = unit(c(0,0.5,0,0), "mm"),
                               strip.background = element_blank(), strip.text = element_blank(),
                               plot.title = element_text(size=9),
                               text = element_text(size=9)) +
            ggtitle("A) cold climate"),
          b + theme(legend.position = "none", 
                    plot.margin = unit(c(0,0,0,0), "mm"),
                    text = element_text(size=9),
                    strip.background.x = element_blank(), strip.text.x = element_blank()), 
          c + xlab("") + theme(legend.position = "none", 
                               plot.margin = unit(c(0,0.5,0,0), "mm"),
                               strip.background = element_blank(), strip.text = element_blank(),
                               plot.title = element_text(size=9),
                               text = element_text(size=9)) +
            ggtitle("B) temperate climate"),
          d + theme(legend.position = "none", 
                    plot.margin = unit(c(0,0,0,0), "mm"),
                    text = element_text(size=9),
                    strip.background.x = element_blank(), strip.text.x = element_blank()),
          rel_heights = c(1,1,1,1), rel_widths = c(1,0.6,1,0.6), nrow=2, align="h", axis="tb")
plot_grid(a_plot, a_leg, rel_heights = c(2,0.05), rel_widths = c(2,0.8), nrow=2)


# count wells, clusters ----
n_g1 <- g1 %>% distinct(id, Tunnus, year) %>%
  mutate(omr_stn = paste(id, Tunnus, sep = "_")) %>%
  group_by(omr_stn) %>% mutate(nOmr_Stn = length(omr_stn)) %>% 
  group_by(id)

n_g8 <- g8 %>% distinct(id, Tunnus, year) %>%
  mutate(omr_stn = paste(id, Tunnus, sep = "_")) %>%
  group_by(omr_stn) %>% mutate(nOmr_Stn = length(omr_stn))


table <- g %>% distinct(id, Station, Tunnus, elevation, country) %>% mutate(m.a.s.l. = ifelse(elevation <= 100, "0-100", NA),
                                                          m.a.s.l. = ifelse(elevation > 100, "101-200", m.a.s.l.),
                                                          m.a.s.l. = ifelse(elevation > 200, "201-300", m.a.s.l.),
                                                          m.a.s.l. = ifelse(elevation > 300, "301-500", m.a.s.l.), 
                                                          # m.a.s.l. = ifelse(elevation > 400, "401-500", m.a.s.l.),
                                                          mean_rec = (g %>% filter(!is.na(mean_dep)) %>% 
                                                                           group_by(Station, Tunnus, id) %>% 
                                                                           mutate(mean_dep = mean(raw_dep)) %>%
                                                                           distinct(Tunnus, Station, mean_dep))$mean_dep %>% round(.,2),
                                                          # mean_wt = ifelse(mean_rec > 26, "> 21", NA),
                                                          # mean_wt = ifelse(mean_rec < 21, "17-20", mean_wt),
                                                          # mean_wt = ifelse(mean_rec < 17, "11-16", mean_wt),
                                                          mean_wt = ifelse(mean_rec >= 7, "7-27", NA),
                                                          mean_wt = ifelse(mean_rec < 7, "3-6", mean_wt), 
                                                          mean_wt = ifelse(mean_rec < 3, "0-2", mean_wt),
                                                          mean_wt = ifelse(mean_rec < 0, "artesian", mean_wt))
table %>% add_count(wt=!is.na(Tunnus), country, m.a.s.l., mean_wt) %>%
  distinct(country, m.a.s.l., n, mean_wt) %>% 
  arrange(country, m.a.s.l., mean_wt) %>% rename(wells=n) %>% View()
