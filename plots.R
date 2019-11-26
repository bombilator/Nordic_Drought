
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

library(magrittr)
library(purrr)
# library(tidyquant)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(sp)
library(lubridate)

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
g.df <- g.df %>% filter(year >= 1980 & year <=2010) %>% mutate(period=ifelse(year<1990, 1, 8)) %>% 
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


g.df <- readRDS("output/process/sgi_durs.rds") %>% ungroup() %>% select(-id) %>% #readRDS("output/3.sgi.rds") %>% 
  dplyr::rename(id=Cluster) %>% filter(id!="34") %>%
  group_by(id, Station) %>% mutate(date = as.Date(paste(format(date, "%Y-%m"), "-15", sep=""))) %>% 
  as.data.frame()
spi.ind <- readRDS("output/process/3clusters_spi.rds") %>% select(-month_past) %>% ungroup() %>% 
  as.data.frame() %>% group_by(id, agg)
smri.ind <- readRDS("output/process/3clusters_smri.rds") %>% select(-month_past) %>% ungroup() %>% 
  as.data.frame() %>% group_by(id, agg)
spei.ind <- readRDS("output/process/3clusters_spei.rds") %>% ungroup() %>% 
  as.data.frame() %>% group_by(id, agg)
smrei.ind <- readRDS("output/process/3cluster_smrei.rds") %>% ungroup() %>% 
  as.data.frame() %>% group_by(id, agg)

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
tibm <- readRDS("output/process/fennos_tibm_all_2.R") %>% select(E, N, id, date, RRday, Tday, Q)
# pet <- readRDS("output/process/fennos_pet_all.r") 

# Background map and coordinates ----
bbox = as.numeric(c(11,55,24,69)) # Sweden and Finland: c(11, 55, 32, 70))
names(bbox) <- c('left','bottom','right','top')
source("rfuncs/get_stamenmapPNG.R")
stamen <- get_stamenmapPNG(bbox, zoom = 6, maptype = "terrain")

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
  left_join(., plot %>% distinct(id, E, N) %>% rename(lat=N)) %>% 
  mutate(group = ifelse(lat < 59, "-59˚", NA),
         group = ifelse(lat >= 59, "59˚-63˚", group),
         group = ifelse(lat > 63, "63˚-", group)) 

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

t <- test %>% distinct(group, id, RRyr, Tyr, RRmean, Tmean, year) %>% 
           ggplot(.) + 
  geom_line(aes(year, Tyr, group = id, colour = group), alpha = .4, size=.1) +
  geom_smooth(aes(year, Tyr, group = group, colour = group), size=.6,
              method='lm', level = 0) +
  xlim(1980, 2011) + theme(axis.text =element_text(size=10)) + 
  scale_colour_viridis_d("Latitude", direction=-1) + ylab("average, ˚C /yr")

p <- test %>% distinct(group, id, RRyr, Tyr, RRmean, Tmean, year) %>% 
  ggplot(.) + 
  geom_line(aes(year, RRyr, group = id, colour=group),alpha = .4, size=.1)+
  geom_smooth(aes(year, RRyr, group = group, colour = group), size=.6,
              method='lm', level = 0) + 
  xlim(1980, 2011) + theme(axis.text =element_text(size=10)) + 
  scale_colour_viridis_d("Latitude", direction=-1) + ylab("sum, mm /yr") 
  
wt <- test.g %>% ggplot(.) + 
  geom_line(aes(year, yr_gw_all, colour = group, group = id), alpha=.4, size=.1) +
  # geom_smooth(aes(year, yr_gw_all, group = group), colour = "white", method = 'lm', level = 0, size=1.5) +
  geom_smooth(aes(year, yr_gw_all, group = group, colour = group), method = 'lm', level = 0, size=.6) +
  xlim(1980, 2011) + theme(axis.text =element_text(size=10))+ 
  scale_colour_viridis_d("Latitude", direction=-1) + ylab("average, m /yr") + 
  scale_y_reverse() 

library(cowplot)
prow1 <- plot_grid(t + theme(legend.position = "none",
                             axis.title.x = element_blank()) + ggtitle("Temperature"),
                   p + theme(legend.position = "none",
                             axis.title.x = element_blank()) + ggtitle("Precipitation"), 
                   wt + theme(legend.position = "none") + ggtitle("Water tables"),
                   nrow=3, align = "hv", axis = "lb", ncol = 1,
                   rel_heights = c(1,1))
legend1 <- get_legend(t)
plot_grid(prow1, legend1, rel_widths = c(6, 1.5), ncol = 2)

  
# plotting the annual mean fluctuations, all ids ----
# wb_gw <- 
g8  %>% #filter(id==station) %>%
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
           # panel.background = element_blank(),
           # strip.text.x = element_blank()
           legend.position = "none", axis.text =element_text(size=12), 
           axis.line.x = element_blank(),
           panel.grid.major=element_blank(), 
           panel.background = element_rect(fill="white", colour="white")) + 
    ggtitle("1980-1989 (P1)") +
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
tibm.f %>% ungroup() %>% filter(period==1) %>% 
  # group_by(date) %>%
  # mutate(
  #   P_snow_av = geometric.mean(P_snow +1)-1,
  #   RRday_av = geometric.mean(RRday+1)-1 - P_snow_av,
  #   Tday_av = geometric.mean(Tday+40)-40,
  #   Q_av = geometric.mean(Q+1)-1,
  #   OSliq_av = geometric.mean(OSliq+1)-1,
  #   OSQ_av = geometric.mean(OSQ+1)-1
  # ) %>% ungroup() %>% 
  # distinct(date, P_snow_av, 
  #          RRday_av, Tday_av, Q_av, OSliq_av, OSQ_av) %>% 
  ggplot(.) + 
  # ggtitle(label = "3697")+
  
  geom_col(aes(ymd, Q/500, fill = id, group = id),
           # colour = NA,
           size = .1, alpha =.7) + 
  
  # geom_col(
  #   aes(date, RRday_av,
  #       fill = "Rain"), alpha = .5
  # ) + 
  
  # geom_col(aes(date, P_snow_av,
  #              fill = "Snow"), alpha=.4
  #   )  +
  
  geom_line(aes(ymd, Tday, colour = id, group = id),
              alpha=.5) +
  
  # scale_fill_manual("",
  #                     values=c("Rain"="steelblue2", 
  #                              "Snow"="purple",
  #                              "Snowmelt" = "gray50")) +
  # scale_colour_manual("", 
  #                     values = c("Temp" = "red",
                                 # "Rain + snowmelt" = "gray50")) +
  # scale_x_date(date_labels = "%m-%d",
  #                  limits=c(as.Date("2006-01-01"),as.Date("2010-12-30"))
  #                  ) +
  # scale_y_continuous("mm", limits = c(-15,30), sec.axis = sec_axis(~.*2, "Celsius")) +
  theme_classic() #+
  theme(
    panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        legend.position="none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

library(cowplot)
plot_grid(a,b,c, ncol = 1, align = "hv", axis = "rlb")

# water balance with gw, investigating ids----
require(smooth)
require(Mcomp)

station = 55
Aper = 4
g.df %>% group_by(id, Station) %>%
  filter((Station==9 & id==55) | ( id==37 & Station==34) | (id==14 & Station==2)) %>%
  ggplot(.) + #ggtitle(label = station) +
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
    # geom_smooth(aes(ymd, g_geom*1), level=0) +
    
    #standardised ----

    # geom_line(data=spi.ind, aes(date, index, group=agg), colour="salmon", alpha=.4) +
    geom_ribbon(data = spi.ind %>% filter(id!=55) %>%
                mutate(type=ifelse(as.numeric(agg)<6, "short", "long")) %>%
                  group_by(id, date, type) %>% 
                  mutate(mini=min(index), maxi=max(index)),
              aes(date, ymin=mini, ymax=maxi, colour=type), #group=agg),
            alpha=.2) +
  # geom_ribbon(data = spei.ind %>%
  #               group_by(id, date) %>%
  #               mutate(mini=min(index), maxi=max(index)),
  #             aes(date, ymin=mini, ymax=maxi, group=id, colour="all spei"), #group=agg),
  #             alpha=.2) +
  # geom_ribbon(data = smri.ind %>%
  #               group_by(id, date) %>%
  #               mutate(mini=min(index), maxi=max(index)),
  #             aes(date, ymin=mini, ymax=maxi, group=id, colour="all smri"), #group=agg),
  #             alpha=.2) +
  geom_ribbon(data = smrei.ind %>% filter(id==55) %>%
                mutate(type=ifelse(as.numeric(agg)<6, "short", "long")) %>%
                group_by(id, date, type) %>%
                mutate(mini=min(index), maxi=max(index)),
              aes(date, ymin=mini, ymax=maxi, colour=type), #group=agg),
              alpha=.2) +

  geom_line(aes(date, sgi, group=Station, colour="sgi"), size=1, alpha = .8) +
  
  # geom_ribbon(data=d.spi %>% group_by(id, date) %>% filter(index < 0.1) %>% 
  #               mutate(mini=min(index), maxi=max(index)),
  #             aes(date, ymin=mini, ymax=0, group=id, 
  #                 colour="all spi"), alpha=.3) +
  # geom_ribbon(data=d.spei %>% group_by(id, date) %>% filter(index < 0.1) %>% 
  #               mutate(mini=min(index)),
  #             aes(date, ymin=mini, ymax=0, group=id, 
  # #                 colour="all spei"), alpha=.3) +
    # geom_ribbon(data=. %>% group_by(id, dry_year) %>% filter(any(sgi <= -1.5) & dry_dur > 365) %>%
    #             mutate(dry_dur=as.numeric(dry_dur)),
    #           aes(x=dry_dat, ymin=sgi, ymax=0, group=dry_year), alpha=.2)+
  # 55 ----
# 
#   # geom_line(data = smrei.ind %>% mutate(Station=1) %>% filter(id == station & agg==5),
#   #     aes(date, index, colour="Cmax"), linetype=2, size=1) +
#   # geom_line(data = spei.ind %>% mutate(Station=11) %>% filter(id == station & agg==6),
#   #           aes(date, index, group=agg, colour="Cmax"), linetype=2, size=1) +
#   # geom_line(data = smrei.ind %>% mutate(Station=12) %>% filter(id == station & agg==6),
#   #           aes(date, index, group=agg, colour="Cmax"), linetype=2, size=1) +
#   # geom_line(data = smri.ind %>% mutate(Station=13) %>% filter(id == station & agg==48),
#   #           aes(date, index, colour="Cmax"), linetype=2, size=1) +
#   # geom_line(data = smri.ind %>% mutate(Station=14) %>% filter(id == station & agg==7),
#   #           aes(date, index, group=agg, colour="Cmax"), linetype=2, size=1) +
#   # geom_line(data = smri.ind %>% mutate(Station=17) %>% filter(id == station & agg==11),
#   #           aes(date, index, group=agg, colour="Cmax"), linetype=2, size=1) +
#   # geom_line(data = smri.ind %>% mutate(Station=7) %>% filter(id == station & agg==5),
#   #           aes(date, index, colour="Cmax"), linetype=2, size=1) +
#   # geom_line(data = smrei.ind %>% mutate(Station=8) %>% filter(id == station & agg==12),
#   #           aes(date, index, group=agg, colour="Cmax"), linetype=2, size=1) +

  geom_line(data = smrei.ind %>% mutate(Station=9) %>% filter(id == station & agg==5), #round(6*2,0)),
            aes(date, index, group=agg, colour="Cmax"), linetype=1, size=1) +
  # geom_line(data = smrei.ind %>% mutate(Station=9) %>% filter(id == station & agg==6), #round(6*2,0)),
  #           aes(date, index, group=agg, colour="short"), linetype=2, size=.8) +
  # # geom_line(data = smrei.ind %>% mutate(Station=9) %>% filter(id == station & agg==24), #round(6*2,0)),
  # #           aes(date, index, group=agg, colour="long"), linetype=2, size=.8) +
  # 
  # # geom_line(data = spi.ind %>% mutate(Station=1) %>% filter(id == station & agg==round(4/3,0)),
  # #           aes(date, index, colour="spei-2"), linetype=2, size=1) +
  # # geom_line(data = spi.ind %>% mutate(Station=11) %>% filter(id == station & agg==round(6/3,0)),
  # #           aes(date, index, group=agg, colour="spei-2"), linetype=2, size=1) +
  # # geom_line(data = spei.ind %>% mutate(Station=12) %>% filter(id == station & agg==round(6/3,0)),
  # #           aes(date, index, group=agg, colour="spei-2"), linetype=2, size=1) +
  # # geom_line(data = spei.ind %>% mutate(Station=13) %>% filter(id == station & agg==round(11/3,0)),
  # #           aes(date, index, colour="spei-2"), linetype=2, size=1) +
  # # geom_line(data = spei.ind %>% mutate(Station=14) %>% filter(id == station & agg==round(9/3,0)),
  # #           aes(date, index, group=agg, colour="spei-2"), linetype=2, size=1) +
  # # geom_line(data = spei.ind %>% mutate(Station=17) %>% filter(id == station & agg==round(6/3,0)),
  # #           aes(date, index, group=agg, colour="spei-2"), linetype=2, size=1) +
  # # geom_line(data = spei.ind %>% mutate(Station=7) %>% filter(id == station & agg==round(4/3,0)),
  # #           aes(date, index, colour="spei-2"), linetype=2, size=1) +
  # # geom_line(data = spi.ind %>% mutate(Station=8) %>% filter(id == station & agg==round(11/3,0)),
  # #           aes(date, index, group=agg, colour="spei-2"), linetype=2, size=1) +
  # geom_line(data = smri.ind %>% mutate(Station=9) %>% filter(id == station & agg==5), #round(5/3,0)),
  #           aes(date, index, group=agg, colour="smri"), linetype=2, size=1) +
  # geom_line(data = spei.ind %>% mutate(Station=9) %>% filter(id == station & agg==5), #round(5/3,0)),
  #           aes(date, index, group=agg, colour="spei"), linetype=2, size=1) +
  # geom_line(data = smrei.ind %>% mutate(Station=9) %>% filter(id == station & agg==5), #round(5/3,0)),
  #           aes(date, index, group=agg, colour="smrei"), linetype=2, size=1) +
  # # 37 ----
  # #     geom_line(data = spei.ind %>% mutate(Station=32) %>% filter(id == 37 & agg==7),
  # #             aes(date, index, group=agg, colour="Cmax"), linetype=2, size=1) +
  # #     geom_line(data = spi.ind %>% mutate(Station=1) %>% filter(id == 37 & agg==7),
  # #               aes(date, index, group=agg, colour="Cmax"), linetype=2, size=1) +
  # #     geom_line(data = smrei.ind %>% mutate(Station=10) %>% filter(id == 37 & agg==10),
  # #               aes(date, index, colour="Cmax"), linetype=2, size=1) +
  # #     geom_line(data = smrei.ind %>% mutate(Station=12) %>% filter(id == 37 & agg==10),
  # #               aes(date, index, group=agg, colour="Cmax"), linetype=2, size=1) +

      geom_line(data = spi.ind %>% mutate(Station=34) %>% filter(id == 37 & agg==10), #round(11*2,0)),
                aes(date, index, group=agg, colour="Cmax"), linetype=1, size=1) +
  # geom_line(data = spei.ind %>% mutate(Station=34) %>% filter(id == 37 & agg==14), #round(11*2,0)),
  #           aes(date, index, group=agg, colour="short"), linetype=2, size=.8) +
  # # geom_line(data = spi.ind %>% mutate(Station=34) %>% filter(id == 37 & agg==24), #round(11*2,0)),
  # #           aes(date, index, group=agg, colour="long"), linetype=2, size=.8) +
  # 
  # # geom_line(data = spei.ind %>% mutate(Station=32) %>% filter(id == 37 & agg==round(7/3,0)),
  # #           aes(date, index, group=agg, colour="spei-2"), linetype=2, size=1) +
  # # geom_line(data = spei.ind %>% mutate(Station=1) %>% filter(id == 37 & agg==round(7/3,0)),
  # #           aes(date, index, group=agg, colour="spei-2"), linetype=2, size=1) +
  # # geom_line(data = spi.ind %>% mutate(Station=10) %>% filter(id == 37 & agg==round(10/3,0)),
  # #           aes(date, index, colour="spei-2"), linetype=2, size=1) +
  # # geom_line(data = spi.ind %>% mutate(Station=12) %>% filter(id == 37 & agg==round(10/3,0)),
  # #           aes(date, index, group=agg, colour="spei-2"), linetype=2, size=1) +
  # geom_line(data = spei.ind %>% mutate(Station=34) %>% filter(id == 37 & agg==10), #round(10/3, 0)),
  #           aes(date, index, group=agg, colour="spei"), linetype=2, size=1) +
  # geom_line(data = smri.ind %>% mutate(Station=34) %>% filter(id == 37 & agg==10), #round(10/3, 0)),
  #           aes(date, index, group=agg, colour="smri"), linetype=2, size=1) +
  # geom_line(data = smrei.ind %>% mutate(Station=34) %>% filter(id == 37 & agg==10), #round(10/3, 0)),
  #           aes(date, index, group=agg, colour="smrei"), linetype=2, size=1) +
  # 
  # #14 ----
  # # geom_line(data = spi.ind %>% mutate(Station=3) %>% filter(id == 14 & agg==10),
  # #           aes(date, index, colour="Cmax"), linetype=2, size=1) +

  geom_line(data = spi.ind %>% mutate(Station=2) %>% filter(id == 14 & agg==14),
            aes(date, index, group=agg, colour="Cmax"), linetype=1, size=1) +
  # geom_line(data = spi.ind %>% mutate(Station=2) %>% filter(id == 14 & agg==14),
  #           aes(date, index, group=agg, colour="short"), linetype=2, size=.8) +
  # # geom_line(data = spi.ind %>% mutate(Station=2) %>% filter(id == 14 & agg==24),
  # #           aes(date, index, group=agg, colour="long"), linetype=2, size=.8) +

  # geom_line(data = spi.ind %>% mutate(Station=6) %>% filter(id == 14 & agg==17),
  #           aes(date, index, group=agg, colour="Cmax"), linetype=2, size=1) +
  # # geom_line(data = spei.ind %>% mutate(Station=3) %>% filter(id == 14 & agg==10),
  # #           aes(date, index, colour="spei-2"), linetype=2, size=1) +
  # geom_line(data = spei.ind %>% mutate(Station=2) %>% filter(id == 14 & agg==14),
  #           aes(date, index, group=agg, colour="spei"), linetype=2, size=1) +
  # geom_line(data = smri.ind %>% mutate(Station=2) %>% filter(id == 14 & agg==14),
  #           aes(date, index, group=agg, colour="smri"), linetype=2, size=1) +
  # geom_line(data = smrei.ind %>% mutate(Station=2) %>% filter(id == 14 & agg==14),
  #           aes(date, index, group=agg, colour="smrei"), linetype=2, size=1) +
  # # geom_line(data = spei.ind %>% mutate(Station=6) %>% filter(id == 14 & agg==16),
  # #           aes(date, index, group=agg, colour="spei-2"), linetype=2, size=1) +

  
  # geom_hline(yintercept = 0, alpha=.5) +
  # geom_ribbon(aes(date, ymin=0, ymax=2.5), fill="white")+
      xlim(as.Date("1970-01-15"),as.Date("1978-01-15")) +
    theme_classic() +
    facet_wrap(~id, ncol=1) +
    scale_colour_manual(values=c("sgi" = "black", "long" = "red", 
                                 "short" = "cyan", "Cmax" = "blue",
                                 "spei" = "red",
                                 "spi" = "green", "smri" = "yellow",
                                 "smrei" = "orange",
                                 "all spi" = "lightgreen",
                                 "all spei" = "pink",
                                 "all smri" = "yellow",
                                 "all smrei" = "orange")) 

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

g %>% distinct(id, Tunnus, mean_wt, elevation, N, country) %>% #filter(id %in% dtibm$id) %>% 
  mutate(Nn = round(N*0.001,0)) %>%
  ggplot(.) + 
  theme_classic() +
  aes(mean_wt, N ) + #Nn) + #, colour = max(gmax)-min(gmin)) + 
  geom_point(alpha=.4, aes(colour = country)) +
  geom_label_repel(data = g %>% filter(mean_wt > 10 | mean_wt < 0) %>% 
                     distinct(id, Tunnus, mean_wt, elevation, N) %>%  
                     mutate(Nn = round(N*0.001,0)),
                                       aes(mean_wt, N, label=id),
                   box.padding   = 0.4, 
                   point.padding = 0.5,
                   segment.color = 'grey50', alpha = .7, size = 3) #+
  scale_colour_gradient2(low="blue", mid = "yellow", high="red", midpoint = 0)
                         
g %>% group_by(id, Tunnus) %>%
  filter(id== "f_1162" | id == "s_157" |
           id == "f_3453" | id == "s_98" | 
           id == "f_131" | id == "s_23" |
           id=="s_93" | id == "f_2465") %>% distinct(id, raw_dep) %>% 
  group_by(id) %>%mutate(mean_dep = mean(raw_dep, na.rm=TRUE)) %>%
  distinct(id, mean_dep) %>% 
  ggplot(.) + aes(id, mean_dep) +geom_bar(stat="identity", width=0.5) + 
  ylab("mean water table depth, m") + 
  theme(axis.text = element_text(size=18), axis.title = element_text(size=18)) +
  xlab("")

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
# raster::shapefile(dur_type, "dur_type.shp")

# dur plot  ----
library(sp)
library(cowplot)
library(ggmap)
# a <- 
ggmap(stamen) +
  geom_point(data = full_join(dur_type8 %>% mutate(period="P1"), dur_type1 %>% mutate(period="P2")),
             aes(x = E, y = N, group = id, colour = qpet_type, alpha=re, 
                 size = re),
             position=position_jitter(h=0.2,w=0.2)) + #scale_size(range = c(.1,3)) +
  scale_colour_viridis_d() +
  facet_wrap(~season+period, nrow=2) + ggtitle("RWT") + xlab("")
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
# re dis
dur1 %>%
  filter(id== "f_1162" | id == "s_157" |
           id == "f_3453" | id == "s_98" |
           id == "f_131" | id == "s_46" |
           id=="s_93" | id == "f_2465") %>% mutate(period="2001-2010") %>%
  transform(id=factor(id, levels=c("f_3453", "s_93", "f_2465", "f_1162", 
                                      "s_46", "f_131", "s_98", "s_157"))) %>%
  ggplot(.) +
  geom_segment(data = dur1_2 %>%
                 filter(id== "f_1162" | id == "s_157" |
                          id == "f_3453" | id == "s_98" |
                          id == "f_131" | id == "s_46" |
                          id=="s_93" | id == "f_2465")%>%
                 transform(id=factor(id, levels=c("f_3453", "s_93", "f_2465", "f_1162", 
                                                  "s_46", "f_131", "s_98", "s_157"))),
               aes(x = ymd,y=0.8, xend=ymd, yend=1, colour = re_dis_type), 
               size=1, alpha=.6) +
  geom_segment(data = dur8_2 %>%
                 filter(id== "f_1162" | id == "s_157" |
                          id == "f_3453" | id == "s_98" |
                          id == "f_131" | id == "s_46" |
                          id=="s_93" | id == "f_2465")%>%
                 transform(id=factor(id, levels=c("f_3453", "s_93", "f_2465", "f_1162", 
                                                  "s_46", "f_131", "s_98", "s_157"))),
               aes(x = ymd,y=-1, xend=ymd, yend=-0.8, colour = re_dis_type), 
               size=1, alpha=.6) +
    geom_line(data=dur8%>% 
                filter(id== "f_1162" | id == "s_157" |
                         id == "f_3453" | id == "s_98" |
                         id == "f_131" | id == "s_46" |
                         id=="s_93" | id == "f_2465") %>%
                transform(id=factor(id, levels=c("f_3453", "s_93", "f_2465", "f_1162", 
                                                 "s_46", "f_131", "s_98", "s_157"))) %>%
                mutate(period="1980-1989"), aes(ymd, g_geom, linetype=period), size =.8)+
  geom_line(aes(ymd, g_geom, linetype=period), size=0.8) +
  scale_colour_viridis_d(option="cividis")+ 
  facet_wrap(~id, nrow=2) + 
  scale_x_date(date_labels = "%m", date_breaks = "3 months", name="month") +
  scale_y_continuous(breaks=c(-0.5,0,0.5), limits=c(-1,1), 
                     name="normalised biweekly mean water table") +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        # axis.ticks.x = element_blank(),
        # axis.text.x = element_blank()
        legend.title = element_blank()) #+ scale_shape_discrete(guide="none") +
  


library(colormap)
scales::show_col(colormap(colormap=colormaps$inferno), labels=T)
scales::show_col(colormap(), labels=T)
dur1 <- left_join(dur1, dur_met1 %>% distinct(id, ymd, N, E, qpet_type))
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
                              "RWT" = "#fde725ff",
                              "DWT" = "#440154ff"),
                     breaks=c("RWT", "DWT", "high PET", "snowfall", "rain",
                                       "snowmelt")) +
  geom_hline(aes(yintercept=1.3))+geom_hline(aes(yintercept=-1.3))+
  facet_wrap(~id, nrow=2) + 
  scale_x_date(date_labels = "%m", date_breaks = "3 months", name="December-November") +
  scale_y_reverse(breaks=c(0.5,0,-0.5), limits=c(1.5,-1.5), 
                     name="normalised biweekly mean water table [m]") +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        # axis.ticks.x = element_blank(),
        # axis.text.x = element_blank(),
        # panel.grid.minor=element_blank(),
        axis.line.x = element_blank(), axis.text =element_text(size=12),
        panel.grid.major=element_blank(), panel.background = element_rect(fill="white",
                                                                        colour="white"),
        legend.title = element_blank()) + guides(size=5)




# Metrics prep ----
# blog: http://www.statsoft.com/Textbook/Time-Series-Analysis#analysis
# data from 'removing trends'
head(g8)
head(g1)
head(g)
diff <- g %>% filter(!is.na(raw_dep)) %>% 
  distinct(date, Tunnus, year, id, raw_dep) %>% group_by(Tunnus) %>% 
  mutate(stdev_rec = sd(raw_dep, na.rm =TRUE),
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
  group_by(Tunnus, season) %>% 
  mutate(stdev_8 = sd(raw_dep, na.rm = TRUE),
         mean_8 = median(raw_dep, na.rm = TRUE),
         min_8 = min(raw_dep, na.rm = TRUE),
         max_8 = max(raw_dep, na.rm = TRUE)) %>% 
  distinct(id, Tunnus, 
           stdev_8, mean_8, season, 
           min_8, max_8, mean_all, mean_rec
           # type,
           )

diff1 <- diff %>% filter(year <= 2010 & year >= 2001) %>% 
  group_by(Tunnus, season) %>% #before Tunnus and season and year
  mutate(stdev_1 = sd(raw_dep, na.rm = TRUE),
         mean_1 = median(raw_dep, na.rm = TRUE),
         min_1 = min(raw_dep, na.rm = TRUE),
         max_1 = max(raw_dep, na.rm = TRUE)) %>% 
  distinct(id, Tunnus, 
           stdev_1, mean_1, season, 
           min_1, max_1,  mean_all, mean_rec
           # type, 
           )

diff <- full_join(diff %>% distinct(id, Tunnus, 
                                    stdev_rec, mean_rec, season, mean_all), 
                  diff1) %>% 
  filter(!is.na(stdev_1) & !is.na(stdev_rec))
diff <- full_join(diff, diff8) %>% filter(!is.na(stdev_8))
diff <- full_join(diff, plot %>% # from other section, make plot
                    rename(lat = N,
                           lon = E) %>% 
                    group_by(id) %>%
                    mutate(lon = mean(lon)) %>% 
                    distinct(lon, lat,Tunnus, 
                             id)) #lon lat versions of N E, from "#make shapefiles/maps ----"
diff <- diff %>% 
  # mutate(group = lat)
  # mutate(group = ifelse(lat >= 62.5, 62.5, NA),
  #        group = ifelse(lat < 62.5 & lat >= 61.0, 61, group),
  #        group = ifelse(lat < 61.0 & lat >= 60.0, 60, group),
  #        group = ifelse(lat < 60.0 & lat >= 59.0, 59, group),
  #        group = ifelse(lat < 59.0 & lat >= 58.0, 58, group),
  #        group = ifelse(lat < 58.0 & lat >= 57.0, 57, group),
  #        group = ifelse(lat < 57.0, 55, group))
  # mutate(group = ifelse(lat < 57, "-57˚", NA),
  #        group = ifelse(lat >= 57, "57˚-60˚", group),
  #        group = ifelse(lat > 60, "60˚-63˚", group),
  #        group = ifelse(lat > 63, "63˚-65˚", group),
  #        group = ifelse(lat > 65, "65˚-", group))
  mutate(group = ifelse(lat < 59, "-59˚", NA),
         group = ifelse(lat >= 59, "59˚-63˚", group),
         group = ifelse(lat > 63, "63˚-", group))
  # mutate(group = ifelse(lon >= 27.5, 27.5, NA),
  #        group = ifelse(lon < 27.5 & lon >= 25.0, 25, group),
  #        group = ifelse(lon < 25.0 & lon >= 21.0, 21, group),
  #        group = ifelse(lon < 21.0 & lon >= 16.0, 16, group),
  #        group = ifelse(lon < 16.0, 14, group))
# Metrics plots ----
s <- ggplot(diff) + 
  aes((stdev_8/mean_rec), (stdev_1/mean_rec), group = group) +
        geom_point(aes(fill=as.factor(group), shape = season),shape =21, colour ="grey19",
                   alpha = .8, size = 2) +
        geom_abline(intercept = 0, slope = 1) +
  scale_fill_viridis_d(guides(name="Latitude"), direction=-1) +
  # scale_fill_manual(values=c("-59˚"= "#fde725ff",
  #                              "59˚-63˚" = "#8dd544ff",
  #                              "63˚-" = "#218f8dff"), guides(name="Latitude")) + 
  geom_abline(intercept = 0, slope = 1) + 
  scale_x_continuous(trans='log10', limits = c(0.005, 20)) +
  scale_y_continuous(trans='log10', limits = c(0.005, 20)) +
  # geom_hline(yintercept = 0) +
  theme(axis.text =element_text(size=12)) +
  facet_wrap(~season) + xlab("sd. depth P1")+
  ylab("sd. depth P2") 
m <- ggplot(diff) + 
  aes((mean_8/mean_rec), (mean_1/mean_rec), group = group) +
  geom_point(aes(fill=as.factor(group), shape = season),shape =21, colour ="grey19",
             alpha = .8, size = 2) +
  geom_abline(intercept = 0, slope = 1) + 
  scale_x_continuous(trans='log10', limits = c(0.01,5))+
  scale_y_continuous(trans='log10', limits = c(0.01,5))+
  # ylim(0,4) + xlim(0,4) +
  scale_fill_viridis_d(guides(name="Latitude"), direction=-1) +
  # scale_fill_manual(values=c("-59˚"= "#fde725ff",
  #                            "59˚-63˚" = "#8dd544ff",
  #                            "63˚-" = "#218f8dff"), guides(name="Latitude")) + 
  theme(axis.text =element_text(size=12)) + #theme_classic() + 
  facet_wrap(~season) + xlab("median depth P1")+
  ylab("median depth P2") 
min <- ggplot(diff) + 
  aes((min_8/mean_rec), (min_1)/mean_rec, group = group) +
  geom_point(aes(fill=as.factor(group), shape = season), shape =21, colour ="grey19",
             alpha = .8, size = 2) +
  geom_abline(intercept = 0, slope = 1) +  
  scale_x_continuous(trans='log10', limits = c(0.01,3))+
  scale_y_continuous(trans='log10', limits = c(0.01,3))+
  scale_fill_viridis_d(guides(name="Latitude"), direction=-1) +
  # scale_fill_manual(values=c("-59˚"= "#fde725ff",
  #                            "59˚-63˚" = "#8dd544ff",
  #                            "63˚-" = "#218f8dff"), guides(name="Latitude")) + 
  theme(axis.text =element_text(size=12)) +# theme_classic() + 
  facet_wrap(~season) + xlab("min. depth P1")+
  ylab("min. depth P2")
max <- ggplot(diff) + 
  aes((max_8)/mean_rec, (max_1)/mean_rec, group = group) +
  geom_point(aes(fill=as.factor(group) , shape = season), shape =21, colour ="grey19",
             alpha = .8, size = 2) +
  geom_abline(intercept = 0, slope = 1) + 
  scale_x_continuous(trans='log10', limits = c(.1,20))+
  scale_y_continuous(trans='log10', limits = c(.1,20))+ 
  scale_fill_viridis_d(guides(name="Latitude"), direction=-1) +
  # scale_fill_manual(values=c("-59˚"= "#fde725ff",
  #                            "59˚-63˚" = "#8dd544ff",
  #                            "63˚-" = "#218f8dff"), guides(name="Latitude")) + 
  # scale_fill_manual("Latitude",
  #                     values=c("-59˚" = "orange", "59˚-63˚" = "olivedrab3",
  #                              "63˚-"="darkgreen")) +
  theme(axis.text =element_text(size=12)) + #theme_classic() + 
  facet_wrap(~season) + xlab("max. depth P1")+
  ylab("max. depth P2") 


library(cowplot)
prow <- plot_grid(m + theme(legend.position = "none"), 
          s + theme(legend.position = "none"), 
          min  + theme(legend.position = "none"), 
          max + theme(legend.position = "none"), 
          align = "hv", axis = "lb", ncol = 2,
  rel_heights = c(1,1))
legend <- get_legend(m)
plot_grid(prow, legend, rel_widths = c(3, .5))


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

wilcox.test((diff %>% filter(season=="Mar-May"))$max_8, (diff %>% filter(season=="Mar-May"))$max_1)$p.value
wilcox.test((diff %>% filter(season=="Jun-Aug"))$max_8, (diff %>% filter(season=="Jun-Aug"))$max_1, paired=TRUE)#$p.value
wilcox.test((diff %>% filter(season=="Sep-Nov"))$max_8, (diff %>% filter(season=="Sep-Nov"))$max_1, paired=TRUE)#$p.value

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
  mutate(sd = ((median(stdev_8) - median(stdev_1))) %>% round(.,2),
         sd.p = wilcox.test(stdev_8, stdev_1, paired=TRUE)$p.value,
         median = ((median(mean_8) - median(mean_1))) %>% round(.,2),
         median.p = wilcox.test(mean_8, mean_1, paired=TRUE)$p.value,
         min = ((median(min_8) - median(min_1))) %>% round(.,2),
         min.p = wilcox.test(min_8, min_1, paired=TRUE)$p.value,
         max = ((median(max_8) - median(max_1))) %>% round(.,2),
         max.p = wilcox.test(max_8, max_1, paired=TRUE)$p.value) %>%
  group_by(group) %>% 
  mutate(sd.y = ((median(stdev_8) - median(stdev_1))) %>% round(.,2),
         sd.yp = wilcox.test(stdev_8, stdev_1, paired=TRUE)$p.value,
         median.y = ((median(mean_8) - median(mean_1))) %>% round(.,2),
         median.yp = wilcox.test(mean_8, mean_1, paired=TRUE)$p.value,
         min.y = ((median(min_8) - median(min_1))) %>% round(.,2),
         min.yp = wilcox.test(min_8, min_1, paired=TRUE)$p.value,
         max.y = ((median(max_8) - median(max_1))) %>% round(.,2),
         max.yp = wilcox.test(max_8, max_1, paired=TRUE)$p.value) %>% 
  distinct(season, sd, median, min, max,sd.p, median.p, min.p, max.p) #,
           #sd.y, median.y, min.y, max.y,sd.yp, median.yp, min.yp, max.yp)
test <- 0.001
sign %>% dplyr::select(-sd, -median, -min, -max) %>% 
  mutate(sd.p = sd.p < test, median.p = median.p < test, min.p = min.p < test, max.p = max.p  < test)

sign <- sign %>% 
  mutate(sd = ifelse(season=="Mar-May", paste(sd), paste(sd, "*", sep="")),
         sd = ifelse(season=="Dec-Feb" | season=="Sep-Nov", paste(sd, "**", sep=""), sd),
         median = ifelse(season=="Mar-May", paste(median, "*", sep=""), 
                                                  paste(median, "***", sep="")),
         min = ifelse(season=="Dec-Feb", paste(min), paste(min,  "**", sep="")),
         min = ifelse(season=="Mar-May" | season=="Sep-Nov", paste(min,  "*", sep=""), min),
         max = paste(max,  "***", sep=""))
sign <- sign %>% 
  mutate(sd.t = ifelse(sd.p > 0.05, paste(","), paste("*")),
         sd.t = ifelse(sd.p < 0.01, paste("**"), sd.t),
         sd.t = ifelse(sd.p < 0.00001, paste("***"), sd.t),
         median.t = ifelse(median.p > 0.05, paste(","), paste("*")),
         median.t = ifelse(median.p < 0.01, paste("**"), median.t),
         median.t = ifelse(median.p < 0.00001, paste("***"), median.t),
         min.t = ifelse(min.p > 0.05, paste(","), paste("*")),
         min.t = ifelse(min.p < 0.01, paste("**"), min.t),
         min.t = ifelse(min.p < 0.00001, paste("***"), min.t),
         max.t = ifelse(max.p > 0.05, paste(","), paste("*")),
         max.t = ifelse(max.p < 0.01, paste("**"), max.t),
         max.t = ifelse(max.p < 0.00001, paste("***"), max.t))

sign %>% dplyr::select(season, sd, median, min, max) %>% View()

library(grid); library(gridExtra)
library(gtable)
table <- sign %>% ungroup() %>% distinct(season, sd, mean,min,max) %>% 
  tableGrob(., rows=NULL, theme = ttheme_default(core = list(fg_params=list(hjust=1, x=0.95))))
grid.draw(gtable_add_grob(table, grobs = rectGrob(gp = gpar(fill = "white", lwd = 2)),
                          t = 1, l = 1))



# Staudinger et al 2014 drought duration plots ----
d.g <- g.df %>% filter((Station==9 & id==55) | ( id==37 & Station==34))
d.spi <- spi.ind %>% filter(id==55 | id==37) %>% ungroup() %>% mutate(Station = paste(ifelse(id==55, 9, 34))) %>%
  group_by(id, agg) %>% mutate(balance=ifelse(index<0, "y", "n"),
                          dry=ifelse(index<=-1.5 &
                                       (lead(index, 1) <=-1.5 | lag(index,1) <=-1.5), "y", "n"),
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
  group_by(id, agg, dry_year) %>% mutate(dry_dur=as.Date(dry_end) - as.Date(dry_start),
                                    dry_end=as.Date(dry_end), dry_start = as.Date(dry_start),
                                    dry_dat=as.Date(dry_dat))
  # mutate(balance = ifelse(index<lead(index,1), "drop", "rise"))

d.spei <- spei.ind %>% filter(id==55| id==37) %>% ungroup() %>% mutate(Station = paste(ifelse(id==55, 9, 34))) %>%
  group_by(id, agg) %>% 
  mutate(balance=ifelse(index<0, "y", "n"), dry=ifelse(index<=-1.5 &
                                            (lead(index, 1) <=-1.5 | lag(index,1) <=-1.5), "y", "n"),
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
  group_by(id, agg, dry_year) %>% mutate(dry_dur=as.Date(dry_end) - as.Date(dry_start),
                                         dry_end=as.Date(dry_end), dry_start = as.Date(dry_start),
                                         dry_dat=as.Date(dry_dat))
  # mutate(balance = ifelse(index<lead(index,1), "drop", "rise"))

d.g %>% #mutate(id=paste(id, Station)) %>% 
  mutate(dry_start = as.Date(dry_start),dry_end = as.Date(dry_end)) %>% 
  ggplot(.) + 
  # geom_curve(aes(x=dry_start, xend=dry_end, y=0,yend=0.001, group = dry_start, colour="sgi"),
  #            size=1.5, curvature = 1) +
  # geom_curve(data= d.spi %>% filter(id==37 & agg==10),
  #              aes(x=dry_start, xend=dry_end, y=0,yend=0.001, group = dry_start, colour="spi"),
  #            size=1, curvature = 1) +
  # geom_curve(data= d.spei %>% filter(id==37 & agg==10),
  #            aes(x=dry_start, xend=dry_end, y=0,yend=0.001, group = dry_start, colour="spei"),
  #            size=1, curvature = 1) +
  # # geom_curve(data= d.spi %>% filter(id==37 & agg==round(10*2,0)),
  # #            aes(x=dry_start, xend=dry_end, y=0,yend=0.001, group = dry_start, colour="spi"),
  # #            size=1, curvature = 1) +
  # # geom_curve(data= d.spi %>% filter(id==37 & agg==round(10/3,0)),
  # #            aes(x=dry_start, xend=dry_end, y=0,yend=0.001, group = dry_start, colour="spei"),
  # #            size=1, curvature = 1) +
  # geom_curve(data= d.spei %>% filter(id==55 & agg==6),
  #            aes(x=dry_start, xend=dry_end, y=0,yend=0.001, group = dry_start, colour="spei"),
  #            size=1, curvature = 1) +
  # geom_curve(data= d.spi %>% filter(id==55 & agg==5),
  #            aes(x=dry_start, xend=dry_end, y=0,yend=0.001, group = dry_start, colour="spi"),
  #            size=1, curvature = 1) +
  # # geom_curve(data= d.spei %>% filter(id==55 & agg==6*2),
  # #            aes(x=dry_start, xend=dry_end, y=0,yend=0.001, group = dry_start, colour="spi"),
  # #            size=1, curvature = 1) +
  # # geom_curve(data= d.spei %>% filter(id==55 & agg==round(5/3,0)),
  # #            aes(x=dry_start, xend=dry_end, y=0,yend=0.001, group = dry_start, colour="spei"),
  # #            size=1, curvature = 1) +
  
  # geom_ribbon(data=d.spi %>% group_by(id, date) %>% filter(index < 0.1) %>% 
  #               mutate(mini=min(index), maxi=max(index)),
  #                               aes(date, ymin=mini, ymax=0, group=id, 
  #                                   colour="all spi"), alpha=.3) +
  # geom_ribbon(data=d.spei %>% group_by(id, date) %>% filter(index < 0.1) %>% 
  #               mutate(mini=min(index)),
  #             aes(date, ymin=mini, ymax=0, group=id, 
  #                 colour="all spei"), alpha=.3) +
  geom_segment(data=d.spei %>% filter(id==55 & agg==6), 
               aes(x=date, xend=date, y=2, yend=2.5, group=id, colour=balance), size=1)+
  geom_segment(data=d.spi %>% filter(id==37 & agg==10), 
               aes(x=date, xend=date, y=2, yend=2.5, group=id, colour=balance), size=1)+
  geom_line(aes(date, sgi)) + geom_hline(yintercept=0) +
  xlim(as.Date("2001-01-15"), as.Date("2008-01-15"))+
  ylim(-3, 3)+
  facet_wrap(~id+Station,ncol=1) 
    scale_colour_manual(values=c("sgi"="black","spi"="salmon","spei" = "blue",
                                                "all spei"="pink",
                                                "all spi"="lightgreen")) + theme_classic()

# linear regression ----
# info: http://r-statistics.co/Linear-Regression.html
trend <- diff %>% filter(year>=1980 & year <=2010) %>%
    distinct(Tunnus, id, date, raw_dep, year, season, mean_all) %>%
  group_by(id, Tunnus, season, year) %>%  
  mutate(mean=mean(raw_dep, na.rm=TRUE)) %>% distinct(id, Tunnus, year, season, mean, mean_all)

library(reshape2)
library(broom)
trend <- trend %>%
    group_by(id, Tunnus, season, mean_all) %>% 
    do(model=tidy(lm(mean~year, data=.)),
       sum = summary(lm(mean~year, data=.)))
trend$p=0
trend$est=0
trend$resid=0
trend$r.sq = 0
for(i in 1:length(trend$Tunnus)){
    trend$p[i] = trend$model[[i]]$p.value[2]
    trend$est[i] = trend$model[[i]]$estimate[2]
    
    trend$resid[i] = trend$sum[[i]]$sigma
    trend$r.sq[i] = trend$sum[[i]]$r.squared
}

trend <- trend %>% mutate(sign=ifelse(p < 0.05, "p<0.05", "not significant"),
                 sign=ifelse(p < 0.01, "p<0.01", sign),
                 sign=ifelse(p < 0.001, "p<0.001", sign),
                 trend = ifelse(est > 0, "greater", "less"))
trend <- left_join(trend, plot)
ggmap(stamen) + 
    geom_point(data=trend, 
               aes(E, N, colour = trend, shape = sign, alpha=r.sq), size=2) + 
    scale_colour_manual(values=c("greater"="red", "less" = "blue")) + 
    facet_wrap(~season)


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
