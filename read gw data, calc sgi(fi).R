rm(list = ls())

sapply(list.files(pattern="[.]R$", path="rfuncs/", full.names=TRUE), source)
library(tidyverse); library(readxl); library(lubridate)

# read groundwater data fi ----

lsGW <- list.files(path = "R_finland_drought/input/groundwater_updated/", full.names = TRUE)
lsGWna <- list.files(path = "R_finland_drought/input/groundwater_updated/") %>% 
    gsub("\\..*","", .) %>% 
    gsub('[[:digit:]]+', '', .) %>% 
    trimws(., "left")

# still problems with Pertunma, Karijoki Rajamäki, Laihia Kylänpää, and more
# Some of the wells in Salla Vallovaara belong to an aquifer and have more detailed information,
# whereas some do not and lack these columns: I can't read these files because they have different
# numbers of columns. Try adding an ifelse statement in the function, e.g. if the column 
# doesn't exist fill it with NAs. For the other files, even where data is missing the columns are 
# still there. Add them manually?

source("R_finland_drought/rfuncs/read_gwdata_fin.R")
gw.df <- lapply(lsGW, function (lsGW) {
    return(tryCatch(read_gwdata_fin(lsGW), error=function(e) NULL))
}
) 


names(gw.df) <- lsGWna

# `Koordinaatit (ETRS-TM35FIN)`
locale(data_names ="sv")

# updated gw ----
gw.df.all <- gw.df[-which(sapply(gw.df, is.null))]
# source("R_finland_drought/rfuncs/qqnorm_dpl.R")
gw.df.all <- lapply(gw.df.all, mutate_if, is.numeric, as.character) %>% 
  bind_rows(., .id = "Stationname") %>%
    rename(
        Koordinaatit = "Koordinaatit (ETRS-TM35FIN)",
        Vedenkorkeus = "Vedenkorkeus [m]",
        material = "Aquifer material",
        area_km = "PValueen koko [km2]",
        elevation  = "Maanpinta [m(N2000)]"
        ) %>%
    separate(Koordinaatit, c("N", "E"), sep = ",") %>%
    mutate(N = as.numeric(N), E = as.numeric(E), 
           month = month(Aika), 
           date = as.Date(Aika)) %>% select(-Aika) %>%
    group_by(Stationname, 
             Tunnus) %>% mutate(
               N = first(N),
               E = first(E),
               material = first(material),
               area_km = first(area_km),
               elevation = first(elevation)
             ) %>%
  group_by(Stationname, 
           Tunnus, N, E, material, area_km, elevation) %>%
    nest(.key = "raw") #%>%
    # mutate(
    #     mean_mon = raw %>% modify(tq_transmute,
    #         select     = Vedenkorkeus,
    #         mutate_fun = apply.monthly,
    #         FUN        = mean,
    #         na.rm      = TRUE,
    #         col_rename = "mean_Vedenkorkeus") ,
    # 
    #     sgi = mean_mon %>%
    #        map("mean_Vedenkorkeus")) #, %>% map(qqnorm_dpl),
# saveRDS(gw.df.all, "R_finland_drought/output/process/fin_gw_sgi.rds")


library(tidyr)
# tidyr complete function docs: https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-exploratory-79f2a321e6b5
new <- gw.df.all %>% unnest() %>% select(Stationname, Tunnus, N, E, date, Vedenkorkeus) %>% 
  mutate(date2 = as.Date(date))  %>%
  group_by(Stationname, Tunnus, N, E) %>%
  complete(date2 = seq.Date(min(date), max(date), by="day")) 
new <- full_join(new, gw.df.all %>% unnest()) %>% mutate(month = month(date2))%>% 
  group_by(Stationname, Tunnus, N, E) %>% mutate( elevation = first(elevation)) %>%
  ungroup()
saveRDS(new, "R_finland_drought/output/process/fin_gw_raw.rds")
    
# extract coordinates
gw.df.all <- gw.df.all %>% select(Tunnus, raw) %>% unnest %>% 
  distinct(Tunnus, N, E, elevation, material, area_km) %>%
  filter(!is.na(N), !is.na(E), !is.na(elevation))
  # mutate(
  #       N = raw %>% map(filter, !is.na(N)) %>% map(distinct, N) %>% map_dbl("N"),
  #       E = raw %>% map(filter, !is.na(E)) %>% map(distinct, E) %>% map_dbl("E"),
  #       elevation = raw %>% 
  #         map(filter, !is.na(elevation)) %>% 
  #         map(distinct, elevation) %>% map_dbl("elevation"),
  #       material = raw %>% 
  #         map(filter, !is.na(material)) %>% 
  #         map(distinct, material) %>% map_chr("material"),
  #       area_km = raw %>% 
  #         map(filter, !is.na(area_km)) %>% 
  #         map(distinct, area_km) %>% map_dbl("area_km"))
  #   

saveRDS(gw.df.all, "R_finland_drought/output/process/gwts_chars.rds")

# old code ----
Date = as.Date(strptime(paste(YEAR = lubridate::year(gw.df.all$Aika), MONTH = gw.df.all$MONTH, "15", sep = "-"), 
                        format = "%Y-%b-%d"))

agg = gw.df.all %>%
    mutate(YEAR = lubridate::year(Aika)) %>%
    group_by(Stationname, Tunnus, YEAR, MONTH) %>%
    summarise(
        # Stationname = first(Stationname),
        # Tunnus = first(Tunnus),
        Date = first(Date),
        N = first(N),
        E = first(E),
        mean_mon = mean(Vedenkorkeus)#,
        #mean_mon_zeromean = Vedenkorkeus - mean(Vedenkorkeus, na.rm = T)
    ) %>%
    ungroup()

fin_sgi = agg %>%
    group_by(Stationname, Tunnus, MONTH) %>%
    dplyr::mutate(
        SGIm = qqnorm(mean_mon)$x
    )

fin_sgi %>% ungroup()
write_rds(fin_sgi, "output/process/fin_gw_sgi.rds")



g.df.all <- readRDS("R_finland_drought/output/process/gwts.rds")

# BC OW data ----
library(readxl); library(magrittr); library(dplyr)

data <- read.csv("Q:/Data/SFU_April/OW_Michelle/OW002.csv") %>% mutate(id="ow2") %>% 
  full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW008.csv") %>% 
              mutate(id="ow8"))%>% 
  full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW255.csv") %>% 
              mutate(id="ow255"))%>% 
  # full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW259.csv") %>%
  #             mutate(id="ow259"))%>%
  # full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW272.csv") %>%
  #             mutate(id="ow272"))%>%
  full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW275.csv") %>% 
              mutate(id="ow275"))%>% 
  # full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW292.csv") %>% 
  #             mutate(id="ow292"))%>% 
  full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW299.csv") %>% 
              mutate(id="ow299"))%>% 
  full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW301.csv") %>% 
              mutate(id="ow301"))%>% 
  # full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW349.csv") %>% 
  #             mutate(id="ow349"))%>% 
  # full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW352.csv") %>% 
  #             mutate(id="ow352"))%>% 
  # full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW353.csv") %>% 
  #             mutate(id="ow353"))%>% #domestic water use, 175 gallons per minute
  full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW354.csv") %>% 
              mutate(id="ow354"))%>% 
  full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW357.csv") %>% 
              mutate(id="ow357"))%>% 
  # full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW359.csv") %>% 
  #             mutate(id="ow359"))%>% # negative trend and data gaps
  full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW360.csv") %>% 
              mutate(id="ow360"))#%>% 
  # full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW361.csv") %>% 
  #             mutate(id="ow361"))

data <- data %>% 
  # mutate(Timestamp..UTC. = ifelse(is.na(Timestamp..UTC.) & 
  #                                                  !is.na(Timestamp..UTC.08.00.), 
  #                                                as.POSIXct(Timestamp..UTC.08.00., format="%Y-%m-%d %H:%M"), 
  #                                                as.POSIXct(Timestamp..UTC., format="%Y-%m-%d %H:%M"))) %>% 
  mutate(time = as.POSIXct(Timestamp..UTC., format="%Y-%m-%d %H:%M")) %>% 
  # filter(year(time) >= 2000) %>% 
  distinct(id, time, Value..Metres.)
data <- data  %>% rename(h = Value..Metres.) %>% distinct(time, h, id)
data <- data %>% group_by(id) %>% mutate(season=ifelse(month(time) >= 10 | 
                                        month(time) < 4,
                                      "winter", "summer")) %>% ungroup()
sgi <- data %>% mutate(month = month(time), year=year(time)) %>% 
  group_by(id, year, month) %>% mutate(m = mean(h, na.rm=TRUE)*-1) %>% ungroup() %>% 
  distinct(id, year, month, season, m) %>% 
  group_by(id, month) %>% mutate(sgi = qqnorm(m)$x) %>% ungroup() %>% 
  mutate(date=as.Date(paste(year, month, "15", sep="-")))
data <- data %>% mutate(date=as.Date(time, "%Y-%m-%d", tz = "UTC")) %>% 
  group_by(id, date) %>% mutate(daily = mean(h, na.rm=TRUE)) %>% ungroup() %>% 
  distinct(id, date, season, daily) %>% group_by(id) %>% 
  complete(date = seq.Date(min(date, na.rm=TRUE), max(date,na.rm=TRUE), by=1))

aa <- read.csv("Q:/Data/SFU_April/OW_Michelle/pcds_data(1)/EC/1100030_aa1.csv")
aa <- aa %>% #filter(year(time) >= 2000) %>%
  rename(p = ONE_DAY_PRECIPITATION) %>% distinct(time, p, MIN_TEMP, MAX_TEMP)
aa <- aa %>% mutate(time = as.POSIXct(time))
aa <- aa %>% mutate(season=ifelse(month(time) >= 10 | 
                                    month(time) < 4,
                                  "winter", "summer"))

source("rfuncs/SIcalc.R")

aa.ind <- aa %>% na.omit(cde) %>% mutate(id="aa", year = year(time),
                                         month = month(time)) %>% 
  group_by(id, year, month) %>% summarise(sump=sum(p)) %>%
  group_by(id) %>% nest() %>%
  mutate(spi = data %>% map("sump") %>% map(PQ_index_mutate, 1:48))
p.time <- aa.ind$data[[1]] %>% mutate(time = as.Date(paste(year, month, "15", sep="-"), 
                                                     format="%Y-%m-%d")) %>% 
  select(time,year,month) %>% ungroup() %>% 
  mutate(month_past = 1:length(time)) %>% filter(!is.na(time))
aa.ind <- aa.ind %>% dplyr::select(-data) %>% unnest() %>% #select(-agg1) %>% rename(sti = index1) %>%
  group_by(id, agg) %>% 
  mutate(month_past = 1:length(index)) %>% 
  left_join(., p.time, by=c("month_past"))

# calculate autocorrelation of the SGI time series
acorr <- sgi %>% group_by(id) %>% select(id, sgi) %>% filter(!is.na(sgi)) %>% nest()
for(i in 1:length(acorr$id)) {
  print(i)
  acorr$acorr[[i]]$acorr = acf(acorr$data[[i]], lag.max = 48, plot=FALSE)$acf
  acorr$acorr[[i]]$sign = abs(acorr$acorr[[i]]$acorr)[-1] > 
    qnorm(1-0.05/2)/sqrt(length(acorr$data[[i]]$sgi))
  acorr$acorr[[i]]$conf = qnorm(1-0.05/2)/sqrt(length(acorr$data[[i]]$sgi))
}
a <- acorr %>% select(id)
for(i in 1:length(acorr$id)){
  a$conf[[i]] = acorr$acorr[[i]]$conf
  # a$sign[[i]] = acorr$acorr[[i]]$sign
  a$acorr[[i]] = acorr$acorr[[i]]$acorr[1:length(acorr$acorr[[i]]$acorr)]
  a$lag[[i]] = (1:length(acorr$acorr[[1]]$acorr) -1 )
}
acorr <- a %>%unnest() 


source("rfuncs/cor_indices.R")
a = as.Date("2000-01-01")
b = as.Date("2006-01-01")

indices <- sgi %>% 
  merge(., aa.ind %>% rename(date=time) %>% ungroup() %>%
          select(-id, -month_past) %>% 
          group_by(agg)) %>%
  group_by(id, agg) %>% arrange(id, agg, date)
corr.index <- indices %>% 
  spearcorr.pval(data=., y1_start=a, y1_end = b) %>% #, dofilter=1) %>%
  ungroup() %>% group_by(id) %>% 
  mutate(maxcorr=ifelse(corr==max(corr), agg, NA), agg = as.numeric(agg)) %>% 
  group_by(id, tresh) %>% arrange(id, agg)


##probably need to write a for loop or an if-statement
z <- #corr.index %>% ungroup () %>% select(-tresh, -pval) %>% group_by(id) %>% 
  # full_join(acorr %>% filter(acorr!=1),.) %>% 
  acorr %>% filter(acorr!=1) %>% 
  mutate(groups = 0, groups = ifelse(acorr >= conf, groups+1, groups)) %>%
  distinct(id, conf, acorr, lag, groups) %>% 
  filter(acorr >= conf) %>% group_by(id) %>% nest
 # make function of for loop I can put in dplyr mutate function
for(i in 1:length(z$id)){
  for(j in 2:length(z$data[[i]]$groups)){
    if((z$data[[i]]$lag[j] - z$data[[i]]$lag[j-1])>1){
      z$data[[i]]$groups[j] = z$data[[i]]$groups[j-1] + 1
    } else if(z$data[[i]]$groups[j-1] >= 1){
      z$data[[i]]$groups[j] = z$data[[i]]$groups[j-1]
    }
  }
}
# now get mean agg month per group -- GROUPS ARE PER SIGNFIFICANT AUTOCORR OR CROSSCORR GROUP
# z  <- z %>% unnest() %>% group_by(id, groups) %>%
#   mutate(medagg = round(median(agg),0),
#          medcorr = ifelse(corr==max(corr), agg, NA))
# corr.index <- corr.index %>% left_join(., z) %>%
#   ungroup() %>% group_by(id, groups) %>% 
#   add_tally(acorr >= conf) #%>%
  # mutate(bold = ifelse(corr<0.4 & corr > -0.4, NA, "+/- 0.4")) %>%
  # add_tally(bold=="+/- 0.4")
# BLOOMFIELDS ARTICLE IS FOR SIGNIFICANT AUTOCORR LENGTH AGAINST MAX CROSSCORR
z <- z %>% unnest %>% group_by(id, groups) %>% 
  filter(lag ==max(lag))
corr.index %>% ungroup %>% group_by(id) %>% 
  filter(corr==max(corr)) %>% left_join(., z) %>% 
  #group_by(id, groups) %>% filter(acorr >= conf & corr==max(corr) &
                                                 # groups==1) %>% 
  group_by(id) %>% #filter(groups==1) %>%
  mutate(agg = as.numeric(agg), auto = ifelse(acorr >= conf &
                                                acorr >0, "sign", "nosign")) %>% 
  ggplot(.) + 
  # geom_point(aes(medcorr, corr, colour=id, shape=as.factor(groups)),size=3) +
  geom_point(aes(agg, lag, colour=id, shape=as.factor(groups)), 
             size=3) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid = element_line(colour="lightgrey", linetype=2)) +
  # ylim(-1,1)
  ylim(0,50)


agg.groups <- corr.index %>% arrange(id, agg) %>% group_by(id, tresh) %>% 
  mutate(dif = ifelse(agg==1, NA, diff(agg))) %>% filter(dif > 1) %>% ungroup %>% distinct(id)


## BC data plots ----
a <- sgi %>% filter(id=="ow357") %>% 
  ggplot(.) + 
  scale_x_date(limits=c(as.Date("1945-01-01"),as.Date("2020-01-01"))) + 
  # geom_vline(aes(xintercept=date, colour=season, group=id), 
  #            alpha=.05, size=2)+ 
  geom_line(aes(date, sgi, group=id)) + geom_hline(yintercept=0) + 
  scale_colour_manual(values=c("winter" = "navy",
                               "summer" = "red")) +
  facet_wrap(~id, ncol=1)
a <- sgi %>% #filter(year>1995)%>%
  ggplot(.) +
  geom_tile(aes(date, id, fill=sgi))  + 
  scale_fill_viridis_c(option="magma", direction=-1) +
  scale_x_date(date_breaks="1 year", limits=c(as.Date("1945-01-01"),as.Date("2020-01-01"))) + 
  theme(axis.text.x = element_text(angle = 90))
  
y <- data %>% filter(id=="ow357") %>%
  mutate(time=as.Date(time, "%Y-%m-%d")) %>%
  ggplot(.) + 
  # geom_segment(aes(x=time, xend=time,yend=15.5, y=15, 
  #              colour=season))+
  aes(time, h, group = id) + geom_line() +
  scale_x_date(limits=c(as.Date("1945-01-01"),as.Date("2020-01-01"))) + 
  scale_y_continuous(trans="reverse") + 
  scale_colour_manual(values=c("winter" = "lightblue",
                               "summer" = "salmon")) + 
  facet_wrap(~id, ncol=1, scale="free_y") + 
  theme(legend.position="none")
library(cowplot)
plot_grid(y,a, ncol=2) # all ids
plot_grid(x, a, y, ncol=1, align="hv") # comparing SIs and data, one id
plot_grid(x, a, ncol=1, align="hv", rel_heights = c(0.1,1))




x <- aa.ind %>% filter(agg==23) %>% mutate(season=ifelse(month(time) >= 10 | 
                           month(time) < 4,
                         "winter", "summer"))%>% 
  ggplot(.) + 
  # geom_vline(aes(xintercept=time, colour=season),
                         # size=3, alpha=.04) + 
  scale_x_date(limits=c(as.Date("1945-01-01"),as.Date("2020-01-01"))) + 
  geom_line(aes(time, index)) + geom_hline(yintercept=0) + 
  scale_colour_manual(values=c("winter" = "navy",
                               "summer" = "red")) + facet_wrap(~id)

d <- aa.ind %>% ungroup() %>% mutate(agg=as.numeric(agg), date=time) %>% ggplot(.) +
  scale_fill_viridis_c(option="magma", direction=-1) +
  scale_x_date(date_breaks="1 year", limits=c(as.Date("1945-01-01"),as.Date("2020-01-01"))) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  geom_tile(aes(date, agg, fill=index)) + facet_wrap(~id, ncol=1)

plot_grid(d + theme(axis.text.x = element_blank()), a, ncol=1, align="hv")



corr.index %>% arrange(id) %>% ungroup() %>% group_by(id) %>%
  mutate(agg=as.numeric(agg), tresh=(acorr>=conf)) %>% 
  arrange(agg) %>% ggplot(.) + aes(agg,id, group=id) +
  geom_tile(aes(fill = corr)) +
  geom_point(aes(colour=tresh))+
  geom_tile(data=. %>% filter(corr==max(corr)), colour="black", fill=NA) +
  scale_fill_viridis_c(option="magma", limits=c(-1,1)) +
  scale_colour_manual(na.value = NA, values=c("TRUE"="black", "FALSE"=NA))+
  ggtitle("SI")  
  theme(axis.text.x=element_text(angle=90))


library(cowplot)
plot_grid(z,x,y, ncol = 1, align="v")



# map BC data ----
library(ggmap)
bbox = as.numeric(c(49, 49.25, -123, -122))  #all of SCR: c(48, 51, -125, -121)
names(bbox) <- c('bottom', 'top', 'left','right')
source("rfuncs/get_stamenmapPNG.R")
stamen <- get_stamenmapPNG(bbox, zoom = 9, maptype = "toner-lite")

library(sp)
geo <- read.csv("Q:/Data/SFU_April/metadata.csv") %>% filter(Area=="SCR") %>% 
  distinct(Area, OW_nr, OW_depth_m, Lat, Lon) %>% mutate(id=paste("ow", OW_nr, sep="")) %>% 
  filter(id %in% data$id) %>%
  mutate(bold=ifelse(id=="ow2" | id=="ow357" |id=="ow301" |
                       id=="ow299" |id=="ow275",
                     "blue", "red"))
coordinates(geo) = ~Lon+Lat
proj4string(geo) = CRS("+proj=longlat + datum=WGS84")
library(raster)
raster::shapefile(geo, "BC_OW_selection.shp", overwrite=TRUE)
geo = as.data.frame(geo) 

ggmap(stamen) + geom_point(data=geo, 
                           aes(Lon, Lat, shape=bold, colour=id), size=4)
  scale_colour_viridis_d()
  theme(legend.position='none')
