# 2021-03-05
#
# Take data from papers 1 and 2, perform drougth analysis and check the temporal consistency of the
# response to drought in different hydrogeological settings. 
# In this document, data from different parts of the worls are combined to do the analysis for
# the paper on hydraulic memory effects in different northern aquifers in the propagation of 
# anomalies, from meteorology to groundwater
#
#

rm(list = ls())
library(magrittr); library(purrr); library(ggplot2); library(dplyr)
library(sp);library(lubridate); library(ggmap); library(tidyr); library(cowplot)
setwd("Q:/Projects/Drought Scandinavia/Michelle/Nordic_Drought/")


# background maps ----

source("rfuncs/get_stamenmapPNG.R")

bbox = as.numeric(c(10, 57, 32, 70)) # Sweden: c(11,55,24,69)) # Sweden and Finland: c(11, 55, 32, 70))
names(bbox) <- c('left','bottom','right','top')
stamen_fenn <- get_stamenmapPNG(bbox, zoom = 6, maptype = "terrain-background", color="bw",
                                force = TRUE) 

bbox = as.numeric(c(-123.30, 49, -121.80, 49.50)) # Fraser valley, zoom 10
names(bbox) <- c('left','bottom','right','top')
stamen_bc <- get_stamenmapPNG(bbox, zoom = 10, maptype = "terrain-background", color="color",
                              force = TRUE)


bbox = as.numeric(c(-125, 47, 33, 70.5)) # world
names(bbox) <- c('left','bottom','right','top')
stamen_world <- get_stamenmapPNG(bbox, zoom = 3, maptype = "toner-background", color="color",
                              force = TRUE)
ggmap(stamen_world, darken = c(0.6, "white")) + 
  geom_text(aes(x=-122, y=53, label="BC"), size=3) +
  geom_text(aes(x=-100, y=59, label="Canada"), size=4) +
  geom_text(aes(x=16, y=60, label="Swe."), size=4) +
  geom_text(aes(x=27, y=63, label="Fin."), size=4) +
  
  geom_rect(aes(xmin=49, xmax=49.50,ymin=-122,ymax=-123), colour="red", fill="white")

rm(bbox)

plot_grid(ggmap(stamen_bc),#, darken = c(0.6, "white")), 
          ggmap(stamen_fenn)) #, darken = c(0.6, "white")))#,
          # ncol = 1, align="hv", rel_widths = c(3,1), rel_heights = c(1,3))
###
# calculate distance to stream ----
# soure: https://stackoverflow.com/questions/42237021/finding-the-nearest-distance-between-two-spatialpointsdataframes-using-gdistance
# Below example with line features that doesn't work
# source: https://gis.stackexchange.com/questions/310489/calculating-euclidian-distance-in-r-between-lines-and-points
# https://stackoverflow.com/questions/37333747/for-each-point-in-one-data-set-calculate-distance-to-nearest-point-in-second-da
library(rgdal);library(sf);library(rgeos)

swe <- readOGR("C:/Users/xnygmi/Desktop/data/SWE/data/SMHI/flodeslinjer_vd_l_2016_3", 
                   "vd_l_2016_3")
fin <- readOGR("C:/Users/xnygmi/Desktop/data/FI/uomaverkosto, kanalnätet", 
                   "Uoma10")

gstat <- readRDS("output/process/g_raw2.rds") %>% 
  distinct(Tunnus, Station, id, N, E, country)
plots <- gstat %>% filter(country=="swe") %>% distinct(Tunnus, Station, id, N, E)
coordinates(plots) = ~E+N
proj4string(plots) = CRS("+init=epsg:3006")
plots <- spTransform(plots, " +proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
sp.g_s <- SpatialPointsDataFrame(plots, 
                                 data=gstat %>% filter(country=="swe") %>% 
                                   distinct(Tunnus, Station, id), 
                       proj4string = CRS(" +proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"))

dist.s <- gDistance(swe, sp.g_s, byid=TRUE)
min_dist.s <- apply(dist.s, 1, min)
library(Rfast)
min_dist2.s <- apply(dist.s, MARGIN=1, k=2, descending=F, FUN=Rfast::nth)

sp.g_s@data$dist_stream<- min_dist.s
sp.g_s@data$stream <- as.vector(apply(dist.s, 1, which.min))
sp.g_s@data$dist_stream2<- min_dist2.s



plotf <- gstat %>% filter(country=="fi") %>% distinct(Tunnus, Station, id, N, E)
coordinates(plotf) = ~E+N
proj4string(plotf) = CRS("+init=epsg:3067")
plotf <- spTransform(plotf, "+proj=utm +zone=35 +ellps=GRS80 +units=m +no_defs")
sp.g_f <- SpatialPointsDataFrame(plotf, 
                                 data=gstat %>% filter(country=="fi") %>% 
                                   distinct(Tunnus, Station, id), 
                                 proj4string = CRS("+proj=utm +zone=35 +ellps=GRS80 +units=m +no_defs"))
dist.f <- gDistance(fin, sp.g_f, byid=TRUE)
min_dist.f <- apply(dist.f, 1, min)
library(Rfast)
min_dist2.f <- apply(dist.f, MARGIN=1, k=2, descending=F, FUN=Rfast::nth)

sp.g_f@data$dist_stream<- min_dist.f
sp.g_f@data$stream <- as.vector(apply(dist.f, 1, which.min))
sp.g_f@data$dist_stream2<- min_dist2.f

g.s <- sp.g_s %>% as.data.frame()
g.f <- sp.g_f %>% as.data.frame()

g.stream <- g.s %>% distinct(Tunnus, Station, id, dist_stream, dist_stream2) %>%
  full_join(., g.f %>% distinct(Tunnus, Station, id, dist_stream, dist_stream2))
saveRDS(g.stream, "output/process/OWs_stream1_stream2_dists.rds")

library(sf)

swe_st <- st_read("C:/Users/xnygmi/Desktop/data/SWE/data/SMHI/flodeslinjer_vd_l_2016_3/vd_l_2016_3.shp")
fin_st <- st_read("C:/Users/xnygmi/Desktop/data/FI/uomaverkosto, kanalnätet/Uoma10.shp")

st.g_s <- st_as_sf(x=sp.g_s, crs=3035)
st.g_f <- st_as_sf(x=sp.g_f, crs=3035)

swe_st <- st_transform(swe_st, crs = 3035)
fin_st <- st_transform(fin_st, crs = 3035)
# streams <- st_union(swe_st, fin_st)

dist.s <- st_distance(swe_st, st.g_s)
dist.f <- st_distance(fin_st, st.g_f)

# get land use data ----

library(sf)
landuse <- st_read("C:/Users/xnygmi/Desktop/data/world/CORINE land use/u2018_clc2018_v2020_20u1_geoPackage/u2018_clc2018_v2020_20u1_geoPackage/DATA/U2018_CLC2018_V2020_20u1.gpkg")

# create masks 300 m and 3 km around wells, cut out the land use data with this ----
# calculate % land use, water/lakes, correlate
gstat <- readRDS("output/process/g_raw2.rds") %>% 
  distinct(Tunnus, Station, id, N, E, country)
plots <- gstat %>% filter(country=="swe") %>% distinct(Tunnus, Station, id, N, E)
coordinates(plots) = ~E+N
proj4string(plots) = CRS("+init=epsg:3006")
plots <- spTransform(plots, "+init=epsg:3035")
sp.g_s <- SpatialPointsDataFrame(plots, 
                                 data=gstat %>% filter(country=="swe") %>% 
                                   distinct(Tunnus, Station, id), 
                                 proj4string = CRS("+init=epsg:3035"))

plotf <- gstat %>% filter(country=="fi") %>% distinct(Tunnus, Station, id, N, E)
coordinates(plotf) = ~E+N
proj4string(plotf) = CRS("+init=epsg:3067")
plotf <- spTransform(plotf, "+init=epsg:3035")
sp.g_f <- SpatialPointsDataFrame(plotf, 
                                 data=gstat %>% filter(country=="fi") %>% 
                                   distinct(Tunnus, Station, id), 
                                 proj4string = CRS("+init=epsg:3035"))
st.g <- rbind(sp.g_s, sp.g_f) %>% st_as_sf(x=., crs=3035)


# r300 <- st_buffer(st.g, dist = 300)
# r3k <- st_buffer(st.g, dist = 3000)

# lu_r3k <- st_intersection(landuse, r3k)

# st_write(lu_r3k, "lu_r3k.shp", driver="ESRI Shapefile")
lu_r3k <- st_read("lu_r3k.shp")

lu_r3k$area <- st_area(lu_r3k)
lu_r3k$area_percent <- lu_r3k$area/(pi * 3000^2)
plot((lu_r3k %>% filter(id_1=="s_2" | id_1=="s_1"))["area_percent"])


lu_r300 <- st_intersection(lu_r3k, r300)
lu_r300$area <- st_area(lu_r300)
lu_r300$area_percent <- lu_r300$area/(pi * 300^2)
plot((lu_r300 %>% filter(id_1=="s_2" | id_1=="s_1"))["area_percent"])
lu_r300 <- st_write(lu_r300, "lu_r300.shp", driver="ESRI Shapefile")
lu_r300 <- st_read("lu_r300.shp")

plot(st_geometry(lu_r3k %>% filter(id=="s_2" | id=="s_1")), col=lu_r3k$Code_18)
plot(st_geometry(st.g %>%filter(id=="s_1" | id=="s_2")),  pch = 16, col = 'black', cex=3, add=TRUE)
plot(st_geometry(lu_r300 %>% filter(id=="s_2" | id=="s_1")), col=lu_r300$Code_18, alpha=0.6, add=TRUE)

lu_legend <- read.csv("C:/Users/xnygmi/Desktop/data/world/CORINE land use/u2018_clc2018_v2020_20u1_geoPackage/u2018_clc2018_v2020_20u1_geoPackage/Legend/CLC_legend.csv",
                      sep=";")
lu_density <- lu_r3k %>% as.data.frame() %>% distinct(Code_18, Tunnus, Station, id_1, area_percent) %>%
  rename(CLC_CODE=Code_18)
lu_density <- left_join(lu_density, lu_legend %>% distinct(LABEL3, CLC_CODE) %>% 
            mutate(CLC_CODE = as.factor(CLC_CODE)))

# saveRDS(lu_density, "lu_density_r300.rds")

#distance to lake
lu_density %>% filter(grepl("Water", LABEL3))
lu_r3k$clc_code = as.character(lu_r3k$Code_18)
water_r3k<- lu_r3k %>% left_join(., lu_legend %>% 
                   mutate(clc_code = as.character(CLC_CODE),
                          label = as.character(LABEL3)) %>% 
                     distinct(label, clc_code), by="clc_code") %>% 
  filter(TRUE==grepl("Water", label))
lakes <- water_r3k %>% filter(label=="Water bodies")
rivers <- water_r3k %>% filter(label=="Water courses")

dist <- st_distance(rivers, st.g)
min_dist <- apply(dist, 2, min)
library(Rfast)
min_dist2 <- apply(dist, MARGIN=2, k=2, descending=F, FUN=Rfast::nth)
hist(min_dist)

st.g$dist_lake1 <- min_dist
st.g$dist_lake2 <- min_dist2
dist_lakes <- st.g %>% as.data.frame() %>% distinct(id, Tunnus, Station, dist_lake1, dist_lake2)
# saveRDS(dist_lakes, "fenn_dist_lakes_r300.rds")
saveRDS(dist_lakes, "fenn_dist_wbodies_r3k.rds")
saveRDS(dist_lakes %>% rename(dist_river1=dist_lake1,
                              dist_river2 =dist_lake2), "fenn_dist_wcourses_r3k.rds")
# clip stream data
sl <- st_as_sf(swe); fl <- st_as_sf(fin)
sl <- sl %>% distinct(VDVattenId) %>% rename(id=VDVattenId)
fl <- fl %>% distinct(UomaNro, Shape_len, Korkeus_m, ValuYlaPa_) %>%
                   rename(id = UomaNro, length=Shape_len, height=Korkeus_m)
sl <- st_transform(sl, crs=3035)
sl_r300 <- st_intersection(sl, r300)
fl <- st_transform(fl, crs=3035)
fl_r300 <- st_intersection(fl, r300)


plot(st_geometry(sl_r300 %>% filter(id=="s_2" | id=="s_1")))
plot(st_geometry(fl_r300 %>% filter(id=="f_1352")))

sl_r300$stream_l <- st_length(sl_r300)
sl_r300$stream_density <- sl_r300$stream_l/(300^2 * pi)
  
fl_r300$stream_l <- st_length(fl_r300)
fl_r300$stream_density <- fl_r300$stream_l/(300^2 * pi)

fin_stream_density <- fl_r300 %>% as.data.frame() %>% distinct(Tunnus, Station, id, stream_l, stream_density)
swe_stream_density <- sl_r300 %>% as.data.frame() %>% distinct(Tunnus, Station, id, stream_l, stream_density)

stream_density <- full_join(fin_stream_density, swe_stream_density)
saveRDS(stream_density, "fenn_stream_dens_r300.rds")


# BC data download ----
library(readxl); library(magrittr); library(dplyr)

# data_bc <- read.csv("Q:/Data/SFU_April/OW_Michelle/OW002.csv") %>% mutate(id="ow2") %>% 
#   full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW008.csv") %>% 
#               mutate(id="ow8"))%>% 
#   full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW255.csv") %>% 
#               mutate(id="ow255"))%>% 
#   # full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW259.csv") %>%
#   #             mutate(id="ow259"))%>%
#   # full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW272.csv") %>%
#   #             mutate(id="ow272"))%>%
#   full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW275.csv") %>% 
#               mutate(id="ow275"))%>% 
#   # full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW292.csv") %>% 
#   #             mutate(id="ow292"))%>% 
#   full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW299.csv") %>% 
#               mutate(id="ow299"))%>% 
#   full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW301.csv") %>% 
#               mutate(id="ow301"))%>% 
#   # full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW349.csv") %>% 
#   #             mutate(id="ow349"))%>% 
#   # full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW352.csv") %>% 
#   #             mutate(id="ow352"))%>% 
#   # full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW353.csv") %>% 
#   #             mutate(id="ow353"))%>% #domestic water use, 175 gallons per minute
#   full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW354.csv") %>% 
#               mutate(id="ow354"))%>% 
#   full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW357.csv") %>% 
#               mutate(id="ow357"))%>% 
#   # full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW359.csv") %>% 
#   #             mutate(id="ow359"))%>% # negative trend and data gaps
#   full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW360.csv") %>% 
#               mutate(id="ow360"))#%>% 
# # full_join(., read.csv("Q:/Data/SFU_April/OW_Michelle/OW361.csv") %>% 
# #             mutate(id="ow361"))

file_list <- list.files("Q:/Data/SFU_April/OW_Michelle/updated/", pattern="*.csv", full.names=TRUE)
data_bc <- lapply(file_list, read.csv, sep=',')
names <- #list.files(
  file_list %>% gsub("\\..*","", .) %>% 
                      gsub('..../*', '', .)#)
names(data_bc) <- names
data_bc <- data_bc %>% bind_rows(., .id="id")

data_bc <- data_bc %>% 
  dplyr::mutate(time = ifelse(is.na(Timestamp..UTC.) & !is.na(Timestamp..UTC.08.00.),
    Timestamp..UTC.08.00., Timestamp..UTC.)) %>% 
  mutate(time = as.POSIXct(time, format="%Y-%m-%d %H:%M")) %>%
  filter(year(time) >= 2000) 
data_bc <- data_bc  %>% dplyr::rename(h = Value..m.) %>% distinct(time, h, id)
data_bc <- data_bc %>% group_by(id) %>% mutate(season=ifelse(month(time) >= 10 | 
                                                         month(time) < 4,
                                                       "winter", "summer")) %>% ungroup()

# data_bc %>% mutate(month = month(time), year=year(time), date=as.Date(time)) %>% 
#   group_by(id, date) %>% 
#   mutate(m=median(h, na.rm=TRUE)) %>% distinct(id, date, year, month, m, season) %>% 
#   ggplot(.) + 
#   # geom_vline(aes(xintercept=date, colour=season))+
#   scale_y_reverse() +
#   geom_line(aes(date, m, col=id)) + 
#   scale_x_date(limits=c(as.Date("2010-01-01"), as.Date("2020-01-01"))) 

data_bc <- data_bc %>% filter(id!="OW359"  & id!="OW352" & id!="OW359" &
                                id!="OW349" & id!="OW361" & id !="OW292") %>% 
  ungroup() %>% mutate(id = gsub('OW/*','', id)) %>%
  left_join(., read.csv("levl_.csv") %>% mutate(id = gsub('BC_/*','', id)) %>%
              distinct(id, surface.masl) %>% 
              dplyr::rename(elevation=surface.masl)) %>% 
  dplyr::group_by(id) %>%
  dplyr::mutate(m=elevation - h) 

# data_bc %>% mutate(month = month(time), year=year(time), date=as.Date(time)) %>% 
#   group_by(id, date) %>% 
#   mutate(m=median(m, na.rm=TRUE)) %>%
#   distinct(id, date, year, month, m) %>% 
#   ggplot(.) + 
#   geom_line(aes(date, m, col=id)) + 
#   scale_x_date(limits=c(as.Date("2010-01-01"), as.Date("2020-01-01"))) +
  # theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), 
  #       panel.background = element_rect(fill="white"),
  #       axis.text.x = element_blank(),
  #       axis.text =element_text(size=10),
  #       axis.line = element_line(colour="black"))


library(zoo)
data_bc <- data_bc %>% mutate(month = month(time), year=year(time), 
                              date=as.Date(time, "%Y-%m-%d", tz = "UTC")) %>% 
  group_by(id, date) %>% 
  dplyr::mutate(h=median(h, na.rm=TRUE)) %>% select(-time) %>% distinct() 
data_bc <- data_bc %>% 
  distinct(id, date, h) %>% group_by(id) %>% 
  complete(date = seq.Date(min(date, na.rm=TRUE), max(date,na.rm=TRUE), by=1)) %>% 
  dplyr::mutate(h_int= na.approx(h, na.rm=TRUE))

data_bc %>% 
  ggplot(.) + 
  geom_point(aes(date, h), size=0.3) + 
  facet_grid(rows=vars(id), scale="free_y") + 
  scale_x_date("", limits=c(as.Date("2000-01-01"), 
                            as.Date("2020-01-01")), expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0), minor_breaks = NULL) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), 
         panel.background = element_rect(fill="white"),
         axis.text =element_text(size=6),
         axis.line = element_line(colour="black"))


sgi_bc <- data_bc %>% dplyr::mutate(month=month(date), year=year(date)) %>%
  filter(year(date) >= 2005 & year(date) < 2020) %>% 
  # filter(id!="357" & id!="360") %>%
  group_by(id, year, month) %>% 
  dplyr::mutate(m = mean(h_int, na.rm=TRUE)) %>% ungroup() %>% 
  distinct(id, year, month, m)  %>% ungroup() %>% 
  dplyr::mutate(date=as.Date(paste(year, month, "15", sep="-")))


sgi_bc <- sgi_bc %>%
  group_by(id, month) %>% dplyr::mutate(sgi = qqnorm(m, plot.it=FALSE)$x)


# data read for plots below
pre_aa <- read.csv("Q:/Data/SFU_April/OW_Michelle/pcds_data(1)/EC/1100030_aa1.csv")
pre_aa <- pre_aa %>% #filter(year(time) >= 2000) %>%
  dplyr::rename(p = ONE_DAY_PRECIPITATION) %>% distinct(time, p, MIN_TEMP, MAX_TEMP)
pre_aa <- pre_aa %>% mutate(time = as.POSIXct(time))
# pre_aa <- pre_aa %>% mutate(season=ifelse(month(time) >= 10 |
#                                     month(time) < 4,
#                                   "winter", "summer"))
pre_urc_bc <- readRDS("output/process/URC_BC_P.rds") %>%
  unnest_wider(col = c(monthly)) %>% unnest() %>% 
  dplyr::rename(p=value)  
#quick comparison:
urc <- pre_urc_bc %>% filter(dplyr::between(year(date), 1980,2020)) %>% 
  group_by(date) %>% dplyr::mutate(prec=median(p,na.rm=T)) %>%
  distinct(date, prec) %>% ungroup() %>% 
  mutate(year=year(date)) %>% group_by(year) %>%
  dplyr::summarise(sum=sum(prec))

aa<- pre_aa %>% dplyr::rename(date=time) %>%filter(dplyr::between(year(date), 1980,2020)) %>% 
  group_by(date) %>% dplyr::mutate(prec=median(p,na.rm=T)) %>%
  distinct(date, prec) %>% ungroup() %>% 
  mutate(year=year(date)) %>% group_by(year) %>%
  dplyr::summarise(sum=sum(prec))


library(Cairo)
# grDevices::png(filename='figs_MS3/Figure7.png', width=130, height=70, units='mm',
#                res = 400, type='cairo-png')
urc %>% ggplot(., aes(year, sum)) +geom_line(aes(y=sum*0.8, col='CRU'), size=0.5) + 
  geom_line(data=aa, aes(col='AA'), size=0.5) + 
  scale_y_continuous("P, AA data (annual sum in mm)", 
                     sec.axis = sec_axis(~.*0.8, 'P, CRU data (annual sum in mm)')) +
  theme(panel.background = element_rect(colour="lightgrey", fill="white"),
        panel.grid = element_line(colour=alpha("lightgrey",0.5)),
        # strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
        # strip.text = element_text(size=8),
        strip.background = element_blank(), strip.text=element_blank(),
        legend.key = element_rect(fill="white"),
        axis.ticks = element_line(colour="grey"),
        plot.margin = unit(c(0,1,0,0), "mm"),
        axis.title=element_text(size=8),
        axis.title.y.right = element_text(angle = 90),
        axis.text = element_text(size=8), legend.text = element_text(size=8),
        legend.title=element_text(size=8)) +
  scale_colour_manual(name="dataset", 
                    values=c("#9D6C06", "#077DAA", "#FFEA46FF"))
ggsave(filename='figs_MS3/Figure7.pdf', width=130, height=70, units='mm', 
         device=cairo_pdf, dpi=400) 
dev.off()

correlation <- urc %>% right_join(., aa, by='year') %>%
  filter(!is.na(sum.y)) %>%
  mutate(corr=cor(x=sum.x, y=sum.y, method = 'spearman'))

# grDevices::png(filename='figs_MS3/Figure8.png', width=70, height=70, units='mm',
#                res = 400, type='cairo-png')
library(ggpmisc); library(broom)
correlation %>% ggplot(., aes(sum.x, sum.y)) + geom_point() +
  geom_smooth(level=0, method='lm', linetype='dashed', colour='grey', size=0.5) +
  stat_fit_glance(method = 'cor.test',
                  method.args = list(formula = ~ x + y, method = "spearman", exact = FALSE),
                  geom = 'text_npc',
                  mapping = aes(label = sprintf('r=%.3f\np=%.2g',
                                                after_stat(estimate), after_stat(p.value))),
                                # colour=after_stat(p.value)<0.05),
                  label.x = "right", label.y = "bottom", size = 3) +
  theme(panel.background = element_rect(colour="lightgrey", fill="white"),
        panel.grid = element_line(colour=alpha("lightgrey",0.5)),
        # strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
        # strip.text = element_text(size=8),
        strip.background = element_blank(), strip.text=element_blank(),
        legend.key = element_rect(fill="white"),
        axis.ticks = element_line(colour="grey"),
        plot.margin = unit(c(0,1,0,0), "mm"),
        axis.title=element_text(size=8),
        axis.text = element_text(size=8), legend.text = element_text(size=8),
        legend.title=element_text(size=8)) +
  ylab('P, AA data (annual sum in mm)') + xlab('P, CRU data (annual sum in mm)')

ggsave(filename='figs_MS3/Figure8.pdf', width=55, height=50, units='mm', 
         device=cairo_pdf, dpi=400)
dev.off()




source("rfuncs/SIcalc.R")

# spi_aa <- pre_aa %>% na.omit(cde) %>% mutate(id="aa", year = year(time),
#                                          month = month(time)) %>% filter(year>=2005) %>%
#   group_by(id, year, month) %>% summarise(sump=sum(p)) %>%
#   group_by(id) %>% nest() %>%
#   mutate(spi = data %>% map("sump") %>% map(PQ_index_mutate, 1:48))
# p.time <- spi_aa$data[[1]] %>% mutate(time = as.Date(paste(year, month, "15", sep="-"),
#                                                      format="%Y-%m-%d")) %>%
#   select(time,year,month) %>% ungroup() %>%
#   mutate(month_past = 1:length(time)) %>% filter(!is.na(time))
# spi_aa <- spi_aa %>% #dplyr::select(-data) %>%
#   unnest(cols="spi") %>%
#   group_by(id, agg) %>%
#   mutate(month_past = 1:length(index)) %>%
#   left_join(., p.time, by=c("month_past"))

spi_urc_bc <- pre_urc_bc %>% mutate(id="URC TS4.04 BC") %>% group_by(id, lon, lat) %>% 
  filter(year(date) >= 2005) %>% 
  nest() %>%
  mutate(spi = data %>% map("p") %>% map(PQ_index_mutate, 1:60))
p.time <- spi_urc_bc$data[[1]] %>% mutate(time = as.Date(paste(year(date), month(date), "15", sep="-"), 
                                                     format="%Y-%m-%d")) %>% 
  select(time) %>% ungroup() %>% 
  mutate(month_past = 1:length(time)) %>% filter(!is.na(time))
spi_urc_bc <- spi_urc_bc %>% unnest(cols="spi") %>% group_by(lon,lat, agg) %>% 
  mutate(month_past= 1:length(index)) %>% left_join(., p.time, by=c("month_past"))


# connecting BC grids to right well, using sp data package ----
# proximity
data_bc <- data_bc %>% left_join(., 
                                 read.csv("Q:/Data/SFU_April/metadata.csv") %>% filter(Area=="SCR") %>% 
  distinct(OW_nr, Lat, Lon) %>% mutate(id= ifelse(OW_nr==8, "008", NA),
                                       id= ifelse(OW_nr==2, "002", id),
                                       id= ifelse(is.na(id), as.character(OW_nr), id)) %>% #paste("ow", OW_nr, sep="")) %>% 
    rename(lat = Lat, lon = Lon) %>% select(-OW_nr)) %>% 
  filter(id %in% sgi_bc$id)
library(sp)
sp.g <- data_bc %>% distinct(id, lat, lon)
coordinates(sp.g) = ~lon+lat
proj4string(sp.g) = CRS("+proj=longlat + datum=WGS84")
sp.g <-  SpatialPoints(sp.g, proj4string = CRS("+proj=longlat + datum=WGS84"))

spi_urc_bc$a <- spi_urc_bc %>% ungroup() %>% group_by(lon, lat) %>%
  group_indices(lon, lat) 
sp.p <- spi_urc_bc %>% ungroup() %>% group_by(lon, lat) %>% distinct(id, lon, lat, a)
coordinates(sp.p) = ~lon+lat
proj4string(sp.p) = CRS("+proj=longlat + datum=WGS84")
sp.p <- SpatialPoints(sp.p, proj4string = CRS("+proj=longlat + datum=WGS84"))
library(rgeos)
id <- apply(gDistance(sp.p, sp.g, byid=TRUE), 1, which.min)
sgi_bc <- sgi_bc %>% group_by(id) %>% nest()
# attach identifying number to datasets to match grids to wells
sgi_bc$id_urc <- id
spi_urc_bc <- spi_urc_bc %>% rename(dataset = id, id=a) %>% 
  filter(id %in% sgi_bc$id_urc)
spi_urc_bc <- spi_urc_bc %>% group_by(lon, lat, id) %>% rename(date=time)
# double-check proximity
ggmap(stamen_bc) +#, darken = c(0.6, "white")) + 
  geom_point(data=spi_urc_bc %>% ungroup() %>% distinct(id, lon,lat), 
             aes(lon,lat, colour=as.character(id)), size=5,
            alpha=.5) +
  geom_point(data=data_bc %>% distinct(id, lon, lat), aes(lon,lat),
             alpha=.5)
# # combine datasets 
# spi_bc <- spi_urc_bc %>% select(id, dataset, agg, index, date) %>% 
#   full_join(spi_aa %>% ungroup() %>%
#               rename(date=time) %>% mutate(dataset=id) %>% group_by(dataset, agg) %>%
#               select(dataset, agg, index, date))


# spi_sgi_bc <- sgi_bc %>% full_join(., data_bc %>% distinct(id, lon, lat)) %>% 
#   unnest() %>% group_by(id, lon, lat) %>% select(-year, -month, -season)  %>% 
#   nest() %>%
#   mutate(stand_aa= map(data, merge, spi_aa %>% ungroup %>% rename(dataset=id) %>% rename(date=time) %>%
#                       select(dataset, agg, index, date) %>% group_by(agg), by=c("date")))
#trying to put both URC and abbotsford airport data into the same dataset with SGI values
spi_sgi_bc <- #spi_sgi_bc %>%
  sgi_bc %>% full_join(., data_bc %>% distinct(id, lon, lat)) %>% 
    unnest() %>% group_by(id, lon, lat) %>% select(-year, -month)  %>%
    nest() %>%
  mutate(stand_urc= map(data, merge, spi_urc_bc %>% ungroup %>% 
                      select(id, dataset, agg, index, date) %>% group_by(id, agg) %>%
                      rename(id_urc = id)))
saveRDS(spi_sgi_bc, "output/process/spi_sgi_bc_2005.rds")
# Swedish and Finnish wells ----
# use appendix seasonality figures from MS1 to find wells with low vs high 
# intra-annual groundwater level fluctuations, as a proxy for diffusivity
find <- readRDS("output/process/g_raw2.rds") %>% distinct(Station, Tunnus, id, country)
find <- find %>% mutate(place=ifelse(country=="swe", paste("SE_", Station, sep=""), 
                                     paste("FI_", Station, sep="")))
find <- find %>% filter(#id== "f_2043" | 
                          id=="f_2113" | id=="f_742" | id=="f_2167" | id=="f_783" |
                          id=="f_471" | id=="f_3453" | id=="f_3697" | 
                          id=="f_1352" | id=="f_1855" | #id=="f_1526" | 
                          # id=="f_1162" | id=="f_1305" |
                          id=="s_40"| id=="s_79"| id=="s_69" | id=="s_9" | id=="s_98" | id=="s_152" |
                          id=="s_34" | id=="s_37" | id=="s_16" | id=="s_26" | id=="s_94")


# wells
gs.df <- readRDS("output/process/g_ds_raw2.rds") %>% 
  distinct(Station, Tunnus, N,E, id, date,raw) %>% filter(!is.na(raw)) %>%
  rename(lat = N, lon = E)
gf.df <- readRDS("output/process/g_df_raw.rds") %>%
  distinct(Station, Tunnus, N,E, id, date,raw) %>% filter(!is.na(raw))%>%
  rename(lat = N, lon = E)
# combine
g <- gs.df %>% mutate(Station=as.character(Station),
                      id = paste("s_", id, sep=""), country="swe") %>% 
  full_join(., gf.df %>% mutate(id = paste("f_", id, sep=""), country="fi"))

gstat <- g %>% distinct(Tunnus, Station, id, lon, lat, country)
plots <- gstat %>% filter(country=="swe") %>% distinct(Tunnus, Station, id, lon,lat)
coordinates(plots) = ~lon+lat
proj4string(plots) = CRS("+init=epsg:3006")
plots = spTransform(plots, CRS("+proj=longlat + datum=WGS84"))
plots = as.data.frame(plots)
plotf <- gstat %>% filter(country=="fi") %>% distinct(Tunnus, Station, id, lon,lat)
coordinates(plotf) = ~lon+lat
proj4string(plotf) = CRS("+init=epsg:3067")
plotf = spTransform(plotf, CRS("+proj=longlat + datum=WGS84"))
plotf = as.data.frame(plotf)
plot <- full_join(plots, plotf)
rm(plotf, plots, gstat, gs.df, gf.df)
ggmap(stamen_fenn, darken = c(0.6, "white")) + 
  geom_point(data=plot, aes(lon, lat, group=Tunnus, colour=Station))
g <- g %>% select(-lat, -lon) %>% left_join(plot)
# urc data
urc.df <- readRDS("output/process/urcdf.rds")

# connect Fennos wells to right URC grid
library(sp)

sp.g <- g %>% distinct(id, Station, Tunnus, lat, lon)
coordinates(sp.g) = ~lon+lat
proj4string(sp.g) = CRS("+proj=longlat + datum=WGS84")
sp.g <-  SpatialPoints(sp.g, proj4string = CRS("+proj=longlat + datum=WGS84"))


sp.p <- urc.df %>% ungroup() %>% group_by(lon, lat) %>% distinct(id, lon, lat)
coordinates(sp.p) = ~lon+lat
proj4string(sp.p) = CRS("+proj=longlat + datum=WGS84")
sp.p <- SpatialPoints(sp.p, proj4string = CRS("+proj=longlat + datum=WGS84"))
library(rgeos)
id <- apply(gDistance(sp.p, sp.g, byid=TRUE), 1, which.min)
g <- g %>% group_by(id, Station, Tunnus) %>% nest()
# attach identifying number to datasets to match grids to wells
g$id_urc <- id
g <- g %>% filter(id %in% find$id)
urc.df <- urc.df %>% filter(id %in% g$id_urc)

# saveRDS(urc.df, 'kappa/19_18_urc.rds')
# calculate sgi

sgi <- g %>% unnest() %>% mutate(month = month(date), year=year(date)) %>% 
  group_by(id, Tunnus, id_urc, year, month) %>% 
  mutate(m = mean(raw, na.rm=TRUE)) %>% ungroup() %>% 
  distinct(id, id_urc, Station, Tunnus, year, month, m) %>% 
  group_by(id, Station, Tunnus, id_urc, month) %>% 
  mutate(date=as.Date(paste(year, month, "15", sep="-")))

sgi %>% #ungroup() %>% mutate(place =id, id =paste(Station, Tunnus, sep="_")) %>%
  # left_join(.,s.name) %>% ungroup() %>%
  # mutate(id = ifelse(grepl("s", place)==TRUE, Namn, id)) %>% select(-Namn) %>%
  ggplot() + 
  geom_point(aes(date, m, group=Tunnus), size=0.3) + 
  facet_grid(rows=vars(id), scale="free_y") + 
  scale_x_date("", limits=c(as.Date("2000-01-01"), 
                            as.Date("2020-01-01")), expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0), minor_breaks = NULL) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        panel.background = element_rect(fill="white"),
        axis.text =element_text(size=6),
        axis.line = element_line(colour="black"))
# saveRDS(sgi, 'output/process/gwlevels_forRRL.rds')

sgi <- sgi %>% filter(year(date) >= 2000 & year(date) < 2015) %>%
  mutate(sgi = qqnorm(m, plot.it=FALSE)$x)
sgi <- sgi %>% left_join(., plot %>% distinct(id, Tunnus, lon, lat))  
saveRDS(sgi, "output/process/sgi_Fenn_2000.rds")
# calculate spi
source("rfuncs/SIcalc.R")
spi_urc_fenn <- urc.df %>% unnest_wider('monthly') %>% unnest() %>% 
  dplyr::filter(year(date) >= 2000 & year(date) < 2015) %>%
  nest(monthly=c(date, prec, temp, pet, frs, wet)) %>%
  mutate(spi = monthly %>% map("prec") %>% map(PQ_index_mutate, 1:60))
p.time <- urc.df$monthly[[1]] %>% filter(year(date) >= 2000 & year(date) < 2015) %>% 
  mutate(time = as.Date(paste(year(date), month(date), "15", sep="-"), 
                        format="%Y-%m-%d")) %>% 
  select(time) %>% ungroup() %>% 
  mutate(month_past = 1:length(time)) %>% filter(!is.na(time))
spi_urc_fenn <- spi_urc_fenn %>% unnest(cols="spi") %>% group_by(lon,lat, agg) %>% 
  mutate(month_past= 1:length(index)) %>% left_join(., p.time, by=c("month_past"))
spi_urc_fenn <- spi_urc_fenn %>% group_by(lon, lat, id) %>% select(-month_past,-monthly) %>% 
  rename(date=time) %>% nest(.key="spi")

saveRDS(spi_urc_fenn, "output/process/URC-SPI_Fenn_2000.rds")
# combine sgi and spi from Fennoscandia in one dataset
sgi <- readRDS("output/process/sgi_Fenn_2000.rds")

urc.df %>% dplyr::mutate(annual = data %>% map(prec, ))

spi_urc_fenn <- readRDS("output/process/URC-SPI_Fenn_2000.rds")
spi_sgi_fenn <- sgi %>%
  unnest() %>% group_by(id, Tunnus, lon, lat, Station) %>%
  select(-year, -month)  %>% 
  nest() %>%
  mutate(spi_urc= map(data, merge, spi_urc_fenn %>% unnest() %>% ungroup %>% rename(id_urc = id) %>%
                         select(id_urc, agg, index, date) %>% group_by(id_urc, agg))) 
saveRDS(spi_sgi_fenn, "output/process/spi_sgi_fenn_2000.rds")
# combine datasets ----
# spi_sgi_swe = readRDS("output/process/clusters_spi_sgi1911.rds")
# spi_sgi_fenn <- readRDS("output/process/spi_sgi_fenn.rds") # from Finland and Sweden
spi_sgi_fenn <- readRDS("output/process/spi_sgi_fenn_2000.rds")
# spi_sgi_bc <- readRDS("output/process/spi_sgi_bc.rds") # from BC
spi_sgi_bc <- readRDS("output/process/spi_sgi_bc_2005.rds")

# # four sites in Swe and Fraser Valley
# sgi <- spi_sgi_swe %>% unnest(stand) %>% 
#   rename(lon = E, lat=N) %>% mutate(place=paste("SE", strsplit(id, "_")[[1]][[1]], sep="_"), 
#                                     region="Fenn") %>% 
#   select(-data) %>%
#   full_join(., spi_sgi_bc %>% unnest(stand_aa) %>% mutate(place="BC Fraser Valley",
#             region="BC")) %>% 
#   group_by(id, lon, lat, place, region) %>% distinct(date, sgi) %>% nest()
# saveRDS(sgi, "output/process/sgi_SEBC.rds")

#more Swe sites and sites from Finland, Fraser Valley
sgi <- spi_sgi_fenn %>% ungroup() %>% 
  # filter(id != "f_1526" & id != "f_491" & id != "f_783" & id != "f_742" & id != "s_40") %>% 
  unnest(spi_urc) %>% 
  mutate(place=ifelse(substr(id, 0,1) == "s", 
                      paste("SE", Station, sep="_"), paste("FI", Station, sep="_")),
         region="Fenn",
         id= paste(Station, Tunnus, sep="_")) %>% 
  select(-data) %>%
  full_join(., spi_sgi_bc %>% unnest(stand_urc) %>%ungroup() %>% 
              mutate(place="BC Fraser Valley", region="BC",
                     id=paste("BC", id, sep="_"))) %>% 
  # full_join(., spi_sgi_swe %>% unnest(stand) %>% ungroup() %>%
  #             rename(lon = E, lat=N) %>% 
  #             mutate(place=paste("SE", Cluster, sep="_"),
  #                    region="Fenn",
  #                    id=paste(Cluster, Station, sep="_")) %>% 
  #             select(-data)) %>%
  group_by(id, lon, lat, place, region) %>% distinct(date, sgi, m) %>% nest()
saveRDS(sgi, "output/process/sgi_FennBC_00_05.rds")



# # four sites in Swe and Fraser Valley
# sgi_spi <- spi_sgi_swe %>% unnest(stand) %>% select(-data) %>% 
#   rename(lon = E, lat=N) %>% mutate(place=paste("SE", strsplit(id, "_")[[1]][[1]], sep="_"),
#                                     region="Fenn") %>%
#   full_join(., spi_sgi_bc %>% unnest(stand_urc) %>% select(-data) %>% 
#               mutate(place="BC Fraser Valley",
#             region="BC")) 
# sgi_spi <- sgi_spi %>% group_by(id, lon, lat, place, region, dataset) %>% 
#   distinct(id, lon, lat, place, region, dataset, id_urc, date, sgi, agg, index) %>% nest()
# saveRDS(sgi_spi, "output/process/sgi_spi_SEBC.rds")

# more Swe sites and sites from Finland, Fraser Valley
sgi_spi <- spi_sgi_fenn %>% ungroup() %>% 
  # filter(id != "f_1526" & id != "f_491" & id != "f_783" & id != "f_742" & id != "s_40")  %>%
  unnest(spi_urc) %>% select(-data) %>% 
  mutate(place=ifelse(substr(id, 0,1) == "s", 
                      paste("SE", Station, sep="_"), paste("FI", Station, sep="_")),
         region="Fenn",
         id= paste(Station, Tunnus, sep="_"),
         agg=as.numeric(agg)) %>%
  full_join(., spi_sgi_bc %>% unnest(stand_urc) %>% ungroup() %>% 
              select(-data, -dataset) %>% 
              mutate(place="BC Fraser Valley", region="BC",
                     id=paste("BC", id, sep="_"),
                     agg=as.numeric(agg))) %>% 
  # full_join(., spi_sgi_swe %>% unnest(stand) %>% ungroup() %>%
  #             rename(lon = E, lat=N, m=mean_mon) %>% 
  #             mutate(place=paste("SE", Cluster, sep="_"),
  #                    region="Fenn",
  #                    id=paste(Cluster, Station, sep="_"),
  #                    agg=as.numeric(agg)) %>% 
  #             select(-data)) %>%
  group_by(id, lon, lat, place, region) %>% distinct(date, agg, index, m, sgi) %>% nest()
saveRDS(sgi_spi, "output/process/sgi_spi_FennBC_00_05.rds")

# characteristics
stream <- readRDS("output/process/OWs_stream1_stream2_dists.rds") %>%
  mutate(id = paste(Station, Tunnus, sep="_")) %>%
  left_join(.,s.name) %>% ungroup() %>%
  mutate(id = ifelse(!is.na(Namn), Namn, id)) %>% select(-Namn)
stream_dens <- readRDS("fenn_stream_dens.rds") %>%
  mutate(id = paste(Station, Tunnus, sep="_")) %>%
  left_join(.,s.name) %>% ungroup() %>%
  mutate(id = ifelse(!is.na(Namn), Namn, id)) %>% select(-Namn)
lu_dens <- readRDS("lu_density.rds")%>%
  mutate(id = paste(Station, Tunnus, sep="_")) %>%
  left_join(.,s.name) %>% ungroup() %>%
  mutate(id = ifelse(!is.na(Namn), Namn, id)) %>% select(-Namn) %>% 
  select(Tunnus, Station, id, area_percent, LABEL3) %>% rename(label=LABEL3, Apercent_lu = area_percent)
# dist_lakes <- readRDS("fenn_dist_lakes.rds")%>%
#   mutate(id = paste(Station, Tunnus, sep="_")) %>%
#   left_join(.,s.name) %>% ungroup() %>%
#   mutate(id = ifelse(!is.na(Namn), Namn, id)) %>% select(-Namn)

dist_lakes <- readRDS("fenn_dist_wbodies_r3k.rds")%>%
  mutate(id = paste(Station, Tunnus, sep="_")) %>%
  left_join(.,s.name) %>% ungroup() %>%
  mutate(id = ifelse(!is.na(Namn), Namn, id)) %>% select(-Namn)
dist_rivers <- readRDS("fenn_dist_wcourses_r3k.rds")%>%
  mutate(id = paste(Station, Tunnus, sep="_")) %>%
  left_join(.,s.name) %>% ungroup() %>%
  mutate(id = ifelse(!is.na(Namn), Namn, id)) %>% select(-Namn)


lc <- full_join(stream, stream_dens) %>%
  full_join(., lu_dens) %>% full_join(., dist_lakes)%>% full_join(., dist_rivers)
saveRDS(lc, "landcover_fenn.rds")


stream <- readRDS("output/process/OWs_stream1_stream2_dists.rds") %>%
  mutate(id = paste(Station, Tunnus, sep="_")) %>%
  left_join(.,s.name) %>% ungroup() %>%
  mutate(id = ifelse(!is.na(Namn), Namn, id)) %>% select(-Namn)
stream_dens <- readRDS("fenn_stream_dens_r300.rds") %>%
  mutate(id = paste(Station, Tunnus, sep="_")) %>%
  left_join(.,s.name) %>% ungroup() %>%
  mutate(id = ifelse(!is.na(Namn), Namn, id)) %>% select(-Namn)
lu_dens <- readRDS("lu_density_r300.rds")%>%
  mutate(id = paste(Station, Tunnus, sep="_")) %>%
  left_join(.,s.name) %>% ungroup() %>%
  mutate(id = ifelse(!is.na(Namn), Namn, id)) %>% select(-Namn) %>% 
  select(Tunnus, Station, id, ar_prcn, LABEL3) %>% rename(label=LABEL3, Apercent_lu = ar_prcn)
dist_lakes <- readRDS("fenn_dist_lakes_r300.rds")%>%
  mutate(id = paste(Station, Tunnus, sep="_")) %>%
  left_join(.,s.name) %>% ungroup() %>%
  mutate(id = ifelse(!is.na(Namn), Namn, id)) %>% select(-Namn)

lc <- full_join(stream, stream_dens) %>%
  full_join(., lu_dens) %>% full_join(., dist_lakes)
saveRDS(lc, "landcover_fenn_r300.rds")

# data on well characteristics/stats ----
els <- readRDS("output/process/g_ds_raw2.rds") %>% 
  distinct(Station, Tunnus, elevation, material, type) %>%
  mutate(Station=as.character(Station), Tunnus=as.character(Tunnus)) %>%
  full_join(., readRDS("output/process/g_df_raw.rds") %>% 
              distinct(Station, Tunnus, elevation)) %>% 
  mutate(id = paste(Station, Tunnus, sep="_")) %>%
  mutate(confinement = ifelse(grepl("öppet", type), "unconfined", NA),
         confinement = ifelse(grepl("slutet", type), "confined", confinement),
         aquifer.material = ifelse(material=="Moränlera", "clayey till", NA),
         aquifer.material = ifelse(material=="Morän", "till", aquifer.material),
         aquifer.material = ifelse(material=="Grus", "gravel", aquifer.material),
         aquifer.material = ifelse(material=="Sand", "sand", aquifer.material),
         aquifer.material = ifelse(material=="Silt", "silt", aquifer.material),
         aquifer.material = ifelse(grepl("Lera", material)==TRUE, "clay", aquifer.material)) %>%
  select(-type, -material)


level_stats <- readRDS("output/process/sgi_spi_FennBC_00_05.rds") %>% 
  unnest_wider(data) %>% unnest() %>% 
  dplyr::select(-agg, -index, -sgi) %>% distinct() %>% 
  left_join(., els %>% distinct(id, elevation)) %>% 
  left_join(., read.csv("levl_.csv") %>% 
              select(-lon, -lat))%>% 
  dplyr::mutate(elevation=ifelse(is.na(elevation), surface.masl, elevation))

level_stats <- level_stats %>% 
  group_by(id, place, region) %>%
  mutate(h = m, m = elevation - m, 
         sd_all = sd(m, na.rm=TRUE), med_all = median(m, na.rm=TRUE),
         min_all = min(m, na.rm=TRUE), max_all = max(m, na.rm=TRUE),
         iqr = IQR(h, na.rm=TRUE), h_all = median(h, na.rm=TRUE))
level_stats <- level_stats %>% mutate(year = year(date)) %>% 
  group_by(id, place, region, year) %>%
  mutate(sd_yr = sd(m, na.rm=TRUE), med_yr = median(m, na.rm=TRUE),
         min_yr = min(m, na.rm=TRUE), max_yr = max(m, na.rm=TRUE)) %>% 
  ungroup() %>% group_by(id, place, region) %>%
  mutate(sd_med = median(sd_yr, na.rm=TRUE), med_med = median(med_yr, na.rm=TRUE),
         min_med = median(min_yr, na.rm=TRUE), max_med = median(max_yr, na.rm=TRUE)) %>%
  distinct(elevation, #sd_yr, 
           sd_all, 
           sd_med, h_all, iqr,
           #med_yr, 
           med_all, med_med, 
           #min_yr, 
           min_all, min_med, 
           #max_yr, max_all, 
           max_med)
level_stats <- level_stats %>% 
  left_join(., read.csv("levl_.csv", sep=",") %>% 
              select(-lon, -lat)) %>%
  mutate(aq_thick = soil.thickness - med_med, 
         artesian = ifelse(min_all < 0, "artesian", "subartesian"))
saveRDS(level_stats, "output/process/OW_chardata-new.rds")


# data load ----
# Swedish dataset created in cralculate sgi and correlate indices

s.name <- readRDS("Q:/Projects/Drought Scandinavia/Michelle/Nordic_Drought/input/SWE_data/processed/gw_swe.RDS")$meta %>% 
  distinct(Omr_stn, Namn, N, E) %>%
  # filter(Omr_stn %in% readRDS("output/process/sgi_FennBC.rds")$id) %>% 
  dplyr::rename(id=Omr_stn)

# spi_sgi_swe = readRDS("output/process/clusters_spi_sgi1911.rds")
# 
# spi_sgi_fenn <- readRDS("output/process/spi_sgi_fenn.rds") # from Finland and Sweden
# 
# spi_sgi_bc <- readRDS("output/process/spi_sgi_bc.rds")

# spi_sgi <- readRDS("output/process/sgi_spi_SEBC.rds")

# spi_sgi <- readRDS("output/process/sgi_spi_FennBC.rds")
spi_sgi <- readRDS("output/process/sgi_spi_FennBC_00_05.rds")%>%
  left_join(.,s.name) %>% ungroup() %>%
  mutate(id = ifelse(grepl("SE", place)==TRUE, Namn, id)) %>% dplyr::select(-Namn)

# sgi <- readRDS("output/process/sgi_SEBC.rds")
# sgi <- readRDS("output/process/sgi_FennBC.rds") %>%
#   left_join(.,s.name) %>% ungroup() %>%
#   mutate(id = ifelse(grepl("SE", place)==TRUE, Namn, id)) %>% select(-Namn)
sgi <- readRDS("output/process/sgi_FennBC_00_05.rds") %>%
  left_join(.,s.name) %>% ungroup() %>%
  mutate(id = ifelse(grepl("SE", place)==TRUE, Namn, id)) %>% dplyr::select(-Namn)


level_stats <- #readRDS("output/process/OW_chardata.rds") %>%
  readRDS("output/process/OW_chardata-new.rds") %>%
  left_join(.,s.name) %>% ungroup() %>%
  mutate(id = ifelse(grepl("SE", place)==TRUE, Namn, id)) %>% dplyr::select(-Namn)
# ezra_indices <- read.csv("C:/Users/xnygmi/Desktop/ezra-indices/ezra_indices_SGI-OWs.csv") %>%
#   select(-X) %>% rename(id=pid)%>%
#   left_join(.,s.name) %>% ungroup() %>%
#   mutate(id = ifelse(!is.na(Namn), Namn, id)) %>% select(-Namn)
land_cover <- readRDS("landcover_fenn.rds")
lc_r300 <- readRDS("landcover_fenn_r300.rds")



land_cover <- land_cover %>% group_by(id) %>% 
  mutate(stream_density = sum(stream_density, na.rm=TRUE),
         stream_l = sum(stream_l, na.rm=TRUE)) %>%
  group_by(id, label) %>% 
  mutate(Apercent_lu=sum(Apercent_lu, na.rm=TRUE)) %>% 
  ungroup() %>%
  distinct()
lc_r300 <- lc_r300 %>% group_by(id) %>% 
  mutate(stream_density = sum(stream_density, na.rm=TRUE),
         stream_l = sum(stream_l, na.rm=TRUE)) %>%
  group_by(id, label) %>% mutate(Apercent_lu=sum(Apercent_lu)) %>% ungroup() %>%
  distinct() 

cross_auto <- readRDS("output/process/cross_auto_refperiod_00_05_2.rds")
chars <- cross_auto %>% 
  distinct(id, place, memory, Arange, Qmax, slope1, cp.auto,
           cp.corr, rt) %>%
  left_join(., land_cover) %>%
  left_join(., level_stats) %>%
  dplyr::mutate(aquifer.material = first(na.omit(aquifer.material)),
                dist_stream = ifelse(is.na(dist_stream), dist.stream1, dist_stream),
                dist_stream = first(na.omit(dist_stream)),
                dist_stream2 = ifelse(is.na(dist_stream2), dist.stream2, dist_stream2),
                dist_stream2 = first(na.omit(dist_stream2)),
                dist_lake1 = ifelse(is.na(dist_lake1), dist.lake, dist_lake1),
                dist_lake1= first(na.omit(dist_lake1))) 

bloom_data15 <- data.frame(region = "Bloomfield et al. 2015",
                           place="Bloomfield et al. 2015",
                           geo=c("Limestone", "Chalk", "Sandstone", 
                                 "Limestone",
                                 "Chalk", "Sandstone"),
                           id= c("CL1", "CL2", "CL3", "CL4", "CL5", "CL6"),
                           maxcorr = c(0.74, 0.86, 0.36, 0.82, 0.53, 0.09),
                           Qmax = c(4, 16, 15, 9, 17, NA),
                           Mmax = c(15, 23, 60, 18, 28, NA))
bloom_data13 <- data.frame(region = "Bloomfield and Marchant 2013",
                           id= c("Asthon Farm", 
                                 "Bussels No. 7", 
                                 "Chilgrove House", 
                                 "Dalton Holme", 
                                 "Heathlanes", 
                                 "Little Bucket Farm",
                                 "Llanfair DC",
                                 "Lower Barn Cottage",
                                 "New Red Lion",
                                 "Rockley",
                                 "Stonor park",
                                 "Therfield Rectory",
                                 "Well Hose Inn",
                                 "West Dean No. 3"), 
                           place="Bloomfield and Marchant 2013", geo=c("Chalk",
                                                                       "Sandstone",
                                                                       "Chalk",
                                                                       "Chalk",
                                                                       "Sandstone",
                                                                       "Chalk",
                                                                       "Sandstone",
                                                                       "sand",
                                                                       "Limestone",
                                                                       "Chalk",
                                                                       "Chalk",
                                                                       "Chalk",
                                                                       "Chalk",
                                                                       "Chalk"),
                           maxcorr = c(0.72, 0.83, 0.74, 
                                       0.76, 0.74, 0.87,
                                       0.79, 0.81, 0.83, 
                                       0.74, 0.79, 0.77, 0.78,
                                       0.70),
                           Qmax = c(6,9,6,10,28,10,25,15,8,7,21,21,12,7),
                           Mmax = c(5,19,9,12,24,11,32,18,20,8,15,20,14,12))
bloom_data <- bloom_data13 %>% full_join(., bloom_data15); rm(bloom_data13, bloom_data15)

# make plots for kappa 220316 ----
g <- readRDS('kappa/19_18_OW.rds') %>% unnest %>% select(-id_urc) %>% 
  dplyr::mutate(med=median(raw, na.rm=T),
                h=raw-med, month=month(date), year=year(date)) %>% 
  select(-lon,-lat) %>% 
  dplyr::mutate(date=as.Date(paste(year, month, '15', sep='-'))) %>%
  group_by(date) %>% dplyr::mutate(h=median(raw, na.rm=T)) %>% 
  distinct(date, year, month, h)
prec <- readRDS('kappa/19_18_urc.rds') %>% unnest_wider(monthly) %>% 
  select(-id) %>% unnest %>%
  dplyr::mutate(month=month(date), year=year(date),
                date=as.Date(paste(year, month, '15', sep='-')))

data <- g %>% full_join(., prec, by='date') 

raw <- data %>% dplyr::filter(between(year(date),2002,2005)) %>% 
  ggplot(., aes(x=date)) + 
  geom_col(aes(y=(prec))) +
  geom_line(aes(y=(h-152)*-150+200)) + my_theme +
  scale_x_date('', expand=c(0,0))+
  scale_y_reverse('Precipitation (m)', expand=c(0,0), 
                  sec.axis =  sec_axis('Groundwater level (mm)',
                                       trans = ~rev(((.+200)/150)+152)))

spi_sgi <- readRDS("output/process/sgi_spi_FennBC_00_05.rds") %>% 
  dplyr::filter(id=='19_18') %>% unnest_wider(data) %>% unnest

spi <-spi_sgi %>% dplyr::filter(agg==1 | agg==3| agg==12 | agg==24) %>%
  dplyr::mutate(type=paste('SPI', agg, sep='-'), 
                type=factor(type, levels=c('SPI-1', 'SPI-3', 'SPI-6',
                                           'SPI-12', 'SPI-24', 'SGI'))) %>% 
  dplyr::filter(between(year(date),2002,2005))%>% 
  
  ggplot(., aes(x=date, group=type)) +
  geom_hline(yintercept=0, linetype='dashed') +
  
  geom_ribbon(aes(ymin=index, ymax=0), fill='black', alpha=0.3) +
  
  geom_ribbon(data=. %>% dplyr::distinct(date, sgi) %>%
                dplyr::mutate(type=factor('SGI')),
              aes(ymin=sgi, ymax=0, x=date), fill='black', alpha=0.3) +
  
  geom_line(data=. %>% dplyr::distinct(date, sgi) %>% 
              dplyr::mutate(type=factor('SGI')), aes(y=sgi)) +
  geom_line(aes(y=index)) + 
  facet_grid(rows=vars(type)) +
  scale_x_date('', expand=c(0,0)) + scale_y_continuous('', expand=c(0,0))+
  my_theme
  

# from: plot cross and auto corr ts
cross_auto <- readRDS("output/process/cross_auto_refperiod_00_05_2.rds") %>% 
  dplyr::filter(id=='Brattforsheden_18')
acf <- ac_plot(cross_auto %>% dplyr::mutate(mat = NA, confinement=NA, dist_stream=NA, 
                                     landform=NA,med_med=NA, aquifer.material=NA, 
                                     surface.material=NA), 
        type='auto', n='ex') + my_theme + theme(strip.text=element_blank())
ccf <- ac_plot(cross_auto %>% dplyr::mutate(mat = NA, confinement=NA, dist_stream=NA, 
                                            landform=NA,med_med=NA, aquifer.material=NA, 
                                            surface.material=NA), 
               type='cross', n='ex') + my_theme + theme(strip.text=element_blank())
one <- plot_grid(raw + theme(#axis.text.x = element_blank(), axis.ticks.x=element_blank(),
  axis.title.x=element_blank()), 
  acf+ theme(#axis.text.x = element_blank(), axis.ticks.x=element_blank(),
    #axis.title.x=element_blank(), 
    legend.position='none'), 
  ccf + theme(legend.position='none'), 
  axis='tblr', align='hv', ncol=1)#, rel_heights = c(2,1.2))
library(cowplot); library(Cairo)
grDevices::cairo_pdf(filename='sweden_class/Fig7.pdf', width=6, height=4.3,
                     fallback_resolution = 400)
plot_grid(one, spi, align='hv')

dev.off()
# make plots of 'raw' data ----

data <- sgi %>% unnest_wider(data) %>% unnest() %>% 
  dplyr::distinct(id, place, date, m, lat) %>% dplyr::group_by(id) %>% 
  dplyr::mutate(rec = median(m), year=year(date), month=lubridate::month(date, label =TRUE, abbr=TRUE,
                                                       locale='English'),
         depth = m - rec) %>% 
  dplyr::group_by(id, month) %>%
  dplyr::mutate(monthly=median(depth), monthly_m = median(m)) %>% distinct(id, place, month, monthly, monthly_m)

data <- data %>% right_join(., chars %>% 
                dplyr::select(-c(2:11), -region, -Apercent_lu, -label) %>% distinct() %>% 
  dplyr::mutate(confinement = as.character(confinement),
         confinement = ifelse(confinement =="confined" | 
                                grepl("potentially confined", confinement)==TRUE,
                              "confined/potentially confined", confinement),
         confinement = ifelse(grepl("unconfined", confinement)==TRUE,
                              "unconfined/probably unconfined", confinement),
         confinement = ifelse(grepl("silt", aquifer.material)==TRUE,
                              "confined/potentially confined", confinement),
         confinement = ifelse(grepl("till", aquifer.material)==TRUE & is.na(confinement),
                              "unconfined/probably unconfined", confinement),
         confinement = ifelse(grepl("sand", aquifer.material)==TRUE & is.na(confinement),
                              "confined/potentially confined", confinement),
         water = min(dist_stream,dist_lake1))) 

x <- data %>%
  dplyr::filter(id=="Brattforsheden_17"|
         id=="BC_002" | id=="BC_259" |
         id=="Valkeala_0402p10" |
         id=="Kälviä_1003p1" | id== "Kälviä_1003p5" |
         id=="Alavus Taipale_0802p8" |
         id=="Abisko_10" ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(id= ifelse(grepl("Bratt", id), "B17", id),
       id = ifelse(grepl("002", id), "FV2", id),
       id = ifelse(grepl("259", id), "FV259", id),
       id = ifelse(grepl("Valk", id), "V10", id),
       id = ifelse(grepl("3p1", id), "K1", id),
       id = ifelse(grepl("3p5", id), "K5", id),
       id = ifelse(grepl("Alavus", id), "AT8", id),
       id = ifelse(grepl("Abisko", id), "A10", id)) %>%
  # dplyr::mutate(n=NA,
  #        n = ifelse(id=="Brattforsheden_17", "B17", n),
  #        n = ifelse(id=="BC_002","FV2",n),
  #        n = ifelse(id=="BC_259","FV259", n),
  #        n = ifelse(id=="Valkeala_0402p10","V10",n),
  #        n = ifelse(id=="Kälviä_1003p1","K1",n),
  #        n = ifelse(id== "Kälviä_1003p5","K5",n),
  #        n = ifelse(id=="Alavus Taipale_0802p8","AT8",n),
  #        n = ifelse(id=="Abisko_10", "A10",n)) %>% 
  # distinct(id, place, aquifer.material, confinement, month, monthly, climate) %>% ungroup() %>%
  ungroup() %>% dplyr::mutate(month=factor(month, levels=c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May",
                                      "Jun", "Jul", "Aug", "Sep", "Oct"))) %>%
  # mutate(month=factor(month, levels=c(11,12,1,2,3,4,5,6,7,8,9,10))) %>%
  
  dplyr::mutate(#id=ifelse(grepl("SE", place) | grepl("BC", place),
         #           as.numeric(gsub(".*_", "", id)), id),
         # id=ifelse(grepl("FI", place),
         #           as.numeric(gsub(".*p", "", id)), id),
         mat = ifelse(aquifer.material=="sand" & grepl("unconfined", confinement)==F, 
                      paste(aquifer.material, "*", sep=""),
                      paste(aquifer.material))) #%>%
  # mutate(place=ifelse(place=="SE_19", "SE_Brattforsheden", place),
  #         place=ifelse(place=="SE_39", "SE_Abisko", place),
  #        id=paste(place, id, sep="_")) %>% group_by(id, place)
not_x <- data %>%
  dplyr::distinct(id, place, aquifer.material, confinement, month, monthly, monthly_m, climate) %>% ungroup() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(month=factor(month, levels=c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May",
                                                    "Jun", "Jul", "Aug", "Sep", "Oct"))) %>%
  dplyr::mutate(id= ifelse(grepl("Brattforsheden_17", id), "B17", id),
          id = ifelse(grepl("BC_002", id), "FV2", id),
          id = ifelse(grepl("BC_259", id), "FV259", id),
          id = ifelse(grepl("0402p10", id), "V10", id),
          id = ifelse(grepl("1003p1", id), "K1", id),
          id = ifelse(grepl("1003p5", id), "K5", id),
          id = ifelse(grepl("0802p8", id), "AT8", id),
          id = ifelse(grepl("Abisko_10", id), "A10", id), 
      mat = ifelse(aquifer.material=="sand" & grepl("unconfined", confinement)==F, 
                 paste(aquifer.material, "*", sep=""),
                 paste(aquifer.material))) 
dat_plot <- function(data, text="") {
  library(corrplot)
  ggplot(data %>% mutate(tipo = ifelse(climate=="temperate", "rainfall", "snow"))) + 
  # geom_line(aes(month, monthly, colour=confinement, linetype=climate, group=id)) + 
  geom_line(aes(month, monthly, group=id, colour=climate, 
                linetype='rest', size='rest'), alpha=0.3) + 
    
    # facet_grid(~aquifer.material) + 
  
    geom_text(data= as.data.frame(1), 
              size=2.5,  aes(x=Inf, y=Inf, label=paste(text)), vjust=1, hjust=1) +
  
   scale_colour_manual("climate:", 
                       values=c("cold,"=COL2('RdBu')[150], "temperate"=COL2('RdBu')[30]), 
                       na.translate=T, na.value="black") + 
    # scale_linetype_manual("", values=c(2,4), na.translate=T, na.value=1) +
    guides(colour=guide_legend(override.aes = list(size=1))) +
    scale_size_manual("well:", values=c('examples,'=0.5, 'rest'=0.2)) +
    scale_linetype_manual("well:", values=c('examples,'='dashed', 'rest'='solid')) +
           
  scale_y_continuous("Average monthly groundwater level (m)", expand=c(0,0), limits = c(-1.6,1.5)) +
    scale_x_discrete("", expand=c(0,0), breaks=c("Nov", "Feb", "May", "Aug")) + 
  # scale_x_discrete("", expand=c(0,0), breaks=c(11,2,5,8)) +
  theme(panel.background = element_rect(colour="lightgrey", fill="white"),
        panel.grid = element_line(colour=alpha("lightgrey",0.5)),
        # strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
        # strip.text = element_text(size=8),
        strip.background = element_blank(), strip.text=element_blank(),
        legend.key = element_rect(fill="white"),
        axis.ticks = element_line(colour="grey"),
        plot.margin = unit(c(0,1,0,0), "mm"),
        axis.text = element_text(size=8), legend.text = element_text(size=8),
        legend.title=element_text(size=8))
}


library(gridExtra); library(grid); library(ggrepel)

csand <- dat_plot(data=not_x %>% filter(mat=="sand*") %>% filter(id!='V10' & id!='FV259') %>%
                    dplyr::mutate(climate=ifelse(climate=='cold', 'cold,', climate)), 
         text="n=21") +
  geom_line(data=x %>% filter(id=='V10' | id=='FV259') %>%
              dplyr::mutate(climate=ifelse(climate=='cold', 'cold,', climate))%>% 
              mutate(tipo = ifelse(climate=="temperate", "rainfall", "snow")),
            aes(month, monthly, group=id, colour=climate, 
                size='examples,', linetype='examples,')) +
  geom_label_repel(data=x %>% filter((id=='V10' | id=='FV259') & month=="Sep") %>%
                     dplyr::mutate(climate=ifelse(climate=='cold', 'cold,', climate))%>% 
                     mutate(tipo = ifelse(climate=="temperate", "rainfall", "snow")),
                   aes('Sep', monthly, label = id, colour=climate, group=id),
                   nudge_x=1, size=2,na.rm = TRUE, show.legend=F, max.overlaps = 3)+
  facet_grid(rows=vars(mat)) +
  geom_text(data=as.data.frame(1), aes(-Inf,Inf, label='(a)'), hjust=-0.5, vjust=1)+ 
  theme(axis.title = element_blank(), #axis.text.y = element_text(colour='white'),
        axis.text.x = element_text(colour='white'),
        legend.position = 'none')

unsand <- dat_plot(data=not_x %>% filter(mat=="sand") %>% filter(id!='B17' & id!='FV2')%>%
                     dplyr::mutate(climate=ifelse(climate=='cold', 'cold,', climate)), 
                  text="n=34") +
  geom_line(data=x %>% filter(id=='B17' | id=='FV2') %>%
              dplyr::mutate(climate=ifelse(climate=='cold', 'cold,', climate))%>% 
              mutate(tipo = ifelse(climate=="temperate", "rainfall", "snow")),
            aes(month, monthly, group=id, colour=climate, 
                size='examples,', linetype='examples,')) +
  geom_label_repel(data=x %>% filter((id=='B17' | id=='FV2') & month=="Sep") %>%
                     dplyr::mutate(climate=ifelse(climate=='cold', 'cold,', climate))%>% 
                     mutate(tipo = ifelse(climate=="temperate", "rainfall", "snow")),
                   aes('Sep', monthly, label = id, colour=climate, group=id),
                   nudge_x=1,size=2,na.rm = TRUE, max.overlaps = 3)+
  facet_grid(rows=vars(mat)) +
  geom_text(data=as.data.frame(1), aes(-Inf,Inf, label='(b)'), hjust=-0.5, vjust=1)+ 
  theme(axis.title = element_blank(), axis.text = element_text(colour='white'),
        legend.position = 'none')
silt <- dat_plot(data=not_x %>% filter(mat=="silt") %>%
                   dplyr::mutate(climate=ifelse(climate=='cold', 'cold,', climate))%>% 
                   filter(id!='K1' & id!='K5'), 
                  text="n=6") +
  geom_line(data=x %>% filter(id=='K1' | id=='K5') %>%
              dplyr::mutate(climate=ifelse(climate=='cold', 'cold,', climate)) %>% 
              mutate(tipo = ifelse(climate=="temperate", "rainfall", "snow")),
            aes(month, monthly, group=id, colour=climate, 
                size='examples,', linetype='examples,')) +
  geom_label_repel(data=x %>% filter((id=='K1' | id=='K5') & month=="Sep") %>%
                     dplyr::mutate(climate=ifelse(climate=='cold', 'cold,', climate))%>% 
                     mutate(tipo = ifelse(climate=="temperate", "rainfall", "snow")),
                   aes('Sep', monthly, label = id, colour=climate, group=id),
                   nudge_x=1,size=2,na.rm = TRUE, max.overlaps = 3)+
  facet_grid(rows=vars(mat)) +
  geom_text(data=as.data.frame(1), aes(-Inf,Inf, label='(c)'), hjust=-0.5, vjust=1)+ 
  theme(axis.title = element_blank(), #axis.text.y = element_text(colour='white'),
        legend.position = 'none')
till <- dat_plot(data=not_x %>% filter(mat=="till") %>% filter(id!='AT8' & id!='A10')%>%
                   dplyr::mutate(climate=ifelse(climate=='cold', 'cold,', climate)), 
                  text="n=20") +
  geom_line(data=x %>% filter(id=='AT8' | id=='A10') %>%
              dplyr::mutate(climate=ifelse(climate=='cold', 'cold,', climate))%>% 
              mutate(tipo = ifelse(climate=="temperate", "rainfall", "snow")),
            aes(month, monthly, group=id, colour=climate, 
                size='examples,', linetype='examples,')) +
  geom_label_repel(data=x %>% filter((id=='AT8' | id=='A10') & month=="Sep") %>%
                     dplyr::mutate(climate=ifelse(climate=='cold', 'cold,', climate))%>% 
                     mutate(tipo = ifelse(climate=="temperate", "rainfall", "snow")),
                   aes('Sep', monthly, label = id, colour=climate, group=id),
                   nudge_x=1,size=2,na.rm = TRUE, max.overlaps = 3)+
  facet_grid(rows=vars(mat)) +
  geom_text(data=as.data.frame(1), aes(-Inf,Inf, label='(d)'), hjust=-0.5, vjust=1)+ 
  theme(axis.title = element_blank(), axis.text.y = element_text(colour='white'),
        legend.position = 'none')
comb <- grid.arrange(arrangeGrob(csand, top=textGrob('confined sand', gp=gpar(fontsize=8))), 
                     arrangeGrob(unsand, top=textGrob('unconfined sand', gp=gpar(fontsize=8))), 
                     arrangeGrob(silt, top=textGrob('silt', gp=gpar(fontsize=8))), 
                     arrangeGrob(till, top=textGrob('till', gp=gpar(fontsize=8))),
                     left=textGrob('median monthly groundwater levels (m)', 
                                   gp=gpar(fontsize=8), rot=90))
library(Cairo)
# grDevices::png(filename='figs_MS3/Figure2.png', width=140, height=110, units='mm',
#                      res = 400, type='cairo-png')

grid.arrange(comb, get_legend(csand + theme(legend.position='bottom')),
             ncol=1, heights=c(2,0.3)) %>% 
  ggsave(filename='figs_MS3/Figure2.pdf', width=100, height=80, units='mm', 
         device=cairo_pdf, dpi=400, plot=.)
dev.off()

# re-calculate SGI 2022-05-04 ----
# changing SGI, not grouping by month
test <- sgi %>% unnest_wider('data') %>% unnest() %>% dplyr::filter(T==grepl('Pålkem)_10', id))
test <- test %>% dplyr::mutate(month=month(date)) %>%
  group_by(id, month) %>% dplyr::mutate(sgi = qqnorm(m)$x)

test %>% ggplot(.) + geom_line(aes(date, sgi)) + 
   facet_wrap(~id, scales='free')


spi_sgi <- spi_sgi %>% unnest_wider('data') %>% unnest() %>%
  dplyr::select(-sgi) %>% 
  left_join(., sgi %>% dplyr::select(id, place, region, date, sgi, m), 
            by=c('id', 'place', 'region', 'date', 'm'))
  


# make plots for review response letter, using well V10 as the example ----
# need sgi, cross_auto
data <- readRDS('output/process/gwlevels_forRRL.rds') %>% 
  dplyr::mutate(id=paste(Station, Tunnus, sep='_')) %>% ungroup() %>%
  dplyr::select(-Station, -Tunnus, -id_urc, -year, -month) %>%
  left_join(.,s.name) %>%
  mutate(id = ifelse(grepl("s_", id)==TRUE, Namn, id)) %>% 
  dplyr::select(-Namn, -N, -E) %>% group_by(id)

data <- data %>% filter(year(date) >= 2000 & id=='Valkeala_0402p10') %>%
  group_by(month(date)) %>%
  mutate(sgi = qqnorm(m, plot.it=FALSE)$x)

sgi_x <- sgi %>% dplyr::filter(id=='Valkeala_0402p10') %>% unnest()

cross_auto_x <- cross_auto %>% dplyr::filter(id=='Valkeala_0402p10') 
my_theme <- theme(panel.background = element_rect(colour="lightgrey", fill="white"),
                  panel.grid = element_line(colour=alpha("lightgrey",0.5)),
                  strip.background = element_blank(), 
                  legend.key = element_rect(fill="white"),
                  axis.ticks = element_line(colour="grey"),
                  plot.margin = unit(c(0,0,0,0), "mm"),
                  axis.title=element_text(size=8),
                  axis.text = element_text(size=8), legend.text = element_text(size=8),
                  legend.title=element_text(size=8))

plotraw <- data %>% ggplot(., aes(date, m)) + 
  geom_line(data=. %>% mutate(type='raw'), aes(colour='now')) +
  geom_vline(xintercept=as.Date('2015-01-01'), linetype=2, colour='red') +
  scale_colour_manual('record length', values=c('before'='red', 'now'='black')) +
  my_theme + facet_wrap(~type, scales='free') +
  geom_text(data=. %>% distinct(id), 
            aes(as.Date('2000-01-01'),Inf, label='(a)'), hjust=-0.5, vjust=1)
  

plotsgi <- data %>% ggplot(., aes(date, sgi)) + 
  geom_line(data=. %>% mutate(type='SGI'), aes(colour='before')) +
  geom_line(data=sgi_x %>% mutate(type='SGI'), aes(colour='now')) +
  geom_hline(yintercept=0, linetype=2) +
  scale_colour_manual('record length', values=c('before'='red', 'now'='black')) +
  facet_wrap(~type, scales='free') + my_theme+
  geom_text(data=. %>% distinct(id), 
            aes(as.Date('2000-01-01'),Inf, label='(b)'), hjust=-0.5, vjust=1)

plotauto <- data %>% ggplot(., aes(lag, auto)) + 
  geom_line(data=sgi_a %>% mutate(type='Autocorrelation') %>% dplyr::filter(lag<=60), 
            aes(colour='before')) +
  geom_line(data=cross_auto_x %>% mutate(type='Autocorrelation'), 
            aes(colour='now')) +
  
  geom_hline(aes(yintercept = 0.2, colour='before'), linetype=2) +
  geom_hline(aes(yintercept = 0.173, colour='now'), linetype=2) +
  scale_colour_manual('record length', values=c('before'='red', 'now'='black')) +
  facet_wrap(~type) + my_theme+
  geom_text(data=. %>% distinct(id), 
            aes(0,-Inf, label='(c)'), hjust=-0.5, vjust=-1)

library(gridExtra); library(grid)
grDevices::png(filename='figs_MS3/Figure_responseletter.png', width=165, height=150, units='mm',
               res = 400, type='cairo-png')
plot_grid(plot_grid(plotraw + theme(legend.position = 'none'), 
                      plotsgi + theme(legend.position = 'none'), 
                      plotauto + theme(legend.position = 'none'), align='v', ncol=1),
             get_legend(plotsgi + theme( legend.position='bottom')), 
             ncol=1, rel_heights=c(3,0.3))
dev.off()


short <- data %>% filter(id=='19_17') %>%
  dplyr::filter(between(year(date), 2000, 2015)) %>% group_by(month(date)) %>%
  dplyr::mutate(sgi = qqnorm(m, plot.it=FALSE)$x, type='2000-2015')

medium <- data %>% filter(id=='19_17') %>% group_by(month(date)) %>%
  dplyr::filter(between(year(date), 1990, 2015)) %>% 
  dplyr::mutate(sgi = qqnorm(m, plot.it=FALSE)$x, type='1990-2015')

long <- data %>% filter(id=='19_17') %>% group_by(month(date)) %>%
  dplyr::filter(between(year(date), 1980, 2015)) %>%
  dplyr::mutate(sgi = qqnorm(m, plot.it=FALSE)$x, type='1980-2015')

month <- long %>% ggplot(., aes(date, sgi, colour=type)) + 
  geom_line() +
  geom_line(data=short) +
  geom_line(data=medium) +
  geom_hline(yintercept=0, linetype=2) +
  # facet_wrap(~type, ncol=1) + 
  my_theme

plot_grid(month, nomonth, ncol=1)
# autocorrelate ----
# calculate autocorrelation of the SGI time series

# data from above, for response letter to reviewers 2022-05-25
sgi_a <- data %>% filter(!is.na(sgi)) %>% nest(data=c(sgi, date, m),.key="data")


# original:
sgi_a <- sgi %>% unnest_wider('data') %>% unnest() %>% 
  dplyr::select(id, lon, lat, sgi, place) %>% 
  filter(!is.na(sgi)) %>% nest(data=c(sgi),.key="data")



for(i in 1:length(sgi_a$id)) {
  print(i)
  sgi_a$an[[i]]$auto = acf(sgi_a$data[[i]], lag.max = 60, plot=FALSE)$acf
  # fun below <- https://stackoverflow.com/questions/14266333/extract-confidence-interval-values-from-acf-correlogram
  # confidence interval is the same as the level of significant confidence
  # for significant autocorrelation you want the p value (pval) to be higher than the highest level
  # of significance (below)
  sgi_a$an[[i]]$conf = qnorm((1+0.95)/2)/sqrt(length(sgi_a$data[[i]]$sgi))
  sgi_a$an[[i]]$confBM = 2/sqrt(length(sgi_a$data[[i]]$sgi))
}

for(i in 1:length(sgi_a$id)){
  sgi_a$conf[[i]] = sgi_a$an[[i]]$conf
  sgi_a$confBM[[i]] = sgi_a$an[[i]]$confBM
  sgi_a$auto[[i]]$auto = sgi_a$an[[i]]$auto[,,1][1:length(sgi_a$an[[i]]$auto)]
  sgi_a$auto[[i]]$lag = (1:length(sgi_a$an[[i]]$auto) -1 )
} 

sgi_a <- sgi_a  %>% dplyr::group_by(place) %>% 
  unnest_wider('auto') %>% unnest(c('auto', 'lag', 'conf')) %>% 
  ungroup() %>% 
  dplyr::mutate(level = max(conf)) %>% dplyr::group_by(id, lon, lat, place) %>%
  dplyr::select(conf, confBM, level, auto, lag) %>% unnest #%>%
  # mutate(Mmax = ifelse(auto>level & (lead(auto,1) < level | is.na(lead(auto,1))), lag, NA),
         # Mmax=first(na.omit(Mmax)))
saveRDS(sgi_a, "output/process/sgi_autocorr_FennBC_00_05_2.rds")


# autocorrelation using the cross-correlation approach, to enable autocorrelating
# SGI for shorter periods only
# use load data sgi

sgi <- readRDS("output/process/sgi_FennBC.rds")
autofun <- function(y, agg){
  test <- lapply(agg, function (x) {
    data.frame(index = as.numeric(dplyr::lag(y, x)))
    } # the spi function requires a vector input
  )
  return(bind_rows(test, .id="agg") %>% as_tibble())
}
auto <- sgi %>% mutate(sgi_lag = data %>% map("sgi") %>% 
                         map(autofun, 1:48))
p.time <- auto %>% unnest(cols="data") %>% rename(time=date) %>% 
  select(id, lon,lat, place, region, time) %>% 
  mutate(month_past = 1:length(time)) %>% filter(!is.na(time))
auto <- auto %>% unnest(cols="sgi_lag") %>% group_by(id, lon, lat, place, region, agg) %>% 
  mutate(month_past= 1:length(index)) %>% left_join(., p.time)
dat <- auto %>% ungroup() %>% distinct(id, lon, lat, place, region, data) %>% unnest()
auto <- auto %>% select(-month_past, -data) %>% rename(date=time) 
auto <- merge(dat, auto)
auto <- auto %>% nest(lag=c(date, sgi, agg, index))
saveRDS(auto, "output/process/sgi_lag_data_FennBC.rds")

source("rfuncs/cor_indices.R")
auto_corr_ref = auto %>% mutate(Cluster = place) %>% unnest() %>% 
  spearcorr.pval(data=., dofilter=0) %>%
  left_join(., auto %>% distinct(id, lon, lat, place, region))
auto_corr_ref <- auto_corr_ref %>% group_by(id, lon, lat, place) %>%
  mutate(agg=as.numeric(agg)) %>% arrange(id, lon, lat, place, agg) %>%
  mutate(Mmax = ifelse(pval<0.05 & (lead(pval,1) > 0.05 | is.na(lead(corr,1))), agg, NA),
         Mmax=first(na.omit(Mmax)))
saveRDS(auto_corr_ref, "output/process/sgi_auto_wcross_FennBC.rds")




# old autocorrelation code for BC ----
# acorr <- sgi %>% group_by(id) %>% select(id, sgi) %>% filter(!is.na(sgi)) %>% nest()
# for(i in 1:length(acorr$id)) {
#   print(i)
#   acorr$an[[i]]$auto = acf(acorr$data[[i]]$sgi, lag.max = 48, plot=FALSE)$acf
#   # acorr$acorr[[i]]$signval = abs(acorr$acorr[[i]]$acorr)
#   # fun below <- https://stackoverflow.com/questions/14266333/extract-confidence-interval-values-from-acf-correlogram
#   # confidence interval is the same as the level of significant confidence
#   # for significant autocorrelation you want the p value (pval) to be higher than the highest level
#   # of significance (belowe)
#   acorr$an[[i]]$conf = qnorm(1-0.05/2)/sqrt(length(acorr$data[[i]]$sgi))
# }
# a <- acorr %>% select(id)
# for(i in 1:length(acorr$id)){
#   a$conf[[i]] = acorr$an[[i]]$conf
#   a$auto[[i]] = acorr$an[[i]]$auto[1:length(acorr$an[[i]]$auto)]
#   a$agg[[i]] = (1:length(acorr$an[[1]]$auto) -1 )
# } 

# plot autocorrelation heatmaps ----
auto_ref <- readRDS("output/process/sgi_autocorr_FennBC.rds")
# autocorrelation with confidence/significance per well
auto_ref %>% ggplot(.) + geom_tile(aes(lag, id, fill=auto)) + 
  geom_point(aes(lag, id, colour=auto>conf)) + scale_fill_viridis_c(limits=c(-1,1))+ 
  scale_colour_manual(values=c("TRUE"="black", "FALSE" = NA)) +
  facet_grid(place~., space="free", scales="free")

auto_ref %>% filter(id%in%ex$id) %>% 
  ggplot(data=.) + geom_line(aes(lag, auto, group=id)) + 
  geom_hline(aes(yintercept=level), linetype=5, colour="black") +
  facet_grid(id~.) + 
  scale_y_continuous("Autocorrelation", limits=c(-0.4,1), expand=c(0,0)) + 
  scale_x_continuous("lag", limits=c(0,48), expand = c(0, 0)) +
  theme(panel.background = element_rect(colour="lightgrey", fill="white"),
        panel.grid = element_line(colour=alpha("lightgrey",0.5)),
        strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
        axis.ticks = element_line(colour="grey"),
        plot.margin = unit(c(0,0,0,0), "mm")) 

# autocorrelation with minimum confidence/significance  per group
old <- auto_ref %>% ggplot(.) + geom_tile(aes(lag, id, fill=auto)) + 
  geom_point(aes(lag, id, colour=auto>level)) + scale_fill_viridis_c(limits=c(-1,1)) + 
  scale_colour_manual(values=c("TRUE"="black", "FALSE" = NA)) +
  geom_point(data = . %>% filter(Mmax==lag), aes(lag, id), colour="pink", shape=5) +
  facet_grid(place~., space="free", scales="free")

new <- readRDS("output/process/sgi_auto_wcross_FennBC.rds") %>% 
  mutate(agg=as.numeric(agg)) %>% ggplot(.) + geom_tile(aes(agg, id, fill=corr)) + 
  geom_point(aes(agg, id, colour=pval < 0.05)) + scale_fill_viridis_c(limits=c(-1,1)) + 
  scale_colour_manual(values=c("TRUE"="black", "FALSE" = NA)) +
  geom_point(data = . %>% filter(Mmax==agg), aes(agg, id), colour="pink", shape=5) +
  facet_grid(place~., space="free", scales="free")


#compare auto-correlation plot results to cross-correlation method to calculate auto-correlation
auto_ref %>% left_join(., readRDS("output/process/sgi_auto_wcross_FennBC.rds") %>% 
  rename(M2 = Mmax)) %>% 
  distinct(id, place, Mmax, M2) %>% ggplot(.) + geom_point(aes(Mmax, M2, group=id, colour=place)) + 
  scale_colour_viridis_d() + theme_bw() + ylim(0,50) + xlim(0,50)

# identify drought years ----
spi_sgi %>% unnest() %>% filter(year(date) >1980) %>% ggplot(.) + 
  geom_ribbon(aes(x=date, ymin=ifelse(sgi<0,sgi, 0), ymax=0, group=id), fill="grey") + 
  geom_line(aes(date, sgi, group=id)) + 
  facet_wrap(~place, scales = "free")
spi_sgi %>% unnest() %>% filter(year(date) >1990) %>% 
  mutate(sgi=ifelse(sgi<0,sgi, NA),
         date=as.POSIXct(date)) %>%
  ggplot(.) + 
  geom_tile(aes(date, id, fill=sgi)) + 
  scale_fill_viridis_c(option="magma", direction=-1, begin=0.1) +
  facet_grid(place~., scales="free", space="free") + scale_x_datetime(date_breaks = "2 years")

# drought-free period in BC for comparison
spi_sgi %>% unnest() %>% filter(region=="BC") %>% ggplot(.) + 
  geom_ribbon(aes(x=date, ymin=ifelse(sgi<0,sgi, 0), ymax=0, group=id), fill="grey") + 
  geom_line(aes(date, sgi, colour=id)) + 
  facet_wrap(~id,  ncol=1) + 
  geom_vline(data = . %>% ungroup() %>% filter(region=="BC"),
             xintercept = c(as.Date("2010-08-15"),
                            as.Date("2013-01-15")), colour="green", size=1) 
#drought-free period in Fennoscandia for comparison
spi_sgi %>% unnest() %>% filter(region=="Fenn") %>% ggplot(.) + 
  geom_ribbon(aes(x=date, ymin=ifelse(sgi<0,sgi, 0), ymax=0, group=id), fill="grey") + 
  geom_line(aes(date, sgi, colour=id)) + 
  facet_wrap(~id,  ncol=1) + 
  geom_vline(data = . %>% ungroup() %>% filter(region=="Fenn"),
             xintercept = c(as.Date("1993-01-15"),
                            as.Date("1994-06-15")), colour="green", size=1)

spi_sgi %>% unnest() %>% mutate(sgi_0 = ifelse(sgi > 0, NA, sgi)) %>% ggplot(.) + 
  geom_tile(aes(date, id, fill=sgi_0)) + 
  scale_fill_viridis_c(option="magma")
# facet_grid(rows = c(~place), scales = "free_y") 

hm_fen <- spi_sgi %>% unnest() %>% mutate(sgi_0 = ifelse(sgi > 0, NA, sgi),
                                          index_0 = ifelse(index > 0, NA, index),
                                          agg = as.numeric(agg)) %>% 
  arrange(id, place, region, agg) %>% filter(region=="Fenn") %>%
  ggplot(.) + 
  geom_tile(data=. %>% mutate(type="SGI"), aes(date, id, fill=sgi)) + 
  geom_tile(data=. %>% mutate(type="SPI") %>% ungroup() %>%
              distinct(type, date, agg, region, index_0, index) %>% arrange(agg, date), 
            aes(date, agg, fill=index)) +
  scale_fill_viridis_c(option="viridis") + 
  facet_grid(type~., scales = "free") + # space=free
  ggtitle("Fennoscandia") + ylab("")
hm_bc <- spi_sgi %>% unnest() %>% mutate(sgi_0 = ifelse(sgi > 0, NA, sgi),
                                         index_0 = ifelse(index > 0, NA, index),
                                         agg = as.numeric(agg)) %>% 
  arrange(id, place, region, agg) %>% filter(region=="BC") %>%
  ggplot(.) + 
  geom_tile(data=. %>% mutate(type="SGI"), aes(date, id, fill=sgi)) + 
  geom_tile(data=. %>% mutate(type="SPI") %>% ungroup() %>%
              distinct(type, date, agg, region, index_0, index) %>% arrange(agg, date), 
            aes(date, agg, fill=index)) +
  scale_fill_viridis_c(option="viridis") + 
  facet_grid(type~., scales = "free") + # space=free
  ggtitle("BC") + ylab("")

plot_grid(hm_fen + theme(legend.position = "none"), hm_bc + theme(legend.position = "none"))

spi_sgi %>% unnest() %>% mutate(sgi_0 = ifelse(sgi > 0, NA, sgi),
                                index_0 = ifelse(index > 0, NA, index),
                                agg = as.numeric(agg)) %>% 
  arrange(id, place, region, agg) %>% ungroup() %>% filter(region=="BC") %>%
  ggplot(.) + 
  geom_tile(data=. %>% mutate(type="SGI"), aes(date, id, fill=sgi_0)) + 
  scale_fill_viridis_c(option="magma") + #, na.value="NA") +
  
  # geom_vline(data = . %>% ungroup() %>% filter(region=="Fenn"),
  #            xintercept = c(as.Date("2002-05-15"), 
  #                           as.Date("2004-06-15"), 
  #                           as.Date("2007-01-15")), colour="green", size=1) +
  # geom_vline(data = . %>% ungroup() %>% filter(region=="Fenn"), 
  #            xintercept = c(as.Date("1995-08-15"),
  #                           as.Date("1996-06-15"),
  #                           as.Date("1998-08-15")), colour="blue", size=1) +
  
  geom_vline(data = . %>% ungroup() %>% filter(region=="BC"),
             xintercept = c(as.Date("2000-08-15"),
                            as.Date("2005-01-15")), colour="green", size=1) +
  geom_vline(data = . %>% ungroup() %>% filter(region=="BC"),
             xintercept = c(as.Date("1992-05-15"),
                            as.Date("1996-07-15")), colour="blue", size=1) +
  
  facet_grid(region~., scales = "free", space="free") + ylab("") + theme_bw()


# cross-correlate ----
source("rfuncs/cor_indices.R")
# why only Kendall correlations work-ish: https://stackoverflow.com/questions/27047598/r-cor-method-pearson-returns-na-but-method-spearman-returns-value-why

yesno = 0

# #without pval
# cross_corr_ref = spi_sgi %>% #mutate(Cluster = place) %>% 
#   unnest() %>% 
#   spearcorr(data=., dofilter=yesno)
#with pval
cross_corr_ref = spi_sgi %>% #mutate(Cluster = place) %>% 
  unnest() %>% 
  spearcorr.pval(data=., dofilter=yesno) %>%
  left_join(., spi_sgi %>% distinct(id, lon, lat, place, region)) 
cross_corr_ref <- cross_corr_ref %>% 
  group_by(id) %>% 
  mutate(Qmax = ifelse(corr==max(corr), agg, NA),
         Qmax=first(na.omit(Qmax)))
saveRDS(cross_corr_ref, "output/process/cross_corr_ref_FennBC_00_05_2_NOMONTH.rds")

# plot cross-correlations ----
cross_corr_ref %>% 
  ggplot(.) + 
  geom_tile(aes(agg, id, fill=corr)) + 
  scale_fill_viridis_c(limits=c(-1,1)) + 
  # scale_colour_viridis_d(option="magma") +
  geom_point(data=.%>% filter(pval<=0.05), aes(agg, id, colour="<0.05"), shape=19, size=.5)+
  geom_point(data=.%>% filter(corr==max(corr)), aes(agg, id, colour="Qmax"), shape=4, size=1) + 
  scale_colour_manual(values=c("<0.05"="grey", "Qmax"="black"))+
  facet_grid(place~., space="free", scales="free")
  

# # auto- and cross-correlate for different time periods (identified in 'identify drought years') ----
source("rfuncs/cor_indices.R")
source("rfuncs/autocor.r")

## 1st drought, p1 and p2 = normal Fenn, p3 = group 34, p4 and p5 = BC
p1=as.Date("1995-08-15")
p2=as.Date("1997-06-15")
p3=as.Date("1998-08-15")
p4=as.Date("1992-05-15")
p5=as.Date("1996-07-15")

## drought 2
p1=as.Date("2002-05-15")
p2=as.Date("2004-06-15")
p3=as.Date("2007-01-15")
p4=as.Date("2000-08-15")
p5=as.Date("2005-01-15")

## non-drought
p1=as.Date("1992-08-15")
p2=as.Date("1994-08-15")
p3=as.Date("1994-08-15")
p4=as.Date("2010-08-15")
p5=as.Date("2013-01-15")

yesno = 3

## filter different dates for different places, how many filters ----
sgi <- readRDS("output/process/sgi_FennBC.rds")
auto_corr_c <- autocorr(data=sgi, Cluster="place", dofilter=yesno,
                        diffid1="SE_34", y1_start = p1, y1_end = p2, y2_start = p1, y2_end = p3,
                        diffid2="BC Fraser Valley", y3_start = p4, y3_end = p5)
auto_corr_c <- auto_corr_c %>% group_by(place) %>% mutate(level = min(conf)) %>% 
  group_by(id, lon, lat, place, region) %>%
  select(conf, level, auto, lag) %>% unnest %>%
  mutate(Mmax = ifelse(auto>level & (lead(auto,1) < level | is.na(lead(auto,1))), lag, NA),
         Mmax=first(na.omit(Mmax)))
# as above but using the cross correlation and double sgi time series ----
sgi <- readRDS("output/process/sgi_lag_data_FennBC.rds")
auto_corr_a = sgi %>% mutate(Cluster = place) %>% unnest() %>% 
  spearcorr.pval(data=., dofilter=yesno,
                 diffid1="SE_34", y1_start = p1, y1_end = p2, y2_start = p1, y2_end = p3,
                 diffid2 = "BC Fraser Valley", y3_start = p4, y3_end = p5) %>%
  left_join(., sgi %>% distinct(id, lon, lat, place, region))
auto_corr_a <- auto_corr_a %>% group_by(id, lon, lat, place) %>%
  mutate(agg=as.numeric(agg)) %>% arrange(id, lon, lat, place, agg) %>%
  mutate(Mmax = ifelse(pval<0.05 & (lead(pval,1) > 0.05 | is.na(lead(corr,1))), agg, NA),
         Mmax=first(na.omit(Mmax))) %>%  
  mutate(highcorrs = ifelse(corr>max(corr)-0.1, agg, NA)) #%>% filter(!is.na(highcorrs))



# saveRDS(auto_corr_a, "output/process/auto_SEBC_dry90s.rds")
saveRDS(auto_corr_a, "output/process/auto_FennBC_dry00s_cross.rds")

## cross-correlate by drought ----
cross_corr_c = spi_sgi %>% mutate(Cluster = place) %>% unnest() %>% 
  spearcorr.pval(data=., dofilter=yesno,
                 diffid1="SE_34", y1_start = p1, y1_end = p2, y2_start = p1, y2_end = p3,
                 diffid2 = "BC Fraser Valley", y3_start = p4, y3_end = p5) %>%
  left_join(., spi_sgi %>% distinct(id, lon, lat, place, region))
# saveRDS(cross_corr_a, "output/process/cross_SEBC_dry90s.rds")
saveRDS(cross_corr_c, "output/process/cross_FennBC_dry00s.rds")

# plot heatmaps of auto and cross correlations, plot funs ----

auto_ref <- readRDS("output/process/sgi_autocorr_FennBC.rds")
cross_ref <- readRDS("output/process/cross_corr_ref_FennBC.rds")

# ref auto against cross
plot_auto <- function(x, title="reference"){
  plot <- x %>% ggplot(.) + geom_tile(aes(lag, id, fill=auto)) + 
    scale_colour_manual(values=c("TRUE"="black", "FALSE" = NA)) +
    geom_point(data = . %>% filter(Mmax==lag), aes(lag, id), fill="pink",
               colour="black", shape=23) +
    geom_point(aes(lag, id, colour=auto>level), size=.4) + scale_fill_viridis_c(limits=c(0,1),
                                                                                na.value="grey") + 
    facet_grid(landform~., space="free", scales="free") + 
    ggtitle(title) + xlab("auto-corr") + theme(legend.position = "bottom")
  return(plot)
}
plot_cross <- function(x, title="reference"){
  plot <- x %>% 
    ggplot(.) + geom_tile(aes(agg, id, fill=corr)) + 
    scale_colour_manual(values=c("TRUE"="black", "FALSE" = NA)) +
    geom_point(data = . %>% filter(Qmax==agg), aes(agg, id), fill="pink", colour="black", 
               shape=23) +
    geom_point(aes(agg, id, colour=pval<=0.05), size=.4) + scale_fill_viridis_c(limits=c(0,1),
                                                                                na.value="grey") + 
    facet_grid(landform~., space="free", scales="free") + ggtitle(title) +
    xlab("cross-corr") + theme(legend.position = "bottom")
  return(plot)
}
plot_auto_wcross <- function(x, title="reference"){
  plot <- x %>% ggplot(.) + geom_tile(aes(lag, id, fill=auto)) + 
    scale_colour_manual(values=c("TRUE"="black", "FALSE" = NA)) +
    geom_point(data = . %>% filter(Mmax==lag), aes(lag, id), fill="pink",
               colour="black", shape=23) +
    geom_point(aes(lag, id, colour=pval<0.05), size=.4) + scale_fill_viridis_c(limits=c(0,1),
                                                                               na.value = "grey") + 
    facet_grid(landform~., space="free", scales="free") + 
    ggtitle(title) + xlab("auto-corr") + theme(legend.position = "bottom")
  return(plot)
}
 

auto_old <- auto_ref %>% 
  filter(id!="Haapajärvi_1002p5"& id!="Haapajärvi_1002p7" &
           id != "Pyhäntä_1101p15" & id != "Pyhäntä_1101p4") %>% 
  left_join(., level_stats%>% dplyr::select(-lon, -lat)) %>%
  plot_auto(., title="reference")
cross_old <- cross_ref %>% 
  filter(id!="Haapajärvi_1002p5"& id!="Haapajärvi_1002p7" &
           id != "Pyhäntä_1101p15" & id != "Pyhäntä_1101p4") %>% 
  group_by(id) %>% 
  mutate(Qmax = ifelse(corr==max(corr), agg, NA),
         Qmax=first(na.omit(Qmax)))%>%
  left_join(., level_stats %>% dplyr::select(-lon, -lat)) %>%
  plot_cross(., title="reference") 

library(cowplot)
plot_grid(plot_grid(auto_old + theme(legend.position = "none", plot.title = element_text(size=9),
                                     text = element_text(size=9)), 
          cross_old + theme(legend.position = "none", plot.title = element_text(size=9),
                            text = element_text(size=9))),
          get_legend(cross_old), rel_heights = c(2.7,0.3), rel_widths = c(1,1), rows = 2) 

## shorter periods (droughts and non-droughts)

drytext1 = "1990's drought"
drytext2 = "2000's drought"
drytext3 = "non-drought period"

# auto_b <- readRDS("output/process/auto_SEBC_dry90s.rds") %>% 
#   full_join(., auto_ref %>% distinct(id, place, region) %>%
#               filter(!id %in% .$id)) %>% 
#   plot_auto(., title=drytext1)

auto_b <- readRDS("output/process/auto_FennBC_non-dry90s_cross.rds") %>% 
  filter(id!="Haapajärvi_1002p5"& id!="Haapajärvi_1002p7" &
           id != "Pyhäntä_1101p15" & id != "Pyhäntä_1101p4") %>% 
  rename(lag=agg, auto=corr) %>%
  mutate(auto=ifelse(auto<0.09, NA, auto)) %>%
  # full_join(auto_ref %>% mutate(auto=NA, Mmax=NA), .) %>%
  left_join(., level_stats %>% dplyr::select(-lon, -lat)) %>% 
  plot_auto_wcross(., title=drytext3)

cross_b <- readRDS("output/process/cross_FennBC_non-dry90s.rds") %>% 
  filter(id!="Haapajärvi_1002p5"& id!="Haapajärvi_1002p7" &
           id != "Pyhäntä_1101p15" & id != "Pyhäntä_1101p4") %>% 
  group_by(id) %>% 
  mutate(agg=as.numeric(agg),
    Qmax = ifelse(corr==max(corr), agg, NA),
         Qmax=first(na.omit(Qmax)), corr=ifelse(corr<0.09, NA, corr)) %>% 
  left_join(., level_stats %>% dplyr::select(-lon, -lat)) %>%
  # full_join(auto_ref %>% mutate(corr=NA, Qmax=NA), .) %>%
  plot_cross(., title=drytext3)
library(cowplot)
plot_grid(auto_b + theme(legend.position = "none", plot.title = element_text(size=9),
                         text = element_text(size=9)), 
          cross_b + theme(legend.position = "none", plot.title = element_text(size=9),
                          text = element_text(size=9)))


readRDS("output/process/auto_SEBC_non-dry90s_cross.rds") %>% rename(lag=agg, auto=corr) %>% 
  mutate(auto=ifelse(auto<0.09, NA, auto)) %>%
  # filter(place=="SE_34") %>% 
  ggplot(.) + geom_tile(aes(lag, id, fill=auto)) + 
  scale_colour_manual(values=c("TRUE"="black", "FALSE" = NA)) +
  geom_point(data = . %>% filter(Mmax==lag), aes(lag, id), fill="pink",
             colour="black", shape=23) +
  geom_point(aes(lag, id, colour=pval<0.05), size=.4) + scale_fill_viridis_c(limits=c(-1,1),
                                                                             na.value = "grey") + 
  facet_grid(place~., space="free", scales="free")


# load data for cross-auto plots  ----
# auto_ref <- readRDS("output/process/sgi_autocorr_FennBC.rds")
# cross_ref <- readRDS("output/process/cross_corr_ref_FennBC.rds")
# # auto_a <- readRDS("output/process/auto_FennBC_dry90s.rds")
# cross_a <- readRDS("output/process/cross_FennBC_dry90s.rds")
# # auto_b <- readRDS("output/process/auto_FennBC_dry00s.rds")
# cross_b <- readRDS("output/process/cross_FennBC_dry00s.rds")
# # auto_c <- readRDS("output/process/auto_FennBC_non-dry90s.rds")
# cross_c <- readRDS("output/process/cross_FennBC_non-dry90s.rds")
# auto <- readRDS("output/process/sgi_auto_wcross_FennBC.rds")
# autowc_a <- readRDS("output/process/auto_FennBC_dry90s_cross.rds")
# autowc_b <- readRDS("output/process/auto_FennBC_dry00s_cross.rds")
# autowc_c <- readRDS("output/process/auto_FennBC_non-dry90s_cross.rds")

name <- function(data) {
  new <- data %>%  
    # filter(id!="Haapajärvi_1002p5"& id!="Haapajärvi_1002p7" &
             # place != "FI_Pyhäntä") %>%
    group_by(id) %>%
    left_join(.,s.name) %>% ungroup() %>%
    # mutate(id = ifelse(!is.na(Namn), Namn, id)) %>%
    mutate(id = ifelse(grepl("SE", place)==TRUE, Namn, id)) %>% 
    dplyr::select(-Namn)
  return(new)
}

cross_auto <- 
  #readRDS("output/process/sgi_autocorr_FennBC.rds") %>% 
  readRDS("output/process/sgi_autocorr_FennBC_00_05_2.rds")%>% 
  dplyr::select(-lon,-lat) %>%
  #rename(agg = lag) %>% 
  ungroup() %>%
  mutate(level=max(conf)) %>%
  left_join(., #readRDS("output/process/cross_corr_ref_FennBC.rds") %>% 
              readRDS("output/process/cross_corr_ref_FennBC_00_05_2.rds") %>%
              name(.)%>% 
              dplyr::select(-N,-E,-lon,-lat)
              # readRDS("output/process/cross_corr_ref_FennBC_00_05_2_NOMONTH.rds")
            )  %>%
  group_by(id) %>%
  mutate(Qmax = ifelse(max(corr, na.rm=TRUE)==corr, agg, NA),
  Qmax=first(na.omit(Qmax)),
  maxcorr = ifelse(Qmax==agg, corr, NA),
  Arange = ifelse(auto>level & (lead(auto,1) < level | is.na(lead(auto,1))), lag, NA),
  Arange=first(na.omit(Arange)))
#find auto range peaks
# library(pracma); library(tidyverse)
# peaks <- cross_auto %>% 
#   split(.$id) %>%
#     map(~findpeaks(.$auto, nups=1,  minpeakdistance = 3)) 
# peaks <- peaks[-which(sapply(peaks, is.null))]
# peaks <- peaks %>%
#   map_df(~tibble(peak = .x[,1],
#                    peak_mid = .x[,2], 
#                    peak_start = .x[,3],
#                    peak_end= .x[,4]),
#        .id="id")
# cross_auto <- cross_auto %>% left_join(., peaks)
cross_auto <- cross_auto %>% group_by(id) %>%
  mutate(dif1 = auto-lag(auto,1),
         dif1_c = ifelse(dif1 < 0.05 & dif1 >-0.05 & agg >3, 0, sign(dif1)),
         cp.auto = ifelse(dif1_c >=0, agg-1, NA), cp.auto=first(na.omit(cp.auto)),
         memory= ifelse(auto<0.2, lag-1, NA), 
         memory= ifelse(is.na(memory) & is.na(lead(auto,1)), lag, memory), 
         memory=first(na.omit(memory)),
         dif1.corr = corr-lag(corr,1),
         # dif1.corr_c = ifelse(dif1.corr < 0.01 & dif1.corr >-0.01, 0, sign(dif1.corr)),
         cp.corr = ifelse(!is.na(dif1.corr) & corr>0 & dif1.corr<=0 & agg>2 & !is.na(agg), agg-1, NA), 
         cp.corr=first(na.omit(cp.corr)),
         rt = ifelse(max(corr, na.rm=TRUE)==corr, agg, NA), 
         rt = first(na.omit(rt)), rt = min(cp.corr, rt, na.rm=TRUE))
         # memory = min(memory, cp.auto, na.rm=TRUE))
#check results in plot:
cross_auto  %>% ggplot() +
  geom_vline(aes(xintercept=rt), linetype=4, size=.5, col="grey") +
  geom_rect(aes(xmin = Qmax, ymin = -Inf, xmax = rt, ymax = Inf), alpha=0.04, fill="lightgrey") +
  geom_vline(aes(xintercept=Qmax), linetype=4, size=.5, col="grey50") +

  geom_line(aes(agg, dif1.corr*10), col="blue", linetype=2) +
  geom_line(data= . %>% distinct(id, place, corr, agg),
            aes(agg, corr, group=id)) +
  geom_point(data= . %>% filter(Qmax==agg),
             aes(Qmax, corr, group=id), col="black", shape=4, size=2) +
  geom_text(data=. %>% filter(Qmax==agg),
            size=2.5,  aes(Qmax, corr, label=Qmax), nudge_x = -0.6, nudge_y=0.2) +
  geom_text(data=. %>% filter(rt==agg),
            size=2.5,  aes(rt, corr, label=rt), nudge_x = -0.6, nudge_y=0.2, colour="red") +
  facet_wrap(~id) + scale_y_continuous(sec.axis = sec_axis(~./10), limit=c(-1,1)) +
  geom_hline(yintercept=0.01*10, colour="red") + geom_hline(yintercept=0.0*10, colour="green")

library(broom)
slope1 <- cross_auto %>% group_by(id) %>% 
  filter(lag<=min(Arange, cp.auto, na.rm=TRUE)) %>% 
  do(model=tidy(lm(auto~lag, data=.)))
for(i in 1:length(slope1$id)){
  slope1$slope1[i] = slope1$model[[i]]$estimate[2]
  slope1$intercept1[i] = slope1$model[[i]]$estimate[1]
}
cross_auto <- cross_auto %>% left_join(., slope1 %>% distinct(id, slope1, intercept1))

# slope2 <- cross_auto %>% group_by(id) %>% 
#   filter(agg>=min(memory, cp.auto, na.rm=TRUE)) %>% 
#   do(model=tidy(lm(auto~agg, data=.)))
# for(i in 1:length(slope2$id)){
#   slope2$slope2[i] = slope2$model[[i]]$estimate[2]
#   slope2$intercept2[i] = slope2$model[[i]]$estimate[1]
# }
# cross_auto <- cross_auto %>% left_join(., slope2 %>% distinct(id, slope2, intercept2))

#still using auto threshold, not the 'steps' as roland wished
saveRDS(cross_auto, "output/process/cross_auto_refperiod_00_05_2.rds")


level <- min(cross_auto$level)

cross_auto_a <- readRDS("output/process/auto_FennBC_dry90s_cross.rds") %>%
  rename(auto=corr) %>% dplyr::select(-tresh, -pval) %>% 
  left_join(., readRDS("output/process/cross_FennBC_dry90s.rds") %>% 
              mutate(agg=as.numeric(agg))) %>% mutate(level=level) %>% name(.)

cross_auto_b <-  readRDS("output/process/auto_FennBC_dry00s_cross.rds") %>%
  rename(auto=corr) %>% dplyr::select(-tresh, -pval) %>%
  left_join(., readRDS("output/process/cross_FennBC_dry00s.rds") %>% 
              mutate(agg=as.numeric(agg))) %>% mutate(level=level) %>% name(.)

cross_auto_c <- readRDS("output/process/auto_FennBC_non-dry90s_cross.rds") %>%
  rename(auto=corr) %>% dplyr::select(-tresh, -pval) %>% 
  left_join(., readRDS("output/process/cross_FennBC_non-dry90s.rds") %>% 
              mutate(agg=as.numeric(agg))) %>% mutate(level=level) %>% name(.)

# compare auto- to cross-correlations using time series and example wells ----
ex <- cross_auto %>% distinct(id) %>% 
  filter(id == "Haapajärvi_1002p9" |id == "BC_ow301" |id == "Skellefteå_12")
ex_ind <- ezra_indices%>%
  left_join(.,s.name) %>% ungroup() %>%
  mutate(id = ifelse(!is.na(Namn), Namn, id)) %>%
  select(-Namn) %>% filter(id %in% ex$id)


ts.plot <- function(data){
  data %>% 
    ggplot(data=.) + 
    geom_line(data =. %>% mutate(type="Auto"), aes(agg, auto, group=id)) + 
    geom_line(data =. %>% mutate(type="Cross"), aes(agg, corr, group=id)) + 
    geom_hline(data =. %>% mutate(type="Auto"), 
               aes(yintercept=level), linetype=5, colour="black") +
    geom_vline(data =. %>% mutate(type="Cross"), 
               aes(xintercept=Qmax, group=id), linetype=5, colour="black") +
    
    geom_text(data=. %>% filter(agg==Mmax) %>% distinct(id, Mmax, auto, level) %>% 
                mutate(type="Auto"), size=2.5,
              aes(y=level, x=Mmax, label=Mmax, group=id), hjust = -1.1, vjust  = -0.5) + 
    geom_text(data=. %>% distinct(id, Qmax, maxcorr) %>% 
                filter(!is.na(maxcorr) & maxcorr<0.7) %>%
                mutate(type="Cross"), size=2.5, 
              aes(y=maxcorr, x=Qmax, label=Qmax, group=id), hjust = 1.1, vjust  = -0.5) + 
    geom_text(data=. %>% distinct(id, Qmax, maxcorr) %>% 
                filter(!is.na(maxcorr) & maxcorr>0.7) %>%
                mutate(type="Cross"), size=2.5, 
              aes(y=maxcorr, x=Qmax, label=Qmax, group=id), hjust = 1.1, vjust  = 1) + 
    
    facet_grid(type~id) + 
    scale_y_continuous("correlation", limits=c(-1,1), expand=c(0,0)) + 
    scale_x_continuous("months", limits=c(1,48), expand = c(0, 1)) +
    theme(panel.background = element_rect(colour="lightgrey", fill="white"),
          panel.grid = element_line(colour=alpha("lightgrey",0.5)),
          strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
          axis.ticks = element_line(colour="grey"),
          text = element_text(size=8),
          plot.margin = unit(c(1,0,0,0), "mm"))
}
a <- cross_auto %>% filter(id%in%ex$id) %>% ts.plot(.) 
b <- cross_auto_a %>% 
  filter(id%in%ex$id) %>% ts.plot(.) 
c <- cross_auto_b %>% 
  filter(id%in%ex$id) %>% ts.plot(.) 
d <- cross_auto_c %>% 
  filter(id%in%ex$id) %>% ts.plot(.) 
drytext1 = "drought 1"
drytext2 = "drought 2"
drytext3 = "non-drought period"
plot_grid(a+ggtitle("a. record"),b+ggtitle(paste("b.",drytext1)),
          c+ggtitle(paste("c.",drytext2)),d+ggtitle(paste("d.",drytext3)), cols = 1)


# plot sgi with sgi lags
sgi <- readRDS("output/process/sgi_lag_data_FennBC.rds") %>%
  left_join(.,s.name) %>% ungroup() %>%
  mutate(id = ifelse(!is.na(Namn), Namn, id)) %>%
  select(-Namn) %>% filter(id %in% ex$id) 
m <- spi_sgi%>%
  left_join(.,s.name) %>% ungroup() %>%
  mutate(id = ifelse(!is.na(Namn), Namn, id)) %>%
  select(-Namn) %>% filter(id %in% ex$id) %>% 
  unnest() %>%  
  distinct(id, lon, lat, place, region, date, m)
sgi <- sgi %>% unnest() %>% mutate(agg=as.numeric(agg)) %>% filter(agg==6 |
                                                              agg==12 |agg==48)

sgi %>% group_by(id, agg) %>% ggplot(.) +
  geom_line(data=m %>% distinct(id, date, m) %>% group_by(id) %>%
              mutate(type=NA,min=min(m), max=max(m), m=m-max/max-min),
            aes(date, m)) +
  
  geom_line(data=. %>% distinct(id, date, sgi) %>% mutate(type=0), 
            aes(date, sgi)) + 
  geom_line(data=. %>% distinct(id, date, index, agg) %>% mutate(type=agg), 
            aes(date, index, group=agg)) + 
  
  facet_grid(type~id) + 
  scale_y_continuous("SI", limits=c(-3,4), expand=c(0,0)) + 
  scale_x_date("date", limits=c(as.Date("1990-01-01"),as.Date("2005-01-01")), 
                                expand = c(0, 0)) +
  theme(panel.background = element_rect(colour="lightgrey", fill="white"),
        panel.grid = element_line(colour=alpha("lightgrey",0.5)),
        strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
        axis.ticks = element_line(colour="grey"),
        text = element_text(size=8),
        plot.margin = unit(c(1,0,0,0), "mm"))




# plot cross and auto corr ts ----
cross_auto <- readRDS("output/process/cross_auto_refperiod_00_05_2.rds") %>%
  ungroup() %>% 
  dplyr::mutate(level=max(conf)) %>% group_by(id)  %>%
  dplyr::mutate(memory = ifelse(auto>level & (lead(auto,1) < level | is.na(lead(auto,1))), lag, NA),
                memory=first(na.omit(memory)))
  # readRDS("output/process/cross_auto_refperiod_00_05_2_NOMONTH.rds")
# cross_auto <- cross_auto %>% 
# filter(case_when(grepl("BC", id)==FALSE ~ id %in% find$id, 
# grepl("BC", id)==TRUE ~ !id %in% find$id)) 
ac_plot <- function(data, type="auto", n="ex") {
if(type=='auto'){
  if(n=='ex'){
    ggplot(data) + 
      geom_hline(yintercept=0.173, linetype="dashed", col="black", size=0.5) +
      # geom_hline(yintercept=0, linetype=1, col="black", size=0.5) +
      # geom_vline(aes(xintercept=memory), linetype=4, size=.5) +
      # geom_abline(data=. %>% distinct(id, slope2, intercept2, place, memory),
      #  aes(slope=slope2, intercept=intercept2, col="black"), linetype=3) +
      # geom_point(data= . %>% filter(cp.auto==agg),
      #   aes(cp.auto, auto, group=id), col="green", shape=1, size=2) +
      
      geom_abline(data=. %>% distinct(id, slope1, intercept1, place, memory,mat),
                  aes(slope=slope1, intercept=intercept1), col="blue", linetype="longdash") +
      geom_point(data= . %>% filter(memory==lag)%>% distinct(memory, auto, id, lag),
                 aes(memory, auto, group=id, shape="memory"), col="black", size=2) +
      geom_line(data= . %>% distinct(id, place, auto, lag, confinement, dist_stream, landform, surface.material, 
                                     med_med, aquifer.material,mat),
                aes(lag, auto, group=id),size=0.5) +
      geom_text(data=. %>% filter(memory==lag) %>% distinct(memory, auto, id, lag),
                size=2.5,  aes(memory, auto, label=memory), nudge_x = 0.7, nudge_y=0.2) +
      
      # guides(colour = "none") +
      scale_colour_manual(values=c("white","black")) +
      scale_x_continuous("lag [months]", expand=c(0.01,0), limits=c(0,60), breaks=c(0,10,20,30,40,50)) +
      scale_y_continuous("ACF", limits = c(-0.5,1.0), expand=c(0.1,0))+
      scale_shape_manual("", values = c('memory'=4)) +
      # facet_grid(rows=vars(id), cols=vars(loc)) + 
      facet_grid(cols=vars(id), 
        rows=vars(mat))
  } else if(n=='all'){
    ggplot(data %>% distinct(id, place, auto, lag, confinement, dist_stream, landform, surface.material,
                            med_med, aquifer.material, mat) %>%
             ungroup() %>% group_by(lag) %>%
             dplyr::mutate(max = max(auto), min=min(auto),
                           std= sd(auto), median=median(auto))) + 
      geom_hline(yintercept=0.173, linetype="dashed", col="black", size=0.5) +
      # geom_hline(yintercept=0, linetype=1, col="black", size=0.5) +
      # geom_vline(aes(xintercept=memory), linetype=4, size=.5) +
      # geom_abline(data=. %>% distinct(id, slope2, intercept2, place, memory),
      #  aes(slope=slope2, intercept=intercept2, col="black"), linetype=3) +
      # geom_point(data= . %>% filter(cp.auto==agg),
      #   aes(cp.auto, auto, group=id), col="green", shape=1, size=2) +
      
      geom_ribbon(aes(ymin=min, ymax=max, x=lag, alpha='range,'), fill='grey') +
      geom_ribbon(aes(ymin=median-std, ymax=median+std, x=lag, alpha='sd'), fill='grey') +
      geom_line(aes(y=median, x=lag), col='black',size=0.5) +
      
      guides(colour = "none") +
      scale_alpha_manual('correlation:', values=c('range,'=0.2, 'sd'=0.5))+
      scale_colour_manual(values=c("white","black")) +
      scale_x_continuous("lag [months]", expand=c(0,0), limits=c(0,60), breaks=c(0,10,20,30,40,50)) +
      scale_y_continuous("ACF", limits = c(-0.5,1.0), expand=c(0.1,0))+
      scale_shape_manual("", values = c('memory'=4)) +
      # facet_grid(rows=vars(id), cols=vars(loc)) + 
      facet_grid(rows=vars(mat))
  }
    
} 
  else if(type=="cross"){
    if(n=='ex'){
      ggplot(data) + 
        # geom_rug(aes(x=agg, y=-Inf, colour=dif1_c), size=2) +
        geom_hline(yintercept=0, linetype=3, col="black", size=0.5) +
        # geom_vline(aes(xintercept=rt), linetype=4, size=.5, col="grey") +
        # geom_rect(aes(xmin = Qmax, ymin = -Inf, xmax = rt, ymax = Inf), alpha=0.01, fill="lightgrey") +
        # geom_vline(aes(xintercept=Qmax), linetype=4, size=.5, col="grey50") +
        
        # geom_line(aes(agg, dif1.corr), col="blue", linetype=2) +
        
    
      geom_line(data= . %>% dplyr::distinct(id, place, corr, agg, confinement, dist_stream, landform, surface.material,
                                     med_med, aquifer.material, mat),
                aes(agg, corr, group=id),size=0.5) +
      geom_point(data= . %>% dplyr::filter(Qmax==agg)%>% dplyr::distinct(id, Qmax, corr,agg),
                 aes(Qmax, corr, group=id, shape="max. correlation"),
                 col="black", size=2) +
      geom_rug(aes(x=agg, col=pval<.05, group=id), sides = 'b') +
      geom_text(data=. %>% dplyr::filter(Qmax==agg)%>% dplyr::distinct(id, Qmax, corr,agg),
                size=2.5,  aes(Qmax, corr, label=Qmax), nudge_x = -0.8, nudge_y=0.2) +

        geom_point(data= . %>% filter(rt==agg)%>% dplyr::distinct(id, rt, corr, agg),
        aes(rt, corr, group=id, shape='correlation slope'), size=3) +
        geom_text(data=. %>% filter(rt==agg)%>% dplyr::distinct(id, rt, corr, agg),
        size=2.5,  aes(rt, corr, label=rt), nudge_x = -0.6, nudge_y=0.2) +
      
      
      scale_shape_manual("response time by: ", values=c('max. correlation'=4,
                                                        'correlation slope'=1))+
        guides(colour = "none") +
        # scale_color_viridis_d(na.value = "green1")) +
        scale_colour_manual(values=c("white","black")) +
        scale_x_continuous("accumulation period [months]", expand=c(0.01,0), limits=c(1,60), breaks=c(1,10,20,30,40,50)) +
        scale_y_continuous("CCF", limits = c(-0.5,1.0), expand=c(0.1,0))+
        # geom_text(data=. %>% filter(!is.na(peak_mid)) %>% distinct(id, peak_mid, peak, level),
        # size=2.5,  aes(y=peak, x=peak_mid, label=peak_mid, col=id)) +
        # facet_wrap(~id) + 
        # facet_grid(rows=vars(id), cols=vars(loc)) + 
        facet_grid(cols=vars(id), 
          rows=vars(mat))
      
    } else if(n=='all'){
      ggplot(data %>% dplyr::distinct(id, place, corr, agg, confinement, dist_stream, landform, surface.material,
                                     med_med, aquifer.material, mat) %>%
               dplyr::ungroup() %>% dplyr::group_by(agg) %>%
               dplyr::mutate(median=median(corr),
                             max = max(corr), min=min(corr),
                             std= sd(corr))) + 
        # geom_rug(aes(x=agg, y=-Inf, colour=dif1_c), size=2) +
        geom_hline(yintercept=0, linetype=3, col="black", size=0.5) +
        # geom_vline(aes(xintercept=rt), linetype=4, size=.5, col="grey") +
        # geom_rect(aes(xmin = Qmax, ymin = -Inf, xmax = rt, ymax = Inf), alpha=0.01, fill="lightgrey") +
        # geom_vline(aes(xintercept=Qmax), linetype=4, size=.5, col="grey50") +
        
        # geom_line(aes(agg, dif1.corr), col="blue", linetype=2) +
        geom_ribbon(aes(ymin=min, ymax=max, x=agg, alpha='range,'), fill='grey') +
        geom_ribbon(aes(ymin=median-std, ymax=median+std, x=agg, alpha='sd'), fill='grey') +
        geom_line(aes(y=median, x=agg), col='black',size=0.5) +
      
      scale_shape_manual("response time by: ", values=c('max. correlation'=4,
                                                        'correlation slope'=1))+
        guides(colour = "none") +
        # scale_color_viridis_d(na.value = "green1")) +
        scale_alpha_manual('correlation:', values=c('range,'=0.2, 'sd'=0.5))+
        scale_colour_manual(values=c("white","black")) +
        scale_x_continuous("accumulation period [months]", expand=c(0,0), limits=c(1,60), breaks=c(1,10,20,30,40,50)) +
        scale_y_continuous("CCF", limits = c(-0.5,1.0), expand=c(0.1,0))+
        # geom_text(data=. %>% filter(!is.na(peak_mid)) %>% distinct(id, peak_mid, peak, level),
        # size=2.5,  aes(y=peak, x=peak_mid, label=peak_mid, col=id)) +
        # facet_wrap(~id) + 
        # facet_grid(rows=vars(id), cols=vars(loc)) + 
       
        facet_grid( rows=vars(mat))
    }
  
}
}

x <- cross_auto %>% 
 right_join(., chars %>% distinct(id, confinement, dist_stream, landform, surface.material, 
                                med_med, aquifer.material)) %>%
###
  ungroup() %>% dplyr::mutate(loc=gsub("\\_.*", "", id),
                      loc=ifelse(loc=="BC", "Simon Fraser valley", loc),
                      id=ifelse(grepl("SE", place) | grepl("BC", place),
                                as.numeric(gsub(".*_", "", id)), id),
                      id=ifelse(grepl("FI", place),
                                as.numeric(gsub(".*p", "", id)), id)) %>%
  dplyr::filter((id==17 & place=="SE_19" )|
           ((id==2 | id==259) & place=="BC Fraser Valley" )|
           (id==10 & place=="FI_Valkeala" )|
           ((id==1 | id== 5) & grepl("FI_K", place)==T )|
           (id==8 & place=="FI_Alavus Taipale" )|
           (id==10 & place=="SE_39" )) %>%
###
  dplyr::mutate(confinement = as.character(confinement),
         confinement = ifelse(confinement =="confined" | 
                                grepl("potentially confined", confinement)==TRUE,
                              "confined", confinement),
         confinement = ifelse(grepl("unconfined", confinement)==TRUE,
                              "unconfined", confinement),
         confinement = ifelse(grepl("silt", aquifer.material)==TRUE,
                              "aquiclude", confinement),
         confinement = ifelse(grepl("till", aquifer.material)==TRUE & is.na(confinement),
                              "aquiclude", confinement),
         confinement = ifelse(grepl("sand", aquifer.material)==TRUE & is.na(confinement),
                              "confined", confinement)) %>%
  dplyr::mutate(mat = ifelse(aquifer.material=="sand" & confinement=="confined", paste(aquifer.material, "*", sep=""),
                      paste(aquifer.material))) %>%
  # filter(aquifer.material=="sand" & grepl("unconfined", confinement)==T &
  #          med_med > 3) %>%
  dplyr::mutate(place=ifelse(place=="SE_19", "SE_Brattforsheden", place),
         place=ifelse(place=="SE_39", "SE_Abisko", place),
         id=paste(place, id, sep="_")) %>% group_by(id, place) %>% ungroup() %>%
  dplyr::mutate(id = ifelse(grepl("Bratt", id), "B17", id),
         id = ifelse(grepl("259", id), "FV259", id),
         id = ifelse(grepl("_2", id), "FV2", id),
         id = ifelse(grepl("Valk", id), "V10", id),
         id = ifelse(grepl("ä_1", id), "K1", id),
         id = ifelse(grepl("ä_5", id), "K5", id),
         id = ifelse(grepl("Alavus", id), "AT8", id),
         id = ifelse(grepl("Abisko", id), "A10", id))
not_x <- cross_auto %>% 
    right_join(., chars %>% distinct(id, confinement, dist_stream, landform, surface.material, 
                                     med_med, aquifer.material)) %>%
  dplyr::mutate(confinement = as.character(confinement),
                confinement = ifelse(confinement =="confined" | 
                                       grepl("potentially confined", confinement)==TRUE,
                                     "confined", confinement),
                confinement = ifelse(grepl("unconfined", confinement)==TRUE,
                                     "unconfined", confinement),
                confinement = ifelse(grepl("silt", aquifer.material)==TRUE,
                                     "aquiclude", confinement),
                confinement = ifelse(grepl("till", aquifer.material)==TRUE & is.na(confinement),
                                     "aquiclude", confinement),
                confinement = ifelse(grepl("sand", aquifer.material)==TRUE & is.na(confinement),
                                     "confined", confinement)) %>%
    mutate(mat = ifelse(aquifer.material=="sand" & confinement=="confined", paste(aquifer.material, "*", sep=""),
                        paste(aquifer.material))) 

my_theme <-  theme(panel.background = element_rect(colour="lightgrey", fill="white"),
                   panel.grid = element_line(colour=alpha("lightgrey",0.5)),
                   # strip.background = element_rect(fill=alpha("white", 0.5), colour="white"),
                   # strip.text = element_text(size=8),
                   strip.background = element_blank(), strip.text=element_blank(),
                   axis.ticks = element_line(colour="grey"),
                   axis.title=element_text(size=8),
                   legend.key = element_blank(),
                   plot.margin = unit(c(0,1,0,0), "mm"),
                   axis.text = element_text(size=8))
library(gridExtra); library(grid)
nr='auto' 
csand <- grid.arrange(
  arrangeGrob(
    x %>% filter(mat=="sand*") %>% 
    ac_plot(data=., type=nr, n='ex') +
    geom_text(data=. %>% mutate(id='FV259') %>% distinct(id),
            aes(-Inf,Inf, label='(a)'), hjust=-0.5, vjust=1)+
    geom_text(data=. %>% mutate(id='V10') %>% distinct(id),
              aes(-Inf,Inf, label='(b)'), hjust=-0.5, vjust=1) + 
      my_theme + 
    theme(#axis.title = element_blank(), 
      axis.title.x = element_blank(), 
          legend.position = 'none')),
  arrangeGrob(
    not_x %>% filter(mat=="sand*") %>% 
      ac_plot(data=., type=nr, n='all') +
      geom_text(data=. %>% mutate(id='FV259') %>% ungroup() %>% distinct(id), 
                aes(-Inf,Inf, label='(c)'), hjust=-0.5, vjust=1)+ 
      my_theme +
      theme(axis.title = element_blank(), axis.text.y = element_text(colour='white'),
            legend.position = 'none')),
  nrow=1, widths=c(2,1),
  right=textGrob('confined sand', gp=gpar(fontsize=8), rot=0))
unsand <- grid.arrange(
  arrangeGrob(
    x %>% filter(mat=="sand") %>% 
      ac_plot(data=., type=nr, n='ex') +
      geom_text(data=. %>% mutate(id='B17') %>% distinct(id),
                aes(-Inf,Inf, label='(d)'),  hjust=-0.5, vjust=1)+
      geom_text(data=. %>% mutate(id='FV2') %>% distinct(id),
                aes(-Inf,Inf, label='(e)'),  hjust=-0.5, vjust=1) +
      my_theme +
      theme(#axis.title = element_blank(), 
        axis.title.x = element_blank(), 
            legend.position = 'none')),
  arrangeGrob(
    not_x %>% filter(mat=="sand") %>% 
      ac_plot(data=., type=nr, n='all') +
      geom_text(data=. %>% mutate(id='B17') %>% ungroup()%>% distinct(id), 
    aes(-Inf,Inf, label='(f)'),  hjust=-0.5, vjust=1)+ 
      my_theme +
      theme(axis.title = element_blank(), axis.text.y = element_text(colour='white'),
            legend.position = 'none')),
  nrow=1, widths=c(2,1),
  right=textGrob('unconfined sand', gp=gpar(fontsize=8), rot=0))
silt <- grid.arrange(
  arrangeGrob(
    x %>% filter(mat=="silt") %>% 
      ac_plot(data=., type=nr, n='ex') +
      geom_text(data=. %>% mutate(id='K1') %>% distinct(id),
                aes(-Inf,Inf, label='(g)'), hjust=-0.5, vjust=1)+
      geom_text(data=. %>% mutate(id='K5') %>% distinct(id),
                aes(-Inf,Inf, label='(h)'), hjust=-0.5, vjust=1) + 
      my_theme +
      theme(#axis.title = element_blank(), 
        axis.title.x = element_blank(), 
        legend.position = 'none')),
  arrangeGrob(
    not_x %>% filter(mat=="silt") %>% 
      ac_plot(data=., type=nr, n='all') +
      geom_text(data=. %>% mutate(id='K1') %>% ungroup()%>% distinct(id), 
                aes(-Inf,Inf, label='(i)'), hjust=-0.5, vjust=1)+ 
      my_theme +
      theme(axis.title = element_blank(), axis.text.y = element_text(colour='white'),
            legend.position = 'none')),
  nrow=1, widths=c(2,1),
  right=textGrob('silt', gp=gpar(fontsize=8), rot=0))
till <- grid.arrange(
  arrangeGrob(
    x %>% filter(mat=="till") %>% 
      ac_plot(data=., type=nr, n='ex') +
      geom_text(data=. %>% mutate(id='A10') %>% distinct(id),
                aes(-Inf,Inf, label='(j)'), hjust=-0.5, vjust=1)+
      geom_text(data=. %>% mutate(id='AT8') %>% distinct(id),
                aes(-Inf,Inf, label='(k)'), hjust=-0.5, vjust=1) + 
      my_theme +
      theme(#axis.title = element_blank(), 
        axis.title.x = element_blank(), 
            legend.position = 'none')),
  arrangeGrob(
    not_x %>% filter(mat=="till") %>% 
      ac_plot(data=., type=nr, n='all') +
      geom_text(data=. %>% mutate(id='A10') %>% ungroup()%>% distinct(id), 
                aes(-Inf,Inf, label='(l)'), hjust=-0.5, vjust=1)+ 
      my_theme +
      theme(axis.title = element_blank(), axis.text.y = element_text(colour='white'),
            legend.position = 'none')),
  nrow=1, widths=c(2,1),
  right=textGrob('till', gp=gpar(fontsize=8), rot=0))
#combine
comb <- grid.arrange(rbind(csand, unsand, silt, till),
             ncol=1, 
             # left=textGrob('CCF', gp=gpar(fontsize=8), rot=90),
             bottom=textGrob('months', gp=gpar(fontsize=8)))
legend <- grid.arrange(get_legend(x %>% filter(mat=="sand*") %>% 
                       ac_plot(data=., type=nr, n='ex') + 
                       theme(legend.position = 'bottom',
                             legend.text = element_text(size=8),
                             legend.title = element_text(size=8))),
                       get_legend(not_x %>% filter(mat=="sand*") %>% 
                                    ac_plot(data=., type=nr, n='all') + 
                                    theme(legend.position = 'bottom',
                                          legend.text = element_text(size=8),
                                          legend.title = element_text(size=8))),
                       ncol=1, heights=c(1,1))
# grDevices::png(filename='figs_MS3/Figure3.png', width=140, height=160, units='mm',
#                res = 400, type='cairo-png')
grid.arrange(comb, legend, ncol=1, heights=c(2,0.3)) %>% 
  ggsave(filename='figs_MS3/Figure3.pdf', width=140, height=160, units='mm', 
         device=cairo_pdf, dpi=400, plot=.)
dev.off()

# not_x %>% distinct(id, mat, rt, Qmax, memory)%>% ungroup() %>% group_by(mat) %>%
#   dplyr::mutate(minrt = min(rt),
#                 meanrt = median(rt),
#                 maxrt = max(rt),
#                 minqmax = min(Qmax),
#                 meanqmax = median(Qmax),
#                 maxqmax = max(Qmax),
#                 minmemory = min(memory),
#                 meanmemory = median(memory),
#                 maxmemory = max(memory)) %>%
#   distinct(mat, minrt,meanrt,maxrt,minqmax,meanqmax,maxqmax,minmemory,
#            meanmemory,maxmemory) %>% write.csv(., 'summarystats.csv')


# grDevices::cairo_pdf(filename='sweden_class/acf_ccf.pdf', width=4, height=5,
#                      fallback_resolution = 400)
# grid.arrange(csand,csand.c, unsand,unsand.c, silt,silt.c,  till,  till.c,
#              ncol=2)
# dev.off()

# same but only SGI ts ----


sgi_plot <- function(data,  n="ex") {
  if(n=='ex'){
    ggplot(data) + 
      geom_hline(yintercept=0, linetype=3, col="black", size=0.5) +
      geom_line(data= . %>% dplyr::distinct(id, place, sgi, date, confinement, 
                                            dist_stream, landform, surface.material,
                                            med_med, aquifer.material, mat),
                aes(date, sgi, group=id),size=0.2) +
      
      guides(colour = "none") +
      scale_colour_manual(values=c("white","black")) +
      scale_x_date("year", expand=c(0,0),date_breaks='5 years', date_labels = "%Y") +
      scale_y_continuous("SGI", expand=c(0.1,0))+
       theme(panel.background = element_rect(colour="lightgrey", fill="white"),
            panel.grid = element_line(colour=alpha("lightgrey",0.5)),
            strip.background = element_blank(), strip.text=element_blank(),
            axis.ticks = element_line(colour="grey"),
            axis.title=element_text(size=8),
            legend.key = element_blank(),
            plot.margin = unit(c(0,1,0,0), "mm"),
            axis.text = element_text(size=8))+
      facet_grid(cols=vars(id), 
                 rows=vars(mat), scales='free')
    
  } else if(n=='all'){
    ggplot(data %>% dplyr::distinct(id, place, date, sgi, confinement, dist_stream, landform, surface.material,
                                    med_med, aquifer.material, mat) %>%
             dplyr::ungroup() %>% dplyr::group_by(date) %>%
             dplyr::mutate(median=median(sgi),
                           max = max(sgi), min=min(sgi),
                           std= sd(sgi))) + 
      geom_hline(yintercept=0, linetype=3, col="black", size=0.5) +
      geom_ribbon(aes(ymin=min, ymax=max, x=date, alpha='range'), fill='grey') +
      geom_ribbon(aes(ymin=median-std, ymax=median+std, x=date, alpha='sd'), fill='grey') +
      geom_line(aes(y=median, x=date), col='black',size=0.5) +
      
      guides(colour = "none") +
      scale_alpha_manual('SGI', values=c('range'=0.2, 'sd'=0.5))+
      scale_colour_manual(values=c("white","black")) +
      scale_x_date("year", expand=c(0,0), date_breaks='1 year', date_labels = "%Y") +
      scale_y_continuous("SGI", expand=c(0.1,0))+
       theme(panel.background = element_rect(colour="lightgrey", fill="white"),
            panel.grid = element_line(colour=alpha("lightgrey",0.5)),
            strip.background = element_blank(), strip.text=element_blank(),
            axis.ticks = element_line(colour="grey"),
            axis.title=element_text(size=8),
            legend.key = element_blank(),
            plot.margin = unit(c(0,1,0,0), "mm"),
            axis.text = element_text(size=8))+
      facet_grid(rows=vars(mat))
  }
  

}

x <- sgi %>% unnest_wider(data) %>% unnest() %>%
  right_join(., chars %>% distinct(id, confinement, dist_stream, landform, surface.material, 
                                   med_med, aquifer.material)) %>%
  ungroup() %>% dplyr::mutate(loc=gsub("\\_.*", "", id),
                              loc=ifelse(loc=="BC", "Simon Fraser valley", loc),
                              id=ifelse(grepl("SE", place) | grepl("BC", place),
                                        as.numeric(gsub(".*_", "", id)), id),
                              id=ifelse(grepl("FI", place),
                                        as.numeric(gsub(".*p", "", id)), id)) %>%
  dplyr::filter(((id==17 & place=="SE_19" )|
                  ((id==2 | id==259) & place=="BC Fraser Valley" )|
                  (id==10 & place=="FI_Valkeala" )|
                  ((id==1 | id== 5) & grepl("FI_K", place)==T )|
                  (id==8 & place=="FI_Alavus Taipale" )|
                  (id==10 & place=="SE_39" )) #&
                  # (year(date)>2004 & year(date)<2010)
                ) %>%
  dplyr::mutate(confinement = as.character(confinement),
                confinement = ifelse(confinement =="confined" | 
                                       grepl("potentially confined", confinement)==TRUE,
                                     "confined", confinement),
                confinement = ifelse(grepl("unconfined", confinement)==TRUE,
                                     "unconfined", confinement),
                confinement = ifelse(grepl("silt", aquifer.material)==TRUE,
                                     "aquiclude", confinement),
                confinement = ifelse(grepl("till", aquifer.material)==TRUE & is.na(confinement),
                                     "aquiclude", confinement),
                confinement = ifelse(grepl("sand", aquifer.material)==TRUE & is.na(confinement),
                                     "confined", confinement)) %>%
  dplyr::mutate(mat = ifelse(aquifer.material=="sand" & confinement=="confined", paste(aquifer.material, "*", sep=""),
                             paste(aquifer.material))) %>%
  dplyr::mutate(place=ifelse(place=="SE_19", "SE_Brattforsheden", place),
                place=ifelse(place=="SE_39", "SE_Abisko", place),
                id=paste(place, id, sep="_")) %>% group_by(id, place) %>% ungroup() %>%
  dplyr::mutate(id = ifelse(grepl("Bratt", id), "B17", id),
                id = ifelse(grepl("259", id), "FV259", id),
                id = ifelse(grepl("_2", id), "FV2", id),
                id = ifelse(grepl("Valk", id), "V10", id),
                id = ifelse(grepl("ä_1", id), "K1", id),
                id = ifelse(grepl("ä_5", id), "K5", id),
                id = ifelse(grepl("Alavus", id), "AT8", id),
                id = ifelse(grepl("Abisko", id), "A10", id))
not_x <- sgi %>% unnest_wider(data) %>% unnest() %>%
  right_join(., chars %>% distinct(id, confinement, dist_stream, landform, surface.material, 
                                   med_med, aquifer.material)) %>%
  # dplyr::filter(year(date)>2004 & year(date)<2010) %>%
  dplyr::mutate(confinement = as.character(confinement),
                confinement = ifelse(confinement =="confined" | 
                                       grepl("potentially confined", confinement)==TRUE,
                                     "confined", confinement),
                confinement = ifelse(grepl("unconfined", confinement)==TRUE,
                                     "unconfined", confinement),
                confinement = ifelse(grepl("silt", aquifer.material)==TRUE,
                                     "aquiclude", confinement),
                confinement = ifelse(grepl("till", aquifer.material)==TRUE & is.na(confinement),
                                     "aquiclude", confinement),
                confinement = ifelse(grepl("sand", aquifer.material)==TRUE & is.na(confinement),
                                     "confined", confinement)) %>%
  mutate(mat = ifelse(aquifer.material=="sand" & confinement=="confined", paste(aquifer.material, "*", sep=""),
                      paste(aquifer.material))) 
library(gridExtra); library(grid)
csand <- grid.arrange(
  arrangeGrob(
    x %>% filter(mat=="sand*") %>% 
      sgi_plot(data=., n='ex') +
      geom_text(data=. %>% filter(id=='FV259') %>% mutate(md = min(date)) %>% distinct(id, md),
                aes(md,-Inf, label='(a)'), hjust=-0.5, vjust=-1)+
      geom_text(data=. %>% filter(id=='V10') %>% dplyr::mutate(md = min(date)) %>% distinct(id, md),
                aes(md,Inf, label='(b)'), hjust=-0.5, vjust=1) + 
      theme(#axis.title = element_blank(), 
        axis.title.x = element_blank(), 
        legend.position = 'none')),
  # arrangeGrob(
  #   not_x %>% filter(mat=="sand*") %>% 
  #     ac_plot(data=., n='all') +
  #     geom_text(data=. %>% mutate(id='FV259') %>% ungroup() %>% distinct(id), 
  #               aes(as.Date('2005-01-01'),Inf, label='(c)'), hjust=-0.5, vjust=1)+ 
  #     theme(axis.title = element_blank(), axis.text.y = element_text(colour='white'),
  #           legend.position = 'none')),
  # nrow=1, widths=c(2,1),
  right=textGrob('confined sand', gp=gpar(fontsize=8), rot=0))
unsand <- grid.arrange(
  arrangeGrob(
    x %>% filter(mat=="sand") %>% 
      sgi_plot(data=., n='ex') +
      geom_text(data=. %>% filter(id=='B17') %>% mutate(md = min(date)) %>% distinct(id, md),
                aes(md,-Inf, label='(c)'),  hjust=-0.5, vjust=-1)+
      geom_text(data=. %>% filter(id=='FV2') %>% mutate(md = min(date)) %>% distinct(id, md),
                aes(md,Inf, label='(d)'),  hjust=-0.5, vjust=1) + 
      theme(#axis.title = element_blank(), 
        axis.title.x = element_blank(), 
        legend.position = 'none')),
  # arrangeGrob(
  #   not_x %>% filter(mat=="sand") %>% 
  #     ac_plot(data=., n='all') +
  #     geom_text(data=. %>% mutate(id='B17') %>% ungroup()%>% distinct(id), 
  #               aes(as.Date('2005-01-01'),Inf, label='(f)'),  hjust=-0.5, vjust=1)+ 
  #     theme(axis.title = element_blank(), axis.text.y = element_text(colour='white'),
  #           legend.position = 'none')),
  # nrow=1, widths=c(2,1),
  right=textGrob('unconfined sand', gp=gpar(fontsize=8), rot=0))
silt <- grid.arrange(
  arrangeGrob(
    x %>% filter(mat=="silt") %>% 
      sgi_plot(data=., n='ex') +
      geom_text(data=. %>% filter(id=='K1') %>% mutate(md = min(date)) %>% distinct(id, md),
                aes(md,-Inf, label='(e)'), hjust=-0.5, vjust=-1)+
      geom_text(data=. %>% filter(id=='K5') %>% mutate(md = min(date)) %>% distinct(id, md),
                aes(md,-Inf, label='(f)'), hjust=-0.5, vjust=-1) + 
      theme(#axis.title = element_blank(), 
        axis.title.x = element_blank(), 
        legend.position = 'none')),
  # arrangeGrob(
  #   not_x %>% filter(mat=="silt") %>% 
  #     ac_plot(data=., n='all') +
  #     geom_text(data=. %>% mutate(id='K1') %>% ungroup()%>% distinct(id), 
  #               aes(as.Date('2005-01-01'),Inf, label='(i)'), hjust=-0.5, vjust=1)+ 
  #     theme(axis.title = element_blank(), axis.text.y = element_text(colour='white'),
  #           legend.position = 'none')),
  # nrow=1, widths=c(2,1),
  right=textGrob('silt', gp=gpar(fontsize=8), rot=0))
till <- grid.arrange(
  arrangeGrob(
    x %>% filter(mat=="till") %>% 
      sgi_plot(data=., n='ex') +
      geom_text(data=. %>% filter(id=='A10') %>% mutate(md = min(date)) %>% distinct(id, md),
                aes(md,Inf, label='(g)'), hjust=-0.5, vjust=1)+
      geom_text(data=. %>% filter(id=='AT8') %>% mutate(md = min(date)) %>% distinct(id, md),
                aes(md,Inf, label='(h)'), hjust=-0.5, vjust=1) + 
      theme(#axis.title = element_blank(), 
        axis.title.x = element_blank(), 
        legend.position = 'none')),
  # arrangeGrob(
  #   not_x %>% filter(mat=="till") %>% 
  #     ac_plot(data=., n='all') +
  #     geom_text(data=. %>% mutate(id='A10') %>% ungroup()%>% distinct(id), 
  #               aes(as.Date('2005-01-01'),Inf, label='(l)'), hjust=-0.5, vjust=1)+ 
  #     theme(axis.title = element_blank(), axis.text.y = element_text(colour='white'),
  #           legend.position = 'none')),
  # nrow=1, widths=c(2,1),
  right=textGrob('till', gp=gpar(fontsize=8), rot=0))
#combine
comb <- grid.arrange(rbind(csand, unsand, silt, till),
                     ncol=1, 
                     # left=textGrob('CCF', gp=gpar(fontsize=8), rot=90),
                     bottom=textGrob('year', gp=gpar(fontsize=8)))
# grDevices::png(filename='figs_MS3/Figure9.png', width=150, height=110, units='mm',
               # res = 400, type='cairo-png')
grid.arrange(comb) %>% 
  ggsave(filename='figs_MS3/Figure9.pdf', width=150, height=110, units='mm', 
         device=cairo_pdf, dpi=400, plot=.)
dev.off()

# stat and significance to depth and stream plots ----
chars %>%
  mutate(confinement = as.character(confinement),
         confinement = ifelse(confinement =="confined" | 
                                grepl("potentially confined", confinement)==TRUE,
                              "confined/potentially confined", confinement),
         confinement = ifelse(grepl("unconfined", confinement)==TRUE,
                              "unconfined/probably unconfined", confinement),
         confinement = ifelse(grepl("silt", aquifer.material)==TRUE,
                              "confined/potentially confined", confinement),
         confinement = ifelse(grepl("till", aquifer.material)==TRUE & is.na(confinement),
                              "unconfined/probably unconfined", confinement),
         confinement = ifelse(grepl("sand", aquifer.material)==TRUE & is.na(confinement),
                              "confined/potentially confined", confinement),
         water = min(dist_stream,dist_lake1)) %>% select(-Apercent_lu, -label) %>%
  distinct %>% View 
 y<- cross_auto %>% left_join(., chars %>% 
                           dplyr::select(-c(2:11), -region, -Apercent_lu, -label) %>% dplyr::distinct()) %>% 
  dplyr::mutate(confinement = as.character(confinement),
    confinement = ifelse(confinement =="confined" | 
                           grepl("potentially confined", confinement)==TRUE,
                              "confined/potentially confined", confinement),
    confinement = ifelse(grepl("unconfined", confinement)==TRUE,
                              "unconfined/probably unconfined", confinement),
    confinement = ifelse(grepl("silt", aquifer.material)==TRUE,
                         "confined/potentially confined", confinement),
    confinement = ifelse(grepl("till", aquifer.material)==TRUE & is.na(confinement),
                         "unconfined/probably unconfined", confinement),
    confinement = ifelse(grepl("sand", aquifer.material)==TRUE & is.na(confinement),
                         "confined/potentially confined", confinement),
    water = min(dist_stream,dist_lake1),
    mat = ifelse(aquifer.material=="sand" & grepl("unconfined", confinement)==F, 
                 paste(aquifer.material, "*", sep=""),
                 paste(aquifer.material)),
    mat=factor(mat, levels=c("sand*", "sand", "silt", "till"))) %>% 
  # filter(grepl("potentially confined", confinement)==TRUE & aquifer.material=="sand" 
            # grepl("Ruo", id)==T
          # ) %>%
  # filter(grepl("potentially confined", confinement)==TRUE) %>%
   filter(!is.na(mat)) 
 
all_plot <- function(data, type1 = "cont", type2 = "auto", col="med"){
   if(type1=="cont"){
     if(type2=="auto"){
       xy = data  %>%
         distinct(id, place, auto, lag, aquifer.material,med_med, dist_stream, water, confinement, mat)
       
       if(col=="med"){
         xy %>% 
           ggplot(., aes(lag, auto,
                         group=id, colour=(med_med))) + 
           
           geom_hline(yintercept=0.2, linetype=3, col="black", size=0.7) +
           geom_line(alpha=0.9) +
           
           scale_color_viridis_c("median depth (mbgs)", na.value = "grey", end=0.9, trans="log10",
                                 limits=c(min(chars$med_med)-0.01, max(chars$med_med)+0.01)) +
           scale_x_continuous("lag (months)", expand=c(0,0), limits=c(0,60))+
           scale_y_continuous("ACF", limits = c(-0.5,1), expand=c(0,0))+
           
           theme(panel.background = element_rect(colour="lightgrey", fill="white"),
                 panel.grid = element_line(colour=alpha("lightgrey",0.5)),
                 strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
                 axis.ticks = element_line(colour="grey"),
                 plot.margin = unit(c(0,0,0,0), "mm"),
                 legend.title = element_text(size=8),
                 legend.position = "bottom",
                 # legend.key.width = unit(9, "mm"),
                 legend.key.height = unit(2, "mm"),
                 axis.text = element_text(size=8),
                 strip.text = element_text(size=8),
                 axis.title = element_text(size=8))+
           guides(colour=guide_colourbar(title.position="top", title.hjust=0.5)) + 
           facet_grid(rows=vars(mat))
       } 
       else if(col=="stream"){
         xy %>% 
           ggplot(., aes(lag, auto,
                         group=id, colour=(dist_stream))) + 
           
           geom_hline(yintercept=0.2, linetype=3, col="black", size=0.7) +
           geom_line(alpha=0.9) +
           
           scale_color_viridis_c("dist. stream (m)", na.value = "grey", end=0.9, option = "magma",
           limits=c(min(chars$dist_stream), max(chars$dist_stream))) +
           
           scale_x_continuous("lag (months)", expand=c(0,0), limits=c(0,60))+
           scale_y_continuous("ACF", limits = c(-0.5,1), expand=c(0,0))+
           
           theme(panel.background = element_rect(colour="lightgrey", fill="white"),
                 panel.grid = element_line(colour=alpha("lightgrey",0.5)),
                 strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
                 axis.ticks = element_line(colour="grey"),
                 plot.margin = unit(c(0,0,0,0), "mm"),
                 legend.title = element_text(size=8),
                 legend.position = "bottom",
                 # legend.key.width = unit(9, "mm"),
                 legend.key.height = unit(2, "mm"),
                 axis.text = element_text(size=8),
                 strip.text = element_text(size=8),
                 axis.title = element_text(size=8))+
           guides(colour=guide_colourbar(title.position="top", title.hjust=0.5)) + 
           facet_grid(rows=vars(mat))
       }
     }
     else if(type2=="cross"){
       xy = data %>%
         distinct(id, place, corr, agg, aquifer.material,med_med, dist_stream, water, confinement, mat) 
         
       if(col=="med"){
         xy  %>%
           ggplot(., aes(agg, corr,group=id, 
                         colour=(med_med))) + 
           
           
           geom_hline(yintercept=0, linetype=3, col="black", size=0.5) +
           geom_line(alpha=0.9) +
           
           scale_color_viridis_c("median depth (mbgs)", na.value = "grey", end=0.9, trans="log10",
                                 limits=c(min(chars$med_med)-0.01, max(chars$med_med)+0.01)) +
           # scale_color_viridis_c("dist. stream (m)", na.value = "grey", end=0.9, option = "magma",
           # limits=c(min(chars$dist_stream), max(chars$dist_stream))) +
           
           
           scale_x_continuous("accumulation (months)", expand=c(0,0), limits=c(1,60))+
           scale_y_continuous("CCF", limits = c(-0.5,1), expand=c(0,0))+
           
           theme(panel.background = element_rect(colour="lightgrey", fill="white"),
                 panel.grid = element_line(colour=alpha("lightgrey",0.5)),
                 strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
                 axis.ticks = element_line(colour="grey"),
                 plot.margin = unit(c(0,0,0,0), "mm"),
                 legend.title = element_text(size=8),
                 legend.position = "bottom",
                 # legend.key.width = unit(9, "mm"),
                 legend.key.height = unit(2, "mm"),
                 axis.text = element_text(size=8),
                 strip.text = element_text(size=8),
                 axis.title = element_text(size=8))+
           guides(colour=guide_colourbar(title.position="top", title.hjust=0.5)) + 
           facet_grid(rows=vars(mat)) 
       }
       else if(col=="stream"){
         xy %>% 
           ggplot(., aes(agg, corr,group=id, 
                         colour=(dist_stream))) + 
           
           
           geom_hline(yintercept=0, linetype=3, col="black", size=0.5) +
           geom_line(alpha=0.9) +
           
           scale_color_viridis_c("dist. stream (m)", na.value = "grey", end=0.9, option = "magma",
           limits=c(min(chars$dist_stream), max(chars$dist_stream))) +
           
           scale_x_continuous("accumulation (months)", expand=c(0,0), limits=c(1,60))+
           scale_y_continuous("CCf", limits = c(-0.5,1), expand=c(0,0))+
           
           theme(panel.background = element_rect(colour="lightgrey", fill="white"),
                 panel.grid = element_line(colour=alpha("lightgrey",0.5)),
                 strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
                 axis.ticks = element_line(colour="grey"),
                 plot.margin = unit(c(0,0,0,0), "mm"),
                 legend.title = element_text(size=8),
                 legend.position = "bottom",
                 # legend.key.width = unit(9, "mm"),
                 legend.key.height = unit(2, "mm"),
                 axis.text = element_text(size=8),
                 strip.text = element_text(size=8),
                 axis.title = element_text(size=8))+
           guides(colour=guide_colourbar(title.position="top", title.hjust=0.5)) + 
           facet_grid(rows=vars(mat))
       }
       
     }
     
   } 
   else if(type1=="point"){
     xy = data %>%
       distinct(id, aquifer.material,med_med, dist_stream, water, confinement, mat,
                memory, Qmax, maxcorr) %>% filter(!is.na(maxcorr))
     if(type2=="auto"){
       if(col=="med"){
         xy %>%
           ggplot(., aes(memory, med_med)) +

           geom_point() +
           guides(colour = "none") +
           scale_x_continuous("memory (months)", expand=c(0,0), limits=c(0,60))+
           scale_y_continuous("median depth (mbgs)",expand=c(0,0), trans = "log", breaks=c(0.3,1,3,10))+
           theme(panel.background = element_rect(colour="lightgrey", fill="white"),
                 panel.grid = element_line(colour=alpha("lightgrey",0.5)),
                 strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
                 axis.ticks = element_line(colour="grey"),
                 plot.margin = unit(c(0,0,0,0), "mm"),
                 legend.title = element_text(size=8),
                 legend.position = "bottom",
                 # legend.key.width = unit(9, "mm"),
                 legend.key.height = unit(2, "mm"),
                 axis.text = element_text(size=8),
                 strip.text = element_text(size=8),
                 axis.title = element_text(size=8))+
           # facet_grid(rows=vars(mat))
           facet_wrap(~mat)
       }
       else if(col=="stream"){
         xy %>%
           ggplot(., aes(memory, dist_stream)) + 
           geom_point() +
           guides(colour = "none") +
           scale_x_continuous("memory (months)", expand=c(0,0), limits=c(0,60))+
           scale_y_continuous("dist. stream (m)",expand=c(0,0), 
                              breaks=c(500,1000,1500))+
           theme(panel.background = element_rect(colour="lightgrey", fill="white"),
                 panel.grid = element_line(colour=alpha("lightgrey",0.5)),
                 strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
                 axis.ticks = element_line(colour="grey"),
                 plot.margin = unit(c(0,0,0,0), "mm"),
                 legend.title = element_text(size=8),
                 legend.position = "bottom",
                 # legend.key.width = unit(9, "mm"),
                 legend.key.height = unit(2, "mm"),
                 axis.text = element_text(size=8),
                 strip.text = element_text(size=8),
                 axis.title = element_text(size=8)) + 
           # facet_grid(rows=vars(mat))
           facet_wrap(~mat)
       }
       
     } 
     else if(type2=="cross"){
       if(col=="med"){
         xy %>%
           ggplot(., aes(Qmax, med_med)) + 
           geom_point() +
           guides(colour = "none") +
           
           scale_x_continuous("response time (months)", expand=c(0,0), limits=c(0,60))+
           scale_y_continuous("median depth (mbgs)",expand=c(0,0), trans = "log", breaks=c(0.3,1,3,10))+
           theme(panel.background = element_rect(colour="lightgrey", fill="white"),
                 panel.grid = element_line(colour=alpha("lightgrey",0.5)),
                 strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
                 axis.ticks = element_line(colour="grey"),
                 plot.margin = unit(c(0,0,0,0), "mm"),
                 legend.position = "bottom",
                 # legend.key.width = unit(9, "mm"),
                 legend.key.height = unit(2, "mm"),
                 axis.text = element_text(size=8),
                 strip.text = element_text(size=8),
                 axis.title = element_text(size=8)) + 
           # facet_grid(rows=vars(mat))
           facet_wrap(~mat)
       } 
       else if (col=="stream"){
         xy %>%
           ggplot(., aes(Qmax, dist_stream)) + 
           geom_point() +
           guides(colour = "none") +
           
           scale_x_continuous("response time (months)", expand=c(0,0), limits=c(0,60))+
           scale_y_continuous("dist. stream (m)",expand=c(0,0), breaks=c(500,1000,1500))+
           theme(panel.background = element_rect(colour="lightgrey", fill="white"),
                 panel.grid = element_line(colour=alpha("lightgrey",0.5)),
                 strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
                 axis.ticks = element_line(colour="grey"),
                 plot.margin = unit(c(0,0,0,0), "mm"),
                 legend.position = "bottom",
                 # legend.key.width = unit(9, "mm"),
                 legend.key.height = unit(2, "mm"),
                 axis.text = element_text(size=8),
                 strip.text = element_text(size=8),
                 axis.title = element_text(size=8)) + 
           # facet_grid(rows=vars(mat))
           facet_wrap(~mat)
       }
       
     }
   }
   
}
y %>% all_plot(data=., type1 = "cont", type2 = "cross", col ="med" )
library(gridExtra); library(grid); library(ggpmisc)


grid.arrange(
  arrangeGrob(y %>% all_plot(data=., type1 = "point", type2 = "auto", col ="med" ) +
                theme(legend.position = "bottom") +
                # geom_smooth(method="lm", se=F) +
                stat_fit_glance(method = 'lm',
                                method.args = list(formula = y~x),
                                geom = 'text_npc',
                                mapping = aes(label = sprintf('r^2=%.3f\np=%.2g',
                                                              after_stat(r.squared), 
                                                              after_stat(p.value)),
                                              colour=after_stat(p.value)<0.05),
                                label.x = "right", label.y = "bottom", size = 3) +
                scale_colour_manual(values=c("grey", "black"))),
  arrangeGrob(y %>% all_plot(data=., type1 = "point", type2 = "auto", col ="stream" ) +
                theme(legend.position = "bottom")+
                # geom_smooth(method="lm", se=F) +
                stat_fit_glance(method = 'lm',
                                method.args = list(formula = y~x),
                                geom = 'text_npc',
                                mapping = aes(label = sprintf('r^2=%.3f\np=%.2g',
                                                              after_stat(r.squared), 
                                                              after_stat(p.value)),
                                              colour=after_stat(p.value)<0.05),
                                label.x = "right", label.y = "bottom", size = 3) +
                scale_colour_manual(values=c("grey", "black"))),
  
  arrangeGrob(y %>% select(-Qmax) %>% rename(Qmax=rt)%>% 
                all_plot(data=., type1 = "point", type2 = "cross", col ="med" )  + 
                theme(legend.position = "bottom")+
                # geom_smooth(method="lm", se=F) +
                stat_fit_glance(method = 'lm',
                                method.args = list(formula = y~x),
                                geom = 'text_npc',
                                mapping = aes(label = sprintf('r^2=%.3f\np=%.2g',
                                                              after_stat(r.squared), 
                                                              after_stat(p.value)),
                                              colour=after_stat(p.value)<0.05),
                                label.x = "right", label.y = "bottom", size = 3) +
                scale_colour_manual(values=c("grey", "black"))),
  
  arrangeGrob(y %>% select(-Qmax) %>% rename(Qmax=rt)%>% 
                all_plot(data=., type1 = "point", type2 = "cross", col ="stream" )  + 
                theme(legend.position = "bottom")+
                # geom_smooth(method="lm", se=F) +
                stat_fit_glance(method = 'lm',
                                method.args = list(formula = y~x),
                                geom = 'text_npc',
                                mapping = aes(label = sprintf('r^2=%.3f\np=%.2g',
                                                              after_stat(r.squared), 
                                                              after_stat(p.value)),
                                              colour=after_stat(p.value)<0.05),
                                label.x = "right", label.y = "bottom", size = 3) +
                scale_colour_manual(values=c("grey", "black"))),
  ncol=2, nrow=2, 
  left=textGrob("", gp=gpar(fontsize=8), rot=90)
)

# y %>% mutate(
#   A_Qmax = memory/med_med,
#   A_Qmax_p = wilcox.test(memory, med_med, paired = T)$p.value
#   ) %>% distinct(A_Qmax, A_Qmax_p) %>% 
#   ggplot(.)+ geom_abline(aes(slope=A_Qmax, colour=A_Qmax_p>0.05, intercept=0))


grid.arrange(
  arrangeGrob(a +theme(legend.title = element_text(size=8),
                                  legend.position = "bottom",
                                  # legend.key.width = unit(9, "mm"),
                                  legend.key.height = unit(2, "mm"),
                                  axis.text = element_text(size=8),
                       strip.text = element_text(size=8),
                       axis.title = element_text(size=8),
                                  axis.title.y = element_blank())+
                           guides(colour=guide_colourbar(title.position="top", title.hjust=0.5))),
  arrangeGrob(b+ theme(legend.title = element_text(size=8),
                                legend.position = "bottom",
                                # legend.key.width = unit(9, "mm"),
                                legend.key.height = unit(2, "mm"),
                                axis.text = element_text(size=8),
                       strip.text = element_text(size=8),
                       axis.title = element_text(size=8),
                                axis.title.y = element_blank())+
                          guides(colour=guide_colourbar(title.position="top", title.hjust=0.5))),
             ncol=2, nrow=1, 
  left=textGrob("ACF", gp=gpar(fontsize=8), rot=90)
  )


grid.arrange(arrangeGrob(c +theme(axis.text = element_text(size=8),
                                  axis.title.x = element_blank())),
             
             arrangeGrob(d +theme(axis.text = element_text(size=8),
                                  axis.title.x = element_blank())),
             ncol=2, nrow=1, bottom="memory")

library(cowplot)
plot_grid(a +theme(legend.position="none",
                   axis.text = element_text(size=8),
                   axis.title.y = element_blank()),
          b+ theme(legend.position = "none",
                   axis.text = element_text(size=8),
                   axis.title.y = element_blank()),
          get_legend(a +theme(legend.title = element_text(size=8),
                              legend.position = "bottom",
                              # legend.key.width = unit(9, "mm"),
                              legend.key.height = unit(2, "mm"))+
                       guides(colour=guide_colourbar(title.position="top", title.hjust=0.5))),
          get_legend(b +theme(legend.title = element_text(size=8),
                              legend.position = "bottom",
                              # legend.key.width = unit(9, "mm"),
                              legend.key.height = unit(2, "mm"))+
                       guides(colour=guide_colourbar(title.position="top", title.hjust=0.5))),
          ncol=2, nrow=2, rel_heights=c(2,2,0.05,0.05)
          
)

a <- y  %>%
  distinct(id, place, auto, corr, lag, confinement, mat) %>% 
  ggplot(., aes(lag, auto, group=id)) + 
  
  geom_hline(yintercept=0.2, linetype="dashed", col="black", size=0.7) +
  geom_line(alpha=0.8, size=0.5) +
  
  
  scale_x_continuous("lag (months)", expand=c(0,0), limits=c(0,60), breaks=c(0,10,20,30,40,50))+
  scale_y_continuous("ACF", limits = c(-0.5,1), expand=c(0,0))+
  
  theme(panel.background = element_rect(colour="lightgrey", fill="white"),
        panel.grid = element_line(colour=alpha("lightgrey",0.5)),
        strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
        axis.ticks = element_line(colour="grey"),
        plot.margin = unit(c(0,0,0,0), "mm"),
        legend.title = element_text(size=8),
        legend.position = "bottom",
        # legend.key.width = unit(9, "mm"),
        legend.key.height = unit(2, "mm"),
        axis.text = element_text(size=8),
        strip.text = element_text(size=8),
        axis.title = element_text(size=8))+
  guides(colour=guide_colourbar(title.position="top", title.hjust=0.5)) + 
  facet_wrap(.~mat, ncol=1)
b<- y  %>%
  distinct(id, place, auto, corr, agg, lag, confinement, mat) %>% 
  ggplot(., aes(agg, corr, group=id)) + 
  
  geom_hline(yintercept=0, linetype="dotted", col="black", size=0.7) +
  geom_line(alpha=0.8, size=0.5) +
  
  scale_x_continuous("accumulation (months)", expand=c(0,0), limits=c(0,60), breaks=c(0,10,20,30,40,50))+
  scale_y_continuous("CCF", limits = c(-0.5,1), expand=c(0,0))+
  
  theme(panel.background = element_rect(colour="lightgrey", fill="white"),
        panel.grid = element_line(colour=alpha("lightgrey",0.5)),
        strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
        axis.ticks = element_line(colour="grey"),
        plot.margin = unit(c(0,0,0,0), "mm"),
        legend.title = element_text(size=8),
        legend.position = "bottom",
        # legend.key.width = unit(9, "mm"),
        legend.key.height = unit(2, "mm"),
        axis.text = element_text(size=8),
        strip.text = element_text(size=8),
        axis.title = element_text(size=8))+
  guides(colour=guide_colourbar(title.position="top", title.hjust=0.5)) + 
  facet_wrap(.~mat, ncol=1)

grid.arrange(a,b, ncol=2)

# the Bloomfield and Marchant (2013) plot ----
#edit for reviews - try to estiate memory using 0.2, 0.1, 0 and the statistical threshold of min 'level'
# data <- not_x 
# not_x <- not_x %>% dplyr::mutate(memory0.16 = ifelse(auto>0.16 & (lead(auto,1) < 0.16 | 
#                                                       is.na(lead(auto,1))), lag, NA),
#                         memory0.16=first(na.omit(memory0.16)),
#                         memory0 = ifelse(auto>0 & (lead(auto,1) < 0 | 
#                                                             is.na(lead(auto,1))), lag, NA),
#                         memory0=first(na.omit(memory0)))


# original code below 
bplot_qmax<-
  not_x %>% distinct(memory, Qmax, rt,id, mat, maxcorr, 
                     #        memory0.16,memory0
                     ) %>% 
  filter(!is.na(maxcorr)) %>% 
  ggplot(.)  + 
  geom_point(data=bloom_data, #%>%mutate(mat="other studies"),
             aes(Mmax, Qmax, fill=geo, shape=place), alpha=0.7) +
  
  geom_point(data=. %>% mutate(place="this study", ACF =0.2),
             aes(memory, Qmax, group=id, shape=place), size=0.8,
             alpha=0.7) +
  
  
  # geom_point(data=. %>% mutate(place="this study", ACF =0.16),
  #            aes(memory0.16, rt, group=id, shape=place), size=0.5,
  #            alpha=0.7) +
  # geom_point(data=. %>% mutate(place="this study", ACF =0),
  #            aes(memory0, rt, group=id, shape=place), size=0.5,
  #            alpha=0.7) +
  
  
  
  geom_text(data=. %>% filter(id=='Brattforsheden_17') %>% distinct(id),
            aes(x=0,y=Inf,label="(a)"), hjust=-0.5,vjust=1)+
  geom_abline(slope=1, intercept = 0, linetype=2)+
  # scale_x_continuous("memory (months)", expand=c(0,0.6), limits=c(0,60),
  #                    breaks=c(0,10,20,30,40,50,60))+
  # scale_y_continuous("response time (months)", expand=c(0,0.6), limits=c(0,60),
  #                    breaks = c(0,10,20,30,40,50, 60))+
  scale_x_continuous("log memory (months)", expand=c(0,0.6), limits=c(1.0,60),
                     breaks=c(2,5,10,20,35, 60), trans="log")+
  scale_y_continuous("log response time (months)", expand=c(0,0.6), limits=c(1.0,60),
                     breaks = c(2,5,10,20,35, 60), trans="log")+
  
  scale_shape_manual("",values=c(21,22,18))+
  scale_fill_viridis_d("setting:", direction = -1, option="cividis") +
  # scale_colour_grey("",start=0.8, end=0) +
  theme(panel.background = element_rect(colour="lightgrey", fill="white"),
        panel.grid = element_line(colour=alpha("lightgrey",0.5)),
        strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
        axis.ticks = element_line(colour="grey"),
        axis.text = element_text(size=8),
        strip.text = element_text(size=8),
        axis.title = element_text(size=8),
        plot.margin = unit(c(0,1,0,0), "mm"), legend.text = element_text(size=8),
        legend.title = element_text(size=8),
        legend.key = element_rect(fill=NA),
        legend.position="bottom", 
        legend.box="vertical", legend.margin=margin())+ 
  # facet_grid(rows=vars(ACF)) +
  guides(fill=guide_legend(override.aes = list(shape=21, size=3), order=2),
         shape=guide_legend(order=1, override.aes=list(fill='white')))  

bplot<- grid.arrange(bplot_qmax + theme(legend.position = 'none'), 
                 bplot_rt+ theme(legend.position = 'none'),
                 # mem+ theme(legend.position = 'none'),
                 ncol=2)
library(Cairo)
# grDevices::png(filename='figs_MS3/Figure6.png', width=120, height=80, units='mm',
               # res = 400, type='cairo-png')
grid.arrange(bplot, get_legend(bplot_qmax),
             ncol=1, heights=c(2,0.5)) %>% 
  ggsave(filename='figs_MS3/Figure6.pdf', width=120, height=82, units='mm', 
         device=cairo_pdf, dpi=400, plot=.)
dev.off()

# make density plots:: viridis::cividis(n=3)
denplot <- function(data){
  data %>% ggplot(.) + 
  geom_density(data=. %>% mutate(type='max. correlation'), 
               aes(x=Qmax,  fill=type), alpha=0.5)+ 
  geom_density(data=. %>% mutate(type='correlation slope'), 
               aes(x=rt,  fill=type), alpha=0.5) + 
  geom_density(data=. %>% mutate(type='memory'), 
               aes(x=memory, fill=type), alpha=0.5)+ 
  # scale_fill_viridis_d("", option='viridis') +
    scale_fill_manual(name="response time by:", 
                      values=c("max. correlation" ="#9D6C06",
                               "correlation slope"= "#077DAA", 
                               "memory" = "#FFEA46FF")) +
  scale_x_continuous("months", expand=c(0,0), limits=c(0,60), breaks=c(0,10,20,30,40,50)) +
  scale_y_continuous("density", expand=c(0,0))+
  theme(panel.background = element_rect(colour="lightgrey", fill="white"),
        panel.grid = element_line(colour=alpha("lightgrey",0.5)),
        # strip.background = element_rect(fill="white", colour="white"),
        # strip.text = element_text(size=8),
        strip.background = element_blank(), strip.text=element_blank(),
        axis.title = element_text(size=8),
        axis.ticks = element_line(colour="grey"),
        legend.key = element_blank(),
        legend.position='bottom',
        plot.margin = unit(c(0,1,0,0), "mm"),
        axis.text = element_text(size=8))+
  facet_grid(rows=vars(mat), scales='free')
}

linplot <- function(data){
    data %>% ggplot(., aes(x=memory))  + 
    geom_point(data=. %>%mutate(type="max. correlation"), 
               aes(y=Qmax, group=id, fill=type,shape=type),
               alpha=0.5) +
    geom_point(data=. %>%mutate(type="correlation slope"), 
               aes(y=rt, group=id, fill=type,shape=type),
               alpha=0.5) +
  geom_abline(slope=1, intercept = 0, linetype=2)+
  scale_x_continuous("log memory (months)", expand=c(0,0.2), limits=c(1.0,60),
                     breaks=c(2,5,10,20,35, 60), trans="log")+
  scale_y_continuous(paste("log response", "\n", "time (months)"), 
                     expand=c(0,0.2), limits=c(1.0,60),
                     breaks = c(2,5,10,20,35, 60), trans="log")+
  
  scale_shape_manual(name="response time by:", values=c('max. correlation'=21,
                                                       'correlation slope' = 22,
                                                       'memory' = 22))+
  scale_fill_manual(name="response time by:", 
                      values=c("max. correlation" = "#9D6C06",
                               "correlation slope"= "#077DAA", 
                               "memory" =  "#FFEA46FF")) +
  # scale_fill_viridis_d(name="response time by", direction=-1, option="cividis")+
  theme(panel.background = element_rect(colour="lightgrey", fill="white"),
        panel.grid = element_line(colour=alpha("lightgrey",0.5)),
        axis.ticks = element_line(colour="grey"),
        axis.text = element_text(size=8),
        axis.title= element_text(size=8),
        strip.background = element_blank(), strip.text=element_blank(),
        # strip.text = element_text(size=8),
        # strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
        plot.margin = unit(c(0,1,0,0), "mm"), legend.text = element_text(size=8),
        legend.title = element_text(size=8),
        legend.position="bottom", 
        legend.box="vertical", legend.margin=margin()) 
  # facet_wrap(~mat) + 
    # guides(shape=guide_legend(override.aes = list(size=3)))
}
not_x  %>% distinct(memory, Qmax, rt, mat) %>% 
  filter(mat=="sand") %>% denplot(.)
csand <- grid.arrange(
  arrangeGrob(
    not_x %>% distinct(memory, Qmax, rt,id, mat) %>% filter(mat=='sand*') %>% 
      linplot(data=.) +
      geom_text(data=. %>% filter(id=='Brattforsheden_17') %>% distinct(id), 
                aes(x=0,y=Inf, label='(a)'), hjust=-0.5, vjust=1) + 
      theme(legend.position = 'none')),
  arrangeGrob(
    not_x %>% distinct(memory, Qmax, rt,id, mat) %>% filter(mat=='sand*')  %>%
      mutate(mat="confined sand") %>%
      denplot(data=.) +
      geom_text(data=. %>% filter(id=='Brattforsheden_17') %>% distinct(id), 
                aes(x=Inf,y=Inf, label='(b)'), hjust=1, vjust=1) + 
      theme(legend.position='none')),
  nrow=1, widths=c(1,1),
  right=textGrob('confined sand', gp=gpar(fontsize=8), rot=0))
unsand <- grid.arrange(
  arrangeGrob(
    not_x %>% distinct(memory, Qmax, rt,id, mat) %>% filter(mat=='sand') %>% 
      linplot(data=.) +
      geom_text(data=. %>% filter(id=='Valkeala_10') %>% distinct(id), 
                aes(x=0,y=Inf, label='(c)'), hjust=-0.5, vjust=1) + 
      theme(legend.position = 'none')),
  arrangeGrob(
    not_x %>% distinct(memory, Qmax, rt,id, mat) %>% filter(mat=='sand')  %>%
      mutate(mat="unconfined sand") %>%
      denplot(data=.) +
      geom_text(data=. %>% filter(id=='Valkeala_10') %>% distinct(id), 
                aes(x=Inf,y=Inf, label='(d)'), hjust=1, vjust=1) + 
    theme(legend.position='none')),
  nrow=1, widths=c(1,1),
  right=textGrob('unconfined sand', gp=gpar(fontsize=8), rot=0))
silt <- grid.arrange(
  arrangeGrob(
    not_x %>% distinct(memory, Qmax, rt,id, mat) %>% filter(mat=='silt') %>% 
      linplot(data=.) +
      geom_text(data=. %>% filter(id=='Kälviä_5') %>% distinct(id), 
                aes(x=0,y=Inf, label='(e)'), hjust=-0.5, vjust=1) + 
      theme(legend.position = 'none')),
  arrangeGrob(
    not_x %>% distinct(memory, Qmax, rt,id, mat) %>% filter(mat=='silt')  %>% 
      denplot(data=.) +
      geom_text(data=. %>% filter(id=='Kälviä_5') %>% distinct(id), 
                aes(x=Inf,y=Inf, label='(f)'), hjust=1, vjust=1) + 
      theme(legend.position = 'none')),
  nrow=1, widths=c(1,1),
  right=textGrob('silt', gp=gpar(fontsize=8), rot=0))
till <- grid.arrange(
  arrangeGrob(
    not_x %>% distinct(memory, Qmax, rt,id, mat) %>% filter(mat=='till') %>% 
      linplot(data=.) +
      geom_text(data=. %>% filter(id=='Abisko_10') %>% distinct(id), 
                aes(x=0,y=Inf, label='(g)'), hjust=-0.5, vjust=1) + 
      theme(legend.position = 'none')),
  arrangeGrob(
    not_x %>% distinct(memory, Qmax, rt,id, mat) %>% filter(mat=='till')  %>% 
      denplot(data=.) +
      geom_text(data=. %>% filter(id=='Abisko_10') %>% distinct(id), 
                aes(x=Inf,y=Inf, label='(h)'), hjust=1, vjust=1) + 
      theme(legend.position='none')),
  nrow=1, widths=c(1,1),
  right=textGrob('till', gp=gpar(fontsize=8), rot=0))
comb <- grid.arrange(rbind(csand, unsand, silt, till),
                     ncol=1#, 
                     # left=textGrob('response time (months)', gp=gpar(fontsize=8), rot=90),
                     # bottom=textGrob('memory (months)', gp=gpar(fontsize=8))
                     )
# grDevices::png(filename='figs_MS3/Figure5.png', width=100, height=140, units='mm',
               # res = 400, type='cairo-png')
grid.arrange(comb,  get_legend(x %>% filter(mat=="sand*") %>% 
                                 linplot(data=.) + 
                                 theme(legend.position = 'bottom',
                                       # legend.text = element_text(size=8),
                                       # legend.title = element_text(size=8),
                                       legend.key = element_rect(fill='white'))),
             ncol=1, heights=c(2,0.3)) %>% 
  ggsave(filename='figs_MS3/Figure5.pdf', width=110, height=140, units='mm', 
         device=cairo_pdf, dpi=400, plot=.)
dev.off()

# take only max auto range and max cross coeff ----
ca_sing <- cross_auto %>%group_by(id) %>% 
  distinct(id, place, Mmax, Qmax, maxcorr, region) %>% 
  filter(!is.na(maxcorr))  %>% 
  # filter(id!="Haapajärvi_1002p5"& id!="Haapajärvi_1002p7" &
  # place != "FI_Pyhäntä") %>%
  distinct(id, place, region, Qmax, Mmax, maxcorr) %>% 
  left_join(., level_stats)

cross_auto_a <- cross_auto_a %>%group_by(id) %>%
  mutate(Qmax = ifelse(max(corr, na.rm=TRUE)==corr, agg, NA),
         Qmax=first(na.omit(Qmax)),
         maxcorr = ifelse(Qmax==agg, corr, NA)) %>% 
  mutate() %>%
  filter(id!="Haapajärvi_1002p5"& id!="Haapajärvi_1002p7" &
           id != "Pyhäntä_1101p15" & id != "Pyhäntä_1101p4")

cross_auto_b <- cross_auto_b %>% group_by(id) %>%
  mutate(Qmax = ifelse(max(corr, na.rm=TRUE)==corr, agg, NA),
         Qmax=first(na.omit(Qmax)),
         maxcorr = ifelse(Qmax==agg, corr, NA)) %>% 
  filter(id!="Haapajärvi_1002p5"& id!="Haapajärvi_1002p7" &
           id != "Pyhäntä_1101p15" & id != "Pyhäntä_1101p4")

cross_auto_c <- cross_auto_c %>% group_by(id) %>%
  mutate(Qmax = ifelse(max(corr, na.rm=TRUE)==corr, agg, NA),
         Qmax=first(na.omit(Qmax)),
         maxcorr = ifelse(Qmax==agg, corr, NA))  %>% 
  filter(id!="Haapajärvi_1002p5"& id!="Haapajärvi_1002p7" &
           id != "Pyhäntä_1101p15" & id != "Pyhäntä_1101p4")

# make plot comparing autocorrelation to crosscorelations ----

plot_ref <- cross_auto %>% group_by(id, place, region) %>% 
  filter(auto==max(auto) & corr==max(corr)) %>% distinct(id, place, region, Qmax, Mmax, corr, auto) %>% 
  ggplot(.) +
  geom_point(aes(Mmax, Qmax, group=id, fill=place, colour=place, shape=region), size=2) + 

  geom_point(data= bloom_data, 
             aes(Mmax, Qmax, group=id, shape=region), colour="black", size=2) + 
  
  # geom_smooth(aes(Mmax, Qmax), level=0, method="lm") +
  scale_colour_viridis_d() +
  theme_bw() + 
  scale_fill_viridis_d() + 
  scale_shape_manual(values= c("Fenn" = 21,
                               "SE_26" = 22,
                               "SE_34" = 23,
                               "SE_37" = 24,
                     "BC" = 21,
                     "Bloomfield and Marchant 2013" = 4,
                     "Bloomfield et al. 2015" = 8)) +
  ylim(0,50) + xlim(0, 50) + geom_abline(slope=1, linetype=5, colour="grey") +
  geom_text(data = data.frame(label = c("reference period")),
            mapping = aes(x = -Inf, y = Inf, label = label), hjust = -0.2, vjust  = 1.2)
compfun <- function(x, title="reference period"){
  ggplot(x) +
    
    # geom_point(data=. %>% distinct(id, place, Mmax, Qmax, highcorrs) %>% 
    #              filter(!is.na(highcorrs) & highcorrs!=Mmax) %>% mutate(type="alt"),
    #            aes(highcorrs, Qmax, group=id, colour=place, shape=place), size=2) +
    
    scale_colour_viridis_d() +
    theme_bw() + 
    ylim(0,48) + xlim(0, 48) + geom_abline(slope=1, linetype=5, colour="grey") +
    geom_text(data = data.frame(label = c(title)),
              mapping = aes(x = -Inf, y = Inf, label = label), hjust = -0.2, vjust  = 1.2)  
}

drytext1 = "1990's drought"
drytext2 = "2000's drought"
drytext3 = "non-drought period"

plot_a <- cross_auto_a %>% compfun(., title=drytext1) + 
  geom_point(data=. %>% distinct(id, place, Mmax, Qmax, maxcorr) %>% 
               filter(!is.na(maxcorr)),
             aes(Mmax, Qmax, group=id, colour=place), size=2) 
    

plot_b <- cross_auto_b %>% compfun(., title=drytext2) +
  geom_point(data=. %>% distinct(id, place, Mmax, Qmax, maxcorr) %>% 
               filter(!is.na(maxcorr)),
             aes(Mmax, Qmax, group=id, colour=place), size=2) 

plot_c <- cross_auto_c %>% compfun(., title=drytext3) +
  geom_point(data=. %>% distinct(id, place, Mmax, Qmax, maxcorr) %>% 
               filter(!is.na(maxcorr)),
             aes(Mmax, Qmax, group=id, colour=place), size=2) 

plot_grid(plot_grid(plot_ref + theme(legend.position = "none"), 
          plot_a + theme(legend.position = "none"), 
          plot_b + theme(legend.position = "none"), 
          plot_c + theme(legend.position = "none")),
          get_legend(plot_ref + theme(legend.position = "bottom")),
          rel_heights = c(3, 0.5),
          ncol = 1)

# geo fig ----


auto_ref %>% rename(agg = lag) %>%
  left_join(., cross_ref %>% mutate(agg=as.numeric(agg))) %>% group_by(id) %>%
  mutate(Qmax = ifelse(max(corr, na.rm=TRUE)==corr, agg, NA),
         Qmax=first(na.omit(Qmax)),
         maxcorr = ifelse(Qmax==agg, corr, NA)) %>% 
  distinct(id, place, Mmax, Qmax, maxcorr, region, geo) %>% 
  filter(!is.na(maxcorr)) %>% 
  ggplot(.) +
  geom_point(aes(Mmax, Qmax, group=id, shape=geo, colour=geo), size=2) + 
  geom_point(data= bloom_data, 
             aes(Mmax, Qmax, group=id, shape=geo, colour=geo), size=2) + 
  scale_colour_manual(values=c("#E69F00", "#56B4E9", "#009E73", 
                               "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))   + theme_bw() +
  ylim(0,60) + xlim(0, 60) + geom_abline(slope=1, linetype=5, colour="grey") +
  geom_text(data = data.frame(label = c("reference period")),
            mapping = aes(x = -Inf, y = Inf, label = label), hjust = -0.2, vjust  = 1.2)



# plot cross and auto against well characteristics ----


library(ggrepel)
# plot_ref <- 
ca_sing %>%
  ggplot(.) +
  geom_point(aes(Mmax, Qmax, group=id, shape=aquifer.material, 
                 fill=landform, colour=landform), size=2) + 
  # geom_label_repel(data= .  %>% filter(Qmax/Mmax < 0.4),aes(Mmax, Qmax, label=place, group=place),
                   # size=2.5, label.padding = 0.1, alpha=.4) +
  
  geom_point(data= bloom_data %>% rename(aquifer.material = geo),
             aes(Mmax, Qmax, group=id, shape=aquifer.material), colour="black", size=2) +
  
  scale_colour_viridis_d(na.value="black") +   scale_fill_viridis_d(na.value="white") +
  theme_bw() + 
  scale_shape_manual(values= c("sand" = 25,
                               "sorted" = 22,
                               "till"=23,
                               "silt"=24,
                               "Limestone"=5,
                               "Sandstone" = 4,
                               "Chalk" = 8)) +
  geom_abline(slope=1, linetype=5, colour="grey") + 
  scale_y_continuous("cross-max",limits=c(0,50), expand=c(0,0)) + 
  scale_x_continuous("auto range",limits=c(0,50), expand=c(0,0))+
  geom_text(data = data.frame(label = c("reference period")),
            mapping = aes(x = -Inf, y = Inf, label = label), hjust = -0.2, vjust  = 1.2)

# outliers <- cross_auto %>% distinct(id, place, region, Qmax, Mmax, maxcorr) %>% 
  # left_join(., level_stats) %>% filter(Qmax/Mmax < 0.4)


compfun_c <- function(x, title="reference period"){
  plot <- ggplot(x) +
    
    # geom_point(data=. %>% distinct(id, place, Mmax, Qmax, highcorrs) %>% 
    #              filter(!is.na(highcorrs) & highcorrs!=Mmax) %>% mutate(type="alt"),
    #            aes(highcorrs, Qmax, group=id, colour=place, shape=place), size=2) +
    
    scale_colour_viridis_c(na.value="black") + scale_fill_viridis_c(na.value="white") +
    theme_bw() + ylab("cross-max") + xlab("auto-max")+
    ylim(0,48) + xlim(0, 48) + geom_abline(slope=1, linetype=5, colour="grey") +
    geom_text(data = data.frame(label = c(title)),
              mapping = aes(x = -Inf, y = Inf, label = label), hjust = -0.2, vjust  = 1.2) +
    scale_shape_manual(values= c("sand" = 25,
                                 "sorted" = 22,
                                 "till"=23,
                                 "silt"=24,
                                 "Limestone"=5,
                                 "Sandstone" = 4,
                                 "Chalk" = 8)) 
  return(plot)
}

compfun_d <- function(x, title="reference period"){
  plot <- ggplot(x) +
    
    # geom_point(data=. %>% distinct(id, place, Mmax, Qmax, highcorrs) %>% 
    #              filter(!is.na(highcorrs) & highcorrs!=Mmax) %>% mutate(type="alt"),
    #            aes(highcorrs, Qmax, group=id, colour=place, shape=place), size=2) +
    
    scale_colour_viridis_d(na.value="black") + scale_fill_viridis_d(na.value="white") +
    theme_bw() + ylab("cross-max") + xlab("auto-max")+
    ylim(0,48) + xlim(0, 48) + geom_abline(slope=1, linetype=5, colour="grey") +
    geom_text(data = data.frame(label = c(title)),
              mapping = aes(x = -Inf, y = Inf, label = label), hjust = -0.2, vjust  = 1.2) +
    scale_shape_manual(values= c("sand" = 25,
                                 "sorted" = 22,
                                 "till"=23,
                                 "silt"=24,
                                 "Limestone"=5,
                                 "Sandstone" = 4,
                                 "Chalk" = 8)) 
  return(plot)
}

drytext1 = "1990's drought"
drytext2 = "2000's drought"
drytext3 = "non-drought period"

plot_a <- cross_auto_a %>% group_by(id, place, region) %>% 
  filter(id!="Haapajärvi_1002p5"& id!="Haapajärvi_1002p7" &
           id != "Pyhäntä_1101p15" & id != "Pyhäntä_1101p4") %>% 
  distinct(id, place, region, Qmax, Mmax, maxcorr) %>% filter(!is.na(maxcorr)) %>%
  left_join(., level_stats) %>% 
  compfun_c(., title=drytext1) + 
  geom_point(aes(Mmax, Qmax, group=id, shape=aquifer.material, 
                 fill=maxcorr, colour=maxcorr), size=2
                 )

plot_b <- cross_auto_b %>% group_by(id, place, region) %>% 
  filter(id!="Haapajärvi_1002p5"& id!="Haapajärvi_1002p7" &
           id != "Pyhäntä_1101p15" & id != "Pyhäntä_1101p4") %>% 
  distinct(id, place, region, Qmax, Mmax, maxcorr) %>% filter(!is.na(maxcorr)) %>%
  left_join(., level_stats) %>% 
  compfun_c(., title=drytext2) + 
  geom_point(aes(Mmax, Qmax, group=id, shape=aquifer.material, 
                 fill=maxcorr, colour=maxcorr), size=2
                 )

plot_c <- cross_auto_c %>% group_by(id, place, region) %>% 
  filter(id!="Haapajärvi_1002p5"& id!="Haapajärvi_1002p7" &
           id != "Pyhäntä_1101p15" & id != "Pyhäntä_1101p4") %>% 
  distinct(id, place, region, Qmax, Mmax, maxcorr) %>% filter(!is.na(maxcorr)) %>%
  left_join(., level_stats) %>% 
  compfun_c(., title=drytext3) + 
  geom_point(aes(Mmax, Qmax, group=id, shape=aquifer.material, 
                 fill=maxcorr, colour=maxcorr), size=2
                 )

plot_grid(plot_grid(plot_ref + theme(legend.position = "none"), 
                    plot_a + theme(legend.position = "none"), 
                    plot_b + theme(legend.position = "none"), 
                    plot_c + theme(legend.position = "none")),
          get_legend(plot_ref + theme(legend.position = "bottom")),
          rel_heights = c(3, 0.5),
          ncol = 1)

c <- cross_auto %>% 
  filter(id!="Haapajärvi_1002p5"& id!="Haapajärvi_1002p7" &
           id != "Pyhäntä_1101p15" & id != "Pyhäntä_1101p4") %>%
  distinct(id, place, region, Qmax, Mmax, maxcorr) %>% 
  
  left_join(., cross_auto_c %>% group_by(id, place, region) %>% 
              filter(id!="Haapajärvi_1002p5"& id!="Haapajärvi_1002p7" &
                       id != "Pyhäntä_1101p15" & id != "Pyhäntä_1101p4")  %>% 
              filter(!is.na(maxcorr)) %>% mutate(Qmax_nondry=Qmax)%>%
              distinct(id, place, region, Qmax_nondry)) %>% 
  
  ggplot(.) + geom_point(aes(Qmax, Qmax_nondry, colour=maxcorr)) + 
  scale_colour_viridis_c() + ylim(0,50) + ggtitle(drytext3)
plot_grid(plot_grid(
  a + theme(legend.position = "none")+ geom_abline(slope=1, linetype=5, colour="grey"), 
  b + theme(legend.position = "none")+ geom_abline(slope=1, linetype=5, colour="grey"), 
  c + theme(legend.position = "none")+ geom_abline(slope=1, linetype=5, colour="grey")),
  get_legend(a + theme(legend.position = "bottom")),
  rel_heights = c(3, 0.5),
  ncol = 1)


# chars, correlation analysis to drought indices and characteristics ----
chars %>% filter(Apercent_lu>0.1 & grepl("forest", label)) %>% group_by(id) %>% 
  mutate(per = sum(Apercent_lu)) %>%
  ggplot(.) + geom_histogram(aes(per), bins=10)
library(rlang)
corfun <- function(data=., x, y){
  data= data%>% distinct(id, !! sym(x), !! sym(y))
  result = cor.test(data[[x]], data[[y]])
  data.frame(x, y, result[c("estimate","p.value","statistic","method")], 
             stringsAsFactors=TRUE)
}
# val="memory"
efs=c("rt", "Qmax", "memory", "slope1", "slope2")
vars = c(colnames(chars[14:37]), #35
         colnames(chars['aq_thick'])) #56
vars <- vars[vars!="label"]

corrs_3 = do.call(rbind, mapply(corfun, x=efs[1], y=vars, 
                              MoreArgs=list(data=chars), 
                              SIMPLIFY=FALSE)) %>% 
  full_join(., do.call(rbind, mapply(corfun, x=efs[2], y=vars, 
                                  MoreArgs=list(data=chars), 
                                  SIMPLIFY=FALSE))) %>% 
  full_join(., do.call(rbind, mapply(corfun, x=efs[3], y=vars, 
                                     MoreArgs=list(data=chars), 
                                     SIMPLIFY=FALSE))) %>% 
  full_join(., do.call(rbind, mapply(corfun, x=efs[4], y=vars, 
                                     MoreArgs=list(data=chars), 
                                     SIMPLIFY=FALSE))) %>% 
  full_join(., do.call(rbind, mapply(corfun, x=efs[5], y=vars, 
                                     MoreArgs=list(data=chars), 
                                     SIMPLIFY=FALSE)))
corrs_3k %>% #filter(p.value<0.1)
  mutate(p = ifelse(p.value<0.05, "p<0.05", NA),
         est = ifelse(p.value<0.1, estimate, NA)) %>% 
  ggplot(.) + 
  geom_tile(aes(x, y, fill=est))+
  geom_tile(aes(x, y, col=p), alpha=0)+
  scale_fill_viridis_c(limits=c(-1,1), na.value=NA) +
  scale_colour_manual(values=c("black")) +
  scale_x_discrete("correlation effects", expand_scale(mult = 0, add = 0)) + 
  scale_y_discrete(vars, limits=vars, breaks=vars, expand_scale(mult = 0, add = 0),
                   drop=FALSE) +
  guides(colour= "none") + 
  theme(panel.background = element_rect(colour="lightgrey", fill="white"),
        panel.grid = element_line(colour=alpha("lightgrey",0.5)),
        strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
        axis.ticks = element_line(colour="grey"),
        plot.margin = unit(c(0,0,0,0), "mm")) 
corrs_3 %>% #filter(p.value<0.1)
  mutate(p = ifelse(p.value<0.05, "p<0.05", NA),
         est = ifelse(p.value<0.1, estimate, NA)) %>% 
  ggplot(.) + 
  geom_tile(aes(x, y, fill=est))+
  geom_tile(aes(x, y, col=p), alpha=0)+
  scale_fill_viridis_c(limits=c(-1,1), na.value=NA) + 
  scale_colour_manual(values=c("black")) +
  scale_x_discrete(expand_scale(mult = 0, add = 0)) + 
  scale_y_discrete(expand_scale(mult = 0, add = 0)) +
  guides(colour= "none") + 
  theme(panel.background = element_rect(colour="lightgrey", fill="white"),
        panel.grid = element_line(colour=alpha("lightgrey",0.5)),
        strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
        axis.ticks = element_line(colour="grey"),
        plot.margin = unit(c(0,0,0,0), "mm"))

# chars %>% filter(grepl("Water", label)==TRUE) %>% group_by(id) %>%
#             mutate(Apercent_lu = sum(Apercent_lu)) %>% 
#   corfun(data=., x=efs[2], y="Apercent_lu")

chars %>% filter(grepl("forest", label)==TRUE) %>% group_by(id) %>%
  mutate(Apercent_lu = sum(Apercent_lu)) %>% 
  corfun(data=., x=efs[2], y="Apercent_lu")
chars %>% filter(grepl("arable", label)==TRUE | grepl("agri", label)==TRUE |
                   grepl("Pasture", label)==TRUE) %>% 
  group_by(id) %>%
  mutate(Apercent_lu = sum(Apercent_lu)) %>% 
  corfun(data=., x=efs[1], y="Apercent_lu")


el <- chars %>% distinct(id, elevation, med_all, med_med,
                         rt, memory, Qmax, Arange, cp.auto,
                         slope1, slope2) %>% na.omit() %>% 
  mutate(ma_masl = elevation-med_all, mm_masl = elevation-med_med)
corfun(data=el, x=efs[1], y="ma_masl")
corfun(data=el, x=efs[1], y="mm_masl")




chars %>%
  ggplot(.) +
  geom_point(aes(memory, dist_stream2, group=id, colour=log(dist_stream)), size=2) + 
  
  # geom_point(data= bloom_data %>% rename(aquifer.material = geo),
             # aes(Mmax, Qmax, group=id, shape=place), colour="black", size=2) +
  
  scale_colour_viridis_c(na.value="black") + 
  scale_shape_manual(values= c("Fenn" = 21,
                               "SE_26" = 22,
                               "SE_34" = 23,
                               "SE_37" = 24,
                               "BC" = 21,
                               "Bloomfield and Marchant 2013" = 4,
                               "Bloomfield et al. 2015" = 8)) +  
  geom_abline(slope=1, linetype=5, colour="grey") + 
  scale_y_continuous(expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0))
  # geom_text(data = data.frame(label = c("reference period")),
            # mapping = aes(x = -Inf, y = Inf, label = label), hjust = -0.2, vjust  = 1.2)

#combining Ezra's indices with the results ----

testfun <- function(x,y){ezra_indices %>% left_join(., cross_auto) %>% 
    mutate(comparison=ifelse(Mmax/Qmax<0.4, "<0.4", "linear"),
           comparison= ifelse(Mmax/Qmax>1.6, ">1.6",comparison),
           type=y) %>% 
    ggplot(.) + 
    
    # geom_point(aes(Mmax,Qmax,colour=comparison))+
    # geom_abline(slope=1, linetype=5, colour="grey") + 
    # xlab("auto range")+
    
    geom_boxplot(aes(comparison,{{x}}, fill=comparison))+
    theme(panel.background = element_rect(colour="lightgrey", fill="white"),
          panel.grid = element_line(colour=alpha("lightgrey",0.5)),
          strip.background = element_rect(fill=alpha("lightgrey", 0.5), colour="grey"),
          axis.ticks = element_line(colour="grey"),
          plot.margin = unit(c(0,0,0,0), "mm")
          # axis.text.x = element_text(angle=90)
    ) + facet_grid(~type, scales="free", space="free")}
plot_grid(plot_grid(testfun(colwell.M, y="seasonality")+theme(legend.position = "none"),
                    testfun(iaf.s, y="seasonality")+theme(legend.position = "none"),
                    testfun(colwell.C, y="density")+theme(legend.position = "none"), 
                    testfun(bimod, y="modality")+theme(legend.position = "none"),
                    testfun(dc.slp.01.09, y="boundness")+theme(legend.position = "none"),
                    testfun(dc.slp.02.08, y="boundness")+theme(legend.position = "none"),
                    testfun(dc.slp.025.075, y="boundness")+theme(legend.position = "none"),
                    testfun(l2, y="boundness")+theme(legend.position = "none"), ncol=2),
          get_legend(testfun(colwell.M,y="none")+theme(legend.position = "bottom")),
          rel_heights = c(1,0.1),ncol=1
)

test<-ezra_indices %>% left_join(., cross_auto) %>%mutate(diff=Mmax/Qmax)
cor.test(test$Mmax,test$iaf.s, method="spearman")$p.value <0.05
cor.test(test$Mmax,test$iaf.y, method="spearman")$p.value <0.05
cor.test(test$Mmax,test$l1, method="spearman")$p.value <0.05
cor.test(test$Mmax,test$l2, method="spearman")$p.value <0.05
cor.test(test$Mmax,test$l3, method="spearman")$p.value <0.05
cor.test(test$Mmax,test$l4, method="spearman")$p.value <0.05
cor.test(test$Mmax,test$bimod, method="spearman")$p.value <0.05
cor.test(test$Mmax,test$colwell.C, method="spearman")$p.value <0.05
cor.test(test$Mmax,test$colwell.M, method="spearman")$p.value <0.05
cor.test(test$Mmax,test$colwell.P, method="spearman")$p.value <0.05
cor.test(test$Mmax,test$cvmon.min, method="spearman")$p.value <0.05
cor.test(test$Mmax,test$cvmon.max, method="spearman")$p.value <0.05
cor.test(test$Mmax,test$dc.rng.01.09, method="spearman")$p.value <0.05
cor.test(test$Mmax,test$dc.rng.02.08, method="spearman")$p.value <0.05
cor.test(test$Mmax,test$dc.rng.025.075, method="spearman")$p.value <0.05
cor.test(test$Mmax,test$dc.slp.01.09, method="spearman")$p.value <0.05
cor.test(test$Mmax,test$dc.slp.02.08, method="spearman")$p.value <0.05
cor.test(test$Mmax,test$dc.slp.025.075, method="spearman")$p.value <0.05
cor.test(test$Mmax,test$med, method="spearman")$p.value <0.05
cor.test(test$Mmax,test$parde, method="spearman")$p.value <0.05

ind<-as.data.frame(t(ezra_indices[,-1])) 
colnames(ind) <- ezra_indices[,1]
library(tidyverse)
ind <- ind %>%  rownames_to_column() %>%
  pivot_longer(cols = -rowname) %>% rename(index=rowname,id=name)

ind  %>%
  left_join(cross_auto) %>%
  ggplot(.) +
  geom_point(aes(Mmax, value,  group=id, colour=landform), size=2) + 
  # scale_y_log10()+scale_x_log10()+
  
  scale_colour_viridis_d(na.value="white") +
  theme_bw() +
  scale_shape_manual(values= c("sand" = 25,
                               "sorted" = 22,
                               "till"=23,
                               "silt"=24,
                               "Limestone"=5,
                               "Sandstone" = 4,
                               "Chalk" = 8)) +
  geom_smooth(method="lm", level=0, aes(Mmax,value)) +
  facet_wrap(.~index, scale="free")


# plotting time series ----
a <- spi_sgi %>% #filter(place!="BC Fraser Valley" & place!="FI_Haapajärvi" & place!="SE_58") %>%
  # filter(place=="BC Fraser Valley" | place=="FI_Haapajärvi" | place=="SE_58") %>%
  # filter(id %in% outliers$id & id!="BC_ow255" & id!="Haapajärvi_1002p5"& id!="Haapajärvi_1002p7" &
  #          id != "Pyhäntä_1101p15" & id != "Pyhäntä_1101p4") %>%
  filter(id!="Haapajärvi_1002p5"& id!="Haapajärvi_1002p7" &
           id != "Pyhäntä_1101p15" & id != "Pyhäntä_1101p4") %>%
  unnest() %>%
  ggplot(.) + 
  scale_x_date(limits=c(as.Date("1980-01-01"),as.Date("2020-01-01")), expand = expand_scale(add=0)) +
  # geom_vline(aes(xintercept=date, colour=season, group=id), 
  #            alpha=.05, size=2)+ 
  geom_line(data= .%>% distinct(date,sgi,place) %>% mutate(type="sgi"), 
            aes(date, sgi, group=id)) + 
  geom_ribbon(data= . %>% distinct(date,sgi, place) %>% group_by(id) %>%
                mutate(sgi=ifelse(sgi<=0, sgi, NA), sev=ifelse(sgi<-0.99, "moderate", "mild"),
                       sev=ifelse(sgi<-1.49, "severe", sev), type="sgi"), 
              aes(date, ymin=sgi, ymax=0, group=id), alpha=0.05) + 
  
  # geom_line(data=. %>% group_by(id) %>% mutate(med=median(m, na.rm=TRUE)) %>%
              # distinct(date,m, med, place) %>% mutate(type="data"), aes(date, m-med, group=id, colour=id))+
  # geom_hline(yintercept=0) + 
  # scale_colour_manual(values=c("winter" = "navy",
                               # "summer" = "red")) +
  facet_grid(rows=vars(place), scales="free") + 
  theme(strip.text = element_text(size=6))


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














