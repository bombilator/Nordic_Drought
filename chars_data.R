library(magrittr); library(dplyr); library(readr); library(ggplot2)
rm(list=ls())
# metadata ----
# meta <- readRDS("input data/sgu_21_11.RDS")$meta 
# meta <- meta %>%
#   dplyr::rename(pid = Omr_stn, name = Namn, setting=Jordart, type=Akvifertyp, 
#          topo = TopoLage, welldep = RorLangd) %>%
#   dplyr::mutate(elevation=as.numeric(RefNivOkRor) - as.numeric(RorHojdOMark)) %>%
#   dplyr::select(pid, name, setting, type, topo, welldep, elevation)
# meta <- meta %>%
#   dplyr::mutate(
#     confinement = ifelse(T==grepl("slutet", type), "confined", NA),
#     confinement = ifelse(T==grepl("öppet", type), "unconfined", confinement),
#     type = ifelse(T==grepl("jord och berg", type), "mix", type),
#     type = ifelse(T==grepl("berg", type), "bedrock", type),
#     type = ifelse(T==grepl("jord", type), "sediment", type),
#     sed = ifelse(T==grepl("Grus", setting), "gravel", NA),
#     sed = ifelse(T==grepl("Lera", setting), "clay", sed),
#     sed = ifelse(T==grepl("Moränlera", setting), "clayey till", sed),
#     sed = ifelse(T==grepl("Morän", setting), "till", sed),
#     sed = ifelse(T==grepl("Sand", setting), "sand", sed),
#     sed = ifelse(T==grepl("Silt", setting), "silt", sed),
#     sed = ifelse(T==grepl("Torv", setting), "peat", sed)
#   )
# write_rds(meta, 'sweden_class/processed/meta.rds')
meta <- read_rds('sweden_class/processed/meta.rds')
library(rgdal);library(sf);library(rgeos)


# DESCRIPTORS (old)-----
library(rgdal);library(sf);library(rgeos)

#dist to stream

stream <- readOGR("C:/Users/xnygmi/Desktop/data/SWE/data/SMHI/flodeslinjer_vd_l_2016_3", 
                  "vd_l_2016_3")
g <- readr::read_rds("sweden_class/processed/data_sel_15yrs_filters.rds") %>%
  dplyr::distinct(pid) %>% 
  dplyr::left_join(., readRDS("sweden_class/input data/sgu_21_11.RDS")$meta  %>% 
                     dplyr::distinct(Omr_stn, N, E) %>% 
                     dplyr::rename(pid=Omr_stn, lon=N, lat=E))

sp.g <- g
coordinates(sp.g) = ~lon+lat
proj4string(sp.g) = CRS("+init=epsg:3006")
# sp.g = spTransform(sp.g, CRS("+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
sp.g <- SpatialPointsDataFrame(sp.g, data=g); rm(g)

n <- length(sp.g)
neareststream <- character(n)
diststream <- numeric(n)
library(rgeos); for(i in seq_along(neareststream)){
  dist.s <- gDistance(stream, sp.g[i,], byid=TRUE)
  neareststream[i] <- as.vector(apply(dist.s, 1, which.min))
  diststream[i] <- min(dist.s) 
}
sp.g$nearstream <- neareststream; sp.g$dist.s <- diststream

g.s <- sp.g %>% as.data.frame() %>% dplyr::select(-lon.1, -lat.1) %>%
  dplyr::mutate(dist.stream = round(dist.s, 2))
write_rds(g.s, 'sweden_class/processed/descriptors/dist_stream.rds')

# clip stream data
sf.g <- st_as_sf(sp.g)
# r300 <- st_buffer(st.g, dist = 300)
r1k <- st_buffer(sf.g, dist=5000); r1k <- st_transform(r1k, crs=3006)
# st_write(r1k, dsn= "sweden_class/processed/descriptors/1kmbuffer.shp")

sl <- st_as_sf(stream); sl <- st_transform(sl, crs=3006)

sl_r1k <- st_intersection(sl, r1k)

plot(st_geometry(sf.g %>% filter(pid=='3_2' | pid=='3_25' | pid=='3_26')),  
     pch = 16, col = 'black', cex=1)
plot(st_geometry(r1k %>% filter(pid=='3_2' | pid=='3_25' | pid=='3_26')),  
     add=T)
plot(st_geometry(sl_r1k %>% filter(pid=='3_2' | pid=='3_25' | pid=='3_26')), add=T)

sl_r1k$stream_l <- st_length(sl_r1k)
sl_r1k$stream_density <- sl_r1k$stream_l/(5000^2 * pi)

stream_density <- sl_r1k %>% as.data.frame() %>% distinct(pid, stream_l, stream_density)
write_rds(stream_density, 'sweden_class/processed/descriptors/dens_stream5k.rds')
rm(stream_density)


# land use polyons
gc()
landuse <- st_read("C:/Users/xnygmi/Desktop/data/world/CORINE land use/u2018_clc2018_v2020_20u1_geoPackage/u2018_clc2018_v2020_20u1_geoPackage/DATA/U2018_CLC2018_V2020_20u1.gpkg")
lu_legend <- read.csv("C:/Users/xnygmi/Desktop/data/world/CORINE land use/u2018_clc2018_v2020_20u1_geoPackage/u2018_clc2018_v2020_20u1_geoPackage/Legend/CLC_legend.csv",
                      sep=";")

r1k <- st_read("sweden_class/processed/descriptors/5kmbuffer.shp")
r1k <- st_transform(r1k, crs=3035)
lu_r1k <- st_intersection(landuse, r1k)


lu_r1k$area <- st_area(lu_r1k)
lu_r1k$area_percent <- lu_r1k$area/(pi * 5000^2)
plot((lu_r1k  %>% dplyr::filter(pid=='3_2' | pid=='3_25' | pid=='3_26'))["area_percent"])

plot(st_geometry(lu_r1k %>% dplyr::filter(pid=='3_2' | pid=='3_25' | pid=='3_26')), 
     col=lu_r1k$Code_18)
plot(st_geometry(sf.g  %>% filter(pid=='3_2' | pid=='3_25' | pid=='3_26')),  
     pch = 16, col = 'black', cex=3, add=TRUE)

lu_density <- lu_r1k %>% as.data.frame() %>% distinct(Code_18, pid, area_percent) %>%
  rename(CLC_CODE=Code_18)
lu_density <- left_join(lu_density, lu_legend %>% distinct(LABEL3, CLC_CODE) %>% 
                          mutate(CLC_CODE = as.factor(CLC_CODE)))

lu_r1k$clc_code = as.character(lu_r1k$Code_18)
lu_r1k <- lu_r1k %>% dplyr::left_join(., lu_legend %>% 
                                        dplyr::mutate(clc_code = as.character(CLC_CODE),
                                                      label = as.character(LABEL3)) %>% 
                                        dplyr::distinct(label, clc_code), by="clc_code")
write_sf(lu_r1k, dsn= "sweden_class/processed/descriptors/5km_landuse.shp")


# streams data----
swe <- readOGR("C:/Users/xnygmi/Desktop/data/SWE/data/SMHI/flodeslinjer_vd_l_2016_3", 
               "vd_l_2016_3")

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

g.s <- sp.g_s %>% as.data.frame()

g.stream <- g.s %>% distinct(Tunnus, Station, id, dist_stream, dist_stream2) 
saveRDS(g.stream, "processed/stream1_stream2_dist.rds")

library(sf)

swe_st <- st_read("C:/Users/xnygmi/Desktop/data/SWE/data/SMHI/flodeslinjer_vd_l_2016_3/vd_l_2016_3.shp")

st.g_s <- st_as_sf(x=sp.g_s, crs=3035)
swe_st <- st_transform(swe_st, crs = 3035)
dist.s <- st_distance(swe_st, st.g_s)


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


st.g <- sp.g_s %>% st_as_sf(x=., crs=3035)


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
# saveRDS(dist_lakes, "dist_lakes_r300.rds")
saveRDS(dist_lakes, "dist_wbodies_r3k.rds")
saveRDS(dist_lakes %>% rename(dist_river1=dist_lake1,
                              dist_river2 =dist_lake2), "fenn_dist_wcourses_r3k.rds")
# clip stream data
sl <- st_as_sf(swe); fl <- st_as_sf(fin)
sl <- sl %>% distinct(VDVattenId) %>% rename(id=VDVattenId)
sl <- st_transform(sl, crs=3035)
sl_r300 <- st_intersection(sl, r300)


plot(st_geometry(sl_r300 %>% filter(id=="s_2" | id=="s_1")))

sl_r300$stream_l <- st_length(sl_r300)
sl_r300$stream_density <- sl_r300$stream_l/(300^2 * pi)

swe_stream_density <- sl_r300 %>% as.data.frame() %>% distinct(Tunnus, Station, id, stream_l, stream_density)

saveRDS(stream_density, "fenn_stream_dens_r300.rds")

# get hydraulic and soil data from EU, soil depth ----
# raster file, like land use?
library(sf);library(rgeos);library(rgdal);library(raster)
# K_5cm <- raster("Q:/MICHELLE_PHD/EU_SoilHydroGrids_1km/EU_SoilHydroGrids_1km/KS_sl2.tif")
# K_15cm <- raster("Q:/MICHELLE_PHD/EU_SoilHydroGrids_1km/EU_SoilHydroGrids_1km/KS_sl3.tif")
# K_30cm <- raster("Q:/MICHELLE_PHD/EU_SoilHydroGrids_1km/EU_SoilHydroGrids_1km/KS_sl4.tif")
# K_60cm <- raster("Q:/MICHELLE_PHD/EU_SoilHydroGrids_1km/EU_SoilHydroGrids_1km/KS_sl5.tif")
# K_100cm <- raster("Q:/MICHELLE_PHD/EU_SoilHydroGrids_1km/EU_SoilHydroGrids_1km/KS_sl6.tif")
# K_200cm <- raster("Q:/MICHELLE_PHD/EU_SoilHydroGrids_1km/EU_SoilHydroGrids_1km/KS_sl7.tif")
sgu_rock <- raster("C:/Users/xnygmi/Desktop/data/SWE/data/SGU/hydr-konduktivitet-berg/hydr_konduktivitet_berg.tif")
sgu_depth <- raster("C:/Users/xnygmi/Desktop/data/SWE/data/SGU/m.nygren/lev1806/jorddjupsmodell/jorddjup_10x10m.tif")

st.g <- readRDS("sweden_class/input data/sgu_21_11.RDS")$meta %>% 
  dplyr::rename(pid=Omr_stn, N=E, E=N) %>% dplyr::distinct(pid, N, E) %>% 
  dplyr::filter(!is.na(N) & !is.na(E)) %>% dplyr::rename(lon=E, lat=N)
coordinates(st.g) = ~lon+lat
proj4string(st.g) = CRS("+init=epsg:3006")
# st.g = spTransform(st.g, CRS("+proj=longlat + datum=WGS84"))



# k5 <- extract(K_5cm, st.g) %>% cbind(st.g, .) %>% as.data.frame()
# colnames(k5)[2] <- 'KS'; k5$sl <- 2
# k15 <- extract(K_15cm, st.g) %>% cbind(st.g, .) %>% as.data.frame()
# colnames(k15)[2] <- 'KS'; k15$sl <- 3
# k30 <- extract(K_30cm, st.g)%>% cbind(st.g, .) %>% as.data.frame()
# colnames(k30)[2] <- 'KS'; k30$sl <- 4
# k60 <- extract(K_60cm, st.g)%>% cbind(st.g, .) %>% as.data.frame()
# colnames(k60)[2] <- 'KS'; k60$sl <- 5
# k100 <- extract(K_100cm, st.g)%>% cbind(st.g, .) %>% as.data.frame()
# colnames(k100)[2] <- 'KS'; k100$sl <- 6
# k200 <- extract(K_200cm, st.g)%>% cbind(st.g, .) %>% as.data.frame()
# colnames(k200)[2] <- 'KS'; k200$sl <- 7

# sgu <- extract(sgu_rock, st.g)%>% cbind(st.g, .) %>% as.data.frame()
# colnames(sgu)[2] <- 'logK'; sgu$sl <- 8
sgu <- extract(sgu_depth, st.g)%>% cbind(st.g, .) %>% as.data.frame()
colnames(sgu)[2] <- 'soildepth'



k <- rbind(k5, k15,k30,k60,k100,k200)
rm(K_5cm, K_15cm, K_30cm, K_60cm, K_100cm, K_200cm, k5, k15, k30, k60, k100, k200)

ggmap(stamen_fenn, darken = c(0.7, 'white')) +
  geom_point(data=k, aes(lon, lat, col=KS)) + facet_wrap(~sl) + 
  scale_colour_viridis_c()

# saveRDS(k, 'sweden_class/processed/descriptors/2-7KS_perOBS_1kmgrid.rds')
# saveRDS(sgu, 'sweden_class/processed/descriptors/sgu_K-rock.rds')
saveRDS(sgu, 'sweden_class/processed/descriptors/sgu_soildepth.rds')

# read data climate data E-OBS ----
# if prec, pattern='^rr_', if temp pattern='^tg_'
lsDat <- list.files(path = "sweden_class/input data/EObsTS/EObsTS/", 
                    full.names = T, pattern='^tg_')
lsDatna <- lsDat %>% 
  sub('.*(?=.{15}$)', '', ., perl=T) %>% 
  gsub('.[[:alpha:]]', '', .) %>% 
  gsub('/.','',.)


read_tin_dat <- function(lspath) {
  met <- read.table(lspath) 
  return(met)
}
dat <- lapply(lsDat, function(lsDat){
  return(tryCatch(read_tin_dat(lsDat), error = function(e) NULL))
})
names(dat) <- lsDatna
dat.df <- dat[-which(sapply(dat, is.null))]
dat.df <- dat %>% bind_rows(., .id = "FNID") 

# if pattern='^rr_'
# colnames(dat.df) <- c('FNID', 'year', 'month', 'day', 'prec')
# write_rds(dat.df, 'sweden_class/processed/descriptors/TO_prec.rds')
# else if pattern='^tg_'
colnames(dat.df) <- c('FNID', 'year', 'month', 'day', 'temp')
write_rds(dat.df, 'sweden_class/processed/descriptors/TO_temp.rds')
###
# dat.df <- read_rds('sweden_class/processed/TO_prec.rds')
# 
# pid <- readRDS("sweden_class/input data/sgu_21_11.RDS")$meta %>% 
#   dplyr::rename(pid=Omr_stn, N=E, E=N) %>% distinct(pid) 
# 
# ids <- dat.df %>% 
#   dplyr::distinct(FNID) 
# 
# ids$pid <- NA
# 
# for(i in 1:length(ids$pid)){
#   for(x in 1:length(pid$pid)){
#     ids$pid[i][grepl(paste0(pid[[x,1]],'$'), ids$FNID[i])] <- pid[[x,1]] 
#   }
# }
# head(ids)
# 
# dat.df <- dat.df %>% left_join(., ids, by='FNID')
# 
# write_rds(ids, 'sweden_class/processed/descriptors/ids_climatetowells.rds')
# write_rds(dat.df, 'sweden_class/processed/descriptors/TO_prec.rds')
ids <- read_rds('sweden_class/processed/descriptors/ids_climatetowells.rds')
dat.df <- read_rds('sweden_class/processed/descriptors/TO_temp.rds')
dat.df <- dat.df %>% left_join(., ids, by='FNID') # should only need to find ids once, for prec
write_rds(dat.df, 'sweden_class/processed/descriptors/TO_temp.rds')
###
prec <- read_rds('sweden_class/processed/descriptors/TO_prec.rds')
temp <- read_rds('sweden_class/processed/descriptors/TO_temp.rds')

data_sel <- read_rds("sweden_class/processed/data_sel_15yrs_filters.rds")

prec <- prec %>% dplyr::filter(pid %in% data_sel$pid)
temp <- temp %>% dplyr::filter(pid %in% data_sel$pid)


# transform yearmonday to date formst
prec <- prec %>% dplyr::mutate(date=as.Date(paste(year, month, day, sep='-'), 
                                            format='%Y-%m-%d')) %>% 
  dplyr::select(-year, -month, -day, -FNID)
temp <- temp %>% dplyr::mutate(date=as.Date(paste(year, month, day, sep='-'), 
                                            format='%Y-%m-%d'))%>% 
  dplyr::select(-year, -month, -day, -FNID)
climate <- prec %>% left_join(., temp, by=c('pid', 'date'))
write_rds(climate, 'sweden_class/processed/descriptors/climate.rds')


# analyse climate data E-OBS ----
# seasonality, annual climate

# mean summer P and T and wet spells, mean winter P and T and wet spells,
# annual P and T and wet spells.

data_sel <- read_rds("sweden_class/processed/data_sel_15yrs_00-15.rds")
climate <- read_rds('sweden_class/processed/descriptors/climate.rds') %>% 
  dplyr::filter(pid %in% data_sel$pid & between(year(date), 2000, 2015))

climate <- climate %>% mutate(group=gsub('_.', '', pid)) 
climate <- climate %>% group_by(group, date) %>%
  mutate(temp = ifelse(temp < (-50), NA, temp),
    temp = ifelse(is.na(temp), mean(temp, na.rm=T), temp),
    prec = ifelse(prec < 0, NA, prec),
    prec = ifelse(is.na(prec), mean(prec, na.rm=T), prec)) %>% 
  ungroup() %>% filter(!is.na(prec) | !is.na(temp))

# write.csv(climate, 'sweden_class/processed/descriptors/climate.csv')
library(lubridate); library(tidyr); library(tidyverse)

## source("rfuncs/tibm2.r")
source('sweden_class/tibm_new.R')
tibm <- climate %>%
  tibm2(P=.$prec, temp=.$temp, date=.$date, id=.$pid) %>% 
  dplyr::rename(pid=id)
saveRDS(tibm, "sweden_class/processed/tibm.rds")

# tibm <- read.csv('sweden_class/processed/Eva/tibm_output.csv') 
tibm <- read_rds('sweden_class/processed/tibm.rds') 

seasons <-  tibm %>% as.data.frame() %>% #rename(Q=prec) %>%
  dplyr::mutate(annual=year(date), 
                season= ifelse(between(month(date),3,5),'MAM',NA),
                season= ifelse(between(month(date),6,8),'JJA',season),
                season= ifelse(between(month(date),9,11),'SON',season),
                season= ifelse((month(date)==12 | 
                                  month(date)<3),'DJF',season),
                frostseason = ifelse(between(month(date), 5, 10), 
                                     'frost-free', 'frost'),
                Q=as.numeric(Q), pid=as.character(pid))
#if prec
# seasons <- seasons %>% dplyr::rename(Q=prec)

ws <- seasons %>% group_by(spell = data.table::rleid(Q >0.5)) %>% 
  dplyr::mutate(wetspell =  ifelse(Q>0.5, row_number(), 0L),
                wetspell = last(wetspell)) %>%
  dplyr::distinct(pid, wetspell, spell, annual, season, frostseason) %>% 
  
  group_by(pid, annual) %>%
  dplyr::mutate(ws_yr = max(wetspell)) %>% 
  group_by(pid) %>%
  dplyr::mutate(ws_yr = mean(ws_yr)) %>% 
  
  
  group_by(pid, annual, season) %>%
  dplyr::mutate(ws_s = max(wetspell)) %>% 
  group_by(pid, season) %>%
  dplyr::mutate(ws_s = mean(ws_s)) %>% 
  
  
  group_by(pid, annual, frostseason) %>%
  dplyr::mutate(ws_fs = max(wetspell)) %>% 
  group_by(pid, frostseason) %>%
  dplyr::mutate(ws_fs = mean(ws_fs)) %>% 
  
  distinct(pid, season, frostseason, ws_yr, ws_s, ws_fs)
  
pt <- seasons %>% group_by(annual, pid) %>% 
  dplyr::mutate(p_yr = sum(Q),
                # t_yr = mean(temp)
                ) %>% 
  group_by(pid) %>% 
  dplyr::mutate(p_yr = mean(p_yr),
                # t_yr = mean(t_yr)
                ) %>% 
  
  
  group_by(pid, annual, season) %>% 
  dplyr::mutate(p_s = sum(Q),
                # t_s = mean(temp)
                ) %>%  
  group_by(pid, season) %>% 
  dplyr::mutate(p_s = mean(p_s),
                # t_s = mean(t_s)
                ) %>% 
  
  
  group_by(pid, annual, frostseason) %>% 
  dplyr::mutate(p_fs = sum(Q),
                # t_fs = mean(temp)
                ) %>% 
  group_by(pid, frostseason) %>% 
  dplyr::mutate(p_fs = mean(p_fs),
                # t_fs = mean(t_fs)
                ) %>% 
  
  group_by(pid) %>%
  dplyr::distinct(pid, p_yr, season, frostseason, p_s, p_fs, 
                  # t_s, t_yr, t_fs
                  )

join <- pt %>% left_join(., ws)

# write_rds(join, 'sweden_class/processed/descriptors/climate_processed.rds')
# write_rds(join, 'sweden_class/processed/descriptors/tibm_processed.rds')

# combine all -----
rm(list=ls())
library(sf)
# ts<- read_rds('sweden_class/input data/ts.rds') %>% group_by(pid) %>% 
#   filter(!is.na(m) & !is.na(level_masl))
# ind_wkl <- read_rds("sweden_class/processed/indices_monthly_15yrs-00-15.rds")
# meta <- read_rds('sweden_class/input data/meta_4clusts.rds') %>% 
#   left_join(., ts %>% dplyr::distinct(pid, lon, lat), by='pid')
lu <- st_read("sweden_class/processed/descriptors/5km_landuse.shp") %>%
  as.data.frame()
forest <- lu %>% dplyr::filter(grepl("forest", label)==TRUE) %>% group_by(pid) %>%
  summarise(forest = sum(area)); fields <- lu %>% 
  dplyr::filter(grepl("arable", label)==T | grepl("agri", label)==T |
                  grepl("Pasture", label)==T | grepl('grass', label)==T) %>% group_by(pid) %>%
  summarise(fields = sum(area)); water <- lu %>% 
  dplyr::filter(grepl("Water", label)==T) %>% group_by(pid) %>%
  summarise(water = sum(area)) 
stream <-  read_rds('sweden_class/processed/descriptors/dist_stream.rds') %>%
  dplyr::distinct(pid, dist.stream) %>% 
  left_join(., read_rds('sweden_class/processed/descriptors/dens_stream5k.rds') %>%
              group_by(pid) %>% dplyr::summarise(stream_l = sum(stream_l, na.rm=T),
                                                 stream_density = sum(stream_density, na.rm=T)))
tibm <- read_rds('sweden_class/processed/descriptors/tibm_processed.rds') 
winter <- tibm %>% dplyr::select(-frostseason) %>% 
  dplyr::filter(season=='DJF') %>% distinct(pid, p_s, ws_s) 
fall = tibm %>% dplyr::select(-frostseason) %>% 
  dplyr::filter(season=='SON') %>% distinct(pid, p_s, ws_s)
summer <- tibm %>% dplyr::select(-frostseason) %>% 
  dplyr::filter(season=='JJA') %>% distinct(pid, p_s, ws_s)
spring <- tibm %>% dplyr::select(-frostseason) %>% 
  dplyr::filter(season=='MAM') %>% distinct(pid, p_s, ws_s)
frost <- tibm %>% dplyr::select(-season) %>% 
  dplyr::filter(frostseason=='frost') %>% distinct(pid, p_fs, ws_fs)
ff <- tibm %>% dplyr::select(-season) %>% 
  dplyr::filter(frostseason!='frost') %>% distinct(pid, p_fs, ws_fs)
yr <- tibm %>% dplyr::distinct(pid, p_yr, ws_yr) 
colnames(winter) <- c('pid', 'q_DJF', 'qws_DJF')
colnames(fall) <- c('pid', 'q_SON',  'qws_SON')
colnames(summer) <- c('pid', 'q_JJA',  'qws_JJA')
colnames(spring) <- c('pid', 'q_MAM',  'qws_MAM')
colnames(frost) <- c('pid', 'q_frost',  'qws_frost')
colnames(ff) <- c('pid', 'q_ff', 'qws_ff')
colnames(yr) <- c('pid', 'q_yr', 'qws_yr')

climate <- read_rds('sweden_class/processed/descriptors/climate_processed.rds') 
pwinter <- climate %>% dplyr::select(-frostseason) %>% 
  dplyr::filter(season=='DJF') %>% distinct(pid, p_s, ws_s, t_s)
pfall = climate %>% dplyr::select(-frostseason) %>% 
  dplyr::filter(season=='SON') %>% distinct(pid, p_s, ws_s, t_s)
psummer <- climate %>% dplyr::select(-frostseason) %>% 
  dplyr::filter(season=='JJA') %>% distinct(pid, p_s, ws_s, t_s)
pspring <- climate %>% dplyr::select(-frostseason) %>% 
  dplyr::filter(season=='MAM') %>% distinct(pid, p_s, ws_s, t_s)
pyr <- climate %>% dplyr::distinct(pid, p_yr, ws_yr, t_yr) 
pff <- climate %>% dplyr::select(-season) %>% 
  dplyr::filter(frostseason=='frost-free') %>% dplyr::distinct(pid, p_fs, ws_fs, t_fs) 
pfrost <- climate %>% dplyr::select(-season) %>% 
  dplyr::filter(frostseason!='frost-free')%>% dplyr::distinct(pid, p_fs, ws_fs, t_fs) 
colnames(pyr) <- c('pid', 'p_yr', 'pws_yr','t_yr')
colnames(pwinter) <- c('pid', 'p_DJF', 'pws_DJF','t_DJF')
colnames(pfall) <- c('pid', 'p_SON', 'pws_SON','t_SON')
colnames(psummer) <- c('pid', 'p_JJA','pws_JJA', 't_JJA')
colnames(pspring) <- c('pid', 'p_MAM', 'pws_MAM', 't_MAM')
colnames(pfrost) <- c('pid', 'p_frost', 'pws_frost', 't_frost')
colnames(pff) <- c('pid', 'p_ff', 'pws_ff', 't_ff')

library(tidyr)
soil <- read_rds('sweden_class/processed/descriptors/2-7KS_perOBS_1kmgrid.rds') %>%
  dplyr::mutate(sl=as.factor(sl)) %>% 
  pivot_wider(values_from=KS, names_from=sl) %>% dplyr::select(-lon, -lat)
colnames(soil) <- c('pid', 'KS2', 'KS3', 'KS4', 'KS5', 'KS6', 'KS7')

sgu<- read_rds('sweden_class/processed/descriptors/sgu_K-rock.rds') %>%
  dplyr::select(-lon,-lat,-sl)
soildepth <- read_rds('sweden_class/processed/descriptors/sgu_soildepth.rds') %>%
  dplyr::select(-lon, -lat)

comb <- stream %>% left_join(., forest, by='pid') %>%
  left_join(., fields, by='pid') %>% left_join(., water, by='pid') %>% 
  left_join(., winter, by='pid') %>% left_join(., spring, by='pid') %>% 
  left_join(., fall, by='pid') %>%
  left_join(., summer, by='pid') %>% left_join(., frost, by='pid') %>% 
  left_join(., ff, by='pid') %>%
  left_join(., yr, by='pid') %>% left_join(., pyr, by='pid') %>% 
  left_join(., pwinter, by='pid') %>% 
  left_join(., pspring, by='pid') %>% left_join(., pfall, by='pid') %>% 
  left_join(., psummer, by='pid') %>% 
  left_join(., pfrost, by='pid') %>% 
  left_join(., pff, by='pid') %>% 
  left_join(., soil, by='pid') %>% left_join(., sgu, by='pid') %>% 
  left_join(., soildepth, by='pid') 
# rm(lu, forest, fields, water, stream, climate, winter, fall, summer, spring, 
#    frost, ff, yr, soil)

write_rds(comb, 'sweden_class/processed/descriptors/combined-test.rds')
