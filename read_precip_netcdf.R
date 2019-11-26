# read netcdf data ------------------------------------------------------------
rm(list = ls())
library(ncdf4)
library(tidyverse)
library(RColorBrewer)
library(viridis)
library(magrittr)

# Load Finnish data ----
lsP <- list.files(path = "input/daily_precipitation_FI_netcdf/", full.names = TRUE)
lsHum <- list.files(path = "input/daily_avg_rel_hum_FI_netcdf/", full.names = TRUE)
lsT <- list.files(path  = "R_finland_drought/input/daily_mean_temperature_FI_netcdf", full.names = TRUE)

lsTmax <- list.files(path  = 
                       "R_finland_drought/input/daily_maximum_temperature_FI_netcdf", full.names = TRUE)
lsTmin <- list.files(path  = 
                       "R_finland_drought/input/daily_minimum_temperature_FI_netcdf", full.names = TRUE)



ncP2df <- function(lsPnc, en) {
    
    bom <- nc_open(lsPnc)    
    # print(bom) # Inspect the data
    # Extract data
    lon <- ncvar_get(bom, "longitude")
    lat <- ncvar_get(bom, "latitude")
    dates <- as.Date("1800-01-01") + ncvar_get(bom, "Time")
    data <- ncvar_get(bom, en)
    dimnames(data) <- list(paste(lon, lat, sep="_"), as.character(dates))
      
      
  data.df <-data %>% 
      as.data.frame %>%
      rownames_to_column(var = "lonlat") %>% 
      as.tibble %>% 
      gather(date, value, - lonlat) %>% 
      mutate(date = as.Date(date)) %>%
      separate(lonlat, into = c("lon", "lat"), sep = "_") %>% 
      mutate_at(vars(lon:lat), as.numeric) 
  
  return(data.df)
}

p.df <- lapply(X=lsP, FUN = ncP2df, en="RRday") %>%
  bind_rows %>%
  group_by(lon,lat) %>%
  nest(.key="daily") %>%
  mutate(id=1:n())

hum.df <- lapply(X = lsHum, FUN = ncP2df, en="Hum") %>% 
    bind_rows %>% 
    rename(Hum = value) %>%
    group_by(lon, lat) %>% 
    nest(.key = "daily") %>% 
    mutate(id = 1:n())

write_rds(hum.df, "output/process/Humdf.rds")

tmax.df <- lapply(X = lsTmax, FUN = ncP2df, en="Tmax") %>% 
  bind_rows %>% 
  # rename(Tday = value) %>%
  group_by(lon, lat) %>% 
  nest(.key = "daily") %>% 
  mutate(id = 1:n())

tmin.df <- lapply(X = lsTmin, FUN = ncP2df, en="Tmin") %>% 
  bind_rows %>% 
  # rename(Tday = value) %>%
  group_by(lon, lat) %>% 
  nest(.key = "daily") %>% 
  mutate(id = 1:n())

write_rds(tmax.df, "R_finland_drought/output/process/Tmaxdf.rds")

library(tiff)
lsGRad <- list.files(path  = "input/GlobalRad_10km_1961_2014_geotif", full.names = TRUE, pattern = "*.tif$")
getRasterData(readGDAL(lsGRad)[[1]])
grad.df <- as.data.frame(rasterToPoints(raster(lsGRad)))




# plot for random day ---------------------------------------------------------
    
p.df <- readRDS("R_finland_drought/output/process/Pdf.rds")
d <- "1961-10-10"
p.df %>% dplyr::filter(id == 50) %>% unnest() %>% ggplot(.)  + 
  geom_line(aes(date, value))

# get SMHI data ---------------------------------------------------------------

# from Tinghai
lsDat <- list.files(path = "data/raw/tinghai/Daily/", full.names = TRUE)
lsDatna <- list.files(path = "data/raw/tinghai/Daily/") %>% 
  gsub('[[:alpha:]]', '', .) %>%
  gsub('[[:punct:]]', '', .) %>% as.numeric() %>% 
  gsub('[[0]]', '', .)

read_tin_dat <- function(lspath) {
  met <- read.delim(lspath, sep= "") # any nr of white space as delims
  return(met)
}
dat <- lapply(lsDat, function(lsDat){
  return(tryCatch(read_tin_dat(lsDat), error = function(e) NULL))
})
names(dat) <- lsDatna
dat.df <- dat[-which(sapply(dat, is.null))]
dat.df <- dat %>% bind_rows(., .id = "FNID") %>%
  mutate(date = as.Date(paste(YYYY, MM, DD, sep="-"), format = "%Y-%m-%d")) %>%
  select(-YYYY, -MM, -DD)
# coors <- read.delim("data/raw/tinghai/ID_coordinates.txt", sep = "",
#                     colClasses = "character") %>%
#   mutate(E = as.numeric(Lon %>% gsub('.{1}$', '', .)), 
#          N = as.numeric(Lat %>% gsub('.{1}$', '', .))) %>% 
#   select(-Lon, -Lat)
coors <- read.csv("data/raw/tinghai/ID_coordinates2.csv")
dat.df <- left_join(dat.df, coors %>% mutate(FNID=as.character(FNID)))

dat.df <- readRDS("R_finland_drought/output/process/swe_met.rds")
dat.df <- dat.df %>% mutate(Tm = Tm-272.15, Tmax = Tmax-272.15, Tmin = Tmin-272.15)
write_rds(dat.df, "R_finland_drought/output/process/swe_met.rds")


# Loading SMHI data from Semjon
# bom <- nc_open("data/raw/netcdf/RR24_Sweden_1961-2017.nc")
# bom <- nc_open("data/raw/netcdf/T2m_daymean_Sweden_1961-2017.nc")
# bom <- nc_open(lsT)
# names(bom$var)
# attributes(bom$dim)$names
# lon <- ncvar_get(bom, "lon_bnds")
# lat <- ncvar_get(bom, "lat_bnds")
# coor <- list(paste(lon, lat, sep = "_")) 
# date <- as.Date("1961-01-01 00:00:00") + ncvar_get(bom, "time_bnds") # hours since 1961-01-01 00:00:00
# head(date)
# val <- ncvar_get(bom, "tp")
# dimnames(val) <- list(paste(lon, lat, sep="_"), as.character(date))
# ncP2df <- function(lsPnc, en) 
{
  
  bom <- nc_open(lsPnc)    
  # print(bom) # Inspect the data
  # Extract data
  lon <- ncvar_get(bom, "longitude")
  lat <- ncvar_get(bom, "latitude")
  dates <- as.Date("1800-01-01") + ncvar_get(bom, "Time")
  data <- ncvar_get(bom, en)
  dimnames(data) <- list(paste(lon, lat, sep="_"), as.character(dates))
  
  
  data.df <-data %>% 
    as.data.frame %>%
    rownames_to_column(var = "lonlat") %>% 
    as.tibble %>% 
    gather(date, value, - lonlat) %>% 
    mutate(date = as.Date(date)) %>%
    separate(lonlat, into = c("lon", "lat"), sep = "_") %>% 
    mutate_at(vars(lon:lat), as.numeric) 
  
  return(data.df)
}
