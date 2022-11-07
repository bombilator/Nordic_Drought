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
                       "input/daily_minimum_temperature_FI_netcdf", full.names = TRUE)



ncP2df <- function(lsPnc, en) {
    
    bom <- nc_open(lsPnc)    
    # print(bom) # Inspect the data
    # Extract data
    lon <- ncvar_get(bom, "longitude")
    lat <- ncvar_get(bom, "latitude")
    dates <- as.Date("1800-01-01") + ncvar_get(bom, "Time")
    data <- ncvar_get(bom, en)
    dimnames(data) <- list(paste(lon, lat, sep="_"), as.character(dates))
      
      
  data.df <- data %>% 
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
  nest(.key = "daily") %>% ungroup() %>% 
  mutate(id = 1:n())

write_rds(tmin.df, "output/process/Tmindf.rds")

library(tiff)
lsGRad <- list.files(path  = "input/GlobalRad_10km_1961_2014_geotif", full.names = TRUE, pattern = "*.tif$")
getRasterData(readGDAL(lsGRad)[[1]])
grad.df <- as.data.frame(rasterToPoints(raster(lsGRad)))



# download CRU data Fennos ----------------------------------------------------------
#http://data.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.04/data/frs
# frost, PET, P and meanT

lsfrs <- list.files(path="Q:/Data/URC monthly TS 1901-2019/frs_1960-2019/",
                 full.names = TRUE, pattern="*.nc")
lsT <- list.files(path="Q:/Data/URC monthly TS 1901-2019/temp_1960-2019/",
                    full.names = TRUE, pattern="*.nc")
lsP <- list.files(path="Q:/Data/URC monthly TS 1901-2019/prec_1960-2019/",
                    full.names = TRUE, pattern="*.nc")
lspet <- list.files(path="Q:/Data/URC monthly TS 1901-2019/pet_1960-2019/",
                    full.names = TRUE, pattern="*.nc")
lswet <- list.files(path="Q:/Data/URC monthly TS 1901-2019/wet_days_1960-2019/",
                    full.names = TRUE, pattern="*.nc")
ncfun <- function(lsnc, en){
  bom <- nc_open(lsnc)    
  # print(bom) # Inspect the data
  # Extract data
  lon <- ncvar_get(bom, "lon")
  lat <- ncvar_get(bom, "lat")
  dates <- as.Date("1900-01-01") + ncvar_get(bom, "time")
  data <- ncvar_get(bom, varid=en)
  # dimnames(data) <- list(paste(lon, lat, sep="_"), as.character(dates))
  dimnames(data) <- list(lon, lat)
  nc_close(bom)
  
  data.df <- data %>% 
    as.data.frame %>%
    rownames_to_column(var="lon") %>%
    pivot_longer(cols = c(2:length(colnames(.))), names_to = "lat") %>% 
    mutate(date=dates) %>%
    as_tibble() %>% 
    mutate(date = as.Date(date)) %>%
    mutate_at(vars(lon:lat), as.numeric) 
  
  
  return(data.df)
}

frs.df <- lapply(X = lsfrs, FUN = ncfun, en="frs") %>% 
  bind_rows %>% 
  group_by(lon, lat) %>% 
  nest(.key = "monthly") 
saveRDS(frs.df, "output/process/frs.df.rds")
p.df <- lapply(X = lsP, FUN = ncfun, en="pre") %>% 
  bind_rows %>% 
  group_by(lon, lat) %>% 
  nest(.key = "monthly")
t.df <- lapply(X = lsT, FUN = ncfun, en="tmp") %>% 
  bind_rows %>% 
  group_by(lon, lat) %>% 
  nest(.key = "monthly")
pet.df <- lapply(X = lspet, FUN = ncfun, en="pet") %>% 
  bind_rows %>% 
  group_by(lon, lat) %>% 
  nest(.key = "monthly")
wet.df <- lapply(X = lswet, FUN = ncfun, en="wet") %>% 
  bind_rows %>% 
  group_by(lon, lat) %>% 
  nest(.key = "monthly")

urc.df <- p.df %>% unnest() %>% rename(prec = value) %>% 
  left_join(., t.df %>% unnest() %>% rename(temp = value)) %>% 
  left_join(., pet.df %>% unnest() %>% rename(pet = value)) %>% 
  left_join(., frs.df %>% unnest() %>% rename(frs = value)) %>%
  left_join(., wet.df %>% unnest() %>% rename(wet=value)) %>% 
  group_by(lon, lat) %>% nest(.key="monthly")
urc.df$id <- 1:length(row.names(urc.df))
saveRDS(urc.df, "output/process/urcdf.rds")

#check data on map
urc.df <- readRDS("output/process/urcdf.rds")
ok <- urc.df %>% unnest() %>% group_by(lon, lat) %>% mutate(year=year(date)) %>%
  group_by(lon, lat, year) %>% mutate(pet=sum(pet)) %>% 
  ungroup() %>% group_by(lon, lat) %>% 
  summarise(med_pet=median(pet))
ggmap(stamen) + geom_tile(data=ok, aes(lon,lat, fill=med_pet), alpha = .7) + 
  scale_fill_viridis_c(na.value="grey50")

# load URC data for BC ----
lsbc <- list.files(path="Q:/Projects/Drought Scandinavia/Michelle/Nordic_Drought/input/URC_BC",
                    full.names = TRUE, pattern="*.nc")
ncfun <- function(lsnc, en){
  bom <- nc_open(lsnc)    
  # print(bom) # Inspect the data
  # Extract data
  lon <- ncvar_get(bom, "lon")
  lat <- ncvar_get(bom, "lat")
  dates <- as.Date("1900-01-01") + ncvar_get(bom, "time")
  data <- ncvar_get(bom, varid=en)
  dimnames(data) <- list(paste(lon, lat, sep="_"))
  nc_close(bom)
  
  data.df <- data %>% 
    as.data.frame %>%
    rownames_to_column(var = "lonlat") %>% 
    as.tibble %>% 
    gather(date, value, - lonlat) %>% 
    mutate(date = as.Date(dates)) %>%
    separate(lonlat, into = c("lon", "lat"), sep = "_") %>% 
    mutate_at(vars(lon:lat), as.numeric) 
  
  return(data.df)
}

bc.df <- lapply(X = lsbc, FUN = ncfun, en="pre") %>% 
  bind_rows %>% 
  group_by(lon, lat) %>% 
  nest(.key = "monthly") 
saveRDS(bc.df, "output/process/URC_BC_P.rds")

# plot for random day ---------------------------------------------------------
    
Tmin.df <- readRDS("output/process/Tmindf.rds")
d <- "1961-10-10"
Tmin.df %>% dplyr::filter(id == 50) %>% unnest() %>% ggplot(.)  + 
  geom_line(aes(date, value))

# get SMHI data ---------------------------------------------------------------

# from luftweb/Tinghai
lsDat <- list.files(path = "input/sgi_met/14/", full.names = TRUE) #"data/raw/tinghai/Daily/"
lsDatna <- list.files(path = "input/sgi_met/14/") %>% 
  gsub('[[:alpha:]]', '', .) %>%
  gsub('([_])|[[:punct:]]', '\\1', .) %>% #as.numeric() %>% #prev expression ('[[:punct:]]', '', .)
  gsub('[[0]]', '', .)

read_tin_dat <- function(lspath) {
  met <- read.csv(lspath) # any nr of white space as delims: read.delim(lspath, sep="")
  return(met)
}
dat <- lapply(lsDat, function(lsDat){
  return(tryCatch(read_tin_dat(lsDat), error = function(e) NULL))
})
names(dat) <- lsDatna
dat.df <- dat[-which(sapply(dat, is.null))]
dat.df <- dat %>% bind_rows(., .id = "FNID") #%>%
  # mutate(date = as.Date(paste(YYYY, MM, DD, sep="-"), format = "%Y-%m-%d")) %>%
  # select(-YYYY, -MM, -DD)
# coors <- read.delim("data/raw/tinghai/ID_coordinates.txt", sep = "",
#                     colClasses = "character") %>%
#   mutate(E = as.numeric(Lon %>% gsub('.{1}$', '', .)), 
#          N = as.numeric(Lat %>% gsub('.{1}$', '', .))) %>% 
#   select(-Lon, -Lat)
# saveRDS(dat.df, "input/sgi_met/14/met.rds")
coors <- read.csv("data/raw/tinghai/ID_coordinates2.csv")
dat.df <- left_join(dat.df, coors %>% mutate(FNID=as.character(FNID)))

dat.df <- readRDS("R_finland_drought/output/process/swe_met.rds")
dat.df <- dat.df %>% mutate(Tm = Tm-272.15, Tmax = Tmax-272.15, Tmin = Tmin-272.15)
write_rds(dat.df, "R_finland_drought/output/process/swe_met.rds")


# Loading SMHI data from Anna Kuentz ----
lsP <- list.files(path = "C:/Users/xnygmi/Desktop/data/SWE/data/SMHI/PTHBV/1961/01", 
                  full.names = TRUE) # "input/PTHBV/"

bom <- nc_open(lsP)
names(bom$var)
attributes(bom$dim)$names
lon <- ncvar_get(bom, "lon")
lat <- ncvar_get(bom, "lat")
coor <- list(paste(lon, lat, sep = "_"))
date <- as.POSIXct(ncvar_get(bom, "time"), origin="1970-01-01 00:00:00") # hours since 1970-01-01 00:00:00
head(date)
val <- ncvar_get(bom, "p")
# val <- val[!sapply(val,is.na)] # look at all non-NaN values 
dimnames(val) <- list(coor[[1]]) #, as.character(date)) # other format of date etc so code does not work

p.df <- lapply(X=lsP, FUN=ncP2df, en="p") %>% 
  bind_rows
ncP2df <- function(lsPnc, en)
{
  
  bom <- nc_open(lsPnc)    
  # print(bom) # Inspect the data
  # Extract data
  lon <- ncvar_get(bom, "longitude")
  lat <- ncvar_get(bom, "latitude")
  dates <- as.POSIXct(ncvar_get(bom, "time"), origin="1970-01-01 00:00:00") 
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
