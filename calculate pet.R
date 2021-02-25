p.df <- readRDS("R_finland_drought/output/process/fin_p.rds") %>% 
  select(-daily_met) %>% unnest() %>%  # we only want monthly sums of P
  group_by(id)  %>% filter(date >= as.Date("1974-01-01")) # the time series differ in length,


# calculate PET ----
library(SPEI)
library(rgdal)
library(stringr)
### below: for paper 1 (all wells)
# corS <- readRDS("output/process/fennos_tibm_all2.rds") %>%
#   filter(str_detect(id, "s_"))
# coordinates(corS) = ~E+N
# proj4string(corS) = CRS("+init=epsg:3006")
# corS = spTransform(corS, CRS("+proj=longlat +datum=WGS84"))
# corF <- readRDS("output/process/fennos_tibm_all.rds") %>% 
#   filter(str_detect(id, "f_"))
# coordinates(corF) = ~E+N
# proj4string(corF) = CRS("+init=epsg:3067") 
# corF = spTransform(corF, CRS("+proj=longlat +datum=WGS84"))
# cor = full_join(as.data.frame(corF), as.data.frame(corS))
cor <- readRDS("input/sgi_met/14/met.rds") %>% #sgi paper, 3 met grids
  mutate(year= year(date), month = month(date)) 

# pet <- cor %>% #can't load tidyquant
#   ungroup() %>% 
#   group_by(id, E, N, year) %>% nest() %>%
#   mutate(monthly_T = data %>% #map(., group_by, N) %>%
#            map(., tq_transmute,
#                select= Tday,
#                mutate_fun = apply.monthly,
#                FUN = mean, 
#                na.rm = TRUE,
#                col_rename= "mean_T"))
pet <- cor %>% #filter(year >= 1965) %>% 
  group_by(id, E, N, year, month) %>% summarise(mean_T = mean(temp), sum_P=sum(precip)) %>% 
  group_by(id, E, N, year) %>% nest() %>% rename(monthly_T = data)

# pet2 <- pet %>% group_by(id, E, N) %>% select(-data)
spei_calc <- function (t, lat) {
  library(SPEI)
  pq <- lapply(lat, function (x) {
    data.frame(pet = as.numeric(SPEI::thornthwaite(t, x # the spi function requires a vector input
                                             ), 3))}
  )
  return(bind_rows(pq, .id = "N") %>% as.tibble)
}

for(i in pet$N){
  pet.df <- pet %>% 
    # ungroup() %>% group_by(id, E, N) %>%
    # nest %>%
    mutate(
      pet = monthly_T %>% map("mean_T") %>% map(spei_calc, i)
    )
}

saveRDS(pet.df, "output/process/clusters_pet191128.R") #paper1: fennos_pet_all.R ||| tibm_pet ||| fennos_pet

# change dates into same style as gw data for paper 1: ----
pet <- readRDS("output/process/fennos_pet_all.r")

pet <- pet %>% select(-data) %>% unnest %>% select(-N1) %>%
  group_by(id, E, N, year) %>% mutate(ymd=date) %>% filter(year>=1980 & year <=2010) %>% 
  complete(ymd = seq.Date(min(as.Date(paste0(year(ymd), "-01-01", sep=""), format="%Y-%m-%d")),
                          max(ymd), by="day")) %>%
  mutate(pet = pet/2, # divide monthly pet val to get biweekly representatives of pet 
         pet = zoo::na.locf(pet, fromLast = TRUE,
                            na.rm = FALSE),
         mean_T = zoo::na.locf(mean_T, fromLast = TRUE,
                               na.rm = FALSE)) 
saveRDS(pet, "output/process/fennos_pet_all.R") 




# PET using Evapotranspiration ----
library(Evapotranspiration)
# data("processeddata")
# data("constants")
# exresults <- ET.Penman(data, constants, ts="daily", solar="sunshine hours", wind = "yes",
#                        windfunction_ver = "1948", alpha = 0.08, z0 = 0.001,
#                        message="yes", save.csv="yes")
