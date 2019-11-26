# calculate meteorological indices
# 1, using the SPEI function with probability distributions
# 2, using the Weibull plotting position??: Stagge et al 2015
#     |--> https://rmets.onlinelibrary.wiley.com/doi/pdf/10.1002/joc.4267

rm(list=ls())
library(lubridate)

#
# 1, SPEI ----
#
# source("rfuncs/spi_.r")
# p.df <- readRDS("output/process/fin_p.rds") %>% 
#   select(-daily_met) %>% unnest() %>%  # we only want monthly sums of P
#   group_by(id)  %>% filter(date >= as.Date("1974-01-01")) # the time series differ in length,
#  ^ get them in the same time frame

tibm<- readRDS("output/process/3clusters_tibm.RDS") %>% rename(Cluster=id) %>% select(Cluster, date, Q, M)
met <- readRDS("input/sgi_met/met.RDS") %>% dplyr::filter(as.Date(date) >= as.Date("1965-01-01")) %>% 
  full_join(., tibm)
met <- met %>%mutate(year=year(date), month=month(date)) %>%  group_by(Cluster, N,E,year, month) %>% 
  summarise(sum_P=sum(precip), sum_Q=sum(Q))
met <- met %>% rename(id=Cluster)

# Ezra's algorithm for SPI/SMRI ----
PQ_index_mutate <- function (sumq, agg) {
  library(SPEI)
  
  pq <- lapply(agg, function (x) {
    data.frame(index = as.numeric(SPEI::spi(sumq, x, distribution = "Gamma")$fitted, 3))} # the spi function requires a vector input
  )
  
  return(bind_rows(pq, .id = "agg") %>% as.tibble)
}


library(purrr)
library(tidyr)
library(tibble)
# calculate SPI ----
spi.ind <- met %>% 
  na.omit(met) %>% 
  group_by(id, N,E) %>% 
  nest %>% 
  mutate(
    spi = data %>% map("sum_P") %>% map(PQ_index_mutate, 1:48)
  ) 


# get dates ----
# try to get them in the same time frame
# get time
# p.time <- p.df %>% mutate(month_past= 1:length(date)) %>% 
#   select(id, ymd, month_past)
p.time <- met %>% mutate(date = as.Date(paste(year, month, "15", sep="-"), format="%Y-%m-%d")) %>% 
  select(id, date,year,month) %>% ungroup() %>% group_by(id) %>% 
  mutate(month_past = 1:length(date)) %>% filter(!is.na(date))
spi.ind <- spi.ind %>% dplyr::select(-data) %>% unnest() %>% group_by(id, agg) %>% 
  mutate(month_past = 1:length(index)) %>% left_join(., p.time, by=c("id", "month_past", "N", "E"))

saveRDS(spi.ind, "output/process/3clusters_spi.rds")


# calculate tibm ----
# source("Rscripts/tibm2.R")
# tibm_df <- finland.df %>% tail(10) %>% unnest() %>% group_by(id) %>%
# tibm2(df = ., P = .$RRday)
# tibm.df <- tibm_df %>% filter(date >= as.Date("1974-01-01")) %>%
# mutate(month = month(date),
# year = year(date)) %>%
# group_by(id, year, month) %>%
# mutate(sum_Q = sum(Q),
# date = as.Date(paste0(format(as.Date(date), "%Y-%m"), "-15", sep = ""))) %>%
# distinct(id, date, year, month, sum_Q)  %>% ungroup()
# calculation SMRI -----
tibm.ind <- met %>% na.omit(met) %>% 
  group_by(id, N, E) %>%
  nest %>%
  mutate(
    smri = data %>% map("sum_Q") %>% map(PQ_index_mutate, 1:48)
  )
tibm.ind <- tibm.ind %>% select(-data) %>% unnest() %>% group_by(id, agg) %>% 
  mutate(month_past = 1:length(index)) %>% left_join(., p.time, by=c("id", "month_past", "N", "E"))
saveRDS(tibm.ind, "output/process/3clusters_smri.rds")


# SPEI = SPI and SMRI - PET ----
# pet<- pet %>% mutate(p_pet = P_mon - pet,
#                      p_pet = ifelse(p_pet < 0, 0, p_pet)) %>% # why did I do this? doesn't seem to be 
#                       according to the instructions for the spei function
# filter(date >= as.Date("1974-01-01"))

tibm <- readRDS("output/process/3clusters_tibm.rds") %>% rename(Cluster=id) %>%
  mutate(year=year(date), month=month(date)) %>%
  select(Cluster, Q, year, month) %>% group_by(Cluster, year, month) %>%
  summarise(sum_Q = sum(Q))
pet <- readRDS("output/process/3clusters_pet.R") %>% 
  unnest() %>% select(-N1) %>% rename(Cluster=id) %>% 
  mutate(p_pet = sum_P - pet) %>% full_join(., tibm) %>% mutate(q_pet = sum_Q - pet)

# SPEI algorithm----
PQspei_index_mutate <- function (q_pet, agg) {
  library(SPEI)
  
  pq <- lapply(agg, function (x) {
    data.frame(index = as.numeric(SPEI::spei(q_pet, x, # the spi function requires a vector input
                                             distribution = "log-Logistic")$fitted, 3))}
  )
  
  return(bind_rows(pq, .id = "agg") %>% as.tibble)
}

# implement SPEI algorithm ----
# SMREI ----
spei_tibm.df_ind <- pet %>% rename(id = Cluster) %>% filter(!is.na(pet)) %>% 
  ungroup() %>% group_by(id) %>%
  nest %>%
  mutate(
    spei_tibm = data %>% map("q_pet") %>% map(PQspei_index_mutate, 1:48)
  )


spei_tibm.df_ind <- spei_tibm.df_ind %>%
  select(-data) %>% unnest %>% group_by(id, agg) %>% mutate(month_past = 1:length(index))
spei_tibm.df_ind <- left_join(spei_tibm.df_ind, p.time %>%
                                filter(id %in% 
                                         spei_tibm.df_ind$id), 
                              by=c("id", "month_past")) %>%
  select(-month_past) 
saveRDS(spei_tibm.df_ind, "output/process/3cluster_smrei.rds")

# SPEI ----
spei.df_ind <- pet %>% rename(id = Cluster) %>% filter(!is.na(pet)) %>% 
  ungroup() %>% group_by(id) %>%
  nest %>%
  mutate(
    spei = data %>% map("p_pet") %>% map(PQspei_index_mutate, 1:48)
  )
spei.df_ind <- spei.df_ind  %>%
  select(-data) %>% unnest %>% group_by(id, agg) %>% mutate(month_past = 1:length(index))
spei.df_ind <- left_join(spei.df_ind, p.time %>%
                           filter(id %in% 
                                    spei.df_ind$id), 
                         by=c("id", "month_past")) %>%
  select(-month_past) 
saveRDS(spei.df_ind, "output/process/3clusters_spei.rds")

#
# 2, Weibull plotting distribution? ----
# 

# data
tibm <- readRDS("output/process/3clusters_tibm.RDS") %>% select(Cluster, date, Q, M)
met <- readRDS("input/sgi_met/met.RDS") %>% dplyr::filter(as.Date(date) >= as.Date("1970-01-01")) %>% 
  full_join(., tibm); rm(tibm)
met <- met %>%mutate(year=year(date), month=month(date)) %>%  group_by(Cluster, N,E,year, month) %>% 
  summarise(sum_P=sum(precip), sum_Q=sum(Q))
met <- met %>% rename(id=Cluster)
pet <- readRDS("output/process/3clusters_pet.R") %>% 
  unnest() %>% select(-N1) %>% rename(Cluster=id) %>% 
  mutate(p_pet = sum_P - pet) %>% full_join(., met) %>% mutate(q_pet = sum_Q - pet, p_pet = sum_P - pet)




#
# 2, Weibull plotting positions ----
#
library(ExtDist)

tibm<- readRDS("output/process/3clusters_tibm.RDS") %>% rename(Cluster=id) %>% select(Cluster, date, Q, M)
met <- readRDS("input/sgi_met/met.RDS") %>% dplyr::filter(as.Date(date) >= as.Date("1970-01-01")) %>% 
  left_join(., tibm); rm(tibm)
met <- met %>%mutate(year=year(date), month=month(date)) %>%  group_by(Cluster, N,E,year, month) %>% 
  summarise(sum_P=sum(precip), sum_Q=sum(Q))
met <- met %>% rename(id=Cluster)
pet <- readRDS("output/process/3clusters_pet.R") %>% 
  unnest() %>% select(-N1) %>% 
  mutate(p_pet = sum_P - pet) %>% full_join(., met) %>% mutate(q_pet = sum_Q - pet); rm(met)

# https://stackoverflow.com/questions/39781527/cumulative-sum-of-a-division-with-varying-denominators-r
# new <- data.frame(id = matrix(pet$id, nrow=length(pet$id))) %>% group_by(id)
# new$agg[1] = NA  
# for(i in 1:48){
#   for(j in i:length(pet$sum_P)){
#     new$agg[j] = i
#     new$plag[j] <- sum(pet$sum_P[j],pet$sum_P[j-1:i])
#   }
# }

pet %>% ungroup() %>% group_by(id, E, N, year, month) %>%
  select(id, N, E, year, month, sum_P) %>% mutate(plag = dplyr::lag(sum_P, 3))

