### Melt and Rain model, from Maria Staudinger et al.'s paper
# if T(t) < Tt: Freezing
#     Refreezing(t) = liquid water content(t-1)
#     if Refreezing(t) > Refreezing factor*Degree-day factor*(ThresholdT-Temp(t)):
#       Refreezing(t) = Refreezing factor*Degree-day factor*(ThresholdT-Temp(t))
# 
# else: Melting
#         Melt(t) = snow storage(t-1)
#         if Melt(t) > Degree-day factor*(Temp(t) - ThresholdT):
#           Melt(t) = Degree-day factor*(Temp(t) - ThresholdT)
# 
#         snow storage(t) = Snow storage(t-1) - Melt(t)
# 
#         liquid water content(t) = liquid water content(t-1) + P(t) + Melt(t)
# 
#         if liquid water content(t) > water holding capacity * snow storage(t):
#           Runoff(t) = liquid water content(t) - water holding capacity * snow storage(t)
#           liquid water content(t) = water holding capacity * snow storage(t)
#          

library(magrittr)
library(purrr)
library(tidyquant)

# Amandas matlab data ----
library(R.matlab)
solar <- readMat("Q:/Teaching/Theses/2018_Amandas_BSc/Codes/Codes/Solar.mat") %>% data.frame() %>%
  dplyr::mutate(
    Date = as.Date(strptime(paste(I.Year, I.Month, I.Day, sep = "-"), format = "%Y-%m-%d"))
  ) %>%
  dplyr::rename(Irradiance = P.I) %>%
  dplyr::select(Date, Irradiance)

library(zoo)
amanda_ws <- readMat("Q:/Teaching/Theses/2018_Amandas_BSc/Codes/Codes/Michelle_AmandasWS.mat") %>% data.frame()
Almdalen_met <- amanda_ws %>% dplyr::select(Year, Month, Day, A.Prec, A.Snow, A.Temp) %>% 
  dplyr::mutate(Date = as.Date(strptime(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d")),
                P = replace(A.Prec, is.na(A.Prec), 0), # replaces NA with 0
                obsSnow = A.Snow,
                Temp = na.approx(A.Temp)) %>% # zoo function; replaces NA with interpolated values
  dplyr::select(-Year, -Month, -Day, -A.Prec, -A.Snow, -A.Temp)
Hoglekardalen_met <- amanda_ws %>% dplyr::select(Year, Month, Day, H.Prec, H.Snow, H.Temp) %>% 
  dplyr::mutate(Date = as.Date(strptime(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d")),
                P = replace(H.Prec, is.na(H.Prec), 0),
                obsSnow = H.Snow,
                Temp = na.approx(H.Temp)) %>%
  dplyr::select(-Year, -Month, -Day, -H.Prec, -H.Snow, -H.Temp)

Lillharddal_met <- amanda_ws %>% dplyr::select(Year, Month, Day, L.Prec, L.Snow, L.Temp) %>% 
  dplyr::mutate(Date = as.Date(strptime(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d")),
                P = replace(L.Prec, is.na(L.Prec), 0),
                obsSnow = L.Snow,
                Temp = na.approx(L.Temp, na.rm = FALSE)) %>%
  dplyr::group_by(Month, Day) %>% 
  dplyr::mutate(meanTemp = mean(Temp, na.rm = TRUE),
                Temp = replace(Temp, is.na(Temp), meanTemp)) %>%
  ungroup() %>%
  dplyr::select(-Year, -Month, -Day, -L.Prec, -L.Snow, -L.Temp)

Beatriz_streams <- readMat("Q:/Data/Uppsala_drought/Data_Ezra/Data_Ezra.mat") %>% data.frame() # messy

# The Amandas have made separate vectors for every variable, change this to a raw data dataframe, 
# a TIBM dataframe and a PPBM dataframe

# 
# 
# ----------------------automating the process----------------------------------------------------
# 
# 
# 
finland.df <- readRDS("output/process/fin_met.rds")
source("rfuncs/tibm2.r")
# dtibm <-  finland.df %>% tail(10) %>% unnest() %>% group_by(id) %>% tibm2(df = ., P = .$RRday) 
# saveRDS(dtibm, "R_finland_drought/output/process/dtibm.r")

tibm <-  finland.df %>% unnest() %>% group_by(id) %>% tibm2(df = ., P = .$RRday) 
saveRDS(tibm, "output/process/tibm.r")

# dmet <- finland.df %>% tail(10) %>% unnest() %>% group_by(id)

sweden.df <- readRDS("output/process/swe_met.rds")
tibm <- sweden.df %>% group_by(id) %>% tibm2(df =., P = .$Precip, temp = .$Tm)
saveRDS(tibm, "output/process/sweden_tibm.rds")

# 3 cluster for sgi paper
south <- read.csv("input/sgi_met/14.csv") %>% mutate(Cluster="14") %>% rename(date=X)
mid <- read.csv("input/sgi_met/55.csv")%>% mutate(Cluster="55") %>% rename(date=X)
met <- full_join(south, mid)
north <- read.csv("input/sgi_met/37.csv") %>% mutate(Cluster="37") %>% rename(date=X)
met <- full_join(met, north)
met <- met %>% rename(precip = starts_with("Nederbö"),
                      temp = starts_with("Temper"))
rm(south, mid, north)
met <- met %>% mutate(N=ifelse(Cluster=="14", 57.39736, 59.40424), N = ifelse(Cluster=="37", 66.417632, N),
                      E = ifelse(Cluster==14, 13.61697, 18.19911), E = ifelse(Cluster=="37", 21.702111, E))
saveRDS(met, "input/sgi_met/met.rds")
met <- readRDS("input/sgi_met/met.rds")
tibm <- met %>% filter(as.Date(date) >= as.Date("1965-01-01")) %>%
  group_by(Cluster) %>% tibm2(df=., id = .$Cluster, date = .$date, P =.$precip, temp = .$temp)
tibm2 <- tibm %>% rename(Cluster=id) %>% mutate(Cluster=as.character(Cluster)) %>% 
  full_join(., met %>% select(Cluster, N, E)) # for some reason N and E are saved opposite in tibm
saveRDS(tibm, "output/process/3clusters_tibm.rds")


# 
# 
# 
# # The Amanda's code ---------------------------------------------
# 
# 
# 

# factors (same for all)
Scf = 0.7 # Snowfall correction factor
Tt = 0 # Threshold temperature
Cm = 2.74 # Degree-day factor
Cfr = 0.05 # Refreezing coefficient (default)
Cwh = 0.1 # Water holding capacity (default)
rf = 0.0006 # radiation factor for snow
mf = 1.8 # melt factor


# Temperature index-based model (TIBM) ----
# Data is year, month, day, mean temperature, sum precipitation

# variables
tibm <- data.frame(r = matrix(0, nrow=length(solar$Date))) #, ncol = 6))
tibm$ss[1] = 0 # Snow storage 0 mm day 1
tibm$liq[1] = 0 #liquid water in the snowpack 0 mm day 1 
tibm$prec[1] = 0
tibm$m[1] = 0 # melting (still in snowpack)
tibm$q[1] = 0 # snowmelt discharge (liquid water leaving the snowpack), output
# tibm <- data.frame(c(Q, SS))
tibm$id[1]=0

# for (t in 2:length(Almdalen_met$Date)){ 
for (t in seq_along(from = 2, Almdalen_met$Date)){ 
  if(Almdalen_met$Temp[t] < Tt){ # precipitation as snow
    tibm$r[t] = Cfr * Cm * (Tt - Almdalen_met$Temp[t])
    if(tibm$r[t] > tibm$liq[t-1]){
      tibm$r[t] = tibm$liq[t-1] 
    }
    
    tibm$ss[t] = Almdalen_met$P[t] * Scf + tibm$ss[t-1] + tibm$r[t]
    tibm$liq[t] = tibm$liq[t-1] - tibm$r[t] 
    tibm$prec[t] = Almdalen_met$P[t] * Scf
  }
  
  else{ # melting occurs
    tibm$r[t] = 0 # there is no refreezing if the snow melts
    tibm$m[t] = Cm*(Almdalen_met$Temp[t] - Tt)
    
    if(tibm$m[t] > tibm$ss[t-1]){
      tibm$m[t] = tibm$ss[t-1] 
    }
  
    tibm$ss[t] = tibm$ss[t-1] - tibm$m[t]
    tibm$liq[t] = tibm$liq[t-1] + Almdalen_met$P[t] + tibm$m[t]
    
    if(Cwh*tibm$ss[t] < tibm$liq[t]){
      tibm$q[t] = tibm$liq[t] - Cwh * tibm$ss[t]
      tibm$liq[t] = Cwh * tibm$ss[t]
    }
  
    tibm$prec[t] = Almdalen_met$P[t]
  }
  
}
tibm$snow_total = tibm$ss + tibm$liq

# Physical process-based model (PPBM) ----
# Data is year, month, day, mean temperature, sum precipitation, mean global irradiation, 
# what do we want? Mean direct radiation, mean diffuse radiation, sun hours? to account for snowmelt? 
# Should factors affecting evapotranspiration in summer be eventually included? such as radiation to estimate amount of 
# evporation and transpiration by plants?

# variables
ppbm <- data.frame(r = matrix(0, nrow=length(solar$Date)))
ppbm$q[1] = 0 # snowmelt discharge (liquid water leaving the snowpack), output
ppbm$ss[1] = 0 # Snow storage 0 mm day 1
ppbm$liq[1] = 0 #liquid water in the snowpack 0 mm day 1 
ppbm$r[1] = 0 # Freezing/refreezing 
ppbm$m[1] = 0 # melting (still in snowpack)
ppbm$prec[1] = 0 # simulated precipitation

for(t in 2:length(Almdalen_met$Date)){
  if (Almdalen_met$Temp[t] < Tt){ # precipitation as snow, liquid water in snow freezes
    ppbm$r[t] = Cfr * Cm * (Tt-Almdalen_met$Temp[t])
    
    if (ppbm$r[t] > ppbm$liq[t-1]){
      ppbm$r[t] = ppbm$liq[t-1]
      }
    
    ppbm$ss[t] = Almdalen_met$P[t] * Scf + ppbm$ss[t-1] + ppbm$r[t]
    ppbm$liq[t] = ppbm$liq[t-1] - ppbm$r[t]
    
    ppbm$prec[t] = Almdalen_met$P[t] * Scf
    
    if(ppbm$ss[t-1] > 0){
      albedo = 0.05
    }
    if(ppbm$m > 0){
      albedo = 0.25
    }
    
  }
  
  else {
    ppbm$m[t] = (mf * Almdalen_met$Temp[t] - Tt) + rf * solar$Irradiance[t] * albedo
    
    if(ppbm$m[t] > ppbm$ss[t-1]){
      ppbm$m[t] = ppbm$ss[t-1]
    }
    
    ppbm$ss[t] = ppbm$ss[t-1] - ppbm$m[t]
    ppbm$liq[t] = ppbm$liq[t-1] + Almdalen_met$P[t] + ppbm$m[t]
    
    if(Cwh*ppbm$ss[t] < ppbm$liq[t]){
      ppbm$q[t] = ppbm$liq[t] - Cwh * ppbm$ss[t]
      ppbm$liq[t] = Cwh * ppbm$ss[t]
    }
  
    ppbm$prec[t] = Almdalen_met$P[t]
  }
}
ppbm$snow_total = ppbm$ss + ppbm$liq
ppbm$Date = Almdalen_met$Date
ppbm$month = month(ppbm$Date)


# plotting the results against each other --------------------------------------------------------------------
ggplot(ppbm) + 
  geom_line(aes(Date, q), size=0.2, color = "blue") +
  geom_line(aes(Date, tibm$q), size=0.1, color = "red", alpha=0.5)
  # geom_line(aes(Date, ss/3), size=0.2, color = "red") #+
  # scale_y_continuous(sec.axis = sec_axis(~.*3, name = "Snow storage"))

qplot(ppbm$m, tibm$m, colour = ppbm$month) + 
  # scale_colour_gradient(low="red", high="blue") +
  scale_colour_gradient2(low="yellow", mid = "red", high="blue", midpoint = 6) +
  geom_abline(mapping = NULL, data = NULL, slope = 1, alpha = 0.3) + 
  ylab("TIBM melt") + xlab("PPBM melt")  + labs(colour="Month") +
  xlim(0, 30) # This graph shows more red and more blue where TIBM
# have higher values in relation to PPBM, which indicates PPBM gives higher values outside the winter months,
# i.e. as we already theorised, the PPBM gives lower runoff/melt values when irradiation is lower

