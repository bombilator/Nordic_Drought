rm(list = ls())
library(tidyverse)
library(sp)
library(rgeos)
library(tidyquant)

# workflow

# read precip data ----
p.df <- read_rds("R_finland_drought/output/process/Pdf.rds") 
t.df <- readRDS("R_finland_drought/output/process/Tdf.rds")
hum.df <- readRDS("R_finland_drought/output/process/Humdf.rds")


# read groundwater data transformed to sgi
g.df <- read_rds("R_finland_drought/output/process/fin_gw_sgi.rds") %>%
    ungroup() %>%
  group_by(Stationname, Tunnus, N, E) %>%
  nest(.key = "monthly") %>%
    # group_by(Stationname, Tunnus) %>%
    # mutate(N = first(N),
           # E = first(E)) %>% 
    # group_by(Stationname, Tunnus, MONTH) %>%
    filter(!is.na(N) | !is.na(E)) #%>% dplyr::distinct(Tunnus, N, E)

# g2.df <- g.df %>% ungroup() %>% dplyr::distinct(Tunnus, N, E) %>% dplyr::filter(Tunnus  == "0802p1")

# find grid cells representative of groundwater stations (coordinate transform?)
# sp.g <- SpatialPoints(g.df[ , c(6, 7)], proj4string=CRS("+init=epsg:3067"))
sp.g <- SpatialPoints(g.df[ , c(3, 4)], proj4string=CRS("+init=epsg:3067"))
# sp.g <- SpatialPoints(g.dfnull[ , c(4, 5)])

# p2.df <- p.df %>% dplyr::filter(1<=id & id<=100)
sp.p <- SpatialPoints(p.df[ , c(2, 1)], proj4string=CRS("+init=epsg:3067"))
# sp.p <- SpatialPoints(p.dfsame[ , c(2, 1)])

# g.df$idP <- apply(gDistance(sp.p, sp.g, byid=TRUE), 1, which.min)
idP <- apply(gDistance(sp.p, sp.g, byid=TRUE), 1, which.min)
p2.df <- p.df %>% dplyr::filter(id %in% idP)
g.df$idP <- idP
t2.df <- t.df %>% dplyr::filter(id %in% idP)
hum2.df <- hum.df %>% dplyr::filter(id %in% idP)

# 
# 
# 
# # -------------------------------------------------------------------------------------------------------
# 
# 
# Checking that meteorological station is actually close to the groundwater well

t2.plot <- t2.df %>% dplyr::filter(id==1352) %>% dplyr::distinct(lon, lat, id)
coordinates(t2.plot) = ~lon+lat
proj4string(t2.plot) = CRS("+init=epsg:3067")
t2.plot = spTransform(t2.plot, CRS("+proj=longlat +datum=WGS84"))
t2.plot = as.data.frame(t2.plot)

g.plot <- g.df %>% dplyr::filter(idP==1352) %>% dplyr::distinct(N, E, Tunnus)
coordinates(g.plot) = ~E+N
proj4string(g.plot) = CRS("+init=epsg:3067")
g.plot = spTransform(g.plot, CRS("+proj=longlat +datum=WGS84"))
g.plot = as.data.frame(g.plot)

ggmap(stamen) +
  # ggmap(lines) +
  
  geom_point(aes(x = lon, y = lat), shape = 1,
             data = p2.plot, # gw data coordinates to plot
             colour = 'blue', size = 4) +
  geom_point(aes(x = lon, y = lat), shape = 1,
             data = t2.plot, # gw data coordinates to plot
             colour = 'red', size = 3) +
  
  geom_point(aes(x = E, y = N), shape = 1,
             data = g.plot, # gw data coordinates to plot
             colour = 'black', size = 2, alpha = .85) +
  
  theme_bw(base_size = 18)
# 
# 
# 
# # -------------------------------------------------------------------------------------------------------
# 
# 

p2.df <- p2.df %>%  unnest() %>% rename(RRday = value, N = lat, E = lon)
t2.df <- t2.df %>%  unnest() %>%  rename(N = lat, E = lon)
hum2.df <- hum2.df %>% unnest() %>% rename(N = lat, E = lon)
g.df <- g.df %>% rename(id = idP) %>% dplyr::select(-idP) 
finland.df <- left_join(p2.df, t2.df)
finland.df <- left_join(finland.df, hum2.df)
finland.df <- finland.df %>% group_by(E, N, id) #%>% nest(.key=daily_met)

saveRDS(g.df, "R_finland_drought/output/process/fin_gw_sgi.rds")
saveRDS(finland.df, "output/process/fin_met.rds")

p.sum <- finland.df %>%
    # dplyr::filter(id %in% gc) %>%
    mutate(
        # raw <- map(data, mutate(date2 = as.Date(date, format = "%Y-%m-%d")))
        # monthly sums
        mean_mon = map(daily_met, tq_transmute,
                       select     = RRday,
                       mutate_fun = apply.monthly,
                       FUN        = sum,
                       na.rm      = TRUE,
                       col_rename = "sum_P")
    )
saveRDS(p.sum, "R_finland_drought/output/process/fin_p.rds")
p.sum <- readRDS("R_finland_drought/output/process/fin_p.rds")


