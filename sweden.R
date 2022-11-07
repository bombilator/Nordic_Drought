library(RPostgreSQL)
library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(gwdyn)
library(TSclust)
library(dendextend)
library(DBI)
library(data.table)
library(sf)
library(stars)
library(tiff)
library(raster)
library(sp)
library(rgdal)
rm(list=ls())
sapply(list.files("Q:/Projects/Drought Scandinavia/Michelle/Nordic_Drought/sweden_class/gwdyn/R/", 
                  pattern="*.R", full.names=TRUE), source)

setwd("Q:/Projects/Drought Scandinavia/Michelle/Nordic_Drought/sweden_class/")
# import data
ts <- readRDS("input data/sgu_21_11.RDS")$meas %>%
  rename(pid = Omr_stn, location=Omr, well_nr=Stn, mdate=Datum, level_masl=m_o_h,
         m = UnderMark) %>% 
  dplyr::select(-UnderOkRor, - YEAR,-MONTH,-YEARWEEK,
                -MONTHDAY,-YEARDAY,-HYD_YEAR)

write_rds(ts, 'input data/ts.rds')
# analyse all with NA
ff <- ts %>%
  group_by(pid) %>% 
  nest() %>%
  ungroup %>% 
  dplyr::filter(row_number() %in% 1:10000) %>%
  mutate(
    purrr::map_df(data, function (x) summarise(x, start = min(mdate))),
    purrr::map_df(data, function (x) summarise(x, end = max(mdate))),
    purrr::map_df(data, function (x) summarise(x, n_meas = length(mdate))),
    ndays = as.numeric(end-start, unit = "days"),
    meas_freq_d = round(n_meas/ndays, digits = 4),
    # len = as.numeric(len), units = "days")
  )

#Daily_data - NO DAILY DATA
d_full <- ff %>%
  dplyr::filter(meas_freq_d == 1) %>%
  unnest(cols = c(data)) %>%
  mutate(
    YEAR = year(mdate), 
    MONTH = month(mdate), 
    WEEK = week(mdate),
    DAY = mday(mdate),
  )%>%
  group_by(pid, YEAR, MONTH)

d_tmp <- d_full %>%
  dplyr::filter(row_number() == 15)
  
d_len <- d_tmp %>%
  ungroup(YEAR) %>%
  mutate(
    YEARDAY = yday(mdate)
  ) %>%
  group_by(pid, YEAR) %>% 
  arrange(pid, YEAR) %>% 
  mutate(
    data_length = length(unique(MONTH))
  )

#Daily_gaps
d_gaps <- ff %>%
  dplyr::filter(between (meas_freq_d, 0.3000, 0.9999)) %>%
  unnest(cols = c(data)) %>%
  mutate(
    YEAR = year(mdate), 
    MONTH = month(mdate), 
    WEEK = week(mdate),
    DAY = mday(mdate),
  )%>%
  group_by(pid, YEAR, MONTH)

d_gaps_tmp <- d_gaps %>%
  dplyr::filter(row_number() == 15)

d_gap_len <- d_gaps_tmp %>%
  ungroup(YEAR) %>%
  mutate(
    YEARDAY = yday(mdate)
  ) %>%
  group_by(pid, YEAR) %>% 
  arrange(pid, YEAR) %>% 
  mutate(
    data_length = length(unique(MONTH))
  )

#Daily_piece
d_piece <- ff %>%
  dplyr::filter(between (meas_freq_d, 0.1431, 0.2999)) %>%
  unnest(cols = c(data)) %>%
  mutate(
    YEAR = year(mdate), 
    MONTH = month(mdate), 
    WEEK = week(mdate),
    DAY = mday(mdate),
  )%>%
  group_by(pid, YEAR, MONTH)

d_piece_tmp <- d_piece %>%
  dplyr::filter(row_number() == 5)

d_piece_len <- d_piece_tmp %>%
  ungroup(YEAR) %>%
  mutate(
    YEARDAY = yday(mdate)
  ) %>%
  group_by(pid, YEAR) %>% 
  arrange(pid, YEAR) %>% 
  mutate(
    data_length = length(unique(MONTH))
  )

#weekly 
w_gaps <- ff %>%
  dplyr::filter(between (meas_freq_d, 0.0340, 0.1430)) %>%
  unnest(cols = c(data)) %>%
  mutate(
    YEAR = year(mdate), 
    MONTH = month(mdate), 
    WEEK = week(mdate),
  )%>%
  group_by(pid, YEAR, MONTH)

w_tmp <- w_gaps %>%
  dplyr::filter(row_number() == 3)

w_len <- w_tmp %>%
  ungroup(YEAR) %>%
  mutate(
    YEARDAY = yday(mdate)
  ) %>%
  group_by(pid, YEAR) %>% 
  arrange(pid, YEAR) %>% 
  mutate(
    data_length = length(unique(MONTH))
  )

#w_m <- ff %>%
#  dplyr::filter(between (meas_freq_d, 0.0330, 0.1379))

m_gaps <- ff %>%
  dplyr::filter(between (meas_freq_d, 0.0200, 0.0339)) %>%
  unnest(cols = c(data)) %>%
  mutate(
    YEAR = year(mdate), 
    MONTH = month(mdate), 
    WEEK = week(mdate),
  )%>%
  group_by(pid, YEAR, MONTH)

m_tmp <- m_gaps %>%
  dplyr::filter(row_number() == 1)

m_len <- m_tmp %>%
  ungroup(YEAR) %>%
  mutate(
    YEARDAY = yday(mdate)
  ) %>%
  group_by(pid, YEAR) %>% 
  arrange(pid, YEAR) %>% 
  mutate(
    data_length = length(unique(MONTH))
  )

rest <- ff %>%
  dplyr::filter (meas_freq_d < 0.0199 | meas_freq_d > 1)  
 
all_data <- bind_rows(#d_tmp, d_gaps_tmp, d_piece_tmp, - NO DAILY DATA
                      w_tmp, m_tmp)
all_data_len <- bind_rows(#d_len, d_gap_len, d_piece_len, - NO DAILY DATA
                          w_len, m_len)
write_rds(all_data, "processed/all_data_swe.rds")

rm(d_gaps, d_full, d_gap_len, d_gaps_tmp, d_len, d_piece, d_piece_len, d_piece_tmp, d_tmp, m_gaps, m_len, m_tmp, rest, w_full, w_m,  w_gaps, w_len, w_tmp, d_w, ts)

# filter years with > 9 measurements/year
viable_meas <- all_data_len %>% 
  mutate(maxmeas = max(data_length)) %>% 
  dplyr::filter(maxmeas >= 9) # minimum 9 measurements/year

#Time series with more than 10 continous years
viable_cont_years <- viable_meas %>% 
  dplyr::filter(row_number() == n()) %>% 
  group_by(pid) %>% 
  mutate(
    cont = YEAR - lag(YEAR),
    cont = ifelse(is.na(cont), 1, cont) #I had to change if_else to ifelse in order to run the code- Why?
  ) %>% 
  arrange(mdate, .by_group = TRUE) %>% 
  group_by(run = data.table::rleid(pid, cont), pid) %>% 
  mutate(
    run_count = 1:n(),
    maxruncount = max(run_count, na.rm = T) 
  ) %>%
  group_by(pid) %>% 
  dplyr::filter(maxruncount == max(maxruncount)) %>% 
  dplyr::filter(maxruncount >= 8) #series with more than 8 years of continous data

#join basedata with number of continous data
data_sel_join <- right_join(all_data, viable_cont_years, by = c("pid", "YEAR"))

#complete monthly series where values are missing
data_sel_mcomp <- data_sel_join %>% 
  dplyr::select(pid, mdate.x) %>% 
  group_by(pid) %>% 
  complete(mdate.x = seq(ymd(min(mdate.x)), ymd(max(mdate.x)), by= "month")) %>% 
  mutate(
    YEAR =year(mdate.x),
    MONTH = month(mdate.x)
  ) %>% 
  group_by(pid, YEAR, MONTH) %>% 
  dplyr::filter(row_number() == 1)

#join and subset data
names(data_sel_join)[names(data_sel_join) == 'MONTH.x'] <- 'MONTH'

data_sel_mfin <- right_join(data_sel_join, data_sel_mcomp, by = c("pid", "YEAR", "MONTH"))

data_sel_mfin = subset(data_sel_mfin, select = c("pid","mdate.x.y", "m.x"))

#fill with new interpolated data, calculate normalized values and z-score. 
data_sel <- data_sel_mfin %>%
  group_by(pid) %>%
  mutate(VALUE_INTP = na.approx(m.x, na.rm =FALSE)) %>%
  fill(VALUE_INTP, .direction = "updown") %>%
  dplyr::select(1,2,3,4) %>%
  group_by(pid) %>%
  mutate(
    VALUE_norm = (VALUE_INTP - min(VALUE_INTP, na.rm = T)) /
      (max(VALUE_INTP, na.rm = T) - min(VALUE_INTP, na.rm = T)),
    VALUE_open = VALUE_INTP - min(VALUE_INTP, na.rm = T),
    VALUE_meas =( VALUE_INTP - mean(VALUE_INTP, na.rm=T)) / sd(VALUE_INTP, na.rm=T)
  ) %>%
  dplyr::select(pid, mdate.x.y , everything()) %>%
  mutate_at(
    ., vars(VALUE_norm:VALUE_open), ~round(., 2)
  )

write_rds(data_sel, "processed/data_sel_swe.rds")

# Figures
# dsn = "PG:dbname='swe_1' host='localhost' port='5432' user='postgres' password='abc'"
# p1_seq <- seq(500, 83e3, by= 500)
# p1 <- lapply(p1_seq, function (x) st_read(dsn, "p", query = paste0("SELECT id , geom FROM p WHERE id <", x," AND id >", x-500)))
# p2 <- p1 %>% bind_rows()
# 
# data_sel_loc <- data_sel_join %>%
#   group_by(pid) %>%
#   mutate(
#     id = pid
#   ) %>%
#   group_by(id) %>%
#   nest() %>%
#   dplyr::inner_join(p2, data_sel_loc, by = "id") %>%
#   dplyr::relocate(id, geom, data)
# data_sel_loc2 <- st_as_sf(data_sel_loc)
# 
# poly <- st_read(dsn, "ro_export_polygons_115")
# 
# grid <- st_read(dsn, "cntr_rg_03m_2014_3035")
# grid %>% dplyr::filter(cntr_id %in% c("RO", "HU", "AT", "DE", "HR", "SK", "BG", "SI", "RS")) %>%
#   ggplot(data=.)+geom_sf()+geom_sf(data=data_sel_loc2)

#join with all_data

#skip this step and use time_series <- data_sel
write.csv(data_sel, "complete_data.csv", row.names = FALSE)
# write_rds(ts, "processed/data_sel.rds")
# write_rds(ts, "processed/data_sel_loc2.rds")

# time_series <- dbGetQuery(con, "SELECT * FROM danube1.wts_danube")

# run indices:
ind_wkl <- data_sel %>%
  dplyr::filter(!is.na(VALUE_norm)) %>%
  group_by(pid) %>%
  dplyr::summarise(
    # originally open scale - now also standardized
    iaf.s = mean(intannfluc(mdate.x.y, VALUE_norm, out = "s")),
    iaf.y = intannfluc(mdate.x.y, VALUE_norm, out = "y"),
    # BLS = baseflow(mdate.x.y,VALUE_norm, out = "BFS", block.len = 3),
    # BLI = baseflow(mdate.x.y,VALUE_norm, out = "BFI", block.len = 3),
    l1 = lmom(mdate.x.y, VALUE_norm, out = "l1"),
    l2 = lmom(mdate.x.y, VALUE_norm, out = "l2"),
    l3 = lmom(mdate.x.y, VALUE_norm, out = "l3"),
    l4 = lmom(mdate.x.y, VALUE_norm, out = "l4"),
    # standardized
    bimod = bimodality(VALUE_norm),
    #Colwell- Standard value: s = 11(why?) -> changed to s = 12 
    #Colwell- Standard value: aggregation = weeks -> changed to month
    colwell.C = colwells(mdate.x.y, VALUE_norm, out = "constancy"),
    colwell.M = colwells(mdate.x.y, VALUE_norm, out = "contingency"),
    colwell.P = colwells(mdate.x.y, VALUE_norm, out = "predictability"),
    cvmon.min = cvmonminmax(mdate.x.y, VALUE_norm, out = "min"),
    cvmon.max = cvmonminmax(mdate.x.y, VALUE_norm, out = "max"),
    #dip = list(diptest::dip.test(VALUE_norm)),
    dc.rng.01.09 = fdcindices(mdate.x.y, VALUE_norm, out = "range", y1 = 0.1, y2 = 0.9),
    dc.rng.02.08 = fdcindices(mdate.x.y, VALUE_norm, out = "range", y1 = 0.2, y2 = 0.8),
    dc.rng.025.075 = fdcindices(mdate.x.y, VALUE_norm, out = "range", y1 = 0.25, y2 = 0.75),
    dc.slp.01.09 = fdcindices(mdate.x.y, VALUE_norm, out = "slope", y1 = 0.1, y2 = 0.9),
    dc.slp.02.08 = fdcindices(mdate.x.y, VALUE_norm, out = "slope", y1 = 0.2, y2 = 0.8),
    dc.slp.025.075 = fdcindices(mdate.x.y, VALUE_norm, out = "slope", y1 = 0.25, y2 = 0.75),
    med = median(VALUE_norm, na.rm = TRUE),
    parde = parde(mdate.x.y, VALUE_norm, out = "metric"),
    #pulse.count.l = pulses(mdate.x.y, VALUE_norm, out = "count", threshold.type = "low"),
    #pulse.count.h = pulses(mdate.x.y, VALUE_norm, out = "count", threshold.type = "high"),
    #pulse.dur.l = pulses(mdate.x.y, VALUE_norm, out = "duration", threshold.type = "low"),
    #pulse.dur.h = pulses(mdate.x.y, VALUE_norm, out = "duration", threshold.type = "high"),
    #pulse.cv.l = pulses(mdate.x.y, VALUE_norm, out = "CV", threshold.type = "low"),
    #pulse.cv.h = pulses(mdate.x.y, VALUE_norm, out = "CV", threshold.type = "high"),
    #bandwd = silverman(VALUE_norm),
    #vardoy.min = varjdminmax(mdate.x.y, VALUE_norm, out = "min"),
    #vardoy.max = varjdminmax(mdate.x.y, VALUE_norm, out = "max")
  )

write_rds(ind_wkl, "processed/indices.rds")
---
# DESCRIPTORS

#med gw-level (needed for the calculation of depth to gw)
gw_med <- data.frame(
  pid = data_sel$pid,
  VALUE_INTP = data_sel$VALUE_INTP
) %>%
  group_by(pid) %>%
  mutate(
    med = median(VALUE_INTP, na.rm = TRUE)
  ) %>%
  dplyr::relocate(pid, med, VALUE_INTP) %>%
  group_by(pid, med) %>%
  nest()

#land use/elevation
loc <- data_sel_loc2 %>% 
  dplyr::select(id, geom)
loc_buffer_s <- st_buffer(loc, 300) # change the number in order to in/decrease the radius around the well
loc_buffer_l <- st_buffer(loc, 3000) # change the number in order to in/decrease the radius around the well

corine_name <- "C:/06EU_Danube/R_EU_Danube/corine_danube.tif"
DEM_name <- "C:/06EU_Danube/R_EU_Danube/DEM_danube1.tif"
riv <- st_read("C:/06EU_Danube/R_EU_Danube/C_Danube_riv.shp")
corine <- raster(corine_name, crs ="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
DEM <- raster(DEM_name, crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#descriptors <- data.frame(
#  cor = raster::extract(corine, data_sel_loc2)) %>%
#  mutate(
#    pid = data_sel_loc2$id, cor,
#    ele = raster::extract(DEM, data_sel_loc2),
#  ) %>%
#  dplyr::relocate(pid, cor, ele)
#descriptors <- dplyr::inner_join(descriptors, gw_med, by = "pid") %>%
#  mutate(
#    dpt = round(ele - med, digits = 2)
#  )

----
LUs <- raster::extract(corine, loc_buffer_s)
names(LUs) <- as.integer(loc$id)
LUs.counts <- rapply(LUs, f=function(x) ifelse(is.na(x),-99,x), how="replace")
LUs.counts <- lapply(LUs.counts,table)
LUs.pct <- lapply(LUs.counts, FUN=function(x){ x / sum(x) })


LUl <- raster::extract(corine, loc_buffer_l)
names(LUl) <- as.integer(loc$id)
LUl.counts <- rapply(LUl, f=function(x) ifelse(is.na(x),-99,x), how="replace")
LUl.counts <- lapply(LUl.counts,table)
LUl.pct <- lapply(LUl.counts, FUN=function(x){ x / sum(x) })

LUs.ov <- LUs.pct %>% 
  lapply(., as.data.frame) %>%
  bind_rows(., .id="pid") %>%
  as_tibble() %>%
  dplyr::select(pid, Class="Var1", Frac=Freq) %>%
  complete(pid = names(LUs)) %>%
  mutate(pid = as.integer(pid)) %>%
  arrange(pid) %>%
  group_by(pid) %>%
  nest(LUs = c(Class, Frac))
#  pivot_wider(names_from = Class, values_from = Frac) overview!

LUl.ov <- LUl.pct %>% 
  lapply(., as.data.frame) %>%
  bind_rows(., .id="pid") %>%
  as_tibble() %>%
  dplyr::select(pid, Class="Var1", Frac=Freq) %>%
  complete(pid = names(LUl)) %>%
  mutate(pid = as.integer(pid)) %>%
  arrange(pid) %>%
  group_by(pid) %>%
  nest(LUl = c(Class, Frac))
#  pivot_wider(names_from = Class, values_from = Frac) overview!
  
descriptors <- data.frame(
    pid = loc$id,
    ele = raster::extract(DEM, loc)
    )
descriptors <- dplyr::inner_join(descriptors, gw_med, by = "pid") %>%
  mutate(
    dpt = round(ele - med, digits = 2)
  )
descriptors <- dplyr::inner_join(descriptors, LUs.ov, by = "pid")
descriptors <- dplyr::inner_join(descriptors, LUl.ov, by = "pid")

write_rds(descriptors, "C:/06EU_Danube/R_EU_Danube/processed/descriptors.rds")

----
#Distance to river
wd <- 1e3

data_sel_loc2_sf <- data_sel_loc2 %>%
  dplyr::select(id, geom) %>%
  st_set_crs(sf::st_crs(riv)) %>% 
  group_by(id) %>% 
  nest(data = c(geom))

coord <- data.frame(
  id = data_sel_loc2$id,
  st_coordinates(data_sel_loc2)
  ) 

   #calculate distances:

riv_bb <- coord %>%
  dplyr::select(id, X, Y) %>% 
  bind_cols(., riv %>% nest(data = everything())) %>% 
  left_join(., data_sel_loc2_sf, by ="id") %>% 
  dplyr::rename(rivers = `data.x`, wells = `data.y`) %>% 
  mutate(
    xmin = X - wd, 
    xmax = X + wd, 
    ymin = Y - wd, 
    ymax = Y + wd
  ) %>% 
  group_by(id) %>% 
  mutate(
    bbox = purrr::pmap(list(xmin, ymin, xmax, ymax), function(...) {
      c(xmin = xmin, ymin =ymin, xmax=xmax, ymax=ymax) %>% 
        st_bbox()
    } )
  ) %>% 
  mutate(
    # crop and simplify
    cropped = purrr::map2(rivers, bbox, st_crop),
      purrr::map(st_simplify, dTolerance = 50),
      purrr::map(st_cast, "MULTILINESTRING"),
    dist = purrr::map2(cropped, wells, st_distance, by_element = TRUE) 
  )

write_rds(riv_bb, "C:/06EU_Danube/R_EU_Danube/processed/riv_bb_danube.rds")  
    




    
df %>% group_by(ID, YEAR) %>%
  dplyr::filter(row_number() == 1) %>%
  count(YEAR) %>%
  ggplot(data=data_sel_join$pid)



# indices according to Wang et al 2006 ---------------------------------

# indices according to Wang et al 2006 ---------------------------------
# read data ---- loop, if errors in dataserie

# id_vector <- ind_wkl[['id2']]
# 
# 
# for (x in id_vector){
#   test = paste("SELECT * FROM danube1.wts_danube WHERE id2 =", x)
#   data_sel2 <- dbGetQuery(con, test)
# 
#   if(nrow(time_series) != 0){
# 
# 
#   print(x)
# 
#     res <- try(


ind_wkl_wang <- time_series %>% 
  group_by(pid) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(
    start = map(data %>% map("date_x_y"), .f = start_YEARMON),
    tsobj = map2(data %>% map("value_norm"), start, .f = as_wkly_ts),
    wang = map(tsobj, .f = tschar)
  ) %>% 
  tidyr::unnest(wang) %>% 
  rename(
    hurst = Hurst,
    lyapunov = Lyapunov,
    autocor = autocorrelation,
    freq = frequency,
    seas = seasonal,
    skew = skewness,
    kurt = kurtosis,
    autocor.dc = dc_autocorrelation,
    nonlinear.dc = dc_nonlinear,
    skew.dc = dc_skewness,
    kurt.dc = dc_kurtosis
  )
#   )
#   if(inherits(res, "try-error"))
#   {
#     print(x)
#     print("error")
#     next
#   }
# }
# 
# }

all_indicies <- merge(ind_wkl, ind_wkl_wang, by = c("id2"))

#optimize this when time, currently removing indices not working or unnecessery columns
all_indicies = subset(all_indicies, select = -c(dip, start))
all_indicies = all_indicies %>% BBmisc::dropNamed("tsobj")
all_indicies = all_indicies %>% BBmisc::dropNamed("data")
all_indicies = all_indicies %>% BBmisc::dropNamed("freq")
all_indicies = all_indicies %>% BBmisc::dropNamed("seas")
all_indicies = all_indicies %>% BBmisc::dropNamed("freq")

#scale and as matrix
z_indicies <- scale(all_indicies[2:39])

# z_test <- scale(all_indicies["iaf.s"])

z_frame = as.data.frame(z_indicies)

z_ind <- cbind(all_indicies$id2, z_frame)

names(z_ind)[names(z_ind) == 'all_indicies$id2'] <- 'id2'

#preparing clustering

x <- z_ind

time_series_sub <- subset(time_series, select =c("id2","date_x_y", "value_meas"))

ts <- time_series_sub %>% 
  mutate(id2 = as.character(id2))

dfind_to_mat = function (x) {
  x %>% 
    column_to_rownames(var= "id2") %>% 
    as.matrix
}


# index-based clustering ------------------------------------------------------

# selct indices to cluster
ind_sel <- c(
  "IAF.s",
  "vardoy.min",
  "rev.avg", "BLI",
  "autocor", "hurst",
  "recov.const", "reces.const",
  "avg.ann.max",
  "peakts.avg", 
  "bimod",
  "nonlinear")

ind_sel2 <- c(
  "IAF.y",
  "rise.cv",
  "rev.avg",
  "autocor", "hurst",
  "recov.const",
  "peakbase.cv",
  "pulse.dur.h", 
  "pulse.count.h",
  "nonlinear")

ind_sel3 <- c(
  "hurst", 
  "autocor",
  "pulse.count.l",
  "pulse.count.h",
  "iaf.y"
)

ind_series <- z_ind %>% 
  dplyr::select(id2, one_of(ind_sel3)) %>% 
  nest(data = everything()) %>% 
  mutate(
    mat       = map(data, dfind_to_mat),
    man_diss  = map(mat, dist, method = "manhattan"),
    euc_diss  = map(mat, dist, method = "euclidean"),
    man_clust = map(man_diss, hclust, method = "ward.D2") %>% 
      map(as.dendrogram),
    euc_clust = map(euc_diss, hclust, method = "ward.D2") %>% 
      map(as.dendrogram),
    man_cut  = map(man_clust, cutree, k=6),
    euc_cut  = map(euc_clust, cutree, k=6),
  )

dend = as.dendrogram(ind_series$euc_clust[[1]])



dend <- color_branches(dend, k = 6)

plot(dend)

# plotting and diagnosis -------------------------
# plot ts and clusters ------

ts_cluster <- ts %>% 
  ungroup %>% 
  left_join(., tibble(
    id2 = names(ind_series$man_cut[[1]]), 
    c_ind_man = as.character(ind_series$man_cut[[1]])), by = "id2"
  ) %>% 
  left_join(., tibble(
    id2 = names(ind_series$euc_cut[[1]]), 
    c_ind_euc = as.character(ind_series$euc_cut[[1]])), by = "id2"
  )

write_csv(ts_cluster, "clusters/clusters.csv")


#theme
ez_theme_pl <- theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "grey95"),
    strip.background = element_rect(fill=NA, colour = NA),
    axis.text = element_text(colour = "grey5"),
    axis.ticks.length=unit(-0.2, "cm"), 
    axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
    axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
    legend.position = "none"
    # axis.title.y = element_blank(),
    # plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

# ind man ---------
ts_cluster %>% 
  ggplot() + 
  aes(DATE, VALUE_C, colour = c_ind_man) + 
  labs(title = "index-based / manhattan distance") +
  geom_line(aes(group = ID), size = .2) + 
  stat_summary(fun.y = mean,
               geom = "line", colour = "black") +
  facet_wrap(~c_ind_man, nrow = 3) +
  ez_theme_pl

# ind eucl ---------
ts_cluster_plot %>% 
  filter(date_x_y > as.Date("1980-01-01"))%>%
  filter(date_x_y < as.Date("1995-01-01"))%>%
  ggplot() + 
  aes(date_x_y, value_meas, colour = c_ind_euc) + 
  labs(y="Standardized groundwater level", x="Date") +
  geom_line(aes(group = id2), size = .2) + 
  ylim(c(-6,6))+
  # stat_summary(fun = mean,
  #              geom = "line", colour = "black", size = 0.1) +
  facet_wrap(~c_ind_euc, nrow = 3) +
  ez_theme_pl