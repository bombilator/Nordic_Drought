rm(list = ls())

sapply(list.files(pattern="[.]R$", path="rfuncs/", full.names=TRUE), source)
library(tidyverse)
library(readxl)
library(lubridate)
library(tidyquant)

# read groundwater data

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
