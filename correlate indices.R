# correlate indices to sgi

rm(list=ls())

library(stringr)
# load data, get distance between points ----
g.df <- readRDS("output/process/sgi_durs.rds") %>% #readRDS("output/3.sgi.rds") %>% 
  ungroup() %>% select(-id) %>%
  dplyr::rename(id=Cluster) %>% 
  group_by(id, Station) %>% mutate(date = as.Date(paste(format(date, "%Y-%m"), "-15", sep=""))) %>% 
  as.data.frame() %>% 
  mutate(sgi=ifelse(is.na(sgi), lag(sgi,1), sgi))

spi.ind <- readRDS("output/process/clusters_spi191128.rds") %>% select(-month_past) %>% ungroup() %>% 
  as.data.frame() %>% group_by(id, agg) %>% select(-N,-E)

smri.ind <- readRDS("output/process/clusters_smri191129.rds")%>% select(-month_past)%>% ungroup()%>%
 as.data.frame() %>% group_by(id, agg)%>% select(-N,-E)
spei.ind <- readRDS("output/process/clusters_spei191128.rds")%>% ungroup()%>% 
  as.data.frame() %>% group_by(id, agg)  %>% select(-N, -E)
smrei.ind <- readRDS("output/process/cluster_smrei191129.rds")%>% ungroup()%>%
  as.data.frame() %>% group_by(id, agg)%>% select(-N,-E)

# proximity ----
library(sp)
sp.g <- g.df %>% distinct(id, Station, N, E) %>% rename(N=E, E=N)
coordinates(sp.g) = ~N+E
proj4string(sp.g) = CRS("+proj=longlat + datum=WGS84")
sp.g <- spTransform(sp.g, CRS("+init=epsg:3006"))
sp.g <-  SpatialPoints(sp.g, proj4string = CRS("+init=epsg:3006"))

sp.p <- spi.ind %>% ungroup() %>% distinct(id, N, E)
coordinates(sp.p) = ~E+N
# proj4string(sp.p) = CRS("+proj=longlat + datum=WGS84")
proj4string(sp.p) = CRS("+init=epsg:3021")
sp.p <- spTransform(sp.p, CRS("+init=epsg:3006"))
sp.p <- SpatialPoints(sp.p, proj4string = CRS("+init=epsg:3006"))
library(rgeos)
id <- apply(gDistance(sp.p, sp.g, byid=TRUE), 1, which.min)
coordinates(spi.ind) = ~E+N
proj4string(spi.ind) =CRS("+init=epsg:3021")
spi.ind <- spTransform(spi.ind, CRS("+proj=longlat + datum=WGS84")) %>% as.data.frame()
ggmap(stamen) + 
  geom_point(data=g.df %>% distinct(id, Station, N, E), aes(N,E), colour="black",
             alpha=.5)+
  geom_point(data=spi.ind %>% ungroup() %>% distinct(id, N, E), aes(E,N), colour="red",
             alpha=.5)

# Step 1: get sgi and spi data in one dataframe ----

spi.sgi <- g.df %>% rename(Cluster=id) %>% mutate(id=paste(Cluster, Station, sep="_")) %>% 
  group_by(Cluster, Station, N, E) %>% nest() %>% 
  mutate(stand= map(data, merge, spi.ind %>% select(id, agg, index, date), by=c("date", "id")))
saveRDS(spi.sgi, "output/process/clusters_spi_sgi1911.rds")

smri.sgi <- g.df %>% rename(Cluster=id) %>% mutate(id=paste(Cluster, Station, sep="_")) %>% 
  group_by(Cluster, Station, N, E)  %>% nest() %>%
  mutate(stand= map(data, merge, smri.ind %>% select(id, agg, index, date), by=c("date", "id")))
saveRDS(smri.sgi, "output/process/clusters_smri_sgi1911.rds")

spei.sgi <- g.df %>% rename(Cluster=id) %>% mutate(id=paste(Cluster, Station, sep="_")) %>% 
  group_by(Cluster, Station, N, E) %>% nest() %>% 
  mutate(stand= map(data, merge, spei.ind%>% select(id, agg,index,  date),  by=c("date", "id")))
saveRDS(spei.sgi, "output/process/clusters_spei_sgi1911.rds")

smrei.sgi <- g.df %>% rename(Cluster=id) %>% mutate(id=paste(Cluster, Station, sep="_")) %>% 
  group_by(Cluster, Station, N, E) %>% nest() %>%
  mutate(stand= map(data, merge, smrei.ind%>% select(id, agg, index, date), by=c("date", "id")))
saveRDS(smrei.sgi, "output/process/clusters_smrei_sgi1911.rds")


# Step 2: correlate the values fun----

spearcorr <- function(data, diffid1, diffid2, y1_start, y1_end, y2_start, y2_end, 
                      y3_start, y3_end, dofilter){
  if(dofilter==2){
    print("doing 2")
    new <- data %>% select(-data) %>% unnest() %>% 
      filter(case_when(Cluster==diffid1 ~date >= y2_start & date <= y2_end,
                       Cluster != diffid1 ~date >= y1_start & date <= y1_end)) %>%
      select(-mean_mon, -year, -month) %>% group_by(id, N, E) %>% arrange(agg, date) %>% 
      filter(!is.na(index)) %>% nest(.key="stand") %>% 
      mutate(corr = purrr::map(stand, group_by, agg) %>% 
               purrr::map(summarize, corr = cor(sgi, index, use = "everything", method="spearman")))
  } else if(dofilter==3) {
    print("doing 3")
    new <- data %>% select(-data) %>% unnest() %>% 
      filter(case_when(Cluster==diffid1 ~date >= y2_start & date <= y2_end,
                       Cluster==diffid2 ~date >= y3_start & date <= y3_end,
                       Cluster != diffid1 & Cluster != diffid2 ~date >= y1_start & date <= y1_end)) %>%
      select(-mean_mon, -year, -month) %>% group_by(id, N, E) %>% arrange(agg, date) %>% 
      filter(!is.na(index)) %>% nest(.key="stand") %>% 
      mutate(corr = purrr::map(stand, group_by, agg) %>% 
               purrr::map(summarize, corr = cor(sgi, index, use = "everything", method="spearman")))
  } else{
    print("doing same for all")
    new <- data %>% select(-data) %>% unnest() %>% 
      select(-mean_mon, -year, -month) %>% group_by(id, N, E) %>% arrange(agg, date) %>% 
      filter(!is.na(index)) %>% nest(.key="stand") %>% 
      mutate(corr = purrr::map(stand, group_by, agg) %>% 
               purrr::map(summarize, corr = cor(sgi, index, use = "everything", method="spearman")))
  }
  new <- new %>% select(-stand) %>% unnest() %>% arrange(id) %>% ungroup()
  return(new)
  # df %>% select(-data) %>% unnest() %>% 
  # filter(case_when(Cluster==34 ~ year >= 2003 & year <= 2006,    # filter for different times as the 
  #                 Cluster!=34 ~ year>= 2002 & year <= 2004)) %>% # different clusters have droughts for different 
  #                                                                # periods
  # select(-mean_mon, -year, -month) %>% group_by(id, N, E) %>% arrange(agg, date) %>% 
  # filter(!is.na(index)) %>% # if I use the same period for standardisation for met as for groundwater, 
  # # NAs will be produced in the first years of the met index value calculations for all aggregation periods
  # nest(.key = "stand") %>%
  # mutate(
  #   cor_sgi_spi = 
  #     # purrr::map(stand, gather, spi_agg, spi, -date, -sgi) %>% 
  #     purrr::map(stand, group_by, agg) %>% 
  #     purrr::map(summarize, cor_sgi_spi = cor(sgi, index, use = "everything",
  #                                             method="spearman"))
  # )
}

# correlate values for longer periods ----
# why only Kendall correlations work-ish: https://stackoverflow.com/questions/27047598/r-cor-method-pearson-returns-na-but-method-spearman-returns-value-why
p1=as.Date("1995-08-15")
p2=as.Date("1996-06-15")
p3=as.Date("1995-11-15")
p4=as.Date("1998-08-15")
# p1=as.Date("2002-08-15")
# p2=as.Date("2003-06-15")
# p3=as.Date("2003-01-15")
# p4=as.Date("2007-01-15")
p5=as.Date("2002-05-15")
p6=as.Date("2004-06-15")
yano = 2
spi_sgi_corr = readRDS("output/process/clusters_spi_sgi1911.rds") %>% 
  spearcorr(data=., dofilter=yano,
            diffid1=34, y1_start = p1, y1_end = p2, y2_start = p3, y2_end = p4,
            diffid2 = 37, y3_start = p5, y3_end = p6)
# saveRDS(spi_sgi_corr, "output/process/clusters_spi_sgi_corr_94.rds")
smri_sgi_corr = readRDS("output/process/clusters_smri_sgi1911.rds") %>% 
  spearcorr(data=., dofilter=yano,
            diffid1=34, y1_start = p1, y1_end = p2, y2_start = p3, y2_end = p4,
            diffid2 = 37, y3_start = p5, y3_end = p6)
# saveRDS(smri_sgi_corr, "output/process/clusters_smri_sgi_corr_allrec.rds")
spei_sgi_corr = readRDS("output/process/clusters_spei_sgi1911.rds") %>%
  spearcorr(data=., dofilter=yano,
            diffid1=34, y1_start = p1, y1_end = p2, y2_start = p3, y2_end = p4,
            diffid2 = 37, y3_start = p5, y3_end = p6)
# saveRDS(spei_sgi_corr, "output/process/clusters_spei_sgi_corr_allrec.rds")
smrei_sgi_corr = readRDS("output/process/clusters_smrei_sgi1911.rds") %>% 
  spearcorr(data=., dofilter=yano,
            diffid1=34, y1_start = p1, y1_end = p2, y2_start = p3, y2_end = p4,
            diffid2 = 37, y3_start = p5, y3_end = p6)
# saveRDS(smrei_sgi_corr, "output/process/clusters_smrei_sgi_corr_allrec.rds")

# heatmaps ----
a <- spi_sgi_corr %>% mutate(bold = ifelse(corr<0.4 & corr > -0.4, NA, "+/- 0.4")) %>%
  #readRDS("output/process/clusters_spi_sgi_corr_allrec.rds") %>% 
  arrange(id) %>%  ungroup() %>%
  # mutate(id = paste(id, Station, sep = "_")) %>%
  group_by(id) %>% mutate(agg=as.numeric(agg)) %>%
  arrange(agg) %>% ggplot(.) + aes(agg, id, group=id, fill = corr) + geom_tile() +
  # geom_point(aes(agg, id, colour=bold), shape=21)+
  # scale_fill_gradient2("", low="blue", mid = "yellow", high="red", midpoint = 0, limits=c(-1,1)) + 
  scale_fill_viridis_c(limits=c(-1,1), option="plasma") +
  scale_colour_manual(na.value = NA, values=c("+/- 0.4"="black"))+
  ggtitle("SPI") #+ xlab("")
a
b <- smri_sgi_corr %>% arrange(id) %>%  ungroup() %>%
  mutate(bold = ifelse(corr<0.4 & corr > -0.4, NA, "+/- 0.4")) %>%
  # mutate(id = paste(id, Station, sep = "_")) %>%
  group_by(id) %>% mutate(agg=as.numeric(agg)) %>%
  arrange(agg) %>% ggplot(.) + aes(agg, id, group=id, fill = corr) + geom_tile() +
  # geom_point(aes(agg, id, colour=bold), shape=21)+
  scale_colour_manual(na.value = NA, values=c("+/- 0.4"="black"))+
  
  # scale_fill_gradient2("", low="blue", mid = "yellow", high="red", midpoint = 0.5,
  #                      limits=c(0.0,1)) + 
  scale_fill_viridis_c(limits=c(-1,1), option="plasma") +
  ggtitle("SMRI") + xlab("")
c <- spei_sgi_corr %>% arrange(id) %>%  ungroup() %>%
  mutate(bold = ifelse(corr<0.4 & corr > -0.4, NA, "+/- 0.4")) %>%
  # mutate(id = paste(id, Station, sep = "_")) %>%
  group_by(id) %>% mutate(agg=as.numeric(agg)) %>%
  arrange(agg) %>% ggplot(.) + aes(agg, id, group=id, fill = corr) + geom_tile() +
  # geom_point(aes(agg, id, colour=bold), shape=21)+
  scale_colour_manual(na.value = NA, values=c("+/- 0.4"="black"))+
  
  # scale_fill_gradient2(low="blue", mid = "yellow", high="red", midpoint = 0.5,
  #                      limits=c(0.0,1)) +
  scale_fill_viridis_c(limits=c(-1,1), option="plasma") +
  ggtitle("SPEI")
d <- smrei_sgi_corr %>% arrange(id) %>%  ungroup() %>%
  mutate(bold = ifelse(corr<0.4 & corr > -0.4, NA, "+/- 0.4")) %>%
  # mutate(id = paste(id, Station, sep = "_")) %>%
  group_by(id) %>% mutate(agg=as.numeric(agg)) %>%
  arrange(agg) %>% ggplot(.) + aes(agg, id, group=id, fill = corr) + geom_tile() +
  # geom_point(aes(agg, id, colour=bold), shape=21)+
  scale_colour_manual(na.value = NA, values=c("+/- 0.4"="black"))+
  
  # scale_fill_gradient2(name = NULL, low="blue", mid = "yellow", high="red", midpoint = 0.5,
  #                      limits=c(0.0,1)) + 
  scale_fill_viridis_c(limits=c(-1,1), option="plasma") +
  ggtitle("SMREI")
library(cowplot)
legend <- get_legend(a)
plots <- plot_grid(a + theme(legend.position = "none"),
          b+ ylab("") + theme(axis.text.y = element_blank(), legend.position = "none"),
          c+ theme(legend.position = "none"),
          d  + ylab("") + theme(axis.text.y = element_blank(), legend.position = "none"),
          ncol = 2)
title <- ggdraw() + draw_label(paste(year(p1), " to ", year(p2), " or ", year(p3), " to ", year(p4), sep=""))
plot_grid(title, plot_grid(plots, legend, ncol=2, rel_widths=c(1,.2)), ncol=1, 
          rel_heights = c(.1,1))
plot_grid(title, plot_grid(a + theme(legend.position = "none"), legend, ncol=2, rel_widths=c(1,.2)), ncol=1, 
          rel_heights = c(.1,1))



# correlate values for many years and save in one df ----
p1=1992
p2=1994
p3=1996
p4=1998
p5=2000
spi_sgi_corr = readRDS("output/process/clusters_spi_sgi1911.rds") %>% 
  spearcorr(data=., dofilter=TRUE,
            diffid=34, y1_start = p1, y1_end = p2, y2_start = p1, y2_end = p2) %>% 
  group_by(id) %>% mutate(type= "spi",
                          Cmax = max(corr, na.rm=TRUE),
                          Amax = ifelse(Cmax==corr, agg, NA)) %>% filter(!is.na(Amax)) %>%
  distinct(id, Cmax, Amax, type) %>% mutate(time=paste(p1, p2, sep="-")) %>% 
  full_join(., 
            readRDS("output/process/clusters_spi_sgi1911.rds") %>% 
              spearcorr(data=., dofilter=TRUE,
                        diffid=34, y1_start = p2, y1_end = p3, y2_start = p2, y2_end = p3) %>% 
              group_by(id) %>% mutate(type= "spi",
                                      Cmax = max(corr, na.rm=TRUE),
                                      Amax = ifelse(Cmax==corr, agg, NA)) %>% filter(!is.na(Amax)) %>%
              distinct(id, Cmax, Amax, type) %>% mutate(time=paste(p2, p3, sep="-"))
            ) %>% 
  full_join(., 
            readRDS("output/process/clusters_spi_sgi1911.rds") %>% 
              spearcorr(data=., dofilter=TRUE,
                        diffid=34, y1_start = p3, y1_end = p4, y2_start = p3, y2_end = p4) %>% 
              group_by(id) %>% mutate(type= "spi",
                                      Cmax = max(corr, na.rm=TRUE),
                                      Amax = ifelse(Cmax==corr, agg, NA)) %>% filter(!is.na(Amax)) %>%
              distinct(id, Cmax, Amax, type) %>% mutate(time=paste(p3, p4, sep="-"))
  )%>% 
  full_join(., 
            readRDS("output/process/clusters_spi_sgi1911.rds") %>% 
              spearcorr(data=., dofilter=TRUE,
                        diffid=34, y1_start = p4, y1_end = p5, y2_start = p4, y2_end = p5) %>% 
              group_by(id) %>% mutate(type= "spi",
                                      Cmax = max(corr, na.rm=TRUE),
                                      Amax = ifelse(Cmax==corr, agg, NA)) %>% filter(!is.na(Amax)) %>%
              distinct(id, Cmax, Amax, type) %>% mutate(time=paste(p4, p5, sep="-"))
  )
# spi_sgi_corr %>% mutate(Cluster=gsub("_.*$", "", id)) %>% 
#   ggplot(.) + aes(y = Cmax, x= as.numeric(Amax), group=time) +
#   geom_point(aes(shape=Cluster, colour = type), size=2) +
#   scale_x_continuous(trans="log", breaks=c(1, 5, 10, 20, 50), limits=c(2,50)) +
#   facet_wrap(~time)


smri_sgi_corr = readRDS("output/process/clusters_smri_sgi1911.rds") %>% 
  spearcorr(data=., dofilter=TRUE,
            diffid=34, y1_start = p1, y1_end = p2, y2_start = p1, y2_end = p2) %>% 
  group_by(id) %>% mutate(type= "smri",
                          Cmax = max(corr, na.rm=TRUE),
                          Amax = ifelse(Cmax==corr, agg, NA)) %>% filter(!is.na(Amax)) %>%
  distinct(id, Cmax, Amax, type) %>% mutate(time=paste(p1, p2, sep="-")) %>% 
  full_join(., 
            readRDS("output/process/clusters_spi_sgi1911.rds") %>% 
              spearcorr(data=., dofilter=TRUE,
                        diffid=34, y1_start = p2, y1_end = p3, y2_start = p2, y2_end = p3) %>% 
              group_by(id) %>% mutate(type= "smri",
                                      Cmax = max(corr, na.rm=TRUE),
                                      Amax = ifelse(Cmax==corr, agg, NA)) %>% filter(!is.na(Amax)) %>%
              distinct(id, Cmax, Amax, type) %>% mutate(time=paste(p2, p3, sep="-"))
  ) %>% 
  full_join(., 
            readRDS("output/process/clusters_spi_sgi1911.rds") %>% 
              spearcorr(data=., dofilter=TRUE,
                        diffid=34, y1_start = p3, y1_end = p4, y2_start = p3, y2_end = p4) %>% 
              group_by(id) %>% mutate(type= "smri",
                                      Cmax = max(corr, na.rm=TRUE),
                                      Amax = ifelse(Cmax==corr, agg, NA)) %>% filter(!is.na(Amax)) %>%
              distinct(id, Cmax, Amax, type) %>% mutate(time=paste(p3, p4, sep="-"))
  )%>% 
  full_join(., 
            readRDS("output/process/clusters_spi_sgi1911.rds") %>% 
              spearcorr(data=., dofilter=TRUE,
                        diffid=34, y1_start = p4, y1_end = p5, y2_start = p4, y2_end = p5) %>% 
              group_by(id) %>% mutate(type= "smri",
                                      Cmax = max(corr, na.rm=TRUE),
                                      Amax = ifelse(Cmax==corr, agg, NA)) %>% filter(!is.na(Amax)) %>%
              distinct(id, Cmax, Amax, type) %>% mutate(time=paste(p4, p5, sep="-"))
  )
spei_sgi_corr = readRDS("output/process/clusters_spei_sgi1911.rds") %>% 
  spearcorr(data=., dofilter=TRUE,
            diffid=34, y1_start = p1, y1_end = p2, y2_start = p1, y2_end = p2) %>% 
  group_by(id) %>% mutate(type= "spei",
                          Cmax = max(corr, na.rm=TRUE),
                          Amax = ifelse(Cmax==corr, agg, NA)) %>% filter(!is.na(Amax)) %>%
  distinct(id, Cmax, Amax, type) %>% mutate(time=paste(p1, p2, sep="-")) %>% 
  full_join(., 
            readRDS("output/process/clusters_spi_sgi1911.rds") %>% 
              spearcorr(data=., dofilter=TRUE,
                        diffid=34, y1_start = p2, y1_end = p3, y2_start = p2, y2_end = p3) %>% 
              group_by(id) %>% mutate(type= "spei",
                                      Cmax = max(corr, na.rm=TRUE),
                                      Amax = ifelse(Cmax==corr, agg, NA)) %>% filter(!is.na(Amax)) %>%
              distinct(id, Cmax, Amax, type) %>% mutate(time=paste(p2, p3, sep="-"))
  ) %>% 
  full_join(., 
            readRDS("output/process/clusters_spi_sgi1911.rds") %>% 
              spearcorr(data=., dofilter=TRUE,
                        diffid=34, y1_start = p3, y1_end = p4, y2_start = p3, y2_end = p4) %>% 
              group_by(id) %>% mutate(type= "spei",
                                      Cmax = max(corr, na.rm=TRUE),
                                      Amax = ifelse(Cmax==corr, agg, NA)) %>% filter(!is.na(Amax)) %>%
              distinct(id, Cmax, Amax, type) %>% mutate(time=paste(p3, p4, sep="-"))
  )%>% 
  full_join(., 
            readRDS("output/process/clusters_spi_sgi1911.rds") %>% 
              spearcorr(data=., dofilter=TRUE,
                        diffid=34, y1_start = p4, y1_end = p5, y2_start = p4, y2_end = p5) %>% 
              group_by(id) %>% mutate(type= "spei",
                                      Cmax = max(corr, na.rm=TRUE),
                                      Amax = ifelse(Cmax==corr, agg, NA)) %>% filter(!is.na(Amax)) %>%
              distinct(id, Cmax, Amax, type) %>% mutate(time=paste(p4, p5, sep="-"))
  )
smrei_sgi_corr = readRDS("output/process/clusters_smrei_sgi1911.rds") %>% 
  spearcorr(data=., dofilter=TRUE,
            diffid=34, y1_start = p1, y1_end = p2, y2_start = p1, y2_end = p2) %>% 
  group_by(id) %>% mutate(type= "smrei",
                          Cmax = max(corr, na.rm=TRUE),
                          Amax = ifelse(Cmax==corr, agg, NA)) %>% filter(!is.na(Amax)) %>%
  distinct(id, Cmax, Amax, type) %>% mutate(time=paste(p1, p2, sep="-")) %>% 
  full_join(., 
            readRDS("output/process/clusters_spi_sgi1911.rds") %>% 
              spearcorr(data=., dofilter=TRUE,
                        diffid=34, y1_start = p2, y1_end = p3, y2_start = p2, y2_end = p3) %>% 
              group_by(id) %>% mutate(type= "smrei",
                                      Cmax = max(corr, na.rm=TRUE),
                                      Amax = ifelse(Cmax==corr, agg, NA)) %>% filter(!is.na(Amax)) %>%
              distinct(id, Cmax, Amax, type) %>% mutate(time=paste(p2, p3, sep="-"))
  ) %>% 
  full_join(., 
            readRDS("output/process/clusters_spi_sgi1911.rds") %>% 
              spearcorr(data=., dofilter=TRUE,
                        diffid=34, y1_start = p3, y1_end = p4, y2_start = p3, y2_end = p4) %>% 
              group_by(id) %>% mutate(type= "smrei",
                                      Cmax = max(corr, na.rm=TRUE),
                                      Amax = ifelse(Cmax==corr, agg, NA)) %>% filter(!is.na(Amax)) %>%
              distinct(id, Cmax, Amax, type) %>% mutate(time=paste(p3, p4, sep="-"))
  )%>% 
  full_join(., 
            readRDS("output/process/clusters_spi_sgi1911.rds") %>% 
              spearcorr(data=., dofilter=TRUE,
                        diffid=34, y1_start = p4, y1_end = p5, y2_start = p4, y2_end = p5) %>% 
              group_by(id) %>% mutate(type= "smrei",
                                      Cmax = max(corr, na.rm=TRUE),
                                      Amax = ifelse(Cmax==corr, agg, NA)) %>% filter(!is.na(Amax)) %>%
              distinct(id, Cmax, Amax, type) %>% mutate(time=paste(p4, p5, sep="-"))
  )

# put values in same df, analyse ----
a <- spi_sgi_corr %>% 
  #readRDS("output/process/clusters_spi_sgi_corr_allrec.rds") %>%
  group_by(id) %>% mutate(type="spi", Cmax = max(corr, na.rm=TRUE),
                          Amax = ifelse(Cmax==corr, agg, NA)) %>% filter(!is.na(Amax)) %>%
  distinct(id, Cmax, Amax, type)
a <- smri_sgi_corr %>%
  # readRDS("output/process/clusters_smri_sgi_corr_allrec.rds") %>%
  group_by(id) %>% mutate(type= "smri", Cmax = max(corr, na.rm=TRUE), Amax = ifelse(Cmax==corr, agg, NA)) %>%
  filter(!is.na(Amax)) %>% distinct(id, Cmax, Amax, type) %>%
  full_join(.,a)
a <- spei_sgi_corr %>%
  # readRDS("output/process/clusters_spei_sgi_corr_allrec.rds") %>%
  group_by(id) %>% mutate(type="spei",Cmax = max(corr, na.rm=TRUE), Amax = ifelse(Cmax==corr, agg, NA)) %>%
  filter(!is.na(Amax)) %>% distinct(id, Cmax, Amax, type) %>%
  full_join(., a)
a <- smrei_sgi_corr %>%
  # readRDS("output/process/clusters_smrei_sgi_corr_allrec.rds") %>%
  group_by(id) %>% mutate(type= "smrei", Cmax = max(corr, na.rm=TRUE), Amax = ifelse(Cmax==corr, agg, NA)) %>%
  filter(!is.na(Amax)) %>% distinct(id, Cmax, Amax, type) %>%
  full_join(.,a)
b <- a %>% ungroup() %>% mutate(Cluster=gsub("_.*$", "", id), Station=gsub(".*_", "", id)) %>% 
  # rename(Cluster=id) %>% mutate(id=paste(Cluster, Station, sep="_")) %>% 
  group_by(id) %>% #, time) %>% 
  arrange(Cluster, Station, type) %>% #, time) %>%  
  mutate(Amax=as.numeric(Amax), minC = min(Cmax), maxC = max(Cmax), minA = min(Amax), maxA = max(Amax),
         rangeC=maxC-minC, rangeA=maxA-minA, type=ifelse(rangeC==0, "no gain", type)) %>% filter(Cmax==maxC) %>% 
  select(-minA, -maxA, -minC,-maxC) %>% distinct()


a <- b %>% mutate(period = "80")

# saveRDS(a, "output/process/clusters_cmax_amax_80.rds")

# a <- full_join(readRDS("output/process/clusters_cmax_amax_allrec.rds"),
#           readRDS("output/process/clusters_cmax_amax_95.rds")) %>% 
#   full_join(., readRDS("output/process/clusters_cmax_amax_03.rds"))
# a <- a %>% group_by(id, period) %>% mutate(Cmax=first(Cmax), 
#                                    Amax=first(Amax),
#                                    type=first(type)) %>% distinct(Cluster, Station, id, Cmax, Amax, type, period)
# saveRDS(a, "output/process/clusters_cmax_amax_all.rds")
# x y Cmax Amax plot ----
# a <- readRDS("output/process/clusters_cmax_amax_all.rds")
# a %>% filter(period=="03") %>% ungroup() %>% select(-period, -Cluster, -Station) %>% View()
a %>% #filter(period=="03") %>% 
  ggplot(.) + 
  aes(y = Cmax, x= Amax) +
  # geom_errorbar(aes(ymin=minC, ymax=maxC, group=Cluster), width = .05, alpha=.5) +  
  # geom_errorbarh(aes(xmin=minA, xmax=maxA), width=.05) +
  geom_point(aes(shape=Cluster, colour=type), size=3) +
  # scale_colour_viridis_d() + 
  scale_color_manual(values=c("smrei" = "#440154ff", 
                              "smri" = "#2d6e8eff", 
                              "spei" = "#bb3754",
                              "spi" = "#f9900dff",
                              "no gain" = "#42bb72ff")) +
  scale_x_continuous(#trans="log", 
                     breaks=c(10,20,30,40), limits=c(1,50)) +
  scale_y_continuous(limits=c(0.5,1), breaks=c(0.4,0.5,0.6, 0.7, 0.8, 0.9)) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid = element_line(colour="lightgrey", linetype=2)) + 
  ggtitle(paste(year(p1), " to ", year(p2), " or ", year(p3), " to ", year(p4), sep=""))
  facet_wrap(~time)
