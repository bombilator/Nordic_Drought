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

spi.ind <- readRDS("output/process/3clusters_spi.rds") %>% select(-month_past) %>% ungroup() %>% 
  as.data.frame() %>% group_by(id, agg) 

smri.ind <- readRDS("output/process/3clusters_smri.rds")%>% select(-month_past)%>% ungroup()%>%
  as.data.frame() %>% group_by(id, agg)
spei.ind <- readRDS("output/process/3clusters_spei.rds")%>% ungroup()%>% 
  as.data.frame() %>% group_by(id, agg) 
smrei.ind <- readRDS("output/process/3cluster_smrei.rds")%>% ungroup()%>%
  as.data.frame() %>% group_by(id, agg)


library(sp)
sp.g <- g.df %>% distinct(id, Station, N, E) %>% rename(N=E, E=N)
coordinates(sp.g) = ~N+E
proj4string(sp.g) = CRS("+proj=longlat + datum=WGS84")
sp.g <- spTransform(sp.g, CRS("+init=epsg:3006"))
sp.g <-  SpatialPoints(sp.g, proj4string = CRS("+init=epsg:3006"))
sp.p <- spi.ind %>% ungroup() %>% distinct(id, N, E)
coordinates(sp.p) = ~N+E
proj4string(sp.p) = CRS("+proj=longlat + datum=WGS84")
sp.p <- spTransform(sp.p, CRS("+init=epsg:3006"))
sp.p <- SpatialPoints(sp.p, proj4string = CRS("+init=epsg:3006"))
library(rgeos)
id <- apply(gDistance(sp.p, sp.g, byid=TRUE), 1, which.min)
ggmap(stamen) + geom_point(data=g.df %>% distinct(id, Station, N, E), aes(N,E), colour="black")+
  geom_point(data=spi.ind %>% ungroup() %>% distinct(id, N, E), aes(E,N), colour="red")

# Step 1: get sgi and spi data in one dataframe ----

spi.sgi <- g.df %>%  group_by(Station, N, E) %>% nest() %>% 
  mutate(stand= map(data, merge, spi.ind %>% select(id, agg, index, date), by=c("date", "id")))
saveRDS(spi.sgi, "output/process/3clusters_spi_sgi.rds")

smri.sgi <- g.df %>% group_by(Station, N, E) %>% nest() %>% 
  mutate(stand= map(data, merge, smri.ind %>% select(id, agg, index, date), by=c("date", "id")))
saveRDS(smri.sgi, "output/process/3clusters_smri_sgi.rds")

spei.sgi <- g.df %>% group_by(Station, N, E) %>% nest() %>% 
  mutate(stand= map(data, merge, spei.ind%>% select(id, agg,index,  date),  by=c("date", "id")))
saveRDS(spei.sgi, "output/process/3clusters_spei_sgi.rds")

smrei.sgi <- g.df %>% group_by(Station, N, E) %>% nest() %>% 
  mutate(stand= map(data, merge, smrei.ind%>% select(id, agg, index, date), by=c("date", "id")))
saveRDS(smrei.sgi, "output/process/3clusters_smrei_sgi.rds")


# Step 2: correlate the values ----

# why only Kendall correlations work-ish: https://stackoverflow.com/questions/27047598/r-cor-method-pearson-returns-na-but-method-spearman-returns-value-why
spi_sgi_corr = readRDS("output/process/3clusters_spi_sgi.rds") %>% 
  select(-data) %>% unnest() %>%
  filter(date >= as.Date("1990-01-15") & date <= as.Date("1995-01-15")) %>%
  select(-mean_mon, -year, -month) %>% group_by(id, Station, N, E) %>% arrange(agg, date) %>% 
  nest(.key = "stand") %>%
  mutate(
    cor_sgi_spi = 
      # purrr::map(stand, gather, spi_agg, spi, -date, -sgi) %>% 
      purrr::map(stand, group_by, agg) %>% 
      purrr::map(summarize, cor_sgi_spi = cor(sgi, index, use = "everything",
                                              method="spearman"))
  )
# saveRDS(spi_sgi_corr, "output/process/3clusters_spi_sgi_corr.rds")

smri_sgi_corr = readRDS("output/process/3clusters_smri_sgi.rds") %>% select(-data) %>% unnest() %>% 
  filter(date >= as.Date("1990-01-15") & date <= as.Date("1995-01-15")) %>%
  select(-mean_mon, -year, -month) %>% group_by(id, Station, N, E) %>% arrange(agg, date) %>% 
  nest(.key = "stand") %>% 
  mutate(
    cor_sgi_smri = 
      # purrr::map(stand, gather, spi_agg, spi, -date, -sgi) %>% 
      purrr::map(stand, group_by, agg) %>% 
      purrr::map(summarize, cor_sgi_smri = cor(sgi, index, use = "everything", 
                                               method="spearman"))
  )
# saveRDS(smri_sgi_corr, "output/process/3clusters_smri_sgi_corr.rds")

spei_sgi_corr = readRDS("output/process/3clusters_spei_sgi.rds") %>% 
  select(-data) %>% unnest() %>% 
  filter(date >= as.Date("1990-01-15") & date <= as.Date("1995-01-15")) %>%
  select(-mean_mon, -year, -month) %>% group_by(id, Station, N, E) %>% arrange(agg, date) %>% 
  nest(.key = "stand") %>% 
  mutate(
    cor_sgi_smri = 
      # purrr::map(stand, gather, spi_agg, spi, -date, -sgi) %>% 
      purrr::map(stand, group_by, agg) %>% 
      purrr::map(summarize, cor_sgi_spei = cor(sgi, index, use = "everything", 
                                               method="spearman"))
  )

smrei_sgi_corr = readRDS("output/process/3clusters_smrei_sgi.rds") %>% select(-data) %>% unnest() %>%
  filter(date >= as.Date("1990-01-15") & date <= as.Date("1995-01-15")) %>%
  select(-mean_mon, -year, -month) %>% group_by(id, Station, N, E) %>% arrange(agg, date) %>% 
  nest(.key = "stand") %>% 
  mutate(
    cor_sgi_smri = 
      # purrr::map(stand, gather, spi_agg, spi, -date, -sgi) %>% 
      purrr::map(stand, group_by, agg) %>% 
      purrr::map(summarize, cor_sgi_smrei = cor(sgi, index, use = "everything", 
                                               method="spearman"))
  )

# heatmaps ----
# a <- spi_sgi_corr %>% select(-stand) %>% unnest() %>% arrange(id, Station) %>%  ungroup() %>%
#   mutate(id = paste(id, Station, sep = "_")) %>%
#   group_by(id, Station) %>% mutate(agg=as.numeric(agg)) %>%
#   arrange(agg) %>% ggplot(.) + aes(agg, id, group=id, fill = cor_sgi_spi) + geom_tile() +
#   scale_fill_gradient2("", low="blue", mid = "yellow", high="red", midpoint = 0.4,
#                        limits=c(0.0,0.9)) + ggtitle("SPI") + xlab("")
# b <- smri_sgi_corr %>% select(-stand) %>% unnest() %>% arrange(id, Station) %>%  ungroup() %>%
#   mutate(id = paste(id, Station, sep = "_")) %>%
#   group_by(id, Station) %>% mutate(agg=as.numeric(agg)) %>%
#   arrange(agg) %>% ggplot(.) + aes(agg, id, group=id, fill = cor_sgi_smri) + geom_tile() +
#   scale_fill_gradient2("", low="blue", mid = "yellow", high="red", midpoint = 0.4,
#                        limits=c(0.0,0.9)) + ggtitle("SMRI") + xlab("")
# c <- spei_sgi_corr %>% select(-stand) %>% unnest() %>% arrange(id, Station) %>%  ungroup() %>%
#   mutate(id = paste(id, Station, sep = "_")) %>%
#   group_by(id, Station) %>% mutate(agg=as.numeric(agg)) %>%
#   arrange(agg) %>% ggplot(.) + aes(agg, id, group=id, fill = cor_sgi_spei) + geom_tile() +
#   scale_fill_gradient2(low="blue", mid = "yellow", high="red", midpoint = 0.4,
#                        limits=c(0.0,0.9)) + ggtitle("SPEI")
# d <- smrei_sgi_corr %>% select(-stand) %>% unnest() %>% arrange(id, Station) %>%  ungroup() %>%
#   mutate(id = paste(id, Station, sep = "_")) %>%
#   group_by(id, Station) %>% mutate(agg=as.numeric(agg)) %>%
#   arrange(agg) %>% ggplot(.) + aes(agg, id, group=id, fill = cor_sgi_smrei) + geom_tile() +
#   scale_fill_gradient2(name = NULL, low="blue", mid = "yellow", high="red", midpoint = 0.4,
#                        limits=c(0.0,0.9)) + ggtitle("SMREI")
# library(cowplot)
# legend <- get_legend(d)
# plots <- plot_grid(a + theme(legend.position = "none"),
#           b+ ylab("") + theme(axis.text.y = element_blank(), legend.position = "none"), 
#           c+ theme(legend.position = "none"), 
#           d  + ylab("") + theme(axis.text.y = element_blank(), legend.position = "none"), ncol = 2)
# plot_grid(plots, legend, ncol=2, rel_widths=c(1,.2))


# x y Cmax Amax plot ----
a <- spi_sgi_corr %>% select(-stand) %>% 
  unnest() %>% group_by(id, Station) %>% mutate(type="spi", 
                                                Cmax = max(cor_sgi_spi, na.rm=TRUE),
                                                Amax = ifelse(Cmax==cor_sgi_spi, agg, NA)) %>% 
  filter(!is.na(Amax)) %>%
  distinct(Station, id, Cmax, Amax, type)
a <- smri_sgi_corr %>% select(-stand) %>% 
  unnest() %>% group_by(id, Station) %>% mutate(type= "smri", 
                                                Cmax = max(cor_sgi_smri, na.rm=TRUE),
                                                Amax = ifelse(Cmax==cor_sgi_smri, agg, NA)) %>% 
  filter(!is.na(Amax)) %>%
  distinct(Station, id, Cmax, Amax, type) %>% full_join(.,a)
a <- spei_sgi_corr %>% select(-stand) %>% 
  unnest() %>% group_by(id, Station) %>% mutate(type="spei",
                                                Cmax = max(cor_sgi_spei, na.rm=TRUE),
                                                Amax = ifelse(Cmax==cor_sgi_spei, agg, NA)) %>% 
  filter(!is.na(Amax)) %>%
  distinct(Station, id, Cmax, Amax, type) %>% full_join(., a)
a <- smrei_sgi_corr %>% select(-stand) %>% 
  unnest() %>% group_by(id, Station) %>% mutate(type= "smrei",
                                                Cmax = max(cor_sgi_smrei, na.rm=TRUE),
                                                Amax = ifelse(Cmax==cor_sgi_smrei, agg, NA)) %>% 
  filter(!is.na(Amax)) %>%
  distinct(Station, id, Cmax, Amax, type) %>% full_join(.,a)
a %>% ungroup() %>% filter(id==14 | id==55 | id==37) %>% 
  rename(Cluster=id) %>% mutate(id=paste(Cluster, Station, sep="_")) %>% group_by(id) %>% 
  arrange(Cluster, Station, type) %>%  mutate(Amax=as.numeric(Amax), 
                                        minC = min(Cmax), maxC = max(Cmax),
                                        minA = min(Amax), maxA = max(Amax)) %>% filter(Cmax==max(Cmax)) %>% 
  # View()
  ggplot(.) + 
  aes(y = Cmax, x= Amax) +
  # geom_errorbar(aes(ymin=minC, ymax=maxC, group=Cluster), width = .05, alpha=.5) +  
  # geom_errorbarh(aes(xmin=minA, xmax=maxA), width=.05) +
  geom_point(aes(shape=Cluster, colour=type), size=3) +
  scale_colour_viridis_d() + scale_x_continuous(trans="log", breaks=c(1, 5, 10, 20, 50), limits=c(3,50)) +
  scale_y_continuous(limits=c(0.35,1), breaks=c(0.4,0.5,0.6, 0.7, 0.8, 0.9)) +
  theme(panel.background = element_rect(fill="white", colour = "black"),
        panel.grid = element_line(colour="lightgrey", linetype=2))
