# # #convoluted and messy way to organise the data, rendered obsolete:
# library(tidyr)
# spi_sgi_corr <- spi_sgi_corr %>% distinct() %>% 
#   gather(key = "spi", value = "corr_value", 
#          starts_with("spi")) %>%
#   gather(key = "lag", value = "spi", spi) %>% 
#   mutate(lag = str_sub(spi, -1, -1), #get the last character
#          spi = str_sub(spi, 4,5), #get the 4th and 5th character
#          spi = gsub("_","",spi)) #remove "_" from the character
# saveRDS(spi_sgi_corr, "R_finland_drought/output/process/fin_spi_sgi_corr.rds")

# 
# 
# plot spi sgi correlations ----
# 
#     
library(ggplot2)

met.coor <- p.df %>% distinct(E,N,id) # for mapping the results spatially ----
# 
# get max corr ----
smri_sgi_corr <- readRDS("R_finland_drought/output/process/fin_smri_sgi10_corr.rds") %>% 
  mutate(
  max_corr = purrr::map(cor_sgi_smri, summarise, 
                        smri_max_corr=max(cor_sgi_smri, na.rm = TRUE))) %>% 
  unnest(.key=max_corr) %>% select(-max_corr)
spi_sgi_corr <- readRDS("output/process/fin_spi_sgi_corr.rds") %>% 
  mutate(
  max_corr = purrr::map(cor_sgi_spi, summarise, 
                        spi_max_corr=max(cor_sgi_spi, na.rm = TRUE))) %>% 
  unnest(.key=max_corr) %>% select(-max_corr)



# Michelle's 'confusing' individualistic plots ----
spi_sgi_corr_bind <- spi_sgi_corr %>% select(-stand) %>% unnest %>% 
  mutate(agg = as.numeric(agg)) %>% filter(id %in% smri_sgi_corr$id & 
                                             cor_sgi_spi == spi_max_corr) %>%
  select(-cor_sgi_spi) %>% rename(agg.spi = agg)
smri_sgi_corr_bind <- smri_sgi_corr %>% select(-stand) %>% unnest %>% 
  mutate(agg = as.numeric(agg)) %>% filter(cor_sgi_smri == smri_max_corr) %>%
  select(-cor_sgi_smri) %>% rename(agg.smri = agg)
spi_smri_bind <- left_join(spi_sgi_corr_bind, smri_sgi_corr_bind, by = 
                             c("id", "Stationname", "Tunnus")) %>% 
  mutate(diff = smri_max_corr - spi_max_corr,
         agg.diff = ifelse(diff>0, agg.smri, agg.spi),
         diff_best = ifelse(diff>0, 'smri', 'spi'),
         max_max_corr = ifelse(diff_best=='smri', 
                               smri_max_corr, spi_max_corr)) 

spi_smri_bind %>%
  ggplot(.) + 
  geom_point(aes(x=agg.diff,
                 y=max_max_corr, group = Tunnus, #colour = Tunnus,
        # colour = Tunnus,
        size =diff),
        alpha=.5) +
  # geom_point(aes(agg.diff,
  #                spi_max_corr, group = Tunnus, #colour = Tunnus,
        # colour = Tunnus), shape = 2) +
  facet_wrap(~diff_best)

#heatmap
spi_sgi_corr %>% head
smri_sgi_corr %>% select(id, Stationname, Tunnus, cor_sgi_smri) %>% unnest %>% 
  filter(id==2491) %>% ggplot(.) + 
  aes(as.numeric(agg), Tunnus,fill = cor_sgi_smri) + geom_tile() 
  


# Ezra's Boxplots  ----
spi_sgi_corr_bind <- spi_sgi_corr %>% select(-stand) %>% unnest %>% 
  mutate(agg = as.numeric(agg),
         type = "spi") %>% filter(id %in% smri_sgi_corr$id & 
                                             cor_sgi_spi == spi_max_corr) %>%
  select(-cor_sgi_spi) %>% rename(max_corr = spi_max_corr) %>%
  select(type, agg, max_corr, Tunnus)
smri_sgi_corr_bind <-  smri_sgi_corr %>% select(-stand) %>% unnest %>% 
  mutate(agg = as.numeric(agg),
         type = "smri") %>% filter(cor_sgi_smri == smri_max_corr) %>%
  select(-cor_sgi_smri) %>% rename(max_corr = smri_max_corr) %>%
  select(type, agg, max_corr, Tunnus)
spi_smri_bind <- full_join(spi_sgi_corr_bind, 
                           smri_sgi_corr_bind, by = c('agg', 'max_corr', 
                                                      'type', 'Tunnus'), 
                           all = FALSE)
library(ggrepel)
spi_smri_bind %>% 
  mutate( #type_2 = type) %>% 
    type2 = diff_best) %>%
  complete(diff_best, agg.diff, type2) %>%
    #type, agg, type_2) %>%
  ggplot(.) +  
  aes(as.factor(agg.diff), 
      max_max_corr, fill = diff_best, colour = type2) +
  # geom_boxplot(position = position_dodge(1)) +
  geom_jitter() +
  geom_text_repel(aes(label = Tunnus)) +
  scale_color_brewer(palette = "Accent") +
  theme_classic() +
  facet_wrap(~agg.diff, nrow = 1, scales = "free_x") +
  theme(strip.background = element_blank(), strip.text = element_blank())

# geom_tile() + 
# scale_fill_gradient2(limits=c(-1,1)) + 
# ylim(0, 1) +
# coord_equal() #+
# geom_point(
#   data = smri_max_corr, fill = "black", colour = "black"
# )

g.df <- readRDS("R_finland_drought/output/process/fin_gw_sgi.rds") %>% 
  select(Stationname, Tunnus, id, gw_monthly) %>% unnest() %>% rename(date = Date)
# smri_sgi_corr %>%
g.df %>%
  filter(Tunnus=="1104p8") %>% 
  # unnest(.key = stand) %>%
  # group_by(agg) %>% filter(agg==9) %>%
  ggplot(.) + aes(date, mean_mon) + 
                  # sgi) +
                  geom_line() #+ #geom_smooth(method='lm') + 
  # geom_line(aes(date, index), colour = "red")
