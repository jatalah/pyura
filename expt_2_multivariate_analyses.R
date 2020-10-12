library(tidyverse)
library(vegan)
library(ggord)
library(ggpubr)
library(knitr)
source('theme_javier.R')
theme_set(theme_javier())

# read data--------
cover <- read_csv('data/cover_data.csv') %>%
  mutate(
    inter = paste(site, density, sep = " - "),
    inter = fct_relevel(
      inter,
      "K - Control",
      "K - Low",
      "K - High",
      "S - Control",
      "S - Low",
      "S - High"
    )
  )

log_cover <- 
  log(cover[,c(2:20)]+1) %>% 
  select(-`Pyura doppel`)

fourth_root_cover <- 
  sqrt(sqrt(cover[,c(2:20)]+1)) %>% 
  select(-`Pyura doppel`)

names(log_cover)

# Plot Pyura recruits data-----------
ggplot(cover) +
  geom_point(aes(density, Perna)) +
  facet_wrap( ~ site)

ggplot(cover) +
  geom_point(aes(density, `Pyura doppel`)) +
  facet_wrap( ~ site)


cover %>%
  group_by(site, density) %>%
  summarise_at(vars(Perna, `Pyura doppel`),list(mean = mean, se = ~sd(.)/sqrt(n())))

# Figure 4 -  nMDS plot---------------
mds <- metaMDS(log_cover, distance = 'bray')

mds_plot <- 
ggord(
  mds,
  grp_in = cover$inter,
  poly = F,
  ellipse = F,
  arrow = 0,
  repel = T,
  text = .01,
  vec_ext = 1
) + 
  theme_javier() +
  annotate(geom = 'text', x = -0.55, y = 1.5, label = paste("Stress =",round(mds$stress, 2)), size = 4) +
  scale_shape_manual(values = c(1, 21, 21, 0, 22, 22))+
  scale_color_manual(values = c(rep(1,6))) +
  scale_fill_manual(values = rep(c(1, 'gray50', 1),2))

print(mds_plot)

ggsave(mds_plot, 
       filename = 'figures/figure_4_mds_plot.tiff',
       device = 'tiff',
       compression = 'lzw',
       width = 8,
       height = 5,
       dpi = 600)

ggsave(mds_plot, 
       filename = 'figures/figure_4_nMDS.svg',
       width = 8,
       height = 5)

# 03 PERMANOVA-----------
permanova <-
  adonis(
    log_cover ~ site * density,
    data = cover,
    method = 'bray',
    permutations = 9999
  )
permanova

# prepare PERANOVA table-
permanova_table <- 
  data.frame(permanova$aov.tab) %>% 
  rownames_to_column() %>% 
  dplyr::rename(Terms = rowname,
                df = Df,
                SS = SumsOfSqs,
                MS = MeanSqs,
                "Pseudo-F" = F.Model,
                P = Pr..F.)%>%
  filter(Terms != 'Total') %>% 
  mutate_at(vars(SS:R2), ~round(.,digits = 2)) %>% 
  mutate(P = signif(P,3)) %>%
  print() %>% 
  write_csv(., 'outputs/permanova_table.csv', na = "")

# 04 SIMPER------------
simp <- simper(log_cover, cover$site, permutations = 999)
simp_res <- summary(simp, digits = 2, ordered = T)


simp_res$K_S %>% 
  data.frame() %>% 
  rownames_to_column("Taxa") %>% 
  dplyr::filter(cumsum<.9) %>% 
  dplyr::select(Taxa, average, sd,ratio, ava, avb, cumsum, p) %>%
    rename(P = p,
         `Mean Koutou` = ava,
         `Mean Shipwreck` = avb) %>% 
  mutate_at(vars(average:cumsum), ~ round(., digits = 2)) %>%
  mutate(P = round(P, 3),
         Taxa = fct_recode(Taxa,
                           `Xenostrobus pulex` = "Xenostrobus",
                           `Ulva sp.` = "Ulva",
                           # `Epopella plicata` = "Epopella",
                           `Scytothamnus australis` = "Scytothamnus",
                           `Ralfsia sp.` = "Ralfsia",
                           `Gelidium sp.` = "Gelidium",
                           `Crustose coralline algae` = "Crustose Coralline")) %>%
  print() %>% 
  write_csv( 'outputs/SIMPER_table.csv', na = "")


simp_density <- simper(log_cover, cover$density, permutations = 999)
simp_res_density <- summary(simp_density, digits = 2, ordered = T)


simp_res_density$Low_Control %>% data.frame() %>% 
  rownames_to_column("Taxa")

simp_res_density$High_Control %>% data.frame() %>% 
  rownames_to_column("Taxa")


# Using multipatt------
library(indicspecies)
ind_sp_density <- multipatt(log_cover, factor(cover$density), duleg = TRUE)
summary(ind_sp_density, indvalcomp = TRUE)

ind_sp_site <- multipatt(log_cover, cover$site, duleg = TRUE)
summary(ind_sp_site, indvalcomp = TRUE)
