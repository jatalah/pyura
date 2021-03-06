library(tidyverse)
library(readxl)
library(vegan)
library(ggord)
library(ggpubr)

source('theme_javier.R')
theme_set(theme_javier())

# read and prepare data ---------
quad <- read_excel('data/pyura.xlsx')

factors <- 
  quad %>% 
  transmute(image = `Frame image name`,file = `CPC filename`) %>% 
  distinct() %>% 
  mutate(image = str_sub(image,-12),
         file = str_sub(file,-11, -5),
         site = str_sub(file,1,1),
         density = str_sub(file,3,3),
         treat =  str_sub(file,5,5),
         rep =  str_sub(file,-1),
         density = fct_recode(density, Control = "O", High = "H", Low = "L"),
         density = fct_relevel(density, "Control", "Low", "High"))


cover_data <- 
  read_excel('data/pyura.xlsx', sheet = "pyura_%cover", skip = 1) %>% 
  select(`Photo Name`, `Cellana_ornata (Cel)`:`Epopella plicata (Epo)`) %>% 
  select_if(~sum(!is.na(.)) > 0) %>% 
  select_if(~ !is.numeric(.) || sum(.) != 0) %>% 
  rename(image = `Photo Name` ) 

names(cover_data) <- 
  str_replace(names(cover_data), "(?s) .*", "") %>%
  str_replace(., "_", " ")
  
write_csv(cover_data, 'data/cover_data.csv')


##Merge cover data with factors
cover <- 
  left_join(cover_data, factors) %>% 
  rename(Sand = sand,
         `Bare space` = `bare `,
         Ulva = Ulva.) %>% 
  select(-`Pyura doppel`)
  
log_cover <- log10(cover[,c(2:21)]+1)


# nMDS plot---------------
mds <- metaMDS(log_cover, distance = 'bray')


mds_plot <- 
  ggord(
    mds,
    grp_in = paste(cover$site,cover$density, sep = " - "),
    poly = F,
    ellipse = F,
    arrow = 0,
    repel = T,
    text = .01,
    vec_ext = 1
  ) + 
  theme_javier() +
  annotate(geom = 'text', x = 1.3, y = .7, label = paste("Stress =",round(mds$stress, 2)), size = 5) +
  scale_shape_manual(values = c(1, 2, 5, 21, 24, 23))+
  scale_color_manual(values = c(1, 2, 3, 1, 2, 3)) +
  scale_fill_manual(values = c(1, 2, 3, 1, 2, 3))


ggsave(mds_plot, 
       filename = 'figures/mds_plot.tiff',
       device = 'tiff',
       compression = 'lzw',
       width = 8,
       height = 5,
       dpi = 600)

# PERMANOVA-----------
permanova <- adonis(log10(cover[,c(2:21)]+1)~site*density,data = cover,method = 'bray',permutations = 9999)
permanova

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
  write_csv(., 'outputs/permanova_table.csv', na = "")

# SIMPER------------
simp <- simper(log10(cover[,c(2:21)]+1), cover$density, permutations = 999)
# simp <- simper(fauna_dat[,-c(1:4)], fauna_dat$Treatment, permutations = 999)
summary(simp, digits = 2, ordered = T)


%>%
  rename(P = p) %>% 
  mutate_at(vars(average:cusum), ~ round(., digits = 3)) %>%
  mutate(P = round(P, 3)) %>%
  write_csv(., 'tables/SIMPER_tables.csv', na = "")


# diversity indices----
indices <- 
  cover[,c(2:21)] %>% 
  round(.) %>% 
  transmute(N = rowSums(.) - (`Bare space` + Sand + Biofilm),
            H = diversity(.),
            S = specnumber(.),
            J = H/log(S),
            ES = rarefy(., min(N))) %>% 
  bind_cols(factors)

indices_long <- 
  indices %>% 
  gather(index, value, c("S", "N", "J", "H")) %>% 
  mutate(index = fct_relevel(index, c("S","N", "J")))


# diversity plots--------------
p1 <- 
  ggplot(filter(indices_long, site=='K'), aes(x = density, y = value)) +
  geom_boxplot(alpha = .8) +
  facet_wrap(~index, scales = 'free',ncol = 4) + labs(x = '') +
  theme_javier()
p2 <- 
  ggplot(filter(indices_long, site=='S'), aes(x = density, y = value)) +
  geom_boxplot(alpha = .8) +
  facet_wrap(~index, scales = 'free', ncol = 4)+ labs(x = '') +
  theme_javier()


ggsave(ggarrange(p2,p1, nrow = 2,labels = c("A. Kautou", "B. Shipwreck"), vjust = c(1,.8), hjust = c(-.1,-.1)), 
       filename = 'figures/diversity_plot.tiff',
       device = 'tiff',
       compression = 'lzw',
       width = 8,
       height = 5,
       dpi = 600)


# anova indices---------------
indices_dat <-
  indices %>%
  mutate(N = log(N),
         S = log(S)) %>% 
  gather(index, value, c("N","H", "S", "J")) %>%
  group_by(index) %>%
  nest() %>%  
  mutate(
    lms = map(.x = data, ~ aov(value ~ site*density, data = .x)),
    anova_tab = map(lms, anova),
    anova_table = map(anova_tab, broom::tidy),
    residuals = map(lms, broom::augment)
  ) 

anova_tables <- 
  indices_dat %>% 
  select(index,anova_table) %>% 
  unnest()

## model validation----
res <- 
  indices_dat %>% 
  select(index, residuals) %>% 
  unnest(residuals, .drop = TRUE)

# Plot of fitted vs. residual values by index to check the assumptions heteroscedasticity or any other pattern,
ggplot(res) +
  geom_point(aes(x = .fitted, y = .resid), alpha = .3) +
  facet_wrap( ~ index, scale = 'free') +
  geom_hline(yintercept = 0,
             lty = 2,
             col = 2) +
  theme_bw()


# fitted vs obs
ggplot(res, aes(x = .fitted, y = value)) +
  geom_point( alpha = .3) +
  geom_smooth(method = 'lm') +
  facet_wrap( ~ index, scale = 'free') +
  theme_bw()

# boxplot by density 
ggplot(res) +
  geom_boxplot(aes(x = density, y = .resid), alpha = .3) +
  facet_wrap( ~ index, scale = 'free') +
  geom_hline(yintercept = 0,
             lty = 2,
             col = 2)

# qqplot of the normalised residuals to check the assumption of normality------------
ggplot(res) +
  stat_qq(aes(sample = .std.resid), alpha = .3) +
  facet_wrap( ~ index, scale = 'free') +
  geom_abline(
    intercept =  0,
    slope = 1,
    lty = 2,
    col = 2
  )
