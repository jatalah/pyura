library(tidyverse)
library(vegan)
library(ggord)
library(ggpubr)
library(DT)
library(knitr)
library(broom)
source('theme_javier.R')
theme_set(theme_javier())

# read data--------
cover <- read_csv('data/cover_data.csv')


# 05 Diversity indices-------
indices <- 
cover %>% 
  select(`Cellana ornata`:Epopella, -Biofilm, - `Bare space`) %>% 
  mutate(`H'` = diversity(.),
         S = specnumber(.),
         `J'` = `H'`/log(S),
         N =  rowSums(.)) %>% 
  bind_cols(cover %>% select(image, site, density, treat))
  

indices_long <- 
  indices %>% 
  gather(index, value, c("S", "N", `J'`, `H'`)) %>% 
  mutate(index = fct_relevel(index, c("N","S", "J'")),
         site = fct_recode(site, Koutau = "K", Shipwreck = "S")) %>% 
  mutate(density = fct_relevel(density, "Control", "Low")) 

write_csv(indices_long, 'data/indices_long.csv')


# Figure 5 Diversity plots--------------
div_boxplots <- 
  ggplot(indices_long, aes(x = density, y = value, fill = site)) +
  geom_boxplot(alpha = .8) +
  facet_wrap(~index, scales = 'free', ncol = 4)+ labs(x = '') +
  theme_javier() +
  scale_fill_grey(name = 'Site', start = .8, end = 1) +
  facet_wrap( ~ index, scales = 'free_y', nrow = 2) +
  theme_javier(base_size = 10) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank())
div_boxplots

ggsave(div_boxplots, 
       filename = 'figures/figure_5_diversity_boxplot.tiff',
       device = 'tiff',
       compression = 'lzw',
       width = 8,
       height = 4,
       dpi = 600)

ggsave(div_boxplots, 
       filename = 'figures/figure_5_diversity_boxplot.svg',
       width = 8,
       height = 4)


# bar plots ----
div_plots <- 
  ggplot(indices_long, aes(x = density, y = value, fill = site)) +
  stat_summary(
    fun = mean,
    geom = "bar",
    position = position_dodge(width = .9),
    color = 1
  ) +
  stat_summary(
    fun.data = mean_se,
    position = position_dodge(width = .9),
    geom = "errorbar",
    width = 0.2
  ) +
  # labs(y = '', x = 'Density') +
  scale_fill_grey(name = 'Site', start = .8, end = 1, guide = F) +
  facet_wrap( ~ index, scales = 'free_y', nrow = 2) +
  theme_javier(base_size = 10) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(.1,.9),
        legend.title = element_blank())
div_plots



ggsave(div_plots, 
       filename = 'figures/diversity_plot.tiff',
       device = 'tiff',
       compression = 'lzw',
       width = 8,
       height = 5,
       dpi = 600)

# Indices descriptive stats----
indices_long %>% 
  group_by(index, site) %>% 
  summarise(mean = mean(value),
            n = n(),
            sd = sd(value),
            se = sd/sqrt(n)) %>% 
  print(n = 40)

indices_long %>% 
  group_by(index, site, density) %>% 
  summarise(mean = mean(value),
            n = n(),
            sd = sd(value),
            se = sd/sqrt(n)) %>% 
  print(n = 40)

# 07 Anova indices---------------
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
    residuals = map(lms, broom::augment),
    posthoc = map(lms,TukeyHSD),
    posthoc_tables = map(posthoc, tidy)
  ) 

anova_tables <-
  indices_dat %>%
  select(index, anova_table) %>%
  unnest() %>%
  rename(Index = index,
         Term = term,
         P = p.value,
         MS = meansq,
         F = statistic) %>%
  select(-sumsq) %>%
  mutate(Term = fct_recode(
    Term,
    Site = "site",
    Density = "density",
    `Site x Density` = "site:density"
  )) %>%
  write_csv('outputs/anova_indices.csv', na = "")

datatable(anova_tables,caption = 'ANOVAs univariate indices', filter = 'top')%>% 
  formatRound(columns=c(4:10), digits=3)


# posthoc tables -----
post_hoc_tables <- 
  indices_dat %>%
  select(index, posthoc_tables) %>%
  unnest(cols = c(posthoc_tables)) %>%
  rename(
    Index = index,
    Term = term,
    P = adj.p.value,
    Comparison = comparison
  ) %>% 
  mutate(Term = fct_recode(
    Term,
    Site = "site",
    Density = "density",
    `Site x Density` = "site:density"
  )) %>%
  write_csv('outputs/posthoc_indices.csv', na = "")

datatable(post_hoc_tables, filter = 'top') %>% 
  formatRound(columns=c(4:10), digits=3)

# 08 ANOVA model validation------
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

# qqplot of the normalised residuals to check the assumption of normality
ggplot(res) +
  stat_qq(aes(sample = .std.resid), alpha = .3) +
  facet_wrap( ~ index, scale = 'free') +
  geom_abline(
    intercept =  0,
    slope = 1,
    lty = 2,
    col = 2
  )
