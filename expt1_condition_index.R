# load libraries
library(tidyverse)
library(readxl)
library(broom)
library(knitr)
library(ggpmisc)
library(ggpubr)
library(DT)
source('theme_javier.R')

# 01 read and prepare data----- 
ci <-
  read_excel('data/Length and CI.xlsx') %>% 
  filter(CI > 1 & Length>20) %>% # remove one outlier sample 32 CI<1
  mutate(
    Site = fct_recode(Site, Koutau = "K", Shipwreck = "S"),
    Density = fct_recode(
      Density,
      Control = "C",
      Low = "L",
      High = "H"
    ),
    Density = fct_relevel(Density, "Control", "Low"),
    Type = if_else(is.na(Type) & Site == "Koutau", "U", Type),
    Type = if_else(is.na(Type), "E", Type),
    Type = fct_recode(Type, Transplanted = "E", Undisturbed = "U"),
    Type = fct_relevel(Type, "Transplanted", "Undisturbed"),
    Site = fct_relevel(Site, "Shipwreck")
  )
  
dotchart(ci$CI)
hist(ci$CI)
summary(ci)

# 02 Summary stats condition index ----------
ci %>% 
  drop_na(CI) %>% 
  group_by(Site, Density,	 Type) %>% 
  summarise(mean = mean(CI),
            n = n(),
            sd = sd(CI),
            se = sd/sqrt(n)) %>% 
  datatable(filter = 'top',caption = 'Condition index')%>% 
  formatRound(columns=c(4:10),  digits=2)


# 03 Boxplot of mussel size-----
ggplot(ci, aes(x = Density, y = Length)) +
  geom_boxplot() +
  facet_grid( Type ~ Site) +
  theme_bw()

# mussel length and dry weight allometric relationship----------
ci %>% 
  ggplot(aes(y = Flesh_DW, x = Length,  fill = Density, shape = Density, color = Density)) +
  geom_point(size = 3, alpha = 0.9) +
  facet_wrap( Type ~ Site) +
  theme_javier() +
  # geom_smooth(aes(linetype = Density),
  #   method = "nls",
  #   formula = y ~ a * exp(b * x),
  #   method.args = list(start = c(a = 0.5, b = 0)),
  #   se = FALSE,  size = 0.6
  # ) +
  geom_smooth(aes(linetype = Density),
              method = "lm",
              formula = y ~ poly(x, 2),
              se = FALSE,  size = 0.6
  ) +
  scale_fill_manual(values = c(1, 'gray30', 'gray80')) +
  scale_color_manual(values = c(1, 1, 1)) +
  scale_shape_manual(values = c(1, 21, 21 )) +

  labs(y = "Flesh gry weight (g)", x = "Length (cm)" ) +
  theme(legend.position = c(.1,.8),
        legend.title = element_blank(),
        legend.background = element_blank()) 


# Figure 3 - mussel log-log length and dry weight allometric relationship----------
allo_plots <- 
  ci %>% 
  ggplot(aes(y = Flesh_DW, x = Length,  fill = Density, shape = Density, color = Density)) +
  geom_point(size = 3, alpha = 0.7) +
  facet_wrap(Type ~ Site) +
  theme_javier() +
  geom_smooth(method = "lm", se = F, aes(linetype = Density), size = 0.5) +
  scale_fill_manual(values = c('transparent', 'gray80', 'gray30')) +
  scale_color_manual(values = c(1, 1, 1)) +
  scale_shape_manual(values = c(1, 21, 21 )) +
  
  labs(y = "Flesh dry weight (g)", x = "Length (mm)" ) +
  theme(legend.position = c(.1,.8),
        legend.title = element_blank(),
        legend.background = element_blank()) +
  scale_y_log10() +
  scale_x_log10(breaks = c(20,30,50, 80)) +
  stat_poly_eq(formula = y ~ log(x),
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse=TRUE,label.x.npc = "right", label.y.npc = "bottom", size = 3, vstep = 0.05)
               

ggsave(
  allo_plots,
  filename = 'figures/figure_3_allometry_plots.tiff',
  compression = 'lzw',
  width = 12,
  height = 4,
  dpi = 900
)
ggsave(
  allo_plots,
  filename = 'figures/figure_3_allometry_plots.svg',
  device = 'svg',
  width = 8,
  height = 4
)

# fit exponential allometric model----------
allo_model <- 
  nls(Flesh_DW ~ a * exp(b *Length) ,
      start = list(a = 0.06, b = 0.04),
      trace = TRUE,
      data = ci)

summary(allo_model)


# fit linear allometric model----------
allo_model_log_T <-
  lm(
    log(Flesh_DW) ~ log(Length) *  Density ,
    data = filter (ci, Type == "Transplanted" &
                     Site == "Shipwreck")
  )

summary(allo_model_log_T)
tidy(anova(allo_model_log_T))

allo_model_log_T1 <-   lm(
  log(Flesh_DW) ~ log(Length) * Density ,
  data = filter (ci, Type == "Transplanted" &
                   Site == "Shipwreck")
)
summary(allo_model_log_T1)
anova(allo_model_log_T1)

allo_model_log_SU <-
  lm(
    log(Flesh_DW) ~ log(Length) * Density,
    data = filter (ci, Type == "Undisturbed" &
                     Site == "Shipwreck")
  )

summary(allo_model_log_SU)
tidy(anova(allo_model_log_SU))

allo_model_log_KU <-
  lm(log(Flesh_DW) ~ log(Length) + Density,
     data = filter (ci, Type == "Undisturbed" & Site == "Kautou"))

summary(allo_model_log_KU)
tidy(anova(allo_model_log_KU))


# 04 Figure 2 - Box-plots of condition index ---------
ci_boxplot <- 
  ggplot(ci, aes(x = Density, y = CI, fill = Density )) +
  geom_boxplot() +
  facet_wrap(Type ~ Site) +
  theme_javier() +
  labs(y = 'Condition index', x = '') +
  scale_fill_manual(values = c('transparent', 'gray80', 'gray30'), guide = F)

ggplot(ci, aes(x = Density, y = CI, fill = Site )) +
  geom_boxplot() +
  theme_javier() + 
  labs(y = 'Condition index', x = '') +
  facet_grid(~Type,  space = 'free', scales = 'free') +
  scale_fill_manual(values = c('transparent', 'gray80', 'gray30')) +
  scale_shape_manual(values = c(1, 21, 21 )) +
  theme(legend.position = c(.1,.85),
        legend.title = element_blank(),
        legend.background = element_blank())



ggsave(ci_boxplot, 
       filename = 'figures/figure_2_ci_boxplots.tiff',
       device = 'tiff',
       compression = 'lzw',
       width = 8,
       height = 4,
       dpi = 600)

ggsave(ci_boxplot, 
       filename = 'figures/figure_2_ci_boxplots.svg',
       width = 8,
       height = 4)


# bar plot CI-----
ggplot(ci, aes(x = Density, y = CI, fill = Site)) +
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
  theme_javier() +
  labs(y = '', x = 'Density') +
  scale_fill_grey(name = 'Site', start = .8, end = 1) +
  facet_wrap(~ Type)

ggsave(
  last_plot(),
  filename = 'figures/bar_plots_ci.tiff',
  device = 'tiff',
  compression = 'lzw',
  dpi = 600,
  width = 6,
  heigh = 3
)

# ANOVA condition index ---------
# undisturbed mussels
und_ci <- filter(ci,Type=="Undisturbed")
m1 <- aov(sqrt(CI)~Site*Density, und_ci)

kable(tidy(m1))
kable(tidy(TukeyHSD(m1)))

# transplanted mussels
tr_ci <- filter(ci,Type!="Undisturbed")
m2 <- aov(sqrt(CI)~Density, tr_ci)
kable(tidy(m2))
kable(tidy(TukeyHSD(m2)))


# table for CI both tests ----
bind_rows(
  'Transplanted' = tidy(m2),
  'Undisturbed' =  tidy(m1),
  .id = 'Test'
) %>%
  write_csv('tables/anova_ci.csv', na = '')


# ANOVA size 
m3 <- aov(sqrt(Length)~Site*Density, ci)
tidy(m3)
kable(tidy(TukeyHSD(m3)))

# Initial mussel size data---------------
size <- 
  read_csv('data/UW-Caliper 0111_Pyura_4_17.CSV') %>% 
  filter(mm>0) 

summary(size$mm)

size %>% 
  group_by(Site) %>% 
  summarise(mean = mean(mm),
            n = n(),
            sd = sd(mm),
            se = sd/sqrt(n))

ci %>% 
  group_by(Site) %>% 
  summarise(mean = mean(Length),
            n = n(),
            sd = sd(Length ),
            se = sd/sqrt(n))

size_freq_dist <- 
  ggplot(size, aes(x = mm,  stat(count),fill = Site)) +
  geom_density(alpha = .3) +
  labs(x = 'Mussel size (mm)', y = 'Number of individuals') + 
  theme_javier() +
  theme(legend.position = c(.9,.7))

print(size_freq_dist)

ggsave(size_freq_dist, 
       filename = 'figures/size_freq_dist.tiff',
       device = 'tiff',
       compression = 'lzw',
       width = 4,
       height = 2.5,
       dpi = 600)
