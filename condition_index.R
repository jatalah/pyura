library(tidyverse)
library(readxl)

ci <- read_excel('data/Length and CI.xlsx')
dotchart(ci$CI)

# remove one outlier
head(arrange(ci, CI))
# sample 32 CI<1

ci <- 
  filter(ci,CI>1) %>% 
  mutate(Site = fct_recode(Site, Koutou = "K", Shipwreck = "S"),
         Density = fct_recode(Density, Control = "C", Low = "L", High = "H"),
         Density = fct_relevel(Density, "Control", "Low"),
         Type = if_else(is.na(Type) & Site=="Koutou", "U", Type),
         Type = if_else(is.na(Type), "E", Type))
  
dotchart(ci$CI)

# plot---------
ggplot(ci, aes(x = Density, y = CI)) +
  geom_boxplot() +
  facet_grid( Type ~ Site) +
  theme_bw()

# useless dynamite plot---
# ggplot(ci, aes(x = Density, y = CI)) +  
#   stat_summary(fun.y = mean, geom = "bar", fill = 'gray80', color = 1, alpha =0) +  
#   stat_summary(fun.data =  "mean_cl_boot", geom = "errorbar", width = 0.2) +
#   facet_grid( Type ~ Site) +
#   theme_bw()

ggplot(ci, aes(x = Density, y = CI)) +  
  stat_summary(fun.data = "mean_cl_boot", colour = 1, size = 1 ) +
  facet_grid( Type ~ Site) +
  theme_bw()

und_ci <- filter(ci,Type=="U")


ggplot(und_ci, aes(x = Density, y = CI)) +  
  stat_summary(fun.data = "mean_cl_boot", colour = 1, size = 1 ) +
  facet_wrap( ~ Site) +
  theme_bw() +
  ylab('Condition index')


ggplot(und_ci, aes(x = Density, y = CI)) +  
  geom_boxplot() +
  facet_wrap( ~ Site) +
  theme_bw() +
  ylab('Condition index')


m1 <- aov(sqrt(CI)~Site*Density, und_ci)
anova(m1)
TukeyHSD(m1)
