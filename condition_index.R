library(tidyverse)
library(readxl)

ci <- read_excel('CI_index_mussels.xlsx')
names(ci)
dotchart(ci$CI)

# two outlier
head(arrange(ci, desc(CI)))
# sample 42 and 96 >1,5000

ci <- 
  filter(ci,CI<1000 & CI>3) %>% 
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
ggplot(ci, aes(x = Density, y = CI)) +  
  stat_summary(fun.y = mean, geom = "bar", fill = 'gray80', color = 1, alpha =0) +  
  stat_summary(fun.data =  "mean_cl_boot", geom = "errorbar", width = 0.2) +
  facet_grid( Type ~ Site) +
  theme_bw()

ggplot(ci, aes(x = Density, y = CI)) +  
  stat_summary(fun.data = "mean_cl_boot", colour = 1, size = 1 ) +
  facet_grid( Type ~ Site) +
  theme_bw()

und_ci <- filter(ci,Type=="U")


ggplot(filter(ci,Type=="U"), aes(x = Density, y = CI)) +  
  stat_summary(fun.data = "mean_cl_boot", colour = 1, size = 1 ) +
  facet_wrap( ~ Site) +
  theme_bw() +
  ylab('Condition index')

m1 <- aov(CI~Site*Density, und_ci)
summary(m1)
plot(m1)

