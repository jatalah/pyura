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

# summary stats condion index ----------
ci %>% 
  drop_na(CI) %>% 
  group_by(Site,	Density,	Rep, Type) %>% 
  summarise(n = n())


ci %>% 
  drop_na(CI) %>% 
  group_by(Site) %>% 
  summarise(mean = mean(CI),
            n = n(),
            sd = sd(CI),
            se = sd/sqrt(n))


# Boxplot---------
ggplot(ci, aes(x = Density, y = CI)) +
  geom_boxplot() +
  facet_grid( Type ~ Site) +
  theme_javier()

ggplot(ci, aes(x = Density, y = Length)) +
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
tr_ci <- filter(ci,Type!="U")

ggplot(und_ci, aes(x = Density, y = CI)) +  
  stat_summary(fun.data = "mean_cl_boot", colour = 1, size = 1 ) +
  facet_wrap( ~ Site) +
  theme_javier() +
  ylab('Condition index')


ci_plot_und <- 
  ggplot(und_ci, aes(x = Density, y = CI)) +  
  geom_boxplot() +
  facet_wrap( ~ Site) +
  theme_javier() +
  labs(y = 'Condition index', x = '', title = "B. Undisturbed mussels")


ci_plot_tr <- 
  ggplot(tr_ci, aes(x = Density, y = CI)) +  
  geom_boxplot() +
  theme_javier() +
  labs(y = 'Condition index', x = '', title = "A. Transplanted mussels")


ggarrange(ci_plot_und, ci_plot_tr)

ci %>% 
  drop_na(CI) %>% 
ggplot( aes(x = Density, y = CI)) +  
  geom_boxplot() +
  facet_wrap( ~ paste(Site,Type, sep =" ")) +
  theme_javier() +
  labs(y = 'Condition index', x = '')


# anova condition index ---------
m1 <- aov(sqrt(CI)~Site*Density, und_ci)
anova(m1)
TukeyHSD(m1)

m2 <- aov(sqrt(CI)~Density, tr_ci)
anova(m2)
TukeyHSD(m2)

m3 <- aov(sqrt(Length)~Site*Density, ci)
anova(m3)
TukeyHSD(m3)

# Initial mussel size data---------------
size <- 
  read_csv('data/UW-Caliper 0111_Pyura_4_17.CSV') %>% 
  filter(mm>0) 


summary(size$mm)
ggplot(size, aes(x = mm,  stat(count),fill = Site)) +
  geom_density(alpha = .3) +
  labs(x = 'Mussel size (mm)', y = 'Number of individuals')

size %>% 
  group_by(Site) %>% 
  summarise(mean = mean(mm),
            n = n(),
            sd = sd(mm),
            se = sd/sqrt(n))


