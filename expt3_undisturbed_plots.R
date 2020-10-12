library(readxl)
library(broom.mixed)
library(tidyverse)
library(lmerTest)
library(lsmeans)
source('theme_javier.R')

# Read data-------------
und <- 
  read_excel('data/S_Undisturbed.xlsx',3,skip = 1) %>% 
  select(`Photo Name`,`Pyura_doppel (Pyu)`, `Perna (PER)`, `detri (Det)`) %>% 
  rename (ID = `Photo Name`, Pyura = `Pyura_doppel (Pyu)`, Perna = `Perna (PER)`, Other = `detri (Det)` ) %>% 
  mutate(Treat = str_sub(ID,start = 2,end = 2),
         Time = str_sub(ID, 6,6),
         Date = fct_recode(Time, `May-17` = "1", `Nov-17` = "2", `Nov-18` = "3"),
         Treat = fct_recode(Treat, Control = "O", Low = "L", High = "H")) %>%
  mutate(Plot = sub("_.*", "", ID))

glimpse(und)  


# create long data ----------
  und %>%
  filter(Time == 1) %>% 
  group_by(Treat) %>% 
  summarise_at(vars(Pyura), mean)
# create long data ----------
und_long <- 
  und %>%
  gather(key, value, Pyura:Other) 

write_csv(und_long, 'data/und_long.csv')
  
# mean +/- se plot ----- 
und_long %>%
  filter(key != "Other") %>%
  ggplot(aes(
    x = Date,
    y = value,
    shape = fct_rev(key),
    group = fct_rev(key),
    fill = fct_rev(key)
  )) +
  facet_wrap( ~ fct_rev(Treat)) +
  stat_summary(fun = mean, geom = 'line', lty = 3) +
  stat_summary(fun.data = "mean_se",
               size = .7,
               position = position_dodge(width = .1)) +
  theme_javier(base_size = 12) +
  labs(y = 'cover (%)') +
  theme(axis.title.x = element_blank()) +
  scale_shape_manual(values = c(21, 21)) +
  scale_fill_manual(values = c(1, 'white')) +
  theme(legend.title = element_blank(),
        legend.text = element_text(face = "italic"))
  


ggsave(
  last_plot(),
  filename = 'figures/figure_6_undisturbed_plots.tiff',
  width = 10,
  compression = 'lzw',
  dpi = 600,
  height = 3
)
ggsave(
  last_plot(),
  filename = 'figures/figure_6_undisturbed_plots.svg',
  device = 'svg',
  width = 10,
  height = 3
)


# Summary tables of the plot-------
und_long %>%
  group_by(key, Date, Treat) %>%
  summarise_at(vars(value), .funs = c(
    mean = mean,
    sd = sd,
    n = ~n(),
    se = ~ sd / sqrt(n())
  ))

und_long %>%
  group_by(key, Treat) %>%
  summarise_at(vars(value), .funs = c(
    mean = mean,
    sd = sd,
    n = ~n(),
    se = ~ sd / sqrt(n())
  ))

# ANOVA 
anovas <-
  und_long %>%
  group_by(key) %>%
  nest() %>%
  mutate(
    lms = map(.x = data, ~ lmer(value ~ Date * Treat + (1|Plot), data = .x)),
    # reduced_lms = map(.x = lms, ~step(.x, reduce.random=FALSE)),
    anova_tab = map(lms, anova),
    anova_table = map(anova_tab, broom::tidy),
    residuals = map(lms, broom::augment),
    posthoc_date = map(lms, ~emmeans(.x, list(pairwise ~ Date), adjust = "none")),
    posthoc_treat = map(lms, ~emmeans(.x, list(pairwise ~ Treat), adjust = "none"))
    # posthoc_tables = map(posthoc, tidy)
  )

# LMMs tables ----------
anova_tables <-
  anovas %>%
  select(key, anova_table) %>%
  unnest(cols = c(anova_table)) %>%
  mutate(term = fct_recode(
    term,
    Treatment  = "Treat",
    `Date x Treatment` = "Date:Treat"
  )) %>% 
  mutate_if(is.numeric, ~signif(., 2)) %>% 
  print %>% 
  write_csv('outputs/rm_anova_undistrubed.csv', na = "")

knitr::kable(anova_tables,digits = 2, caption = 'RM - ANOVAs for Perna, Pyura and Others')


m1 <- lmer(Pyura~ Date * Treat + (1 |Plot), data = und)
anova(m1)
ranova(m1)
step(m1, reduce.random=FALSE)
emmeans(m1, list(pairwise ~ Date), adjust = "none")

# posthoc tables ---------

anovas$posthoc_date[[3]]$`pairwise differences of Date`
anovas$posthoc_treat[[2]]$`pairwise differences of Treat`


# 08 LMM validation------
res <- 
  anovas %>% 
  select(key, residuals) %>% 
  unnest(residuals)

# Plot of fitted vs. residual values by index to check the assumptions heteroscedasticity or any other pattern,
ggplot(res) +
  geom_point(aes(x = .fitted, y = .resid), alpha = .3) +
  facet_wrap( ~ key, scale = 'free') +
  geom_hline(yintercept = 0,
             lty = 2,
             col = 2) +
  theme_bw()

# fitted vs obs
ggplot(res, aes(x = .fitted, y = value)) +
  geom_point( alpha = .3) +
  geom_smooth(method = 'lm') +
  facet_wrap( ~ key, scale = 'free') +
  theme_bw() +
  geom_abline(slope = 1,intercept = 0)

# boxplot by density 
ggplot(res) +
  geom_boxplot(aes(x = Date, y = .resid), alpha = .3) +
  facet_wrap( ~ key, scale = 'free') +
  geom_hline(yintercept = 0,
             lty = 2,
             col = 2) +
  theme_javier()

# qqplot of the normalised residuals to check the assumption of normality
ggplot(res) +
  stat_qq(aes(sample = .wtres ), alpha = .3) +
  facet_wrap( ~ key, scale = 'free') +
  # geom_abline(
  #   intercept =  0,
  #   slope = 1,
  #   lty = 2,
  #   col = 2
  # ) +
  theme_bw()



