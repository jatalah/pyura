library(tidyverse)
library(readxl)
library(vegan)
library(ggord)

quad <- read_excel('pyura.xlsx')


factors <- 
  quad %>% 
  transmute(image = `Frame image name`,file = `CPC filename`) %>% 
  distinct() %>% 
  mutate(image = str_sub(image,-12),
         file = str_sub(file,-11, -5),
         site = str_sub(file,1,1),
         density = str_sub(file,3,3),
         treat =  str_sub(file,5,5),
         rep =  str_sub(file,-1))


cover <- 
  read_excel('pyura.xlsx', sheet = "pyura_%cover", skip = 1) %>% 
  select(`Photo Name`, `Cellana_ornata (Cel)`:`Epopella plicata (Epo)`) %>% 
  select_if(~sum(!is.na(.)) > 0) %>% 
  select_if(~ !is.numeric(.) || sum(.) != 0) %>% 
  rename(image = `Photo Name` ) %>% 
  left_join(factors)

glimpse(cover)

log_cover <- log10(cover[,c(2:22)]+1)


mds <- metaMDS(log_cover, distance = 'bray')


mds_plot <- 
  ggord(
  mds,
  grp_in = cover$density,
  poly = F,
  alpha = 1,
  ellipse = F,
  # arrow = .3,
  arrow = 0,
  repel = T,
  text = .01,
  vec_ext = 1
)
mds_plot

# PERMANOVA-----------
permanova <- adonis(log10(cover[,c(2:22)]+1)~site*density,data = cover,method = 'bray',permutations = 9999)
permanova

# SIMPER------------
simp <- simper(log10(cover[,c(2:22)]+1), cover$density, permutations = 999)
# simp <- simper(fauna_dat[,-c(1:4)], fauna_dat$Treatment, permutations = 999)
summary(simp, digits = 2, ordered = T)

# diversity indices----
indices <- 
  cover[,c(2:22)] %>% 
  round(.) %>% 
  transmute(N = rowSums(.),
            H = diversity(.),
            S = specnumber(.),
            J = H/log(S),
            ES = rarefy(., min(N))) %>% 
  bind_cols(factors)

boxplots <- 
  indices %>% 
  gather(index, value, c("S", "J", "H")) %>% 
  mutate(index = fct_relevel(index, c("S", "J"))) %>% 
  ggplot(., aes(x = density, y = value, fill = site)) +
  geom_boxplot(alpha = .8) +
  facet_wrap(~index, scales = 'free')
boxplots



indices %>% 
  gather(index, value, c("S", "J", "H")) %>% 
  mutate(index = fct_relevel(index, c("S", "J"))) %>% 
ggplot(., aes(density, value, fill = site)) +
  stat_summary(
    fun.y = mean,
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
  theme_bw() +
  facet_wrap(.~index, scales = 'free')


# anova indices---------------
indices_dat <-
  indices %>%
  mutate(N = log(N +1)) %>% 
  gather(index, value, c("H", "S", "J")) %>%
  group_by(index) %>%
  nest() %>%  
  mutate(
    lms = map(.x = data, ~ lm(value ~ site*density, data = .x)),
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
