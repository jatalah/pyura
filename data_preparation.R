library(tidyverse)
library(readxl)
library(vegan)
library(ggord)
library(ggpubr)
library(knitr)

source('theme_javier.R')
theme_set(theme_javier())

# 01 read and prepare data ---------
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
  str_replace(., "_", " ") %>%  
  left_join(cover_data, factors) %>% ##Merge cover data with factors
  mutate(`bare ` = `bare ` + sand) %>% 
  rename(Ulva = Ulva.,
         `Bare space` = `bare `) %>% 
  select(-sand)

write_csv(cover, 'data/cover_data.csv')

