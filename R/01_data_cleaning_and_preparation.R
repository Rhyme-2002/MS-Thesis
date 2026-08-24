# ============================================================
# Data Cleaning and Preparation
# Study: Causal Effect of Higher Education on ANC Utilization
# Dataset: BDHS 2022
# ============================================================

# necessary packages
library(haven)
library(tidyverse)

# read  the dataset
KR_data1<- read_dta("data/raw/BDKR81FL.DTA")

# recode variable
KR_data <- KR_data1 %>% 
  mutate(ANC = factor(if_else(m14 >= 4, 1, 0)),
         mother_age_birth = floor(v012 - (v008 - b3) / 12),
         mother_age_birth_cat = case_when(
           mother_age_birth <= 19 ~ "-19",
           mother_age_birth >= 20 & mother_age_birth <= 34 ~ "20_34",
           mother_age_birth >= 35 ~ "35+"),
         birth_order_cat = case_when(
           bord == 1 ~ "1st",
           bord %in% c(2, 3) ~ "2-3",
           bord > 3 ~ "3+"),
         wealth_index = factor(v190),
         residence = factor(v025),
         employment = factor(v714),
         education = factor(if_else(v106 <= 2, 0, 1)),
         Partners_education = factor(v107),
         women_empower = factor(
           if_else(v743a %in% c(1, 2) & v743b %in% c(1, 2) &  v743d %in% c(1, 2), "High",
                   if_else(v743a %in% c(1, 2) | v743b %in% c(1, 2) | v743d %in% c(1, 2), "Moderate", "Low"))),
         religion = factor(v130))

# select only necessary variable
KR_data <- KR_data %>% select(
  education,
  ANC,
  mother_age_birth_cat,
  birth_order_cat,
  wealth_index,
  residence,
  employment,
  Partners_education,
  women_empower,
  religion)

# removw row where missing value exist
KR_data <- na.omit(KR_data)

# save the processed dataset
write.csv(x = KR_data, file = "data/processed/ANC_data.csv", row.names = FALSE)
