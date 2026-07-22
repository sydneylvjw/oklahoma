# ---- setup ----
## ---- packages ----
library(pacman)
p_load(dplyr, arrow, stringr, janitor)

comanche <- read_parquet("data/oscn_comanche_CT31.parquet")
