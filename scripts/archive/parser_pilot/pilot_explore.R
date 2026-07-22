# ---- set up ----
  ## ---- packages ----
library(pacman)

p_load(arrow, tidyverse, tidycensus, stringr, janitor)

  ## ---- load data ----
cleveland_cf <- read_parquet("data/pilot_cleveland_CF.parquet")
ok_smoke <- read_parquet("data/smoke_oklahoma_CF.parquet")
