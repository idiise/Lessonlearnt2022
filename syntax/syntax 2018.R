
# import Package ----------------------------------------------------------

library(tidyverse)
library(fs)
library(haven)
library(here)
library(labelled)
library(writexl)

# import datasets ---------------------------------------------------------

here::here("data/Baseline 2018/")

List_data <- dir_ls(regexp = here::here("data/Baseline 2018/.sav"))
here::set_here("data/Baseline 2018/")
