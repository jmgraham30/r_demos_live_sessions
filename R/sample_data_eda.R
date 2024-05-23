# load packages
library(tidyverse) # for data manipulation and plotting
library(readxl) # for reading excel files

# load data
list.files("./data")
maca_sample <- read_xlsx("data/MacA_sample data for R training.xlsx") |>
  janitor::clean_names()

glimpse(maca_sample)

skimr::skim(maca_sample)

maca_sample_select <- maca_sample |>
  select(starts_with(c("a9","b2","b3d","d2","d29",
                       "e1","f7","f9","f10","f11",
                       "g1","g2","g4","g5","g7","g8","g9","g10"))) 

glimpse(maca_sample_select)

write_csv(maca_sample_select, "data/maca_sample_select.csv")
