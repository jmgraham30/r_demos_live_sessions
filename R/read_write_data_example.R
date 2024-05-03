# load packages
library(tidyverse)
library(readxl)

# read in csv file
csv_df <- read_csv("data/simulated_data.csv")
excel_df <- read_xlsx("data/diamonds_excel.xlsx",sheet = "diamonds")

# examine read-in data
glimpse(csv_df)
glimpse(excel_df)
