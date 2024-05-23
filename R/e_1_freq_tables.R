# load packages
library(tidyverse) # for data manipulation and plotting

theme_set(theme_minimal(base_size = 12))

# get D29 data
source("R/d_29_sum_check.R")

# import data
maca_sample_select <- read_csv("data/maca_sample_select.csv")
# examine the data
glimpse(maca_sample_select)

# select columns that start with "e1"
maca_sample_select_e_1 <- maca_sample_select |>
  select(starts_with("e1"))

# check result
glimpse(maca_sample_select_e_1)

original_col_names <- names(maca_sample_select_e_1)

extract_col_category <- function(col_name) {
  # extract the category from the column name
  category <- gsub(".*\\milk_", "", col_name)
  return(category)
}

# test function
extract_col_category(original_col_names[1])

new_col_names <- map_vec(original_col_names, extract_col_category)

names(maca_sample_select_e_1) <- new_col_names

# check result
glimpse(maca_sample_select_e_1)

maca_sample_select_e_1_long <- maca_sample_select_e_1 |>
  select(-other_specify_2) |>
  pivot_longer(cols = everything(),
               names_to = "category",
               values_to = "use")

# check result
glimpse(maca_sample_select_e_1_long)

e1_freq_table <- maca_sample_select_e_1_long |>
  mutate(use_01 = ifelse(use == "Yes", 1, 0)) |>
  group_by(category) |>
  summarise(number=sum(use_01, na.rm=TRUE)) |>
  mutate(prop = number / sum(number)) 

e1_freq_table

e1_freq_table |>
  ggplot(aes(x=reorder(category,-number),y=number)) +
  geom_col(fill = 'steelblue') +
  geom_text(aes( label = scales::percent(prop),
                 y= number ), vjust = -.5) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "Fuel category", y = "Number of uses", title = "Usage of cooking sources") 



maca_sample_select_e_1_long_expenditures <- cbind(maca_sample_select_e_1,
                                                  maca_sample_select_with_total_percentiles |> 
                                                    select(total_category)) |>
  select(-other_specify_2) |>
  pivot_longer(cols = -total_category,
               names_to = "category",
               values_to = "use")

glimpse(maca_sample_select_e_1_long_expenditures)

cross_tab_df <- maca_sample_select_e_1_long_expenditures |>
  filter(use == "Yes") |>
  group_by(total_category, category) 

table(cross_tab_df$total_category, cross_tab_df$category)

