# load packages
library(tidyverse) # for data manipulation and plotting

theme_set(theme_minimal(base_size = 12))

# import data
maca_sample_select <- read_csv("data/maca_sample_select.csv")
# examine the data
glimpse(maca_sample_select)

# select columns that start with "d29"
maca_sample_select_d_29 <- maca_sample_select |>
  select(starts_with("d29"))

# check result
glimpse(maca_sample_select_d_29)

# extract total column for comparison
maca_sample_select_d_29_total <- maca_sample_select_d_29 |>
  select(contains("total")) |>
  mutate_all(round,2)

# check result
glimpse(maca_sample_select_d_29_total)

# extract columns that are not total columns
maca_sample_select_d_29_comps <- maca_sample_select_d_29 |>
  select(-contains("total"),-contains("savings"))

# check result
glimpse(maca_sample_select_d_29_comps)

# sum the columns that are not total columns
maca_sample_select_d_29_comps_sum <- maca_sample_select_d_29_comps |>
  mutate(my_total = round(rowSums(across(where(is.double)),na.rm = TRUE),2)) 

# check result
glimpse(maca_sample_select_d_29_comps_sum)

# compare the total columns
total_comparisons <- cbind(maca_sample_select_d_29_total, maca_sample_select_d_29_comps_sum |> select(my_total))

names(total_comparisons) <- c("total_expenditure_col","my_total")

# check result
head(total_comparisons)

# check result
glimpse(total_comparisons)

# check for discrepancies
tot_compare <- total_comparisons$total_expenditure_col == total_comparisons$my_total

nrow(total_comparisons) - sum(tot_compare)

total_comparisons[!tot_compare,] |> head()

# plot the comparison
# plot the comparison
total_comparisons |>
  ggplot(aes(x = total_expenditure_col, 
             y = my_total)) +
  geom_line() 

quantile(total_comparisons$my_total, probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))

quantile(total_comparisons$total_expenditure_col, 
         probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))


# Compute percentiles (10%, 20%, ..., 100%)
percentile_values <- quantile(total_comparisons$my_total, probs = c(seq(0.0, 0.9, by = 0.1),0.99,1.0))

# Create a categorical variable based on the percentiles
total_comparisons$total_category <- cut(
  total_comparisons$my_total,
  breaks = percentile_values,
  labels = paste0(c(0,seq(10, 90, by = 10),99), "th Percentile"),
  include.lowest = TRUE,
  right = FALSE,
  ordered_result = TRUE
)

# Check the result
head(total_comparisons)

maca_sample_select_with_total_percentiles <- cbind(maca_sample_select, total_comparisons)
