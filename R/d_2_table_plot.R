# load packages
library(tidyverse) # for data manipulation and plotting

theme_set(theme_minimal(base_size = 12))

maca_sample_select <- read_csv("data/maca_sample_select.csv")

table(maca_sample_select$d2_which_social_group_do_you_belong_to)

D2_freq_table <- maca_sample_select |>
  mutate(d2_simplified = 
           fct_recode(d2_which_social_group_do_you_belong_to, 
                      unknown = "Don't know/Not sure",
                      general = "General/other/Forward Caste",
                      none = "None of the above/Don't belong to a social group",
                      OBC = "OBC",
                      other = "Other (specify)",
                      refused = "Refused",
                      SC = "SC",
                      ST = "ST")) |>
  group_by(d2_simplified) |>
  summarise(n = n()) |>
  mutate(prop = n / sum(n))

D2_freq_table

D2_freq_table |>
  ggplot(aes(x=reorder(d2_simplified,-n),y=n)) +
  geom_col(fill = 'steelblue') +
  geom_text(aes( label = scales::percent(prop),
                 y= n ), vjust = -.5) +
  labs(x = "Social group", y = "Number of respondents", title = "Social group of respondents") 


