
# Script to do data work before running shiny app

# 1. Load in packages
library(datasets)

# 2. Create df object 
df_x77 <- as.data.frame(datasets::state.x77)
df_abb <- as.data.frame(datasets::state.abb)
df_name <- as.data.frame(datasets::state.name)

df_states <- cbind(df_name, df_abb, df_x77)

df_states <- df_states %>% 
  dplyr::rename(State = 1) %>%
  dplyr::rename(StateAbb = 2) %>%
  dplyr::mutate(PopShare = 100*(Population / sum(Population))) %>%
  dplyr::mutate(RankPop = dense_rank(desc(Population))) %>%
  dplyr::mutate(RankArea = dense_rank(desc(Area))) %>%
  dplyr::mutate(RankIncome = dense_rank(desc(Income)))


