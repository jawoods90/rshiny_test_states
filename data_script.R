
# Script to do data work before running shiny app

# 1. Load in packages
library(datasets)

# 2. Create df object 
df_x77 <- as.data.frame(datasets::state.x77)
df_abb <- as.data.frame(datasets::state.abb)

df_states <- cbind(df_abb, df_x77)

df_states <- df_states %>% 
  dplyr::rename(State = 1)


