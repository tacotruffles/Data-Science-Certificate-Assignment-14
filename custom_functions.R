library('tidyverse')

split_test_train <- function(df, split_percentage = .70, seed_number = 123) {
  set.seed(seed_number)
  
  df_id = df %>%
    mutate(internal_id_zzz = row_number())
  
  df_train = df_id %>%
    sample_frac(split_percentage)
  
  df_test = df_id %>%
    anti_join(df_train, by='internal_id_zzz') %>%
    select(-internal_id_zzz)
  
  df_train <- df_train %>% 
    select(-internal_id_zzz)
  
  return (list(train=as.data.frame(df_train), test=as.data.frame(df_test)))
}