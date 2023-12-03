library(tidyverse)

data <-
  read_delim("inputs/2.txt", delim = ":", col_names = c("game", "data_string")) %>%
  mutate(
    game = str_extract(game, "\\d+") %>% as.integer()
  ) %>%  
  separate_longer_delim(
    data_string, ";"
  ) %>% 
  group_by(game) %>% 
  mutate(obs = seq_len(n())) %>% 
  ungroup() %>% 
  separate_longer_delim(
    data_string, ","
  ) %>% 
  mutate(
    val = str_extract(data_string, "\\d+") %>% as.integer(),
    colour = str_extract(data_string, "[A-Za-z]+")
  ) %>% 
  pivot_wider(
    id_cols = c(game, obs),
    names_from = colour,
    values_from = val,
    values_fill = 0
  )
  
data %>% 
  mutate(
    is_valid = !(red > 12 | green > 13 | blue > 14)
  ) %>%
  group_by(game) %>% 
  filter(all(is_valid)) %>% 
  ungroup() %>% 
  distinct(game) %>% 
  summarise(sum(game))

data %>% 
  group_by(game) %>% 
  summarise_if(is.numeric, max) %>% 
  ungroup() %>% 
  mutate(power = red*blue*green) %>% 
  summarise(sum(power))
