# library(data.table)
library(tidyverse)

make_adj_template <- function() {
  
  adj_vec <- c(-1, 0, 1)
  
  expand_grid(
    x_delta = adj_vec,
    y_delta = adj_vec,
  )
  
}
make_grid_long_adj <- function(grid_template, adj_template) {
  
  grid_template %>% 
    cross_join(
      adj_template
    ) %>% 
    mutate(
      x = x + x_delta,
      y = y + y_delta,
    ) %>% 
    select(-x_delta, -y_delta)# %>% 
    # distinct(x, y) %>% 
}


grid_raw <- read_csv("inputs/3.txt", col_names = "grid_data")
WIDTH <- grid_raw[1,, drop = TRUE] %>% str_length()


adj_template <- make_adj_template()

grid_long <-
  grid_raw %>% 
  mutate(
    y = seq_len(n()), 
    .before = grid_data
  ) %>% 
  separate_wider_position(
    cols = grid_data,
    widths = setNames(rep(1, WIDTH), seq_len(WIDTH))
  ) %>% 
  pivot_longer(
    cols = -y,
    names_to = "x",
    values_to = "val",
    names_transform = as.integer
  ) %>% 
  mutate(
    is_digit = str_detect(val, "\\d"),
    is_sym = str_detect(val, "[^.\\d]"),
    is_star = val == "*",
  )


grid_long_digit <- 
  grid_long %>% 
  mutate(
    number_id = consecutive_id(y, is_digit)
  ) %>% 
  filter(is_digit) %>% 
  group_by(number_id) %>% 
  mutate(
    number = paste0(val, collapse = "") %>% as.integer()
  ) %>% 
  ungroup()

grid_long_sym <- 
  grid_long %>% 
  filter(is_sym) %>% 
  mutate(
    sym_id = seq_len(n())
  )

grid_long_star <- 
  grid_long %>% 
  filter(is_star) %>% 
  mutate(
    star_id = seq_len(n())
  )
  
  
  
#### part 1 ####.

grid_long_sym_adj <-
  grid_long_sym %>% 
  make_grid_long_adj(
    adj_template
  )

grid_long_digit %>% 
  semi_join(
    grid_long_sym_adj,
    by = c("x", "y")
  ) %>% 
  distinct(
    number_id, number
  ) %>% 
  summarise(sum(number))


#### part 2 ####.

grid_long_star_adj <-
  grid_long_star %>% 
  make_grid_long_adj(
    adj_template
  )
  
grid_long_star_adj %>% 
  inner_join(
    grid_long_digit,
    by = c("x", "y"),
    relationship = "one-to-one"
  ) %>% 
  group_by(star_id) %>% 
  distinct(
    star_id, number
  ) %>% 
  filter(n() == 2) %>% 
  summarise(
    gear_ratio = number[1] * number[2]
  ) %>% 
  ungroup() %>% 
  summarise(sum(gear_ratio))
  
  




