library(data.table)
library(tidyverse)


grid_raw <- read_csv("inputs/3.txt", col_names = "grid_data")

WIDTH <- grid_raw[1,, drop = TRUE] %>% str_length()

make_adj_template <- function() {
  
  adj_vec <- c(-1, 0, 1)
  
  expand_grid(
    x_delta = adj_vec,
    y_delta = adj_vec,
  )
  
}
calc_sym_adj <- function(sym_template, adj_template) {
  
  sym_template %>% 
    cross_join(
      adj_template
    ) %>% 
    mutate(
      x = x + x_delta,
      y = y + y_delta,
    ) %>% 
    distinct(x, y) %>% 
    mutate(
      is_sym_adj = TRUE
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
update_sym_adj <- function(grid_long, adj_template) {
  
  sym_adj <- 
    calc_sym_adj(
      grid_long %>% filter(is_sym),
      adj_template
    )
  
  grid_long %>% 
    mutate(
      is_sym_adj = FALSE,
    ) %>% 
  rows_update(
    sym_adj %>% 
      select(x, y, is_sym_adj),
    by = c("x", "y"),
    unmatched = "ignore"
  )
  
}

## 543867

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

# %>% 
#   mutate(
#     is_sym_adj = FALSE
#   ) %>% 
#   update_sym_adj(
#     adj_template
#   ) %>% 
#   mutate(
#     number_id = consecutive_id(y, is_digit)
#   ) %>% 
#   filter(is_digit) %>% 
#   group_by(number_id) %>% 
#   filter(any(is_sym_adj)) %>% 
#   summarise(
#     number = paste0(val, collapse = "") %>% as.integer()
#   ) %>% 
#   summarise(
#     sum(number)
#   )



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
  filter(is_sym) 
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
  
  




grid_long0 <-
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
    values_to = "val"
  ) %>% 
  mutate(
    is_digit = str_detect(val, "\\d"),
    is_sym = str_detect(val, "[^.]") & !is_digit
  ) %>% 
  calc_grid_adj(WIDTH)

calc_grid_adj <- function(grid_long, width) {
  
  length <- nrow(grid_long)
  
  grid_long %>% 
    mutate(
      is_sym_adj = 
        (shift(is_sym, n = 1, fill = FALSE) & x != 1) |   
        (shift(is_sym, n = -1, fill = FALSE) & x != !!width) |
        
        (shift(is_sym, n = !!width + 1, fill = FALSE) & x != 1 & y != 1) |  
        (shift(is_sym, n = !!width, fill = FALSE) & y != 1) |  
        (shift(is_sym, n = !!width - 1, fill = FALSE) & x != 1 & y != 1) |  
        
        (shift(is_sym, n = -!!width + 1, fill = FALSE) & x != !!width & y != !!length) |  
        (shift(is_sym, n = -!!width, fill = FALSE) & x != !!width) |  
        (shift(is_sym, n = -!!width - 1, fill = FALSE) & x != !!width & y != !!length)
    )
  
}

