library(tidyverse)
library(bit64)

mark_empties <- function(is_empty_row, is_empty_col) {
  
  if (is_empty_row && is_empty_col)
    return(1:4)
  if (is_empty_row || is_empty_col)
    return(1:2)
  
  1
  
}


grid <- read_csv("inputs/11.txt", col_names = "grid_data")
WIDTH <- grid[1,, drop = TRUE] %>% str_length()

grid_long <-
  grid %>% 
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
    values_to = "sym",
    names_transform = as.integer
  ) %>% 
  mutate(
    is_gal = sym == "#",
    is_void = sym == ".",
  ) %>% 
  group_by(x) %>% 
  mutate(
    is_empty_row = all(is_void)
  ) %>% 
  group_by(y) %>% 
  mutate(
    is_empty_col = all(is_void)
  ) %>% 
  ungroup() %>% 
  mutate(
    gal_id = if_else(
      is_gal,
      cumsum(is_gal),
      NA_integer_
    ),
    empty_flags = 
      map2(
        is_empty_row, is_empty_col,
        mark_empties
      )
  ) %>% 
  unchop(empty_flags) %>% 
  group_by(x) %>% 
  mutate(
    new_y = seq_len(n())
  ) %>% 
  group_by(y) %>% 
  mutate(
    new_x = seq_len(n())
  ) %>% 
  ungroup() %>% 
  filter(is_gal) %>% {
  cross_join(
    .,
    select(., gal_id, new_x, new_y, x, y)
  )} %>% 
  filter(gal_id.x < gal_id.y) %>% 
  mutate(
    orig_dist = abs(x.x - x.y) + abs(y.x - y.y),
    dist_p1 = abs(new_x.x - new_x.y) + abs(new_y.x - new_y.y),
    dist_diff = dist - orig_dist,
    ## trick for p2
    dist_p2 = orig_dist + dist_diff * (1e6 - 1)
  )
  


10422930
699909023130
