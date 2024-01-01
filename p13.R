library(tidyverse)

grid <- 
  read_csv(
    "inputs/13.txt", col_names = c("grid_data"), skip_empty_rows = FALSE
  ) %>% 
  mutate(
    is_blank = is.na(grid_data),
    grid_id = consecutive_id(is_blank)
  ) %>% 
  filter(!is_blank) %>% 
  group_by(grid_id) %>% 
  mutate(y = seq_len(n())) %>% 
  ungroup() %>% 
  # select(grid_id, grid_data) %>% 
  mutate(
    char = map(grid_data, str_split_1, pattern = "")
  ) %>% 
  unchop(char) %>% 
  group_by(grid_id, y) %>% 
  mutate(x = seq_len(n())) %>% 
  group_by(grid_id, y) %>% 
  mutate(
    col_syms = list(calc_symmetries(char))
  ) %>% 
  group_by(grid_id, x) %>% 
  mutate(
    row_syms = list(calc_symmetries(char))
  ) %>% 
  ungroup()
  
grid %>% 
  group_by(grid_id) %>% 
  summarise(
    col_syms = list(reduce(col_syms, intersect)),
    row_syms = list(reduce(row_syms, intersect))
  ) %>% 
  mutate(
    type = if_else(
      map_lgl(col_syms, length),
      "col", "row"
    ),
    sym =
      map2_int(
        col_syms,
        row_syms,
        ~{c(.x,.y)}
      ),
    multiple = 
      if_else(type == "col", 1, 100)
  ) %>% 
  summarise(
    sum(multiple * sym)
  )
## 35691
  

full_join(
  grid %>% 
    distinct(grid_id, y, col_syms) %>%  
    group_by(grid_id) %>% 
    summarise(
      col_syms = list(filter_by_count(flatten_int(col_syms), n() - 1))
    ) %>% 
    mutate(
      col_sym_count = map_int(col_syms, length)
    ),
  grid %>% 
    distinct(grid_id, x, row_syms) %>%  
    group_by(grid_id) %>% 
    summarise(
      row_syms = list(filter_by_count(flatten_int(row_syms), n() - 1))
    ) %>% 
    mutate(
      row_sym_count = map_int(row_syms, length)
    ),
  by = "grid_id"
) %>% 
  mutate(
    type = if_else(
      map_lgl(col_syms, length),
      "col", "row"
    ),
    sym =
      map2_int(
        col_syms,
        row_syms,
        ~{c(.x,.y)}
      ),
    multiple = 
      if_else(type == "col", 1, 100)
  ) %>% 
  summarise(
    sum(multiple * sym)
  )


  
filter_by_count <- function(x, nn) {
  
  force(nn)
  (vctrs::vec_count(x) %>% filter(count == nn))$key
  
}

calc_symmetries <- function(x) {
  
  len <- length(x)
  
  flags <- logical(len)
  
  for (i in seq_along(x)[c(-len)]) {
    
    j <- 0L

    repeat {
      is_sym <- x[i-j] == x[i + 1L + j]
      j <- j + 1L
      if (!is_sym || j >= i || i + j + 1L > len)
        break
    }
    if (is_sym)
      flags[i] <- TRUE
    
  }
  
  
  which(flags)
  
}


