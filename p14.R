library(tidyverse)


grid <- read_csv("inputs/14.txt", col_names = "grid_data")
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
    values_to = "rocks",
    names_transform = as.integer
  ) %>% 
  mutate(
    is_o = rocks == "O",
    is_hash = rocks == "#",
    is_dot = rocks == ".",
  ) 


grid_p1 <-
  grid_long %>% 
  group_by(x) %>%
  mutate(
    rolled_rocks = roll_rocks(rocks),
    max_y = n(),
    weight = max_y - y + 1
  ) %>% 
  ungroup()

## 105784
grid_p1 %>% 
  summarise(sum((rolled_rocks == "O") * weight))
  

res <- 
grid_long %>% 
  group_by(x) %>%
  mutate(
    max_y = n(),
    weight = max_y - y + 1
  ) %>% 
  ungroup() %>% 
  roll_rocks_cycles(1000)


check_for_cycles <- function(x) {
  
  len <- length(x)
  x <- rev(x)
  i <- i
  
  while (2*i <= len && !all(x[1:i] == x[i+1:(2*i)])) {
    i <- i + 1L
  }
  
  if (2*i > len)
    return("No cycle")
  else 
    list((len - 2*i), i)
  
  
}


roll_rocks_cycles <- function(grid, cycles = 1, print_sum = TRUE) {
  
  storage <- integer(64)
  
  for (i in seq_len(cycles)) {
    grid <-
      grid %>% 
        group_by(x) %>%
        mutate(
          rocks = roll_rocks(rocks),
        ) %>% 
        group_by(y) %>% 
        mutate(
          rocks = roll_rocks(rocks)
        ) %>% 
        group_by(x) %>%
        mutate(
          rocks = rev(roll_rocks(rev(rocks))),
        ) %>% 
        group_by(y) %>% 
        mutate(
          rocks = rev(roll_rocks(rev(rocks))),
        ) %>% 
        ungroup()
    
    
    if (i > length(storage))
      storage[length(storage)*2] <- NA_integer_
    
    # if (print_sum)
    #   grid %>% 
    #     summarise(weight = sum((rocks == "O") * weight)) %>% 
    #     pull()
    
    storage[i] <-  sum((grid$weight * (grid$rocks == "O")))
    print(storage[i])
    
    if (storage[i] %in% storage[seq_len(i-1)]) {
      message("possible cycle")
    }
    
  }
  
  storage[1:i]
  
}
    
roll_rocks <- function(x) {
  len <- length(x)
  
  i <- 1L
  j <- 2L
  
  while(i <= len) {
  #   print(i)
  # print(j)
    if (x[i] %in% c("#", "O")) {
      i <- i + 1L
      j <- max(i + 1L, j)
      next
    }
    
    while(j <= len && x[j] == ".")
      j <- j + 1L
      
    if (j > len)
      break
    
    if (x[j] == "#") {
      i <- j + 1L
      j <- j + 2L
      next
    }
  
    x[i] <- "O"
    x[j] <- "."
    i <- i + 1L
    j <- j + 1L
    
  }  
 
  x 
}


for (i in seq_len(5)) {
  print(i)
  if (i == 2)
    i <- 4
}
