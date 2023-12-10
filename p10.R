library(tidyverse)
library(data.table)

make_adj_template <- function() {
  
  tribble(
    ~sym, ~x1_d, ~y1_d, ~x2_d, ~y2_d,
    "|",  0,  1,   0, -1,
    "-",  1,  0,  -1,  0,
    "L",  0, -1,   1,  0,
    "F",  1,  0,   0,  1,
    "7",  0,  1,  -1,  0,
    "J",  0, -1,  -1,  0,
  )

}
walk_the_pipe <- function(grid_long) {
  
  get_unwalked_neighbours <- function(ids_, grid_long) {
    id1_ <- grid_long[ids_, id1]
    id1_ <- grid_long[id1_][is.na(dist), id]
    id2_ <- grid_long[ids_, id2]
    id2_ <- grid_long[id2_][is.na(dist), id]
    c(id1_, id2_) %>% unique()  
  }  
  
  grid_long[, dist := NA_integer_]
  unwalked_places <- grid_long[is_S == TRUE, id]
  dist_ <- 0L
 
  while (TRUE) {
    # print(unwalked_places)
    # Sys.sleep(1)
    grid_long[unwalked_places, dist := dist_]
    unwalked_places <- get_unwalked_neighbours(unwalked_places, grid_long)
    
    if (!length(unwalked_places))
      break
    
    dist_ <- dist_ + 1L
  }
 
  dist_ 

}
mark_regions <- function(syms) {
  
  state <- 0L
  marker <- -1L
  region <- integer(length(syms))
  
  for (i in seq_along(syms)) {
    
    pipe <- TRUE
    if (syms[i] == "|")
      state <- 2L
    else if (syms[i] == "-")
      ## do nothing
      TRUE
    else if (syms[i] == "F")
      state <- state + 1L
    else if (syms[i] == "J")
      state <- state + 1L
    else if (syms[i] == "7")
      state <- state - 1L
    else if (syms[i] == "L")
      state <- state - 1L
    else
      ## do nothing
      TRUE
    
    
    pipe <- FALSE
    
    if (state %in% c(-2L, 2L)) {
      marker <- -1L * marker
      state <- 0L
    }
    
    if (!pipe)
      region[i] <- marker
    
  }

  region
  
}

grid <- read_csv("inputs/10.txt", col_names = "grid_data")
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
    is_S = sym == "S",
    ## manual hack to save headaches!
    sym = if_else(is_S, "F", sym)
  ) %>% 
  left_join(
    make_adj_template(),
    by = "sym",
    relationship = "many-to-one"
  ) %>% 
  mutate(
    x1 = x + x1_d,
    y1 = y + y1_d,
    x2 = x + x2_d,
    y2 = y + y2_d,
  ) %>% 
  as.data.table()


grid_long[, id := seq_len(.N)]
grid_long[
  grid_long,
  on = .(x1 = x, y1 = y),
  id1 := i.id
]
grid_long[
  grid_long,
  on = .(x2 = x, y2 = y),
  id2 := i.id
]


#### part 1 ####.

## 6931
walk_the_pipe(grid_long)


#### part 2 ####.

## replace pipe parts that are not on the main pipe
grid_long[
  is.na(dist), ## determined in part 1
  sym := "."
]

grid_long[,
  region := mark_regions(sym),
  by = "y"
]

## 357
grid_long %>% count(region)











