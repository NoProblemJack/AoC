library(tidyverse)
library(data.table)


grid <- read_csv("inputs/17.txt", col_names = "grid_data", col_types = "c")
WIDTH <- grid[1,, drop = TRUE] %>% str_length()
LENGTH <- nrow(grid)



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
    values_to = "score",
    names_transform = as.integer,
    values_transform = as.integer
  )

grid_verbose <-
  grid_long %>% 
  cross_join(
    make_from_moves_tbl(n = 10)
  ) %>% 
  bind_rows(
    slice(., 1) %>% 
      mutate(dx_hist = 0L, dy_hist = 0L),
    .
  ) %>% 
  mutate(
    hist_x = x - dx_hist,
    hist_y = y - dy_hist
  ) %>% 
  filter(
    hist_x >= 1L & hist_x <= WIDTH &
    hist_y >= 1L & hist_y <= LENGTH
  ) %>% 
  mutate(
    state_id = seq_len(n())
  ) %>% 
  cross_join(
    make_next_moves_tbl()
  ) %>% 
  mutate(
    next_x = x + to_dx,
    next_y = y + to_dy,
    # next_hist_x = hist_x - to_dx,
    # next_hist_y = hist_y - to_dy,
  ) %>% 
  filter(
    next_x >= 1L & next_x <= WIDTH &
    next_y >= 1L & next_y <= LENGTH,
    abs(hist_x - next_x) %in% 0:10,
    abs(hist_y - next_y) %in% 0:10,
    sign(to_dx) != -sign(dx_hist) | to_dx == 0,
    sign(to_dy) != -sign(dy_hist) | to_dy == 0,
    
    case_when(
      dx_hist %in% 1:3 & to_dx != 1 ~ FALSE,
      dx_hist %in% -(1:3) & to_dx != -1 ~ FALSE,
      dy_hist %in% 1:3 & to_dy != 1 ~ FALSE,
      dy_hist %in% -(1:3) & to_dy != -1 ~ FALSE,
      TRUE ~ TRUE
    )
    # abs(next_hist_x) <= 3,
    # abs(next_hist_y) <= 3,
  ) %>%
  mutate(
    next_hist_x = if_else(
      to_dx == 0,
      x,
      hist_x
    ),
    next_hist_y = if_else(
      to_dy == 0,
      y,
      hist_y
    )
  ) %>% 
  left_join(
    select(., x, y, hist_x, hist_y, next_state_id = state_id) %>% distinct(),
    by = c("next_x"="x", "next_y"="y", "next_hist_x"="hist_x", "next_hist_y" ="hist_y"),
    relationship = "many-to-many"
  ) %>% 
  mutate(
    next_state_id = if_else(x == WIDTH & y == LENGTH, 0L, next_state_id)
  ) #%>% 
  # filter(!is.na(next_state_id))

# new_state_id_mappings <-
#   grid_verbose %>% 
#   select(
#     state_id, next_state_id
#   ) %>% 
#   mutate(
#     new_state_id = frank(state_id, ties.method = "dense")
#   ) %>% 
#   left_join(
#     select(., 
  
  
grid_prepped <-
  grid_verbose %>% 
  select(state_id, score, next_state_id) %>% 
  chop(next_state_id) %>% 
  arrange(state_id)

make_from_moves_tbl <- function(n) {
  
  bind_rows(
    tibble(
      dx_hist = seq_len(n),
      dy_hist = 0,
    ),
    tibble(
      dx_hist = -seq_len(n),
      dy_hist = 0,
    ),
    tibble(
      dx_hist = 0,
      dy_hist = -seq_len(n),
    ),
    tibble(
      dx_hist = 0,
      dy_hist = seq_len(n),
    )
  ) %>% 
    mutate_all(as.integer)
  
}
make_next_moves_tbl <- function() {
  
  tribble(
    ~to_dx, ~to_dy,
    -1,  0,
    1,  0,
    0, -1,
    0,  1,
  ) %>% 
    mutate_all(as.integer)
  
}

## 1076
find_path(1L, grid_prepped$next_state_id, grid_prepped$score)

find_path2(1L, grid_prepped)



find_path <- function(state, moves, scores) {
  
  ## vector of moves
  moves ## list of next states
  scores
  
  state
  total_score <- 0L
  visited <- logical(length(moves))
  total_score_tracker <- integer(length(moves))
  state_cache <- integer()
  score_cache <- integer()
  count <- 0L
  ## double brackets
  ## scores used twice
  while (TRUE) {
   # browser()
    # if (state == 20)
    #   browser()
    
    
    if (visited[state])
      next_states <- integer()
    else {
      next_states <- moves[state][[1]]
      visited[state] <- 1L
      total_score_tracker[state] <- total_score
    }
    
    if (identical(next_states, 0L))
      ## victory
      break
    next_scores <- scores[next_states]  
    
    state_cache <- c(state_cache, next_states)
    score_cache <- c(score_cache, next_scores + total_score)
    
    pos <- which.min(score_cache)
    state <- state_cache[pos]
    total_score <- score_cache[pos]
    
    state_cache <- state_cache[-pos]
    score_cache <- score_cache[-pos]
    
    count <- count + 1L
    if (count %% 1000 == 0)
      print(state)
     
  }
  
   
  list(
    visited,
    total_score
  )
  
}


find_path2 <- function(state, state_dtb) {
  
  
  
  states_ <- state
  total_scores_ <- integer(length(states_))
  
  
  ## state_id , next_state_id, score
  state_dtb <-
    as.data.table(state_dtb)[,
      `:=`(
        visited = FALSE,
        total_score = 999999999L
      )
    ]
   best_score_ <- 999999999L
  
  ## vector of moves
  # moves ## list of next states
  # scores
  # 
  # state
  # total_score <- 0L
  # visited <- logical(length(moves))
  # total_score_tracker <- integer(length(moves))
  # state_cache <- integer()
  # score_cache <- integer()
  count <- 0L
  ## double brackets
  ## scores used twice
  while (TRUE) {
    # if (state == 20)
    #   browser()
    
    # browser()
    # total_scores_ <- total_scores_[!state_dtb[states_]$visited]
    # states_ <- state_dtb[states_][visited == FALSE, state_id]
    # if (length(states_) == 320)
    #   browser()

    states_ <- state_dtb[state_id %in% states_, new_total_score := total_scores_][state_id %in% states_][new_total_score < total_score, state_id]
    total_scores_ <- state_dtb[state_id %in% states_, new_total_score]
    state_dtb[state_id %in% states_, total_score := new_total_score]
    
    
    ## leads to error??
    if (!length(states_))
      stop("ran out of states!")
    
    
    state_dtb[state_id %in% states_, total_score := total_scores_]
    state_dtb[state_id %in% states_, visited := TRUE]
    
    
    next_states <- 
      state_dtb[state_id %in% states_, .(state_id, total_score, next_state_id)] %>% 
      as_tibble() %>% 
      unchop(next_state_id) %>% 
      filter(!is.na(next_state_id)) %>% 
      as.data.table()
    
    if (0L %in% next_states$next_state_id) {
      best_score_ <- min(best_score_, next_states[next_state_id == 0L, total_score])
      next_states <- next_states[next_state_id != 0L]
      # message("ts: ", total_score_)
      # if (nrow(next_states[total_score_ > total_score]) == 0)
      #   break
    }

    if (nrow(next_states[best_score_ > total_score]) == 0)
      ## victory
      break
    
    next_states <-
      next_states[
        state_dtb,
        on = .(next_state_id = state_id),
        total_score := total_score + i.score
      ] %>% 
      setkey(next_state_id, total_score)
    
    next_states <-  
      next_states[,
        .(total_score = min(total_score)),
        by = "next_state_id"
      ] %>% 
      setkey(next_state_id)
    
    states_ <- next_states$next_state_id
    total_scores_ <- next_states$total_score
    
    

    count <- count + 1L
    if (count %% 10 == 0) {
      print(state_dtb %>% head())
      print(length(states_))
    }
  }
  
   
    best_score_
  # list(
  #   visited,
  #   total_score
  # )
  
}
