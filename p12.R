library(tidyverse)
library(zeallot)
library(bit64)




grid <- 
  read_delim(
    "inputs/12.txt", delim = " ", 
    col_names = c("section_str", "spring_groups"), col_types = c("cc")
  ) %>% 
  mutate(
    spring_groups = 
      map(spring_groups, ~{str_split_1(.x, pattern = ",") %>% as.integer()}),
    section_str2 =
      map_chr(section_str, ~{strrep(paste0(.x, "?"), 5) %>% str_sub(1,-2)}),
    spring_groups2 =
      map(spring_groups, ~rep(.x,5))
  )

grid1 <-
  grid %>% 
  mutate(c = map2(spring_groups, section_str, traverse_all_sections) %>% list_flatten) %>%
  mutate(ans = map_int(c, ~pluck(.x, "combos"))) %>% 
  as.data.table()
## 7753

grid2 <-
  grid %>% 
  # head(3) %>% 
  mutate(c = map2(spring_groups2, section_str2, traverse_all_sections) %>% list_flatten) %>%
  mutate(ans = map_int(c, ~pluck(.x, "combos"))) %>% 
  # mutate(ans = map(c, ~pluck(.x, "combos")) %>% unlist()) %>% 
  as.data.table()



# grid

# traverse_all_sections(c(1,3,1,6), "?#?#?#?#?#?#?#?")



get_next_section <- function(idx, section_str, with_size = TRUE) {
  
  if (idx == 0)
    initial_type <- "START"
  else  
    initial_type <- section_str[idx]
    
  # return(list(1, section_str[1]), 0)
  # }
  
  len <- length(section_str)
  
  i <- idx + 1
  
  while (i <= len && section_str[i] == initial_type)
    i <- i + 1
  
  if (i > len)
    return(list(i, "END", 0))
  
  if (with_size)
    c(next_i, ., .) %<-% get_next_section(i, section_str, FALSE)
  else
    next_i <- NA_integer_
  
  
  list(
    i, section_str[i], next_i - i
  )
  
}

traverse_all_sections <- function(spring_groups, section_str) {
  
  sections <- str_split_1(section_str, pattern = "")
  sect_pos <- 0
  type <- "START"
  state_ls <- list()
  state_ls <- update_state_ls(state_ls, make_state(0,0,T,T,1))
  
  while (type != "END") {
    
    next_state_ls <- list()
    c(sect_pos, type, size) %<-% get_next_section(sect_pos, sections)
    # browser()
    if (type == ".")
      for (i in state_ls)
        next_state_ls <- handle_dots(i, next_state_ls, spring_groups)
    else if (type == "?")
      for (i in state_ls)
        next_state_ls <- handle_qms(i, next_state_ls, size, spring_groups)
    else if (type == "#")
      for (i in state_ls)
        next_state_ls <- handle_hashes(i, next_state_ls, size, spring_groups)
    else if (type == "END")
      for (i in state_ls)
        next_state_ls <- handle_end(i, next_state_ls, spring_groups)

    state_ls <- next_state_ls   
  
  }
  
  message("finished: ", state_ls[[1]]$combos, ": ", Sys.time())
  
  # state_ls[[1]]$combos <- as.integer64(state_ls[[1]]$combos)
  
  state_ls
  
}


update_state_ls <- function(state_ls, state) {
  
  name <- make_state_name(state)
  
  if (!is.null(state_ls[[name]]))
    state_ls[[name]]$combos <- state_ls[[name]]$combos + state$combos
  else 
    state_ls[[name]] <- state
  
  state_ls
  
}

handle_dots <- function(state, state_ls, spring_groups) {
  
  if (!state$group_is_finished)
    return(state_ls)
    
  if (!state$ends_with_gap)
    state$ends_with_gap <- TRUE
  
  new_state <-
    make_state(
      idx1 = state$idx1,
      idx2 = state$idx2,
      group_is_finished = TRUE,
      ends_with_gap = TRUE,
      combos = state$combos
    )
  
  state_ls <- update_state_ls(state_ls, new_state)
  
  state_ls
}
handle_end <- function(state, state_ls, spring_groups) {
  
  if (!state$group_is_finished)
    return(state_ls)
    
  if (state$idx1 < length(spring_groups))
    return(state_ls)
  
  ## not a relavent value, but need to standardise
  state$ends_with_gap <- TRUE
  
  update_state_ls(state_ls, state)
}

handle_hashes <- function(state, state_ls, section_size, spring_groups) {
  
  # if (section_size == 0) {
  #   state_ls <- update_state_ls(state_ls, state)
  #   return(state_ls)
  # }
  
  if (!state$group_is_finished) {
    remaining_group <- spring_groups[state$idx1] - state$idx2
    if (section_size > remaining_group)
      return(state_ls)

    new_state <- 
      make_state(
        idx1 = state$idx1,
        idx2 = state$idx2 + section_size,
        group_is_finished = section_size == remaining_group,
        ends_with_gap = FALSE,
        combos = state$combos
      )
    state_ls <- update_state_ls(state_ls, new_state)
    return(state_ls)
  }
  
  if (!state$ends_with_gap)
    return(state_ls)
    
  if (state$idx1 + 1 <= length(spring_groups) && spring_groups[state$idx1 + 1] >= section_size) {

    new_state <- 
      make_state(
        idx1 = state$idx1 + 1,
        idx2 = section_size,
        group_is_finished = section_size == spring_groups[state$idx1 + 1],
        ends_with_gap = FALSE,
        combos = state$combos
      )
    state_ls <- update_state_ls(state_ls, new_state)
    return(state_ls)
  }  
    
  return(state_ls)
}

handle_qms <- function(state, state_ls, section_size, spring_groups) {

  if (section_size == 0) {
    state_ls <- update_state_ls(state_ls, state)
    return(state_ls)
  }
  
  if (!state$group_is_finished) {
    remaining_group <- spring_groups[state$idx1] - state$idx2
    if (section_size <= remaining_group) {
      state_ls <- handle_hashes(state, state_ls, section_size, spring_groups)
    } else {
      new_state <- 
        make_state(
          idx1 = state$idx1,
          idx2 = spring_groups[state$idx1],
          group_is_finished = TRUE,
          ends_with_gap = FALSE,
          combos = state$combos
        )
      state_ls <- handle_qms(new_state, state_ls, section_size - remaining_group, spring_groups)
    }
    
    return(state_ls)
  }
  
  if (!state$ends_with_gap) {
    new_state <- 
      make_state(
        idx1 = state$idx1,
        idx2 = state$idx2,
        group_is_finished = TRUE,
        ends_with_gap = TRUE,
        combos = state$combos
      )
    state_ls <- handle_qms(new_state, state_ls, section_size - 1, spring_groups)
    
    return(state_ls)
  }
  
  ## the 'do nothing' update
  state_ls <- update_state_ls(state_ls, state)
  
  offset <- state$idx1
  # if (!offset) {
  #   browser()
  # }
  
  ## ensure loop does not go OOB
  for (i in seq_along(spring_groups[offset < seq_len(length(spring_groups))])) {
    
    grp_sz <- spring_groups[offset + i]
    
    for (j in seq_len(grp_sz)) {
    
      if (j < grp_sz) {
        ## mid group
        spring_sig <- c(spring_groups[offset+seq_len(i-1)], j)
        
        combos_all <- calc_combos(section_size, spring_sig)
        combos_with_end_gap <- calc_combos(section_size - 1, spring_sig)
        combos_without_end_gap <- combos_all - combos_with_end_gap
        
        if (combos_without_end_gap) {
          new_state <-
            make_state(
              idx1 = state$idx1 + i,
              idx2 = j,
              group_is_finished = FALSE,
              ends_with_gap = FALSE,
              combos = state$combos * combos_without_end_gap
            )
          state_ls <- update_state_ls(state_ls, new_state)
        } else 
          break
      } else {
        ## end of group
        spring_sig <- c(spring_groups[offset+seq_len(i)])
        
        combos_all <- calc_combos(section_size, spring_sig)
        combos_with_end_gap <- calc_combos(section_size - 1, spring_sig)
        combos_without_end_gap <- combos_all - combos_with_end_gap
        
        if (combos_with_end_gap) {
          new_state_with_gap <- 
            make_state(
              idx1 = state$idx1 + i,
              idx2 = j,
              group_is_finished = TRUE,
              ends_with_gap = TRUE,
              combos = state$combos * combos_with_end_gap
            )
          state_ls <- update_state_ls(state_ls, new_state_with_gap)
        }
        
        if (combos_without_end_gap) {
          new_state_without_gap <- 
            make_state(
              idx1 = state$idx1 + i,
              idx2 = j,
              group_is_finished = TRUE,
              ends_with_gap = FALSE,
              combos = state$combos * combos_without_end_gap
            )
          state_ls <- update_state_ls(state_ls, new_state_without_gap)
        } else 
          break
      }
      
    }
    
  }
  
  
  state_ls
  
}

calc_combos <- function(section_size, spring_sig) {
  
  if (!length(spring_sig))
    stop("bad spring_sig")
  
  # x <- section_size - (length(spring_sig) - 1) - sum(spring_sig)
  
  x <- section_size - (sum(spring_sig) - length(spring_sig)) -(length(spring_sig)-1)
  
  if (x <= 0)
    return(0)
  
  # print(x, length(spring_sig))
  # print(choose(x+2, length(spring_sig)))
  # 
  # print(section_size - sum(spring_sig) -1)
  choose(x, length(spring_sig))
  # 
    
}

make_state <- function(idx1, idx2, group_is_finished, ends_with_gap, combos) {
  
  list(
    idx1 = idx1,
    idx2 = idx2,
    group_is_finished = group_is_finished,
    ends_with_gap = ends_with_gap, 
    combos = combos
  )
  
}


make_state_name <- function(state) {
  
  paste0("x", state$idx1, "_", state$idx2, "__", as.integer(state$ends_with_gap))
  
}


