## shit show - not properly solved


library(data.table)
library(bit64)

path_instructions <- 
  fread("inputs/8.txt", nrows = 1, header = FALSE, col.names = "path_data") %>% 
  pull(path_data) %>% 
  str_split_1(pattern = "")

edges <- 
  fread("inputs/8.txt", skip = 1, header = FALSE, col.names = "edge_data", sep = "") %>% 
  separate_wider_regex(
    cols = edge_data,
    patterns = c(
      x = "^[0-9A-Za-z]+",
      " = \\(",
      l = "[0-9A-Za-z]+",
      ", ",
      r = "[0-9A-Za-z]+",
      "\\)"
    )
  ) %>% 
  as.data.table() %>% 
  setkey(x)


#### part 1 ####.

step_count <- 0
node <- "AAA"

while (node != "ZZZ") {
  
  for (i in path_instructions) {

    if (i == "L")
      node <- edges[node, on = .(x), l]
    else 
      node <- edges[node, on = .(x), r]
  
    step_count <- step_count + 1    
    
    if (node == "ZZZ")
      break
    
    if (step_count %% 1000 == 0)
      print(step_count)
  }
}

print(step_count)


#### part 2  fail ####.

edges[, is_A := str_sub(x, 3, 3) == "A"]
edges[, is_Z := str_sub(x, 3, 3) == "Z"]
edges[, id := seq_len(.N)]
edges[
  edges,
  on = .(l = x),
  l_id := i.id
]
edges[
  edges,
  on = .(r = x),
  r_id := i.id
]

edges[
  CJ(1:3)
]

states <-
  edges %>% 
  as_tibble() %>% 
  cross_join(
    tibble(
      instr_id = seq_len(length(path_instructions)),
      next_instr_id = c(seq_len(length(path_instructions)-1)+1, 1),
      instr = path_instructions,
      next_instr = lead(path_instructions, default = path_instructions[1])
    )
  ) %>% 
  mutate(
    state_id = seq_len(n()),
    next_x = if_else(instr == "L", l, r)
  ) %>% 
  as.data.table()


states[
  states,
  on = .(next_x = x, next_instr_id = instr_id),
  next_state_id := i.state_id
]

beginning_states <-
  states[
    instr_id == 1 & is_A == TRUE,
    state_id
  ]

path_objs <- list()
for (i in beginning_states) {
  path_objs <-
    append(
      path_objs,
      make_path_obj(i, states[, next_state_id]) %>% 
        compress_paths(states) %>% 
        list()
    )
}

c(19631,
17287,
23147,
20803,
17873,15529)

gcd(19631, 17287/293*23147)
gcd(19631, 17873/293*15529)
17873/293*15529
lcm(17873,15529)

step_count <- 0
nodes <- edges[is_A == TRUE]

while (!all(nodes[, is_Z])) {
  
  for (i in path_instructions) {
    
    if (i == "L")
      node_ids <- edges[nodes$id, l_id]
    else 
      node_ids <- edges[nodes$id, r_id]
    
    nodes <- edges[node_ids]
    
    step_count <- step_count + 1    
    
    if (all(nodes[, is_Z]))
      break
    
    if (step_count %% 1000 == 0)
      print(step_count)
  }
}

print(step_count)

instr_id
## 396286894121

# 21003205388413
make_path_obj <- function(start_id, next_ids) {
  
  id_count <- length(next_ids)
  visited <- logical(id_count)
  
  id <- start_id
  
  path <- numeric(id_count)
  i <- 1

  while (TRUE) {
    
    visited[id] <- TRUE
    path[i] <- id
    
    next_id <- next_ids[id]
    
    if (visited[next_id]) {
      run_in <- which(path == next_id) - 1
      path_objs <- 
        list(
          path = path[1:i],
          run_in = run_in,
          cycle_length = i - run_in,
          cycle_start = run_in + 1
        )
      break
    }
    
    id <- next_id
    i <- i + 1
    
  }
  
  path_objs
    
}
compress_paths <- function(path_obj, states) {
  
  path_obj$path <- states[path_obj$path][, is_Z] %>% which()
  
  path_obj
  
}

gcd <- function(x,y) {
  r <- x%%y;
  return(ifelse(r, gcd(y, r), y))
}

merge_path_objs <- function(po1, po2) {
  
  # run_in <- max(po1$run_in, po2$run_in)
  
  gcd <- gcd(po1$cycle_length, po2$cycle_length)
  
  new_cycle_length <- po1$cycle_length / gcd * po2$cycle_length
  # browser()
  paths1 <- po1$path + (seq_len(new_cycle_length / po1$cycle_length) - 1) * po1$cycle_length
  paths2 <- po2$path + (seq_len(new_cycle_length / po2$cycle_length) - 1) * po2$cycle_length
  
  list(
    path = intersect(paths1, paths2),
    cycle_length = new_cycle_length 
  )
  
}

reduce(path_objs, merge_path_objs)


make_clusters <- function(next_ids) {
  
  id_count <- length(next_ids)
  visited <- logical(id_count)
  path_objs <- list()
  
  while (TRUE) {
  
    if (!all(visited))
      id <- which(!visited)[1]
    else 
      break
  
    path <- numeric(id_count)
    i <- 1
  
    while (TRUE) {
      
      visited[id] <- TRUE
      path[i] <- id
      
      next_id <- next_ids[id]
      
      if (visited[next_id]){
        run_in <- which(path == next_id) - 1
        path_objs <-
          append(
            path_objs,
            list(
              list(
                path = path[1:i],
                run_in = run_in,
                cycle_length = i - run_in,
                cycle_start = run_in + 1
              )
            )
          )
        break
      }
      
      id <- next_id
      i <- i + 1
      
    }
    
  }
  
  path_objs
  
}





