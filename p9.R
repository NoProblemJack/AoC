library(tidyverse)

extend_seq <- function(seq) {
  
  len <- length(seq)
  delta <- seq[-1] - seq[-len]
    
  if (length(unique(delta)) > 1)
    delta <- extend_seq(delta)
  
  c(seq, seq[len] + delta[length(delta)])
  
}


data <-
  read_csv("inputs/9.txt", col_names = c("seqs")) %>% 
  mutate(
    seqs = 
      map(
        seqs, 
        ~{str_split_1(.x, pattern = " ") %>% as.integer()}
      ),
  )


#### part 1 ####.

data %>% 
  mutate(
    ext_seq =
      map(
        seqs,
        extend_seq
      ),
    last_el =
      map_int(
        ext_seq,
        ~{.x[length(.x)]}
      )
  ) %>% 
  summarise(sum(last_el))
## 1637452029


#### part 2 ####.

data %>% 
  mutate(
    ext_seq =
      map(
        map(seqs, rev),
        extend_seq
      ),
    last_el =
      map_int(
        ext_seq,
        ~{.x[length(.x)]}
      )
  ) %>% 
  summarise(sum(last_el))

