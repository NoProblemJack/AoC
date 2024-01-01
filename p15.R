library(tidyverse)


data <-
  read_delim("inputs/15.txt", delim = ":", col_names = "data") %>% 
  separate_longer_delim(
    cols = data, 
    delim = ","
  )

## 516657
p1 <- 
  data %>% 
  mutate(
    hash_val = map_int(data, hash_algo)
  )

p1 %>% summarise(sum(hash_val))
  
p2 <-
data %>% 
  mutate(
    label = str_extract(data, "^[A-Za-z]+"),
    op = str_extract(data, "[-=]"),
    val = str_extract(data, "\\d+$"),
    box_num = map_int(label, hash_algo)
  )

 
box_ls <- list()

for (i in seq_len(nrow(p2))) {
  if (p2$op[i] == "-")
    box_ls <- remove_lens(box_ls, p2$box_num[i], p2$label[i])
  else
    box_ls <- add_lens(box_ls, p2$box_num[i], p2$label[i], p2$val[i])
}

names <- names(box_ls)
ans <- 0
for (i in seq_along(box_ls)) {
  
  if (!length(box_ls[[i]]))
    next
  temp_ans <- 0
  for (j in seq_along(box_ls[[i]]))
    temp_ans <- temp_ans + j * box_ls[[i]][[j]]
  
  ans <- ans + temp_ans * (as.integer(names[i]) + 1)
  
}
ans ## 210906

remove_lens <- function(box_ls,box_num, label) {
  # print(label)
# browser(0)
  box_num <- as.character(box_num)
  
  # if (label == "flfk")
  #   browser()
  if (is.null(box_ls[[box_num]]))
    box_ls[[box_num]] <- list()
# browser(0)
  box_ls[[box_num]][[label]] <- NULL
  
  
  box_ls
  
}

add_lens <- function(box_ls, box_num, label, val) {
  box_num <- as.character(box_num)
  # print(label)
  # if (label == "bv")
  #   browser()
  if (is.null(box_ls[[box_num]]))
    box_ls[[box_num]] <- list()
  
  box_ls[[box_num]][[label]] <- as.integer(val)

  
  box_ls

}


hash_algo <- function(x) {
  
  x <- str_split_1(x, pattern = "")
  
  out <- 0L
  
  for (i in x) {
    
    out <- out + as.integer(charToRaw(i))
    out <- out * 17
    out <- out %% 256
    
  }
  
  
  out
  
}