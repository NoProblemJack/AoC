library(tidyverse)
library(data.table)
library(bit64)


data <- read_table("inputs/19.txt", col_names = "data")


parts_raw <- data %>% filter(str_sub(data,1,1) == "{")
wfs_raw <- data %>% filter(str_sub(data,1,1) != "{")


# px{a<2006:qkq,m>2090:A,rfg}
# pv{a>1716:R,A}
# lnx{m>1548:A,A}
# rfg{s<537:gd,x>2440:R,A}
# qs{s>3448:A,lnx}
# qkq{x<1416:A,crn}
# crn{x>2662:A,R}
# in{s<1351:px,qqz}
# qqz{s>2770:qs,m<1801:hdj,R}
# gd{a>3333:R,R}
# hdj{m>838:A,pv}
# 
# {x=787,m=2655,a=1222,s=2876}
# {x=1679,m=44,a=2067,s=496}
# {x=2036,m=264,a=79,s=2244}
# {x=2461,m=1339,a=466,s=291}
# {x=2127,m=1623,a=2188,s=1013}





wf_list <- map(wfs_raw$data, parse_wfs)
parts <- parse_parts(parts_raw)

apply_all_wfs(parts, wf_list)


# parse_wfs2(wfs_raw[1,])



wf_list2 <- map(c(wfs_raw$data, "A{A}"), parse_wfs2) %>% flatten()
store_ls <- list("in" = parse_parts2())
new_store_ls <- list()

repeat {

  for (i in seq_along(store_ls)) {
    wf_name <- names(store_ls)[i]
    new_store_ls <- update_store_ls_wf(new_store_ls, store_ls[[i]], wf_list2[[wf_name]])
  }

  store_ls <- map(new_store_ls, rbindlist)
  new_store_ls <- list()
  print(store_ls)
  message("-------------")
  if (identical(names(store_ls), "A"))
    break
}
store_ls[["A"]][, lapply(.SD, as.integer64)][, .(sum((xmax-xmin+1)*(mmax-mmin+1)*(amax-amin+1)*(smax-smin+1)))]

update_store_ls_wf <- function(store_ls, sections, wf_conds) {
  
  
  for (i in wf_conds) {
    in_set <- rlang::expr(
      sections[
        !!rlang::parse_expr(i$in_filt_expr)
      ][, 
        !!rlang::parse_expr(i$in_upd_expr)
      ]
    ) %>% eval()
    
    sections <- rlang::expr(
      sections[
        !!rlang::parse_expr(i$out_filt_expr)
      ][, 
        !!rlang::parse_expr(i$out_upd_expr)
      ]
    ) %>% eval()
  
    if (is.null(store_ls[[i$next_wf]]))
      store_ls[[i$next_wf]] <- list()
    store_ls[[i$next_wf]] <-
      append(
        store_ls[[i$next_wf]],
        list(in_set)
      )
  }
  
  
  store_ls
  
}



apply_all_wfs <- function(parts, wf_list) {
  
  repeat {
  
    for (i in wf_list)
      apply_wf(parts, i)
    
    parts[, wf := next_wf]
  
    if (!nrow(parts[!wf %in% c("A", "R")]))
      break
    
    # print(parts)
    # browser()
  }
  
  list(
    parts,
    parts[wf %in% c("A")] %>% summarise_if(is.numeric, sum) %>% sum()
  )
  
}


parse_wfs2 <- function(x) {
  
  name <- str_extract(x, "^[^{]+")
  x <- str_extract(x, "\\{(.+)\\}", group = 1)
  x <- str_split_1(x, ",")
  
  conds <- 
    c(x[-length(x)], paste0("TRUE:", x[length(x)])) %>% 
    map(str_split_1, pattern = ":")
  
  conds_ls <- list()
  
  for (cnd in conds) {
    # browser()
    if (identical(cnd[1], "TRUE")) {
      in_filt_expr <- paste0("TRUE")
      in_upd_expr <- paste0(".SD")
      out_filt_expr <- paste0("FALSE")
      out_upd_expr <- paste0(".SD")
    } else {
      char <- cnd[1] %>% str_sub(1,1)
      sym <- cnd[1] %>% str_sub(2,2)
      val <- cnd[1] %>% str_extract("\\d+")
      if (sym == "<"){
        in_filt_expr <- paste0(char, "min<", val)
        in_upd_expr <- paste0(char, "max:=pmin(", char, "max, ", val, "-1)")
        out_filt_expr <- paste0(char, "max>=", val)
        out_upd_expr <- paste0(char, "min:=pmax(", char, "min, ", val, ")")
      } else {
        in_filt_expr <- paste0(char, "max>", val)
        in_upd_expr <- paste0(char, "min:=pmax(", char, "min, ", val, "+1)")
        out_filt_expr <- paste0(char, "min<=", val)
        out_upd_expr <- paste0(char, "max:=pmin(", char, "max, ", val, ")")
      }
    }
    conds_ls <- append(conds_ls,
      list(
        in_filt_expr = in_filt_expr,
        out_filt_expr = out_filt_expr,
        in_upd_expr = in_upd_expr,
        out_upd_expr = out_upd_expr,
        next_wf = cnd[2]
      ) %>% list
    )
      
  }
  
  # list(
  #   name = name,
  #   # next_ins,
  #   conds = conds,
  #   conds_ls = conds_ls
  # )
  
  list(conds_ls) %>% setNames(name)
  
}
parse_parts2 <- function() {
  
  tibble(
    # id = 1L,
    xmin = 1, 
    xmax = 4000,
    mmin = 1, 
    mmax = 4000,
    amin = 1, 
    amax = 4000,
    smin = 1, 
    smax = 4000,
    # wf = "in"
  ) %>% 
  as.data.table()
  
}

parse_wfs <- function(x) {
  
  name <- str_extract(x, "^[^{]+")
  x <- str_extract(x, "\\{(.+)\\}", group = 1)
  x <- str_split_1(x, ",")
  
  conds <- 
    c(x[-length(x)], paste0("TRUE:", x[length(x)])) %>% 
    map(str_split_1, pattern = ":")
  
  
  list(
    name = name,
    # next_ins,
    conds = conds
  )
  
}
parse_parts <- function(p) {
  
  p %>% 
    separate_wider_regex(
      col = data,
      patterns = c(
        "^[^\\d]+",
        x = "[\\d]+",
        "[^\\d]+",
        m = "[\\d]+",
        "[^\\d]+",
        a = "[\\d]+",
        "[^\\d]+",
        s = "[\\d]+",
        "[^\\d]+$"
      )
    ) %>% 
    mutate_all(as.integer) %>% 
    mutate(wf = "in", next_wf = "jcdklsjfd") %>% 
    as.data.table()
  
}

apply_wf <- function(parts, wf) {
  
  wf_name <- wf$name
  wf_cond <- wf$conds
  
  for (i in rev(seq_along(wf_cond)))
    eval(rlang::expr(parts[wf == !!wf_name & !!rlang::parse_expr(wf_cond[[i]][1]), 
                            next_wf := !!wf_cond[[i]][2]]))
  
  wf
  
}


