library(dplyr)
library(data.table)
library(stringr)

`%!in%` <- function(x, y) !(x %in% y)

data <- fread("boardingpass_AoC_2020_5.txt", header = FALSE) %>% setnames(., "V1", "FB") %>%
      .[, LR := str_extract(FB, ".{3}$") %>% str_replace_all(., "L", "0") %>% str_replace_all(., "R", "1")] %>% 
      .[, FB := str_extract(FB, "^.{7}") %>% str_replace_all(., "B", "1") %>% str_replace_all(., "F", "0")] %>%
      .[, `:=` (LR_score = lapply(LR, function(x) strtoi(x, base = 2)) %>% unlist,
                FB_score = lapply(FB, function(x) strtoi(x, base = 2)) %>% unlist)] %>%
      .[, score := FB_score * 8 + LR_score]

max(data$score)

a <- c(min(data$score):max(data$score))

a[which(a %!in% data$score)]
