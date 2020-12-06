library(dplyr)
library(data.table)
library(stringr)

data <- fread("passwords_AoC_2020_2_1.txt", header = FALSE) %>%
      .[, `:=` (min = lapply(V1, function(x) x %>% str_remove(., "\\-.*")) %>% as.numeric(),
                max = lapply(V1, function(x) x %>% str_remove(., ".*\\-")) %>% as.numeric(),
                rule = V2 %>% str_remove(., "\\:"))] %>%
      .[str_count(V3, rule) >= min & str_count(V3, rule) <= max, .N]


data_2 <- fread("passwords_AoC_2020_2_1.txt", header = FALSE) %>%
      .[, `:=` (min = lapply(V1, function(x) x %>% str_remove(., "\\-.*")) %>% as.numeric(),
                max = lapply(V1, function(x) x %>% str_remove(., ".*\\-")) %>% as.numeric(),
                rule = V2 %>% str_remove(., "\\:"))] %>%
      .[str_detect(V3, paste0("^.{", min - 1, "}", rule)) + str_detect(V3, paste0("^.{", max - 1, "}", rule)) == 1, .N]


