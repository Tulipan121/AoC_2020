library(dplyr)
library(data.table)
library(stringr)

data <- fread("slopes_AoC_2020_3.txt", header = FALSE)

slide <- function(down, right) {
      
   down_seq <- seq(1, nrow(data), down)
   
   right_seq <- c(seq(0, length(down_seq) * right, right))
            
   pozicija <- round((right_seq/forest_length - floor(right_seq/forest_length)) * forest_length, 0)
   
   lapply(c(1:length(down_seq)),
          function(x) str_detect(data[down_seq[x], V1], paste0("^.{", pozicija[x], "}\\#"))) %>% unlist %>% sum
      
}

forest_length <- str_length(data$V1[1])

right_step <- c(1, 3, 5, 7, 1)

down_step <- c(1, 1, 1, 1, 2)


solution <- lapply(c(1:length(down_step)), function(x) slide(down_step[x], right_step[x])) %>% unlist %>% prod()




