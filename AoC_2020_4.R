library(dplyr)
library(data.table)
library(stringr)

`%!in%` <- function(x, y) !(x %in% y)

data <- readChar("passports_AoC_2020_4.txt", nchars=1e6) %>% 
      str_split(., "[:space:]{2}") %>%
      lapply(., as.list) %>% 
      unlist(recursive = FALSE) %>% 
      lapply(., function(x) lapply(x, function(y) str_split(y, "[:space:]"))) %>%
      lapply(., function(x) lapply(x, function(y) lapply(y, function(z) str_split(z, "\\:")) %>% 
                                         unlist(recursive = FALSE))) %>% 
      unlist(recursive = FALSE)


required_attributes <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

passport_data <- data.table()

for(i in c(1:length(data))) {
      
      passport_data <- rbind(passport_data,
                             
      do.call(rbind, data[[i]]) %>% 
            t() %>% 
            data.table() %>% 
            setnames(., colnames(.), .[1, ] %>% unlist) %>% 
            .[-1, ] %>% 
            .[, ID := i],
      
      fill = TRUE)
      
}

passport_data[, V1 := NULL]

passport_data[{str_detect(byr, "^[:digit:]{4}$") &
      byr %>% as.numeric %>% between(., 1920, 2002) &
      str_detect(iyr, "^[:digit:]{4}$") & 
      iyr %>% as.numeric %>% between(., 2010, 2020) &
      str_detect(eyr, "^[:digit:]{4}$") &
      eyr %>% as.numeric %>% between(., 2020, 2030) &
      ((str_detect(hgt, "^[:digit:]+cm$") & 
               str_extract(hgt, "[:digit:]+") %>% as.numeric %>% between(., 150, 193)) |
         (str_detect(hgt, "^[:digit:]+in$") &
               str_extract(hgt, "[:digit:]+") %>% as.numeric %>% between(., 59, 76))) &
      str_detect(hcl, "^\\#[[:digit:]|[a-f]]{6}$") &
      ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth") &
      str_detect(pid, "^[:digit:]{9}$")}, ok := 1]

passport_data$ok %>% sum(., na.rm = TRUE)