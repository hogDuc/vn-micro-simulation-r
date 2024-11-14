setwd('C:/Users/ADMIN/Desktop/SciEcons/UKMOD/VHLSS 2022/VHLSS2022')

library(dplyr)
library(tidyr)
library(haven)
library(readxl)
library(jsonlite)
library(readr)

file_names <- list.files('./')
for (file_name in file_names) {
  print(paste('Reading file:', file_name))
  tryCatch(
    {
      eval(parse(text = paste0(
        file_name, '<- read_dta("./', file_name, '")'
      )))
    },
    error = function(e) e
  )
}

cities <- read_dta("./weight2022.dta") %>% 
  select(
    tinh, huyen, xa, diaban, tentinh, tenhuyen, tenxa, tendb, wt9
  ) 
cities$tentinh <- gsub("Tỉnh ", "", cities$tentinh)
cities$tentinh <- gsub("Thành phố ", "", cities$tentinh)

wb_poverty <- Ho_ThongTinHo.dta %>% 
  mutate(
    poverty_status = case_when(
      ((THUNHAP/12)/30) / SONHANKHAU < 52.8 ~ "extreme poverty",
      ((THUNHAP/12)/30) / SONHANKHAU < 89.662 ~ "lower-middle",
      ((THUNHAP/12)/30) / SONHANKHAU < 217.40 ~ "upper-middle",
      TRUE ~ "high"
    )
  ) %>% 
  select(
    IDHO, MATINH, MAHUYEN, MAXA, MADIABAN, poverty_status
  ) %>% 
  merge(
    cities,
    by.x = c('MATINH', 'MAHUYEN', 'MAXA', 'MADIABAN'),
    by.y = c('tinh', 'huyen', 'xa', 'diaban')
  ) 
wb_poverty %>% 
  group_by(poverty_status) %>% 
  summarise(
    count = n()
  )
