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

# 1 la k co viec, 0 la co viec
pov_info <- read_dta("./Ho_ThanhVien.dta") %>%
  mutate(
    occupation =  case_when(
      M1A_C2 == 1 & M1A_C5 < 59 & M1A_C5 >= 15 & M4A_C2 == 2 ~ 1,
      M1A_C2 == 2 & M1A_C5 < 54 & M1A_C5 >= 15 & M4A_C2 == 2 ~ 1,
      TRUE ~ 0
    ),
    dependant = case_when(
      M1A_C5 < 16 ~ 1,
      M1A_C11 != 3 ~ 1,
      TRUE ~ 0
    ),
    BMI = M3B_C7 / ((M3B_C5/100)**2),
    nutrition = case_when(
      M1A_C5 < 16 & BMI < 18.5 ~ 1,
      TRUE ~ 0
    ),
    insurance = case_when(
      M1A_C5 > 6 & M3A_C7 == 2 ~ 1,
      TRUE ~ 0
    ),
    education = case_when(
      M1A_C5 >= 16 & M1A_C5 < 18 & M2_C2A %in% c(0, 1) ~ 0,
      M1A_C5 >= 18 & M1A_C5 < 30 & M2_C2A %in% c(2, 3, 7, 8, 9) ~ 0,
      M1A_C5 >= 18 & M1A_C5 < 30 & M2_C2B %in% c(4, 5, 6) ~ 0,
      TRUE ~ 1
    ),
    child_education = case_when(
      M1A_C5 >= 3 & M1A_C5 < 6 & M2_C6 == 0 ~ 0,
      M1A_C5 >= 6 & M1A_C5 < 12 & M2_C6 == 1 ~ 0,
      M1A_C5 >= 12 & M1A_C5 < 16 & M2_C6 == 2 ~ 0,
      TRUE ~ 1
    ),
    internet = case_when(
      M1A_C10 == 2 ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  group_by(IDHO) %>% 
  summarise(
    total_oc = sum(occupation, na.rm = TRUE),
    total_dep = sum(dependant, na.rm = TRUE),
    total_nutr = sum(nutrition, na.rm = TRUE),
    total_insu = sum(insurance, na.rm = TRUE),
    total_edu = sum(education, na.rm = TRUE),
    total_child_edu = sum(child_education, na.rm = TRUE),
    total_internet = sum(internet, na.rm = TRUE)
  ) %>% 
  mutate(
    total_oc = case_when(
      total_oc > 1 ~ 1,
      TRUE ~ 0
    ),
    total_dep = case_when(
      total_dep > 1 ~ 1,
      TRUE ~ 0
    ),
    total_nutr = case_when(
      total_nutr > 1 ~ 1,
      TRUE ~ 0
    ),
    total_insu = case_when(
      total_insu > 1 ~ 1,
      TRUE ~ 0
    ),
    total_edu = case_when(
      total_edu > 1 ~ 1,
      TRUE ~ 0
    ),
    total_child_edu = case_when(
      total_child_edu > 1 ~ 1,
      TRUE ~ 0
    ),
    total_internet = case_when(
      total_internet == 0 ~ 1,
      TRUE ~ 0
    )
  )

cities <- read_dta("./weight2022.dta") %>% 
  select(
    tinh, huyen, xa, diaban, tentinh, tenhuyen, tenxa, tendb, wt9
  ) 
cities$tentinh <- gsub("Tỉnh ", "", cities$tentinh)
cities$tentinh <- gsub("Thành phố ", "", cities$tentinh)
city_zone <- distinct(
  merge(
    cities,
    area_type,
    by = c('tentinh', 'tenhuyen')
  )
) %>% 
  subset(
    select = -c(tentinh, tenhuyen, tenxa, tendb, wt9)
  )

poverty_df <- read_dta("./Ho_ThongTinHo.dta") %>% 
  merge(
    cities,
    by.x = c('MATINH', 'MAHUYEN', 'MAXA', 'MADIABAN'),
    by.y = c('tinh', 'huyen', 'xa', 'diaban')
  ) %>% 
  mutate(
    pov_income = case_when(
      grepl("Quận", tenhuyen) | grepl("Thành phố", tenhuyen) ~ 2000,
      TRUE ~ 1500
    ),
    housing = case_when(
      M7_C4A %in% c(4, 5) & M7_C4B != 1 ~ 1,
      M7_C4A %in% c(4, 5) & !(M7_C4C %in% c(1, 2)) ~ 1,
      M7_C4B != 1 & !(M7_C4C %in% c(1, 2)) ~ 1,
      TRUE ~ 0
    ),
    house_size = case_when(
      M7_C2 / SONHANKHAU < 8 ~ 1,
      TRUE ~ 0
    ),
    water = case_when(
      M7_C13A %in% c(1, 2, 3, 4, 5, 6, 8, 10, 14) ~ 0,
      TRUE ~ 1
    ),
    toilet = case_when(
      M7_C15 %in% c(1, 2, 3, 4, 5, 6 ,7) ~ 0,
      TRUE ~ 1
    )
  ) %>% 
  select(
    IDHO, MATINH, MAHUYEN, MADIABAN, tentinh, tenhuyen, THUBQ,
    pov_income, housing, water, toilet, house_size
  ) %>% 
  merge(
    pov_info
  ) %>% 
  mutate(
    poverty = case_when(
      THUBQ <= pov_income &
        housing + house_size + water + toilet + total_oc + total_dep + 
        total_nutr + total_insu + total_edu + total_child_edu >= 3 ~ 2,
      THUBQ <= pov_income &
        housing + house_size + water + toilet + total_oc + total_dep + 
        total_nutr + total_insu + total_edu + total_child_edu > 0 ~ 1,
      TRUE ~ 0
    )
  )

poverty_df %>% 
  group_by(tentinh, poverty) %>% 
  summarise(
    poor = n()
  ) %>% 
  View()
