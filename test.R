setwd('C:/Users/ADMIN/Desktop/SciEcons/UKMOD/VHLSS 2022/VHLSS2022')
library(dplyr)
library(tidyr)
library(haven)
library(readxl)
library(jsonlite)
library(readr)
library(sf)
library(ggplot2)


# Đọc dữ liệu
file_names <- list.files('./')
for (file_name in file_names) {
  # print(paste("reading file", file_name))
  if (grepl(".dta", file_name)) {
    tryCatch(
      {
        eval(parse(text = paste0(
          file_name, '<- read_dta("./', file_name, '")'
        )))
      },
      error = function(e) e
    )
  }
}

URL <- 'https://data.opendevelopmentmekong.net/dataset/55bdad36-c476-4be9-a52d-aa839534200a/resource/b8f60493-7564-4707-aa72-a0172ba795d8/download/vn_iso_province.geojson'
vietnam <- st_read(URL)
vietnam <- vietnam %>% 
  mutate(
    Name_VI = case_when(
      Name_VI == "Bình  Dương" ~ "Bình Dương",
      Name_VI == "Thanh Hóa" ~ "Thanh Hóa",
      Name_VI == "Thừa Thiên-Huế" ~ "Thừa Thiên Huế",
      Name_VI == "TP. Hồ Chí Minh" ~ "Hồ Chí Minh",
      Name_VI == "Khánh  Hòa" ~ "Khánh Hòa",
      Name_VI == "Kiên  Giang" ~ "Kiên Giang",
      Name_VI == "Ninh  Thuận" ~ "Ninh Thuận",
      Name_VI == "Hòa Bình" ~ "Hoà Bình",
      Name_VI == "Quảng  Nam" ~ "Quảng Nam",
      Name_VI == "Quảng  Ngãi" ~ "Quảng Ngãi",
      Name_VI == "Thái  Nguyên" ~ "Thái Nguyên",
      TRUE ~ Name_VI
    )
  )


vung_mien <- read_csv("./vung_mien_vn.csv", show_col_types = FALSE) %>% 
  fill(Miền) %>% 
  fill(`Lãnh thổ`) %>% 
  rename(
    c(
      "mien" = "Miền",
      "lanhtho" = "Lãnh thổ",
      "tentinh" = "Thành phố"
    )
  ) %>% 
  mutate(
    tentinh = case_when(
      tentinh  == "Hòa Bình" ~ "Hoà Bình",
      tentinh == "Thừa Thiên - Huế" ~ "Thừa Thiên Huế",
      TRUE ~ tentinh
    )
  )

Ho_ThongTinHo.dta <- read.csv("./merged_cp_thongtinho.csv")

area_type <- read_xlsx("../ma_vung.xlsx") %>% 
  select(
    `Tỉnh/thành phố trực thuộc TW`, `Quận/ /thị xã/thành phố thuộc tỉnh`, `Vùng`
  ) %>% 
  fill(`Tỉnh/thành phố trực thuộc TW`, .direction = "down") %>% 
  drop_na() %>% 
  dplyr::rename(
    tentinh = `Tỉnh/thành phố trực thuộc TW`,
    tenhuyen = `Quận/ /thị xã/thành phố thuộc tỉnh`,
    vung = `Vùng`
  ) %>% 
  merge(
    data.frame(
      vung = c('I', 'II', 'III', 'IV'),
      min_salary = c(4960, 4410, 3860, 3450)
    )
  )

# new_weight <- read.csv("./new_weight.csv")
cities <- read.csv("./new_weight.csv") %>% # trong so va ten khu vuc
  select(
    MATINH, MAHUYEN, MAXA, MADIABAN, tentinh, tenhuyen, tenxa, tendb, wt45
  ) %>% 
  dplyr::rename(
    tinh = MATINH,
    huyen = MAHUYEN,
    xa = MAXA,
    diaban = MADIABAN
  # ) %>% 
  # mutate(
  #   wt_household = WT_SCIECO / 15
  ) # Tính weight mỗi hộ trong các địa bàn

cities$tentinh <- gsub("Tỉnh ", "", cities$tentinh)
cities$tentinh <- gsub("Thành phố ", "", cities$tentinh)

cities <- left_join(
  cities,
  vung_mien,
  by = join_by(tentinh)
) 
  # left_join(
  #   area_type,
  #   by = join_by(tentinh, tenhuyen),
  #   relationship = "many-to-many"
  # )

Ho_ThongTinHo_2 <- left_join(    # lấy số người độc thân trong mỗi hộ
    Ho_ThongTinHo.dta,
    select(
      Ho_ThanhVien.dta,
      IDHO, M1A_C7, M1A_C5, M1A_C2
    ) %>% # Lấy thông tin về tình trạng hôn nhân
      mutate(
        IDHO = as.numeric(IDHO),
        marriage_status = case_when(
          ((M1A_C2 = 1 & M1A_C5 >= 20)|(M1A_C2 = 2 & M1A_C5 >= 18)) &
            (M1A_C7 != 2 & M1A_C7 != 5) ~ 1,
          TRUE ~ 0
        )
      ) %>% # Gán 1 cho các trường hợp độc thân
      group_by(IDHO) %>%
      summarise(
        number_of_singles = sum(marriage_status, na.rm = TRUE)
      ),
    by = "IDHO"
  ) %>% 
  mutate_all(~replace(., is.na(.), 0))
# write_csv(Ho_ThongTinHo_2, "thongtinho.csv")
# write_csv(cities, "cities_weight.csv")
# write.csv(Ho_ThanhVien.dta, "thongtinthanhvien.csv")

get_market_income <- function(
    tc_that_nghiep = 0, tc_thoi_viec = 0, luong_bt = 0, 
    luong_som = 0, tc_mat_suc = 0,
    tc_thuong_binh = 0, tc_bao_tro_xh = 0, tc_thien_tai = 0,
    db_trong_trot = 0, db_chan_nuoi = 0, db_dvnn = 0,
    db_trong_rung = 0, db_nuoi_thuy_san = 0,
    thue_chan_nuoi = 0, thue_dvnn = 0, thue_lam_nghiep = 0, 
    thue_kd_lam_nghiep = 0, thue_doc_than = 0,
    inc_tax_1 = 0.05, inc_tax_2 = 0.1, inc_tax_3 = 0.15, inc_tax_4 = 0.2, 
    inc_tax_5 = 0.25, inc_tax_6 = 0.3, inc_tax_7 = 0.35,
    inc_threshold_1 = 5000, inc_threshold_2 = 10000, inc_threshold_3 = 18000, inc_threshold_4 = 32000,
    inc_threshold_5 = 52000, inc_threshold_6 = 80000
  ) {
  personal_income <- Ho_ThanhVien.dta %>% # tính thu nhập từ lương của mỗi cá nhân
    mutate( 
      M4A_C18A = M4A_C18A * (1 + tc_that_nghiep), # custom tro cap
      M4A_C18B = M4A_C18B * (1 + tc_thoi_viec), 
      M4A_C18C = M4A_C18C * (1 + luong_bt), 
      M4A_C18D = M4A_C18D * (1 + luong_som), 
      M4A_C18E = M4A_C18E * (1 + tc_mat_suc),
      tro_cap_personal = M4A_C18A + M4A_C18B + M4A_C18E,
      monthly_salary = (M4A_C5 + M4A_C11 + M4A_C15)/12,
      salary_tax = case_when(
         monthly_salary <= inc_threshold_1 ~ inc_tax_1 * monthly_salary,
         monthly_salary <= inc_threshold_2 ~ inc_tax_2 * monthly_salary,
         monthly_salary <= inc_threshold_3 ~ inc_tax_3 * monthly_salary,
         monthly_salary <= inc_threshold_4 ~ inc_tax_4 * monthly_salary,
         monthly_salary <= inc_threshold_5 ~ inc_tax_5 * monthly_salary,
         monthly_salary <= inc_threshold_6 ~ inc_tax_6 * monthly_salary,
         TRUE ~ inc_tax_7 * monthly_salary
      )
    ) %>% 
    group_by(IDHO) %>% 
    summarise(
      salary_income = sum(
        sum(M4A_C5, na.rm = TRUE), sum(M4A_C6A, na.rm = TRUE), 
        sum(M4A_C6B, na.rm = TRUE), sum(M4A_C11, na.rm = TRUE), 
        sum(M4A_C12A, na.rm = TRUE), sum(M4A_C12B, na.rm = TRUE), 
        sum(M4A_C15, na.rm = TRUE), sum(M4A_C18A, na.rm = TRUE), 
        sum(M4A_C18B, na.rm = TRUE), sum(M4A_C18C, na.rm = TRUE), 
        sum(M4A_C18D, na.rm = TRUE), sum(M4A_C18E, na.rm = TRUE),
        na.rm = TRUE
      ),
      salary_tax = sum(salary_tax, na.rm = TRUE),
      tro_cap_personal = sum(tro_cap_personal, na.rm = TRUE)
    ) %>% 
    mutate_all(~replace(., is.na(.), 0))
  
  household_income <- Ho_ThongTinHo_2 %>% # Tính thu nhập từ hộ kinh doanh
    mutate(
      M4B1T2 = M4B1T2 * (1 + db_trong_trot), # custom den bu
      M4B2T2 = M4B2T2 * (1 + db_chan_nuoi),
      M4B3T2 = M4B3T2 * (1 + db_dvnn),
      M4B4T2 = M4B4T2 * (1 + db_trong_rung),
      M4B5T2 = M4B5T2 * (1 + db_nuoi_thuy_san),
      M4D_04 = M4D_04 * (1 + tc_thuong_binh),
      M4D_05 = M4D_05 * (1 + tc_bao_tro_xh),
      M4D_06 = M4D_06 * (1 + tc_thien_tai),
      M4B22_C17_2 = M4B22_C17 * (1 * thue_chan_nuoi),
      M4B32_C15_2 = M4B32_C15 * (1 + thue_dvnn),
      M4B42_C12_2 = M4B42_C12 * (1 + thue_lam_nghiep),
      M4B52_C17_2 = M4B52_C17 * (1 + thue_kd_lam_nghiep)
    ) %>% 
    mutate_all(~replace(., is.na(.), 0))
  household_income$business_income <- rowSums(
    household_income[
      c(
        'M3TN', 'M2TN',
        'M4B0TN', "M4B1T2",
        'M4B11T', 'M4B12T', 'M4B13T', 'M4B14T', 'M4B15T',  
        'M4B21T', # Thu chăn nuôi
        'M4B22T', # Thu săn bắt 
        'M4B3T', 'M4B4T', 'M4B5T1', 'M4CT', 'M4D_01', 'M4D_02',
        'M4D_03', 'M4D_04', 'M4D_05', 'M4D_06', 'M4D_07', 'M4D_08', 'M4D_09', 
        'M4D_10', 'M4D_11', 'M4D_12',
        'M7_C12'
      )
    ],
    na.rm = TRUE
  )

  thongtinho_modified <- left_join(
    household_income ,
    personal_income %>% 
      mutate(
        IDHO = as.numeric(IDHO)
      ),
    by = "IDHO"
  ) %>% 
    mutate_all(
      ~replace(., is.na(.), 0)
    ) %>%  
    mutate(
      bachelor_tax = thue_doc_than * number_of_singles,
      total_household_income = business_income + salary_income,
      total_household_fee = TONGCHI - (M4B22_C17 + M4B32_C15 + M4B42_C12 + M4B52_C17) + 
        + (M4B22_C17_2 + M4B32_C15_2 + M4B42_C12_2 + M4B52_C17_2) + bachelor_tax
    ) %>% 
    merge(
      cities,
      by.x = c("MATINH", "MAHUYEN", "MAXA", "MADIABAN"),
      by.y = c("tinh", "huyen", "xa", "diaban")
    ) %>% 
    mutate(
      wt_household = wt45 * SONHANKHAU,
      custom_thubq = ((total_household_income - total_household_fee) / SONHANKHAU)/12,
      custom_thubq_weight = (((total_household_income - total_household_fee)*wt45) / SONHANKHAU)/12,
      total_household_income_weight = (business_income + salary_income) * wt45,
      total_household_fee_weight = total_household_fee * wt45
    ) 
  # vẫn có chút chênh lệch k đáng kể
  return(thongtinho_modified)
}

get_poverty_rate <- function(df) {
  return(
    df %>% mutate(
      poverty_status = case_when(
        (custom_thubq) / 30 < 2.15  *  8.721  ~ "extreme poverty",
        (custom_thubq) / 30 < 3.65  *  8.721 ~ "Poor",
        # (THUBQ) / 30 < 6.85  *  8.621 ~ "upper-middle",
        TRUE ~ "Normal"
      )
    ) %>% 
      select(poverty_status, wt45, SONHANKHAU) %>% 
      group_by(poverty_status) %>% 
      summarise(n_poverty = sum(wt45 * SONHANKHAU))%>% 
      mutate(pc = n_poverty / sum(.$n_poverty))
  )
}

poverty_theo_vung <- function(df) {
  list_vung <- list()
  lanhtho <- unique(df$lanhtho)
  for (i in 1:length(lanhtho)) {
    vung <- lanhtho[i]
    if (!is.na(vung)) {
      data <-  df[df$lanhtho == vung,]
      list_vung[[vung]] <- get_poverty_rate(data)
    }
    else {
      next
    }
  }
  return(
    list_vung
  )
}

get_area_poverty <- function(df) {
  table <- data.frame()
  for (i in 1:length(df)) {
    table <- rbind(
      table,
      df[[i]] %>% 
        select(
          poverty_status, pc
        ) %>% 
        pivot_wider(
          names_from = poverty_status,
          values_from = pc
        ) %>% 
        mutate(
          vung = names(df[i]),
          Normal = round(Normal, digits = 4),
          Poor = round(Poor, digits = 4),
          `extreme poverty` = round(`extreme poverty`, digits = 4)
        ) %>% 
        select(
          vung, Normal, Poor, `extreme poverty`
        )
    )
  }
  return(table)
}

  get_poverty_rate_tinh <- function(df) {
    return(
      df %>% mutate(
        poverty_status = case_when(
          (custom_thubq) / 30 < 2.15  *  8.721  ~ "extreme poverty",
          (custom_thubq) / 30 < 3.65  *  8.721 ~ "Poor",
          # (THUBQ) / 30 < 6.85  *  8.621 ~ "upper-middle",
          TRUE ~ "Normal"
        )
      ) %>% 
        select(tentinh, poverty_status, wt45, SONHANKHAU) %>% 
        group_by(tentinh, poverty_status) %>% 
        summarise(n_poverty = sum(wt45 * SONHANKHAU)) %>% 
        mutate(pc = n_poverty / sum(.$n_poverty))
    )
  }


gini_by_region <- function (df, region="") {
  if (region != "") {
    df2 <- df[df$mien == region, ]
  } else {
    df2 <- df
  }
  gini <- reldist::gini(df2$custom_thubq, df2$wt_household) * 100
  return(
    data.frame(
      "GINI" = gini
    )
  )
}

gini_by_province <- function (df, province="") {
  if (province != "") {
    df2 <- df[df$tentinh == province, ]
  } else {
    df2 <- df
  }
  gini <- reldist::gini(df2$custom_thubq, df2$wt_household) * 100
  return(gini)
}


# province <- unique(bf_tax$tentinh)
# gini <- sapply(
#   province,
#   function (province) gini_by_province(bf_tax, province = province)
# )
# gini_2 <- sapply(
#   province,
#   function (province) gini_by_province(af_tax, province = province)
# )
# 
# abc <- cbind(
#     data.frame(gini),
#     data.frame(gini_2)
#   ) %>% 
#   tibble::rownames_to_column(var = "tinh")
# # grouped column chart thể hiện thay đổi tỷ lệ hộ nghèo trước và sau thay đổi data
# bf_tax <- get_market_income()
# af_tax <- get_market_income(thue_doc_than = 20000)

# baseline_poverty <- get_poverty_rate(bf_tax)
# reformed_poverty <- get_poverty_rate(af_tax)

# poverty_population <- data.frame(
#   cbind(

#     select(baseline_poverty, poverty_status, n_poverty), 
#     select(reformed_poverty, poverty_status, n_poverty)
#   )
# ) %>% 
#   select(1,2, 4) %>% 
#   rename(
#     c(
#       "Tình trạng nghèo" = "poverty_status" ,
#       "baseline" = "n_poverty" ,
#       "reformed" = "n_poverty.1" 
#     )
#   )
# aft_tax_10 <- get_market_income(thue_doc_than = 10000)
# aft_tax_20 <- get_market_income(thue_doc_than = 20000)
# 
# base_line <- poverty_theo_vung(bf_tax)
# reformed_10tr <- poverty_theo_vung(aft_tax_10)
# reformed_20tr <- poverty_theo_vung(aft_tax_20)

# bs_pov <- get_poverty_rate(bf_tax) %>% 
#   select(poverty_status, pc) %>% 
#   filter(
#     poverty_status != "Normal"
#   )
# af_pov <- get_poverty_rate(af_tax) %>% 
#   select(poverty_status, pc) %>% 
#   filter(
#     poverty_status != "Normal"
#   )
# bs_pov$condition <- "baseline"
# af_pov$condition <- "reformed"
# compare_df <- rbind(bs_pov, af_pov)
# ggplot(
#   compare_df,
#   aes(x = pc, y = condition, fill = poverty_status)
# ) + 
#   geom_bar(
#     position = "dodge", stat = "identity"
#   )



# tax_plot <- c()
# for (bach_tax in seq(0, 20000, by = 1000)) {
#     print(paste0("At ", bach_tax))
#     df <- get_market_income(thue_doc_than = bach_tax) %>%
#       get_poverty_rate()
#     df$tax_amount <- bach_tax
#     tax_plot <- rbind(tax_plot, df)
#     write_csv(tax_plot, "tax_tier.csv")
# }

# merged_tax <- read_csv("./tax_tier.csv") # data vẽ biểu đồ tax qua các mức
# # 
# poverty_levels <- merged_tax %>%
#   group_by(tax_amount, poverty_status) %>%
#   summarise(
#     value = sum(pc)
#   ) %>%
#   mutate(
#     poverty_status = factor(
#       poverty_status, levels = c("Normal", "Poor", "extreme poverty")
#     )
#   )

# ggplot(
#   poverty_levels %>% filter(poverty_status != "Normal"),
#   aes(x = tax_amount, y = value * 100, fill = poverty_status)
# ) +
#   geom_area() +
#   labs(
#     y = "% Population",
#     x = "Tax amount (1000 VND)",
#     fill = "Poverty levels"
#   )






# -------------------------------------------------------------------------
# 
# base_mk <- get_market_income()
# base_gov <- get_gov_income()
# 
# baseline <- vnmod(
#   base_market_inc = base_mk,
#   base_gov_income = base_gov,
#   bachelortax = 0
# )$pov %>%
#   group_by(tentinh, wb_poverty) %>%
#   summarise(
#     count = sum(wt_household)
#   )
# baseline$tax_amount <- 0
# 
# a <- baseline %>%
#   group_by(wb_poverty) %>%
#   summarise(
#     sum = sum(count)
#   )
# 
# (a[a$wb_poverty == "extreme poverty", "sum"] + 
#     a[a$wb_poverty == "lower-middle", "sum"]) / sum(a$sum)
# 
# # Vẽ biểu đồ miền thay đổi về số người nghèo trc và sau thuế --------------
# 
# # Lấy dữ liệu vẽ biểu đồ
# # baseline_2 <- baseline
# # for (bach_tax in seq(1000, 20000, by = 1000)) {
# #   print(paste0("At ", bach_tax))
# #   test <- vnmod(
# #     base_market_inc = base_mk,
# #     base_gov_income = base_gov,
# #     bachelortax = bach_tax
# #   )$pov %>%
# #     group_by(tentinh, wb_poverty) %>%
# #     summarise(
# #       count = sum(wt_household)
# #     )
# #   test$tax_amount <- bach_tax
# #   baseline_2 <- rbind(baseline_2, test)
# #   write_csv(baseline_2, "multiple_tax_tier.csv")
# # }
# 
# merged_tax <- read_csv("./multiple_tax_tier.csv") # data vẽ biểu đồ tax qua các mức
# poverty_levels <- merged_tax %>%
#  group_by(tax_amount, wb_poverty) %>%
#  summarise(
#    value = sum(count)
#  ) %>%
#  group_by(tax_amount) %>%
#  summarise(
#    percent = value / sum(value),
#    across()
#  ) %>%
#  mutate(
#    wb_poverty = factor(
#      wb_poverty, levels = c("normal", "upper-middle", "lower-middle", "extreme poverty")
#    )
#  )
# 
# ggplot(
#  poverty_levels %>% filter(wb_poverty != "normal") ,
#  aes(x = tax_amount, y = percent, fill = wb_poverty)
# ) + 
# geom_area() +
# labs(
#  y = "Population",
#  x = "Tax amount",
#  fill = "Poverty levels"
# )
# 
# 
# # Export dữ liệu trước và sau thuế ----------------------------------------
# 
# 
# test <- vnmod(
#   base_market_inc = base_mk,
#   base_gov_income = base_gov
# )
# 
# write_csv(
#   test$pov %>%
#     group_by(tentinh, tenhuyen, tenxa, tendb, wb_poverty) %>%
#     summarise(
#       count = sum(wt_household)
#     ),
#   "wb_poverty_data.csv"
# )
# 
# test_2 <- vnmod(
#   base_market_inc = base_mk,
#   base_gov_income = base_gov,
#   bachelortax = 10000
# )
# 
# write_csv(
#   test_2$pov %>%
#     group_by(tentinh, tenhuyen, tenxa, tendb, wb_poverty) %>%
#     summarise(
#       count = sum(wt_household)
#     ),
#   "wb_poverty_data_after_tax.csv"
# )
