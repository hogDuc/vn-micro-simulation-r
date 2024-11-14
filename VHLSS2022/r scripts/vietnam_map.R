setwd('C:/Users/ADMIN/Desktop/SciEcons/UKMOD/VHLSS 2022/VHLSS2022')
library(DT)
library(shiny)
library(bslib)
library(ggplot2)
library(sf)
source("../test.R")

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
      summarise(n_poverty = sum(wt45 * SONHANKHAU))%>% 
      mutate(pc = n_poverty / sum(.$n_poverty))
  )
}

baseline <- get_market_income()
pov <- get_poverty_rate_tinh(baseline) %>% select(
  -c("n_poverty")
) %>% filter(
    poverty_status != "Normal"
  )

data <- left_join(
  vietnam,
  pov,
  by = c("Name_VI" = "tentinh"),
  relationship = "many-to-many"
)

ggplot(data) + 
  geom_sf(aes(fill = pc)) +
  scale_fill_gradient(
    low = "lightblue", 
    high = "darkblue",
    na.value = "grey",
    # breaks = c(0.025, 0.1)
  ) +
  theme_void() +
  theme(
    legend.position = c(0.78, 0.82),
    legend.title = element_text(size=9),
    plot.background = element_rect(fill='white', colour='white')
  )
