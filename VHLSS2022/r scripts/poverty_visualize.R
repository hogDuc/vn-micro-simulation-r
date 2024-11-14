setwd('C:/Users/ADMIN/Desktop/SciEcons/UKMOD/VHLSS 2022/VHLSS2022')
library(ggplot2)
library(scales)
library(dplyr)
library(reshape)
library(formattable)
library(data.table)
library(flextable)
pre_tax <- read.csv("./wb_poverty_data.csv")
post_tax <- read.csv("./wb_poverty_data_after_tax.csv")

View(pre_tax)
View(post_tax) # 10tr / year / single

create_pie_chart <- function(data) {
  return(
    data %>% 
      group_by(wb_poverty) %>% 
      summarise(
        count = sum(count)
      ) %>% 
      mutate(
        wb_poverty = factor(
          wb_poverty,
          levels = c(
            "normal", "upper-middle", "lower-middle", "extreme poverty" 
          ),
          ordered = TRUE
        )
      ) %>% 
      ggplot(
        aes(x = "", y = count, fill = wb_poverty)
      ) + 
      geom_bar(
        stat = "identity",
        position = "dodge",
        width = 1
      ) + 
      # coord_polar(
      #   "y", start = 0
      # ) +
      theme_minimal() + 
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank() 
      # ) + 
      # geom_text(
      #   aes(label = count/1000),
      #   size = 1.5
      ) + 
      scale_y_continuous(
        labels = label_comma()
      ) + 
      guides(
        fill = guide_legend(title = "Poverty levels")
      )
  )
}

create_pie_chart(pre_tax)
create_pie_chart(post_tax)


# -------------------------------------------------------------------------



compare_df <- full_join(
  pre_tax, post_tax,
  by = c("tentinh", "wb_poverty")
) %>%
  mutate(
    diff = count.y - count.x
  ) %>% 
  melt(
    id.vars = c("tentinh", "wb_poverty")
  ) %>% 
  mutate(
    value = case_when(is.na(value) ~ 0, TRUE ~ value),
    variable = case_when(
      variable == "count.x" ~ "before tax",
      variable == "count.y" ~ "after tax",
      TRUE ~ variable
    ),
    variable = factor(
      variable, levels = c("before tax", "after tax", "diff"), 
      order = TRUE
    ),
    wb_poverty = factor(
      wb_poverty,
      levels = c("extreme poverty", "lower-middle", "upper-middle", "normal")
    )
  )

# compare_df[is.na(compare_df)] <- 0

ggplot(
  # data = compare_df[compare_df$wb_poverty != "normal" & compare_df$variable != "diff", ] %>%
  data = compare_df[compare_df$variable != "diff", ] %>%
  # data = compare_df %>% 
    group_by(wb_poverty, variable) %>% 
    summarise(
      value = sum(value)
    ),
  aes(
    x = wb_poverty,
    y = value,
    fill = variable
  )
) +
  geom_bar(
    position = "dodge",
    stat = "identity"
  ) + 
  labs(
    x = "Income groups",
    y = "Population",
    fill = "Poverty rate"
  ) +
  scale_fill_manual(values = c("#F05A7E", "#0B8494"))
  
# -------------------------------------------------------------------------

compare_df_no_normal <- compare_df[compare_df$wb_poverty != "normal",] 
df <- compare_df_no_normal %>% 
  group_by(wb_poverty, variable) %>% 
  summarise(population = sum(value))

header_1 <- df$wb_poverty
header_2 <- df$variable
df <- transpose(df)[-c(1,2),]

flextable(df) %>% 
  delete_part(part = "header") %>% 
  add_header_row(
    values = header_2
  ) %>% 
  add_header_row(
    values = header_1
  ) %>% 
  merge_h(
    part = "header", i = 1
  ) %>% 
  merge_h() %>% 
  theme_booktabs(bold_header = TRUE) %>% 
  align(
    align = "center",
    part = "all"
  ) %>% 
  vline(
    j = c(2, 4),
    border =  fp_border_default()
  ) %>% 
  hline(
    i = 1
  )
