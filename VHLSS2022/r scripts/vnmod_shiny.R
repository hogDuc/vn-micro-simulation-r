setwd('C:/Users/ADMIN/Desktop/SciEcons/UKMOD/VHLSS 2022/VHLSS2022')
library(DT)
library(shiny)
library(bslib)
library(shinyjs)
source("../test.R")

# Đọc dữ liệu

ui <- page_sidebar(
  tags$script(HTML(
    "document.addEventListener('wheel', function(event){
        if(document.activeElement.type === 'number'){
            document.activeElement.blur();
        }
     });"
  )),
  title = "",
  sidebar = sidebar(
    useShinyjs(),
    style = "font-size: 15px",
    div(
        style = "display: flex; gap: 10px",
        actionButton(
          inputId = "submit",
          label = "Submit"
        ),
        actionButton(
          inputId = "reset",
          label = "Reset"
        )
    ),
    navset_card_tab(
      nav_panel(
        title = "Trợ cấp, đền bù",
        numericInput(
          "tc_thatnghiep",
          label = "Tỷ lệ thay đổi các khoản trợ cấp thất nghiệp",
          value = 0
        ),
        numericInput(
          "tc_thoiviec",
          label = "Tỷ lệ thay đổi các khoản trợ cấp thôi việc 1 lần",
          value = 0
        ),
        numericInput(
          "tc_matsuc",
          label = "Tỷ lệ thay đổi các khoản trợ cấp mất sức lao động",
          value = 0
        ),
        numericInput(
          "tc_baotro",
          label = "Tỷ lệ thay đổi các khoản trợ cấp các đối tượng bảo trợ xã hội",
          value = 0
        ),
        numericInput(
          "tc_thuongbinh",
          label = "Tỷ lệ thay đổi các khoản trợ cấp thương binh, liệt sỹ",
          value = 0
        ),
        numericInput(
          "tc_thientai",
          label = "Tỷ lệ thay đổi các khoản trợ cấp khắc phục thiên tai, dịch bệnh",
          value = 0
        ),
        numericInput(
          "db_trongtrot",
          label = "Tỷ lệ thay đổi các khoản đền bù trồng trọt",
          value = 0
        ),
        numericInput(
          "db_channuoi",
          label = "Tỷ lệ thay đổi các khoản đền bù về chăn nuôi",
          value = 0
        ),
        numericInput(
          "db_dvnn",
          label = "Tỷ lệ thay đổi các khoản đền bù dịch vụ nông nghiệp",
          value = 0
        ),
        numericInput(
          "db_trongrung",
          label = "Tỷ lệ thay đổi các khoản đền bù trồng rừng, lâm nghiệp",
          value = 0
        ),
        numericInput(
          "db_thuysan",
          label = "Tỷ lệ thay đổi các khoản đền bù về nuôi trồng thủy sản",
          value = 0
        )
      ),
      nav_panel(
        title = "Lương",
        numericInput(
          "luong_bt",
          label = "Tỷ lệ thay đổi lương nghỉ hưu thông thường",
          value = 0
        ),
        numericInput(
          "luong_som",
          label = "Tỷ lệ thay đổi lương nghỉ hưu sớm",
          value = 0
        )
      ),
      nav_panel(
        title = "Thuế",
        numericInput(
          "t_docthan",
          label = "Thuế độc thân (nghìn đồng / năm)",
          value = 0
        ),
        numericInput(
          "t_channuoi",
          label = "Tỷ lệ thay đổi về thuế chăn nuôi",
          value = 0
        ),
        numericInput(
          "t_dvnn",
          label = "Tỷ lệ thay đổi về thuế kinh doanh dịch vụ nông nghiệp",
          value = 0
        ),
        numericInput(
          "t_lamnghiep",
          label = "Tỷ lệ thay đổi về thuế lâm nghiệp",
          value = 0
        ),
        numericInput(
          "t_kdlamnghiep",
          label = "Tỷ lệ thay đổi về thuế kinh doanh lâm nghiệp",
          value = 0
        ),
        numericInput(
          "t_channuoi",
          label = "Tỷ lệ thay đổi về thuế chăn nuôi",
          value = 0
        ),
        numericInput(
          "income_tax_1",
          label = "% thuế TNCN mức 1",
          value = 0.05
        ),
        numericInput(
          "income_tax_2",
          label = "% thuế TNCN mức 2",
          value = 0.1
        ),
        numericInput(
          "income_tax_3",
          label = "% thuế TNCN mức 3",
          value = 0.15
        ),
        numericInput(
          "income_tax_4",
          label = "% thuế TNCN mức 4",
          value = 0.2
        ),
        numericInput(
          "income_tax_5",
          label = "% thuế TNCN mức 5",
          value = 0.25
        ),
        numericInput(
          "income_tax_6",
          label = "% thuế TNCN mức 6",
          value = 0.3
        ),
        numericInput(
          "income_tax_7",
          label = "% thuế TNCN mức 7",
          value = 0.35
        ),
        numericInput(
          "threshold_1",
          label = "Mức thu nhập chịu thuế 1",
          value = 5000
        ),
        numericInput(
          "threshold_2",
          label = "Mức thu nhập chịu thuế 2",
          value = 10000
        ),
        numericInput(
          "threshold_3",
          label = "Mức thu nhập chịu thuế 3",
          value = 18000
        ),
        numericInput(
          "threshold_4",
          label = "Mức thu nhập chịu thuế 4",
          value = 32000
        ),
        numericInput(
          "threshold_5",
          label = "Mức thu nhập chịu thuế 5",
          value = 52000
        ),
        numericInput(
          "threshold_6",
          label = "Mức thu nhập chịu thuế 6",
          value = 80000
        )
      )
    ),
    width = "30%"
  ),
    navset_card_tab(
      title = "Kết quả",
      nav_panel(
        "Thu nhập, chi tiêu",
        p("Tổng thu toàn thị trường"),
        tableOutput(
          "market_income"
        ),
        p("Tổng thu chính phủ"),
        tableOutput(
          "government_income"
        ),
        p("Tổng chi chính phủ"),
        tableOutput(
          "government_expenditure"
        )
      ),
      nav_panel(
        "GINI",
        tableOutput(
          "vietnam_gini"
        ),
        tableOutput(
          "gini_mien"
        ),
        tableOutput(
          "gini_tinh"
        )
      ),
      nav_panel(
        "Tỷ lệ nghèo",
        layout_columns(
          card(
            card_header(
              "Tỷ lệ nghèo toàn quốc"
            ),
            card_body(
              plotOutput(
                "compare_overall_poverty"
              )
            )
          ),
          card(
            card_header(
              "Dân số nghèo toàn quốc"
            ),
            card_body(
              tableOutput("compare_poverty_population")
            )
          )
        ),
        layout_columns(
          card(
            card_header(
              "Bản đồ tỷ lệ nghèo Việt Nam (baseline)"
            ),
            card_body(
              plotOutput(
                "vietnam_map_baseline"
              )
            )
          ),
          card(
            card_header(
              "Bản đồ tỷ lệ nghèo Việt Nam (reformed)"
            ),
            card_body(
              plotOutput("vietnam_map_reformed")
            )
          )
        )
      )
    )
  )

server <- function(input, output) {
  baseline <- get_market_income() 
  baseline_poverty <- get_poverty_rate(baseline) 
  baseline_poverty_2 <- baseline_poverty %>% 
    select(poverty_status, pc) %>% 
    filter(
      poverty_status != "Normal"
    )
  baseline_vung_poverty <- poverty_theo_vung(baseline)
  baseline_poverty_2$condition <- "baseline"
  
  map_baseline <- get_poverty_rate_tinh(baseline)
  
  region_1 <- unique(baseline$mien)
  gini <- sapply(
    region_1,
    function (region_1) gini_by_region(baseline, region = region_1)
  ) %>% t
  
  province_1 <- unique(baseline$tentinh)
  gini_tinh_baseline <- sapply(
    province_1, 
    function (province_1) gini_by_province(baseline, province = province_1)
  ) 
  
  observeEvent(input$submit, {
    reformed <- get_market_income(
      tc_that_nghiep = input$tc_thatnghiep, tc_thoi_viec = input$tc_thoiviec, 
      luong_bt = input$luong_bt, luong_som = input$luong_som, 
      tc_mat_suc = input$tc_matsuc,
      tc_thuong_binh = input$tc_thuongbinh, tc_bao_tro_xh = input$tc_baotro, 
      tc_thien_tai = input$tc_thientai,
      db_trong_trot = input$db_trongtrot, db_chan_nuoi = input$db_channuoi, 
      db_dvnn = input$db_dvnn, db_trong_rung = input$db_trongrung, 
      db_nuoi_thuy_san = input$db_thuysan,
      thue_chan_nuoi = input$t_channuoi, thue_dvnn = input$t_dvnn, 
      thue_lam_nghiep = input$t_lamnghiep, thue_kd_lam_nghiep = input$t_kdlamnghiep, 
      thue_doc_than = input$t_docthan,
      inc_tax_1 = input$income_tax_1, inc_tax_2 = input$income_tax_2, 
      inc_tax_3 = input$income_tax_3, inc_tax_4 = input$income_tax_4, 
      inc_tax_5 = input$income_tax_5, inc_tax_6 = input$income_tax_6, 
      inc_tax_7 = input$income_tax_7,
      inc_threshold_1 = input$threshold_1, inc_threshold_2 = input$threshold_2, 
      inc_threshold_3 = input$threshold_3, inc_threshold_4 = input$threshold_4,
      inc_threshold_5 = input$threshold_5, inc_threshold_6 = input$threshold_6
    )
    reformed_poverty <- get_poverty_rate(reformed) 
    reformed_poverty_2 <- reformed_poverty %>% 
      select(poverty_status, pc) %>% 
      filter(
        poverty_status != "Normal"
      )
    reformed_poverty_2$condition <- "reformed"
    
    compare_poverty <- rbind(
      baseline_poverty_2, reformed_poverty_2
    )
    compare_poverty_plot <- ggplot(
      compare_poverty,
      aes(x = pc, y = condition, fill = poverty_status)
    ) + 
      geom_bar(
        position = "dodge", stat = "identity"
      ) + 
      theme(legend.position = "bottom") + 
      coord_flip()
    
    reformed_vung_poverty <- poverty_theo_vung(reformed)
    
    market_income <- data.frame(
      "category" = c(
        "Tổng thu toàn thị trường", "Thu nhập (Tổng thu - chi phí) toàn thị trường"
      ),
      "baseline_million" = c(
        sum(baseline$total_household_income_weight) / 1000,
        sum(
          baseline$total_household_income_weight - baseline$total_household_fee_weight
        ) / 1000
      ),
      "reformed_million" = c(
        sum(reformed$total_household_income_weight) / 1000,
        sum(
          reformed$total_household_income_weight - reformed$total_household_fee_weight
        ) / 1000
      )
    ) %>% mutate(
      diff = reformed_million - baseline_million
    )
    
    government_income <- data.frame(
        "category" = c(
          "Tổng thu nhập chính phủ", "Thuế TNCN từ lương", "Thuế kinh doanh chăn nuôi",
          "Thuế dịch vụ nông nghiệp", "Thuế lâm nghiệp", "Thuế kinh doanh lâm nghiệp",
          "Thuế độc thân"
        ),
        "baseline_million" = c(
          sum(
            sum(baseline$salary_tax * baseline$wt45),
            sum(baseline$M4B22_C17_2 * baseline$wt45),
            sum(baseline$M4B32_C15_2 * baseline$wt45),
            sum(baseline$M4B42_C12_2 * baseline$wt45),
            sum(baseline$M4B52_C17_2 * baseline$wt45),
            sum(baseline$bachelor_tax * baseline$wt45)
          ),
          sum(baseline$salary_tax * baseline$wt45),
          sum(baseline$M4B22_C17_2 * baseline$wt45),
          sum(baseline$M4B32_C15_2 * baseline$wt45),
          sum(baseline$M4B42_C12_2 * baseline$wt45),
          sum(baseline$M4B52_C17_2 * baseline$wt45),
          sum(baseline$bachelor_tax * baseline$wt45)
        ),
        "reformed_million" = c(
          sum(
            sum(reformed$salary_tax * reformed$wt45),
            sum(reformed$M4B22_C17_2 * reformed$wt45),
            sum(reformed$M4B32_C15_2 * reformed$wt45),
            sum(reformed$M4B42_C12_2 * reformed$wt45),
            sum(reformed$M4B52_C17_2 * reformed$wt45),
            sum(reformed$bachelor_tax * reformed$wt45)
          ),
          sum(reformed$salary_tax * reformed$wt45),
          sum(reformed$M4B22_C17_2 * reformed$wt45),
          sum(reformed$M4B32_C15_2 * reformed$wt45),
          sum(reformed$M4B42_C12_2 * reformed$wt45),
          sum(reformed$M4B52_C17_2 * reformed$wt45),
          sum(reformed$bachelor_tax * reformed$wt45)
        )
      ) %>% 
      mutate(
        diff = reformed_million - baseline_million
      )
    
    government_expenditure <- data.frame(
      "category" = c(
        "Tổng chi tiêu chính phủ",
        "Trợ cấp thất nghiệp, thôi việc 1 lần và trợ cấp mất sức lao động",
        "Đền bù trồng trọt",
        "Đền bù hoạt động chăn nuôi",
        "Đền bù dịch vụ nông nghiệp",
        "Đền bù hoạt động trồng rừng",
        "Đền bù hoạt động nuôi, trồng thủy sản",
        "Trợ cấp thương binh, liệt sỹ",
        "Trợ cấp các đối tượng bảo trợ xã hội",
        "Trợ cấp khắc phục thiên tai, dịch bệnh"
      ),
      "baseline_million" = c(
        sum(
          sum(baseline$tro_cap_personal * baseline$wt45, na.rm = TRUE),
          sum(baseline$M4B1T2 * baseline$wt45, na.rm = TRUE),
          sum(baseline$M4B2T2 * baseline$wt45, na.rm = TRUE),
          sum(baseline$M4B3T2 * baseline$wt45, na.rm = TRUE),
          sum(baseline$M4B4T2 * baseline$wt45, na.rm = TRUE),
          sum(baseline$M4B5T2 * baseline$wt45, na.rm = TRUE),
          sum(baseline$M4D_04 * baseline$wt45, na.rm = TRUE),
          sum(baseline$M4D_05 * baseline$wt45, na.rm = TRUE),
          sum(baseline$M4D_06 * baseline$wt45, na.rm = TRUE)
        ),
        sum(baseline$tro_cap_personal * baseline$wt45, na.rm = TRUE),
        sum(baseline$M4B1T2 * baseline$wt45, na.rm = TRUE),
        sum(baseline$M4B2T2 * baseline$wt45, na.rm = TRUE),
        sum(baseline$M4B3T2 * baseline$wt45, na.rm = TRUE),
        sum(baseline$M4B4T2 * baseline$wt45, na.rm = TRUE),
        sum(baseline$M4B5T2 * baseline$wt45, na.rm = TRUE),
        sum(baseline$M4D_04 * baseline$wt45, na.rm = TRUE),
        sum(baseline$M4D_05 * baseline$wt45, na.rm = TRUE),
        sum(baseline$M4D_06 * baseline$wt45, na.rm = TRUE)
      ),
      "reformed_million" = c(
        sum(
          sum(reformed$tro_cap_personal * reformed$wt45, na.rm = TRUE),
          sum(reformed$M4B1T2 * reformed$wt45, na.rm = TRUE),
          sum(reformed$M4B2T2 * reformed$wt45, na.rm = TRUE),
          sum(reformed$M4B3T2 * reformed$wt45, na.rm = TRUE),
          sum(reformed$M4B4T2 * reformed$wt45, na.rm = TRUE),
          sum(reformed$M4B5T2 * reformed$wt45, na.rm = TRUE),
          sum(reformed$M4D_04 * reformed$wt45, na.rm = TRUE),
          sum(reformed$M4D_05 * reformed$wt45, na.rm = TRUE),
          sum(reformed$M4D_06 * reformed$wt45, na.rm = TRUE)
        ),
        sum(reformed$tro_cap_personal * reformed$wt45, na.rm = TRUE),
        sum(reformed$M4B1T2 * reformed$wt45, na.rm = TRUE),
        sum(reformed$M4B2T2 * reformed$wt45, na.rm = TRUE),
        sum(reformed$M4B3T2 * reformed$wt45, na.rm = TRUE),
        sum(reformed$M4B4T2 * reformed$wt45, na.rm = TRUE),
        sum(reformed$M4B5T2 * reformed$wt45, na.rm = TRUE),
        sum(reformed$M4D_04 * reformed$wt45, na.rm = TRUE),
        sum(reformed$M4D_05 * reformed$wt45, na.rm = TRUE),
        sum(reformed$M4D_06 * reformed$wt45, na.rm = TRUE)
      ) 
    ) %>% 
      mutate(
        diff = reformed_million - baseline_million
      )
    
    poverty_population <- data.frame(
      cbind(
        select(baseline_poverty, poverty_status, n_poverty), 
        select(reformed_poverty, poverty_status, n_poverty)
      )
    ) %>% 
      select(1,2, 4) %>% 
      rename(
        c(
          "Tình trạng nghèo" = "poverty_status" ,
          "baseline" = "n_poverty" ,
          "reformed" = "n_poverty.1" 
        )
      ) %>% 
      mutate(
        diff = reformed - baseline
      )
    
    # Ve map Vietnam
    map_reformed <- get_poverty_rate_tinh(reformed) %>% select(
      -c("n_poverty")
    ) %>% filter(
      poverty_status != "Normal"
    )
    map_baseline <- map_baseline %>% select(
      -c("n_poverty")
    ) %>% filter(
      poverty_status != "Normal"
    )
    
    data_map_baseline <- left_join(
      vietnam,
      map_baseline,
      by = c("Name_VI" = "tentinh"),
      relationship = "many-to-many"
    )
    data_map_reformed <- left_join(
      vietnam,
      map_reformed,
      by = c("Name_VI" = "tentinh"),
      relationship = "many-to-many"
    )
    
    vietnam_map_base <- ggplot(data_map_baseline) + 
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
    
    vietnam_map_ref <- ggplot(data_map_reformed) + 
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
    
    # Bang tinh GINI
    gini_vietnam <- cbind(
      gini_by_region(baseline) %>% 
        rename(
          c("baseline" = "GINI")
        ),
      gini_by_region(reformed) %>% 
        rename(
          c("reformed" = "GINI")
        )
    )
    
    region_2 <- unique(reformed$mien)
    gini_2 <- sapply(
      region_2,
      function (region_2) gini_by_region(reformed, region = region_2)
    ) %>% t
    
    mien_gini <- data.frame(rbind(gini, gini_2))
    mien_gini$type <-  c("baseline", "reformed")

    province_2 <- unique(reformed$tentinh)
    gini_tinh_reformed <- sapply(
      province_2, 
      function (province_2) gini_by_province(reformed, province = province_2)
    )
    
    table_gini_tinh <- cbind(
      data.frame(gini_tinh_baseline),
      data.frame(gini_tinh_reformed)
    ) %>% 
    tibble::rownames_to_column(var = "tinh") %>% 
      mutate(
        diff = gini_tinh_reformed - gini_tinh_baseline
      )
    
    output$market_income <- renderTable(market_income)
    output$government_income <- renderTable(government_income)
    output$government_expenditure <- renderTable(government_expenditure)
    output$compare_overall_poverty <- renderPlot(compare_poverty_plot)
    output$compare_poverty_population <- renderTable(poverty_population)
    output$vietnam_map_baseline <- renderPlot(vietnam_map_base)
    output$vietnam_map_reformed <- renderPlot(vietnam_map_ref)
    output$vietnam_gini <- renderTable(gini_vietnam)
    output$gini_mien <- renderTable(mien_gini)
    output$gini_tinh <- renderTable(table_gini_tinh)
  })
  observeEvent(input$reset, {
    reset("tc_thatnghiep")
    reset("tc_thoiviec")
    reset("luong_bt")
    reset("luong_som") 
    reset("tc_matsuc")
    reset("tc_thuongbinh")
    reset("tc_baotro") 
    reset("tc_thientai")
    reset("db_trongtrot")
    reset("db_channuoi") 
    reset("db_dvnn")
    reset("db_trongrung") 
    reset("db_thuysan")
    reset("t_channuoi")
    reset("t_dvnn") 
    reset("t_lamnghiep")
    reset("t_kdlamnghiep") 
    reset("t_docthan")
    reset("income_tax_1")
    reset("income_tax_2") 
    reset("income_tax_3")
    reset("income_tax_4") 
    reset("income_tax_5")
    reset("income_tax_6") 
    reset("income_tax_7")
    reset("threshold_1")
    reset("threshold_2") 
    reset("threshold_3")
    reset("threshold_4")
    reset("threshold_5")
    reset("threshold_6")
  })
}

shinyApp(ui, server)

