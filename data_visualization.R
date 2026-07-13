install.packages(c("shiny", "ggplot2", "dplyr", "DT", "bslib"))


library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(bslib)

ui <- page_sidebar(
  theme = bs_theme(
    bootswatch = "minty",
    primary = "#4E79A7",
    base_font = font_google("Noto Sans KR")
  ),
  title = "📊 데이터 자동 시각화",
  
  sidebar = sidebar(
    fileInput("file", "CSV 파일 업로드",
              accept = ".csv",
              buttonLabel = "파일 선택",
              placeholder = "파일을 선택하세요"),
    hr(),
    uiOutput("chart_selector"),
    uiOutput("col_selectors"),
    hr(),
    downloadButton("download_plot", "📥 그래프 저장", class = "btn-primary w-100")
  ),
  
  navset_card_tab(
    nav_panel("📈 그래프", plotOutput("plot", height = "480px")),
    nav_panel("📋 데이터", DTOutput("table")),
    nav_panel("📝 요약통계", verbatimTextOutput("summary"))
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, fileEncoding = "UTF-8-BOM")
  })
  
  output$chart_selector <- renderUI({
    req(data())
    radioButtons("chart_type", "차트 종류",
                 choices = c("막대그래프"      = "bar",
                             "점 그래프"       = "point",
                             "박스플롯"        = "box",
                             "빈도 히스토그램" = "hist"),
                 selected = "bar")
  })
  
  output$col_selectors <- renderUI({
    req(data(), input$chart_type)
    df   <- data()
    cols     <- names(df)
    num_cols <- names(df)[sapply(df, is.numeric)]
    
    facet_ui <- selectInput("facet_col", "📐 패싯 변수 (선택)",
                            choices = c("없음" = "", cols))
    
    if (input$chart_type == "hist") {
      tagList(
        checkboxGroupInput("y_cols", "변수 선택 (여러 개 가능)",
                           choices = num_cols, selected = num_cols[1]),
        facet_ui
      )
    } else {
      tagList(
        selectInput("x_col", "X축 변수", choices = cols),
        checkboxGroupInput("y_cols", "Y축 변수 (여러 개 가능)",
                           choices = num_cols, selected = num_cols[1]),
        # Y변수 1개일 때만 색상 구분 의미 있음
        conditionalPanel(
          condition = "input.y_cols.length === 1",
          selectInput("color_col", "색상 구분 (선택)",
                      choices = c("없음" = "", cols))
        ),
        facet_ui
      )
    }
  })
  
  make_plot <- reactive({
    req(data(), input$y_cols, input$chart_type)
    df        <- data()
    facet_var <- if (!is.null(input$facet_col) && input$facet_col != "") input$facet_col else NULL
    multi_y   <- length(input$y_cols) > 1
    
    # ---------- 히스토그램 ----------
    if (input$chart_type == "hist") {
      cols_sel <- input$y_cols
      if (!is.null(facet_var)) cols_sel <- c(cols_sel, facet_var)
      
      df_long <- df %>%
        select(all_of(unique(cols_sel))) %>%
        pivot_longer(cols = all_of(input$y_cols),
                     names_to = "variable", values_to = "value")
      
      p <- ggplot(df_long, aes(x = value, fill = variable)) +
        geom_histogram(bins = 30, alpha = 0.7, color = "white", position = "identity") +
        labs(title = "빈도 분포", x = "값", y = "빈도수", fill = "변수")
      
      # ---------- 나머지 차트 ----------
    } else {
      req(input$x_col)
      
      if (multi_y) {
        # 여러 Y변수 → pivot_longer로 long format 변환
        cols_sel <- unique(c(input$x_col, input$y_cols, facet_var))
        df_long  <- df %>%
          select(all_of(cols_sel)) %>%
          rename(x_val = all_of(input$x_col)) %>%
          pivot_longer(cols = all_of(input$y_cols),
                       names_to = "variable", values_to = "value")
        
        p <- ggplot(df_long, aes(x = x_val, y = value,
                                 fill = variable, color = variable))
      } else {
        # Y변수 1개
        color_var <- if (!is.null(input$color_col) && input$color_col != "") input$color_col else NULL
        p <- ggplot(df, aes_string(x = input$x_col, y = input$y_cols,
                                   fill = color_var, color = color_var))
      }
      
      p <- switch(input$chart_type,
                  "bar"   = p + geom_bar(stat = "summary", fun = "mean",
                                         position = "dodge", alpha = 0.85),
                  "point" = p + geom_point(size = 3, alpha = 0.7),
                  "box"   = p + geom_boxplot(alpha = 0.7)
      )
      
      y_label <- if (multi_y) "값" else input$y_cols
      p <- p +
        labs(title = paste(input$x_col, "vs", paste(input$y_cols, collapse = ", ")),
             x = input$x_col, y = y_label, fill = "변수", color = "변수") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    # 패싯 적용
    if (!is.null(facet_var)) {
      p <- p + facet_wrap(as.formula(paste("~", facet_var)), scales = "free_y")
    }
    
    p + theme_minimal(base_size = 14)
  })
  
  output$plot <- renderPlot({ make_plot() })
  
  output$download_plot <- downloadHandler(
    filename = function() paste0("plot_", Sys.Date(), ".png"),
    content  = function(file) {
      ggsave(file, plot = make_plot(), width = 10, height = 6, dpi = 300)
    }
  )
  
  output$table <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$summary <- renderPrint({
    req(data())
    summary(data())
  })
}

shinyApp(ui, server)
