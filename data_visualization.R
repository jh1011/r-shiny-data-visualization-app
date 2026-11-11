install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "DT", "bslib", "readxl", "writexl"))

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(bslib)
library(readxl)
library(writexl)

# 결측값을 포함한 필터 선택지 표기
as_filter_text <- function(x) {
  out <- as.character(x)
  out[is.na(x)] <- "(결측값)"
  out
}

# 집계 함수: 전체가 NA이면 count를 제외하고 NA 반환
aggregate_values <- function(x, method) {
  good <- x[!is.na(x)]
  if (method == "count") return(as.numeric(length(good)))
  if (length(good) == 0) return(NA_real_)
  switch(method,
    mean = mean(good),
    sum = sum(good),
    median = median(good),
    stop("지원하지 않는 집계 방법입니다.")
  )
}

safe_sd <- function(x) {
  if (sum(!is.na(x)) < 2) return(NA_real_)
  sd(x, na.rm = TRUE)
}

ui <- page_sidebar(
  theme = bs_theme(
    bootswatch = "minty",
    primary = "#4E79A7",
    base_font = font_google("Noto Sans KR")
  ),
  title = "📊 데이터 자동 시각화 & 집계",
  sidebar = sidebar(
    width = 350,
    fileInput(
      "file", "CSV 또는 Excel 파일 업로드",
      accept = c(".csv", ".xlsx"),
      buttonLabel = "파일 선택",
      placeholder = "CSV / XLSX 파일을 선택하세요"
    ),
    uiOutput("file_options"),
    hr(),
    radioButtons(
      "chart_type", "차트 종류",
      choices = c(
        "선 그래프" = "line",
        "막대그래프" = "bar",
        "점 그래프" = "point",
        "박스플롯" = "box",
        "빈도 히스토그램" = "hist"
      ),
      selected = "line"
    ),
    uiOutput("col_selectors"),
    hr(),
    uiOutput("aggregation_controls"),
    hr(),
    uiOutput("filter_controls"),
    hr(),
    downloadButton("download_plot", "📥 그래프 PNG 저장", class = "btn-primary w-100")
  ),
  navset_card_tab(
    nav_panel(
      "📈 그래프",
      plotOutput("plot", height = "530px"),
      textOutput("plot_note")
    ),
    nav_panel("📋 원자료 (필터 적용)", DTOutput("raw_table")),
    nav_panel(
      "🧮 집계 결과",
      p("그래프 생성에 사용되는 데이터입니다. 박스플롯과 히스토그램은 원자료를 사용합니다."),
      DTOutput("result_table"),
      div(
        class = "d-flex gap-2 flex-wrap mt-3",
        downloadButton("download_csv", "집계 결과 CSV 다운로드"),
        downloadButton("download_xlsx", "집계 결과 XLSX 다운로드")
      )
    ),
    nav_panel("📝 요약통계", verbatimTextOutput("summary"))
  )
)

server <- function(input, output, session) {

  file_ext <- reactive({
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    validate(need(ext %in% c("csv", "xlsx"), "CSV 또는 XLSX 파일만 지원합니다."))
    ext
  })

  # readxl이 확장자를 확실히 판별하도록 원본 파일명을 임시 경로에 반영
  upload_path <- reactive({
    ext <- file_ext()
    target <- tempfile(fileext = paste0(".", ext))
    copied <- file.copy(input$file$datapath, target, overwrite = TRUE)
    validate(need(copied, "파일을 읽을 수 없습니다."))
    target
  })

  output$file_options <- renderUI({
    ext <- file_ext()
    if (ext == "xlsx") {
      sheets <- tryCatch(readxl::excel_sheets(upload_path()), error = function(e) character(0))
      validate(need(length(sheets) > 0, "엑셀 시트를 읽을 수 없습니다."))
      selectInput("sheet", "Excel 시트", choices = sheets, selected = sheets[1])
    } else {
      selectInput(
        "encoding", "CSV 문자 인코딩",
        choices = c("UTF-8 (기본)" = "UTF-8-BOM", "CP949 / EUC-KR" = "CP949"),
        selected = "UTF-8-BOM"
      )
    }
  })

  raw_data <- reactive({
    ext <- file_ext()
    path <- upload_path()
    if (ext == "xlsx") {
      sheets <- readxl::excel_sheets(path)
      req(input$sheet, input$sheet %in% sheets)
      df <- as.data.frame(readxl::read_xlsx(path, sheet = input$sheet), check.names = FALSE)
    } else {
      req(input$encoding)
      df <- read.csv(
        path, fileEncoding = input$encoding,
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
    validate(need(ncol(df) > 0 && nrow(df) > 0, "데이터가 비어 있습니다."))
    # 같은 열 이름이 여러 개 있으면 유일한 이름으로 정리
    nms <- trimws(names(df))
    nms[nms == ""] <- paste0("Column_", which(nms == ""))
    names(df) <- make.unique(nms)
    df
  })

  output$col_selectors <- renderUI({
    df <- raw_data()
    cols <- names(df)
    num_cols <- cols[vapply(df, is.numeric, logical(1))]

    selected_x <- isolate(input$x_col)
    if (is.null(selected_x) || !(selected_x %in% cols)) selected_x <- cols[1]

    selected_y <- intersect(isolate(input$y_cols), num_cols)
    if (length(selected_y) == 0) {
      alternatives <- setdiff(num_cols, selected_x)
      selected_y <- if (length(alternatives)) alternatives[1] else character(0)
    }
    selected_color <- isolate(input$color_col)
    if (is.null(selected_color) || !(selected_color %in% cols)) selected_color <- ""
    selected_facet <- isolate(input$facet_col)
    if (is.null(selected_facet) || !(selected_facet %in% cols)) selected_facet <- ""

    tagList(
      if (input$chart_type != "hist") {
        selectInput("x_col", "X축 변수", choices = cols, selected = selected_x)
      },
      checkboxGroupInput(
        "y_cols",
        if (input$chart_type == "hist") "분포를 볼 수치형 변수" else "Y축 변수 (복수 선택 가능)",
        choices = num_cols,
        selected = selected_y
      ),
      selectInput(
        "color_col", "색상 그룹 (선택)",
        choices = c("선택 안함" = "", stats::setNames(cols, cols)),
        selected = selected_color
      ),
      selectInput(
        "facet_col", "패싯 변수 (선택)",
        choices = c("선택 안함" = "", stats::setNames(cols, cols)),
        selected = selected_facet
      ),
      if (input$chart_type == "hist") {
        sliderInput("bins", "히스토그램 구간 수 (bins)", min = 5, max = 100, value = 30)
      }
    )
  })

  output$aggregation_controls <- renderUI({
    req(raw_data())
    if (input$chart_type %in% c("hist", "box")) {
      return(helpText("이 차트는 집계하지 않은 원자료를 사용합니다."))
    }
    tagList(
      selectInput(
        "agg_method", "집계 방법",
        choices = c(
          "평균 (Mean)" = "mean",
          "합계 (Sum)" = "sum",
          "중앙값 (Median)" = "median",
          "개수 (Count, 결측 제외)" = "count",
          "집계 안 함 (원자료)" = "raw"
        ),
        selected = "mean"
      ),
      conditionalPanel(
        condition = "input.agg_method == 'mean'",
        selectInput(
          "error_method", "오차 표시 (평균 집계 시)",
          choices = c(
            "표시 안 함" = "none",
            "표준편차 ±SD" = "sd",
            "표준오차 ±SE" = "se",
            "95% 신뢰구간 (t 분포)" = "ci"
          ),
          selected = "none"
        )
      )
    )
  })

  output$filter_controls <- renderUI({
    df <- raw_data()
    cols <- names(df)
    selected_filter <- isolate(input$filter_col)
    if (is.null(selected_filter) || !(selected_filter %in% cols)) selected_filter <- ""
    tagList(
      selectInput(
        "filter_col", "행 필터 변수 (선택)",
        choices = c("필터 없음" = "", stats::setNames(cols, cols)),
        selected = selected_filter
      ),
      uiOutput("filter_values_ui")
    )
  })

  output$filter_values_ui <- renderUI({
    req(raw_data(), input$filter_col)
    if (input$filter_col == "") return(NULL)
    vals <- unique(as_filter_text(raw_data()[[input$filter_col]]))
    # 항목이 많아도 검색으로 선택 가능
    selectizeInput(
      "filter_values", "포함할 값 (선택하지 않으면 전체)",
      choices = vals, selected = character(0), multiple = TRUE,
      options = list(placeholder = "값 검색 및 다중 선택")
    )
  })

  filtered_data <- reactive({
    df <- raw_data()
    col <- input$filter_col
    values <- input$filter_values
    if (!is.null(col) && col != "" && col %in% names(df) && length(values) > 0) {
      df <- df[as_filter_text(df[[col]]) %in% values, , drop = FALSE]
    }
    df
  })

  # 그래프에서 사용할 원자료를 통일된 long format으로 변환
  long_data <- reactive({
    df <- filtered_data()
    req(input$chart_type, input$y_cols)
    validate(need(nrow(df) > 0, "필터 조건에 맞는 데이터가 없습니다."))
    validate(need(length(input$y_cols) > 0, "Y축 수치형 변수를 선택하세요."))
    validate(need(all(input$y_cols %in% names(df)), "유효한 Y축 변수를 선택하세요."))
    validate(need(all(vapply(df[input$y_cols], is.numeric, logical(1))),
                  "Y축에는 수치형 열을 선택해야 합니다."))

    is_hist <- identical(input$chart_type, "hist")
    if (!is_hist) {
      req(input$x_col)
      validate(need(input$x_col %in% names(df), "X축 변수를 선택하세요."))
      validate(need(!(input$x_col %in% input$y_cols),
                    "X축 변수와 Y축 변수는 서로 달라야 합니다."))
    }

    color_col <- input$color_col
    facet_col <- input$facet_col
    has_color <- !is.null(color_col) && color_col != "" && color_col %in% names(df)
    has_facet <- !is.null(facet_col) && facet_col != "" && facet_col %in% names(df)
    multi_y <- length(input$y_cols) > 1

    # 내부 전용 열 이름은 일반적인 CSV 열 이름과 충돌하지 않도록 검사
    internal_names <- c("..app_x", "..app_color", "..app_facet", "..app_variable",
                        "..app_value", "..app_series", "..app_n", "..app_sd", "..app_se",
                        "..app_ci_low", "..app_ci_high")
    validate(need(!any(input$y_cols %in% internal_names),
                  "Y축 변수에 내부 예약 이름(..app_)이 포함되어 있습니다. 열 이름을 변경해주세요."))

    base <- data.frame(
      ..app_x = if (is_hist) seq_len(nrow(df)) else df[[input$x_col]],
      ..app_color = if (has_color) as_filter_text(df[[color_col]]) else "전체",
      ..app_facet = if (has_facet) as_filter_text(df[[facet_col]]) else "전체",
      check.names = FALSE
    )
    base <- dplyr::bind_cols(base, df[input$y_cols])
    long <- tidyr::pivot_longer(
      base, cols = all_of(input$y_cols),
      names_to = "..app_variable", values_to = "..app_value"
    )
    long$..app_series <- if (multi_y && has_color) {
      paste(long$..app_variable, long$..app_color, sep = " · ")
    } else if (multi_y) {
      long$..app_variable
    } else if (has_color) {
      long$..app_color
    } else {
      "전체"
    }
    long
  })

  # 평균/합계/중앙값/개수 집계를 X, 그룹, 패싯, 변수별로 수행
  plot_data <- reactive({
    long <- long_data()
    if (input$chart_type %in% c("hist", "box")) return(long)
    req(input$agg_method)
    if (input$agg_method == "raw") return(long)

    method <- input$agg_method
    out <- long %>%
      group_by(..app_x, ..app_color, ..app_facet, ..app_variable, ..app_series) %>%
      summarise(
        ..app_n = sum(!is.na(..app_value)),
        ..app_sd = safe_sd(..app_value),
        ..app_value = aggregate_values(..app_value, method),
        .groups = "drop"
      ) %>%
      mutate(
        ..app_se = if_else(..app_n >= 2, ..app_sd / sqrt(..app_n), NA_real_),
        ..app_ci_low = if_else(
          ..app_n >= 2,
          ..app_value - qt(0.975, df = pmax(..app_n - 1, 1)) * ..app_se,
          NA_real_
        ),
        ..app_ci_high = if_else(
          ..app_n >= 2,
          ..app_value + qt(0.975, df = pmax(..app_n - 1, 1)) * ..app_se,
          NA_real_
        )
      )
    out
  })

  make_plot <- reactive({
    dat <- plot_data()
    validate(need(nrow(dat) > 0, "그래프로 표시할 데이터가 없습니다."))
    chart <- input$chart_type
    is_multi <- length(input$y_cols) > 1
    has_color <- !is.null(input$color_col) && input$color_col != ""
    show_legend <- is_multi || has_color

    if (chart == "hist") {
      dat <- dat[!is.na(dat$..app_value), , drop = FALSE]
      validate(need(nrow(dat) > 0, "수치형 값이 없습니다."))
      p <- ggplot(dat, aes(x = ..app_value, fill = ..app_series)) +
        geom_histogram(bins = if (is.null(input$bins)) 30 else input$bins, alpha = 0.65,
                       color = "white", position = "identity") +
        labs(title = "변수별 빈도 분포", x = "값", y = "빈도수")
    } else if (chart == "box") {
      dat <- dat[!is.na(dat$..app_value), , drop = FALSE]
      validate(need(nrow(dat) > 0, "수치형 값이 없습니다."))
      p <- ggplot(dat,
                  aes(x = factor(..app_x), y = ..app_value,
                      fill = ..app_series,
                      group = interaction(..app_x, ..app_series))) +
        geom_boxplot(alpha = 0.75, outlier.alpha = 0.5) +
        labs(title = "그룹별 값 분포", x = input$x_col,
             y = if (is_multi) "값" else input$y_cols[1])
    } else {
      dat <- dat[!is.na(dat$..app_value) & !is.na(dat$..app_x), , drop = FALSE]
      validate(need(nrow(dat) > 0, "수치형 값이 없습니다."))

      if (chart == "bar") {
        p <- ggplot(dat,
                    aes(x = ..app_x, y = ..app_value,
                        fill = ..app_series, group = ..app_series)) +
          geom_col(position = position_dodge(width = 0.8), alpha = 0.85,
                   width = 0.7)
      } else {
        p <- ggplot(dat,
                    aes(x = ..app_x, y = ..app_value,
                        color = ..app_series, group = ..app_series))
        if (chart == "line") {
          p <- p + geom_line(linewidth = 1, na.rm = TRUE) +
            geom_point(size = 2, na.rm = TRUE)
        } else if (chart == "point") {
          p <- p + geom_point(size = 2.7, alpha = 0.75, na.rm = TRUE)
        }
      }

      # 오차막대: 평균 집계일 때만 SD/SE/CI 제공
      err <- input$error_method
      if (!is.null(input$agg_method) && input$agg_method == "mean" &&
          !is.null(err) && err != "none") {
        if (err == "sd") {
          dat$..app_ymin <- dat$..app_value - dat$..app_sd
          dat$..app_ymax <- dat$..app_value + dat$..app_sd
        } else if (err == "se") {
          dat$..app_ymin <- dat$..app_value - dat$..app_se
          dat$..app_ymax <- dat$..app_value + dat$..app_se
        } else {
          dat$..app_ymin <- dat$..app_ci_low
          dat$..app_ymax <- dat$..app_ci_high
        }
        if (chart == "bar") {
          p <- p + geom_errorbar(
            data = dat,
            aes(ymin = ..app_ymin, ymax = ..app_ymax),
            width = 0.18, position = position_dodge(width = 0.8),
            na.rm = TRUE
          )
        } else {
          p <- p + geom_errorbar(
            data = dat,
            aes(ymin = ..app_ymin, ymax = ..app_ymax),
            width = 0.15, na.rm = TRUE
          )
        }
      }

      method_text <- if (is.null(input$agg_method) || input$agg_method == "raw") {
        "원자료"
      } else {
        c(mean = "평균", sum = "합계", median = "중앙값", count = "개수")[[input$agg_method]]
      }
      p <- p + labs(
        title = paste0(input$x_col, "별 ", paste(input$y_cols, collapse = ", "), " (", method_text, ")"),
        x = input$x_col,
        y = if (is_multi) "값" else if (input$agg_method == "count") "개수" else input$y_cols[1]
      )
    }

    if (!is.null(input$facet_col) && input$facet_col != "") {
      p <- p + facet_wrap(vars(..app_facet), scales = "free_y")
    }
    p + labs(fill = "그룹 / 변수", color = "그룹 / 변수") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = if (show_legend) "bottom" else "none",
        panel.grid.minor = element_blank()
      )
  })

  # 표는 그래프와 동일한 처리 결과를 출력
  result_data <- reactive({
    dat <- plot_data()
    if (input$chart_type == "hist") {
      return(data.frame(
        변수 = dat$..app_variable,
        그룹 = dat$..app_color,
        패싯 = dat$..app_facet,
        값 = dat$..app_value,
        check.names = FALSE
      ))
    }
    out <- data.frame(
      X = dat$..app_x,
      변수 = dat$..app_variable,
      그룹 = dat$..app_color,
      패싯 = dat$..app_facet,
      값 = dat$..app_value,
      check.names = FALSE
    )
    names(out)[1] <- input$x_col
    if ("..app_n" %in% names(dat)) {
      out$n <- dat$..app_n
      out$SD <- dat$..app_sd
      out$SE <- dat$..app_se
      out$CI95_하한 <- dat$..app_ci_low
      out$CI95_상한 <- dat$..app_ci_high
    }
    out
  })

  output$plot <- renderPlot({
    req(input$file)
    make_plot()
  }, res = 110)

  output$plot_note <- renderText({
    req(input$file)
    if (input$chart_type == "line" && identical(input$agg_method, "raw")) {
      return("참고: 집계하지 않은 선 그래프는 동일 X값의 반복 관측치를 그대로 연결합니다. 평균 추세를 보려면 집계 방법을 '평균'으로 선택하세요.")
    }
    if (input$chart_type == "bar" && identical(input$agg_method, "raw")) {
      return("참고: 원자료 막대그래프는 같은 X·그룹의 막대가 겹칠 수 있습니다. 필요하면 평균 또는 합계로 집계하세요.")
    }
    if (input$chart_type == "line" && length(input$y_cols) > 1) {
      return("참고: 여러 Y변수의 단위가 다르면 하나의 Y축에서 직접 비교하기 어려울 수 있습니다.")
    }
    ""
  })

  output$raw_table <- renderDT({
    req(input$file)
    DT::datatable(filtered_data(), options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })

  output$result_table <- renderDT({
    req(input$file)
    DT::datatable(result_data(), options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })

  output$summary <- renderPrint({
    req(input$file)
    df <- filtered_data()
    cat("필터 적용 후 행 수:", nrow(df), "\n")
    cat("열 수:", ncol(df), "\n\n")
    print(summary(df))
  })

  output$download_plot <- downloadHandler(
    filename = function() paste0("plot_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = make_plot(), width = 11, height = 7,
             dpi = 300, bg = "white")
    }
  )

  # UTF-8 BOM을 추가하여 Windows Excel에서 한글 깨짐을 최소화
  output$download_csv <- downloadHandler(
    filename = function() paste0("aggregated_", Sys.Date(), ".csv"),
    content = function(file) {
      con <- base::file(file, open = "wb")
      on.exit(close(con), add = TRUE)
      writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
      utils::write.csv(result_data(), con, row.names = FALSE, na = "")
    }
  )

  output$download_xlsx <- downloadHandler(
    filename = function() paste0("aggregated_", Sys.Date(), ".xlsx"),
    content = function(file) {
      writexl::write_xlsx(result_data(), path = file)
    }
  )
}

shinyApp(ui, server)
