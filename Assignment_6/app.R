# BDA400 Assignment 6 - Technical Analysis Visualization Dashboard
# Student: Sara Alnaameh
# Repository folder: Assignment_6
# Data source: Yahoo Finance through quantmod

required_packages <- c("shiny", "ggplot2", "quantmod", "TTR")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Install these packages before running the app: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(shiny)
library(ggplot2)

portfolio_symbols <- c("SHOP.TO", "RY.TO", "TD.TO")

# Download daily OHLCV and adjusted-close data. Extra history is included so
# moving averages and RSI have warm-up observations before the selected dates.
fetch_stock_data <- function(symbol, start_date, end_date) {
  from_date <- as.Date(start_date) - 500
  to_date <- as.Date(end_date) + 2

  prices <- quantmod::getSymbols(
    Symbols = symbol,
    src = "yahoo",
    from = from_date,
    to = to_date,
    auto.assign = FALSE,
    warnings = FALSE
  )

  result <- data.frame(
    Date = as.Date(zoo::index(prices)),
    Open = as.numeric(quantmod::Op(prices)),
    High = as.numeric(quantmod::Hi(prices)),
    Low = as.numeric(quantmod::Lo(prices)),
    Close = as.numeric(quantmod::Cl(prices)),
    Volume = as.numeric(quantmod::Vo(prices)),
    Adjusted = as.numeric(quantmod::Ad(prices))
  )
  result <- result[stats::complete.cases(result$Date, result$Open, result$High,
                                         result$Low, result$Close), , drop = FALSE]
  if (nrow(result) == 0) stop("Yahoo Finance returned no usable observations.")
  result
}

# Convert daily rows to weekly or monthly OHLCV bars without changing the
# chronological order of the observations.
aggregate_bars <- function(data, timeframe) {
  if (timeframe == "Daily") return(data)

  group_key <- if (timeframe == "Weekly") {
    format(data$Date, "%G-%V")
  } else {
    format(data$Date, "%Y-%m")
  }
  groups <- split(data, factor(group_key, levels = unique(group_key)))

  rows <- lapply(groups, function(part) {
    data.frame(
      Date = tail(part$Date, 1),
      Open = part$Open[1],
      High = max(part$High, na.rm = TRUE),
      Low = min(part$Low, na.rm = TRUE),
      Close = tail(part$Close, 1),
      Volume = sum(part$Volume, na.rm = TRUE),
      Adjusted = tail(part$Adjusted, 1)
    )
  })
  do.call(rbind, rows)
}

# Calculate moving averages, RSI, MACD, and example crossover signals.
calculate_indicators <- function(data, price_source, fast_period, slow_period,
                                 ma_period, rsi_period, rsi_buy, rsi_sell,
                                 macd_signal_period) {
  data$Price <- data[[price_source]]
  data$SMA <- as.numeric(TTR::SMA(data$Price, n = ma_period))
  data$EMA_Fast <- as.numeric(TTR::EMA(data$Price, n = fast_period))
  data$EMA_Slow <- as.numeric(TTR::EMA(data$Price, n = slow_period))
  data$RSI <- as.numeric(TTR::RSI(data$Price, n = rsi_period))

  macd_values <- TTR::MACD(
    data$Price,
    nFast = fast_period,
    nSlow = slow_period,
    nSig = macd_signal_period
  )
  data$MACD <- as.numeric(macd_values[, "macd"])
  data$MACD_Signal <- as.numeric(macd_values[, "signal"])

  previous_fast <- c(NA_real_, head(data$EMA_Fast, -1))
  previous_slow <- c(NA_real_, head(data$EMA_Slow, -1))
  cross_up <- data$EMA_Fast > data$EMA_Slow & previous_fast <= previous_slow
  cross_down <- data$EMA_Fast < data$EMA_Slow & previous_fast >= previous_slow

  data$Signal <- "Hold"
  buy_signal <- !is.na(cross_up) & cross_up & !is.na(data$RSI) & data$RSI <= rsi_buy
  sell_signal <- !is.na(cross_down) & cross_down |
    (!is.na(data$RSI) & data$RSI >= rsi_sell)
  data$Signal[buy_signal] <- "Buy"
  data$Signal[sell_signal & !buy_signal] <- "Sell"
  data
}

ui <- fluidPage(
  titlePanel("Sara Alnaameh - Portfolio Technical Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("symbol", "Portfolio stock", choices = portfolio_symbols,
                  selected = "SHOP.TO"),
      dateRangeInput(
        "dates", "Date range",
        start = Sys.Date() - 365,
        end = Sys.Date(),
        max = Sys.Date()
      ),
      selectInput("timeframe", "Bar timeframe",
                  choices = c("Daily", "Weekly", "Monthly"), selected = "Daily"),
      selectInput("chart_type", "Price chart",
                  choices = c("Line", "Area", "Candlestick"), selected = "Line"),
      selectInput("price_source", "Indicator price series",
                  choices = c("Adjusted", "Close"), selected = "Adjusted"),
      checkboxGroupInput(
        "overlays", "Show technical indicators",
        choices = c("SMA" = "SMA", "EMA" = "EMA", "RSI" = "RSI", "MACD" = "MACD"),
        selected = c("SMA", "EMA", "RSI", "MACD")
      ),
      numericInput("ma_period", "SMA period", value = 20, min = 2, max = 200),
      numericInput("fast_period", "Fast EMA period", value = 12, min = 2, max = 100),
      numericInput("slow_period", "Slow EMA period", value = 26, min = 3, max = 200),
      numericInput("rsi_period", "RSI period", value = 14, min = 2, max = 100),
      numericInput("rsi_buy", "Buy rule: RSI at or below", value = 70, min = 1, max = 99),
      numericInput("rsi_sell", "Sell rule: RSI at or above", value = 70, min = 1, max = 99),
      numericInput("macd_signal_period", "MACD signal period", value = 9, min = 2, max = 50),
      actionButton("refresh", "Fetch / refresh Yahoo Finance data", class = "btn-primary"),
      br(), br(),
      helpText("Example rule: Buy on a fast EMA crossover above the slow EMA when RSI is below the buy threshold. Sell on a downward EMA crossover or when RSI reaches the sell threshold.")
    ),
    mainPanel(
      h4("Price chart with signal annotations"),
      plotOutput("price_plot", height = "460px"),
      uiOutput("indicator_panels"),
      h4("Latest observation"),
      tableOutput("latest_summary"),
      helpText("Yahoo Finance data is retrieved when the refresh button is pressed. Historical values can be revised by the data provider.")
    )
  )
)

server <- function(input, output, session) {
  raw_data <- eventReactive(input$refresh, {
    req(input$symbol, input$dates)
    tryCatch(
      fetch_stock_data(input$symbol, input$dates[1], input$dates[2]),
      error = function(e) e
    )
  }, ignoreNULL = FALSE)

  chart_data <- reactive({
    validate(need(input$fast_period < input$slow_period,
                  "Fast EMA period must be less than slow EMA period."))
    downloaded <- raw_data()
    validate(need(!inherits(downloaded, "error"),
                  if (inherits(downloaded, "error")) conditionMessage(downloaded) else ""))

    bars <- aggregate_bars(downloaded, input$timeframe)
    bars <- calculate_indicators(
      bars,
      price_source = input$price_source,
      fast_period = input$fast_period,
      slow_period = input$slow_period,
      ma_period = input$ma_period,
      rsi_period = input$rsi_period,
      rsi_buy = input$rsi_buy,
      rsi_sell = input$rsi_sell,
      macd_signal_period = input$macd_signal_period
    )
    selected <- bars[bars$Date >= as.Date(input$dates[1]) &
                       bars$Date <= as.Date(input$dates[2]), , drop = FALSE]
    validate(need(nrow(selected) > 0,
                  "No bars are available in this date range. Choose a wider range."))
    selected
  })

  output$price_plot <- renderPlot({
    data <- chart_data()
    width_days <- switch(input$timeframe, Daily = 0.6, Weekly = 3.5, Monthly = 12)
    p <- ggplot(data, aes(x = Date))

    if (input$chart_type == "Line") {
      p <- p + geom_line(aes(y = Price, colour = "Price"), linewidth = 0.65)
    } else if (input$chart_type == "Area") {
      p <- p + geom_area(aes(y = Price), fill = "#DDEBF7", alpha = 0.9) +
        geom_line(aes(y = Price, colour = "Price"), linewidth = 0.55)
    } else {
      data$Up_Day <- data$Close >= data$Open
      p <- ggplot(data, aes(x = Date)) +
        geom_segment(aes(xend = Date, y = Low, yend = High),
                     colour = "#3D4B5C", linewidth = 0.35) +
        geom_rect(
          aes(xmin = Date - width_days / 2, xmax = Date + width_days / 2,
              ymin = pmin(Open, Close), ymax = pmax(Open, Close), fill = Up_Day),
          colour = "#3D4B5C", linewidth = 0.25
        ) +
        scale_fill_manual(values = c(`TRUE` = "#2E8B57", `FALSE` = "#C44E52"),
                          labels = c(`TRUE` = "Up", `FALSE` = "Down"),
                          name = "Candle")
    }

    if ("SMA" %in% input$overlays) {
      p <- p + geom_line(aes(y = SMA, colour = "SMA"), linewidth = 0.65, na.rm = TRUE)
    }
    if ("EMA" %in% input$overlays) {
      p <- p + geom_line(aes(y = EMA_Fast, colour = "Fast EMA"), linewidth = 0.65, na.rm = TRUE) +
        geom_line(aes(y = EMA_Slow, colour = "Slow EMA"), linewidth = 0.65, na.rm = TRUE)
    }

    signals <- data[data$Signal %in% c("Buy", "Sell"), , drop = FALSE]
    if (nrow(signals) > 0) {
      p <- p + geom_point(
        data = signals,
        aes(y = Price, colour = Signal, shape = Signal),
        size = 3, inherit.aes = TRUE
      )
    }

    p +
      scale_colour_manual(
        values = c("Price" = "#16324F", "SMA" = "#E69F00",
                   "Fast EMA" = "#0072B2", "Slow EMA" = "#CC79A7",
                   "Buy" = "#16803C", "Sell" = "#C62828"),
        breaks = c("Price", "SMA", "Fast EMA", "Slow EMA", "Buy", "Sell"),
        name = "Series / signal"
      ) +
      scale_shape_manual(values = c("Buy" = 24, "Sell" = 25), name = "Signal") +
      labs(
        title = paste(input$symbol, "-", input$timeframe, "technical analysis"),
        subtitle = paste("Price series:", input$price_source,
                         "| Green triangle = Buy | Red triangle = Sell"),
        x = NULL, y = "Price (CAD for .TO tickers)"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  })

  output$indicator_panels <- renderUI({
    panels <- list()
    if ("RSI" %in% input$overlays) panels <- c(panels, list(plotOutput("rsi_plot", height = "190px")))
    if ("MACD" %in% input$overlays) panels <- c(panels, list(plotOutput("macd_plot", height = "190px")))
    if (length(panels) == 0) return(helpText("Select RSI or MACD to display its indicator panel."))
    tagList(panels)
  })

  output$rsi_plot <- renderPlot({
    req("RSI" %in% input$overlays)
    data <- chart_data()
    ggplot(data, aes(x = Date, y = RSI)) +
      geom_line(colour = "#6A3D9A", linewidth = 0.65, na.rm = TRUE) +
      geom_hline(yintercept = c(input$rsi_buy, input$rsi_sell),
                 linetype = "dashed", colour = "#7A7A7A") +
      scale_y_continuous(limits = c(0, 100)) +
      labs(title = paste("RSI", input$rsi_period), x = NULL, y = "RSI") +
      theme_minimal(base_size = 12)
  })

  output$macd_plot <- renderPlot({
    req("MACD" %in% input$overlays)
    data <- chart_data()
    ggplot(data, aes(x = Date)) +
      geom_hline(yintercept = 0, colour = "#777777", linewidth = 0.35) +
      geom_line(aes(y = MACD, colour = "MACD"), linewidth = 0.65, na.rm = TRUE) +
      geom_line(aes(y = MACD_Signal, colour = "Signal"), linewidth = 0.65, na.rm = TRUE) +
      scale_colour_manual(values = c("MACD" = "#0072B2", "Signal" = "#D55E00")) +
      labs(title = "MACD", x = NULL, y = "MACD", colour = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$latest_summary <- renderTable({
    data <- chart_data()
    latest <- tail(data, 1)
    data.frame(
      Symbol = input$symbol,
      Date = as.character(latest$Date),
      Price = round(latest$Price, 2),
      RSI = round(latest$RSI, 2),
      Signal = latest$Signal,
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
}

shinyApp(ui = ui, server = server)
