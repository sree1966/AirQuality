# app.R
options(shiny.fullstacktrace = TRUE)
library(shiny)
library(bslib)
library(tidyverse)
library(randomForest)
library(lubridate)
library(bsicons)

# ---- Load model and data --------------------------------------
rf_model <- readRDS("C:/Users/kvsre/AirQuality/output/rf_model.rds")

pm25_model <- read_csv("C:/Users/kvsre/AirQuality/data/pm25_daily.csv",
                       show_col_types = FALSE) |>
  mutate(
    date = as_date(as.character(date)),
    city = as.character(city)
  ) |>
  arrange(city, date)

weekday_levels <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
month_levels   <- month.abb

# Model input: weekday must be an ordered factor, as in training
prep_inputs <- function(df) {
  df |>
    mutate(
      city    = as.character(city),
      weekday = factor(as.character(weekday), levels = weekday_levels, ordered = TRUE)
    )
}

model_input      <- prep_inputs(pm25_model)
model_input$pred <- as.numeric(predict(rf_model, newdata = model_input))

cutoff    <- max(pm25_model$date) - days(90)
test      <- model_input |> filter(date > cutoff)
city_list <- sort(unique(pm25_model$city))
last_obs  <- max(pm25_model$date)
test_rmse <- sqrt(mean((test$pred - test$pm25)^2, na.rm = TRUE))

# ---- Colour palette -------------------------------------------
accent   <- "#00d4ff"
accent2  <- "#ff4d6d"
bg_card  <- "#13132a"
txt      <- "#e2e8f0"
txt_dim  <- "#94a3b8"

# ---- ggplot dark theme ----------------------------------------
dark_plot <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.background   = element_rect(fill = bg_card, colour = NA),
      panel.background  = element_rect(fill = bg_card, colour = NA),
      panel.grid.major  = element_line(colour = "#1e2a45", linewidth = 0.6),
      panel.grid.minor  = element_line(colour = "#151f35", linewidth = 0.3),
      text              = element_text(colour = txt),
      axis.text         = element_text(colour = txt_dim, size = 12),
      axis.title        = element_text(colour = txt, size = 13, face = "bold"),
      plot.title        = element_text(face = "bold", colour = txt, size = 15),
      plot.subtitle     = element_text(colour = txt_dim, size = 12),
      plot.caption      = element_text(colour = "#64748b", size = 10),
      legend.background = element_rect(fill = bg_card, colour = NA),
      legend.text       = element_text(colour = txt, size = 12),
      legend.title      = element_text(colour = txt, size = 12, face = "bold"),
      plot.margin       = ggplot2::margin(16, 16, 16, 16, unit = "pt")
    )
}

# ---- Recursive forecast ---------------------------------------
forecast_city <- function(city_name, n_days) {
  hist      <- pm25_model |> filter(city == city_name) |> arrange(date)
  last_vals <- tail(hist$pm25, 7)
  out       <- vector("list", n_days)
  
  for (i in seq_len(n_days)) {
    next_date <- max(hist$date) + i
    row <- tibble(
      city    = city_name,
      date    = next_date,
      lag1    = tail(last_vals, 1),
      roll7   = mean(tail(last_vals, 7)),
      month   = month(next_date),
      weekday = factor(weekday_levels[wday(next_date)],
                       levels = weekday_levels, ordered = TRUE)
    )
    p          <- as.numeric(predict(rf_model, newdata = row))
    row$pred   <- p
    out[[i]]   <- row
    last_vals  <- c(last_vals, p)
  }
  bind_rows(out) |> mutate(city = as.character(city))
}

# ---- Custom CSS -----------------------------------------------
website_css <- "
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Space+Grotesk:wght@500;600;700&display=swap');

html, body { background: #0d0d1a !important; color: #e2e8f0 !important;
  font-family: 'Inter', sans-serif !important; }

.navbar { background: rgba(13,13,26,0.97) !important;
  border-bottom: 1px solid rgba(0,212,255,0.15) !important;
  padding: 0.6rem 2rem !important; }
.navbar .nav-link { color: #94a3b8 !important; font-weight: 500 !important; border-radius: 8px; }
.navbar .nav-link:hover { color: #e2e8f0 !important; background: rgba(255,255,255,0.07) !important; }
.navbar .nav-link.active { color: #00d4ff !important; background: rgba(0,212,255,0.1) !important; }

.bslib-sidebar-layout > .sidebar { background: #0f0f24 !important;
  border-right: 1px solid rgba(0,212,255,0.1) !important; }

.bslib-value-box { border: 1px solid rgba(0,212,255,0.2) !important; border-radius: 16px !important;
  box-shadow: 0 8px 32px rgba(0,0,0,0.4) !important; }
.bslib-value-box .value-box-value { font-family: 'Space Grotesk', sans-serif !important;
  font-size: 2rem !important; font-weight: 700 !important; }

.card { background: #13132a !important; border: 1px solid rgba(255,255,255,0.07) !important;
  border-radius: 16px !important; box-shadow: 0 4px 24px rgba(0,0,0,0.35) !important; overflow: hidden; }
.card-header { background: rgba(0,212,255,0.04) !important;
  border-bottom: 1px solid rgba(0,212,255,0.1) !important;
  font-family: 'Space Grotesk', sans-serif !important; font-weight: 600 !important; color: #e2e8f0 !important; }

.about-hero { background: linear-gradient(135deg, #0f0f28 0%, #1a1040 50%, #0f0f28 100%);
  border: 1px solid rgba(0,212,255,0.15); border-radius: 20px; padding: 3rem 2.5rem; margin-bottom: 1.5rem; }
.about-hero h3 { font-family: 'Space Grotesk', sans-serif; font-size: 1.8rem; font-weight: 700; color: #f1f5f9; }
.about-hero p { color: #94a3b8; font-size: 1rem; line-height: 1.7; max-width: 640px; }
.feature-pill { display: inline-block; background: rgba(0,212,255,0.1);
  border: 1px solid rgba(0,212,255,0.25); color: #00d4ff; border-radius: 99px;
  padding: 0.35rem 1rem; font-size: 0.8rem; font-weight: 600; margin: 0.25rem; }
"

# ---- UI -------------------------------------------------------
ui <- page_navbar(
  title = tagList(
    bs_icon("wind"),
    tags$span(" PM2.5", style = "color:#00d4ff; font-weight:700;"),
    tags$span(" Forecast Studio", style = "font-weight:400; opacity:0.85;")
  ),
  id = "nav",
  theme = bs_theme(
    version      = 5,
    bootswatch   = "darkly",
    primary      = accent,
    base_font    = font_google("Inter"),
    heading_font = font_google("Space Grotesk"),
    bg           = "#0d0d1a",
    fg           = txt
  ),
  header = tags$head(tags$style(HTML(website_css))),
  
  sidebar = sidebar(
    width = 280,
    open  = "open",
    tags$small(style = "color:#64748b; text-transform:uppercase; letter-spacing:1px; font-weight:600;",
               bs_icon("geo-alt-fill"), " City"),
    selectInput("city", NULL, choices = city_list, selected = city_list[1]),
    tags$hr(),
    conditionalPanel(
      "input.nav == 'Forecast'",
      tags$small(style = "color:#64748b; text-transform:uppercase; letter-spacing:1px; font-weight:600;",
                 bs_icon("sliders"), " Forecast Mode"),
      radioButtons("mode", NULL,
                   choices = c("Test-period check" = "test",
                               "Specific date"     = "date",
                               "Next 7 days"       = "week")),
      conditionalPanel(
        "input.mode == 'date'",
        dateInput("target", "Target date", value = last_obs + 1, min = last_obs + 1)
      ),
      conditionalPanel(
        "input.mode == 'test'",
        dateRangeInput("dates", "Date range",
                       start = min(test$date), end = max(test$date),
                       min   = min(test$date), max = max(test$date))
      )
    ),
    conditionalPanel(
      "input.nav == 'Compare Cities'",
      tags$small(style = "color:#64748b; text-transform:uppercase; letter-spacing:1px; font-weight:600;",
                 bs_icon("check2-square"), " Cities to Compare"),
      checkboxGroupInput("compare_cities", NULL,
                         choices = city_list, selected = city_list)
    )
  ),
  
  nav_panel(
    title = "Forecast", value = "Forecast",
    tags$div(style = "padding: 1.5rem 2rem;",
             layout_column_wrap(
               width = 1/3, gap = "1.25rem",
               value_box(title = "Model Test RMSE", value = uiOutput("rmse"),
                         theme = "primary", showcase = bs_icon("activity")),
               value_box(title = "Latest Observed", value = uiOutput("latest"),
                         theme = "info", showcase = bs_icon("wind")),
               value_box(title = "Forecast", value = uiOutput("forecast_val"),
                         theme = "success", showcase = bs_icon("stars"))
             ),
             tags$div(style = "height:1.5rem;"),
             card(full_screen = TRUE,
                  card_header("Forecast / Test Comparison"),
                  plotOutput("main_plot", height = "420px")),
             tags$div(style = "height:1.25rem;"),
             card(card_header("Historical PM2.5: Last 180 Days"),
                  plotOutput("history_plot", height = "220px"))
    )
  ),
  
  nav_panel(
    title = "Compare Cities", value = "Compare Cities",
    tags$div(style = "padding: 1.5rem 2rem;",
             card(card_header("Daily PM2.5 by City"),
                  plotOutput("compare_plot", height = "420px")),
             tags$div(style = "height:1.25rem;"),
             card(card_header("Monthly PM2.5 Distribution by City"),
                  plotOutput("box_plot", height = "420px"))
    )
  ),
  
  nav_panel(
    title = "Patterns", value = "Patterns",
    tags$div(style = "padding: 1.5rem 2rem;",
             layout_column_wrap(
               width = 1/2, gap = "1.25rem",
               card(card_header("Average PM2.5 by Weekday"),
                    plotOutput("weekday_plot", height = "360px")),
               card(card_header("Forecast Error Distribution (Test Set)"),
                    plotOutput("error_plot", height = "360px"))
             )
    )
  ),
  
  nav_panel(
    title = "About", value = "About",
    tags$div(style = "padding: 1.5rem 2rem;",
             tags$div(class = "about-hero",
                      tags$h3("PM2.5 Forecast Studio"),
                      tags$p("A machine-learning dashboard that forecasts next-day PM2.5 air quality
                across five Indian cities, using a Random Forest model trained on
                Open-Meteo Air Quality data."),
                      tags$div(style = "margin-top:1.5rem;",
                               tags$span(class = "feature-pill", "Random Forest"),
                               tags$span(class = "feature-pill", "Lag-1 and 7-day rolling features"),
                               tags$span(class = "feature-pill", "5 Indian cities"),
                               tags$span(class = "feature-pill", "Open-Meteo API")
                      )
             ),
             layout_column_wrap(
               width = 1/2, gap = "1.25rem",
               card(card_header("How It Works"),
                    tags$p(style = "color:#94a3b8; line-height:1.8; padding:1rem 1.5rem;",
                           "The model uses yesterday's PM2.5 (lag-1) and the 7-day rolling mean as inputs,
                plus month and weekday for seasonality. Multi-day forecasts are generated
                recursively, so each day's prediction becomes the next day's lag-1 input.")),
               card(card_header("Accuracy and Caveats"),
                    tags$p(style = "color:#94a3b8; line-height:1.8; padding:1rem 1.5rem;",
                           "Test RMSE is computed on the last 90 days, held out from training.
                Forecasts beyond about 3 days accumulate error, so treat them as indicative trends.")))
    )
  )
)

# ---- Server ---------------------------------------------------
server <- function(input, output, session) {
  
  output$rmse <- renderUI({
    d <- test |> filter(city == input$city)
    if (nrow(d) == 0) return("No data")
    paste(round(sqrt(mean((d$pred - d$pm25)^2, na.rm = TRUE)), 1), "µg/m³")
  })
  
  output$latest <- renderUI({
    v <- pm25_model |> filter(city == input$city) |> arrange(date) |> tail(1)
    paste(round(v$pm25, 1), "µg/m³")
  })
  
  forecast_data <- reactive({
    n <- switch(input$mode,
                week = 7,
                date = as.integer(input$target - last_obs),
                test = 0)
    if (is.na(n) || n <= 0) return(NULL)
    forecast_city(input$city, n)
  })
  
  output$forecast_val <- renderUI({
    if (input$mode == "test") return("See chart")
    f <- forecast_data()
    if (is.null(f)) return("—")
    if (input$mode == "date") paste(round(tail(f$pred, 1), 1), "µg/m³")
    else paste(round(mean(f$pred), 1), "µg/m³ avg")
  })
  
  output$main_plot <- renderPlot({
    if (input$mode == "test") {
      d <- test |>
        filter(city == input$city,
               date >= input$dates[1], date <= input$dates[2]) |>
        pivot_longer(c(pm25, pred), names_to = "type", values_to = "value") |>
        mutate(type = ifelse(type == "pm25", "Actual", "Predicted"))
      validate(need(nrow(d) > 0, "No test data in this range."))
      
      ggplot(d, aes(date, value, colour = type)) +
        geom_line(linewidth = 1.2) +
        scale_colour_manual(values = c(Actual = accent, Predicted = accent2)) +
        labs(title = paste("Actual vs Predicted PM2.5:", input$city),
             subtitle = "Test period",
             x = "Date", y = "PM2.5 (µg/m³)", colour = NULL,
             caption = "Source: Open-Meteo Air Quality API") +
        dark_plot() + theme(legend.position = "top")
      
    } else {
      f <- forecast_data()
      validate(need(!is.null(f), "Pick a valid date after the last observation."))
      hist_tail <- pm25_model |> filter(city == input$city) |> arrange(date) |> tail(30)
      
      ggplot() +
        geom_line(data = hist_tail, aes(date, pm25), colour = accent, linewidth = 1.2) +
        geom_ribbon(data = f,
                    aes(date, ymin = pred - test_rmse, ymax = pred + test_rmse),
                    fill = accent2, alpha = 0.2) +
        geom_line(data = f, aes(date, pred), colour = accent2, linewidth = 1.3) +
        geom_point(data = f, aes(date, pred), colour = accent2, size = 3) +
        labs(title = paste("Forecast for", input$city),
             subtitle = "Cyan: last 30 observed days · Pink: forecast ± RMSE band",
             x = "Date", y = "PM2.5 (µg/m³)",
             caption = "Source: Open-Meteo Air Quality API") +
        dark_plot()
    }
  })
  
  output$history_plot <- renderPlot({
    d <- pm25_model |> filter(city == input$city) |> arrange(date) |> tail(180)
    ggplot(d, aes(date, pm25)) +
      geom_area(fill = accent, alpha = 0.15) +
      geom_line(colour = accent, linewidth = 0.8) +
      labs(x = "Date", y = "PM2.5 (µg/m³)") +
      dark_plot()
  })
  
  output$compare_plot <- renderPlot({
    d <- pm25_model |> filter(city %in% input$compare_cities)
    validate(need(nrow(d) > 0, "Select at least one city."))
    ggplot(d, aes(date, pm25, colour = city)) +
      geom_line(linewidth = 0.5, alpha = 0.9) +
      labs(x = "Date", y = "PM2.5 (µg/m³)", colour = "City",
           caption = "Source: Open-Meteo Air Quality API") +
      dark_plot() + theme(legend.position = "top")
  })
  
  output$box_plot <- renderPlot({
    d <- pm25_model |>
      filter(city %in% input$compare_cities) |>
      mutate(month_lab = factor(month.abb[month(date)], levels = month.abb))
    validate(need(nrow(d) > 0, "Select at least one city."))
    ggplot(d, aes(month_lab, pm25, fill = city)) +
      geom_boxplot(outlier.alpha = 0.3) +
      labs(x = "Month", y = "PM2.5 (µg/m³)", fill = "City",
           caption = "Source: Open-Meteo Air Quality API") +
      dark_plot() + theme(legend.position = "top")
  })
  
  output$weekday_plot <- renderPlot({
    d <- pm25_model |>
      filter(city == input$city) |>
      mutate(wd = factor(weekday_levels[wday(date)], levels = weekday_levels))
    validate(need(nrow(d) > 0, "No data for this city."))
    d |>
      group_by(wd) |>
      summarise(avg = mean(pm25, na.rm = TRUE), .groups = "drop") |>
      ggplot(aes(wd, avg)) +
      geom_col(fill = accent, alpha = 0.9, width = 0.7) +
      labs(title = paste("Average PM2.5 by Weekday:", input$city),
           x = NULL, y = "Mean PM2.5 (µg/m³)",
           caption = "Source: Open-Meteo Air Quality API") +
      dark_plot()
  })
  
  output$error_plot <- renderPlot({
    d <- test |> mutate(err = pred - pm25)
    ggplot(d, aes(err)) +
      geom_histogram(bins = 35, fill = accent2, alpha = 0.85) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = txt) +
      labs(title = "Forecast Error Distribution",
           subtitle = "Predicted minus actual · All cities · Test period",
           x = "Error (µg/m³)", y = "Count") +
      dark_plot()
  })
}

shinyApp(ui, server)
