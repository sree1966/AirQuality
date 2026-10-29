library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(plotly)
library(randomForest)
library(lubridate)

# ---- Load model and data ---------------------------------------
rf_model   <- readRDS("C:/Users/kvsre/AirQuality/output/rf_model.rds")
pm25_model <- read_csv("C:/Users/kvsre/AirQuality/data/pm25_daily.csv", show_col_types = FALSE) |>
  mutate(date = as_date(as.character(date)), city = as.character(city)) |>
  arrange(city, date)

weekday_levels <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
city_list      <- sort(unique(pm25_model$city))
last_obs       <- max(pm25_model$date)
cutoff         <- last_obs - days(90)

model_input <- pm25_model |>
  mutate(weekday = factor(as.character(weekday), levels = weekday_levels, ordered = TRUE))
model_input$pred <- as.numeric(predict(rf_model, newdata = model_input))
test      <- model_input |> filter(date > cutoff)
test_rmse <- sqrt(mean((test$pred - test$pm25)^2, na.rm = TRUE))

# ---- Palette ----------------------------------------------------
pal <- c(Bengaluru = "#E4572E", Chennai = "#76B041", Delhi = "#17A398",
         Kolkata = "#2E86AB", Mumbai = "#A23B72")
ink <- "#1F2933"; muted <- "#5B6770"; paper <- "#FAFAF7"

# ---- Recursive forecast -----------------------------------------
forecast_city <- function(city_name, n_days) {
  hist      <- pm25_model |> filter(city == city_name) |> arrange(date)
  last_vals <- tail(hist$pm25, 7)
  out <- vector("list", n_days)
  for (i in seq_len(n_days)) {
    nd <- last_obs + i
    row <- tibble(
      city = city_name, date = nd,
      lag1 = tail(last_vals, 1),
      roll7 = mean(tail(last_vals, 7)),
      month = month(nd),
      weekday = factor(weekday_levels[wday(nd)], levels = weekday_levels, ordered = TRUE)
    )
    p <- as.numeric(predict(rf_model, newdata = row))
    row$pred <- p
    out[[i]] <- row
    last_vals <- c(last_vals, p)
  }
  bind_rows(out)
}

# ---- Theme -------------------------------------------------------
clean_theme <- function() {
  theme_minimal(base_size = 15) +
    theme(
      text = element_text(colour = ink),
      plot.title = element_text(face = "bold", size = 18, colour = ink),
      plot.subtitle = element_text(size = 14, colour = muted),
      plot.caption = element_text(size = 11, colour = muted),
      axis.text = element_text(size = 13, colour = muted),
      axis.title = element_text(size = 14, colour = ink),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#E4E7EB"),
      legend.position = "top",
      legend.text = element_text(size = 13)
    )
}

# ---- CSS ---------------------------------------------------------
site_css <- "
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
body { background: #FAFAF7; color: #1F2933; font-family: 'Inter', sans-serif; font-size: 16px; }
.navbar { background: #FFFFFF !important; border-bottom: 1px solid #E4E7EB; }
.navbar-brand, .nav-link { font-size: 16px !important; }
.nav-link.active { font-weight: 600; color: #1F2933 !important; }
.hero h1 { font-size: 2.6rem; font-weight: 700; letter-spacing: -0.5px; margin-bottom: .3rem; }
.hero p { font-size: 1.1rem; color: #5B6770; max-width: 760px; }
.card { background: #FFFFFF; border: 1px solid #E4E7EB; border-radius: 10px; overflow: visible !important; }
.card-body { overflow: visible !important; }
.card-header { background: #FFFFFF; font-weight: 600; font-size: 1.05rem; border-bottom: 1px solid #E4E7EB; }
.kpi-card { padding: 1.1rem 1.4rem; min-height: 120px; display: flex; flex-direction: column; justify-content: center; overflow: visible !important; }
.kpi-label { font-size: .8rem; color: #5B6770; text-transform: uppercase; letter-spacing: .6px; margin-bottom: .4rem; }
.kpi-value { font-size: 1.9rem; font-weight: 700; line-height: 1.2; white-space: nowrap; overflow: visible; }
.caption { font-size: .95rem; color: #5B6770; margin-top: .5rem; }
"

# ---- UI ----------------------------------------------------------
ui <- page_navbar(
  title = tagList(bs_icon("cloud-haze2"), " PM2.5 Explorer"),
  id = "nav",
  theme = bs_theme(version = 5, base_font = font_google("Inter"),
                   primary = "#2E86AB", bg = paper, fg = ink),
  header = tags$head(tags$style(HTML(site_css))),
  sidebar = sidebar(
    width = 260,
    selectInput("city", "City", choices = city_list, selected = "Delhi"),
    conditionalPanel("input.nav == 'Forecast'",
                     radioButtons("mode", "Forecast mode",
                                  choices = c("Test period" = "test",
                                              "Specific date" = "date",
                                              "Next 7 days" = "week")),
                     conditionalPanel("input.mode == 'date'",
                                      dateInput("target", "Target date", value = last_obs + 1, min = last_obs + 1)),
                     conditionalPanel("input.mode == 'test'",
                                      dateRangeInput("dates", "Date range", start = min(test$date), end = max(test$date)))
    ),
    conditionalPanel("input.nav == 'Compare'",
                     checkboxGroupInput("cities", "Cities", choices = city_list, selected = city_list))
  ),
  
  nav_panel("Forecast", value = "Forecast",
            div(class = "hero", h1("Next-day PM2.5, explained"),
                p("Hover any line to read the exact value. The forecast band shows typical error.")),
            layout_column_wrap(width = 1/3,
                               div(class = "card kpi-card", div(class = "kpi-label", "Model test RMSE"),
                                   div(class = "kpi-value", textOutput("kpi_rmse"))),
                               div(class = "card kpi-card", div(class = "kpi-label", "Latest observed"),
                                   div(class = "kpi-value", textOutput("kpi_latest"))),
                               div(class = "card kpi-card", div(class = "kpi-label", "Forecast"),
                                   div(class = "kpi-value", textOutput("kpi_forecast")))
            ),
            card(card_header("Actual vs forecast"),
                 plotlyOutput("main_plot", height = "440px"),
                 p(class = "caption", "Source: Open-Meteo Air Quality API. Hover for daily values.")),
            card(card_header("Last 180 days"),
                 plotlyOutput("history_plot", height = "300px"))
  ),
  
  nav_panel("Compare", value = "Compare",
            card(card_header("Daily PM2.5 by city"),
                 plotlyOutput("compare_plot", height = "460px")),
            card(card_header("Stacked monthly average (share of total)"),
                 plotlyOutput("share_plot", height = "400px")),
            card(card_header("Monthly spread by city"),
                 plotlyOutput("box_plot", height = "420px"))
  ),
  
  nav_panel("Patterns", value = "Patterns",
            card(card_header("Average PM2.5 by weekday"),
                 plotlyOutput("weekday_plot", height = "380px")),
            card(card_header("Forecast error distribution (test set)"),
                 plotlyOutput("error_plot", height = "380px"))
  ),
  
  nav_panel("About", value = "About",
            card(div(class = "hero", h1("About this project"),
                     p("A random forest model forecasts next-day PM2.5 for five Indian cities using lag-1 and 7-day rolling features. Forecasts further ahead accumulate error, so treat them as indicative.")))
  )
)

# ---- Server ------------------------------------------------------
server <- function(input, output, session) {
  
  test_city <- reactive(test |> filter(city == input$city))
  
  output$kpi_rmse <- renderText({
    d <- test_city(); if (nrow(d) == 0) return("No data")
    paste(round(sqrt(mean((d$pred - d$pm25)^2, na.rm = TRUE)), 1), "µg/m³")
  })
  output$kpi_latest <- renderText({
    v <- pm25_model |> filter(city == input$city) |> tail(1)
    paste(round(v$pm25, 1), "µg/m³")
  })
  output$kpi_forecast <- renderText({
    if (input$mode == "test") return("See chart")
    n <- if (input$mode == "week") 7 else as.integer(input$target - last_obs)
    f <- forecast_city(input$city, n)
    if (input$mode == "date") paste(round(tail(f$pred, 1), 1), "µg/m³")
    else paste(round(mean(f$pred), 1), "µg/m³ avg")
  })
  
  output$main_plot <- renderPlotly({
    if (input$mode == "test") {
      d <- test_city() |>
        filter(date >= input$dates[1], date <= input$dates[2]) |>
        pivot_longer(c(pm25, pred), names_to = "type", values_to = "value") |>
        mutate(type = ifelse(type == "pm25", "Actual", "Predicted"))
      validate(need(nrow(d) > 0, "No test data in this range."))
      plot_ly(d, x = ~date, y = ~value, color = ~type,
              colors = c(Actual = "#1F2933", Predicted = "#E4572E"),
              type = "scatter", mode = "lines", hoverinfo = "text",
              text = ~paste0(format(date, "%d %b %Y"), "<br>", type, ": ",
                             round(value, 1), " µg/m³")) |>
        layout(xaxis = list(title = ""),
               yaxis = list(title = "PM2.5 (µg/m³)"),
               font = list(family = "Inter", size = 14),
               legend = list(orientation = "h", y = -0.15))
    } else {
      n <- if (input$mode == "week") 7 else as.integer(input$target - last_obs)
      f <- forecast_city(input$city, n)
      hist <- pm25_model |> filter(city == input$city) |> arrange(date) |> tail(30)
      f <- f |> mutate(lo = pred - test_rmse, hi = pred + test_rmse)
      
      plot_ly() |>
        add_trace(data = hist, x = ~date, y = ~pm25, type = "scatter", mode = "lines",
                  name = "Observed", line = list(color = "#1F2933", width = 2),
                  hoverinfo = "text",
                  text = ~paste0(format(date, "%d %b %Y"), "<br>Observed: ",
                                 round(pm25, 1), " µg/m³")) |>
        add_ribbons(data = f, x = ~date, ymin = ~lo, ymax = ~hi, name = "Typical error",
                    fillcolor = "rgba(228,87,46,0.15)", line = list(color = "transparent"),
                    hoverinfo = "skip") |>
        add_trace(data = f, x = ~date, y = ~pred, type = "scatter", mode = "lines+markers",
                  name = "Forecast", line = list(color = "#E4572E", width = 2),
                  marker = list(color = "#E4572E", size = 7), hoverinfo = "text",
                  text = ~paste0(format(date, "%d %b %Y"), "<br>Forecast: ",
                                 round(pred, 1), " µg/m³")) |>
        layout(xaxis = list(title = ""),
               yaxis = list(title = "PM2.5 (µg/m³)"),
               font = list(family = "Inter", size = 14),
               legend = list(orientation = "h", y = -0.15))
    }
  })
  
  output$history_plot <- renderPlotly({
    d <- pm25_model |> filter(city == input$city) |> arrange(date) |> tail(180)
    plot_ly(d, x = ~date, y = ~pm25, type = "scatter", mode = "lines",
            fill = "tozeroy", line = list(color = "#2E86AB", width = 1.5),
            fillcolor = "rgba(46,134,171,0.2)", hoverinfo = "text",
            text = ~paste0(format(date, "%d %b %Y"), "<br>", round(pm25, 1), " µg/m³")) |>
      layout(xaxis = list(title = ""),
             yaxis = list(title = "PM2.5 (µg/m³)"),
             font = list(family = "Inter", size = 14))
  })
  
  output$compare_plot <- renderPlotly({
    d <- pm25_model |> filter(city %in% input$cities)
    validate(need(nrow(d) > 0, "Select at least one city."))
    plot_ly(d, x = ~date, y = ~pm25, color = ~city, colors = pal,
            type = "scatter", mode = "lines",
            hoverinfo = "text",
            text = ~paste0(city, "<br>", format(date, "%d %b %Y"), "<br>",
                           round(pm25, 1), " µg/m³")) |>
      layout(xaxis = list(title = ""),
             yaxis = list(title = "PM2.5 (µg/m³)"),
             font = list(family = "Inter", size = 14),
             legend = list(orientation = "h", y = -0.15))
  })
  
  output$share_plot <- renderPlotly({
    d <- pm25_model |> filter(city %in% input$cities) |>
      mutate(ym = floor_date(date, "month")) |>
      group_by(ym, city) |>
      summarise(avg = mean(pm25), .groups = "drop")
    validate(need(nrow(d) > 0, "Select at least one city."))
    plot_ly(d, x = ~ym, y = ~avg, color = ~city, colors = pal,
            type = "scatter", mode = "lines", stackgroup = "one",
            hoverinfo = "text",
            text = ~paste0(city, "<br>", format(ym, "%b %Y"), "<br>",
                           round(avg, 1), " µg/m³")) |>
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Monthly mean PM2.5 (µg/m³)"),
             font = list(family = "Inter", size = 14),
             legend = list(orientation = "h", y = -0.15))
  })
  
  output$box_plot <- renderPlotly({
    d <- pm25_model |> filter(city %in% input$cities) |>
      mutate(m = factor(month.abb[month(date)], levels = month.abb))
    p <- ggplot(d, aes(m, pm25, fill = city)) +
      geom_boxplot(outlier.size = 0.8, linewidth = 0.3) +
      scale_fill_manual(values = pal) +
      labs(x = NULL, y = "PM2.5 (µg/m³)", fill = NULL) + clean_theme()
    ggplotly(p)
  })
  
  output$weekday_plot <- renderPlotly({
    d <- pm25_model |> filter(city == input$city) |>
      mutate(wd = factor(weekday_levels[wday(date)], levels = weekday_levels)) |>
      group_by(wd) |> summarise(avg = mean(pm25), .groups = "drop")
    p <- ggplot(d, aes(wd, avg, text = paste0(wd, ": ", round(avg, 1), " µg/m³"))) +
      geom_col(fill = "#2E86AB", width = 0.7) +
      labs(x = NULL, y = "Mean PM2.5 (µg/m³)") + clean_theme()
    ggplotly(p, tooltip = "text")
  })
  
  output$error_plot <- renderPlotly({
    d <- test |> mutate(err = pred - pm25)
    p <- ggplot(d, aes(err)) +
      geom_histogram(bins = 35, fill = "#E4572E", alpha = 0.85) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = ink) +
      labs(x = "Error, predicted minus actual (µg/m³)", y = "Count") + clean_theme()
    ggplotly(p)
  })
}

shinyApp(ui, server)
