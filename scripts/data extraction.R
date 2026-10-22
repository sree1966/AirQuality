# ----------------------------------------------------------
# CSI3005 Case Study: PM2.5 Forecasting Across Indian Cities
# Step 1-3: Pull hourly PM2.5, aggregate to daily, add features
# ----------------------------------------------------------
library(slider)
library(httr2)
library(tidyverse)
library(janitor)
library(lubridate)

# ---- 1. Setup folders -----------------------------------------
dir.create("data/raw",    recursive = TRUE, showWarnings = FALSE)
dir.create("data/clean",  recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# ---- 2. Cities ------------------------------------------------
cities <- tribble(
  ~city,       ~lat,  ~lon,
  "Delhi",     28.61, 77.21,
  "Mumbai",    19.08, 72.88,
  "Kolkata",   22.57, 88.36,
  "Chennai",   13.08, 80.27,
  "Bengaluru", 12.97, 77.59
)

# ---- 3. Download hourly PM2.5 ---------------------------------
get_pm25 <- function(lat, lon) {
  resp <- request("https://air-quality-api.open-meteo.com/v1/air-quality") |>
    req_url_query(latitude = lat, longitude = lon, hourly = "pm2_5",
                  start_date = "2023-01-01", end_date = "2026-10-15",
                  timezone = "Asia/Kolkata") |>
    req_timeout(300) |>
    req_perform() |>
    resp_body_json()
  
  tibble(time = unlist(resp$hourly$time),
         pm25 = unlist(resp$hourly$pm2_5))
}

pm25_hourly <- cities |>
  mutate(data = map2(lat, lon, get_pm25)) |>
  select(city, data) |>
  unnest(data) |>
  mutate(time = ymd_hm(time))

write_csv(pm25_hourly, "data/raw/pm25_hourly.csv")

pm25_hourly |> count(city)

# ---- 4. Hourly to daily ---------------------------------------
pm25_daily <- pm25_hourly |>
  mutate(date = as_date(time)) |>
  group_by(city, date) |>
  summarise(pm25 = mean(pm25, na.rm = TRUE), .groups = "drop") |>
  arrange(city, date)

# Drop cities/days with gaps: keep only complete daily series
# (check missing days per city)
pm25_daily |> count(city)

# ---- 5. Feature engineering -----------------------------------
pm25_model <- pm25_daily |>
  group_by(city) |>
  mutate(
    lag1      = lag(pm25, 1),
    roll7     = slide_dbl(pm25, mean, .before = 6, .complete = TRUE),
    month     = month(date),
    weekday   = wday(date, label = TRUE),
    season    = case_when(
      month %in% c(12, 1, 2)  ~ "Winter",
      month %in% c(3, 4, 5)   ~ "Summer",
      month %in% c(6, 7, 8, 9) ~ "Monsoon",
      TRUE                     ~ "Post-monsoon"
    )
  ) |>
  ungroup() |>
  drop_na(lag1, roll7)

write_csv(pm25_model, "C:/Users/kvsre/AirQuality/data/pm25_daily.csv")

# ---- 6. Time-based train/test split ---------------------------
cutoff <- max(pm25_model$date) - days(90)   # last ~3 months = test

train <- pm25_model |> filter(date <= cutoff)
test  <- pm25_model |> filter(date >  cutoff)

test |> count(city)
pm25_model |> count(city)
cat("Train rows:", nrow(train), " Test rows:", nrow(test), "\n")