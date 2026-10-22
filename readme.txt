# PM2.5 Forecast Studio

A machine-learning dashboard that forecasts next-day PM2.5 air pollution for five Indian cities (Delhi, Mumbai, Kolkata, Chennai, Bengaluru). It uses a random forest model trained on daily data from the Open-Meteo Air Quality API and presents results in an interactive Shiny app.

## Features

- **Forecast tab:** test-period comparison of actual vs predicted PM2.5, plus forecasts for a chosen future date or the next 7 days, with a ±RMSE band.
- **Compare Cities tab:** daily PM2.5 across selected cities and monthly distribution by city.
- **Patterns tab:** average PM2.5 by weekday and the forecast error histogram.
- **About tab:** model summary and caveats.

## Project Structure

```
AirQuality/
├── data/
│   ├── raw/                  # Hourly PM2.5 pulled from the API
│   ├── clean/                # Daily data with engineered features
│   └── pm25_daily.csv        # Model input used by the app
├── output/
│   ├── rf_model.rds          # Saved random forest model
│   ├── model_metrics.csv     # Test RMSE and R²
│   ├── variable_importance.csv
│   └── figures/              # Plots used in the report
├── scripts/
│   └── air_quality_pipeline.R  # Download, clean, feature engineering, training
├── app.R                     # Shiny dashboard
└── README.md
```

## Data

- **Source:** [Open-Meteo Air Quality API](https://open-meteo.com/en/docs/air-quality-api)
- **Pollutant:** PM2.5 (µg/m³), hourly, aggregated to daily means
- **Cities:** Delhi, Mumbai, Kolkata, Chennai, Bengaluru
- **Period:** 1 January 2023 to 15 October 2026

## Method

1. **Download** hourly PM2.5 for each city from the API.
2. **Aggregate** to daily means per city.
3. **Engineer features:** `lag1` (yesterday's PM2.5), `roll7` (7-day rolling mean), `month`, `weekday`, `city`.
4. **Split by time:** training runs up to 17 July 2026; the test set is the last 90 days (18 July to 15 October 2026).
5. **Train** a random forest (500 trees) to predict the same day's PM2.5 from the features above.
6. **Evaluate** on the held-out test set using RMSE and R².
7. **Forecast** future dates recursively: each predicted day becomes the next day's `lag1` input.

## Results (test period)

| Metric | Value |
|---|---|
| Test RMSE | ~9.8 µg/m³ |
| Test R² | ~0.84 |

Most important predictors: `lag1`, `roll7`, `month`, `city`. `weekday` contributes little.

## Limitations

- Accuracy is measured on one 90-day window, mostly monsoon. Winter performance is not yet validated.
- Only one-day-ahead accuracy is validated. Multi-day forecasts accumulate error.
- No weather inputs are used yet.
- R² is pooled across cities; per-city accuracy should be reported separately.
- For future dates, only the forecast and an approximate ±RMSE band are shown, since no actual values exist yet.

## Installation

Requires R (≥ 4.1) and the following packages:

```r
install.packages(c("shiny", "bslib", "bsicons", "tidyverse",
                   "randomForest", "lubridate", "httr2", "janitor"))
```

## How to Run

1. **Run the pipeline** to download data and train the model:
```r
   source("scripts/air_quality_pipeline.R")
```
2. **Launch the dashboard** from RStudio by opening `app.R` and clicking **Run App**, or from the console:
```r
   shiny::runApp("app.R")
```

Update the file paths in `app.R` and the pipeline to relative paths (for example, `"output/rf_model.rds"`) before sharing the repository, so the project runs on any machine.

