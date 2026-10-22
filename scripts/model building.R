library(randomForest)

set.seed(42)

rf_model <- randomForest(
  pm25 ~ lag1 + roll7 + month + weekday + city,
  data = train,
  ntree = 500,
  importance = TRUE
)

test$pred <- predict(rf_model, newdata = test)

rmse <- sqrt(mean((test$pred - test$pm25)^2))
r2   <- 1 - sum((test$pm25 - test$pred)^2) / sum((test$pm25 - mean(test$pm25))^2)

cat("Test RMSE:", round(rmse, 2), " R2:", round(r2, 3), "\n")

importance(rf_model)
varImpPlot(rf_model)
dir.create("output", showWarnings = FALSE)
saveRDS(rf_model, "C:/Users/kvsre/AirQuality/output/rf_model.rds")