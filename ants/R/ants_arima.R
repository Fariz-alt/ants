#' ants_arima: Analisis ARIMA / SARIMA Lengkap
#'
#' @param data Data frame berisi deret waktu
#' @param var Nama kolom deret waktu (string)
#' @param seasonal Logical, TRUE jika data musiman
#' @param h Horizon forecasting
#' @param model ARIMA model yang sudah dibuat (opsional), default NULL = auto
#' @return List berisi output lengkap: ADF test, plot, model, signifikansi, diagnostic, forecast
#' @export
ants_arima <- function(data, var, seasonal = FALSE, h = 12, model = NULL){

  if(!requireNamespace("forecast", quietly = TRUE)) stop("Package 'forecast' diperlukan")
  if(!requireNamespace("tseries", quietly = TRUE)) stop("Package 'tseries' diperlukan")
  if(!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' diperlukan")

  library(forecast)
  library(tseries)
  library(ggplot2)

  ts_data <- ts(data[[var]])

  # 1️⃣ ADF test sebelum differencing
  adf_before <- adf.test(ts_data)
  cat("\n========================================\n")
  cat("🟢 1️⃣ Stasioneritas\n")
  cat("========================================\n")
  cat("ADF Test Value :", round(adf_before$statistic,4), "\n")
  cat("ADF Test p-value:", round(adf_before$p.value,4), "\n")

  stationary_before <- adf_before$p.value <= 0.05
  if(!stationary_before){
    cat("Kesimpulan: Data asli tidak stasioner ❌ → melakukan differencing\n")
  } else {
    cat("Kesimpulan: Data asli stasioner ✅\n")
  }

  plot(ts_data, main=paste("Data Asli:", var), col="red", lwd=2)

  # 2️⃣ Differencing jika perlu
  diff_order <- 0
  ts_transformed <- ts_data
  adf_after <- NULL

  if(!stationary_before){
    ts_transformed <- diff(ts_data)
    diff_order <- 1
    adf_after <- adf.test(ts_transformed)
    cat("\n========================================\n")
    cat("🟢 2️⃣ Diferencing\n")
    cat("========================================\n")
    cat("ADF Test Value :", round(adf_after$statistic,4), "\n")
    cat("ADF Test p-value:", round(adf_after$p.value,4), "\n")
    if(adf_after$p.value > 0.05){
      cat("Kesimpulan: Data masih tidak stasioner ❌\n")
    } else {
      cat("Kesimpulan: Data setelah differencing stasioner ✅\n")
    }
    plot(ts_transformed, main=paste("Data Diferencing:", var), col="blue", lwd=2)
  }

  # 3️⃣ Identifikasi model ARIMA/SARIMA
  if(is.null(model)){
    cat("\n====================\n")
    cat("🟡 3️⃣ Identifikasi Model ARIMA/SARIMA\n")
    cat("====================\n")

    par_orig <- par(no.readonly = TRUE)
    par(mfrow=c(1,2))
    acf(ts_transformed, main="ACF")
    pacf(ts_transformed, main="PACF")
    par(par_orig)

    model <- auto.arima(
      ts_data,
      seasonal = seasonal,
      stepwise = FALSE,
      approximation = FALSE
    )
  }

  # Deteksi ARIMA atau SARIMA
  is_sarima <- (model$arma[3] + model$arma[4]) > 0 & model$arma[5] > 1
  model_type <- ifelse(is_sarima, "SARIMA", "ARIMA")
  cat("Model otomatis terpilih:", model_type, "\n")

  # Cetak detail model dalam format ARIMA(p,d,q) atau SARIMA(p,d,q)x(P,D,Q)[m]
  if(is_sarima){
    cat("Detail model: ARIMA(", model$arma[1], ",", model$arma[6], ",", model$arma[2],
        ") x (", model$arma[3], ",", model$arma[7], ",", model$arma[4], ")[", model$arma[5], "]\n", sep="")
  } else {
    cat("Detail model: ARIMA(", model$arma[1], ",", model$arma[6], ",", model$arma[2], ")\n", sep="")
  }

  # 4️⃣ Signifikansi parameter
  coef_table <- summary(model)$coef
  cat("\n====================\n")
  cat("🟠 4️⃣ Signifikansi Parameter\n")
  cat("====================\n")
  print(coef_table)

  # 5️⃣ Diagnostic residual
  cat("\n====================\n")
  cat("🔵 5️⃣ Diagnostic Residual\n")
  cat("====================\n")
  par_orig <- par(no.readonly = TRUE)
  par(mfrow=c(2,2))
  checkresiduals(model)
  par(par_orig)

  lb_test <- Box.test(model$residuals, type="Ljung-Box")
  cat("Ljung-Box test p-value:", round(lb_test$p.value,4), "\n")
  if(lb_test$p.value > 0.05){
    cat("Kesimpulan: Residual tidak ada autokorelasi signifikan ✅\n")
  } else {
    cat("Kesimpulan: Residual ada autokorelasi signifikan ❌\n")
  }

  nor_test <- shapiro.test(model$residuals)
  cat("Shapiro-Wilk p-value:", round(nor_test$p.value,4), "\n")
  if(nor_test$p.value > 0.05){
    cat("Kesimpulan: Residual berdistribusi normal ✅\n")
  } else {
    cat("Kesimpulan: Residual tidak normal ❌\n")
  }

  # 6️⃣ Forecasting
  cat("\n====================\n")
  cat("🟣 6️⃣ Forecasting\n")
  cat("====================\n")
  fc <- forecast(model, h = h)
  plot(fc, main=paste("Forecast", var), col="darkgreen", lwd=2)

  forecast_table <- data.frame(
    Time = time(fc$mean),
    Forecast = round(as.numeric(fc$mean),2),
    Lo80 = round(fc$lower[,1],2),
    Hi80 = round(fc$upper[,1],2),
    Lo95 = round(fc$lower[,2],2),
    Hi95 = round(fc$upper[,2],2)
  )
  cat("\nTabel Forecast:\n")
  print(forecast_table)

  return(list(
    ts_data = ts_data,
    ts_transformed = ts_transformed,
    adf_before = adf_before,
    adf_after = adf_after,
    stationary_before = stationary_before,
    diff_order = diff_order,
    model = model,
    model_type = model_type,
    coef_table = coef_table,
    residuals = model$residuals,
    lb_test = lb_test,
    shapiro_test = nor_test,
    forecast = fc,
    forecast_table = forecast_table
  ))
}
