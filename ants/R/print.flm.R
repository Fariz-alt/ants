#' @export
print.flm <- function(x, ...) {
  cat("\n=========================================\n")
  cat("     REGRESI LINEAR\n")
  cat("=========================================\n\n")

  cat("📌 Persamaan Regresi:\n")
  cat(x$equation, "\n")

  cat("\n📊 Koefisien (Uji Parsial / t-test):\n")
  print(knitr::kable(x$coef_table, format = "rst"))

  cat("\n🔎 Uji Simultan (F-test):\n")
  print(knitr::kable(x$anova_table, format = "rst"))
  cat("\nKesimpulan Simultan: ", x$kesimpulan_simultan, "\n")

  cat("\n🧪 Uji Multikolinearitas (VIF):\n")
  print(knitr::kable(x$vif_table, format = "rst"))

  cat("\n🧪 Uji Asumsi Klasik:\n")
  print(knitr::kable(x$assumption_table, format = "rst"))

  cat("\n📈 Interpretasi R-Square & Adjusted R-Square:\n")
  cat("R-Square = ", round(x$r_squared, 4), "\n")
  cat(x$interpretasi_r2, "\n\n")
  cat("Adjusted R-Square = ", round(x$adj_r_squared, 4), "\n")
  cat(x$interpretasi_adj_r2, "\n")
}
