#' @export
flm <- function(formula, data) {

  # ----------------------------------------------------------
  # 1. FIT MODEL
  # ----------------------------------------------------------
  model <- lm(formula, data)
  sm <- summary(model)

  # ----------------------------------------------------------
  # 2. REGRESSION EQUATION
  # ----------------------------------------------------------
  co <- coef(model)
  terms <- names(co)

  eq_str <- paste0(
    "ŷ = ",
    paste(
      sprintf("%.4f * %s", co, terms),
      collapse = " + "
    )
  )
  eq_str <- gsub(" *Intercept", "", eq_str)

  # ----------------------------------------------------------
  # 3. T-TEST (PARSIAL)
  # ----------------------------------------------------------
  ttab <- as.data.frame(sm$coefficients)
  names(ttab) <- c("Estimate", "Std_Error", "t_value", "p_value")
  ttab$Term <- rownames(ttab)
  rownames(ttab) <- NULL

  ttab$Kesimpulan <- ifelse(
    ttab$p_value < 0.05,
    "Berpengaruh signifikan",
    "Tidak berpengaruh signifikan"
  )

  # ----------------------------------------------------------
  # 4. F-TEST (MANUAL — TANPA ANOVA)
  # ----------------------------------------------------------
  Fvalue <- sm$fstatistic[1]
  df1    <- sm$fstatistic[2]
  df2    <- sm$fstatistic[3]
  pvalueF <- pf(Fvalue, df1, df2, lower.tail = FALSE)

  # SSE (Galat)
  SSE <- sum(resid(model)^2)

  # MSE
  MSE <- SSE / df2

  # SSR dari rumus F = (SSR/df1) / MSE
  SSR <- Fvalue * MSE * df1

  # Total
  SST <- SSR + SSE
  df_total <- df1 + df2

  # Buat tabel ANOVA manual
  anova_tab <- data.frame(
    Sumber = c("Regresi", "Galat", "Total"),
    Df = c(df1, df2, df_total),
    JK = c(SSR, SSE, SST),
    RJK = c(SSR/df1, MSE, NA),
    F_hitung = c(Fvalue, NA, NA),
    p_value = c(pvalueF, NA, NA)
  )

  simultan_kesimpulan <- ifelse(
    pvalueF < 0.05,
    "Model signifikan (variabel bebas secara bersama-sama mempengaruhi Y)",
    "Model tidak signifikan"
  )

  # ----------------------------------------------------------
  # 5. VIF + Kesimpulan
  # ----------------------------------------------------------
  vif_val <- car::vif(model)

  vif_ket <- ifelse(
    vif_val < 5, "Tidak ada multikolinearitas",
    ifelse(vif_val < 10, "Perlu perhatian (mulai ada multikolinearitas)",
           "Multikolinearitas tinggi!")
  )

  vif_tab <- data.frame(
    Term = names(vif_val),
    VIF = vif_val,
    Kesimpulan = vif_ket
  )

  # ----------------------------------------------------------
  # 6. ASSUMPTION TESTS
  # ----------------------------------------------------------
  residu <- resid(model)

  sw <- shapiro.test(residu)
  bp <- car::ncvTest(model)
  dw <- car::durbinWatsonTest(model)

  assumption_tab <- data.frame(
    Uji = c(
      "Normalitas (Shapiro-Wilk)",
      "Heteroskedastisitas (Breusch-Pagan)",
      "Autokorelasi (Durbin-Watson)"
    ),
    p_value = c(sw$p.value, bp$p, dw$p),
    Kesimpulan = c(
      ifelse(sw$p.value > 0.05, "Lulus Normalitas", "Tidak Lulus Normalitas"),
      ifelse(bp$p > 0.05, "Homoskedastis (Lulus)", "Heteroskedastisitas"),
      ifelse(dw$p > 0.05, "Tidak Ada Autokorelasi", "Ada Autokorelasi")
    )
  )

  # ----------------------------------------------------------
  # 7. R-SQUARE INTERPRETATION
  # ----------------------------------------------------------
  r2 <- sm$r.squared
  ar2 <- sm$adj.r.squared

  r2_int <- paste0(
    "Sebesar ", sprintf("%.2f%%", r2 * 100),
    " variasi variabel dependen dapat dijelaskan oleh model."
  )

  ar2_int <- paste0(
    "Setelah menyesuaikan jumlah variabel, model menjelaskan ",
    sprintf("%.2f%%", ar2 * 100),
    " variasi Y."
  )

  # ----------------------------------------------------------
  # 8. OUTPUT OBJECT
  # ----------------------------------------------------------
  out <- list(
    model = model,
    equation = eq_str,
    coef_table = ttab,
    anova_table = anova_tab,
    vif_table = vif_tab,
    assumption_table = assumption_tab,
    r_squared = r2,
    adj_r_squared = ar2,
    f_pvalue = pvalueF,
    kesimpulan_simultan = simultan_kesimpulan,
    interpretasi_r2 = r2_int,
    interpretasi_adj_r2 = ar2_int
  )

  class(out) <- "flm"
  return(out)
}
