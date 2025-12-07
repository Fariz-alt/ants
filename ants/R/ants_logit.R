#' ants_logit: Regresi Logistik Lengkap ala ants
#'
#' @param data Data frame
#' @param formula Formula model (y ~ x1 + x2)
#' @param conf.level Confidence level, default 0.95
#' @param include_odds_ratio Tampilkan OR, default TRUE
#' @param plot_roc Tampilkan ROC Curve, default TRUE
#' @param plot_effects Tampilkan efek variabel, default FALSE
#' @param return_model Kembalikan objek model, default FALSE
#' @return List berisi summary, OR, ROC, Confusion Matrix, efek variabel
ants_logit <- function(data, formula, conf.level=0.95,
                       include_odds_ratio=TRUE, plot_roc=TRUE,
                       plot_effects=FALSE, return_model=FALSE){

  # ==== 1. Cek distribusi variabel dependen ====
  dep_var <- all.vars(formula)[1]
  y <- data[[dep_var]]
  if(length(unique(y)) != 2) stop("Variabel dependen harus biner (0/1 atau factor).")
  cat("Variabel dependen:", dep_var, "terdeteksi biner.\n")

  # ==== 2. Fit model regresi logistik ====
  model <- glm(formula, data=data, family=binomial)
  summary_model <- summary(model)

  # ==== 3. Odds Ratio dan CI ====
  if(include_odds_ratio){
    OR <- exp(cbind(Estimate=coef(model), confint(model, level=conf.level)))
    colnames(OR) <- c("OR","2.5%","97.5%")
  } else { OR <- NULL }

  # ==== 4. Evaluasi model ====
  # Prediksi probabilitas
  pred_prob <- predict(model, type="response")
  pred_class <- ifelse(pred_prob >= 0.5, 1, 0)

  # Confusion Matrix
  conf_matrix <- table(Predicted=pred_class, Actual=y)

  # ROC & AUC
  if(plot_roc){
    if(!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
    library(pROC)
    roc_obj <- roc(y, pred_prob)
    auc_val <- auc(roc_obj)
    plot(roc_obj, main=paste("ROC Curve - AUC:", round(auc_val,3)))
  } else { roc_obj <- auc_val <- NULL }

  # ==== 5. Plot efek variabel ====
  if(plot_effects){
    if(!requireNamespace("effects", quietly = TRUE)) install.packages("effects")
    library(effects)
    plot(allEffects(model), main="Effects Plot")
  }

  # ==== 6. Return list ====
  result <- list(
    summary = summary_model,
    odds_ratio = OR,
    confusion_matrix = conf_matrix,
    roc = roc_obj,
    auc = auc_val
  )
  if(return_model) result$model <- model

  return(result)
}


