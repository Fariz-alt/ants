#' @export
print.regspline <- function(x, ...) {
  cat("\n=========================\n")
  cat("📌  REGSPLINE MODEL SUMMARY\n")
  cat("=========================\n\n")

  cat("Spline Type :", x$type, "\n")
  cat("Degree      :", x$degree, "\n")
  cat("Knots per variable:\n")

  for (v in names(x$knots)) {
    cat(sprintf("  • %s : %s\n", v,
                paste(round(x$knots[[v]], 4), collapse = ", ")))
  }

  cat("\nAIC :", x$AIC, "\n\n")
  print(x$summary)

  invisible(x)
}
