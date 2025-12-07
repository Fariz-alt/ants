#' Multivariate Spline Regression with Automatic Knot Selection
#'
#' @param formula formula
#' @param data data frame
#' @param type spline type: "linear","quadratic","cubic","quartic"
#' @param max_knots maximum number of knots per variable
#' @export
regspline <- function(formula, data,
                      type = c("linear","quadratic","cubic","quartic"),
                      max_knots = 3) {

  library(splines)
  library(ggplot2)
  library(dplyr)

  type <- match.arg(type)
  degree <- switch(type,
                   linear = 1,
                   quadratic = 2,
                   cubic = 3,
                   quartic = 4)

  # extract variables
  mf <- model.frame(formula, data)
  yname <- names(mf)[1]
  xnames <- names(mf)[-1]
  y <- mf[[1]]

  # generate basis function for a single variable with k knots
  generate_basis <- function(x, k) {
    if (k == 0) {
      list(basis = matrix(x, ncol = 1), info = NULL)
    } else {
      knots <- as.numeric(quantile(x, probs = seq(0.1, 0.9, length.out = k)))
      b <- bs(x, degree = degree, knots = knots,
              Boundary.knots = range(x))
      info <- list(
        degree = degree,
        knots = knots,
        Boundary.knots = range(x)
      )
      list(basis = b, info = info)
    }
  }

  # try all combinations of knot counts for each variable
  knot_grid <- expand.grid(rep(list(0:max_knots), length(xnames)))
  colnames(knot_grid) <- xnames

  best_aic <- Inf
  best_model <- NULL
  best_info <- list()

  for (i in 1:nrow(knot_grid)) {
    df2 <- data.frame(y = y)
    info_list <- list()

    for (xv in xnames) {
      k <- knot_grid[i, xv]
      basis <- generate_basis(data[[xv]], k)
      bmat <- basis$basis

      # basis columns names
      if (is.null(basis$info)) {
        colnames(bmat) <- paste0(xv)  # linear
      } else {
        colnames(bmat) <- paste0(xv, "_s", seq_len(ncol(bmat)))
      }

      df2 <- cbind(df2, bmat)
      info_list[[xv]] <- basis$info
    }

    f <- as.formula(paste("y ~ ."))
    mod <- lm(f, data = df2)

    if (AIC(mod) < best_aic) {
      best_aic <- AIC(mod)
      best_model <- mod
      best_info <- info_list
      best_df2 <- df2
      best_k <- knot_grid[i,]
    }
  }

  # -------------------------------
  # GENERATE PLOTS FOR EACH VARIABLE
  # -------------------------------
  plots <- list()

  for (xv in xnames) {

    grid_x <- seq(min(data[[xv]]), max(data[[xv]]), length.out = 200)
    grid <- data.frame(grid_x)
    names(grid) <- xv

    # add basis functions to prediction grid
    if (!is.null(best_info[[xv]])) {
      bnew <- bs(grid[[xv]],
                 degree = best_info[[xv]]$degree,
                 knots = best_info[[xv]]$knots,
                 Boundary.knots = best_info[[xv]]$Boundary.knots)
      colnames(bnew) <- paste0(xv, "_s", seq_len(ncol(bnew)))
      grid <- cbind(grid, bnew)
    } else {
      grid[[xv]] <- grid_x
    }

    # build grid with all other variables fixed at mean
    full_pred <- data.frame(matrix(nrow = 200, ncol = 0))

    for (var in colnames(best_df2)[-1]) {
      if (grepl(xv, var)) {
        full_pred[[var]] <- grid[[var]]
      } else {
        # set other basis columns to mean
        full_pred[[var]] <- mean(best_df2[[var]])
      }
    }

    pred_y <- predict(best_model, newdata = full_pred)

    plot_df <- data.frame(x = grid_x, yhat = pred_y)

    p <- ggplot(data, aes_string(x = xv, y = yname)) +
      geom_point() +
      geom_line(data = plot_df, aes(x = x, y = yhat),
                linewidth = 1.2, color = "blue") +
      labs(title = paste("Spline Regression:", xv),
           x = xv, y = yname) +
      theme_minimal()

    plots[[xv]] <- p
  }

  # -------------------------------
  # OUTPUT
  # -------------------------------
  out <- list(
    type = type,
    degree = degree,
    best_k = best_k,
    model = best_model,
    summary = summary(best_model),
    coef = summary(best_model)$coefficients,
    anova = anova(best_model),
    AIC = AIC(best_model),
    knots = lapply(best_info, function(x) { if (!is.null(x)) x$knots else NA }),
    boundary = lapply(best_info, function(x) { if (!is.null(x)) x$Boundary.knots else NA }),
    plots = plots
  )

  class(out) <- "regspline"
  return(out)
}


