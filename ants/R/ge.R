#' gene: Generate synthetic data with specific characteristics
#'
#' @param n Number of observations
#' @param p Number of predictor variables
#' @param type Vector of characteristics:
#'   c("multikolinearitas", "heteroskedastisitas", "autokorelasi", "residual_tidak_normal", "tidak_linear")
#' @return Data frame with predictors X1..Xp and response Y
#' @export
gene <- function(n, p, type = c("multikolinearitas", "heteroskedastisitas", "autokorelasi", "residual_tidak_normal", "tidak_linear")) {

  # ===== 1. Base predictors =====
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste0("X", 1:p)

  # ===== 2. Multikolinearitas =====
  if("multikolinearitas" %in% type) {
    # buat kolom terakhir kombinasi linear dari beberapa kolom lain + noise kecil
    X[,p] <- rowSums(X[,1:min(3,p-1), drop=FALSE]) + rnorm(n, 0, 0.1)
  }

  # ===== 3. Residual =====
  # default residual normal
  eps <- rnorm(n, 0, 1)

  # Non-normal residual
  if("residual_tidak_normal" %in% type) {
    eps <- rt(n, df=3)  # t-distribution heavy tails
  }

  # Heteroskedastisitas
  if("heteroskedastisitas" %in% type) {
    # var residual tergantung pada X1
    eps <- eps * (1 + 0.5 * X[,1])
  }

  # Autokorelasi
  if("autokorelasi" %in% type) {
    rho <- 0.7
    for(i in 2:n) {
      eps[i] <- rho * eps[i-1] + eps[i]
    }
  }

  # ===== 4. Response Y =====
  beta <- runif(p, 0.5, 1.5)
  Y <- X %*% beta + eps

  # Tidak linearitas
  if("tidak_linear" %in% type) {
    # tambahkan interaksi / kuadrat
    Y <- Y + 0.5 * X[,1]^2 - 0.3 * X[,2]*X[,3]
  }

  # Kembalikan data frame
  data <- data.frame(Y, X)
  return(data)
}


