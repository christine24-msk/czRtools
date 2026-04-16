
#' AUC bias correction
#'
#' This function correct logistic regression AUC by using Efron's bootstrap optimism method
#'
#' @param formula A logistic regression formula used which need to be corrected
#' @param df_input Data
#' @param B Bootstrap times default is 200
#' @param seed If you want to set a seed default is NULL
#'
#' @return bootstrap corrected AUC and CI
#' @examples
#' boot_corrected_auc(
#'  rec5y_yn ~ kps_lab + pdsids_lab + resid_lab1 + bev_lab + brcaparp_lab + rb1_lab,
#'  data = logis_mv1_db,
#'  B = 200,
#'  seed = 304
#' )
#' @examples
#' dxdt = purrr::map_vec(dxdt_orig, date_format)
#'
#' @references Efron, B., & Tibshirani, R. J. (1993). An Introduction to the Bootstrap. Chapman & Hall/CRC.
#' @references Harrell FE Jr. Regression Modeling Strategies: With Applications to Linear Models, Logistic and Ordinal Regression, and Survival Analysis. 2nd ed. Cham, Switzerland: Springer; 2015.
#' @references Harrell Jr FE, Lee KL, Mark DB. Mutlivariable prognostic models: issues in developing models evaluating assumptions and adequacy, and measuring and reducing errors. Statistics in Medicine 1996;15:361–87.
#' @export
boot_corrected_auc <- function(formula, df_input, B = 200, seed = NULL) {

  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("Package 'pROC' is required. Please install it.")
  }

  if (!is.null(seed)) set.seed(seed)

  n <- nrow(df_input)

  # Fit model on original data
  fit_orig <- glm(formula, data = df_input, family = binomial)
  prob_orig <- predict(fit_orig, type = "response")

  # Extract outcome
  outcome_name <- all.vars(formula)[1]
  y <- data[[outcome_name]]

  # Apparent AUC
  auc_app <- as.numeric(pROC::auc(y, prob_orig))

  optimism <- numeric(B)

  for (b in seq_len(B)) {

    # Bootstrap sample indices
    idx <- sample(seq_len(n), replace = TRUE)
    boot_data <- df_input[idx, ]

    # Fit on bootstrap sample
    fit_boot <- glm(formula, data = boot_data, family = binomial)

    # AUC in bootstrap sample
    prob_boot <- predict(fit_boot, type = "response")
    auc_boot <- as.numeric(
      pROC::auc(boot_data[[outcome_name]], prob_boot)
    )

    # Test on original data
    prob_test <- predict(fit_boot, newdata = df_input, type = "response")
    auc_test <- as.numeric(
      pROC::auc(y, prob_test)
    )

    optimism[b] <- auc_boot - auc_test
  }

  auc_corrected <- auc_app - mean(optimism)

  ## calculate the CI ##
  ci_adj <- quantile(optimism, c(0.025, 0.975))
  corrected_ci <- auc_app - ci_adj
  auc_ci_final <- sprintf("%.3f(95%%CI: %.3f-%.3f)", auc_corrected, corrected_ci[[2]], corrected_ci[[1]])

  return(list(
    auc_ci_final = auc_ci_final,
    apparent_auc = auc_app,
    optimism_mean = mean(optimism),
    corrected_auc = auc_corrected,
    corrected_ci = corrected_ci,
    boot_optimism_distribution = optimism
  ))
}

