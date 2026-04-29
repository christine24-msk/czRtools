#' label the factor
#'
#' This function format pvalue turning it to character
#'
#' @param x variable name
#' @param levels variable levels
#' @param labels each levels' label
#' @param zq_label this variable label
#'
#' @return labeled variable (used in mutation())
#' @examples
#' bmi_lab = factor_label(bmi, leves=c(0,1), labels=c("Not_obese", "Obese"), zq_label="Obesity")
#'
#' @export
factor_label <- function(x, levels, labels, zq_label) {
  tmp <- factor(x, levels = levels, labels = labels)
  labelled::var_label(tmp) <- zq_label
  tmp
}
