#' pvalue format
#'
#' This function format pvalue turning it to character
#'
#' @param pv_input
#'
#' @return a character pvalue(<0.001) or 3 digits pvalue
#' @examples
#' pv_char = purrr::map_vec(pv_input, pv_format)
#'
#' @export
pv_format<-function(pv_input){
  pv_out <- ifelse(is.na(pv_input),'',
               ifelse(pv_input<0.001, '__<0.001__',
                   ifelse(pv_input>=0.001 & pv<0.05, paste0('__',sprintf('%.3f',pv_input),'__'), sprintf('%.3f',pv_input)))
  )
  return(pv_out) }



#' Change to permutation pv
#'
#' This function replace the program generating log-rank with permutation log-rank test pvalue (which should be running separately)
#'
#' @param infput.df Original data with which the univariate survival analyses were done
#' @param infput.uv.df The program generated univariate analyses result data_frame
#' @param change_vars variables which pvalue should be changed to permutation pv or ''
#' @param change_to_pvs permutation pvalue or NA
#'
#' @return another survival analyses data frame-replacing original pvalue to be permutation pvalue
#' @examples
#' perm_pv_change(input.df=latrec.wk, input.uv.df=pfs.uv[[1]], change_vars=c('race_lab','hist_lab'), changeto_pvs=c(0.9865, 0.0005))
#'
#' @export
perm_pv_change <- function(input.df, input.uv.df, change_vars, change_to_pvs){
  require(rms)
  require(labelled)
    ## format the inputed pvalue first ##
  pv_char = purrr::map_vec(change_to_pvs, pv_format)
  for(i in 1:length(change_vars)){
    tt.label = paste0("__",label(input.df[[change_vars[i]]]),"__")
    tt.nrow <- seq(1:nrow(input.uv.df))[input.uv.df$Var==tt.label] +1
    #  print(tt.nrow)
    input.uv.df$PV[tt.nrow]<- pv_char[i]
  }
  return(input.uv.df)
}


