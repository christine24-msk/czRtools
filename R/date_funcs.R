#' Format_Date
#'
#' This function takes whatever assuming to be date value_text,number,date transform them into the correct date format
#'
#' @param x A date value which is inputted as or as not the format as date
#'
#' @return a formated date
#' @examples
#' date_format(34567)
#' @examples
#' dxdt = purrr::map_vec(dxdt_orig, date_format)
#'
#' @export

date_format <- function(date_input){
  if(!is.na(date_input) & stringr::str_detect(date_input, "/")){
      #check if the format using two digits year #
    if(stringr::str_sub(date_input,-3,-3)=="/"){ return(as.Date(date_input, tryFormats = '%m/%d/%y') )}
      #otherwise the year should be four digits year #
    else{return(as.Date(date_input, tryFormats = '%m/%d/%Y') )}
  }
      # some input show as mm-dd-yyyy
  else if(!is.na(date_input) & stringr::str_detect(date_input, "-")){ return(as.Date(date_input, tryFormats = '%Y-%m-%d') )}
  else if(is.na(date_input)){ return(NA) }
     ##numeric value for date when read in ##
  else{ return(as.Date(as.numeric(date_input), origin = '1899-12-30')) }
}




#' Check date co-variables' relationship
#'
#' This function check if one date variable_which should be earlier then the other date variables
#'
#' @param dt.mat A dataframe include all the dates columns in your data
#' @param small.col A date column which should show earlier
#' @param larger.col A date column which should show larger
#' @param equal.show A logic inputted vector as TRUE or FALSE-whether to show the eaqual date
#' @param direct_out A logic input vector as TRUE or FALSE-whether to output the error in console
#'
#' @return conflict date error
#' @examples
#' date_check(dt_data, small.col=req(2,3), larger.col=c(4,5,6), equal.show=rep(FALSE,3), direct_out=rep(TRUE,3))
#'
#' @export

dt_check<-function(dt.mat, small.col, larger.col, equal.show, direct_out){
   print('!!! input need to be data.frame')
   require(rms)
   require(survival)
   require(dplyr)
        final.out.d<-NULL

     for(i in 1:length(small.col)){
        label.small<- dt.mat |> select(small.col[i]) |> label()
        label.large<- dt.mat |> select(larger.col[i]) |>label()

        name.small<-dt.mat |> select(small.col[i]) |> names()
        name.large<-dt.mat |> select(larger.col[i]) |> names()
        cat("\n")
        cat(label.small, ">", label.large, "?:","\n")
        temp.name<-paste("Is there any '",label.small,"' later than '",label.large,"'?")
        sk<-small.col[i]
        lk<-larger.col[i]
        if(!equal.show[i]){
        temp.xx<-dt.mat[,1][!is.na(dt.mat[,sk]) & !is.na(dt.mat[,lk]) & dt.mat[,sk]>dt.mat[,lk],]}
         else if(equal.show[i]){
        temp.xx<-dt.mat[,1][!is.na(dt.mat[,sk]) & !is.na(dt.mat[,lk]) & dt.mat[,sk]>=dt.mat[,lk],]}

        if(nrow(temp.xx)>=1){
           temp.xx.out <- dt.mat |>
                          inner_join(temp.xx) |>
                          select(all_of(c(1, sk, lk))) |>
                          mutate(
                            !!name.small := structure(format(ymd(.data[[name.small]]), '%m/%d/%Y'), label = label.small),
                            !!name.large := structure(format(ymd(.data[[name.large]]), '%m/%d/%Y'), label = label.large),
                          )
           print(temp.xx.out)

        #  show.sk<-format(ymd(temp.xx[,sk]), '%m/%d/%Y')

         if(!direct_out){final.out.d<-append(final.out.d, list(temp.xx.out))}
         else if(direct_out){
        #   temp_outdf<-knitr::kable(temp.xx.out, format='markdown',booktabs=T, align='lccc', caption= temp.name)
           temp_outdf <- temp.xx.out %>%
                         flextable::flextable() %>%
                         flextable::align(j=1, align = 'left', part = 'body') %>%
                         flextable::align(j=c(2,3), align = 'center', part = 'body') %>%
                         flextable::align(j=1:3, align = 'center', part = 'header') %>%
                         flextable::bold(j=1:3, bold=T, part = 'header')
           final.out.d<-append(final.out.d, list(temp_outdf))
         }

         }
        else{print("good")} }

       #    names(final.out.d)<-final.list.name
           return(final.out.d)
        }




### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #######
############ Permutation PV add #############
perm.pv.func0 <- function(input.dt, input.uv.df, change_vars, change_to_pvs){
  require(rms)
  for(i in 1:length(change_vars)){
    tt.label = paste0("__",label(input.dt[[change_vars[i]]]),"__")
    tt.nrow <- seq(1:nrow(input.uv.df))[input.uv.df$Var==tt.label] +1
    #  print(tt.nrow)
    input.uv.df$PV[tt.nrow]<- change_to_pvs[i]
  }
  return(input.uv.df)
}

perm.pv.func <- function(input.dt, input.uv.df, change_vars, change_to_pvs){
  require(labels)
  for(i in 1:length(change_vars)){
    tt.label = label(input.dt[[change_vars[i]]])
    print(tt.label)
    tt.nrow <- seq(1:nrow(input.uv.df))[input.uv.df$Var==tt.label] +1
      print(tt.nrow)
    input.uv.df$PV[tt.nrow]<- change_to_pvs[i]
  }
  return(input.uv.df)
}

#### change to permutation pv example
#ttt <- perm.pv.func(input.dt=cnl.wk, input.uv.df=cnl_pfs.uv1[[1]], change_vars=c('ovretain_lab','ovsize_lab'), change_to_pvs = c('0.274', '0.311'))

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #######
############ PV format #############
pv.format.zq<-function(pv){
  pv_out <- ifelse(is.na(pv),'',
               ifelse(pv<0.001, '__<0.001__',
                   ifelse(pv>=0.001 & pv<0.05, paste0('__',sprintf('%.3f',pv),'__'), sprintf('%.3f',pv)))
  )
  return(pv_out) }





### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #######
############ GPT: bootstrap corrected AUC for logistic regression #############
boot_corrected_auc <- function(formula, data, B = 200, seed = NULL) {

  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("Package 'pROC' is required. Please install it.")
  }

  if (!is.null(seed)) set.seed(seed)

  n <- nrow(data)

  # Fit model on original data
  fit_orig <- glm(formula, data = data, family = binomial)
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
    boot_data <- data[idx, ]

    # Fit on bootstrap sample
    fit_boot <- glm(formula, data = boot_data, family = binomial)

    # AUC in bootstrap sample
    prob_boot <- predict(fit_boot, type = "response")
    auc_boot <- as.numeric(
      pROC::auc(boot_data[[outcome_name]], prob_boot)
    )

    # Test on original data
    prob_test <- predict(fit_boot, newdata = data, type = "response")
    auc_test <- as.numeric(
      pROC::auc(y, prob_test)
    )

    optimism[b] <- auc_boot - auc_test
  }

  auc_corrected <- auc_app - mean(optimism)

  return(list(
    apparent_auc = auc_app,
    optimism = mean(optimism),
    corrected_auc = auc_corrected,
    boot_optimism_distribution = optimism
  ))
}

