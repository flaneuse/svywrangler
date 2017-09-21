#' Create factor levels based on labels from a dataset
#'
#' Reads labels from a reference dataset (usually imported from Stata, SPSS, or SAS)
#' and applies them to the same variable in a label-free dataset to create a factor variable
#' with levels and labels. By default, the levels will be sorted by their frequency in the dataset.

#' @author Laura Hughes, laura.d.hughes@gmail.com
#'
#' @param df main data frame containing the dataset to modify
#' @param ref_df reference labelled data frame whose labels you want to use
#' @param var string containing the variable name you want to change to a factor
#' @param new_var string for the name of the new factor-ized variable you create.
#'
# @import dplyr

factorize = function(df, new_var, ...) {
  codebk = pull_labels(df)

  # ref_df has labels associated with it.
  # Note: can pipe with magrittr pipe, a la: df %>% factorize(ref_df, var, new_var)

  vars = quos(...)


  vars = lapply(vars, function(x) quo_name(x))

  factorize1var = function(df, var, new_var, codebk){

  # -- check var is within both df and ref_df --
  if(!var %in% colnames(df)) {
    stop('variable is not in the current dataset (argument df)')
  }

  if(!var %in% codebk$var_name) {
    stop('variable is not in the reference dataset (argument ref_df)')
  }

  # -- pull out the label values --
  codebk = codebk %>% filter(var_name == var)

  # -- create a factor with the labels from the original dataset --
  # levels will be sorted by the frequency of occurance (high to low)
  df = df %>%
    transmute_(.dots = setNames(
      list(paste0('forcats::fct_infreq(
                  factor(', var, ',',
                  'levels = ', list(codebk$codes), ',',
                  'labels = ', list(codebk$labels),'))'
                  )), paste0(var, new_var)
    ))

  return(df)

  }

  new_cols = lapply(vars, function(x) factorize1var(df, x, new_var, codebk)) %>% bind_cols()

  return(df %>% bind_cols(new_cols))

}

