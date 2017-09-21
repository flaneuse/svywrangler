#' @examples 
#' dummize(mtcars, remove_factors = FALSE, cyl)
#' dummize(mtcars, remove_factors = TRUE, cyl, gear)

dummize = function(df, remove_factors, ...) {
  
  var = quos(...)
  var_formulas = lapply(var, function(x) as.formula(paste0('~', quo_name(x), '-1')))
  var_names = lapply(var, function(x) quo_name(x))
  
# Convert to factors, if not already  
  df = df %>% mutate_at(.funs = funs(as.factor(.)), .vars = var)
  
  # create dummies across list of vars
  dummies = lapply(var_formulas, function(x) 
    model.matrix(x, model.frame(~ ., df, na.action = na.pass)) %>% data.frame())
  
  
  df = df %>% bind_cols(dummies)
  
  if(remove_factors == TRUE) {
    var_names = unlist(var_names)
    return(df %>% select(-one_of(var_names)))
  } else {
    return(df)
  }
}
