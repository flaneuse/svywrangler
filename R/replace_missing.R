#' @description  Replace multiple missing codes with NAs over any number of variables
#' @import dplyr
#' @export
#'
#' @examples
#' x = data.frame(var1 = sample(c(1:10, 9996:9999), 25, replace = TRUE), var2 = sample(c(1:10, 9996:9999), 25, replace = TRUE))
#' x
#' replace_missing(x, c(9996, 9997, 9998, 9999), var1)
#' replace_missing(x, c(9998, 9999), var1, var2)

replace_missing = function(df, missing_codes,  ...) {

  # more generic version of Hadley's na_if which can take in list of codes rather than single one.
  na.if = function(df, missing_codes) {
    df[df %in% missing_codes] = NA

    return(df)
  }

    var = quos(...)

    df %>% mutate_at(funs(na.if(., missing_codes)), .vars = var)
}


