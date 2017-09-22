#' @import dplyr
#' @export
#'
pull_labels = function(df) {
  cols = colnames(df)

  get_labels = function(df, col) {
    label_name = attr(df[[col]], 'label')

    labels = attr(df[[col]], 'labels')

    if (is.null(labels)) {
      return(NA)
    } else{
      data.frame(descrip = label_name, codes = labels) %>% mutate(labels = row.names(.))
    }
  }

  lapply(cols, function(x) data.frame(var_name = x, get_labels(df, x)))  %>% bind_rows() %>% select(-`get_labels.df..x.`)
}

#' @import dplyr stringr data.table
#' @export
id_weirdos = function(df, lookfor = c('or more', 'unknown', "don't"), lookfor_codes = c(8, 9, 95:99, 996:999, 9996:9999)) {
  codebk = pull_labels(df)

  codebk %>%
    mutate(labels = str_to_lower(labels)) %>%
    filter_(paste0(paste(paste0('labels %like% "', lookfor, '"'), collapse = '|'), '| codes %in% c(', paste(lookfor_codes, collapse = ','), ")"))
}

#' @import dplyr
#' @export
id_decimals = function(df) {
  codebk = pull_labels(df)

  codebk %>%
    filter(descrip %like% 'decimals')
}
