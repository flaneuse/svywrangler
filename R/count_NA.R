#' @import dplyr
#' @export
count_NA = function(df, filter = TRUE){
  temp = t(df %>% summarise_all(funs(sum(is.na(.)))))

  temp = data.frame(var = row.names(temp), n = temp) %>% arrange(desc(n))

  if(filter == TRUE) {
    temp %>% filter(n > 0)
  } else {
    temp
  }
}

#' @import dplyr
#' @export
count_value = function(df, value = c(8, 9, 98, 99, 998, 999), filter = TRUE){

  temp = t(df %>% summarise_all(funs(sum(. %in% value))))

  temp = data.frame(var = row.names(temp), n = temp) %>% arrange(desc(n))

  if(filter == TRUE) {
    temp %>% filter(n > 0)
  } else {
    temp
  }
}

#' @import dplyr
#' @export
is_binary = function(data) {
  sum(! na.exclude(unique(data)) %in% c(0,1)) == 0
}

#' @import dplyr
#' @export
id_type = function(data) {
  case_when(is_binary(data) ~ 'binary',
            is_numeric(data) ~ 'numeric',
            is.factor(data) ~ 'factor',
            is.character(data) ~ 'character',
            TRUE ~ 'unknown'
            )
}


#' @import dplyr
#' @export
# isolates continuous variables:
type_of = function(df) {

  temp = df %>% summarise_all(funs(id_type(.))) %>% t()

  data.frame(var = row.names(temp), type = temp) %>% arrange(type)
}

#' @import dplyr
#' @export
# calculates incidence of binary values
calc_pct = function(df) {

  temp = t(df %>% summarise_if(.predicate = is.numeric, .funs = funs(sum(., na.rm = TRUE)/n())))

  data.frame(var = row.names(temp), pct = temp) %>% arrange(desc(pct))
}
