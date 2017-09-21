#' Recoding and cleaning of survey data
#'
#' Converts values of a data frame to encoded values using a dictionary.
#'
#' @param df data frame to change
#' @param dict Assumes that the dictionary will be a 2-column data frame with key in first column and value in the second. Third column is an optional column explaining the relationship.  Should be some sort of logical operator, e.g. '==', '<', '<=', ...
#' @param oldVar name of the variable to change
#' @param newVar name of the new variable to generate
#' 
#' @examples 
#' df = data.frame(region_code = sample(6, 10, replace = TRUE))
#' dict = data.frame(region_code = 1:6, region_name = c('Africa', 'Asia', 'Australia', 'Europe', 'North America', 'South America'))
#' 
#' # straight dictionary
#' llamar::apply_code(df, dict, 'region_code', 'name')
#' 
#' # fancy math
#' dict2 = data.frame(region_code = 1:5, 
#' region_name = c('Africa', 'Asia', 'Australia', 'Europe', 'Americas'),
#' logic = c(rep('==', 4), '>=')) # If >= 5, assign to Americas.
#' llamar::apply_code(df, dict2, 'region_code', 'name')

apply_code = function(df, dict, oldVar, newVar) {
  
  if (ncol(dict) == 2) {
    # Assumes all relationships are equalities
    dict = dict %>% mutate(rel = "==")
  } else if (ncol(dict) != 3) {
    stop("Not enough columns in dictionary.  Dictionary should be 2 or 3 columns.")
  }
  
  nestedCondit = ""
  
  for (i in 1:nrow(dict)) {
    nestedCondit = paste0(nestedCondit, "ifelse(", oldVar, dict[i, 3], " \"", dict[i, 
                                                                                   1], "\" , \"", dict[i, 2], "\" , ")
  }
  
  closure = paste(rep(")", nrow(dict)), collapse = "")
  
  nestedCondit = paste0(nestedCondit, "NA", closure)
  
  newDF = mutate_(df, .dots = setNames(list(nestedCondit), newVar))
  
  return(newDF)
}

