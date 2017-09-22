#' Standardize inputs into a regression model
#'
#' @description Standardizes regression input columns to have comparable regression coefficients.
#' Rules are as follows:
#' * categoricals: ignore (leave as dummy vars with 0/1)
#' * binaries: ignore (leave as 0/1)
#' * continuous: divide by 2 std dev. if scale = F; or reset so sd = 1, mean = 0 if center = T, scale = T; divided by root mean squared if scale = T, center = F
#' Based off of Andrew Gelman's suggestions: http://andrewgelman.com/2009/07/11/when_to_standar/
#'
#' @param df data frame containing all the data
#' @param center binary if the continuous data should be centered (set so mean = 0)
#' @param scale binary if the data should be scaled so the new standard deviation is 1 (if centered), or the value divided by the RMS (if not centered). If false, the column will merely be divided by 2 std. dev.
#'
#' @export
#'
#' @examples
#' scaledcenter_mtcars = center_scale(mtcars)
#' scaled_mtcars = center_scale(mtcars, center = FALSE)
#' adj_mtcars = center_scale(mtcars, center = FALSE, scale = FALSE)
#'

center_scale = function(df,
                       center = TRUE,
                       scale = TRUE,
                       cols2ignore = NA){

  # For each column...
  std_col = function(df, col_num, center, scale, cols2ignore){

    col_name = names(df)[col_num]

    data_col = df[[col_name]]

    # -- if a column to ignore, don't do anything --
    if(col_name %in% cols2ignore){
      return(data_col)
    }


    # -- categorical --
    if (is.factor(data_col) | is.character(data_col)) {
      return(data_col)
    }


    # -- binary --
    # Note: technically, anything with a range of 1 would classify as being binary
    if (diff(range(data_col, na.rm = TRUE)) == 1) {
      return(data_col)
    }

    # -- continuous --
    if(scale == TRUE) {
      return(scale(data_col, center = center, scale = scale))
    } else {
      # Use Gelman's scaling by 2 std. deviations
      return(data_col/(2*sd(data_col)))
    }
  }

  scaled = data.frame(lapply(seq_along(df), function(x) std_col(df, x, center = center, scale = scale, cols2ignore = cols2ignore)))

  colnames(scaled) = colnames(df)

  return(scaled)
}
