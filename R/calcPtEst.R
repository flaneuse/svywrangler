#' Calculate point estimates and errors
#'
#' @description Calculates point estimates and errors using the package `survey` based on sampling weights from a survey design
#'
#' @import dplyr survey
#' @export
#'
#' @param df main data frame containing the raw data
#' @param var string containing the variable name you want to average
#' @param use_weights TRUE/FALSE for whether to apply a sampling frame or calculate a simple average.
#' @param by_var (optional) string containing the variable name over which you want to calculate the estimates
#' @param design (optional) svydesign object containing the sample frame
#' @param psu_var (optional) if design isn't specified, string containing the primary sampling unit variable from the survey design (argument \code{id} in library('survey'))
#' @param strata_var (optional) if design isn't specified, string containing the strata variable from the survey design (argument \code{strata} in library('survey')
#' @param weight_var (optional)if design isn't specified, string containing the weights variable from the survey design (argument \code{weights} in library('survey')
#' @param na.rm remove NAs from the mean or not
#' @param ci_factor value to calculate confidence interval; in standard deviations. 1.96 standard deviations --> ~ 95 percent of the area under a normal gaussian distribution
#'
#' @examples
#' # Generate simple random data to average
#' df = data.frame(region = rep(letters, 100), value = sample(100, 2600, replace = TRUE))
#'
#' calcPtEst(df, 'value', by_var = 'region', use_weights = FALSE)
#'
#' # generate data from a survey
#' data(api, package = 'survey')
#' dclus1 <- survey::svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
#' # no weights
#' calcPtEst(apiclus1, var = 'api99', by_var = 'stype', use_weights = FALSE)
#' # specifying the sampling frame as a design object
#' calcPtEst(apiclus1, var = 'api99', by_var = 'stype', design = dclus1, use_weights = TRUE)
#'


# -- Wrapper to survey::svyby --
# Calculates point estimates with standard errors, weighted by sample weights
calcPtEst = function(df, # main data frame containing raw data,
                     var, # What you want to average
                     use_weights = FALSE,
                     by_var = NA, # variable
                     design = NA, # survey design object (containing sampling procedure)
                     psu_var = NA,
                     strata_var = NA,
                     weight_var = NA,
                     na.rm = TRUE,
                     ci_factor = 2) {


  # Calculate sample size and unweighted avg.
  if(na.rm == TRUE & is.na(by_var)) {
    # Exclude missing values
    n = df %>%
      filter_(paste0('!is.na(', var,')')) %>%
      summarise_(.dots = list(N = 'n()',
                              unweighted_avg = paste0('mean(', var, ')'),
                              unweighted_sd = paste0('sd(', var, ')')))
  } else if(na.rm == TRUE) {
    # Exclude missing values
    n = df %>%
      filter_(paste0('!is.na(', var,')')) %>%
      group_by_(by_var) %>%
      summarise_(.dots = list(N = 'n()',
                              unweighted_avg = paste0('mean(', var, ')'),
                              unweighted_sd = paste0('sd(', var, ')')))
  } else if(is.na(by_var)){
    n = df %>%
      summarise_(.dots = list(N = 'n()',
                              unweighted_avg = paste0('mean(', var, ')'),
                              unweighted_sd = paste0('sd(', var, ')')))
  } else{
    n = df %>%
      group_by_(by_var) %>%
      summarise_(.dots = list(N = 'n()',
                              unweighted_avg = paste0('mean(', var, ')'),
                              unweighted_sd = paste0('sd(', var, ')')))
  }


  # -- define survey design --
  if(use_weights == TRUE) {
    # create the survey design object (containing sampling procedure) if not specified
    if (is.na(design)){
      # Check all the correct variables are inputted
      if(is.na(psu_var) | is.na(strata_var) | is.na(weight_var)) {
        stop('Survey design not specified')
      }

      # Check if there are any missing values in the psu, strata, or weight variables.
      if(any(is.na(df[[psu_var]]) | any(is.na(df[[strata_var]])) | any(is.na(df[[weight_var]])))) {
        warning("removing NAs in the sample design variables psu_var, strata_var, or weight_var")
        df = df %>%
          filter_(paste0('!is.na(', psu_var, ')')) %>%
          filter_(paste0('!is.na(', strata_var, ')')) %>%
          filter_(paste0('!is.na(', weight_var, ')'))
      }

      design = svydesign(id = as.formula(paste0('~', psu_var)),
                                 strata = as.formula(paste0('~', strata_var)),
                                 weights = as.formula(paste0('~', weight_var)),
                                 data = df)
    }

    # -- Calculate weighted averages --
    if(is.na(by_var)) {
      # -- just calculate the survey mean --
      pt_est = svymean(as.formula(paste0('~', var)),
                               by = as.formula(paste0('~', by_var)),
                               design = design,
                               na.rm = na.rm)

      # Convert to data frame, if not already
      # rename columns
      pt_est = as.data.frame(pt_est) %>%
        rename_('se' = var) %>%
        mutate(avg = mean) %>%
        rename_(.dots = setNames('mean', var)) %>%
        mutate(N = nrow(df %>%  filter_(paste0('!is.na(', var,')'))))


    } else {
      # Calculate point estimate and standard error.
      pt_est = svyby(as.formula(paste0('~', var)),
                             by = as.formula(paste0('~', by_var)),
                             design = design,
                             svymean,
                             na.rm = na.rm)
      # Convert to data frame, if not already
      pt_est = as.data.frame(pt_est)


      # Calculate CI and upper and lower bounds.
      # Default is to use 95% CI (1.96)
      pt_est = pt_est %>%
        mutate_(.dots = setNames(var, 'avg')) %>% # Create a copy of the average value named 'avg'
        mutate(ci = se * ci_factor,
               ub = avg + ci,
               lb = avg - ci) %>%
        arrange(desc(avg))

      # Merge the two together
      pt_est = full_join(pt_est, n)
    }

  } else{
    # Don't apply sampling frame.  Just calculate average and point interval.
    pt_est = n %>%
      mutate(ci = ci_factor * (unweighted_sd /sqrt(N)),
             ub = unweighted_avg + ci,
             lb = unweighted_avg - ci) %>%
      arrange(desc(unweighted_avg))
  }

  return(pt_est)
}
