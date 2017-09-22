#' Plot coefficients of a model with errors
#'
#' @description Plots coefficients from a model, along with standard errors.  Coefficients that are statistically significant at agiven level are highlighted in darker text. Inspired by https://github.com/jaredlander/coefplot/blob/master/R/coefplot.r but works w/ ggplot2 version > 2.2
#'
#' @import ggplot2 broom dplyr forcats RColorBrewer multiwayvcov lmtest extrafont
#' @importFrom data.table %like%
#'
#' @export
#'
#' @param model Fitted model object from a lm or glm
#' @param negative_good Should negative coefficients be displayed in red (default) or blue (negative_good = TRUE)?
#' @param cluster_col Column in the *original* data frame used to build the model to be used to calculate clustered standards errors using the `multiwayvcov` package. By default, no clustering correction is used, and the standard erorrs are those calculated in the model.  If using clustered errors, should be specified as: 'orginal_data$clustering_column'
#' @param level confidence level for error bars / significance indicator
#' @param CI_factor Factor to adjust the standard errors by. By default, 1.96, assuming a normal distribution with sufficient sample size (95 percent of distribution lies within 1.96 standard deviations)
#' @param exclude_intercept whether to include the intercept from the model in the color-coding of coefficients.
#' @param exclude_terms (optional) string of terms to exclude
#' @param plot_left_labels include names of variables on the left side of the y-axis
#' @param plot_right_labels include names of variables on the right side of the y-axis
#' @param alpha_insignificant alpha (opacity) level for variables that are insignificant
#' @param size_point size to plot the coefficients, in mm
#' @param x_buffer percentage offset for the y-axis labels
#' @param label_margin offset for the y-axis labels
#' @param font_normal string containing the name of the font to use in darkest text
#' @param font_semi string containing the name of the font to use in medium dark text
#' @param font_light string containing the name of the font to use in lightest text
#'
#' @examples
#' data(diamonds, package = 'ggplot2')
#' model1 <- lm(price ~ carat + cut*color, data=diamonds)
#' plot_coef(model1)
#'
#' # Sort of a contrived example to show the effect of clustering standard errors.
#' model2 <- lm(price ~ carat + color, data=diamonds)
#' # No clustered errors
#' plot_coef(model2)
#' # Errors clustered by cut
#' plot_coef(model2, cluster_col = diamonds$cut)

plot_coef = function(model,
                     negative_ontop = TRUE,
                     negative_good = FALSE,
                     cluster_col = NA,
                     level = 0.95,
                     CI_factor = 1.96, # assuming normal distribution, 95% CI level
                     exclude_terms = NA,
                     exclude_intercept = TRUE,
                     plot_left_labels = TRUE,
                     plot_right_labels = FALSE,
                     alpha_insignificant = 0.3,
                     size_point = 3,
                     x_buffer = 0.1,
                     label_margin = 1,
                     font_normal = 'Lato',
                     font_semi = 'Lato Light',
                     font_light = 'Lato Light'){

  # check if the fonts are installed.  If not, default to 'sans'.
  font_normal = llamar::replace_font(font_normal)
  font_semi = llamar::replace_font(font_semi)
  font_light = llamar::replace_font(font_light)


  # pull out coefficients in a nice data frame


  # ditto for confidence intervals; assumes 95% level
  if(is.na(all(cluster_col))){
    coefs = broom::tidy(model)

    CIs = broom::confint_tidy(model, level)

    # Remove any NAs from CIs (factors which have been dropped from model)
    CIs = CIs %>% filter(!is.na(conf.low))

    df = dplyr::bind_cols(coefs, CIs)

  } else {
    # check that cluster_col has the right number of observations
    if(length(cluster_col) != (length(model$fitted.values) + length(model$na.action))) {
      stop('check cluster_col; incorrect number of observations inputted.')
    }


    # recalculate the CIs based on the clustering variable

    coefs = lmtest::coeftest(model, vcov = multiwayvcov::cluster.vcov(model, cluster_col))

    df = data.frame(term = row.names(coefs),
                    estimate = coefs[, 'Estimate'],
                    std.error = coefs[, 'Std. Error'],
                    statistic = coefs[, 't value'],
                    p.value = coefs[,'Pr(>|t|)'])

    df = df %>%
      mutate(conf.low = estimate - CI_factor * std.error,
             conf.high = estimate + CI_factor * std.error)
  }



  # determine if statistically significant difference
  df = df %>%
    mutate(stat_signif = ifelse(p.value < (1 - level),
                                1, alpha_insignificant))

  # Exclude terms, as needed
  if(!is.na(exclude_terms)) {
    df = df %>%
      filter(! term %like% exclude_terms)
  }

  # find limits for colors
  if(exclude_intercept == TRUE){
    max_estimate = df %>% filter(term != '(Intercept)') %>% summarise(min = min(estimate),
                                                                      max = max(estimate))

    max_estimate = max(max_estimate$max, abs(max_estimate$min))

    # find limits for labels
    xmin = min(df$conf.low)
    xmax = max(df$conf.high)

    x_breaks = pretty(c(xmin, xmax), n = 5)
    x_breaks = x_breaks[x_breaks < xmax & x_breaks > xmin] # truncate at edges

  } else {
    max_estimate = max(max(df$estimate), abs(min(df$estimate)))
  }

  if(negative_good == TRUE) {
    # flip the color scheme

    max_estimate = max_estimate * -1
  }

  # plot --------------------------------------------------------------------

  p = ggplot(df, aes(x = estimate, y = forcats::fct_reorder(term, estimate, .desc = negative_ontop),
                     fill = estimate, alpha = stat_signif)) +

    # -- zero point --
    geom_vline(xintercept = 0, colour = grey90K, size = 0.2) +


    # -- error bars --
    geom_segment(aes(x = conf.low, xend = conf.high,
                     yend = forcats::fct_reorder(term, estimate, .desc = negative_ontop)),
                 colour = grey15K, size = 1.5) +

    # -- estimate --
    geom_point(size = size_point,
               shape = 21,
               stroke = 0.2,
               colour = grey90K) +

    # -- scales --
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, 'RdBu'),
                         limits = c(-1 * max_estimate, max_estimate)) +
    scale_alpha_identity() +

    # # -- themes --
    theme_minimal() +
    # theme_xgrid(font_normal = font_normal, font_semi = font_semi, font_light = font_light) +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_blank())

  # -- model params --
  if(packageVersion('ggplot2') > '2.1.0') {
    p = p + ggtitle(paste0('adj. r^2: ', round(summary(model)$adj.r.squared, 3)),
                    subtitle = paste0(model$df.residual, ' observations; # NAs: ', length(model$na.action)))
  } else {
    p = p + ggtitle(paste0('adj. r^2: ', round(summary(model)$adj.r.squared, 3)))
  }

  # -- variable labels --
  if(plot_left_labels & plot_right_labels){
    p = p + geom_text(aes(x = xmin - abs(xmin) * x_buffer, y = forcats::fct_reorder(term, estimate, .desc = negative_ontop),
                          label = forcats::fct_reorder(term, estimate, .desc = negative_ontop)),
                      size = 3.5, hjust = 'outward',
                      colour = grey90K, family = font_light) +

      geom_text(aes(x = xmax + xmax * x_buffer, y = forcats::fct_reorder(term, estimate, .desc = negative_ontop),
                    label = forcats::fct_reorder(term, estimate, .desc = negative_ontop)),
                size = 3.5, hjust = 'outward',
                colour = grey90K, family = font_light) +

      scale_x_continuous(limits = c(xmin - abs(xmax - xmin) * label_margin, xmax + abs(xmax - xmin) * x_buffer * 3),
                         breaks = x_breaks)


  } else if(plot_left_labels) {
    p = p + geom_text(aes(x = xmin - abs(xmin) * x_buffer, y = forcats::fct_reorder(term, estimate, .desc = negative_ontop),
                          label = forcats::fct_reorder(term, estimate, .desc = negative_ontop)),
                      size = 3.5, hjust = 'outward',
                      colour = grey90K, family = font_light) +

      scale_x_continuous(limits = c(xmin - abs(xmax - xmin) * label_margin, xmax),
                         breaks = x_breaks)


  } else if(plot_right_labels) {
    p = p + geom_text(aes(x = xmax + xmax * x_buffer, y = forcats::fct_reorder(term, estimate, .desc = negative_ontop),
                          label = forcats::fct_reorder(term, estimate, .desc = negative_ontop)),
                      size = 3.5, hjust = 'outward',
                      colour = grey90K, family = font_light) +

      scale_x_continuous(limits = c(xmin, xmax + abs(xmax - xmin) * label_margin),
                         breaks = x_breaks)
  } else {
    p
  }

  return(p)
}
