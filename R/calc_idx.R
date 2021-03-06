#' @import llamar dplyr ggplot2
#' @export

  calc_idx = function(df, var_name = 'wlth_idx',
                      center = TRUE, scale = TRUE,
                      save_params = FALSE, save_all = FALSE) {

    # Check that everything has *some* variation

    std = df %>% summarise_all(funs(sd(.))) %>% t() %>% data.frame()
    colnames(std) = 'sd'

    no_var = std %>% mutate(var = row.names(std)) %>% filter(sd == 0)

    if (nrow(no_var) > 0) {
      warning(paste('Removing columns:', paste(no_var$var, collapse = ', '), 'due to no variation'))

      df = df %>% select(-one_of(no_var$var))
    }

    pca = df %>%
      prcomp(center = center, scale = scale)


    # calculate variance explained:
    var_expl = cumsum((pca$sdev)^2) / sum(pca$sdev^2)
    scree = data.frame(eigenvalues = pca$sdev^2)
    scree = scree %>% mutate(PC = 1:nrow(scree))

    # plot the loadings
    loadings = data.frame(pca$rotation) %>% mutate(var = row.names(pca$rotation))

    fill_lim = max(max(loadings$PC1), abs(min(loadings$PC1)))

    # Scree plot
    s = ggplot(scree, aes(x = PC, y = eigenvalues, group = 1)) +
      geom_path(size = 0.5, colour = grey50K) +
      geom_point(size = 5, shape = 21, fill = grey25K) +
      ggtitle('Scree plot') +
      theme_ygrid()

    print(s)

    # Loadings
    p = ggplot(loadings, aes(x = PC1, fill = PC1,
                             y = forcats::fct_reorder(var, PC1))) +
      geom_vline(xintercept = 0, colour = grey75K, size = 1.5) +

      geom_point(size = 5, shape = 22) +
      scale_fill_gradientn(colours = PiYG, limits = c(-fill_lim, fill_lim)) +
      ggtitle(paste('Variance explained: ', var_expl[1])) +
      theme_xgrid()

    print(p)




    # save index to df
    if(save_all == TRUE) {
      df = df %>% bind_cols(!!var_name := pca$x[,1])
    } else {
      df = data.frame(x = pca$x[,1]) %>% rename(!!var_name := x)

    }

    if(save_params == FALSE){
      return(df)
    } else{
      return(list(data = df, loadings = loadings, variance = var_expl, scree = scree, pca = pca))
    }
  }
