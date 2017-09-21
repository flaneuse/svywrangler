library(llamar)

calc_idx = function(df, var_name = 'wlth_idx', save_params = FALSE, save_all = FALSE) {
  pca = df %>% 
    prcomp()
  
  
  # calculate variance explained:
  var_expl = cumsum((pca$sdev)^2) / sum(pca$sdev^2)
  
  # plot the loadings
  loadings = data.frame(pca$rotation) %>% mutate(var = row.names(pca$rotation))
  
  fill_lim = max(max(loadings$PC1), abs(min(loadings$PC1)))
  
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
    return(list(data = df, loadings = loadings, variance = var_expl))
  }
} 
