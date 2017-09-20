x = kids %>% select(hh_num, stunting, kid_hemoglobin) %>% sample_frac(0.1)


replace_missing = function(df, missing_codes,  ...) {
  
  # more generic version of Hadley's na_if which can take in list of codes rather than single one.
  na.if = function(df, missing_codes) {
    df[df %in% missing_codes] = NA
    
    return(df)
  }
  
    var = quos(...)
                       
    df %>% mutate_at(funs(na.if(., missing_codes)), .vars = var)
  # }
}



replace_missing(x, c(9996, 9997, 9998, 9999), kid_hemoglobin)
replace_missing(x, c(9996, 9997, 9998, 9999), stunting, kid_hemoglobin)
