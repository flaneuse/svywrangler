#' based on FAO http://www.fao.org/wairdocs/ilri/x5443e/x5443e04.htm
#' @import dplyr
#' @export

calc_tlu = function(df, cow_var = cows, camel_var = camels,
               sheep_var = sheep, goat_var = goats,
               equine_var = equines,
               horse_var = NA, mule_var = NA, ass_var = NA,
               pig_var = NA,
               chicken_var = chickens,
               duck_var = ducks) {
  camel_wt = 1
  cow_wt = 0.7
  sheep_wt = 0.1
  goat_wt = 0.1
  horse_wt = 0.8
  mule_wt = 0.7
  ass_wt = 0.5
  equine_wt = sum(ass_wt, mule_wt, horse_wt)/3
  pig_wt = 0.2
  poultry_wt = 0.01

  cow_var = enquo(cow_var)
  camel_var = enquo(camel_var)
  sheep_var = enquo(sheep_var)
  goat_var = enquo(goat_var)
  equine_var = enquo(equine_var)
  horse_var = enquo(horse_var)
  mule_var = enquo(mule_var)
  ass_var = enquo(ass_var)
  pig_var = enquo(pig_var)
  chicken_var = enquo(chicken_var)
  duck_var = enquo(duck_var)


  wt_animal = function(var, wt) {
    if(!is.na(var)){
      as.numeric(var) * wt
    } else {
      0
    }
  }

  df %>% mutate(TLU =
                  wt_animal(!!camel_var, camel_wt) +
                  wt_animal(!!cow_var, cow_wt) +
                  wt_animal(!!sheep_var, sheep_wt) +
                  wt_animal(!!goat_var, goat_wt) +
                  wt_animal(!!equine_var, equine_wt) +
                  wt_animal(!!horse_var, horse_wt) +
                  wt_animal(!!mule_var, mule_wt) +
                  wt_animal(!!ass_var, ass_wt) +
                  wt_animal(!!pig_var, pig_wt) +
                  wt_animal(!!chicken_var, poultry_wt) +
                  wt_animal(!!duck_var, poultry_wt)

  )
}
