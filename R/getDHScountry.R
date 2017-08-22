#' Create a lookup table for DHS country and indicator codes, and find the country/indicator code based on the list
#'
#' @name getDHScodes
NULL

#' @describeIn  getDHScodes Function to pull the DHS codes for country names.
#' Can take either a single country or multiple ones; also can return entire table or just the country code
#' Matching is *exact* matching; input name must exactly match a country name within the DHS dataset.
#' @import dplyr rvest
#' @export
#'
#' @param country_names string or list of strings of country names
#' @param return_table whether to return the codes in a table containing their name and code, or just a collapsed list of the codes
#'
#' @examples
#' getDHScountry(c('Niger', 'Nigeria', 'Rwanda', 'Zambia'), return_table = FALSE)
#' getDHScountry(c('Niger', 'Nigeria', 'Rwanda', 'Zambia'), return_table = TRUE)
#' getDHScountry('Rwanda')
#' getDHScountry('Nige') # Returns an error.

getDHScountry = function(country_names, return_table = FALSE){

  # Get most up-to-date list of DHScountries
  DHSctry = importDHScountries()

  if(is.null(DHSctry)){
    warning('Could not connect to DHS database. Using list of country name/codes from 2017.')
    DHSctry = DHScountries
  }


  # Apply to the list
  filtered = DHSctry %>%
    filter(country %in% country_names)

  if(nrow(filtered) > 0){
    if(return_table == TRUE) {
      return(filtered)
    } else{
      # Collapse to a comma-separated list
      codes = filtered %>% pull(code)

      if(length(codes) != length(country_names)) {
        warning("Not all countries matched with database. Returning those that did.")
      }

      # Convert to a comma-separated list
      codes = paste0(codes, collapse = ',')

      return(codes)
    }
  } else{
    warning(paste("No countries found. Country names include:",
                  DHSctry %>% pull(country) %>% paste(., collapse = ', ')))
  }
}

#' @import dplyr rvest
importDHScountries = function(save_file = FALSE,
                              file_name = '~/GitHub/llamar/data/DHScountries.rda') {
  dhs_country = read_html('http://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10136')

  codes = dhs_country %>%
    html_node('#CS_Element_countrycodes .CS_Textblock_Text [summary="Table Summary"]')

  if(!is.na(codes)){
    codes = codes %>%
      html_table(header = TRUE)



    # clean up to remove the crap.
    colnames(codes) = c('code1', 'country1', 'code2', 'country2')

    # divide into 2 and bind
    left = codes %>%
      slice(-1) %>%
      select(code1, country1) %>%
      rename(code = code1,
             country = country1)

    right = codes %>%
      slice(-1) %>%
      select(code2, country2) %>%
      rename(code = code2,
             country = country2)

    DHScountries = bind_rows(left, right)

    if(save_file == TRUE){
      save(DHScountries, file = file_name)
    }
  } else {
    DHScountries = NULL
  }

  return(DHScountries)

}

#' @import dplyr RJSONIO
importDHSindicators = function(save_file = FALSE,
                               file_name = '~/GitHub/llamar/data/DHSindicators.rda'){

  indic = fromJSON('http://api.dhsprogram.com/rest/dhs/indicators')

  # unlist and convert to data frame
  indic = lapply(indic$Data, function(x) {
    unlist(x)
  })

  DHSind = as.data.frame(do.call("rbind", indic), stringsAsFactors = FALSE)


  if(save_file == TRUE){
    save(DHSind, file = file_name)
  }

  return(DHSind)
}

#' @describeIn  getDHScodes Function to lookup the DHS indicator codes. Partially matches to input indicator; user can select which indicator(s) they want
#'
#' @import dplyr data.table
#' @export
#'
#' @param indicators string or list of strings of indicator names (or parts of their names)
#' @param refresh_indicators T/F of whether to re-pull the indicator list from the DHS website. Takes a bit of time to execute
#'
#' @examples
#' stunted_indic = getDHSindicator('stunted', return_table = TRUE)
#' malnourished = getDHSindicator(c('stunted', 'wasted', 'underweight'))
#' getDHSindicator('studnted') # returns nothing

getDHSindicator = function(indicators,
                           refresh_indicators = FALSE,
                           return_table = FALSE){

  # Get DHS indicator lookup table
  if(refresh_indicators == TRUE){
    # Re-pull the DHS indicators
    DHSind = importDHSindicators()

    if(!exists('DHSind')){
      warning('Could not connect to DHS database. Using list of indicator name/codes from 2017.')
      DHSind = DHSindic
    }
  } else {
    DHSind = DHSindic
  }

  # Define function to grab the indicators
  pull_indic = function(sel_indicator) {
    filtered_indic = DHSind %>% filter(Label %like% sel_indicator)

    if(nrow(filtered_indic) > 1){

      indic_name = select.list(choices = c('all', as.character(filtered_indic$Label)),
                               multiple = TRUE,
                               title = 'Multiple indicators were found. Enter which one(s) you want.')

      if(indic_name != 'all') {
        filtered_indic = filtered_indic %>% filter(Label %in% indic_name)
      }
    }
    return(filtered_indic)
  }

  # loop over the indicators
  filtered_indic = lapply(indicators, pull_indic) %>%
    bind_rows()

  if(nrow(filtered_indic) > 0){
    if(return_table == TRUE) {
      return(filtered_indic)
    } else {
      return(as.character(paste(filtered_indic$IndicatorId, collapse=",")))
    }
  } else {
    warning("No indicators found.")
    return(NULL)
  }
}
