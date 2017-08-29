#' Access Demographic & Health Surveys Data
#'
#' Accesses information from the Demographic and Health Surveys, via their API.
#'
#' Information is the same as from the DHS StatCompiler
#' Documentation code for DHS API: \url{http://api.dhsprogram.com/#/index.html}
#'
#' Filter options: \url{http://api.dhsprogram.com/#/api-data.cfm}
#'
#' Country codes: \url{http://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10136}
#'
#' Indicator codes: \url{http://api.dhsprogram.com/rest/dhs/indicators?returnFields=IndicatorId,Label,Definition&f=html}
#'
#' Return value definitions: \url{http://api.dhsprogram.com/rest/dhs/data/fields}
#'
#' @param breakdown one of 'national', 'subnational', 'background', or 'all'. See \url{http://api.dhsprogram.com/#/api-data.cfm} for description
#' @param indicators: either a list of indicator names/codes to search for, or a string of DHS indicator names/codes separated by commas, e.g. c('wasted', 'stunted', 'underweight') or CN_NUTS_C_WH2,CN_NUTS_C_HA2,CN_NUTS_C_WA2'
#' @param countries: either a list of DHS country names or codes, or a string of country names/codes separated by commas, e.g. 'SN,SL,TG'
#' @param start_year: first survey year to include. By default, will return everything from first year (1984) of survey to \code{end_year}.
#' @param end_year: last survey year to include. By default, will return everything from \code{start_year} to present.
#' @param years: if specified, list of survey years to include as a string separated by commas, e.g. '2010,2012,2014'. Overrides \code{start_year} and \code{end_year}; allows you to exclude years from analysis
#' @param apiKey: API Key to include, to be able to obtain 5000 records instead of 1000 at a time.
#' @param numResults: number of records to include
#' @param return_gunk: TRUE/FALSE whether to return everything, or just the useful fields
#' @param return_params: TRUE/FALSE whether to return the search parameters with the data
#'
#' @return See \url{http://api.dhsprogram.com/rest/dhs/data/fields} for descriptions of the outputs. The default option (\code{return_gunk = FALSE}) returns only CountryName, DHS_CountryCode, SurveyYear, SurveyYearLabel, IndicatorId, Indicator, Definition, MeasurementType, Denominator, DenominatorWeighted, DenominatorUnweighted, Value, Percent, CILow, CIHigh, CharacteristicCategory, CharacteristicLabel, IsTotal
#'
#' @author Laura Hughes
#'
#' @import dplyr RJSONIO stringr
#' @export
#'
#' @examples
#' rw_stunting = loadDHS(breakdown = 'all', indicators = 'CN_NUTS_C_HA2,CN_NUTS_C_HA3', countries = 'RW', years = '2000,2015')
#' glimpse(rw_stunting)
#' rw_stunting = loadDHS(breakdown = 'subnational', indicators = 'stunted', countries = 'Rwanda', start_year = 2005, end_year = 2015)
#' malnutrition = loadDHS(breakdown = 'national', indicators = c('stunted', 'wasted', 'underweight'), countries = c('Rwanda', 'Burundi', 'Niger'), return_params = TRUE)


loadDHS = function(breakdown = "national",
                   indicators,
                   countries,
                   start_year = 1984,
                   end_year = as.numeric(format(Sys.Date(), "%Y")),
                   years = NA,
                   apiKey = NA, numResults = 1000,
                   return_gunk = FALSE,
                   return_params = FALSE) {


  # helper function to define countries and indicators
  define_codes = function(input, lookup_table, code_var, input_type = c('countries', 'indicators')) {

    input_string = str_replace_all(paste(input, collapse = ','), ", ", ",")
    input_list = lapply(str_split(input_string, pattern = ','), str_trim)

    # check if codes match
    code_match = input_list[[1]] %in% lookup_table[[code_var]]

    if(any(code_match)) {
      # codes inputted
      output = input_string

      # check if all codes match
      if(!all(code_match)) {
        warning(paste(input_type, input_list[[1]][!code_match],
                      "were not found in DHS. Check the codes you inputted, or try matching by name."))
      }
    } else{
      if(input_type == 'countries'){
        output = getDHScountry(input_list[[1]])
      } else {
        output = getDHSindicator(input_list[[1]])
      }
    }

    return(output)
  }

  # -- define countries --
  countries = define_codes(countries, DHScountries, 'code', 'countries')


  if(is.null(countries)) {
    warning("Exiting; no countries found")
    return(NULL)
  }

  # -- define indicators --
  indicators = define_codes(indicators, DHSindic, 'IndicatorId', 'indicators')

  if(is.null(indicators)) {
    warning("Exiting; no indicators found")
    return(NULL)
  }

  # -- define years --
  if(is.na(years)) {
    years = paste(seq(start_year, end_year), collapse = ",")
  } else {
    years = str_replace_all(years, ", ", ",")
  }

  # define the query
  query = paste0("http://api.dhsprogram.com/rest/dhs/data?breakdown=",
                 breakdown, "&indicatorIds=", indicators, "&countryIds=", countries, "&SurveyYear=",
                 years, "&apiKey=", apiKey, "&perpage=", numResults)

  json_file = fromJSON(query)

  # Unlist the JSON file entries
  json_data = lapply(json_file$Data, function(x) {
    unlist(x)
  })

  # Convert JSON input to a data frame
  df = as.data.frame(do.call("rbind", json_data), stringsAsFactors = FALSE)


  # Throw a warning if number of returned results > numResults
  if (json_file$RecordCount > numResults) {
    warning(paste0("query results in ", json_file$RecordCount, " hits; first ", numResults,
                   " returned"))
  }

  # Check that everything are numbers.  grepl('^[[:digit:]]',y$Indicator)

  # Check that it returns a value.
  if (length(df) > 0) {
    # Convert values to numbers.
    df = df %>%
      mutate_at(.funs = as.numeric, .vars = vars(Value, Precision, SurveyYear, IsTotal,
                                                 CILow, CIHigh, DenominatorUnweighted, DenominatorWeighted))

    # Convert percents to percents
    df = df %>%
      left_join(DHSindic %>% select(IndicatorId, MeasurementType, Definition, Denominator), by = 'IndicatorId') %>%
      mutate(Percent = ifelse(MeasurementType == "Percent", Value/100, NA))
  }

  # Reorder into a more logical order
  if(return_gunk == FALSE){
    df = df %>%
      select(CountryName, DHS_CountryCode, SurveyYear, SurveyYearLabel,
             IndicatorId, Indicator, Definition, MeasurementType,
             Denominator, DenominatorWeighted, DenominatorUnweighted,
             Value, Percent, CILow, CIHigh,
             CharacteristicCategory, CharacteristicLabel, IsTotal
             )
  }

  if(return_params == TRUE) {
    return(list(data = df, indicators = indicators, countries = countries, breakdown = breakdown))

  } else {
    return(df)
  }
}
