#' Access Demographic & Health Surveys Data
#'
#' Accesses information from the Demographic and Health Surveys, via their API.
#'
#' Information is the same as from the DHS StatCompiler
#' Documentation code for DHS API: \link{http://api.dhsprogram.com/#/index.html}
#' Filter options: \link{http://api.dhsprogram.com/#/api-data.cfm}
#' Country codes: \link{http://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10136}
#' Indicator codes: \link{http://api.dhsprogram.com/rest/dhs/indicators?returnFields=IndicatorId,Label,Definition&f=html}
#' @param breakdown one of 'national', 'subnational', 'background', or 'all'
#' @param indicators: either a list of indicators to search for, or a string of DHS indicator codes separated by commas *NO SPACES between*, e.g. 'CN_NUTS_C_WH2,CN_NUTS_C_HA2,ED_EDUC_W_SEP'
#' @param countries: list of DHS country names as a string separated by commas with *NO SPACES between*, e.g. 'SN,SL,TG'
#' @param years: list of survey years to include as a string separated by commas, e.g. '2010,2012,2014'. By default, will return all available years.
#' @param apiKey: API Key to include, to be able to obtain 5000 records instead of 1000 at a time.
#' @param numResults: number of records to include
#'
#' @author Laura Hughes
#'
#' @import dplyr RJSONIO stringr
#' @export
#'
#' @examples
#' rw_stunting = loadDHS(breakdown = 'all', indicators = 'CN_NUTS_C_HA2,CN_NUTS_C_HA3', countries = 'RW', years = '2010,2015')
#' rw_stunting = loadDHS(breakdown = 'subnational', indicators = 'stunted', countries = 'Rwanda')
#' malnutrition = loadDHS(breakdown = 'national', indicators = c('stunted', 'wasted', 'underweight'), countries = c('Rwanda', 'Burundi', 'Niger'))


loadDHS = function(breakdown = "national",
                   indicators,
                   countries,
                   start_year = 1984,
                   end_year = as.numeric(format(Sys.Date(), "%Y")),
                   years = NA,
                   apiKey = NA, numResults = 1000,
                   return_params = FALSE) {
  if(length(countries) > 1) {
    countries = getDHScountry(countries)
  }


  if(length(indicators) > 1) {
    indicators = getDHSindicator(indicators)
  }

  # define the query
  query = paste0("http://api.dhsprogram.com/rest/dhs/data?breakdown=",
                 breakdown, "&indicatorIds=", paste(indicators, collapse = ','), "&countryIds=", paste(countries, collapse = ','), "&SurveyYear=",
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
    df = df %>% mutate_at(.funs = as.numeric, .vars = vars(Value, Precision, SurveyYear, IsTotal,
                                                           CILow, CIHigh, DenominatorUnweighted, DenominatorWeighted))
  }

  if(return_params == TRUE) {
    return(list(data = df, indicators = indicators, countries = countries, breakdown = breakdown))

  } else {
    return(df)
  }
}
