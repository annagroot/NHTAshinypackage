#' Get live currencies
#'
#' Daily update with API from to EUR, from other currencies
#' @param api_key The temperature in degrees Fahrenheit
#' @param base_currency the currency it converts to
#' @param convert_to_api_currencies the other currencies it converts from
#' @return The rates to calculate from currencies to the base currency
#' @export


get_live_currency <- function(api_key, base_currency, convert_to_api_currencies){

  base_url <- "https://v6.exchangerate-api.com/v6"
  endpoint <- paste0(base_url, "/", api_key, "/latest/", base_currency)

  response <- GET(endpoint)

  if (http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  parsed <- fromJSON(content(response, "text"), simplifyVector = FALSE)

  if (status_code(response) != 200) {
    stop(
      paste("API request failed with status code", status_code(response)),
      call. = FALSE
    )
  }

  rates <- parsed$conversion_rates[convert_to_api_currencies]
  inverted_rates <- 1 / unlist(rates)
  rates <- data.frame(Currency = names(inverted_rates),
                      Rate = inverted_rates)
  return(rates)
}
