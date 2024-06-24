#' Get live currencies
#'
#' Daily update with API from to EUR, from other currencies
#' @param name name of cost variable
#' @param currency the three-letter code of the currency
#' @return Returns the relevant cost label
#' @export


create_cost_label <- function(name, currency){
  #Arguments:
  #name = name of costing unit (character)
  #currency: active curreny (character)

  if(!is.character(name) | is.null(currency) | !is.character(currency)){
    return("Label name or currency are not correctly formatted")
  }

  if (currency == "EUR") {
    paste(name, " (€)",  sep = "")
  } else {
    paste(name," (", currency, ")", sep = "")
  }


}
