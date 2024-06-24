#' Create a Group Countries Map
#'
#' This function creates a Leaflet map with polygons for each country, allowing a selected country to be highlighted with a different color.
#'
#' @param country_maps A named list of country polygons. Each element should be a spatial object that represents a country's shape.
#' @param selected_country A data frame or list containing the selected country. It should have a column or element named `country` with the country name.
#' @param selected_color A string representing the HEX code for the color of the selected country. Default is `"#4C8187"`.
#' @param unselected_color A string representing the HEX code for the color of the unselected countries. Default is `"#E2DFDA"`.
#' @param border_color A string representing the HEX code for the border color of the countries. Default is `"#214E70"`.
#' @return A Leaflet map object with the countries' polygons added, highlighting the selected country.
#' @details This function adds polygons for each country to a Leaflet map. The selected country is highlighted with a different fill color, while other countries have a default fill color. The map's zoom, dragging, and scroll wheel zoom are disabled, and it is set to a fixed zoom level.
#' @examples
#' \dontrun{
#' # Example usage:
#' country_maps <- list(USA = usa_polygon, Canada = canada_polygon, Mexico = mexico_polygon)
#' selected_country <- data.frame(country = "Canada")
#' map <- create_group_countries_map(country_maps, selected_country)
#' print(map)
#' }
#' @export
create_group_countries_map <- function(country_maps,
                                       selected_country,
                                       selected_color = "#4C8187",
                                       unselected_color = "#E2DFDA",
                                       border_color = "#214E70"){

  map <- leaflet(options = leafletOptions(attributionControl = FALSE, zoomControl = FALSE, dragging = FALSE, scrollWheelZoom = FALSE, zoomSnap = 0.1, zoomDelta = 0.1, minZoom = 3.8, maxZoom = 3.8))

  # Add each country's polygon with the dynamic fill color
  for (name in names(country_maps)) {
    map <- addPolygons(map, data = country_maps[[name]], layerId = ~COUNTRY,
                       fillColor = ~ifelse(COUNTRY == selected_country$country, selected_color, unselected_color), #set fillColor based on selected region
                       opacity = 0.8, #Opacity of border
                       weight = 1, #Weight/size of border
                       color = border_color, #color of border
                       fillOpacity = 1, #Opacity of fill
                       label = ~as.character(COUNTRY), #Use COUNTRY (name of country) as label
                       labelOptions = labelOptions( #This code determines how the label displayed on mouse hover looks; could be improved
                         direction = "auto",
                         noHide = F,
                         textOnly = TRUE,
                         style = list('background' = 'rgba(255,255,255,0.8)',
                                      'border' = '2px solid black',
                                      'padding' = '12px')))
  }

  map
}


