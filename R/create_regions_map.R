#' Create a Regions Map
#'
#' This function creates a Leaflet map with polygons for each region within a selected country,
#' allowing a selected region to be highlighted with a different color.
#'
#' @param selected_country A string representing the name of the selected country.
#' @param region_maps A named list of region polygons for each country. Each element should be a spatial object that represents a country's regions.
#' @param selected_color A string representing the HEX code for the color of the selected region. Default is `"#4C8187"`.
#' @param unselected_color A string representing the HEX code for the color of the unselected regions. Default is `"#E2DFDA"`.
#' @param border_color A string representing the HEX code for the border color of the regions. Default is `"#214E70"`.
#' @return A Leaflet map object with the regions' polygons added, highlighting the selected region.
#' @details This function adds polygons for each region within the selected country to a Leaflet map. The selected region is highlighted with a different fill color, while other regions have a default fill color. The map's zoom, dragging, and scroll wheel zoom are disabled, and it is set to a fixed zoom level.
#' @examples
#' \dontrun{
#' # Example usage:
#' region_maps <- list(USA = usa_regions, Canada = canada_regions)
#' selected_country <- "Canada"
#' selected_region <- "Ontario"
#' map <- create_regions_map(selected_country, region_maps)
#' print(map)
#' }
#' @export
#'
create_regions_map <- function(selected_country,
                               region_maps,
                               selected_color = "#4C8187",
                               unselected_color = "#E2DFDA",
                               border_color = "#214E70"){

  country <- selected_country
  region <- selected_region
  region_data <- region_maps[[country]]

  # Use the reactive selected_region directly in fillColor
  map <- leaflet(options = leafletOptions(
    attributionControl = FALSE,
    zoomControl        = FALSE,
    dragging           = FALSE,
    scrollWheelZoom    = FALSE,
    zoomSnap  = 0.1,
    zoomDelta = 0.1
  )) %>%
    addPolygons(
      data      = region_data,
      layerId   = ~NAME_1,
      fillColor = ~ifelse(NAME_1 == region, selected_color, unselected_color), #set fillColor based on selected region
      opacity   = 0.8,
      weight    = 1,
      color     = border_color,
      fillOpacity = 1,
      label     = ~as.character(NAME_1),
      labelOptions = labelOptions(
        direction  = "auto",
        noHide     = F,
        textOnly   = TRUE,
        style      = list(
          'background' = 'rgba(255,255,255,0.8)', #This code determines how the label displayed on mouse hover looks; could be improved
          'border'     = '2px solid black',
          'padding'    = '12px'
        )
      )
    )
  map

}
