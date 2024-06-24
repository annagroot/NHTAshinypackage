#' Write or Append Custom styles.css to /www Directory
#'
#' This function writes or appends to a `styles.css` file in the `/www` directory of the user's R project,
#' applying the user-defined primary and secondary colors for the headers.
#'
#' @param primary A string representing the HEX code for the primary color (e.g., "#FC1921").
#' @param secondary A string representing the HEX code for the secondary color (e.g., "#808284").
#' @return A message indicating that the file has been written or appended to in the `/www` directory.
#' @details This function checks if the `/www` directory exists in the current working directory.
#' If it does not exist, the function creates it and then writes the `styles.css` file with the specified colors.
#' If the `styles.css` file already exists, the function appends the new styles to it.
#' @examples
#' write_styles_css(primary = "#FC1921", secondary = "#808284")
#' @export

write_styles_css <- function(primary, secondary) {
  # Validate HEX codes (basic validation)
  if (!grepl("^#[0-9A-Fa-f]{6}$", primary)) {
    stop("Invalid HEX code for primary color.")
  }
  if (!grepl("^#[0-9A-Fa-f]{6}$", secondary)) {
    stop("Invalid HEX code for secondary color.")
  }

  # Define the CSS content with custom primary and secondary colors
  css_content <- sprintf("
  /* Custom CSS Styles */
  .primary-header {
    background-color: %s; /* Primary background */
    color: white; /* White text */
  }

  .secondary-header {
    background-color: %s; /* Secondary background */
    color: white; /* White text */
  }
  ", primary, secondary)

  # Get the current working directory
  project_dir <- getwd()

  # Define the /www directory path
  www_dir <- file.path(project_dir, "www")

  # Check if /www directory exists, if not create it
  if (!dir.exists(www_dir)) {
    dir.create(www_dir)
  }

  # Define the path to the styles.css file
  css_file_path <- file.path(www_dir, "styles.css")

  # Check if the styles.css file exists and write or append the CSS content
  if (file.exists(css_file_path)) {
    writeLines(css_content, css_file_path, append = TRUE)
    message("New styles have been appended to the existing styles.css in the /www directory.")
  } else {
    writeLines(css_content, css_file_path)
    message("styles.css has been written to the /www directory with primary and secondary colors.")
  }
}
