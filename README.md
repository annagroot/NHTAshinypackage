---
title: "NHTA Shiny package"
output: html_document
---

# Introduction to NHTAshinypackage

Welcome to `NHTAshinypackage`, an R package dedicated to the facilitation of NHTA Shiny apps in R.

## Features

-   **Data Access**: Instantly load cleaned and preprocessed datasets about penguin species, including measurements like flipper length, body mass, and bill dimensions, along with demographic information such as species, sex, and breeding pairs.

-   **Interactive Visualization**: Utilize a suite of plotting functions to create dynamic, interactive visualizations that help in conveying complex ecological data in a straightforward manner. Visualizations include scatter plots, histograms, and geographical mapping of penguin colonies.

-   **Analytical Tools**: Perform statistical analysis directly within the package to assess trends, correlations, and population dynamics. `PenguinDataR` integrates seamlessly with popular R packages for statistical testing and modeling.

-   **Educational Content**: Access to detailed documentation and vignettes that provide insights into penguin biology and the ecological importance of each dataset. These resources are designed to enhance learning and teaching experiences in ecological and zoological studies.

## Getting Started

To install `NHTAshinypackage`, you can use the following command in R:


``` 
remotes::install_github("annagroot/NHTAshinypackage")
```

## Setting the NHTA theme

```
theme_NHTA <- bs_theme(version = 5,
                      bg = "#f8f9fa", fg = "#000000",  # Ensure both background and foreground colors are set
                      primary = "#4C8187", secondary = "#214E70",
                      base_font = font_google("Questrial"), 
                      heading_font = font_google("Questrial"), 
)
```
