# CityPulse 360° — Urban Analytics and Visualization

## Project Overview

CityPulse 360° is an interactive urban analytics and data visualization project developed using R. It presents urban data through statistical charts, interactive dashboards, and geospatial visualization techniques to help users explore patterns, compare indicators, and understand relationships between different urban attributes.

## Objectives

- Analyze and explore urban datasets.
- Present complex information through clear and effective visualizations.
- Compare urban indicators and identify patterns and trends.
- Explore geographical distributions and spatial relationships.
- Provide an interactive dashboard for data exploration.

## Visualization Techniques

The project includes a range of statistical and geospatial visualization techniques, including:

- Bar charts
- Donut charts
- Time-series and trend charts
- Scatter plots
- Histograms
- Box plots
- Violin plots
- Area charts
- Heatmaps
- Interactive point maps
- Proportional symbol maps
- Cluster maps
- Choropleth and Voronoi-based visualizations
- Cartograms
- Hexagonal binning
- Spatial heatmaps
- Topographic contour visualizations
- Flow maps
- Spider maps
- Time-space trajectory visualizations
- Attribute proximity maps

*Note: The list describes the project's intended or documented visualization techniques. Refer to the uploaded R source code for the exact implementations included in the submitted version.*

## Technologies Used

- **R** — Data analysis and visualization
- **Shiny** — Interactive web dashboard
- **ggplot2** — Statistical graphics
- **Plotly** — Interactive charts
- **Leaflet** — Interactive geographical maps
- **dplyr** — Data manipulation
- **DT** — Interactive data tables

## Project Structure

```text
CityPulse-360/
├── README.md
├── app.R              # Main Shiny application (if named app.R)
└── supporting-files/   # Optional datasets and other resources
```

If the main source file is named `CityPulse.R`, update the filename in this structure accordingly.

## How to Run the Project

1. Install R and RStudio.
2. Install the required R packages:

```r
install.packages(c(
  "shiny",
  "ggplot2",
  "plotly",
  "dplyr",
  "leaflet",
  "DT"
))
```

3. Open the project source file in RStudio.
4. Run the application using:

```r
shiny::runApp()
```

Ensure that all additional packages and supporting files required by the complete application are available.

## Data and Limitations

If synthetic or demonstration data are used, the resulting visualizations should be interpreted as demonstrations rather than verified measurements of real urban conditions. The accuracy of the analysis depends on the quality, completeness, and source of the data provided.

## Academic Use

This project was developed for academic work related to advanced data visualization and urban analytics. It demonstrates the use of multiple visualization approaches to communicate data insights.

## Source Code

The complete R source code is available in this GitHub repository.

**Repository:** https://github.com/manthenasriadithya2024-design/CityPulse-360

