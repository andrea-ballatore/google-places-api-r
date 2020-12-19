R scripts for Google Places API 
========

## Abstract

This R script takes geospatial geometries as input and retrieves all 
Google Places Points of Interest (POIs) in the area, 
generating as many as sub-queries as needed.

This script was developed for non-profit academic research in geographic data science.

## Instructions

1) Run `google_place_scraper.R` to collect data from the Google API. Input needed: 

* A Google Maps API key or multiple keys (one on each row) in `data/input/google_api.txt`
* List of Google Place types to retrieve in `input_place_types.csv`. Edit file to change selection.
* Spatial Polygon data frame with polygons from function `load_input_areas()` in `setup.R`. Modify function to load other geometries.

2) Run `google_place_extract.Rmd` to extract results into a GeoPackage. See markdown for details.

Output:

* JSON results from the API.
* GeoPackage with all retrieved POIs linked to specific areas from `load_input_areas()`.



## Publications

N.A.

## Author

[Andrea Ballatore](https://aballatore.space) © Birkbeck, University of London, 2020

### Last updated

Dec 2020
