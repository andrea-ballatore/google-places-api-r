R scripts for Google Places API 
========

### Abstract:

This R script takes geospatial geometries as input and retrieves all 
Google Places Points of Interest (POIs) in the area, 
generating as many as sub-queries as needed.

This script was developed for non-profit academic research in geographic data science.

Input needed: 

* A Google Maps API key or multiple keys in `data/input/google_api.txt`
* Spatial Polygon data frame with geometries in `spatial_polygon_data_frame.rds`
* List of Google Place types to retrieve in `input_place_types.csv`

Output:

* JSON results from the API
* GeoPackage with all retrieved POIs

### Publications:

N.A.

### Author:

[Andrea Ballatore](https://aballatore.space) © Birkbeck, University of London, 2020

### Last updated:

June 2020