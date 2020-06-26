#
# title: "Setup"
#

options("encoding" = "UTF-8")

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr,readr,readr,plyr,rgdal,dplyr,jsonlite,RCurl,sp,vegan,ineq,data.table, foreach, rvest, rgeos, uuid,
               gtools,digest,sf,R.utils,stringr,maptools)

#R.utils::gcDLLs()

# ------------------------------------------------------------------
# Common variables
# ------------------------------------------------------------------
british_grid_crs = CRS("+init=epsg:27700")
ll_crs = CRS("+init=epsg:4326")
utm_crs = CRS("+init=epsg:32631")


# ------------------------------------------------------------------
# Utilities
# ------------------------------------------------------------------

# This accepts any Spatial Polygon Data Frame
load_input_areas = function(){
  sdf = read_rds("../data/input/spatial_polygon_data_frame.rds")
  sdf = subset(sdf, sdf$GM_CODE == "GM0439")
  stopifnot(nrow(sdf)>0)
  sdf = spTransform(sdf, ll_crs)
  print(paste("load_input_areas", nrow(sdf)))
  return(sdf)
}

sort_df <- function( df, col, asc=T ){
  sdf = df[ with(df, order(df[,c(col)], decreasing = !asc)), ]
  return(sdf)
}

sleep = function(secs){
  print(paste("sleep",secs))
  Sys.sleep(secs)
}


diffSets <- function(a,b, label_a,label_b){
  d = setdiff(a,b)
  invd = setdiff(b,a)
  print(paste0("Found in '",label_a,"' and not in '",label_b,"' [",length(d),'/',length(a),"]: ", paste(sort(d),collapse = ' ')))
  print(paste0("Found in '",label_b,"' and not in '",label_a,"' [",length(invd),'/',length(b),"]: ", paste(sort(invd),collapse = ' ')))
  return(c(length(d),length(invd)))
}

write_geopackage <- function( sdf, fn, layer_name ){
  sf::st_write( st_as_sf(sdf), fn, layer_name, delete_dsn = TRUE)
  gzip(fn,overwrite=T)
  print(paste0('Geopackage written: ',fn,'.gz'))
}

# Simplify SpatialPolygonsDataFrame. It does not create slivers (based on rmapshaper).
#
# retain_tolerance: 1 --> no change
#                   .5 --> keep half points
#                   0 --> remove all points
#
simplify_poly <- function( sdf, retain_tolerance ){
  print( paste0("simplify with tolerance ", retain_tolerance  ))
  stopifnot(class(sdf)=="SpatialPolygonsDataFrame")
  stopifnot(retain_tolerance > 0 && retain_tolerance < 1)
  df = sdf@data
  sz = object.size(sdf)
  n = nrow(sdf)
  geom_simpl = rmapshaper::ms_simplify(sdf, keep = retain_tolerance) # does not create slivers
  #geom_simpl <- rgeos::gSimplify(sdf, tol = tolerance, topologyPreserve = T) # does create slivers
  sdf_simpl = SpatialPolygonsDataFrame( geom_simpl, df )
  sz2 = object.size(sdf_simpl)
  print(paste0(n," geometries: new size ",round(sz2/sz*100,1),"% of input"))
  return(sdf_simpl)
}


print_count_NAs <- function(x){
  n = sum( is.na(x), na.rm=FALSE)
  paste0('> NA/missing values: ',n,' (',
               round(n/length(x)*100,3),
               '%)')
}

p_summary_list_num <- function(x){
  p_summary_list(x)
  print("DISTRIBUTION")
  print(summary(x))
}

write_geojson <- function( sdf, fn, layer_name ){
  stopifnot(proj4string(sdf)==ll_crs@projargs)
  writeOGR(obj = sdf, dsn = fn, layer=layer_name,
           driver="GeoJSON", overwrite_layer=T, delete_dsn=T)
  gzip(fn,overwrite=T)
  print(paste0('GeoJson written: ',fn,'.gz'))
}

p_summary_list <- function( x, lab, sort_by_pc = T, top_n = NA ){
  #cat('\n')
  t = table(x, useNA = "always")
  d = data.frame( VAL = t )
  d$PC = round((d$VAL.Freq / sum(d$VAL.Freq)*100),2)
  if (sort_by_pc)
    d = sort_df(d,'PC',asc = F)
  d$VAR = lab
  stopifnot(nrow(d)>0)
  if (!is.na(top_n))
    return(head(d,top_n))

  return(d)
  #print_count_NAs(x)
  #return(t)
}

