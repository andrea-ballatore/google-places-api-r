# Andrea Ballatore
# Scraper
# 2020

# Google API
# https://developers.google.com/places/web-service/intro

# Google places API

# To check Google credits: https://console.cloud.google.com/

# Load utils functions
source("code/setup.R")

API_KEYs = trimws(readLines("data/input/google_api.txt"))

MAX_RES_API = 60  # max results for a single query
cost_per_query_usd = 0.017
MAX_QUERIES = 10000  # VERY IMPORTANT TO AVOID GOOGLE BILLING
gquery_counter = 0


# https://developers.google.com/places/web-service/supported_types
# NOTE: "food" doesn't work
load_google_types = function(fn){
  cat_df = read_csv(fn)
  cats = trimws(subset(cat_df, cat_df$Inclusion == 'Y')$Category)
  cats = sort(unique(cats))
  print(length(cats))
  return(cats)
}

get_center_radius_covering_polygon = function(sdf){
  bg_sdf = spTransform(sdf, METRIC_PROJ)
  bb = bbox(sdf)
  #print(bb)
  #rect = "rectangle:south,west|north,east"
  stopifnot(bb[1] < bb[3])
  stopifnot(bb[2] < bb[4])
  bg_bb = bbox(bg_sdf)
  m = matrix(c(bg_bb[1],bg_bb[3],bg_bb[2],bg_bb[4]), nrow = 2)
  # radius is max distance between bbox vertices / 2
  rad_m = ceiling(stats::dist(m)[1] / 2)

  #print(paste(rad_m))
  stopifnot(rad_m > 0)
  #print(paste(bg_bb))

  #print(paste(bb[1],bb[2],bb[3],bb[4]))

  rect = paste0("rectangle:", bb[2], ",", bb[1], "|", bb[4], ",", bb[3])
  # get query centre (latlon)
  ct = gCentroid(sdf,byid=TRUE)

  res = c(ct@coords[2], ct@coords[1], rad_m)
  res = round(res, 5)  # 1m resolution
  print(paste("get_center_radius_covering_polygon:",paste(res,collapse = ' ')))
  return(res)
}

# "rectangle:south,west|north,east"
split_area_rects = function(sdf){
  stopifnot(nrow(sdf)==1)
  bb = bbox(sdf)
  minx = bb[1]
  miny = bb[2]
  maxx = bb[3]
  maxy = bb[4]
  stopifnot(minx < maxx)
  stopifnot(miny < maxy)
  midx = minx + ((maxx - minx)/2)
  midy = miny + ((maxy - miny)/2)

  # generate 4 rectangles
  #  rectangle:south,west,north,east (minx,miny,maxx,maxy)
  r1 = get_polygon_from_extent(minx,midx,miny,midy,proj4string(sdf),1)
  r2 = get_polygon_from_extent(midx,maxx,miny,midy,proj4string(sdf),2)
  r3 = get_polygon_from_extent(midx,maxx,midy,maxy,proj4string(sdf),3)
  r4 = get_polygon_from_extent(minx,midx,midy,maxy,proj4string(sdf),4)

  subrect_sdf = rbind(r1,r2,r3,r4)
  return(subrect_sdf)
}

get_polygon_from_extent = function(minx, maxx, miny, maxy, crs, id){
  stopifnot(minx < maxx)
  stopifnot(miny < maxy)
  e <- as(raster::extent(minx, maxx, miny, maxy), "SpatialPolygons")
  proj4string(e) <- ll_crs
  sdf = SpatialPolygonsDataFrame(e, data.frame(ID=id))
  return(sdf)
}

get_out_fn = function(gid, gtype, fn_prefix, i){
  fn = paste0("tmp/scraped_data/gplace_results-geom_",gid, "-", gtype, fn_prefix, "-",sprintf("%04d", i),".json")
  return(fn)
}


fix_json_issues = function(res, pcode){
  # fix bug with backslash in addresses. E.g. replace 12\14 with 12/14
  res = gsub('([0-9])\\\\([0-9])', '\\1/\\2', res)
  res = gsub('([A-Z])\\\\([A-Z])', '\\1/\\2', res)
  res = gsub('(\\s+)\\\\(\\s+)', '\\1/\\2', res)
  return(res)
  # bad fixes for JSON format
  # if (pcode=="DN14"){
  #   # bad fix for backslash
  #   res = gsub("4\\2", "4/2", res, fixed = T)
  # }
  # if (pcode=="SK6"){
  #   res = gsub("1\\2", "1/2", res, fixed = T)
  #
  # }
  # if (pcode=="NE8"){
  #   res = gsub("3\\7", "3/7", res, fixed = T)
  # }
  # return(res)
}

are_results_ok = function(j, url){
  if (j[["status"]]=="ZERO_RESULTS")
    return(T)
  if (j[["status"]] != "OK"){
    stop(paste("Google API error: ", j[["status"]], url))
  }
  return(T)
}

query_google_api = function(url){
  gquery_counter <<- gquery_counter + 1
  #return(F)  # DEBUG
  if (gquery_counter > MAX_QUERIES){
    print(paste("excuted",gquery_counter,"gqueries. Estimated cost $:",round(cost_per_query_usd*gquery_counter,2)))
    stop("Max queries MAX_QUERIES reached. Check Google API billing limits.")
  }
  print(paste("query_google_api gquery_counter =",gquery_counter))
  return(getURLContent(url))
}

query_gplaces_by_area = function(gid, gtype, input_area_sdf, fn_prefix=''){
  # start_i: used for recursive call
  print(paste("query_gplaces_by_area", gid, gtype, fn_prefix))
  #enctxt = URLencode(txt)

  api_key = sample(API_KEYs, 1)

  fields = "place_id,name,price_level,photos,formatted_address,opening_hours,rating,types,geometry,permanently_closed"
  # vicinity
  # location: locationbias=circle:2000@47.6918452,-122.2226413
  #   Circular: A string specifying radius in meters, plus lat/lng in decimal degrees.
  #     Use the following format: circle:radius@lat,lng.
  #   Rectangular: A string specifying two lat/lng pairs in decimal degrees, representing the south/west and north/east points of a rectangle. Use the following format:
  #     rectangle:south,west|north,east. Note that east/west values are wrapped to the range -180, 180, and north/south values are clamped to the range -90, 90.
  # fields=&locationbias=circle:2000@47.6918452,-122.2226413&
  sdf = input_area_sdf
  stopifnot(nrow(sdf) == 1)
  # project in metres to get radius
  ptrad = get_center_radius_covering_polygon(sdf)
  radius_m = round(ptrad[3],0)

  # DEBUG
  #ptrad[1]="54.07959"
  #ptrad[2]="-0.2107"
  #radius_m="1317"

  ## location=-33.8670522,151.1957362&radius=1500&
  location_pt_radius = paste0("location=", ptrad[1], ',', ptrad[2], "&radius=", radius_m)
  #print(ct)
  #print(rect)
  i = 1
  # https://developers.google.com/places/web-service/search
  api_url = paste0("https://maps.googleapis.com/maps/api/place/nearbysearch/json?",
             "fields=", fields, "&", location_pt_radius, "&type=", gtype, "&key=", api_key)
  #print(api_url)
  query_info = list("query_url" = api_url, "geom_i" = gid, "radius_m" = radius_m,
                    "location_lat"=ptrad[1], "location_lon"=ptrad[2],
                    "type"=gtype, "timestamp"=Sys.time())
  fn = get_out_fn(gid, gtype, fn_prefix, i)
  if (file.exists(fn)){
    # NOTE: avoid re-querying the same thing over and over because it's $$$
    print("already done, skipping.")
    return()
  }
  sleep(1)
  # query API
  res = query_google_api(api_url)
  res = fix_json_issues(res, pcode)

  #print(res)
  j = fromJSON(res, flatten = F)
  are_results_ok(j, api_url)
  j[["input"]] = query_info
  j[["result_page"]] = i
  n_results = nrow(j$results)
  
  if (is.null(n_results)){n_results = 0}
  j[["n_results"]] = n_results
  write(toJSON(j, auto_unbox=T, pretty = T), fn)

  # consume next pages if present
  while("next_page_token" %in% names(j)){
    # API: next_page_token contains a token that can be used to return up to 20 additional results.
    #    A next_page_token will not be returned if there are no additional results to display.
    #    The maximum number of results that can be returned is 60. There is a short delay
    #    between when a next_page_token is issued, and when it will become valid.
    i = i + 1
    print(paste("  get next result page - ",i))
    # get next page of results
    next_url = paste0("https://maps.googleapis.com/maps/api/place/nearbysearch/json?",
                       "pagetoken=", j[["next_page_token"]], "&key=", api_key)
    #print(next_url)
    sleep(4)  # VERY IMPORTANT DELAY for pagetoken.
    res = query_google_api(next_url)
    res = fix_json_issues(res, pcode)

    j = fromJSON(res, flatten = F)
    are_results_ok(j, next_url)
    query_info[['page_query_url']] = next_url
    j[["input"]] = query_info
    j[["result_page"]] = i
    # calculate n results
    n_subres = nrow(j$results)
    if (!is.null(n_subres)){
      n_results = n_results + n_subres
    } else {
      n_subres = 0
    }
    j[["n_results"]] = n_subres
    j[["tot_results"]] = n_results

    fn = get_out_fn(gid, gtype, fn_prefix, i)
    write(toJSON(j, auto_unbox=T, pretty = T), fn)
  }
  print(paste("   ",n_results,"results"))

  if (n_results == MAX_RES_API){
    print("max results hit, probably more POIs available. Generating 4 sub-queries.")
    sub_sdf = split_area_rects(sdf)
    stopifnot(nrow(sub_sdf)==4)
    for(pid in sub_sdf$ID){
      query_gplaces_by_area(gid, gtype, sub_sdf[pid,], paste0(fn_prefix,"-subquery", pid))
    }
  }
}


# Main ---------------------------------

# create outfolders
dir.create('tmp/scraped_data',showWarnings = F)

METRIC_PROJ = utm_crs

# load input data
input_sdf = load_input_areas()
#input_pcodes = c("YO8") #,"SK15")  # DEBUG
#input_pcodes

google_types = load_google_types("data/input/input_place_types.csv")

# query APIs
for (geom_i in seq(nrow(input_sdf))){
  ingeom = input_sdf[geom_i]
  print(paste(">> area geom", "--", geom_i, "of" , nrow(input_sdf)))
  for (gtype in google_types){
    query_gplaces_by_area(geom_i, gtype, ingeom)
  }
}

print(paste("excuted",gquery_counter,"gqueries. Estimated cost $:",round(cost_per_query_usd*gquery_counter,2)))
print("OK")
