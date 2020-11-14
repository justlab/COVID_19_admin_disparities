# Create an NYC-region water mask to use in maps that matches the high resolution of the MODZCTA shapes

# for ggplot axis limits
plot_bounds = as.list(st_bbox(MODZCTA_NYC_shp1) + (c(-0.05, -0.05, 0.05, 0.05))*0.5)
#      xmin      ymin      xmax      ymax 
# -74.28059  40.47112 -73.67501  40.94053 

# Download water boundaries in the map region from OpenStreetmap
osm_api = "http://overpass-api.de/api/interpreter?data="

# Lower Hudson River
pm(
get_lower_hudson <- function(){
  osm_query = "[out:json];relation(1845135);out geom;"
  qurl = paste0(osm_api, URLencode(osm_query, reserved = TRUE))
  res_hud = GET(url = qurl)
  hud_data = fromJSON(rawToChar(res_hud$content))
  hudlines <- lapply(hud_data$elements$members[[1]]$geometry, function(x){
    thisline <- st_geometry(st_linestring(as.matrix(x[, c("lon", "lat")])))
    st_crs(thisline) <- st_crs(4326)
    st_sf(thisline)
  })
  bind_rows(hudlines)
})
hudson = get_lower_hudson()

# Upper Hudson River
pm(
get_upper_hudson <- function(){
  osm_query = "[out:json];relation(2922180);out geom;"
  qurl = paste0(osm_api, URLencode(osm_query, reserved = TRUE))
  res_uphud = GET(url = qurl)
  uphud_data = fromJSON(rawToChar(res_uphud$content))
  uphudlines <- lapply(uphud_data$elements$members[[1]]$geometry, function(x){
    if(!is.null(x)){
      thisline <- st_geometry(st_linestring(as.matrix(x[, c("lon", "lat")])))
      st_crs(thisline) <- st_crs(4326)
      st_sf(thisline)
    }
  })
  bind_rows(uphudlines)
})
upper_hudson = get_upper_hudson()

# Coastlines
pm(
get_coastlines <- function(){
  osm_query = "[out:json];way[natural=coastline](40.38,-74.36,41.02,-73.59);out geom;"
  qurl = paste0(osm_api, URLencode(osm_query, reserved = TRUE))
  res_coast = GET(url = qurl)
  coast_data = fromJSON(rawToChar(res_coast$content))
  coastlines <- lapply(coast_data$elements$geometry, function(x){
    thisline <- st_geometry(st_linestring(as.matrix(x[, c("lon", "lat")])))
    st_crs(thisline) <- st_crs(4326)
    st_sf(thisline)
  })
  bind_rows(coastlines)
})
coast = get_coastlines()

# create a background polygon at least as large as the plot area
# the extent of this polygon was also used to query OSM coastlines 
background_poly = st_as_sfc(st_bbox(MODZCTA_NYC_shp1) + (c(-0.05, -0.05, 0.05, 0.05))*2.2)
background_poly = st_transform(background_poly, crs = 4326)

# all OSM coastlines and riverbank lines queried so far
osm_shore = bind_rows(coast, hudson, upper_hudson)
osm_blade = st_combine(osm_shore)
split_water = lwgeom::st_split(background_poly, osm_blade) %>% st_collection_extract("POLYGON")

# create points to select water polygons 
water_points = st_as_sf(data.frame(name = c("lower bay", "lower Hudson", "upper Hudson"),
                                   x = c(-74.026, -73.931, -73.895),
                                   y = c(40.564, 40.885, 40.995)),
                        coords = c("x", "y"), crs = 4326)
water_poly_ids = st_intersects(split_water, st_combine(water_points), sparse = FALSE)

basemap_water = split_water[water_poly_ids,] %>% st_union()

rm(water_points, water_poly_ids, split_water, osm_blade, osm_shore, background_poly, coast, upper_hudson, hudson)
