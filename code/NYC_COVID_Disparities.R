# CRAN packages:
library(tidyverse)
library(sf)
library(lubridate)
library(tidycensus)
library(ggExtra)
library(ggridges)
library(ggsn)
library(ragg) 
library(rstan)
library(drc)
library(spdep)
library(broom)
library(MASS)
library(spatialreg)
library(here)
library(pdftools)
library(matrixStats)
library(egg)
library(ggpubr)
library(scales)
library(qs)
library(corrplot)
library(readxl)
library(splines)
library(magic)
library(httr)
library(jsonlite)
library(DHARMa)
library(kableExtra)
library(lwgeom)
# Github packages available via: 
#   remotes::install_github("justlab/Just_universal")  
#   remotes::install_github("justlab/MTA_turnstile")
library(Just.universal) 

#### SESSION CONFIGURATION ####
options(dplyr.summarise.inform=FALSE)

# data will default to a subfolder "data/" within working directory
# unless 1. set by an environment variable:
data.root = Sys.getenv("COVID_DATA")
# or 2. set with an alternative path here:
if (data.root == "") data.root = here("data")
if (!dir.exists(data.root)) dir.create(data.root)
print(paste("data being downloaded into directory", dQuote(data.root)))

# Get some data from the git repository rather than downloading from original
#   source, to avoid changes in model results due to updated data
use_repo_data = TRUE

if(Sys.getenv("MTA_TURNSTILE_DATA_DIR") == ""){ 
  # set up default download location for MTA turnstile data
  mta_dir = file.path(data.root, "mta_turnstile")
  if(!dir.exists(mta_dir)) dir.create(mta_dir, recursive = TRUE)
  Sys.setenv(MTA_TURNSTILE_DATA_DIR = mta_dir)
}
library(MTA.turnstile)

# output path for figures
fig.path = here("figures")
if(!dir.exists(fig.path)) dir.create(fig.path)

export.figs = TRUE 
if(export.figs) message("Saving figures to:\n ", fig.path) else message("Not saving figures")

# pairmemo function cache
pairmemo.dir = file.path(data.root, "pairmemo")
dir.create(pairmemo.dir, showWarnings = F)
pm = function(...) pairmemo(
    directory = pairmemo.dir,
    n.frame = 2,
    ...)

# To generate census data, you need an API key, which you can request here: https://api.census.gov/data/key_signup.html
#census_api_key("INSERT YOUR CENSUS API KEY HERE", install = TRUE) 
if(Sys.getenv("CENSUS_API_KEY")=="") warning("Census API Key Missing")

#### Functions ####

read_w_filenames <- function(flnm) {
  read_csv(flnm) %>%
    mutate(filename = flnm)
}

extract_waic <- function (stanfit){
  log_lik <- rstan::extract(stanfit, "log_lik")$log_lik
  dim(log_lik) <- if (length(dim(log_lik)) == 1) 
    c(length(log_lik), 1)
  else c(dim(log_lik)[1], prod(dim(log_lik)[2:length(dim(log_lik))]))
  S <- nrow(log_lik)
  n <- ncol(log_lik)
  lpd <- log(colMeans(exp(log_lik)))
  p_waic <- colVars(log_lik)
  elpd_waic <- lpd - p_waic
  waic <- -2 * elpd_waic
  loo_weights_raw <- 1/exp(log_lik - max(log_lik))
  loo_weights_normalized <- loo_weights_raw/matrix(colMeans(loo_weights_raw), 
                                                   nrow = S, ncol = n, byrow = TRUE)
  loo_weights_regularized <- pmin(loo_weights_normalized, sqrt(S))
  elpd_loo <- log(colMeans(exp(log_lik) * loo_weights_regularized)/colMeans(loo_weights_regularized))
  p_loo <- lpd - elpd_loo
  pointwise <- cbind(waic, lpd, p_waic, elpd_waic, p_loo, elpd_loo)
  total <- colSums(pointwise)
  se <- sqrt(n * colVars(pointwise))
  return(list(waic = total["waic"], elpd_waic = total["elpd_waic"], 
              p_waic = total["p_waic"], elpd_loo = total["elpd_loo"], 
              p_loo = total["p_loo"]))
}

# Download a file, update metadata records, and load it with function `f`
# File metadata is stored in a sqlite file, by default in data/downloads/meta.sqlite
download = function(url, to, f, ...){
    f(download.update.meta(url, file.path(data.root, "downloads"), to),
        ...)
}

#### Load Data ####

# get the Pluto dataset from #https://www1.nyc.gov/site/planning/data-maps/open-data/dwn-pluto-mappluto.page 
pm(fst = T,
get.Pluto <- function() download(
    "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_20v3_csv.zip",
    "pluto.zip",
    function(p)
        read_csv(unz(p, "pluto_20v3.csv"), col_types = cols(spdist2 = col_character(), 
                                                overlay2 = col_character(),
                                                zonedist4 = col_character()))[,
            c("landuse", "bbl", "numfloors", "unitstotal", "unitsres",
                "zipcode")]))
Pluto <- as.data.frame(get.Pluto())


if (file.exists(file.path(data.root, "Bldg_Footprints.qs"))) {
    Bldg_Footprints <- qread(file.path(data.root, "Bldg_Footprints.qs"))
} else {
  Bldg_Footprints <- download(
    # https://data.cityofnewyork.us/Housing-Development/Building-Footprints/nqwf-w8eh
    "https://data.cityofnewyork.us/api/geospatial/nqwf-w8eh?method=export&format=Shapefile",
    "building_footprints.zip",
    function(p)
      st_read(paste0("/vsizip/", p)))
  qsave(Bldg_Footprints, file.path(data.root, "Bldg_Footprints.qs"))
}

ZCTA_by_boro <- download(
    "https://www.health.ny.gov/statistics/cancer/registry/appendix/neighborhoods.htm",
    "uhf_neighborhoods.html",
    function(p)
       {# XML::readHTMLTable doesn't identify the columns correctly.
        x = str_match_all(read_file(p), regex(dotall = T, paste0(
            '<td headers="header1"[^>]+>\\s*(.+?)</td>',
            '(.+?)',
            '(?=<td headers="header1"|</table>)')))[[1]]
        do.call(rbind, lapply(1 : nrow(x), function(i)
            data.frame(boro = x[i, 2], zip = as.integer(
                str_extract_all(x[i, 3], "\\b\\d{5}\\b")[[1]]))))})

# Download the specific day of test results by ZCTA being used
ZCTA_test_download <- download(
  "https://raw.githubusercontent.com/nychealth/coronavirus-data/6d7c4a94d6472a9ffc061166d099a4e5d89cd3e3/tests-by-zcta.csv",
  "2020-05-07_tests-by-zcta.csv",
  identity
)

# Download COVID-19 testing data 
ZCTA_test_series <- ZCTA_test_download %>% 
  map_df(~read_w_filenames(.)) %>%
  mutate(date = as.Date(str_extract(filename, "[:digit:]{4}-[:digit:]{2}-[:digit:]{2}"))) %>%
  dplyr::select(-filename)

# Subway ridership data
Subway_ridership_by_UHF <- relative.subway.usage(2020L, "nhood")

# UHF definitions by zip codes
UHF_ZipCodes <- UHF_ZipCodes <- download(
    "http://www.infoshare.org/misc/UHF.pdf",
    "uhf_zips.pdf",
    function(p)
       {x = str_match_all(pdf_text(p)[2],
            "(\\d+)\\s+(\\S.+?\\S)\\s*([0-9,]+)")[[1]]
        do.call(rbind, lapply(1 : nrow(x), function(i)
            data.frame(code = x[i, 2], name = x[i, 3], zip = as.integer(
                str_extract_all(x[i, 4], "\\b\\d{5}\\b")[[1]]))))})

# UHF shapefile 
UHF_shp <- download(
    "https://www1.nyc.gov/assets/doh/downloads/zip/uhf42_dohmh_2009.zip",
    "nyc_uhf_nhoods_shapefile.zip",
    function(p) read_sf(paste0("/vsizip/", p, "/UHF_42_DOHMH_2009")))

# NYC boroughs from NYC Open Data
NYC_basemap_shp <- download(
  "https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=Shapefile",
  "Borough_Boundaries.zip",
  function(p){
    unzip(p, exdir = file.path(data.root, "downloads"))
    # open data platform generates a random UUID for every download
    ufile = list.files(file.path(data.root, "downloads"), pattern = "geo_export_.*\\.shp", full.names = TRUE)
    st_read(ufile, stringsAsFactors = FALSE, quiet = TRUE) %>% st_transform(., crs = 2263)
  } 
)

# DOHMH MODZCTA Shapefile
MODZCTA_NYC_shp <- download(
  "https://data.cityofnewyork.us/api/geospatial/pri4-ifjk?method=export&format=Shapefile",
  "Modified Zip Code Tabulation Areas (MODZCTA).zip", 
  function(p) read_sf(paste0("/vsizip/", p))
)

# Food outlets 
if(use_repo_data){
  food_retail <- read_csv(file.path(data.root, "retail_food_stores_2019-06-13.csv"))
} else {
  food_retail <- download(
      "https://data.ny.gov/api/views/9a8c-vfzj/rows.csv",
      "retail_food_stores.csv",
      read_csv)
}

# Download deaths by ZCTA as of May 23rd
deaths_by23May2020_by_zcta <- download(
  "https://raw.githubusercontent.com/nychealth/coronavirus-data/8d88b2c06cf6b65676d58b28979731faa10c193c/data-by-modzcta.csv",
  "2020-05-23_data-by-modzcta.csv",
  read_csv
)

# download MODZCTA to ZCTA crosswalk, current version from repo
modzcta_to_zcta <- download(
  "https://raw.githubusercontent.com/nychealth/coronavirus-data/master/Geography-resources/ZCTA-to-MODZCTA.csv",
  "ZCTA-to-MODZCTA.csv",
  read_csv
)
modzcta_to_zcta1 <- modzcta_to_zcta %>% mutate(ZCTA = as.character(ZCTA))
modzcta_to_zcta2 <- modzcta_to_zcta1 %>% mutate(MODZCTA = as.character(MODZCTA))
MODZCTAs_in_NYC <- as.character(unique(ZCTA_test_series$MODZCTA))
ZCTAs_in_NYC <- as.character(unique(modzcta_to_zcta$ZCTA))

# download ZIP to Tract crosswalk from HUD
zip_to_tract <- download( 
  "https://www.huduser.gov/portal/datasets/usps/ZIP_TRACT_062020.xlsx",
  "ZIP_TRACT_062020.xlsx",
  function(p) suppressWarnings(read_excel(path = p, col_types = c("text", "text", "numeric", "skip", "skip", "skip")))
) 

# Block to ZCTA and County crosswalk for NY
ny_xwalk <- download("https://lehd.ces.census.gov/data/lodes/LODES7/ny/ny_xwalk.csv.gz",
                     "ny_xwalk.csv.gz",
                     function(p) {
                       zzf = gzfile(p)
                       read.csv(zzf) %>% 
                         dplyr::select(cty, tabblk2010, zcta, blklondd, blklatdd) %>% 
                         mutate(tabblk2010 = as.character(tabblk2010), 
                                zcta = as.character(zcta),
                                cntyfips = as.character(cty)) %>% 
                         dplyr::select(-cty)
                     }
)

# We have many sources of data, so these just help to combine the various data types
NYC_counties1 <- c("Bronx","Kings","Queens","New York","Richmond")
NYC_counties1_full <- c("Bronx County","Kings County","Queens County","New York County","Richmond County")
NYC_boro_county_match <- tibble(County = c("Bronx","Kings","Queens","New York","Richmond"), 
                                boro = c("Bronx","Brooklyn","Queens","Manhattan","Staten Island"), 
                                full_county = c("Bronx County","Kings County","Queens County","New York County","Richmond County"),
                                fips = c("36005", "36047", "36081", "36061", "36085"))

# stan model script
BWQS_stan_model <- here("code", "nb_bwqs_cov.stan") 


#### Census Data ####

# function to pull 2010 block population for Queens & Nassau counties
pm(get.qn.blocks <- function(){
  nassau_blk_pop <- get_decennial(geography = "block", variables = "P001001", 
                                  state = "NY", county = "Nassau", geometry = FALSE)
  queens_blk_pop <- get_decennial(geography = "block", variables = "P001001", 
                                  state = "NY", county = "Queens", geometry = FALSE)
  bind_rows(nassau_blk_pop, queens_blk_pop) %>% 
    dplyr::select(GEOID, value) %>% rename("pop2010" = "value")
})

# function to pull 2018 ACS data 
pm(acs.main <- function(admin_unit = c("zcta", "tract"), state_unit = c(NULL, "NY"), sf_shapes = c(TRUE, FALSE)) {
     ACS_Data <- get_acs(geography = admin_unit,
                         state = state_unit,
                         geometry = sf_shapes,
                         variables = c(medincome = "B19013_001",
                                       total_pop1 = "B01003_001",
                                       fpl_100 = "B06012_002", 
                                       fpl_100to150 = "B06012_003",
                                       median_rent = "B25031_001",
                                       total_hholds1 = "B22003_001",
                                       hholds_snap = "B22003_002",
                                       over16total_industry1 = "C24050_001",
                                       ag_industry = "C24050_002",
                                       construct_industry = "C24050_003",
                                       manufact_industry = "C24050_004",
                                       wholesaletrade_industry = "C24050_005",
                                       retail_industry = "C24050_006",
                                       transpo_and_utilities_industry = "C24050_007",
                                       information_industry = "C24050_008",
                                       finance_and_realestate_industry = "C24050_009",
                                       science_mngmt_admin_industry = "C24050_010",
                                       edu_health_socasst_industry = "C24050_011",
                                       arts_entertain_rec_accomodate_industry = "C24050_012",
                                       othsvcs_industry = "C24050_013",
                                       publicadmin_industry = "C24050_014",
                                       total_commute1 = "B08301_001",
                                       drove_commute = "B08301_002",
                                       pubtrans_bus_commute = "B08301_011",
                                       pubtrans_subway_commute = "B08301_013",
                                       pubtrans_railroad_commute = "B08301_013",
                                       pubtrans_ferry_commute = "B08301_015",
                                       taxi_commute = "B08301_016",
                                       bicycle_commute = "B08301_018",
                                       walked_commute = "B08301_019",
                                       workhome_commute = "B08301_021",
                                       unemployed = "B23025_005",
                                       under19_noinsurance = "B27010_017",
                                       age19_34_noinsurance = "B27010_033",
                                       age35_64_noinsurance = "B27010_050",
                                       age65plus_noinsurance = "B27010_066",
                                       hisplat_raceethnic = "B03002_012",
                                       nonhispLat_white_raceethnic = "B03002_003",
                                       nonhispLat_black_raceethnic = "B03002_004",
                                       nonhispLat_amerindian_raceethnic = "B03002_005",
                                       nonhispLat_asian_raceethnic = "B03002_006",
                                       age65_plus  = "B08101_008"),
                         year = 2018,
                         output = "wide",
                         survey = "acs5")
     
     if(admin_unit=="zcta"){
       ACS_Data <- ACS_Data %>% #only pull out the estimates and cleaning variable names
         filter(GEOID %in% ZCTAs_in_NYC) %>%
         dplyr::select(-NAME)  %>%
         dplyr::select(GEOID, !ends_with("M")) %>%
         rename_at(vars(ends_with("E")), .funs = list(~str_sub(., end = -2)))
     }
     
     if(admin_unit=="tract"){
       ACS_Data <- ACS_Data %>% #only pull out the estimates and cleaning variable names
         filter(substr(GEOID,1,5) %in% NYC_boro_county_match$fips) %>% # Tracts in NYC counties
         dplyr::select(-NAME)  %>%
         dplyr::select(GEOID, !ends_with("M")) %>%
         rename_at(vars(ends_with("E")), .funs = list(~str_sub(., end = -2)))
     }
     
     return(ACS_Data)
   })

# Use 2010 block population to scale Nassau County ZCTA contribution to NYC MODZCTAs
# This is the method used according to data dictionary at NYC Open Data:
#   https://data.cityofnewyork.us/Health/Modified-Zip-Code-Tabulation-Areas-MODZCTA-/pri4-ifjk
nassau_zcta_weights <- function(zcta_acs, mz_to_z, blk_to_z){
  blk_pop = get.qn.blocks()
  
  modzcta_span = c("11429", "11411", "11004") # MODZCTA with ZCTAs from both Queens & Nassau
  border_zcta <- mz_to_z %>% filter(MODZCTA %in% modzcta_span) %>% pull(ZCTA)
  # Filter block-zcta crosswalk table to Queens-Nassau ZCTA of interest
  blk_to_z <- blk_to_z %>% filter(zcta %in% border_zcta)
  
  # Join population to block-zcta crosswalk
  xwalk_pop <- blk_to_z %>% left_join(blk_pop, by = c("tabblk2010" = "GEOID")) 
  
  # Summarise 2010 population by ZCTA to calculate proportions inside NYC
  zcta_pop_2010 <- xwalk_pop %>%
    group_by(zcta) %>%
    summarise(z_pop_2010 = sum(pop2010), .groups = "drop_last")
  queens_zcta_pop_2010 <- xwalk_pop %>%
    filter(cntyfips == "36081") %>%
    group_by(zcta) %>%
    summarise(queens_z_pop_2010 = sum(pop2010), .groups = "drop_last")
  zcta_pop_props <- queens_zcta_pop_2010 %>% 
    left_join(zcta_pop_2010, by = "zcta") %>% 
    mutate(in_NYC_prop = queens_z_pop_2010/z_pop_2010)
  
  zcta_weights <- zcta_acs %>% dplyr::select(GEOID) %>% 
    left_join(zcta_pop_props, by = c("GEOID" = "zcta")) %>%
    dplyr::select(GEOID, in_NYC_prop) %>% 
    mutate(in_NYC_prop = 
             case_when(is.na(in_NYC_prop) ~ 1,
                       TRUE               ~ in_NYC_prop))
  
  # Apply weights for all Census variables except median vars
  zcta_acs <- zcta_acs %>% left_join(zcta_weights, by = "GEOID") 
  varvec <- 1:ncol(zcta_acs)
  varvec <- varvec[-grep("GEOID|in_NYC_prop|medincome|median_rent", names(zcta_acs))]
  zcta_acs <- zcta_acs %>% mutate_at(vars(all_of(varvec)), ~ . * in_NYC_prop)
}

# function to clean ACS data
clean_acs_data_and_derive_vars <- function(df, admin_unit = c("zcta", "tract")){
  if(admin_unit=="zcta"){
    ACS_Data1a <- df %>%
      left_join(., modzcta_to_zcta1, by = c("GEOID" = "ZCTA")) %>%
      group_by(MODZCTA) %>%
      summarise_at(vars(medincome, median_rent), ~weighted.mean(., total_pop1, na.rm = T)) %>% 
      rename(zcta = "MODZCTA")
    
    ACS_Data1b <- df %>%
      left_join(., modzcta_to_zcta1, by = c("GEOID" = "ZCTA")) %>%
      group_by(MODZCTA) %>%
      summarise_at(vars(total_pop1:fpl_100to150, total_hholds1:age65_plus), ~sum(.)) %>%
      mutate_at(vars(ends_with("_commute")), ~round((./total_commute1)*100, 2)) %>% #proportion of people relying on a given mode of transit
      mutate_at(vars(ends_with("_raceethnic")), ~round((./total_pop1)*100, 2)) %>% #proportion of ppl reporting a given race/ethncity 
      mutate(not_insured = round(((under19_noinsurance + age19_34_noinsurance + age35_64_noinsurance + age65plus_noinsurance) / total_pop1)*100, 2), #proportion uninsured
             #snap_hholds = round((hholds_snap/total_hholds1)*100, 2), #proportion relying on SNAP
             #fpl_150 = round(((fpl_100+fpl_100to150)/total_pop1)*100, 2), #proportion 150% or less of FPL
             unemployed = round((unemployed/over16total_industry1)*100, 2), #proportion unemployed
             not_quarantined_jobs = round(((ag_industry+(construct_industry*.25)+wholesaletrade_industry+ #an estimate of who is still leaving the house for work based on industry
                                              (edu_health_socasst_industry*.5)+transpo_and_utilities_industry)/over16total_industry1)*100, 2)) %>%
      dplyr::select(-ends_with("_noinsurance"), -fpl_100, -fpl_100to150, -ends_with("_industry"), -hholds_snap) %>%
      rename(zcta = "MODZCTA") 
    
    ACS_Data2 <- left_join(ACS_Data1a, ACS_Data1b, by = "zcta") %>%
      mutate(zcta = as.character(zcta))
    
  } else{
    
    ACS_Data2 <- df %>%
      mutate_at(vars(ends_with("_commute")), ~round((./total_commute1)*100, 2)) %>% #proportion of people relying on a givenmode of transit
      mutate_at(vars(ends_with("_raceethnic")), ~round((./total_pop1)*100, 2)) %>% #proportion of ppl reporting a given race/ethncity 
      mutate(not_insured = round(((under19_noinsurance + age19_34_noinsurance + age35_64_noinsurance + age65plus_noinsurance) / total_pop1)*100, 2), #proportion uninsured
             unemployed = round((unemployed/over16total_industry1)*100, 2), #proportion unemployed
             not_quarantined_jobs = round(((ag_industry+(construct_industry*.25)+wholesaletrade_industry+ #an estimate of who is still leaving the house for work based on industry
                                              (edu_health_socasst_industry*.5)+transpo_and_utilities_industry)/over16total_industry1)*100, 2)) %>%
      dplyr::select(-ends_with("_noinsurance"), -ends_with("_industry"),-fpl_100, -fpl_100to150,-hholds_snap) 
  }
  
  return(ACS_Data2)
}

# Functions to pull mode of transportation for our approximate of essential workers
pm(fst = T, 
   get_essential_acs <- function(admin_unit, state_unit) {
     get_acs(geography = admin_unit, #pull down the relevant categories 
             state = state_unit,
             variables = c(ag_car1_commute = "B08126_017",
                           ag_pubtrans_commute = "B08126_047",
                           construct_car1_commute ="B08126_018",
                           construct_pubtrans_commute = "B08126_048",
                           wholesale_car1_commute = "B08126_020",
                           wholesale_pubtrans_commute = "B08126_050",
                           transpo_car1_commute = "B08126_022",
                           transpo_pubtrans_commute = "B08126_052",
                           ed_hlthcare_car1_commute = "B08126_026",
                           ed_hlthcare_pubtrans_commute = "B08126_056"),
             year = 2018, 
             output = "wide",
             survey = "acs5")
})

acs.essential <- function(admin_unit, zcta_pop = NULL, state_unit = NULL) {
  if(!admin_unit %in% c("zcta", "tract")) stop("admin_unit must be either 'zcta' or 'tract'")
  if(admin_unit == "tract" & is.null(state_unit)) stop("state_unit must be set to download tracts")
  if(!is.null(state_unit)) if(state_unit != "NY") stop("state_unit must be either NULL or 'NY'")
  
  ACS_EssentialWrkr_Commute <- get_essential_acs(admin_unit = admin_unit, state_unit = state_unit)
     
   if(admin_unit == "zcta"){
     if(is.null(zcta_pop)) stop("zcta_pop must be set for scaling MODZCTA on Queens/Nassau boundary")
     
     ACS_Essential_worker_estimates <- ACS_EssentialWrkr_Commute %>% #clean data and aggregate 
       dplyr::select(-ends_with("M"), -NAME) %>%
       filter(GEOID %in% ZCTAs_in_NYC) %>%
       # scale ZCTAs by proportion of population in NYC
       right_join(zcta_pop %>% dplyr::select("GEOID", "in_NYC_prop"), by = "GEOID") %>% 
       mutate_at(vars(2:11), ~ . * in_NYC_prop) %>% 
       # summarize ZCTA to MODZCTA
       left_join(., modzcta_to_zcta1, by = c("GEOID" = "ZCTA")) %>%
       group_by(MODZCTA) %>%
       summarise_at(vars(2:11),
                    ~ sum(., na.rm = T)) %>% 
       rename(zcta = "MODZCTA") %>%
       mutate_at(vars(starts_with("ed_hlthcare")), ~ round(. / 2), 0) %>% #maintain same proportions as estimated nonquarintined jobs
       mutate_at(vars(starts_with("construct")), ~ round(. / 4), 0) %>%
       mutate(
         essentialworker_drove = rowSums(dplyr::select(., contains("car1_commute"))),
         essentialworker_pubtrans = rowSums(dplyr::select(., contains("pubtrans")))) %>%
       dplyr::select(zcta, essentialworker_drove, essentialworker_pubtrans) %>%
       mutate(zcta = as.character(zcta))
   } 
   else { # tracts
       
     ACS_Essential_worker_estimates <- ACS_EssentialWrkr_Commute %>% #clean data and aggregate 
         dplyr::select(-ends_with("M"), -NAME) %>%
         filter(substr(GEOID,1,5) %in% NYC_boro_county_match$fips) %>% # Tracts in NYC counties
         mutate_at(vars(starts_with("ed_hlthcare")), ~round(./2), 0) %>% #maintain same proportions as estimated nonquarintined jobs
         mutate_at(vars(starts_with("construct")), ~round(./4), 0) %>%
         mutate(essentialworker_drove = rowSums(dplyr::select(., contains("car1_commute"))), 
                essentialworker_pubtrans = rowSums(dplyr::select(., contains("pubtrans")))) %>%
         dplyr::select(GEOID, essentialworker_drove, essentialworker_pubtrans)   
   }
     
   return(ACS_Essential_worker_estimates)
}


# ZCTA CENSUS DATA
options(tigris_use_cache = TRUE)
ACS_Data1 <- as.data.frame(acs.main("zcta", NULL, FALSE)) #download the zcta data
ACS_Data_scaled <- nassau_zcta_weights(ACS_Data1, modzcta_to_zcta2, ny_xwalk)
ACS_Data2 <- clean_acs_data_and_derive_vars(ACS_Data_scaled, "zcta")
ACS_EssentialWrkr_Commute1 = as.data.frame(acs.essential("zcta", zcta_pop = ACS_Data_scaled, state_unit = NULL))

print(paste("The 2018 5-year ACS population range in NYC MODZCTAs is:", paste(range(ACS_Data2$total_pop1), collapse = "-")))

# TRACT CENSUS DATA  
acs_tracts <- acs.main("tract", "NY", TRUE)
acs_tracts2 <- clean_acs_data_and_derive_vars(acs_tracts, "tract")
acs_tracts_commute1 = as.data.frame(acs.essential("tract", state_unit = "NY"))


#### Grocery stores per area ####

non_supermarket_strings <- c("DELI|TOBACCO|GAS|CANDY|7 ELEVEN|7-ELEVEN|LIQUOR|ALCOHOL|BAKERY|CHOCOLATE|DUANE READE|WALGREENS|CVS|RITE AID|RAVIOLI|WINERY|WINE|BEER|CAFE|COFFEE")

food_retail_filtered <- food_retail %>% 
  filter(County %in% NYC_boro_county_match$County) %>% 
  filter(str_detect(`Establishment Type`, "J") & str_detect(`Establishment Type`, "A") & str_detect(`Establishment Type`, "C") &
           !str_detect(`Establishment Type`, "H")) %>%
  filter(!str_detect(`Entity Name`, non_supermarket_strings) & !str_detect(`DBA Name`, non_supermarket_strings)) %>%
  filter(`Square Footage`>=4500) %>%
  mutate(zcta = as.character(str_extract(Location, "[:digit:]{5}"))) %>% 
  mutate(Address = case_when(
    # some locations geocode better when address includes city name
    `License Number` %in% c("638599", "712410", "706967", "710078") ~ 
      paste(paste(`Street Number`, `Street Name`), City, State, zcta, sep = ","),
    # but most geocode better without it, see limitation #4 at: http://gis.ny.gov/gisdata/inventories/details.cfm?DSID=1278
    TRUE ~ 
      paste(paste(`Street Number`, `Street Name`), State, zcta, sep = ",") )
  )

# Geocode grocers, using a cached version if available to make analysis reproducible
# The geocoding service may be updated in the future and give different results
cached_grocers = file.path(data.root, "grocers_geocode_2020-11-09.csv")
if(file.exists(cached_grocers) & use_repo_data){
  gctable <- read.csv(cached_grocers)
  failed = which(gctable$score == 0)
  message("Loaded cached geocoded grocers: ", nrow(gctable)-length(failed), "/", nrow(gctable), " have coordinates.") # 997/1037
  if(nrow(gctable) != nrow(food_retail_filtered)) warning("Cached geocoded table has different row count than non-geocoded table")
} else {
  # locations are returned in crs=26918, UTM 18N NAD83
  api = "https://gisservices.its.ny.gov/arcgis/rest/services/Locators/Street_and_Address_Composite/GeocodeServer/findAddressCandidates?f=json&maxLocations=1&SingleLine="
  message("Geocoding ", nrow(food_retail_filtered), " grocers with NY State ITS geocoder...")
  t1 = Sys.time()
  res = lapply(food_retail_filtered$Address, function(addr) {
    GET(url = paste0(api, URLencode(addr))) })
  tdiff = Sys.time() - t1
  # extract results
  geocodes = lapply(res, function(page) fromJSON(rawToChar(page$content), flatten = TRUE)$candidates)
  failed = which(sapply(geocodes,class) != "data.frame")
  geocodes[failed] <- lapply(1:length(failed), function(void){
    data.frame(address = NA_character_, score = 0, location.x = NA_real_, location.y = NA_real_)})
  gctable = bind_rows(geocodes)
  message("Geocoded ", nrow(food_retail_filtered)-length(failed), "/", nrow(food_retail_filtered), 
          " grocers in ", round(as.numeric(tdiff),1), " ", attributes(tdiff)$units)
  write.csv(gctable, paste0(data.root, "/grocers_geocode_", Sys.Date(), ".csv"))
}

# Count grocers by tract
gctable = filter(gctable, score > 0)
grocerSF = st_as_sf(gctable, coords = c("location.x", "location.y"), crs = 26918) %>% st_transform(crs = 2263)
tractSF = acs_tracts2[, "GEOID"] %>% st_transform(., crs = 2263)
tract_grocers = suppressWarnings(st_intersection(tractSF, grocerSF)) %>%
  st_set_geometry(., NULL) %>%
  group_by(GEOID) %>%
  summarise(grocers = n_distinct(`address`))
#nrow(tract_grocers) # 749
#sum(tract_grocers$grocers) # 993

# Count grocers by ZCTA 
zctaSF <- MODZCTA_NYC_shp %>% dplyr::select(modzcta, geometry) %>% st_transform(crs = 2263) 
zcta_grocers <- suppressWarnings(st_intersection(zctaSF, grocerSF)) %>%
  st_set_geometry(., NULL) %>%
  group_by(modzcta) %>%
  summarise(grocers = n_distinct(`address`))
# sum(zcta_grocers$grocers) # 993
# nrow(zcta_grocers) # 172
# range(zcta_grocers$grocers) # 1, 21

#### Subway station locations ####

SubwayStation_shp <- as_tibble(turnstile()$stations) %>%
  st_as_sf(., coords = c("lon", "lat"), crs = 4269) %>%
  st_transform(., crs = 2263) %>%
  filter(!str_detect(ca, "PTH")) #removing New Jersey PATH stations

#### Residential area ####

Pluto_ResOnly <- Pluto %>%
  filter(landuse>="01" & landuse<="04") %>%
  mutate(base_bbl = as.character(bbl)) %>%
  dplyr::select(-bbl)

ResBBLs <- as.character(Pluto_ResOnly$base_bbl)

# zcta-level residential area
Res_Bldg_Footprints <- Bldg_Footprints %>%  
  st_set_geometry(., NULL) %>%
  mutate(base_bbl = as.character(base_bbl)) %>%
  filter(base_bbl %in% ResBBLs &
           feat_code == "2100") %>%
  mutate(bldg_volume = shape_area * heightroof) %>%
  left_join(., Pluto_ResOnly, by = "base_bbl") %>%
  mutate(bldg_volume = if_else(is.na(bldg_volume), shape_area*numfloors*10, bldg_volume),
         res_volume = (bldg_volume/unitstotal)*unitsres, 
         zcta = as.character(zipcode)) %>%
  group_by(zcta) %>%
  summarise(total_res_volume_zcta = sum(res_volume, na.rm = TRUE)) 

# tract-level residential area 
Res_Bldg_Footprints2 <- Bldg_Footprints %>% 
  st_transform(crs = 2263) %>%
  suppressWarnings(st_centroid(of_largest_polygon = TRUE)) %>%
  mutate(base_bbl = as.character(base_bbl)) %>%
  filter(base_bbl %in% ResBBLs &
           feat_code == "2100") %>%
  mutate(bldg_volume = shape_area * heightroof) %>%
  left_join(., Pluto_ResOnly, by = "base_bbl") %>%
  mutate(bldg_volume = if_else(is.na(bldg_volume), shape_area*numfloors*10, bldg_volume),
         res_volume = (bldg_volume/unitstotal)*unitsres)
pm(get.tract.res <- function(res, tracts) st_intersection(res, tracts)) # takes a few minutes
res_bldg_tract <- get.tract.res(Res_Bldg_Footprints2, tractSF)
res_bldg_tract_sum <- st_set_geometry(res_bldg_tract, NULL) %>%
  group_by(GEOID) %>%
  summarise(total_res_volume_tract = sum(res_volume, na.rm = TRUE))
#nrow(res_bldg_tract_sum) # 2132


#### COVID Tests ####

MODZCTA_NYC_shp1 <- MODZCTA_NYC_shp %>%
  dplyr::select(modzcta, geometry) %>%
  rename("zcta" = "modzcta")

May7_tests <- ZCTA_test_series %>%
  filter(date=="2020-05-07") %>%
  mutate(zcta = as.character(MODZCTA)) %>%
  rename(total_tests = "Total") %>%
  dplyr::select(zcta, date, Positive, total_tests)

ZCTA_by_boro1 <- ZCTA_by_boro %>%
  mutate(boro = as.character(boro),
         zcta = as.character(zip)) %>%
  dplyr::select(-zip) %>%
  bind_rows(., 
            tibble(boro = as.character(c("Manhattan", "Manhattan" ,"Queens")), #correcting nas
                     zcta = as.character(c("10069", "10282", "11109"))))

# get water mask for maps
source(here("code/water_mask.R"))

# Figure 3B - Map of Tests by MODZCTA

theme_smallmaps <- theme(legend.title = element_text(face = "bold", size = 9), 
                         plot.title = element_text(size = 9.5),
                         panel.background = element_rect(fill = "#cccccc"), 
                         panel.grid = element_blank(),
                         legend.background = element_rect(fill = "transparent"),
                         legend.position = c(0.22, 0.80),
                         legend.text = element_text(size = 8.5),
                         plot.margin = unit(c(4,0,4,0), "pt"),
                         legend.key.size = unit(1.1, "lines"),
                         axis.text = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.y = element_blank(),
                         axis.title.x = element_blank())

fig3b <- MODZCTA_NYC_shp1 %>%
  left_join(., May7_tests, by = "zcta") %>%
  left_join(., ACS_Data2, by = "zcta") %>%
  filter(zcta != "99999") %>%
  mutate(pos_per_100000 = (Positive/total_pop1)*100000) %>%
  ggplot() +
  geom_sf(data = basemap_water, fill = "white", lwd = 0) + 
  geom_sf(aes(fill = pos_per_100000), lwd = 0.2)+
  scalebar(MODZCTA_NYC_shp1, dist = 5, dist_unit = "km", 
           transform = TRUE, model = "WGS84", 
           st.size = 2.8, height = 0.015, border.size = 0.5, 
           anchor = c(x = -73.71, y = 40.51)) + 
  labs(fill = "Positives per 100,000") +
  ggtitle("Cumulative positive COVID tests by zip code (May 7, 2020)") +
  scale_fill_gradientn(colours=brewer_pal("YlGnBu", type = "seq")(7)) + 
  coord_sf(crs = st_crs(MODZCTA_NYC_shp1),
           xlim = c(plot_bounds$xmin, plot_bounds$xmax), 
           ylim = c(plot_bounds$ymin, plot_bounds$ymax),
           expand = FALSE) +
  theme_bw(base_size = 6) + 
  theme_smallmaps
  
fig3b

#### Create data frames of all above information ####

ZCTA_ACS_COVID_shp <- MODZCTA_NYC_shp1 %>%
  st_transform(., crs = 2263) %>%
  dplyr::select(zcta, geometry) %>%
  left_join(., ACS_Data2, by = "zcta") %>%
  left_join(., May7_tests, by = "zcta") %>%
  left_join(., Res_Bldg_Footprints, by = "zcta") %>%
  left_join(., ACS_EssentialWrkr_Commute1, by = "zcta") %>%
  left_join(., zcta_grocers, by = c("zcta" = "modzcta")) %>%
  mutate(pop_density = as.numeric(total_pop1/st_area(geometry)),
         avg_hhold_size = round((total_pop1/total_hholds1), 2),
         pos_per_100000 = (Positive/total_pop1)*100000,
         testing_ratio = (total_tests/total_pop1),
         res_vol_zctadensity = as.numeric(total_res_volume_zcta/st_area(geometry)), 
         res_vol_popdensity = as.numeric(total_pop1/total_res_volume_zcta),
         pubtrans_ferrysubway_commute = pubtrans_subway_commute + pubtrans_ferry_commute,
         grocers = replace_na(grocers, 0),
         grocers_per_1000 = (grocers/total_pop1)*1000,
         pos_per_100000 = round(pos_per_100000, 0),
         valid_var = "0",
         one_over_grocers_per_1000 = if_else(is.infinite(1/grocers_per_1000), (1/.0293)+1, 1/grocers_per_1000), #max + 1 for zero values
         didnot_workhome_commute = 100 - workhome_commute,
         one_over_medincome = 1/medincome) %>%
  dplyr::select(-pubtrans_subway_commute, -pubtrans_ferry_commute) %>%
  left_join(., ZCTA_by_boro1, by = "zcta") %>%
  mutate_at(vars(starts_with("essentialworker_")), ~round((./over16total_industry1)*100, 2)) %>%
  filter(zcta != "99999") #remove na

ZCTA_ACS_COVID <- ZCTA_ACS_COVID_shp %>%
  st_set_geometry(., NULL) #remove geometry

tract_vars <- tractSF %>% # uses local CRS
  left_join(., st_set_geometry(acs_tracts2, NULL), by = "GEOID") %>%
  left_join(., acs_tracts_commute1, by = "GEOID") %>%
  left_join(., res_bldg_tract_sum, by = "GEOID") %>%
  left_join(., tract_grocers, by = "GEOID") %>%
  mutate(pop_density = as.numeric(total_pop1/st_area(geometry)),
         avg_hhold_size = round((total_pop1/total_hholds1), 2),
         res_vol_tractdensity = as.numeric(total_res_volume_tract/st_area(geometry)), 
         res_vol_popdensity = as.numeric(total_pop1/total_res_volume_tract),
         pubtrans_ferrysubway_commute = pubtrans_subway_commute + pubtrans_ferry_commute,
         grocers = replace_na(grocers, 0),
         grocers_per_1000 = (grocers/total_pop1)*1000,
         valid_var = "0",
         didnot_workhome_commute = 100 - workhome_commute,
         one_over_grocers_per_1000 = if_else(is.infinite(1/grocers_per_1000), (1/.0293)+1, 1/grocers_per_1000),
         one_over_medincome = 1/medincome) %>% 
  dplyr::select(-pubtrans_subway_commute, -pubtrans_ferry_commute) %>%
  mutate_at(vars(starts_with("essentialworker_")), ~round((./over16total_industry1)*100, 2))

#### Tract to ZCTA weighted assignment ####
# Shares of residential addresses in ZCTAs by Tract are later used to select
# representative tract-level SES values at a specified location on the ECDF

modzcta_to_zcta_chr <- data.frame(ZCTA = as.character(modzcta_to_zcta$ZCTA), 
                                  MODZCTA = as.character(modzcta_to_zcta$MODZCTA))

# Calculate the proportion of population each combined ZCTA contributes to MODZCTA
modzcta_to_zcta_pop <- modzcta_to_zcta_chr %>%
  left_join(ACS_Data_scaled[, c("GEOID", "total_pop1")], by = c("ZCTA" = "GEOID")) %>% 
  group_by(MODZCTA) %>%
  mutate(sum_pop = sum(total_pop1)) %>%
  ungroup() %>%
  mutate(pop_prop = total_pop1/sum_pop) %>% 
  arrange(MODZCTA)

# Sum ZCTA->Tract RES_RATIO weights when multiple ZCTA combine to a single MODZCTA
modzcta_to_tract <- modzcta_to_zcta_pop %>%
  filter(MODZCTA != "99999") %>% 
  # Weights of each ZCTA are scaled by their proportion of MODZCTA population
  left_join(zip_to_tract, by = c("ZCTA" = "ZIP")) %>%
  mutate(scaled_res_ratio = RES_RATIO * pop_prop) %>% 
  # note: MODZCTA 10001 & 10007 contain a ZCTA with a small share of population
  # but a zero share of RES_RATIO. This makes their sum of scaled_res_ratio by
  # MODZCTA less than 1, but this will not effect the weighted median or 3Q values
  # by tract.
  group_by(MODZCTA, TRACT) %>%
  dplyr::summarize(SUM_RES_RATIO = sum(scaled_res_ratio), .groups = "drop") %>%
  # Weights are then multiplied by the population density of each tract
  left_join(tract_vars, by = c("TRACT" = "GEOID")) %>%
  dplyr::select(MODZCTA, TRACT, SUM_RES_RATIO, total_pop1, total_hholds1) %>%
  filter(total_hholds1 > 0) %>% 
  mutate(tract_popdens = total_pop1/total_hholds1) %>% 
  mutate(W_RES_RATIO = SUM_RES_RATIO * tract_popdens) %>%
  dplyr::select(MODZCTA, TRACT, W_RES_RATIO)
modzcta_to_tract

# length(unique(modzcta_to_tract$MODZCTA)) #  177
# length(unique(modzcta_to_tract$TRACT))   # 2111
# length(unique(tractSF$GEOID))            # 2167

# check res_ratio against tracts with no population
tract_modzcta_pop <- modzcta_to_tract %>%
  left_join(acs_tracts2, by = c("TRACT" = "GEOID")) %>%
  dplyr::select(MODZCTA, TRACT, total_pop1, W_RES_RATIO)

# checking for tracts with no population but res_ratio > 0
# (not possible now that the weighted ratio uses population)
#tract_modzcta_pop %>% filter(total_pop1 == 0 & W_RES_RATIO > 0) %>% nrow() # 0

# check to make sure no MODZCTA have all zeroes for all tract res_ratios
all_zero_ratio <- tract_modzcta_pop %>% group_by(MODZCTA) %>% filter(all(W_RES_RATIO == 0))
if(nrow(all_zero_ratio) > 0){
  warning("The following MODZCTA have all zeroes for weighted tract residents:\n ", 
          paste(all_zero_ratio$MODZCTA, collapse = ","))
} 
modzcta_to_tract2 <- dplyr::select(tract_modzcta_pop, -total_pop1)

### Preparation done --- Now for the Analysis ###

#### Part 1: Creation of BWQS Neighborhood Infection Risk Scores ####

# Step 1: Create univariate scatterplots to make sure direction of associations are consistent for all variables
ggplot(ZCTA_ACS_COVID, aes(x = testing_ratio, y = pos_per_100000)) + geom_point() + geom_smooth(method = "lm") #covariate
ggplot(ZCTA_ACS_COVID, aes(x = one_over_medincome, y = pos_per_100000)) + geom_point() + geom_smooth(method = "lm")
ggplot(ZCTA_ACS_COVID, aes(x = not_insured, y = pos_per_100000)) + geom_point() + geom_smooth(method = "lm") 
ggplot(ZCTA_ACS_COVID, aes(x = one_over_grocers_per_1000, y = pos_per_100000)) + geom_point() + geom_smooth(method = "lm")
ggplot(ZCTA_ACS_COVID, aes(x = unemployed, y = pos_per_100000)) + geom_point() + geom_smooth(method = "lm") 
ggplot(ZCTA_ACS_COVID, aes(x = res_vol_popdensity, y = pos_per_100000)) + geom_point() + geom_smooth(method = "lm") 
ggplot(ZCTA_ACS_COVID, aes(x = didnot_workhome_commute, y = pos_per_100000)) + geom_point() + geom_smooth(method = "lm") 
ggplot(ZCTA_ACS_COVID, aes(x = not_quarantined_jobs, y = pos_per_100000)) + geom_point() + geom_smooth(method = "lm") 
ggplot(ZCTA_ACS_COVID, aes(x = avg_hhold_size, y = pos_per_100000)) + geom_point() + geom_smooth(method = "lm")
ggplot(ZCTA_ACS_COVID, aes(x = essentialworker_drove, y = pos_per_100000)) + geom_point() + geom_smooth(method = "lm") 
ggplot(ZCTA_ACS_COVID, aes(x = essentialworker_pubtrans, y = pos_per_100000)) + geom_point() + geom_smooth(method = "lm") 


SES_vars <- names(ZCTA_ACS_COVID %>% dplyr::select(one_over_medincome, not_insured, one_over_grocers_per_1000, unemployed, 
                                                    not_quarantined_jobs, essentialworker_pubtrans, essentialworker_drove, 
                                                    didnot_workhome_commute, res_vol_popdensity, avg_hhold_size))

# Step 2a: Examine relationships between explanatory variables to make sure nothing >0.9 correlation, as this could bias BWQS
Cors_SESVars <- cor(x = ZCTA_ACS_COVID %>% dplyr::select(all_of(SES_vars)), method = "kendall")
Cors_SESVars1 <- as.data.frame(Cors_SESVars)
Cors_SESVars1$var1 <- row.names(Cors_SESVars1)
Cors_SESVars2 <- gather(data = Cors_SESVars1, key = "var2", value = "correlation", -var1) %>%
  filter(var1 != var2) 

Cors_SESVars2 %>% arrange(desc(correlation)) %>% distinct(correlation, .keep_all = T) 

corrplot(Cors_SESVars, p.mat = Cors_SESVars, insig = "p-value", type = "lower", sig.level = -1, tl.col = "black", tl.srt = 45)

# Step 2b: Examine Univariable kendall associations for all selected variables with the outcome  
bind_cols(Variables = SES_vars,
          
        ZCTA_ACS_COVID %>%
            dplyr::select(all_of(SES_vars), pos_per_100000) %>%
            summarise_at(vars(all_of(SES_vars)), list(~cor(., pos_per_100000, method = "kendall"))) %>%
            t() %>%
            as_tibble(),
        
        ZCTA_ACS_COVID %>%
          dplyr::select(all_of(SES_vars), pos_per_100000) %>%
          summarise_at(vars(all_of(SES_vars)), 
                       list(~cor.test(., pos_per_100000, method = "kendall")$p.value)) %>%
          t() %>%
          as_tibble()) %>%
  
  mutate(`Correlation (Tau)` = round(V1...2, 3),
         `p value` = as.character(ifelse(V1...3 < 0.0001, "< 0.0001", round(V1...3, 3))),) %>%
  dplyr::select(-V1...2, -V1...3) 

# Step 3: Prepare data for BWQS and pass to stan for model fitting 
pm(fit_BWQS_model <- function(data_list, stan_model_path){
  fitted_model <- stan(
    file = stan_model_path,
    data = data_list,
    chains = 1,
    warmup = 2500,
    iter = 20000,
    thin = 10,
    refresh = 0,
    algorithm = "NUTS",
    seed = 1234,
    control = list(max_treedepth = 20,
                   adapt_delta = 0.999999999999999))
  return(fitted_model)
})

Compute_Bayes_R2 <- function(fit) {
  y_pred = exp(extract(fit,"eta")$eta)
  var_fit = apply(y_pred, 1, var)
  mean_fit = apply(y_pred, 1, mean)
  phi = extract(fit,"phi")$phi
  var_res = mean_fit  + (mean_fit^2)/phi
  r2 = var_fit / (var_fit + var_res)
  return(list(meanr2 = mean(r2),
              medianr2 = median(r2)))
}

prep_BWQS_data <- function(df, ses_varnames){
  y = as.numeric(df$pos_per_100000)
  X <- df %>%
    dplyr::select(all_of(ses_varnames))
  K <- ns(df$testing_ratio, df = 3)
  for (vname in ses_varnames)
    X[[vname]] <- ecdf(X[[vname]])(X[[vname]]) * 10
  data <-as.data.frame(cbind(y,X)) # Aggregate data in a data.frame
  
  data_list = list(N      = NROW(data),
                   N_new  = NROW(data),
                   C      = NCOL(X),
                   K      = NCOL(K),
                   XC     = cbind(as.matrix(X)),
                   XK     = cbind(K),
                   XC_new = cbind(as.matrix(X)),
                   XK_new = cbind(K),
                   Dalp   = rep(1,length(ses_varnames)),
                   y      = as.vector(data$y))
  return(list(data_list = data_list, X = X, K = K))
}

m1data <- prep_BWQS_data(ZCTA_ACS_COVID, SES_vars)

# fit our primary model -- negative binomial
m1 <- fit_BWQS_model(m1data$data_list, BWQS_stan_model)

# model diagnostics (n_eff as % of 1750, Rhat, trace plots, acf)
m1
min(summary(m1)$summary[,"n_eff"]/1750) # effective sample size is at worst 78%
traceplot(m1, pars = c("beta1", "W"))
stan_ac(m1, pars = c("beta1", "W"))

extract_waic(m1)
Compute_Bayes_R2(m1)$meanr2

# residual analysis with DHARMa
residuals_qq <- createDHARMa(simulatedResponse = t(extract(m1,"y_new")$y_new), observedResponse = m1data$data_list$y)
plotQQunif(residuals_qq, testOutliers = F, testDispersion = F)

# examine parameter estimates
exp(mean(extract(m1, "beta1")$beta1))
vars = c("phi", "beta0", "beta1", paste0("delta", 1:3), SES_vars)
parameters_to_drop <- c("phi", paste0("delta", 1:3), "beta0", "beta1")
number_of_coefficients <- length(vars)

BWQS_params <- bind_cols(as_tibble(summary(m1)$summary[1:number_of_coefficients,c(1,4,6,8:10)]), label = vars)

BWQS_params %>% filter(label == "beta1") %>% mutate_at(vars(1:3), ~exp(.))

BWQS_fits <- BWQS_params %>%
  rename(lower = "2.5%",
         upper = "97.5%") %>%
  filter(!label %in% parameters_to_drop) %>%
  arrange(desc(mean)) %>%
  mutate(group = factor(if_else(label == "one_over_medincome"|label =="not_insured"|label == "unemployed", "Finances &\nAccess to care",
                         if_else(label == "one_over_grocers_per_1000", "Food\nAccess",
                                 if_else(str_detect(label, "essential")|label == "not_quarantined_jobs"|label=="didnot_workhome_commute", "Commuting and\nEssential Work",
                                         if_else(label == "avg_hhold_size"|label == "res_vol_popdensity", "Population\nDensity", "Unmatched")))),
                        levels = c("Commuting and\nEssential Work", "Finances &\nAccess to care", "Population\nDensity", "Food\nAccess")))

labels1 <- c("one_over_medincome" = "1/\nMedian income", 
             "not_insured" = "Uninsured", 
             "unemployed" = "Unemployed", 
             "one_over_grocers_per_1000" = "1/\nGrocers per 1000",
             "not_quarantined_jobs" = "Essential Workers", 
             "essentialworker_pubtrans" = "Essential Worker:\nPublic Transit", 
             "essentialworker_drove" = "Essential Worker:\nDriving Commute", 
             "didnot_workhome_commute" = "1/\nWork from home", 
             "res_vol_popdensity" = "Population Density\nby Residential Volume", 
             "avg_hhold_size" = "Average people\nper household")

labels2 <- c("phi" = "Overdispersion", 
             "beta0" = "Intercept", 
             "beta1" = "BWQS term",
             "delta1" = "Testing ratio: spline term 1",
             "delta2" = "Testing ratio: spline term 2",
             "delta3" = "Testing ratio: spline term 3",
             labels1)

# create a table of parameter estimates
BWQS_params %>% bind_cols(., "terms" = labels2) %>%
  dplyr::select(-label) %>%
  mutate_at(vars(1:6), ~round(., 3)) %>%
  mutate_at(vars(5), ~round(., 0)) %>%
  kbl(caption = paste0("Negative Binomial BWQS")) %>%
  kable_classic(full_width = F, html_font = "Cambria")

# create a figure with parameter estimates for the weights
fig2 <- ggplot(data=BWQS_fits, aes(x= reorder(label, mean), y=mean, ymin=lower, ymax=upper)) +
  geom_pointrange() + 
  coord_flip() + 
  xlab("") + 
  ylab("Mean (95% credible intervals)") +
  scale_x_discrete(labels = labels1) + 
  theme_set(theme_bw(base_size = 18)) +
  facet_grid(group~., scales = "free", space = "free") +
  theme(strip.text.x = element_text(size = 14))
fig2
if(export.figs) ggsave(fig2, filename = file.path(fig.path, paste0("fig2", "_", Sys.Date(),".png")), dpi = 600, width = 8, height = 8)

# Step 4: Construct the summative COVID-19 inequity index value for each ZCTA
# Use the variable-specific weight on the quantiled splits to create a 10 point ZCTA-level infection risk score  
BWQS_weights <- as.numeric(summary(m1)$summary[(length(vars)-length(SES_vars) + 1):number_of_coefficients,c(1)])

ZCTA_ACS_COVID2 <- m1data$X * BWQS_weights[col(ZCTA_ACS_COVID)] 

BWQS_index <- ZCTA_ACS_COVID2 %>% 
  dplyr::mutate(BWQS_index = rowSums(.)) %>% 
  dplyr::select(BWQS_index) 

# calculate credible interval over the mean predicted infections
# at the median testing_ratio
sim_out <- data.frame(extract(m1, pars = c("beta0", "beta1", "delta")))
median_testing <- predict(m1data$K, median(ZCTA_ACS_COVID$testing_ratio))
# calculate term for median testing_rate
sim_out$deltamedian <- with(sim_out, median_testing[1] * delta.1 + 
                              median_testing[2] * delta.2 + 
                              median_testing[3] * delta.3)
# make a sequence of BWQS values
xseqlength <- 300
bwqs_seq <- seq(from = min(BWQS_index$BWQS_index), 
                to = max(BWQS_index$BWQS_index), 
                length.out = xseqlength)
sim_matrix <- sim_out$beta0 %o% rep(1, xseqlength) + 
  sim_out$beta1 %o% bwqs_seq + 
  sim_out$deltamedian%o% rep(1, xseqlength)
sim_df <- data.frame(bwqs_seq, 
                     lower = exp(colQuantiles(sim_matrix, probs = 0.025)),
                     upper = exp(colQuantiles(sim_matrix, probs = 0.975)),
                     mean = exp(colMeans(sim_matrix)))
ggplot(sim_df, aes(x = bwqs_seq)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey90") + 
  geom_line(aes(y = mean)) + 
  geom_point(data = data.frame(BWQS_index, y = m1data$data_list$y), aes(BWQS_index, y))
#beta0 + beta1*(XC_new[n,]*W) + XK_new[n,]*delta

# calculate credible interval over the mean predicted infections
# at the median COVID-19 inequity index
sim_out$beta1median <- sim_out$beta1 * median(BWQS_index$BWQS_index)
# make a sequence of testing_ratio values
xseqlength <- 300
inequity_seq <- data.frame(x = seq(from = min(ZCTA_ACS_COVID$testing_ratio),
                                   to = max(ZCTA_ACS_COVID$testing_ratio),
                                   length.out = xseqlength))
inequity_seq <- data.frame(x = inequity_seq$x, t(sapply(inequity_seq$x, FUN = function(x) predict(m1data$K, x))))
sim_matrix <- 
  sim_out$beta0 %o% rep(1, xseqlength) + 
  sim_out$beta1median %o% rep(1, xseqlength) + 
  sim_out$delta.1 %o% inequity_seq$X1  + 
  sim_out$delta.2 %o% inequity_seq$X2  + 
  sim_out$delta.3 %o% inequity_seq$X3
sim_df <- data.frame(x = inequity_seq$x, 
                     lower = exp(colQuantiles(sim_matrix, probs = 0.025)),
                     upper = exp(colQuantiles(sim_matrix, probs = 0.975)),
                     mean = exp(colMeans(sim_matrix)))
ggplot(sim_df, aes(x = x)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey90") + 
  geom_line(aes(y = mean)) + 
  geom_point(data = data.frame(testing_ratio = ZCTA_ACS_COVID$testing_ratio, y = m1data$data_list$y), aes(testing_ratio, y))


BWQS_predicted_infections = exp(BWQS_params[BWQS_params$label == "beta0", ]$mean + 
  (BWQS_params[BWQS_params$label == "beta1", ]$mean * BWQS_index) + 
  m1data$K %*% as.matrix(BWQS_params[grepl(pattern = "delta", BWQS_params$label), "mean"]))
colnames(BWQS_predicted_infections) <- "predicted"

# calculate predictions at the median BWQS index value
tests_predicted_terms = BWQS_params[BWQS_params$label == "beta0", ]$mean + 
  (BWQS_params[BWQS_params$label == "beta1", ]$mean * median(BWQS_index$BWQS_index)) 
tests_pred_median_bwqs <- data.frame(mean = exp(tests_predicted_terms + 
                                                  m1data$K %*% as.matrix(BWQS_params[grepl(pattern = "delta", BWQS_params$label), "mean"])))
rm(tests_predicted_terms)

# Visualize the relationship between BWQS and test_ratio
ggplot(data.frame(BWQS_index = BWQS_index$BWQS_index, testing_ratio = ZCTA_ACS_COVID$testing_ratio), aes(testing_ratio, BWQS_index)) + geom_point() + 
  geom_smooth() + geom_smooth(color = "red", method = "lm")
# BWQS is correlated with testing_ratio
cor.test(BWQS_index$BWQS_index, ZCTA_ACS_COVID$testing_ratio, method = "spearman")  

# Visualize the non-linear relationship between infection rate and test_ratio (not adjusted for social inequity)
# infections versus testing ratio with smoothers
ggplot(data.frame(y = ZCTA_ACS_COVID$pos_per_100000, testing_ratio = ZCTA_ACS_COVID$testing_ratio), aes(testing_ratio, y)) + geom_point() +
  ylab("infections per 100,000") +
  geom_smooth(color = "red", formula = y ~ x, method = "glm.nb") +
  stat_smooth(color = "green", method = "gam", formula = y ~ s(x), method.args = list(family = "nb"), se = F) +
  geom_smooth(method = "glm.nb", formula = y ~ splines::ns(x, 3), se = FALSE)

# Partial regression plot for BWQS index
# shows a nice linear relationship of BWQS index with infections after adjustment for testing
nb_testing_ns3df <- glm.nb(m1data$data_list$y ~ m1data$K)
yresid <- resid(nb_testing_ns3df)
bwqs_testing_ns3df <- lm(BWQS_index$BWQS_index ~ m1data$K)
bwqsresid <- resid(bwqs_testing_ns3df)
ggplot(data.frame(yresid, bwqsresid), aes(bwqsresid, yresid)) + geom_point() + 
  geom_smooth(formula = y ~ x, method = "lm", se = F) + 
  ylab("residual log infection rate\n(adjusted for testing)") + xlab("residual BWQS infection risk index\n(adjusted for testing)")
summary(lm(yresid ~ bwqsresid))

# Visualize the full model observed vs predicted -- shows a close relationship (which is why R2 is so high)
ggplot(data.frame(pred = BWQS_predicted_infections$predicted, m1data$data_list$y), aes(pred, m1data$data_list$y)) + 
  geom_point() +  
  geom_abline(linetype = "dashed", color = "grey10") + 
  coord_fixed() + 
  theme(aspect.ratio=1)

# Visualize the relationship between BWQS index and infection rate at the median testing_ratio
BWQS_scatter <- ggplot(data.frame(BWQS_index, y = m1data$data_list$y, BWQS_pred_median_testing), aes(BWQS_index, y)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = 'grey90') + 
  geom_line(aes(y = mean)) + 
  geom_point() + 
  scale_x_continuous("BWQS infection risk index") + 
  scale_y_continuous("Infections per 100,000", label=comma)
BWQS_scatter <- ggExtra::ggMarginal(BWQS_scatter, type = "histogram", margins = "both", xparams = list(binwidth = 1))
BWQS_scatter
if(export.figs) {
  png(filename = file.path(fig.path, paste0("fig1_", Sys.Date(), ".png")), width = 96*5, height = 96*5)
  print(BWQS_scatter)
  dev.off()
}

# Visualize the relationship between testing_ratio and infection rate at the median BWQS
testing_scatter <- ggplot(data.frame(BWQS_index, y = m1data$data_list$y, tests_pred_median_bwqs, testing_ratio = ZCTA_ACS_COVID$testing_ratio), 
                          aes(testing_ratio, m1data$data_list$y)) + 
  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) + 
  geom_line(aes(y = mean)) + 
  geom_point() + 
  scale_x_continuous("Testing ratio") + 
  scale_y_continuous("Infections per 100,000", label=comma) #+ 
  #theme(axis.title.y = element_blank(), axis.text.y=element_blank())
testing_scatter <- ggExtra::ggMarginal(testing_scatter, type = "histogram", xparams = list(binwidth = 0.005), yparams = list(binwidth = 200))
testing_scatter
if(export.figs) {
  png(filename = file.path(fig.path, paste0("Sfig2_", Sys.Date(), ".png")), width = 96*5, height = 96*5)
  print(testing_scatter)
  dev.off()
}

# Step 5: Visualize the spatial distribution of ZCTA-level infection risk scores 

ZCTA_BWQS_COVID_shp <- ZCTA_ACS_COVID_shp %>%
  bind_cols(., BWQS_index)

# reproject to WGS84 to be compatible with scalebar

ZCTA_BWQS_COVID_shp <- st_transform(ZCTA_BWQS_COVID_shp, 4326)

# Figure 3A - BWQS Index by ZCTA
fig3a <- ggplot() + 
  geom_sf(data = basemap_water, fill = "white", lwd = 0) + 
  geom_sf(data = ZCTA_BWQS_COVID_shp, aes(fill = BWQS_index), lwd = 0.2) + 
  scalebar(ZCTA_BWQS_COVID_shp, dist = 5, dist_unit = "km", 
           transform = TRUE, model = "WGS84", 
           st.size = 2.8, height = 0.015, border.size = 0.5, st.dist = 0.011,
           anchor = c(x = -73.71, y = 40.49)) + 
  scale_fill_gradientn(colours=brewer_pal("YlGnBu", type = "seq")(7)) + 
  coord_sf(crs = st_crs(ZCTA_BWQS_COVID_shp),
           xlim = c(plot_bounds$xmin, plot_bounds$xmax), 
           ylim = c(plot_bounds$ymin, plot_bounds$ymax),
            expand = FALSE) +
  theme_bw(base_size = 6) + 
  labs(fill = "COVID-19 Inequity Index") +
  theme(legend.title = element_text(face = "bold", size = 9), 
        panel.background = element_rect(fill = "#cccccc"), 
        legend.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        legend.position = c(0.125, 0.90),
        legend.text = element_text(size = 8.5),
        legend.key.size = unit(1.1, "lines"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())
fig3a

# Step 6: Compare quantile distribution of ZCTA-level BWQS scores by the race/ethnic composition of residents  
Demographics <- ACS_Data_scaled %>% 
  dplyr::select(GEOID, ends_with("_raceethnic"), total_pop1) %>%
  left_join(., modzcta_to_zcta1, by = c("GEOID" = "ZCTA")) %>%
  group_by(MODZCTA) %>%
  summarise_at(vars(ends_with("_raceethnic"), total_pop1), ~sum(.)) %>%
  mutate(zcta = as.character(MODZCTA),
         other_raceethnic = total_pop1 - (hisplat_raceethnic + nonhispLat_white_raceethnic + nonhispLat_black_raceethnic + 
                                            nonhispLat_amerindian_raceethnic + nonhispLat_asian_raceethnic))
  
ZCTA_BQWS <- ZCTA_BWQS_COVID_shp %>%
  st_set_geometry(., NULL) %>%
  dplyr::select(zcta, BWQS_index)

Demographics_for_ridges <- Demographics %>%
  left_join(., ZCTA_BQWS, by = "zcta") %>%
  dplyr::select(-total_pop1) %>%
  gather(key = "Race/Ethnicity", value = "Population", hisplat_raceethnic:other_raceethnic) %>%
  mutate(`Race/Ethnicity` = if_else(`Race/Ethnicity`=="hisplat_raceethnic","Hispanic/Latinx",
                                    if_else(`Race/Ethnicity`=="nonhispLat_black_raceethnic", "Black",
                                            if_else(`Race/Ethnicity`=="nonhispLat_white_raceethnic", "White",
                                                    if_else(`Race/Ethnicity`== "nonhispLat_asian_raceethnic", "Asian", "Other")))),
         `Race/Ethnicity` = factor(`Race/Ethnicity`, levels = c( "White",  "Asian", "Other","Hispanic/Latinx","Black")),
         Population = as.numeric(Population)) 

(Demographics_for_ridges %>%
  group_by(`Race/Ethnicity`) %>%
  summarise(weighted.mean(BWQS_index, Population),
            weightedMedian(BWQS_index, Population)))


fig4 <- ggplot(Demographics_for_ridges,
       aes(x = BWQS_index, y = `Race/Ethnicity`)) + 
  xlab("BWQS infection risk index")+
  theme(legend.position = "none") +
  geom_density_ridges(
    aes(height = ..density..,  
        weight = Population / sum(Population)),
    scale = 0.95,
    stat ="density") 
fig4
if(export.figs) ggsave(plot = fig4, filename = file.path(fig.path, paste0("fig4","_",Sys.Date(),".png")), dpi = 400, device = "png", width = 8, height = 5)

Below_25th_zctas <- ZCTA_BQWS %>%
  filter(BWQS_index<quantile(BWQS_index, .25))

Race_Ethncity_below25th <- Demographics %>%
  filter(zcta %in% Below_25th_zctas$zcta) %>%
  dplyr::select(-zcta, -MODZCTA) %>% 
  summarise_all(funs(sum(.))) %>%
  mutate_at(vars(ends_with("_raceethnic")), funs(round((./total_pop1)*100, 2))) %>%
  mutate(Group = "Below 25th percentile BWQS")

Between_25thand75th_zctas <- ZCTA_BQWS %>%
  filter(BWQS_index>quantile(BWQS_index, .25) & BWQS_index<quantile(BWQS_index, .75))

Race_Ethncity_btw_25th75th <- Demographics %>%
  filter(zcta %in% Between_25thand75th_zctas$zcta) %>%
  dplyr::select(-zcta, -MODZCTA) %>% 
  summarise_all(funs(sum(.))) %>%
  mutate_at(vars(ends_with("_raceethnic")), funs(round((./total_pop1)*100, 2)))%>%
  mutate(Group = "Between 25-75th percentile BWQS")

Above_75th_zctas <- ZCTA_BQWS %>%
  filter(BWQS_index>quantile(BWQS_index, .75))

Race_Ethncity_above_75th <- Demographics %>%
  filter(zcta %in% Above_75th_zctas$zcta) %>%
  dplyr::select(-zcta, -MODZCTA) %>% 
  summarise_all(funs(sum(.))) %>%
  mutate_at(vars(ends_with("_raceethnic")), funs(round((./total_pop1)*100, 2))) %>%
  mutate(Group = "Above 75th percentile BWQS")

Race_Ethncity_all <- Demographics %>%
  dplyr::select(-zcta, -MODZCTA) %>% 
  summarise_all(funs(sum(.))) %>%
  mutate_at(vars(ends_with("_raceethnic")), funs(round((./total_pop1)*100, 2)))%>%
  mutate(Group = "NYC Population")

Demographics_by_BWQS <- bind_rows(Race_Ethncity_all, Race_Ethncity_below25th, Race_Ethncity_btw_25th75th, Race_Ethncity_above_75th) %>%
  mutate(Other = other_raceethnic + nonhispLat_amerindian_raceethnic)  %>%
  dplyr::select(-other_raceethnic, -nonhispLat_amerindian_raceethnic, - total_pop1) %>%
  rename("Hispanic/Latinx" = "hisplat_raceethnic",
         "Black" = "nonhispLat_black_raceethnic", 
          "White" = "nonhispLat_white_raceethnic", 
         "Asian" = "nonhispLat_asian_raceethnic") %>%
  gather(., "Race/Ethnicity", "Proportion", 1:4,6) %>%
  mutate(`Race/Ethnicity` = factor(`Race/Ethnicity`, levels = c("Other", "Asian", "White", "Hispanic/Latinx", "Black")),
         Group = factor(Group, levels = c("NYC Population", "Below 25th percentile BWQS", "Between 25-75th percentile BWQS", "Above 75th percentile BWQS")))

labels_demographics <- c("NYC Population" = "NYC Population", "Below 25th percentile BWQS" = "Below 25th\npercentile BWQS", 
                         "Between 25-75th percentile BWQS" = "Between 25-75th\npercentile BWQS", "Above 75th percentile BWQS" = "Above 75th\npercentile BWQS")

sfig2 <- ggplot(Demographics_by_BWQS, aes(fill=`Race/Ethnicity`, y=Proportion, x=Group)) + 
    geom_rect(data = subset(Demographics_by_BWQS, Group=="NYC Population"), 
            aes(xmin=as.numeric(Group)-.35,xmax=as.numeric(Group)+.35, ymin=0, ymax=100, fill="gray85"), color = "gray", alpha = .1) +
  geom_bar(data = subset(Demographics_by_BWQS, Group!="NYC Population"), position="stack", stat="identity", width = .75) +
  geom_bar(data = subset(Demographics_by_BWQS, Group=="NYC Population"), position="stack", stat="identity", width = .45) +
  scale_fill_manual(breaks = c("Other", "Asian", "White", "Hispanic/Latinx", "Black"), 
                    values = c("#984ea3","#ff7f00","gray85", "#4daf4a", "#e41a1c", "#377eb8"))  +
  geom_text(aes(label=ifelse(Proportion >= 5, paste0(sprintf("%.0f", Proportion),"%"),"")),
            position=position_stack(vjust=0.5), colour="black", size = 8) + 
  scale_y_continuous("Percentage", expand = c(0,0), labels = function(x) paste0(x, "%")) + 
  scale_x_discrete(limits = c( "NYC Population", "Below 25th percentile BWQS", "Between 25-75th percentile BWQS", "Above 75th percentile BWQS"), 
                   labels = labels_demographics) + 
  theme_bw(base_size = 16) +
  theme(legend.text = element_text(size = 14),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"), 
        axis.title.x = element_blank()) 
sfig2
if(export.figs) ggsave(sfig2, filename = file.path(fig.path, paste0("sfig2","_",Sys.Date(),".png")), device = "png", dpi = 500, width = 12, height = 6)

#### BWQS by Tract ####

get_tract_vars_by_zcta <- function(tract_vars, modzcta_tract_crosswalk, whichq){
  whichq = str_to_lower(whichq)
  if(whichq %in% c("median", "q2", "2q", "2")){
    qname = "median"
    qnum = 2
  } else if(whichq %in% c("q3", "3q", "3")){
    qname = "3Q"
    qnum = 3
  } else { stop("unhandled quartile selected: ", whichq)}
  
  ses_zcta <- tract_vars %>% 
    st_set_geometry(., NULL) %>%
    left_join(., modzcta_tract_crosswalk, by = c("GEOID" = "TRACT")) %>% 
    filter(!is.na(MODZCTA)) %>% 
    group_by(MODZCTA) %>%
    summarise(essentialworker_drove_rename = Hmisc::wtd.quantile(essentialworker_drove, W_RES_RATIO)[[qnum]],
              essentialworker_pubtrans_rename = Hmisc::wtd.quantile(essentialworker_pubtrans, W_RES_RATIO)[[qnum]],
              not_quarantined_jobs_rename = Hmisc::wtd.quantile(not_quarantined_jobs, W_RES_RATIO)[[qnum]],
              didnot_workhome_commute_rename = Hmisc::wtd.quantile(didnot_workhome_commute, W_RES_RATIO)[[qnum]],
              not_insured_rename = Hmisc::wtd.quantile(not_insured, W_RES_RATIO)[[qnum]],
              one_over_medincome_rename = Hmisc::wtd.quantile(one_over_medincome, W_RES_RATIO)[[qnum]],
              unemployed_rename = Hmisc::wtd.quantile(unemployed, W_RES_RATIO)[[qnum]],
              avg_hhold_size_rename = Hmisc::wtd.quantile(avg_hhold_size, W_RES_RATIO)[[qnum]],
              res_vol_popdensity_rename = Hmisc::wtd.quantile(res_vol_popdensity, W_RES_RATIO)[[qnum]],
              one_over_grocers_per_1000_rename = Hmisc::wtd.quantile(one_over_grocers_per_1000, W_RES_RATIO)[[qnum]]) 
  
  ses_zcta %>% rename_with(~ gsub("rename", qname, .x))
}

# Select median tract values by MODZCTA, weighted by residential address share and population density by tract
SES_zcta_median <- get_tract_vars_by_zcta(tract_vars, modzcta_to_tract2, "median")
SES_vars_median = names(SES_zcta_median)[names(SES_zcta_median) != "MODZCTA"]

# join SES to testing data: positive/100k and testing_ratio
SES_zcta_median_testing <- ZCTA_ACS_COVID %>%
  dplyr::select(zcta, pos_per_100000, testing_ratio) %>%
  left_join(SES_zcta_median, by = c("zcta" = "MODZCTA"))

# BWQS using median at the tract level 
m2data_median <- prep_BWQS_data(SES_zcta_median_testing, SES_vars_median)
m2_median <- fit_BWQS_model(data_list = m2data_median$data_list, stan_model_path = BWQS_stan_model)

# Select third quartile tract values by MODZCTA, weighted by residential address share and population density by tract
SES_zcta_3q <- get_tract_vars_by_zcta(tract_vars, modzcta_to_tract2, "3q")
SES_vars_3q = names(SES_zcta_3q)[names(SES_zcta_3q) != "MODZCTA"]
SES_zcta_3q_testing <- ZCTA_ACS_COVID %>%
  dplyr::select(zcta, pos_per_100000, testing_ratio) %>%
  left_join(SES_zcta_3q, by = c("zcta" = "MODZCTA"))

#BWQS using the 3rd quartile at the tract level 
m2data_3q <- prep_BWQS_data(SES_zcta_3q_testing, SES_vars_3q)
m2_3q <- fit_BWQS_model(data_list = m2data_3q$data_list, stan_model_path = BWQS_stan_model)

Summarize_BWQS_performance <- function(model, df){
         model_type = deparse(substitute(model))
         waic = extract_waic(model)$waic[[1]]
         bayesr2 = Compute_Bayes_R2(model)$meanr2
         rmse = sqrt(mean((df$pos_per_100000 - colMeans(extract(model,"y_new")$y_new))^2))  
         effect_estimate = exp(mean(extract(model, "beta1")$beta1))
         
         summarized <- bind_cols(model_name = as.character(model_type), waic = waic, bayesr2 = bayesr2, rmse = rmse, effect_estimate = effect_estimate)
         
         return(summarized)
}

bind_rows(Summarize_BWQS_performance(m1, SES_zcta_median_testing),
  Summarize_BWQS_performance(m2_median, SES_zcta_median_testing),
  Summarize_BWQS_performance(m2_3q, SES_zcta_3q_testing),
)


#### Part 2: Compare capacity to socially distance (as measured by transit data) by neighborhood-level risk scores ####  

UHF_ZipCodes1 <- UHF_ZipCodes %>%
  mutate(zcta = as.character(zip),
         uhf = as.character(code)) %>%
  dplyr::select(zcta, uhf)
  
ZCTA_BWQS_score <- ZCTA_BWQS_COVID_shp %>%
  st_drop_geometry() %>%
  dplyr::select(zcta, BWQS_index)
                
UHF_BWQS <- ZCTA_ACS_COVID %>%
  left_join(., UHF_ZipCodes1, by = "zcta") %>%
  left_join(., ZCTA_BWQS_score, join = "zcta") %>%
  group_by(uhf) %>%
  summarise(uhf_weighted_bwqs = weighted.mean(BWQS_index, total_pop1)) #population weighting

UHF_BWQS_COVID_shp <- UHF_shp %>% 
  mutate(uhf = as.character(UHFCODE)) %>%
  left_join(., UHF_BWQS, by = "uhf") %>%
  mutate(Risk = factor(ifelse(uhf_weighted_bwqs > median(uhf_weighted_bwqs, na.rm = T), #median split of neighborhood risk
                              "High", "Low"), levels = c("High", "Low")))

ggplot(UHF_BWQS_COVID_shp) + 
  geom_sf(aes(fill = uhf_weighted_bwqs))

Subway_ridership_by_UHF %>% filter(place=="all" & date >= "2020-01-29" & date <= "2020-04-30") %>% 
           ggplot() + geom_point(aes(date, usage.median.ratio, color = place))

Mean_Ridership <- Subway_ridership_by_UHF %>% #Use the mean ridership to identify the proper function for a nonlinear model
  filter(date >= "2020-01-29" & date <= "2020-04-30" & place=="all") %>%   
  arrange(date) %>%
  mutate(time_index = time(date))

Mean_Ridership %>% 
  ggplot() + geom_point(aes(date, usage.median.ratio))

#The weibull probability distribution works best for these data
fit_drm_w2.4 <- drm(usage.median.ratio ~ time_index, fct =  W2.4(), data = Mean_Ridership)

# suppress warning about vector recycling in predict.drc.R
handler <- function(w) if( any( grepl( "Recycling array of length 1 in array-vector arithmetic is deprecated.", w) ) ){
  invokeRestart( "muffleWarning" ) } 

DRM_mean_predictions <- bind_cols(Mean_Ridership,
                                  as_tibble(withCallingHandlers(predict(fit_drm_w2.4, interval = "confidence"), warning = handler ))) 

sfig4 <- ggplot() + geom_point(data = DRM_mean_predictions, aes(x = Mean_Ridership$date, y = Mean_Ridership$usage.median.ratio)) + 
  geom_ribbon(data = DRM_mean_predictions, aes(x = date, ymin = Lower, ymax = Upper), fill = "grey50", alpha = .5) +
  geom_line(aes(x = DRM_mean_predictions$date, y = DRM_mean_predictions$Prediction), color = "red") + 
  theme_bw(base_size = 16) +
  xlab("Date") +
  ylab("Relative Subway Use (%)")
sfig4
if(export.figs) ggsave(sfig4, filename = file.path(fig.path, paste0("sfig4", "_", Sys.Date(), ".png")), device = "png", dpi = 400, width = 8, height = 5)

# create a dataframe for the analysis 
service_changes_in_lowsubway_areas <- tibble(date = as.Date(c("2020-02-01", "2020-02-02", "2020-02-08", "2020-02-09", "2020-02-15", "2020-02-16", "2020-02-22", "2020-02-23", "2020-02-29", "2020-03-01", "2020-03-07", "2020-03-08", 
                                                      "2020-02-01", "2020-02-02", "2020-02-08", "2020-02-09", "2020-02-16", "2020-02-16")),
                                             place = c("403","403","403","403","403","403","403","403","403","403","403","403", 
                                                       "502","502","502","502","502","502"))

Subway_BWQS_df <- Subway_ridership_by_UHF %>%
  left_join(., st_set_geometry(UHF_BWQS_COVID_shp, NULL), by = c("place" = "uhf")) %>%
  filter(!is.na(place) & date >= "2020-01-29" & date <= "2020-04-30") %>%
  group_by(place) %>%
  arrange(date) %>%
  mutate(time_index = time(date)) %>%
  filter(!is.na(Risk) & date != "2020-02-17") %>% # removing Presidents' Day national holiday 
  anti_join(., service_changes_in_lowsubway_areas, by = c("date", "place")) %>% # removing low outliers due to service changes in low subway density areas
  ungroup()
  
fit_drm_all <- drm(usage.median.ratio ~ time_index, fct = W2.4(), data = Subway_BWQS_df)
plot(fit_drm_all)
fit_drm_interact <- drm(usage.median.ratio ~ time_index, curveid = Risk, fct = W2.4(), data = Subway_BWQS_df, separate = T) #using this for inference
fit_drm_interact1 <- drm(usage.median.ratio ~ time_index, curveid = Risk, fct = W2.4(), data = Subway_BWQS_df, separate = F) #using this for plotting -- can't get separate = T to plot correctly!
fit_drm_risk_high <- drm(usage.median.ratio ~ time_index, curveid = Risk, fct = W2.4(), data = Subway_BWQS_df, subset = Risk=="High")
fit_drm_risk_low <- drm(usage.median.ratio ~ time_index, curveid = Risk, fct = W2.4(), data = Subway_BWQS_df, subset = Risk=="Low")
anova(fit_drm_all, fit_drm_interact) # comparing the mean only model to the interaction model 

summary(fit_drm_interact)
confint(fit_drm_interact)
compParm(fit_drm_interact, "b", "-")
compParm(fit_drm_interact, "c", "-")

slopes <- as_tibble(coef(fit_drm_interact), rownames = "vars") %>%
  separate(col = vars, into = c("vars", "BWQS_risk"), sep = ":") %>%
  spread(., vars, value) %>%
  mutate(slope = -b*(-d/(4*e)))

as_tibble(confint(fit_drm_interact), rownames = "vars") %>%
  separate(col = vars, into = c("vars", "BWQS_risk"), sep = ":") %>%
  filter(vars == "b") %>%
  right_join(., slopes, by = "BWQS_risk") %>%
  mutate(slope_lower_ci = -`2.5 %`*(-d/(4*e)),
  slope_higher_ci = -`97.5 %`*(-d/(4*e))) %>%
  dplyr::select(BWQS_risk, slope, slope_lower_ci, slope_higher_ci)

fit_drm_predictions <- as_tibble(withCallingHandlers(predict(fit_drm_interact1, as.data.frame(Subway_BWQS_df %>% dplyr::select(usage.median.ratio, time_index, Risk)), 
                                                             interval = "confidence"), warning = handler))
Subway_BWQS_df1 <- bind_cols(Subway_BWQS_df, fit_drm_predictions) 

Subway_BWQS_df2 <- Subway_BWQS_df1 %>%
  filter(date>"2020-02-16") %>% # subsetting for visualization
  mutate(Risk = if_else(Risk == "High", "High (above median)", "Low (below median)"))

fig5 <- ggplot() + 
  geom_jitter(data = Subway_BWQS_df2, aes(x = date, y = usage.median.ratio, color = Risk), alpha = .5, position = position_jitter(height = 0, width = 0.4))+ 
  geom_ribbon(data = subset(Subway_BWQS_df2, Risk == "High (above median)"), aes(x = date, ymin = Lower, ymax = Upper), fill = "grey50") +
  geom_ribbon(data = subset(Subway_BWQS_df2, Risk == "Low (below median)"), aes(x = date, ymin = Lower, ymax = Upper), fill = "grey50") +
  geom_line(data = Subway_BWQS_df2, aes(x = date, y = Prediction, color = Risk)) +
  scale_x_date("Date", date_minor_breaks = "1 week") + 
  scale_y_continuous("Relative Subway Ridership (%)", labels = scales::percent) + 
  geom_vline(xintercept = date("2020-03-22"),color = "grey30", lty = 2) + 
  theme_bw(base_size = 16) +
  labs(colour="BWQS Infection Risk Index") +
  theme(legend.title = element_text(face = "bold", size = 12), legend.position = c(0.8, 0.7))  
fig5 
if(export.figs) ggsave(fig5, filename = file.path(fig.path, paste0("fig5", "_", Sys.Date() ,".png")), dpi = 600, width = 8, height = 6)

# which UHF neighborhoods were dropped?
included_uhf <- Subway_BWQS_df %>% distinct(UHFCODE)
notincluded_uhf_shp <- UHF_BWQS_COVID_shp %>%
  filter(!UHFCODE %in% included_uhf$UHFCODE & 
           UHFCODE !=0) %>%
  mutate(NotIncluded = "*")

sfig3 <- ggplot() + 
  geom_sf(data = NYC_basemap_shp) +
  geom_sf(data = subset(UHF_BWQS_COVID_shp, !is.na(Risk)), aes(fill = Risk)) + 
  geom_sf(data = SubwayStation_shp) +
  geom_sf_text(data = notincluded_uhf_shp, aes(label = NotIncluded), size = 9) +
  xlab("") + ylab("") +
  theme_bw()
sfig3
if(export.figs) ggsave(sfig3, filename = file.path(fig.path, paste0("sfig3", "_", Sys.Date(),".png")), dpi = 500)


#### Part 3: Spatial analysis of mortality in relation to BWQS scores  ####

# Step 1: Create dataframes with the relevant information 
deaths_by23May2020_by_zcta1 <- deaths_by23May2020_by_zcta %>%
  left_join(., modzcta_to_zcta, by = c("MODIFIED_ZCTA" = "MODZCTA")) %>%
  mutate(zcta = as.character(MODIFIED_ZCTA)) %>%
  rename("deaths_count" = "COVID_DEATH_COUNT") %>%
  dplyr::select(zcta, deaths_count, COVID_DEATH_RATE) %>%
  distinct(zcta, .keep_all = T)

ZCTA_BWQS_COVID_shp1 <- ZCTA_ACS_COVID_shp %>% 
  left_join(.,ZCTA_BWQS_score, by = "zcta") %>%
  left_join(., deaths_by23May2020_by_zcta1, by = "zcta") %>%
  mutate(prop_65plus = age65_plus/total_pop1,
         zcta = as.numeric(zcta)) 

# Figure 3C - Mortality
fig3c <- ZCTA_BWQS_COVID_shp1 %>% 
  ggplot() +
  geom_sf(data = basemap_water, fill = "white", lwd = 0) + 
  geom_sf(aes(fill = COVID_DEATH_RATE), lwd = 0.2)+
  scalebar(MODZCTA_NYC_shp1, dist = 5, dist_unit = "km", 
           transform = TRUE, model = "WGS84", 
           st.size = 2.8, height = 0.015, border.size = 0.5,
           anchor = c(x = -73.71, y = 40.51)) + 
  labs(fill = "Mortality per 100,000") +
  ggtitle("Cumulative COVID mortality by zip code (May 23, 2020)") +
  scale_fill_gradientn(colours=brewer_pal("YlGnBu", type = "seq")(7)) + 
  coord_sf(crs = st_crs(ZCTA_BWQS_COVID_shp),
           xlim = c(plot_bounds$xmin, plot_bounds$xmax), 
           ylim = c(plot_bounds$ymin, plot_bounds$ymax),
           expand = FALSE) +
  theme_bw(base_size = 6) + 
  theme_smallmaps

fig3c

# Combine subfigures to make Figure 3: maps of BWQS, Tests, and Mortality by ZCTA
fig3a_2 = fig3a + labs(tag = "a") + theme(plot.tag.position = c(-0.018, 0.985), plot.tag = element_text(face = "bold", size = 15))
fig3b_2 = fig3b + labs(tag = "b") + theme(plot.tag.position = c(-0.028, 0.93), plot.tag = element_text(face = "bold", size = 15))
fig3c_2 = fig3c + labs(tag = "c") + theme(plot.tag.position = c(-0.028, 0.93), plot.tag = element_text(face = "bold", size = 15))

plotres = 300
agg_png(filename = file.path(fig.path, paste0("fig3_combined_", Sys.Date(), ".png")), 
        width = plotres*4.25, height = plotres*6, scaling = 2)
grid.arrange(arrangeGrob(fig3a_2, ncol = 1, nrow = 1), 
             arrangeGrob(fig3b_2, fig3c_2, ncol = 2, nrow = 1),
             nrow = 2, ncol = 1, heights = c(1.9,1))
dev.off()

# Step 2: Run negative binomial model with spatial filtering  

# Step 2a: Identify the neighbor relationships 
spdat.sens <- as_Spatial(ZCTA_BWQS_COVID_shp1)
ny.nb6 <- knearneigh(sp::coordinates(spdat.sens), k=6)
ny.nb6 <- knn2nb(ny.nb6)
ny.nb6 <- make.sym.nb(ny.nb6)
ny.wt6 <- nb2listw(ny.nb6, style="W")

# Step 2b: Fit the model to identify the component of the data with substantial spatial autocorrelation
fit.nb.ny.sens<-glm.nb(deaths_count~offset(log(total_pop1))+BWQS_index, spdat.sens)
lm.morantest(fit.nb.ny.sens, listw = ny.wt6)
me.fit.sens <- spatialreg::ME(deaths_count~offset(log(total_pop1))+BWQS_index,
                              spdat.sens@data, family=negative.binomial(fit.nb.ny.sens$theta), listw = ny.wt6, verbose=T, alpha=.1, nsim = 999)

# Step 2c: Pull out these fits and visualize the autocorrelation
fits.sens <- data.frame(fitted(me.fit.sens))
spdat.sens$me16 <- fits.sens$vec16
spdat.sens$me23 <- fits.sens$vec23
spplot(spdat.sens, "me16", at=quantile(spdat.sens$me16, p=seq(0,1,length.out = 7)))
spplot(spdat.sens, "me23", at=quantile(spdat.sens$me23, p=seq(0,1,length.out = 7)))

# Step 2d: Include the fits in our regression model as an additional parameter
clean.nb.sens<-glm.nb(deaths_count~offset(log(total_pop1))+BWQS_index+fitted(me.fit.sens), spdat.sens@data)
tidy(clean.nb.sens) %>% mutate(estimate_exp = exp(estimate))
as_tibble(confint(clean.nb.sens), rownames = "vars")%>% mutate_at(vars(2:3), .funs = list(~exp(.)))
lm.morantest(clean.nb.sens, resfun = residuals, listw=ny.wt6)
spdat.sens$nb_residuals <- clean.nb.sens$residuals
spplot(spdat.sens, "nb_residuals", at=quantile(spdat.sens$nb_residuals, p=seq(0,1,length.out = 10)))
ggplot() + geom_sf(data = st_as_sf(spdat.sens),aes(fill = cut_width(nb_residuals, width = 1))) + labs(fill = "Residuals\n(Standard Deviations)")


#### Sensitivity Analyses ####

# half-Cauchy prior for overdispersion parameter
BWQS_NB_halfcauchy_stan_model <- here("code", "nb_bwqs_halfcauchy.stan") 
m1_halfcauchy <- fit_BWQS_model(m1data$data_list, BWQS_NB_halfcauchy_stan_model)
# beta1 effect estimate using half-cauchy prior for overdispersion
exp(summary(m1_halfcauchy)$summary[3,c(1,4,8)])
# main model (inverse gamma prior for overdispersion)
exp(summary(m1)$summary[3,c(1,4,8)])
# no meaningful difference in effect estimate or credible interval!

# BWQS weights -- stability of the rankings 
as_tibble(extract(m1, c("W[1]","W[2]", "W[3]", "W[4]", "W[5]", "W[6]", "W[7]", "W[8]", "W[9]", "W[10]"))) %>%
  rownames_to_column("iteration") %>%
  gather(weight, value, "W[1]":"W[10]") %>%
  group_by(iteration) %>%
  slice(which.max(value)) %>%
  ungroup() %>%
    group_by(weight) %>%
    dplyr::summarise(times_largest_weight = n()) %>%
    mutate(pct_largest_weight = round((times_largest_weight/1750)*100, 2))


# using essential workers, testing ratio, and median income
glm_esstl_and_income <- glm.nb(pos_per_100000 ~ not_quarantined_jobs + medincome + testing_ratio, data = ZCTA_ACS_COVID)
broom::tidy(glm_esstl_and_income)
plot(glm_esstl_and_income)
enframe(predict(glm_esstl_and_income)) %>% mutate(value = exp(value))

# Just using proportion uninsured and median income
glm_insurance_and_income <- glm.nb(pos_per_100000 ~ not_insured + medincome, data = ZCTA_ACS_COVID)
broom::tidy(glm_insurance_and_income)

# PCA of social variables 
pca_socialvars <- prcomp(ZCTA_ACS_COVID %>% dplyr::select(all_of(SES_vars)), center = T, scale. = T)
PC1 <- enframe(pca_socialvars$rotation[,1]) %>% arrange(desc(value)) %>% rename("label" = "name")

df_for_PCA_analysis <- ZCTA_ACS_COVID %>% bind_cols(., PC1 = pca_socialvars$x[,1])

# Compare BWQS ranks to Principal Components ranks
BWQS_fits %>%
  dplyr::select(mean, label) %>%
  mutate(rank_bwqs = rank(mean)) %>%
  left_join(., PC1, by = "label") %>%
  mutate(rank_pca = rank(value)) %>%
  dplyr::select(2, 3, 5, 1, 4)

glm_princomp <- glm.nb(pos_per_100000 ~ PC1 + ns(testing_ratio, df = 3), data = df_for_PCA_analysis)
summary(glm_princomp)
exp(confint(glm.nb(pos_per_100000 ~ PC1 + ns(testing_ratio, df = 3), data = df_for_PCA_analysis)))

enframe(predict(glm_princomp)) %>% mutate(glm_princomp = exp(value)) %>% dplyr::select(glm_princomp)
enframe(predict(glm_esstl_and_income)) %>% mutate(glm_esstl_and_income = exp(value)) %>% dplyr::select(glm_esstl_and_income)

Compare_Metrics <- bind_cols(ZCTA_ACS_COVID,
                             glm_princomp = enframe(predict(glm_princomp)) %>% mutate(glm_princomp = exp(value)) %>% dplyr::select(glm_princomp),
                             glm_esstl_and_income = enframe(predict(glm_esstl_and_income)) %>% mutate(glm_esstl_and_income = exp(value)) %>% dplyr::select(glm_esstl_and_income),
                             BWQS_index = colMeans(extract(m1,"y_new")$y_new))

# comparison of RMSE and Kendall's tau metrics for three different modeling approaches
# Supplemental Table 4
bind_rows(Compare_Metrics %>% 
            summarise_at(vars(c("BWQS_index", "glm_princomp", "glm_esstl_and_income")), 
                         ~cor(pos_per_100000, .x, method = "kendall")) %>%
            mutate(parameter = "kendalls"),
          Compare_Metrics %>% 
            summarise_at(vars(c("BWQS_index", "glm_princomp", "glm_esstl_and_income")), 
                         ~sqrt(mean(pos_per_100000-.x)^2)) %>%
            mutate(parameter = "RMSE"))


#### Subway analyses ####

# Different BWQS groupings for subway analysis (<.25, .25-.75, .75)

UHF_BWQS_COVID_3split_shp <- UHF_shp %>% 
  mutate(uhf = as.character(UHFCODE)) %>%
  left_join(., UHF_BWQS, by = "uhf") %>%
  mutate(Risk = factor(ifelse(uhf_weighted_bwqs < quantile(uhf_weighted_bwqs, probs = .25, na.rm = T), "Low",
                              if_else(uhf_weighted_bwqs > quantile(uhf_weighted_bwqs, probs = .75, na.rm = T), "High", "Mid")),
                       levels = c("High", "Mid", "Low")))


Subway_BWQS_3split_df <- Subway_ridership_by_UHF %>%
  left_join(., st_set_geometry(UHF_BWQS_COVID_3split_shp, NULL), by = c("place" = "uhf")) %>%
  filter(!is.na(place) & date >= "2020-01-29" & date <= "2020-04-30") %>%
  group_by(place) %>%
  arrange(date) %>%
  mutate(time_index = time(date)) %>%
  filter(!is.na(Risk) & date != "2020-02-17") %>% #removing Presidents' Day national holiday 
  anti_join(., service_changes_in_lowsubway_areas, by = c("date", "place")) #removing low outliers due to service changes in low subway density areas

fit_drm_risk_3split <- drm(usage.median.ratio ~ time_index, curveid = Risk, fct = W2.4(), data = Subway_BWQS_3split_df, separate = F)
anova(fit_drm_all, fit_drm_risk_3split)
summary(fit_drm_risk_3split)
compParm(fit_drm_risk_3split, "b", operator = "-")
compParm(fit_drm_risk_3split, "c", operator = "-")
plot(fit_drm_risk_3split)

fit_drm_risk_3split_high <- drm(usage.median.ratio ~ time_index, curveid = Risk, fct = W2.4(), data = Subway_BWQS_3split_df, subset = Risk=="High")
fit_drm_risk_3split_mid <- drm(usage.median.ratio ~ time_index, curveid = Risk, fct = W2.4(), data = Subway_BWQS_3split_df, subset = Risk=="Mid")
fit_drm_risk_3split_low <- drm(usage.median.ratio ~ time_index, curveid = Risk, fct = W2.4(), data = Subway_BWQS_3split_df, subset = Risk=="Low")

summary(fit_drm_risk_3split_high)
summary(fit_drm_risk_3split_mid)
summary(fit_drm_risk_3split_low)

confint(fit_drm_risk_3split_high)
confint(fit_drm_risk_3split_mid)
confint(fit_drm_risk_3split_low)

# b is not actually a slope - but a scaling factor that needs to be transformed into the slope
slopes_3split <- as_tibble(coef(fit_drm_risk_3split_high), rownames = "vars") %>%
  bind_cols(BWQS_risk = "high") %>%
  bind_rows(as_tibble(coef(fit_drm_risk_3split_mid), rownames = "vars") %>% bind_cols(BWQS_risk = "mid")) %>%
  bind_rows(as_tibble(coef(fit_drm_risk_3split_low), rownames = "vars") %>% bind_cols(BWQS_risk = "low")) %>%
  mutate(vars = str_remove(vars, fixed(":(Intercept)"))) %>%
  spread(., vars, value) %>%
  mutate(slope = -b*(-d/(4*e))) 

as_tibble(confint(fit_drm_risk_3split_high), rownames = "vars") %>%
  bind_cols(BWQS_risk = "high") %>%
  bind_rows(as_tibble(confint(fit_drm_risk_3split_mid), rownames = "vars") %>% bind_cols(BWQS_risk = "mid")) %>%
  bind_rows(as_tibble(confint(fit_drm_risk_3split_low), rownames = "vars") %>% bind_cols(BWQS_risk = "low")) %>%
  mutate(vars = str_remove(vars, fixed(":(Intercept)"))) %>%
  filter(vars == "b") %>%
  right_join(., slopes_3split, by = "BWQS_risk") %>%
  mutate(slope_lower_ci = -`2.5 %`*(-d/(4*e)),
         slope_higher_ci = -`97.5 %`*(-d/(4*e))) %>%
  dplyr::select(BWQS_risk, slope, slope_lower_ci, slope_higher_ci)

fit_drm_predictions_3split <- as_tibble(withCallingHandlers(predict(fit_drm_risk_3split_high, interval = "confidence"), warning = handler) %>% 
                                   bind_cols(., Risk = "High")) %>%
  bind_rows(., as_tibble(withCallingHandlers(predict(fit_drm_risk_3split_mid, interval = "confidence"), 
                                             warning = handler ) %>% bind_cols(., Risk = "Mid"))) %>%
  bind_rows(., as_tibble(withCallingHandlers(predict(fit_drm_risk_3split_low, interval = "confidence"), 
                                             warning = handler ) %>% bind_cols(., Risk = "Low")))

Subway_BWQS_3split_df1 <- bind_cols(Subway_BWQS_3split_df %>% arrange(Risk, date), 
                             fit_drm_predictions_3split %>% dplyr::select(-Risk)) 


Subway_BWQS_3split_df2 <- Subway_BWQS_3split_df1 %>%
  filter(date>"2020-02-16") %>% # subsetting for visualization
  mutate(Risk = factor(if_else(Risk == "High", "High (≥ 75%ile)", 
                        if_else(Risk=="Mid", "Mid (IQR)", "Low (≤ 25%ile)")),
                       levels = c("High (≥ 75%ile)", "Mid (IQR)", "Low (≤ 25%ile)")))

# Supplementary Figure 5
sfig5 <- ggplot() + 
  geom_jitter(data = Subway_BWQS_3split_df2, aes(x = date, y = usage.median.ratio, color = as.factor(Risk)), alpha = .5, position = position_jitter(height = 0, width = 0.4))+ 
  geom_ribbon(data = subset(Subway_BWQS_3split_df2, Risk == "High (≥ 75%ile)"), aes(x = date, ymin = Lower, ymax = Upper), fill = "grey50") +
  geom_ribbon(data = subset(Subway_BWQS_3split_df2, Risk == "Mid (IQR)"), aes(x = date, ymin = Lower, ymax = Upper), fill = "grey50") +
  geom_ribbon(data = subset(Subway_BWQS_3split_df2, Risk == "Low (≤ 25%ile)"), aes(x = date, ymin = Lower, ymax = Upper), fill = "grey50") +
  geom_line(data = Subway_BWQS_3split_df2, aes(x = date, y = Prediction, color = as.factor(Risk))) +
  scale_x_date("Date", date_minor_breaks = "1 week") +
  scale_y_continuous("Relative Subway Ridership (%)", labels = scales::percent) + 
  geom_vline(xintercept = date("2020-03-22"),color = "grey30", lty = 2) + 
  theme_bw(base_size = 16) +
  labs(colour="BWQS Infection Risk Index") +
  theme(legend.title = element_text(face = "bold", size = 12), legend.position = c(0.8, 0.7))  
sfig5 

# ZCTA level BWQS for subway 
Subway_ridership_by_ZCTA <- relative.subway.usage(2020, by = "zcta")

# There are clearly some higher outliers that need to be removed at the zcta level 
# Remove the zctas associated with the uhfs below

high_april_bronx_subway <- tibble(date = as.Date(c("2020-04-04", "2020-04-05", "2020-04-11", "2020-04-12", "2020-04-18", "2020-04-19", "2020-04-25", "2020-04-26")),
                                  place = c("10469", "10469","10469","10469","10469","10469","10469","10469"))

Subway_BWQS_ZCTA_df <- Subway_ridership_by_ZCTA %>%
  left_join(., ZCTA_BWQS_score, by = c("place" = "zcta")) %>%
  filter(!is.na(place) & date >= "2020-01-29" & date <= "2020-04-30") %>%
  group_by(place) %>%
  arrange(date) %>%
  mutate(time_index = time(date)) %>%
  ungroup() %>%
  filter(!is.na(BWQS_index) & date != "2020-02-17") %>% #removing Presidents' Day national holiday 
  anti_join(., high_april_bronx_subway, by = c("date", "place")) %>% #removing low outliers due to service changes in low subway density areas
  mutate(Risk = factor(if_else(BWQS_index < quantile(BWQS_index, probs = .25), "Low",
                              if_else(BWQS_index > quantile(BWQS_index, probs = .75), "High", "Mid")),
                       levels = c("High", "Mid", "Low")))

fit_drm_risk_3split_zcta_high <- drm(usage.median.ratio ~ time_index, curveid = Risk, fct = W2.4(), data = Subway_BWQS_ZCTA_df, subset = Risk=="High")
fit_drm_risk_3split_zcta_mid <- drm(usage.median.ratio ~ time_index, curveid = Risk, fct = W2.4(), data = Subway_BWQS_ZCTA_df, subset = Risk=="Mid")
fit_drm_risk_3split_zcta_low <- drm(usage.median.ratio ~ time_index, curveid = Risk, fct = W2.4(), data = Subway_BWQS_ZCTA_df, subset = Risk=="Low")


summary(fit_drm_risk_3split_zcta_high)
summary(fit_drm_risk_3split_zcta_mid)
summary(fit_drm_risk_3split_zcta_low)


# b is not actually a slope - but a scaling factor that needs to be transformed into the slope
slopes_zcta <- as_tibble(coef(fit_drm_risk_3split_zcta_high), rownames = "vars") %>%
  bind_cols(BWQS_risk = "high") %>%
  bind_rows(as_tibble(coef(fit_drm_risk_3split_zcta_mid), rownames = "vars") %>% bind_cols(BWQS_risk = "mid")) %>%
  bind_rows(as_tibble(coef(fit_drm_risk_3split_zcta_low), rownames = "vars") %>% bind_cols(BWQS_risk = "low")) %>%
  mutate(vars = str_remove(vars, fixed(":(Intercept)"))) %>%
  spread(., vars, value) %>%
  mutate(slope = -b*(-d/(4*e))) 

as_tibble(confint(fit_drm_risk_3split_zcta_high), rownames = "vars") %>%
  bind_cols(BWQS_risk = "high") %>%
  bind_rows(as_tibble(confint(fit_drm_risk_3split_zcta_mid), rownames = "vars") %>% bind_cols(BWQS_risk = "mid")) %>%
  bind_rows(as_tibble(confint(fit_drm_risk_3split_zcta_low), rownames = "vars") %>% bind_cols(BWQS_risk = "low")) %>%
  mutate(vars = str_remove(vars, fixed(":(Intercept)"))) %>%
  filter(vars == "b") %>%
  right_join(., slopes_zcta, by = "BWQS_risk") %>%
  mutate(slope_lower_ci = -`2.5 %`*(-d/(4*e)),
         slope_higher_ci = -`97.5 %`*(-d/(4*e))) %>%
  dplyr::select(BWQS_risk, slope, slope_lower_ci, slope_higher_ci)

fit_drm_predictions_zcta <- as_tibble(withCallingHandlers(predict(fit_drm_risk_3split_zcta_high, interval = "confidence"), warning = handler) %>% 
                                   bind_cols(., Risk = "High")) %>%
  bind_rows(., as_tibble(withCallingHandlers(predict(fit_drm_risk_3split_zcta_mid, interval = "confidence"), 
                                             warning = handler ) %>% bind_cols(., Risk = "Mid"))) %>%
  bind_rows(., as_tibble(withCallingHandlers(predict(fit_drm_risk_3split_zcta_low, interval = "confidence"), 
                                             warning = handler ) %>% bind_cols(., Risk = "Low"))) # %>% mutate(time_index = row_number()) 

Subway_BWQS_ZCTA_df1 <- bind_cols(Subway_BWQS_ZCTA_df %>% arrange(Risk, date), 
                             fit_drm_predictions_zcta %>% dplyr::select(-Risk)) 


Subway_BWQS_ZCTA_df2 <- Subway_BWQS_ZCTA_df1 %>%
  filter(date>"2020-02-16") %>% # subsetting for visualization
  mutate(Risk = factor(if_else(Risk == "High", "High (≥ 75%ile)", 
                        if_else(Risk=="Mid", "Mid (IQR)", "Low (≤ 25%ile)")), 
                       levels = c("High (≥ 75%ile)", "Mid (IQR)", "Low (≤ 25%ile)")))

# Supplementary Figure 6
sfig6 <- ggplot() + 
  geom_jitter(data = Subway_BWQS_ZCTA_df2, aes(x = date, y = usage.median.ratio, color = Risk), alpha = .5, position = position_jitter(height = 0, width = 0.4))+ 
  geom_ribbon(data = subset(Subway_BWQS_ZCTA_df2, Risk == "High (≥ 75%ile)"), aes(x = date, ymin = Lower, ymax = Upper), fill = "grey50") +
  geom_ribbon(data = subset(Subway_BWQS_ZCTA_df2, Risk == "Mid (IQR)"), aes(x = date, ymin = Lower, ymax = Upper), fill = "grey50") +
  geom_ribbon(data = subset(Subway_BWQS_ZCTA_df2, Risk == "Low (≤ 25%ile)"), aes(x = date, ymin = Lower, ymax = Upper), fill = "grey50") +
  geom_line(data = Subway_BWQS_ZCTA_df2, aes(x = date, y = Prediction, color = Risk)) +
  scale_x_date("Date", date_minor_breaks = "1 week") + 
  scale_y_continuous("Relative Subway Ridership (%)", labels = scales::percent) + 
  geom_vline(xintercept = date("2020-03-22"),color = "grey30", lty = 2) + 
  theme_bw(base_size = 16) +
  labs(colour="BWQS Infection Risk Index") +
  theme(legend.title = element_text(face = "bold", size = 12), legend.position = c(0.8, 0.7))  
sfig6

# Supplementary Figure 9: compare MTA turnstile data to Google mobility reports
source(here("code/mta_vs_google.R"))
mobplot

#### Appendix
Sys.time()
sessioninfo::session_info()
