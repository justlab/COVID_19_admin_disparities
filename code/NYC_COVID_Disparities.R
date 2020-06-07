#CRAN packages:
library(tidyverse)
library(sf)
library(lubridate)
library(tidycensus)
library(GGally)
library(rstan)
library(drc)
library(spdep)
library(mgcv)
library(broom)
library(MASS)
library(spatialreg)
library(here)
library(pdftools)
library(matrixStats)
#Github packages available via remotes::install_github("justlab/Just_universal") and remotes::install_github("justlab/MTA_turnstile")
library(Just.universal) 
library(MTA.turnstile)

here() # current working directory

##To generate census data, you need an API key, which you can request here: https://api.census.gov/data/key_signup.html
#census_api_key("INSERT YOUR CENSUS API KEY HERE", install = TRUE) 

# data will default to a subfolder "data/" within working directory
# unless 1. set by an environment variable:
data.root = Sys.getenv("COVID_DATA")
# or 2. set with an alternative path here:
if (data.root == "") data.root = "data"
if (data.root == "data" & !dir.exists(data.root)) dir.create("data")
print(paste("data being downloaded into directory", dQuote(data.root)))
MTA_TURNSTILE_DATA_DIR = here("data")

##### FUNCTIONS ####

read_w_filenames <- function(flnm) {
  read_csv(flnm) %>%
    mutate(filename = flnm)
}

extract_waic <- function (stanfit){
  log_lik <- extract(stanfit, "log_lik")$log_lik
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

quantile_split <- function (data, mix_name = mix_name, q, shift = TRUE) {
  if (shift) {
    for (i in 1:length(mix_name)) {
      dat_num = as.numeric(unlist(data[, mix_name[i]]))
      data[[mix_name[i]]] = cut(dat_num, breaks = unique(quantile(dat_num, 
                                                                  probs = seq(0, 1, by = 1/q), na.rm = TRUE)), 
                                labels = FALSE, include.lowest = TRUE) - 1
    }
  }
  else {
    for (i in 1:length(mix_name)) {
      dat_num = as.numeric(unlist(data[, mix_name[i]]))
      data[[mix_name[i]]] = cut(dat_num, breaks = unique(quantile(dat_num, 
                                                                  probs = seq(0, 1, by = 1/q), na.rm = TRUE)), 
                                labels = FALSE, include.lowest = TRUE)
    }
  }
  return(data)
}

download = function(url, to, f, ...){
    download.update.meta(data.root, url, to, f, ...)
}


##### Load Data #####


# get the Pluto dataset from #https://www1.nyc.gov/site/planning/data-maps/open-data/dwn-pluto-mappluto.page 
Pluto = download(
    "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_20v3_csv.zip",
    "pluto.zip",
    function(p)
        read_csv(unz(p, "pluto_20v3.csv")))

Bldg_Footprints <- download(
  # https://data.cityofnewyork.us/Housing-Development/Building-Footprints/nqwf-w8eh
    "https://data.cityofnewyork.us/api/geospatial/utk2-gkjy?method=export&format=Shapefile",
    "building_footprints.zip",
    function(p)
        st_read(paste0("/vsizip/", p)))

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

#Download testing data 
ZCTA_test_series <- ZCTA_test_download %>% 
  map_df(~read_w_filenames(.)) %>%
  mutate(date = as.Date(str_extract(filename, "[:digit:]{4}-[:digit:]{2}-[:digit:]{2}"))) %>%
  dplyr::select(-filename)

ZCTAs_in_NYC <- as.character(unique(ZCTA_test_series$MODZCTA))

##Subway ridership data
Subway_ridership_by_UHF <- relative.subway.usage(2020L, "nhood")

#UHF definitions by zip codes
UHF_ZipCodes <- UHF_ZipCodes <- download(
    "http://www.infoshare.org/misc/UHF.pdf",
    "uhf_zips.pdf",
    function(p)
       {x = str_match_all(pdf_text(p)[2],
            "(\\d+)\\s+(\\S.+?\\S)\\s*([0-9,]+)")[[1]]
        do.call(rbind, lapply(1 : nrow(x), function(i)
            data.frame(code = x[i, 2], name = x[i, 3], zip = as.integer(
                str_extract_all(x[i, 4], "\\b\\d{5}\\b")[[1]]))))})

#UHF shapefile 
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

#DOHMH MODZCTA Shapefile
MODZCTA_NYC_shp <- download(
  "https://data.cityofnewyork.us/api/geospatial/pri4-ifjk?method=export&format=Shapefile",
  "Modified Zip Code Tabulation Areas (MODZCTA).zip", 
  function(p) read_sf(paste0("/vsizip/", p))
)

#Food outlets 
food_retail <- download(
    "https://data.ny.gov/api/views/9a8c-vfzj/rows.csv",
    "retail_food_stores.csv",
    read_csv)

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

##We have many sources of data, so these just help to combine the various data types
NYC_counties1 <- c("Bronx","Kings","Queens","New York","Richmond")
NYC_counties1_full <- c("Bronx County","Kings County","Queens County","New York County","Richmond County")
NYC_boro_county_match <- tibble(County = c("Bronx","Kings","Queens","New York","Richmond"), 
                                boro = c("Bronx","Brooklyn","Queens","Manhattan","Staten Island"), 
                                full_county = c("Bronx County","Kings County","Queens County","New York County","Richmond County"))

#upload the stan code alongside the disparities code 
BWQS_stan_model <- here("code", "nb_bwqs_cov.stan") 



####Census Data Collection and Cleaning####

ACS_Data <- get_acs(geography = "zcta", 
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

ACS_Data1 <- ACS_Data %>% #only pull out the estimates and cleaning variable names
  filter(GEOID %in% ZCTAs_in_NYC) %>%
  dplyr::select(-NAME)  %>%
  dplyr::select(GEOID, !ends_with("M")) %>%
  rename_at(vars(ends_with("E")), .funs = list(~str_sub(., end = -2)))


ACS_Data2 <- ACS_Data1 %>%
  mutate_at(vars(ends_with("_commute")), ~round((./total_commute1)*100, 2)) %>% #proportion of people relying on a given mode of transit
  mutate_at(vars(ends_with("_raceethnic")), ~round((./total_pop1)*100, 2)) %>% #proportion of ppl reporting a given race/ethncity 
  mutate(not_insured = round(((under19_noinsurance + age19_34_noinsurance + age35_64_noinsurance + age65plus_noinsurance) / total_pop1)*100, 2), #proportion uninsured
         snap_hholds = round((hholds_snap/total_hholds1)*100, 2), #proportion relying on SNAP
         fpl_150 = round(((fpl_100+fpl_100to150)/total_pop1)*100, 2), #proportion 150% or less of FPL
         unemployed = round((unemployed/over16total_industry1)*100, 2), #proportion unemployed
         not_quarantined_jobs = round(((ag_industry+(construct_industry*.25)+wholesaletrade_industry+ #an estimate of who is still leaving the house for work based on industry
                                        (edu_health_socasst_industry*.5)+transpo_and_utilities_industry)/over16total_industry1)*100, 2)) %>%
  dplyr::select(-ends_with("_noinsurance"), -fpl_100, -fpl_100to150, -ends_with("_industry"), -hholds_snap) %>%
  rename(zcta = "GEOID")


#### Estimating the mode of transportation for essential workers ####

ACS_EssentialWrkr_Commute <- get_acs(geography = "zcta", #pull down the relevant categories 
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


ACS_EssentialWrkr_Commute1 <- ACS_EssentialWrkr_Commute %>% #clean data and aggregate 
  dplyr::select(-ends_with("M"), -NAME) %>%
  filter(GEOID %in% ZCTAs_in_NYC) %>%
  mutate_at(vars(starts_with("ed_hlthcare")), ~round(./2), 0) %>% #maintain same proportions as estimated nonquarintined jobs
  mutate_at(vars(starts_with("construct")), ~round(./4), 0) %>%
  mutate(essentialworker_drove = rowSums(dplyr::select(., contains("car1_commute"))), 
         essentialworker_pubtrans = rowSums(dplyr::select(., contains("pubtrans")))) %>%
  rename(zcta = GEOID) %>%
  dplyr::select(zcta, essentialworker_drove, essentialworker_pubtrans)


#### Identify the number of supermarkets/grocery stores per area ####
non_supermarket_strings <- c("DELI|TOBACCO|GAS|CANDY|7 ELEVEN|7-ELEVEN|LIQUOR|ALCOHOL|BAKERY|CHOCOLATE|DUANE READE|WALGREENS|
                             CVS|RITE AID|RAVIOLI|WINERY|WINE|BEER|CAFE|COFFEE")

food_retail1 <- food_retail %>% #an estimate of how many supermarkets are in a ZCTA
  filter(County %in% NYC_boro_county_match$County) %>% #get rid of those that have the non_supermarket_strings words in their store & legal names
  filter(str_detect(`Establishment Type`, "J") & str_detect(`Establishment Type`, "A") & str_detect(`Establishment Type`, "C") &
           !str_detect(`Establishment Type`, "H")) %>%
  filter(!str_detect(`Entity Name`, non_supermarket_strings) & !str_detect(`DBA Name`, non_supermarket_strings)) %>%
  filter(`Square Footage`>=4500) %>%
  mutate(zcta = as.character(str_extract(Location, "[:digit:]{5}"))) %>%
  group_by(zcta) %>%
  summarise(grocers = n_distinct(`License Number`))

### Where are subway stations located? ###

SubwayStation_shp <- as_tibble(turnstile()$stations) %>%
  st_as_sf(., coords = c("lon", "lat"), crs = 4269) %>%
  st_transform(., crs = 2263) %>%
  filter(!str_detect(ca, "PTH")) #removing New Jersey PATH stations

### Calculate the residential area per ZCTA ###

Pluto_ResOnly <- Pluto %>%
  filter(landuse>="01" & landuse<="04") %>%
  mutate(base_bbl = as.character(bbl)) %>%
  dplyr::select(-bbl)

ResBBLs <- as.character(Pluto_ResOnly$base_bbl)

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



#### COVID Tests  ####
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

MODZCTA_NYC_shp1 %>%
  left_join(., May7_tests, by = "zcta") %>%
  left_join(., ACS_Data2, by = "zcta") %>%
  filter(zcta != "99999") %>%
  mutate(pos_per_100000 = (Positive/total_pop1)*100000) %>%
  ggplot() +
  geom_sf(data = NYC_basemap_shp)+
  geom_sf(aes(fill = pos_per_100000))+
  labs(fill = "Positives per 100,000") +
  ggtitle("Cumulative Positive COVID tests by zip code (May 7, 2020)") +
  scale_fill_gradient(low = "#6bf756", high = "#f75656") + 
  theme_bw() +
  theme_bw(base_size = 15) + 
  theme(legend.title = element_text(face = "bold"), legend.position = c(0.25, 0.8))
 


#### Create data frames of all above information ####

ZCTA_ACS_COVID_shp <- MODZCTA_NYC_shp1 %>%
  st_transform(., crs = 2263) %>%
  dplyr::select(zcta, geometry) %>%
  left_join(., ACS_Data2, by = "zcta") %>%
  left_join(., May7_tests, by = "zcta") %>%
  left_join(., Res_Bldg_Footprints, by = "zcta") %>%
  left_join(., ACS_EssentialWrkr_Commute1, by = "zcta") %>%
  left_join(., food_retail1, by = "zcta") %>%
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
         didnot_workhome_commute = 1/workhome_commute,
         one_over_grocers_per_1000 = if_else(is.infinite(1/grocers_per_1000), 0, 1/grocers_per_1000),
         one_over_medincome = 1/medincome) %>%
  dplyr::select(-pubtrans_subway_commute, -pubtrans_ferry_commute) %>%
  left_join(., ZCTA_by_boro1, by = "zcta") %>%
  mutate_at(vars(starts_with("essentialworker_")), ~round((./over16total_industry1)*100, 2)) %>%
  filter(zcta != "99999") #remove na

ZCTA_ACS_COVID <- ZCTA_ACS_COVID_shp %>%
  st_set_geometry(., NULL) #remove geometry

#any missing data?
ZCTA_ACS_COVID %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_each(funs(sum(is.na(.))))


### Cleaning done --- Now for the Analysis ###

#### Part 1: Creation of BWQS Neighborhood Infection Risk Scores ####

#Step 1: Create univariate scatterplots to make sure direction of associations are consistent for all variables, otherwise BWQS is biased
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

#Step 2a: Visualize relationships between explanatory variables to make sure nothing >0.9 correlation, as this could bias BWQS
ggpairs(ZCTA_ACS_COVID, columns = SES_vars) 

## Step 2b: Examine Univariable spearman associations for all selected variables with the outcome  
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
  
  mutate(`Correlation (Tau)` = round(V1, 3),
         `p value` = as.character(ifelse(V11 < 0.0001, "< 0.0001", round(V11, 3))),) %>%
  dplyr::select(-V1, -V11) 

#Step 3: Prepare data for BWQS and pass to stan for model fitting 
y = as.numeric(ZCTA_ACS_COVID$pos_per_100000)
X <- ZCTA_ACS_COVID %>%
  dplyr::select(all_of(SES_vars))
K <- ZCTA_ACS_COVID %>%
  dplyr::select(testing_ratio)
X <- quantile_split(data = X, mix = SES_vars, q = 10)
data <-as.data.frame(cbind(y,X)) # Aggregate data in a data.frame

data_list = list(N  = NROW(data), 
                 C  = NCOL(X), 
                 K  = NCOL(K), 
                 XC = cbind(as.matrix(X)), 
                 XK = cbind(K), 
                 Dalp = rep(1,length(SES_vars)), 
                 y = as.vector(data$y))

m1 = stan(file = BWQS_stan_model,
                         data = data_list, chains = 1,
                         warmup = 2500, iter = 20000, cores = 1,
                         thin = 10, refresh = 0, algorithm = "NUTS",
                         seed = 1234, control = list(max_treedepth = 20,
                                                     adapt_delta = 0.999999999999999))

extract_waic(m1)

vars = c("phi", "beta0", "beta1", "delta1", SES_vars)
parameters_to_drop <- c("phi", "delta1", "beta0", "beta1")
number_of_coefficients <- length(SES_vars) + 4

BWQS_params <- bind_cols(as_tibble(summary(m1)$summary[1:number_of_coefficients,c(1,4,8)]), label = vars)

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

labels1 <- c("one_over_medincome" = "1/\nmedian income", 
             "not_insured" = "Uninsured", 
             "unemployed" = "Unemployed", 
             "one_over_grocers_per_1000" = "1/\nGrocers per 1000",
             "not_quarantined_jobs" = "Essential Workers", 
             "essentialworker_pubtrans" = "Essential Worker:\nPublic Transit", 
             "essentialworker_drove" = "Essential Worker:\nDriving Commute", 
             "didnot_workhome_commute" = "1/\nWork from home", 
             "res_vol_popdensity" = "Population Density\nby Residential Volume", 
             "avg_hhold_size" = "Average people\nper household")


ggplot(data=BWQS_fits, aes(x=label, y=mean, ymin=lower, ymax=upper)) +
  geom_pointrange() + 
  coord_flip() + 
  xlab("") + 
  ylab("Mean (95% credible intervals)") +
  scale_x_discrete(labels = labels1) + 
  theme_set(theme_bw(base_size = 18)) +
  facet_grid(group~., scales = "free", space = "free") +
  theme(strip.text.x = element_text(size = 14))
  
  
#Step 4: Use the variable-specific weight on the decile quantile splits to create a 10 point ZCTA-level infection risk score  

BWQS_weights <- as.numeric(summary(m1)$summary[5:number_of_coefficients,c(1)])

ZCTA_ACS_COVID2 <- X*BWQS_weights[col(ZCTA_ACS_COVID)] 

BWQS_index <- ZCTA_ACS_COVID2 %>% 
  dplyr::mutate(BWQS_index = rowSums(.)) %>% 
  dplyr::select(BWQS_index) 

BWQS_predicted_infection_median_testing = exp(BWQS_params[BWQS_params$label == "beta0", ]$mean + 
  (BWQS_params[BWQS_params$label == "beta1", ]$mean * BWQS_index) + 
  (BWQS_params[BWQS_params$label == "delta1", ]$mean * median(K$testing_ratio)))
colnames(BWQS_predicted_infection_median_testing) <- "predicted"

# Visualize the relationship between BWQS index and infection rate
BWQS_scatter <- ggplot(data.frame(BWQS_index, y, BWQS_predicted_infection_median_testing), aes(BWQS_index, y)) + geom_point() + 
  geom_line(aes(y = predicted)) + scale_x_continuous("BWQS Index") + scale_y_continuous("Infections per 100,000")
BWQS_scatter <- ggExtra::ggMarginal(BWQS_scatter, type = "histogram", xparams = list(binwidth = 1), yparams = list(binwidth = 200))
# png(filename = "bwqs_scatter.png", width = 96*5, height = 96*5)
#   print(BWQS_scatter)
# dev.off()

ZCTA_BWQS_COVID_shp <- ZCTA_ACS_COVID_shp %>% bind_cols(., BWQS_index)

#Step 5: Visualize the spatial distribution of ZCTA-level infection risk scores 
plot <- ggplot(ZCTA_BWQS_COVID_shp) + 
  geom_sf(aes(fill = BWQS_index, text = paste("ZCTA <b>", zcta, "</b> had \n", round(pos_per_100000, 0), 
                                             "positive cases per 100,000 and a WQS score of \n", round(BWQS_index, 2)))) + 
  scale_fill_gradient(low = "#6bf756", high = "#f75656")

plot + theme_bw(base_size = 15) + 
  labs(fill = "Infection Risk Index") +
  theme(legend.title = element_text(face = "bold"), 
        legend.position = c(0.25, 0.8))

#Step 6: Example spatial autocorrelation 

##Step 6a: Create spatial residuals dataframes 
BWQS_residuals <- bind_cols(infection_rate = y, K, BWQS_index) %>%
  mutate(beta0 = BWQS_params[BWQS_params$label == "beta0", ]$mean, 
         beta1 = BWQS_params[BWQS_params$label == "beta1", ]$mean, 
         delta = BWQS_params[BWQS_params$label == "delta1", ]$mean)

BWQS_residuals_shp <- ZCTA_BWQS_COVID_shp %>%
  dplyr::select(zcta, geometry) %>%
  bind_cols(., BWQS_residuals) %>%
  mutate(prediction = beta0 + (beta1 * BWQS_index) + (delta * testing_ratio),
         residuals = log(infection_rate) - prediction,
         std_residuals = residuals/sd(residuals))

plot(BWQS_residuals_shp$residuals)
plot(BWQS_residuals_shp$std_residuals)
hist(BWQS_residuals_shp$residuals)
hist(BWQS_residuals_shp$std_residuals)

# #creating contiguity matrix to assess spatial autocorrelation
# spdat_bwqs_resid <- as_Spatial(BWQS_residuals_shp)
# bwqs.resid.ny.nb4 <- knearneigh(coordinates(spdat_bwqs_resid), k=4)
# bwqs.resid.ny.nb4 <- knn2nb(bwqs.resid.ny.nb4)
# bwqs.resid.ny.nb4 <- make.sym.nb(bwqs.resid.ny.nb4)
# bwqs.resid.ny.wt4 <- nb2listw(bwqs.resid.ny.nb4, style="W")
# 
# moran.mc(spdat_bwqs_resid$infection_rate, listw=ny.wt4, nsim = 999)
# moran.mc(spdat_bwqs_resid$residuals, listw=ny.wt4, nsim = 999)
# 
# #need to construct the nhood matrix up here -- right now it's at the end of the code
# moran.mc(BWQS_residuals_shp$infection_rate, listw=ny.wt4, nsim = 999)
# # moran.test(BWQS_residuals_shp$residuals, listw=ny.wt4)
# moran.mc(BWQS_residuals_shp$residuals, listw=ny.wt4, nsim = 999)


#Step 6: Compare quantile distribution of ZCTA-level BWQS scores by the race/ethnic composition of residents  
Demographics <- ACS_Data1 %>% rename(zcta = "GEOID") %>%
  dplyr::select(zcta,ends_with("_raceethnic"), total_pop1) %>%
  mutate(other_raceethnic = total_pop1 - (rowSums(.[2:6])))

ZCTA_BQWS <- ZCTA_BWQS_COVID_shp %>%
  st_set_geometry(., NULL) %>%
  dplyr::select(zcta, BWQS_index)


Below_25th_zctas <- ZCTA_BQWS %>%
  filter(BWQS_index<quantile(BWQS_index, .25))

Race_Ethncity_below25th <- Demographics %>%
  filter(zcta %in% Below_25th_zctas$zcta) %>%
  dplyr::select(-zcta) %>% 
  summarise_all(funs(sum(.))) %>%
  mutate_at(vars(ends_with("_raceethnic")), funs(round((./total_pop1)*100, 2)))%>%
  mutate(Group = "Below 25th percentile BWQS")


Between_25thand75th_zctas <- ZCTA_BQWS %>%
  filter(BWQS_index>quantile(BWQS_index, .25) & BWQS_index<quantile(BWQS_index, .75))

Race_Ethncity_btw_25th75th <- Demographics %>%
  filter(zcta %in% Between_25thand75th_zctas$zcta) %>%
  dplyr::select(-zcta) %>% 
  summarise_all(funs(sum(.))) %>%
  mutate_at(vars(ends_with("_raceethnic")), funs(round((./total_pop1)*100, 2)))%>%
  mutate(Group = "Between 25-75th percentile BWQS")

Above_75th_zctas <- ZCTA_BQWS %>%
  filter(BWQS_index>quantile(BWQS_index, .75))

Race_Ethncity_above_75th <- Demographics %>%
  filter(zcta %in% Above_75th_zctas$zcta) %>%
  dplyr::select(-zcta) %>% 
  summarise_all(funs(sum(.))) %>%
  mutate_at(vars(ends_with("_raceethnic")), funs(round((./total_pop1)*100, 2))) %>%
  mutate(Group = "Above 75th percentile BWQS")

Race_Ethncity_all <- Demographics %>%
  dplyr::select(-zcta) %>% 
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

ggplot(Demographics_by_BWQS, aes(fill=`Race/Ethnicity`, y=Proportion, x=Group)) + 
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


#### Step 2: Compare capacity to socially distance (as measured by transit data) by neighborhood-level risk scores ####  

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

DRM_mean_predictions <- bind_cols(Mean_Ridership,
                                  as.tibble(predict(fit_drm_w2.4, interval = "confidence"))) 

ggplot() + geom_point(data = DRM_mean_predictions, aes(x = Mean_Ridership$date, y = Mean_Ridership$usage.median.ratio)) + 
  geom_ribbon(data = DRM_mean_predictions, aes(x = date, ymin = Lower, ymax = Upper), fill = "grey50", alpha = .5) +
  geom_line(aes(x = DRM_mean_predictions$date, y = DRM_mean_predictions$Prediction), color = "red") + 
  theme_bw(base_size = 16) +
  xlab("Date") +
  ylab("Relative Subway Use (%)")

#create a dataframe for the analysis 
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
  filter(!is.na(Risk) & date != "2020-02-17") %>% #removing Presidents' Day national holiday 
  anti_join(., service_changes_in_lowsubway_areas, by = c("date", "place")) #removing low outliers due to service changes in low subway density areas

fit_drm_all <- drm(usage.median.ratio ~ time_index, fct = W2.4(), data = Subway_BWQS_df)
fit_drm_interact <- drm(usage.median.ratio ~ time_index, curveid = Risk, fct = W2.4(), data = Subway_BWQS_df)

anova(fit_drm_all, fit_drm_interact) #comparing the mean only model to the stratified 

summary(fit_drm_interact)
confint(fit_drm_interact)

#b is not actually a slope - but a scaling factor that needs to be transformed into the slope with this formula: -b*(-d/(4*t[e])))
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

fit_drm_predictions <- as_tibble(predict(fit_drm_interact, interval = "confidence"))
Subway_BWQS_df1 <- bind_cols(Subway_BWQS_df, fit_drm_predictions) 

ggplot() + 
  geom_jitter(data = Subway_BWQS_df1, aes(x = date, y = usage.median.ratio, color = Risk), alpha = .5, position = position_jitter(height = 0, width = 0.4))+ 
  geom_ribbon(data = subset(Subway_BWQS_df1, Risk == "High"), aes(x = date, ymin = Lower, ymax = Upper), fill = "grey50") +
  geom_ribbon(data = subset(Subway_BWQS_df1, Risk == "Low"), aes(x = date, ymin = Lower, ymax = Upper), fill = "grey50") +
  geom_line(data = Subway_BWQS_df1, aes(x = date, y = Prediction, color = Risk)) +
  scale_x_date(limits = c(date("2020-02-16"), date("2020-04-30")), date_minor_breaks = "1 week") + 
  xlab("Date") +
  ylab("Relative Subway Ridership (%)") + 
  geom_vline(xintercept = date("2020-03-22"),color = "grey30", lty = 2) + 
  theme_bw(base_size = 16) +
  theme(legend.title = element_text(face = "bold"), legend.position = c(0.9, 0.7)) + 
  scale_y_continuous(limits = c(0, 1.1))

#which ones were dropped?
included_uhf <- Subway_BWQS_df %>% distinct(UHFCODE)
notincluded_uhf_shp <- UHF_BWQS_COVID_shp %>%
  filter(!UHFCODE %in% included_uhf$UHFCODE & 
           UHFCODE !=0) %>%
  mutate(NotIncluded = "*")

ggplot() + 
  geom_sf(data = NYC_basemap_shp) +
  geom_sf(data = subset(UHF_BWQS_COVID_shp, !is.na(Risk)), aes(fill = Risk)) + 
  geom_sf(data = SubwayStation_shp) +
  geom_sf_text(data = notincluded_uhf_shp, aes(label = NotIncluded), size = 9) +
  xlab("") + ylab("") +
  theme_bw()


#### Part 3: Spatial analysis of mortality in relation to BWQS scores  ####

#Step 1: Create dataframes with the relevant information 
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

ZCTA_BWQS_COVID_shp1 %>% #1000*750
  ggplot() +
  geom_sf(data = NYC_basemap_shp)+
  geom_sf(aes(fill = COVID_DEATH_RATE))+
  labs(fill = "Mortality per 100,000") +
  ggtitle("Cumulative COVID Mortality by zip code (May 23, 2020)") +
  scale_fill_gradient(low = "#6bf756", high = "#f75656") + 
  theme_bw() +
  theme_bw(base_size = 15) + 
  theme(legend.title = element_text(face = "bold"), legend.position = c(0.25, 0.8))


#Step 2: Run negative binomial model with spatial filtering  

#Step 2a: Identify the neighbor relationships 
spdat <- as_Spatial(ZCTA_BWQS_COVID_shp1)
ny.nb4 <- knearneigh(coordinates(spdat), k=4)
ny.nb4 <- knn2nb(ny.nb4)
ny.nb4 <- make.sym.nb(ny.nb4)
ny.wt4 <- nb2listw(ny.nb4, style="W")

#Step 2b: Fit the model to identify the component of the data with substantial spatial autocorrelation
fit.nb.ny<-glm.nb(deaths_count~offset(log(total_pop1))+scale(age65_plus)+BWQS_index, spdat)
lm.morantest(fit.nb.ny, listw = ny.wt4)
# me.fit <- spatialreg::ME(deaths_count~offset(log(total_pop1))+scale(age65_plus)+BWQS_index, 
#            spdat@data, family=negative.binomial(6.804), listw = ny.wt4, verbose=T,alpha=.05)
# 
# #Step 2c: Pull out these fits and visualize the autocorrelation
# fits <- data.frame(fitted(me.fit))
# spdat$me22 <- fits$vec22 
# spplot(spdat, "me22", at=quantile(spdat$me22, p=seq(0,1,length.out = 7)))
# 
# #Step 2d: Include the fits in our regression model as an additional parameter 
# clean.nb<-glm.nb(deaths_count~offset(log(total_pop1))+scale(age65_plus)+BWQS_index+fitted(me.fit), spdat@data)
# tidy(clean.nb) %>% mutate(estimate_exp = exp(estimate))
# as_tibble(confint(clean.nb), rownames = "vars")%>% mutate_at(vars(2:3), .funs = list(~exp(.)))
# lm.morantest(clean.nb, resfun = residuals, listw=ny.wt4)

#### Appendix
sessioninfo::session_info()
