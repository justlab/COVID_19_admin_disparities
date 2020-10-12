# Use distribution of tract-scale cariables for ZCTA-scale outcomes

library(httr)
library(jsonlite)

#### Census variables #### 
# population, median income, unemployed, unemployed, commute, essential workers
options(tigris_use_cache = TRUE)
pm(
get_pop_tracts <- function() {
   ACS_Data <- get_acs(geography = "tract", 
                       geometry = TRUE,
                       state = "36", 
                       variables = c(medincome = "B19013_001",
                                     total_pop1 = "B01003_001",
                                     #fpl_100 = "B06012_002", 
                                     #fpl_100to150 = "B06012_003",
                                     #median_rent = "B25031_001",
                                     total_hholds1 = "B22003_001",
                                     #hholds_snap = "B22003_002",
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
                       survey = "acs5",
                       cache_table = TRUE)
   
   ACS_Data %>% #only pull out the estimates and cleaning variable names
     filter(substr(GEOID,1,5) %in% NYC_boro_county_match$fips) %>% # Tracts in NYC counties
     dplyr::select(-NAME) %>%
     dplyr::select(GEOID, !ends_with("M")) %>%
     rename_at(vars(ends_with("E")), .funs = list(~str_sub(., end = -2)))
})
# download NY tract data
acs_tracts = get_pop_tracts()
dim(acs_tracts) # 2167, 39

# derived variables
acs_tracts2 <- acs_tracts %>%
  mutate_at(vars(ends_with("_commute")), ~round((./total_commute1)*100, 2)) %>% #proportion of people relying on a given mode of transit
  mutate_at(vars(ends_with("_raceethnic")), ~round((./total_pop1)*100, 2)) %>% #proportion of ppl reporting a given race/ethncity 
  mutate(not_insured = round(((under19_noinsurance + age19_34_noinsurance + age35_64_noinsurance + age65plus_noinsurance) / total_pop1)*100, 2), #proportion uninsured
         unemployed = round((unemployed/over16total_industry1)*100, 2), #proportion unemployed
         not_quarantined_jobs = round(((ag_industry+(construct_industry*.25)+wholesaletrade_industry+ #an estimate of who is still leaving the house for work based on industry
                                          (edu_health_socasst_industry*.5)+transpo_and_utilities_industry)/over16total_industry1)*100, 2)) %>%
  dplyr::select(-ends_with("_noinsurance"), -ends_with("_industry"))
dim(acs_tracts2) # 2167, 24

# commute times for essential workers
pm(fst = T,     
get_acs_tracts_commute <- function() {
  ACS_EssentialWrkr_Commute <- get_acs(geography = "tract", 
                                      state = "36", 
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
                                      survey = "acs5",
                                      cache_table = TRUE)
  
  ACS_EssentialWrkr_Commute %>% #clean data and aggregate 
    dplyr::select(-ends_with("M"), -NAME) %>%
    filter(substr(GEOID,1,5) %in% NYC_boro_county_match$fips) %>% # Tracts in NYC counties
    mutate_at(vars(starts_with("ed_hlthcare")), ~round(./2), 0) %>% #maintain same proportions as estimated nonquarintined jobs
    mutate_at(vars(starts_with("construct")), ~round(./4), 0) %>%
    mutate(essentialworker_drove = rowSums(dplyr::select(., contains("car1_commute"))), 
           essentialworker_pubtrans = rowSums(dplyr::select(., contains("pubtrans")))) %>%
    dplyr::select(GEOID, essentialworker_drove, essentialworker_pubtrans)
})
# download commute mode by tract for essential workers
acs_tracts_commute1 = get_acs_tracts_commute()
dim(acs_tracts_commute1) # 2167, 3


#### Identify the number of supermarkets/grocery stores per area ####
# Need to geocode grocers; previously used their ZIP codes
food_retail_filtered <- food_retail %>% 
  filter(County %in% NYC_boro_county_match$County) %>% 
  filter(str_detect(`Establishment Type`, "J") & str_detect(`Establishment Type`, "A") & str_detect(`Establishment Type`, "C") &
           !str_detect(`Establishment Type`, "H")) %>%
  filter(!str_detect(`Entity Name`, non_supermarket_strings) & !str_detect(`DBA Name`, non_supermarket_strings)) %>%
  filter(`Square Footage`>=4500) %>%
  mutate(zcta = as.character(str_extract(Location, "[:digit:]{5}"))) %>% 
  mutate(Address = paste(paste(`Street Number`, `Street Name`), City, State, zcta, sep = ","))
dim(food_retail_filtered) # 1037,16

# Geocode grocers, using a cached version if available to make analysis reproducible
# The geocoding service may be updated in the future and give different results
if(file.exists("data/grocers_geocode_2020-10-02.csv")){
  gctable <- read.csv("data/grocers_geocode_2020-10-02.csv")
  failed = which(gctable$score == 0)
  message("Loaded cached geocoded grocers: ", nrow(food_retail_filtered)-length(failed), "/", nrow(food_retail_filtered), " have coordinates.")
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
  write.csv(gctable, paste0("data/grocers_geocode_", Sys.Date(), ".csv"))
}

# Count by tract
gctable = filter(gctable, score > 0)
grocerSF = st_as_sf(gctable, coords = c("location.x", "location.y"), crs = 26918) %>% st_transform(crs = 2263)
tractSF = acs_tracts2[, "GEOID"] %>% st_transform(crs = 2263)
tract_grocers = suppressWarnings(st_intersection(tractSF, grocerSF)) %>%
  st_set_geometry(., NULL) %>%
  group_by(GEOID) %>%
  summarise(grocers = n_distinct(`address`))
nrow(tract_grocers) # 754
sum(tract_grocers$grocers) # 997

#### Calculate the residential area per ZCTA ####
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
res_bldg_tract <- st_intersection(Res_Bldg_Footprints2, tractSF)
res_bldg_tract_sum <- st_set_geometry(res_bldg_tract, NULL) %>%
  group_by(GEOID) %>%
  summarise(total_res_volume_tract = sum(res_volume, na.rm = TRUE))
nrow(res_bldg_tract_sum) # 2125

#### Create data frames of all above information ####
tract_vars <- tractSF %>% # uses local CRS
  left_join(., st_set_geometry(acs_tracts2, NULL), by = "GEOID") %>%
  left_join(., acs_tracts_commute1, by = "GEOID") %>%
  left_join(., res_bldg_tract_sum, by = "GEOID") %>%
  left_join(., tract_grocers, by = "GEOID") %>%
  mutate(pop_density = as.numeric(total_pop1/st_area(geometry)),
         avg_hhold_size = round((total_pop1/total_hholds1), 2),
         #pos_per_100000 = (Positive/total_pop1)*100000, 
         #testing_ratio = (total_tests/total_pop1), 
         res_vol_zctadensity = as.numeric(total_res_volume_tract/st_area(geometry)), 
         res_vol_popdensity = as.numeric(total_pop1/total_res_volume_tract),
         pubtrans_ferrysubway_commute = pubtrans_subway_commute + pubtrans_ferry_commute,
         grocers = replace_na(grocers, 0),
         grocers_per_1000 = (grocers/total_pop1)*1000,
         #pos_per_100000 = round(pos_per_100000, 0), 
         valid_var = "0",
         didnot_workhome_commute = 100 - workhome_commute,
         one_over_grocers_per_1000 = if_else(is.infinite(1/grocers_per_1000), 0, 1/grocers_per_1000),
         one_over_medincome = 1/medincome) %>% 
  dplyr::select(-pubtrans_subway_commute, -pubtrans_ferry_commute) %>%
  mutate_at(vars(starts_with("essentialworker_")), ~round((./over16total_industry1)*100, 2))
  
View(tract_vars)

#### Tract -> ZCTA weighted assignment ####

# prepare tract->zip weights by summing RES_RATIO when multiple ZIPs combine to a single MODZCTA
modzcta_to_zcta_chr <- data.frame(ZCTA = as.character(modzcta_to_zcta$ZCTA), 
                                  MODZCTA = as.character(modzcta_to_zcta$MODZCTA))
tract_to_modzcta <- tract_to_zip %>% 
  left_join(., modzcta_to_zcta_chr, by = c("ZIP" = "ZCTA")) %>%
  filter(!is.na(MODZCTA)) %>%
  group_by(MODZCTA, TRACT) %>%
  dplyr::summarize(SUM_RES_RATIO = sum(RES_RATIO), .groups = "drop") 

    # length(unique(tract_to_modzcta$MODZCTA)) #  177
    # length(unique(tract_to_modzcta$TRACT))   # 2188
    # length(unique(tractSF$GEOID))            # 2167

# check res_ratio against tracts with no population
tract_modzcta_pop <- tract_to_modzcta %>%
  left_join(acs_tracts2, by = c("TRACT" = "GEOID")) %>%
  dplyr::select(MODZCTA, TRACT, total_pop1, SUM_RES_RATIO)

# checking for tracts with no population but res_ratio > 0
tract_modzcta_pop %>% filter(MODZCTA == "11697") # still has a zero pop tract with res_ratio = 1
tract_modzcta_pop %>% filter(total_pop1 == 0 & SUM_RES_RATIO > 0) %>% nrow() # 30
# set tracts with no population to have a res_ratio of 0
tract_modzcta_pop2 <- tract_modzcta_pop %>% 
  mutate(SUM_RES_RATIO = case_when(
    total_pop1 == 0 & SUM_RES_RATIO > 0 ~ 0,
    TRUE                                ~ SUM_RES_RATIO ))

# check to make sure no MODZCTA have all zeroes for all tract res_ratios
tract_modzcta_pop2 %>% group_by(MODZCTA) %>% filter(all(SUM_RES_RATIO == 0)) %>% nrow() # 0
tract_modzcta_pop2 %>% filter(MODZCTA == "10020") # this MODZCTA doesn't exist, but the ZIP did and had all zeroes
tract_to_modzcta2 <- dplyr::select(tract_modzcta_pop2, -total_pop1)


#### model 2 - select median tract value by ZCTA ####

# get weighted median tract values of selected variables by MODZCTA
SES_zcta_median <- tract_vars %>% 
  st_set_geometry(., NULL) %>%
  left_join(., tract_to_modzcta2, by = c("GEOID" = "TRACT")) %>% 
  filter(!is.na(MODZCTA)) %>% 
  group_by(MODZCTA) %>%
  summarise(essentialworker_drove_median = Hmisc::wtd.quantile(essentialworker_drove, SUM_RES_RATIO)[[3]],
            essentialworker_pubtrans_median = Hmisc::wtd.quantile(essentialworker_pubtrans, SUM_RES_RATIO)[[3]],
            not_quarantined_jobs_median = Hmisc::wtd.quantile(not_quarantined_jobs, SUM_RES_RATIO)[[3]],
            didnot_workhome_commute_median = Hmisc::wtd.quantile(didnot_workhome_commute, SUM_RES_RATIO)[[3]],
            not_insured_median = Hmisc::wtd.quantile(not_insured, SUM_RES_RATIO)[[3]],
            one_over_medincome_median = Hmisc::wtd.quantile(one_over_medincome, SUM_RES_RATIO)[[3]],
            unemployed_median = Hmisc::wtd.quantile(unemployed, SUM_RES_RATIO)[[3]],
            avg_hhold_size_median = Hmisc::wtd.quantile(avg_hhold_size, SUM_RES_RATIO)[[3]],
            res_vol_popdensity_median = Hmisc::wtd.quantile(res_vol_popdensity, SUM_RES_RATIO)[[3]],
            one_over_grocers_per_1000_median = Hmisc::wtd.quantile(one_over_grocers_per_1000, SUM_RES_RATIO)[[3]]) 
SES_zcta_median  

# join SES to testing data: positive/100k and testing_ratio
SES_zcta_median_testing <- ZCTA_ACS_COVID %>%
  dplyr::select(zcta, pos_per_100000, testing_ratio) %>%
  left_join(SES_zcta_median, by = c("zcta" = "MODZCTA"))

#Step 2a: Examine relationships between explanatory variables to make sure nothing >0.9 correlation, as this could bias BWQS
Cors_SESVars_median <- cor(SES_zcta_median %>% dplyr::select(-MODZCTA), method = "kendall") %>% as.data.frame()
Cors_SESVars_median$var1 <- row.names(Cors_SESVars_median)
Cors_SESVars_median2 <- gather(data = Cors_SESVars_median, key = "var2", value = "correlation", -var1) %>%
  filter(var1 != var2)
Cors_SESVars_median2 %>% filter(is.na(correlation)) %>% nrow() # 0
max(Cors_SESVars_median2$correlation, na.rm = T) # 0.6

## Step 2b: Examine Univariable kendall associations for all selected variables with the outcome  
SES_vars_median = names(SES_zcta_median)[names(SES_zcta_median) != "MODZCTA"]
bind_cols(Variables = SES_vars_median,
          
          SES_zcta_median_testing %>%
            summarise_at(vars(all_of(SES_vars_median)), list(~cor(., pos_per_100000, method = "kendall"))) %>%
            t() %>%
            as_tibble(),
          
          SES_zcta_median_testing %>%
            summarise_at(vars(all_of(SES_vars_median)),
                         list(~cor.test(., pos_per_100000, method = "kendall")$p.value)) %>%
            t() %>%
            as_tibble()) %>%
  
  mutate(`Correlation (Tau)` = round(V1...2, 3),
         `p value` = as.character(ifelse(V1...3 < 0.0001, "< 0.0001", round(V1...3, 3))),) %>%
  dplyr::select(-V1...2, -V1...3) 


#Step 3: Prepare data for BWQS and pass to stan for model fitting 
y <- SES_zcta_median_testing$pos_per_100000
X <- SES_zcta_median_testing %>% dplyr::select(all_of(SES_vars_median))
K <- SES_zcta_median_testing$testing_ratio
for (vname in SES_vars_median){
  X[[vname]] <- ecdf(X[[vname]])(X[[vname]]) * 10}
data_median <- as.data.frame(cbind(y,X)) # Aggregate data in a data.frame

data_median_list = list(N  = NROW(data_median), 
                 C  = NCOL(X), 
                 K  = NCOL(K), 
                 XC = cbind(as.matrix(X)), 
                 XK = cbind(K), 
                 Dalp = rep(1,length(SES_vars_median)), 
                 y = as.vector(data_median$y))

#pm( # do not use pairmemo while debugging
  stan.model_median <- function(){
    stan(file = BWQS_stan_model,
         data = data_median_list, chains = 1,
         warmup = 2500, iter = 20000, cores = 16,
         thin = 10, refresh = 0, algorithm = "NUTS",
         seed = 1234, control = list(max_treedepth = 20,
                                     adapt_delta = 0.999999999999999))
    }#)
system.time(m2_median <- stan.model_median()) # 209 seconds, ran single core
extract_waic(m2_median)

m2vars = c("phi", "beta0", "beta1", "delta1", SES_vars_median)
parameters_to_drop <- c("phi", "delta1", "beta0", "beta1")
number_of_coefficients <- length(SES_vars_median) + 4

BWQS_params_m2 <- bind_cols(as_tibble(summary(m2_median)$summary[1:number_of_coefficients,c(1,4,8)]), 
                            label = m2vars)

BWQS_fits_m2 <- BWQS_params_m2 %>%
  rename(lower = "2.5%", upper = "97.5%") %>%
  filter(!label %in% parameters_to_drop) %>%
  arrange(desc(mean)) %>%
  mutate(group = factor(if_else(str_detect(label,"one_over_medincome")|str_detect(label,"not_insured")|str_detect(label,"unemployed"), "Finances &\nAccess to care",
                                if_else(str_detect(label,"one_over_grocers_per_1000"), "Food\nAccess",
                                        if_else(str_detect(label, "essential")|str_detect(label,"not_quarantined_jobs")|str_detect(label,"didnot_workhome_commute"), "Commuting and\nEssential Work",
                                                if_else(str_detect(label,"avg_hhold_size")|str_detect(label,"res_vol_popdensity"), "Population\nDensity", "Unmatched")))),
                        levels = c("Commuting and\nEssential Work", "Finances &\nAccess to care", "Population\nDensity", "Food\nAccess")))

# labels1 already in memory

m2_fig2 <- ggplot(data=BWQS_fits_m2, aes(x= reorder(label, mean), y=mean, ymin=lower, ymax=upper)) +
  geom_pointrange() + 
  coord_flip() + 
  xlab("") + 
  ylab("Mean (95% credible intervals)") +
  scale_x_discrete(labels = labels1) + 
  theme_set(theme_bw(base_size = 18)) +
  facet_grid(group~., scales = "free", space = "free") +
  theme(strip.text.x = element_text(size = 14))
m2_fig2

m2_Cors_SESVars_quantiled <- cor(X, method = "kendall")  
m2_Cors_SESVars_quantiled1 <- as.data.frame(m2_Cors_SESVars_quantiled)
m2_Cors_SESVars_quantiled1$var1 <- row.names(m2_Cors_SESVars_quantiled1)
m2_Cors_SESVars_quantiled2 <- gather(data = m2_Cors_SESVars_quantiled1, key = "var2", value = "correlation", -var1) %>%
  filter(var1 != var2)
m2_Cors_SESVars_quantiled2

#Step 4: Use the variable-specific weight on the decile quantile splits to create a 10 point ZCTA-level infection risk score  

BWQS_weights_m2 <- as.numeric(summary(m2_median)$summary[5:number_of_coefficients,c(1)])

ZCTA_ACS_COVID2_m2 <- X*BWQS_weights_m2[col(SES_zcta_median_testing)] 

BWQS_DF_m2 <- ZCTA_ACS_COVID2_m2 %>% 
  dplyr::mutate(BWQS_index = rowSums(.)) %>% 
  dplyr::select(BWQS_index) 

BWQS_predicted_infection_median_testing_m2 = 
  exp(BWQS_params_m2[BWQS_params_m2$label == "beta0", ]$mean + 
      (BWQS_params_m2[BWQS_params_m2$label == "beta1", ]$mean * BWQS_DF_m2) + 
      (BWQS_params_m2[BWQS_params_m2$label == "delta1", ]$mean * median(K)))
colnames(BWQS_predicted_infection_median_testing_m2) <- "predicted"
BWQS_predicted_infection_median_testing_m2

# Visualize the relationship between BWQS index and infection rate
m2_scatter_data = data.frame(BWQS_DF_m2, y, BWQS_predicted_infection_median_testing_m2)
BWQS_scatter_m2 <- ggplot(data = m2_scatter_data, aes(BWQS_index, y)) + 
  geom_point() + 
  geom_line(aes(y = predicted)) + 
  scale_x_continuous("BWQS infection risk index") + 
  scale_y_continuous("Infections per 100,000", label=comma)
BWQS_scatter_m2 <- ggExtra::ggMarginal(BWQS_scatter_m2, type = "histogram", 
                                       xparams = list(binwidth = 1), yparams = list(binwidth = 200))
BWQS_scatter_m2
if(export.figs) {
  png(filename = here("figures", paste0("fig1_m2_", Sys.Date(), ".png")), width = 96*5, height = 96*5)
  print(BWQS_scatter_m2)
  dev.off()
}

# SF object with median variables and BWQS index from model 2
all(ZCTA_ACS_COVID_shp$zcta == SES_zcta_median$MODZCTA) # confirming matching zcta order before bind_cols
ZCTA_BWQS_COVID_shp_m2 <- ZCTA_ACS_COVID_shp %>% dplyr::select(zcta, geometry) %>% 
  left_join(SES_zcta_median, by = c("zcta" = "MODZCTA")) %>% 
  bind_cols(BWQS_DF_m2)

#Step 5: Visualize the spatial distribution of ZCTA-level infection risk scores 

# reproject to WGS84 to be compatible with scalebar
ZCTA_BWQS_COVID_shp_m2 <- st_transform(ZCTA_BWQS_COVID_shp_m2, 4326)

# BWQS infection risk map by ZCTA, median tract model (version of Figure 3)
fig3_m2 <- ggplot(ZCTA_BWQS_COVID_shp_m2) + 
  geom_sf(aes(fill = BWQS_index), lwd = 0.2) + 
  scalebar(ZCTA_BWQS_COVID_shp_m2, dist = 5, dist_unit = "km", 
           transform = TRUE, model = "WGS84", 
           st.size = 2.3, height = 0.015, border.size = 0.5,
           anchor = c(x = -73.71, y = 40.51)) + 
  scale_fill_gradientn(colours=brewer_pal("YlGnBu", type = "seq")(7)) + 
  theme_bw(base_size = 5) + 
  labs(fill = "BWQS infection risk index - Median Tract Values") +
  theme(legend.title = element_text(face = "bold", size = 7), 
        panel.background = element_rect(fill = "#dedede"), 
        legend.background = element_rect(fill = "transparent"),
        legend.position = c(0.343, 0.80), # adjust X for longer legend title
        legend.text = element_text(size = 6),
        legend.key.size = unit(1.1, "lines"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())
fig3_m2
if(export.figs) ggsave(plot = fig3_m2, filename = here("figures", paste0("fig3","_m2_",Sys.Date(),".png")), 
                       dpi = 600, device = "png", width = 4, height = 3.7)

# Difference map between original ZCTA-wide model and model 2 using tract medians

ZCTA_BWQS_COVID_shp_m2$m2_m1_diff <- ZCTA_BWQS_COVID_shp_m2$BWQS_index - ZCTA_BWQS_COVID_shp$BWQS_index
library(colorspace)
fig_m2m1_diff <- ggplot(ZCTA_BWQS_COVID_shp_m2) + 
  geom_sf(aes(fill = m2_m1_diff), lwd = 0.2) + 
  scalebar(ZCTA_BWQS_COVID_shp_m2, dist = 5, dist_unit = "km", 
           transform = TRUE, model = "WGS84", 
           st.size = 2.3, height = 0.015, border.size = 0.5,
           anchor = c(x = -73.71, y = 40.51)) + 
  scale_fill_continuous_divergingx(palette = "PiYG", mid = 0) + 
  theme_bw(base_size = 5) + 
  labs(fill = "BWQS infection risk index: Median Tract Model - ZCTA Model") +
  theme(legend.title = element_text(face = "bold", size = 7), 
        panel.background = element_rect(fill = "#dedede"), 
        legend.background = element_rect(fill = "transparent"),
        legend.position = c(0.425, 0.80), # adjust X for longer legend title
        legend.text = element_text(size = 6),
        legend.key.size = unit(1.1, "lines"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())
if(export.figs) ggsave(plot = fig_m2m1_diff, filename = here("figures", paste0("m2m1_diff_",Sys.Date(),".png")), 
                       dpi = 600, device = "png", width = 4, height = 3.7)



