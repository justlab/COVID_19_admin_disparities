# Compare MTA turnstile usage vs. Google mobility trends 

# Packages ####
library(Just.universal)
library(data.table)
library(fst)
library(ggplot2)
library(zip)
library(here)
library(readr)

here() # current working directory
if(Sys.getenv("MTA_TURNSTILE_DATA_DIR") == ""){ # set up default download location for MTA turnstile data
  mta_dir = here("data/mta_turnstile")
  if(!dir.exists(mta_dir)) dir.create(mta_dir, recursive = TRUE)
  Sys.setenv(MTA_TURNSTILE_DATA_DIR = here("data/mta_turnstile")) 
}
library(MTA.turnstile)

# data will default to a subfolder "data/" within working directory
# unless 1. set by an environment variable:
data.root = Sys.getenv("COVID_DATA")
# or 2. set with an alternative path here:
if (data.root == "") data.root = "data"
if (data.root == "data" & !dir.exists(data.root)) dir.create("data")
print(paste("data being downloaded into directory", dQuote(data.root)))
if(Sys.getenv("MTA_TURNSTILE_DATA_DIR") == "") message("MTA turnstile processing in a temp directory. To cache persistently set an environment variable 'MTA_TURNSTILE_DATA_DIR'. See also ?usethis::edit_r_environ()")

download = function(url, to, f, ...){
  f(download.update.meta(url, file.path(data.root, "downloads"), to),
    ...)}

# Get Google mobility data #### 
google_data <- download(url = "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip",
                        to = paste0("google_mobility_", format(Sys.Date(), "%Y-%m-%d"), ".zip"),
                        f = function(p){
                          # get date CSV file was last updated by Google: typically older than date ZIP file released
                          zl = as.data.table(zip_list(p))
                          googletime = as.POSIXct(zl[filename=="2020_US_Region_Mobility_Report.csv", timestamp])
                          update_date = format(googletime, "%Y-%m-%d") 
                          
                          # subset mobility data to NYC boroughs 
                          zzf = unz(p, "2020_US_Region_Mobility_Report.csv")
                          suppressMessages(gacts <- as.data.table(read_csv(zzf)))
                          gacts = gacts[sub_region_1 == "New York" & 
                                          sub_region_2 %in% c("New York County", "Bronx County", "Kings County",
                                                              "Queens County", "Richmond County"), ]
                          gacts = gacts[, .(sub_region_2, date, 
                                            retail_and_recreation_percent_change_from_baseline,
                                            grocery_and_pharmacy_percent_change_from_baseline,
                                            parks_percent_change_from_baseline,
                                            transit_stations_percent_change_from_baseline,
                                            workplaces_percent_change_from_baseline,
                                            residential_percent_change_from_baseline)]
                          setnames(gacts, c("county", "date", "retail_recre", "groc_phar", "parks", "transit_stns", "workplaces", "residential"))
                          setkey(gacts, county, date)
                          message("The downloaded copy of the Google mobility data was last updated ", update_date)
                          gacts
                        })
# Subset Google data to time range of interest
gacts <- google_data[date >= "2020-01-29" & date <= "2020-04-30", ]
gacts[, min(date)] # 2020-02-15

# Load subway data ####
subway_boro <- relative.subway.usage(2020L, "boro")
subway_boro[, min(date)] # 2020-01-01
subway_boro <- subway_boro[date >= "2020-01-29" & date <= "2020-04-30", ]
subway_boro[, range(date)] # 2020-01-29 to 2020-04-30

# center relative ration on 0
subway_boro[, rel_usage := usage.median.ratio - 1]

# Join MTA and Google ####
boro_cnty = data.table(place = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"), 
                       county = c("Bronx County", "Kings County", "New York County", "Queens County", "Richmond County"))
setkey(boro_cnty, place)
setkey(subway_boro, place)
subway_boro[boro_cnty, county := county]
setkey(subway_boro, date, county)
setkey(gacts, date, county)
comp = gacts[subway_boro]
comp = comp[!is.na(county)]
comp = comp[, .(place, date, Google_Mobility = transit_stns/100, MTA_Turnstiles = rel_usage)]

# table that allows more recent records from one source than the other
comp_long = melt.data.table(comp[date >= as.Date("2020-03-01")], id.vars = c("date", "place"), 
                            variable.name = "source", value.name = "relative_use", 
                            measure.vars = c("MTA_Turnstiles", "Google_Mobility"))

# Plots #### 
first_date = comp_long[, min(date)]
last_date = comp_long[, max(date)]

# Dashed lines for Google, solid for MTA
ggplot(comp_long[place != "Staten Island" & !is.na(relative_use)], 
       aes(x = date, y = relative_use, linetype = source, col = place)) + 
  geom_line() + scale_color_discrete(name = "Borough") + scale_linetype_discrete(name = "Data Source") + 
  xlab("") + ylab("relative to baseline") + 
  scale_x_date(date_breaks = "1 week", limits = c(first_date, last_date)) + ggtitle("Transit Trends")
