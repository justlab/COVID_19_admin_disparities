# Compare MTA turnstile usage vs. Google mobility trends 

message("Running comparison of MTA turnstile data with Google mobility reports...")

# Packages ####
library(Just.universal)
library(RSQLite)
library(data.table)
library(fst)
library(ggplot2)
library(zip)
library(here)
library(readr)
library(ragg)

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
#gacts[, min(date)] # 2020-02-15

# Load subway data ####
subway_boro <- relative.subway.usage(2020L, "boro")
#subway_boro[, min(date)] # 2020-01-01
subway_boro <- subway_boro[date >= "2020-01-29" & date <= "2020-04-30", ]
#subway_boro[, range(date)] # 2020-01-29 to 2020-04-30

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
comp = comp[, .(place, date, `Google Mobility` = (transit_stns/100)+1, `MTA Turnstiles` = usage.median.ratio)]

# table that allows more recent records from one source than the other
comp_long = melt.data.table(comp[date >= as.Date("2020-03-01")], id.vars = c("date", "place"), 
                            variable.name = "source", value.name = "relative_use", 
                            measure.vars = c("MTA Turnstiles", "Google Mobility"))

# Plots #### 
first_date = comp_long[, min(date)]
last_date = comp_long[, max(date)]

# Supplemental Figure 9
mobplot <- ggplot(comp_long[!is.na(relative_use)], 
       aes(x = date, y = relative_use, linetype = source, col = place)) + 
  geom_line() + scale_color_discrete(name = "Borough") + scale_linetype_discrete(name = "Data Source") + 
  xlab("Date") + 
  scale_y_continuous("Relative to Baseline (%)", breaks = c(0.25, 0.5, 0.75, 1), 
                     labels = scales::percent) + 
  scale_x_date(date_minor_breaks = "1 week", limits = c(first_date, last_date + 1), 
               breaks = as.Date(c("2020-03-01", "2020-04-01", "2020-05-01")),
               labels = scales::date_format("%b")) + 
  theme_bw(base_size = 16) + 
  theme(legend.title = element_text(face = "bold", size = 8), legend.position = c(0.68, 0.75),
        legend.spacing = unit(1, "points"), legend.box="horizontal") 

if(export.figs) {
  if(vector.figs){
    fig_outpath = file.path(fig.path, paste0("sfig9_mobility_", Sys.Date(),".svg"))
    svg(filename = fig_outpath, width = 1.3*6, height = 1.3*4.25)
  } else {
    plotres = 300
    fig_outpath = file.path(fig.path, paste0("sfig9_mobility_", Sys.Date(),".png"))
    message("Writing ", fig_outpath)
    agg_png(filename = fig_outpath, 
            width = plotres*6, height = plotres*4, scaling = 3.3)
  }
  print(mobplot)
  dev.off()
  if(vector.figs){
    pdf_outpath = paste0(str_sub(fig_outpath,1,-5), ".pdf")
    message("Writing ", pdf_outpath)
    system(paste0("rsvg-convert -f pdf -o ", pdf_outpath, " ", fig_outpath))
    unlink(fig_outpath)
  } 
}
