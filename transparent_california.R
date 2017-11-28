
#==============================================================================
#   Transparent California
#==============================================================================

# original by Michael Kevane 10/14/2017

# Description: Create tables of descriptive statistics for 
# Salary data for public employees in California
# data is from http://transparentcalifornia.com

#==============================================================================
#   1. Settings, packages, and options
#==============================================================================

# Clear the working space
rm(list = ls())

# Set working directory
#setwd("/Users/wsundstrom/econ_42/data")
#or 
#setwd("C:/Users/wsundstrom/econ_42/data")

# Load useful packages 

# Clear the working space
rm(list = ls())

#install.packages("viridis", "raster", "ggmap", "mapproj", "maps", "maptools", "mapdata", "sp", "ggplot2", "dplyr")

#install.packages(c("maps", "mapdata"))
#install.packages("dplyr")
#install.packages("reshape")
install.packages("ggrepel")



# Load packages
library(plyr)
library(sp) 
library(raster)
library(viridis)
library(leaflet)
library(doBy)
library(dplyr)
library(foreign)
library(gdata)
library(ggplot2)
library(sandwich)
library(stargazer)
library(tidyr)
library(maps)
library(mapdata)
library(mapproj)
library(maptools)
library(ggmap)
library(reshape2)
library(reshape)
library(ggrepel)


# turn off scientific notation except for big numbers
options(scipen = 9)
# function to calculate corrected SEs for regression 
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

#==============================================================================
#   2. Data section
#==============================================================================

### Read data 

# Data input using read.csv
# Note, albany data available for 2011-2016
# many other cities, counties, agencies available

 # Data input using read.csv
  sonoma_c <- read.csv("http://transparentcalifornia.com/export/sonoma-county-2016.csv")
  napa_c <- read.csv("http://transparentcalifornia.com/export/napa-county-2016.csv")
  solano_c <- read.csv("http://transparentcalifornia.com/export/solano-county-2016.csv")
  marin_c <- read.csv("http://transparentcalifornia.com/export/marin-county-2016.csv")
  contra_costa_c <- read.csv("http://transparentcalifornia.com/export/contra-costa-county-2016.csv")
  san_fran_c <- read.csv("https://transparentcalifornia.com/export/san-francisco-2016.csv")
  san_mateo_c <- read.csv("http://transparentcalifornia.com/export/san-mateo-county-2016.csv")
  santa_cruz_c <- read.csv("http://transparentcalifornia.com/export/santa-cruz-county-2016.csv")
  santa_clara_c <- read.csv("http://transparentcalifornia.com/export/santa-clara-county-2016.csv")
  alameda_c <- read.csv("http://transparentcalifornia.com/export/alameda-county-2016.csv")
  
  
  #Average Sonoma County base pay over $75k
  sonoma_c$basepay = tolower(sonoma_c$Base.Pay)
  son75 <- sonoma_c$basepay[sonoma_c$Base.Pay > 75000]
  son75 <- as.numeric(son75)
  son75 <- matrix(data = son75, ncol = 1)
  son_avg <- mean(son75)
  
  #Average Napa County base pay over $75k
  napa_c$basepay = tolower(napa_c$Base.Pay)
  napa75 <- napa_c$basepay[napa_c$Base.Pay >75000]
  napa75 <- as.numeric(napa75)
  napa75 <- matrix(data = napa75, ncol=1)
  napa_avg <- mean(napa75)
  
  #Average Solano County base pay over $75k
  solano_c$basepay = tolower(solano_c$Base.Pay)
  solano75 <- solano_c$basepay[solano_c$Base.Pay >75000]
  solano75 <- as.numeric(solano75)
  solano75 <- matrix(data = solano75, ncol = 1)
  solano_avg <- mean(solano75)
  
  #Average Marin base pay over $75k
  marin_c$basepay = tolower(marin_c$Base.Pay)
  marin75 <- marin_c$basepay[marin_c$Base.Pay > 75000]
  marin75 <- as.numeric(marin75)
  marin75 <- matrix(data = marin75, ncol = 1)
  marin_avg <- mean(marin75)
  
  #Average Contra Costa County base pay over $75k
  contra_costa_c$basepay = tolower(contra_costa_c$Base.Pay)
  cc75 <- contra_costa_c$basepay[contra_costa_c$Base.Pay > 75000]
  cc75 <- as.numeric(cc75)
  cc75 <- matrix(data = cc75, ncol = 1)
  cc_avg <- mean(cc75)
  
  #Average San Francisco County base pay over $75k
  san_fran_c$basepay = tolower(san_fran_c$Base.Pay)
  sf75 <- san_fran_c$basepay[san_fran_c$Base.Pay > 75000]
  sf75 <- as.numeric(sf75)
  sf75 <- matrix(data = sf75, ncol = 1)
  sf_avg <- mean(sf75)
  
  #Average San Mateo County base pay over $75k
  san_mateo_c$basepay = tolower(san_mateo_c$Base.Pay)
  san_mateo75 <- san_mateo_c$basepay[san_mateo_c$Base.Pay > 75000]
  san_mateo75 <- as.numeric(san_mateo75)
  san_mateo75 <- matrix(data = san_mateo75, ncol = 1)
  san_mateo_avg <- mean(san_mateo75)
  
  #Average Santa Cruz County base pay over $75k
  santa_cruz_c$basepay = tolower(santa_cruz_c$Base.Pay)
  cruz75 <- santa_cruz_c$basepay[santa_cruz_c$Base.Pay > 75000]
  cruz75 <- as.numeric(cruz75)
  cruz75 <- matrix(data = cruz75, ncol = 1)
  cruz_avg <- mean(cruz75)
  
  #Average Santa Clara County base pay over $75k
  santa_clara_c$basepay = tolower(santa_clara_c$Base.Pay)
  clara75 <- santa_clara_c$basepay[santa_clara_c$Base.Pay >75000]
  clara75 <- as.numeric(clara75)
  clara75 <- matrix(data = clara75, ncol = 1)
  clara_avg <- mean(clara75)
 
  #Average Alameda County base pay over $75k
  alameda_c$basepay = tolower(alameda_c$Base.Pay)
  alameda75 <- alameda_c$basepay[alameda_c$Base.Pay > 75000]
  alameda75 <- as.numeric(alameda75)
  alameda75 <- matrix(data = alameda75, ncol = 1)
  alameda_avg <- mean(alameda75)
  
  son_avg
  napa_avg
  solano_avg
  marin_avg
  cc_avg
  sf_avg
  san_mateo_avg
  cruz_avg
  clara_avg
  alameda_avg


  geom_polygon(color = "black", fill = NA)  # get the state border back on top

cities <- c("SAN JOSE","Sausalito", "San Diego", "Los Angeles", "Sacramento", "Oakland", "Los Gatos", "Beverly Hills","S. San Francisco")
geocode(cities)

#Inserts values into separate Data Frames (each data frame is one column)
city_avg <- as.data.frame(c(sj_avg, sf_avg, saus_avg, sd_avg, la_avg, sac_avg, oak_avg, lg_avg, bh_avg))
city_names <- as.data.frame(cities)
city_locs <- as.data.frame(geocode(cities))

#Merge data frames horizontally
all_together <- as.data.frame(c(city_names, city_avg, city_locs))

#Rename the columns
colnames(all_together) <- c("CityName", "AveragePay", "Longitude", "Latitude")
all_together <- all_together[, -c(5:7)]
all_together


#Makes Maps
baybox <- make_bbox(lon = all_together$Longitude, lat = all_together$Latitude, f = .1)

ca_map <- get_map(location = baybox, maptype = "roadmap", source = "google")

ggmap(ca_map) + 
  geom_point(data = all_together, mapping = aes(x = Longitude, y = Latitude, size = AveragePay), color = "red")  +   labs(x = 'Longitude', y = 'Latitude') +
  geom_label_repel(data = all_together, aes(x = Longitude, y = Latitude, label = CityName), 
                   fill = "white", box.padding = unit(.4, "lines"),
                   label.padding = unit(.15, "lines"),
                   segment.color = "red", segment.size = 1)

   counties <- c("Napa County", "Solano County", "Marin County", "Contra Costa County", "San Francisco", "San Mateo County", "Santa Cruz County","Santa Clara County", "Alameda County")
  geocode(counties)
  
  #Inserts values into separate Data Frames (each data frame is one column)
  county_avg <- as.data.frame(c( napa_avg, solano_avg, marin_avg, cc_avg, sf_avg, san_mateo_avg, cruz_avg, clara_avg))
  county_names <- as.data.frame(counties)
  county_locs <- as.data.frame(geocode(counties))
  
  #Merge data frames horizontally
  all_together <- as.data.frame(c(county_names, county_locs))

  #Rename the columns
  colnames(all_together) <- c("County Name", "AveragePay", "Longitude", "Latitude")
  all_together <- all_together[, -c(5:7)]
  all_together

  #makes Maps
  cabox <- make_bbox(lon = all_together$Longitude, lat = all_together$Latitude, f = .1)
  
  ca_map <- get_map(location = cabox, maptype = "roadmap", source = "google")
   
  ggmap(ca_map) + geom_point(data = all_together, mapping = aes(x = Longitude, y = Latitude, size = (AveragePay) ) +
    scale_color_gradient(low = "#FF0000", high = "#00FF00")
  
