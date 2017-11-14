
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

san_jose_2016 <- read.csv("http://transparentcalifornia.com/export/san-jose-2016.csv")
s_san_fran_2016 <- read.csv("http://transparentcalifornia.com/export/south-san-francisco-2016.csv")
sausalito_2016 <- read.csv("http://transparentcalifornia.com/export/sausalito-2016.csv")
san_diego_2016 <- read.csv("http://transparentcalifornia.com/export/san-diego-2016.csv")
los_angeles_2016 <- read.csv("http://transparentcalifornia.com/export/los-angeles-2016.csv")
sacramento_2016 <- read.csv("http://transparentcalifornia.com/export/sacramento-2016.csv")
oakland_2016 <- read.csv("http://transparentcalifornia.com/export/oakland-2016.csv")
los_gatos_2016 <- read.csv("http://transparentcalifornia.com/export/los-gatos-2016.csv")
beverly_hills_2016 <- read.csv("http://transparentcalifornia.com/export/beverly-hills-2016.csv")

#Average SJ base pay over $75k
san_jose_2016$basepay = tolower(san_jose_2016$Base.Pay)
sj75 <- san_jose_2016$basepay[san_jose_2016$Base.Pay > 75000]
sj75 <- as.numeric(sj75)
sj75 <- matrix(data = sj75, ncol = 1)
sj_avg <- mean(sj75)

#Average S. San Francisco base pay over $75k
s_san_fran_2016$basepay = tolower(s_san_fran_2016$Base.Pay)
sf75 <- s_san_fran_2016$basepay[s_san_fran_2016$Base.Pay >75000]
sf75 <- as.numeric(sf75)
sf75 <- matrix(data = sf75, ncol=1)
sf_avg <- mean(sf75)

#Average Sausalito base pay over $75k
sausalito_2016$basepay = tolower(sausalito_2016$Base.Pay)
saus75 <- sausalito_2016$basepay[sausalito_2016$Base.Pay >75000]
saus75 <- as.numeric(saus75)
saus75 <- matrix(data = saus75, ncol = 1)
saus_avg <- mean(saus75)

#Average San Diego base pay over $75k
san_diego_2016$basepay = tolower(san_diego_2016$Base.Pay)
sd75 <- san_diego_2016$basepay[san_diego_2016$Base.Pay > 75000]
sd75 <- as.numeric(sd75)
sd75 <- matrix(data = sd75, ncol = 1)
sd_avg <- mean(sd75)

#Average Los Angeles base pay over $75k
los_angeles_2016$basepay = tolower(los_angeles_2016$Base.Pay)
la75 <- los_angeles_2016$basepay[los_angeles_2016$Base.Pay > 75000]
la75 <- as.numeric(la75)
la75 <- matrix(data = la75, ncol = 1)
la_avg <- mean(la75)

#Average Sacramento base pay over $75k
sacramento_2016$basepay = tolower(sacramento_2016$Base.Pay)
sac75 <- sacramento_2016$basepay[sacramento_2016$Base.Pay > 75000]
sac75 <- as.numeric(sac75)
sac75 <- matrix(data = sac75, ncol = 1)
sac_avg <- mean(sac75)

#Average Oakland base pay over $75k
oakland_2016$basepay = tolower(oakland_2016$Base.Pay)
oak75 <- oakland_2016$basepay[oakland_2016$Base.Pay > 75000]
oak75 <- as.numeric(oak75)
oak75 <- matrix(data = oak75, ncol = 1)
oak_avg <- mean(oak75)

#Average Los Gatos base pay over $75k
los_gatos_2016$basepay = tolower(los_gatos_2016$Base.Pay)
lg75 <- los_gatos_2016$basepay[los_gatos_2016$Base.Pay > 75000]
lg75 <- as.numeric(lg75)
lg75 <- matrix(data = lg75, ncol = 1)
lg_avg <- mean(lg75)

#Average Beverly Hills base pay over $75k
beverly_hills_2016$basepay = tolower(beverly_hills_2016$Base.Pay)
bh75 <- beverly_hills_2016$basepay[beverly_hills_2016$Base.Pay >75000]
bh75 <- as.numeric(bh75)
bh75 <- matrix(data = bh75, ncol = 1)
bh_avg <- mean(bh75)

sj_avg
sf_avg
saus_avg
sd_avg
la_avg
sac_avg
oak_avg
lg_avg
bh_avg


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
