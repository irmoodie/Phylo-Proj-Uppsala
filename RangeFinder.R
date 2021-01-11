# Moss Phylogeny Project Script - Range Estimates from GBIF and GADM data
# IR Moodie 2021
# This script uses some code from: Blow et al. 2020. bioRxiv 2020.06.06.137828.

# ---- SETUP ENV ----

rm(list=ls()) # clear environment

library(raster) # for dealing with shapefiles
library(countrycode) # easy way to convert iso2c to iso3c and viceversa
library(tidyverse) 

# ---- IMPORT DATASET ----

# the data to be used with this script comes from GADM, and assumes that the following GADM files have been downloaded:
# gadm36_levels_shp (world wide shapefiles at the country level)
# gadm36_XXX_shp (shape files for large countries at the level of province/state)
# XXX = AUS, USA, CAN, IND, RUS, BRA, CHN
# Large countries were split into states as not to bias the range estimates of species found within


data <- read_tsv("gbif.csv") %>% # import dataset downloaded from GBIF
  filter(occurrenceStatus == "PRESENT", # remove absence records
         decimalLatitude != "NA", # only include records with lat/long listed 
         decimalLongitude != "NA") # (this tended to remove any odd placements (eg centre of ocean) from literature/museum records)

data$stateProvince <-str_replace(data$stateProvince, "Калужская обл.", "Kaluga") # Russian provinces have many names it turns out
data$stateProvince <- str_replace(data$stateProvince, "Altay Republic", "Altay") # this sorts out the vast majority of issues for this dataset
data$stateProvince <- str_replace(data$stateProvince, "Sakha-Yakutiya Republic", "Sakha") # there were similar issues in CHINA/BRAZIL/AUSTRALIA, but fixing them did not change range estimates

# ---- LARGE COUNTRY RANGE ---- 
# Split large countries into provinces as not to bias range sizes towards them

largecountries <- c("AUS", "USA", "CAN", "IND", "RUS", "BRA", "CHN") # list large countries by iso3c code

rangeDataset <- data.frame(species = character(),
                           range = double()) # create empty dataset to enter range data into

for (i in seq_along(largecountries)) { # for each country in the list do the following:
  
  shp <- shapefile(paste0("gadm36_", largecountries[i], "_shp/gadm36_", largecountries[i], "_1.shp", sep = "")) # import country shape file that contains province/states
  
  shp$NAME_1 <- str_replace(shp$NAME_1, "QuÃ©bec", "Québec") # fix weird Canada issue
  
  countrydata <- data %>%
    filter(countryCode == countrycode(largecountries[i], origin = "iso3c", destination = "iso2c"), # filter to country of interest
           stateProvince %in% shp$NAME_1) %>% # filter to data with correct prov names only
    group_by(species, stateProvince) %>% # group by unique combinations of species and provs
    count() # add a count (not actually used anymore)
  
  prov <- unique(countrydata$stateProvince) # list provs in dataset
  area <- vector(length = length(prov)) # make empty vector of same length as prov vector
  
  for (i in 1:length(prov)){ # for the number of provs do the following:
    area[i] <- raster::area(shp[which(shp$NAME_1 == paste0(prov[i])),])/1000000 # measure area of prov given in prov vector and write area to area vector
  }
  
  area <- data.frame(prov, area) # match up areas with provinces
  area <- rename(area, stateProvince = prov) # rename variable for joining
  countrydata <- left_join(countrydata, area, by = "stateProvince") # join areas to gbif data
  
  range <- countrydata %>% 
    group_by(species) %>%
    summarise(range = sum(area)) # add up all areas of countires in which a species is located
  
  rangeDataset <- rbind(rangeDataset, range) # add ranges to rangeDataset
  
  rm(area, countrydata, prov, shp, range) # remove temp variables before next loop
  
}

rm(largecountries, i)

# ---- REST OF THE WORLD RANGE ----

shp <- shapefile("gadm36_levels_shp/gadm36_0.shp") #import world shape file

worlddata <- data %>% # filter out countires already done
  filter(countryCode != "AU",
         countryCode != "CA" ,
         countryCode != "IN" ,
         countryCode != "RU" ,
         countryCode != "US",
         countryCode != "BR",
         countryCode != "CN") %>%
  group_by(species, countryCode) %>%
  count()

prov <- unique(worlddata$countryCode, na.rm=TRUE) # get list of countryCodes in dataset
prov <- countrycode(prov, origin = "iso2c", destination = "iso3c") # convert 2 letter codes to 3 letter, as used in the shapefile
prov <- na.omit(prov) # remove NAs (such as ZZ, cannot estimate unknown country)
area <- vector(length = length(prov)) # make empty vector for loop

for (i in 1:length(prov)){ # almost same loop as above
  area[i] <- raster::area(shp[which(shp$GID_0==paste0(prov[i])),])/1000000
}

prov <- countrycode(prov, origin = "iso3c", destination = "iso2c") # convert codes back
area <- data.frame(prov, area) # match up areas with provinces
area <- rename(area, countryCode = prov) # rename varaible for join
worlddata <- left_join(worlddata, area, by = "countryCode") # join areas to gbif data
worlddata <- worlddata %>%
  filter(area != "NA") # remove NAs

range <- worlddata %>%
  group_by(species) %>%
  summarise(range = sum(area)) # add up all areas of countires in which a species is located

rangeDataset <- rbind(rangeDataset, range) # add ranges to rangeDataset

rangeDataset <- rangeDataset %>%
  group_by(species) %>%
  summarise(range = sum(range)) # summarise range dataset to list of species and ranges

rm(area, worlddata, prov, shp, range, i) # remove temp variables

write_csv(rangeDataset,
          file = "rangeDataset.csv")
