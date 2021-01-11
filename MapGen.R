# Moss Phylogeny Project Script - Map Generation
# IR Moodie 2021
# Generates Maps of species ranges
# Uses code from the Aberdeen Study Group

# ---- SETUP ENV ----

rm(list=ls()) # clear environment
library(maps)
library(mapdata)
library(viridis) # colour mapping set
library(tidyverse) # data handling and plotting

# ---- GBIF DATA HANDLING ----

coords <- read_tsv("gbif.csv") %>% # import dataset downloaded from GBIF (as above)
  filter(occurrenceStatus == "PRESENT", # remove absence records
         decimalLatitude != "NA", # only include records with lat/long listed 
         decimalLongitude != "NA") %>%
  filter(countryCode != "NA") %>% # as anything with an NA wouldn't be in the dataset above
  select(species, decimalLatitude, decimalLongitude) %>%
  mutate(species = str_replace(species, " ", "_")) # subset dataset to just be latlong# (this tended to remove any odd placements (eg centre of ocean) from literature/museum records)

specieslist <- coords %>%
  pull(species) %>%
  unique() # make a list of species in dataset

# ---- MAPPING ----

map_world <- borders(database = "world", colour = "gray50") # makes blank map to plot on top of

map_all <- ggplot() + map_world +  # plot the map
  geom_point(data = coords,  # specify the data for geom_point()
             aes(x = decimalLongitude,  # specify the x axis as longitude
                 y = decimalLatitude,  # specify the y axis as latitude
                 colour = species),  # colour the points based on species name
             alpha = 0.1,  # set point opacity 
             size = 1) +  # set point size to 1
  theme_classic() +  # Remove gridlines and shading inside the plot
  ylab(expression("Latitude ("*degree*")" )) +  # Add a smarter x axis label
  xlab(expression("Longitude ("*degree*")" )) +  # Add a smarter y axis label
  theme(legend.position = "none",  # Move the legend to below the plot
        legend.title = element_blank())  # Remove the legend title

map_all # may take a while as it maps out 350K points

for (i in seq_along(specieslist)) { # this loop will plot a map for each species in specieslist, and make a png and store it in the main directory
  mapplot <- ggplot() + map_world +  # plot the map
    geom_point(data = subset(coords, coords$species == specieslist[i]),  # specify the data for geom_point()
               aes(x = decimalLongitude,  # specify the x axis as longitude
                   y = decimalLatitude,  # specify the y axis as latitude
                   colour = species),  # colour the points based on species name
               alpha = 0.3,  # set point opacity
               size = 1) +  # set point size to 1
    theme_classic() +  # Remove gridlines and shading inside the plot
    ylab(expression("Latitude ("*degree*")" )) +  # Add a smarter x axis label
    xlab(expression("Longitude ("*degree*")" )) +  # Add a smarter y axis label
    theme(legend.position = "none",  # Move the legend to below the plot
          legend.title = element_blank()) +  # Remove the legend title
    ggtitle(paste(specieslist[i]), "All")
  
  ggsave(maplot, file=paste(specieslist[i], ".png", sep=''),
         dpi= "retina",
         width = 10,
         height = 5)  # export each map to working directory (it keeps breaking if I send it elsewhere)
  
}