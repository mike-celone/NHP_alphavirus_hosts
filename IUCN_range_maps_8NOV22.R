#############################
# Mapping NHP species ranges#
#############################

# Set working directory
setwd("~/Dropbox/MAYV_NHP_prediction")

# Necessary packages
map_packages <- c("sp", "rgeos", "maps", "mapdata", "maptools", "geosphere", "ggplot2", "viridis", "cowplot",
                  "ggthemes", "reshape2", "RColorBrewer", "rgdal", "dplyr", "tidyr", "stringr", "mapproj", "broom",
                  "raster","rmapshaper")

sapply(map_packages, library, character.only = T)

###################
### Import data ###
###################

# Read in the shape file
load(file = "./mammal_iucn.RData")
mammal_iucn <- as(mammal_iucn, Class = "Spatial")

# Data for world map
world_map <- map_data("world")

# Create a list of species names to filter the IUCN data (N=37 NHPs)
allnames <-  c("Alouatta belzebul",
               "Alouatta caraya",
               "Alouatta pigra",
               "Alouatta seniculus",
               "Ateles marginatus",
               "Cercopithecus ascanius",
               "Cercopithecus denti", 
               "Cercopithecus pogonias",
               "Chlorocebus aethiops",
               "Chlorocebus pygerythrus",
               "Colobus guereza",
               "Erythrocebus patas",
               "Eulemur fulvus",
               "Galago senegalensis",
               "Gorilla gorilla",
               "Lophocebus albigena",
               "Macaca fascicularis", 
               "Macaca mulatta",
               "Mandrillus sphinx",
               "Pan troglodytes",
               "Papio anubis",
               "Papio hamadryas",
               "Papio ursinus", 
               "Pithecia pithecia",
               "Saguinus midas",
               "Sapajus apella",
               "Cercopithecus mitis",
               "Papio papio",
               "Pongo pygmaeus",
               "Saimiri sciureus", #Taxonomy issues
               "Mico argentatus",
               "Mico leucippe",
               "Sapajus libidinosus",
               "Cacajao calvus",
               "Lagothrix	poeppigii", #Taxonomy issues
               "Plecturocebus brunneus",
               "Sapajus xanthosternos")

# Filter mammterr down to just binomial names
all.binomial <- mammal_iucn$binomial

# Check to see if allnames are in the binomial names
unique.allnames <- unique(allnames)

# Which rows of the data are the species I care about
keep <- list()
for(i in 1:length(unique.allnames)){
  keep[[i]] <- which(all.binomial == unique.allnames[i])
}
x <- keep[[1]]
for(i in 2:length(unique.allnames)){
  x <- c(x, keep[[i]])
}
keep.species <- data.frame(x = x, species = mammal_iucn[x,]$binomial)
myspecies.distr <- mammal_iucn[x,]

#################################################################
### Species distribution plots - number of overlapping ranges ###
#################################################################

# Sample points across the species distributions on a regular grid
# n increases resolution, but also computation time
spp.points <- spsample(myspecies.distr, n = 50000, type = "regular")

# Summarize the species distributions that overlap the chosen points
spp.pointsInPolygons <- sp::over(x = spp.points, y = myspecies.distr, returnList = TRUE)

# Count the number of intersections
spp.counting <- lapply(spp.pointsInPolygons, FUN = function(x) nrow(x))
spp.over.df <- data.frame("point" = rownames(t(do.call("cbind", spp.counting))), 
                          "count" = t(do.call("cbind", spp.counting)), 
                          "polygon" = paste(spp.pointsInPolygons))

# Summarize counts in a data frame
spp.points.df <- as.data.frame(spp.points)
spp.points.df$count <- spp.over.df$count

# Combine world map data and species range data into one object
sf_data <- list(world = world_map, overlap = spp.points.df)

# Plot map of species range overlap counts for all species
ggplot() +
  geom_polygon(data = sf_data$world, aes(x = long, y = lat, group = group),
               color = NA,
               fill = "grey90") +
  geom_tile(data = sf_data$overlap, aes(x = x1, y = x2, fill = count)) +
  scale_fill_viridis(option = "H", name = "Species") +
  theme_map(base_size = 14) +
  coord_cartesian(xlim = c(-150, 150), ylim = c(-40, 40)) +
  theme(legend.text = element_text(size = 10)) 

# Output plot to file
ggsave("./NHP_species_alphavirus_range_overlap_counts.png",
       device = "png", dpi = 300, width = 10, height = 5, units = "in")