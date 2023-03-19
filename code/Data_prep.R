# Code for creating several climate and biogeographical variables
# And importing/merging/cleaning the data for BRT analysis

# Data sources include: 
#    (1) COMBINE: a coalesced mammal database of intrinsic and extrinsic traits
#        (https://doi.org/10.1002/ecy.3344)
#
#    (2) Ecological Traits of the World's Primates
#        (https://doi.org/10.1038/s41597-019-0059-9)
#
#    (3) Raster files from several sources, including WorldClim and SEDAC               
##
library(dplyr)
library(tidyr)
library(tidyverse)
library(sp)
library(sf)
library(rgdal)
library(raster)
library(exactextractr)
library(RISmed)
options(scipen = 999) 
#############################################
# Import COMBINE and Ecological Traits data #
#############################################
setwd("C:/Users/Mike/Dropbox/MAYV_NHP_prediction/NHP_alphavirus_hosts/Data")

# COMBINE trait data
combine <- read.csv("COMBINE_trait_data_imputed.csv") %>%
  #Remove the extinct species
  filter(iucn2020_binomial != "Not recognised") %>%
  dplyr::select(-c(order, genus, species)) %>%
  #Fixing up some of the species vs sub-species discrepencies
  mutate(iucn2020_binomial = case_when(
    iucn2020_binomial == "Lagothrix lagothricha" & phylacine_binomial == "Lagothrix cana" ~ "Lagothrix cana",
    iucn2020_binomial == "Lagothrix lagothricha" & phylacine_binomial == "Lagothrix lugens" ~ "Lagothrix lugens",
    iucn2020_binomial == "Lagothrix lagothricha" & phylacine_binomial == "Lagothrix poeppigii" ~ "Lagothrix poeppigii",
    iucn2020_binomial == "Sapajus apella" & phylacine_binomial == "Sapajus macrocephalus" ~ "Sapajus macrocephalus",
    TRUE ~ iucn2020_binomial))

# Bring in Ecological Traits of the World's Primates data
mass <- read.csv("ETWP_BodyMass.csv") 
diel <- read.csv("ETWP_DielActivity.csv")
habitat <- read.csv("ETWP_Habitat.csv")
iucn <- read.csv("ETWP_IUCN_Poptrend_Realm.csv")
locomotion <- read.csv("ETWP_Locomotion.csv")
trophic <- read.csv("ETWP_TrophicGuild.csv")
etwp <- list(diel, mass, habitat, iucn, locomotion, trophic)

# Fixing up a few issues with ETWP species
etwp_new <- list()
for (i in 1:length(etwp)){
  etwp_new[[i]] <- etwp[[i]] %>%
    mutate(
      #Remove the underscore
      Species = str_replace(Species, "_", " "),
      Species = case_when(
        #If binomial is NA fill in with the ITIS binomial
        is.na(Species) ~ Species..ITIS.,
        #Fix up a few binomials to match the IUCN spelling
        Species == "Galagoides demidovii" ~ "Galagoides demidoff",
        Species == "Lepilemur ahmansonorum" ~ "Lepilemur ahmansonori",
        Species == "Lepilemur hubbardi" ~ "Lepilemur hubbardorum",
        Species == "Lepilemur sahamalazensis" ~ "Lepilemur sahamalaza",
        Species == "Piliocolobus waldronae" ~ "Piliocolobus waldroni",
        TRUE ~ as.character(Species)))
}
# Note that some species have conflicting entries for Trophic Guild and Locomotion and these were changed to NA
# When there are multiple entries for mass per species I took the average

#---One-hot encoding/cleaning some of the variables---#

#---IUCN variables
iucn_new <- data.frame(etwp_new[[4]]) %>%
  mutate(valuea = 1, valueb = 1) %>%
  spread(IUCN, valuea, fill = 0) %>%
  spread(Pop_T, valueb, fill = 0) %>%
  dplyr::select(Family, Species, 10:19) %>%
  rename(threat_critical=CR, threat_endangered=EN, threat_vulnerable=VU, 
         threat_near_threatened=NT, threat_least_concern=LC, threat_data_deficient=DD,
         threat_not_evaluated=NE, ppoulation_increasing=I, population_stable=S,
         population_decreasing=D)

#---Habitat
habitat_new <- data.frame(etwp_new[[3]]) %>%
  dplyr::select(Family, Species, 6:12)

#---Trophic
trophic <- data.frame(etwp_new[[6]]) %>%
  dplyr::select(Family, Species, TrophicGuild) %>%
  distinct()  

# Remove duplicate species with conflicting data
trophic$dup <- duplicated(trophic$Species)
dups <- trophic %>% filter(dup==T) %>% dplyr::select(Species)
trophic <- trophic[ ! trophic$Species %in% dups$Species, ]
trophic_new <- trophic %>%
  mutate(value = 1) %>%
  spread(TrophicGuild, value, fill = 0) %>%
  dplyr::select(1:2, 4:9)

#---Locomotion
locomotion <- data.frame(etwp_new[[5]]) %>%
  dplyr::select(Family, Species, Locomotion) %>%
  distinct()

# Remove duplicate species with conflicting data
locomotion$dup <- duplicated(locomotion$Species)
dups_loc <- locomotion %>% filter(dup==T) %>% dplyr::select(Species)
locomotion <- locomotion[ ! locomotion$Species %in% dups_loc$Species, ]

locomotion_new <- locomotion %>%
  mutate(value = 1) %>%
  spread(Locomotion, value, fill = 0) %>%
  dplyr::select(Family, Species, locomotion_arboreal=AR, locomotion_both=BOTH, locomotion_terrestrial=`T`) 

# Mass
mass_new <-data.frame(etwp_new[[2]]) %>%
  group_by(Species) %>%
  dplyr::select(Family, Species, BodyMassMale_kg, BodyMassFemale_kg) %>% 
  summarize(massMale_kg = mean(BodyMassMale_kg, na.rm =TRUE, na.action = na.pass),
            massFemale_kg = mean(BodyMassFemale_kg, na.rm = TRUE, na.action = na.pass))

# Final DF includes data from the three sources
join_etwp <- mass_new %>%
  full_join(habitat_new) %>% 
  full_join(iucn_new) %>% 
  full_join(locomotion_new) %>% 
  full_join(trophic_new)  

combined_traits <- right_join(join_etwp, combine, by=c("Species"="iucn2020_binomial", "Family"="family")) %>%
  mutate(biogeographical_realm = case_when(
    grepl(",", biogeographical_realm) ~ "realm_multiple",
    TRUE ~ as.character(biogeographical_realm)),
    activity_cycle = case_when(
      activity_cycle == 1 ~ "activity_cycle_nocturnal",
      activity_cycle == 3 ~ "activity_cycle_diurnal",
      activity_cycle == 2 ~ "activity_cycle_multiple"),
    valuea = 1, valueb = 1, valuec = 1, valued = 1)  %>% 
  spread(Family, valuea,  fill = 0) %>%
  spread(biogeographical_realm, valueb, fill = 0) %>%
  spread(foraging_stratum, valuec, fill = 0) %>%
  spread(activity_cycle, valued, fill = 0) %>%
  rename(foraging_stratum_arboreal=Ar 
         foraging_stratum_ground=G, 
         foraging_stratum_scansorial=S) %>%
  dplyr::select(-`<NA>`)

# Calculating derived variables from COMBINE data
combined_traits <- combined_traits %>%
  mutate(
    body_size_ratio = adult_mass_g/neonate_mass_g,
    rel_age_fb_days = age_first_reproduction_d/max_longevity_d,
    rel_sexual_maturity_days = maturity_d/max_longevity_d,
    postnatal_growth_rate = weaning_mass_g/neonate_mass_g,
    # Formula for production from Hamilton et al., 2010
    mass_specific_production = (neonate_mass_g/adult_mass_g)*litter_size_n*litters_per_year_n,
    home_range_scaled_km2_g = home_range_km2/adult_mass_g)

names(combined_traits) <- tolower(names(combined_traits))

# Set working directory
setwd("C:/Users/Mike/Dropbox/MAYV_NHP_prediction")

# Read in the IUCN shape files
load(file = "./mammal_iucn.RData")
mammal_iucn <- as(mammal_iucn, Class = "Spatial")

# Select only the primate records
prim_range <- mammal_iucn[mammal_iucn@data$order_=="PRIMATES",]
prim_range_sf <- st_as_sf(prim_range)
prim_range_area <- prim_range_sf$SHAPE_Area
binomial <- prim_range_sf[,c(2,9)]

# Extract maximum and minimum lat/long of the species’ range
min_max <- as.data.frame(do.call("rbind", lapply(st_geometry(prim_range_sf), st_bbox)))
latitude <- min_max %>% 
  dplyr::select(ymin, ymax) %>%
  mutate(range_lat = ymax-ymin)

# Extract the centroid of species range
centroids <- as.data.frame(getSpPPolygonsLabptSlots(prim_range))

# Download the WorldClim rasters
#worldclim_rasters <- getData("worldclim",var="bio",res=10)
setwd("C:/Users/Mike/Dropbox/MAYV_NHP_prediction/wc10")
raster_list <- list.files(pattern = ".bil")
worldclim_rasters <- stack(raster_list)

# Calculate the mean temp, max temp, and min temp from WorldClim (Bio1, Bio5, Bio6)
mean_temp <- exact_extract(worldclim_rasters$bio1, prim_range, 'mean')
max_temp <- exact_extract(worldclim_rasters$bio5, prim_range, 'max')
min_temp <- exact_extract(worldclim_rasters$bio6, prim_range, 'min')

# Subtract max temp from min temp to get the range
subtract_temp <- worldclim_rasters$bio5 - worldclim_rasters$bio6
range_temp <- exact_extract(subtract_temp, prim_range, 'mean')

# Calculate the mean, min, and max precipitation()
mean_prec <- exact_extract(worldclim_rasters$bio12, prim_range, 'mean')
max_prec <- exact_extract(worldclim_rasters$bio13, prim_range, 'max')
min_prec <- exact_extract(worldclim_rasters$bio14, prim_range, 'min')

# Subtract max temp from min temp to get the range
subtract_prec <- worldclim_rasters$bio13 - worldclim_rasters$bio14
range_prec <- exact_extract(subtract_prec, prim_range, 'mean')

# Import the population density, mammal richness, and human footprint rasters
setwd("C:/Users/Mike/Dropbox/MAYV_NHP_prediction")
popdens <- raster("gpw_v4_population_density_rev11_2020_15_min.asc")
species_rich <- raster("all_mammals.tif")
hfp <- raster("wildareas-v3-2009-human-footprint.tif")

# Calculate the median, min, and max pop density within each species range
median_popdens <- exact_extract(popdens, prim_range, 'median')
mean_popdens <- exact_extract(popdens, prim_range, 'mean')
max_popdens <- exact_extract(popdens, prim_range, 'max')
min_popdens <- exact_extract(popdens, prim_range, 'min')

# Calculate the average mammal species richness and human footprint within each species range
species_avg <- exact_extract(species_rich, prim_range, 'mean')
hfp_avg <- exact_extract(hfp, prim_range, 'mean')

# Bring in WWF ecoregions
ecor <- st_read("tnc_terr_ecoregions.shp")
# Rasterize the polygons
ecor_rast <- rasterize_polygons(ecor, popdens)

# calculate the number of unique ecoregions in each range
ecor_n <- exact_extract(ecor_rast, prim_range, 'variety')

# Search PubMed for number of hits per species
species <- data.frame(combine$iucn2020_binomial)
pubmed <- list()
df <- data.frame(matrix(ncol = 1, nrow = nrow(species)))
colnames(df) <- 'pubmed_count'

for (i in 1:nrow(species)){
  Sys.sleep(1)
  pubmed[[i]] <- EUtilsSummary(species[i,1], type="esearch", db="pubmed")
  df$pubmed_count[i] <- pubmed[[i]]@count
}

final_count <- cbind(species, df)
final_count$log_count <- log(final_count$pubmed_count)

# Bind the columns
new_vars <- cbind(binomial, ecor_n, hfp_avg, species_avg, median_popdens, max_popdens, 
                  min_popdens, mean_popdens, mean_temp, max_temp, min_temp, range_temp,
                  mean_prec, max_prec, min_prec, range_prec, latitude, centroids)

new_vars <- st_drop_geometry(new_vars)

# Calculate the mean/min/max values across sub-species ranges
new_vars_collapse <- new_vars %>%
  group_by(binomial) %>%
  summarise(ecoregions_n = mean(ecor_n),
            human_footprint_mean = mean(hfp_avg),
            mammal_richness_mean = mean(species_avg),
            
            human_pop_dens_median = median(median_popdens),
            human_pop_dens_mean = mean(mean_popdens),
            human_pop_dens_max = max(max_popdens),
            human_pop_dens_min = min(min_popdens),
            
            temp_mean_dC = mean(mean_temp),
            temp_max_dC = max(max_temp),
            temp_min_dC = min(min_temp),
            temp_range_dC = mean(range_temp),
            
            precip_mean_mm = mean(mean_prec),
            precip_max_mm = max(max_prec),
            precip_min_mm = min(min_prec),
            precip_range_mm = mean(range_prec),
            
            #centroid_latitude_WGS84dd = mean(centroids), UNCLEAR
            range_latitude_WGS84dd = mean(range_lat),
            min_latitude_WGS84dd = min(ymin),
            max_latitude_WGS84dd = max(ymax),
            .groups = 'drop') %>%
  distinct() %>%
  as.data.frame()

final_df <- left_join(combined_traits, new_vars_collapse, by=c("species"="binomial")) %>%
  dplyr::select(-phylacine_binomial)

#####################
#Clean up trait data#
#####################

na_rem <- nrow(final_df)*.8

#Remove variables with >80% NA (i.e., 300 or more NA)
prim_data_reduced <- final_df[, colSums(is.na(final_df)) < na_rem]

#Check the variance of the variables
no_var <- caret::nearZeroVar(
  prim_data_reduced[2:ncol(prim_data_reduced)],
  freqCut = 90/10,
  uniqueCut = 10,
  saveMetrics = T,
  names = T)

#Remove the variables with little variance
prim_data_final <- prim_data_reduced %>%
  dplyr::select(-c(det_vfish, 
                   det_scav, 
                   freshwater, 
                   marine, 
                   terrestrial_non.volant, 
                   terrestrial_volant, 
                   glaciation,
                   # Irrelevant variables
                   hibernation_torpor, 
                   fossoriality,
                   # Redundant
                   dphy_invertebrate,
                   dphy_vertebrate,
                   dphy_plant,
                   trophic_level,
                   island_endemicity))

# Replace some NaN with NA
prim_data_final[prim_data_final == "NaN"] <- NA

# Re-order a few cols
prim_data_final <- prim_data_final %>%
  dplyr::select(species, vir_pos, everything())

# Variable coverage
missing_vars <- prim_data_final %>%
  dplyr::select(everything()) %>%  
  summarise_all(
    funs(sum(!is.na(.))/nrow(prim_data_final)))

missing_species <- rowSums(is.na(prim_data_final))

# Checking skewness
skew <- sapply(c(3:5, 32:51, 61:63, 96:119), function(x) moments::skewness(na.omit(prim_data_final[, x])))
CLSK <- colnames(prim_data_final)[c(3:5, 32:51, 61:63, 96:119)][which(skew > 2)]

for(i in CLSK) {
  if(i %in% c("wos_hits", "lower_elevation_m", "human_pop_dens_min", "precip_min_mm")) { #deal with negative infinity issue using an if statement
    prim_data_final[, i] <- log(prim_data_final[, i] + 1)
  } else prim_data_final[, i] <- log(prim_data_final[, i])
}
