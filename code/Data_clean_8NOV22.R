# Code for importing and merging the data for BRT analysis

# Data includes: 
#    (1) imputed Pantheria data from the Fischhoff et al., 2021 paper 
#        (https://doi.org/10.25390/caryinstitute.c.5293339.v5)
#
#    (2) Trophic guild, IUCN status, and habitat from Ecological Traits of the World's Primates
#        (https://doi.org/10.1038/s41597-019-0059-9)
#               
#    (3) Elton Traits foraging data 
#        (https://doi.org/10.1890/13-1917.1)
##
library(dplyr)
library(tidyr)

# Bring in the imputed dataset from the Fischhoff et al, 2021 paper
prim_data <- read.csv("NHP_Traits_Imp_Fischoff_30Oct22.csv")

# Bring in Ecological Traits of the World's Primates
# The taxonomy has been changed to match the Pantheria data
# Note that some species have conflicting entries for Trophic Guild and these were changed to NA
ecol_traits <- read.csv("Ecol_Traits_habitat_iucn_trophic_9NOV22.csv")

# Bring in EltonTraits data
elton <- read.csv("Elton_Traits_9NOV22.csv", fileEncoding="UTF-8-BOM")

# Final DF includes data from the three sources
join_df <- elton %>% 
  right_join(prim_data) %>%
  full_join(ecol_traits)

#Coerce columns to factor
cols_fact <- c("ForStrat_Value", "IUCN", "Pop_T", "TrophicGuild")

final_df <- join_df %>%
  mutate_each(funs(factor(.)),cols_fact) %>%
  #Transform a few variables from character to numeric
  mutate_at(c('Diet_Nect', 
              'Activity_Nocturnal', 
              'Activity_Crepuscular', 
              'Activity_Diurnal'), 
            as.numeric) %>%
  #Remove two redundant variables
  select(-BodyMass_Value, -X6.2_TrophicLevel, -X)

#####################
#Clean up trait data#
#####################

#One-hot encoding of the primate families 
prim_df <- final_df %>% 
  mutate(value = 1)  %>% 
  spread(Family, value,  fill = 0)

#Remove variables with >80% NA (i.e., 300 or more NA)
prim_data_reduced <- prim_df[, colSums(is.na(prim_df)) < 300]

#Check the variance of the variables
no_var <- caret::nearZeroVar(
  prim_data_reduced[3:72],
  freqCut = 95/5,
  uniqueCut = 10,
  saveMetrics = T,
  names = T)

#Remove the variables with little variance
prim_data_final <- prim_data_reduced %>%
  select(-one_of(no_var)) 