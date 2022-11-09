#Bring in the imputed dataset from the Fischhoff et al, 2021 paper
prim_data <- read.csv("prim_database_30Oct22.csv")

#Load functions from Han Lab github
source("https://raw.githubusercontent.com/hanlab-ecol/GBMFunctions/main/partial_plotR.R")

#Bring in Trophic Guild, Habitat, and IUCN status from Ecological Traits of the World's Primates
trophic <- read.csv("TrophicGuild.csv")
habitat <- read.csv("Habitat.csv")
iucn <- read.csv("IUCN_Poptrend_Realm.csv")

#Fix up the species names and select the relevant columns
trophic <- trophic %>%
  mutate(
    Species=case_when(
      is.na(Species) ~ Species..ITIS.,
      TRUE ~ as.character(Species))) %>%
  dplyr::select(Species, TrophicGuild) 

iucn <- iucn %>%
  mutate(
    Species=case_when(
      is.na(Species) ~ Species..ITIS.,
      TRUE ~ as.character(Species))) %>%
  dplyr::select(Species, IUCN, Pop_T)

habitat <- habitat %>%
  mutate(
    Species=case_when(
      is.na(Species) ~ Species..ITIS.,
      TRUE ~ as.character(Species))) %>%
  dplyr::select(Species, 6:12)

#---Deal with conflicting info for Trophic Guild---#
#First remove any duplicates (some species are assigned the same trophic guild in multiple rows)
trophicguild <- unique(trophic)

#Any entries with conflicting information for a species will be assigned NA
trophicguild$unique <- !(duplicated(trophicguild$Species) | duplicated(trophicguild$Species, fromLast = TRUE))

trophicguild <- trophicguild %>%
  mutate(TrophicGuild = case_when(
    unique == "FALSE" ~ NA_character_,
    TRUE ~ as.character(TrophicGuild))) %>%
  dplyr::select(-unique) %>%
  distinct()

#Join the 3 datasets together
joined_prim_traits <- habitat %>%
  full_join(iucn) %>%
  full_join(trophicguild) 

#Update the taxonomy to match Pantheria
join_update <- joined_prim_traits %>% 
  mutate(
    Species = case_when(Species == "Cheracebus_purinus" ~ "Callicebus_purinus",
                        Species == "Semnopithecus_entellus" ~ "Semnopithecus_dussumieri",
                        Species == "Aotus_lemurinus" ~ "Aotus_hershkovitzi",
                        Species == "Hoolock_hoolock" ~	"Bunopithecus_hoolock", 
                        Species == "Plecturocebus_baptista" ~	"Callicebus_baptista",
                        Species == "Plecturocebus_bernhardi" ~	"Callicebus_bernhardi",
                        Species == "Plecturocebus_brunneus" ~	"Callicebus_brunneus",
                        Species == "Plecturocebus_caligatus" ~	"Callicebus_caligatus",
                        Species == "Plecturocebus_cinerascens" ~	"Callicebus_cinerascens",
                        Species == "Plecturocebus_cupreus" ~	"Callicebus_cupreus",
                        Species == "Plecturocebus_discolor" ~	"Callicebus_discolor",
                        Species == "Plecturocebus_donacophilus" ~	"Callicebus_donacophilus",
                        Species == "Plecturocebus_hoffmannsi" ~	"Callicebus_hoffmannsi",
                        Species == "Cheracebus_lucifer" ~	"Callicebus_lucifer",
                        Species == "Cheracebus_lugens" ~	"Callicebus_lugens",
                        Species == "Cheracebus_medemi" ~	"Callicebus_medemi", 
                        Species == "Plecturocebus_modestus" ~	"Callicebus_modestus",
                        Species == "Plecturocebus_moloch" ~	"Callicebus_moloch",
                        Species == "Plecturocebus_oenanthe" ~	"Callicebus_oenanthe",
                        Species == "Plecturocebus_olallae" ~	"Callicebus_olallae",
                        Species == "Plecturocebus_ornatus" ~	"Callicebus_ornatus",
                        Species == "Plecturocebus_pallescens" ~	"Callicebus_pallescens",
                        Species == "Cheracebus_regulus" ~	"Callicebus_regulus",
                        Species == "Plecturocebus_stephennashi" ~	"Callicebus_stephennashi",
                        Species == "Cheracebus_torquatus" ~	"Callicebus_torquatus",
                        Species == "Mico_acariensis" ~	"Callithrix_acariensis",
                        Species == "Mico_argentatus" ~	"Callithrix_argentata",
                        Species == "Mico_chrysoleucos" ~ "Callithrix_chrysoleuca",
                        Species == "Mico_emiliae" ~	"Callithrix_emiliae",
                        Species == "Mico_humeralifer" ~	"Callithrix_humeralifera",
                        Species == "Mico_humilis" ~	"Callithrix_humilis",
                        Species == "Mico_intermedius" ~	"Callithrix_intermedia",
                        Species == "Mico_leucippe" ~	"Callithrix_leucippe",
                        Species == "Mico_mauesi" ~	"Callithrix_mauesi",
                        Species == "Mico_melanurus" ~	"Callithrix_melanura",
                        Species == "Mico_nigriceps" ~	"Callithrix_nigriceps",
                        Species == "Cebuella_pygmaea" ~	"Callithrix_pygmaea",
                        Species == "Mico_saterei" ~	"Callithrix_saterei",
                        Species == "Sapajus_apella" ~	"Cebus_apella",
                        Species == "Sapajus_libidinosus" ~	"Cebus_libidinosus",
                        Species == "Sapajus_nigritus" ~	"Cebus_nigritus",
                        Species == "Sapajus_xanthosternos" ~	"Cebus_xanthosternos",
                        Species == "Allochrocebus_lhoesti" ~	"Cercopithecus_lhoesti",
                        Species == "Allochrocebus_preussi" ~	"Cercopithecus_preussi",
                        Species == "Allochrocebus_solatus" ~	"Cercopithecus_solatus",
                        Species == "Lagotrix_lugens" ~	"Lagothrix_lugens",
                        Species == "Lagothrix_flavicauda" ~	"Oreonax_flavicauda",
                        Species == "Leontocebus_fuscicollis" ~	"Saguinus_fuscicollis",
                        Species == "Leontocebus_nigricollis" ~	"Saguinus_nigricollis",
                        Species == "Leontocebus_tripartitus" ~	"Saguinus_tripartitus",
                        Species == "Cephalopachus_bancanus" ~	"Tarsius_bancanus",
                        Species == "Carlito_syrichta" ~	"Tarsius_syrichta",
                        Species == "Semnopithecus_johnii" ~	"Trachypithecus_johnii",
                        Species == "Semnopithecus_vetulus" ~	"Trachypithecus_vetulus",
                        Species == "Sciurocheirus_alleni" ~	"Galago_alleni",
                        Species == "Sciurocheirus_cameronensis" ~	"Galago_cameronensis",
                        Species == "Galagoides_demidovii" ~	"Galago_demidoff",
                        Species == "Sciurocheirus_gabonensis" ~	"Galago_gabonensis",
                        Species == "Galagoides_thomasi" ~ "Galago_thomasi",
                        TRUE ~ Species))

#Join data with the imputed dataset
joined_new <- join_update %>%
  separate(Species, c("Genus", "Species"), "_") %>%
  right_join(prim_data, by=c("Genus", "Species"))

#Bring in EltonTraits data
prim_names <- joined_new %>% 
  dplyr::select(Genus, Species)

elton <- read_excel("EltonTraits.xlsx") %>%
  inner_join(prim_names) 

#Final DF includes data from the three sources
final_df <- elton %>% 
  inner_join(joined_new)

#Coerce columns to factor
cols_fact <- c("ForStrat_Value", "IUCN", "Pop_T", "TrophicGuild")

final_df <- final_df %>%
  mutate_each(funs(factor(.)),cols_fact) %>%
  mutate_at(c('Diet_Nect', 'Activity_Nocturnal'), as.numeric) 

#####################
#Clean up trait data#
#####################

#One-hot encoding of the primate families 
prim_df <- final_df %>% 
  mutate(value = 1)  %>% 
  spread(Family, value,  fill = 0 )

#Remove variables with >80% NA (i.e., 300 or more NA)
prim_data_reduced <- prim_df[, colSums(is.na(prim_df)) < 300]

#Check the variance for each variable
no_var <- nearZeroVar(
  prim_data_reduced[3:72],
  freqCut = 95/5,
  uniqueCut = 10,
  saveMetrics = F,
  names = T)

#Remove the variables with little variance
prim_data_final <- prim_data_reduced %>%
  dplyr::select(-one_of(no_var)) %>%
  #Remove variable that is in both EltonTraits and Pantheria
  dplyr::select(-c(X6.2_TrophicLevel, `BodyMass_Value`, Genus, Species)) 

prim_species <- prim_data_reduced %>% 
  dplyr::select(Genus, Species)
