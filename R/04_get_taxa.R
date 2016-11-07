library(readr)
library(dplyr)
library(purrr)

#read in the kelp slopes data
kelp_slopes <- read_csv("../derived_data/kelp_slopes_with_temp_waves.csv")

#Merge with envt info from kelp raw data
raw_data_info <- read_csv("../raw_data/raw_data.csv") %>%
  group_by(SiteName) %>%
  summarize(Duration = max(Year)- min(Year),
            max_year = max(Year),
            min_year = min(Year),
            mean_Depth = mean_Depth[1],
            min_Depth = min_Depth[1],
            max_Depth = max_Depth[1],
            Study = Study[1],
            Latitude = Latitude[1],
            Longitude = Longitude[1],
            focalUnit = focalUnit[1],
            n = n()) %>%
  ungroup()

kelp_slopes_merged <- right_join(raw_data_info, kelp_slopes) %>%
  #deal with a few dropped study names
  mutate(Study = ifelse(is.na(Study), gsub("(.*)\\:", "", SiteName), Study))

#
taxa <- read_csv("../raw_data/taxa.csv") %>%
  dplyr::select(-ECOREGION) %>%
  dplyr::rename(Study = Study.ID)

canopy <- c("Ecklonia maxima",  "Macrocystis pyrifera", "Eisenia arborea", 
             "Nereocystis luetkeana",  "All annuals", 
              "understory and canopy kelps", 
            "Nereocystis sp.",  
            "Eualaria Areschoug 1884 fistulosa (Postels & Ruprecht) M.J.Wynne, 2009", 
            "Nereocystis Postels & Ruprecht, 1840 sp.",  
            "Nereocystis leutkana", "Egregia menziesii", "Macrocystis.pyrifera", 
            "Macrocystis angustifolia", 
            "Alaria fistulosa",  "All brown algae", 
            "Macrocystis integrifolia")


single_multi <- taxa %>%
  group_by(Study) %>%
  dplyr::summarize(has_canopy = sum(Species %in% canopy)) %>%
  ungroup() %>%
  mutate(has_canopy = ifelse(has_canopy>0, "canopy", "no canopy")) %>%
  mutate(Study = gsub("\\.", "_", Study)) %>%
  mutate(Study = gsub("KelpAbundance_", "", Study)) %>%
  mutate(Study = gsub("NovaScotia", "Nova Scotia", Study)) %>%
  mutate(Study = gsub("\\&", "", Study)) %>%
  mutate(Study = gsub("et al", "etal", Study)) %>%
  mutate(Study = gsub(" ", "_", Study)) %>%
  mutate(Study = gsub("__", "_", Study)) %>%
  mutate(Study = gsub("KelpCover_Temperate_Australia_Laminariales", "KelpCover_Temperate_Australia", Study))%>%
  mutate(Study = gsub("Ettinger-Epstein_Kingsford_2008_Austral_Ecol", "Ettinger_etal_2008", Study))%>%
  mutate(Study = gsub("SA_Gansbaai_kelp", "SA_Gansbaai_kelp_data", Study))%>%
  mutate(Study = gsub("Estes_and_Duggins_1995", "Estes_Duggins_AK", Study))%>%
  mutate(Study = gsub("Leinaas_and_Christie_1996", "Leinaas_Christie_1996", Study))%>%
  mutate(Study = gsub("norway_moy_christie_with_Åsen_2", "norway_moy_christie_with_Åsen_2", Study))%>%
  mutate(Study = gsub("en_2006", "en_2", Study))

single_multi <- rbind(single_multi, 
                      tibble(Study=c("Aus_Marmion", "Dayton_etal_1999_Laminaria","Dayton_etal_1999_Pterygophora"),
                                     has_canopy=c("no canopy","no canopy","no canopy")))

kelp_slopes_merged2 <- kelp_slopes_merged %>%
  mutate(Study = gsub(" ", "_", Study))%>%
  mutate(Study = gsub("__", "_", Study))%>%
  mutate(Study = gsub("(norway_inner_coast_norderhaug)(.*)", "\\1", Study))%>%
  mutate(Study = gsub("atal", "etal", Study))


kelp_slopes_merged_canopy <- left_join(kelp_slopes_merged2, single_multi)

#debug
anti_join(kelp_slopes_merged2, single_multi)$SiteName


#something with norway characters
kelp_slopes_merged_canopy$has_canopy[which(kelp_slopes_merged_canopy$Study=="norway_moy_christie_with_Åsen_2")] <- "no canopy"
kelp_slopes_merged_canopy$has_canopy[which(kelp_slopes_merged_canopy$Study=="Trowbridge_etal_2013")] <- "no canopy"
kelp_slopes_merged_canopy$has_canopy[grep("South_Africa", kelp_slopes_merged_canopy$Study)] <- "canopy" #Ecklonia maxima

sum(is.na(kelp_slopes_merged_canopy$has_canopy))

write_csv(kelp_slopes_merged_canopy, "../derived_data/kelp_slopes_with_temp_waves_canopy.csv")
