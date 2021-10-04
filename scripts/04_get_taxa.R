library(readr)
library(dplyr)
library(purrr)
library(meowR)
setwd(here::here())

# 1. Read in data with waves and kelp
rd_wave <- read.csv("derived_data/raw_data_with_temp_waves.csv") %>% as_tibble

# 2. Read in taxa file and setup for merge - canopy/nocanopy ####
taxa <- read_csv("raw_data/taxa.csv") %>%
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

# categorize stipe types
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
  mutate(Study = gsub("en_2006", "en_2", Study)) %>%
  rbind(single_multi, 
        tibble(Study=c("Aus_Marmion", "Dayton_etal_1999_Laminaria","Dayton_etal_1999_Pterygophora", "Trowbridge_etal_2013", "Kirihara_etal_2003"),
                has_canopy=c("no canopy","no canopy","no canopy", "no canopy", "no canopy")))

#clean some names
rd_wave_cleaned <- rd_wave %>%
  mutate(Study = case_when(
    Study == "Anderson_etal_ 2006" ~ "Anderson_etal_2006",
    Study == "Gagnon_atal_2005" ~ "Gagnon_etal_2004",
    Study == "Gerard_ 1976"   ~ "Gerard_1976",
    Study == "Kvitek_etal_ 2001" ~ "Kvitek_etal_2001",
    Study == "norway_inner_coast_norderhaug-Ei" ~ "norway_inner_coast_norderhaug",
    Study == "norway_inner_coast_norderhaug-Gr" ~ "norway_inner_coast_norderhaug",
    Study == "norway_inner_coast_norderhaug-No" ~ "norway_inner_coast_norderhaug",
    Study == "norway_moy_christie_with_Åsen_2" ~ "norway_moy_christie_with_Åsen_2" ,
    Study == "Smith_ 1985" ~ "Smith_1985",
    Study == "South_Africa_Kwaaibaii"  ~ "SA_Gansbaai_kelp_data",
    Study == "South_Africa_Pump_House"  ~ "SA_Gansbaai_kelp_data",
    Study == "South_Africa_Mauritzbaai"  ~ "SA_Gansbaai_kelp_data",

    TRUE ~ Study
    
#    Study == "Kirihara_etal_2003"

  ))


#debug
rd_wave_cleaned$Study %>% unique
anti_join(rd_wave_cleaned, single_multi)$Study %>% unique()
anti_join(single_multi, rd_wave_cleaned)$Study %>% unique()

# 3. merge data
rd_for_analysis <- left_join(rd_wave_cleaned, single_multi)



#non-tasmanian Austrlian sites problematic
rd_for_analysis[which(rd_for_analysis$Longitude>100 &
                        rd_for_analysis$Longitude<160 & #NZ
                        rd_for_analysis$Latitude>-39.3 &
                        rd_for_analysis$Latitude < 50),]$has_canopy <- "no canopy"

#Europe has no canopy - fix that
rd_for_analysis[which(rd_for_analysis$Longitude > -10 & 
                        rd_for_analysis$Longitude < 0 & 
                        rd_for_analysis$Latitude > 40 & 
                        rd_for_analysis$Latitude < 50),]$has_canopy <- "no canopy"

#all of south africa should be canopy.... It's E. maxima
rd_for_analysis[which(rd_for_analysis$Latitude < 0 & 
                        rd_for_analysis$Longitude > 0 &
                        rd_for_analysis$Longitude < 40),]$has_canopy <- "canopy"
# 
# #visual check
# worldmap <- map_data("world")
# 
# ggplot() +
#   geom_map(
#     data = worldmap, map = worldmap,
#     aes(x = long, y = lat, map_id = region),
#     fill = "white", color = "black",
#   )  +
#   geom_point(data = rd_for_analysis,
#              aes(Longitude, Latitude, color=has_canopy)) +
#   theme_void()


write_csv(rd_for_analysis, "derived_data/raw_data_with_temp_waves_canopy.csv")
