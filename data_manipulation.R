# 15.2.2022
# Data science MAP
# Shawn schneidereit


library(readr)
library(tidyverse)
library(stringr)

# tree prediction

# data import 
forest_cover <- read_csv("Forest Cover Type Prediction.csv")
forest_cover_big <- read_csv("covtype.csv")

fc <- pivot_longer(forest_cover_big, cols = Soil_Type1:Soil_Type40, names_to = "soil_type") %>% 
  filter(value==1) %>% 
  select(-value) %>% 
  pivot_longer(., cols = Wilderness_Area1:Wilderness_Area4, names_to = "wilderness_area") %>% 
  filter(value==1) %>% 
  select(-value)

# clean and simplefly soil types

fc_clean <- fc %>% 
  mutate(soil_type = str_replace(soil_type, "Soil_Type", ""),
         soil_type = if_else(soil_type==3, "Haploborolis", soil_type),
         soil_type = if_else(soil_type %in% c(2,4,5,6), "Vanet", soil_type),
         soil_type = if_else(soil_type==7, "Gothic", soil_type),
         soil_type = if_else(soil_type %in% c(1,8,9,10,11,13,18,26,31,32,33), "Catacount", soil_type),
         soil_type = if_else(soil_type %in% c(12,29,30), "Legault", soil_type),
         soil_type = if_else(soil_type==14, "Pachic Argiborolis", soil_type),
         soil_type = if_else(soil_type==15, "unspecified", soil_type),
         soil_type = if_else(soil_type %in% c(16,17,19,20,23,35), "Cryaquolis", soil_type),
         soil_type = if_else(soil_type %in% c(21,22,23,24,25,27,28,31,32,33,38,39), "Leighcan", soil_type),
         soil_type = if_else(soil_type %in% c(34,35,36,37,40), "Cryorthents", soil_type),
       
         wilderness_area = str_replace(wilderness_area, "Wilderness_Area", ""),
         wilderness_area = if_else(wilderness_area==1, "Rawah", wilderness_area),
         wilderness_area = if_else(wilderness_area==2, "Neota", wilderness_area),
         wilderness_area = if_else(wilderness_area==3, "Comanche Peak", wilderness_area),
         wilderness_area = if_else(wilderness_area==4, "Cache la Poudre", wilderness_area),
       
         Cover_Type = as.character(Cover_Type),
         Cover_Type = if_else(Cover_Type==1, "Spruce_Fir", Cover_Type),
         Cover_Type = if_else(Cover_Type==2, "Lodgepole Pine", Cover_Type),
         Cover_Type = if_else(Cover_Type==3, "Ponderosa Pine", Cover_Type),
         Cover_Type = if_else(Cover_Type==4, "Cottonwood/Willow", Cover_Type),
         Cover_Type = if_else(Cover_Type==5, "Aspen", Cover_Type),
         Cover_Type = if_else(Cover_Type==6, "Douglas-fir", Cover_Type),
         Cover_Type = if_else(Cover_Type==7, "Krummholz", Cover_Type))
  


write_csv(fc_clean,"Forest Cover Type Prediction big tidy.csv")



ggplot(fc_clean, aes(sort(soil_type))) +
  geom_bar() +
  theme_classic() 




# # old code
# 
# # import insect abundance data
# abundance <- read_csv("InsectAbundanceBiomassData/InsectAbundanceBiomassData.csv")
# # import plot meta data
# plots     <- read_csv("InsectAbundanceBiomassData/PlotData.csv")
# 
# 
# data <- left_join(abundance, plots) #%>% 
# # rename(biomass = Number) 
# #data <- rename_with(data, tolower)
# 
# biomass <- data %>% filter(MetricAB=="biomass")
# abundance <- data %>% filter(MetricAB=="abundance") %>% na.omit()
# 
# write_csv(abundance, "abundance.csv",na = "0" )
# 
# 
# ggplot(abundance, aes(Year, Number, group=as.factor(Plot_ID), color =)) +
#   geom_line() +
#   theme_classic() 