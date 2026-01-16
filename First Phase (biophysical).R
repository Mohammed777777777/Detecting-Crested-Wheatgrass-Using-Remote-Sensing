#set working directory
library(readr)
library(tidyverse)

#setwd("C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets")
#setwd("D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets")
setwd("C:/Users/Mohammed/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets")

Bio <- read_csv("Complete Dataset_Analysis ready_0.csv")
#View(Bio)
names(Bio)

##Descriptive Stat (NOTE: calculated the mean, coefficient of varition (Suggestion from meeting (14-04-25)) and standard deviation of everything. This
## gives us the statistics at the quadrat level. Next we will do it in the plot level / species level)
Bio %>% 
  group_by(Site) %>% 
  summarise(## Cover of CW
            mean_Cover_CW = mean(C_CW, na.rm = TRUE), ##
            sd_Cover_CW = sd(C_CW, na.rm = TRUE),
            cv_Cover_CW = (sd_Cover_CW / mean_Cover_CW) * 100,
            
            ## Height of CW
            mean_H_CW = mean(H_CW, na.rm = TRUE),
            sd_H_CW = sd(H_CW, na.rm = TRUE),
            cv_H_CW = (sd_H_CW / mean_H_CW) * 100,
            
            ## Cover of CW
            mean_Cover_N = mean(C_N, na.rm = TRUE),
            sd_Cover_N = sd(C_N, na.rm = TRUE),
            cv_Cover_N = (sd_Cover_N / mean_Cover_N) * 100,
            
            ## Height of CW
            mean_H_N = mean(H_N, na.rm = TRUE),
            sd_H_N = sd(H_N, na.rm = TRUE),
            cv_H_N = (sd_H_N / mean_H_N) * 100,
            
            ## Cover of Forbs
            mean_C_Forbs = mean(C_Forbs, na.rm = TRUE),
            sd_C_Forbs = sd(C_Forbs, na.rm = TRUE),
            cv_C_Forbs = (sd_C_Forbs / mean_C_Forbs) * 100,
            
            ## Cover of Standing Dead
            mean_C_SD = mean(C_SD, na.rm = TRUE),
            sd_C_SD = sd(C_SD, na.rm = TRUE),
            cv_C_SD = (sd_C_SD / mean_C_SD) * 100,
            
            ## Cover of Litter
            mean_C_Litter = mean(C_Litter, na.rm = TRUE),
            sd_C_Litter = sd(C_Litter, na.rm = TRUE),
            cv_C_Litter = (sd_C_Litter / mean_C_Litter) * 100,
            
            ## Cover of Bareground
            mean_C_BG = mean(C_BG, na.rm = TRUE),
            sd_C_BG = sd(C_BG, na.rm = TRUE),
            cv_C_BG = (sd_C_BG / mean_C_BG) * 100,
            
            ## Cover of Shrub
            mean_C_Shrub = mean(C_Shrub, na.rm = TRUE),
            sd_C_Shrub = sd(C_Shrub, na.rm = TRUE),
            cv_C_Shrub = (sd_C_Shrub / mean_C_Shrub) * 100,
            
            ## Cover of Shrub2
            mean_C_Shrub_2 = mean(C_Shrub_2, na.rm = TRUE),
            sd_C_Shrub_2 = sd(C_Shrub_2, na.rm = TRUE),
            cv_C_Shrub_2 = (sd_C_Shrub_2 / mean_C_Shrub_2) * 100,
            
            ## Height of Shrub
            mean_H_Shrub = mean(H_Shrub, na.rm = TRUE),
            sd_H_Shrub = sd(H_Shrub, na.rm = TRUE),
            cv_H_Shrub = (sd_H_Shrub / mean_H_Shrub) * 100,
            
            ## Height of Shrub 2
            mean_H_Shrub_2 = mean(H_Shrub_2, na.rm = TRUE),
            sd_H_Shrub_2 = sd(H_Shrub_2, na.rm = TRUE),
            cv_H_Shrub_2 = (sd_H_Shrub_2 / mean_H_Shrub_2) * 100,
            
            ## Cover of Moss
            mean_C_Moss = mean(C_Moss, na.rm = TRUE),
            sd_C_Moss = sd(C_Moss, na.rm = TRUE),
            cv_C_Moss = (sd_C_Moss / mean_C_Moss) * 100,
            
            ## Leaf Area Index
            mean_LAI = mean(LAI, na.rm = TRUE),
            sd_LAI = sd(LAI, na.rm = TRUE),
            cv_LAI = (sd_LAI / mean_LAI) * 100,
            
            ## Dry Grass biomass of Crested Wheatgrass
            mean_B_Crested_m2 = mean(B_Crested_m2, na.rm = TRUE),
            sd_B_Crested_m2 = sd(B_Crested_m2, na.rm = TRUE),
            cv_B_Crested_m2 = (sd_B_Crested_m2 / mean_B_Crested_m2) * 100,
            
            ## Dry Grass biomass of Native Grass
            mean_B_Native_m2 = mean(B_Native_m2, na.rm = TRUE),
            sd_B_Native_m2 = sd(B_Native_m2, na.rm = TRUE),
            cv_B_Native_m2 = (sd_B_Native_m2 / mean_B_Native_m2) * 100,
            
            ## Total dry grass biomass (Dont Need this one)
            mean_B_Grass_m2 = mean(B_Grass_m2, na.rm = TRUE),
            sd_B_Grass_m2 = sd(B_Grass_m2, na.rm = TRUE),
            cv_B_Grass_m2 = (sd_B_Grass_m2 / mean_B_Grass_m2) * 100,
            
            ## Standing Dead biomass
            mean_B_Dead_m2 = mean(B_Dead_m2, na.rm = TRUE),
            sd_B_Dead_m2 = sd(B_Dead_m2, na.rm = TRUE),
            cv_B_Dead_m2 = (sd_B_Dead_m2 / mean_B_Dead_m2) * 100,
            
            ## Forbs biomass
            mean_B_Forbs_m2 = mean(B_Forbs_m2, na.rm = TRUE),
            sd_B_Forbs_m2 = sd(B_Forbs_m2, na.rm = TRUE),
            cv_B_Forbs_m2 = (sd_B_Forbs_m2 / mean_B_Forbs_m2) * 100,
            
            ## Shrub biomass
            mean_B_Shrub_m2 = mean(B_Shrub_m2, na.rm = TRUE),
            sd_B_Shrub_m2 = sd(B_Shrub_m2, na.rm = TRUE),
            cv_B_Shrub_m2 = (sd_B_Shrub_m2 / mean_B_Shrub_m2) * 100,
            
            ## Total biomass
            mean_B_Total_m2 = mean(B_Total_m2, na.rm = TRUE),
            sd_B_Total_m2 = sd(B_Total_m2, na.rm = TRUE),
            cv_B_Total_m2 = (sd_B_Total_m2 / mean_B_Total_m2) * 100,
            
            ## Total biomass
            mean_Wet_Biomas_m2 = mean(Wet_Biomas_m2, na.rm = TRUE),
            sd_Wet_Biomas_m2 = sd(Wet_Biomas_m2, na.rm = TRUE),
            cv_B_Biomas_m2 = (sd_Wet_Biomas_m2 / mean_Wet_Biomas_m2) * 100,
            
            ## moisture content
            mean_Moisture_m2 = mean(Moisture_m2, na.rm = TRUE),
            sd_Moisture_m2 = sd(Moisture_m2, na.rm = TRUE),
            cv_Moisture_m2 = (sd_Moisture_m2 / mean_Moisture_m2) * 100,
            
            ## moisture content
            mean_Moisture_m2 = mean(Moisture_m2, na.rm = TRUE),
            sd_Moisture_m2 = sd(Moisture_m2, na.rm = TRUE),
            cv_Moisture_m2 = (sd_Moisture_m2 / mean_Moisture_m2) * 100,
            
            ) -> DescriptiveStatdata 

#View(DescriptiveStatdata)
names(DescriptiveStatdata)

#Export the file
#write.table(DescriptiveStatdata, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data/DescriptiveStat.csv", sep = ",", row.names = FALSE)

## Load the Descriptive Stat file
#setwd("D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data")
#setwd("C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data")
setwd("C:/Users/Mohammed/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data")

descri <- read_csv("DescriptiveStat.csv")
#View(descri) ## NOTE: I added a species column manually in the excel sheet.
names(descri)

## Calcualte the species-wise mean, standard deviation and coefficient of variation

descri %>%
  group_by(Species) %>%  
  summarise(
    meann_Cover_CW = mean(mean_Cover_CW, na.rm = TRUE),
    sd_Cover_CW = sd(mean_Cover_CW, na.rm = TRUE),
    cv_Cover_CW = (sd_Cover_CW / meann_Cover_CW) * 100,
    
    meann_Cover_N = mean(mean_Cover_N, na.rm = TRUE),
    sd_Cover_N = sd(mean_Cover_N, na.rm = TRUE),
    cv_Cover_N = (sd_Cover_N / meann_Cover_N) * 100,
    
    meann_H_CW = mean(mean_H_CW, na.rm = TRUE),
    sd_H_CW = sd(mean_H_CW, na.rm = TRUE),
    cv_H_CW = (sd_H_CW / meann_H_CW) * 100,
    
    meann_H_N = mean(mean_H_N, na.rm = TRUE),
    sd_H_N = sd(mean_H_N, na.rm = TRUE),
    cv_H_N = (sd_H_N / meann_H_N) * 100,
    
    meann_C_Forbs = mean(mean_C_Forbs, na.rm = TRUE),
    sd_C_Forbs = sd(mean_C_Forbs, na.rm = TRUE),
    cv_C_Forbs = (sd_C_Forbs / meann_C_Forbs) * 100,
    
    meann_C_SD = mean(mean_C_SD, na.rm = TRUE),
    sd_C_SD = sd(mean_C_SD, na.rm = TRUE),
    cv_C_SD = (sd_C_SD / meann_C_SD) * 100,
    
    meann_C_Litter = mean(mean_C_Litter, na.rm = TRUE),
    sd_C_Litter = sd(mean_C_Litter, na.rm = TRUE),
    cv_C_Litter = (sd_C_Litter / meann_C_Litter) * 100,
    
    meann_C_BG = mean(mean_C_BG, na.rm = TRUE),
    sd_C_BG = sd(mean_C_BG, na.rm = TRUE),
    cv_C_BG = (sd_C_BG / meann_C_BG) * 100,
    
    meann_C_Shrub = mean(mean_C_Shrub, na.rm = TRUE),
    sd_C_Shrub = sd(mean_C_Shrub, na.rm = TRUE),
    cv_C_Shrub = (sd_C_Shrub / meann_C_Shrub) * 100,
    
    meann_C_Moss = mean(mean_C_Moss, na.rm = TRUE),
    sd_C_Moss = sd(mean_C_Moss, na.rm = TRUE),
    cv_C_Moss = (sd_C_Moss / meann_C_Moss) * 100,
    
    meann_LAI = mean(mean_LAI, na.rm = TRUE),
    sd_LAI = sd(mean_LAI, na.rm = TRUE),
    cv_LAI = (sd_LAI / meann_LAI) * 100,
    
    meann_LAI = mean(mean_LAI, na.rm = TRUE),
    sd_LAI = sd(mean_LAI, na.rm = TRUE),
    cv_LAI = (sd_LAI / meann_LAI) * 100,
    
    meann_B_Crested_m2 = mean(mean_B_Crested_m2, na.rm = TRUE),
    sd_B_Crested_m2 = sd(mean_B_Crested_m2, na.rm = TRUE),
    cv_B_Crested_m2 = (sd_B_Crested_m2 / meann_B_Crested_m2) * 100,
    
    meann_B_Native_m2 = mean(mean_B_Native_m2, na.rm = TRUE),
    sd_B_Native_m2 = sd(mean_B_Native_m2, na.rm = TRUE),
    cv_B_Native_m2 = (sd_B_Native_m2 / meann_B_Native_m2) * 100,
    
    meann_B_Grass_m2 = mean(mean_B_Grass_m2, na.rm = TRUE),
    sd_B_Grass_m2 = sd(mean_B_Grass_m2, na.rm = TRUE),
    cv_B_Grass_m2 = (sd_B_Grass_m2/ meann_B_Grass_m2) * 100,
    
    meann_B_Dead_m2 = mean(mean_B_Dead_m2, na.rm = TRUE),
    sd_B_Dead_m2 = sd(mean_B_Dead_m2, na.rm = TRUE),
    cv_B_Dead_m2 = (sd_B_Dead_m2/ meann_B_Dead_m2) * 100,
    
    meann_B_Forbs_m2 = mean(mean_B_Forbs_m2, na.rm = TRUE),
    sd_B_Forbs_m2 = sd(mean_B_Forbs_m2, na.rm = TRUE),
    cv_B_Forbs_m2 = (sd_B_Forbs_m2/ meann_B_Forbs_m2) * 100,
    
    meann_B_Shrub_m2 = mean(mean_B_Shrub_m2, na.rm = TRUE),
    sd_B_Shrub_m2 = sd(mean_B_Shrub_m2, na.rm = TRUE),
    cv_B_Shrub_m2 = (sd_B_Shrub_m2/ meann_B_Shrub_m2) * 100,
    
    meann_B_Total_m2 = mean(mean_B_Total_m2, na.rm = TRUE),
    sd_B_Total_m2 = sd(mean_B_Total_m2, na.rm = TRUE),
    cv_B_Total_m2 = (sd_B_Total_m2/ meann_B_Total_m2) * 100,
    
    meann_Wet_Biomas_m2 = mean(mean_Wet_Biomas_m2, na.rm = TRUE),
    sd_Wet_Biomas_m2 = sd(mean_Wet_Biomas_m2, na.rm = TRUE),
    cv_Wet_Biomas_m2 = (sd_Wet_Biomas_m2/ meann_Wet_Biomas_m2) * 100,
    
    meann_Moisture_m2 = mean(mean_Moisture_m2, na.rm = TRUE),
    sd_Moisture_m2 = sd(mean_Moisture_m2, na.rm = TRUE),
    cv_Moisture_m2 = (sd_Moisture_m2/ meann_Moisture_m2) * 100,
    
    .groups = 'drop'  # Avoids grouped output
  ) -> SpeciesWiseMeans

# View results
#View(SpeciesWiseMeans)
names(SpeciesWiseMeans)

#Export the file
#write.table(SpeciesWiseMeans, "D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data/SpeciesWiseDescriptiveStat.csv", sep = ",", row.names = FALSE)
#write.table(SpeciesWiseMeans, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data/SpeciesWiseDescriptiveStat.csv", sep = ",", row.names = FALSE)

## Done
##Lets create the graphs
##Combine & organize the file
combined_data <- data.frame(
  Site = c(descri$Site[!is.na(descri$mean_Cover_CW)], descri$Site[!is.na(descri$mean_Cover_N)]),
  Cover = c(na.omit(descri$mean_Cover_CW), na.omit(descri$mean_Cover_N)),
  Height = c(na.omit(descri$mean_H_CW), na.omit(descri$mean_H_N)),
  #LAI = c(na.omit(descri$mean_LAI)),
  Grassbiomass = c(na.omit(descri$mean_B_Crested_m2), na.omit(descri$mean_B_Native_m2)),
  #TotalBiomass = c(na.omit(descri$mean_B_Total_m2), na.omit(descri$mean_B_Total_m2)),
  #MoistureContent = c(na.omit(descri$mean_Moisture_m2), na.omit(descri$mean_Moisture_m2)),
  #WetBiomass = c(na.omit(descri$mean_Wet_Biomas_m2), na.omit(descri$mean_Wet_Biomas_m2)),
  Species = rep(c("Crested Wheatgrass", "Native grass"), 
    times = c(length(na.omit(descri$mean_Cover_CW)),  
      length(na.omit(descri$mean_Cover_N))    
    )
  )
)
#View(combined_data)
names(combined_data)

## Add the others column as well
combined_data$TotalBiomass<- descri$mean_B_Total_m2
combined_data$MoistureContent<- descri$mean_Moisture_m2
combined_data$WetBiomass<- descri$mean_Wet_Biomas_m2
combined_data$LAI<-descri$mean_LAI
combined_data$ForbsCover<-descri$mean_C_Forbs
combined_data$StandingDeadCover<-descri$mean_C_SD
combined_data$LitterCover<-descri$mean_C_Litter
combined_data$BareGroundCover<-descri$mean_C_BG
combined_data$ShrubCover<-descri$mean_C_Shrub
combined_data$MossCover<-descri$mean_C_Moss
combined_data$DeadBiomass<-descri$mean_B_Dead_m2
combined_data$ForbsBiomass<-descri$mean_B_Forbs_m2
combined_data$ShrubBiomass<-descri$mean_B_Shrub_m2
combined_data$ShrubBiomass<-descri$mean_B_Shrub_m2

#View(combined_data)
names(combined_data)

#write.table(combined_data, "D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data/PlotWiseDescriptiveStat.csv", sep = ",", row.names = FALSE)
#write.table(combined_data, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data/PlotWiseDescriptiveStat.csv", sep = ",", row.names = FALSE)

library(ggplot2)

# ### Now, lets create the graph and find which plots the outliers are located in!!!!!
# #1# Checking Outliers for Grass Cover
# outlier_sites_cover <- combined_data %>%
#   group_by(Species) %>%
#   mutate(outlier = Cover < quantile(Cover, 0.25) - 1.5 * IQR(Cover) |
#            Cover > quantile(Cover, 0.75) + 1.5 * IQR(Cover)) %>%
#   filter(outlier) %>%
#   select(Site, Cover, Species)
# outlier_sites_cover

# ## Ploting with labeled outliers_cover
# p1 <- ggplot(combined_data, aes(x = Species, y = Cover, fill = Species)) +
#   geom_boxplot() +
#   # Add mean markers (red diamonds)
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 23,           # Diamond shape
#     size = 3,
#     color = "black",      # Outline color
#     fill = "red",         # Fill color
#     show.legend = FALSE
#   ) +
#   # Add mean value labels (rounded to 1 decimal)
#   stat_summary(
#     aes(label = round(..y.., 1)),
#     fun = mean,
#     geom = "text",
#     vjust = -1.5,         # Position above mean marker
#     color = "black",
#     size = 3.5
#   ) +
#   # Original outlier labels
#   geom_text(
#     data = outlier_sites_cover,
#     aes(label = Site, x = Species, y = Cover),
#     position = position_jitter(width = 0.05, height = 0.02),
#     vjust = -0.2,
#     hjust = 0.5,
#     color = "red",
#     size = 3.5
#   ) +
#   labs(
#     title = "Grass Cover (%)",
#     y = "Grass Cover (%)",  # Added % for consistency
#     x = "Species"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers the title
#     legend.position = "none"                 # Removes the legend
#   )
# p1
# 
# #2# Checking Outliers for Height
# outlier_sites_Height <- combined_data %>%
#   group_by(Species) %>%
#   mutate(outlier = Height < quantile(Height, 0.25) - 1.5 * IQR(Height) |
#            Height > quantile(Height, 0.75) + 1.5 * IQR(Height)) %>%
#   filter(outlier) %>%
#   select(Site, Height, Species)
# 
# outlier_sites_Height ## C11 (73.1) and C19 (30.9)
# 
# ## Ploting with labeled outliers
# p2 <- ggplot(combined_data, aes(x = Species, y = Height, fill = Species)) +
#   geom_boxplot() +
#   # Add mean markers (red diamonds)
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 23,           # Diamond shape
#     size = 3,
#     color = "black",      # Outline color
#     fill = "red",         # Fill color
#     show.legend = FALSE
#   ) +
#   # Add mean value labels (rounded to 1 decimal)
#   stat_summary(
#     aes(label = round(..y.., 1)),
#     fun = mean,
#     geom = "text",
#     vjust = -1.5,         # Position above mean marker
#     color = "black",
#     size = 3.5
#   ) +
#   # Original outlier labels
#   geom_text(
#     data = outlier_sites_Height,
#     aes(label = Site, x = Species, y = Height),
#     position = position_jitter(width = 0.05, height = 0.02),
#     vjust = -0.2,
#     hjust = 0.5,
#     color = "red",
#     size = 3.5
#   ) +
#   labs(title = "Height (cm)",
#        y = "Height (cm)",  # Added units for clarity
#        x = "Species") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers the title
#     legend.position = "none"                 # Removes the legend
#   )
# 
# p2
# 
# #3# Checking Outliers for Grassbiomass
# outlier_sites_Grassbiomass <- combined_data %>%
#   group_by(Species) %>%
#   mutate(outlier = Grassbiomass < quantile(Grassbiomass, 0.25) - 1.5 * IQR(Grassbiomass) |
#            Grassbiomass > quantile(Grassbiomass, 0.75) + 1.5 * IQR(Grassbiomass)) %>%
#   filter(outlier) %>%
#   select(Site, Grassbiomass, Species)
# 
# outlier_sites_Grassbiomass #C19 (55)
# 
# ## Ploting with labeled outliers
# p3 <- ggplot(combined_data, aes(x = Species, y = Grassbiomass, fill = Species)) +
#   geom_boxplot() +
#   # Add mean markers (red diamonds)
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 23,           # Diamond shape
#     size = 3,
#     color = "black",      # Outline color
#     fill = "red",         # Fill color
#     show.legend = FALSE
#   ) +
#   # Add mean value labels (rounded to 1 decimal)
#   stat_summary(
#     aes(label = round(..y.., 1)),
#     fun = mean,
#     geom = "text",
#     vjust = -1.5,         # Position above mean marker
#     color = "black",
#     size = 3.5
#   ) +
#   # Original outlier labels
#   geom_text(
#     data = outlier_sites_Grassbiomass,
#     aes(label = Site, x = Species, y = Grassbiomass),
#     position = position_jitter(width = 0.05, height = 0.02),
#     vjust = -0.2,
#     hjust = 0.5,
#     color = "red",
#     size = 3.5
#   ) +
#   labs(title = "Grass Biomass (g/m²)",  # Corrected unit from gm/cm2 to g/m²
#        y = "Grass Biomass",             # Capitalized for consistency
#        x = "Species") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers the title
#     legend.position = "none"                 # Removes the legend
#   )
# p3
# 
# #4# Checking Outliers for TotalBiomass
# outlier_sites_TotalBiomass <- combined_data %>%
#   group_by(Species) %>%
#   mutate(outlier = TotalBiomass < quantile(TotalBiomass, 0.25) - 1.5 * IQR(TotalBiomass) |
#            TotalBiomass > quantile(TotalBiomass, 0.75) + 1.5 * IQR(TotalBiomass)) %>%
#   filter(outlier) %>%
#   select(Site, TotalBiomass, Species)
# 
# outlier_sites_TotalBiomass
# 
# ## Ploting with labeled outliers
# p4 <- ggplot(combined_data, aes(x = Species, y = TotalBiomass, fill = Species)) +
#   geom_boxplot() +
#   # Add mean markers (red diamonds)
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 23,           # Diamond shape
#     size = 3,
#     color = "black",      # Outline color
#     fill = "red",         # Fill color
#     show.legend = FALSE
#   ) +
#   # Add mean value labels (rounded to 1 decimal)
#   stat_summary(
#     aes(label = round(..y.., 1)),
#     fun = mean,
#     geom = "text",
#     vjust = -1.5,         # Position above mean marker
#     color = "black",
#     size = 3.5
#   ) +
#   # Original outlier labels
#   geom_text(
#     data = outlier_sites_TotalBiomass,
#     aes(label = Site, x = Species, y = TotalBiomass),
#     position = position_jitter(width = 0.05, height = 0.02),
#     vjust = -0.2,
#     hjust = 0.5,
#     color = "red",
#     size = 3.5
#   ) +
#   labs(title = "Total Dry Biomass (g/m²)",  # Corrected unit from gm/cm2 to g/m²
#        y = "Total Dry Biomass",             # Capitalized for consistency
#        x = "Species") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers the title
#     legend.position = "none"                 # Removes the legend
#   )
# p4
# 
# #5# Checking Outliers for MoistureContent
# outlier_sites_MoistureContent <- combined_data %>%
#   group_by(Species) %>%
#   mutate(outlier = MoistureContent < quantile(MoistureContent, 0.25) - 1.5 * IQR(MoistureContent) |
#            MoistureContent > quantile(MoistureContent, 0.75) + 1.5 * IQR(MoistureContent)) %>%
#   filter(outlier) %>%
#   select(Site, MoistureContent, Species)
# 
# outlier_sites_MoistureContent #C1(723) & C7(769)
# 
# ## Ploting with labeled outliers
# p5 <- ggplot(combined_data, aes(x = Species, y = MoistureContent, fill = Species)) +
#   geom_boxplot() +  # Single boxplot layer (removed duplicate)
#   # Add mean markers (red diamonds)
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 23,           # Diamond shape
#     size = 3,
#     color = "black",       # Outline color
#     fill = "red",          # Fill color
#     show.legend = FALSE
#   ) +
#   # Add mean value labels (rounded to 1 decimal)
#   stat_summary(
#     aes(label = round(..y.., 1)),
#     fun = mean,
#     geom = "text",
#     vjust = -1.5,          # Position above markers
#     color = "black",
#     size = 3.5
#   ) +
#   # Original outlier labels
#   geom_text(
#     data = outlier_sites_MoistureContent,
#     aes(label = Site, x = Species, y = MoistureContent),
#     position = position_jitter(width = 0.05, height = 0.02),
#     vjust = -0.2,
#     hjust = 0.5,
#     color = "red",
#     size = 3.5
#   ) +
#   labs(title = "Moisture Content (gm/m²)",  # Fixed unit formatting
#        y = "Moisture Content",
#        x = "Species") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centered title
#     legend.position = "none"                 # No legend
#   )
# p5
# 
# #6# Checking Outliers for WetBiomass
# outlier_sites_WetBiomass <- combined_data %>%
#   group_by(Species) %>%
#   mutate(outlier = WetBiomass < quantile(WetBiomass, 0.25) - 1.5 * IQR(WetBiomass) |
#            WetBiomass > quantile(WetBiomass, 0.75) + 1.5 * IQR(WetBiomass)) %>%
#   filter(outlier) %>%
#   select(Site, WetBiomass, Species)
# 
# outlier_sites_WetBiomass #C7 (1204)
# 
# ## Ploting with labeled outliers
# p6 <- ggplot(combined_data, aes(x = Species, y = WetBiomass, fill = Species)) +
#   geom_boxplot() +
#   # Add mean markers (red diamonds)
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 23,           # Diamond shape
#     size = 3,
#     color = "black",      # Outline color
#     fill = "red",         # Fill color
#     show.legend = FALSE
#   ) +
#   # Add mean value labels (rounded to 1 decimal)
#   stat_summary(
#     aes(label = round(..y.., 1)),
#     fun = mean,
#     geom = "text",
#     vjust = -1.5,         # Position above mean marker
#     color = "black",
#     size = 3.5
#   ) +
#   # Original outlier labels
#   geom_text(
#     data = outlier_sites_WetBiomass,
#     aes(label = Site, x = Species, y = WetBiomass),
#     position = position_jitter(width = 0.05, height = 0.02),
#     vjust = -0.2,
#     hjust = 0.5,
#     color = "red",
#     size = 3.5
#   ) +
#   labs(title = "Total Wet Biomass (gm/m²)",  # Note: Using proper superscript
#        y = "Total Wet Biomass",
#        x = "Species") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers the title
#     legend.position = "none"                 # Removes the legend
#   )
# 
# p6
# 
# #7# Checking Outliers for LAI
# outlier_sites_LAI <- combined_data %>%
#   group_by(Species) %>%
#   mutate(outlier = LAI < quantile(LAI, 0.25) - 1.5 * IQR(LAI) |
#            LAI > quantile(LAI, 0.75) + 1.5 * IQR(LAI)) %>%
#   filter(outlier) %>%
#   select(Site, LAI, Species)
# 
# outlier_sites_LAI #C1(2.85) & N10 (1.44)
# 
# ## Ploting with labeled outliers
# p7 <- ggplot(combined_data, aes(x = Species, y = LAI, fill = Species)) +
#   geom_boxplot() +
#   # Add mean markers (red diamonds)
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 23,           # Diamond shape
#     size = 3,
#     color = "black",      # Outline color
#     fill = "red",         # Fill color
#     show.legend = FALSE
#   ) +
#   # Add mean value labels (rounded to 2 decimals for LAI)
#   stat_summary(
#     aes(label = round(..y.., 2)),  # Using 2 decimals for LAI values
#     fun = mean,
#     geom = "text",
#     vjust = -1.5,         # Position above mean marker
#     color = "black",
#     size = 3.5
#   ) +
#   # Original outlier labels
#   geom_text(
#     data = outlier_sites_LAI,
#     aes(label = Site, x = Species, y = LAI),
#     position = position_jitter(width = 0.05, height = 0.02),
#     vjust = -0.2,
#     hjust = 0.5,
#     color = "red",
#     size = 3.5
#   ) +
#   labs(title = "Leaf Area Index",
#        y = "Leaf Area Index",
#        x = "Species") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers the title
#     legend.position = "none"                 # Removes the legend
#   )
# p7
# 
# 
# #8# Checking Outliers for ForbsCover
# outlier_sites_ForbsCover <- combined_data %>%
#   group_by(Species) %>%
#   mutate(outlier = ForbsCover < quantile(ForbsCover, 0.25) - 1.5 * IQR(ForbsCover) |
#            ForbsCover > quantile(ForbsCover, 0.75) + 1.5 * IQR(ForbsCover)) %>%
#   filter(outlier) %>%
#   select(Site, ForbsCover, Species)
# 
# outlier_sites_ForbsCover #N5 (20.6)
# 
# ## Ploting with labeled outliers
# p8 <- ggplot(combined_data, aes(x = Species, y = ForbsCover, fill = Species)) +
#   geom_boxplot() +
#   # Add mean markers (red diamonds)
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 23,           # Diamond shape
#     size = 3,
#     color = "black",      # Outline color
#     fill = "red",         # Fill color
#     show.legend = FALSE
#   ) +
#   # Add mean value labels (rounded to 1 decimal)
#   stat_summary(
#     aes(label = round(..y.., 1)),
#     fun = mean,
#     geom = "text",
#     vjust = -1.5,         # Position above mean marker
#     color = "black",
#     size = 3.5
#   ) +
#   # Original outlier labels
#   geom_text(
#     data = outlier_sites_ForbsCover,
#     aes(label = Site, x = Species, y = ForbsCover),
#     position = position_jitter(width = 0.05, height = 0.02),
#     vjust = -0.2,
#     hjust = 0.5,
#     color = "red",
#     size = 3.5
#   ) +
#   labs(title = "Forbs Cover (%)",
#        y = "Forbs Cover",
#        x = "Species") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers the title
#     legend.position = "none"                 # Removes the legend
#   )
# p8
# 
# 
# #9# Checking Outliers for StandingDeadCover
# outlier_sites_StandingDeadCover <- combined_data %>%
#   group_by(Species) %>%
#   mutate(outlier = StandingDeadCover < quantile(StandingDeadCover, 0.25) - 1.5 * IQR(StandingDeadCover) |
#            StandingDeadCover > quantile(StandingDeadCover, 0.75) + 1.5 * IQR(StandingDeadCover)) %>%
#   filter(outlier) %>%
#   select(Site, StandingDeadCover, Species)
# 
# outlier_sites_StandingDeadCover #C13 (25.6) & N10 (18.8)
# 
# ## Ploting with labeled outliers
# p9 <- ggplot(combined_data, aes(x = Species, y = StandingDeadCover, fill = Species)) +
#   geom_boxplot() +
#   # Add mean markers (red diamonds)
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 23,            # Diamond shape (23)
#     size = 3,
#     color = "black",       # Diamond outline
#     fill = "red",          # Diamond fill
#     show.legend = FALSE
#   ) +
#   # Add mean value labels (rounded to 1 decimal)
#   stat_summary(
#     aes(label = round(..y.., 1)),
#     fun = mean,
#     geom = "text",
#     vjust = -1.5,          # Position above markers
#     color = "black",
#     size = 3.5
#   ) +
#   # Original outlier labels
#   geom_text(
#     data = outlier_sites_StandingDeadCover,
#     aes(label = Site, x = Species, y = StandingDeadCover),
#     position = position_jitter(width = 0.05, height = 0.02),
#     vjust = -0.2,
#     hjust = 0.5,
#     color = "red",
#     size = 3.5
#   ) +
#   labs(title = "Standing Dead Cover (%)",
#        y = "Standing Dead Cover",
#        x = "Species") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centered title
#     legend.position = "none"                 # No legend
#   )
# p9
# 
# #10# Checking Outliers for LitterCover
# outlier_sites_LitterCover <- combined_data %>%
#   group_by(Species) %>%
#   mutate(outlier = LitterCover < quantile(LitterCover, 0.25) - 1.5 * IQR(LitterCover) |
#            LitterCover > quantile(LitterCover, 0.75) + 1.5 * IQR(LitterCover)) %>%
#   filter(outlier) %>%
#   select(Site, LitterCover, Species)
# 
# outlier_sites_LitterCover #C13 (25.6) & N10 (18.8)
# 
# ## Ploting with labeled outliers
# p10 <- ggplot(combined_data, aes(x = Species, y = LitterCover, fill = Species)) +
#   geom_boxplot() +
#   # Add mean markers (red diamonds)
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 23,            # Diamond shape
#     size = 3,
#     color = "black",       # Outline color
#     fill = "red",          # Fill color
#     show.legend = FALSE
#   ) +
#   # Add mean value labels (rounded to 1 decimal)
#   stat_summary(
#     aes(label = round(..y.., 1)),
#     fun = mean,
#     geom = "text",
#     vjust = -1.5,          # Position above markers
#     color = "black",
#     size = 3.5
#   ) +
#   # Original outlier labels
#   geom_text(
#     data = outlier_sites_LitterCover,
#     aes(label = Site, x = Species, y = LitterCover),
#     position = position_jitter(width = 0.05, height = 0.02),
#     vjust = -0.2,
#     hjust = 0.5,
#     color = "red",
#     size = 3.5
#   ) +
#   labs(title = "Litter Cover (%)",
#        y = "Litter Cover",
#        x = "Species") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centered title
#     legend.position = "none"                 # No legend
#   )
# p10
# 
# 
# #11# Checking Outliers for BareGroundCover
# outlier_sites_BareGroundCover <- combined_data %>%
#   group_by(Species) %>%
#   mutate(outlier = BareGroundCover < quantile(BareGroundCover, 0.25) - 1.5 * IQR(BareGroundCover) |
#            BareGroundCover > quantile(BareGroundCover, 0.75) + 1.5 * IQR(BareGroundCover)) %>%
#   filter(outlier) %>%
#   select(Site, BareGroundCover, Species)
# 
# outlier_sites_BareGroundCover#C13 (13.8)
# 
# ## Ploting with labeled outliers
# p11 <- ggplot(combined_data, aes(x = Species, y = BareGroundCover, fill = Species)) +
#   geom_boxplot() +
#   # Add mean markers (red diamonds)
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 23,            # Diamond shape
#     size = 3,
#     color = "black",       # Outline color
#     fill = "red",          # Fill color
#     show.legend = FALSE
#   ) +
#   # Add mean value labels (rounded to 1 decimal)
#   stat_summary(
#     aes(label = round(..y.., 1)),
#     fun = mean,
#     geom = "text",
#     vjust = -1.5,          # Position above markers
#     color = "black",
#     size = 3.5
#   ) +
#   # Original outlier labels
#   geom_text(
#     data = outlier_sites_BareGroundCover,
#     aes(label = Site, x = Species, y = BareGroundCover),
#     position = position_jitter(width = 0.05, height = 0.02),
#     vjust = -0.2,
#     hjust = 0.5,
#     color = "red",
#     size = 3.5
#   ) +
#   labs(title = "Bare Ground Cover (%)",
#        y = "Bare Ground Cover",
#        x = "Species") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centered title
#     legend.position = "none"                 # No legend
#   )
# p11
# 
# #12# Checking Outliers for ShrubCover
# outlier_sites_ShrubCover <- combined_data %>%
#   group_by(Species) %>%
#   mutate(outlier = ShrubCover < quantile(ShrubCover, 0.25) - 1.5 * IQR(ShrubCover) |
#            ShrubCover > quantile(ShrubCover, 0.75) + 1.5 * IQR(ShrubCover)) %>%
#   filter(outlier) %>%
#   select(Site, ShrubCover, Species)
# 
# outlier_sites_ShrubCover#C1 (2.5), C2(5.62) and & C6(4.38)
# 
# ## Ploting with labeled outliers
# p12 <- ggplot(combined_data, aes(x = Species, y = ShrubCover, fill = Species)) +
#   geom_boxplot() +
#   # Add mean markers (red diamonds)
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 23,           # Diamond shape
#     size = 3,
#     color = "black",      # Diamond outline
#     fill = "red",         # Diamond fill
#     show.legend = FALSE
#   ) +
#   # Add mean value labels (rounded to 1 decimal)
#   stat_summary(
#     aes(label = round(..y.., 1)),
#     fun = mean,
#     geom = "text",
#     vjust = -1.5,         # Position above mean marker
#     color = "black",
#     size = 3.5
#   ) +
#   # Original outlier labels
#   geom_text(
#     data = outlier_sites_ShrubCover,
#     aes(label = Site, x = Species, y = ShrubCover),
#     position = position_jitter(width = 0.05, height = 0.02),
#     vjust = -0.2,
#     hjust = 0.5,
#     color = "red",
#     size = 3.5
#   ) +
#   labs(title = "Shrub Cover (%)",
#        y = "Shrub Cover",
#        x = "Species") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     legend.position = "none"
#   )
# p12
# 
# 
# #13# Checking Outliers for MossCover
# outlier_sites_MossCover <- combined_data %>%
#   group_by(Species) %>%
#   mutate(outlier = MossCover < quantile(MossCover, 0.25) - 1.5 * IQR(MossCover) |
#            MossCover > quantile(MossCover, 0.75) + 1.5 * IQR(MossCover)) %>%
#   filter(outlier) %>%
#   select(Site, MossCover, Species)
# 
# outlier_sites_MossCover#C12 (0.625)
# 
# ## Ploting with labeled outliers
# p13 <- ggplot(combined_data, aes(x = Species, y = MossCover, fill = Species)) +
#   geom_boxplot() +
#   # Add mean markers (red diamonds)
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 23,           # Diamond shape
#     size = 3,
#     color = "black",      # Diamond outline
#     fill = "red",         # Diamond fill
#     show.legend = FALSE
#   ) +
#   # Add mean value labels (rounded to 1 decimal)
#   stat_summary(
#     aes(label = round(..y.., 1)),
#     fun = mean,
#     geom = "text",
#     vjust = -1.5,         # Position above mean marker
#     color = "black",
#     size = 3.5
#   ) +
#   # Original outlier labels
#   geom_text(
#     data = outlier_sites_MossCover,
#     aes(label = Site, x = Species, y = MossCover),
#     position = position_jitter(width = 0.05, height = 0.02),
#     vjust = -0.2,
#     hjust = 0.5,
#     color = "red",
#     size = 3.5
#   ) +
#   labs(title = "Moss Cover (%)",
#        y = "Moss Cover",
#        x = "Species") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     legend.position = "none"
#   )
# p13
# 
# 
# #14# Checking Outliers for ForbsBiomass
# outlier_sites_ForbsBiomass <- combined_data %>%
#   group_by(Species) %>%
#   mutate(outlier = ForbsBiomass < quantile(ForbsBiomass, 0.25) - 1.5 * IQR(ForbsBiomass) |
#            ForbsBiomass > quantile(ForbsBiomass, 0.75) + 1.5 * IQR(ForbsBiomass)) %>%
#   filter(outlier) %>%
#   select(Site, ForbsBiomass, Species)
# 
# outlier_sites_ForbsBiomass#C19(6.25), C20 (2.75), C6 (5.75) & N5 (41)
# 
# ## Ploting with labeled outliers
# p14 <- ggplot(combined_data, aes(x = Species, y = ForbsBiomass, fill = Species)) +
#   geom_boxplot() +
#   # Add red diamond markers for mean values
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 23,           # Diamond shape
#     size = 3,
#     color = "black",      # Outline color
#     fill = "red",         # Fill color
#     show.legend = FALSE   # Hide from legend
#   ) +
#   # Add numeric mean labels (rounded to 1 decimal)
#   stat_summary(
#     aes(label = round(..y.., 1)),  
#     fun = mean,
#     geom = "text",
#     vjust = -1.5,         # Position above mean marker
#     color = "black",
#     size = 3.5
#   ) +
#   # Original outlier labels
#   geom_text(
#     data = outlier_sites_ForbsBiomass,
#     aes(label = Site, x = Species, y = ForbsBiomass),
#     position = position_jitter(width = 0.05, height = 0.02),
#     vjust = -0.2,
#     hjust = 0.5,
#     color = "red",
#     size = 3.5
#   ) +
#   labs(title = "Forbs Biomass (gm/m²)",  # Note: Use "gm/m²" for correct units
#        y = "Forbs Biomass",
#        x = "Species") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     legend.position = "none"
#   )
# p14
# 
# 
# #15# Checking Outliers for ShrubBiomass
# outlier_sites_ShrubBiomass <- combined_data %>%
#   group_by(Species) %>%
#   mutate(outlier = ShrubBiomass < quantile(ShrubBiomass, 0.25) - 1.5 * IQR(ShrubBiomass) |
#            ShrubBiomass > quantile(ShrubBiomass, 0.75) + 1.5 * IQR(ShrubBiomass)) %>%
#   filter(outlier) %>%
#   select(Site, ShrubBiomass, Species)
# 
# outlier_sites_ShrubBiomass#C12 (0.625)
# 
# ## Ploting with labeled outliers
# p15 <- ggplot(combined_data, aes(x = Species, y = ShrubBiomass, fill = Species)) +
#   geom_boxplot() +
#   # Add mean markers (red diamonds)
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 23,       # Diamond shape
#     size = 3,
#     color = "black",
#     fill = "red",
#     show.legend = FALSE
#   ) +
#   # Optional: Add mean value labels
#   stat_summary(
#     aes(label = round(..y.., 1)),  # Rounds to 1 decimal
#     fun = mean,
#     geom = "text",
#     vjust = -1.5,     # Adjust vertical position
#     color = "black",
#     size = 3.5
#   ) +
#   geom_text(
#     data = outlier_sites_ShrubBiomass,
#     aes(label = Site, x = Species, y = ShrubBiomass),
#     position = position_jitter(width = 0.05, height = 0.02),
#     vjust = -0.2,
#     hjust = 0.5,
#     color = "red",
#     size = 3.5
#   ) +
#   labs(title = "Shrub Biomass (gm/m²)",
#        y = "Shrub Biomass",
#        x = "Species") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     legend.position = "none"
#   )
# p15
# 
# library(patchwork)
# # Combine the Cover graphs with a single legend at the bottom
# Cover_plot <- (
#   (p1 + p9) /
#     (p11 + p10) /
#     (p12 + p8) 
# ) + 
#   plot_layout(
#     heights = c(3, 3, 3),
#     guides = "collect"
#   ) & 
#   theme(
#     legend.position = "bottom",
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.title.x = element_blank()
#   )
# 
# # Print the Cover plot
# Cover_plot
# 
# 
# # Combine the Cover graphs with a single legend at the bottom
# Biomass_plot <- (
#   (p3 + p4) /
#     (p14 + p15)
# ) + 
#   plot_layout(
#     heights = c(3, 3),
#     guides = "collect"
#   ) & 
#   theme(
#     legend.position = "bottom",
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.title.x = element_blank()
#   )
# 
# # Print the Cover plot
# Biomass_plot
# 
# # Combine the Cover graphs with a single legend at the bottom
# Biopara_plot <- (
#   (p2 + p7) 
#   ) + 
#   plot_layout(
#     heights = c(3, 3),
#     guides = "collect"
#   ) & 
#   theme(
#     legend.position = "bottom",
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.title.x = element_blank()
#   )
# 
# # Print the Cover plot
# Biopara_plot
# 
# 
# ###Lets try the barplots
# ## Grass Cover
# pp1 <-ggplot(combined_data, aes(x = Species, y = Cover, fill = Species)) +
#   geom_bar(stat = "summary", fun = "mean", alpha = 0.7) +
#   geom_errorbar(
#     stat = "summary",
#     fun.data = mean_sdl,
#     fun.args = list(mult = 1),
#     width = 0.2
#   ) +
#   labs(title = "Grass Cover (%)",  # Fixed capitalization
#        x = "Species", 
#        y = "Grass cover (%)") +
#   scale_fill_manual(values = c("lightgreen", "lightblue")) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers title
#     legend.position = "none"                 # Removes legend
#   )
# pp1
# 
# ## Height (cm)
# pp2 <-ggplot(combined_data, aes(x = Species, y = Height, fill = Species)) +
#   geom_bar(stat = "summary", fun = "mean", alpha = 0.7) +
#   geom_errorbar(
#     stat = "summary",
#     fun.data = mean_sdl,
#     fun.args = list(mult = 1),
#     width = 0.2
#   ) +
#   labs(title = "Height (cm)",  # Fixed capitalization
#        x = "Species", 
#        y = "Height (cm)") +
#   scale_fill_manual(values = c("lightgreen", "lightblue")) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers title
#     legend.position = "none"                 # Removes legend
#   )
# pp2
# 
# ## Grass biomass
# pp3 <-ggplot(combined_data, aes(x = Species, y = Grassbiomass, fill = Species)) +
#   geom_bar(stat = "summary", fun = "mean", alpha = 0.7) +
#   geom_errorbar(
#     stat = "summary",
#     fun.data = mean_sdl,
#     fun.args = list(mult = 1),
#     width = 0.2
#   ) +
#   labs(title = "Grass biomass (gm/m2)",  # Fixed capitalization
#        x = "Species", 
#        y = "Grass biomass (gm/m2)") +
#   scale_fill_manual(values = c("lightgreen", "lightblue")) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers title
#     legend.position = "none"                 # Removes legend
#   )
# pp3
# 
# ## Total Biomass
# pp4 <-ggplot(combined_data, aes(x = Species, y = TotalBiomass, fill = Species)) +
#   geom_bar(stat = "summary", fun = "mean", alpha = 0.7) +
#   geom_errorbar(
#     stat = "summary",
#     fun.data = mean_sdl,
#     fun.args = list(mult = 1),
#     width = 0.2
#   ) +
#   labs(title = "Total biomass (gm/m2)",  # Fixed capitalization
#        x = "Species", 
#        y = "Total biomass (gm/m2)") +
#   scale_fill_manual(values = c("lightgreen", "lightblue")) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers title
#     legend.position = "none"                 # Removes legend
#   )
# pp4
# 
# ## Moisture Content
# pp5 <-ggplot(combined_data, aes(x = Species, y = MoistureContent, fill = Species)) +
#   geom_bar(stat = "summary", fun = "mean", alpha = 0.7) +
#   geom_errorbar(
#     stat = "summary",
#     fun.data = mean_sdl,
#     fun.args = list(mult = 1),
#     width = 0.2
#   ) +
#   labs(title = "Moisture Content (gm/m2)",  # Fixed capitalization
#        x = "Species", 
#        y = "Moisture Content (gm/m2)") +
#   scale_fill_manual(values = c("lightgreen", "lightblue")) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers title
#     legend.position = "none"                 # Removes legend
#   )
# pp5
# 
# ## Moisture Content
# pp6 <-ggplot(combined_data, aes(x = Species, y = WetBiomass, fill = Species)) +
#   geom_bar(stat = "summary", fun = "mean", alpha = 0.7) +
#   geom_errorbar(
#     stat = "summary",
#     fun.data = mean_sdl,
#     fun.args = list(mult = 1),
#     width = 0.2
#   ) +
#   labs(title = "Wet Biomass (gm/m2)",  # Fixed capitalization
#        x = "Species", 
#        y = "Wet Biomass (gm/m2)") +
#   scale_fill_manual(values = c("lightgreen", "lightblue")) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers title
#     legend.position = "none"                 # Removes legend
#   )
# pp6
# 
# ## LAI
# pp7 <-ggplot(combined_data, aes(x = Species, y = LAI, fill = Species)) +
#   geom_bar(stat = "summary", fun = "mean", alpha = 0.7) +
#   geom_errorbar(
#     stat = "summary",
#     fun.data = mean_sdl,
#     fun.args = list(mult = 1),
#     width = 0.2
#   ) +
#   labs(title = "Leaf area index",  # Fixed capitalization
#        x = "Species", 
#        y = "Leaf area index") +
#   scale_fill_manual(values = c("lightgreen", "lightblue")) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers title
#     legend.position = "none"                 # Removes legend
#   )
# pp7
# 
# ## Forbs Cover
# pp8 <-ggplot(combined_data, aes(x = Species, y = ForbsCover, fill = Species)) +
#   geom_bar(stat = "summary", fun = "mean", alpha = 0.7) +
#   geom_errorbar(
#     stat = "summary",
#     fun.data = mean_sdl,
#     fun.args = list(mult = 1),
#     width = 0.2
#   ) +
#   labs(title = "Forbs cover (%)",  # Fixed capitalization
#        x = "Species", 
#        y = "Forbs Cover (%)") +
#   scale_fill_manual(values = c("lightgreen", "lightblue")) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers title
#     legend.position = "none"                 # Removes legend
#   )
# pp8
# 
# ## Standing Dead Cover
# pp9 <-ggplot(combined_data, aes(x = Species, y = StandingDeadCover, fill = Species)) +
#   geom_bar(stat = "summary", fun = "mean", alpha = 0.7) +
#   geom_errorbar(
#     stat = "summary",
#     fun.data = mean_sdl,
#     fun.args = list(mult = 1),
#     width = 0.2
#   ) +
#   labs(title = "Standing dead cover (%)",  # Fixed capitalization
#        x = "Species", 
#        y = "Standing dead cover (%)") +
#   scale_fill_manual(values = c("lightgreen", "lightblue")) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers title
#     legend.position = "none"                 # Removes legend
#   )
# pp9
# 
# ## Litter cover
# pp10 <-ggplot(combined_data, aes(x = Species, y = LitterCover, fill = Species)) +
#   geom_bar(stat = "summary", fun = "mean", alpha = 0.7) +
#   geom_errorbar(
#     stat = "summary",
#     fun.data = mean_sdl,
#     fun.args = list(mult = 1),
#     width = 0.2
#   ) +
#   labs(title = "Litter cover (%)",  # Fixed capitalization
#        x = "Species", 
#        y = "Litter cover (%)") +
#   scale_fill_manual(values = c("lightgreen", "lightblue")) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers title
#     legend.position = "none"                 # Removes legend
#   )
# pp10
# 
# ## Bare ground cover 
# pp11 <-ggplot(combined_data, aes(x = Species, y = BareGroundCover, fill = Species)) +
#   geom_bar(stat = "summary", fun = "mean", alpha = 0.7) +
#   geom_errorbar(
#     stat = "summary",
#     fun.data = mean_sdl,
#     fun.args = list(mult = 1),
#     width = 0.2
#   ) +
#   labs(title = "Bare ground cover (%)",  # Fixed capitalization
#        x = "Species", 
#        y = "Bare ground cover (%)") +
#   scale_fill_manual(values = c("lightgreen", "lightblue")) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers title
#     legend.position = "none"                 # Removes legend
#   )
# pp11
# 
# ## Shrub cover
# pp12 <-ggplot(combined_data, aes(x = Species, y = ShrubCover, fill = Species)) +
#   geom_bar(stat = "summary", fun = "mean", alpha = 0.7) +
#   geom_errorbar(
#     stat = "summary",
#     fun.data = mean_sdl,
#     fun.args = list(mult = 1),
#     width = 0.2
#   ) +
#   labs(title = "Shrub cover (%)",  # Fixed capitalization
#        x = "Species", 
#        y = "Shrub cover (%)") +
#   scale_fill_manual(values = c("lightgreen", "lightblue")) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers title
#     legend.position = "none"                 # Removes legend
#   )
# pp12
# 
# ## Moss cover
# pp13 <-ggplot(combined_data, aes(x = Species, y = MossCover, fill = Species)) +
#   geom_bar(stat = "summary", fun = "mean", alpha = 0.7) +
#   geom_errorbar(
#     stat = "summary",
#     fun.data = mean_sdl,
#     fun.args = list(mult = 1),
#     width = 0.2
#   ) +
#   labs(title = "Moss cover (%)",  # Fixed capitalization
#        x = "Species", 
#        y = "Moss cover (%)") +
#   scale_fill_manual(values = c("lightgreen", "lightblue")) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers title
#     legend.position = "none"                 # Removes legend
#   )
# pp13
# 
# ## Forbs Biomass
# pp14 <-ggplot(combined_data, aes(x = Species, y = ForbsBiomass, fill = Species)) +
#   geom_bar(stat = "summary", fun = "mean", alpha = 0.7) +
#   geom_errorbar(
#     stat = "summary",
#     fun.data = mean_sdl,
#     fun.args = list(mult = 1),
#     width = 0.2
#   ) +
#   labs(title = "Forbs biomass (gm/m2)",  # Fixed capitalization
#        x = "Species", 
#        y = "Forbs biomass (gm/m2)") +
#   scale_fill_manual(values = c("lightgreen", "lightblue")) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers title
#     legend.position = "none"                 # Removes legend
#   )
# pp14
# 
# ## Shrub Biomass
# pp15 <-ggplot(combined_data, aes(x = Species, y = ShrubBiomass, fill = Species)) +
#   geom_bar(stat = "summary", fun = "mean", alpha = 0.7) +
#   geom_errorbar(
#     stat = "summary",
#     fun.data = mean_sdl,
#     fun.args = list(mult = 1),
#     width = 0.2
#   ) +
#   labs(title = "Shrub biomass (gm/m2)",  # Fixed capitalization
#        x = "Species", 
#        y = "Shrub biomass (gm/m2)") +
#   scale_fill_manual(values = c("lightgreen", "lightblue")) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Centers title
#     legend.position = "none"                 # Removes legend
#   )
# pp15
# 
# 
# library(patchwork)
# # Combine the Cover graphs with a single legend at the bottom
# Cover_pplot <- (
#   (pp1 + pp9) /
#     (pp11 + pp10) /
#     (pp12 + pp8) 
# ) + 
#   plot_layout(
#     heights = c(3, 3, 3),
#     guides = "collect"
#   ) & 
#   theme(
#     legend.position = "bottom",
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.title.x = element_blank()
#   )
# 
# # Print the Cover plot
# Cover_pplot
# 
# # Combine the Cover graphs with a single legend at the bottom
# Biomass_pplot <- (
#   (pp3 + pp4) /
#     (pp14 + pp15)
# ) + 
#   plot_layout(
#     heights = c(3, 3),
#     guides = "collect"
#   ) & 
#   theme(
#     legend.position = "bottom",
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.title.x = element_blank()
#   )
# 
# # Print the Cover plot
# Biomass_pplot
# 
# # Combine the Cover graphs with a single legend at the bottom
# Biopara_pplot <- (
#   (pp2 + pp7) 
# ) + 
#   plot_layout(
#     heights = c(3, 3),
#     guides = "collect"
#   ) & 
#   theme(
#     legend.position = "bottom",
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.title.x = element_blank()
#   )
# 
# # Print the Cover plot
# Biopara_pplot

#######################################################
######################################################
######################################################
# Note: I will be trying to produce all the test here. Also, will create the test graph using the codes that I have written
# one year ago using the ggstatplot package.

#View (combined_data)
names(combined_data)

# Subset data for the two species
crested <- subset(combined_data, Species == "Crested Wheatgrass")
#view(crested)
native <- subset(combined_data, Species == "Native grass")
#View(native)

# --------------------------------------------------------------------------
# Cover
# --------------------------------------------------------------------------

# Student’s t-test (assumes normality and equal variances)
t_test_Cover <- t.test(Cover ~ Species, data = combined_data, var.equal = TRUE)
print(t_test_Cover)

# Weltch t-test (assumes normality)
W_t_test_Cover <- t.test(Cover ~ Species, data = combined_data)
print(W_t_test_Cover)

# Mann-Whitney U test (non-parametric)
mw_test_Cover <- wilcox.test(Cover ~ Species, data = combined_data)
print(mw_test_Cover)

# --------------------------------------------------------------------------
# Height
# --------------------------------------------------------------------------

# Student’s t-test (assumes normality and equal variances)
t_test_height <- t.test(Height ~ Species, data = combined_data, var.equal = TRUE)
print(t_test_height)

# Weltch t-test (assumes normality)
W_t_test_height <- t.test(Height ~ Species, data = combined_data)
print(W_t_test_height)

# Mann-Whitney U test (non-parametric)
mw_test_height <- wilcox.test(Height ~ Species, data = combined_data)
print(mw_test_height)


# --------------------------------------------------------------------------
# Grassbiomass
# --------------------------------------------------------------------------

# Student’s t-test (assumes normality and equal variances)
t_test_Grassbiomass <- t.test(Grassbiomass ~ Species, data = combined_data, var.equal = TRUE)
print(t_test_Grassbiomass)

# Weltch t-test (assumes normality)
W_t_test_Grassbiomass <- t.test(Grassbiomass ~ Species, data = combined_data)
print(W_t_test_Grassbiomass)

# Mann-Whitney U test (non-parametric)
mw_test_Grassbiomass <- wilcox.test(Grassbiomass ~ Species, data = combined_data)
print(mw_test_Grassbiomass)


# --------------------------------------------------------------------------
# Total Biomass
# --------------------------------------------------------------------------

# Student’s t-test (assumes normality and equal variances)
t_test_TotalBiomass <- t.test(TotalBiomass ~ Species, data = combined_data, var.equal = TRUE)
print(t_test_TotalBiomass)

# Weltch t-test (assumes normality)
W_t_test_TotalBiomass <- t.test(TotalBiomass ~ Species, data = combined_data)
print(W_t_test_TotalBiomass)

# Mann-Whitney U test (non-parametric)
mw_test_TotalBiomass <- wilcox.test(TotalBiomass ~ Species, data = combined_data)
print(mw_test_TotalBiomass)


# --------------------------------------------------------------------------
# Leaf area index
# --------------------------------------------------------------------------

# Student’s t-test (assumes normality and equal variances)
t_test_LAI <- t.test(LAI ~ Species, data = combined_data, var.equal = TRUE)
print(t_test_LAI)

# Weltch t-test (assumes normality)
W_t_test_LAI <- t.test(LAI ~ Species, data = combined_data)
print(W_t_test_LAI)

# Mann-Whitney U test (non-parametric)
mw_test_LAI <- wilcox.test(LAI ~ Species, data = combined_data)
print(mw_test_LAI)


# --------------------------------------------------------------------------
# Forbs Cover
# --------------------------------------------------------------------------

# Student’s t-test (assumes normality and equal variances)
t_test_ForbsCover <- t.test(ForbsCover ~ Species, data = combined_data, var.equal = TRUE)
print(t_test_ForbsCover)

# Weltch t-test (assumes normality)
W_t_test_ForbsCover <- t.test(ForbsCover ~ Species, data = combined_data)
print(W_t_test_ForbsCover)

# Mann-Whitney U test (non-parametric)
mw_test_ForbsCover <- wilcox.test(ForbsCover ~ Species, data = combined_data)
print(mw_test_ForbsCover)


# --------------------------------------------------------------------------
# Standing Dead Cover
# --------------------------------------------------------------------------

# Student’s t-test (assumes normality and equal variances)
t_test_StandingDeadCover <- t.test(StandingDeadCover ~ Species, data = combined_data, var.equal = TRUE)
print(t_test_StandingDeadCover)

# Weltch t-test (assumes normality)
W_t_test_StandingDeadCover <- t.test(StandingDeadCover ~ Species, data = combined_data)
print(W_t_test_StandingDeadCover)

# Mann-Whitney U test (non-parametric)
mw_test_StandingDeadCover <- wilcox.test(StandingDeadCover ~ Species, data = combined_data)
print(mw_test_StandingDeadCover)


# --------------------------------------------------------------------------
# LitterCover
# --------------------------------------------------------------------------

# Student’s t-test (assumes normality and equal variances)
t_test_LitterCover <- t.test(LitterCover ~ Species, data = combined_data, var.equal = TRUE)
print(t_test_LitterCover)

# Weltch t-test (assumes normality)
W_t_test_LitterCover <- t.test(LitterCover ~ Species, data = combined_data)
print(W_t_test_LitterCover)

# Mann-Whitney U test (non-parametric)
mw_test_LitterCover<- wilcox.test(LitterCover ~ Species, data = combined_data)
print(mw_test_LitterCover)


# --------------------------------------------------------------------------
# BareGroundCover
# --------------------------------------------------------------------------

# Student’s t-test (assumes normality and equal variances)
t_test_BareGroundCover <- t.test(BareGroundCover ~ Species, data = combined_data, var.equal = TRUE)
print(t_test_BareGroundCover)

# Weltch t-test (assumes normality)
W_t_test_BareGroundCover <- t.test(BareGroundCover ~ Species, data = combined_data)
print(W_t_test_BareGroundCover)

# Mann-Whitney U test (non-parametric)
mw_test_BareGroundCover<- wilcox.test(BareGroundCover ~ Species, data = combined_data)
print(mw_test_BareGroundCover)


# --------------------------------------------------------------------------
# ShrubCover
# --------------------------------------------------------------------------

# Student’s t-test (assumes normality and equal variances)
t_test_ShrubCover <- t.test(ShrubCover ~ Species, data = combined_data, var.equal = TRUE)
print(t_test_ShrubCover)

# Weltch t-test (assumes normality)
W_t_test_ShrubCover <- t.test(ShrubCover ~ Species, data = combined_data)
print(W_t_test_ShrubCover)

# Mann-Whitney U test (non-parametric)
mw_test_ShrubCover<- wilcox.test(ShrubCover ~ Species, data = combined_data)
print(mw_test_ShrubCover)


# --------------------------------------------------------------------------
# DeadBiomass
# --------------------------------------------------------------------------

# Student’s t-test (assumes normality and equal variances)
t_test_DeadBiomass <- t.test(DeadBiomass ~ Species, data = combined_data, var.equal = TRUE)
print(t_test_DeadBiomass)

# Weltch t-test (assumes normality)
W_t_test_DeadBiomass <- t.test(DeadBiomass ~ Species, data = combined_data)
print(W_t_test_DeadBiomass)

# Mann-Whitney U test (non-parametric)
mw_test_DeadBiomass<- wilcox.test(DeadBiomass ~ Species, data = combined_data)
print(mw_test_DeadBiomass)


# --------------------------------------------------------------------------
# ForbsBiomass
# --------------------------------------------------------------------------

# Student’s t-test (assumes normality and equal variances)
t_test_ForbsBiomass <- t.test(ForbsBiomass ~ Species, data = combined_data, var.equal = TRUE)
print(t_test_ForbsBiomass)

# Weltch’s t-test (assumes normality and equal variances)
t_test_ForbsBiomass <- t.test(ForbsBiomass ~ Species, data = combined_data)
print(t_test_ForbsBiomass)

# Mann-Whitney U test (non-parametric)
mw_test_ForbsBiomass<- wilcox.test(ForbsBiomass ~ Species, data = combined_data)
print(mw_test_ForbsBiomass)


# --------------------------------------------------------------------------
# ShrubBiomass
# --------------------------------------------------------------------------

# Student’s t-test (assumes normality and equal variances)
t_test_ShrubBiomass <- t.test(ShrubBiomass ~ Species, data = combined_data, var.equal = TRUE)
print(t_test_ShrubBiomass)

# Weltch’s t-test (assumes normality and equal variances)
t_test_ShrubBiomass <- t.test(ShrubBiomass ~ Species, data = combined_data)
print(t_test_ShrubBiomass)

# Mann-Whitney U test (non-parametric)
mw_test_ShrubBiomass<- wilcox.test(ShrubBiomass ~ Species, data = combined_data)
print(mw_test_ShrubBiomass)



#################################################################################
#############################Assumption testing##################################
#################################################################################
##
## Grass Cover (%)
##
library(ggpubr)      
A<-ggqqplot(
  data = combined_data, 
  x = "Cover", 
  facet.by = "Species",  # Separate plots for CW and NG
  title = "Grass cover (%)"
)
A

# Split data by species
split_data_cover <- split(combined_data$Cover, 
                    combined_data$Species)
#View(split_data_cover)

# Run Shapiro-Wilk test for each species
lapply(split_data_cover, shapiro.test)

library(car)
leveneTest(Cover ~ Species, data = combined_data)

##
## Height (cm)
##
library(ggpubr)      
B<-ggqqplot(
  data = combined_data, 
  x = "Height", 
  facet.by = "Species",
  title = "Height (cm)"
)
B

# Split data by species
split_data_height <- split(combined_data$Height, 
                          combined_data$Species)
#View(split_data_height)

# Run Shapiro-Wilk test for each species
lapply(split_data_height, shapiro.test)

library(car)
leveneTest(Height ~ Species, data = combined_data)

##
## LAI
##
library(ggpubr)      
C<-ggqqplot(
  data = combined_data, 
  x = "LAI", 
  facet.by = "Species",
  title = "Leaf area index"
)
C

# Split data by species
split_data_LAI <- split(combined_data$LAI, 
                           combined_data$Species)
#View(split_data_LAI)

# Run Shapiro-Wilk test for each species
lapply(split_data_LAI, shapiro.test)

library(car)
leveneTest(LAI ~ Species, data = combined_data)

##
## Grassbiomass
##
library(ggpubr)      
D<-ggqqplot(
  data = combined_data, 
  x = "Grassbiomass", 
  facet.by = "Species",
  title = "Grass biomass (gm/m2)"
)
D

# Split data by species
split_data_Grassbiomass <- split(combined_data$Grassbiomass, 
                        combined_data$Species)
#View(split_data_Grassbiomass)

# Run Shapiro-Wilk test for each species
lapply(split_data_Grassbiomass, shapiro.test)

library(car)
leveneTest(Grassbiomass ~ Species, data = combined_data)

##
## Total Biomass
##
library(ggpubr)      
E<-ggqqplot(
  data = combined_data, 
  x = "TotalBiomass", 
  facet.by = "Species",
  title = "Total biomass (gm/m2)"
)
E

# Split data by species
split_data_Totalbiomass <- split(combined_data$TotalBiomass, 
                                 combined_data$Species)
#View(split_data_TotalBiomass)

# Run Shapiro-Wilk test for each species
lapply(split_data_Totalbiomass, shapiro.test)

library(car)
leveneTest(TotalBiomass ~ Species, data = combined_data)
##
## Dead Biomass
##
library(ggpubr)      
F<-ggqqplot(
  data = combined_data, 
  x = "DeadBiomass", 
  facet.by = "Species",
  title = "Dead biomass (gm/m2)"
)
F

# Split data by species
split_data_DeadBiomass <- split(combined_data$DeadBiomass, 
                                 combined_data$Species)
#View(split_data_DeadBiomass)

# Run Shapiro-Wilk test for each species
lapply(split_data_DeadBiomass, shapiro.test)

library(car)
leveneTest(DeadBiomass ~ Species, data = combined_data)
##
## Forbs Biomass
##
library(ggpubr)      
G<-ggqqplot(
  data = combined_data, 
  x = "ForbsBiomass", 
  facet.by = "Species",
  title = "Forbs biomass (gm/m2)"
)
G

# Split data by species
split_data_ForbsBiomass <- split(combined_data$ForbsBiomass, 
                                combined_data$Species)
#View(split_data_ForbsBiomass)

# Run Shapiro-Wilk test for each species
lapply(split_data_ForbsBiomass, shapiro.test) #Not normal

library(car)
leveneTest(ForbsBiomass ~ Species, data = combined_data) # unequal variances

##
## Bare Ground Cover
##
library(ggpubr)      
H<-ggqqplot(
  data = combined_data, 
  x = "BareGroundCover", 
  facet.by = "Species",
  title = "Bare ground cover (%)"
)
H

# Split data by species
split_data_BareGroundCover <- split(combined_data$BareGroundCover, 
                                 combined_data$Species)
#View(split_data_BareGroundCover)

# Run Shapiro-Wilk test for each species
lapply(split_data_BareGroundCover, shapiro.test)

library(car)
leveneTest(BareGroundCover~ Species, data = combined_data)

##
## ForbsCover
##
library(ggpubr)      
I<-ggqqplot(
  data = combined_data, 
  x = "ForbsCover", 
  facet.by = "Species",
  title = "Forbs cover (%)"
)
I

# Split data by species
split_data_ForbsCover <- split(combined_data$ForbsCover, 
                                    combined_data$Species)
#View(split_data_ForbsCover)

# Run Shapiro-Wilk test for each species
lapply(split_data_ForbsCover, shapiro.test)

library(car)
leveneTest(ForbsCover~ Species, data = combined_data)


##
## StandingDeadCover
##
library(ggpubr)      
SD<-ggqqplot(
  data = combined_data, 
  x = "StandingDeadCover", 
  facet.by = "Species",
  title = "Standing dead cover (%)"
)
SD

# Split data by species
split_data_StandingDeadCover <- split(combined_data$StandingDeadCover, 
                               combined_data$Species)
#View(split_data_StandingDeadCover)

# Run Shapiro-Wilk test for each species
lapply(split_data_StandingDeadCover, shapiro.test)

library(car)
leveneTest(StandingDeadCover~ Species, data = combined_data)

##
## ShrubCover
##
library(ggpubr)      
SC<-ggqqplot(
  data = combined_data, 
  x = "ShrubCover", 
  facet.by = "Species",
  title = "Shrub cover (%)"
)
SC

# Split data by species
split_data_ShrubCover <- split(combined_data$ShrubCover, 
                                      combined_data$Species)
#View(split_data_ShrubCover)

# Run Shapiro-Wilk test for each species
lapply(split_data_ShrubCover, shapiro.test)

library(car)
leveneTest(ShrubCover~ Species, data = combined_data)


##
## ShrubBiomass
##
library(ggpubr)      
SB<-ggqqplot(
  data = combined_data, 
  x = "ShrubBiomass", 
  facet.by = "Species",
  title = "Shrub Biomass (gm/m2)"
)
SB

# Split data by species
split_data_ShrubBiomass <- split(combined_data$ShrubBiomass, 
                               combined_data$Species)
#View(split_data_ShrubBiomass)

# Run Shapiro-Wilk test for each species
lapply(split_data_ShrubBiomass, shapiro.test)

library(car)
leveneTest(ShrubBiomass~ Species, data = combined_data)


##
## LitterCover
##
library(ggpubr)      
LC<-ggqqplot(
  data = combined_data, 
  x = "LitterCover", 
  facet.by = "Species",
  title = "Litter Cover (%)"
)
LC

# Split data by species
split_data_LitterCover <- split(combined_data$LitterCover, 
                                 combined_data$Species)
#View(split_data_LitterCover)

# Run Shapiro-Wilk test for each species
lapply(split_data_LitterCover, shapiro.test)

library(car)
leveneTest(LitterCover~ Species, data = combined_data)

#################
#Lets combine them
################
library(patchwork)
# Combine the Cover graphs with a single legend at the bottom
QQPlot <- (
  (A + B + C) /
    (D + E + F) /
    (G + H + I) /
    (SD + SC + LC) /
    SB
) +
  plot_layout(
    guides = "collect",   # collect legends
    heights = c(1, 1, 1, 1, 1)  # 5 rows, equal height
  ) &
  theme(
    legend.position = "bottom"
  )

# Print the Cover plot
QQPlot

# ##############################Student t test graph###############################
# ###############################Student t test graph###############################
# ##############################Student t test graph################################
# 
# library(ggstatsplot)
# 
# GC <- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = Cover,
#   type = "parametric",
#   var.equal = TRUE,
#   results.subtitle = FALSE
#   
# )+
#   labs(y = "Grass cover (%)")
# GC
# 
# H<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = Height,
#   type = "parametric",
#   var.equal = TRUE,
#   results.subtitle = FALSE
# )+
#   labs(y = "Height (cm)")
# H
# 
# GB<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = Grassbiomass,
#   type = "parametric",
#   var.equal = TRUE,
#   results.subtitle = FALSE
# )+
#   labs(y = "Grass Biomass (gm/m2)")
# GB
# 
# 
# TB<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = TotalBiomass,
#   type = "parametric",
#   var.equal = TRUE
# )+
#   labs(y = "Total Biomass (gm/m2)")
# TB
# 
# LAI<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = LAI,
#   type = "parametric",
#   var.equal = TRUE,
#   results.subtitle = FALSE
# )+
#   labs(y = "Leaf area index")
# LAI
# 
# FC<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = ForbsCover,
#   type = "parametric",
#   var.equal = TRUE
# )+
#   labs(y = "Forbs Cover (%)")
# FC
# 
# SDC<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = StandingDeadCover,
#   type = "parametric",
#   var.equal = TRUE
# )+
#   labs(y = "Standing Cover (%)")
# SDC
# 
# LC<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = LitterCover,
#   type = "parametric",
#   var.equal = TRUE
# )+
#   labs(y = "Litter Cover (%)")
# LC
# 
# BGC<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = BareGroundCover,
#   type = "parametric",
#   var.equal = TRUE,
#   results.subtitle = FALSE
# )+
#   labs(y = "Baregound Cover (%)")
# BGC
# 
# SC<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = ShrubCover,
#   type = "parametric",
#   var.equal = TRUE
# )+
#   labs(y = "Shrub Cover (%)")
# SC
# 
# DB<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = DeadBiomass,
#   type = "parametric",
#   var.equal = TRUE,
#   results.subtitle = FALSE
# )+
#   labs(y = "Dead biomass (gm/m2)")
# DB
# 
# SB<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = ShrubBiomass,
#   type = "parametric",
#   var.equal = TRUE
# )+
#   labs(y = "Shrub biomass (gm/m2)")
# SB
# 
# FB<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = ForbsBiomass,
#   type = "parametric",
#   var.equal = TRUE
# )+
#   labs(y = "Forbs biomass (gm/m2)")
# FB
# 
# #################
# #Lets combine them
# ################
# library(patchwork)
# # Combine the Cover graphs with a single legend at the bottom
# Cover_ppplot <- (
#   (GC + SDC) /
#     (BGC + LC) /
#     (SC + FC) 
# ) + 
#   plot_layout(
#     heights = c(3, 3, 3),
#     guides = "collect"
#   ) & 
#   theme(
#     legend.position = "bottom",
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.title.x = element_blank()
#   )
# 
# # Print the Cover plot
# Cover_ppplot
# 
# #################
# #FOr biomass
# ################
# library(patchwork)
# # Define the layout with each plot labeled uniquely
# biomass_ppplot <- (
#   (GB + TB) /
#     (FB + SB) / 
#     DB
# ) + 
#   plot_layout(
#     ncol = 1,           # Arrange in 1 column
#     heights = c(3, 3, 4),  # Adjust heights as needed
#     guides = "collect"
#   ) & 
#   theme(
#     legend.position = "bottom",
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.title.x = element_blank()
#   )
# 
# # Print the plot
# biomass_ppplot
# 
# #################
# #Other biophysical
# ################
# library(patchwork)
# # Combine the Cover graphs with a single legend at the bottom
# other_bio_ppplot <- (
#   (H + LAI) 
# ) + 
#   plot_layout(
#     heights = c(3),
#     guides = "collect"
#   ) & 
#   theme(
#     legend.position = "bottom",
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.title.x = element_blank()
#   )
# 
# # Print the Cover plot
# other_bio_ppplot
###############################Weltch t test###############################
###############################Weltch t test###############################
##############################Weltch t test################################

library(ggstatsplot)

GC <- ggbetweenstats(
  data = combined_data,
  x = Species,
  y = Cover,
  type = "parametric",
  var.equal = FALSE,
  results.subtitle = FALSE
  
)+
  labs(y = "Grass cover (%)")+
  theme(
    # Increase axis title font size
    axis.title.x = element_text(size = 14),  # Species (x-axis)
    axis.title.y = element_text(size = 14),  # Forbs biomass (y-axis)
    
    # Increase axis text (tick labels) font size
    axis.text.x = element_text(size = 12),   # Species labels
    axis.text.y = element_text(size = 12),   # Y-axis numbers
    
    # Increase legend text size (if legend exists)
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

GC

H<- ggbetweenstats(
  data = combined_data,
  x = Species,
  y = Height,
  type = "parametric",
  var.equal = FALSE,
  results.subtitle = FALSE
)+
  labs(y = "Height (cm)")+
  theme(
    # Increase axis title font size
    axis.title.x = element_text(size = 14),  # Species (x-axis)
    axis.title.y = element_text(size = 14),  # Forbs biomass (y-axis)
    
    # Increase axis text (tick labels) font size
    axis.text.x = element_text(size = 12),   # Species labels
    axis.text.y = element_text(size = 12),   # Y-axis numbers
    
    # Increase legend text size (if legend exists)
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

H

GB<- ggbetweenstats(
  data = combined_data,
  x = Species,
  y = Grassbiomass,
  type = "parametric",
  var.equal = FALSE,
  results.subtitle = FALSE
)+
  labs(y = "Grass Biomass (gm/m2)")+
  theme(
    # Increase axis title font size
    axis.title.x = element_text(size = 14),  # Species (x-axis)
    axis.title.y = element_text(size = 14),  # Forbs biomass (y-axis)
    
    # Increase axis text (tick labels) font size
    axis.text.x = element_text(size = 12),   # Species labels
    axis.text.y = element_text(size = 12),   # Y-axis numbers
    
    # Increase legend text size (if legend exists)
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

GB


TB<- ggbetweenstats(
  data = combined_data,
  x = Species,
  y = TotalBiomass,
  type = "parametric",
  var.equal = FALSE,
  results.subtitle = FALSE
)+
  labs(y = "Total Biomass (gm/m2)")+
  theme(
    # Increase axis title font size
    axis.title.x = element_text(size = 14),  # Species (x-axis)
    axis.title.y = element_text(size = 14),  # Forbs biomass (y-axis)
    
    # Increase axis text (tick labels) font size
    axis.text.x = element_text(size = 12),   # Species labels
    axis.text.y = element_text(size = 12),   # Y-axis numbers
    
    # Increase legend text size (if legend exists)
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

TB

LAI<- ggbetweenstats(
  data = combined_data,
  x = Species,
  y = LAI,
  type = "parametric",
  var.equal = FALSE,
  results.subtitle = FALSE
)+
  labs(y = "Leaf area index")+
  theme(
    # Increase axis title font size
    axis.title.x = element_text(size = 14),  # Species (x-axis)
    axis.title.y = element_text(size = 14),  # Forbs biomass (y-axis)
    
    # Increase axis text (tick labels) font size
    axis.text.x = element_text(size = 12),   # Species labels
    axis.text.y = element_text(size = 12),   # Y-axis numbers
    
    # Increase legend text size (if legend exists)
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

LAI

FC<- ggbetweenstats(
  data = combined_data,
  x = Species,
  y = ForbsCover,
  type = "parametric",
  var.equal = FALSE,
  results.subtitle = FALSE
)+
  labs(y = "Forbs Cover (%)")+
  theme(
    # Increase axis title font size
    axis.title.x = element_text(size = 14),  # Species (x-axis)
    axis.title.y = element_text(size = 14),  # Forbs biomass (y-axis)
    
    # Increase axis text (tick labels) font size
    axis.text.x = element_text(size = 12),   # Species labels
    axis.text.y = element_text(size = 12),   # Y-axis numbers
    
    # Increase legend text size (if legend exists)
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

FC

SDC<- ggbetweenstats(
  data = combined_data,
  x = Species,
  y = StandingDeadCover,
  type = "parametric",
  var.equal = FALSE,
  results.subtitle = FALSE
)+
  labs(y = "Standing Cover (%)")+
  theme(
    # Increase axis title font size
    axis.title.x = element_text(size = 14),  # Species (x-axis)
    axis.title.y = element_text(size = 14),  # Forbs biomass (y-axis)
    
    # Increase axis text (tick labels) font size
    axis.text.x = element_text(size = 12),   # Species labels
    axis.text.y = element_text(size = 12),   # Y-axis numbers
    
    # Increase legend text size (if legend exists)
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

SDC

LC<- ggbetweenstats(
  data = combined_data,
  x = Species,
  y = LitterCover,
  type = "parametric",
  var.equal = FALSE,
  results.subtitle = FALSE
)+
  labs(y = "Litter Cover (%)")+
  theme(
    # Increase axis title font size
    axis.title.x = element_text(size = 14),  # Species (x-axis)
    axis.title.y = element_text(size = 14),  # Forbs biomass (y-axis)
    
    # Increase axis text (tick labels) font size
    axis.text.x = element_text(size = 12),   # Species labels
    axis.text.y = element_text(size = 12),   # Y-axis numbers
    
    # Increase legend text size (if legend exists)
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

LC

BGC<- ggbetweenstats(
  data = combined_data,
  x = Species,
  y = BareGroundCover,
  type = "parametric",
  var.equal = FALSE,
  results.subtitle = FALSE
)+
  labs(y = "Bare ground Cover (%)")+
  theme(
    # Increase axis title font size
    axis.title.x = element_text(size = 14),  # Species (x-axis)
    axis.title.y = element_text(size = 14),  # Forbs biomass (y-axis)
    
    # Increase axis text (tick labels) font size
    axis.text.x = element_text(size = 12),   # Species labels
    axis.text.y = element_text(size = 12),   # Y-axis numbers
    
    # Increase legend text size (if legend exists)
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

BGC

SC<- ggbetweenstats(
  data = combined_data,
  x = Species,
  y = ShrubCover,
  type = "parametric",
  var.equal = FALSE,
  results.subtitle = FALSE
)+
  labs(y = "Shrub Cover (%)")+
  theme(
    # Increase axis title font size
    axis.title.x = element_text(size = 14),  # Species (x-axis)
    axis.title.y = element_text(size = 14),  # Forbs biomass (y-axis)
    
    # Increase axis text (tick labels) font size
    axis.text.x = element_text(size = 12),   # Species labels
    axis.text.y = element_text(size = 12),   # Y-axis numbers
    
    # Increase legend text size (if legend exists)
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

SC

DB<- ggbetweenstats(
  data = combined_data,
  x = Species,
  y = DeadBiomass,
  type = "parametric",
  var.equal = FALSE,
  results.subtitle = FALSE
)+
  labs(y = "Dead biomass (gm/m2)")+
  theme(
    # Increase axis title font size
    axis.title.x = element_text(size = 14),  # Species (x-axis)
    axis.title.y = element_text(size = 14),  # Forbs biomass (y-axis)
    
    # Increase axis text (tick labels) font size
    axis.text.x = element_text(size = 12),   # Species labels
    axis.text.y = element_text(size = 12),   # Y-axis numbers
    
    # Increase legend text size (if legend exists)
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

DB

SB<- ggbetweenstats(
  data = combined_data,
  x = Species,
  y = ShrubBiomass,
  type = "parametric",
  var.equal = FALSE,
  results.subtitle = FALSE
)+
  labs(y = "Shrub biomass (gm/m2)")+
  theme(
    # Increase axis title font size
    axis.title.x = element_text(size = 14),  # Species (x-axis)
    axis.title.y = element_text(size = 14),  # Forbs biomass (y-axis)
    
    # Increase axis text (tick labels) font size
    axis.text.x = element_text(size = 12),   # Species labels
    axis.text.y = element_text(size = 12),   # Y-axis numbers
    
    # Increase legend text size (if legend exists)
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

SB

FB<- ggbetweenstats(
  data = combined_data,
  x = Species,
  y = ForbsBiomass,
  type = "parametric",
  var.equal = FALSE,
  results.subtitle = FALSE
)+
  labs(y = "Forbs biomass (gm/m2)")+
  theme(
    # Increase axis title font size
    axis.title.x = element_text(size = 14),  # Species (x-axis)
    axis.title.y = element_text(size = 14),  # Forbs biomass (y-axis)
    
    # Increase axis text (tick labels) font size
    axis.text.x = element_text(size = 12),   # Species labels
    axis.text.y = element_text(size = 12),   # Y-axis numbers
    
    # Increase legend text size (if legend exists)
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

FB

#################
#Lets combine them
################

library(patchwork)
# Combine the SIGNIFICANT graphs with a single legend at the bottom
Cover_ppplot_all_sig <- (
  (GC + BGC+ H) /
    (LAI + GB+ TB) /
    (DB+plot_spacer()) 
) + 
  plot_layout(
    heights = c(3, 3, 3),
    guides = "collect"
  ) & 
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )

# Print the Cover plot
Cover_ppplot_all_sig

library(patchwork)
# Combine the Cover graphs with a single legend at the bottom
Cover_ppplot <- (
  (GC + SDC) /
    (BGC + LC) /
    (SC + FC) 
) + 
  plot_layout(
    heights = c(3, 3, 3),
    guides = "collect"
  ) & 
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )

# Print the Cover plot
Cover_ppplot

#################
#FOr biomass
################
library(patchwork)
# Combine the Cover graphs with a single legend at the bottom
biomass_ppplot <- (
  (GB + TB) /
    (FB + SB) 
) + 
  plot_layout(
    heights = c(3, 3),
    guides = "collect"
  ) & 
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )

# Print the Cover plot
biomass_ppplot


#################
#Other biophysical
################
library(patchwork)
# Combine the Cover graphs with a single legend at the bottom
other_bio_ppplot <- (
  (H + LAI) 
) + 
  plot_layout(
    heights = c(3),
    guides = "collect"
  ) & 
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )

# Print the Cover plot
other_bio_ppplot
# ###############################Man wintny U test###############################
# ###############################Man wintny U test###############################
# ##############################Man wintny U test################################
# 
# library(ggstatsplot)
# 
# GC <- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = Cover,
#   type = "nonparametric"
#  
# )+
#   labs(y = "Grass cover (%)")
# GC
# 
# ggboxplot(combined_data, x = "Species", y = "Cover",
#           color = "Species", palette = c("#FDC086", "#7FC97F"),
#           add = NULL,       # <-- No jitter
#           notch = TRUE, 
#           outlier.shape = 16,  # <-- Show outliers normally
#           outlier.size = 2     # <-- Control outlier size
# ) +
#   stat_compare_means(method = "t.test", label = "p.format") +
#   theme_minimal()
# 
# H<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = Height,
#   type = "nonparametric"
# )+
#   labs(y = "Height (cm)")
# H
# 
# ggboxplot(combined_data, x = "Species", y = "Height",
#           color = "Species", palette = c("#FDC086", "#7FC97F"),
#           add = NULL,       # <-- No jitter
#           notch = TRUE, 
#           outlier.shape = 16,  # <-- Show outliers normally
#           outlier.size = 2     # <-- Control outlier size
# ) +
#   stat_compare_means(method = "t.test", label = "p.format") +
#   theme_minimal()
# 
# GB<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = Grassbiomass,
#   type = "nonparametric"
# )+
#   labs(y = "Grass Biomass (gm/m2)")
# GB
# 
# TB<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = TotalBiomass,
#   type = "nonparametric"
# )+
#   labs(y = "Total Biomass (gm/m2)")
# TB
# 
# LAI<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = LAI,
#   type = "nonparametric"
# )+
#   labs(y = "Leaf area index")
# LAI
# 
# FC<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = ForbsCover,
#   type = "nonparametric"
# ) +
#   labs(y = "Forbs Cover (%)")
# FC
# 
# SDC<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = StandingDeadCover,
#   type = "nonparametric"
# )+
#   labs(y = "Standing Cover (%)")
# SDC
# 
# LC<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = LitterCover,
#   type = "nonparametric"
# )+
#   labs(y = "Litter Cover (%)")
# LC
# 
# BGC<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = BareGroundCover,
#   type = "nonparametric"
# )+
#   labs(y = "Baregound Cover (%)")
# BGC
# 
# SC<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = ShrubCover,
#   type = "nonparametric"
# )+
#   labs(y = "Shrub Cover (%)")
# SC
# 
# DB<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = DeadBiomass,
#   type = "nonparametric"
# )+
#   labs(y = "Dead biomass (gm/m2)")
# DB
# 
# SB<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = ShrubBiomass,
#   type = "nonparametric"
# )+
#   labs(y = "Shrub biomass (gm/m2)")
# SB
# 
# FB<- ggbetweenstats(
#   data = combined_data,
#   x = Species,
#   y = ForbsBiomass,
#   type = "nonparametric"
# )+
#   labs(y = "Forbs biomass (gm/m2)")
# FB
# 
# #################
# #Lets combine them
# ################
# library(patchwork)
# # Combine the Cover graphs with a single legend at the bottom
# Cover_ppplot <- (
#   (GC + SDC) /
#     (BGC + LC) /
#     (SC + FC) 
# ) + 
#   plot_layout(
#     heights = c(3, 3, 3),
#     guides = "collect"
#   ) & 
#   theme(
#     legend.position = "bottom",
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.title.x = element_blank()
#   )
# 
# # Print the Cover plot
# Cover_ppplot
# 
# #################
# #FOr biomass
# ################
# library(patchwork)
# # Combine the Cover graphs with a single legend at the bottom
# biomass_ppplot <- (
#   (GB + TB) /
#     (FB + SB) 
# ) + 
#   plot_layout(
#     heights = c(3, 3),
#     guides = "collect"
#   ) & 
#   theme(
#     legend.position = "bottom",
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.title.x = element_blank()
#   )
# 
# # Print the Cover plot
# biomass_ppplot
# 
# 
# #################
# #Other biophysical
# ################
# library(patchwork)
# # Combine the Cover graphs with a single legend at the bottom
# other_bio_ppplot <- (
#   (H + LAI)
# ) +
#   plot_layout(
#     heights = c(3),
#     guides = "collect"
#   ) &
#   theme(
#     legend.position = "bottom",
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.title.x = element_blank()
#   )
# 
# # Print the Cover plot
# other_bio_ppplot

