#set working directory
library(readr)
library(tidyverse)
library(ggplot2)

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
#names(DescriptiveStatdata)

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
#names(combined_data)

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
#names(combined_data)

#write.table(combined_data, "D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data/PlotWiseDescriptiveStat.csv", sep = ",", row.names = FALSE)
#write.table(combined_data, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data/PlotWiseDescriptiveStat.csv", sep = ",", row.names = FALSE)

############################################################################################################################################################################################
################################# 1. T tests without graphs 2. Assumption tests 3. T test with graphs######################################################################################
Note: I will be trying to produce all the test here. Also, will create the test graph using the codes that I have written###################################################################
############################################################################################################################################################################################
#View (combined_data)
#names(combined_data)
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
