#setwd("D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets")
#setwd("C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets")
#setwd("C:/Users/Mohammed/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets")
setwd("C:/Users/Mohammed/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets")
#install.packages(c("ggplot2", "cli", "vctrs", "tidyverse"))

# Load required libraries
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)

# Load dataset
Hy <- read.csv("Hyperspectral Reflectance.csv", check.names = FALSE)
#View(Hy)
names(Hy)

# Compute mean reflectance per site (averaging across quadrats)
mean_reflectance_per_site <- Hy %>%
  group_by(Site) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

#View(mean_reflectance_per_site)

#Note: I have to delete some of the bands because they contain noise. Check Irini and Yihan's paper. They talked about it.
# Define the wavelength ranges to be set as NA
wavelength_na_ranges <- c(1350:1430, 1750:1980, 2330:2500) # Took this range from Irini and Yihan's article.
# Replace values with NA for the specified wavelength ranges
mean_reflectance_per_site[, as.character(wavelength_na_ranges)] <- NA
#View(mean_reflectance_per_site)

# Add a new column 'Species' based on the first letter of 'Site'
mean_reflectance_per_site <- mean_reflectance_per_site %>%
  mutate(Species = ifelse(grepl("^C", Site), "CW", "NG"))
#View(mean_reflectance_per_site)
###########################################################
###########################################################

# Convert data to long format for plotting
long_data <- mean_reflectance_per_site %>%
  pivot_longer(
    cols = -c(Site, Species),
    names_to = "Wavelength",
    values_to = "Reflectance"
  ) %>%
  mutate(Wavelength = as.numeric(Wavelength))  # Ensure wavelengths are numeric
#View(long_data)

# Calculate mean and SD per species and wavelength
species_summary <- long_data %>%
  group_by(Species, Wavelength) %>%
  summarise(
    Mean_Reflectance = mean(Reflectance, na.rm = TRUE),
    SD_Reflectance = sd(Reflectance, na.rm = TRUE)
  ) %>%
  # Replace NaN with NA (for gaps)
  mutate(
    Mean_Reflectance = ifelse(is.nan(Mean_Reflectance), NA, Mean_Reflectance),
    SD_Reflectance = ifelse(is.nan(SD_Reflectance), NA, SD_Reflectance)
  )
#View(species_summary)


###########################################################
#****************Spectral profile graph final*************#
###########################################################
library(ggplot2)
p2 <- ggplot(species_summary, aes(x = Wavelength, y = Mean_Reflectance, color = Species)) +
  geom_line(linewidth = 1) +
  geom_ribbon(
    aes(ymin = Mean_Reflectance - SD_Reflectance, 
        ymax = Mean_Reflectance + SD_Reflectance, 
        fill = Species),
    alpha = 0.1, color = NA
  ) +
  
  # Vertical boundaries (optional)
  geom_vline(xintercept = c(350, 750, 1350,1800, 2500), 
             linetype = "dashed", color = "gray50", linewidth = 0.3) +
  
  # RGB
  annotate("segment", x = 350, xend = 470, y = -0.04, yend = -0.04,
           arrow = arrow(length = unit(0.1, "inches"), ends = "first"), color = "black") +
  annotate("segment", x = 630, xend = 750, y = -0.04, yend = -0.04,
           arrow = arrow(length = unit(0.1, "inches"), ends = "last"), color = "black") +
  annotate("text", x = 550, y = -0.036, label = "Visible", vjust = 1, size = 4) +  
  
  # NIR
  annotate("segment", x = 750, xend = 995, y = -0.04, yend = -0.04,
           arrow = arrow(length = unit(0.1, "inches"), ends = "first"), color = "black") +
  annotate("segment", x = 1100, xend = 1350, y = -0.04, yend = -0.04,
           arrow = arrow(length = unit(0.1, "inches"), ends = "last"), color = "black") +
  annotate("text", x = 1050, y = -0.036, label = "NIR", vjust = 1, size = 4) +  
  
  # SWIR
  annotate("segment", x = 1350, xend = 1810, y = -0.04, yend = -0.04,
           arrow = arrow(length = unit(0.1, "inches"), ends = "first"), color = "black") +
  annotate("segment", x = 1920, xend = 2500, y = -0.04, yend = -0.04,
           arrow = arrow(length = unit(0.1, "inches"), ends = "last"), color = "black") +
  annotate("text", x = 1870, y = -0.036, label = "SWIR", vjust = 1, size = 4) +
  
  
  # Cosmetic settings
  scale_color_manual(values = c("CW" = "red", "NG" = "green")) +
  scale_fill_manual(values = c("CW" = "red", "NG" = "green")) +
  scale_x_continuous(
  ) +
  scale_x_continuous(
    name = "Wavelength (nm)",
    sec.axis = dup_axis(name = NULL, labels = NULL)  # This adds the top axis
    
  ) +
  scale_y_continuous(
    name = "Reflectance",
    sec.axis = dup_axis(name = NULL, labels = NULL)  # This adds the right axis
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(color = "black", size = 1),  # Left & bottom
    axis.line.x.top = element_line(color = "black", size = 1),
    axis.line.y.right = element_line(color = "black", size = 1),
    axis.ticks.x.top = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.text.x.top = element_blank(),
    axis.text.y.right = element_blank()
  )+
  
  # Critical adjustments
  coord_cartesian(ylim = c(0, NA), clip = "off") +
  theme(plot.margin = margin(b = 40))

###########################################################
#****************Hyperspectral data t test***************#
###########################################################
#Note: I have to change the arrangement of the file and remove the bands showing noise and the "NA" values.
mean_reflectance_per_site <- mean_reflectance_per_site %>%
  pivot_longer(cols = `350`:`2500`, 
               names_to = "Band",
               values_to = "Reflectance")

#View (mean_reflectance_per_site)
names(mean_reflectance_per_site)

#lets remove the "NA" values, because t test gives an error due to the NA.
absorption_bands <- c(1350:1430, 1750:1980, 2330:2500)
mean_reflectance_per_site <- mean_reflectance_per_site %>%
  filter(!(Band %in% absorption_bands))
#View(mean_reflectance_per_site)

# Converting the species values (CW and NG) to factors
mean_reflectance_per_site$Species <- as.factor(mean_reflectance_per_site$Species)
#View(mean_reflectance_per_site)

###########################################################
#********************Assumption tests*********************#
###########################################################
library(plotly)
library(ggpubr)  
library(dplyr)

mean_reflectance_per_site <- mean_reflectance_per_site %>%
  mutate(
    Band_region = case_when(
      Band >= 350 & Band <= 750 ~ "VIS",      
      Band >= 1351 & Band <= 2500 ~ "SWIR",
      Band >= 751 & Band <= 1350 ~ "NIR",
      TRUE ~ "NIR"
    )
  )
#View(mean_reflectance_per_site)

D<-ggqqplot(
  data = mean_reflectance_per_site, 
  x = "Reflectance", 
  facet.by = "Species",
  title = "Q-Q Plot: Grass biomass (gm/m2)"
)
D

# Split data by species
split_data <- split(mean_reflectance_per_site$Reflectance,mean_reflectance_per_site$Species)
#View(split_data)

# Run Shapiro-Wilk test for each species
lapply(split_data, shapiro.test)
#Note: Looks like this is one of the limitation of shapiro test of normality.

library(car)
leveneTest(Reflectance ~ Species, data = mean_reflectance_per_site)

variance_results2 <- mean_reflectance_per_site %>%
  group_by(Band) %>%
  summarise(
    levene_p = leveneTest(Reflectance ~ Species)$`Pr(>F)`[1],
    variance_ratio = var(Reflectance[Species == "CW"]) / 
      var(Reflectance[Species == "NG"]),
    .groups = 'drop'
  )
#View(variance_results2)


#Note: Hypersepctral data appears to be abnormal but I can make it normal based on the logic
# from Irini's work. And, the variances might be unequal. So, I will end up with the Weltch T test like Irini.

ggqqplot(
  data = mean_reflectance_per_site,
  x = "Reflectance",
  facet.by = c("Species", "Band_region"),  # Facet by both Species AND Band
  title = "Q-Q Plots: Reflectance Normality by Species and Spectral Band",
  color = "Band_region",                   # Color points by band
  palette = c("red", "darkgreen", "gray")  # Custom colors for bands
) +
  theme(legend.position = "top") 

p <- ggqqplot(
  data = mean_reflectance_per_site,
  x = "Reflectance",
  facet.by = c("Species", "Band_region"),
  title = "Interactive Q-Q Plot by Band_region",
  color = "Band_region"
)

# Convert to interactive plot
ggplotly(p, tooltip = c("x", "y", "Rand")) 

###########################################################
#********************Significant tests********************#
###########################################################

#Student t test (assumes normality and equality of variance)
student_t_test_results <- mean_reflectance_per_site %>%
  group_by(Band) %>%
  summarize(p_value_student = t.test(Reflectance ~ Species, var.equal = TRUE)$p.value)
#View(student_t_test_results)

write.table(student_t_test_results, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Hyperspectralstudenttest.csv", sep = ",", row.names = FALSE)

# Student t Test
ggplot(student_t_test_results, aes(x = as.numeric(Band), y = p_value_student)) +
  geom_point(aes(color = p_value_student < 0.05)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  scale_color_manual(
    name = "Significance",  # This title will now be removed
    values = c("grey", "steelblue"),
    labels = c("Not Significant", "Significant")
  ) +
  labs(
    title = "Significant Bands by Species",
    x = "Bands (nm)",
    y = "p-value"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    legend.title = element_blank()  # Removes the legend title
  )

#Weltch t test (assumes normality) Note: I will be using this one as my data are normal but some parts have unequal variances
weltch_t_test_results <- mean_reflectance_per_site %>%
  group_by(Band) %>%
  summarize(p_value_WEL = t.test(Reflectance ~ Species, var.equal = FALSE)$p.value)
#View(weltch_t_test_results)

write.table(weltch_t_test_results, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/HyperspectralWeltchTtest.csv", sep = ",", row.names = FALSE)

# Weltch t Test
p1 <-ggplot(student_t_test_results, aes(x = as.numeric(Band), y = p_value_student)) +
  # Main plot elements
  geom_point(aes(color = p_value_student < 0.05)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  
  # Vertical lines for spectral regions
  geom_vline(xintercept = c(350, 750, 1350, 1800, 2500), 
             linetype = "dashed", color = "gray50", linewidth = 0.3) +
  
  # Spectral region annotations (positioned below x-axis)
  # Visible region
  annotate("segment", x = 350, xend = 470, y = -0.15, yend = -0.15,
           arrow = arrow(length = unit(0.1, "inches"), ends = "first"), color = "black") +
  annotate("segment", x = 630, xend = 750, y = -0.15, yend = -0.15,
           arrow = arrow(length = unit(0.1, "inches"), ends = "last"), color = "black") +
  annotate("text", x = 550, y = -0.14, label = "Visible", vjust = 1, size = 4) +
  
  # NIR region
  annotate("segment", x = 750, xend = 995, y = -0.15, yend = -0.15,
           arrow = arrow(length = unit(0.1, "inches"), ends = "first"), color = "black") +
  annotate("segment", x = 1100, xend = 1350, y = -0.15, yend = -0.15,
           arrow = arrow(length = unit(0.1, "inches"), ends = "last"), color = "black") +
  annotate("text", x = 1050, y = -0.14, label = "NIR", vjust = 1, size = 4) +
  
  # SWIR region
  annotate("segment", x = 1350, xend = 1810, y = -0.15, yend = -0.15,
           arrow = arrow(length = unit(0.1, "inches"), ends = "first"), color = "black") +
  annotate("segment", x = 1920, xend = 2500, y = -0.15, yend = -0.15,
           arrow = arrow(length = unit(0.1, "inches"), ends = "last"), color = "black") +
  annotate("text", x = 1870, y = -0.14, label = "SWIR", vjust = 1, size = 4) +
  
  # Color and labels
  scale_color_manual(
    name = "Significance",
    values = c("grey", "steelblue"),
    labels = c("Not Significant", "Significant")
  ) +
  labs(
    #title = "Significant Bands by Species",
    x = "Wavelength (nm)",
    y = "p-value"
  ) +
  
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    plot.margin = margin(b = 60, unit = "pt"),
    axis.line = element_line(color = "black", size = 1),  # Left & bottom
    axis.line.x.top = element_line(color = "black", size = 1),
    axis.line.y.right = element_line(color = "black", size = 1),
    axis.ticks.x.top = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.text.x.top = element_blank(),
    axis.text.y.right = element_blank()  # Increased bottom margin
  ) +
  
  # Critical adjustments for annotations
  coord_cartesian(ylim = c(0, 1), clip = "off") + # Allow drawing outside plot area
  scale_x_continuous(
    name = "Wavelength (nm)",
    sec.axis = dup_axis(name = NULL, labels = NULL)  # This adds the top axis
    
  ) +
  scale_y_continuous(
    name = "p-value",
    sec.axis = dup_axis(name = NULL, labels = NULL)  # This adds the right axis
  )+
  
  theme(plot.margin = margin(b = 40))
p1

#Man whitny U test
wilcx_t_test_results <- mean_reflectance_per_site %>%
  group_by(Band) %>%
  summarize(p_value_WIL = wilcox.test(Reflectance ~ Species)$p.value)
View(wilcx_t_test_results)

write.table(wilcx_t_test_results, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/HyperspectralWilcoxTtest.csv", sep = ",", row.names = FALSE)

# Man whitney u Test
ggplot(wilcx_t_test_results, aes(x = as.numeric(Band), y = p_value_WIL)) +
  geom_point(aes(color = p_value_WIL < 0.05)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  scale_color_manual(
    name = "Significance",
    values = c("grey", "steelblue"),
    labels = c("Not Significant", "Significant")  # Proper legend labels
  )+
  labs(title = "Significant Bands by Species",
       x = "Bands (nm)",
       y = "p-value") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),  # Increase axis title size
    axis.text = element_text(size = 12),   # Increase axis label size
    plot.title = element_text(size = 16),  # Increase plot title size
    legend.text = element_text(size = 12), # Increase legend text size
    legend.title = element_text(size = 14) # Increase legend title size
  )

######################################################################
#########I will use graph for the hyperspectral#######################
############ Step 1: Identify significant bands###################
weltch_t_test_results$Band <- as.numeric(as.character(weltch_t_test_results$Band))

signif_regions <- weltch_t_test_results %>%
  filter(p_value_WEL < 0.05)

# Step 2: Reflectance plot with shaded regions
library(ggplot2)

p_combined <- ggplot(species_summary, aes(x = Wavelength, y = Mean_Reflectance, color = Species)) +
  geom_line(linewidth = 1) +
  geom_ribbon(
    aes(ymin = Mean_Reflectance - SD_Reflectance, 
        ymax = Mean_Reflectance + SD_Reflectance, 
        fill = Species),
    alpha = 0.1, color = NA
  ) +
  
  # Highlight significant bands
  geom_rect(data = signif_regions, inherit.aes = FALSE,
            aes(xmin = Band - 1, xmax = Band + 1, ymin = -Inf, ymax = Inf),
            fill = "lightblue", alpha = 0.2) +
  
  # Optional vertical lines and labels (same as before)
  geom_vline(xintercept = c(350, 750, 1350,1800, 2500), 
             linetype = "dashed", color = "gray50", linewidth = 0.3) +
  
  # Region annotations (Visible, NIR, SWIR)
  annotate("segment", x = 350, xend = 470, y = -0.06, yend = -0.06, arrow = arrow(length = unit(0.1, "inches"), ends = "first"), color = "black") +
  annotate("segment", x = 630, xend = 750, y = -0.06, yend = -0.06, arrow = arrow(length = unit(0.1, "inches"), ends = "last"), color = "black") +
  annotate("text", x = 550, y = -0.052, label = "Visible", vjust = 1, size = 5) +
  annotate("segment", x = 750, xend = 995, y = -0.06, yend = -0.06, arrow = arrow(length = unit(0.1, "inches"), ends = "first"), color = "black") +
  annotate("segment", x = 1100, xend = 1350, y = -0.06, yend = -0.06, arrow = arrow(length = unit(0.1, "inches"), ends = "last"), color = "black") +
  annotate("text", x = 1050, y = -0.052, label = "NIR", vjust = 1, size = 5) +
  annotate("segment", x = 1350, xend = 1810, y = -0.06, yend = -0.06, arrow = arrow(length = unit(0.1, "inches"), ends = "first"), color = "black") +
  annotate("segment", x = 1930, xend = 2500, y = -0.06, yend = -0.06, arrow = arrow(length = unit(0.1, "inches"), ends = "last"), color = "black") +

    annotate("text", x = 1870, y = -0.052, label = "SWIR", vjust = 1, size = 5) +
  
  # Cosmetic
  scale_color_manual(values = c("CW" = "red", "NG" = "green")) +
  scale_fill_manual(values = c("CW" = "red", "NG" = "green", "Band" = "lightblue")) +
  labs(x = "Wavelength (nm)", y = "Reflectance") +
  coord_cartesian(ylim = c(0, NA), clip = "off") +
  theme_classic() +
 theme(
  axis.title = element_text(size = 18),
  axis.text = element_text(size = 16),
  plot.title = element_text(size = 20),
  legend.text = element_text(size = 16),
  legend.title = element_blank(),
  plot.margin = margin(b = 60, unit = "pt"),
  axis.line = element_line(color = "black", size = 1),  # Left & bottom
  axis.line.x.top = element_line(color = "black", size = 1),
  axis.line.y.right = element_line(color = "black", size = 1),
  axis.ticks.x.top = element_blank(),
  axis.ticks.y.right = element_blank(),
  axis.text.x.top = element_blank(),
  axis.text.y.right = element_blank()  # Increased bottom margin
)+
  coord_cartesian(ylim = c(0, 0.4), clip = "off") + # Allow drawing outside plot area
  scale_x_continuous(
    name = "Wavelength (nm)",
    sec.axis = dup_axis(name = NULL, labels = NULL)  # This adds the top axis
    
  )+
  
  scale_y_continuous(
    name = "Reflectance",
    sec.axis = dup_axis(name = NULL, labels = NULL) ,
    # This adds the right axis
  )+
  
  theme(plot.margin = margin(b = 40))

p_combined

######################################
######################################
#Vegetation indices calculation
######################################
######################################
#setwd("D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets")
#setwd("C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets")
#setwd("C:/Users/Mohammed/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets")


# Load required libraries
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(spectrolab)

# Load dataset
Hy <- read.csv("Hyperspectral Reflectance.csv", check.names = FALSE)
View(Hy)
names(Hy)

# Compute mean reflectance per site (averaging across quadrats)
mean_reflectance_per_site <- Hy %>%
  group_by(Site) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

#View(mean_reflectance_per_site)

#Note: I have to delete some of the bands because they contain noise. Check Irini and Yihan's paper. They talked about it.
# Define the wavelength ranges to be set as NA
wavelength_na_ranges <- c(1350:1430, 1750:1980, 2330:2500) # Took this range from Irini and Yihan's article.
# Replace values with NA for the specified wavelength ranges
mean_reflectance_per_site[, as.character(wavelength_na_ranges)] <- NA
#View(mean_reflectance_per_site)

# Add a new column 'Species' based on the first letter of 'Site'
mean_reflectance_per_site <- mean_reflectance_per_site %>%
  mutate(Species = ifelse(grepl("^C", Site), "CW", "NG"))
#View(mean_reflectance_per_site)

# Convert data to long format for plotting
long_data <- mean_reflectance_per_site %>%
  pivot_longer(
    cols = -c(Site, Species),
    names_to = "Wavelength",
    values_to = "Reflectance"
  ) %>%
  mutate(Wavelength = as.numeric(Wavelength))  # Ensure wavelengths are numeric
#View(long_data)


##*******************************##
###Vegetation index calculation###
###**************************###

#*****************************************************************************************************#
#Normalized Differenced Vegetation indices (NDVI) (682-553) ((A. Gitelson & Merzlyak, 1994rudel, Thierry
#Fabre, Sophie, Houet, Thomas, Mazier, Florence, Briottet, Xavier# Opposite to traditional NDVI
#*****************************************************************************************************#

## C1 plots
## NDVI(682/553) (Number in the VI list: 1)
R682C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 682]
R553C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 553]
NDVI_682_553_C1 <- (R682C1 - R553C1) / (R682C1 + R553C1)
NDVI_682_553_C1

## C2 plots
R682C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 682]
R553C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 553]
NDVI_682_553_C2 <- (R682C2 - R553C2) / (R682C2 + R553C2)
NDVI_682_553_C2

## C3 plots
R682C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 682]
R553C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 553]
NDVI_682_553_C3 <- (R682C3 - R553C3) / (R682C3 + R553C3)
NDVI_682_553_C3

## C4 plots
R682C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 682]
R553C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 553]
NDVI_682_553_C4 <- (R682C4 - R553C4) / (R682C4 + R553C4)
NDVI_682_553_C4

## C5 plots
R682C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 682]
R553C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 553]
NDVI_682_553_C5 <- (R682C5 - R553C5) / (R682C5 + R553C5)
NDVI_682_553_C5

## C6 plots
R682C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 682]
R553C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 553]
NDVI_682_553_C6 <- (R682C6 - R553C6) / (R682C6 + R553C6)
NDVI_682_553_C6

## C7 plots
R682C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 682]
R553C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 553]
NDVI_682_553_C7 <- (R682C7 - R553C7) / (R682C7 + R553C7)
NDVI_682_553_C7

## C11 plots
R682C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 682]
R553C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 553]
NDVI_682_553_C11 <- (R682C11 - R553C11) / (R682C11 + R553C11)
NDVI_682_553_C11

## C12 plots
R682C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 682]
R553C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 553]
NDVI_682_553_C12 <- (R682C12 - R553C12) / (R682C12 + R553C12)
NDVI_682_553_C12

## C13 plots
R682C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 682]
R553C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 553]
NDVI_682_553_C13 <- (R682C13 - R553C13) / (R682C13 + R553C13)
NDVI_682_553_C13

## C15 plots
R682C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 682]
R553C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 553]
NDVI_682_553_C15 <- (R682C15 - R553C15) / (R682C15 + R553C15)
NDVI_682_553_C15

## C19 plots
R682C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 682]
R553C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 553]
NDVI_682_553_C19 <- (R682C19 - R553C19) / (R682C19 + R553C19)
NDVI_682_553_C19

## C20 plots
R682C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 682]
R553C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 553]
NDVI_682_553_C20 <- (R682C20 - R553C20) / (R682C20 + R553C20)
NDVI_682_553_C20

## N3 plots
R682N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 682]
R553N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 553]
NDVI_682_553_N3 <- (R682N3 - R553N3) / (R682N3 + R553N3)
NDVI_682_553_N3

## N4 plots (Fixed typo: now uses R682N4)
R682N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 682]
R553N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 553]
NDVI_682_553_N4 <- (R682N4 - R553N4) / (R682N4 + R553N4)
NDVI_682_553_N4

## N5 plots
R682N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 682]
R553N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 553]
NDVI_682_553_N5 <- (R682N5 - R553N5) / (R682N5 + R553N5)
NDVI_682_553_N5

## N10 plots
R682N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 682]
R553N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 553]
NDVI_682_553_N10 <- (R682N10 - R553N10) / (R682N10 + R553N10)
NDVI_682_553_N10

## N13 plots
R682N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 682]
R553N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 553]
NDVI_682_553_N13 <- (R682N13 - R553N13) / (R682N13 + R553N13)
NDVI_682_553_N13

## N14 plots
R682N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 682]
R553N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 553]
NDVI_682_553_N14 <- (R682N14 - R553N14) / (R682N14 + R553N14)
NDVI_682_553_N14

## N15 plots
R682N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 682]
R553N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 553]
NDVI_682_553_N15 <- (R682N15 - R553N15) / (R682N15 + R553N15)
NDVI_682_553_N15

## N16 plots (Fixed typo: now uses R682N16)
R682N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 682]
R553N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 553]
NDVI_682_553_N16 <- (R682N16 - R553N16) / (R682N16 + R553N16)
NDVI_682_553_N16

#########################################
###########Combine and Export############
#########################################
NDVI_682_553 <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20","N3","N4","N5","N10","N13","N14","N15","N16"),
  NDVI_682_553 = c(NDVI_682_553_C1, NDVI_682_553_C2, NDVI_682_553_C3, NDVI_682_553_C4,
                   NDVI_682_553_C5, NDVI_682_553_C6, NDVI_682_553_C7,
                   NDVI_682_553_C11, NDVI_682_553_C12, NDVI_682_553_C13, NDVI_682_553_C15, 
                   NDVI_682_553_C19, NDVI_682_553_C20, NDVI_682_553_N3, 
                   NDVI_682_553_N4, NDVI_682_553_N5, NDVI_682_553_N10,
                   NDVI_682_553_N13, NDVI_682_553_N14, NDVI_682_553_N15,
                   NDVI_682_553_N16))

#View(NDVI_682_553)


#Export the file
#write.table(NDVI_682_553, "D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/Vegetation indices/NDVI_682_553.csv", sep = ",", row.names = FALSE)

#*****************************************************************************************************#
#                           Carotenoid Reflectance Index I     (Hill, 2013)                                       #
#*****************************************************************************************************#
## C1 plots
## Reciprocal Difference Index (510/550)
R510C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 510]
R550C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 550]
CRI_510_550_C1 <- (1/R510C1) - (1/R550C1)
CRI_510_550_C1

## C2 plots
R510C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 510]
R550C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 550]
CRI_510_550_C2 <- (1/R510C2) - (1/R550C2)
CRI_510_550_C2

## C3 plots
R510C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 510]
R550C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 550]
CRI_510_550_C3 <- (1/R510C3) - (1/R550C3)
CRI_510_550_C3

## C4 plots
R510C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 510]
R550C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 550]
CRI_510_550_C4 <- (1/R510C4) - (1/R550C4)
CRI_510_550_C4

## C5 plots
R510C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 510]
R550C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 550]
CRI_510_550_C5 <- (1/R510C5) - (1/R550C5)
CRI_510_550_C5

## C6 plots
R510C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 510]
R550C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 550]
CRI_510_550_C6 <- (1/R510C6) - (1/R550C6)
CRI_510_550_C6

## C7 plots
R510C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 510]
R550C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 550]
CRI_510_550_C7 <- (1/R510C7) - (1/R550C7)
CRI_510_550_C7

## C11 plots
R510C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 510]
R550C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 550]
CRI_510_550_C11 <- (1/R510C11) - (1/R550C11)
CRI_510_550_C11

## C12 plots
R510C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 510]
R550C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 550]
CRI_510_550_C12 <- (1/R510C12) - (1/R550C12)
CRI_510_550_C12

## C13 plots
R510C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 510]
R550C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 550]
CRI_510_550_C13 <- (1/R510C13) - (1/R550C13)
CRI_510_550_C13

## C15 plots
R510C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 510]
R550C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 550]
CRI_510_550_C15 <- (1/R510C15) - (1/R550C15)
CRI_510_550_C15

## C19 plots
R510C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 510]
R550C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 550]
CRI_510_550_C19 <- (1/R510C19) - (1/R550C19)
CRI_510_550_C19

## C20 plots
R510C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 510]
R550C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 550]
CRI_510_550_C20 <- (1/R510C20) - (1/R550C20)
CRI_510_550_C20

## N3 plots
R510N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 510]
R550N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 550]
CRI_510_550_N3 <- (1/R510N3) - (1/R550N3)
CRI_510_550_N3

## N4 plots
R510N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 510]
R550N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 550]
CRI_510_550_N4 <- (1/R510N4) - (1/R550N4)
CRI_510_550_N4

## N5 plots
R510N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 510]
R550N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 550]
CRI_510_550_N5 <- (1/R510N5) - (1/R550N5)
CRI_510_550_N5

## N10 plots
R510N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 510]
R550N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 550]
CRI_510_550_N10 <- (1/R510N10) - (1/R550N10)
CRI_510_550_N10

## N13 plots
R510N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 510]
R550N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 550]
CRI_510_550_N13 <- (1/R510N13) - (1/R550N13)
CRI_510_550_N13

## N14 plots
R510N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 510]
R550N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 550]
CRI_510_550_N14 <- (1/R510N14) - (1/R550N14)
CRI_510_550_N14

## N15 plots
R510N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 510]
R550N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 550]
CRI_510_550_N15 <- (1/R510N15) - (1/R550N15)
CRI_510_550_N15

## N16 plots
R510N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 510]
R550N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 550]
CRI_510_550_N16 <- (1/R510N16) - (1/R550N16)
CRI_510_550_N16

#########################################
###########Combine and Export############
#########################################
CRI_510_550 <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20","N3","N4","N5","N10","N13","N14","N15","N16"),
  CRI_510_550 = c(CRI_510_550_C1, CRI_510_550_C2, CRI_510_550_C3, CRI_510_550_C4,
                 CRI_510_550_C5, CRI_510_550_C6, CRI_510_550_C7,
                 CRI_510_550_C11, CRI_510_550_C12, CRI_510_550_C13, CRI_510_550_C15, 
                 CRI_510_550_C19, CRI_510_550_C20, CRI_510_550_N3, 
                 CRI_510_550_N4, CRI_510_550_N5, CRI_510_550_N10,
                 CRI_510_550_N13, CRI_510_550_N14, CRI_510_550_N15,
                 CRI_510_550_N16))

#View(CRI_510_550)

#Export the file
#write.table(CRI_510_550, "D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/Vegetation indices/CRI_510_550.csv", sep = ",", row.names = FALSE)


#*****************************************************************************************************#
#                           Anthocyanin Reflectance Index I    (Hill, 2013)                           #            #
#*****************************************************************************************************#
## C1 plots
## ARI (550/700)
R550C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 550]
R700C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 700]
ARI_550_700_C1 <- (1/R550C1) - (1/R700C1)
ARI_550_700_C1

## C2 plots
R550C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 550]
R700C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 700]
ARI_550_700_C2 <- (1/R550C2) - (1/R700C2)
ARI_550_700_C2

## C3 plots
R550C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 550]
R700C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 700]
ARI_550_700_C3 <- (1/R550C3) - (1/R700C3)
ARI_550_700_C3

## C4 plots
R550C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 550]
R700C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 700]
ARI_550_700_C4 <- (1/R550C4) - (1/R700C4)
ARI_550_700_C4

## C5 plots
R550C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 550]
R700C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 700]
ARI_550_700_C5 <- (1/R550C5) - (1/R700C5)
ARI_550_700_C5

## C6 plots
R550C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 550]
R700C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 700]
ARI_550_700_C6 <- (1/R550C6) - (1/R700C6)
ARI_550_700_C6

## C7 plots
R550C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 550]
R700C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 700]
ARI_550_700_C7 <- (1/R550C7) - (1/R700C7)
ARI_550_700_C7

## C11 plots
R550C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 550]
R700C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 700]
ARI_550_700_C11 <- (1/R550C11) - (1/R700C11)
ARI_550_700_C11

## C12 plots
R550C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 550]
R700C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 700]
ARI_550_700_C12 <- (1/R550C12) - (1/R700C12)
ARI_550_700_C12

## C13 plots
R550C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 550]
R700C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 700]
ARI_550_700_C13 <- (1/R550C13) - (1/R700C13)
ARI_550_700_C13

## C15 plots
R550C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 550]
R700C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 700]
ARI_550_700_C15 <- (1/R550C15) - (1/R700C15)
ARI_550_700_C15

## C19 plots
R550C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 550]
R700C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 700]
ARI_550_700_C19 <- (1/R550C19) - (1/R700C19)
ARI_550_700_C19

## C20 plots
R550C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 550]
R700C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 700]
ARI_550_700_C20 <- (1/R550C20) - (1/R700C20)
ARI_550_700_C20

## N3 plots
R550N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 550]
R700N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 700]
ARI_550_700_N3 <- (1/R550N3) - (1/R700N3)
ARI_550_700_N3

## N4 plots
R550N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 550]
R700N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 700]
ARI_550_700_N4 <- (1/R550N4) - (1/R700N4)
ARI_550_700_N4

## N5 plots
R550N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 550]
R700N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 700]
ARI_550_700_N5 <- (1/R550N5) - (1/R700N5)
ARI_550_700_N5

## N10 plots
R550N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 550]
R700N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 700]
ARI_550_700_N10 <- (1/R550N10) - (1/R700N10)
ARI_550_700_N10

## N13 plots
R550N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 550]
R700N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 700]
ARI_550_700_N13 <- (1/R550N13) - (1/R700N13)
ARI_550_700_N13

## N14 plots
R550N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 550]
R700N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 700]
ARI_550_700_N14 <- (1/R550N14) - (1/R700N14)
ARI_550_700_N14

## N15 plots
R550N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 550]
R700N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 700]
ARI_550_700_N15 <- (1/R550N15) - (1/R700N15)
ARI_550_700_N15

## N16 plots
R550N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 550]
R700N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 700]
ARI_550_700_N16 <- (1/R550N16) - (1/R700N16)
ARI_550_700_N16

#########################################
###########Combine and Export############
#########################################
ARI_550_700 <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20","N3","N4","N5","N10","N13","N14","N15","N16"),
  ARI_550_700 = c(ARI_550_700_C1, ARI_550_700_C2, ARI_550_700_C3, ARI_550_700_C4,
                  ARI_550_700_C5, ARI_550_700_C6, ARI_550_700_C7,
                  ARI_550_700_C11, ARI_550_700_C12, ARI_550_700_C13, ARI_550_700_C15, 
                  ARI_550_700_C19, ARI_550_700_C20, ARI_550_700_N3, 
                  ARI_550_700_N4, ARI_550_700_N5, ARI_550_700_N10,
                  ARI_550_700_N13, ARI_550_700_N14, ARI_550_700_N15,
                  ARI_550_700_N16))

#View(ARI_550_700)

#Export the file
#write.table(ARI_550_700, "D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/Vegetation indices/ARI_550_700.csv", sep = ",", row.names = FALSE)


#*****************************************************************************************************#
#                           RGR (Red Green Ratio)  (Hill, 2013)                                      #
#*****************************************************************************************************#
## C1 plots
## RGR (600/699/500/599)
R600C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 600]
R699C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 699]
R500C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 500]
R599C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 599]
RGR_C1 <- (R600C1-R699C1) / (R500C1-R599C1)
RGR_C1

## C2 plots
R600C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 600]
R699C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 699]
R500C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 500]
R599C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 599]
RGR_C2 <- (R600C2-R699C2) / (R500C2-R599C2)
RGR_C2

## C3 plots
R600C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 600]
R699C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 699]
R500C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 500]
R599C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 599]
RGR_C3 <- (R600C3-R699C3) / (R500C3-R599C3)
RGR_C3

## C4 plots
R600C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 600]
R699C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 699]
R500C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 500]
R599C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 599]
RGR_C4 <- (R600C4-R699C4) / (R500C4-R599C4)
RGR_C4

## C5 plots
R600C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 600]
R699C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 699]
R500C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 500]
R599C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 599]
RGR_C5 <- (R600C5-R699C5) / (R500C5-R599C5)
RGR_C5

## C6 plots
R600C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 600]
R699C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 699]
R500C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 500]
R599C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 599]
RGR_C6 <- (R600C6-R699C6) / (R500C6-R599C6)
RGR_C6

## C7 plots
R600C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 600]
R699C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 699]
R500C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 500]
R599C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 599]
RGR_C7 <- (R600C7-R699C7) / (R500C7-R599C7)
RGR_C7

## C11 plots
R600C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 600]
R699C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 699]
R500C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 500]
R599C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 599]
RGR_C11 <- (R600C11-R699C11) / (R500C11-R599C11)
RGR_C11

## C12 plots
R600C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 600]
R699C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 699]
R500C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 500]
R599C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 599]
RGR_C12 <- (R600C12-R699C12) / (R500C12-R599C12)
RGR_C12

## C13 plots
R600C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 600]
R699C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 699]
R500C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 500]
R599C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 599]
RGR_C13 <- (R600C13-R699C13) / (R500C13-R599C13)
RGR_C13

## C15 plots
R600C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 600]
R699C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 699]
R500C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 500]
R599C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 599]
RGR_C15 <- (R600C15-R699C15) / (R500C15-R599C15)
RGR_C15

## C19 plots
R600C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 600]
R699C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 699]
R500C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 500]
R599C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 599]
RGR_C19 <- (R600C19-R699C19) / (R500C19-R599C19)
RGR_C19

## C20 plots
R600C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 600]
R699C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 699]
R500C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 500]
R599C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 599]
RGR_C20 <- (R600C20-R699C20) / (R500C20-R599C20)
RGR_C20

## N3 plots
R600N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 600]
R699N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 699]
R500N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 500]
R599N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 599]
RGR_N3 <- (R600N3-R699N3) / (R500N3-R599N3)
RGR_N3

## N4 plots
R600N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 600]
R699N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 699]
R500N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 500]
R599N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 599]
RGR_N4 <- (R600N4-R699N4) / (R500N4-R599N4)
RGR_N4

## N5 plots
R600N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 600]
R699N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 699]
R500N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 500]
R599N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 599]
RGR_N5 <- (R600N5-R699N5) / (R500N5-R599N5)
RGR_N5

## N10 plots
R600N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 600]
R699N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 699]
R500N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 500]
R599N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 599]
RGR_N10 <- (R600N10-R699N10) / (R500N10-R599N10)
RGR_N10

## N13 plots
R600N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 600]
R699N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 699]
R500N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 500]
R599N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 599]
RGR_N13 <- (R600N13-R699N13) / (R500N13-R599N13)
RGR_N13

## N14 plots
R600N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 600]
R699N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 699]
R500N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 500]
R599N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 599]
RGR_N14 <- (R600N14-R699N14) / (R500N14-R599N14)
RGR_N14

## N15 plots
R600N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 600]
R699N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 699]
R500N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 500]
R599N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 599]
RGR_N15 <- (R600N15-R699N15) / (R500N15-R599N15)
RGR_N15

## N16 plots
R600N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 600]
R699N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 699]
R500N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 500]
R599N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 599]
RGR_N16 <- (R600N16-R699N16) / (R500N16-R599N16)
RGR_N16

#########################################
###########Combine and Export############
#########################################
RGR_results <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20", "N3", "N4", "N5", "N10", "N13", "N14", "N15", "N16"),
  RGR = c(RGR_C1, RGR_C2, RGR_C3, RGR_C4, RGR_C5, RGR_C6, RGR_C7,
          RGR_C11, RGR_C12, RGR_C13, RGR_C15, RGR_C19, RGR_C20,
          RGR_N3, RGR_N4, RGR_N5, RGR_N10, RGR_N13, RGR_N14, RGR_N15, RGR_N16))

#View(RGR_results)

#write.table(RGR_results, "D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/Vegetation indices/RGR.csv", sep = ",", row.names = FALSE)

#*****************************************************************************************************#
#                            RGR (Red Green Ratio)  (McMurtrey et al., 1994)                                      #
#*****************************************************************************************************#                                    #
#*****************************************************************************************************#
## Simple ratio
## C1 plots
R700C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 700]
R670C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 670]
R700_R670_C1 <- R700C1 / R670C1
R700_R670_C1

## C2 plots
R700C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 700]
R670C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 670]
R700_R670_C2 <- R700C2 / R670C2
R700_R670_C2

## C3 plots
R700C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 700]
R670C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 670]
R700_R670_C3 <- R700C3 / R670C3
R700_R670_C3

## C4 plots
R700C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 700]
R670C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 670]
R700_R670_C4 <- R700C4 / R670C4
R700_R670_C4

## C5 plots
R700C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 700]
R670C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 670]
R700_R670_C5 <- R700C5 / R670C5
R700_R670_C5

## C6 plots
R700C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 700]
R670C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 670]
R700_R670_C6 <- R700C6 / R670C6
R700_R670_C6

## C7 plots
R700C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 700]
R670C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 670]
R700_R670_C7 <- R700C7 / R670C7
R700_R670_C7

## C11 plots
R700C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 700]
R670C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 670]
R700_R670_C11 <- R700C11 / R670C11
R700_R670_C11

## C12 plots
R700C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 700]
R670C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 670]
R700_R670_C12 <- R700C12 / R670C12
R700_R670_C12

## C13 plots
R700C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 700]
R670C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 670]
R700_R670_C13 <- R700C13 / R670C13
R700_R670_C13

## C15 plots
R700C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 700]
R670C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 670]
R700_R670_C15 <- R700C15 / R670C15
R700_R670_C15

## C19 plots
R700C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 700]
R670C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 670]
R700_R670_C19 <- R700C19 / R670C19
R700_R670_C19

## C20 plots
R700C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 700]
R670C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 670]
R700_R670_C20 <- R700C20 / R670C20
R700_R670_C20

## N3 plots
R700N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 700]
R670N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 670]
R700_R670_N3 <- R700N3 / R670N3
R700_R670_N3

## N4 plots
R700N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 700]
R670N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 670]
R700_R670_N4 <- R700N4 / R670N4
R700_R670_N4

## N5 plots
R700N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 700]
R670N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 670]
R700_R670_N5 <- R700N5 / R670N5
R700_R670_N5

## N10 plots
R700N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 700]
R670N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 670]
R700_R670_N10 <- R700N10 / R670N10
R700_R670_N10

## N13 plots
R700N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 700]
R670N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 670]
R700_R670_N13 <- R700N13 / R670N13
R700_R670_N13

## N14 plots
R700N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 700]
R670N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 670]
R700_R670_N14 <- R700N14 / R670N14
R700_R670_N14

## N15 plots
R700N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 700]
R670N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 670]
R700_R670_N15 <- R700N15 / R670N15
R700_R670_N15

## N16 plots
R700N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 700]
R670N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 670]
R700_R670_N16 <- R700N16 / R670N16
R700_R670_N16

#########################################
###########Combine and Export############
#########################################
RGR_700_670<-R700_R670_results <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20", "N3", "N4", "N5", "N10", "N13", "N14", "N15", "N16"),
  R700_R670 = c(R700_R670_C1, R700_R670_C2, R700_R670_C3, R700_R670_C4, R700_R670_C5, 
                R700_R670_C6, R700_R670_C7, R700_R670_C11, R700_R670_C12, R700_R670_C13,
                R700_R670_C15, R700_R670_C19, R700_R670_C20, R700_R670_N3, R700_R670_N4,
                R700_R670_N5, R700_R670_N10, R700_R670_N13, R700_R670_N14, R700_R670_N15,
                R700_R670_N16))

#View(RGR_700_670)

#write.table(RGR_700_670, "D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/Vegetation indices/RGR_700_670_results.csv", sep = ",", row.names = FALSE)


#*****************************************************************************************************#
#           Modified Chlorophyll Absorption Ratio Index 2 (MCARI2) (Daughtry, 2000)                                  #
#*****************************************************************************************************#                                    #

## C1 plots
R700C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 700]
R670C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 670]
R550C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 550]
VI_C1 <- ((R700C1 - R670C1) - 0.2 * (R700C1 - R550C1)) * (R700C1 - R670C1)
VI_C1

## C2 plots
R700C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 700]
R670C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 670]
R550C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 550]
VI_C2 <- ((R700C2 - R670C2) - 0.2 * (R700C2 - R550C2)) * (R700C2 - R670C2)
VI_C2

## C3 plots
R700C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 700]
R670C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 670]
R550C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 550]
VI_C3 <- ((R700C3 - R670C3) - 0.2 * (R700C3 - R550C3)) * (R700C3 - R670C3)
VI_C3

## C4 plots
R700C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 700]
R670C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 670]
R550C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 550]
VI_C4 <- ((R700C4 - R670C4) - 0.2 * (R700C4 - R550C4)) * (R700C4 - R670C4)
VI_C4

## C5 plots
R700C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 700]
R670C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 670]
R550C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 550]
VI_C5 <- ((R700C5 - R670C5) - 0.2 * (R700C5 - R550C5)) * (R700C5 - R670C5)
VI_C5

## C6 plots
R700C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 700]
R670C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 670]
R550C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 550]
VI_C6 <- ((R700C6 - R670C6) - 0.2 * (R700C6 - R550C6)) * (R700C6 - R670C6)
VI_C6

## C7 plots
R700C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 700]
R670C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 670]
R550C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 550]
VI_C7 <- ((R700C7 - R670C7) - 0.2 * (R700C7 - R550C7)) * (R700C7 - R670C7)
VI_C7

## C11 plots
R700C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 700]
R670C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 670]
R550C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 550]
VI_C11 <- ((R700C11 - R670C11) - 0.2 * (R700C11 - R550C11)) * (R700C11 - R670C11)
VI_C11

## C12 plots
R700C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 700]
R670C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 670]
R550C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 550]
VI_C12 <- ((R700C12 - R670C12) - 0.2 * (R700C12 - R550C12)) * (R700C12 - R670C12)
VI_C12

## C13 plots
R700C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 700]
R670C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 670]
R550C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 550]
VI_C13 <- ((R700C13 - R670C13) - 0.2 * (R700C13 - R550C13)) * (R700C13 - R670C13)
VI_C13

## C15 plots
R700C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 700]
R670C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 670]
R550C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 550]
VI_C15 <- ((R700C15 - R670C15) - 0.2 * (R700C15 - R550C15)) * (R700C15 - R670C15)
VI_C15

## C19 plots
R700C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 700]
R670C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 670]
R550C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 550]
VI_C19 <- ((R700C19 - R670C19) - 0.2 * (R700C19 - R550C19)) * (R700C19 - R670C19)
VI_C19

## C20 plots
R700C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 700]
R670C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 670]
R550C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 550]
VI_C20 <- ((R700C20 - R670C20) - 0.2 * (R700C20 - R550C20)) * (R700C20 - R670C20)
VI_C20

## N3 plots
R700N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 700]
R670N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 670]
R550N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 550]
VI_N3 <- ((R700N3 - R670N3) - 0.2 * (R700N3 - R550N3)) * (R700N3 - R670N3)
VI_N3

## N4 plots
R700N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 700]
R670N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 670]
R550N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 550]
VI_N4 <- ((R700N4 - R670N4) - 0.2 * (R700N4 - R550N4)) * (R700N4 - R670N4)
VI_N4

## N5 plots
R700N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 700]
R670N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 670]
R550N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 550]
VI_N5 <- ((R700N5 - R670N5) - 0.2 * (R700N5 - R550N5)) * (R700N5 - R670N5)
VI_N5

## N10 plots
R700N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 700]
R670N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 670]
R550N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 550]
VI_N10 <- ((R700N10 - R670N10) - 0.2 * (R700N10 - R550N10)) * (R700N10 - R670N10)
VI_N10

## N13 plots
R700N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 700]
R670N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 670]
R550N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 550]
VI_N13 <- ((R700N13 - R670N13) - 0.2 * (R700N13 - R550N13)) * (R700N13 - R670N13)
VI_N13

## N14 plots
R700N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 700]
R670N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 670]
R550N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 550]
VI_N14 <- ((R700N14 - R670N14) - 0.2 * (R700N14 - R550N14)) * (R700N14 - R670N14)
VI_N14

## N15 plots
R700N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 700]
R670N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 670]
R550N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 550]
VI_N15 <- ((R700N15 - R670N15) - 0.2 * (R700N15 - R550N15)) * (R700N15 - R670N15)
VI_N15

## N16 plots
R700N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 700]
R670N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 670]
R550N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 550]
VI_N16 <- ((R700N16 - R670N16) - 0.2 * (R700N16 - R550N16)) * (R700N16 - R670N16)
VI_N16

#########################################
###########Combine and Export############
#########################################
MCARI2<-VI_results <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20", "N3", "N4", "N5", "N10", "N13", "N14", "N15", "N16"),
  MCARI2 = c(VI_C1, VI_C2, VI_C3, VI_C4, VI_C5, VI_C6, VI_C7,
                       VI_C11, VI_C12, VI_C13, VI_C15, VI_C19, VI_C20,
                       VI_N3, VI_N4, VI_N5, VI_N10, VI_N13, VI_N14, VI_N15, VI_N16))

#View(MCARI2)

#write.table(VI_results, "D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/Vegetation indices/MCARI2.csv", sep = ",", row.names = FALSE)


#*****************************************************************************************************#
#     Transformed Chlorophyll Absorption Reflectance Index (TCARI) (550,670, 700) (Daughtry, 2000)                                  #
#*****************************************************************************************************#                                    #
## C1 plots
R700C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 700]
R670C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 670]
R550C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 550]
TCARI_C1 <- 3 * ((R700C1 - R670C1) - 0.2 * (R700C1 - R550C1)) * (R700C1 / R670C1)
TCARI_C1

## C2 plots
R700C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 700]
R670C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 670]
R550C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 550]
TCARI_C2 <- 3 * ((R700C2 - R670C2) - 0.2 * (R700C2 - R550C2)) * (R700C2 / R670C2)
TCARI_C2

## C3 plots
R700C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 700]
R670C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 670]
R550C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 550]
TCARI_C3 <- 3 * ((R700C3 - R670C3) - 0.2 * (R700C3 - R550C3)) * (R700C3 / R670C3)
TCARI_C3

## C4 plots
R700C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 700]
R670C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 670]
R550C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 550]
TCARI_C4 <- 3 * ((R700C4 - R670C4) - 0.2 * (R700C4 - R550C4)) * (R700C4 / R670C4)
TCARI_C4

## C5 plots
R700C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 700]
R670C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 670]
R550C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 550]
TCARI_C5 <- 3 * ((R700C5 - R670C5) - 0.2 * (R700C5 - R550C5)) * (R700C5 / R670C5)
TCARI_C5

## C6 plots
R700C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 700]
R670C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 670]
R550C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 550]
TCARI_C6 <- 3 * ((R700C6 - R670C6) - 0.2 * (R700C6 - R550C6)) * (R700C6 / R670C6)
TCARI_C6

## C7 plots
R700C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 700]
R670C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 670]
R550C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 550]
TCARI_C7 <- 3 * ((R700C7 - R670C7) - 0.2 * (R700C7 - R550C7)) * (R700C7 / R670C7)
TCARI_C7

## C11 plots
R700C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 700]
R670C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 670]
R550C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 550]
TCARI_C11 <- 3 * ((R700C11 - R670C11) - 0.2 * (R700C11 - R550C11)) * (R700C11 / R670C11)
TCARI_C11

## C12 plots
R700C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 700]
R670C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 670]
R550C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 550]
TCARI_C12 <- 3 * ((R700C12 - R670C12) - 0.2 * (R700C12 - R550C12)) * (R700C12 / R670C12)
TCARI_C12

## C13 plots
R700C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 700]
R670C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 670]
R550C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 550]
TCARI_C13 <- 3 * ((R700C13 - R670C13) - 0.2 * (R700C13 - R550C13)) * (R700C13 / R670C13)
TCARI_C13

## C15 plots
R700C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 700]
R670C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 670]
R550C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 550]
TCARI_C15 <- 3 * ((R700C15 - R670C15) - 0.2 * (R700C15 - R550C15)) * (R700C15 / R670C15)
TCARI_C15

## C19 plots
R700C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 700]
R670C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 670]
R550C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 550]
TCARI_C19 <- 3 * ((R700C19 - R670C19) - 0.2 * (R700C19 - R550C19)) * (R700C19 / R670C19)
TCARI_C19

## C20 plots
R700C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 700]
R670C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 670]
R550C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 550]
TCARI_C20 <- 3 * ((R700C20 - R670C20) - 0.2 * (R700C20 - R550C20)) * (R700C20 / R670C20)
TCARI_C20

## N3 plots
R700N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 700]
R670N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 670]
R550N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 550]
TCARI_N3 <- 3 * ((R700N3 - R670N3) - 0.2 * (R700N3 - R550N3)) * (R700N3 / R670N3)
TCARI_N3

## N4 plots
R700N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 700]
R670N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 670]
R550N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 550]
TCARI_N4 <- 3 * ((R700N4 - R670N4) - 0.2 * (R700N4 - R550N4)) * (R700N4 / R670N4)
TCARI_N4

## N5 plots
R700N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 700]
R670N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 670]
R550N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 550]
TCARI_N5 <- 3 * ((R700N5 - R670N5) - 0.2 * (R700N5 - R550N5)) * (R700N5 / R670N5)
TCARI_N5

## N10 plots
R700N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 700]
R670N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 670]
R550N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 550]
TCARI_N10 <- 3 * ((R700N10 - R670N10) - 0.2 * (R700N10 - R550N10)) * (R700N10 / R670N10)
TCARI_N10

## N13 plots
R700N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 700]
R670N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 670]
R550N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 550]
TCARI_N13 <- 3 * ((R700N13 - R670N13) - 0.2 * (R700N13 - R550N13)) * (R700N13 / R670N13)
TCARI_N13

## N14 plots
R700N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 700]
R670N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 670]
R550N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 550]
TCARI_N14 <- 3 * ((R700N14 - R670N14) - 0.2 * (R700N14 - R550N14)) * (R700N14 / R670N14)
TCARI_N14

## N15 plots
R700N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 700]
R670N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 670]
R550N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 550]
TCARI_N15 <- 3 * ((R700N15 - R670N15) - 0.2 * (R700N15 - R550N15)) * (R700N15 / R670N15)
TCARI_N15

## N16 plots
R700N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 700]
R670N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 670]
R550N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 550]
TCARI_N16 <- 3 * ((R700N16 - R670N16) - 0.2 * (R700N16 - R550N16)) * (R700N16 / R670N16)
TCARI_N16

#########################################
###########Combine and Export############
#########################################
TCARI_results <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20", "N3", "N4", "N5", "N10", "N13", "N14", "N15", "N16"),
  TCARI = c(TCARI_C1, TCARI_C2, TCARI_C3, TCARI_C4, TCARI_C5, TCARI_C6, TCARI_C7,
                                TCARI_C11, TCARI_C12, TCARI_C13, TCARI_C15, TCARI_C19, TCARI_C20,
                                TCARI_N3, TCARI_N4, TCARI_N5, TCARI_N10, TCARI_N13, TCARI_N14, TCARI_N15, TCARI_N16))

#View(TCARI_results)

#write.table(TCARI_results, "D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/Vegetation indices/TCARI_results.csv", sep = ",", row.names = FALSE)


#*****************************************************************************************************#
#                 Photochemical reflectance index (PRI) (550,670, 700) (Daughtry, 2000)                                  #
#*****************************************************************************************************#                                    #
## C1 plots
R531C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 531]
R570C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 570]
PRI_C1 <- (R531C1 - R570C1) / (R531C1 + R570C1)
PRI_C1

## C2 plots
R531C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 531]
R570C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 570]
PRI_C2 <- (R531C2 - R570C2) / (R531C2 + R570C2)
PRI_C2

## C3 plots
R531C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 531]
R570C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 570]
PRI_C3 <- (R531C3 - R570C3) / (R531C3 + R570C3)
PRI_C3

## C4 plots
R531C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 531]
R570C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 570]
PRI_C4 <- (R531C4 - R570C4) / (R531C4 + R570C4)
PRI_C4

## C5 plots
R531C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 531]
R570C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 570]
PRI_C5 <- (R531C5 - R570C5) / (R531C5 + R570C5)
PRI_C5

## C6 plots
R531C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 531]
R570C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 570]
PRI_C6 <- (R531C6 - R570C6) / (R531C6 + R570C6)
PRI_C6

## C7 plots
R531C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 531]
R570C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 570]
PRI_C7 <- (R531C7 - R570C7) / (R531C7 + R570C7)
PRI_C7

## C11 plots
R531C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 531]
R570C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 570]
PRI_C11 <- (R531C11 - R570C11) / (R531C11 + R570C11)
PRI_C11

## C12 plots
R531C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 531]
R570C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 570]
PRI_C12 <- (R531C12 - R570C12) / (R531C12 + R570C12)
PRI_C12

## C13 plots
R531C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 531]
R570C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 570]
PRI_C13 <- (R531C13 - R570C13) / (R531C13 + R570C13)
PRI_C13

## C15 plots
R531C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 531]
R570C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 570]
PRI_C15 <- (R531C15 - R570C15) / (R531C15 + R570C15)
PRI_C15

## C19 plots
R531C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 531]
R570C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 570]
PRI_C19 <- (R531C19 - R570C19) / (R531C19 + R570C19)
PRI_C19

## C20 plots
R531C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 531]
R570C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 570]
PRI_C20 <- (R531C20 - R570C20) / (R531C20 + R570C20)
PRI_C20

## N3 plots
R531N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 531]
R570N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 570]
PRI_N3 <- (R531N3 - R570N3) / (R531N3 + R570N3)
PRI_N3

## N4 plots
R531N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 531]
R570N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 570]
PRI_N4 <- (R531N4 - R570N4) / (R531N4 + R570N4)
PRI_N4

## N5 plots
R531N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 531]
R570N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 570]
PRI_N5 <- (R531N5 - R570N5) / (R531N5 + R570N5)
PRI_N5

## N10 plots
R531N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 531]
R570N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 570]
PRI_N10 <- (R531N10 - R570N10) / (R531N10 + R570N10)
PRI_N10

## N13 plots
R531N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 531]
R570N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 570]
PRI_N13 <- (R531N13 - R570N13) / (R531N13 + R570N13)
PRI_N13

## N14 plots
R531N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 531]
R570N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 570]
PRI_N14 <- (R531N14 - R570N14) / (R531N14 + R570N14)
PRI_N14

## N15 plots
R531N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 531]
R570N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 570]
PRI_N15 <- (R531N15 - R570N15) / (R531N15 + R570N15)
PRI_N15

## N16 plots
R531N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 531]
R570N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 570]
PRI_N16 <- (R531N16 - R570N16) / (R531N16 + R570N16)
PRI_N16

#########################################
###########Combine and Export############
#########################################
PRI_results <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20", "N3", "N4", "N5", "N10", "N13", "N14", "N15", "N16"),
  PRI = c(PRI_C1, PRI_C2, PRI_C3, PRI_C4, PRI_C5, PRI_C6, PRI_C7,
                       PRI_C11, PRI_C12, PRI_C13, PRI_C15, PRI_C19, PRI_C20,
                       PRI_N3, PRI_N4, PRI_N5, PRI_N10, PRI_N13, PRI_N14, PRI_N15, PRI_N16))

#View(PRI_results)

#write.table(PRI_results, "D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/Vegetation indices/PRI_results.csv", sep = ",", row.names = FALSE)

#*****************************************************************************************************#
#                  Cellulose Absorption Index (CAI) (2000,2200, 2100) (Nagler, P.; Daughtry, C.; Goward, S. Plant litter and soil reflectance. Remote Sens. Environ. 2000, 71, 207–215)                                  #
#*****************************************************************************************************#                     

## C1 plots
R2000C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 2000]
R2100C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 2100]
R2200C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 2200]
CAI_C1 <- ((R2000C1 + R2200C1)/2) - R2100C1
CAI_C1

## C2 plots
R2000C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 2000]
R2100C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 2100]
R2200C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 2200]
CAI_C2 <- ((R2000C2 + R2200C2)/2) - R2100C2
CAI_C2

## C3 plots
R2000C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 2000]
R2100C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 2100]
R2200C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 2200]
CAI_C3 <- ((R2000C3 + R2200C3)/2) - R2100C3
CAI_C3

## C4 plots
R2000C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 2000]
R2100C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 2100]
R2200C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 2200]
CAI_C4 <- ((R2000C4 + R2200C4)/2) - R2100C4
CAI_C4

## C5 plots
R2000C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 2000]
R2100C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 2100]
R2200C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 2200]
CAI_C5 <- ((R2000C5 + R2200C5)/2) - R2100C5
CAI_C5

## C6 plots
R2000C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 2000]
R2100C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 2100]
R2200C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 2200]
CAI_C6 <- ((R2000C6 + R2200C6)/2) - R2100C6
CAI_C6

## C7 plots
R2000C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 2000]
R2100C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 2100]
R2200C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 2200]
CAI_C7 <- ((R2000C7 + R2200C7)/2) - R2100C7
CAI_C7

## C11 plots
R2000C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 2000]
R2100C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 2100]
R2200C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 2200]
CAI_C11 <- ((R2000C11 + R2200C11)/2) - R2100C11
CAI_C11

## C12 plots
R2000C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 2000]
R2100C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 2100]
R2200C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 2200]
CAI_C12 <- ((R2000C12 + R2200C12)/2) - R2100C12
CAI_C12

## C13 plots
R2000C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 2000]
R2100C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 2100]
R2200C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 2200]
CAI_C13 <- ((R2000C13 + R2200C13)/2) - R2100C13
CAI_C13

## C15 plots
R2000C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 2000]
R2100C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 2100]
R2200C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 2200]
CAI_C15 <- ((R2000C15 + R2200C15)/2) - R2100C15
CAI_C15

## C19 plots
R2000C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 2000]
R2100C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 2100]
R2200C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 2200]
CAI_C19 <- ((R2000C19 + R2200C19)/2) - R2100C19
CAI_C19

## C20 plots
R2000C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 2000]
R2100C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 2100]
R2200C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 2200]
CAI_C20 <- ((R2000C20 + R2200C20)/2) - R2100C20
CAI_C20

## N3 plots
R2000N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 2000]
R2100N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 2100]
R2200N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 2200]
CAI_N3 <- ((R2000N3 + R2200N3)/2) - R2100N3
CAI_N3

## N4 plots
R2000N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 2000]
R2100N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 2100]
R2200N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 2200]
CAI_N4 <- ((R2000N4 + R2200N4)/2) - R2100N4
CAI_N4

## N5 plots
R2000N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 2000]
R2100N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 2100]
R2200N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 2200]
CAI_N5 <- ((R2000N5 + R2200N5)/2) - R2100N5
CAI_N5

## N10 plots
R2000N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 2000]
R2100N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 2100]
R2200N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 2200]
CAI_N10 <- ((R2000N10 + R2200N10)/2) - R2100N10
CAI_N10

## N13 plots
R2000N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 2000]
R2100N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 2100]
R2200N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 2200]
CAI_N13 <- ((R2000N13 + R2200N13)/2) - R2100N13
CAI_N13

## N14 plots
R2000N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 2000]
R2100N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 2100]
R2200N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 2200]
CAI_N14 <- ((R2000N14 + R2200N14)/2) - R2100N14
CAI_N14

## N15 plots
R2000N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 2000]
R2100N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 2100]
R2200N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 2200]
CAI_N15 <- ((R2000N15 + R2200N15)/2) - R2100N15
CAI_N15

## N16 plots
R2000N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 2000]
R2100N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 2100]
R2200N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 2200]
CAI_N16 <- ((R2000N16 + R2200N16)/2) - R2100N16
CAI_N16

#########################################
###########Combine and Export############
#########################################
CAI_results <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20", "N3", "N4", "N5", "N10", "N13", "N14", "N15", "N16"),
  CAI = c(CAI_C1, CAI_C2, CAI_C3, CAI_C4, CAI_C5, CAI_C6, CAI_C7,
                     CAI_C11, CAI_C12, CAI_C13, CAI_C15, CAI_C19, CAI_C20,
                     CAI_N3, CAI_N4, CAI_N5, CAI_N10, CAI_N13, CAI_N14, CAI_N15, CAI_N16))

#View(CAI_results)

#write.table(CAI_results, "D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/Vegetation indices/CAI_results.csv", sep = ",", row.names = FALSE)


#*****************************************************************************************************#
#                  NDNI (Normalized Difference Nitrogen Index) (2000,2200, 2100) (Serrano, L.; Peñuelas, J.; Ustin, S.L. Remote sensing of nitrogen and lignin in Mediterranean vegetation from AVIRIS data: Decomposing biochemical from structural signals.                                 #
#*****************************************************************************************************#                     

## C1 plots
R1510C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 1510]
R1680C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 1680]
NDNI_C1 <- ((log(1 / R1510C1) - log(1 / R1680C1))/(log(1 / R1510C1) + log(1 / R1680C1)))  
NDNI_C1              
               
## C2 plot
R1510C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 1510]
R1680C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 1680]
NDNI_C2 <- (log(1/R1510C2) - log(1/R1680C2)) / (log(1/R1510C2) + log(1/R1680C2))
NDNI_C2

## C3 plot
R1510C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 1510]
R1680C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 1680]
NDNI_C3 <- (log(1/R1510C3) - log(1/R1680C3)) / (log(1/R1510C3) + log(1/R1680C3))
NDNI_C3

## C4 plot
R1510C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 1510]
R1680C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 1680]
NDNI_C4 <- (log(1/R1510C4) - log(1/R1680C4)) / (log(1/R1510C4) + log(1/R1680C4))
NDNI_C4

## C5 plot
R1510C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 1510]
R1680C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 1680]
NDNI_C5 <- (log(1/R1510C5) - log(1/R1680C5)) / (log(1/R1510C5) + log(1/R1680C5))
NDNI_C5

## C6 plot
R1510C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 1510]
R1680C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 1680]
NDNI_C6 <- (log(1/R1510C6) - log(1/R1680C6)) / (log(1/R1510C6) + log(1/R1680C6))
NDNI_C6

## C7 plot
R1510C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 1510]
R1680C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 1680]
NDNI_C7 <- (log(1/R1510C7) - log(1/R1680C7)) / (log(1/R1510C7) + log(1/R1680C7))
NDNI_C7

## C11 plot
R1510C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 1510]
R1680C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 1680]
NDNI_C11 <- (log(1/R1510C11) - log(1/R1680C11)) / (log(1/R1510C11) + log(1/R1680C11))
NDNI_C11

## C12 plot
R1510C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 1510]
R1680C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 1680]
NDNI_C12 <- (log(1/R1510C12) - log(1/R1680C12)) / (log(1/R1510C12) + log(1/R1680C12))
NDNI_C12

## C13 plot
R1510C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 1510]
R1680C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 1680]
NDNI_C13 <- (log(1/R1510C13) - log(1/R1680C13)) / (log(1/R1510C13) + log(1/R1680C13))
NDNI_C13

## C15 plot
R1510C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 1510]
R1680C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 1680]
NDNI_C15 <- (log(1/R1510C15) - log(1/R1680C15)) / (log(1/R1510C15) + log(1/R1680C15))
NDNI_C15

## C19 plot
R1510C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 1510]
R1680C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 1680]
NDNI_C19 <- (log(1/R1510C19) - log(1/R1680C19)) / (log(1/R1510C19) + log(1/R1680C19))
NDNI_C19

## C20 plot
R1510C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 1510]
R1680C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 1680]
NDNI_C20 <- (log(1/R1510C20) - log(1/R1680C20)) / (log(1/R1510C20) + log(1/R1680C20))
NDNI_C20

## N3 plot
R1510N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 1510]
R1680N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 1680]
NDNI_N3 <- (log(1/R1510N3) - log(1/R1680N3)) / (log(1/R1510N3) + log(1/R1680N3))
NDNI_N3

## N4 plot
R1510N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 1510]
R1680N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 1680]
NDNI_N4 <- (log(1/R1510N4) - log(1/R1680N4)) / (log(1/R1510N4) + log(1/R1680N4))
NDNI_N4

## N5 plot
R1510N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 1510]
R1680N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 1680]
NDNI_N5 <- (log(1/R1510N5) - log(1/R1680N5)) / (log(1/R1510N5) + log(1/R1680N5))
NDNI_N5

## N10 plot
R1510N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 1510]
R1680N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 1680]
NDNI_N10 <- (log(1/R1510N10) - log(1/R1680N10)) / (log(1/R1510N10) + log(1/R1680N10))
NDNI_N10

## N13 plot
R1510N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 1510]
R1680N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 1680]
NDNI_N13 <- (log(1/R1510N13) - log(1/R1680N13)) / (log(1/R1510N13) + log(1/R1680N13))
NDNI_N13

## N14 plot
R1510N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 1510]
R1680N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 1680]
NDNI_N14 <- (log(1/R1510N14) - log(1/R1680N14)) / (log(1/R1510N14) + log(1/R1680N14))
NDNI_N14

## N15 plot
R1510N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 1510]
R1680N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 1680]
NDNI_N15 <- (log(1/R1510N15) - log(1/R1680N15)) / (log(1/R1510N15) + log(1/R1680N15))
NDNI_N15

## N16 plot
R1510N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 1510]
R1680N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 1680]
NDNI_N16 <- (log(1/R1510N16) - log(1/R1680N16)) / (log(1/R1510N16) + log(1/R1680N16))
NDNI_N16         


#########################################
########### Combine Results #############
#########################################
NDNI_results <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20", "N3", "N4", "N5", "N10", "N13", "N14", "N15", "N16"),
  NDNI = c(NDNI_C1, NDNI_C2, NDNI_C3, NDNI_C4, NDNI_C5, NDNI_C6, NDNI_C7,
           NDNI_C11, NDNI_C12, NDNI_C13, NDNI_C15, NDNI_C19, NDNI_C20,
           NDNI_N3, NDNI_N4, NDNI_N5, NDNI_N10, NDNI_N13, NDNI_N14, NDNI_N15, NDNI_N16))

#View(NDNI_results)

#write.table(NDNI_results, "D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/Vegetation indices/NDNI.csv", sep = ",", row.names = FALSE)

######################################################
######### Combine all these spectral indices datasets
######################################################
library(tidyverse)

# List all your loaded data frames (replace with your actual object names)
SI_list <- list(NDNI_results, CAI_results, PRI_results, TCARI_results, MCARI2, RGR_700_670, RGR_results, ARI_550_700, CRI_510_550, NDVI_682_553)

# Merge all by "Plot" (keeps all rows and fills missing values with NA)
combined_data <- reduce(SI_list, ~ full_join(.x, .y, by = "Plot"))

# View the result
head(combined_data)
#View(combined_data)

##################################################################
#########################Assumption test for correlation##########
#setwd("D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data")
#setwd("C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets")
setwd("C:/Users/Mohammed/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data")

Biophy <- read_csv("PlotWiseDescriptiveStat.csv")
Biophy$Plot <- Biophy$Site
data_list <- list(Biophy, combined_data)

# Merge all by "Plot" (keeps all rows and fills missing values with NA)
data <- reduce(data_list, ~ full_join(.x, .y, by = "Plot"))
#View(data)

######
#Note: I have to check normality for each of the spectral indices
names(data)

#NDNI
AA<-ggqqplot(
  data = data, 
  x = "NDNI", 
  facet.by = "Species",
  title = "Q-Q Plot: Normalized difference nitrogen index (NDNI)"
)
AA

# Split data by species
split_data_NDNI <- split(data$NDNI, 
                                 data$Species)
#View(split_data_NDNI)

# Run Shapiro-Wilk test for each species
lapply(split_data_NDNI, shapiro.test) #NOrmal data

library(car)
leveneTest(NDNI ~ Species, data = data) #Equal variances
plot(lm(NDNI ~ Species, data = data), which = 3)

#CAI
BB<-ggqqplot(
  data = data, 
  x = "CAI", 
  facet.by = "Species",
  title = "Q-Q Plot: Chlorophyll absorption index (CAI)"
)
BB

# Split data by species
split_data_CAI <- split(data$CAI, 
                         data$Species)
#View(split_data_CAI)

# Run Shapiro-Wilk test for each species
lapply(split_data_CAI, shapiro.test) #Normal data

library(car)
leveneTest(CAI ~ Species, data = data) #Unequal variance
plot(lm(CAI ~ Species, data = data), which = 3)

#PRI
CC<-ggqqplot(
  data = data, 
  x = "PRI", 
  facet.by = "Species",
  title = "Q-Q Plot: Photochemical reflectance index (PRI)"
)
CC

# Split data by species
split_data_PRI <- split(data$PRI, 
                        data$Species)
#View(split_data_PRI)

# Run Shapiro-Wilk test for each species
lapply(split_data_PRI, shapiro.test) # Crested Wheatgrass abnormal & Native grass normal

library(car)
leveneTest(PRI ~ Species, data = data) #Equal variance
plot(lm(PRI ~ Species, data = data), which = 3)

#TCARI
DD<-ggqqplot(
  data = data, 
  x = "TCARI", 
  facet.by = "Species",
  title = "Q-Q Plot: Transformed Chlorophyll Absorption Reflectance Index  (TCARI)"
)
DD

# Split data by species
split_data_TCARI <- split(data$TCARI, 
                        data$Species)
#View(split_data_TCARI)

# Run Shapiro-Wilk test for each species
lapply(split_data_TCARI, shapiro.test) #Normal data

library(car)
leveneTest(TCARI ~ Species, data = data) #Eqaul variances
plot(lm(TCARI ~ Species, data = data), which = 3)

#Modified Chlorophyll Absorption Ratio Index 2 (MCARI2)
EE<-ggqqplot(
  data = data, 
  x = "MCARI2", 
  facet.by = "Species",
  title = "Q-Q Plot: Modified Chlorophyll Absorption Ratio Index 2 (MCARI2)"
)
EE

# Split data by species
split_data_MCARI2 <- split(data$MCARI2, 
                          data$Species)
#View(split_data_MCARI2)

# Run Shapiro-Wilk test for each species
lapply(split_data_MCARI2, shapiro.test) #Normal data

library(car)
leveneTest(MCARI2 ~ Species, data = data) #Equal variances
plot(lm(MCARI2 ~ Species, data = data), which = 3)

#Simple Ratio (R700_R670)
FF<-ggqqplot(
  data = data, 
  x = "R700_R670", 
  facet.by = "Species",
  title = "Q-Q Plot: Simple Ratio (R700_R670)"
)
FF

# Split data by species
split_data_R700_R670 <- split(data$R700_R670, 
                           data$Species)
#View(split_data_R700_R670)

# Run Shapiro-Wilk test for each species
lapply(split_data_R700_R670, shapiro.test) #Crested Wheatgrass normal data & Native grass abnormal

library(car)
leveneTest(R700_R670 ~ Species, data = data) # Eqaual variances
plot(lm(R700_R670 ~ Species, data = data), which = 3)

#Red Green Ratio (RGR)
GG<-ggqqplot(
  data = data, 
  x = "RGR", 
  facet.by = "Species",
  title = "Q-Q Plot: Red Green Ratio (RGR)"
)
GG

# Split data by species
split_data_RGR <- split(data$RGR, 
                              data$Species)
#View(split_data_RGR)

# Run Shapiro-Wilk test for each species
lapply(split_data_RGR, shapiro.test) #Normal data

library(car)
leveneTest(RGR ~ Species, data = data) # Unequal variances
plot(lm(RGR ~ Species, data = data), which = 3)

#Anthocyanin Reflectance Index I (ARI1)
HH<-ggqqplot(
  data = data, 
  x = "ARI_550_700", 
  facet.by = "Species",
  title = "Q-Q Plot: Anthocyanin Reflectance Index I (ARI1)"
)
HH

# Split data by species
split_data_ARI_550_700 <- split(data$ARI_550_700, 
                        data$Species)
#View(split_data_ARI_550_700)

# Run Shapiro-Wilk test for each species
lapply(split_data_ARI_550_700, shapiro.test) #Normal data

library(car)
leveneTest(ARI_550_700 ~ Species, data = data) #Equal variances
plot(lm(ARI_550_700 ~ Species, data = data), which = 3)

#Carotenoid Reflectance Index I (CRI1)
II<-ggqqplot(
  data = data, 
  x = "CRI_510_550", 
  facet.by = "Species",
  title = "Q-Q Plot: Carotenoid Reflectance Index I (CRI1)"
)
II

# Split data by species
split_data_CRI_510_550 <- split(data$CRI_510_550, 
                                data$Species)
#View(split_data_CRI_510_550)

# Run Shapiro-Wilk test for each species
lapply(split_data_CRI_510_550, shapiro.test) #Crested Wheatgrass Normal data & Native abnormal data

library(car)
leveneTest(CRI_510_550 ~ Species, data = data) #Equal variances
plot(lm(CRI_510_550 ~ Species, data = data), which = 3)

#Normalized Differenced Vegetation indices (NDVI)
JJ<-ggqqplot(
  data = data, 
  x = "NDVI_682_553", 
  facet.by = "Species",
  title = "Q-Q Plot: Normalized Differenced Vegetation indices (NDVI) "
)
JJ

# Split data by species
split_data_NDVI_682_553 <- split(data$NDVI_682_553, 
                                data$Species)
#View(split_data_NDVI_682_553)

# Run Shapiro-Wilk test for each species
lapply(split_data_NDVI_682_553, shapiro.test) #Crested Wheatgrass Normal data & Native abnormal data

library(car)
leveneTest(NDVI_682_553 ~ Species, data = data) #Equal variances
plot(lm(NDVI_682_553 ~ Species, data = data), which = 3)

########################################################
########################################################
########################################################
########################################################
library(patchwork)
# Combine the Cover graphs with a single legend at the bottom
QQPlot <- (
  (AA + BB) /
    (CC+ DD)/
    (EE+ FF)/
    (GG + HH)/
    (II+ JJ)
    ) + 
  plot_layout(
    heights = c(3,3,3,3,3),
    guides = "collect"
  ) & 
  theme(
    legend.position = "bottom"
  )

# Print the Cover plot
QQPlot

####################
## Linearity test ##
####################
#View(data)
names(data)

# Subset for Native and Crested Wheatgrass
NG_data <- data %>% 
  filter(Species == "Native grass")
#View(NG_data)

CW_data <- data %>% 
  filter(Species == "Crested Wheatgrass")
#View(CW_data)
names(CW_data)

biophysical_vars <- c("Cover", "Height", "Grassbiomass", "TotalBiomass", 
                      "LAI", "ForbsCover", "BareGroundCover"
                      , "ForbsBiomass","DeadBiomass")

spectral_indices <- c("NDNI", "CAI", "PRI", "TCARI","MCARI2","R700_R670","RGR","ARI_550_700","CRI_510_550"
                      ,"NDVI_682_553")

#### Native grass & Crested Wheatgrass together
##NDNI With 9 Biophysical properties
# Grass Cover (%)
plot(NG_data$NDNI, NG_data$Cover,
     main = "Native grass NDNI vs Grass cover (%)",
     xlab = "NDNI", ylab = "Grass cover (%)")
abline(lm(Cover ~ NDNI, data = NG_data), col = "red")

plot(CW_data$NDNI, CW_data$Cover,
     main = "Crested Wheatgrass NDNI vs Grass cover (%)",
     xlab = "NDNI", ylab = "Grass cover (%)")
abline(lm( Cover~ NDNI, data = CW_data), col = "red")

plot(lm(Cover~ NDNI, data = NG_data), which = 1)

plot(lm(Cover~ NDNI, data = CW_data), which = 1)

###################
#***Native grass***#
###################

# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against NDNI in one go***#
for (var in biophysical_vars) {
  plot(NG_data$NDNI, NG_data[[var]],
       main = paste("NDNI vs", var, "(Native grass)"),
       xlab = "NDNI", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(NG_data[[var]] ~ NDNI, data = NG_data), col = "red")
  
  r2 <- summary(lm(NG_data[[var]] ~ NDNI, data = NG_data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

###################
#***Crested Wheatgrass***#
###################

# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against NDNI in one go***#
for (var in biophysical_vars) {
  plot(data$NDNI, data[[var]],
       main = paste("NDNI vs", var, "(Crested  Wheatgrass)"),
       xlab = "NDNI", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ NDNI, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ NDNI, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Height (cm)
plot(NG_data$NDNI, NG_data$Height,
     main = "Native grass NDNI vs Height (cm)",
     xlab = "NDNI", ylab = "Height (cm)")
abline(lm(Height ~ NDNI, data = NG_data), col = "red")

plot(CW_data$NDNI, CW_data$Height,
     main = "Crested Wheatgrass NDNI vs Height (cm)",
     xlab = "NDNI", ylab = "Height (cm)")
abline(lm( Height~ NDNI, data = CW_data), col = "red")

plot(lm(Height ~ NDNI, data = NG_data), which = 1)
plot(lm(Height ~ NDNI, data = CW_data), which = 1)

# Grassbiomass (gm/m2)
plot(NG_data$NDNI, NG_data$Grassbiomass,
     main = "Native grass NDNI vs Grass biomass (gm/m2)",
     xlab = "NDNI", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ NDNI, data = NG_data), col = "red")

plot(CW_data$NDNI, CW_data$Grassbiomass,
     main = "Crested Wheatgrass NDNI vs Grass biomass (gm/m2)",
     xlab = "NDNI", ylab = "Grass biomass (gm/m2)")
abline(lm( Grassbiomass~ NDNI, data = CW_data), col = "red")

plot(lm(Grassbiomass ~ NDNI, data = NG_data), which = 1)
plot(lm(Grassbiomass ~ NDNI, data = CW_data), which = 1)


# TotalBiomass (gm/m2)
plot(NG_data$NDNI, NG_data$TotalBiomass,
     main = "Native grass NDNI vs Total biomass (gm/m2)",
     xlab = "NDNI", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ NDNI, data = NG_data), col = "red")

plot(CW_data$NDNI, CW_data$TotalBiomass,
     main = "Crested Wheatgrass NDNI vs Total biomass (gm/m2)",
     xlab = "NDNI", ylab = "Total biomass (gm/m2)")
abline(lm( TotalBiomass~ NDNI, data = CW_data), col = "red")

plot(lm(TotalBiomass ~ NDNI, data = NG_data), which = 1)
plot(lm(TotalBiomass ~ NDNI, data = CW_data), which = 1)


# LAI
plot(NG_data$NDNI, NG_data$LAI,
     main = "Native grass NDNI vs Leaf area index",
     xlab = "NDNI", ylab = "Leaf area index")
abline(lm(LAI ~ NDNI, data = NG_data), col = "red")

plot(CW_data$NDNI, CW_data$LAI,
     main = "Crested Wheatgrass NDNI vs Leaf area index",
     xlab = "NDNI", ylab = "Leaf area index")
abline(lm( LAI~ NDNI, data = CW_data), col = "red")

plot(lm(LAI ~ NDNI, data = NG_data), which = 1)
plot(lm(LAI ~ NDNI, data = CW_data), which = 1)


# ForbsCover
plot(NG_data$NDNI, NG_data$ForbsCover,
     main = "Native grass NDNI vs Forbs cover (%)",
     xlab = "NDNI", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ NDNI, data = NG_data), col = "red")

plot(CW_data$NDNI, CW_data$ForbsCover,
     main = "Crested Wheatgrass NDNI vs Forbs cover (%)",
     xlab = "NDNI", ylab = "Forbs cover (%)")
abline(lm( ForbsCover~ NDNI, data = CW_data), col = "red")

plot(lm(ForbsCover ~ NDNI, data = NG_data), which = 1)
plot(lm(ForbsCover ~ NDNI, data = CW_data), which = 1)


# Bare Ground Cover (%)
plot(NG_data$NDNI, NG_data$BareGroundCover,
     main = "Native grass NDNI vs Bare ground cover (%)",
     xlab = "NDNI", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ NDNI, data = NG_data), col = "red")

plot(CW_data$NDNI, CW_data$BareGroundCover,
     main = "Crested Wheatgrass NDNI vs Bare ground cover (%)",
     xlab = "NDNI", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover~ NDNI, data = CW_data), col = "red")

plot(lm(BareGroundCover ~ NDNI, data = NG_data), which = 1)
plot(lm(BareGroundCover ~ NDNI, data = CW_data), which = 1)


# DeadBiomass (gm/m2)
plot(NG_data$NDNI, NG_data$DeadBiomass,
     main = "Native grass NDNI vs Dead biomass (gm/m2)",
     xlab = "NDNI", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ NDNI, data = NG_data), col = "red")

plot(CW_data$NDNI, CW_data$DeadBiomass,
     main = "Crested Wheatgrass NDNI vs Dead biomass (gm/m2)",
     xlab = "NDNI", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass~ NDNI, data = CW_data), col = "red")

plot(lm(DeadBiomass~ NDNI, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ NDNI, data = CW_data), which = 1)


# ForbsBiomass(gm/m2)
plot(NG_data$NDNI, NG_data$ForbsBiomass,
     main = "Native grass NDNI vs Forb biomass (gm/m2)",
     xlab = "NDNI", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ NDNI, data = NG_data), col = "red")

plot(CW_data$NDNI, CW_data$ForbsBiomass,
     main = "Crested Wheatgrass NDNI vs Forb biomass (gm/m2)",
     xlab = "NDNI", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass~ NDNI, data = CW_data), col = "red")

plot(lm(ForbsBiomass~ NDNI, data = NG_data), which = 1)
plot(lm(ForbsBiomass ~ NDNI, data = CW_data), which = 1)

###################
#***Native Grass***#
###################
##CAI With 9 Biophysical properties
# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against CAI in one go***#
for (var in biophysical_vars) {
  plot(NG_data$CAI, NG_data[[var]],
       main = paste("CAI vs", var, "(Native grass)"),
       xlab = "CAI", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(NG_data[[var]] ~ CAI, data = NG_data), col = "red")
  
  r2 <- summary(lm(NG_data[[var]] ~ CAI, data = NG_data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

###################
#***Crested Wheatgrass***#
###################

# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against CAI in one go***#
for (var in biophysical_vars) {
  plot(data$CAI, data[[var]],
       main = paste("CAI vs", var, "(Crested  Wheatgrass)"),
       xlab = "CAI", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ CAI, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ CAI, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Grass Cover (%)
plot(NG_data$CAI, NG_data$Cover,
     main = "Native grass CAI vs Grass cover (%)",
     xlab = "CAI", ylab = "Grass cover (%)")
abline(lm(Cover ~ CAI, data = NG_data), col = "red")

plot(CW_data$CAI, CW_data$Cover,
     main = "Crested Wheatgrass CAI vs Grass cover (%)",
     xlab = "CAI", ylab = "Grass cover (%)")
abline(lm( Cover~ CAI, data = CW_data), col = "red")

plot(lm(Cover~ CAI, data = NG_data), which = 1)

plot(lm(Cover~ CAI, data = CW_data), which = 1)

# Height (cm)
plot(NG_data$CAI, NG_data$Height,
     main = "Native grass CAI vs Height (cm)",
     xlab = "CAI", ylab = "Height (cm)")
abline(lm(Height ~ CAI, data = NG_data), col = "red")

plot(CW_data$CAI, CW_data$Height,
     main = "Crested Wheatgrass CAI vs Height (cm)",
     xlab = "CAI", ylab = "Height (cm)")
abline(lm( Height~ CAI, data = CW_data), col = "red")

plot(lm(Height ~ CAI, data = NG_data), which = 1)
plot(lm(Height ~ CAI, data = CW_data), which = 1)


# Grassbiomass (gm/m2)
plot(NG_data$CAI, NG_data$Grassbiomass,
     main = "Native grass CAI vs Grass biomass (gm/m2)",
     xlab = "CAI", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ CAI, data = NG_data), col = "red")

plot(CW_data$CAI, CW_data$Grassbiomass,
     main = "Crested Wheatgrass CAI vs Grass biomass (gm/m2)",
     xlab = "CAI", ylab = "Grass biomass (gm/m2)")
abline(lm( Grassbiomass~ CAI, data = CW_data), col = "red")

plot(lm(Grassbiomass ~ CAI, data = NG_data), which = 1)
plot(lm(Grassbiomass ~ CAI, data = CW_data), which = 1)


# TotalBiomass (gm/m2)
plot(NG_data$CAI, NG_data$TotalBiomass,
     main = "Native grass CAI vs Total biomass (gm/m2)",
     xlab = "CAI", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ CAI, data = NG_data), col = "red")

plot(CW_data$CAI, CW_data$TotalBiomass,
     main = "Crested Wheatgrass CAI vs Total biomass (gm/m2)",
     xlab = "CAI", ylab = "Total biomass (gm/m2)")
abline(lm( TotalBiomass~ CAI, data = CW_data), col = "red")

plot(lm(TotalBiomass ~ CAI, data = NG_data), which = 1)
plot(lm(TotalBiomass ~ CAI, data = CW_data), which = 1)


# LAI
plot(NG_data$CAI, NG_data$LAI,
     main = "Native grass CAI vs Leaf area index",
     xlab = "CAI", ylab = "Leaf area index")
abline(lm(LAI ~ CAI, data = NG_data), col = "red")

plot(CW_data$CAI, CW_data$LAI,
     main = "Crested Wheatgrass CAI vs Leaf area index",
     xlab = "CAI", ylab = "Leaf area index")
abline(lm( LAI~ CAI, data = CW_data), col = "red")

plot(lm(LAI ~ CAI, data = NG_data), which = 1)
plot(lm(LAI ~ CAI, data = CW_data), which = 1)


# ForbsCover
plot(NG_data$CAI, NG_data$ForbsCover,
     main = "Native grass CAI vs Forbs cover (%)",
     xlab = "CAI", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ CAI, data = NG_data), col = "red")

plot(CW_data$CAI, CW_data$ForbsCover,
     main = "Crested Wheatgrass CAI vs Forbs cover (%)",
     xlab = "CAI", ylab = "Forbs cover (%)")
abline(lm( ForbsCover~ CAI, data = CW_data), col = "red")

plot(lm(ForbsCover ~ CAI, data = NG_data), which = 1)
plot(lm(ForbsCover ~ CAI, data = CW_data), which = 1)


# Bare Ground Cover (%)
plot(NG_data$CAI, NG_data$BareGroundCover,
     main = "Native grass CAI vs Bare ground cover (%)",
     xlab = "CAI", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ CAI, data = NG_data), col = "red")

plot(CW_data$CAI, CW_data$BareGroundCover,
     main = "Crested Wheatgrass CAI vs Bare ground cover (%)",
     xlab = "CAI", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover~ CAI, data = CW_data), col = "red")

plot(lm(BareGroundCover ~ CAI, data = NG_data), which = 1)
plot(lm(BareGroundCover ~ CAI, data = CW_data), which = 1)


# DeadBiomass (gm/m2)
plot(NG_data$CAI, NG_data$DeadBiomass,
     main = "Native grass CAI vs Dead biomass (gm/m2)",
     xlab = "CAI", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ CAI, data = NG_data), col = "red")

plot(CW_data$CAI, CW_data$DeadBiomass,
     main = "Crested Wheatgrass CAI vs Dead biomass (gm/m2)",
     xlab = "CAI", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass~ CAI, data = CW_data), col = "red")

plot(lm(DeadBiomass~ CAI, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ CAI, data = CW_data), which = 1)


# ForbsBiomass(gm/m2)
plot(NG_data$CAI, NG_data$ForbsBiomass,
     main = "Native grass CAI vs Forb biomass (gm/m2)",
     xlab = "CAI", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ CAI, data = NG_data), col = "red")

plot(CW_data$CAI, CW_data$ForbsBiomass,
     main = "Crested Wheatgrass CAI vs Forb biomass (gm/m2)",
     xlab = "CAI", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass~ CAI, data = CW_data), col = "red")

plot(lm(ForbsBiomass~ CAI, data = NG_data), which = 1)
plot(lm(ForbsBiomass ~ CAI, data = CW_data), which = 1)


#####################
#***Native Grass***#
#*##################
##PRI With 9 Biophysical properties
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against PRI in one go***#
for (var in biophysical_vars) {
  plot(NG_data$PRI, NG_data[[var]],
       main = paste("PRI vs", var, "(Native grass)"),
       xlab = "PRI", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(NG_data[[var]] ~ PRI, data = NG_data), col = "red")
  
  r2 <- summary(lm(NG_data[[var]] ~ PRI, data = NG_data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

##########################
#***Crested Wheatgrass***#
#*########################
# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against PRI in one go***#
for (var in biophysical_vars) {
  plot(data$PRI, data[[var]],
       main = paste("PRI vs", var, "(Crested  Wheatgrass)"),
       xlab = "PRI", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ PRI, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ PRI, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Grass Cover (%)
plot(NG_data$PRI, NG_data$Cover,
     main = "Native grass PRI vs Grass cover (%)",
     xlab = "PRI", ylab = "Grass cover (%)")
abline(lm(Cover ~ PRI, data = NG_data), col = "red")

plot(CW_data$PRI, CW_data$Cover,
     main = "Crested Wheatgrass PRI vs Grass cover (%)",
     xlab = "PRI", ylab = "Grass cover (%)")
abline(lm( Cover~ PRI, data = CW_data), col = "red")

plot(lm(Cover~ PRI, data = NG_data), which = 1)

plot(lm(Cover~ PRI, data = CW_data), which = 1)

# Height (cm)
plot(NG_data$PRI, NG_data$Height,
     main = "Native grass PRI vs Height (cm)",
     xlab = "PRI", ylab = "Height (cm)")
abline(lm(Height ~ PRI, data = NG_data), col = "red")

plot(CW_data$PRI, CW_data$Height,
     main = "Crested Wheatgrass PRI vs Height (cm)",
     xlab = "PRI", ylab = "Height (cm)")
abline(lm( Height~ PRI, data = CW_data), col = "red")

plot(lm(Height ~ PRI, data = NG_data), which = 1)
plot(lm(Height ~ PRI, data = CW_data), which = 1)


# Grassbiomass (gm/m2)
plot(NG_data$PRI, NG_data$Grassbiomass,
     main = "Native grass PRI vs Grass biomass (gm/m2)",
     xlab = "PRI", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ PRI, data = NG_data), col = "red")

plot(CW_data$PRI, CW_data$Grassbiomass,
     main = "Crested Wheatgrass PRI vs Grass biomass (gm/m2)",
     xlab = "PRI", ylab = "Grass biomass (gm/m2)")
abline(lm( Grassbiomass~ PRI, data = CW_data), col = "red")

plot(lm(Grassbiomass ~ PRI, data = NG_data), which = 1)
plot(lm(Grassbiomass ~ PRI, data = CW_data), which = 1)


# TotalBiomass (gm/m2)
plot(NG_data$PRI, NG_data$TotalBiomass,
     main = "Native grass PRI vs Total biomass (gm/m2)",
     xlab = "PRI", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ PRI, data = NG_data), col = "red")

plot(CW_data$PRI, CW_data$TotalBiomass,
     main = "Crested Wheatgrass PRI vs Total biomass (gm/m2)",
     xlab = "PRI", ylab = "Total biomass (gm/m2)")
abline(lm( TotalBiomass~ PRI, data = CW_data), col = "red")

plot(lm(TotalBiomass ~ PRI, data = NG_data), which = 1)
plot(lm(TotalBiomass ~ PRI, data = CW_data), which = 1)


# LAI
plot(NG_data$PRI, NG_data$LAI,
     main = "Native grass PRI vs Leaf area index",
     xlab = "PRI", ylab = "Leaf area index")
abline(lm(LAI ~ PRI, data = NG_data), col = "red")

plot(CW_data$PRI, CW_data$LAI,
     main = "Crested Wheatgrass PRI vs Leaf area index",
     xlab = "PRI", ylab = "Leaf area index")
abline(lm( LAI~ PRI, data = CW_data), col = "red")

plot(lm(LAI ~ PRI, data = NG_data), which = 1)
plot(lm(LAI ~ PRI, data = CW_data), which = 1)


# ForbsCover
plot(NG_data$PRI, NG_data$ForbsCover,
     main = "Native grass PRI vs Forbs cover (%)",
     xlab = "PRI", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ PRI, data = NG_data), col = "red")

plot(CW_data$PRI, CW_data$ForbsCover,
     main = "Crested Wheatgrass PRI vs Forbs cover (%)",
     xlab = "PRI", ylab = "Forbs cover (%)")
abline(lm( ForbsCover~ PRI, data = CW_data), col = "red")

plot(lm(ForbsCover ~ PRI, data = NG_data), which = 1)
plot(lm(ForbsCover ~ PRI, data = CW_data), which = 1)


# Bare Ground Cover (%)
plot(NG_data$PRI, NG_data$BareGroundCover,
     main = "Native grass PRI vs Bare ground cover (%)",
     xlab = "PRI", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ PRI, data = NG_data), col = "red")

plot(CW_data$PRI, CW_data$BareGroundCover,
     main = "Crested Wheatgrass PRI vs Bare ground cover (%)",
     xlab = "PRI", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover~ PRI, data = CW_data), col = "red")

plot(lm(BareGroundCover ~ PRI, data = NG_data), which = 1)
plot(lm(BareGroundCover ~ PRI, data = CW_data), which = 1)


# DeadBiomass (gm/m2)
plot(NG_data$PRI, NG_data$DeadBiomass,
     main = "Native grass PRI vs Dead biomass (gm/m2)",
     xlab = "PRI", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ PRI, data = NG_data), col = "red")

plot(CW_data$PRI, CW_data$DeadBiomass,
     main = "Crested Wheatgrass PRI vs Dead biomass (gm/m2)",
     xlab = "PRI", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass~ PRI, data = CW_data), col = "red")

plot(lm(DeadBiomass~ PRI, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ PRI, data = CW_data), which = 1)


# ForbsBiomass(gm/m2)
plot(NG_data$PRI, NG_data$ForbsBiomass,
     main = "Native grass PRI vs Forb biomass (gm/m2)",
     xlab = "PRI", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ PRI, data = NG_data), col = "red")

plot(CW_data$PRI, CW_data$ForbsBiomass,
     main = "Crested Wheatgrass PRI vs Forb biomass (gm/m2)",
     xlab = "PRI", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass~ PRI, data = CW_data), col = "red")

plot(lm(ForbsBiomass~ PRI, data = NG_data), which = 1)
plot(lm(ForbsBiomass ~ PRI, data = CW_data), which = 1)


####################
#***Native Grass***#
#*##################
##TCARI With 9 Biophysical properties
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

#***Plot all variables against NDNI in one go***#
for (var in biophysical_vars) {
  plot(NG_data$TCARI, NG_data[[var]],
       main = paste("TCARI vs", var, "(Native grass)"),
       xlab = "TCARI", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(NG_data[[var]] ~ TCARI, data = NG_data), col = "red")
  
  r2 <- summary(lm(NG_data[[var]] ~ TCARI, data = NG_data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

##########################
#***Crested Wheatgrass***#
#*########################
# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against PRI in one go***#
for (var in biophysical_vars) {
  plot(data$TCARI, data[[var]],
       main = paste("TCARI vs", var, "(Crested  Wheatgrass)"),
       xlab = "TCARI", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ TCARI, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ TCARI, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Grass Cover (%)
plot(NG_data$TCARI, NG_data$Cover,
     main = "Native grass TCARI vs Grass cover (%)",
     xlab = "TCARI", ylab = "Grass cover (%)")
abline(lm(Cover ~ TCARI, data = NG_data), col = "red")

plot(CW_data$TCARI, CW_data$Cover,
     main = "Crested Wheatgrass TCARI vs Grass cover (%)",
     xlab = "TCARI", ylab = "Grass cover (%)")
abline(lm( Cover~ TCARI, data = CW_data), col = "red")

plot(lm(Cover~ TCARI, data = NG_data), which = 1)

plot(lm(Cover~ TCARI, data = CW_data), which = 1)


# Height (cm)
plot(NG_data$TCARI, NG_data$Height,
     main = "Native grass TCARI vs Height (cm)",
     xlab = "TCARI", ylab = "Height (cm)")
abline(lm(Height ~ TCARI, data = NG_data), col = "red")

plot(CW_data$TCARI, CW_data$Height,
     main = "Crested Wheatgrass TCARI vs Height (cm)",
     xlab = "TCARI", ylab = "Height (cm)")
abline(lm( Height~ TCARI, data = CW_data), col = "red")

plot(lm(Height ~ TCARI, data = NG_data), which = 1)
plot(lm(Height ~ TCARI, data = CW_data), which = 1)


# Grassbiomass (gm/m2)
plot(NG_data$TCARI, NG_data$Grassbiomass,
     main = "Native grass TCARI vs Grass biomass (gm/m2)",
     xlab = "TCARI", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ TCARI, data = NG_data), col = "red")

plot(CW_data$TCARI, CW_data$Grassbiomass,
     main = "Crested Wheatgrass TCARI vs Grass biomass (gm/m2)",
     xlab = "TCARI", ylab = "Grass biomass (gm/m2)")
abline(lm( Grassbiomass~ TCARI, data = CW_data), col = "red")

plot(lm(Grassbiomass ~ TCARI, data = NG_data), which = 1)
plot(lm(Grassbiomass ~ TCARI, data = CW_data), which = 1)


# TotalBiomass (gm/m2)
plot(NG_data$TCARI, NG_data$TotalBiomass,
     main = "Native grass TCARI vs Total biomass (gm/m2)",
     xlab = "TCARI", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ TCARI, data = NG_data), col = "red")

plot(CW_data$TCARI, CW_data$TotalBiomass,
     main = "Crested Wheatgrass TCARI vs Total biomass (gm/m2)",
     xlab = "TCARI", ylab = "Total biomass (gm/m2)")
abline(lm( TotalBiomass~ TCARI, data = CW_data), col = "red")

plot(lm(TotalBiomass ~ TCARI, data = NG_data), which = 1)
plot(lm(TotalBiomass ~ TCARI, data = CW_data), which = 1)


# LAI
plot(NG_data$TCARI, NG_data$LAI,
     main = "Native grass TCARI vs Leaf area index",
     xlab = "TCARI", ylab = "Leaf area index")
abline(lm(LAI ~ TCARI, data = NG_data), col = "red")

plot(CW_data$TCARI, CW_data$LAI,
     main = "Crested Wheatgrass TCARI vs Leaf area index",
     xlab = "TCARI", ylab = "Leaf area index")
abline(lm( LAI~ TCARI, data = CW_data), col = "red")

plot(lm(LAI ~ TCARI, data = NG_data), which = 1)
plot(lm(LAI ~ TCARI, data = CW_data), which = 1)


# ForbsCover
plot(NG_data$TCARI, NG_data$ForbsCover,
     main = "Native grass TCARI vs Forbs cover (%)",
     xlab = "TCARI", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ TCARI, data = NG_data), col = "red")

plot(CW_data$TCARI, CW_data$ForbsCover,
     main = "Crested Wheatgrass TCARI vs Forbs cover (%)",
     xlab = "TCARI", ylab = "Forbs cover (%)")
abline(lm( ForbsCover~ TCARI, data = CW_data), col = "red")

plot(lm(ForbsCover ~ TCARI, data = NG_data), which = 1)
plot(lm(ForbsCover ~ TCARI, data = CW_data), which = 1)


# Bare Ground Cover (%)
plot(NG_data$TCARI, NG_data$BareGroundCover,
     main = "Native grass TCARI vs Bare ground cover (%)",
     xlab = "TCARI", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ TCARI, data = NG_data), col = "red")

plot(CW_data$TCARI, CW_data$BareGroundCover,
     main = "Crested Wheatgrass TCARI vs Bare ground cover (%)",
     xlab = "TCARI", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover~ TCARI, data = CW_data), col = "red")

plot(lm(BareGroundCover ~ TCARI, data = NG_data), which = 1)
plot(lm(BareGroundCover ~ TCARI, data = CW_data), which = 1)


# DeadBiomass (gm/m2)
plot(NG_data$TCARI, NG_data$DeadBiomass,
     main = "Native grass TCARI vs Dead biomass (gm/m2)",
     xlab = "TCARI", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ TCARI, data = NG_data), col = "red")

plot(CW_data$TCARI, CW_data$DeadBiomass,
     main = "Crested Wheatgrass TCARI vs Dead biomass (gm/m2)",
     xlab = "TCARI", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass~ TCARI, data = CW_data), col = "red")

plot(lm(DeadBiomass~ TCARI, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ TCARI, data = CW_data), which = 1)


# ForbsBiomass(gm/m2)
plot(NG_data$TCARI, NG_data$ForbsBiomass,
     main = "Native grass TCARI vs Forb biomass (gm/m2)",
     xlab = "TCARI", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ TCARI, data = NG_data), col = "red")

plot(CW_data$TCARI, CW_data$ForbsBiomass,
     main = "Crested Wheatgrass TCARI vs Forb biomass (gm/m2)",
     xlab = "TCARI", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass~ TCARI, data = CW_data), col = "red")

plot(lm(ForbsBiomass~ TCARI, data = NG_data), which = 1)
plot(lm(ForbsBiomass ~ TCARI, data = CW_data), which = 1)

####################
#***Native Grass***#
####################
##MCARI2 With 9 Biophysical properties

par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

for (var in biophysical_vars) {
  plot(NG_data$MCARI2, NG_data[[var]],
       main = paste("MCARI2 vs", var, "(Native grass)"),
       xlab = "MCARI2", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(NG_data[[var]] ~ MCARI2, data = NG_data), col = "red")
  
  r2 <- summary(lm(NG_data[[var]] ~ MCARI2, data = NG_data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

##########################
#***Crested Wheatgrass***#
#*########################
# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against MCARI2 in one go***#
for (var in biophysical_vars) {
  plot(data$MCARI2, data[[var]],
       main = paste("MCARI2 vs", var, "(Crested  Wheatgrass)"),
       xlab = "MCARI2", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ MCARI2, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ MCARI2, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Grass Cover (%)
plot(NG_data$MCARI2, NG_data$Cover,
     main = "Native grass MCARI2 vs Grass cover (%)",
     xlab = "MCARI2", ylab = "Grass cover (%)")
abline(lm(Cover ~ MCARI2, data = NG_data), col = "red")

plot(CW_data$MCARI2, CW_data$Cover,
     main = "Crested Wheatgrass MCARI2 vs Grass cover (%)",
     xlab = "MCARI2", ylab = "Grass cover (%)")
abline(lm( Cover~ MCARI2, data = CW_data), col = "red")

plot(lm(Cover~ MCARI2, data = NG_data), which = 1)

plot(lm(Cover~ MCARI2, data = CW_data), which = 1)

# Height (cm)
plot(NG_data$MCARI2, NG_data$Height,
     main = "Native grass MCARI2 vs Height (cm)",
     xlab = "MCARI2", ylab = "Height (cm)")
abline(lm(Height ~ MCARI2, data = NG_data), col = "red")

plot(CW_data$MCARI2, CW_data$Height,
     main = "Crested Wheatgrass MCARI2 vs Height (cm)",
     xlab = "MCARI2", ylab = "Height (cm)")
abline(lm( Height~ MCARI2, data = CW_data), col = "red")

plot(lm(Height ~ MCARI2, data = NG_data), which = 1)
plot(lm(Height ~ MCARI2, data = CW_data), which = 1)


# Grassbiomass (gm/m2)
plot(NG_data$MCARI2, NG_data$Grassbiomass,
     main = "Native grass MCARI2 vs Grass biomass (gm/m2)",
     xlab = "MCARI2", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ MCARI2, data = NG_data), col = "red")

plot(CW_data$MCARI2, CW_data$Grassbiomass,
     main = "Crested Wheatgrass MCARI2 vs Grass biomass (gm/m2)",
     xlab = "MCARI2", ylab = "Grass biomass (gm/m2)")
abline(lm( Grassbiomass~ MCARI2, data = CW_data), col = "red")

plot(lm(Grassbiomass ~ MCARI2, data = NG_data), which = 1)
plot(lm(Grassbiomass ~ MCARI2, data = CW_data), which = 1)


# TotalBiomass (gm/m2)
plot(NG_data$MCARI2, NG_data$TotalBiomass,
     main = "Native grass MCARI2 vs Total biomass (gm/m2)",
     xlab = "MCARI2", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ MCARI2, data = NG_data), col = "red")

plot(CW_data$MCARI2, CW_data$TotalBiomass,
     main = "Crested Wheatgrass MCARI2 vs Total biomass (gm/m2)",
     xlab = "MCARI2", ylab = "Total biomass (gm/m2)")
abline(lm( TotalBiomass~ MCARI2, data = CW_data), col = "red")

plot(lm(TotalBiomass ~ MCARI2, data = NG_data), which = 1)
plot(lm(TotalBiomass ~ MCARI2, data = CW_data), which = 1)


# LAI
plot(NG_data$MCARI2, NG_data$LAI,
     main = "Native grass MCARI2 vs Leaf area index",
     xlab = "MCARI2", ylab = "Leaf area index")
abline(lm(LAI ~ MCARI2, data = NG_data), col = "red")

plot(CW_data$MCARI2, CW_data$LAI,
     main = "Crested Wheatgrass MCARI2 vs Leaf area index",
     xlab = "MCARI2", ylab = "Leaf area index")
abline(lm( LAI~ MCARI2, data = CW_data), col = "red")

plot(lm(LAI ~ MCARI2, data = NG_data), which = 1)
plot(lm(LAI ~ MCARI2, data = CW_data), which = 1)


# ForbsCover
plot(NG_data$MCARI2, NG_data$ForbsCover,
     main = "Native grass MCARI2 vs Forbs cover (%)",
     xlab = "MCARI2", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ MCARI2, data = NG_data), col = "red")

plot(CW_data$MCARI2, CW_data$ForbsCover,
     main = "Crested Wheatgrass MCARI2 vs Forbs cover (%)",
     xlab = "MCARI2", ylab = "Forbs cover (%)")
abline(lm( ForbsCover~ MCARI2, data = CW_data), col = "red")

plot(lm(ForbsCover ~ MCARI2, data = NG_data), which = 1)
plot(lm(ForbsCover ~ MCARI2, data = CW_data), which = 1)


# Bare Ground Cover (%)
plot(NG_data$MCARI2, NG_data$BareGroundCover,
     main = "Native grass MCARI2 vs Bare ground cover (%)",
     xlab = "MCARI2", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ MCARI2, data = NG_data), col = "red")

plot(CW_data$MCARI2, CW_data$BareGroundCover,
     main = "Crested Wheatgrass MCARI2 vs Bare ground cover (%)",
     xlab = "MCARI2", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover~ MCARI2, data = CW_data), col = "red")

plot(lm(BareGroundCover ~ MCARI2, data = NG_data), which = 1)
plot(lm(BareGroundCover ~ MCARI2, data = CW_data), which = 1)


# DeadBiomass (gm/m2)
plot(NG_data$MCARI2, NG_data$DeadBiomass,
     main = "Native grass MCARI2 vs Dead biomass (gm/m2)",
     xlab = "MCARI2", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ MCARI2, data = NG_data), col = "red")

plot(CW_data$MCARI2, CW_data$DeadBiomass,
     main = "Crested Wheatgrass MCARI2 vs Dead biomass (gm/m2)",
     xlab = "MCARI2", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass~ MCARI2, data = CW_data), col = "red")

plot(lm(DeadBiomass~ MCARI2, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ MCARI2, data = CW_data), which = 1)


# ForbsBiomass(gm/m2)
plot(NG_data$MCARI2, NG_data$ForbsBiomass,
     main = "Native grass MCARI2 vs Forb biomass (gm/m2)",
     xlab = "MCARI2", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ MCARI2, data = NG_data), col = "red")

plot(CW_data$MCARI2, CW_data$ForbsBiomass,
     main = "Crested Wheatgrass MCARI2 vs Forb biomass (gm/m2)",
     xlab = "MCARI2", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass~ MCARI2, data = CW_data), col = "red")

plot(lm(ForbsBiomass~ MCARI2, data = NG_data), which = 1)
plot(lm(ForbsBiomass ~ MCARI2, data = CW_data), which = 1)

####################
#***Native Grass***#
####################
##R700_R670 With 9 Biophysical properties
##R700_R670 With 9 Biophysical properties
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

for (var in biophysical_vars) {
  plot(NG_data$R700_R670, NG_data[[var]],
       main = paste("R700_R670 vs", var, "(Native grass)"),
       xlab = "R700_R670", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(NG_data[[var]] ~ R700_R670, data = NG_data), col = "red")
  
  r2 <- summary(lm(NG_data[[var]] ~ R700_R670, data = NG_data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

##########################
#***Crested Wheatgrass***#
#*########################
# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against R700_R670 in one go***#
for (var in biophysical_vars) {
  plot(data$R700_R670, data[[var]],
       main = paste("R700_R670 vs", var, "(Crested  Wheatgrass)"),
       xlab = "R700_R670", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ R700_R670, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ R700_R670, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Grass Cover (%)
plot(NG_data$R700_R670, NG_data$Cover,
     main = "Native grass R700_R670 vs Grass cover (%)",
     xlab = "R700_R670", ylab = "Grass cover (%)")
abline(lm(Cover ~ R700_R670, data = NG_data), col = "red")

plot(CW_data$R700_R670, CW_data$Cover,
     main = "Crested Wheatgrass R700_R670 vs Grass cover (%)",
     xlab = "R700_R670", ylab = "Grass cover (%)")
abline(lm( Cover~ R700_R670, data = CW_data), col = "red")

plot(lm(Cover~ R700_R670, data = NG_data), which = 1)

plot(lm(Cover~ R700_R670, data = CW_data), which = 1)

# Height (cm)
plot(NG_data$R700_R670, NG_data$Height,
     main = "Native grass R700_R670 vs Height (cm)",
     xlab = "R700_R670", ylab = "Height (cm)")
abline(lm(Height ~ R700_R670, data = NG_data), col = "red")

plot(CW_data$R700_R670, CW_data$Height,
     main = "Crested Wheatgrass R700_R670 vs Height (cm)",
     xlab = "R700_R670", ylab = "Height (cm)")
abline(lm( Height~ R700_R670, data = CW_data), col = "red")

plot(lm(Height ~ R700_R670, data = NG_data), which = 1)
plot(lm(Height ~ R700_R670, data = CW_data), which = 1)


# Grassbiomass (gm/m2)
plot(NG_data$R700_R670, NG_data$Grassbiomass,
     main = "Native grass R700_R670 vs Grass biomass (gm/m2)",
     xlab = "R700_R670", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ R700_R670, data = NG_data), col = "red")

plot(CW_data$R700_R670, CW_data$Grassbiomass,
     main = "Crested Wheatgrass R700_R670 vs Grass biomass (gm/m2)",
     xlab = "R700_R670", ylab = "Grass biomass (gm/m2)")
abline(lm( Grassbiomass~ R700_R670, data = CW_data), col = "red")

plot(lm(Grassbiomass ~ R700_R670, data = NG_data), which = 1)
plot(lm(Grassbiomass ~ R700_R670, data = CW_data), which = 1)


# TotalBiomass (gm/m2)
plot(NG_data$R700_R670, NG_data$TotalBiomass,
     main = "Native grass R700_R670 vs Total biomass (gm/m2)",
     xlab = "R700_R670", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ R700_R670, data = NG_data), col = "red")

plot(CW_data$R700_R670, CW_data$TotalBiomass,
     main = "Crested Wheatgrass R700_R670 vs Total biomass (gm/m2)",
     xlab = "R700_R670", ylab = "Total biomass (gm/m2)")
abline(lm( TotalBiomass~ R700_R670, data = CW_data), col = "red")

plot(lm(TotalBiomass ~ R700_R670, data = NG_data), which = 1)
plot(lm(TotalBiomass ~ R700_R670, data = CW_data), which = 1)


# LAI
plot(NG_data$R700_R670, NG_data$LAI,
     main = "Native grass R700_R670 vs Leaf area index",
     xlab = "R700_R670", ylab = "Leaf area index")
abline(lm(LAI ~ R700_R670, data = NG_data), col = "red")

plot(CW_data$R700_R670, CW_data$LAI,
     main = "Crested Wheatgrass R700_R670 vs Leaf area index",
     xlab = "R700_R670", ylab = "Leaf area index")
abline(lm( LAI~ R700_R670, data = CW_data), col = "red")

plot(lm(LAI ~ R700_R670, data = NG_data), which = 1)
plot(lm(LAI ~ R700_R670, data = CW_data), which = 1)


# ForbsCover
plot(NG_data$R700_R670, NG_data$ForbsCover,
     main = "Native grass R700_R670 vs Forbs cover (%)",
     xlab = "R700_R670", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ R700_R670, data = NG_data), col = "red")

plot(CW_data$R700_R670, CW_data$ForbsCover,
     main = "Crested Wheatgrass R700_R670 vs Forbs cover (%)",
     xlab = "R700_R670", ylab = "Forbs cover (%)")
abline(lm( ForbsCover~ R700_R670, data = CW_data), col = "red")

plot(lm(ForbsCover ~ R700_R670, data = NG_data), which = 1)
plot(lm(ForbsCover ~ R700_R670, data = CW_data), which = 1)


# Bare Ground Cover (%)
plot(NG_data$R700_R670, NG_data$BareGroundCover,
     main = "Native grass R700_R670 vs Bare ground cover (%)",
     xlab = "R700_R670", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ R700_R670, data = NG_data), col = "red")

plot(CW_data$R700_R670, CW_data$BareGroundCover,
     main = "Crested Wheatgrass R700_R670 vs Bare ground cover (%)",
     xlab = "R700_R670", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover~ R700_R670, data = CW_data), col = "red")

plot(lm(BareGroundCover ~ R700_R670, data = NG_data), which = 1)
plot(lm(BareGroundCover ~ R700_R670, data = CW_data), which = 1)


# DeadBiomass (gm/m2)
plot(NG_data$R700_R670, NG_data$DeadBiomass,
     main = "Native grass R700_R670 vs Dead biomass (gm/m2)",
     xlab = "R700_R670", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ R700_R670, data = NG_data), col = "red")

plot(CW_data$R700_R670, CW_data$DeadBiomass,
     main = "Crested Wheatgrass R700_R670 vs Dead biomass (gm/m2)",
     xlab = "R700_R670", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass~ R700_R670, data = CW_data), col = "red")

plot(lm(DeadBiomass~ R700_R670, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ R700_R670, data = CW_data), which = 1)


# ForbsBiomass(gm/m2)
plot(NG_data$R700_R670, NG_data$ForbsBiomass,
     main = "Native grass R700_R670 vs Forb biomass (gm/m2)",
     xlab = "R700_R670", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ R700_R670, data = NG_data), col = "red")

plot(CW_data$R700_R670, CW_data$ForbsBiomass,
     main = "Crested Wheatgrass R700_R670 vs Forb biomass (gm/m2)",
     xlab = "R700_R670", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass~ R700_R670, data = CW_data), col = "red")

plot(lm(ForbsBiomass~ R700_R670, data = NG_data), which = 1)
plot(lm(ForbsBiomass ~ R700_R670, data = CW_data), which = 1)

####################
#***native grass***#
####################
##RGR With 9 Biophysical properties
##RGR With 9 Biophysical properties
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

for (var in biophysical_vars) {
  plot(NG_data$RGR, NG_data[[var]],
       main = paste("RGR vs", var, "(Native grass)"),
       xlab = "RGR", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(NG_data[[var]] ~ RGR, data = NG_data), col = "red")
  
  r2 <- summary(lm(NG_data[[var]] ~ RGR, data = NG_data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

##########################
#***Crested Wheatgrass***#
#*########################
# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against RGR in one go***#
for (var in biophysical_vars) {
  plot(data$RGR, data[[var]],
       main = paste("RGR vs", var, "(Crested  Wheatgrass)"),
       xlab = "RGR", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ RGR, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ RGR, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Grass Cover (%)
plot(NG_data$RGR, NG_data$Cover,
     main = "Native grass RGR vs Grass cover (%)",
     xlab = "RGR", ylab = "Grass cover (%)")
abline(lm(Cover ~ RGR, data = NG_data), col = "red")

plot(CW_data$RGR, CW_data$Cover,
     main = "Crested Wheatgrass RGR vs Grass cover (%)",
     xlab = "RGR", ylab = "Grass cover (%)")
abline(lm( Cover~ RGR, data = CW_data), col = "red")

plot(lm(Cover~ RGR, data = NG_data), which = 1)

plot(lm(Cover~ RGR, data = CW_data), which = 1)

# Height (cm)
plot(NG_data$RGR, NG_data$Height,
     main = "Native grass RGR vs Height (cm)",
     xlab = "RGR", ylab = "Height (cm)")
abline(lm(Height ~ RGR, data = NG_data), col = "red")

plot(CW_data$RGR, CW_data$Height,
     main = "Crested Wheatgrass RGR vs Height (cm)",
     xlab = "RGR", ylab = "Height (cm)")
abline(lm( Height~ RGR, data = CW_data), col = "red")

plot(lm(Height ~ RGR, data = NG_data), which = 1)
plot(lm(Height ~ RGR, data = CW_data), which = 1)


# Grassbiomass (gm/m2)
plot(NG_data$RGR, NG_data$Grassbiomass,
     main = "Native grass RGR vs Grass biomass (gm/m2)",
     xlab = "RGR", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ RGR, data = NG_data), col = "red")

plot(CW_data$RGR, CW_data$Grassbiomass,
     main = "Crested Wheatgrass RGR vs Grass biomass (gm/m2)",
     xlab = "RGR", ylab = "Grass biomass (gm/m2)")
abline(lm( Grassbiomass~ RGR, data = CW_data), col = "red")

plot(lm(Grassbiomass ~ RGR, data = NG_data), which = 1)
plot(lm(Grassbiomass ~ RGR, data = CW_data), which = 1)


# TotalBiomass (gm/m2)
plot(NG_data$RGR, NG_data$TotalBiomass,
     main = "Native grass RGR vs Total biomass (gm/m2)",
     xlab = "RGR", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ RGR, data = NG_data), col = "red")

plot(CW_data$RGR, CW_data$TotalBiomass,
     main = "Crested Wheatgrass RGR vs Total biomass (gm/m2)",
     xlab = "RGR", ylab = "Total biomass (gm/m2)")
abline(lm( TotalBiomass~ RGR, data = CW_data), col = "red")

plot(lm(TotalBiomass ~ RGR, data = NG_data), which = 1)
plot(lm(TotalBiomass ~ RGR, data = CW_data), which = 1)


# LAI
plot(NG_data$RGR, NG_data$LAI,
     main = "Native grass RGR vs Leaf area index",
     xlab = "RGR", ylab = "Leaf area index")
abline(lm(LAI ~ RGR, data = NG_data), col = "red")

plot(CW_data$RGR, CW_data$LAI,
     main = "Crested Wheatgrass RGR vs Leaf area index",
     xlab = "RGR", ylab = "Leaf area index")
abline(lm( LAI~ RGR, data = CW_data), col = "red")

plot(lm(LAI ~ RGR, data = NG_data), which = 1)
plot(lm(LAI ~ RGR, data = CW_data), which = 1)


# ForbsCover
plot(NG_data$RGR, NG_data$ForbsCover,
     main = "Native grass RGR vs Forbs cover (%)",
     xlab = "RGR", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ RGR, data = NG_data), col = "red")

plot(CW_data$RGR, CW_data$ForbsCover,
     main = "Crested Wheatgrass RGR vs Forbs cover (%)",
     xlab = "RGR", ylab = "Forbs cover (%)")
abline(lm( ForbsCover~ RGR, data = CW_data), col = "red")

plot(lm(ForbsCover ~ RGR, data = NG_data), which = 1)
plot(lm(ForbsCover ~ RGR, data = CW_data), which = 1)


# Bare Ground Cover (%)
plot(NG_data$RGR, NG_data$BareGroundCover,
     main = "Native grass RGR vs Bare ground cover (%)",
     xlab = "RGR", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ RGR, data = NG_data), col = "red")

plot(CW_data$RGR, CW_data$BareGroundCover,
     main = "Crested Wheatgrass RGR vs Bare ground cover (%)",
     xlab = "RGR", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover~ RGR, data = CW_data), col = "red")

plot(lm(BareGroundCover ~ RGR, data = NG_data), which = 1)
plot(lm(BareGroundCover ~ RGR, data = CW_data), which = 1)


# DeadBiomass (gm/m2)
plot(NG_data$RGR, NG_data$DeadBiomass,
     main = "Native grass RGR vs Dead biomass (gm/m2)",
     xlab = "RGR", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ RGR, data = NG_data), col = "red")

plot(CW_data$RGR, CW_data$DeadBiomass,
     main = "Crested Wheatgrass RGR vs Dead biomass (gm/m2)",
     xlab = "RGR", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass~ RGR, data = CW_data), col = "red")

plot(lm(DeadBiomass~ RGR, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ RGR, data = CW_data), which = 1)


# ForbsBiomass(gm/m2)
plot(NG_data$RGR, NG_data$ForbsBiomass,
     main = "Native grass RGR vs Forb biomass (gm/m2)",
     xlab = "RGR", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ RGR, data = NG_data), col = "red")

plot(CW_data$RGR, CW_data$ForbsBiomass,
     main = "Crested Wheatgrass RGR vs Forb biomass (gm/m2)",
     xlab = "RGR", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass~ RGR, data = CW_data), col = "red")

plot(lm(ForbsBiomass~ RGR, data = NG_data), which = 1)
plot(lm(ForbsBiomass ~ RGR, data = CW_data), which = 1)

####################
#***native grass***#
####################
##ARI_550_700 With 9 Biophysical properties
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

for (var in biophysical_vars) {
  plot(NG_data$ARI_550_700, NG_data[[var]],
       main = paste("ARI_550_700 vs", var, "(Native grass)"),
       xlab = "ARI_550_700", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(NG_data[[var]] ~ ARI_550_700, data = NG_data), col = "red")
  
  r2 <- summary(lm(NG_data[[var]] ~ ARI_550_700, data = NG_data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

##########################
#***Crested Wheatgrass***#
#*########################
# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against ARI_550_700 in one go***#
for (var in biophysical_vars) {
  plot(data$ARI_550_700, data[[var]],
       main = paste("ARI_550_700 vs", var, "(Crested  Wheatgrass)"),
       xlab = "ARI_550_700", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ ARI_550_700, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ ARI_550_700, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Grass Cover (%)
plot(NG_data$ARI_550_700, NG_data$Cover,
     main = "Native grass ARI_550_700 vs Grass cover (%)",
     xlab = "ARI_550_700", ylab = "Grass cover (%)")
abline(lm(Cover ~ ARI_550_700, data = NG_data), col = "red")

plot(CW_data$ARI_550_700, CW_data$Cover,
     main = "Crested Wheatgrass ARI_550_700 vs Grass cover (%)",
     xlab = "ARI_550_700", ylab = "Grass cover (%)")
abline(lm( Cover~ ARI_550_700, data = CW_data), col = "red")

plot(lm(Cover~ ARI_550_700, data = NG_data), which = 1)

plot(lm(Cover~ ARI_550_700, data = CW_data), which = 1)

# Height (cm)
plot(NG_data$ARI_550_700, NG_data$Height,
     main = "Native grass ARI_550_700 vs Height (cm)",
     xlab = "ARI_550_700", ylab = "Height (cm)")
abline(lm(Height ~ ARI_550_700, data = NG_data), col = "red")

plot(CW_data$ARI_550_700, CW_data$Height,
     main = "Crested Wheatgrass ARI_550_700 vs Height (cm)",
     xlab = "ARI_550_700", ylab = "Height (cm)")
abline(lm( Height~ ARI_550_700, data = CW_data), col = "red")

plot(lm(Height ~ ARI_550_700, data = NG_data), which = 1)
plot(lm(Height ~ ARI_550_700, data = CW_data), which = 1)


# Grassbiomass (gm/m2)
plot(NG_data$ARI_550_700, NG_data$Grassbiomass,
     main = "Native grass ARI_550_700 vs Grass biomass (gm/m2)",
     xlab = "ARI_550_700", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ ARI_550_700, data = NG_data), col = "red")

plot(CW_data$ARI_550_700, CW_data$Grassbiomass,
     main = "Crested Wheatgrass ARI_550_700 vs Grass biomass (gm/m2)",
     xlab = "ARI_550_700", ylab = "Grass biomass (gm/m2)")
abline(lm( Grassbiomass~ ARI_550_700, data = CW_data), col = "red")

plot(lm(Grassbiomass ~ ARI_550_700, data = NG_data), which = 1)
plot(lm(Grassbiomass ~ ARI_550_700, data = CW_data), which = 1)


# TotalBiomass (gm/m2)
plot(NG_data$ARI_550_700, NG_data$TotalBiomass,
     main = "Native grass ARI_550_700 vs Total biomass (gm/m2)",
     xlab = "ARI_550_700", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ ARI_550_700, data = NG_data), col = "red")

plot(CW_data$ARI_550_700, CW_data$TotalBiomass,
     main = "Crested Wheatgrass ARI_550_700 vs Total biomass (gm/m2)",
     xlab = "ARI_550_700", ylab = "Total biomass (gm/m2)")
abline(lm( TotalBiomass~ ARI_550_700, data = CW_data), col = "red")

plot(lm(TotalBiomass ~ ARI_550_700, data = NG_data), which = 1)
plot(lm(TotalBiomass ~ ARI_550_700, data = CW_data), which = 1)


# LAI
plot(NG_data$ARI_550_700, NG_data$LAI,
     main = "Native grass ARI_550_700 vs Leaf area index",
     xlab = "ARI_550_700", ylab = "Leaf area index")
abline(lm(LAI ~ ARI_550_700, data = NG_data), col = "red")

plot(CW_data$ARI_550_700, CW_data$LAI,
     main = "Crested Wheatgrass ARI_550_700 vs Leaf area index",
     xlab = "ARI_550_700", ylab = "Leaf area index")
abline(lm( LAI~ ARI_550_700, data = CW_data), col = "red")

plot(lm(LAI ~ ARI_550_700, data = NG_data), which = 1)
plot(lm(LAI ~ ARI_550_700, data = CW_data), which = 1)


# ForbsCover
plot(NG_data$ARI_550_700, NG_data$ForbsCover,
     main = "Native grass ARI_550_700 vs Forbs cover (%)",
     xlab = "ARI_550_700", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ ARI_550_700, data = NG_data), col = "red")

plot(CW_data$ARI_550_700, CW_data$ForbsCover,
     main = "Crested Wheatgrass ARI_550_700 vs Forbs cover (%)",
     xlab = "ARI_550_700", ylab = "Forbs cover (%)")
abline(lm( ForbsCover~ ARI_550_700, data = CW_data), col = "red")

plot(lm(ForbsCover ~ ARI_550_700, data = NG_data), which = 1)
plot(lm(ForbsCover ~ ARI_550_700, data = CW_data), which = 1)


# Bare Ground Cover (%)
plot(NG_data$ARI_550_700, NG_data$BareGroundCover,
     main = "Native grass ARI_550_700 vs Bare ground cover (%)",
     xlab = "ARI_550_700", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ ARI_550_700, data = NG_data), col = "red")

plot(CW_data$ARI_550_700, CW_data$BareGroundCover,
     main = "Crested Wheatgrass ARI_550_700 vs Bare ground cover (%)",
     xlab = "ARI_550_700", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover~ ARI_550_700, data = CW_data), col = "red")

plot(lm(BareGroundCover ~ ARI_550_700, data = NG_data), which = 1)
plot(lm(BareGroundCover ~ ARI_550_700, data = CW_data), which = 1)


# DeadBiomass (gm/m2)
plot(NG_data$ARI_550_700, NG_data$DeadBiomass,
     main = "Native grass ARI_550_700 vs Dead biomass (gm/m2)",
     xlab = "ARI_550_700", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ ARI_550_700, data = NG_data), col = "red")

plot(CW_data$ARI_550_700, CW_data$DeadBiomass,
     main = "Crested Wheatgrass ARI_550_700 vs Dead biomass (gm/m2)",
     xlab = "ARI_550_700", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass~ ARI_550_700, data = CW_data), col = "red")

plot(lm(DeadBiomass~ ARI_550_700, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ ARI_550_700, data = CW_data), which = 1)


# ForbsBiomass(gm/m2)
plot(NG_data$ARI_550_700, NG_data$ForbsBiomass,
     main = "Native grass ARI_550_700 vs Forb biomass (gm/m2)",
     xlab = "ARI_550_700", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ ARI_550_700, data = NG_data), col = "red")

plot(CW_data$ARI_550_700, CW_data$ForbsBiomass,
     main = "Crested Wheatgrass ARI_550_700 vs Forb biomass (gm/m2)",
     xlab = "ARI_550_700", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass~ ARI_550_700, data = CW_data), col = "red")

plot(lm(ForbsBiomass~ ARI_550_700, data = NG_data), which = 1)
plot(lm(ForbsBiomass ~ ARI_550_700, data = CW_data), which = 1)


#####################
#***Native grass***#
####################
##CRI_510_550 With 9 Biophysical properties
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

for (var in biophysical_vars) {
  plot(NG_data$CRI_510_550, NG_data[[var]],
       main = paste("CRI_510_550 vs", var, "(Native grass)"),
       xlab = "CRI_510_550", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(NG_data[[var]] ~ CRI_510_550, data = NG_data), col = "red")
  
  r2 <- summary(lm(NG_data[[var]] ~ CRI_510_550, data = NG_data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

##########################
#***Crested Wheatgrass***#
#*########################
# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against CRI_510_550 in one go***#
for (var in biophysical_vars) {
  plot(data$CRI_510_550, data[[var]],
       main = paste("CRI_510_550 vs", var, "(Crested  Wheatgrass)"),
       xlab = "CRI_510_550", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ CRI_510_550, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ CRI_510_550, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Grass Cover (%)
plot(NG_data$CRI_510_550, NG_data$Cover,
     main = "Native grass CRI_510_550 vs Grass cover (%)",
     xlab = "CRI_510_550", ylab = "Grass cover (%)")
abline(lm(Cover ~ CRI_510_550, data = NG_data), col = "red")

plot(CW_data$CRI_510_550, CW_data$Cover,
     main = "Crested Wheatgrass CRI_510_550 vs Grass cover (%)",
     xlab = "CRI_510_550", ylab = "Grass cover (%)")
abline(lm( Cover~ CRI_510_550, data = CW_data), col = "red")

plot(lm(Cover~ CRI_510_550, data = NG_data), which = 1)

plot(lm(Cover~ CRI_510_550, data = CW_data), which = 1)

# Height (cm)
plot(NG_data$CRI_510_550, NG_data$Height,
     main = "Native grass CRI_510_550 vs Height (cm)",
     xlab = "CRI_510_550", ylab = "Height (cm)")
abline(lm(Height ~ CRI_510_550, data = NG_data), col = "red")

plot(CW_data$CRI_510_550, CW_data$Height,
     main = "Crested Wheatgrass CRI_510_550 vs Height (cm)",
     xlab = "CRI_510_550", ylab = "Height (cm)")
abline(lm( Height~ CRI_510_550, data = CW_data), col = "red")

plot(lm(Height ~ CRI_510_550, data = NG_data), which = 1)
plot(lm(Height ~ CRI_510_550, data = CW_data), which = 1)


# Grassbiomass (gm/m2)
plot(NG_data$CRI_510_550, NG_data$Grassbiomass,
     main = "Native grass CRI_510_550 vs Grass biomass (gm/m2)",
     xlab = "CRI_510_550", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ CRI_510_550, data = NG_data), col = "red")

plot(CW_data$CRI_510_550, CW_data$Grassbiomass,
     main = "Crested Wheatgrass CRI_510_550 vs Grass biomass (gm/m2)",
     xlab = "CRI_510_550", ylab = "Grass biomass (gm/m2)")
abline(lm( Grassbiomass~ CRI_510_550, data = CW_data), col = "red")

plot(lm(Grassbiomass ~ CRI_510_550, data = NG_data), which = 1)
plot(lm(Grassbiomass ~ CRI_510_550, data = CW_data), which = 1)


# TotalBiomass (gm/m2)
plot(NG_data$CRI_510_550, NG_data$TotalBiomass,
     main = "Native grass CRI_510_550 vs Total biomass (gm/m2)",
     xlab = "CRI_510_550", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ CRI_510_550, data = NG_data), col = "red")

plot(CW_data$CRI_510_550, CW_data$TotalBiomass,
     main = "Crested Wheatgrass CRI_510_550 vs Total biomass (gm/m2)",
     xlab = "CRI_510_550", ylab = "Total biomass (gm/m2)")
abline(lm( TotalBiomass~ CRI_510_550, data = CW_data), col = "red")

plot(lm(TotalBiomass ~ CRI_510_550, data = NG_data), which = 1)
plot(lm(TotalBiomass ~ CRI_510_550, data = CW_data), which = 1)


# LAI
plot(NG_data$CRI_510_550, NG_data$LAI,
     main = "Native grass CRI_510_550 vs Leaf area index",
     xlab = "CRI_510_550", ylab = "Leaf area index")
abline(lm(LAI ~ CRI_510_550, data = NG_data), col = "red")

plot(CW_data$CRI_510_550, CW_data$LAI,
     main = "Crested Wheatgrass CRI_510_550 vs Leaf area index",
     xlab = "CRI_510_550", ylab = "Leaf area index")
abline(lm( LAI~ CRI_510_550, data = CW_data), col = "red")

plot(lm(LAI ~ CRI_510_550, data = NG_data), which = 1)
plot(lm(LAI ~ CRI_510_550, data = CW_data), which = 1)


# ForbsCover
plot(NG_data$CRI_510_550, NG_data$ForbsCover,
     main = "Native grass CRI_510_550 vs Forbs cover (%)",
     xlab = "CRI_510_550", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ CRI_510_550, data = NG_data), col = "red")

plot(CW_data$CRI_510_550, CW_data$ForbsCover,
     main = "Crested Wheatgrass CRI_510_550 vs Forbs cover (%)",
     xlab = "CRI_510_550", ylab = "Forbs cover (%)")
abline(lm( ForbsCover~ CRI_510_550, data = CW_data), col = "red")

plot(lm(ForbsCover ~ CRI_510_550, data = NG_data), which = 1)
plot(lm(ForbsCover ~ CRI_510_550, data = CW_data), which = 1)


# Bare Ground Cover (%)
plot(NG_data$CRI_510_550, NG_data$BareGroundCover,
     main = "Native grass CRI_510_550 vs Bare ground cover (%)",
     xlab = "CRI_510_550", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ CRI_510_550, data = NG_data), col = "red")

plot(CW_data$CRI_510_550, CW_data$BareGroundCover,
     main = "Crested Wheatgrass CRI_510_550 vs Bare ground cover (%)",
     xlab = "CRI_510_550", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover~ CRI_510_550, data = CW_data), col = "red")

plot(lm(BareGroundCover ~ CRI_510_550, data = NG_data), which = 1)
plot(lm(BareGroundCover ~ CRI_510_550, data = CW_data), which = 1)


# DeadBiomass (gm/m2)
plot(NG_data$CRI_510_550, NG_data$DeadBiomass,
     main = "Native grass CRI_510_550 vs Dead biomass (gm/m2)",
     xlab = "CRI_510_550", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ CRI_510_550, data = NG_data), col = "red")

plot(CW_data$CRI_510_550, CW_data$DeadBiomass,
     main = "Crested Wheatgrass CRI_510_550 vs Dead biomass (gm/m2)",
     xlab = "CRI_510_550", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass~ CRI_510_550, data = CW_data), col = "red")

plot(lm(DeadBiomass~ CRI_510_550, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ CRI_510_550, data = CW_data), which = 1)


# ForbsBiomass(gm/m2)
plot(NG_data$CRI_510_550, NG_data$ForbsBiomass,
     main = "Native grass CRI_510_550 vs Forb biomass (gm/m2)",
     xlab = "CRI_510_550", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ CRI_510_550, data = NG_data), col = "red")

plot(CW_data$CRI_510_550, CW_data$ForbsBiomass,
     main = "Crested Wheatgrass CRI_510_550 vs Forb biomass (gm/m2)",
     xlab = "CRI_510_550", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass~ CRI_510_550, data = CW_data), col = "red")

plot(lm(ForbsBiomass~ CRI_510_550, data = NG_data), which = 1)
plot(lm(ForbsBiomass ~ CRI_510_550, data = CW_data), which = 1)


####################
#***Native grass***#
####################
##NDVI_682_553 With 9 Biophysical properties
##NDVI_682_553 With 9 Biophysical properties
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

for (var in biophysical_vars) {
  plot(NG_data$NDVI_682_553, NG_data[[var]],
       main = paste("NDVI_682_553 vs", var, "(Native grass)"),
       xlab = "NDVI_682_553", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(NG_data[[var]] ~ NDVI_682_553, data = NG_data), col = "red")
  
  r2 <- summary(lm(NG_data[[var]] ~ NDVI_682_553, data = NG_data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

##########################
#***Crested Wheatgrass***#
#*########################
# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against NDVI_682_553 in one go***#
for (var in biophysical_vars) {
  plot(data$NDVI_682_553, data[[var]],
       main = paste("NDVI_682_553 vs", var, "(Crested  Wheatgrass)"),
       xlab = "NDVI_682_553", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ NDVI_682_553, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ NDVI_682_553, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Grass Cover (%)
plot(NG_data$NDVI_682_553, NG_data$Cover,
     main = "Native grass NDVI_682_553 vs Grass cover (%)",
     xlab = "NDVI_682_553", ylab = "Grass cover (%)")
abline(lm(Cover ~ NDVI_682_553, data = NG_data), col = "red")

plot(CW_data$NDVI_682_553, CW_data$Cover,
     main = "Crested Wheatgrass NDVI_682_553 vs Grass cover (%)",
     xlab = "NDVI_682_553", ylab = "Grass cover (%)")
abline(lm( Cover~ NDVI_682_553, data = CW_data), col = "red")

plot(lm(Cover~ NDVI_682_553, data = NG_data), which = 1)

plot(lm(Cover~ NDVI_682_553, data = CW_data), which = 1)

# Height (cm)
plot(NG_data$NDVI_682_553, NG_data$Height,
     main = "Native grass NDVI_682_553 vs Height (cm)",
     xlab = "NDVI_682_553", ylab = "Height (cm)")
abline(lm(Height ~ NDVI_682_553, data = NG_data), col = "red")

plot(CW_data$NDVI_682_553, CW_data$Height,
     main = "Crested Wheatgrass NDVI_682_553 vs Height (cm)",
     xlab = "NDVI_682_553", ylab = "Height (cm)")
abline(lm( Height~ NDVI_682_553, data = CW_data), col = "red")

plot(lm(Height ~ NDVI_682_553, data = NG_data), which = 1)
plot(lm(Height ~ NDVI_682_553, data = CW_data), which = 1)


# Grassbiomass (gm/m2)
plot(NG_data$NDVI_682_553, NG_data$Grassbiomass,
     main = "Native grass NDVI_682_553 vs Grass biomass (gm/m2)",
     xlab = "NDVI_682_553", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ NDVI_682_553, data = NG_data), col = "red")

plot(CW_data$NDVI_682_553, CW_data$Grassbiomass,
     main = "Crested Wheatgrass NDVI_682_553 vs Grass biomass (gm/m2)",
     xlab = "NDVI_682_553", ylab = "Grass biomass (gm/m2)")
abline(lm( Grassbiomass~ NDVI_682_553, data = CW_data), col = "red")

plot(lm(Grassbiomass ~ NDVI_682_553, data = NG_data), which = 1)
plot(lm(Grassbiomass ~ NDVI_682_553, data = CW_data), which = 1)


# TotalBiomass (gm/m2)
plot(NG_data$NDVI_682_553, NG_data$TotalBiomass,
     main = "Native grass NDVI_682_553 vs Total biomass (gm/m2)",
     xlab = "NDVI_682_553", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ NDVI_682_553, data = NG_data), col = "red")

plot(CW_data$NDVI_682_553, CW_data$TotalBiomass,
     main = "Crested Wheatgrass NDVI_682_553 vs Total biomass (gm/m2)",
     xlab = "NDVI_682_553", ylab = "Total biomass (gm/m2)")
abline(lm( TotalBiomass~ NDVI_682_553, data = CW_data), col = "red")

plot(lm(TotalBiomass ~ NDVI_682_553, data = NG_data), which = 1)
plot(lm(TotalBiomass ~ NDVI_682_553, data = CW_data), which = 1)


# LAI
plot(NG_data$NDVI_682_553, NG_data$LAI,
     main = "Native grass NDVI_682_553 vs Leaf area index",
     xlab = "NDVI_682_553", ylab = "Leaf area index")
abline(lm(LAI ~ NDVI_682_553, data = NG_data), col = "red")

plot(CW_data$NDVI_682_553, CW_data$LAI,
     main = "Crested Wheatgrass NDVI_682_553 vs Leaf area index",
     xlab = "NDVI_682_553", ylab = "Leaf area index")
abline(lm( LAI~ NDVI_682_553, data = CW_data), col = "red")

plot(lm(LAI ~ NDVI_682_553, data = NG_data), which = 1)
plot(lm(LAI ~ NDVI_682_553, data = CW_data), which = 1)


# ForbsCover
plot(NG_data$NDVI_682_553, NG_data$ForbsCover,
     main = "Native grass NDVI_682_553 vs Forbs cover (%)",
     xlab = "NDVI_682_553", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ NDVI_682_553, data = NG_data), col = "red")

plot(CW_data$NDVI_682_553, CW_data$ForbsCover,
     main = "Crested Wheatgrass NDVI_682_553 vs Forbs cover (%)",
     xlab = "NDVI_682_553", ylab = "Forbs cover (%)")
abline(lm( ForbsCover~ NDVI_682_553, data = CW_data), col = "red")

plot(lm(ForbsCover ~ NDVI_682_553, data = NG_data), which = 1)
plot(lm(ForbsCover ~ NDVI_682_553, data = CW_data), which = 1)


# Bare Ground Cover (%)
plot(NG_data$NDVI_682_553, NG_data$BareGroundCover,
     main = "Native grass NDVI_682_553 vs Bare ground cover (%)",
     xlab = "NDVI_682_553", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ NDVI_682_553, data = NG_data), col = "red")

plot(CW_data$NDVI_682_553, CW_data$BareGroundCover,
     main = "Crested Wheatgrass NDVI_682_553 vs Bare ground cover (%)",
     xlab = "NDVI_682_553", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover~ NDVI_682_553, data = CW_data), col = "red")

plot(lm(BareGroundCover ~ NDVI_682_553, data = NG_data), which = 1)
plot(lm(BareGroundCover ~ NDVI_682_553, data = CW_data), which = 1)


# DeadBiomass (gm/m2)
plot(NG_data$NDVI_682_553, NG_data$DeadBiomass,
     main = "Native grass NDVI_682_553 vs Dead biomass (gm/m2)",
     xlab = "NDVI_682_553", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ NDVI_682_553, data = NG_data), col = "red")

plot(CW_data$NDVI_682_553, CW_data$DeadBiomass,
     main = "Crested Wheatgrass NDVI_682_553 vs Dead biomass (gm/m2)",
     xlab = "NDVI_682_553", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass~ NDVI_682_553, data = CW_data), col = "red")

plot(lm(DeadBiomass~ NDVI_682_553, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ NDVI_682_553, data = CW_data), which = 1)


# ForbsBiomass(gm/m2)
plot(NG_data$NDVI_682_553, NG_data$ForbsBiomass,
     main = "Native grass NDVI_682_553 vs Forb biomass (gm/m2)",
     xlab = "NDVI_682_553", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ NDVI_682_553, data = NG_data), col = "red")

plot(CW_data$NDVI_682_553, CW_data$ForbsBiomass,
     main = "Crested Wheatgrass NDVI_682_553 vs Forb biomass (gm/m2)",
     xlab = "NDVI_682_553", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass~ NDVI_682_553, data = CW_data), col = "red")

plot(lm(ForbsBiomass~ NDVI_682_553, data = NG_data), which = 1)
plot(lm(ForbsBiomass ~ NDVI_682_553, data = CW_data), which = 1)


#################################
#** Doing for the whole data **##
#################################

####################
#***NDVI_682_553***#
####################
# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against NDVI_682_553 in one go***#
for (var in biophysical_vars) {
  plot(data$NDVI_682_553, data[[var]],
       #main = paste("Grass Cover (%)", var, ""),
       xlab = "NDVI_682_553", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ NDVI_682_553, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ NDVI_682_553, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Grass Cover (%)
plot(data$NDVI_682_553, data$Cover,
     main = "NDVI_682_553 vs Grass cover (%)",
     xlab = "NDVI_682_553", ylab = "Grass cover (%)")
abline(lm(Cover ~ NDVI_682_553, data = data), col = "red")
plot(lm(Cover~ NDVI_682_553, data = data), which = 1)

# Height (cm)
plot(data$NDVI_682_553, data$Height,
     main = "NDVI_682_553 vs Height (cm)",
     xlab = "NDVI_682_553", ylab = "Height (cm)")
abline(lm(Height ~ NDVI_682_553, data = data), col = "red")

plot(lm(Height ~ NDVI_682_553, data = data), which = 1)

# Grassbiomass (gm/m2)
plot(data$NDVI_682_553, data$Grassbiomass,
     main = "NDVI_682_553 vs Grass biomass (gm/m2)",
     xlab = "NDVI_682_553", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ NDVI_682_553, data = data), col = "red")

plot(lm(Grassbiomass ~ NDVI_682_553, data = data), which = 1)

# TotalBiomass (gm/m2)
plot(data$NDVI_682_553, data$TotalBiomass,
     main = "NDVI_682_553 vs Total biomass (gm/m2)",
     xlab = "NDVI_682_553", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ NDVI_682_553, data = data), col = "red")

plot(lm(TotalBiomass ~ NDVI_682_553, data = data), which = 1)

# LAI
plot(data$NDVI_682_553, data$LAI,
     main = "NDVI_682_553 vs Leaf area index",
     xlab = "NDVI_682_553", ylab = "Leaf area index")
abline(lm(LAI ~ NDVI_682_553, data = data), col = "red")

plot(lm(LAI ~ NDVI_682_553, data = data), which = 1)

# ForbsCover
plot(data$NDVI_682_553, data$ForbsCover,
     main = "Native grass NDVI_682_553 vs Forbs cover (%)",
     xlab = "NDVI_682_553", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ NDVI_682_553, data = data), col = "red")

plot(lm(ForbsCover ~ NDVI_682_553, data = data), which = 1)

# Bare Ground Cover (%)
plot(data$NDVI_682_553, data$BareGroundCover,
     main = "NDVI_682_553 vs Bare ground cover (%)",
     xlab = "NDVI_682_553", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ NDVI_682_553, data = data), col = "red")

plot(lm(BareGroundCover ~ NDVI_682_553, data = data), which = 1)

# DeadBiomass (gm/m2)
plot(data$NDVI_682_553, data$DeadBiomass,
     main = "NDVI_682_553 vs Dead biomass (gm/m2)",
     xlab = "NDVI_682_553", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ NDVI_682_553, data = data), col = "red")

plot(lm(DeadBiomass~ NDVI_682_553, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ NDVI_682_553, data = data), which = 1)

# ForbsBiomass(gm/m2)
plot(data$NDVI_682_553, data$ForbsBiomass,
     main = "NDVI_682_553 vs Forb biomass (gm/m2)",
     xlab = "NDVI_682_553", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ NDVI_682_553, data = data), col = "red")

plot(lm(ForbsBiomass~ NDVI_682_553, data = data), which = 1)


####################
#***NDNI***#
####################
# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against NDNI in one go***#
for (var in biophysical_vars) {
  plot(data$NDNI, data[[var]],
       #main = paste("NDNI vs", var, ""),
       xlab = "NDNI", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ NDNI, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ NDNI, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Grass Cover (%)
plot(data$NDNI, data$Cover,
     main = "NDNI vs Grass cover (%)",
     xlab = "NDNI", ylab = "Grass cover (%)")
abline(lm(Cover ~ NDNI, data = data), col = "red")
plot(lm(Cover~ NDNI, data = data), which = 1)

# Height (cm)
plot(data$NDNI, data$Height,
     main = "NDNI vs Height (cm)",
     xlab = "NDNI", ylab = "Height (cm)")
abline(lm(Height ~ NDNI, data = data), col = "red")

plot(lm(Height ~ NDNI, data = data), which = 1)

# Grassbiomass (gm/m2)
plot(data$NDNI, data$Grassbiomass,
     main = "NDNI vs Grass biomass (gm/m2)",
     xlab = "NDNI", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ NDNI, data = data), col = "red")

plot(lm(Grassbiomass ~ NDNI, data = data), which = 1)

# TotalBiomass (gm/m2)
plot(data$NDNI, data$TotalBiomass,
     main = "NDNI vs Total biomass (gm/m2)",
     xlab = "NDNI", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ NDNI, data = data), col = "red")

plot(lm(TotalBiomass ~ NDNI, data = data), which = 1)

# LAI
plot(data$NDNI, data$LAI,
     main = "NDNI vs Leaf area index",
     xlab = "NDNI", ylab = "Leaf area index")
abline(lm(LAI ~ NDNI, data = data), col = "red")

plot(lm(LAI ~ NDNI, data = data), which = 1)

# ForbsCover
plot(data$NDNI, data$ForbsCover,
     main = "Native grass NDNI vs Forbs cover (%)",
     xlab = "NDNI", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ NDNI, data = data), col = "red")

plot(lm(ForbsCover ~ NDNI, data = data), which = 1)

# Bare Ground Cover (%)
plot(data$NDNI, data$BareGroundCover,
     main = "NDNI vs Bare ground cover (%)",
     xlab = "NDNI", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ NDNI, data = data), col = "red")

plot(lm(BareGroundCover ~ NDNI, data = data), which = 1)

# DeadBiomass (gm/m2)
plot(data$NDNI, data$DeadBiomass,
     main = "NDNI vs Dead biomass (gm/m2)",
     xlab = "NDNI", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ NDNI, data = data), col = "red")

plot(lm(DeadBiomass~ NDNI, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ NDNI, data = data), which = 1)

# ForbsBiomass(gm/m2)
plot(data$NDNI, data$ForbsBiomass,
     main = "NDNI vs Forb biomass (gm/m2)",
     xlab = "NDNI", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ NDNI, data = data), col = "red")

plot(lm(ForbsBiomass~ NDNI, data = data), which = 1)


####################
#***CAI***#
####################
# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against CAI in one go***#
for (var in biophysical_vars) {
  plot(data$CAI, data[[var]],
       #main = paste("CAI vs", var, ""),
       xlab = "CAI", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ CAI, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ CAI, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Grass Cover (%)
plot(data$CAI, data$Cover,
     main = "CAI vs Grass cover (%)",
     xlab = "CAI", ylab = "Grass cover (%)")
abline(lm(Cover ~ CAI, data = data), col = "red")
plot(lm(Cover~ CAI, data = data), which = 1)

# Height (cm)
plot(data$CAI, data$Height,
     main = "CAI vs Height (cm)",
     xlab = "CAI", ylab = "Height (cm)")
abline(lm(Height ~ CAI, data = data), col = "red")

plot(lm(Height ~ CAI, data = data), which = 1)

# Grassbiomass (gm/m2)
plot(data$CAI, data$Grassbiomass,
     main = "CAI vs Grass biomass (gm/m2)",
     xlab = "CAI", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ CAI, data = data), col = "red")

plot(lm(Grassbiomass ~ CAI, data = data), which = 1)

# TotalBiomass (gm/m2)
plot(data$CAI, data$TotalBiomass,
     main = "CAI vs Total biomass (gm/m2)",
     xlab = "CAI", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ CAI, data = data), col = "red")

plot(lm(TotalBiomass ~ CAI, data = data), which = 1)

# LAI
plot(data$CAI, data$LAI,
     main = "CAI vs Leaf area index",
     xlab = "CAI", ylab = "Leaf area index")
abline(lm(LAI ~ CAI, data = data), col = "red")

plot(lm(LAI ~ CAI, data = data), which = 1)

# ForbsCover
plot(data$CAI, data$ForbsCover,
     main = "Native grass CAI vs Forbs cover (%)",
     xlab = "CAI", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ CAI, data = data), col = "red")

plot(lm(ForbsCover ~ CAI, data = data), which = 1)

# Bare Ground Cover (%)
plot(data$CAI, data$BareGroundCover,
     main = "CAI vs Bare ground cover (%)",
     xlab = "CAI", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ CAI, data = data), col = "red")

plot(lm(BareGroundCover ~ CAI, data = data), which = 1)

# DeadBiomass (gm/m2)
plot(data$CAI, data$DeadBiomass,
     main = "CAI vs Dead biomass (gm/m2)",
     xlab = "CAI", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ CAI, data = data), col = "red")

plot(lm(DeadBiomass~ CAI, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ CAI, data = data), which = 1)

# ForbsBiomass(gm/m2)
plot(data$CAI, data$ForbsBiomass,
     main = "CAI vs Forb biomass (gm/m2)",
     xlab = "CAI", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ CAI, data = data), col = "red")

plot(lm(ForbsBiomass~ CAI, data = data), which = 1)


####################
#***PRI***#
####################
# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against PRI in one go***#
for (var in biophysical_vars) {
  plot(data$PRI, data[[var]],
       #main = paste("PRI vs", var, ""),
       xlab = "PRI", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ PRI, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ PRI, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Grass Cover (%)
plot(data$PRI, data$Cover,
     main = "PRI vs Grass cover (%)",
     xlab = "PRI", ylab = "Grass cover (%)")
abline(lm(Cover ~ PRI, data = data), col = "red")
plot(lm(Cover~ PRI, data = data), which = 1)

# Height (cm)
plot(data$PRI, data$Height,
     main = "PRI vs Height (cm)",
     xlab = "PRI", ylab = "Height (cm)")
abline(lm(Height ~ PRI, data = data), col = "red")

plot(lm(Height ~ PRI, data = data), which = 1)

# Grassbiomass (gm/m2)
plot(data$PRI, data$Grassbiomass,
     main = "PRI vs Grass biomass (gm/m2)",
     xlab = "PRI", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ PRI, data = data), col = "red")

plot(lm(Grassbiomass ~ PRI, data = data), which = 1)

# TotalBiomass (gm/m2)
plot(data$PRI, data$TotalBiomass,
     main = "PRI vs Total biomass (gm/m2)",
     xlab = "PRI", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ PRI, data = data), col = "red")

plot(lm(TotalBiomass ~ PRI, data = data), which = 1)

# LAI
plot(data$PRI, data$LAI,
     main = "PRI vs Leaf area index",
     xlab = "PRI", ylab = "Leaf area index")
abline(lm(LAI ~ PRI, data = data), col = "red")

plot(lm(LAI ~ PRI, data = data), which = 1)

# ForbsCover
plot(data$PRI, data$ForbsCover,
     main = "Native grass PRI vs Forbs cover (%)",
     xlab = "PRI", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ PRI, data = data), col = "red")

plot(lm(ForbsCover ~ PRI, data = data), which = 1)

# Bare Ground Cover (%)
plot(data$PRI, data$BareGroundCover,
     main = "PRI vs Bare ground cover (%)",
     xlab = "PRI", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ PRI, data = data), col = "red")

plot(lm(BareGroundCover ~ PRI, data = data), which = 1)

# DeadBiomass (gm/m2)
plot(data$PRI, data$DeadBiomass,
     main = "PRI vs Dead biomass (gm/m2)",
     xlab = "PRI", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ PRI, data = data), col = "red")

plot(lm(DeadBiomass~ PRI, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ PRI, data = data), which = 1)

# ForbsBiomass(gm/m2)
plot(data$PRI, data$ForbsBiomass,
     main = "PRI vs Forb biomass (gm/m2)",
     xlab = "PRI", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ PRI, data = data), col = "red")

plot(lm(ForbsBiomass~ PRI, data = data), which = 1)


####################
#***TCARI***#
####################
# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against TCARI in one go***#
for (var in biophysical_vars) {
  plot(data$TCARI, data[[var]],
       #main = paste("TCARI vs", var, ""),
       xlab = "TCARI", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ TCARI, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ TCARI, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Grass Cover (%)
plot(data$TCARI, data$Cover,
     main = "TCARI vs Grass cover (%)",
     xlab = "TCARI", ylab = "Grass cover (%)")
abline(lm(Cover ~ TCARI, data = data), col = "red")
plot(lm(Cover~ TCARI, data = data), which = 1)

# Height (cm)
plot(data$TCARI, data$Height,
     main = "TCARI vs Height (cm)",
     xlab = "TCARI", ylab = "Height (cm)")
abline(lm(Height ~ TCARI, data = data), col = "red")

plot(lm(Height ~ TCARI, data = data), which = 1)

# Grassbiomass (gm/m2)
plot(data$TCARI, data$Grassbiomass,
     main = "TCARI vs Grass biomass (gm/m2)",
     xlab = "TCARI", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ TCARI, data = data), col = "red")

plot(lm(Grassbiomass ~ TCARI, data = data), which = 1)

# TotalBiomass (gm/m2)
plot(data$TCARI, data$TotalBiomass,
     main = "TCARI vs Total biomass (gm/m2)",
     xlab = "TCARI", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ TCARI, data = data), col = "red")

plot(lm(TotalBiomass ~ TCARI, data = data), which = 1)

# LAI
plot(data$TCARI, data$LAI,
     main = "TCARI vs Leaf area index",
     xlab = "TCARI", ylab = "Leaf area index")
abline(lm(LAI ~ TCARI, data = data), col = "red")

plot(lm(LAI ~ TCARI, data = data), which = 1)

# ForbsCover
plot(data$TCARI, data$ForbsCover,
     main = "Native grass TCARI vs Forbs cover (%)",
     xlab = "TCARI", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ TCARI, data = data), col = "red")

plot(lm(ForbsCover ~ TCARI, data = data), which = 1)

# Bare Ground Cover (%)
plot(data$TCARI, data$BareGroundCover,
     main = "TCARI vs Bare ground cover (%)",
     xlab = "TCARI", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ TCARI, data = data), col = "red")

plot(lm(BareGroundCover ~ TCARI, data = data), which = 1)

# DeadBiomass (gm/m2)
plot(data$TCARI, data$DeadBiomass,
     main = "TCARI vs Dead biomass (gm/m2)",
     xlab = "TCARI", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ TCARI, data = data), col = "red")

plot(lm(DeadBiomass~ TCARI, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ TCARI, data = data), which = 1)

# ForbsBiomass(gm/m2)
plot(data$TCARI, data$ForbsBiomass,
     main = "TCARI vs Forb biomass (gm/m2)",
     xlab = "TCARI", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ TCARI, data = data), col = "red")

plot(lm(ForbsBiomass~ TCARI, data = data), which = 1)


####################
#***MCARI2***#
####################
# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against MCARI2 in one go***#
for (var in biophysical_vars) {
  plot(data$MCARI2, data[[var]],
       #main = paste("MCARI2 vs", var, ""),
       xlab = "MCARI2", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ MCARI2, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ MCARI2, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Grass Cover (%)
plot(data$MCARI2, data$Cover,
     main = "MCARI2 vs Grass cover (%)",
     xlab = "MCARI2", ylab = "Grass cover (%)")
abline(lm(Cover ~ MCARI2, data = data), col = "red")
plot(lm(Cover~ MCARI2, data = data), which = 1)

# Height (cm)
plot(data$MCARI2, data$Height,
     main = "MCARI2 vs Height (cm)",
     xlab = "MCARI2", ylab = "Height (cm)")
abline(lm(Height ~ MCARI2, data = data), col = "red")

plot(lm(Height ~ MCARI2, data = data), which = 1)

# Grassbiomass (gm/m2)
plot(data$MCARI2, data$Grassbiomass,
     main = "MCARI2 vs Grass biomass (gm/m2)",
     xlab = "MCARI2", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ MCARI2, data = data), col = "red")

plot(lm(Grassbiomass ~ MCARI2, data = data), which = 1)

# TotalBiomass (gm/m2)
plot(data$MCARI2, data$TotalBiomass,
     main = "MCARI2 vs Total biomass (gm/m2)",
     xlab = "MCARI2", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ MCARI2, data = data), col = "red")

plot(lm(TotalBiomass ~ MCARI2, data = data), which = 1)

# LAI
plot(data$MCARI2, data$LAI,
     main = "MCARI2 vs Leaf area index",
     xlab = "MCARI2", ylab = "Leaf area index")
abline(lm(LAI ~ MCARI2, data = data), col = "red")

plot(lm(LAI ~ MCARI2, data = data), which = 1)

# ForbsCover
plot(data$MCARI2, data$ForbsCover,
     main = "Native grass MCARI2 vs Forbs cover (%)",
     xlab = "MCARI2", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ MCARI2, data = data), col = "red")

plot(lm(ForbsCover ~ MCARI2, data = data), which = 1)

# Bare Ground Cover (%)
plot(data$MCARI2, data$BareGroundCover,
     main = "MCARI2 vs Bare ground cover (%)",
     xlab = "MCARI2", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ MCARI2, data = data), col = "red")

plot(lm(BareGroundCover ~ MCARI2, data = data), which = 1)

# DeadBiomass (gm/m2)
plot(data$MCARI2, data$DeadBiomass,
     main = "MCARI2 vs Dead biomass (gm/m2)",
     xlab = "MCARI2", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ MCARI2, data = data), col = "red")

plot(lm(DeadBiomass~ MCARI2, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ MCARI2, data = data), which = 1)

# ForbsBiomass(gm/m2)
plot(data$MCARI2, data$ForbsBiomass,
     main = "MCARI2 vs Forb biomass (gm/m2)",
     xlab = "MCARI2", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ MCARI2, data = data), col = "red")

plot(lm(ForbsBiomass~ MCARI2, data = data), which = 1)


####################
#***R700_R670***#
####################
# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against R700_R670 in one go***#
for (var in biophysical_vars) {
  plot(data$R700_R670, data[[var]],
       #main = paste("R700_R670 vs", var, ""),
       xlab = "R700_R670", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ R700_R670, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ R700_R670, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Grass Cover (%)
plot(data$R700_R670, data$Cover,
     main = "R700_R670 vs Grass cover (%)",
     xlab = "R700_R670", ylab = "Grass cover (%)")
abline(lm(Cover ~ R700_R670, data = data), col = "red")
plot(lm(Cover~ R700_R670, data = data), which = 1)

# Height (cm)
plot(data$R700_R670, data$Height,
     main = "R700_R670 vs Height (cm)",
     xlab = "R700_R670", ylab = "Height (cm)")
abline(lm(Height ~ R700_R670, data = data), col = "red")

plot(lm(Height ~ R700_R670, data = data), which = 1)

# Grassbiomass (gm/m2)
plot(data$R700_R670, data$Grassbiomass,
     main = "R700_R670 vs Grass biomass (gm/m2)",
     xlab = "R700_R670", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ R700_R670, data = data), col = "red")

plot(lm(Grassbiomass ~ R700_R670, data = data), which = 1)

# TotalBiomass (gm/m2)
plot(data$R700_R670, data$TotalBiomass,
     main = "R700_R670 vs Total biomass (gm/m2)",
     xlab = "R700_R670", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ R700_R670, data = data), col = "red")

plot(lm(TotalBiomass ~ R700_R670, data = data), which = 1)

# LAI
plot(data$R700_R670, data$LAI,
     main = "R700_R670 vs Leaf area index",
     xlab = "R700_R670", ylab = "Leaf area index")
abline(lm(LAI ~ R700_R670, data = data), col = "red")

plot(lm(LAI ~ R700_R670, data = data), which = 1)

# ForbsCover
plot(data$R700_R670, data$ForbsCover,
     main = "Native grass R700_R670 vs Forbs cover (%)",
     xlab = "R700_R670", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ R700_R670, data = data), col = "red")

plot(lm(ForbsCover ~ R700_R670, data = data), which = 1)

# Bare Ground Cover (%)
plot(data$R700_R670, data$BareGroundCover,
     main = "R700_R670 vs Bare ground cover (%)",
     xlab = "R700_R670", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ R700_R670, data = data), col = "red")

plot(lm(BareGroundCover ~ R700_R670, data = data), which = 1)

# DeadBiomass (gm/m2)
plot(data$R700_R670, data$DeadBiomass,
     main = "R700_R670 vs Dead biomass (gm/m2)",
     xlab = "R700_R670", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ R700_R670, data = data), col = "red")

plot(lm(DeadBiomass~ R700_R670, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ R700_R670, data = data), which = 1)

# ForbsBiomass(gm/m2)
plot(data$R700_R670, data$ForbsBiomass,
     main = "R700_R670 vs Forb biomass (gm/m2)",
     xlab = "R700_R670", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ R700_R670, data = data), col = "red")

plot(lm(ForbsBiomass~ R700_R670, data = data), which = 1)


####################
#***RGR***#
####################
# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against RGR in one go***#
for (var in biophysical_vars) {
  plot(data$RGR, data[[var]],
       #main = paste("RGR vs", var, ""),
       xlab = "RGR", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ RGR, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ RGR, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Grass Cover (%)
plot(data$RGR, data$Cover,
     main = "RGR vs Grass cover (%)",
     xlab = "RGR", ylab = "Grass cover (%)")
abline(lm(Cover ~ RGR, data = data), col = "red")
plot(lm(Cover~ RGR, data = data), which = 1)

# Height (cm)
plot(data$RGR, data$Height,
     main = "RGR vs Height (cm)",
     xlab = "RGR", ylab = "Height (cm)")
abline(lm(Height ~ RGR, data = data), col = "red")

plot(lm(Height ~ RGR, data = data), which = 1)

# Grassbiomass (gm/m2)
plot(data$RGR, data$Grassbiomass,
     main = "RGR vs Grass biomass (gm/m2)",
     xlab = "RGR", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ RGR, data = data), col = "red")

plot(lm(Grassbiomass ~ RGR, data = data), which = 1)

# TotalBiomass (gm/m2)
plot(data$RGR, data$TotalBiomass,
     main = "RGR vs Total biomass (gm/m2)",
     xlab = "RGR", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ RGR, data = data), col = "red")

plot(lm(TotalBiomass ~ RGR, data = data), which = 1)

# LAI
plot(data$RGR, data$LAI,
     main = "RGR vs Leaf area index",
     xlab = "RGR", ylab = "Leaf area index")
abline(lm(LAI ~ RGR, data = data), col = "red")

plot(lm(LAI ~ RGR, data = data), which = 1)

# ForbsCover
plot(data$RGR, data$ForbsCover,
     main = "Native grass RGR vs Forbs cover (%)",
     xlab = "RGR", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ RGR, data = data), col = "red")

plot(lm(ForbsCover ~ RGR, data = data), which = 1)

# Bare Ground Cover (%)
plot(data$RGR, data$BareGroundCover,
     main = "RGR vs Bare ground cover (%)",
     xlab = "RGR", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ RGR, data = data), col = "red")

plot(lm(BareGroundCover ~ RGR, data = data), which = 1)

# DeadBiomass (gm/m2)
plot(data$RGR, data$DeadBiomass,
     main = "RGR vs Dead biomass (gm/m2)",
     xlab = "RGR", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ RGR, data = data), col = "red")

plot(lm(DeadBiomass~ RGR, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ RGR, data = data), which = 1)

# ForbsBiomass(gm/m2)
plot(data$RGR, data$ForbsBiomass,
     main = "RGR vs Forb biomass (gm/m2)",
     xlab = "RGR", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ RGR, data = data), col = "red")

plot(lm(ForbsBiomass~ RGR, data = data), which = 1)


####################
#***ARI_550_700***#
####################
# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against ARI_550_700 in one go***#
for (var in biophysical_vars) {
  plot(data$ARI_550_700, data[[var]],
       #main = paste("ARI_550_700 vs", var, ""),
       xlab = "ARI_550_700", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ ARI_550_700, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ ARI_550_700, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Grass Cover (%)
plot(data$ARI_550_700, data$Cover,
     main = "ARI_550_700 vs Grass cover (%)",
     xlab = "ARI_550_700", ylab = "Grass cover (%)")
abline(lm(Cover ~ ARI_550_700, data = data), col = "red")
plot(lm(Cover~ ARI_550_700, data = data), which = 1)

# Height (cm)
plot(data$ARI_550_700, data$Height,
     main = "ARI_550_700 vs Height (cm)",
     xlab = "ARI_550_700", ylab = "Height (cm)")
abline(lm(Height ~ ARI_550_700, data = data), col = "red")

plot(lm(Height ~ ARI_550_700, data = data), which = 1)

# Grassbiomass (gm/m2)
plot(data$ARI_550_700, data$Grassbiomass,
     main = "ARI_550_700 vs Grass biomass (gm/m2)",
     xlab = "ARI_550_700", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ ARI_550_700, data = data), col = "red")

plot(lm(Grassbiomass ~ ARI_550_700, data = data), which = 1)

# TotalBiomass (gm/m2)
plot(data$ARI_550_700, data$TotalBiomass,
     main = "ARI_550_700 vs Total biomass (gm/m2)",
     xlab = "ARI_550_700", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ ARI_550_700, data = data), col = "red")

plot(lm(TotalBiomass ~ ARI_550_700, data = data), which = 1)

# LAI
plot(data$ARI_550_700, data$LAI,
     main = "ARI_550_700 vs Leaf area index",
     xlab = "ARI_550_700", ylab = "Leaf area index")
abline(lm(LAI ~ ARI_550_700, data = data), col = "red")

plot(lm(LAI ~ ARI_550_700, data = data), which = 1)

# ForbsCover
plot(data$ARI_550_700, data$ForbsCover,
     main = "Native grass ARI_550_700 vs Forbs cover (%)",
     xlab = "ARI_550_700", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ ARI_550_700, data = data), col = "red")

plot(lm(ForbsCover ~ ARI_550_700, data = data), which = 1)

# Bare Ground Cover (%)
plot(data$ARI_550_700, data$BareGroundCover,
     main = "ARI_550_700 vs Bare ground cover (%)",
     xlab = "ARI_550_700", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ ARI_550_700, data = data), col = "red")

plot(lm(BareGroundCover ~ ARI_550_700, data = data), which = 1)

# DeadBiomass (gm/m2)
plot(data$ARI_550_700, data$DeadBiomass,
     main = "ARI_550_700 vs Dead biomass (gm/m2)",
     xlab = "ARI_550_700", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ ARI_550_700, data = data), col = "red")

plot(lm(DeadBiomass~ ARI_550_700, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ ARI_550_700, data = data), which = 1)

# ForbsBiomass(gm/m2)
plot(data$ARI_550_700, data$ForbsBiomass,
     main = "ARI_550_700 vs Forb biomass (gm/m2)",
     xlab = "ARI_550_700", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ ARI_550_700, data = data), col = "red")

plot(lm(ForbsBiomass~ ARI_550_700, data = data), which = 1)


####################
#***CRI_510_550***#
####################
# Set up a 3x3 grid (for 9 plots - adjust if you have more/fewer)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

#***Plot all variables against CRI_510_550 in one go***#
for (var in biophysical_vars) {
  plot(data$CRI_510_550, data[[var]],
       # main = paste("CRI_510_550 vs", var, ""),
       xlab = "CRI_510_550", ylab = var,
       pch = 19, col = "blue")
  
  abline(lm(data[[var]] ~ CRI_510_550, data = data), col = "red")
  
  r2 <- summary(lm(data[[var]] ~ CRI_510_550, data = data))$r.squared
  legend("topright", legend = bquote(R^2 == .(round(r2, 3))), bty = "n")
}

# Reset to default single plot layout
par(mfrow = c(1, 1))

# Grass Cover (%)
plot(data$CRI_510_550, data$Cover,
     main = "CRI_510_550 vs Grass cover (%)",
     xlab = "CRI_510_550", ylab = "Grass cover (%)")
abline(lm(Cover ~ CRI_510_550, data = data), col = "red")
plot(lm(Cover~ CRI_510_550, data = data), which = 1)

# Height (cm)
plot(data$CRI_510_550, data$Height,
     main = "CRI_510_550 vs Height (cm)",
     xlab = "CRI_510_550", ylab = "Height (cm)")
abline(lm(Height ~ CRI_510_550, data = data), col = "red")

plot(lm(Height ~ CRI_510_550, data = data), which = 1)

# Grassbiomass (gm/m2)
plot(data$CRI_510_550, data$Grassbiomass,
     main = "CRI_510_550 vs Grass biomass (gm/m2)",
     xlab = "CRI_510_550", ylab = "Grass biomass (gm/m2)")
abline(lm(Grassbiomass ~ CRI_510_550, data = data), col = "red")

plot(lm(Grassbiomass ~ CRI_510_550, data = data), which = 1)

# TotalBiomass (gm/m2)
plot(data$CRI_510_550, data$TotalBiomass,
     main = "CRI_510_550 vs Total biomass (gm/m2)",
     xlab = "CRI_510_550", ylab = "Total biomass (gm/m2)")
abline(lm(TotalBiomass ~ CRI_510_550, data = data), col = "red")

plot(lm(TotalBiomass ~ CRI_510_550, data = data), which = 1)

# LAI
plot(data$CRI_510_550, data$LAI,
     main = "CRI_510_550 vs Leaf area index",
     xlab = "CRI_510_550", ylab = "Leaf area index")
abline(lm(LAI ~ CRI_510_550, data = data), col = "red")

plot(lm(LAI ~ CRI_510_550, data = data), which = 1)

# ForbsCover
plot(data$CRI_510_550, data$ForbsCover,
     main = "Native grass CRI_510_550 vs Forbs cover (%)",
     xlab = "CRI_510_550", ylab = "Forbs cover (%)")
abline(lm(ForbsCover ~ CRI_510_550, data = data), col = "red")

plot(lm(ForbsCover ~ CRI_510_550, data = data), which = 1)

# Bare Ground Cover (%)
plot(data$CRI_510_550, data$BareGroundCover,
     main = "CRI_510_550 vs Bare ground cover (%)",
     xlab = "CRI_510_550", ylab = "Bare ground cover (%)")
abline(lm(BareGroundCover ~ CRI_510_550, data = data), col = "red")

plot(lm(BareGroundCover ~ CRI_510_550, data = data), which = 1)

# DeadBiomass (gm/m2)
plot(data$CRI_510_550, data$DeadBiomass,
     main = "CRI_510_550 vs Dead biomass (gm/m2)",
     xlab = "CRI_510_550", ylab = "Dead biomass (gm/m2)")
abline(lm(DeadBiomass ~ CRI_510_550, data = data), col = "red")

plot(lm(DeadBiomass~ CRI_510_550, data = NG_data), which = 1)
plot(lm(DeadBiomass ~ CRI_510_550, data = data), which = 1)

# ForbsBiomass(gm/m2)
plot(data$CRI_510_550, data$ForbsBiomass,
     main = "CRI_510_550 vs Forb biomass (gm/m2)",
     xlab = "CRI_510_550", ylab = "Forb biomass (gm/m2)")
abline(lm(ForbsBiomass ~ CRI_510_550, data = data), col = "red")

plot(lm(ForbsBiomass~ CRI_510_550, data = data), which = 1)

#######################################
#**** Biophysical properties-wise****#
#######################################
biophysical_var <- c("Cover")
spectral_var <- c("NDNI" , "TCARI","MCARI2","R700_R670", "CRI_510_550", "NDVI_682_553" )
species_levels <- unique(data$Species)
colors <- c("darkgreen", "orange")

###################
#***1. Grass Cover***#
###################
#***NDNI***#
############ 
par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$NDNI, data$Cover,
     xlab = "NDNI", 
     ylab = "Cover (%)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Cover (%) vs NDNI by Species"
     )

# Add regression line (red)
abline(lm(Cover ~ NDNI, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(Cover ~ NDNI, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

############
#***NDVI***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$NDVI_682_553, data$Cover,
     xlab = "NDVI", 
     ylab = "Cover (%)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Cover (%) vs NDVI by Species"
     )

# Add regression line (red)
abline(lm(Cover ~ NDVI_682_553, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(Cover ~ NDVI_682_553, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


############
#***CRI_510_550***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$CRI_510_550, data$Cover,
     xlab = "CRI", 
     ylab = "Cover (%)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Cover (%) vs CRI by Species"
     )

# Add regression line (red)
abline(lm(Cover ~ CRI_510_550, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(Cover ~ CRI_510_550, data = data))$r.squared, 3)
legend("bottomright", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


############
#***MCARI2***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$MCARI2, data$Cover,
     xlab = "MCARI2", 
     ylab = "Cover (%)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Cover (%) vs MCARI2 by Species"
     )

# Add regression line (red)
abline(lm(Cover ~ MCARI2, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(Cover ~ MCARI2, data = data))$r.squared, 3)
legend("bottomright", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


############
#***R700_670***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$R700_R670, data$Cover,
     xlab = "R700_R670", 
     ylab = "Cover (%)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Cover (%) vs R700_R670 by Species"
     )

# Add regression line (red)
abline(lm(Cover ~ R700_R670, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(Cover ~ R700_R670, data = data))$r.squared, 3)
legend("bottomright", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


############
#***TCARI***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$TCARI, data$Cover,
     xlab = "TCARI", 
     ylab = "Cover (%)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Cover (%) vs TCARI by Species"
     )

# Add regression line (red)
abline(lm(Cover ~ TCARI, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(Cover ~ TCARI, data = data))$r.squared, 3)
legend("bottomright", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


###################
#***2. Height***#
###################
#***ARI***#
############ 
par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$ARI_550_700, data$Height,
     xlab = "ARI", 
     ylab = "Height (cm)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Height (cm) vs ARI by Species"
     )

# Add regression line (red)
abline(lm(Height ~ ARI_550_700, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(Height ~ ARI_550_700, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout

#***CRI***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$CRI_510_550, data$Height,
     xlab = "CRI_510_550", 
     ylab = "Height (cm)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Height (cm) vs CRI_510_550 by Species"
     )

# Add regression line (red)
abline(lm(Height ~ CRI_510_550, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(Height ~ CRI_510_550, data = data))$r.squared, 3)
legend("bottomright", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout

#***TCARI***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$TCARI, data$Height,
     xlab = "TCARI", 
     ylab = "Height (cm)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Height (cm) vs TCARI by Species"
)

# Add regression line (red)
abline(lm(Height ~ TCARI, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(Height ~ TCARI, data = data))$r.squared, 3)
legend("bottomright", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout

#***MCARI2***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
#colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
#plot(data$MCARI2, data$Height,
#     xlab = "MCARI2", 
#     ylab = "Height (cm)",
#     pch = 19,  # Solid circles
#     col = colors[data$Species],  # Color by species
     #main = "Height (cm) vs MCARI2 by Species"
#     )

# Add regression line (red)
#abline(lm(Height ~ MCARI2, data = data), col = "black", lwd = 1)

# Calculate and display R²
#r2 <- round(summary(lm(Height ~ MCARI2, data = data))$r.squared, 3)
#legend("bottomright", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout

#***NDNI***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$NDNI, data$Height,
     xlab = "NDNI", 
     ylab = "Height (cm)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Height (cm) vs NDNI by Species"
     )

# Add regression line (red)
abline(lm(Height ~ NDNI, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(Height ~ NDNI, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***R700_670***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$R700_R670, data$Height,
     xlab = "R700_R670", 
     ylab = "Height (cm)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Height (cm) vs R700_R670 by Species"
     )

# Add regression line (red)
abline(lm(Height ~ R700_R670, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(Height ~ R700_R670, data = data))$r.squared, 3)
legend("bottomright", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***RGR***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$RGR, data$Height,
     xlab = "RGR", 
     ylab = "Height (cm)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Height (cm) vs RGR by Species"
     )

# Add regression line (red)
abline(lm(Height ~ RGR, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(Height ~ RGR, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***3. Grass Biomass***#
#***CRI***#
############ 
par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$CRI_510_550, data$Grassbiomass,
     xlab = "CRI_510_550", 
     ylab = "Grass biomass (gm/m2)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Grass biomass (gm/m2) vs CRI by Species"
     )

# Add regression line (red)
abline(lm(Grassbiomass ~ CRI_510_550, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(Grassbiomass ~ CRI_510_550, data = data))$r.squared, 3)
legend("bottomright", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout

#***NDNI***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$NDNI, data$Grassbiomass,
     xlab = "NDNI", 
     ylab = "Grass biomass (gm/m2)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Grass biomass (%) vs NDNI by Species"
     )

# Add regression line (red)
abline(lm(Grassbiomass ~ NDNI, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(Grassbiomass ~ NDNI, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***NDVI***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$NDVI_682_553, data$Grassbiomass,
     xlab = "NDVI", 
     ylab = "Grass biomass (gm/m2)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Grass biomass (gm/m2) vs NDVI by Species"
     )

# Add regression line (red)
abline(lm(Grassbiomass ~ NDVI_682_553, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(Grassbiomass ~ NDVI_682_553, data = data))$r.squared, 3)
legend("bottomleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout

#***R700_670***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$R700_R670, data$Grassbiomass,
     xlab = "R700_R670", 
     ylab = "Grass biomass (gm/m2)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Grass biomass (gm/m2) vs R700_R670 by Species"
     )

# Add regression line (red)
abline(lm(Grassbiomass ~ R700_R670, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(Grassbiomass ~ R700_R670, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***RGR***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$RGR, data$Grassbiomass,
     xlab = "RGR", 
     ylab = "Grass biomass (gm/m2)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Grass biomass (gm/m2) vs RGR by Species"
     )

# Add regression line (red)
abline(lm(Grassbiomass ~ RGR, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(Grassbiomass ~ RGR, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***4. Total biomass***#
#***NDNI***#
############ 
par(mfrow = c(2, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$NDNI, data$TotalBiomass,
     xlab = "RGR", 
     ylab = "Total biomass (gm/m2)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Total biomass (gm/m2) vs NDNI by Species"
     )

# Add regression line (red)
abline(lm(TotalBiomass ~ NDNI, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(TotalBiomass ~ NDNI, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***MCARI2***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$MCARI2, data$TotalBiomass,
     xlab = "MCARI", 
     ylab = "Total biomass (gm/m2)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Total biomass (gm/m2) vs MCARI by Species"
     )

# Add regression line (red)
abline(lm(TotalBiomass ~ MCARI2, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(TotalBiomass ~ MCARI2, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***TCARI***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$TCARI, data$TotalBiomass,
     xlab = "TCARI", 
     ylab = "Total biomass (gm/m2)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Total biomass (gm/m2) vs TCARI by Species"
     )

# Add regression line (red)
abline(lm(TotalBiomass ~ TCARI, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(TotalBiomass ~ TCARI, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***5. Leaf Aarea Index***#
#***CRI***#
############ 
par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$CRI_510_550, data$LAI,
     xlab = "CRI", 
     ylab = "Leaf area index",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Leaf area index vs CRI_510_550 by Species"
     )

# Add regression line (red)
abline(lm(LAI ~ CRI_510_550, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(LAI ~ CRI_510_550, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***MCARI***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$MCARI2, data$LAI,
     xlab = "MCARI", 
     ylab = "Leaf area index",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Leaf area index vs MCARI by Species"
     )

# Add regression line (red)
abline(lm(LAI ~ MCARI2, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(LAI ~ MCARI2, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***NDNI***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$NDNI, data$LAI,
     xlab = "NDNI", 
     ylab = "Leaf area index",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Leaf area index vs NDNI by Species"
     )

# Add regression line (red)
abline(lm(LAI ~ NDNI, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(LAI ~ NDNI, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***NDVI***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$NDVI_682_553, data$LAI,
     xlab = "NDVI_682_553", 
     ylab = "Leaf area index",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Leaf area index vs NDVI by Species"
     )

# Add regression line (red)
abline(lm(LAI ~ NDVI_682_553, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(LAI ~ NDVI_682_553, data = data))$r.squared, 3)
legend("bottomleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***TCARI***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$TCARI, data$LAI,
     xlab = "TCARI", 
     ylab = "Leaf area index",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Leaf area index vs TCARI by Species"
     )

# Add regression line (red)
abline(lm(LAI ~ TCARI, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(LAI ~ TCARI, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout



#***R700_670***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$R700_R670, data$LAI,
     xlab = "R700_R670", 
     ylab = "Leaf area index",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Leaf area index vs R700_R670 by Species"
     )

# Add regression line (red)
abline(lm(LAI ~ R700_R670, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(LAI ~ R700_R670, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***Forbs cover***#
#***ARI_550_700***#
############ 
par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$ARI_550_700, data$ForbsCover,
     xlab = "ARI_550_700", 
     ylab = "Forbs cover (%)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Forbs cover (%) vs ARI_550_700 by Species"
     )

# Add regression line (red)
abline(lm(ForbsCover ~ ARI_550_700, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(ForbsCover ~ ARI_550_700, data = data))$r.squared, 3)
legend("topright", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***CRI_510_550***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$CRI_510_550, data$ForbsCover,
     xlab = "CRI_510_550", 
     ylab = "Forbs cover (%)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Forbs cover (%) vs CRI_510_550 by Species"
     )

# Add regression line (red)
abline(lm(ForbsCover ~ CRI_510_550, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(ForbsCover ~ CRI_510_550, data = data))$r.squared, 3)
legend("topright", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***NDNI***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$NDNI, data$ForbsCover,
     xlab = "NDNI", 
     ylab = "Forbs cover (%)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Forbs cover (%) vs NDNI by Species"
     )

# Add regression line (red)
abline(lm(ForbsCover ~ NDNI, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(ForbsCover ~ NDNI, data = data))$r.squared, 3)
legend("topright", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***RGR***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$RGR, data$ForbsCover,
     xlab = "RGR", 
     ylab = "Forbs cover (%)",
     pch = 19,  # Solid circles
     col = colors[data$Species]  # Color by species
     #main = "Forbs cover (90%) vs RGR by Species"
     )

# Add regression line (red)
abline(lm(ForbsCover ~ RGR, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(ForbsCover ~ RGR, data = data))$r.squared, 3)
legend("topright", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout



#***R700_670***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$R700_R670, data$ForbsCover,
     xlab = "R700_R670", 
     ylab = "Forbs cover (%)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Forbs cover (%) vs R700_R670 by Species"
     )

# Add regression line (red)
abline(lm(ForbsCover ~ R700_R670, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(ForbsCover ~ R700_R670, data = data))$r.squared, 3)
legend("topright", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***Bareground cover(%)***#
#***MCARI2***#
############ 
par(mfrow = c(2, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Turn off scientific notation
options(scipen = 999)

# Create scatter plot (colored points only)
plot(data$MCARI2, data$BareGroundCover,
     xlab = "MCARI2", 
     ylab = "Bare ground cover (%)",
     pch = 19,  # Solid circles
     col = colors[data$Species],
     cex.axis = 1# Color by species
     #main = "Bare ground cover (%) vs MCARI2 by Species"
     )

# Add regression line (red)
abline(lm(BareGroundCover ~ MCARI2, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(BareGroundCover ~ MCARI2, data = data))$r.squared, 3)
legend("topright", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***NDNI***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$NDNI, data$BareGroundCover,
     xlab = "NDNI", 
     ylab = "Bare ground cover (%)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
    #main = "Bare ground cover (%) vs NDNI by Species"
    )

# Add regression line (red)
abline(lm(BareGroundCover ~ NDNI, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(BareGroundCover ~ NDNI, data = data))$r.squared, 3)
legend("bottomleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***TCARI***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$TCARI, data$BareGroundCover,
     xlab = "TCARI", 
     ylab = "Bare ground cover (%)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Bare ground cover (%) vs TCARI by Species"
     )

# Add regression line (red)
abline(lm(BareGroundCover ~ TCARI, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(BareGroundCover ~ TCARI, data = data))$r.squared, 3)
legend("topright", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***Forbs biomass***#
#***ARI_550_700***#
############ 
par(mfrow = c(2, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$ARI_550_700, data$ForbsBiomass,
     xlab = "ARI_550_700", 
     ylab = "Forbs Biomass (gm/m2)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Forbs Biomass (gm/m2) vs ARI_550_700 by Species"
     )

# Add regression line (red)
abline(lm(ForbsBiomass ~ ARI_550_700, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(ForbsBiomass ~ ARI_550_700, data = data))$r.squared, 3)
legend("topright", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***NDNI***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$NDNI, data$ForbsBiomass,
     xlab = "NDNI", 
     ylab = "Forbs biomass (gm/m2)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Forbs biomass (gm/m2) vs NDNI by Species"
     )

# Add regression line (red)
abline(lm(ForbsBiomass ~ NDNI, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(ForbsBiomass ~ NDNI, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout



#***RGR***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$RGR, data$ForbsBiomass,
     xlab = "RGR", 
     ylab = "Forbs biomass (gm/m2)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Forbs biomass (gm/m2) vs RGR by Species"
     )

# Add regression line (red)
abline(lm(ForbsBiomass ~ RGR, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(ForbsBiomass ~ RGR, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout



#***R700_670***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$R700_R670, data$ForbsBiomass,
     xlab = "R700_R670", 
     ylab = "Forbs biomass (gm/m2)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Forbs biomass (gm/m2) vs R700_R670 by Species"
     )

# Add regression line (red)
abline(lm(ForbsBiomass ~ R700_R670, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(ForbsBiomass ~ R700_R670, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***Dead biomass***#
#***MCARI2***#
############ 
par(mfrow = c(1, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$MCARI2, data$DeadBiomass,
     xlab = "MCARI2", 
     ylab = "Dead biomass (gm/m2)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Dead biomass (gm/m2) vs MCARI2 by Species"
     )

# Add regression line (red)
abline(lm(DeadBiomass ~ MCARI2, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(DeadBiomass ~ MCARI2, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout


#***NDNI***#
############ 
#par(mfrow = c(3, 2))  # adjust rows and columns as needed

# Set colors for each species
colors <- c("Native grass" = "blue", "Crested Wheatgrass" = "red")

# Create scatter plot (colored points only)
plot(data$NDNI, data$DeadBiomass,
     xlab = "NDNI", 
     ylab = "Dead biomass (gm/m2)",
     pch = 19,  # Solid circles
     col = colors[data$Species],  # Color by species
     #main = "Dead biomass (gm/m2) vs NDNI by Species"
     )

# Add regression line (red)
abline(lm(DeadBiomass ~ NDNI, data = data), col = "black", lwd = 1)

# Calculate and display R²
r2 <- round(summary(lm(DeadBiomass ~ NDNI, data = data))$r.squared, 3)
legend("topleft", legend = bquote(R^2 == .(r2)), bty = "n")

#par(mfrow = c(1, 1))  # reset layout

#############################################################################################################
#########################Correlation/regression test of biophysical and spectral indices#####################
library(readr)
#setwd("D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data")
setwd("C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data")

#Plot wise biophysical data
Biophy <- read_csv("PlotWiseDescriptiveStat.csv")
#View(Biophy) ## NOTE: I added a species column manually in the excel sheet.
names(Biophy)

#Add a column with the name Plot so it mathes with the spectral indices list
Biophy$Plot <- Biophy$Site
#View(Biophy)

# List all your loaded data frames (replace with your actual object names)
data_list <- list(Biophy, combined_data)

# Merge all by "Plot" (keeps all rows and fills missing values with NA)
data <- reduce(data_list, ~ full_join(.x, .y, by = "Plot"))
#View(data)
names(data)

# Subset for Crested Wheatgrass
Native_data <- data %>% 
  filter(Species == "Native grass")
#View(Native_data)

data <- data %>% 
  filter(Species == "Crested Wheatgrass")
#View(data)

# Define column names (update with your actual column names)
biophysical_vars <- c("Cover", "Height", "Grassbiomass", "TotalBiomass", 
                      "LAI", "ForbsCover", "StandingDeadCover", "LitterCover","BareGroundCover"
                      ,"ShrubCover","ShrubBiomass", "ForbsBiomass","DeadBiomass")

spectral_indices <- c("NDNI", "CAI", "PRI", "TCARI","MCARI2","R700_R670","RGR","ARI_550_700","CRI_510_550"
                      ,"NDVI_682_553")


# Create all pairs of variables
combinations <- expand.grid(Biophysical = biophysical_vars, Spectral = spectral_indices)

# Create a long dataframe with all pairs
plot_data <- purrr::map_dfr(1:nrow(combinations), function(i) {
  bio <- combinations$Biophysical[i]
  spec <- combinations$Spectral[i]
  
  tibble(
    Biophysical = bio,
    Spectral = spec,
    x = data[[spec]],
    y = data[[bio]]
  )
})
#View(plot_data)


############## I LIKE THIS PLOT!!!!!!!!!!!!!!!!!!##############
##### For Crested wheatgrass#
#############################
# Subset the data to include only the two groups
library(ggcorrplot)
# Define column names (update with your actual column names)
biophysical_vars <- c("Cover", "Height", "Grassbiomass", "TotalBiomass", 
                      "LAI", "ForbsCover", "StandingDeadCover", "LitterCover","BareGroundCover"
                      ,"ShrubCover","ShrubBiomass", "ForbsBiomass","DeadBiomass")

spectral_indices <- c("NDNI", "CAI", "PRI", "TCARI","MCARI2","R700_R670","RGR","ARI_550_700","CRI_510_550"
                      ,"NDVI_682_553")

subset_data <- data[, c(biophysical_vars, spectral_indices)]
subset_data <- subset_data %>% 
  select(-StandingDeadCover, -LitterCover,-ShrubCover, -ShrubBiomass)
#View(subset_data)
names(subset_data)

# Compute correlation matrix and p-values
cor_matrix <- cor(subset_data, use = "complete.obs")
p_matrix <- cor_pmat(subset_data)  # Compute p-values

biophysical_vars <- c("Cover", "Height", "Grassbiomass", "TotalBiomass", 
                      "LAI", "ForbsCover", "BareGroundCover", "ForbsBiomass","DeadBiomass")

spectral_indices <- c("NDNI", "CAI", "PRI", "TCARI","MCARI2","R700_R670","RGR","ARI_550_700","CRI_510_550"
                      ,"NDVI_682_553")

# Keep only cross-group correlations (biophysical vs. spectral)
cor_matrix <- cor_matrix[biophysical_vars, spectral_indices]
p_matrix <- p_matrix[biophysical_vars, spectral_indices]

ggcorrplot(
  cor_matrix,
  p.mat = NULL,  # Ignore p-values (no crosses will appear)
  type = "full",
  lab = TRUE,
  lab_size = 3,
  tl.cex = 10,
  colors = c("#6D9EC1", "white", "#E46726")
) +
  labs(title = "Correlations: Biophysical Properties vs. Spectral Indices")
##########################2nd graph################################
ggcorrplot(
  cor_matrix,
  p.mat = p_matrix,
  type = "full",          # Show full rectangular matrix
  insig = "blank",        # Hide non-significant correlations
  lab = TRUE,             # Display correlation coefficients
  sig.level = c(0.001, 0.01, 0.05),  # Significance thresholds
  lab_size = 3,
  tl.cex = 10,            # Adjust text size
  colors = c("#6D9EC1", "white", "#E46726")  # Custom color gradient
) +
  labs(
    title = "",
    x = "Spectral Indices",
    y = "Biophysical Properties"
  )
###############################################################
###############################################################
######################For Native grass#########################
##############################################################

# Define column names (update with your actual column names)
biophysical_vars <- c("Cover", "Height", "Grassbiomass", "TotalBiomass", 
                      "LAI", "ForbsCover", "StandingDeadCover", "LitterCover","BareGroundCover"
                      ,"ShrubCover","ShrubBiomass", "ForbsBiomass","DeadBiomass")

spectral_indices <- c("NDNI", "CAI", "PRI", "TCARI","MCARI2","R700_R670","RGR","ARI_550_700","CRI_510_550"
                      ,"NDVI_682_553")

nativesubset_data <- Native_data[, c(biophysical_vars, spectral_indices)]
nativesubset_data <- nativesubset_data %>% 
  select(-StandingDeadCover, -LitterCover,-ShrubCover, -ShrubBiomass)
#View(nativesubset_data)
names(nativesubset_data)

# Compute correlation matrix and p-values
ncor_matrix <- cor(nativesubset_data, use = "complete.obs")
np_matrix <- cor_pmat(nativesubset_data)  # Compute p-values

biophysical_vars <- c("Cover", "Height", "Grassbiomass", "TotalBiomass", 
                      "LAI", "ForbsCover", "BareGroundCover", "ForbsBiomass","DeadBiomass")

spectral_indices <- c("NDNI", "CAI", "PRI", "TCARI","MCARI2","R700_R670","RGR","ARI_550_700","CRI_510_550"
                      ,"NDVI_682_553")

# Keep only cross-group correlations (biophysical vs. spectral)
ncor_matrix <- ncor_matrix[biophysical_vars, spectral_indices]
np_matrix <- np_matrix[biophysical_vars, spectral_indices]

ggcorrplot(
  ncor_matrix,
  p.mat = NULL,  # Ignore p-values (no crosses will appear)
  type = "full",
  lab = TRUE,
  lab_size = 3,
  tl.cex = 10,
  colors = c("#6D9EC1", "white", "#E46726")
) +
  labs(title = "Correlations: Biophysical Properties vs. Spectral Indices")
##########################2nd graph################################
ggcorrplot(
  ncor_matrix,
  p.mat = p_matrix,
  type = "full",          # Show full rectangular matrix
  insig = "blank",        # Hide non-significant correlations
  lab = TRUE,             # Display correlation coefficients
  sig.level = c(0.001, 0.01, 0.05),  # Significance thresholds
  lab_size = 3,
  tl.cex = 10,            # Adjust text size
  colors = c("#6D9EC1", "white", "#E46726")  # Custom color gradient
) +
  labs(
    title = "",
    x = "Spectral Indices",
    y = "Biophysical Properties"
  )
#################################################################
#################################################################




#Note: From here I am following Golizadeh student approach

##################################Using all the data#########################################
#########################Correlation/regression test of biophysical and spectral indices#####################
#setwd("D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data")
setwd("C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data")

#Plot wise biophysical data
Biophy <- read_csv("PlotWiseDescriptiveStat.csv")
#View(Biophy) ## NOTE: I added a species column manually in the excel sheet.
names(Biophy)

#Add a column with the name Plot so it mathes with the spectral indices list
Biophy$Plot <- Biophy$Site
#View(Biophy)

# List all your loaded data frames (replace with your actual object names)
data_list <- list(Biophy, combined_data)

# Merge all by "Plot" (keeps all rows and fills missing values with NA)
data <- reduce(data_list, ~ full_join(.x, .y, by = "Plot"))
#View(data)
names(data)

# Define column names (update with your actual column names)
biophysical_vars <- c("Cover", "Height", "Grassbiomass", "TotalBiomass", 
                      "LAI", "ForbsCover", "StandingDeadCover", "LitterCover","BareGroundCover"
                      ,"ShrubCover","ShrubBiomass", "ForbsBiomass","DeadBiomass")

spectral_indices <- c("NDNI", "CAI", "PRI", "TCARI","MCARI2","R700_R670","RGR","ARI_550_700","CRI_510_550"
                      ,"NDVI_682_553")


# Create all pairs of variables
combinations <- expand.grid(Biophysical = biophysical_vars, Spectral = spectral_indices)

# Create a long dataframe with all pairs
plot_data <- purrr::map_dfr(1:nrow(combinations), function(i) {
  bio <- combinations$Biophysical[i]
  spec <- combinations$Spectral[i]
  
  tibble(
    Biophysical = bio,
    Spectral = spec,
    x = data[[spec]],
    y = data[[bio]]
  )
})
#View(plot_data)



############## I LIKE THIS PLOT!!!!!!!!!!!!!!!!!!##############
#############################
# Subset the data to include only the two groups
library(ggcorrplot)
# Define column names (update with your actual column names)
biophysical_vars <- c("Cover", "Height", "Grassbiomass", "TotalBiomass", 
                      "LAI", "ForbsCover", "StandingDeadCover", "LitterCover","BareGroundCover"
                      ,"ShrubCover","ShrubBiomass", "ForbsBiomass","DeadBiomass")

spectral_indices <- c("NDNI", "CAI", "PRI", "TCARI","MCARI2","R700_R670","RGR","ARI_550_700","CRI_510_550"
                      ,"NDVI_682_553")

subset_data <- data[, c(biophysical_vars, spectral_indices)]
subset_data <- subset_data %>% 
  select(-StandingDeadCover, -LitterCover,-ShrubCover, -ShrubBiomass)
#View(subset_data)
names(subset_data)

# Compute correlation matrix and p-values
cor_matrix <- cor(subset_data, use = "complete.obs")
p_matrix <- cor_pmat(subset_data)  # Compute p-values

biophysical_vars <- c("Cover", "Height", "Grassbiomass", "TotalBiomass", 
                      "LAI", "ForbsCover", "BareGroundCover", "ForbsBiomass","DeadBiomass")

spectral_indices <- c("NDNI", "CAI", "PRI", "TCARI","MCARI2","R700_R670","RGR","ARI_550_700","CRI_510_550"
                      ,"NDVI_682_553")

# Keep only cross-group correlations (biophysical vs. spectral)
cor_matrix <- cor_matrix[biophysical_vars, spectral_indices]
p_matrix <- p_matrix[biophysical_vars, spectral_indices]

ggcorrplot(
  cor_matrix,
  p.mat = NULL,  # Ignore p-values (no crosses will appear)
  type = "full",
  lab = TRUE,
  lab_size = 3,
  tl.cex = 10,
  colors = c("#6D9EC1", "white", "#E46726")
) +
  labs(title = "Correlations: Biophysical Properties vs. Spectral Indices")
##########################2nd graph################################
ggcorrplot(
  cor_matrix,
  p.mat = p_matrix,
  type = "full",          # Show full rectangular matrix
  insig = "blank",        # Hide non-significant correlations
  lab = TRUE,             # Display correlation coefficients
  sig.level = c(0.001, 0.01, 0.05),  # Significance thresholds
  lab_size = 3,
  tl.cex = 10,            # Adjust text size
  colors = c("#6D9EC1", "white", "#E46726")  # Custom color gradient
) +
  labs(
    title = "",
    x = "Spectral Indices",
    y = "Biophysical Properties"
  )


# Important spectral indices: NDNI, TCARI, R700_670, CRI, NDVI, ARI, RGR, MCARI (Delete:CAI, PRI)
golidata <- data %>% 
  select(-StandingDeadCover, -LitterCover,-ShrubCover, -ShrubBiomass, -CAI, -PRI)
names(golidata)
#View(golidata)

library(ggstatsplot)

# Student’s t-test (assumes normality and equal variances)
t_test_NDNI<- t.test(NDNI ~ Species, data = golidata, var.equal = TRUE)
print(t_test_NDNI)

A1 <- ggbetweenstats(
  data = golidata,
  x = Species,
  y = NDNI,
  type = "parametric",
  var.equal = TRUE,
  
)+
  labs(y = "NDNI")
A1


# Student’s t-test (assumes normality and equal variances)
t_test_TCARI<- t.test(TCARI ~ Species, data = golidata, var.equal = TRUE)
print(t_test_TCARI)
A2 <- ggbetweenstats(
  data = golidata,
  x = Species,
  y = TCARI,
  type = "parametric",
  var.equal = TRUE,
  
)+
  labs(y = "TCARI")
A2


# Student’s t-test (assumes normality and equal variances)
t_test_MCARI2<- t.test(MCARI2 ~ Species, data = golidata, var.equal = TRUE)
print(t_test_MCARI2)

A3 <- ggbetweenstats(
  data = golidata,
  x = Species,
  y = MCARI2,
  type = "parametric",
  var.equal = TRUE,
  
)+
  labs(y = "MCARI2")
A3

# Student’s t-test (assumes normality and equal variances)
t_test_R700_R670<- t.test(R700_R670 ~ Species, data = golidata, var.equal = TRUE)
print(t_test_R700_R670)

A4 <- ggbetweenstats(
  data = golidata,
  x = Species,
  y = R700_R670,
  type = "parametric",
  var.equal = TRUE,
  
)+
  labs(y = "R700_R670")
A4

# Student’s t-test (assumes normality and equal variances)
t_test_RGR<- t.test(RGR ~ Species, data = golidata, var.equal = TRUE)
print(t_test_RGR)

A5 <- ggbetweenstats(
  data = golidata,
  x = Species,
  y = RGR,
  type = "parametric",
  var.equal = TRUE,
  
)+
  labs(y = "RGR")
A5

# Student’s t-test (assumes normality and equal variances)
t_test_ARI_550_700<- t.test(ARI_550_700 ~ Species, data = golidata, var.equal = TRUE)
print(t_test_ARI_550_700)

A6 <- ggbetweenstats(
  data = golidata,
  x = Species,
  y = ARI_550_700,
  type = "parametric",
  var.equal = TRUE,
  
)+
  labs(y = "ARI_550_700")
A6

# Student’s t-test (assumes normality and equal variances)
t_test_CRI_510_550<- t.test(CRI_510_550 ~ Species, data = golidata, var.equal = TRUE)
print(t_test_CRI_510_550)

A7 <- ggbetweenstats(
  data = golidata,
  x = Species,
  y = CRI_510_550,
  type = "parametric",
  var.equal = TRUE,
  
)+
  labs(y = "CRI_510_550")
A7

# Student’s t-test (assumes normality and equal variances)
t_test_NDVI_682_553<- t.test(NDVI_682_553 ~ Species, data = golidata, var.equal = TRUE)
print(t_test_NDVI_682_553)

A8 <- ggbetweenstats(
  data = golidata,
  x = Species,
  y = NDVI_682_553,
  type = "parametric",
  var.equal = TRUE,
  
)+
  labs(y = "NDVI_682_553")
A8


# Student’s t-test (assumes normality and equal variances)
t_test_PRI<- t.test(PRI ~ Species, data = data, var.equal = TRUE)
print(t_test_PRI)

A8 <- ggbetweenstats(
  data = golidata,
  x = Species,
  y = PRI,
  type = "parametric",
  var.equal = TRUE,
  
)+
  labs(y = "PRI")
A8


###############Golizadeh done###############
############################################
# Important spectral indices: NDNI, TCARI, R700_670, CRI, NDVI, ARI, RGR, MCARI (Delete:CAI, PRI)
sdata <- data %>% 
  select(-StandingDeadCover, -LitterCover,-ShrubCover, -ShrubBiomass,-MoistureContent, -WetBiomass,-MossCover)
names(sdata)
#View(sdata)



##Scatter plot
# Load required packages
library(ggplot2)
library(purrr)
library(gridExtra)
install.packages("GGally")
library(GGally)

plot(sdata)
names(sdata)
# Keep only 'Height' and 'Cover' columns
plot(sdata[, c("Cover", "Height", "Grassbiomass", "TotalBiomass", "LAI","ForbsCover","DeadBiomass", "ForbsBiomass","BareGroundCover" ,
               "NDNI" , "CAI", "PRI" , "TCARI", "MCARI2", "R700_R670", "RGR", "ARI_550_700",
               "CRI_510_550", "NDVI_682_553")])

# Panel for lower triangle: scatterplot + regression line
panel.lm <- function(x, y, ...) {
  points(x, y, pch = 19, col = "black", cex = 0.5)
  abline(lm(y ~ x), col = "red", lwd = 1.5)
}

# Panel for upper triangle: show R-squared
panel.r2 <- function(x, y, ...) {
  r2 <- summary(lm(y ~ x))$r.squared
  txt <- bquote(R^2 == .(format(r2, digits = 2)))
  text(mean(x, na.rm = TRUE), mean(y, na.rm = TRUE), labels = txt, cex = 1.2)
}

# Run the pairs plot
pairs(
  sdata[, c("Cover", "Height", "Grassbiomass", "TotalBiomass", "LAI","ForbsCover","DeadBiomass", "ForbsBiomass","BareGroundCover" ,
            "NDNI", "CAI", "PRI", "TCARI", "MCARI2", "R700_R670", "RGR", "ARI_550_700",
            "CRI_510_550", "NDVI_682_553")],
  lower.panel = panel.lm,
  upper.panel = panel.r2,
  pch = 19,
  cex = 0.5
)






# Select your variables
selected_vars <- sdata[, unique(c("Cover", "Height", "Grassbiomass", "TotalBiomass", "LAI",
                                  "ForbsCover", "DeadBiomass", "ForbsBiomass", "BareGroundCover" ,"NDNI",
                                  "CAI", "PRI", "TCARI", "MCARI2", "R700_R670", "RGR",
                                  "ARI_550_700", "CRI_510_550", "NDVI_682_553"))]
selected_vars <- unique(selected_vars)
# Plot lower triangle only, with regression line and R²
ggpairs(
  selected_vars,
  lower = list(continuous = wrap("smooth", method = "lm", se = FALSE)),
  diag = list(continuous = wrap("barDiag", colour = "blue")),
  upper = list(continuous = "blank")
)

ggpairs(
  selected_vars,
  lower = list(continuous = wrap("smooth", method = "lm", se = FALSE)),
  diag = list(continuous = wrap("barDiag", colour = "blue")),
  upper = list(continuous = "blank")
)