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
#names(Hy)

# Compute mean reflectance per site (averaging across quadrats)
mean_reflectance_per_site <- Hy %>%
  group_by(Site) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

#View(mean_reflectance_per_site)

#Note: I have to delete some of the bands because they contain noise. Checked Irini and Yihan's paper. They talked about it.
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

#####################################################################################################################################################################
###############################################################################Spectral profile graph final##########################################################
#####################################################################################################################################################################
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

#################################################################################################################################################
#############################################################Hyperspectral data t test###########################################################
#################################################################################################################################################
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

######################################################################################################################
#######################################################*****Assumption tests*#########################################
######################################################################################################################
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

############################Note: Hypersepctral data appears to be abnormal but I can make it normal based on the logic##################################
###########################from Irini's work. And, the variances might be unequal. So, I will end up with the Weltch T test like Irini###################

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

########################################################################################################################################
##################################################################*Significant tests####################################################
#########################################################################################################################################
#########################################################################################################################################
############Weltch t test (assumes normality) Note: I will be using this one as my data are normal but some parts have unequal variances
##########################################################################################################################################
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

#######################################################################################################
#####################################I will use this graph for the hyperspectral#######################
##################################### #################################################################
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
