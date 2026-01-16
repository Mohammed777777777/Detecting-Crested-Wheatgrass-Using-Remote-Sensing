library(tidyverse)    
library(readxl)      
library(pracma)  

#setwd("D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets")
#setwd("C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets")
setwd("C:/Users/Mohammed/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets")

# Load dataset
Hy <- read.csv("Hyperspectral Reflectance.csv", check.names = FALSE)

# Compute mean reflectance per site (averaging across quadrats)
mean_reflectance_per_site <- Hy %>%
  group_by(Site) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

#Note: I have to delete some of the bands because they contain noise. Check Irini and Yihan's paper. They talked about it.
# Define the wavelength ranges to be set as NA
wavelength_na_ranges <- c(1350:1430, 1750:1980, 2330:2500) # Took this range from Irini and Yihan's article.
# Replace values with NA for the specified wavelength ranges
mean_reflectance_per_site[, as.character(wavelength_na_ranges)] <- NA

# Add a new column 'Species' based on the first letter of 'Site'
mean_reflectance_per_site <- mean_reflectance_per_site %>%
  mutate(Species = ifelse(grepl("^C", Site), "CW", "NG"))

# Reshape data from wide to long format
fieldspec_long <- mean_reflectance_per_site %>%
  pivot_longer(
    cols = -c(Site, Species),
    names_to = "Wavelength",
    values_to = "Reflectance"
  ) %>%
  mutate(Wavelength = as.numeric(Wavelength))  # Ensure wavelengths are numeric

#********* Lets upload the SRF file**************#
#setwd("D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/Sentinel")
#setwd("C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/Sentinel")
setwd("C:/Users/Mohammed/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/Sentinel")

##****Sentinel SRF extraction******##
# Load dataset
SRF <- read.csv("S2A.csv", check.names = FALSE)
names(SRF)
#View(SRF)
# Example for Band 1 
srf_b1 <- SRF %>% 
  select(SR_WL, S2A_SR_AV_B1) %>% 
  rename(wavelength = SR_WL, weight = S2A_SR_AV_B1) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_b1

fieldspec_interp_b1 <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_b1$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_b1$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_b1, by = "wavelength")  # Add SRF weights

#View(fieldspec_interp_b1)

# Simulated Band 1
sentinel_refl_b1 <- fieldspec_interp_b1 %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B1 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )
sentinel_refl_b1<- sentinel_refl_b1 %>% 
  select(-numerator, -denominator)

# Example for Band 2 
srf_b2 <- SRF %>% 
  select(SR_WL, S2A_SR_AV_B2) %>% 
  rename(wavelength = SR_WL, weight = S2A_SR_AV_B2) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_b2

fieldspec_interp_b2 <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_b2$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_b2$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_b2, by = "wavelength")  # Add SRF weights

#View(fieldspec_interp_b2)

# Simulated Band 2
sentinel_refl_b2 <- fieldspec_interp_b2 %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B2 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )
sentinel_refl_b2<- sentinel_refl_b2 %>% 
  select(-numerator, -denominator)

# Example for Band 3
srf_b3 <- SRF %>% 
  select(SR_WL, S2A_SR_AV_B3) %>% 
  rename(wavelength = SR_WL, weight = S2A_SR_AV_B3) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_b3

fieldspec_interp_b3 <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_b3$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_b3$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_b3, by = "wavelength")  # Add SRF weights

# Simulated Band 3
sentinel_refl_b3 <- fieldspec_interp_b3 %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B3 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )
sentinel_refl_b3<- sentinel_refl_b3 %>% 
  select(-numerator, -denominator)

# Example for Band 4
srf_b4 <- SRF %>% 
  select(SR_WL, S2A_SR_AV_B4) %>% 
  rename(wavelength = SR_WL, weight = S2A_SR_AV_B4) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_b4

fieldspec_interp_b4 <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_b4$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_b4$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_b4, by = "wavelength")  # Add SRF weights

# Simulated Band 4
sentinel_refl_b4 <- fieldspec_interp_b4 %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B4 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )
sentinel_refl_b4<- sentinel_refl_b4 %>% 
  select(-numerator, -denominator)

# Example for Band 5
srf_b5 <- SRF %>% 
  select(SR_WL, S2A_SR_AV_B5) %>% 
  rename(wavelength = SR_WL, weight = S2A_SR_AV_B5) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_b5

fieldspec_interp_b5 <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_b5$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_b5$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_b5, by = "wavelength")  # Add SRF weights

# Simulated Band 5
sentinel_refl_b5 <- fieldspec_interp_b5 %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B5 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )
sentinel_refl_b5<- sentinel_refl_b5 %>% 
  select(-numerator, -denominator)

# Example for Band 6
srf_b6 <- SRF %>% 
  select(SR_WL, S2A_SR_AV_B6) %>% 
  rename(wavelength = SR_WL, weight = S2A_SR_AV_B6) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_b6

fieldspec_interp_b6 <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_b6$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_b6$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_b6, by = "wavelength")  # Add SRF weights

# Simulated Band 6
sentinel_refl_b6 <- fieldspec_interp_b6 %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B6 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )
sentinel_refl_b6<- sentinel_refl_b6 %>% 
  select(-numerator, -denominator)

# Example for Band 7
srf_b7 <- SRF %>% 
  select(SR_WL, S2A_SR_AV_B7) %>% 
  rename(wavelength = SR_WL, weight = S2A_SR_AV_B7) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_b7

fieldspec_interp_b7 <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_b7$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_b7$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_b7, by = "wavelength")  # Add SRF weights

# Simulated Band 7
sentinel_refl_b7 <- fieldspec_interp_b7 %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B7 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )
sentinel_refl_b7<- sentinel_refl_b7 %>% 
  select(-numerator, -denominator)

# Example for Band 8
srf_b8 <- SRF %>% 
  select(SR_WL, S2A_SR_AV_B8) %>% 
  rename(wavelength = SR_WL, weight = S2A_SR_AV_B8) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_b8

fieldspec_interp_b8 <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_b8$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_b8$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_b8, by = "wavelength")  # Add SRF weights

# Simulated Band 8
sentinel_refl_b8 <- fieldspec_interp_b8 %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B8 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )
sentinel_refl_b8<- sentinel_refl_b8 %>% 
  select(-numerator, -denominator)

# Example for Band 8A
srf_b8A <- SRF %>% 
  select(SR_WL, S2A_SR_AV_B8A) %>% 
  rename(wavelength = SR_WL, weight = S2A_SR_AV_B8A) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_b8A

fieldspec_interp_b8A <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_b8A$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_b8A$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_b8A, by = "wavelength")  # Add SRF weights

# Simulated Band 8A
sentinel_refl_b8A <- fieldspec_interp_b8A %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B8A = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )
sentinel_refl_b8A<- sentinel_refl_b8A %>% 
  select(-numerator, -denominator)

# Example for Band 9
srf_b9 <- SRF %>% 
  select(SR_WL, S2A_SR_AV_B9) %>% 
  rename(wavelength = SR_WL, weight = S2A_SR_AV_B9) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_b9

fieldspec_interp_b9 <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_b9$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_b9$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_b9, by = "wavelength")  # Add SRF weights

# Simulated Band 9
sentinel_refl_b9 <- fieldspec_interp_b9 %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B9 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )
sentinel_refl_b9<- sentinel_refl_b9 %>% 
  select(-numerator, -denominator)

# Example for Band 10
srf_b10 <- SRF %>% 
  select(SR_WL, S2A_SR_AV_B10) %>% 
  rename(wavelength = SR_WL, weight = S2A_SR_AV_B10) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_b10

fieldspec_interp_b10 <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_b10$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_b10$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_b10, by = "wavelength")  # Add SRF weights

# Simulated Band 10
sentinel_refl_b10 <- fieldspec_interp_b10 %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B10 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )
sentinel_refl_b10<- sentinel_refl_b10 %>% 
  select(-numerator, -denominator)

# Example for Band 11
srf_b11 <- SRF %>% 
  select(SR_WL, S2A_SR_AV_B11) %>% 
  rename(wavelength = SR_WL, weight = S2A_SR_AV_B11) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_b11

fieldspec_interp_b11 <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_b11$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_b11$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_b11, by = "wavelength")  # Add SRF weights

# Simulated Band 11
sentinel_refl_b11 <- fieldspec_interp_b11 %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B11 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )
sentinel_refl_b11<- sentinel_refl_b11 %>% 
  select(-numerator, -denominator)

# Example for Band 12
srf_b12 <- SRF %>% 
  select(SR_WL, S2A_SR_AV_B12) %>% 
  rename(wavelength = SR_WL, weight = S2A_SR_AV_B12) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_b12

fieldspec_interp_b12 <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_b12$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_b12$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_b12, by = "wavelength")  # Add SRF weights

# Simulated Band 12
sentinel_refl_b12 <- fieldspec_interp_b12 %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B12 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )

sentinel_refl_b12 <- sentinel_refl_b12 %>% 
  select(-numerator, -denominator)

final_results <- sentinel_refl_b1 %>%
  full_join(sentinel_refl_b2, by = "Site") %>%
  full_join(sentinel_refl_b3, by = "Site") %>%
  full_join(sentinel_refl_b4, by = "Site") %>%
  full_join(sentinel_refl_b5, by = "Site") %>%
  full_join(sentinel_refl_b6, by = "Site") %>%
  full_join(sentinel_refl_b7, by = "Site") %>%
  full_join(sentinel_refl_b8, by = "Site") %>%
  full_join(sentinel_refl_b8A, by = "Site") %>%
  #full_join(sentinel_refl_b9, by = "Site") %>%
  #full_join(sentinel_refl_b10, by = "Site") %>%
  full_join(sentinel_refl_b11, by = "Site") %>%
  full_join(sentinel_refl_b12, by = "Site")
#View(final_results)
names(final_results)

# Add a new column 'Species' based on the first letter of 'Site'
SimulatedSentinel2A <- final_results %>%
  mutate(Species = ifelse(grepl("^C", Site), "CW", "NG"))
#View(SimulatedSentinel2A)
names(SimulatedSentinel2A)

#***************visualization*********************#
# First, create the tidy/long version of the data
tidy_data <- SimulatedSentinel2A %>%
  pivot_longer(cols = B1:B12, names_to = "Band", values_to = "Reflectance") %>%
  mutate(
    Band = case_when(
      is.na(Band) ~ "B8A",  
      TRUE ~ Band           
    ),
    Band = factor(Band, levels = c(paste0("B", 1:8), "B8A", paste0("B", 11:12)))
  )

#View(tidy_data)

# Calculate summary statistics by Band and Species
summary_data <- tidy_data %>%
  group_by(Band, Species) %>%
  summarise(
    mean_reflectance = mean(Reflectance),
    sd_reflectance = sd(Reflectance),
    .groups = 'drop'
  )

 Plot_S2<- ggplot(summary_data, aes(x = Band, y = mean_reflectance, 
                         group = Species, color = Species)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_reflectance - sd_reflectance, 
                    ymax = mean_reflectance + sd_reflectance),
                width = 0.2) +
  labs(title = "Simulated Sentinel-2A Spectra",
       x = "Band",
       y = "Reflectance",
       color = "Species") +
  scale_color_manual(values = c("CW" = "blue", "NG" = "red")) +
  scale_y_continuous(breaks = seq(0, max(summary_data$mean_reflectance) + 0.1, by = 0.05)) +  # Adjust 'by' as needed
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))+
scale_x_discrete(
  labels = c("B1" = "Coastal Aerosol", 
             "B2" = "Blue",
             "B3" = "Green",
             "B4" = "Red",
             "B5" = "Red edge i",
             "B6" = "Red edge ii",
             "B7" = "Red edge iii",
             "B8" = "NIR",
             "B8A" = "NIR2",
             "B9" = "Water Vapour",
             "B10" = "Cirrus Cloud",
             "B11" = "SWIR1",
             "B12" = "SWIR2"
             )  # Adjust to match your bands
)
 Plot_S2
#****************Extraction done***********************#
#**************Assmption Testing**************#
library(plotly)
library(ggpubr)  
library(dplyr)

#Normality Test
D<-ggqqplot(
  data = tidy_data, 
  x = "Reflectance", 
  facet.by = "Species",
  title = "Q-Q Plot: Grass biomass (gm/m2)"
)
D

p <- ggqqplot(
  data = tidy_data,
  x = "Reflectance",
  facet.by = c("Species", "Band"),
  title = "Interactive Q-Q Plot by Band_region",
  color = "Band"
)

ggqqplot(
  data = tidy_data,
  x = "Reflectance",
  facet.by = c("Species", "Band"),  # Facet by both Species AND Band
  title = "Q-Q Plots: Reflectance Normality by Species and Spectral Band",
  # Color points by band
  palette = c("red", "darkgreen", "gray")  # Custom colors for bands
) +
  theme(legend.position = "top") 

# Convert to interactive plot
ggplotly(p, tooltip = c("x", "y", "Rand")) 

# Split data by species
split_data1 <- split(tidy_data$Reflectance,tidy_data$Species)
#View(split_data1)

# Run Shapiro-Wilk test for each species
lapply(split_data1, shapiro.test)
#Note: Looks like this is one of the limitation of shapiro test of normality.

#Homogeniety test of variance
library(car)
leveneTest(Reflectance ~ Species, data = tidy_data)

variance_results <- tidy_data %>%
  group_by(Band) %>%
  summarise(
    levene_p = leveneTest(Reflectance ~ Species)$`Pr(>F)`[1],
    .groups = 'drop'
  )
variance_results
#Note: sepctral data appears to be abnormal but I can make it normal based on the logic
# from Irini's work. And, the variances might be unequal. So, I will end up with the Weltch T test like Irini.

#*****************Sentinel done********************#

##*****PlanetScope_SRF_extraction******##
#setwd("D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/PlanetScope")
#setwd("C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/PlanetScope")
#setwd("C:/Users/Mohammed/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/PlanetScope")

PS <- read.csv("Superdove.csv", check.names = FALSE)
#View(PS)
names(PS)

# Band 1 (Coastal Blue)
srf_cb <- PS %>% 
  select(Wavelength,  CoastalBlue) %>% 
  rename(wavelength = Wavelength, weight = CoastalBlue) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_cb

fieldspec_interp_b1_cb <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_cb$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_cb$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_cb, by = "wavelength")  # Add SRF weights

#View(fieldspec_interp_b1_cb)

# Simulated Band 1 (Coastal Blue)
Planet_refl_b1 <- fieldspec_interp_b1_cb %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B1 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )
Planet_refl_b1<- Planet_refl_b1 %>% 
  select(-numerator, -denominator)


# Band 2 (Blue)
srf_b <- PS %>% 
  select(Wavelength,  Blue) %>% 
  rename(wavelength = Wavelength, weight = Blue) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_b

fieldspec_interp_b2_b <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_b$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_b$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_b, by = "wavelength")  # Add SRF weights

#View(fieldspec_interp_b2_b)

# Simulated Band 2 (Blue)
Planet_refl_b2 <- fieldspec_interp_b2_b %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B2 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )
Planet_refl_b2<- Planet_refl_b2 %>% 
  select(-numerator, -denominator)


#Band 3 (Green)
srf_gi <- PS %>% 
  select(Wavelength,  Green_i) %>% 
  rename(wavelength = Wavelength, weight = Green_i) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_gi

fieldspec_interp_b3_gi <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_gi$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_gi$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_gi, by = "wavelength")  # Add SRF weights

#View(fieldspec_interp_b3_gi)

# Simulated Band 3 (Green)
Planet_refl_b3 <- fieldspec_interp_b3_gi %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B3 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )
Planet_refl_b3<- Planet_refl_b3 %>% 
  select(-numerator, -denominator)


#Band 4 (Green_ii)
srf_gii <- PS %>% 
  select(Wavelength,  Green_ii) %>% 
  rename(wavelength = Wavelength, weight = Green_ii) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_gii

fieldspec_interp_b4_gii <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_gii$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_gii$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_gii, by = "wavelength")  # Add SRF weights

#View(fieldspec_interp_b4_gii)

# Simulated Band 4 (Green_ii)
Planet_refl_b4 <- fieldspec_interp_b4_gii %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B4 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )
Planet_refl_b4<- Planet_refl_b4 %>% 
  select(-numerator, -denominator)


#Band 5 (Yellow)
srf_Y <- PS %>% 
  select(Wavelength,  Yellow) %>% 
  rename(wavelength = Wavelength, weight = Yellow) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_Y

fieldspec_interp_b5_Y <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_Y$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_Y$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_Y, by = "wavelength")  # Add SRF weights

#View(fieldspec_interp_b5_Y)

# Simulated Band 5 (Yellow)
Planet_refl_b5 <- fieldspec_interp_b5_Y %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B5 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )
Planet_refl_b5<- Planet_refl_b5 %>% 
  select(-numerator, -denominator)

#Band 6 (Red)
srf_R <- PS %>% 
  select(Wavelength,  Red) %>% 
  rename(wavelength = Wavelength, weight = Red) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_R

fieldspec_interp_b6_R <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_R$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_R$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_R, by = "wavelength")  # Add SRF weights

#View(fieldspec_interp_b6_R)

# Simulated Band 6 (Red)
Planet_refl_b6 <- fieldspec_interp_b6_R %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B6 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )
Planet_refl_b6<- Planet_refl_b6 %>% 
  select(-numerator, -denominator)


#Band 7 (Red Edge)
srf_RE <- PS %>% 
  select(Wavelength,  RedEdge) %>% 
  rename(wavelength = Wavelength, weight = RedEdge) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_RE

fieldspec_interp_b7_RE <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_RE$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_RE$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_RE, by = "wavelength")  # Add SRF weights
#View(fieldspec_interp_b7_RE)

# Simulated Band 7 (Red)
Planet_refl_b7 <- fieldspec_interp_b7_RE %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B7 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )
Planet_refl_b7<- Planet_refl_b7 %>% 
  select(-numerator, -denominator)

#Band 8 (NIR)
srf_NIR <- PS %>% 
  select(Wavelength,  NIR) %>% 
  rename(wavelength = Wavelength, weight = NIR) %>% 
  filter(weight > 0)  # Remove zero-weight wavelengths
srf_NIR

fieldspec_interp_b8_NIR <- fieldspec_long %>% 
  group_by(Site) %>% 
  reframe(
    wavelength = srf_NIR$wavelength,  # Use SRF wavelengths
    reflectance = approx(
      x = Wavelength, 
      y = Reflectance, 
      xout = srf_NIR$wavelength, 
      #rule = 2  # Extrapolate if needed
    )$y
  ) %>% 
  left_join(srf_NIR, by = "wavelength")  # Add SRF weights
#View(fieldspec_interp_b8_NIR)

# Simulated Band 8 (NIR)
Planet_refl_b8 <- fieldspec_interp_b8_NIR %>% 
  group_by(Site) %>% 
  summarise(
    numerator = trapz(wavelength, reflectance * weight),
    denominator = trapz(wavelength, weight),
    B8 = numerator / denominator,
    #sentinel_scaled = sentinel_refl * 10000  # Scale for L2A products
  )

Planet_refl_b8<- Planet_refl_b8 %>% 
  select(-numerator, -denominator)

final_resultsPS <- Planet_refl_b1 %>%
  full_join(Planet_refl_b2, by = "Site") %>%
  full_join(Planet_refl_b3, by = "Site") %>%
  full_join(Planet_refl_b4, by = "Site") %>%
  full_join(Planet_refl_b5, by = "Site") %>%
  full_join(Planet_refl_b6, by = "Site") %>%
  full_join(Planet_refl_b7, by = "Site") %>%
  full_join(Planet_refl_b8, by = "Site")

#View(final_resultsPS)
names(final_resultsPS)

# Add a new column 'Species' based on the first letter of 'Site'
SimulatedPS <- final_resultsPS  %>%
  mutate(Species = ifelse(grepl("^C", Site), "CW", "NG"))
#View(SimulatedPS)
names(SimulatedPS)


#***************visualization*********************#
# First, create the tidy/long version of the data
SimulatedPS_data <- SimulatedPS %>%
  pivot_longer(cols = B1:B8, names_to = "Band", values_to = "Reflectance") %>%
  mutate(
    Band = case_when(
      is.na(Band) ~ "B8",  # Replace NA with B8 (if needed)
      TRUE ~ Band           # Keep other bands as-is
    ),
    Band = factor(Band, levels = paste0("B", 1:8))  # Only include B1-B8
  )
SimulatedPS_data
#View(SimulatedPS_data)

# Calculate summary statistics by Band and Species
summary_dataPS <- SimulatedPS_data %>%
  group_by(Band, Species) %>%
  summarise(
    mean_reflectance = mean(Reflectance),
    sd_reflectance = sd(Reflectance),
    .groups = 'drop'
  )

plot_PS <- ggplot(summary_dataPS, aes(x = Band, y = mean_reflectance, 
                         group = Species, color = Species)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_reflectance - sd_reflectance, 
                    ymax = mean_reflectance + sd_reflectance),
                width = 0.2) +
  labs(title = "Simulated PlanetScope Spectra",
       x = "Band",
       y = "Reflectance",
       color = "Species") +
  scale_color_manual(values = c("CW" = "blue", "NG" = "red")) +
  scale_y_continuous(breaks = seq(0, max(summary_data$mean_reflectance) + 0.1, by = 0.05)) +  # Adjust 'by' as needed
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 1))+
scale_x_discrete(
  labels = c("B1" = "Coastal Blue", 
             "B2" = "Blue",
             "B3" = "Green i",
             "B4" = "Green ii",
             "B5" = "Yellow",
             "B6" = "Red",
             "B7" = "Red Edge",
             "B8" = "NIR")  # Adjust to match your bands
)
plot_PS

#****************Extraction done***********************#
#**************Assmption Testing**************#
library(plotly)
library(ggpubr)  
library(dplyr)

#Normality Test
D<-ggqqplot(
  data = SimulatedPS_data, 
  x = "Reflectance", 
  facet.by = "Species",
  title = "Q-Q Plot: Grass biomass (gm/m2)"
)
D

p <- ggqqplot(
  data = SimulatedPS_data,
  x = "Reflectance",
  facet.by = c("Species", "Band"),
  title = "Interactive Q-Q Plot by Band_region",
  color = "Band"
)

ggqqplot(
  data = SimulatedPS_data,
  x = "Reflectance",
  facet.by = c("Species", "Band"),  # Facet by both Species AND Band
  title = "Q-Q Plots: Reflectance Normality by Species and Spectral Band",
  # Color points by band
  palette = c("red", "darkgreen", "gray")  # Custom colors for bands
) +
  theme(legend.position = "top") 

# Convert to interactive plot
ggplotly(p, tooltip = c("x", "y", "Rand")) 

# Split data by species
split_data2 <- split(SimulatedPS_data$Reflectance,SimulatedPS_data$Species)
#View(split_data2)

# Run Shapiro-Wilk test for each species
lapply(split_data2, shapiro.test)
#Note: Looks like this is one of the limitation of shapiro test of normality.

#Homogeniety test of variance
library(car)
leveneTest(Reflectance ~ Species, data = SimulatedPS_data)

variance_results <- SimulatedPS_data %>%
  group_by(Band) %>%
  summarise(
    levene_p = leveneTest(Reflectance ~ Species)$`Pr(>F)`[1],
    .groups = 'drop'
  )
variance_results
#Note: Hypersepctral data appears to be abnormal but I can make it normal based on the logic
# from Irini's work. And, the variances might be unequal. So, I will end up with the Weltch T test like Irini.

#*****************PlanetScope done********************#

#************Combine the data****************#
library(patchwork)
library(ggplot2)
library(dplyr)

# 1. Combine all data with a 'Sensor' identifier
combined_data <- bind_rows(
  summary_data %>% mutate(Sensor = "Sentinel-2"),
  summary_dataPS %>% mutate(Sensor = "PlanetScope")
) %>% 
  mutate(
    # Create unified x-axis labels (Band + Sensor)
    Band_Sensor = paste0(Band, " (", Sensor, ")"),
    Band_Sensor = factor(Band_Sensor, levels = unique(Band_Sensor))
  )

# Combine all data first
all_data <- bind_rows(
  summary_data %>% mutate(Sensor = "Sentinel-2"),
  summary_dataPS %>% mutate(Sensor = "PlanetScope")
)

# Example: Different labels for PlanetScope
band_labels_S2 <- c("B1" = "Coastal Aerosol", 
                    "B2" = "Blue",
                    "B3" = "Green",
                    "B4" = "Red",
                    "B5" = "Red edge i",
                    "B6" = "Red edge ii",
                    "B7" = "Red edge iii",
                    "B8" = "NIR",
                    "B8A" = "NIR2",
                    "B11" = "SWIR1",
                    "B12" = "SWIR2")  

# Example: Different labels for PlanetScope
band_labels_PS <-  c("B1" = "Coastal Blue", 
                     "B2" = "Blue",
                     "B3" = "Green i",
                     "B4" = "Green ii",
                     "B5" = "Yellow",
                     "B6" = "Red",
                     "B7" = "Red Edge",
                     "B8" = "NIR")

all_data <- all_data %>%
  mutate(
    Sensor = factor(Sensor, levels = c("PlanetScope","Sentinel-2")),
    Band_Name = case_when(
      Sensor == "Sentinel-2" ~ band_labels_S2[Band],
      Sensor == "PlanetScope" ~ band_labels_PS[Band],
      TRUE ~ Band  # Fallback to original band code if no match
    )
    # Ensure Band_Name is an ordered factor
    #Band_Name = factor(Band_Name, levels = unique(Band_Name))
  )

highlight_bands <- data.frame(
  Sensor = c("PlanetScope", "PlanetScope", "Sentinel-2", "Sentinel-2"),
  xmin = c(1.5, 5.5, 3.5, 7.5),
  xmax = c(3.5, 7.5, 5.5, 9.5),
  ymin = -Inf,
  ymax = Inf
)

#2
ggplot(all_data, aes(x = Band_Name, y = mean_reflectance, 
                     color = Species, group = interaction(Species, Sensor))) +
  geom_line() +
  geom_point(aes(shape = Species), size = 2) +
  geom_errorbar(aes(ymin = mean_reflectance - sd_reflectance,
                    ymax = mean_reflectance + sd_reflectance),
                width = 0.2) +
  facet_wrap(~Sensor, ncol = 1, scales = "free_x") +
  labs(
    y = "Reflectance",
    x = "Band",
    color = "Species",  # Legend title for color
    shape = "Species"   # Legend title for shape
  ) +
  # Customize legend text (replace "Cw" with "Crested Wheatgrass")
  scale_color_discrete(
    labels = c("Cw" = "Crested Wheatgrass",  # Map original names to new labels
               "Sb" = "Smooth Brome",       # Example for another species
               "Fw" = "Western Wheatgrass")  # Add all species here
  ) +
  scale_shape_discrete(
    labels = c("Cw" = "Crested Wheatgrass",  # Must match color labels
               "Sb" = "Smooth Brome",
               "Fw" = "Western Wheatgrass")
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "gray90", color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.25, size = 11),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  )

highlight_bands <- data.frame(
  Sensor = c("PlanetScope", "PlanetScope","PlanetScope","PlanetScope","PlanetScope","PlanetScope", "Sentinel-2","Sentinel-2", "Sentinel-2","Sentinel-2", "Sentinel-2","Sentinel-2"),
  xmin = c(0.8, 1.8, 2.8, 3.8, 5.8,7.8, 0.8, 1.8,2.8,5.8, 9.8,10.8),
  xmax = c(1.2, 2.2, 3.2, 4.2, 6.2,8.2, 1.2, 2.2,3.2,6.2, 10.2,11.2),
  ymin = -Inf,
  ymax = Inf
)

#2
ggplot(all_data, aes(x = Band_Name, y = mean_reflectance, 
                     color = Species, group = interaction(Species, Sensor))) +
  geom_line() +
  geom_point(aes(shape = Species), size = 2) +
  geom_errorbar(aes(ymin = mean_reflectance - sd_reflectance,
                    ymax = mean_reflectance + sd_reflectance),
                width = 0.2) +
  
  geom_rect(data = highlight_bands,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "blue", alpha = 0.2, inherit.aes = FALSE)+

  facet_wrap(~Sensor, ncol = 1, scales = "free_x",
             labeller = as_labeller(c(
               "PlanetScope" = "PlanetScope SuperDove",
               "Sentinel-2" = "Sentinel-2A"
             ))) +
  labs(
    y = "Reflectance",
    x = "Band",
    color = "Species",
    shape = "Species"
  ) +
  scale_color_manual(
    values = c("CW" = "#1b9e77", "NG" = "#d95f02"),
    labels = c("CW" = "Crested Wheatgrass", "NG" = "Native grass")
  ) +
  scale_shape_manual(
    values = c("CW" = 16, "NG" = 17),
    labels = c("CW" = "Crested Wheatgrass", "NG" = "Native grass")
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "gray90", color = "black"),
    axis.text.x = element_text(angle = 35, hjust = 0.95, size = 15),
    axis.text.y = element_text(angle = 0, hjust = 0.35, size = 15),
    axis.title.y = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    legend.text = element_text(size = 20),
    strip.text = element_text(size = 20) 
  )

#2
ggplot(all_data, aes(x = Band, y = mean_reflectance, 
                     group = Species, color = Species)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_reflectance - sd_reflectance,
                    ymax = mean_reflectance + sd_reflectance),
                width = 0.2) +
  facet_wrap(~Sensor, ncol = 1, scales = "free_x")

#################################################################
#***********************Done the Visualization******************#
#*###############################################################
#install.packages("ggsignif")  # Run once
library(ggsignif)

#View(SimulatedPS_data)
#View(SimulatedSentinel2A_data)
#View(SimulatedL9_data)
#View(SimulatedL8_data)

#*****SimulatedPS_data Weltch t test**********
weltch_t_test_results_SimulatedPS <- SimulatedPS_data %>%
  group_by(Band) %>%
  summarize(p_value_WEL = t.test(Reflectance ~ Species, var.equal = FALSE)$p.value)
#View(weltch_t_test_results_SimulatedPS)

#write.table(weltch_t_test_results_SimulatedPS, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/PlanetScope/Simulated Planet WeltchTtest.csv", sep = ",", row.names = FALSE)


#*****SimulatedSentinel2A Weltch t test**********
weltch_t_test_results_SimulatedSentinel2A <- tidy_data %>%
  group_by(Band) %>%
  summarize(p_value_WEL = t.test(Reflectance ~ Species, var.equal = FALSE)$p.value)
#View(weltch_t_test_results_SimulatedSentinel2A)

#write.table(weltch_t_test_results_SimulatedSentinel2A, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/Sentinel/Simulated Sentinel WeltchTtest.csv", sep = ",", row.names = FALSE)

#######################################################################
#*******************Spectral index calulation*************************#
#######################################################################
#***************************Sentinel 2A*******************************#
#######################################################################

#*****************************************************************************************************#
#   1  Cellulose Absorption Index (CAI) (2000,2200, 2100) (Nagler, P.; Daughtry, C.; Goward, S. Plant litter and soil reflectance. Remote Sens. Environ. 2000, 71, 207–215)                                  #
#****************************************************************************************************#                     
## C1 plots
RB11_C1_S2 <- tidy_data$Reflectance[tidy_data$Site == "C1" & tidy_data$Band == "B11"]
RB12_C1_S2 <- tidy_data$Reflectance[tidy_data$Site == "C1" & tidy_data$Band == "B12"]
CAI_C1_S2 <- RB12_C1_S2 / RB11_C1_S2

## C2 plots
RB11_C2_S2 <- tidy_data$Reflectance[tidy_data$Site == "C2" & tidy_data$Band == "B11"]
RB12_C2_S2 <- tidy_data$Reflectance[tidy_data$Site == "C2" & tidy_data$Band == "B12"]
CAI_C2_S2 <- RB12_C2_S2 / RB11_C2_S2

## C3 plots
RB11_C3_S2 <- tidy_data$Reflectance[tidy_data$Site == "C3" & tidy_data$Band == "B11"]
RB12_C3_S2 <- tidy_data$Reflectance[tidy_data$Site == "C3" & tidy_data$Band == "B12"]
CAI_C3_S2 <- RB12_C3_S2 / RB11_C3_S2

## C4 plots
RB11_C4_S2 <- tidy_data$Reflectance[tidy_data$Site == "C4" & tidy_data$Band == "B11"]
RB12_C4_S2 <- tidy_data$Reflectance[tidy_data$Site == "C4" & tidy_data$Band == "B12"]
CAI_C4_S2 <- RB12_C4_S2 / RB11_C4_S2

## C5 plots
RB11_C5_S2 <- tidy_data$Reflectance[tidy_data$Site == "C5" & tidy_data$Band == "B11"]
RB12_C5_S2 <- tidy_data$Reflectance[tidy_data$Site == "C5" & tidy_data$Band == "B12"]
CAI_C5_S2 <- RB12_C5_S2 / RB11_C5_S2

## C6 plots
RB11_C6_S2 <- tidy_data$Reflectance[tidy_data$Site == "C6" & tidy_data$Band == "B11"]
RB12_C6_S2 <- tidy_data$Reflectance[tidy_data$Site == "C6" & tidy_data$Band == "B12"]
CAI_C6_S2 <- RB12_C6_S2 / RB11_C6_S2

## C7 plots
RB11_C7_S2 <- tidy_data$Reflectance[tidy_data$Site == "C7" & tidy_data$Band == "B11"]
RB12_C7_S2 <- tidy_data$Reflectance[tidy_data$Site == "C7" & tidy_data$Band == "B12"]
CAI_C7_S2 <- RB12_C7_S2 / RB11_C7_S2

## C11 plots
RB11_C11_S2 <- tidy_data$Reflectance[tidy_data$Site == "C11" & tidy_data$Band == "B11"]
RB12_C11_S2 <- tidy_data$Reflectance[tidy_data$Site == "C11" & tidy_data$Band == "B12"]
CAI_C11_S2 <- RB12_C11_S2 / RB11_C11_S2

## C12 plots
RB11_C12_S2 <- tidy_data$Reflectance[tidy_data$Site == "C12" & tidy_data$Band == "B11"]
RB12_C12_S2 <- tidy_data$Reflectance[tidy_data$Site == "C12" & tidy_data$Band == "B12"]
CAI_C12_S2 <- RB12_C12_S2 / RB11_C12_S2

## C13 plots
RB11_C13_S2 <- tidy_data$Reflectance[tidy_data$Site == "C13" & tidy_data$Band == "B11"]
RB12_C13_S2 <- tidy_data$Reflectance[tidy_data$Site == "C13" & tidy_data$Band == "B12"]
CAI_C13_S2 <- RB12_C13_S2 / RB11_C13_S2

## C15 plots
RB11_C15_S2 <- tidy_data$Reflectance[tidy_data$Site == "C15" & tidy_data$Band == "B11"]
RB12_C15_S2 <- tidy_data$Reflectance[tidy_data$Site == "C15" & tidy_data$Band == "B12"]
CAI_C15_S2 <- RB12_C15_S2 / RB11_C15_S2

## C19 plots
RB11_C19_S2 <- tidy_data$Reflectance[tidy_data$Site == "C19" & tidy_data$Band == "B11"]
RB12_C19_S2 <- tidy_data$Reflectance[tidy_data$Site == "C19" & tidy_data$Band == "B12"]
CAI_C19_S2 <- RB12_C19_S2 / RB11_C19_S2

## C20 plots
RB11_C20_S2 <- tidy_data$Reflectance[tidy_data$Site == "C20" & tidy_data$Band == "B11"]
RB12_C20_S2 <- tidy_data$Reflectance[tidy_data$Site == "C20" & tidy_data$Band == "B12"]
CAI_C20_S2 <- RB12_C20_S2 / RB11_C20_S2

## N3 plots
RB11_N3_S2 <- tidy_data$Reflectance[tidy_data$Site == "N3" & tidy_data$Band == "B11"]
RB12_N3_S2 <- tidy_data$Reflectance[tidy_data$Site == "N3" & tidy_data$Band == "B12"]
CAI_N3_S2 <- RB12_N3_S2 / RB11_N3_S2

## N4 plots
RB11_N4_S2 <- tidy_data$Reflectance[tidy_data$Site == "N4" & tidy_data$Band == "B11"]
RB12_N4_S2 <- tidy_data$Reflectance[tidy_data$Site == "N4" & tidy_data$Band == "B12"]
CAI_N4_S2 <- RB12_N4_S2 / RB11_N4_S2

## N5 plots
RB11_N5_S2 <- tidy_data$Reflectance[tidy_data$Site == "N5" & tidy_data$Band == "B11"]
RB12_N5_S2 <- tidy_data$Reflectance[tidy_data$Site == "N5" & tidy_data$Band == "B12"]
CAI_N5_S2 <- RB12_N5_S2 / RB11_N5_S2

## N10 plots
RB11_N10_S2 <- tidy_data$Reflectance[tidy_data$Site == "N10" & tidy_data$Band == "B11"]
RB12_N10_S2 <- tidy_data$Reflectance[tidy_data$Site == "N10" & tidy_data$Band == "B12"]
CAI_N10_S2 <- RB12_N10_S2 / RB11_N10_S2

## N13 plots
RB11_N13_S2 <- tidy_data$Reflectance[tidy_data$Site == "N13" & tidy_data$Band == "B11"]
RB12_N13_S2 <- tidy_data$Reflectance[tidy_data$Site == "N13" & tidy_data$Band == "B12"]
CAI_N13_S2 <- RB12_N13_S2 / RB11_N13_S2

## N14 plots
RB11_N14_S2 <- tidy_data$Reflectance[tidy_data$Site == "N14" & tidy_data$Band == "B11"]
RB12_N14_S2 <- tidy_data$Reflectance[tidy_data$Site == "N14" & tidy_data$Band == "B12"]
CAI_N14_S2 <- RB12_N14_S2 / RB11_N14_S2

## N15 plots
RB11_N15_S2 <- tidy_data$Reflectance[tidy_data$Site == "N15" & tidy_data$Band == "B11"]
RB12_N15_S2 <- tidy_data$Reflectance[tidy_data$Site == "N15" & tidy_data$Band == "B12"]
CAI_N15_S2 <- RB12_N15_S2 / RB11_N15_S2

## N16 plots
RB11_N16_S2 <- tidy_data$Reflectance[tidy_data$Site == "N16" & tidy_data$Band == "B11"]
RB12_N16_S2 <- tidy_data$Reflectance[tidy_data$Site == "N16" & tidy_data$Band == "B12"]
CAI_N16_S2 <- RB12_N16_S2 / RB11_N16_S2


#########################################
###########Combine and Export############
#########################################
CAIS2_results <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20", "N3", "N4", "N5", "N10", "N13", "N14", "N15", "N16"),
  CAI = c(CAI_C1_S2, CAI_C2_S2, CAI_C3_S2, CAI_C4_S2, CAI_C5_S2, CAI_C6_S2, CAI_C7_S2,
          CAI_C11_S2, CAI_C12_S2, CAI_C13_S2, CAI_C15_S2, CAI_C19_S2, CAI_C20_S2,
          CAI_N3_S2, CAI_N4_S2, CAI_N5_S2, CAI_N10_S2, CAI_N13_S2, CAI_N14_S2, CAI_N15_S2, CAI_N16_S2))

#View(CAIS2_results)

#write.table(CAIS2_results, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/Sentinel/CAIS2_results.csv", sep = ",", row.names = FALSE)


#*****************************************************************************************************#
#                       2   Carotenoid Reflectance Index I     (Hill, 2013)                                       #
#*****************************************************************************************************#
## C1 plots
RBB2S2C1 <- tidy_data$Reflectance[tidy_data$Site == "C1" & tidy_data$Band == "B2"]
RBB3S2C1 <- tidy_data$Reflectance[tidy_data$Site == "C1" & tidy_data$Band == "B3"]
CRIS2C1 <- (1/RBB2S2C1) - (1/RBB3S2C1)
CRIS2C1

## C2 plots
RBB2S2C2 <- tidy_data$Reflectance[tidy_data$Site == "C2" & tidy_data$Band == "B2"]
RBB3S2C2 <- tidy_data$Reflectance[tidy_data$Site == "C2" & tidy_data$Band == "B3"]
CRIS2C2 <- (1/RBB2S2C2) - (1/RBB3S2C2)
CRIS2C2

## C3 plots
RBB2S2C3 <- tidy_data$Reflectance[tidy_data$Site == "C3" & tidy_data$Band == "B2"]
RBB3S2C3 <- tidy_data$Reflectance[tidy_data$Site == "C3" & tidy_data$Band == "B3"]
CRIS2C3 <- (1/RBB2S2C3) - (1/RBB3S2C3)
CRIS2C3

## C4 plots
RBB2S2C4 <- tidy_data$Reflectance[tidy_data$Site == "C4" & tidy_data$Band == "B2"]
RBB3S2C4 <- tidy_data$Reflectance[tidy_data$Site == "C4" & tidy_data$Band == "B3"]
CRIS2C4 <- (1/RBB2S2C4) - (1/RBB3S2C4)
CRIS2C4

## C5 plots
RBB2S2C5 <- tidy_data$Reflectance[tidy_data$Site == "C5" & tidy_data$Band == "B2"]
RBB3S2C5 <- tidy_data$Reflectance[tidy_data$Site == "C5" & tidy_data$Band == "B3"]
CRIS2C5 <- (1/RBB2S2C5) - (1/RBB3S2C5)
CRIS2C5

## C6 plots
RBB2S2C6 <- tidy_data$Reflectance[tidy_data$Site == "C6" & tidy_data$Band == "B2"]
RBB3S2C6 <- tidy_data$Reflectance[tidy_data$Site == "C6" & tidy_data$Band == "B3"]
CRIS2C6 <- (1/RBB2S2C6) - (1/RBB3S2C6)
CRIS2C6

## C7 plots
RBB2S2C7 <- tidy_data$Reflectance[tidy_data$Site == "C7" & tidy_data$Band == "B2"]
RBB3S2C7 <- tidy_data$Reflectance[tidy_data$Site == "C7" & tidy_data$Band == "B3"]
CRIS2C7 <- (1/RBB2S2C7) - (1/RBB3S2C7)
CRIS2C7

## C11 plots
RBB2S2C11 <- tidy_data$Reflectance[tidy_data$Site == "C11" & tidy_data$Band == "B2"]
RBB3S2C11 <- tidy_data$Reflectance[tidy_data$Site == "C11" & tidy_data$Band == "B3"]
CRIS2C11 <- (1/RBB2S2C11) - (1/RBB3S2C11)
CRIS2C11

## C12 plots
RBB2S2C12 <- tidy_data$Reflectance[tidy_data$Site == "C12" & tidy_data$Band == "B2"]
RBB3S2C12 <- tidy_data$Reflectance[tidy_data$Site == "C12" & tidy_data$Band == "B3"]
CRIS2C12 <- (1/RBB2S2C12) - (1/RBB3S2C12)
CRIS2C12

## C13 plots
RBB2S2C13 <- tidy_data$Reflectance[tidy_data$Site == "C13" & tidy_data$Band == "B2"]
RBB3S2C13 <- tidy_data$Reflectance[tidy_data$Site == "C13" & tidy_data$Band == "B3"]
CRIS2C13 <- (1/RBB2S2C13) - (1/RBB3S2C13)
CRIS2C13

## C15 plots
RBB2S2C15 <- tidy_data$Reflectance[tidy_data$Site == "C15" & tidy_data$Band == "B2"]
RBB3S2C15 <- tidy_data$Reflectance[tidy_data$Site == "C15" & tidy_data$Band == "B3"]
CRIS2C15 <- (1/RBB2S2C15) - (1/RBB3S2C15)
CRIS2C15

## C19 plots
RBB2S2C19 <- tidy_data$Reflectance[tidy_data$Site == "C19" & tidy_data$Band == "B2"]
RBB3S2C19 <- tidy_data$Reflectance[tidy_data$Site == "C19" & tidy_data$Band == "B3"]
CRIS2C19 <- (1/RBB2S2C19) - (1/RBB3S2C19)
CRIS2C19

## C20 plots
RBB2S2C20 <- tidy_data$Reflectance[tidy_data$Site == "C20" & tidy_data$Band == "B2"]
RBB3S2C20 <- tidy_data$Reflectance[tidy_data$Site == "C20" & tidy_data$Band == "B3"]
CRIS2C20 <- (1/RBB2S2C20) - (1/RBB3S2C20)
CRIS2C20

## N3 plots
RBB2S2N3 <- tidy_data$Reflectance[tidy_data$Site == "N3" & tidy_data$Band == "B2"]
RBB3S2N3 <- tidy_data$Reflectance[tidy_data$Site == "N3" & tidy_data$Band == "B3"]
CRIS2N3 <- (1/RBB2S2N3) - (1/RBB3S2N3)
CRIS2N3

## N4 plots
RBB2S2N4 <- tidy_data$Reflectance[tidy_data$Site == "N4" & tidy_data$Band == "B2"]
RBB3S2N4 <- tidy_data$Reflectance[tidy_data$Site == "N4" & tidy_data$Band == "B3"]
CRIS2N4 <- (1/RBB2S2N4) - (1/RBB3S2N4)
CRIS2N4

## N5 plots
RBB2S2N5 <- tidy_data$Reflectance[tidy_data$Site == "N5" & tidy_data$Band == "B2"]
RBB3S2N5 <- tidy_data$Reflectance[tidy_data$Site == "N5" & tidy_data$Band == "B3"]
CRIS2N5 <- (1/RBB2S2N5) - (1/RBB3S2N5)
CRIS2N5

## N10 plots
RBB2S2N10 <- tidy_data$Reflectance[tidy_data$Site == "N10" & tidy_data$Band == "B2"]
RBB3S2N10 <- tidy_data$Reflectance[tidy_data$Site == "N10" & tidy_data$Band == "B3"]
CRIS2N10 <- (1/RBB2S2N10) - (1/RBB3S2N10)
CRIS2N10

## N13 plots
RBB2S2N13 <- tidy_data$Reflectance[tidy_data$Site == "N13" & tidy_data$Band == "B2"]
RBB3S2N13 <- tidy_data$Reflectance[tidy_data$Site == "N13" & tidy_data$Band == "B3"]
CRIS2N13 <- (1/RBB2S2N13) - (1/RBB3S2N13)
CRIS2N13

## N14 plots
RBB2S2N14 <- tidy_data$Reflectance[tidy_data$Site == "N14" & tidy_data$Band == "B2"]
RBB3S2N14 <- tidy_data$Reflectance[tidy_data$Site == "N14" & tidy_data$Band == "B3"]
CRIS2N14 <- (1/RBB2S2N14) - (1/RBB3S2N14)
CRIS2N14

## N15 plots
RBB2S2N15 <- tidy_data$Reflectance[tidy_data$Site == "N15" & tidy_data$Band == "B2"]
RBB3S2N15 <- tidy_data$Reflectance[tidy_data$Site == "N15" & tidy_data$Band == "B3"]
CRIS2N15 <- (1/RBB2S2N15) - (1/RBB3S2N15)
CRIS2N15

## N16 plots
RBB2S2N16 <- tidy_data$Reflectance[tidy_data$Site == "N16" & tidy_data$Band == "B2"]
RBB3S2N16 <- tidy_data$Reflectance[tidy_data$Site == "N16" & tidy_data$Band == "B3"]
CRIS2N16 <- (1/RBB2S2N16) - (1/RBB3S2N16)
CRIS2N16

#########################################
###########Combine and Export############
#########################################
CRI_S2_results <- CRI_510_550 <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20","N3","N4","N5","N10","N13","N14","N15","N16"),
  CRI = c(CRIS2C1, CRIS2C2, CRIS2C3, CRIS2C4,
                   CRIS2C5, CRIS2C6, CRIS2C7,
                   CRIS2C11, CRIS2C12, CRIS2C13, CRIS2C15, 
                   CRIS2C19, CRIS2C20, CRIS2N3, 
                   CRIS2N4, CRIS2N5, CRIS2N10,
                   CRIS2N13, CRIS2N14, CRIS2N15,
                   CRIS2N16))

CRI_S2_results
#View(CRI_S2_results)

#Export the file
#write.table(CRI_S2_results, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/Sentinel/CRI_S2_results.csv", sep = ",", row.names = FALSE)


#*****************************************************************************************************#
#3 Normalized Differenced Vegetation indices (NDVI) (682-553) ((A. Gitelson & Merzlyak, 1994rudel, Thierry
#Fabre, Sophie, Houet, Thomas, Mazier, Florence, Briottet, Xavier# Opposite to traditional NDVI
#*****************************************************************************************************#
## C1 plots
## NDVI(682/553) (Number in the VI list: 1)
RB3C1 <- tidy_data$Reflectance[tidy_data$Site == "C1" & tidy_data$Band == "B3"]
RB4C1 <- tidy_data$Reflectance[tidy_data$Site == "C1" & tidy_data$Band == "B4"]
NDVIS2_C1 <- (RB4C1-RB3C1) / (RB4C1+RB3C1)
NDVIS2_C1

# C2
RB3C2 <- tidy_data$Reflectance[tidy_data$Site == "C2" & tidy_data$Band == "B3"]
RB4C2 <- tidy_data$Reflectance[tidy_data$Site == "C2" & tidy_data$Band == "B4"]
NDVIS2_C2 <- (RB4C2 - RB3C2) / (RB4C2 + RB3C2)
NDVIS2_C2

# C3
RB3C3 <- tidy_data$Reflectance[tidy_data$Site == "C3" & tidy_data$Band == "B3"]
RB4C3 <- tidy_data$Reflectance[tidy_data$Site == "C3" & tidy_data$Band == "B4"]
NDVIS2_C3 <- (RB4C3 - RB3C3) / (RB4C3 + RB3C3)
NDVIS2_C3

# C4
RB3C4 <- tidy_data$Reflectance[tidy_data$Site == "C4" & tidy_data$Band == "B3"]
RB4C4 <- tidy_data$Reflectance[tidy_data$Site == "C4" & tidy_data$Band == "B4"]
NDVIS2_C4 <- (RB4C4 - RB3C4) / (RB4C4 + RB3C4)
NDVIS2_C4

# C5
RB3C5 <- tidy_data$Reflectance[tidy_data$Site == "C5" & tidy_data$Band == "B3"]
RB4C5 <- tidy_data$Reflectance[tidy_data$Site == "C5" & tidy_data$Band == "B4"]
NDVIS2_C5 <- (RB4C5 - RB3C5) / (RB4C5 + RB3C5)
NDVIS2_C5

# C6
RB3C6 <- tidy_data$Reflectance[tidy_data$Site == "C6" & tidy_data$Band == "B3"]
RB4C6 <- tidy_data$Reflectance[tidy_data$Site == "C6" & tidy_data$Band == "B4"]
NDVIS2_C6 <- (RB4C6 - RB3C6) / (RB4C6 + RB3C6)
NDVIS2_C6

# C7
RB3C7 <- tidy_data$Reflectance[tidy_data$Site == "C7" & tidy_data$Band == "B3"]
RB4C7 <- tidy_data$Reflectance[tidy_data$Site == "C7" & tidy_data$Band == "B4"]
NDVIS2_C7 <- (RB4C7 - RB3C7) / (RB4C7 + RB3C7)
NDVIS2_C7

# C11
RB3C11 <- tidy_data$Reflectance[tidy_data$Site == "C11" & tidy_data$Band == "B3"]
RB4C11 <- tidy_data$Reflectance[tidy_data$Site == "C11" & tidy_data$Band == "B4"]
NDVIS2_C11 <- (RB4C11 - RB3C11) / (RB4C11 + RB3C11)
NDVIS2_C11

# C12
RB3C12 <- tidy_data$Reflectance[tidy_data$Site == "C12" & tidy_data$Band == "B3"]
RB4C12 <- tidy_data$Reflectance[tidy_data$Site == "C12" & tidy_data$Band == "B4"]
NDVIS2_C12 <- (RB4C12 - RB3C12) / (RB4C12 + RB3C12)
NDVIS2_C12

# C13
RB3C13 <- tidy_data$Reflectance[tidy_data$Site == "C13" & tidy_data$Band == "B3"]
RB4C13 <- tidy_data$Reflectance[tidy_data$Site == "C13" & tidy_data$Band == "B4"]
NDVIS2_C13 <- (RB4C13 - RB3C13) / (RB4C13 + RB3C13)
NDVIS2_C13

# C15
RB3C15 <- tidy_data$Reflectance[tidy_data$Site == "C15" & tidy_data$Band == "B3"]
RB4C15 <- tidy_data$Reflectance[tidy_data$Site == "C15" & tidy_data$Band == "B4"]
NDVIS2_C15 <- (RB4C15 - RB3C15) / (RB4C15 + RB3C15)
NDVIS2_C15

# C19
RB3C19 <- tidy_data$Reflectance[tidy_data$Site == "C19" & tidy_data$Band == "B3"]
RB4C19 <- tidy_data$Reflectance[tidy_data$Site == "C19" & tidy_data$Band == "B4"]
NDVIS2_C19 <- (RB4C19 - RB3C19) / (RB4C19 + RB3C19)
NDVIS2_C19

# C20
RB3C20 <- tidy_data$Reflectance[tidy_data$Site == "C20" & tidy_data$Band == "B3"]
RB4C20 <- tidy_data$Reflectance[tidy_data$Site == "C20" & tidy_data$Band == "B4"]
NDVIS2_C20 <- (RB4C20 - RB3C20) / (RB4C20 + RB3C20)
NDVIS2_C20

# N3
RB3N3 <- tidy_data$Reflectance[tidy_data$Site == "N3" & tidy_data$Band == "B3"]
RB4N3 <- tidy_data$Reflectance[tidy_data$Site == "N3" & tidy_data$Band == "B4"]
NDVIS2_N3 <- (RB4N3 - RB3N3) / (RB4N3 + RB3N3)
NDVIS2_N3

# N4
RB3N4 <- tidy_data$Reflectance[tidy_data$Site == "N4" & tidy_data$Band == "B3"]
RB4N4 <- tidy_data$Reflectance[tidy_data$Site == "N4" & tidy_data$Band == "B4"]
NDVIS2_N4 <- (RB4N4 - RB3N4) / (RB4N4 + RB3N4)
NDVIS2_N4

# N5
RB3N5 <- tidy_data$Reflectance[tidy_data$Site == "N5" & tidy_data$Band == "B3"]
RB4N5 <- tidy_data$Reflectance[tidy_data$Site == "N5" & tidy_data$Band == "B4"]
NDVIS2_N5 <- (RB4N5 - RB3N5) / (RB4N5 + RB3N5)
NDVIS2_N5

# N10
RB3N10 <- tidy_data$Reflectance[tidy_data$Site == "N10" & tidy_data$Band == "B3"]
RB4N10 <- tidy_data$Reflectance[tidy_data$Site == "N10" & tidy_data$Band == "B4"]
NDVIS2_N10 <- (RB4N10 - RB3N10) / (RB4N10 + RB3N10)
NDVIS2_N10

# N13
RB3N13 <- tidy_data$Reflectance[tidy_data$Site == "N13" & tidy_data$Band == "B3"]
RB4N13 <- tidy_data$Reflectance[tidy_data$Site == "N13" & tidy_data$Band == "B4"]
NDVIS2_N13 <- (RB4N13 - RB3N13) / (RB4N13 + RB3N13)
NDVIS2_N13

# N14
RB3N14 <- tidy_data$Reflectance[tidy_data$Site == "N14" & tidy_data$Band == "B3"]
RB4N14 <- tidy_data$Reflectance[tidy_data$Site == "N14" & tidy_data$Band == "B4"]
NDVIS2_N14 <- (RB4N14 - RB3N14) / (RB4N14 + RB3N14)
NDVIS2_N14

# N15
RB3N15 <- tidy_data$Reflectance[tidy_data$Site == "N15" & tidy_data$Band == "B3"]
RB4N15 <- tidy_data$Reflectance[tidy_data$Site == "N15" & tidy_data$Band == "B4"]
NDVIS2_N15 <- (RB4N15 - RB3N15) / (RB4N15 + RB3N15)
NDVIS2_N15

# N16
RB3N16 <- tidy_data$Reflectance[tidy_data$Site == "N16" & tidy_data$Band == "B3"]
RB4N16 <- tidy_data$Reflectance[tidy_data$Site == "N16" & tidy_data$Band == "B4"]
NDVIS2_N16 <- (RB4N16 - RB3N16) / (RB4N16 + RB3N16)
NDVIS2_N16

#########################################
###########Combine and Export############
#########################################
NDVI_S2_results <- NDVI_682_553 <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20","N3","N4","N5","N10","N13","N14","N15","N16"),
  NDVIS2 = c(NDVIS2_C1, NDVIS2_C2, NDVIS2_C3, NDVIS2_C4,
             NDVIS2_C5, NDVIS2_C6, NDVIS2_C7,
             NDVIS2_C11, NDVIS2_C12, NDVIS2_C13, NDVIS2_C15, 
             NDVIS2_C19, NDVIS2_C20, NDVIS2_N3, 
             NDVIS2_N4, NDVIS2_N5, NDVIS2_N10,
             NDVIS2_N13, NDVIS2_N14, NDVIS2_N15,
             NDVIS2_N16))

#View(NDVI_S2_results)


#Export the file
#write.table(NDVI_S2_results, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/Sentinel/NDVI_S2_results.csv", sep = ",", row.names = FALSE)


#*****************************************************************************************************#
# 4                                     RGR (Red Green Ratio) 
#*****************************************************************************************************#
## C1 plots
## 
RB3C1 <- tidy_data$Reflectance[tidy_data$Site == "C1" & tidy_data$Band == "B3"]
RB4C1 <- tidy_data$Reflectance[tidy_data$Site == "C1" & tidy_data$Band == "B4"]
RGRS2_C1 <- RB4C1 / RB3C1
RGRS2_C1

# C2
RB3C2 <- tidy_data$Reflectance[tidy_data$Site == "C2" & tidy_data$Band == "B3"]
RB4C2 <- tidy_data$Reflectance[tidy_data$Site == "C2" & tidy_data$Band == "B4"]
RGRS2_C2 <- RB4C2 / RB3C2
RGRS2_C2

# C3
RB3C3 <- tidy_data$Reflectance[tidy_data$Site == "C3" & tidy_data$Band == "B3"]
RB4C3 <- tidy_data$Reflectance[tidy_data$Site == "C3" & tidy_data$Band == "B4"]
RGRS2_C3 <- RB4C3 / RB3C3
RGRS2_C3

# C4
RB3C4 <- tidy_data$Reflectance[tidy_data$Site == "C4" & tidy_data$Band == "B3"]
RB4C4 <- tidy_data$Reflectance[tidy_data$Site == "C4" & tidy_data$Band == "B4"]
RGRS2_C4 <- RB4C4 / RB3C4
RGRS2_C4

# C5
RB3C5 <- tidy_data$Reflectance[tidy_data$Site == "C5" & tidy_data$Band == "B3"]
RB4C5 <- tidy_data$Reflectance[tidy_data$Site == "C5" & tidy_data$Band == "B4"]
RGRS2_C5 <- RB4C5 / RB3C5
RGRS2_C5

# C6
RB3C6 <- tidy_data$Reflectance[tidy_data$Site == "C6" & tidy_data$Band == "B3"]
RB4C6 <- tidy_data$Reflectance[tidy_data$Site == "C6" & tidy_data$Band == "B4"]
RGRS2_C6 <- RB4C6 / RB3C6
RGRS2_C6

# C7
RB3C7 <- tidy_data$Reflectance[tidy_data$Site == "C7" & tidy_data$Band == "B3"]
RB4C7 <- tidy_data$Reflectance[tidy_data$Site == "C7" & tidy_data$Band == "B4"]
RGRS2_C7 <- RB4C7 / RB3C7
RGRS2_C7

# C11
RB3C11 <- tidy_data$Reflectance[tidy_data$Site == "C11" & tidy_data$Band == "B3"]
RB4C11 <- tidy_data$Reflectance[tidy_data$Site == "C11" & tidy_data$Band == "B4"]
RGRS2_C11 <- RB4C11 / RB3C11
RGRS2_C11

# C12
RB3C12 <- tidy_data$Reflectance[tidy_data$Site == "C12" & tidy_data$Band == "B3"]
RB4C12 <- tidy_data$Reflectance[tidy_data$Site == "C12" & tidy_data$Band == "B4"]
RGRS2_C12 <- RB4C12 / RB3C12
RGRS2_C12

# C13
RB3C13 <- tidy_data$Reflectance[tidy_data$Site == "C13" & tidy_data$Band == "B3"]
RB4C13 <- tidy_data$Reflectance[tidy_data$Site == "C13" & tidy_data$Band == "B4"]
RGRS2_C13 <- RB4C13 / RB3C13
RGRS2_C13

# C15
RB3C15 <- tidy_data$Reflectance[tidy_data$Site == "C15" & tidy_data$Band == "B3"]
RB4C15 <- tidy_data$Reflectance[tidy_data$Site == "C15" & tidy_data$Band == "B4"]
RGRS2_C15 <- RB4C15 / RB3C15
RGRS2_C15

# C19
RB3C19 <- tidy_data$Reflectance[tidy_data$Site == "C19" & tidy_data$Band == "B3"]
RB4C19 <- tidy_data$Reflectance[tidy_data$Site == "C19" & tidy_data$Band == "B4"]
RGRS2_C19 <- RB4C19 / RB3C19
RGRS2_C19

# C20
RB3C20 <- tidy_data$Reflectance[tidy_data$Site == "C20" & tidy_data$Band == "B3"]
RB4C20 <- tidy_data$Reflectance[tidy_data$Site == "C20" & tidy_data$Band == "B4"]
RGRS2_C20 <- RB4C20 / RB3C20
RGRS2_C20

# N3
RB3N3 <- tidy_data$Reflectance[tidy_data$Site == "N3" & tidy_data$Band == "B3"]
RB4N3 <- tidy_data$Reflectance[tidy_data$Site == "N3" & tidy_data$Band == "B4"]
RGRS2_N3 <- RB4N3 / RB3N3
RGRS2_N3

# N4
RB3N4 <- tidy_data$Reflectance[tidy_data$Site == "N4" & tidy_data$Band == "B3"]
RB4N4 <- tidy_data$Reflectance[tidy_data$Site == "N4" & tidy_data$Band == "B4"]
RGRS2_N4 <- RB4N4 / RB3N4
RGRS2_N4

# N5
RB3N5 <- tidy_data$Reflectance[tidy_data$Site == "N5" & tidy_data$Band == "B3"]
RB4N5 <- tidy_data$Reflectance[tidy_data$Site == "N5" & tidy_data$Band == "B4"]
RGRS2_N5 <- RB4N5 / RB3N5
RGRS2_N5

# N10
RB3N10 <- tidy_data$Reflectance[tidy_data$Site == "N10" & tidy_data$Band == "B3"]
RB4N10 <- tidy_data$Reflectance[tidy_data$Site == "N10" & tidy_data$Band == "B4"]
RGRS2_N10 <- RB4N10 / RB3N10
RGRS2_N10

# N13
RB3N13 <- tidy_data$Reflectance[tidy_data$Site == "N13" & tidy_data$Band == "B3"]
RB4N13 <- tidy_data$Reflectance[tidy_data$Site == "N13" & tidy_data$Band == "B4"]
RGRS2_N13 <- RB4N13 / RB3N13
RGRS2_N13

# N14
RB3N14 <- tidy_data$Reflectance[tidy_data$Site == "N14" & tidy_data$Band == "B3"]
RB4N14 <- tidy_data$Reflectance[tidy_data$Site == "N14" & tidy_data$Band == "B4"]
RGRS2_N14 <- RB4N14 / RB3N14
RGRS2_N14

# N15
RB3N15 <- tidy_data$Reflectance[tidy_data$Site == "N15" & tidy_data$Band == "B3"]
RB4N15 <- tidy_data$Reflectance[tidy_data$Site == "N15" & tidy_data$Band == "B4"]
RGRS2_N15 <- RB4N15 / RB3N15
RGRS2_N15

# N16
RB3N16 <- tidy_data$Reflectance[tidy_data$Site == "N16" & tidy_data$Band == "B3"]
RB4N16 <- tidy_data$Reflectance[tidy_data$Site == "N16" & tidy_data$Band == "B4"]
RGRS2_N16 <- RB4N16 / RB3N16
RGRS2_N16

#########################################
###########Combine and Export############
#########################################
RGR_S2_results <- RGR <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20","N3","N4","N5","N10","N13","N14","N15","N16"),
  RGRS2 = c(RGRS2_C1, RGRS2_C2, RGRS2_C3, RGRS2_C4,
             RGRS2_C5, RGRS2_C6, RGRS2_C7,
             RGRS2_C11, RGRS2_C12, RGRS2_C13, RGRS2_C15, 
             RGRS2_C19, RGRS2_C20, RGRS2_N3,
             RGRS2_N4, RGRS2_N5, RGRS2_N10,
             RGRS2_N13, RGRS2_N14, RGRS2_N15,
             RGRS2_N16))

#View(RGR_S2_results)

#Export the file
#write.table(RGR_S2_results, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/Sentinel/RGR_S2_results.csv", sep = ",", row.names = FALSE)



#*****************************************************************************************************#
# 5                           Disease-Water Stress Index 2 (Diseases water stress)
#*****************************************************************************************************#
## C1 plots
## 
WSIB3C1 <- tidy_data$Reflectance[tidy_data$Site == "C1" & tidy_data$Band == "B3"]
WSIB11C1 <- tidy_data$Reflectance[tidy_data$Site == "C1" & tidy_data$Band == "B11"]
WSIS2_C1 <- WSIB11C1 / WSIB3C1
WSIS2_C1

# C2
WSIB3C2 <- tidy_data$Reflectance[tidy_data$Site == "C2" & tidy_data$Band == "B3"]
WSIB11C2 <- tidy_data$Reflectance[tidy_data$Site == "C2" & tidy_data$Band == "B11"]
WSIS2_C2 <- WSIB11C2 / WSIB3C2
WSIS2_C2

# C3
WSIB3C3 <- tidy_data$Reflectance[tidy_data$Site == "C3" & tidy_data$Band == "B3"]
WSIB11C3 <- tidy_data$Reflectance[tidy_data$Site == "C3" & tidy_data$Band == "B11"]
WSIS2_C3 <- WSIB11C3 / WSIB3C3
WSIS2_C3

# C4
WSIB3C4 <- tidy_data$Reflectance[tidy_data$Site == "C4" & tidy_data$Band == "B3"]
WSIB11C4 <- tidy_data$Reflectance[tidy_data$Site == "C4" & tidy_data$Band == "B11"]
WSIS2_C4 <- WSIB11C4 / WSIB3C4
WSIS2_C4

# C5
WSIB3C5 <- tidy_data$Reflectance[tidy_data$Site == "C5" & tidy_data$Band == "B3"]
WSIB11C5 <- tidy_data$Reflectance[tidy_data$Site == "C5" & tidy_data$Band == "B11"]
WSIS2_C5 <- WSIB11C5 / WSIB3C5
WSIS2_C5

# C6
WSIB3C6 <- tidy_data$Reflectance[tidy_data$Site == "C6" & tidy_data$Band == "B3"]
WSIB11C6 <- tidy_data$Reflectance[tidy_data$Site == "C6" & tidy_data$Band == "B11"]
WSIS2_C6 <- WSIB11C6 / WSIB3C6
WSIS2_C6

# C7
WSIB3C7 <- tidy_data$Reflectance[tidy_data$Site == "C7" & tidy_data$Band == "B3"]
WSIB11C7 <- tidy_data$Reflectance[tidy_data$Site == "C7" & tidy_data$Band == "B11"]
WSIS2_C7 <- WSIB11C7 / WSIB3C7
WSIS2_C7

# C11
WSIB3C11 <- tidy_data$Reflectance[tidy_data$Site == "C11" & tidy_data$Band == "B3"]
WSIB11C11 <- tidy_data$Reflectance[tidy_data$Site == "C11" & tidy_data$Band == "B11"]
WSIS2_C11 <- WSIB11C11 / WSIB3C11
WSIS2_C11

# C12
WSIB3C12 <- tidy_data$Reflectance[tidy_data$Site == "C12" & tidy_data$Band == "B3"]
WSIB11C12 <- tidy_data$Reflectance[tidy_data$Site == "C12" & tidy_data$Band == "B11"]
WSIS2_C12 <- WSIB11C12 / WSIB3C12
WSIS2_C12

# C13
WSIB3C13 <- tidy_data$Reflectance[tidy_data$Site == "C13" & tidy_data$Band == "B3"]
WSIB11C13 <- tidy_data$Reflectance[tidy_data$Site == "C13" & tidy_data$Band == "B11"]
WSIS2_C13 <- WSIB11C13 / WSIB3C13
WSIS2_C13

# C15
WSIB3C15 <- tidy_data$Reflectance[tidy_data$Site == "C15" & tidy_data$Band == "B3"]
WSIB11C15 <- tidy_data$Reflectance[tidy_data$Site == "C15" & tidy_data$Band == "B11"]
WSIS2_C15 <- WSIB11C15 / WSIB3C15
WSIS2_C15

# C19
WSIB3C19 <- tidy_data$Reflectance[tidy_data$Site == "C19" & tidy_data$Band == "B3"]
WSIB11C19 <- tidy_data$Reflectance[tidy_data$Site == "C19" & tidy_data$Band == "B11"]
WSIS2_C19 <- WSIB11C19 / WSIB3C19
WSIS2_C19

# C20
WSIB3C20 <- tidy_data$Reflectance[tidy_data$Site == "C20" & tidy_data$Band == "B3"]
WSIB11C20 <- tidy_data$Reflectance[tidy_data$Site == "C20" & tidy_data$Band == "B11"]
WSIS2_C20 <- WSIB11C20 / WSIB3C20
WSIS2_C20

# N3
WSIB3N3 <- tidy_data$Reflectance[tidy_data$Site == "N3" & tidy_data$Band == "B3"]
WSIB11N3 <- tidy_data$Reflectance[tidy_data$Site == "N3" & tidy_data$Band == "B11"]
WSIS2_N3 <- WSIB11N3 / WSIB3N3
WSIS2_N3

# N4
WSIB3N4 <- tidy_data$Reflectance[tidy_data$Site == "N4" & tidy_data$Band == "B3"]
WSIB11N4 <- tidy_data$Reflectance[tidy_data$Site == "N4" & tidy_data$Band == "B11"]
WSIS2_N4 <- WSIB11N4 / WSIB3N4
WSIS2_N4

# N5
WSIB3N5 <- tidy_data$Reflectance[tidy_data$Site == "N5" & tidy_data$Band == "B3"]
WSIB11N5 <- tidy_data$Reflectance[tidy_data$Site == "N5" & tidy_data$Band == "B11"]
WSIS2_N5 <- WSIB11N5 / WSIB3N5
WSIS2_N5

# N10
WSIB3N10 <- tidy_data$Reflectance[tidy_data$Site == "N10" & tidy_data$Band == "B3"]
WSIB11N10 <- tidy_data$Reflectance[tidy_data$Site == "N10" & tidy_data$Band == "B11"]
WSIS2_N10 <- WSIB11N10 / WSIB3N10
WSIS2_N10

# N13
WSIB3N13 <- tidy_data$Reflectance[tidy_data$Site == "N13" & tidy_data$Band == "B3"]
WSIB11N13 <- tidy_data$Reflectance[tidy_data$Site == "N13" & tidy_data$Band == "B11"]
WSIS2_N13 <- WSIB11N13 / WSIB3N13
WSIS2_N13

# N14
WSIB3N14 <- tidy_data$Reflectance[tidy_data$Site == "N14" & tidy_data$Band == "B3"]
WSIB11N14 <- tidy_data$Reflectance[tidy_data$Site == "N14" & tidy_data$Band == "B11"]
WSIS2_N14 <- WSIB11N14 / WSIB3N14
WSIS2_N14

# N15
WSIB3N15 <- tidy_data$Reflectance[tidy_data$Site == "N15" & tidy_data$Band == "B3"]
WSIB11N15 <- tidy_data$Reflectance[tidy_data$Site == "N15" & tidy_data$Band == "B11"]
WSIS2_N15 <- WSIB11N15 / WSIB3N15
WSIS2_N15

# N16
WSIB3N16 <- tidy_data$Reflectance[tidy_data$Site == "N16" & tidy_data$Band == "B3"]
WSIB11N16 <- tidy_data$Reflectance[tidy_data$Site == "N16" & tidy_data$Band == "B11"]
WSIS2_N16 <- WSIB11N16 / WSIB3N16
WSIS2_N16

#########################################
###########Combine and Export############
#########################################
WSI_S2_results <- WSI <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20","N3","N4","N5","N10","N13","N14","N15","N16"),
  WSI = c(WSIS2_C1, WSIS2_C2, WSIS2_C3, WSIS2_C4,
          WSIS2_C5, WSIS2_C6, WSIS2_C7,
          WSIS2_C11, WSIS2_C12, WSIS2_C13, WSIS2_C15, 
          WSIS2_C19, WSIS2_C20, WSIS2_N3,
          WSIS2_N4, WSIS2_N5, WSIS2_N10,
          WSIS2_N13, WSIS2_N14, WSIS2_N15,
          WSIS2_N16))

#View(WSI_S2_results)

#Export the file
#write.table(WSI_S2_results, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/Sentinel/WSI_S2_results.csv", sep = ",", row.names = FALSE)

#*****************************************************************************************************#
#6 Normalized differencesenescent vegetation index (NDSVI) (B11-B4) Qi, J., R. Marsett, P. Heilman, S. Bieden-Bender, 
# S. Moran, D. Goodrich, and M. Weltz. 2002. “RANGES Improves Satellite-Based Information and Land Cover 
# Assessments in Southwest United States.” Eos, Transactions American Geophysical Union 83 (51): 601–606. 
# doi:10.1029/2002EO000411.
#*****************************************************************************************************#
## C1 plots
RB11C1 <- tidy_data$Reflectance[tidy_data$Site == "C1" & tidy_data$Band == "B11"]
RB4C1 <- tidy_data$Reflectance[tidy_data$Site == "C1" & tidy_data$Band == "B4"]
NDSVIS2_C1 <- (RB11C1-RB4C1) / (RB11C1+RB4C1)
NDSVIS2_C1

# C2
RB11C2 <- tidy_data$Reflectance[tidy_data$Site == "C2" & tidy_data$Band == "B11"]
RB4C2 <- tidy_data$Reflectance[tidy_data$Site == "C2" & tidy_data$Band == "B4"]
NDSVIS2_C2 <- (RB11C2 - RB4C2) / (RB11C2 + RB4C2)
NDSVIS2_C2

# C3
RB11C3 <- tidy_data$Reflectance[tidy_data$Site == "C3" & tidy_data$Band == "B11"]
RB4C3 <- tidy_data$Reflectance[tidy_data$Site == "C3" & tidy_data$Band == "B4"]
NDSVIS2_C3 <- (RB11C3 - RB4C3) / (RB11C3 + RB4C3)
NDSVIS2_C3

# C4
RB11C4 <- tidy_data$Reflectance[tidy_data$Site == "C4" & tidy_data$Band == "B11"]
RB4C4 <- tidy_data$Reflectance[tidy_data$Site == "C4" & tidy_data$Band == "B4"]
NDSVIS2_C4 <- (RB11C4 - RB4C4) / (RB11C4 + RB4C4)
NDSVIS2_C4

# C5
RB11C5 <- tidy_data$Reflectance[tidy_data$Site == "C5" & tidy_data$Band == "B11"]
RB4C5 <- tidy_data$Reflectance[tidy_data$Site == "C5" & tidy_data$Band == "B4"]
NDSVIS2_C5 <- (RB11C5 - RB4C5) / (RB11C5 + RB4C5)
NDSVIS2_C5

# C6
RB11C6 <- tidy_data$Reflectance[tidy_data$Site == "C6" & tidy_data$Band == "B11"]
RB4C6 <- tidy_data$Reflectance[tidy_data$Site == "C6" & tidy_data$Band == "B4"]
NDSVIS2_C6 <- (RB11C6 - RB4C6) / (RB11C6 + RB4C6)
NDSVIS2_C6

# C7
RB11C7 <- tidy_data$Reflectance[tidy_data$Site == "C7" & tidy_data$Band == "B11"]
RB4C7 <- tidy_data$Reflectance[tidy_data$Site == "C7" & tidy_data$Band == "B4"]
NDSVIS2_C7 <- (RB11C7 - RB4C7) / (RB11C7 + RB4C7)
NDSVIS2_C7

# C11
RB11C11 <- tidy_data$Reflectance[tidy_data$Site == "C11" & tidy_data$Band == "B11"]
RB4C11 <- tidy_data$Reflectance[tidy_data$Site == "C11" & tidy_data$Band == "B4"]
NDSVIS2_C11 <- (RB11C11 - RB4C11) / (RB11C11 + RB4C11)
NDSVIS2_C11

# C12
RB11C12 <- tidy_data$Reflectance[tidy_data$Site == "C12" & tidy_data$Band == "B11"]
RB4C12 <- tidy_data$Reflectance[tidy_data$Site == "C12" & tidy_data$Band == "B4"]
NDSVIS2_C12 <- (RB11C12 - RB4C12) / (RB11C12 + RB4C12)
NDSVIS2_C12

# C13
RB11C13 <- tidy_data$Reflectance[tidy_data$Site == "C13" & tidy_data$Band == "B11"]
RB4C13 <- tidy_data$Reflectance[tidy_data$Site == "C13" & tidy_data$Band == "B4"]
NDSVIS2_C13 <- (RB11C13 - RB4C13) / (RB11C13 + RB4C13)
NDSVIS2_C13

# C15
RB11C15 <- tidy_data$Reflectance[tidy_data$Site == "C15" & tidy_data$Band == "B11"]
RB4C15 <- tidy_data$Reflectance[tidy_data$Site == "C15" & tidy_data$Band == "B4"]
NDSVIS2_C15 <- (RB11C15 - RB4C15) / (RB11C15 + RB4C15)
NDSVIS2_C15

# C19
RB11C19 <- tidy_data$Reflectance[tidy_data$Site == "C19" & tidy_data$Band == "B11"]
RB4C19 <- tidy_data$Reflectance[tidy_data$Site == "C19" & tidy_data$Band == "B4"]
NDSVIS2_C19 <- (RB11C19 - RB4C19) / (RB11C19 + RB4C19)
NDSVIS2_C19

# C20
RB11C20 <- tidy_data$Reflectance[tidy_data$Site == "C20" & tidy_data$Band == "B11"]
RB4C20 <- tidy_data$Reflectance[tidy_data$Site == "C20" & tidy_data$Band == "B4"]
NDSVIS2_C20 <- (RB11C20 - RB4C20) / (RB11C20 + RB4C20)
NDSVIS2_C20

# N3
RB11N3 <- tidy_data$Reflectance[tidy_data$Site == "N3" & tidy_data$Band == "B11"]
RB4N3 <- tidy_data$Reflectance[tidy_data$Site == "N3" & tidy_data$Band == "B4"]
NDSVIS2_N3 <- (RB11N3 - RB4N3) / (RB11N3 + RB4N3)
NDSVIS2_N3

# N4
RB11N4 <- tidy_data$Reflectance[tidy_data$Site == "N4" & tidy_data$Band == "B11"]
RB4N4 <- tidy_data$Reflectance[tidy_data$Site == "N4" & tidy_data$Band == "B4"]
NDSVIS2_N4 <- (RB11N4 - RB4N4) / (RB11N4 + RB4N4)
NDSVIS2_N4

# N5
RB11N5 <- tidy_data$Reflectance[tidy_data$Site == "N5" & tidy_data$Band == "B11"]
RB4N5 <- tidy_data$Reflectance[tidy_data$Site == "N5" & tidy_data$Band == "B4"]
NDSVIS2_N5 <- (RB11N5 - RB4N5) / (RB11N5 + RB4N5)
NDSVIS2_N5

# N10
RB11N10 <- tidy_data$Reflectance[tidy_data$Site == "N10" & tidy_data$Band == "B11"]
RB4N10 <- tidy_data$Reflectance[tidy_data$Site == "N10" & tidy_data$Band == "B4"]
NDSVIS2_N10 <- (RB11N10 - RB4N10) / (RB11N10 + RB4N10)
NDSVIS2_N10

# N13
RB11N13 <- tidy_data$Reflectance[tidy_data$Site == "N13" & tidy_data$Band == "B11"]
RB4N13 <- tidy_data$Reflectance[tidy_data$Site == "N13" & tidy_data$Band == "B4"]
NDSVIS2_N13 <- (RB11N13 - RB4N13) / (RB11N13 + RB4N13)
NDSVIS2_N13

# N14
RB11N14 <- tidy_data$Reflectance[tidy_data$Site == "N14" & tidy_data$Band == "B11"]
RB4N14 <- tidy_data$Reflectance[tidy_data$Site == "N14" & tidy_data$Band == "B4"]
NDSVIS2_N14 <- (RB11N14 - RB4N14) / (RB11N14 + RB4N14)
NDSVIS2_N14

# N15
RB11N15 <- tidy_data$Reflectance[tidy_data$Site == "N15" & tidy_data$Band == "B11"]
RB4N15 <- tidy_data$Reflectance[tidy_data$Site == "N15" & tidy_data$Band == "B4"]
NDSVIS2_N15 <- (RB11N15 - RB4N15) / (RB11N15 + RB4N15)
NDSVIS2_N15

# N16
RB11N16 <- tidy_data$Reflectance[tidy_data$Site == "N16" & tidy_data$Band == "B11"]
RB4N16 <- tidy_data$Reflectance[tidy_data$Site == "N16" & tidy_data$Band == "B4"]
NDSVIS2_N16 <- (RB11N16 - RB4N16) / (RB11N16 + RB4N16)
NDSVIS2_N16

#########################################
###########Combine and Export############
#########################################
NDSVI_S2_results <- NDSVI <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20","N3","N4","N5","N10","N13","N14","N15","N16"),
  NDSVI_S2 = c(NDSVIS2_C1, NDSVIS2_C2, NDSVIS2_C3, NDSVIS2_C4,
             NDSVIS2_C5, NDSVIS2_C6, NDSVIS2_C7,
             NDSVIS2_C11, NDSVIS2_C12, NDSVIS2_C13, NDSVIS2_C15, 
             NDSVIS2_C19, NDSVIS2_C20, NDSVIS2_N3, 
             NDSVIS2_N4, NDSVIS2_N5, NDSVIS2_N10,
             NDSVIS2_N13, NDSVIS2_N14, NDSVIS2_N15,
             NDSVIS2_N16))

#View(NDSVI_S2_results)


#Export the file
#write.table(NDSVI_S2_results, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/Sentinel/NDSVI_S2_results.csv", sep = ",", row.names = FALSE)


#*****************************************************************************************************#
#7 SATVI (Soil Adjusted Total Vegetation Index (B11, B12 and B4)  Marsett, R. C., Qi, J. G., Heilman, 
# P., Biedenbender, S. H., Watson, M. C., Amer, S., et al. (2006). Remote sensing for grassland management 
# in the arid Southwest. Range Ecology and Management, 59, 530–540
#*****************************************************************************************************#
# Define soil adjustment factor (L=0.5 as default)
L <- 0.5

## C1 plots (example template)
RB11C1 <- tidy_data$Reflectance[tidy_data$Site == "C1" & tidy_data$Band == "B11"]  # SWIR1
RB4C1  <- tidy_data$Reflectance[tidy_data$Site == "C1" & tidy_data$Band == "B4"]   # Red
RB12C1 <- tidy_data$Reflectance[tidy_data$Site == "C1" & tidy_data$Band == "B12"]  # SWIR2
SATVIS2_C1 <- ((RB11C1 - RB4C1) / (RB11C1 + RB4C1 + L)) * (1 + L) - (RB12C1 / 2)
SATVIS2_C1

## C2 plots
RB11C2 <- tidy_data$Reflectance[tidy_data$Site == "C2" & tidy_data$Band == "B11"]
RB4C2  <- tidy_data$Reflectance[tidy_data$Site == "C2" & tidy_data$Band == "B4"]
RB12C2 <- tidy_data$Reflectance[tidy_data$Site == "C2" & tidy_data$Band == "B12"]
SATVIS2_C2 <- ((RB11C2 - RB4C2) / (RB11C2 + RB4C2 + L)) * (1 + L) - (RB12C2 / 2)
SATVIS2_C2

## C3 plots
RB11C3 <- tidy_data$Reflectance[tidy_data$Site == "C3" & tidy_data$Band == "B11"]
RB4C3  <- tidy_data$Reflectance[tidy_data$Site == "C3" & tidy_data$Band == "B4"]
RB12C3 <- tidy_data$Reflectance[tidy_data$Site == "C3" & tidy_data$Band == "B12"]
SATVIS2_C3 <- ((RB11C3 - RB4C3) / (RB11C3 + RB4C3 + L)) * (1 + L) - (RB12C3 / 2)
SATVIS2_C3

## C4 plots
RB11C4 <- tidy_data$Reflectance[tidy_data$Site == "C4" & tidy_data$Band == "B11"]
RB4C4  <- tidy_data$Reflectance[tidy_data$Site == "C4" & tidy_data$Band == "B4"]
RB12C4 <- tidy_data$Reflectance[tidy_data$Site == "C4" & tidy_data$Band == "B12"]
SATVIS2_C4 <- ((RB11C4 - RB4C4) / (RB11C4 + RB4C4 + L)) * (1 + L) - (RB12C4 / 2)
SATVIS2_C4

## C5 plots
RB11C5 <- tidy_data$Reflectance[tidy_data$Site == "C5" & tidy_data$Band == "B11"]
RB4C5  <- tidy_data$Reflectance[tidy_data$Site == "C5" & tidy_data$Band == "B4"]
RB12C5 <- tidy_data$Reflectance[tidy_data$Site == "C5" & tidy_data$Band == "B12"]
SATVIS2_C5 <- ((RB11C5 - RB4C5) / (RB11C5 + RB4C5 + L)) * (1 + L) - (RB12C5 / 2)
SATVIS2_C5

## C6 plots
RB11C6 <- tidy_data$Reflectance[tidy_data$Site == "C6" & tidy_data$Band == "B11"]
RB4C6  <- tidy_data$Reflectance[tidy_data$Site == "C6" & tidy_data$Band == "B4"]
RB12C6 <- tidy_data$Reflectance[tidy_data$Site == "C6" & tidy_data$Band == "B12"]
SATVIS2_C6 <- ((RB11C6 - RB4C6) / (RB11C6 + RB4C6 + L)) * (1 + L) - (RB12C6 / 2)
SATVIS2_C6

## C7 plots
RB11C7 <- tidy_data$Reflectance[tidy_data$Site == "C7" & tidy_data$Band == "B11"]
RB4C7  <- tidy_data$Reflectance[tidy_data$Site == "C7" & tidy_data$Band == "B4"]
RB12C7 <- tidy_data$Reflectance[tidy_data$Site == "C7" & tidy_data$Band == "B12"]
SATVIS2_C7 <- ((RB11C7 - RB4C7) / (RB11C7 + RB4C7 + L)) * (1 + L) - (RB12C7 / 2)
SATVIS2_C7

## C11 plots
RB11C11 <- tidy_data$Reflectance[tidy_data$Site == "C11" & tidy_data$Band == "B11"]
RB4C11  <- tidy_data$Reflectance[tidy_data$Site == "C11" & tidy_data$Band == "B4"]
RB12C11 <- tidy_data$Reflectance[tidy_data$Site == "C11" & tidy_data$Band == "B12"]
SATVIS2_C11 <- ((RB11C11 - RB4C11) / (RB11C11 + RB4C11 + L)) * (1 + L) - (RB12C11 / 2)
SATVIS2_C11

## C12 plots
RB11C12 <- tidy_data$Reflectance[tidy_data$Site == "C12" & tidy_data$Band == "B11"]
RB4C12  <- tidy_data$Reflectance[tidy_data$Site == "C12" & tidy_data$Band == "B4"]
RB12C12 <- tidy_data$Reflectance[tidy_data$Site == "C12" & tidy_data$Band == "B12"]
SATVIS2_C12 <- ((RB11C12 - RB4C12) / (RB11C12 + RB4C12 + L)) * (1 + L) - (RB12C12 / 2)
SATVIS2_C12

## C13 plots
RB11C13 <- tidy_data$Reflectance[tidy_data$Site == "C13" & tidy_data$Band == "B11"]
RB4C13  <- tidy_data$Reflectance[tidy_data$Site == "C13" & tidy_data$Band == "B4"]
RB12C13 <- tidy_data$Reflectance[tidy_data$Site == "C13" & tidy_data$Band == "B12"]
SATVIS2_C13 <- ((RB11C13 - RB4C13) / (RB11C13 + RB4C13 + L)) * (1 + L) - (RB12C13 / 2)
SATVIS2_C13

## C15 plots
RB11C15 <- tidy_data$Reflectance[tidy_data$Site == "C15" & tidy_data$Band == "B11"]
RB4C15  <- tidy_data$Reflectance[tidy_data$Site == "C15" & tidy_data$Band == "B4"]
RB12C15 <- tidy_data$Reflectance[tidy_data$Site == "C15" & tidy_data$Band == "B12"]
SATVIS2_C15 <- ((RB11C15 - RB4C15) / (RB11C15 + RB4C15 + L)) * (1 + L) - (RB12C15 / 2)
SATVIS2_C15

## C19 plots
RB11C19 <- tidy_data$Reflectance[tidy_data$Site == "C19" & tidy_data$Band == "B11"]
RB4C19  <- tidy_data$Reflectance[tidy_data$Site == "C19" & tidy_data$Band == "B4"]
RB12C19 <- tidy_data$Reflectance[tidy_data$Site == "C19" & tidy_data$Band == "B12"]
SATVIS2_C19 <- ((RB11C19 - RB4C19) / (RB11C19 + RB4C19 + L)) * (1 + L) - (RB12C19 / 2)
SATVIS2_C19

## C20 plots
RB11C20 <- tidy_data$Reflectance[tidy_data$Site == "C20" & tidy_data$Band == "B11"]
RB4C20  <- tidy_data$Reflectance[tidy_data$Site == "C20" & tidy_data$Band == "B4"]
RB12C20 <- tidy_data$Reflectance[tidy_data$Site == "C20" & tidy_data$Band == "B12"]
SATVIS2_C20 <- ((RB11C20 - RB4C20) / (RB11C20 + RB4C20 + L)) * (1 + L) - (RB12C20 / 2)
SATVIS2_C20

## N3 plots
RB11N3 <- tidy_data$Reflectance[tidy_data$Site == "N3" & tidy_data$Band == "B11"]
RB4N3  <- tidy_data$Reflectance[tidy_data$Site == "N3" & tidy_data$Band == "B4"]
RB12N3 <- tidy_data$Reflectance[tidy_data$Site == "N3" & tidy_data$Band == "B12"]
SATVIS2_N3 <- ((RB11N3 - RB4N3) / (RB11N3 + RB4N3 + L)) * (1 + L) - (RB12N3 / 2)
SATVIS2_N3

## N4 plots
RB11N4 <- tidy_data$Reflectance[tidy_data$Site == "N4" & tidy_data$Band == "B11"]
RB4N4  <- tidy_data$Reflectance[tidy_data$Site == "N4" & tidy_data$Band == "B4"]
RB12N4 <- tidy_data$Reflectance[tidy_data$Site == "N4" & tidy_data$Band == "B12"]
SATVIS2_N4 <- ((RB11N4 - RB4N4) / (RB11N4 + RB4N4 + L)) * (1 + L) - (RB12N4 / 2)
SATVIS2_N4

## N5 plots
RB11N5 <- tidy_data$Reflectance[tidy_data$Site == "N5" & tidy_data$Band == "B11"]
RB4N5  <- tidy_data$Reflectance[tidy_data$Site == "N5" & tidy_data$Band == "B4"]
RB12N5 <- tidy_data$Reflectance[tidy_data$Site == "N5" & tidy_data$Band == "B12"]
SATVIS2_N5 <- ((RB11N5 - RB4N5) / (RB11N5 + RB4N5 + L)) * (1 + L) - (RB12N5 / 2)
SATVIS2_N5

## N10 plots
RB11N10 <- tidy_data$Reflectance[tidy_data$Site == "N10" & tidy_data$Band == "B11"]
RB4N10  <- tidy_data$Reflectance[tidy_data$Site == "N10" & tidy_data$Band == "B4"]
RB12N10 <- tidy_data$Reflectance[tidy_data$Site == "N10" & tidy_data$Band == "B12"]
SATVIS2_N10 <- ((RB11N10 - RB4N10) / (RB11N10 + RB4N10 + L)) * (1 + L) - (RB12N10 / 2)
SATVIS2_N10

## N13 plots
RB11N13 <- tidy_data$Reflectance[tidy_data$Site == "N13" & tidy_data$Band == "B11"]
RB4N13  <- tidy_data$Reflectance[tidy_data$Site == "N13" & tidy_data$Band == "B4"]
RB12N13 <- tidy_data$Reflectance[tidy_data$Site == "N13" & tidy_data$Band == "B12"]
SATVIS2_N13 <- ((RB11N13 - RB4N13) / (RB11N13 + RB4N13 + L)) * (1 + L) - (RB12N13 / 2)
SATVIS2_N13

## N14 plots
RB11N14 <- tidy_data$Reflectance[tidy_data$Site == "N14" & tidy_data$Band == "B11"]
RB4N14  <- tidy_data$Reflectance[tidy_data$Site == "N14" & tidy_data$Band == "B4"]
RB12N14 <- tidy_data$Reflectance[tidy_data$Site == "N14" & tidy_data$Band == "B12"]
SATVIS2_N14 <- ((RB11N14 - RB4N14) / (RB11N14 + RB4N14 + L)) * (1 + L) - (RB12N14 / 2)
SATVIS2_N14

## N15 plots
RB11N15 <- tidy_data$Reflectance[tidy_data$Site == "N15" & tidy_data$Band == "B11"]
RB4N15  <- tidy_data$Reflectance[tidy_data$Site == "N15" & tidy_data$Band == "B4"]
RB12N15 <- tidy_data$Reflectance[tidy_data$Site == "N15" & tidy_data$Band == "B12"]
SATVIS2_N15 <- ((RB11N15 - RB4N15) / (RB11N15 + RB4N15 + L)) * (1 + L) - (RB12N15 / 2)
SATVIS2_N15

## N16 plots
RB11N16 <- tidy_data$Reflectance[tidy_data$Site == "N16" & tidy_data$Band == "B11"]
RB4N16  <- tidy_data$Reflectance[tidy_data$Site == "N16" & tidy_data$Band == "B4"]
RB12N16 <- tidy_data$Reflectance[tidy_data$Site == "N16" & tidy_data$Band == "B12"]
SATVIS2_N16 <- ((RB11N16 - RB4N16) / (RB11N16 + RB4N16 + L)) * (1 + L) - (RB12N16 / 2)
SATVIS2_N16

#########################################
###########Combine and Export############
#########################################
SATVI_S2_results <- SATVI <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20","N3","N4","N5","N10","N13","N14","N15","N16"),
  SATVI_S2 = c(SATVIS2_C1, SATVIS2_C2, SATVIS2_C3, SATVIS2_C4,
              SATVIS2_C5, SATVIS2_C6, SATVIS2_C7,
              SATVIS2_C11, SATVIS2_C12, SATVIS2_C13, SATVIS2_C15, 
              SATVIS2_C19, SATVIS2_C20, SATVIS2_N3,
              SATVIS2_N4, SATVIS2_N5, SATVIS2_N10,
              SATVIS2_N13, SATVIS2_N14, SATVIS2_N15,
              SATVIS2_N16))

#View(SATVI_S2_results)


#Export the file
#write.table(SATVI_S2_results, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/Sentinel/SATVI_S2_results.csv", sep = ",", row.names = FALSE)


######################################################
######### Combine all these spectral indices datasets
######################################################
# List all your loaded data frames (replace with your actual object names)
SI_S2_list <- list(SATVI_S2_results,NDSVI_S2_results, WSI_S2_results, RGR_S2_results, NDVI_S2_results, 
                CRI_S2_results, CAIS2_results)

# Merge all by "Plot" (keeps all rows and fills missing values with NA)
combined_data <- reduce(SI_S2_list, ~ full_join(.x, .y, by = "Plot"))

######################################################
#*************Linear model with interaction test******#
######################################################

#setwd("D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data")
setwd("C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data")
#setwd("C:/Users/Mohammed/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data")

Biophy <- read_csv("PlotWiseDescriptiveStat.csv")
Biophy$Plot <- Biophy$Site
data_listS2 <- list(Biophy, combined_data)

# Merge all by "Plot" (keeps all rows and fills missing values with NA)
dataS2 <- reduce(data_listS2, ~ full_join(.x, .y, by = "Plot"))
dataS2 <- dataS2[, -c(7, 8, 11,12, 14, 15, 18)]
#View(dataS2)
names(dataS2)
#####################################################
#####################################################

##1 Height
#*1* Height VS CAI with species interaction
modelHeightCAI <- lm(Height ~ CAI * Species, data = dataS2)
modelHeightCAI
modelHeightCAI_summary <- summary(modelHeightCAI)
modelHeightCAI_summary

overall_r2HeightCAI <- modelHeightCAI_summary$r.squared
overall_r2HeightCAI

#*2* Height VS CRI with species interaction******************
modelHeightCRI <- lm(Height ~ CRI * Species, data = dataS2)
modelHeightCRI
modelHeightCRI_summary <- summary(modelHeightCRI)
modelHeightCRI_summary

overall_r2HeightCRI <- modelHeightCRI_summary$r.squared
overall_r2HeightCRI

#*3* Height VS NDVIS2 with species interaction******************
modelHeightNDVIS2 <- lm(Height ~ NDVIS2 * Species, data = dataS2)
modelHeightNDVIS2
modelHeightNDVIS2_summary <- summary(modelHeightNDVIS2)
modelHeightNDVIS2_summary

overall_r2HeightNDVIS2 <- modelHeightNDVIS2_summary$r.squared
overall_r2HeightNDVIS2

#*4* Height VS RGRS2 with species interaction******************
modelHeightRGRS2 <- lm(Height ~ RGRS2 * Species, data = dataS2)
modelHeightRGRS2
modelHeightRGRS2_summary <- summary(modelHeightRGRS2)
modelHeightRGRS2_summary

overall_r2HeightRGRS2 <- modelHeightRGRS2_summary$r.squared
overall_r2HeightRGRS2

#*5* Height VS WSI with species interaction******************
modelHeightWSI <- lm(Height ~ WSI * Species, data = dataS2)
modelHeightWSI
modelHeightWSI_summary <- summary(modelHeightWSI)
modelHeightWSI_summary

overall_r2HeightWSI <- modelHeightWSI_summary$r.squared
overall_r2HeightWSI

#*6* Height VS WSI with species interaction******************
modelHeightNDSVI_S2 <- lm(Height ~ NDSVI_S2 * Species, data = dataS2)
modelHeightNDSVI_S2
modelHeightNDSVI_S2_summary <- summary(modelHeightNDSVI_S2)
modelHeightNDSVI_S2_summary

overall_r2HeightNDSVI_S2 <- modelHeightNDSVI_S2_summary$r.squared
overall_r2HeightNDSVI_S2

#*7* Height VS WSI with species interaction******************
modelHeightSATVI_S2 <- lm(Height ~ SATVI_S2 * Species, data = dataS2)
modelHeightSATVI_S2
modelHeightSATVI_S2_summary <- summary(modelHeightSATVI_S2)
modelHeightSATVI_S2_summary

overall_r2HeightSATVI_S2 <- modelHeightSATVI_S2_summary$r.squared
overall_r2HeightSATVI_S2

#**********************************************************************#
#**********************************************************************#
##2 Cover
#*1* Cover VS CAI with species interaction
modelCoverCAI <- lm(Cover ~ CAI * Species, data = dataS2)
modelCoverCAI
modelCoverCAI_summary <- summary(modelCoverCAI)
modelCoverCAI_summary

overall_r2CoverCAI <- modelCoverCAI_summary$r.squared
overall_r2CoverCAI

#*2* Cover VS CRI with species interaction******************
modelCoverCRI <- lm(Cover ~ CRI * Species, data = dataS2)
modelCoverCRI
modelCoverCRI_summary <- summary(modelCoverCRI)
modelCoverCRI_summary

overall_r2CoverCRI <- modelCoverCRI_summary$r.squared
overall_r2CoverCRI

#*3* Cover VS NDVIS2 with species interaction******************
modelCoverNDVIS2 <- lm(Cover ~ NDVIS2 * Species, data = dataS2)
modelCoverNDVIS2
modelCoverNDVIS2_summary <- summary(modelCoverNDVIS2)
modelCoverNDVIS2_summary

overall_r2CoverNDVIS2 <- modelCoverNDVIS2_summary$r.squared
overall_r2CoverNDVIS2

#*4* Cover VS RGRS2 with species interaction******************
modelCoverRGRS2 <- lm(Cover ~ RGRS2 * Species, data = dataS2)
modelCoverRGRS2
modelCoverRGRS2_summary <- summary(modelCoverRGRS2)
modelCoverRGRS2_summary

overall_r2CoverRGRS2 <- modelCoverRGRS2_summary$r.squared
overall_r2CoverRGRS2

#*5* Cover VS WSI with species interaction******************
modelCoverWSI <- lm(Cover ~ WSI * Species, data = dataS2)
modelCoverWSI
modelCoverWSI_summary <- summary(modelCoverWSI)
modelCoverWSI_summary

overall_r2CoverWSI <- modelCoverWSI_summary$r.squared
overall_r2CoverWSI

#*6* Cover VS WSI with species interaction******************
modelCoverNDSVI_S2 <- lm(Cover ~ NDSVI_S2 * Species, data = dataS2)
modelCoverNDSVI_S2
modelCoverNDSVI_S2_summary <- summary(modelCoverNDSVI_S2)
modelCoverNDSVI_S2_summary

overall_r2CoverNDSVI_S2 <- modelCoverNDSVI_S2_summary$r.squared
overall_r2CoverNDSVI_S2

#*7* Cover VS WSI with species interaction******************
modelCoverSATVI_S2 <- lm(Cover ~ SATVI_S2 * Species, data = dataS2)
modelCoverSATVI_S2
modelCoverSATVI_S2_summary <- summary(modelCoverSATVI_S2)
modelCoverSATVI_S2_summary

overall_r2CoverSATVI_S2 <- modelCoverSATVI_S2_summary$r.squared
overall_r2CoverSATVI_S2


#**********************************************************************#
#**********************************************************************#
##3 Grassbiomass
#*1* Grassbiomass VS CAI with species interaction
modelGrassbiomassCAI <- lm(Grassbiomass ~ CAI * Species, data = dataS2)
modelGrassbiomassCAI
modelGrassbiomassCAI_summary <- summary(modelGrassbiomassCAI)
modelGrassbiomassCAI_summary

overall_r2GrassbiomassCAI <- modelGrassbiomassCAI_summary$r.squared
overall_r2GrassbiomassCAI

#*2* Grassbiomass VS CRI with species interaction******************
modelGrassbiomassCRI <- lm(Grassbiomass ~ CRI * Species, data = dataS2)
modelGrassbiomassCRI
modelGrassbiomassCRI_summary <- summary(modelGrassbiomassCRI)
modelGrassbiomassCRI_summary

overall_r2GrassbiomassCRI <- modelGrassbiomassCRI_summary$r.squared
overall_r2GrassbiomassCRI

#*3 Grassbiomass VS NDVIS2 with species interaction******************  ALMOST
modelGrassbiomassNDVIS2 <- lm(Grassbiomass ~ NDVIS2 * Species, data = dataS2)
modelGrassbiomassNDVIS2
modelGrassbiomassNDVIS2_summary <- summary(modelGrassbiomassNDVIS2)
modelGrassbiomassNDVIS2_summary

overall_r2GrassbiomassNDVIS2 <- modelGrassbiomassNDVIS2_summary$r.squared
overall_r2GrassbiomassNDVIS2

#*4* Grassbiomass VS RGRS2 with species interaction****************** ALMOST
modelGrassbiomassRGRS2 <- lm(Grassbiomass ~ RGRS2 * Species, data = dataS2)
modelGrassbiomassRGRS2
modelGrassbiomassRGRS2_summary <- summary(modelGrassbiomassRGRS2)
modelGrassbiomassRGRS2_summary

overall_r2GrassbiomassRGRS2 <- modelGrassbiomassRGRS2_summary$r.squared
overall_r2GrassbiomassRGRS2

#*5* Grassbiomass VS WSI with species interaction******************
modelGrassbiomassWSI <- lm(Grassbiomass ~ WSI * Species, data = dataS2)
modelGrassbiomassWSI
modelGrassbiomassWSI_summary <- summary(modelGrassbiomassWSI)
modelGrassbiomassWSI_summary

overall_r2GrassbiomassWSI <- modelGrassbiomassWSI_summary$r.squared
overall_r2GrassbiomassWSI

#*6* Grassbiomass VS WSI with species interaction******************
modelGrassbiomassNDSVI_S2 <- lm(Grassbiomass ~ NDSVI_S2 * Species, data = dataS2)
modelGrassbiomassNDSVI_S2
modelGrassbiomassNDSVI_S2_summary <- summary(modelGrassbiomassNDSVI_S2)
modelGrassbiomassNDSVI_S2_summary

overall_r2GrassbiomassNDSVI_S2 <- modelGrassbiomassNDSVI_S2_summary$r.squared
overall_r2GrassbiomassNDSVI_S2

#*7* Grassbiomass VS WSI with species interaction******************
modelGrassbiomassSATVI_S2 <- lm(Grassbiomass ~ SATVI_S2 * Species, data = dataS2)
modelGrassbiomassSATVI_S2
modelGrassbiomassSATVI_S2_summary <- summary(modelGrassbiomassSATVI_S2)
modelGrassbiomassSATVI_S2_summary

overall_r2GrassbiomassSATVI_S2 <- modelGrassbiomassSATVI_S2_summary$r.squared
overall_r2GrassbiomassSATVI_S2


#**********************************************************************#
#**********************************************************************#
##4 TotalBiomass
#*1* TotalBiomass VS CAI with species interaction
modelTotalBiomassCAI <- lm(TotalBiomass ~ CAI * Species, data = dataS2)
modelTotalBiomassCAI
modelTotalBiomassCAI_summary <- summary(modelTotalBiomassCAI)
modelTotalBiomassCAI_summary

overall_r2TotalBiomassCAI <- modelTotalBiomassCAI_summary$r.squared
overall_r2TotalBiomassCAI

#*2* TotalBiomass VS CRI with species interaction****************** ALMOST
modelTotalBiomassCRI <- lm(TotalBiomass ~ CRI * Species, data = dataS2)
modelTotalBiomassCRI
modelTotalBiomassCRI_summary <- summary(modelTotalBiomassCRI)
modelTotalBiomassCRI_summary

overall_r2TotalBiomassCRI <- modelTotalBiomassCRI_summary$r.squared
overall_r2TotalBiomassCRI

#*3***** TotalBiomass VS NDVIS2 with species interaction****************** SIGNIFICANT
modelTotalBiomassNDVIS2 <- lm(TotalBiomass ~ NDVIS2 * Species, data = dataS2)
modelTotalBiomassNDVIS2
modelTotalBiomassNDVIS2_summary <- summary(modelTotalBiomassNDVIS2)
modelTotalBiomassNDVIS2_summary

overall_r2TotalBiomassNDVIS2 <- modelTotalBiomassNDVIS2_summary$r.squared
overall_r2TotalBiomassNDVIS2

AS2<- ggplot(dataS2, aes(x = NDVIS2, y = TotalBiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # TotalBiomass label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
AS2 <- AS2 + theme(legend.position = "right")
AS2<-AS2+ annotate("text", 
                   x = 0.00, y = 520,    # Position (adjust based on your dataS2 range)
                   label = expression(italic(P)[int] == 0.0236), 
                   color = "black", 
                   size = 5)
AS2

#*4* TotalBiomass VS RGRS2 with species interaction****************** SIGNIFICANT
modelTotalBiomassRGRS2 <- lm(TotalBiomass ~ RGRS2 * Species, data = dataS2)
modelTotalBiomassRGRS2
modelTotalBiomassRGRS2_summary <- summary(modelTotalBiomassRGRS2)
modelTotalBiomassRGRS2_summary

overall_r2TotalBiomassRGRS2 <- modelTotalBiomassRGRS2_summary$r.squared
overall_r2TotalBiomassRGRS2

BS2<- ggplot(dataS2, aes(x = RGRS2, y = TotalBiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Red Green Ratio Sentinel-2 (RGRS2)",  # Replace with your desired title
    y = expression("Total Biomass" ~ (gm/m^2))# Replace with your desired title
  )+
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 18),  # TotalBiomass label size
    axis.title.y = element_text(size = 18),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 16),   # x-axis numbers
    axis.text.y = element_text(size = 16)    # y-axis numbers
  )
BS2 <- BS2 + theme(legend.position = "none")
BS2

# Calculate species-specific slopes
emtrends(modelTotalBiomassRGRS2, ~ Species, var = "RGRS2")

# Compute R² for each species subset
r2_valuesBS2 <- dataS2 %>%
  group_by(Species) %>%
  summarise(
    r_squared = r2(lm(TotalBiomass ~ RGRS2))$R2
  )
r2_valuesBS2


#*5* TotalBiomass VS WSI with species interaction******************
modelTotalBiomassWSI <- lm(TotalBiomass ~ WSI * Species, data = dataS2)
modelTotalBiomassWSI
modelTotalBiomassWSI_summary <- summary(modelTotalBiomassWSI)
modelTotalBiomassWSI_summary

overall_r2TotalBiomassWSI <- modelTotalBiomassWSI_summary$r.squared
overall_r2TotalBiomassWSI

#*6* TotalBiomass VS WSI with species interaction******************
modelTotalBiomassNDSVI_S2 <- lm(TotalBiomass ~ NDSVI_S2 * Species, data = dataS2)
modelTotalBiomassNDSVI_S2
modelTotalBiomassNDSVI_S2_summary <- summary(modelTotalBiomassNDSVI_S2)
modelTotalBiomassNDSVI_S2_summary

overall_r2TotalBiomassNDSVI_S2 <- modelTotalBiomassNDSVI_S2_summary$r.squared
overall_r2TotalBiomassNDSVI_S2

#*7* TotalBiomass VS WSI with species interaction******************
modelTotalBiomassSATVI_S2 <- lm(TotalBiomass ~ SATVI_S2 * Species, data = dataS2)
modelTotalBiomassSATVI_S2
modelTotalBiomassSATVI_S2_summary <- summary(modelTotalBiomassSATVI_S2)
modelTotalBiomassSATVI_S2_summary

overall_r2TotalBiomassSATVI_S2 <- modelTotalBiomassSATVI_S2_summary$r.squared
overall_r2TotalBiomassSATVI_S2


#**********************************************************************#
#**********************************************************************#
##5 LAI
#*1* LAI VS CAI with species interaction
modelLAICAI <- lm(LAI ~ CAI * Species, data = dataS2)
modelLAICAI
modelLAICAI_summary <- summary(modelLAICAI)
modelLAICAI_summary

overall_r2LAICAI <- modelLAICAI_summary$r.squared
overall_r2LAICAI

#*2* LAI VS CRI with species interaction******************
modelLAICRI <- lm(LAI ~ CRI * Species, data = dataS2)
modelLAICRI
modelLAICRI_summary <- summary(modelLAICRI)
modelLAICRI_summary

overall_r2LAICRI <- modelLAICRI_summary$r.squared
overall_r2LAICRI

#*3* LAI VS NDVIS2 with species interaction****************** SIGNIFICANT
modelLAINDVIS2 <- lm(LAI ~ NDVIS2 * Species, data = dataS2)
modelLAINDVIS2
modelLAINDVIS2_summary <- summary(modelLAINDVIS2)
modelLAINDVIS2_summary

overall_r2LAINDVIS2 <- modelLAINDVIS2_summary$r.squared
overall_r2LAINDVIS2

CS2<- ggplot(dataS2, aes(x = NDVIS2, y = LAI, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # TotalBiomass label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
CS2 <- CS2 + theme(legend.position = "None")
CS2<-CS2+ annotate("text", 
                   x = 0.00, y = 2.8,    # Position (adjust based on your dataS2 range)
                   label = expression(italic(P)[int] == 0.0399), 
                   color = "black", 
                   size = 5)
CS2

#*4* LAI VS RGRS2 with species interaction****************** SIGNIFICANT
modelLAIRGRS2 <- lm(LAI ~ RGRS2 * Species, data = dataS2)
modelLAIRGRS2
modelLAIRGRS2_summary <- summary(modelLAIRGRS2)
modelLAIRGRS2_summary

overall_r2LAIRGRS2 <- modelLAIRGRS2_summary$r.squared
overall_r2LAIRGRS2

DS2<- ggplot(dataS2, aes(x = RGRS2, y = LAI, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Red Green Ratio Sentinel-2 (RGRS2)",  # Replace with your desired title
    y = "Leaf Area Index" # Replace with your desired title
  )+
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 18),  # TotalBiomass label size
    axis.title.y = element_text(size = 18),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 16),   # x-axis numbers
    axis.text.y = element_text(size = 16)    # y-axis numbers
  )
DS2 <- DS2 + theme(legend.position = "None")
DS2

# Calculate species-specific slopes
emtrends(modelLAIRGRS2, ~ Species, var = "RGRS2")

# Compute R² for each species subset
r2_valuesDS2 <- dataS2 %>%
  group_by(Species) %>%
  summarise(
    r_squared = r2(lm(LAI ~ RGRS2))$R2
  )
r2_valuesDS2


#*5* LAI VS WSI with species interaction******************
modelLAIWSI <- lm(LAI ~ WSI * Species, data = dataS2)
modelLAIWSI
modelLAIWSI_summary <- summary(modelLAIWSI)
modelLAIWSI_summary

overall_r2LAIWSI <- modelLAIWSI_summary$r.squared
overall_r2LAIWSI

#*6* LAI VS WSI with species interaction******************
modelLAINDSVI_S2 <- lm(LAI ~ NDSVI_S2 * Species, data = dataS2)
modelLAINDSVI_S2
modelLAINDSVI_S2_summary <- summary(modelLAINDSVI_S2)
modelLAINDSVI_S2_summary

overall_r2LAINDSVI_S2 <- modelLAINDSVI_S2_summary$r.squared
overall_r2LAINDSVI_S2

#*7* LAI VS WSI with species interaction******************
modelLAISATVI_S2 <- lm(LAI ~ SATVI_S2 * Species, data = dataS2)
modelLAISATVI_S2
modelLAISATVI_S2_summary <- summary(modelLAISATVI_S2)
modelLAISATVI_S2_summary

overall_r2LAISATVI_S2 <- modelLAISATVI_S2_summary$r.squared
overall_r2LAISATVI_S2

#**********************************************************************#
#**********************************************************************#
##6 BareGroundCover
#*1* BareGroundCover VS CAI with species interaction
modelBareGroundCoverCAI <- lm(BareGroundCover ~ CAI * Species, data = dataS2)
modelBareGroundCoverCAI
modelBareGroundCoverCAI_summary <- summary(modelBareGroundCoverCAI)
modelBareGroundCoverCAI_summary

overall_r2BareGroundCoverCAI <- modelBareGroundCoverCAI_summary$r.squared
overall_r2BareGroundCoverCAI

#*2* BareGroundCover VS CRI with species interaction******************
modelBareGroundCoverCRI <- lm(BareGroundCover ~ CRI * Species, data = dataS2)
modelBareGroundCoverCRI
modelBareGroundCoverCRI_summary <- summary(modelBareGroundCoverCRI)
modelBareGroundCoverCRI_summary

overall_r2BareGroundCoverCRI <- modelBareGroundCoverCRI_summary$r.squared
overall_r2BareGroundCoverCRI

#*3* BareGroundCover VS NDVIS2 with species interaction****************** SIGNIFICANT
modelBareGroundCoverNDVIS2 <- lm(BareGroundCover ~ NDVIS2 * Species, data = dataS2)
modelBareGroundCoverNDVIS2
modelBareGroundCoverNDVIS2_summary <- summary(modelBareGroundCoverNDVIS2)
modelBareGroundCoverNDVIS2_summary

overall_r2BareGroundCoverNDVIS2 <- modelBareGroundCoverNDVIS2_summary$r.squared
overall_r2BareGroundCoverNDVIS2

ES2<- ggplot(dataS2, aes(x = NDVIS2, y = BareGroundCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # TotalBiomass label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
ES2 <- ES2 + theme(legend.position = "None")
ES2<-ES2+ annotate("text", 
                   x = -0.16, y = 14,    # Position (adjust based on your dataS2 range)
                   label = expression(italic(P)[int] == 0.0319), 
                   color = "black", 
                   size = 5)
ES2

#*4* BareGroundCover VS RGRS2 with species interaction****************** SIGNIFICANT
modelBareGroundCoverRGRS2 <- lm(BareGroundCover ~ RGRS2 * Species, data = dataS2)
modelBareGroundCoverRGRS2
modelBareGroundCoverRGRS2_summary <- summary(modelBareGroundCoverRGRS2)
modelBareGroundCoverRGRS2_summary

overall_r2BareGroundCoverRGRS2 <- modelBareGroundCoverRGRS2_summary$r.squared
overall_r2BareGroundCoverRGRS2

FS2<- ggplot(dataS2, aes(x = RGRS2, y = BareGroundCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # TotalBiomass label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
FS2 <- FS2 + theme(legend.position = "None")
FS2<-FS2+ annotate("text", 
                   x = 0.72, y = 14,    # Position (adjust based on your dataS2 range)
                   label = expression(italic(P)[int] == 0.0249), 
                   color = "black", 
                   size = 5)
FS2

#*5* BareGroundCover VS WSI with species interaction****************** SIGNIFICANT
modelBareGroundCoverWSI <- lm(BareGroundCover ~ WSI * Species, data = dataS2)
modelBareGroundCoverWSI
modelBareGroundCoverWSI_summary <- summary(modelBareGroundCoverWSI)
modelBareGroundCoverWSI_summary

overall_r2BareGroundCoverWSI <- modelBareGroundCoverWSI_summary$r.squared
overall_r2BareGroundCoverWSI

GS2<- ggplot(dataS2, aes(x = WSI, y = BareGroundCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Water Stress Index (WSI)",  # Replace with your desired title
    y = "Bare Ground (%)"# Replace with your desired title
  )+
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 18),  # TotalBiomass label size
    axis.title.y = element_text(size = 18),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 16),   # x-axis numbers
    axis.text.y = element_text(size = 16)    # y-axis numbers
  )
GS2 <- GS2 + theme(legend.position = "None")

GS2

#*********************
#******Lets get species-wise slope!!!
#*******************
library(emmeans)
library(performance)
# Calculate species-specific slopes
emtrends(modelBareGroundCoverWSI, ~ Species, var = "WSI")

# Compute R² for each species subset
r2_valuesGS2 <- dataS2 %>%
  group_by(Species) %>%
  summarise(
    r_squared = r2(lm(BareGroundCover ~ WSI))$R2
  )
r2_valuesGS2

#*6* BareGroundCover VS WSI with species interaction******************
modelBareGroundCoverNDSVI_S2 <- lm(BareGroundCover ~ NDSVI_S2 * Species, data = dataS2)
modelBareGroundCoverNDSVI_S2
modelBareGroundCoverNDSVI_S2_summary <- summary(modelBareGroundCoverNDSVI_S2)
modelBareGroundCoverNDSVI_S2_summary

overall_r2BareGroundCoverNDSVI_S2 <- modelBareGroundCoverNDSVI_S2_summary$r.squared
overall_r2BareGroundCoverNDSVI_S2

#*7* BareGroundCover VS WSI with species interaction******************
modelBareGroundCoverSATVI_S2 <- lm(BareGroundCover ~ SATVI_S2 * Species, data = dataS2)
modelBareGroundCoverSATVI_S2
modelBareGroundCoverSATVI_S2_summary <- summary(modelBareGroundCoverSATVI_S2)
modelBareGroundCoverSATVI_S2_summary

overall_r2BareGroundCoverSATVI_S2 <- modelBareGroundCoverSATVI_S2_summary$r.squared
overall_r2BareGroundCoverSATVI_S2


#**********************************************************************#
#**********************************************************************#
##7 DeadBiomass
#*1* DeadBiomass VS CAI with species interaction
modelDeadBiomassCAI <- lm(DeadBiomass ~ CAI * Species, data = dataS2)
modelDeadBiomassCAI
modelDeadBiomassCAI_summary <- summary(modelDeadBiomassCAI)
modelDeadBiomassCAI_summary

overall_r2DeadBiomassCAI <- modelDeadBiomassCAI_summary$r.squared
overall_r2DeadBiomassCAI

#*2* DeadBiomass VS CRI with species interaction******************
modelDeadBiomassCRI <- lm(DeadBiomass ~ CRI * Species, data = dataS2)
modelDeadBiomassCRI
modelDeadBiomassCRI_summary <- summary(modelDeadBiomassCRI)
modelDeadBiomassCRI_summary

overall_r2DeadBiomassCRI <- modelDeadBiomassCRI_summary$r.squared
overall_r2DeadBiomassCRI

#*3* DeadBiomass VS NDVIS2 with species interaction****************** ALMOST
modelDeadBiomassNDVIS2 <- lm(DeadBiomass ~ NDVIS2 * Species, data = dataS2)
modelDeadBiomassNDVIS2
modelDeadBiomassNDVIS2_summary <- summary(modelDeadBiomassNDVIS2)
modelDeadBiomassNDVIS2_summary

overall_r2DeadBiomassNDVIS2 <- modelDeadBiomassNDVIS2_summary$r.squared
overall_r2DeadBiomassNDVIS2

#*4* DeadBiomass VS RGRS2 with species interaction****************** ALMOST
modelDeadBiomassRGRS2 <- lm(DeadBiomass ~ RGRS2 * Species, data = dataS2)
modelDeadBiomassRGRS2
modelDeadBiomassRGRS2_summary <- summary(modelDeadBiomassRGRS2)
modelDeadBiomassRGRS2_summary

overall_r2DeadBiomassRGRS2 <- modelDeadBiomassRGRS2_summary$r.squared
overall_r2DeadBiomassRGRS2

#*5* DeadBiomass VS WSI with species interaction******************
modelDeadBiomassWSI <- lm(DeadBiomass ~ WSI * Species, data = dataS2)
modelDeadBiomassWSI
modelDeadBiomassWSI_summary <- summary(modelDeadBiomassWSI)
modelDeadBiomassWSI_summary

overall_r2DeadBiomassWSI <- modelDeadBiomassWSI_summary$r.squared
overall_r2DeadBiomassWSI

#*6* DeadBiomass VS WSI with species interaction******************
modelDeadBiomassNDSVI_S2 <- lm(DeadBiomass ~ NDSVI_S2 * Species, data = dataS2)
modelDeadBiomassNDSVI_S2
modelDeadBiomassNDSVI_S2_summary <- summary(modelDeadBiomassNDSVI_S2)
modelDeadBiomassNDSVI_S2_summary

overall_r2DeadBiomassNDSVI_S2 <- modelDeadBiomassNDSVI_S2_summary$r.squared
overall_r2DeadBiomassNDSVI_S2

#*7* DeadBiomass VS WSI with species interaction******************
modelDeadBiomassSATVI_S2 <- lm(DeadBiomass ~ SATVI_S2 * Species, data = dataS2)
modelDeadBiomassSATVI_S2
modelDeadBiomassSATVI_S2_summary <- summary(modelDeadBiomassSATVI_S2)
modelDeadBiomassSATVI_S2_summary

overall_r2DeadBiomassSATVI_S2 <- modelDeadBiomassSATVI_S2_summary$r.squared
overall_r2DeadBiomassSATVI_S2


#**********************************************************************#
#**********************************************************************#
##8 ForbsBiomass                                    SINGNIFICANT
#*1* ForbsBiomass VS CAI with species interaction
modelForbsBiomassCAI <- lm(ForbsBiomass ~ CAI * Species, data = dataS2)
modelForbsBiomassCAI
modelForbsBiomassCAI_summary <- summary(modelForbsBiomassCAI)
modelForbsBiomassCAI_summary

overall_r2ForbsBiomassCAI <- modelForbsBiomassCAI_summary$r.squared
overall_r2ForbsBiomassCAI

HS2<- ggplot(dataS2, aes(x = CAI, y = ForbsBiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # TotalBiomass label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
HS2 <- HS2 + theme(legend.position = "None")
HS2<-HS2+ annotate("text", 
                   x = 0.57, y = 40,    # Position (adjust based on your dataS2 range)
                   label = expression(italic(P)[int] == 0.0227), 
                   color = "black", 
                   size = 5)
HS2

#*2* ForbsBiomass VS CRI with species interaction******************
modelForbsBiomassCRI <- lm(ForbsBiomass ~ CRI * Species, data = dataS2)
modelForbsBiomassCRI
modelForbsBiomassCRI_summary <- summary(modelForbsBiomassCRI)
modelForbsBiomassCRI_summary

overall_r2ForbsBiomassCRI <- modelForbsBiomassCRI_summary$r.squared
overall_r2ForbsBiomassCRI

#*3* ForbsBiomass VS NDVIS2 with species interaction******************
modelForbsBiomassNDVIS2 <- lm(ForbsBiomass ~ NDVIS2 * Species, data = dataS2)
modelForbsBiomassNDVIS2
modelForbsBiomassNDVIS2_summary <- summary(modelForbsBiomassNDVIS2)
modelForbsBiomassNDVIS2_summary

overall_r2ForbsBiomassNDVIS2 <- modelForbsBiomassNDVIS2_summary$r.squared
overall_r2ForbsBiomassNDVIS2

#*4* ForbsBiomass VS RGRS2 with species interaction******************
modelForbsBiomassRGRS2 <- lm(ForbsBiomass ~ RGRS2 * Species, data = dataS2)
modelForbsBiomassRGRS2
modelForbsBiomassRGRS2_summary <- summary(modelForbsBiomassRGRS2)
modelForbsBiomassRGRS2_summary

overall_r2ForbsBiomassRGRS2 <- modelForbsBiomassRGRS2_summary$r.squared
overall_r2ForbsBiomassRGRS2

#*5* ForbsBiomass VS WSI with species interaction******************  SIGNIFICNAT
modelForbsBiomassWSI <- lm(ForbsBiomass ~ WSI * Species, data = dataS2)
modelForbsBiomassWSI
modelForbsBiomassWSI_summary <- summary(modelForbsBiomassWSI)
modelForbsBiomassWSI_summary

overall_r2ForbsBiomassWSI <- modelForbsBiomassWSI_summary$r.squared
overall_r2ForbsBiomassWSI

IS2<- ggplot(dataS2, aes(x = WSI, y = ForbsBiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # TotalBiomass label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
IS2 <- IS2 + theme(legend.position = "None")
IS2<-IS2+ annotate("text", 
                   x = 3.8, y = 40,    # Position (adjust based on your dataS2 range)
                   label = expression(italic(P)[int] == 0.01333), 
                   color = "black", 
                   size = 5)
IS2

#*6* ForbsBiomass VS NDSVI_S2 with species interaction****************** SIGNIFICNAT
modelForbsBiomassNDSVI_S2 <- lm(ForbsBiomass ~ NDSVI_S2 * Species, data = dataS2)
modelForbsBiomassNDSVI_S2
modelForbsBiomassNDSVI_S2_summary <- summary(modelForbsBiomassNDSVI_S2)
modelForbsBiomassNDSVI_S2_summary

overall_r2ForbsBiomassNDSVI_S2 <- modelForbsBiomassNDSVI_S2_summary$r.squared
overall_r2ForbsBiomassNDSVI_S2

JS2<- ggplot(dataS2, aes(x = NDSVI_S2, y = ForbsBiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # TotalBiomass label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
JS2 <- JS2 + theme(legend.position = "None")
JS2<-JS2+ annotate("text", 
                   x = 0.63, y = 40,    # Position (adjust based on your dataS2 range)
                   label = expression(italic(P)[int] == 0.00397), 
                   color = "black", 
                   size = 5)
JS2

#*7* ForbsBiomass VS WSI with species interaction****************** SIGNIFICNAT
modelForbsBiomassSATVI_S2 <- lm(ForbsBiomass ~ SATVI_S2 * Species, data = dataS2)
modelForbsBiomassSATVI_S2
modelForbsBiomassSATVI_S2_summary <- summary(modelForbsBiomassSATVI_S2)
modelForbsBiomassSATVI_S2_summary

overall_r2ForbsBiomassSATVI_S2 <- modelForbsBiomassSATVI_S2_summary$r.squared
overall_r2ForbsBiomassSATVI_S2

KS2<- ggplot(dataS2, aes(x = SATVI_S2, y = ForbsBiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # TotalBiomass label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
KS2 <- KS2 + theme(legend.position = "None")
KS2<-KS2+ annotate("text", 
                   x = 0.265, y = 40,    # Position (adjust based on your dataS2 range)
                   label = expression(italic(P)[int] == 0.00918), 
                   color = "black", 
                   size = 5)
KS2

#**********************************************************************#
#**********************************************************************#
##1 ForbsCover
#*1* ForbsCover VS CAI with species interaction     SIGNIFICANT
modelForbsCoverCAI <- lm(ForbsCover ~ CAI * Species, data = dataS2)
modelForbsCoverCAI
modelForbsCoverCAI_summary <- summary(modelForbsCoverCAI)
modelForbsCoverCAI_summary

overall_r2ForbsCoverCAI <- modelForbsCoverCAI_summary$r.squared
overall_r2ForbsCoverCAI

LS2<- ggplot(dataS2, aes(x = CAI, y = ForbsCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # TotalBiomass label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
LS2 <- LS2 + theme(legend.position = "None")
LS2<-LS2+ annotate("text", 
                   x = 0.57, y = 20,    # Position (adjust based on your dataS2 range)
                   label = expression(italic(P)[int] == 0.0299), 
                   color = "black", 
                   size = 5)
LS2

#*2* ForbsCover VS CRI with species interaction******************
modelForbsCoverCRI <- lm(ForbsCover ~ CRI * Species, data = dataS2)
modelForbsCoverCRI
modelForbsCoverCRI_summary <- summary(modelForbsCoverCRI)
modelForbsCoverCRI_summary

overall_r2ForbsCoverCRI <- modelForbsCoverCRI_summary$r.squared
overall_r2ForbsCoverCRI

#*3* ForbsCover VS NDVIS2 with species interaction******************
modelForbsCoverNDVIS2 <- lm(ForbsCover ~ NDVIS2 * Species, data = dataS2)
modelForbsCoverNDVIS2
modelForbsCoverNDVIS2_summary <- summary(modelForbsCoverNDVIS2)
modelForbsCoverNDVIS2_summary

overall_r2ForbsCoverNDVIS2 <- modelForbsCoverNDVIS2_summary$r.squared
overall_r2ForbsCoverNDVIS2

#*4* ForbsCover VS RGRS2 with species interaction******************
modelForbsCoverRGRS2 <- lm(ForbsCover ~ RGRS2 * Species, data = dataS2)
modelForbsCoverRGRS2
modelForbsCoverRGRS2_summary <- summary(modelForbsCoverRGRS2)
modelForbsCoverRGRS2_summary

overall_r2ForbsCoverRGRS2 <- modelForbsCoverRGRS2_summary$r.squared
overall_r2ForbsCoverRGRS2

#*5* ForbsCover VS WSI with species interaction****************** SIGNIFICANT
modelForbsCoverWSI <- lm(ForbsCover ~ WSI * Species, data = dataS2)
modelForbsCoverWSI
modelForbsCoverWSI_summary <- summary(modelForbsCoverWSI)
modelForbsCoverWSI_summary

overall_r2ForbsCoverWSI <- modelForbsCoverWSI_summary$r.squared
overall_r2ForbsCoverWSI

MS2<- ggplot(dataS2, aes(x = WSI, y = ForbsCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # TotalBiomass label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
MS2 <- MS2 + theme(legend.position = "right")
MS2<-MS2+ annotate("text", 
                   x = 3.8, y = 20,    # Position (adjust based on your dataS2 range)
                   label = expression(italic(P)[int] == 0.0287), 
                   color = "black", 
                   size = 5)
MS2

#*6* ForbsCover VS WSI with species interaction****************** SIGNIFICANT
modelForbsCoverNDSVI_S2 <- lm(ForbsCover ~ NDSVI_S2 * Species, data = dataS2)
modelForbsCoverNDSVI_S2
modelForbsCoverNDSVI_S2_summary <- summary(modelForbsCoverNDSVI_S2)
modelForbsCoverNDSVI_S2_summary

overall_r2ForbsCoverNDSVI_S2 <- modelForbsCoverNDSVI_S2_summary$r.squared
overall_r2ForbsCoverNDSVI_S2

NS2<- ggplot(dataS2, aes(x = NDSVI_S2, y = ForbsCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # TotalBiomass label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
NS2 <- NS2 + theme(legend.position = "None")
NS2<-NS2+ annotate("text", 
                   x = 0.63, y = 20,    # Position (adjust based on your dataS2 range)
                   label = expression(italic(P)[int] == 0.01130), 
                   color = "black", 
                   size = 5)
NS2

#*7* ForbsCover VS WSI with species interaction******************   SIGNIFICANT
modelForbsCoverSATVI_S2 <- lm(ForbsCover ~ SATVI_S2 * Species, data = dataS2)
modelForbsCoverSATVI_S2
modelForbsCoverSATVI_S2_summary <- summary(modelForbsCoverSATVI_S2)
modelForbsCoverSATVI_S2_summary

overall_r2ForbsCoverSATVI_S2 <- modelForbsCoverSATVI_S2_summary$r.squared
overall_r2ForbsCoverSATVI_S2

OS2<- ggplot(dataS2, aes(x = SATVI_S2, y = ForbsCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # TotalBiomass label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
OS2 <- OS2 + theme(legend.position = "right")
OS2<-OS2+ annotate("text", 
                   x = 0.265, y = 20,    # Position (adjust based on your dataS2 range)
                   label = expression(italic(P)[int] == 0.0339), 
                   color = "black", 
                   size = 5)
OS2

#**********************************************************************#
#**********************************************************************#
library(patchwork)
combinedS2 <- 
  (BS2 | DS2 ) /      # Row 1
  (GS2 |JS2 )  /
  (plot_spacer() + NS2 + plot_spacer() & theme(legend.position = "right") ) /
  # Row 2
  # Row 3 (centered R)plot_spacer()& theme(legend.position = "right")
  plot_layout(
    nrow = 2, 
    heights = c(1, 1), 
    guides = "collect"  # Shared legend
  )+ theme(
    # Global text size (affects titles, labels, legends)
    text = element_text(size = 14),  # Base size for all text
    
    # Axis titles
    axis.title = element_text(size = 16),  # X and Y axis labels
    
    # Axis tick labels
    axis.text = element_text(size = 14),   # Tick labels (numbers/categories)
    
    # Legend
    legend.title = element_text(size = 15),  # Legend title
    legend.text = element_text(size = 13),   # Legend items
    
    # Plot titles/subtitles (if any)
    plot.title = element_text(size = 18),    # Main title
    plot.subtitle = element_text(size = 16), # Subtitle
    legend.position = "right",
    legend.justification = "center"
  )

combinedS2


combinedS2 <- 
  (BS2 | DS2) /                # Row 1: 2 plots side-by-side
  ( GS2 + plot_spacer()) /  # Row 2: GS2 centered with spacers
  plot_layout(
    nrow = 2,
    heights = c(1, 1),         # Equal row heights
    widths = c(1, 5),       # Spacers (narrow) + GS2 (same width as BS2 & DS2)
    guides = "collect"
  ) + 
  theme(
    legend.position = "top",
    legend.justification = "center"
  )
combinedS2
#######################################################################
#*******************Spectral index calulation*************************#
#######################################################################
#***************************PlanetScope*******************************#
#######################################################################
#*****************************************************************************************************#
#                       1   Carotenoid Reflectance Index I     (Hill, 2013)                           #
#*****************************************************************************************************#
## C1 plots
RBB2PSC1 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C1" & SimulatedPS_data$Band == "B2"]
RBB4PSC1 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C1" & SimulatedPS_data$Band == "B4"]
CRIPSC1 <- (1/RBB2PSC1) - (1/RBB4PSC1)
CRIPSC1

## C2 plots
RBB2PSC2 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C2" & SimulatedPS_data$Band == "B2"]
RBB4PSC2 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C2" & SimulatedPS_data$Band == "B4"]
CRIPSC2 <- (1/RBB2PSC2) - (1/RBB4PSC2)
CRIPSC2

## C3 plots
RBB2PSC3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C3" & SimulatedPS_data$Band == "B2"]
RBB4PSC3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C3" & SimulatedPS_data$Band == "B4"]
CRIPSC3 <- (1/RBB2PSC3) - (1/RBB4PSC3)
CRIPSC3

## C4 plots
RBB2PSC4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C4" & SimulatedPS_data$Band == "B2"]
RBB4PSC4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C4" & SimulatedPS_data$Band == "B4"]
CRIPSC4 <- (1/RBB2PSC4) - (1/RBB4PSC4)
CRIPSC4

## C5 plots
RBB2PSC5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C5" & SimulatedPS_data$Band == "B2"]
RBB4PSC5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C5" & SimulatedPS_data$Band == "B4"]
CRIPSC5 <- (1/RBB2PSC5) - (1/RBB4PSC5)
CRIPSC5

## C6 plots
RBB2PSC6 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C6" & SimulatedPS_data$Band == "B2"]
RBB4PSC6 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C6" & SimulatedPS_data$Band == "B4"]
CRIPSC6 <- (1/RBB2PSC6) - (1/RBB4PSC6)
CRIPSC6

## C7 plots
RBB2PSC7 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C7" & SimulatedPS_data$Band == "B2"]
RBB4PSC7 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C7" & SimulatedPS_data$Band == "B4"]
CRIPSC7 <- (1/RBB2PSC7) - (1/RBB4PSC7)
CRIPSC7

## C11 plots
RBB2PSC11 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C11" & SimulatedPS_data$Band == "B2"]
RBB4PSC11 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C11" & SimulatedPS_data$Band == "B4"]
CRIPSC11 <- (1/RBB2PSC11) - (1/RBB4PSC11)
CRIPSC11

## C12 plots
RBB2PSC12 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C12" & SimulatedPS_data$Band == "B2"]
RBB4PSC12 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C12" & SimulatedPS_data$Band == "B4"]
CRIPSC12 <- (1/RBB2PSC12) - (1/RBB4PSC12)
CRIPSC12

## C13 plots
RBB2PSC13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C13" & SimulatedPS_data$Band == "B2"]
RBB4PSC13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C13" & SimulatedPS_data$Band == "B4"]
CRIPSC13 <- (1/RBB2PSC13) - (1/RBB4PSC13)
CRIPSC13

## C15 plots
RBB2PSC15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C15" & SimulatedPS_data$Band == "B2"]
RBB4PSC15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C15" & SimulatedPS_data$Band == "B4"]
CRIPSC15 <- (1/RBB2PSC15) - (1/RBB4PSC15)
CRIPSC15

## C19 plots
RBB2PSC19 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C19" & SimulatedPS_data$Band == "B2"]
RBB4PSC19 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C19" & SimulatedPS_data$Band == "B4"]
CRIPSC19 <- (1/RBB2PSC19) - (1/RBB4PSC19)
CRIPSC19

## C20 plots
RBB2PSC20 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C20" & SimulatedPS_data$Band == "B2"]
RBB4PSC20 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C20" & SimulatedPS_data$Band == "B4"]
CRIPSC20 <- (1/RBB2PSC20) - (1/RBB4PSC20)
CRIPSC20

## N3 plots
RBB2PSN3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N3" & SimulatedPS_data$Band == "B2"]
RBB4PSN3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N3" & SimulatedPS_data$Band == "B4"]
CRIPSN3 <- (1/RBB2PSN3) - (1/RBB4PSN3)
CRIPSN3

## N4 plots
RBB2PSN4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N4" & SimulatedPS_data$Band == "B2"]
RBB4PSN4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N4" & SimulatedPS_data$Band == "B4"]
CRIPSN4 <- (1/RBB2PSN4) - (1/RBB4PSN4)
CRIPSN4

## N5 plots
RBB2PSN5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N5" & SimulatedPS_data$Band == "B2"]
RBB4PSN5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N5" & SimulatedPS_data$Band == "B4"]
CRIPSN5 <- (1/RBB2PSN5) - (1/RBB4PSN5)
CRIPSN5

## N10 plots
RBB2PSN10 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N10" & SimulatedPS_data$Band == "B2"]
RBB4PSN10 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N10" & SimulatedPS_data$Band == "B4"]
CRIPSN10 <- (1/RBB2PSN10) - (1/RBB4PSN10)
CRIPSN10

## N13 plots
RBB2PSN13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N13" & SimulatedPS_data$Band == "B2"]
RBB4PSN13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N13" & SimulatedPS_data$Band == "B4"]
CRIPSN13 <- (1/RBB2PSN13) - (1/RBB4PSN13)
CRIPSN13

## N14 plots
RBB2PSN14 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N14" & SimulatedPS_data$Band == "B2"]
RBB4PSN14 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N14" & SimulatedPS_data$Band == "B4"]
CRIPSN14 <- (1/RBB2PSN14) - (1/RBB4PSN14)
CRIPSN14

## N15 plots
RBB2PSN15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N15" & SimulatedPS_data$Band == "B2"]
RBB4PSN15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N15" & SimulatedPS_data$Band == "B4"]
CRIPSN15 <- (1/RBB2PSN15) - (1/RBB4PSN15)
CRIPSN15

## N16 plots
RBB2PSN16 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N16" & SimulatedPS_data$Band == "B2"]
RBB4PSN16 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N16" & SimulatedPS_data$Band == "B4"]
CRIPSN16 <- (1/RBB2PSN16) - (1/RBB4PSN16)
CRIPSN16

#########################################
###########Combine and Export############
#########################################
CRI_PS_results <- CRI_510_550 <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20","N3","N4","N5","N10","N13","N14","N15","N16"),
  CRI = c(CRIPSC1, CRIPSC2, CRIPSC3, CRIPSC4,
          CRIPSC5, CRIPSC6, CRIPSC7,
          CRIPSC11, CRIPSC12, CRIPSC13, CRIPSC15, 
          CRIPSC19, CRIPSC20, CRIPSN3, 
          CRIPSN4, CRIPSN5, CRIPSN10,
          CRIPSN13, CRIPSN14, CRIPSN15,
          CRIPSN16))

CRI_PS_results
#View(CRI_PS_results)

#Export the file
#write.table(CRI_PS_results, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/Sentinel/CRI_PS_results.csv", sep = ",", row.names = FALSE)


#*****************************************************************************************************#
#2 (Normalized Difference Turbidity Index (NDTI)) ///Normalized Differenced Vegetation indices (NDVI) (682-553) ((A. Gitelson & Merzlyak, 1994rudel, Thierry
#Fabre, Sophie, Houet, Thomas, Mazier, Florence, Briottet, Xavier# Opposite to traditional NDVI
#*****************************************************************************************************#
## C1 plots
## NDVI(682/553) (Number in the VI list: 1)
RB4C1 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C1" & SimulatedPS_data$Band == "B4"]
RB6C1 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C1" & SimulatedPS_data$Band == "B6"]
NDVIPS_C1 <- (RB6C1-RB4C1) / (RB6C1+RB4C1)
NDVIPS_C1

# C2
RB4C2 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C2" & SimulatedPS_data$Band == "B4"]
RB6C2 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C2" & SimulatedPS_data$Band == "B6"]
NDVIPS_C2 <- (RB6C2 - RB4C2) / (RB6C2 + RB4C2)
NDVIPS_C2

# C3
RB4C3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C3" & SimulatedPS_data$Band == "B4"]
RB6C3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C3" & SimulatedPS_data$Band == "B6"]
NDVIPS_C3 <- (RB6C3 - RB4C3) / (RB6C3 + RB4C3)
NDVIPS_C3

# C4
RB4C4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C4" & SimulatedPS_data$Band == "B4"]
RB6C4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C4" & SimulatedPS_data$Band == "B6"]
NDVIPS_C4 <- (RB6C4 - RB4C4) / (RB6C4 + RB4C4)
NDVIPS_C4

# C5
RB4C5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C5" & SimulatedPS_data$Band == "B4"]
RB6C5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C5" & SimulatedPS_data$Band == "B6"]
NDVIPS_C5 <- (RB6C5 - RB4C5) / (RB6C5 + RB4C5)
NDVIPS_C5

# C6
RB4C6 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C6" & SimulatedPS_data$Band == "B4"]
RB6C6 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C6" & SimulatedPS_data$Band == "B6"]
NDVIPS_C6 <- (RB6C6 - RB4C6) / (RB6C6 + RB4C6)
NDVIPS_C6

# C7
RB4C7 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C7" & SimulatedPS_data$Band == "B4"]
RB6C7 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C7" & SimulatedPS_data$Band == "B6"]
NDVIPS_C7 <- (RB6C7 - RB4C7) / (RB6C7 + RB4C7)
NDVIPS_C7

# C11
RB4C11 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C11" & SimulatedPS_data$Band == "B4"]
RB6C11 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C11" & SimulatedPS_data$Band == "B6"]
NDVIPS_C11 <- (RB6C11 - RB4C11) / (RB6C11 + RB4C11)
NDVIPS_C11

# C12
RB4C12 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C12" & SimulatedPS_data$Band == "B4"]
RB6C12 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C12" & SimulatedPS_data$Band == "B6"]
NDVIPS_C12 <- (RB6C12 - RB4C12) / (RB6C12 + RB4C12)
NDVIPS_C12

# C13
RB4C13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C13" & SimulatedPS_data$Band == "B4"]
RB6C13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C13" & SimulatedPS_data$Band == "B6"]
NDVIPS_C13 <- (RB6C13 - RB4C13) / (RB6C13 + RB4C13)
NDVIPS_C13

# C15
RB4C15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C15" & SimulatedPS_data$Band == "B4"]
RB6C15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C15" & SimulatedPS_data$Band == "B6"]
NDVIPS_C15 <- (RB6C15 - RB4C15) / (RB6C15 + RB4C15)
NDVIPS_C15

# C19
RB4C19 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C19" & SimulatedPS_data$Band == "B4"]
RB6C19 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C19" & SimulatedPS_data$Band == "B6"]
NDVIPS_C19 <- (RB6C19 - RB4C19) / (RB6C19 + RB4C19)
NDVIPS_C19

# C20
RB4C20 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C20" & SimulatedPS_data$Band == "B4"]
RB6C20 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C20" & SimulatedPS_data$Band == "B6"]
NDVIPS_C20 <- (RB6C20 - RB4C20) / (RB6C20 + RB4C20)
NDVIPS_C20

# N3
RB4N3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N3" & SimulatedPS_data$Band == "B4"]
RB6N3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N3" & SimulatedPS_data$Band == "B6"]
NDVIPS_N3 <- (RB6N3 - RB4N3) / (RB6N3 + RB4N3)
NDVIPS_N3

# N4
RB4N4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N4" & SimulatedPS_data$Band == "B4"]
RB6N4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N4" & SimulatedPS_data$Band == "B6"]
NDVIPS_N4 <- (RB6N4 - RB4N4) / (RB6N4 + RB4N4)
NDVIPS_N4

# N5
RB4N5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N5" & SimulatedPS_data$Band == "B4"]
RB6N5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N5" & SimulatedPS_data$Band == "B6"]
NDVIPS_N5 <- (RB6N5 - RB4N5) / (RB6N5 + RB4N5)
NDVIPS_N5

# N10
RB4N10 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N10" & SimulatedPS_data$Band == "B4"]
RB6N10 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N10" & SimulatedPS_data$Band == "B6"]
NDVIPS_N10 <- (RB6N10 - RB4N10) / (RB6N10 + RB4N10)
NDVIPS_N10

# N13
RB4N13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N13" & SimulatedPS_data$Band == "B4"]
RB6N13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N13" & SimulatedPS_data$Band == "B6"]
NDVIPS_N13 <- (RB6N13 - RB4N13) / (RB6N13 + RB4N13)
NDVIPS_N13

# N14
RB4N14 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N14" & SimulatedPS_data$Band == "B4"]
RB6N14 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N14" & SimulatedPS_data$Band == "B6"]
NDVIPS_N14 <- (RB6N14 - RB4N14) / (RB6N14 + RB4N14)
NDVIPS_N14

# N15
RB4N15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N15" & SimulatedPS_data$Band == "B4"]
RB6N15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N15" & SimulatedPS_data$Band == "B6"]
NDVIPS_N15 <- (RB6N15 - RB4N15) / (RB6N15 + RB4N15)
NDVIPS_N15

# N16
RB4N16 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N16" & SimulatedPS_data$Band == "B4"]
RB6N16 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N16" & SimulatedPS_data$Band == "B6"]
NDVIPS_N16 <- (RB6N16 - RB4N16) / (RB6N16 + RB4N16)
NDVIPS_N16

#########################################
###########Combine and Export############
#########################################
NDVI_PS_results <- NDVI_682_553 <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20","N3","N4","N5","N10","N13","N14","N15","N16"),
  NDVIPS = c(NDVIPS_C1, NDVIPS_C2, NDVIPS_C3, NDVIPS_C4,
             NDVIPS_C5, NDVIPS_C6, NDVIPS_C7,
             NDVIPS_C11, NDVIPS_C12, NDVIPS_C13, NDVIPS_C15, 
             NDVIPS_C19, NDVIPS_C20, NDVIPS_N3, 
             NDVIPS_N4, NDVIPS_N5, NDVIPS_N10,
             NDVIPS_N13, NDVIPS_N14, NDVIPS_N15,
             NDVIPS_N16))

#View(NDVI_PS_results)


#Export the file
#write.table(NDVI_PS_results, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/Sentinel/NDVI_PS_results.csv", sep = ",", row.names = FALSE)


#*****************************************************************************************************#
# 3                                     RGR (Red Green Ratio) 
#*****************************************************************************************************#
## C1 plots
## 
RB4C1 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C1" & SimulatedPS_data$Band == "B4"]
RB6C1 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C1" & SimulatedPS_data$Band == "B6"]
RGRPS_C1 <- RB6C1 / RB4C1
RGRPS_C1

# C2
RB4C2 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C2" & SimulatedPS_data$Band == "B4"]
RB6C2 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C2" & SimulatedPS_data$Band == "B6"]
RGRPS_C2 <- RB6C2 / RB4C2
RGRPS_C2

# C3
RB4C3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C3" & SimulatedPS_data$Band == "B4"]
RB6C3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C3" & SimulatedPS_data$Band == "B6"]
RGRPS_C3 <- RB6C3 / RB4C3
RGRPS_C3

# C4
RB4C4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C4" & SimulatedPS_data$Band == "B4"]
RB6C4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C4" & SimulatedPS_data$Band == "B6"]
RGRPS_C4 <- RB6C4 / RB4C4
RGRPS_C4

# C5
RB4C5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C5" & SimulatedPS_data$Band == "B4"]
RB6C5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C5" & SimulatedPS_data$Band == "B6"]
RGRPS_C5 <- RB6C5 / RB4C5
RGRPS_C5

# C6
RB4C6 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C6" & SimulatedPS_data$Band == "B4"]
RB6C6 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C6" & SimulatedPS_data$Band == "B6"]
RGRPS_C6 <- RB6C6 / RB4C6
RGRPS_C6

# C7
RB4C7 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C7" & SimulatedPS_data$Band == "B4"]
RB6C7 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C7" & SimulatedPS_data$Band == "B6"]
RGRPS_C7 <- RB6C7 / RB4C7
RGRPS_C7

# C11
RB4C11 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C11" & SimulatedPS_data$Band == "B4"]
RB6C11 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C11" & SimulatedPS_data$Band == "B6"]
RGRPS_C11 <- RB6C11 / RB4C11
RGRPS_C11

# C12
RB4C12 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C12" & SimulatedPS_data$Band == "B4"]
RB6C12 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C12" & SimulatedPS_data$Band == "B6"]
RGRPS_C12 <- RB6C12 / RB4C12
RGRPS_C12

# C13
RB4C13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C13" & SimulatedPS_data$Band == "B4"]
RB6C13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C13" & SimulatedPS_data$Band == "B6"]
RGRPS_C13 <- RB6C13 / RB4C13
RGRPS_C13

# C15
RB4C15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C15" & SimulatedPS_data$Band == "B4"]
RB6C15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C15" & SimulatedPS_data$Band == "B6"]
RGRPS_C15 <- RB6C15 / RB4C15
RGRPS_C15

# C19
RB4C19 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C19" & SimulatedPS_data$Band == "B4"]
RB6C19 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C19" & SimulatedPS_data$Band == "B6"]
RGRPS_C19 <- RB6C19 / RB4C19
RGRPS_C19

# C20
RB4C20 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C20" & SimulatedPS_data$Band == "B4"]
RB6C20 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C20" & SimulatedPS_data$Band == "B6"]
RGRPS_C20 <- RB6C20 / RB4C20
RGRPS_C20

# N3
RB4N3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N3" & SimulatedPS_data$Band == "B4"]
RB6N3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N3" & SimulatedPS_data$Band == "B6"]
RGRPS_N3 <- RB6N3 / RB4N3
RGRPS_N3

# N4
RB4N4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N4" & SimulatedPS_data$Band == "B4"]
RB6N4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N4" & SimulatedPS_data$Band == "B6"]
RGRPS_N4 <- RB6N4 / RB4N4
RGRPS_N4

# N5
RB4N5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N5" & SimulatedPS_data$Band == "B4"]
RB6N5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N5" & SimulatedPS_data$Band == "B6"]
RGRPS_N5 <- RB6N5 / RB4N5
RGRPS_N5

# N10
RB4N10 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N10" & SimulatedPS_data$Band == "B4"]
RB6N10 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N10" & SimulatedPS_data$Band == "B6"]
RGRPS_N10 <- RB6N10 / RB4N10
RGRPS_N10

# N13
RB4N13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N13" & SimulatedPS_data$Band == "B4"]
RB6N13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N13" & SimulatedPS_data$Band == "B6"]
RGRPS_N13 <- RB6N13 / RB4N13
RGRPS_N13

# N14
RB4N14 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N14" & SimulatedPS_data$Band == "B4"]
RB6N14 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N14" & SimulatedPS_data$Band == "B6"]
RGRPS_N14 <- RB6N14 / RB4N14
RGRPS_N14

# N15
RB4N15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N15" & SimulatedPS_data$Band == "B4"]
RB6N15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N15" & SimulatedPS_data$Band == "B6"]
RGRPS_N15 <- RB6N15 / RB4N15
RGRPS_N15

# N16
RB4N16 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N16" & SimulatedPS_data$Band == "B4"]
RB6N16 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N16" & SimulatedPS_data$Band == "B6"]
RGRPS_N16 <- RB6N16 / RB4N16
RGRPS_N16

#########################################
###########Combine and Export############
#########################################
RGR_PS_results <- RGR <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20","N3","N4","N5","N10","N13","N14","N15","N16"),
  RGRPS = c(RGRPS_C1, RGRPS_C2, RGRPS_C3, RGRPS_C4,
            RGRPS_C5, RGRPS_C6, RGRPS_C7,
            RGRPS_C11, RGRPS_C12, RGRPS_C13, RGRPS_C15, 
            RGRPS_C19, RGRPS_C20, RGRPS_N3,
            RGRPS_N4, RGRPS_N5, RGRPS_N10,
            RGRPS_N13, RGRPS_N14, RGRPS_N15,
            RGRPS_N16))

#View(RGR_PS_results)

#Export the file
#write.table(RGR_PS_results, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/Sentinel/RGR_PS_results.csv", sep = ",", row.names = FALSE)


#Export the file
#write.table(RGR_S2_results, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/Sentinel/RGR_S2_results.csv", sep = ",", row.names = FALSE)


#*****************************************************************************************************#
#4            YI (Yellowness Index ) (Adams, M. L., Philpot, W. D., & Norvell, W. A. (1999). Yellowness index: An application of spectral second derivatives to estimate chlorosis of leaves in stressed vegetation. International Journal of Remote Sensing, 20(18), 3663–3675. https://doi.org/10.1080/014311699211264.                                 #
#*****************************************************************************************************#                     
## C1 plots
# Extract reflectance values for site "C1" at specific Bands
RB4_C1 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C1" & SimulatedPS_data$Band == "B4"]
RB5_C1 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C1" & SimulatedPS_data$Band == "B5"]
RB6_C1 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C1" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_C1 <- (RB4_C1 - 2*RB5_C1 + RB6_C1) / 0.045
YIPS_C1

## C2 plot
RB4_C2 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C2" & SimulatedPS_data$Band == "B4"]
RB5_C2 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C2" & SimulatedPS_data$Band == "B5"]
RB6_C2 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C2" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_C2 <- (RB4_C2 - 2*RB5_C2 + RB6_C2) / 0.045
YIPS_C2

## C3 plot
RB4_C3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C3" & SimulatedPS_data$Band == "B4"]
RB5_C3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C3" & SimulatedPS_data$Band == "B5"]
RB6_C3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C3" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_C3 <- (RB4_C3 - 2*RB5_C3 + RB6_C3) / 0.045
YIPS_C3

## C4 plot
RB4_C4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C4" & SimulatedPS_data$Band == "B4"]
RB5_C4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C4" & SimulatedPS_data$Band == "B5"]
RB6_C4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C4" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_C4 <- (RB4_C4 - 2*RB5_C4 + RB6_C4) / 0.045
YIPS_C4

## C5 plot
RB4_C5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C5" & SimulatedPS_data$Band == "B4"]
RB5_C5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C5" & SimulatedPS_data$Band == "B5"]
RB6_C5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C5" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_C5 <- (RB4_C5 - 2*RB5_C5 + RB6_C5) / 0.045
YIPS_C5

## C6 plot
RB4_C6 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C6" & SimulatedPS_data$Band == "B4"]
RB5_C6 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C6" & SimulatedPS_data$Band == "B5"]
RB6_C6 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C6" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_C6 <- (RB4_C6 - 2*RB5_C6 + RB6_C6) / 0.045
YIPS_C6

## C7 plot
RB4_C7 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C7" & SimulatedPS_data$Band == "B4"]
RB5_C7 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C7" & SimulatedPS_data$Band == "B5"]
RB6_C7 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C7" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_C7 <- (RB4_C7 - 2*RB5_C7 + RB6_C7) / 0.045
YIPS_C7

## C11 plot
RB4_C11 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C11" & SimulatedPS_data$Band == "B4"]
RB5_C11 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C11" & SimulatedPS_data$Band == "B5"]
RB6_C11 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C11" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_C11 <- (RB4_C11 - 2*RB5_C11 + RB6_C11) / 0.045
YIPS_C11

## C12 plot
RB4_C12 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C12" & SimulatedPS_data$Band == "B4"]
RB5_C12 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C12" & SimulatedPS_data$Band == "B5"]
RB6_C12 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C12" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_C12 <- (RB4_C12 - 2*RB5_C12 + RB6_C12) / 0.045
YIPS_C12

## C13 plot
RB4_C13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C13" & SimulatedPS_data$Band == "B4"]
RB5_C13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C13" & SimulatedPS_data$Band == "B5"]
RB6_C13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C13" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_C13 <- (RB4_C13 - 2*RB5_C13 + RB6_C13) / 0.045
YIPS_C13

## C15 plot
RB4_C15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C15" & SimulatedPS_data$Band == "B4"]
RB5_C15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C15" & SimulatedPS_data$Band == "B5"]
RB6_C15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C15" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_C15 <- (RB4_C15 - 2*RB5_C15 + RB6_C15) / 0.045
YIPS_C15

## C19 plot
RB4_C19 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C19" & SimulatedPS_data$Band == "B4"]
RB5_C19 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C19" & SimulatedPS_data$Band == "B5"]
RB6_C19 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C19" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_C19 <- (RB4_C19 - 2*RB5_C19 + RB6_C19) / 0.045
YIPS_C19

## C20 plot
RB4_C20 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C20" & SimulatedPS_data$Band == "B4"]
RB5_C20 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C20" & SimulatedPS_data$Band == "B5"]
RB6_C20 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C20" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_C20 <- (RB4_C20 - 2*RB5_C20 + RB6_C20) / 0.045
YIPS_C20

## N3 plot
RB4_N3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N3" & SimulatedPS_data$Band == "B4"]
RB5_N3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N3" & SimulatedPS_data$Band == "B5"]
RB6_N3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N3" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_N3 <- (RB4_N3 - 2*RB5_N3 + RB6_N3) / 0.045
YIPS_N3

## N4 plot
RB4_N4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N4" & SimulatedPS_data$Band == "B4"]
RB5_N4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N4" & SimulatedPS_data$Band == "B5"]
RB6_N4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N4" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_N4 <- (RB4_N4 - 2*RB5_N4 + RB6_N4) / 0.045
YIPS_N4

## N5 plot
RB4_N5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N5" & SimulatedPS_data$Band == "B4"]
RB5_N5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N5" & SimulatedPS_data$Band == "B5"]
RB6_N5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N5" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_N5 <- (RB4_N5 - 2*RB5_N5 + RB6_N5) / 0.045
YIPS_N5

## N10 plot
RB4_N10 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N10" & SimulatedPS_data$Band == "B4"]
RB5_N10 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N10" & SimulatedPS_data$Band == "B5"]
RB6_N10 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N10" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_N10 <- (RB4_N10 - 2*RB5_N10 + RB6_N10) / 0.045
YIPS_N10

## N13 plot
RB4_N13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N13" & SimulatedPS_data$Band == "B4"]
RB5_N13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N13" & SimulatedPS_data$Band == "B5"]
RB6_N13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N13" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_N13 <- (RB4_N13 - 2*RB5_N13 + RB6_N13) / 0.045
YIPS_N13

## N14 plot
RB4_N14 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N14" & SimulatedPS_data$Band == "B4"]
RB5_N14 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N14" & SimulatedPS_data$Band == "B5"]
RB6_N14 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N14" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_N14 <- (RB4_N14 - 2*RB5_N14 + RB6_N14) / 0.045
YIPS_N14

## N15 plot
RB4_N15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N15" & SimulatedPS_data$Band == "B4"]
RB5_N15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N15" & SimulatedPS_data$Band == "B5"]
RB6_N15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N15" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_N15 <- (RB4_N15 - 2*RB5_N15 + RB6_N15) / 0.045
YIPS_N15

## N16 plot
RB4_N16 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N16" & SimulatedPS_data$Band == "B4"]
RB5_N16 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N16" & SimulatedPS_data$Band == "B5"]
RB6_N16 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N16" & SimulatedPS_data$Band == "B6"]

# Calculate the spectral index
YIPS_N16 <- (RB4_N16 - 2*RB5_N16 + RB6_N16) / 0.045
YIPS_N16        

library(tidyverse)
#########################################
########### Combine Results #############
#########################################
YIPS_results <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20", "N3", "N4", "N5", "N10", "N13", "N14", "N15", "N16"),
  YIPS = c(YIPS_C1, YIPS_C2, YIPS_C3, YIPS_C4, YIPS_C5, YIPS_C6, YIPS_C7,
           YIPS_C11, YIPS_C12, YIPS_C13, YIPS_C15, YIPS_C19, YIPS_C20,
           YIPS_N3, YIPS_N4, YIPS_N5, YIPS_N10, YIPS_N13, YIPS_N14, YIPS_N15, YIPS_N16))




#*****************************************************************************************************#
#5           Photochemical reflectance index (PRI) (550,670, 700) (Daughtry, 2000)     
#*****************************************************************************************************#
## C1 plots
RB3C1 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C1" & SimulatedPS_data$Band == "B3"]
RB4C1 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C1" & SimulatedPS_data$Band == "B4"]
PRIPS_C1 <- (RB3C1 - RB4C1) / (RB3C1 + RB4C1)
PRIPS_C1

## C2 plots
RB3C2 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C2" & SimulatedPS_data$Band == "B3"]
RB4C2 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C2" & SimulatedPS_data$Band == "B4"]
PRIPS_C2 <- (RB3C2 - RB4C2) / (RB3C2 + RB4C2)
PRIPS_C2

## C3 plots
RB3C3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C3" & SimulatedPS_data$Band == "B3"]
RB4C3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C3" & SimulatedPS_data$Band == "B4"]
PRIPS_C3 <- (RB3C3 - RB4C3) / (RB3C3 + RB4C3)
PRIPS_C3

## C4 plots
RB3C4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C4" & SimulatedPS_data$Band == "B3"]
RB4C4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C4" & SimulatedPS_data$Band == "B4"]
PRIPS_C4 <- (RB3C4 - RB4C4) / (RB3C4 + RB4C4)
PRIPS_C4

## C5 plots
RB3C5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C5" & SimulatedPS_data$Band == "B3"]
RB4C5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C5" & SimulatedPS_data$Band == "B4"]
PRIPS_C5 <- (RB3C5 - RB4C5) / (RB3C5 + RB4C5)
PRIPS_C5

## C6 plots
RB3C6 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C6" & SimulatedPS_data$Band == "B3"]
RB4C6 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C6" & SimulatedPS_data$Band == "B4"]
PRIPS_C6 <- (RB3C6 - RB4C6) / (RB3C6 + RB4C6)
PRIPS_C6

## C7 plots
RB3C7 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C7" & SimulatedPS_data$Band == "B3"]
RB4C7 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C7" & SimulatedPS_data$Band == "B4"]
PRIPS_C7 <- (RB3C7 - RB4C7) / (RB3C7 + RB4C7)
PRIPS_C7

## C11 plots
RB3C11 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C11" & SimulatedPS_data$Band == "B3"]
RB4C11 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C11" & SimulatedPS_data$Band == "B4"]
PRIPS_C11 <- (RB3C11 - RB4C11) / (RB3C11 + RB4C11)
PRIPS_C11

## C12 plots
RB3C12 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C12" & SimulatedPS_data$Band == "B3"]
RB4C12 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C12" & SimulatedPS_data$Band == "B4"]
PRIPS_C12 <- (RB3C12 - RB4C12) / (RB3C12 + RB4C12)
PRIPS_C12

## C13 plots
RB3C13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C13" & SimulatedPS_data$Band == "B3"]
RB4C13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C13" & SimulatedPS_data$Band == "B4"]
PRIPS_C13 <- (RB3C13 - RB4C13) / (RB3C13 + RB4C13)
PRIPS_C13

## C15 plots
RB3C15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C15" & SimulatedPS_data$Band == "B3"]
RB4C15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C15" & SimulatedPS_data$Band == "B4"]
PRIPS_C15 <- (RB3C15 - RB4C15) / (RB3C15 + RB4C15)
PRIPS_C15

## C19 plots
RB3C19 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C19" & SimulatedPS_data$Band == "B3"]
RB4C19 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C19" & SimulatedPS_data$Band == "B4"]
PRIPS_C19 <- (RB3C19 - RB4C19) / (RB3C19 + RB4C19)
PRIPS_C19

## C20 plots
RB3C20 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C20" & SimulatedPS_data$Band == "B3"]
RB4C20 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "C20" & SimulatedPS_data$Band == "B4"]
PRIPS_C20 <- (RB3C20 - RB4C20) / (RB3C20 + RB4C20)
PRIPS_C20

## N3 plots
RB3N3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N3" & SimulatedPS_data$Band == "B3"]
RB4N3 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N3" & SimulatedPS_data$Band == "B4"]
PRIPS_N3 <- (RB3N3 - RB4N3) / (RB3N3 + RB4N3)
PRIPS_N3

## N4 plots
RB3N4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N4" & SimulatedPS_data$Band == "B3"]
RB4N4 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N4" & SimulatedPS_data$Band == "B4"]
PRIPS_N4 <- (RB3N4 - RB4N4) / (RB3N4 + RB4N4)
PRIPS_N4

## N5 plots
RB3N5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N5" & SimulatedPS_data$Band == "B3"]
RB4N5 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N5" & SimulatedPS_data$Band == "B4"]
PRIPS_N5 <- (RB3N5 - RB4N5) / (RB3N5 + RB4N5)
PRIPS_N5

## N10 plots
RB3N10 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N10" & SimulatedPS_data$Band == "B3"]
RB4N10 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N10" & SimulatedPS_data$Band == "B4"]
PRIPS_N10 <- (RB3N10 - RB4N10) / (RB3N10 + RB4N10)
PRIPS_N10

## N13 plots
RB3N13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N13" & SimulatedPS_data$Band == "B3"]
RB4N13 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N13" & SimulatedPS_data$Band == "B4"]
PRIPS_N13 <- (RB3N13 - RB4N13) / (RB3N13 + RB4N13)
PRIPS_N13

## N14 plots
RB3N14 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N14" & SimulatedPS_data$Band == "B3"]
RB4N14 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N14" & SimulatedPS_data$Band == "B4"]
PRIPS_N14 <- (RB3N14 - RB4N14) / (RB3N14 + RB4N14)
PRIPS_N14

## N15 plots
RB3N15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N15" & SimulatedPS_data$Band == "B3"]
RB4N15 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N15" & SimulatedPS_data$Band == "B4"]
PRIPS_N15 <- (RB3N15 - RB4N15) / (RB3N15 + RB4N15)
PRIPS_N15

## N16 plots
RB3N16 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N16" & SimulatedPS_data$Band == "B3"]
RB4N16 <- SimulatedPS_data$Reflectance[SimulatedPS_data$Site == "N16" & SimulatedPS_data$Band == "B4"]
PRIPS_N16 <- (RB3N16 - RB4N16) / (RB3N16 + RB4N16)
PRIPS_N16

#########################################
###########Combine and Export############
#########################################
PRIPS_results <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20", "N3", "N4", "N5", "N10", "N13", "N14", "N15", "N16"),
  PRIPS = c(PRIPS_C1, PRIPS_C2, PRIPS_C3, PRIPS_C4, PRIPS_C5, PRIPS_C6, PRIPS_C7,
            PRIPS_C11, PRIPS_C12, PRIPS_C13, PRIPS_C15, PRIPS_C19, PRIPS_C20,
            PRIPS_N3, PRIPS_N4, PRIPS_N5, PRIPS_N10, PRIPS_N13, PRIPS_N14, PRIPS_N15, PRIPS_N16))

#View(PRIPS_results)

#write.table(PRIPS_results, "D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/Vegetation indices/PRIPS_results.csv", sep = ",", row.names = FALSE)


#####################################################
######### Combine all these spectral indices datasets
######################################################
# List all your loaded data frames (replace with your actual object names)
SI_PS_list <- list(PRIPS_results,YIPS_results, RGR_PS_results, NDVI_PS_results, CRI_PS_results)

# Merge all by "Plot" (keeps all rows and fills missing values with NA)
combined_dataPS <- reduce(SI_PS_list, ~ full_join(.x, .y, by = "Plot"))


######################################################
#*************Linear model with interaction test******#
######################################################
#setwd("D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data")
setwd("C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data")
#setwd("C:/Users/Mohammed/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data")

Biophy <- read_csv("PlotWiseDescriptiveStat.csv")
Biophy$Plot <- Biophy$Site
data_listPS <- list(Biophy, combined_dataPS)

# Merge all by "Plot" (keeps all rows and fills missing values with NA)
dataPS <- reduce(data_listPS, ~ full_join(.x, .y, by = "Plot"))
dataPS <- dataPS[, -c(7, 8, 11,12, 14, 15, 18)]
#View(dataPS)
names(dataPS)
#####################################################
#####################################################
##1 Height
#*1* Height VS PRIPS with species interaction
modelHeightPRIPS <- lm(Height ~ PRIPS * Species, data = dataPS)
modelHeightPRIPS
modelHeightPRIPS_summary <- summary(modelHeightPRIPS)
modelHeightPRIPS_summary

overall_r2HeightPRIPS <- modelHeightPRIPS_summary$r.squared
overall_r2HeightPRIPS

#*2* Height VS YI with species interaction
modelHeightYIPS <- lm(Height ~ YIPS * Species, data = dataPS)
modelHeightYIPS
modelHeightYIPS_summary <- summary(modelHeightYIPS)
modelHeightYIPS_summary

overall_r2HeightYIPS <- modelHeightYIPS_summary$r.squared
overall_r2HeightYIPS

#*3* Height VS RGRPS with species interaction
modelHeightRGRPS <- lm(Height ~ RGRPS * Species, data = dataPS)
modelHeightRGRPS
modelHeightRGRPS_summary <- summary(modelHeightRGRPS)
modelHeightRGRPS_summary

overall_r2HeightRGRPS <- modelHeightRGRPS_summary$r.squared
overall_r2HeightRGRPS

#*4* Height VS NDVIPS with species interaction
modelHeightNDVIPS <- lm(Height ~ NDVIPS * Species, data = dataPS)
modelHeightNDVIPS
modelHeightNDVIPS_summary <- summary(modelHeightNDVIPS)
modelHeightNDVIPS_summary

overall_r2HeightNDVIPS <- modelHeightNDVIPS_summary$r.squared
overall_r2HeightNDVIPS

#*5* Height VS CRI with species interaction
modelHeightCRI <- lm(Height ~ CRI * Species, data = dataPS)
modelHeightCRI
modelHeightCRI_summary <- summary(modelHeightCRI)
modelHeightCRI_summary

overall_r2HeightCRI <- modelHeightCRI_summary$r.squared
overall_r2HeightCRI


#**********************************************************************#
#**********************************************************************#
##2 Cover
#*1* Cover VS PRIPS with species interaction   SIGNIFICANT
modelCoverPRIPS <- lm(Cover ~ PRIPS * Species, data = dataPS)
modelCoverPRIPS
modelCoverPRIPS_summary <- summary(modelCoverPRIPS)
modelCoverPRIPS_summary

overall_r2CoverPRIPS <- modelCoverPRIPS_summary$r.squared
overall_r2CoverPRIPS

APS<- ggplot(dataPS, aes(x = PRIPS, y = Cover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Photochemical Reflectance Index PlanetScope(PRIPS)",  # Replace with your desired title
    y = "Grass Cover (%)"# Replace with your desired title
  )+
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 18),  # CAI label size
    axis.title.y = element_text(size = 18),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 16),   # x-axis numbers
    axis.text.y = element_text(size = 16)    # y-axis numbers
  )
APS<- APS + theme(legend.position = "none")
APS

library(emmeans)
library(performance)
# Calculate species-specific slopes
emtrends(modelCoverPRIPS, ~ Species, var = "PRIPS")

# Compute R² for each species subset
r2_valuesPRIPS <- dataPS %>%
  group_by(Species) %>%
  summarise(
    r_squared = r2(lm(Cover ~ PRIPS))$R2
  )
r2_valuesPRIPS

#*2* Cover VS YIPS with species interaction  
modelCoverYIPS <- lm(Cover ~ YIPS * Species, data = dataPS)  
modelCoverYIPS  
modelCoverYIPS_summary <- summary(modelCoverYIPS)  
modelCoverYIPS_summary  

overall_r2CoverYIPS <- modelCoverYIPS_summary$r.squared  
overall_r2CoverYIPS  

#*3* Cover VS RGRPS with species interaction  
modelCoverRGRPS <- lm(Cover ~ RGRPS * Species, data = dataPS)  
modelCoverRGRPS  
modelCoverRGRPS_summary <- summary(modelCoverRGRPS)  
modelCoverRGRPS_summary  

overall_r2CoverRGRPS <- modelCoverRGRPS_summary$r.squared  
overall_r2CoverRGRPS  

#*4* Cover VS NDVIPS with species interaction  
modelCoverNDVIPS <- lm(Cover ~ NDVIPS * Species, data = dataPS)  
modelCoverNDVIPS  
modelCoverNDVIPS_summary <- summary(modelCoverNDVIPS)  
modelCoverNDVIPS_summary  

overall_r2CoverNDVIPS <- modelCoverNDVIPS_summary$r.squared  
overall_r2CoverNDVIPS  

#*5* Cover VS CRI with species interaction  
modelCoverCRI <- lm(Cover ~ CRI * Species, data = dataPS)  
modelCoverCRI  
modelCoverCRI_summary <- summary(modelCoverCRI)  
modelCoverCRI_summary  

overall_r2CoverCRI <- modelCoverCRI_summary$r.squared  
overall_r2CoverCRI  


#**********************************************************************#
#**********************************************************************#
##3 Grassbiomass
#*1* Grassbiomass VS PRIPS with species interaction
modelGrassbiomassPRIPS <- lm(Grassbiomass ~ PRIPS * Species, data = dataPS)
modelGrassbiomassPRIPS
modelGrassbiomassPRIPS_summary <- summary(modelGrassbiomassPRIPS)
modelGrassbiomassPRIPS_summary

overall_r2GrassbiomassPRIPS <- modelGrassbiomassPRIPS_summary$r.squared
overall_r2GrassbiomassPRIPS


#*2* Grassbiomass VS YIPS with species interaction
modelGrassbiomassYIPS <- lm(Grassbiomass ~ YIPS * Species, data = dataPS)
modelGrassbiomassYIPS
modelGrassbiomassYIPS_summary <- summary(modelGrassbiomassYIPS)
modelGrassbiomassYIPS_summary

overall_r2GrassbiomassYIPS <- modelGrassbiomassYIPS_summary$r.squared
overall_r2GrassbiomassYIPS


##3 Grassbiomass
#*3* Grassbiomass VS RGRPS with species interaction
modelGrassbiomassRGRPS <- lm(Grassbiomass ~ RGRPS * Species, data = dataPS)
modelGrassbiomassRGRPS
modelGrassbiomassRGRPS_summary <- summary(modelGrassbiomassRGRPS)
modelGrassbiomassRGRPS_summary

overall_r2GrassbiomassRGRPS <- modelGrassbiomassRGRPS_summary$r.squared
overall_r2GrassbiomassRGRPS

##3 Grassbiomass
#*4* Grassbiomass VS NDVIPS with species interaction
modelGrassbiomassNDVIPS <- lm(Grassbiomass ~ NDVIPS * Species, data = dataPS)
modelGrassbiomassNDVIPS
modelGrassbiomassNDVIPS_summary <- summary(modelGrassbiomassNDVIPS)
modelGrassbiomassNDVIPS_summary

overall_r2GrassbiomassNDVIPS <- modelGrassbiomassNDVIPS_summary$r.squared
overall_r2GrassbiomassNDVIPS

##3 Grassbiomass
#*5* Grassbiomass VS CRI with species interaction
modelGrassbiomassCRI <- lm(Grassbiomass ~ CRI * Species, data = dataPS)
modelGrassbiomassCRI
modelGrassbiomassCRI_summary <- summary(modelGrassbiomassCRI)
modelGrassbiomassCRI_summary

overall_r2GrassbiomassCRI <- modelGrassbiomassCRI_summary$r.squared
overall_r2GrassbiomassCRI


#**********************************************************************#
#**********************************************************************#
##4 TotalBiomass
#*1* TotalBiomass VS PRIPS with species interaction
modelTotalBiomassPRIPS <- lm(TotalBiomass ~ PRIPS * Species, data = dataPS)
modelTotalBiomassPRIPS
modelTotalBiomassPRIPS_summary <- summary(modelTotalBiomassPRIPS)
modelTotalBiomassPRIPS_summary

overall_r2TotalBiomassPRIPS <- modelTotalBiomassPRIPS_summary$r.squared
overall_r2TotalBiomassPRIPS

BPS<- ggplot(dataPS, aes(x = PRIPS, y = TotalBiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # CAI label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
BPS<- BPS + theme(legend.position = "right")
BPS<-BPS+ annotate("text", 
                   x = -0.0425, y = 530,    # Position (adjust based on your data range)
                   label = expression(italic(P)[int] == 0.0375), 
                   color = "black", 
                   size = 5)
BPS

#*2* TotalBiomass VS YIPS with species interaction
modelTotalBiomassYIPS <- lm(TotalBiomass ~ YIPS * Species, data = dataPS)
modelTotalBiomassYIPS
modelTotalBiomassYIPS_summary <- summary(modelTotalBiomassYIPS)
modelTotalBiomassYIPS_summary

overall_r2TotalBiomassYIPS <- modelTotalBiomassYIPS_summary$r.squared
overall_r2TotalBiomassYIPS


##4 TotalBiomass
#*3* TotalBiomass VS RGRPS with species interaction
modelTotalBiomassRGRPS <- lm(TotalBiomass ~ RGRPS * Species, data = dataPS)
modelTotalBiomassRGRPS
modelTotalBiomassRGRPS_summary <- summary(modelTotalBiomassRGRPS)
modelTotalBiomassRGRPS_summary

overall_r2TotalBiomassRGRPS <- modelTotalBiomassRGRPS_summary$r.squared
overall_r2TotalBiomassRGRPS

CPS<- ggplot(dataPS, aes(x = RGRPS, y = TotalBiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  # Add/modify axis titles here
  labs(
    x = "Red Green Ratio PlanetScope (RGRPS)",  # Replace with your desired title
    y = expression("Total Biomass" ~ (gm/m^2))# Replace with your desired title
  )+
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 18),  # CAI label size
    axis.title.y = element_text(size = 18),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 16),   # x-axis numbers
    axis.text.y = element_text(size = 16)    # y-axis numbers
  )
CPS<- CPS + theme(legend.position = "none")
CPS

library(emmeans)
library(performance)
# Calculate species-specific slopes
emtrends(modelTotalBiomassRGRPS, ~ Species, var = "RGRPS")

# Compute R² for each species subset
r2_valuesRGRPS3 <- dataPS %>%
  group_by(Species) %>%
  summarise(
    r_squared = r2(lm(TotalBiomass ~ RGRPS))$R2
  )
r2_valuesRGRPS3

##4 TotalBiomass
#*4* TotalBiomass VS NDVIPS with species interaction
modelTotalBiomassNDVIPS <- lm(TotalBiomass ~ NDVIPS * Species, data = dataPS)
modelTotalBiomassNDVIPS
modelTotalBiomassNDVIPS_summary <- summary(modelTotalBiomassNDVIPS)
modelTotalBiomassNDVIPS_summary

overall_r2TotalBiomassNDVIPS <- modelTotalBiomassNDVIPS_summary$r.squared
overall_r2TotalBiomassNDVIPS

DPS<- ggplot(dataPS, aes(x = NDVIPS, y = TotalBiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # CAI label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
DPS<- DPS + theme(legend.position = "right")
DPS<-DPS+ annotate("text", 
                   x = -0.1550, y = 530,    # Position (adjust based on your data range)
                   label = expression(italic(P)[int] == 0.0228), 
                   color = "black", 
                   size = 5)
DPS

##4 TotalBiomass
#*5* TotalBiomass VS CRI with species interaction
modelTotalBiomassCRI <- lm(TotalBiomass ~ CRI * Species, data = dataPS)
modelTotalBiomassCRI
modelTotalBiomassCRI_summary <- summary(modelTotalBiomassCRI)
modelTotalBiomassCRI_summary

overall_r2TotalBiomassCRI <- modelTotalBiomassCRI_summary$r.squared
overall_r2TotalBiomassCRI


#**********************************************************************#
#**********************************************************************#
##5 LAI
#*1* LAI VS PRIPS with species interaction
modelLAIPRIPS <- lm(LAI ~ PRIPS * Species, data = dataPS)
modelLAIPRIPS
modelLAIPRIPS_summary <- summary(modelLAIPRIPS)
modelLAIPRIPS_summary

overall_r2LAIPRIPS <- modelLAIPRIPS_summary$r.squared
overall_r2LAIPRIPS

#*2* LAI VS YIPS with species interaction
modelLAIYIPS <- lm(LAI ~ YIPS * Species, data = dataPS)
modelLAIYIPS
modelLAIYIPS_summary <- summary(modelLAIYIPS)
modelLAIYIPS_summary

overall_r2LAIYIPS <- modelLAIYIPS_summary$r.squared
overall_r2LAIYIPS

#*3* LAI VS RGRPS with species interaction
modelLAIRGRPS <- lm(LAI ~ RGRPS * Species, data = dataPS)
modelLAIRGRPS
modelLAIRGRPS_summary <- summary(modelLAIRGRPS)
modelLAIRGRPS_summary

overall_r2LAIRGRPS <- modelLAIRGRPS_summary$r.squared
overall_r2LAIRGRPS

EPS<- ggplot(dataPS, aes(x = RGRPS, y = LAI, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  # Add/modify axis titles here
  labs(
    x = "Red Green Ratio PlanetScope (RGRPS)",  # Replace with your desired title
    y = "Leaf Area Index"   # Replace with your desired title
  )+
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 18),  # CAI label size
    axis.title.y = element_text(size = 18),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 16),   # x-axis numbers
    axis.text.y = element_text(size = 16)    # y-axis numbers
  )
EPS<- EPS + theme(legend.position = "None")

EPS

library(emmeans)
library(performance)
# Calculate species-specific slopes
emtrends(modelLAIRGRPS, ~ Species, var = "RGRPS")

# Compute R² for each species subset
r2_valuesRGRPS2 <- dataPS %>%
  group_by(Species) %>%
  summarise(
    r_squared = r2(lm(LAI ~ RGRPS))$R2
  )
r2_valuesRGRPS2

#*4* LAI VS NDVIPS with species interaction
modelLAINDVIPS <- lm(LAI ~ NDVIPS * Species, data = dataPS)
modelLAINDVIPS
modelLAINDVIPS_summary <- summary(modelLAINDVIPS)
modelLAINDVIPS_summary

overall_r2LAINDVIPS <- modelLAINDVIPS_summary$r.squared
overall_r2LAINDVIPS

FPS<- ggplot(dataPS, aes(x = NDVIPS, y = LAI, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # CAI label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
FPS<- FPS + theme(legend.position = "right")
FPS<-FPS+ annotate("text", 
                   x = 0, y = 2.8,    # Position (adjust based on your data range)
                   label = expression(italic(P)[int] == 0.0403), 
                   color = "black", 
                   size = 5)
FPS

#*5* LAI VS CRI with species interaction
modelLAICRI <- lm(LAI ~ CRI * Species, data = dataPS)
modelLAICRI
modelLAICRI_summary <- summary(modelLAICRI)
modelLAICRI_summary

overall_r2LAICRI <- modelLAICRI_summary$r.squared
overall_r2LAICRI

#**********************************************************************#
#**********************************************************************#
##6 BareGroundCover
#*1* BareGroundCover VS PRIPS with species interaction
modelBareGroundCoverPRIPS <- lm(BareGroundCover ~ PRIPS * Species, data = dataPS)
modelBareGroundCoverPRIPS
modelBareGroundCoverPRIPS_summary <- summary(modelBareGroundCoverPRIPS)
modelBareGroundCoverPRIPS_summary

overall_r2BareGroundCoverPRIPS <- modelBareGroundCoverPRIPS_summary$r.squared
overall_r2BareGroundCoverPRIPS

GPS<- ggplot(dataPS, aes(x = PRIPS, y = BareGroundCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # CAI label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
GPS<- GPS + theme(legend.position = "right")
GPS<-GPS+ annotate("text", 
                   x = -0.0525, y = 13.5,    # Position (adjust based on your data range)
                   label = expression(italic(P)[int] == 0.0321), 
                   color = "black", 
                   size = 5)
GPS

#*2* BareGroundCover VS YIPS with species interaction
modelBareGroundCoverYIPS <- lm(BareGroundCover ~ YIPS * Species, data = dataPS)
modelBareGroundCoverYIPS
modelBareGroundCoverYIPS_summary <- summary(modelBareGroundCoverYIPS)
modelBareGroundCoverYIPS_summary

overall_r2BareGroundCoverYIPS <- modelBareGroundCoverYIPS_summary$r.squared
overall_r2BareGroundCoverYIPS

#*3* BareGroundCover VS RGRPS with species interaction
modelBareGroundCoverRGRPS <- lm(BareGroundCover ~ RGRPS * Species, data = dataPS)
modelBareGroundCoverRGRPS
modelBareGroundCoverRGRPS_summary <- summary(modelBareGroundCoverRGRPS)
modelBareGroundCoverRGRPS_summary

overall_r2BareGroundCoverRGRPS <- modelBareGroundCoverRGRPS_summary$r.squared
overall_r2BareGroundCoverRGRPS

HPS<- ggplot(dataPS, aes(x = RGRPS, y = BareGroundCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  # Add/modify axis titles here
  labs(
    x = "Red Green Ratio PlanetScope (RGRPS)",  # Replace with your desired title
    y = "Bare Ground (%)"   # Replace with your desired title
  )+
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 18),  # CAI label size
    axis.title.y = element_text(size = 18),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 16),   # x-axis numbers
    axis.text.y = element_text(size = 16)    # y-axis numbers
  )
HPS<- HPS + theme(legend.position = "none")
HPS

#*********************
#******Lets get species-wise slope!!!
#*******************
library(emmeans)
library(performance)
# Calculate species-specific slopes
emtrends(modelBareGroundCoverRGRPS, ~ Species, var = "RGRPS")

# Compute R² for each species subset
r2_valuesRGRPS <- dataPS %>%
  group_by(Species) %>%
  summarise(
    r_squared = r2(lm(BareGroundCover ~ RGRPS))$R2
  )
r2_valuesRGRPS

#*4* BareGroundCover VS NDVIPS with species interaction
modelBareGroundCoverNDVIPS <- lm(BareGroundCover ~ NDVIPS * Species, data = dataPS)
modelBareGroundCoverNDVIPS
modelBareGroundCoverNDVIPS_summary <- summary(modelBareGroundCoverNDVIPS)
modelBareGroundCoverNDVIPS_summary

overall_r2BareGroundCoverNDVIPS <- modelBareGroundCoverNDVIPS_summary$r.squared
overall_r2BareGroundCoverNDVIPS

IPS<- ggplot(dataPS, aes(x = NDVIPS, y = BareGroundCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # CAI label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
IPS<- IPS + theme(legend.position = "right")
IPS<-IPS+ annotate("text", 
                   x = -0.15, y = 13.5,    # Position (adjust based on your data range)
                   label = expression(italic(P)[int] == 0.0285), 
                   color = "black", 
                   size = 5)
IPS

#*5* BareGroundCover VS CRI with species interaction
modelBareGroundCoverCRI <- lm(BareGroundCover ~ CRI * Species, data = dataPS)
modelBareGroundCoverCRI
modelBareGroundCoverCRI_summary <- summary(modelBareGroundCoverCRI)
modelBareGroundCoverCRI_summary

overall_r2BareGroundCoverCRI <- modelBareGroundCoverCRI_summary$r.squared
overall_r2BareGroundCoverCRI


#**********************************************************************#
#**********************************************************************#
##7 DeadBiomass
#*1* DeadBiomass VS PRIPS with species interaction
modelDeadBiomassPRIPS <- lm(DeadBiomass ~ PRIPS * Species, data = dataPS)
modelDeadBiomassPRIPS
modelDeadBiomassPRIPS_summary <- summary(modelDeadBiomassPRIPS)
modelDeadBiomassPRIPS_summary

overall_r2DeadBiomassPRIPS <- modelDeadBiomassPRIPS_summary$r.squared
overall_r2DeadBiomassPRIPS

#*2* DeadBiomass VS YIPS with species interaction
modelDeadBiomassYIPS <- lm(DeadBiomass ~ YIPS * Species, data = dataPS)
modelDeadBiomassYIPS
modelDeadBiomassYIPS_summary <- summary(modelDeadBiomassYIPS)
modelDeadBiomassYIPS_summary

overall_r2DeadBiomassYIPS <- modelDeadBiomassYIPS_summary$r.squared
overall_r2DeadBiomassYIPS


#*3* DeadBiomass VS RGRPS with species interaction
modelDeadBiomassRGRPS <- lm(DeadBiomass ~ RGRPS * Species, data = dataPS)
modelDeadBiomassRGRPS
modelDeadBiomassRGRPS_summary <- summary(modelDeadBiomassRGRPS)
modelDeadBiomassRGRPS_summary

overall_r2DeadBiomassRGRPS <- modelDeadBiomassRGRPS_summary$r.squared
overall_r2DeadBiomassRGRPS

#*4* DeadBiomass VS NDVIPS with species interaction
modelDeadBiomassNDVIPS <- lm(DeadBiomass ~ NDVIPS * Species, data = dataPS)
modelDeadBiomassNDVIPS
modelDeadBiomassNDVIPS_summary <- summary(modelDeadBiomassNDVIPS)
modelDeadBiomassNDVIPS_summary

overall_r2DeadBiomassNDVIPS <- modelDeadBiomassNDVIPS_summary$r.squared
overall_r2DeadBiomassNDVIPS

#*5* DeadBiomass VS CRI with species interaction
modelDeadBiomassCRI <- lm(DeadBiomass ~ CRI * Species, data = dataPS)
modelDeadBiomassCRI
modelDeadBiomassCRI_summary <- summary(modelDeadBiomassCRI)
modelDeadBiomassCRI_summary

overall_r2DeadBiomassCRI <- modelDeadBiomassCRI_summary$r.squared
overall_r2DeadBiomassCRI

#**********************************************************************#
#**********************************************************************#
##8 ForbsBiomass                                    
#*1* ForbsBiomass VS PRIPS with species interaction
modelForbsBiomassPRIPS <- lm(ForbsBiomass ~ PRIPS * Species, data = dataPS)
modelForbsBiomassPRIPS
modelForbsBiomassPRIPS_summary <- summary(modelForbsBiomassPRIPS)
modelForbsBiomassPRIPS_summary

overall_r2ForbsBiomassPRIPS <- modelForbsBiomassPRIPS_summary$r.squared
overall_r2ForbsBiomassPRIPS

##8 ForbsBiomass                                   
#*2* ForbsBiomass VS YIPS with species interaction
modelForbsBiomassYIPS <- lm(ForbsBiomass ~ YIPS * Species, data = dataPS)
modelForbsBiomassYIPS
modelForbsBiomassYIPS_summary <- summary(modelForbsBiomassYIPS)
modelForbsBiomassYIPS_summary

overall_r2ForbsBiomassYIPS <- modelForbsBiomassYIPS_summary$r.squared
overall_r2ForbsBiomassYIPS

JPS<- ggplot(dataPS, aes(x = YIPS, y = ForbsBiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # CAI label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
JPS<- JPS + theme(legend.position = "none")
JPS<-JPS+ annotate("text", 
                   x = 0.035, y = 40,    # Position (adjust based on your data range)
                   label = expression(italic(P)[int] == 0.00864), 
                   color = "black", 
                   size = 5)
JPS

##8 ForbsBiomass                                    SIGNIFICANT
#*3* ForbsBiomass VS RGRPS with species interaction
modelForbsBiomassRGRPS <- lm(ForbsBiomass ~ RGRPS * Species, data = dataPS)
modelForbsBiomassRGRPS
modelForbsBiomassRGRPS_summary <- summary(modelForbsBiomassRGRPS)
modelForbsBiomassRGRPS_summary

overall_r2ForbsBiomassRGRPS <- modelForbsBiomassRGRPS_summary$r.squared
overall_r2ForbsBiomassRGRPS

##8 ForbsBiomass                                    
#*4* ForbsBiomass VS NDVIPS with species interaction
modelForbsBiomassNDVIPS <- lm(ForbsBiomass ~ NDVIPS * Species, data = dataPS)
modelForbsBiomassNDVIPS
modelForbsBiomassNDVIPS_summary <- summary(modelForbsBiomassNDVIPS)
modelForbsBiomassNDVIPS_summary

overall_r2ForbsBiomassNDVIPS <- modelForbsBiomassNDVIPS_summary$r.squared
overall_r2ForbsBiomassNDVIPS

##8 ForbsBiomass                                    
#*5* ForbsBiomass VS CRI with species interaction
modelForbsBiomassCRI <- lm(ForbsBiomass ~ CRI * Species, data = dataPS)
modelForbsBiomassCRI
modelForbsBiomassCRI_summary <- summary(modelForbsBiomassCRI)
modelForbsBiomassCRI_summary

overall_r2ForbsBiomassCRI <- modelForbsBiomassCRI_summary$r.squared
overall_r2ForbsBiomassCRI

#**********************************************************************#
#**********************************************************************#
##1 ForbsCover
#*1* ForbsCover VS PRIPS with species interaction     SIGNIFICANT
modelForbsCoverPRIPS <- lm(ForbsCover ~ PRIPS * Species, data = dataPS)
modelForbsCoverPRIPS
modelForbsCoverPRIPS_summary <- summary(modelForbsCoverPRIPS)
modelForbsCoverPRIPS_summary

overall_r2ForbsCoverPRIPS <- modelForbsCoverPRIPS_summary$r.squared
overall_r2ForbsCoverPRIPS

#*2* ForbsCover VS YIPS with species interaction     SIGNIFICANT
modelForbsCoverYIPS <- lm(ForbsCover ~ YIPS * Species, data = dataPS)
modelForbsCoverYIPS
modelForbsCoverYIPS_summary <- summary(modelForbsCoverYIPS)
modelForbsCoverYIPS_summary

overall_r2ForbsCoverYIPS <- modelForbsCoverYIPS_summary$r.squared
overall_r2ForbsCoverYIPS

KPS<- ggplot(dataPS, aes(x = YIPS, y = ForbsCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # CAI label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
KPS<- KPS + theme(legend.position = "none")
KPS<-KPS+ annotate("text", 
                   x = 0.035, y = 40,    # Position (adjust based on your data range)
                   label = expression(italic(P)[int] == 0.000927), 
                   color = "black", 
                   size = 5)
KPS

#*3* ForbsCover VS RGRPS with species interaction     SIGNIFICANT
modelForbsCoverRGRPS <- lm(ForbsCover ~ RGRPS * Species, data = dataPS)
modelForbsCoverRGRPS
modelForbsCoverRGRPS_summary <- summary(modelForbsCoverRGRPS)
modelForbsCoverRGRPS_summary

overall_r2ForbsCoverRGRPS <- modelForbsCoverRGRPS_summary$r.squared
overall_r2ForbsCoverRGRPS


#*4* ForbsCover VS NDVIPS with species interaction     SIGNIFICANT
modelForbsCoverNDVIPS <- lm(ForbsCover ~ NDVIPS * Species, data = dataPS)
modelForbsCoverNDVIPS
modelForbsCoverNDVIPS_summary <- summary(modelForbsCoverNDVIPS)
modelForbsCoverNDVIPS_summary

overall_r2ForbsCoverNDVIPS <- modelForbsCoverNDVIPS_summary$r.squared
overall_r2ForbsCoverNDVIPS

#*5* ForbsCover VS CRI with species interaction     SIGNIFICANT
modelForbsCoverCRI <- lm(ForbsCover ~ CRI * Species, data = dataPS)
modelForbsCoverCRI
modelForbsCoverCRI_summary <- summary(modelForbsCoverCRI)
modelForbsCoverCRI_summary

overall_r2ForbsCoverCRI <- modelForbsCoverCRI_summary$r.squared
overall_r2ForbsCoverCRI

#**********************************************************************#
#**********************************************************************#
library(patchwork)
combinedPS <- 
  (APS | CPS ) /      # Row 1
  (EPS | HPS ) /
  # Row 2
    # Row 3 (centered R)
  plot_layout(
    nrow = 2, 
    heights = c(1, 1), 
    guides = "collect"  # Shared legend
  )+ theme(
    legend.position = "top",           # Legend above all plots
    legend.justification = "center"    # Centers the legend
  )

combinedPS











