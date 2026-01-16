#setwd("D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets")
#setwd("C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets")
setwd("C:/Users/Mohammed/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets")

# Load libraries
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)


# Load dataset
Hy <- read.csv("Hyperspectral Reflectance.csv", check.names = FALSE)
#names(Hy)

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

# Convert data to long format for plotting
long_data <- mean_reflectance_per_site %>%
  pivot_longer(
    cols = -c(Site, Species),
    names_to = "Wavelength",
    values_to = "Reflectance"
  ) %>%
  mutate(Wavelength = as.numeric(Wavelength))  # Ensure wavelengths are numeric


##*******************************##
###Vegetation index calculation###
###**************************###
#*****************************************************************************************************#
#Normalized Differenced Vegetation indices (NDVI) (682-553) ((A. Gitelson & Merzlyak, 1994rudel, Thierry
#Fabre, Sophie, Houet, Thomas, Mazier, Florence, Briottet, Xavier#
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


#*****************************************************************************************************#
#                           RGR (Red Green Ratio)  (Hill, 2013)                                      #
#*****************************************************************************************************#
## C1 plots
## RGR (600/699/500/599)
# Vectorized approach (recommended)
sum_red_C1 <- sum(long_data$Reflectance[long_data$Site == "C1" & 
                                          long_data$Wavelength >= 600 & 
                                          long_data$Wavelength <= 699])

sum_green_C1 <- sum(long_data$Reflectance[long_data$Site == "C1" & 
                                            long_data$Wavelength >= 500 & 
                                            long_data$Wavelength <= 599])

RGR_C1 <- sum_red_C1 / sum_green_C1
RGR_C1

## C2 plots
sum_red_C2 <- sum(long_data$Reflectance[long_data$Site == "C2" & 
                                          long_data$Wavelength >= 600 & 
                                          long_data$Wavelength <= 699])

sum_green_C2 <- sum(long_data$Reflectance[long_data$Site == "C2" & 
                                            long_data$Wavelength >= 500 & 
                                            long_data$Wavelength <= 599])

RGR_C2 <- sum_red_C2 / sum_green_C2
RGR_C2

## C3 plots
sum_red_C3 <- sum(long_data$Reflectance[long_data$Site == "C3" & 
                                          long_data$Wavelength >= 600 & 
                                          long_data$Wavelength <= 699])

sum_green_C3 <- sum(long_data$Reflectance[long_data$Site == "C3" & 
                                            long_data$Wavelength >= 500 & 
                                            long_data$Wavelength <= 599])

RGR_C3 <- sum_red_C3 / sum_green_C3
RGR_C3

## C4 plots
sum_red_C4 <- sum(long_data$Reflectance[long_data$Site == "C4" & 
                                          long_data$Wavelength >= 600 & 
                                          long_data$Wavelength <= 699])

sum_green_C4 <- sum(long_data$Reflectance[long_data$Site == "C4" & 
                                            long_data$Wavelength >= 500 & 
                                            long_data$Wavelength <= 599])

RGR_C4 <- sum_red_C4 / sum_green_C4
RGR_C4

## C5 plots
sum_red_C5 <- sum(long_data$Reflectance[long_data$Site == "C5" & 
                                          long_data$Wavelength >= 600 & 
                                          long_data$Wavelength <= 699])

sum_green_C5 <- sum(long_data$Reflectance[long_data$Site == "C5" & 
                                            long_data$Wavelength >= 500 & 
                                            long_data$Wavelength <= 599])

RGR_C5 <- sum_red_C5 / sum_green_C5
RGR_C5

## C6 plots
sum_red_C6 <- sum(long_data$Reflectance[long_data$Site == "C6" & 
                                          long_data$Wavelength >= 600 & 
                                          long_data$Wavelength <= 699])

sum_green_C6 <- sum(long_data$Reflectance[long_data$Site == "C6" & 
                                            long_data$Wavelength >= 500 & 
                                            long_data$Wavelength <= 599])

RGR_C6 <- sum_red_C6 / sum_green_C6
RGR_C6

## C7 plots
sum_red_C7 <- sum(long_data$Reflectance[long_data$Site == "C7" & 
                                          long_data$Wavelength >= 600 & 
                                          long_data$Wavelength <= 699])

sum_green_C7 <- sum(long_data$Reflectance[long_data$Site == "C7" & 
                                            long_data$Wavelength >= 500 & 
                                            long_data$Wavelength <= 599])

RGR_C7 <- sum_red_C7 / sum_green_C7
RGR_C7

## C11 plots
sum_red_C11 <- sum(long_data$Reflectance[long_data$Site == "C11" & 
                                           long_data$Wavelength >= 600 & 
                                           long_data$Wavelength <= 699])

sum_green_C11 <- sum(long_data$Reflectance[long_data$Site == "C11" & 
                                             long_data$Wavelength >= 500 & 
                                             long_data$Wavelength <= 599])

RGR_C11 <- sum_red_C11 / sum_green_C11
RGR_C11

## C12 plots
sum_red_C12 <- sum(long_data$Reflectance[long_data$Site == "C12" & 
                                           long_data$Wavelength >= 600 & 
                                           long_data$Wavelength <= 699])

sum_green_C12 <- sum(long_data$Reflectance[long_data$Site == "C12" & 
                                             long_data$Wavelength >= 500 & 
                                             long_data$Wavelength <= 599])

RGR_C12 <- sum_red_C12 / sum_green_C12
RGR_C12

## C13 plots
sum_red_C13 <- sum(long_data$Reflectance[long_data$Site == "C13" & 
                                           long_data$Wavelength >= 600 & 
                                           long_data$Wavelength <= 699])

sum_green_C13 <- sum(long_data$Reflectance[long_data$Site == "C13" & 
                                             long_data$Wavelength >= 500 & 
                                             long_data$Wavelength <= 599])

RGR_C13 <- sum_red_C13 / sum_green_C13
RGR_C13

## C15 plots
sum_red_C15 <- sum(long_data$Reflectance[long_data$Site == "C15" & 
                                           long_data$Wavelength >= 600 & 
                                           long_data$Wavelength <= 699])

sum_green_C15 <- sum(long_data$Reflectance[long_data$Site == "C15" & 
                                             long_data$Wavelength >= 500 & 
                                             long_data$Wavelength <= 599])

RGR_C15 <- sum_red_C15 / sum_green_C15
RGR_C15

## C19 plots
sum_red_C19 <- sum(long_data$Reflectance[long_data$Site == "C19" & 
                                           long_data$Wavelength >= 600 & 
                                           long_data$Wavelength <= 699])

sum_green_C19 <- sum(long_data$Reflectance[long_data$Site == "C19" & 
                                             long_data$Wavelength >= 500 & 
                                             long_data$Wavelength <= 599])

RGR_C19 <- sum_red_C19 / sum_green_C19
RGR_C19

## C20 plots
sum_red_C20 <- sum(long_data$Reflectance[long_data$Site == "C20" & 
                                           long_data$Wavelength >= 600 & 
                                           long_data$Wavelength <= 699])

sum_green_C20 <- sum(long_data$Reflectance[long_data$Site == "C20" & 
                                             long_data$Wavelength >= 500 & 
                                             long_data$Wavelength <= 599])

RGR_C20 <- sum_red_C20 / sum_green_C20
RGR_C20

## N3 plots
sum_red_N3 <- sum(long_data$Reflectance[long_data$Site == "N3" & 
                                          long_data$Wavelength >= 600 & 
                                          long_data$Wavelength <= 699])

sum_green_N3 <- sum(long_data$Reflectance[long_data$Site == "N3" & 
                                            long_data$Wavelength >= 500 & 
                                            long_data$Wavelength <= 599])

RGR_N3 <- sum_red_N3 / sum_green_N3
RGR_N3

## N4 plots
sum_red_N4 <- sum(long_data$Reflectance[long_data$Site == "N4" & 
                                          long_data$Wavelength >= 600 & 
                                          long_data$Wavelength <= 699])

sum_green_N4 <- sum(long_data$Reflectance[long_data$Site == "N4" & 
                                            long_data$Wavelength >= 500 & 
                                            long_data$Wavelength <= 599])

RGR_N4 <- sum_red_N4 / sum_green_N4
RGR_N4

## N5 plots
sum_red_N5 <- sum(long_data$Reflectance[long_data$Site == "N5" & 
                                          long_data$Wavelength >= 600 & 
                                          long_data$Wavelength <= 699])

sum_green_N5 <- sum(long_data$Reflectance[long_data$Site == "N5" & 
                                            long_data$Wavelength >= 500 & 
                                            long_data$Wavelength <= 599])

RGR_N5 <- sum_red_N5 / sum_green_N5
RGR_N5

## N10 plots
sum_red_N10 <- sum(long_data$Reflectance[long_data$Site == "N10" & 
                                           long_data$Wavelength >= 600 & 
                                           long_data$Wavelength <= 699])

sum_green_N10 <- sum(long_data$Reflectance[long_data$Site == "N10" & 
                                             long_data$Wavelength >= 500 & 
                                             long_data$Wavelength <= 599])

RGR_N10 <- sum_red_N10 / sum_green_N10
RGR_N10

## N13 plots
sum_red_N13 <- sum(long_data$Reflectance[long_data$Site == "N13" & 
                                           long_data$Wavelength >= 600 & 
                                           long_data$Wavelength <= 699])

sum_green_N13 <- sum(long_data$Reflectance[long_data$Site == "N13" & 
                                             long_data$Wavelength >= 500 & 
                                             long_data$Wavelength <= 599])

RGR_N13 <- sum_red_N13 / sum_green_N13
RGR_N13

## N14 plots
sum_red_N14 <- sum(long_data$Reflectance[long_data$Site == "N14" & 
                                           long_data$Wavelength >= 600 & 
                                           long_data$Wavelength <= 699])

sum_green_N14 <- sum(long_data$Reflectance[long_data$Site == "N14" & 
                                             long_data$Wavelength >= 500 & 
                                             long_data$Wavelength <= 599])

RGR_N14 <- sum_red_N14 / sum_green_N14
RGR_N14

## N15 plots
sum_red_N15 <- sum(long_data$Reflectance[long_data$Site == "N15" & 
                                           long_data$Wavelength >= 600 & 
                                           long_data$Wavelength <= 699])

sum_green_N15 <- sum(long_data$Reflectance[long_data$Site == "N15" & 
                                             long_data$Wavelength >= 500 & 
                                             long_data$Wavelength <= 599])

RGR_N15 <- sum_red_N15 / sum_green_N15
RGR_N15

## N16 plots
sum_red_N16 <- sum(long_data$Reflectance[long_data$Site == "N16" & 
                                           long_data$Wavelength >= 600 & 
                                           long_data$Wavelength <= 699])

sum_green_N16 <- sum(long_data$Reflectance[long_data$Site == "N16" & 
                                             long_data$Wavelength >= 500 & 
                                             long_data$Wavelength <= 599])

RGR_N16 <- sum_red_N16 / sum_green_N16
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

library(tidyverse)
#########################################
########### Combine Results #############
#########################################
NDNI_results <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20", "N3", "N4", "N5", "N10", "N13", "N14", "N15", "N16"),
  NDNI = c(NDNI_C1, NDNI_C2, NDNI_C3, NDNI_C4, NDNI_C5, NDNI_C6, NDNI_C7,
           NDNI_C11, NDNI_C12, NDNI_C13, NDNI_C15, NDNI_C19, NDNI_C20,
           NDNI_N3, NDNI_N4, NDNI_N5, NDNI_N10, NDNI_N13, NDNI_N14, NDNI_N15, NDNI_N16))

#*****************************************************************************************************#
#            YI (Yellowness Index ) (Adams, M. L., Philpot, W. D., & Norvell, W. A. (1999). Yellowness index: An application of spectral second derivatives to estimate chlorosis of leaves in stressed vegetation. International Journal of Remote Sensing, 20(18), 3663–3675. https://doi.org/10.1080/014311699211264.                                 #
#*****************************************************************************************************#                     

## C1 plots
# Extract reflectance values for site "C1" at specific wavelengths
R577_C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 577]
R626_C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 626]
R666_C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_C1 <- (R577_C1 - 2*R626_C1 + R666_C1) / 0.00198025
YI_C1

## C2 plot
R577_C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 577]
R626_C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 626]
R666_C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_C2 <- (R577_C2 - 2*R626_C2 + R666_C2) / 0.00198025
YI_C2

## C3 plot
R577_C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 577]
R626_C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 626]
R666_C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_C3 <- (R577_C3 - 2*R626_C3 + R666_C3) / 0.00198025
YI_C3

## C4 plot
R577_C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 577]
R626_C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 626]
R666_C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_C4 <- (R577_C4 - 2*R626_C4 + R666_C4) / 0.00198025
YI_C4

## C5 plot
R577_C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 577]
R626_C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 626]
R666_C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_C5 <- (R577_C5 - 2*R626_C5 + R666_C5) / 0.00198025
YI_C5

## C6 plot
R577_C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 577]
R626_C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 626]
R666_C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_C6 <- (R577_C6 - 2*R626_C6 + R666_C6) / 0.00198025
YI_C6

## C7 plot
R577_C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 577]
R626_C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 626]
R666_C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_C7 <- (R577_C7 - 2*R626_C7 + R666_C7) / 0.00198025
YI_C7

## C11 plot
R577_C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 577]
R626_C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 626]
R666_C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_C11 <- (R577_C11 - 2*R626_C11 + R666_C11) / 0.00198025
YI_C11

## C12 plot
R577_C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 577]
R626_C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 626]
R666_C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_C12 <- (R577_C12 - 2*R626_C12 + R666_C12) / 0.00198025
YI_C12

## C13 plot
R577_C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 577]
R626_C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 626]
R666_C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_C13 <- (R577_C13 - 2*R626_C13 + R666_C13) / 0.00198025
YI_C13

## C15 plot
R577_C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 577]
R626_C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 626]
R666_C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_C15 <- (R577_C15 - 2*R626_C15 + R666_C15) / 0.00198025
YI_C15

## C19 plot
R577_C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 577]
R626_C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 626]
R666_C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_C19 <- (R577_C19 - 2*R626_C19 + R666_C19) / 0.00198025
YI_C19

## C20 plot
R577_C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 577]
R626_C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 626]
R666_C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_C20 <- (R577_C20 - 2*R626_C20 + R666_C20) / 0.00198025
YI_C20

## N3 plot
R577_N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 577]
R626_N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 626]
R666_N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_N3 <- (R577_N3 - 2*R626_N3 + R666_N3) / 0.00198025
YI_N3

## N4 plot
R577_N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 577]
R626_N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 626]
R666_N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_N4 <- (R577_N4 - 2*R626_N4 + R666_N4) / 0.00198025
YI_N4

## N5 plot
R577_N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 577]
R626_N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 626]
R666_N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_N5 <- (R577_N5 - 2*R626_N5 + R666_N5) / 0.00198025
YI_N5

## N10 plot
R577_N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 577]
R626_N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 626]
R666_N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_N10 <- (R577_N10 - 2*R626_N10 + R666_N10) / 0.00198025
YI_N10

## N13 plot
R577_N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 577]
R626_N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 626]
R666_N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_N13 <- (R577_N13 - 2*R626_N13 + R666_N13) / 0.00198025
YI_N13

## N14 plot
R577_N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 577]
R626_N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 626]
R666_N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_N14 <- (R577_N14 - 2*R626_N14 + R666_N14) / 0.00198025
YI_N14

## N15 plot
R577_N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 577]
R626_N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 626]
R666_N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_N15 <- (R577_N15 - 2*R626_N15 + R666_N15) / 0.00198025
YI_N15

## N16 plot
R577_N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 577]
R626_N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 626]
R666_N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 666]

# Calculate the spectral index
YI_N16 <- (R577_N16 - 2*R626_N16 + R666_N16) / 0.00198025
YI_N16        

library(tidyverse)
#########################################
########### Combine Results #############
#########################################
YI_results <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20", "N3", "N4", "N5", "N10", "N13", "N14", "N15", "N16"),
  YI = c(YI_C1, YI_C2, YI_C3, YI_C4, YI_C5, YI_C6, YI_C7,
         YI_C11, YI_C12, YI_C13, YI_C15, YI_C19, YI_C20,
         YI_N3, YI_N4, YI_N5, YI_N10, YI_N13, YI_N14, YI_N15, YI_N16))


#*****************************************************************************************************#
#                            Simple ratio pigment index (Pigment) (Penuelas et al., 1995)                                  #
#*****************************************************************************************************#                                    #
#*****************************************************************************************************#
## C1 plots
SR430C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 430]
SR680C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 680]
SR680_R430_C1 <- SR430C1 / SR680C1
SR680_R430_C1

## C2 plots
SR430C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 430]
SR680C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 680]
SR680_R430_C2 <- SR430C2 / SR680C2
SR680_R430_C2

## C3 plots
SR430C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 430]
SR680C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 680]
SR680_R430_C3 <- SR430C3 / SR680C3
SR680_R430_C3

## C4 plots
SR430C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 430]
SR680C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 680]
SR680_R430_C4 <- SR430C4 / SR680C4
SR680_R430_C4

## C5 plots
SR430C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 430]
SR680C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 680]
SR680_R430_C5 <- SR430C5 / SR680C5
SR680_R430_C5

## C6 plots
SR430C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 430]
SR680C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 680]
SR680_R430_C6 <- SR430C6 / SR680C6
SR680_R430_C6

## C7 plots
SR430C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 430]
SR680C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 680]
SR680_R430_C7 <- SR430C7 / SR680C7
SR680_R430_C7

## C11 plots
SR430C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 430]
SR680C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 680]
SR680_R430_C11 <- SR430C11 / SR680C11
SR680_R430_C11

## C12 plots
SR430C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 430]
SR680C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 680]
SR680_R430_C12 <- SR430C12 / SR680C12
SR680_R430_C12

## C13 plots
SR430C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 430]
SR680C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 680]
SR680_R430_C13 <- SR430C13 / SR680C13
SR680_R430_C13

## C15 plots
SR430C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 430]
SR680C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 680]
SR680_R430_C15 <- SR430C15 / SR680C15
SR680_R430_C15

## C19 plots
SR430C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 430]
SR680C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 680]
SR680_R430_C19 <- SR430C19 / SR680C19
SR680_R430_C19

## C20 plots
SR430C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 430]
SR680C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 680]
SR680_R430_C20 <- SR430C20 / SR680C20
SR680_R430_C20

## N3 plots
SR430N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 430]
SR680N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 680]
SR680_R430_N3 <- SR430N3 / SR680N3
SR680_R430_N3

## N4 plots
SR430N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 430]
SR680N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 680]
SR680_R430_N4 <- SR430N4 / SR680N4
SR680_R430_N4

## N5 plots
SR430N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 430]
SR680N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 680]
SR680_R430_N5 <- SR430N5 / SR680N5
SR680_R430_N5

## N10 plots
SR430N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 430]
SR680N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 680]
SR680_R430_N10 <- SR430N10 / SR680N10
SR680_R430_N10

## N13 plots
SR430N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 430]
SR680N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 680]
SR680_R430_N13 <- SR430N13 / SR680N13
SR680_R430_N13

## N14 plots
SR430N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 430]
SR680N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 680]
SR680_R430_N14 <- SR430N14 / SR680N14
SR680_R430_N14

## N15 plots
SR430N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 430]
SR680N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 680]
SR680_R430_N15 <- SR430N15 / SR680N15
SR680_R430_N15

## N16 plots
SR430N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 430]
SR680N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 680]
SR680_R430_N16 <- SR430N16 / SR680N16
SR680_R430_N16

#########################################
###########Combine and Export############
#########################################
SR680_R430_results<-SR680_R430_results <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20", "N3", "N4", "N5", "N10", "N13", "N14", "N15", "N16"),
  SR680_R430 = c(SR680_R430_C1, SR680_R430_C2, SR680_R430_C3, SR680_R430_C4, SR680_R430_C5, 
                 SR680_R430_C6, SR680_R430_C7, SR680_R430_C11, SR680_R430_C12, SR680_R430_C13,
                 SR680_R430_C15, SR680_R430_C19, SR680_R430_C20, SR680_R430_N3, SR680_R430_N4,
                 SR680_R430_N5, SR680_R430_N10, SR680_R430_N13, SR680_R430_N14, SR680_R430_N15,
                 SR680_R430_N16))



#*****************************************************************************************************#
#                            Shortwave-Infrared Vegetation Index  ((Lobell et al., 2001))                                  #
#*****************************************************************************************************#                                    #
#*****************************************************************************************************#
## C1 plots
R2210_C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 2210]
R2090_C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 2090]
R2280_C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIC1 <- 37.72 * (R2210_C1 - R2090_C1) + 
  26.27 * (R2280_C1 - R2090_C1) + 0.57
SWVIC1

## C2 plots
R2210_C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 2210]
R2090_C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 2090]
R2280_C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIC2 <- 37.72 * (R2210_C2 - R2090_C2) + 
  26.27 * (R2280_C2 - R2090_C2) + 0.57
SWVIC2

## C3 plots
R2210_C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 2210]
R2090_C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 2090]
R2280_C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIC3 <- 37.72 * (R2210_C3 - R2090_C3) + 
  26.27 * (R2280_C3 - R2090_C3) + 0.57
SWVIC3

## C4 plots
R2210_C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 2210]
R2090_C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 2090]
R2280_C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIC4 <- 37.72 * (R2210_C4 - R2090_C4) + 
  26.27 * (R2280_C4 - R2090_C4) + 0.57
SWVIC4

## C5 plots
R2210_C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 2210]
R2090_C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 2090]
R2280_C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIC5 <- 37.72 * (R2210_C5 - R2090_C5) + 
  26.27 * (R2280_C5 - R2090_C5) + 0.57
SWVIC5

## C6 plots
R2210_C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 2210]
R2090_C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 2090]
R2280_C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIC6 <- 37.72 * (R2210_C6 - R2090_C6) + 
  26.27 * (R2280_C6 - R2090_C6) + 0.57
SWVIC6

## C7 plots
R2210_C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 2210]
R2090_C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 2090]
R2280_C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIC7 <- 37.72 * (R2210_C7 - R2090_C7) + 
  26.27 * (R2280_C7 - R2090_C7) + 0.57
SWVIC7

## C11 plots
R2210_C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 2210]
R2090_C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 2090]
R2280_C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIC11 <- 37.72 * (R2210_C11 - R2090_C11) + 
  26.27 * (R2280_C11 - R2090_C11) + 0.57
SWVIC11

## C12 plots
R2210_C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 2210]
R2090_C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 2090]
R2280_C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIC12 <- 37.72 * (R2210_C12 - R2090_C12) + 
  26.27 * (R2280_C12 - R2090_C12) + 0.57
SWVIC12

## C13 plots
R2210_C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 2210]
R2090_C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 2090]
R2280_C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIC13 <- 37.72 * (R2210_C13 - R2090_C13) + 
  26.27 * (R2280_C13 - R2090_C13) + 0.57
SWVIC13

## C15 plots
R2210_C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 2210]
R2090_C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 2090]
R2280_C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIC15 <- 37.72 * (R2210_C15 - R2090_C15) + 
  26.27 * (R2280_C15 - R2090_C15) + 0.57
SWVIC15

## C19 plots
R2210_C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 2210]
R2090_C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 2090]
R2280_C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIC19 <- 37.72 * (R2210_C19 - R2090_C19) + 
  26.27 * (R2280_C19 - R2090_C19) + 0.57
SWVIC19

## C20 plots
R2210_C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 2210]
R2090_C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 2090]
R2280_C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIC20 <- 37.72 * (R2210_C20 - R2090_C20) + 
  26.27 * (R2280_C20 - R2090_C20) + 0.57
SWVIC20

## N3 plots
R2210_N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 2210]
R2090_N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 2090]
R2280_N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIN3 <- 37.72 * (R2210_N3 - R2090_N3) + 
  26.27 * (R2280_N3 - R2090_N3) + 0.57
SWVIN3

## N4 plots
R2210_N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 2210]
R2090_N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 2090]
R2280_N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIN4 <- 37.72 * (R2210_N4 - R2090_N4) + 
  26.27 * (R2280_N4 - R2090_N4) + 0.57
SWVIN4

## N5 plots
R2210_N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 2210]
R2090_N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 2090]
R2280_N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIN5 <- 37.72 * (R2210_N5 - R2090_N5) + 
  26.27 * (R2280_N5 - R2090_N5) + 0.57
SWVIN5

## N10 plots
R2210_N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 2210]
R2090_N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 2090]
R2280_N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIN10 <- 37.72 * (R2210_N10 - R2090_N10) + 
  26.27 * (R2280_N10 - R2090_N10) + 0.57
SWVIN10

## N13 plots
R2210_N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 2210]
R2090_N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 2090]
R2280_N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIN13 <- 37.72 * (R2210_N13 - R2090_N13) + 
  26.27 * (R2280_N13 - R2090_N13) + 0.57
SWVIN13

## N14 plots
R2210_N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 2210]
R2090_N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 2090]
R2280_N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIN14 <- 37.72 * (R2210_N14 - R2090_N14) + 
  26.27 * (R2280_N14 - R2090_N14) + 0.57
SWVIN14

## N15 plots
R2210_N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 2210]
R2090_N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 2090]
R2280_N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIN15 <- 37.72 * (R2210_N15 - R2090_N15) + 
  26.27 * (R2280_N15 - R2090_N15) + 0.57
SWVIN15

## N16 plots
R2210_N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 2210]
R2090_N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 2090]
R2280_N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 2280]

# Calculate the spectral index
SWVIN16 <- 37.72 * (R2210_N16 - R2090_N16) + 
  26.27 * (R2280_N16 - R2090_N16) + 0.57
SWVIN16

#########################################
###########Combine and Export############
#########################################
SWVI_results<-SWVI_results <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20", "N3", "N4", "N5", "N10", "N13", "N14", "N15", "N16"),
  SWVI = c(SWVIC1, SWVIC2, SWVIC3, SWVIC4, SWVIC5, 
           SWVIC6, SWVIC7, SWVIC11, SWVIC12, SWVIC13,
           SWVIC15, SWVIC19, SWVIC20, SWVIN3, SWVIN4,
           SWVIN5, SWVIN10, SWVIN13, SWVIN14, SWVIN15,
           SWVIN16))



#*****************************************************************************************************#
#                            Disease-Water Stress Index 2  (Apan et al., 2004)                               #
#*****************************************************************************************************#                                    #
#*****************************************************************************************************#
## C1 plots
DWSI1660C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 1660]
DWSI550C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 550]
DWSI1660_550_C1 <- DWSI1660C1 / DWSI550C1
DWSI1660_550_C1

## C2 plots
DWSI1660C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 1660]
DWSI550C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 550]
DWSI1660_550_C2 <- DWSI1660C2 / DWSI550C2
DWSI1660_550_C2

## C3 plots
DWSI1660C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 1660]
DWSI550C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 550]
DWSI1660_550_C3 <- DWSI1660C3 / DWSI550C3
DWSI1660_550_C3

## C4 plots
DWSI1660C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 1660]
DWSI550C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 550]
DWSI1660_550_C4 <- DWSI1660C4 / DWSI550C4
DWSI1660_550_C4

## C5 plots
DWSI1660C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 1660]
DWSI550C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 550]
DWSI1660_550_C5 <- DWSI1660C5 / DWSI550C5
DWSI1660_550_C5

## C6 plots
DWSI1660C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 1660]
DWSI550C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 550]
DWSI1660_550_C6 <- DWSI1660C6 / DWSI550C6
DWSI1660_550_C6

## C7 plots
DWSI1660C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 1660]
DWSI550C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 550]
DWSI1660_550_C7 <- DWSI1660C7 / DWSI550C7
DWSI1660_550_C7

## C11 plots
DWSI1660C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 1660]
DWSI550C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 550]
DWSI1660_550_C11 <- DWSI1660C11 / DWSI550C11
DWSI1660_550_C11

## C12 plots
DWSI1660C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 1660]
DWSI550C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 550]
DWSI1660_550_C12 <- DWSI1660C12 / DWSI550C12
DWSI1660_550_C12

## C13 plots
DWSI1660C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 1660]
DWSI550C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 550]
DWSI1660_550_C13 <- DWSI1660C13 / DWSI550C13
DWSI1660_550_C13

## C15 plots
DWSI1660C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 1660]
DWSI550C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 550]
DWSI1660_550_C15 <- DWSI1660C15 / DWSI550C15
DWSI1660_550_C15

## C19 plots
DWSI1660C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 1660]
DWSI550C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 550]
DWSI1660_550_C19 <- DWSI1660C19 / DWSI550C19
DWSI1660_550_C19

## C20 plots
DWSI1660C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 1660]
DWSI550C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 550]
DWSI1660_550_C20 <- DWSI1660C20 / DWSI550C20
DWSI1660_550_C20

## N3 plots
DWSI1660N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 1660]
DWSI550N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 550]
DWSI1660_550_N3 <- DWSI1660N3 / DWSI550N3
DWSI1660_550_N3

## N4 plots
DWSI1660N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 1660]
DWSI550N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 550]
DWSI1660_550_N4 <- DWSI1660N4 / DWSI550N4
DWSI1660_550_N4

## N5 plots
DWSI1660N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 1660]
DWSI550N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 550]
DWSI1660_550_N5 <- DWSI1660N5 / DWSI550N5
DWSI1660_550_N5

## N10 plots
DWSI1660N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 1660]
DWSI550N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 550]
DWSI1660_550_N10 <- DWSI1660N10 / DWSI550N10
DWSI1660_550_N10

## N13 plots
DWSI1660N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 1660]
DWSI550N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 550]
DWSI1660_550_N13 <- DWSI1660N13 / DWSI550N13
DWSI1660_550_N13

## N14 plots
DWSI1660N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 1660]
DWSI550N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 550]
DWSI1660_550_N14 <- DWSI1660N14 / DWSI550N14
DWSI1660_550_N14

## N15 plots
DWSI1660N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 1660]
DWSI550N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 550]
DWSI1660_550_N15 <- DWSI1660N15 / DWSI550N15
DWSI1660_550_N15

## N16 plots
DWSI1660N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 1660]
DWSI550N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 550]
DWSI1660_550_N16 <- DWSI1660N16 / DWSI550N16
DWSI1660_550_N16

#########################################
###########Combine and Export############
#########################################
DWSI_results<-DWSI_results <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20", "N3", "N4", "N5", "N10", "N13", "N14", "N15", "N16"),
  DWSI1660_550 = c(DWSI1660_550_C1, DWSI1660_550_C2, DWSI1660_550_C3, DWSI1660_550_C4, DWSI1660_550_C5, 
                   DWSI1660_550_C6, DWSI1660_550_C7, DWSI1660_550_C11, DWSI1660_550_C12, DWSI1660_550_C13,
                   DWSI1660_550_C15, DWSI1660_550_C19, DWSI1660_550_C20, DWSI1660_550_N3, DWSI1660_550_N4,
                   DWSI1660_550_N5, DWSI1660_550_N10, DWSI1660_550_N13, DWSI1660_550_N14, DWSI1660_550_N15,
                   DWSI1660_550_N16))



#*****************************************************************************************************#
#15 SATVI (Soil Adjusted Total Vegetation Index (B11, B12 and B4)  Marsett, R. C., Qi, J. G., Heilman, 
# P., Biedenbender, S. H., Watson, M. C., Amer, S., et al. (2006). Remote sensing for grassland management 
# in the arid Southwest. Range Ecology and Management, 59, 530–540
#*****************************************************************************************************#
# Define soil adjustment factor (L=0.5 as default)
L <- 0.5

## C1 plots (example template)
RB11C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 1650]  # SWIR1
RB4C1  <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 680]   # Red
RB12C1 <- long_data$Reflectance[long_data$Site == "C1" & long_data$Wavelength == 2215]  # SWIR2
SATVI_C1 <- ((RB11C1 - RB4C1) / (RB11C1 + RB4C1 + L)) * (1 + L) - (RB12C1 / 2)
SATVI_C1

## C2 plots
RB11C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 1650]
RB4C2  <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 680]
RB12C2 <- long_data$Reflectance[long_data$Site == "C2" & long_data$Wavelength == 2215]
SATVI_C2 <- ((RB11C2 - RB4C2) / (RB11C2 + RB4C2 + L)) * (1 + L) - (RB12C2 / 2)
SATVI_C2

## C3 plots
RB11C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 1650]
RB4C3  <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 680]
RB12C3 <- long_data$Reflectance[long_data$Site == "C3" & long_data$Wavelength == 2215]
SATVI_C3 <- ((RB11C3 - RB4C3) / (RB11C3 + RB4C3 + L)) * (1 + L) - (RB12C3 / 2)
SATVI_C3

## C4 plots
RB11C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 1650]
RB4C4  <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 680]
RB12C4 <- long_data$Reflectance[long_data$Site == "C4" & long_data$Wavelength == 2215]
SATVI_C4 <- ((RB11C4 - RB4C4) / (RB11C4 + RB4C4 + L)) * (1 + L) - (RB12C4 / 2)
SATVI_C4

## C5 plots
RB11C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 1650]
RB4C5  <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 680]
RB12C5 <- long_data$Reflectance[long_data$Site == "C5" & long_data$Wavelength == 2215]
SATVI_C5 <- ((RB11C5 - RB4C5) / (RB11C5 + RB4C5 + L)) * (1 + L) - (RB12C5 / 2)
SATVI_C5

## C6 plots
RB11C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 1650]
RB4C6  <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 680]
RB12C6 <- long_data$Reflectance[long_data$Site == "C6" & long_data$Wavelength == 2215]
SATVI_C6 <- ((RB11C6 - RB4C6) / (RB11C6 + RB4C6 + L)) * (1 + L) - (RB12C6 / 2)
SATVI_C6

## C7 plots
RB11C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 1650]
RB4C7  <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 680]
RB12C7 <- long_data$Reflectance[long_data$Site == "C7" & long_data$Wavelength == 2215]
SATVI_C7 <- ((RB11C7 - RB4C7) / (RB11C7 + RB4C7 + L)) * (1 + L) - (RB12C7 / 2)
SATVI_C7

## C11 plots
RB11C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 1650]
RB4C11  <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 680]
RB12C11 <- long_data$Reflectance[long_data$Site == "C11" & long_data$Wavelength == 2215]
SATVI_C11 <- ((RB11C11 - RB4C11) / (RB11C11 + RB4C11 + L)) * (1 + L) - (RB12C11 / 2)
SATVI_C11

## C12 plots
RB11C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 1650]
RB4C12  <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 680]
RB12C12 <- long_data$Reflectance[long_data$Site == "C12" & long_data$Wavelength == 2215]
SATVI_C12 <- ((RB11C12 - RB4C12) / (RB11C12 + RB4C12 + L)) * (1 + L) - (RB12C12 / 2)
SATVI_C12

## C13 plots
RB11C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 1650]
RB4C13  <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 680]
RB12C13 <- long_data$Reflectance[long_data$Site == "C13" & long_data$Wavelength == 2215]
SATVI_C13 <- ((RB11C13 - RB4C13) / (RB11C13 + RB4C13 + L)) * (1 + L) - (RB12C13 / 2)
SATVI_C13

## C15 plots
RB11C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 1650]
RB4C15  <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 680]
RB12C15 <- long_data$Reflectance[long_data$Site == "C15" & long_data$Wavelength == 2215]
SATVI_C15 <- ((RB11C15 - RB4C15) / (RB11C15 + RB4C15 + L)) * (1 + L) - (RB12C15 / 2)
SATVI_C15

## C19 plots
RB11C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 1650]
RB4C19  <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 680]
RB12C19 <- long_data$Reflectance[long_data$Site == "C19" & long_data$Wavelength == 2215]
SATVI_C19 <- ((RB11C19 - RB4C19) / (RB11C19 + RB4C19 + L)) * (1 + L) - (RB12C19 / 2)
SATVI_C19

## C20 plots
RB11C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 1650]
RB4C20  <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 680]
RB12C20 <- long_data$Reflectance[long_data$Site == "C20" & long_data$Wavelength == 2215]
SATVI_C20 <- ((RB11C20 - RB4C20) / (RB11C20 + RB4C20 + L)) * (1 + L) - (RB12C20 / 2)
SATVI_C20

## N3 plots
RB11N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 1650]
RB4N3  <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 680]
RB12N3 <- long_data$Reflectance[long_data$Site == "N3" & long_data$Wavelength == 2215]
SATVI_N3 <- ((RB11N3 - RB4N3) / (RB11N3 + RB4N3 + L)) * (1 + L) - (RB12N3 / 2)
SATVI_N3

## N4 plots
RB11N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 1650]
RB4N4  <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 680]
RB12N4 <- long_data$Reflectance[long_data$Site == "N4" & long_data$Wavelength == 2215]
SATVI_N4 <- ((RB11N4 - RB4N4) / (RB11N4 + RB4N4 + L)) * (1 + L) - (RB12N4 / 2)
SATVI_N4

## N5 plots
RB11N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 1650]
RB4N5  <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 680]
RB12N5 <- long_data$Reflectance[long_data$Site == "N5" & long_data$Wavelength == 2215]
SATVI_N5 <- ((RB11N5 - RB4N5) / (RB11N5 + RB4N5 + L)) * (1 + L) - (RB12N5 / 2)
SATVI_N5

## N10 plots
RB11N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 1650]
RB4N10  <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 680]
RB12N10 <- long_data$Reflectance[long_data$Site == "N10" & long_data$Wavelength == 2215]
SATVI_N10 <- ((RB11N10 - RB4N10) / (RB11N10 + RB4N10 + L)) * (1 + L) - (RB12N10 / 2)
SATVI_N10

## N13 plots
RB11N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 1650]
RB4N13  <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 680]
RB12N13 <- long_data$Reflectance[long_data$Site == "N13" & long_data$Wavelength == 2215]
SATVI_N13 <- ((RB11N13 - RB4N13) / (RB11N13 + RB4N13 + L)) * (1 + L) - (RB12N13 / 2)
SATVI_N13

## N14 plots
RB11N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 1650]
RB4N14  <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 680]
RB12N14 <- long_data$Reflectance[long_data$Site == "N14" & long_data$Wavelength == 2215]
SATVI_N14 <- ((RB11N14 - RB4N14) / (RB11N14 + RB4N14 + L)) * (1 + L) - (RB12N14 / 2)
SATVI_N14

## N15 plots
RB11N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 1650]
RB4N15  <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 680]
RB12N15 <- long_data$Reflectance[long_data$Site == "N15" & long_data$Wavelength == 2215]
SATVI_N15 <- ((RB11N15 - RB4N15) / (RB11N15 + RB4N15 + L)) * (1 + L) - (RB12N15 / 2)
SATVI_N15

## N16 plots
RB11N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 1650]
RB4N16  <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 680]
RB12N16 <- long_data$Reflectance[long_data$Site == "N16" & long_data$Wavelength == 2215]
SATVI_N16 <- ((RB11N16 - RB4N16) / (RB11N16 + RB4N16 + L)) * (1 + L) - (RB12N16 / 2)
SATVI_N16

#########################################
###########Combine and Export############
#########################################
SATVI_results <- SATVI <- data.frame(
  Plot = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C11", "C12", "C13", 
           "C15", "C19", "C20","N3","N4","N5","N10","N13","N14","N15","N16"),
  SATVI = c(SATVI_C1, SATVI_C2, SATVI_C3, SATVI_C4,
            SATVI_C5, SATVI_C6, SATVI_C7,
            SATVI_C11, SATVI_C12, SATVI_C13, SATVI_C15, 
            SATVI_C19, SATVI_C20, SATVI_N3,
            SATVI_N4, SATVI_N5, SATVI_N10,
            SATVI_N13, SATVI_N14, SATVI_N15,
            SATVI_N16))

#View(SATVI_results)


#Export the file
#write.table(SATVI_results, "C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/HyperSpectral/SImulation/Sentinel/SATVI_results.csv", sep = ",", row.names = FALSE)

######################################################
######### Combine all these spectral indices datasets
######################################################
# List all your loaded data frames (replace with your actual object names)
SI_list <- list(SATVI_results,YI_results, SR680_R430_results, SWVI_results, DWSI_results,NDNI_results, CAI_results, PRI_results, TCARI_results, MCARI2, RGR_700_670, RGR_results, ARI_550_700, CRI_510_550, NDVI_682_553)

# Merge all by "Plot" (keeps all rows and fills missing values with NA)
combined_data <- reduce(SI_list, ~ full_join(.x, .y, by = "Plot"))

######################################################
#*************Linear model with interaction test******#
######################################################

#setwd("D:/Usask/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data")
#setwd("C:/Users/mta401/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data")
setwd("C:/Users/Mohammed/OneDrive - University of Saskatchewan/MY Desktop/USASK Msc Thesis Files/Thesis DataSets/Analysis/Biophysical data")

Biophy <- read_csv("PlotWiseDescriptiveStat.csv")
Biophy$Plot <- Biophy$Site
data_list <- list(Biophy, combined_data)

# Merge all by "Plot" (keeps all rows and fills missing values with NA)
data <- reduce(data_list, ~ full_join(.x, .y, by = "Plot"))
data <- data[, -c(7, 8, 11,12, 14, 15, 18)]
#View(data)

#####################################################
#####################################################

##1 Height
#*1* Height VS NDNI with species interaction
modelHeightNDNI <- lm(Height ~ NDNI * Species, data = data)
modelHeightNDNI
modelHeightNDNI_summary <- summary(modelHeightNDNI)
modelHeightNDNI_summary

overall_r2HeightNDNI <- modelHeightNDNI_summary$r.squared
overall_r2HeightNDNI

#*2* Height VS CAI with species interaction******************
modelHeightCAI <- lm(Height ~ CAI * Species, data = data)
modelHeightCAI
modelHeightCAI_summary <- summary(modelHeightCAI)
modelHeightCAI_summary

overall_r2HeightCAI <- modelHeightCAI_summary$r.squared
overall_r2HeightCAI

A<- ggplot(data, aes(x = CAI, y = Height, color = Species)) + 
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
A <- A + theme(legend.position = "right")
A<-A+ annotate("text", 
            x = 0.021, y = 46,    # Position (adjust based on your data range)
            label = expression(italic(P)[int] == 0.0151), 
            color = "black", 
            size = 5)
A
#*********************
#******Lets get species-wise slope!!!
#*******************
library(emmeans)
# Calculate species-specific slopes
emtrends(modelHeightCAI, ~ Species, var = "CAI")


#*3* Height VS PRI with species interaction
modelHeightPRI <- lm(Height ~ PRI * Species, data = data)
modelHeightPRI
modelHeightPRI_summary <- summary(modelHeightPRI)
modelHeightPRI_summary

overall_r2HeightPRI <- modelHeightPRI_summary$r.squared
overall_r2HeightPRI

#*4* Height VS TCARI with species interaction
modelHeightTCARI<- lm(Height ~ TCARI * Species, data = data)
modelHeightTCARI
modelHeightTCARI_summary <- summary(modelHeightTCARI)
modelHeightTCARI_summary

overall_r2HeightTCARI <- modelHeightTCARI_summary$r.squared
overall_r2HeightTCARI

#*5* Height VS MCARI2 with species interaction
modelHeightMCARI2 <- lm(Height ~ MCARI2 * Species, data = data)
modelHeightMCARI2
modelHeightMCARI2_summary <- summary(modelHeightMCARI2)
modelHeightMCARI2_summary

overall_r2HeightMCARI2 <- modelHeightMCARI2_summary$r.squared
overall_r2HeightMCARI2

#*6* Height VS R700_R670 with species interaction
modelHeightR700_R670 <- lm(Height ~ R700_R670 * Species, data = data)
modelHeightR700_R670
modelHeightR700_R670_summary <- summary(modelHeightR700_R670)
modelHeightR700_R670_summary

overall_r2HeightR700_R670 <- modelHeightR700_R670_summary$r.squared
overall_r2HeightR700_R670

#*7* Height VS RGR with species interaction
modelHeightRGR <- lm(Height ~ RGR * Species, data = data)
modelHeightRGR
modelHeightRGR_summary <- summary(modelHeightRGR)
modelHeightRGR_summary

overall_r2HeightRGR <- modelHeightRGR_summary$r.squared
overall_r2HeightRGR

#*8* Height VS ARI_550_700 with species interaction
modelHeightARI_550_700 <- lm(Height ~ ARI_550_700 * Species, data = data)
modelHeightARI_550_700
modelHeightARI_550_700_summary <- summary(modelHeightARI_550_700)
modelHeightARI_550_700_summary

overall_r2HeightARI_550_700 <- modelHeightARI_550_700_summary$r.squared
overall_r2HeightARI_550_700

#*9* Height VS CRI_510_550 with species interaction
modelHeightCRI_510_550 <- lm(Height ~ CRI_510_550 * Species, data = data)
modelHeightCRI_510_550
modelHeightCRI_510_550_summary <- summary(modelHeightCRI_510_550)
modelHeightCRI_510_550_summary

overall_r2HeightCRI_510_550 <- modelHeightCRI_510_550_summary$r.squared
overall_r2HeightCRI_510_550

#*10* Height VS NDVI_682_553 with species interaction
modelHeightNDVI_682_553 <- lm(Height ~ NDVI_682_553 * Species, data = data)
modelHeightNDVI_682_553
modelHeightNDVI_682_553_summary <- summary(modelHeightNDVI_682_553)
modelHeightNDVI_682_553_summary

overall_r2HeightNDVI_682_553 <- modelHeightNDVI_682_553_summary$r.squared
overall_r2HeightNDVI_682_553

#*11* Height VS YI with species interaction
modelHeightYI <- lm(Height ~ YI * Species, data = data)
modelHeightYI
modelHeightYI_summary <- summary(modelHeightYI)
modelHeightYI_summary

overall_r2HeightYI <- modelHeightYI_summary$r.squared
overall_r2HeightYI

#*12* Height VS SR680_R430 with species interaction
modelHeightSR680_R430 <- lm(Height ~ SR680_R430 * Species, data = data)
modelHeightSR680_R430
modelHeightSR680_R430_summary <- summary(modelHeightSR680_R430)
modelHeightSR680_R430_summary

overall_r2HeightSR680_R430 <- modelHeightSR680_R430_summary$r.squared
overall_r2HeightSR680_R430

#*13* Height VS SWVI with species interaction**********************
modelHeightSWVI <- lm(Height ~ SWVI * Species, data = data)
modelHeightSWVI
modelHeightSWVI_summary <- summary(modelHeightSWVI)
modelHeightSWVI_summary

overall_r2HeightSWVI <- modelHeightSWVI_summary$r.squared
overall_r2HeightSWVI

A2<- ggplot(data, aes(x = SWVI, y = Height, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
    labs(
    x = "Shortwave-Infrared Vegetation Index (SWVI)",  # Replace with your desired title
    y = "Height (cm)"   # Replace with your desired title
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
A2 <- A2 + theme(legend.position = "None")
A2<-A2
A2
#*********************
#******Lets get species-wise slope!!!
#*******************
library(emmeans)
library(performance)
# Calculate species-specific slopes
emtrends(modelHeightSWVI, ~ Species, var = "SWVI")

# Compute R² for each species subset
r2_values <- data %>%
  group_by(Species) %>%
  summarise(
    r_squared = r2(lm(Height ~ SWVI))$R2
  )
r2_values

#*14* Height VS DWSI1660_550 with species interaction
modelHeightDWSI1660_550 <- lm(Height ~ DWSI1660_550 * Species, data = data)
modelHeightDWSI1660_550
modelHeightDWSI1660_550_summary <- summary(modelHeightDWSI1660_550)
modelHeightDWSI1660_550_summary

overall_r2HeightDWSI1660_550 <- modelHeightDWSI1660_550_summary$r.squared
overall_r2HeightDWSI1660_550

#*15* Height VS SATVI with species interaction
modelHeightSATVI <- lm(Height ~ SATVI * Species, data = data)
modelHeightSATVI
modelHeightSATVI_summary <- summary(modelHeightSATVI)
modelHeightSATVI_summary

overall_r2HeightSATVI <- modelHeightSATVI_summary$r.squared
overall_r2HeightSATVI

#**********************************************************************#
#**********************************************************************#

##2 Cover
#*1* Cover VS NDNI with species interaction
modelCoverNDNI <- lm(Cover ~ NDNI * Species, data = data)
modelCoverNDNI
modelCoverNDNI_summary <- summary(modelCoverNDNI)
modelCoverNDNI_summary

overall_r2CoverNDNI <- modelCoverNDNI_summary$r.squared
overall_r2CoverNDNI

#*2* Cover VS CAI with species interaction
modelCoverCAI <- lm(Cover ~ CAI * Species, data = data)
modelCoverCAI
modelCoverCAI_summary <- summary(modelCoverCAI)
modelCoverCAI_summary

overall_r2CoverCAI <- modelCoverCAI_summary$r.squared
overall_r2CoverCAI

AA<- ggplot(data, aes(x = CAI, y = Cover, color = Species)) + 
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
AA <- AA + theme(legend.position = "none")
AA<-AA+ annotate("text", 
               x = 0.0210, y = 53,    # Position (adjust based on your data range)
               label = expression(italic(P)[int] == 0.08), 
               color = "black", 
               size = 4)

library(emmeans)
# Calculate species-specific slopes
emtrends(modelCoverCAI, ~ Species, var = "CAI")

#*3* Cover VS PRI with species interaction
modelCoverPRI <- lm(Cover ~ PRI * Species, data = data)
modelCoverPRI
modelCoverPRI_summary <- summary(modelCoverPRI)
modelCoverPRI_summary

overall_r2CoverPRI <- modelCoverPRI_summary$r.squared
overall_r2CoverPRI

#*4* Cover VS TCARI with species interaction
modelCoverTCARI<- lm(Cover ~ TCARI * Species, data = data)
modelCoverTCARI
modelCoverTCARI_summary <- summary(modelCoverTCARI)
modelCoverTCARI_summary

overall_r2CoverTCARI <- modelCoverTCARI_summary$r.squared
overall_r2CoverTCARI

#*5* Cover VS MCARI2 with species interaction
modelCoverMCARI2 <- lm(Cover ~ MCARI2 * Species, data = data)
modelCoverMCARI2
modelCoverMCARI2_summary <- summary(modelCoverMCARI2)
modelCoverMCARI2_summary

overall_r2CoverMCARI2 <- modelCoverMCARI2_summary$r.squared
overall_r2CoverMCARI2

#*6* Cover VS R700_R670 with species interaction
modelCoverR700_R670 <- lm(Cover ~ R700_R670 * Species, data = data)
modelCoverR700_R670
modelCoverR700_R670_summary <- summary(modelCoverR700_R670)
modelCoverR700_R670_summary

overall_r2CoverR700_R670 <- modelCoverR700_R670_summary$r.squared
overall_r2CoverR700_R670

#*7* Cover VS RGR with species interaction
modelCoverRGR <- lm(Cover ~ RGR * Species, data = data)
modelCoverRGR
modelCoverRGR_summary <- summary(modelCoverRGR)
modelCoverRGR_summary

overall_r2CoverRGR <- modelCoverRGR_summary$r.squared
overall_r2CoverRGR

#*8* Cover VS ARI_550_700 with species interaction
modelCoverARI_550_700 <- lm(Cover ~ ARI_550_700 * Species, data = data)
modelCoverARI_550_700
modelCoverARI_550_700_summary <- summary(modelCoverARI_550_700)
modelCoverARI_550_700_summary

overall_r2CoverARI_550_700 <- modelCoverARI_550_700_summary$r.squared
overall_r2CoverARI_550_700

#*9* Cover VS CRI_510_550 with species interaction
modelCoverCRI_510_550 <- lm(Cover ~ CRI_510_550 * Species, data = data)
modelCoverCRI_510_550
modelCoverCRI_510_550_summary <- summary(modelCoverCRI_510_550)
modelCoverCRI_510_550_summary

overall_r2CoverCRI_510_550 <- modelCoverCRI_510_550_summary$r.squared
overall_r2CoverCRI_510_550

#*10* Cover VS NDVI_682_553 with species interaction
modelCoverNDVI_682_553 <- lm(Cover ~ NDVI_682_553 * Species, data = data)
modelCoverNDVI_682_553
modelCoverNDVI_682_553_summary <- summary(modelCoverNDVI_682_553)
modelCoverNDVI_682_553_summary

overall_r2CoverNDVI_682_553 <- modelCoverNDVI_682_553_summary$r.squared
overall_r2CoverNDVI_682_553

#*11* Cover VS YI with species interaction
modelCoverYI <- lm(Cover ~ YI * Species, data = data)
modelCoverYI
modelCoverYI_summary <- summary(modelCoverYI)
modelCoverYI_summary

overall_r2CoverYI <- modelCoverYI_summary$r.squared
overall_r2CoverYI

#*12* Cover VS SR680_R430 with species interaction
modelCoverSR680_R430 <- lm(Cover ~ SR680_R430 * Species, data = data)
modelCoverSR680_R430
modelCoverSR680_R430_summary <- summary(modelCoverSR680_R430)
modelCoverSR680_R430_summary

overall_r2CoverSR680_R430 <- modelCoverSR680_R430_summary$r.squared
overall_r2CoverSR680_R430

#*13* Cover VS SWVI with species interaction
modelCoverSWVI <- lm(Cover ~ SWVI * Species, data = data)
modelCoverSWVI
modelCoverSWVI_summary <- summary(modelCoverSWVI)
modelCoverSWVI_summary

overall_r2CoverSWVI <- modelCoverSWVI_summary$r.squared
overall_r2CoverSWVI

#*14* Cover VS DWSI1660_550 with species interaction
modelCoverDWSI1660_550 <- lm(Cover ~ DWSI1660_550 * Species, data = data)
modelCoverDWSI1660_550
modelCoverDWSI1660_550_summary <- summary(modelCoverDWSI1660_550)
modelCoverDWSI1660_550_summary

overall_r2CoverDWSI1660_550 <- modelCoverDWSI1660_550_summary$r.squared
overall_r2CoverDWSI1660_550

#*15* Cover VS SATVI with species interaction
modelCoverSATVI <- lm(Cover ~ SATVI * Species, data = data)
modelCoverSATVI
modelCoverSATVI_summary <- summary(modelCoverSATVI)
modelCoverSATVI_summary

overall_r2CoverSATVI <- modelCoverSATVI_summary$r.squared
overall_r2CoverSATVI

#**********************************************************************#
#**********************************************************************#

##3 Grassbiomass
#*1* Grassbiomass VS NDNI with species interaction
modelGrassbiomassNDNI <- lm(Grassbiomass ~ NDNI * Species, data = data)
modelGrassbiomassNDNI
modelGrassbiomassNDNI_summary <- summary(modelGrassbiomassNDNI)
modelGrassbiomassNDNI_summary

overall_r2GrassbiomassNDNI <- modelGrassbiomassNDNI_summary$r.squared
overall_r2GrassbiomassNDNI

#*2* Grassbiomass VS CAI with species interaction
modelGrassbiomassCAI <- lm(Grassbiomass ~ CAI * Species, data = data)
modelGrassbiomassCAI
modelGrassbiomassCAI_summary <- summary(modelGrassbiomassCAI)
modelGrassbiomassCAI_summary

overall_r2GrassbiomassCAI <- modelGrassbiomassCAI_summary$r.squared
overall_r2GrassbiomassCAI

B<- ggplot(data, aes(x = CAI, y = Grassbiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Cellulose Absorption Index (CAI)",
    y = expression("Grass Biomass" ~ (gm/m^2))
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
B<-B+ theme(legend.position = "none")
B

library(emmeans)
# Calculate species-specific slopes
emtrends(modelGrassbiomassCAI, ~ Species, var = "CAI")

# Compute R² for each species subset
r2_valuesBB <- data %>%
  group_by(Species) %>%
  summarise(
    r_squared = r2(lm(Grassbiomass ~ CAI))$R2
  )
r2_valuesBB

#*3* Grassbiomass VS PRI with species interaction
modelGrassbiomassPRI <- lm(Grassbiomass ~ PRI * Species, data = data)
modelGrassbiomassPRI
modelGrassbiomassPRI_summary <- summary(modelGrassbiomassPRI)
modelGrassbiomassPRI_summary

overall_r2GrassbiomassPRI <- modelGrassbiomassPRI_summary$r.squared
overall_r2GrassbiomassPRI

#*4* Grassbiomass VS TCARI with species interaction
modelGrassbiomassTCARI<- lm(Grassbiomass ~ TCARI * Species, data = data)
modelGrassbiomassTCARI
modelGrassbiomassTCARI_summary <- summary(modelGrassbiomassTCARI)
modelGrassbiomassTCARI_summary

overall_r2GrassbiomassTCARI <- modelGrassbiomassTCARI_summary$r.squared
overall_r2GrassbiomassTCARI

#*5* Grassbiomass VS MCARI2 with species interaction
modelGrassbiomassMCARI2 <- lm(Grassbiomass ~ MCARI2 * Species, data = data)
modelGrassbiomassMCARI2
modelGrassbiomassMCARI2_summary <- summary(modelGrassbiomassMCARI2)
modelGrassbiomassMCARI2_summary

overall_r2GrassbiomassMCARI2 <- modelGrassbiomassMCARI2_summary$r.squared
overall_r2GrassbiomassMCARI2

#*6* Grassbiomass VS R700_R670 with species interaction
modelGrassbiomassR700_R670 <- lm(Grassbiomass ~ R700_R670 * Species, data = data)
modelGrassbiomassR700_R670
modelGrassbiomassR700_R670_summary <- summary(modelGrassbiomassR700_R670)
modelGrassbiomassR700_R670_summary

overall_r2GrassbiomassR700_R670 <- modelGrassbiomassR700_R670_summary$r.squared
overall_r2GrassbiomassR700_R670

#*7* Grassbiomass VS RGR with species interaction
modelGrassbiomassRGR <- lm(Grassbiomass ~ RGR * Species, data = data)
modelGrassbiomassRGR
modelGrassbiomassRGR_summary <- summary(modelGrassbiomassRGR)
modelGrassbiomassRGR_summary

overall_r2GrassbiomassRGR <- modelGrassbiomassRGR_summary$r.squared
overall_r2GrassbiomassRGR

#*8* Grassbiomass VS ARI_550_700 with species interaction
modelGrassbiomassARI_550_700 <- lm(Grassbiomass ~ ARI_550_700 * Species, data = data)
modelGrassbiomassARI_550_700
modelGrassbiomassARI_550_700_summary <- summary(modelGrassbiomassARI_550_700)
modelGrassbiomassARI_550_700_summary

overall_r2GrassbiomassARI_550_700 <- modelGrassbiomassARI_550_700_summary$r.squared
overall_r2GrassbiomassARI_550_700

#*9* Grassbiomass VS CRI_510_550 with species interaction
modelGrassbiomassCRI_510_550 <- lm(Grassbiomass ~ CRI_510_550 * Species, data = data)
modelGrassbiomassCRI_510_550
modelGrassbiomassCRI_510_550_summary <- summary(modelGrassbiomassCRI_510_550)
modelGrassbiomassCRI_510_550_summary

overall_r2GrassbiomassCRI_510_550 <- modelGrassbiomassCRI_510_550_summary$r.squared
overall_r2GrassbiomassCRI_510_550

#*10* Grassbiomass VS NDVI_682_553 with species interaction
modelGrassbiomassNDVI_682_553 <- lm(Grassbiomass ~ NDVI_682_553 * Species, data = data)
modelGrassbiomassNDVI_682_553
modelGrassbiomassNDVI_682_553_summary <- summary(modelGrassbiomassNDVI_682_553)
modelGrassbiomassNDVI_682_553_summary

overall_r2GrassbiomassNDVI_682_553 <- modelGrassbiomassNDVI_682_553_summary$r.squared
overall_r2GrassbiomassNDVI_682_553

#*11* Grassbiomass VS YI with species interaction
modelGrassbiomassYI <- lm(Grassbiomass ~ YI * Species, data = data)
modelGrassbiomassYI
modelGrassbiomassYI_summary <- summary(modelGrassbiomassYI)
modelGrassbiomassYI_summary

overall_r2GrassbiomassYI <- modelGrassbiomassYI_summary$r.squared
overall_r2GrassbiomassYI

#*12* Grassbiomass VS SR680_R430 with species interaction
modelGrassbiomassSR680_R430 <- lm(Grassbiomass ~ SR680_R430 * Species, data = data)
modelGrassbiomassSR680_R430
modelGrassbiomassSR680_R430_summary <- summary(modelGrassbiomassSR680_R430)
modelGrassbiomassSR680_R430_summary

overall_r2GrassbiomassSR680_R430 <- modelGrassbiomassSR680_R430_summary$r.squared
overall_r2GrassbiomassSR680_R430

#*13* Grassbiomass VS SWVI with species interaction
modelGrassbiomassSWVI <- lm(Grassbiomass ~ SWVI * Species, data = data)
modelGrassbiomassSWVI
modelGrassbiomassSWVI_summary <- summary(modelGrassbiomassSWVI)
modelGrassbiomassSWVI_summary

overall_r2GrassbiomassSWVI <- modelGrassbiomassSWVI_summary$r.squared
overall_r2GrassbiomassSWVI

#*14* Grassbiomass VS DWSI1660_550 with species interaction
modelGrassbiomassDWSI1660_550 <- lm(Grassbiomass ~ DWSI1660_550 * Species, data = data)
modelGrassbiomassDWSI1660_550
modelGrassbiomassDWSI1660_550_summary <- summary(modelGrassbiomassDWSI1660_550)
modelGrassbiomassDWSI1660_550_summary

overall_r2GrassbiomassDWSI1660_550 <- modelGrassbiomassDWSI1660_550_summary$r.squared
overall_r2GrassbiomassDWSI1660_550

#*15* Grassbiomass VS SATVI with species interaction
modelGrassbiomassSATVI <- lm(Grassbiomass ~ SATVI * Species, data = data)
modelGrassbiomassSATVI
modelGrassbiomassSATVI_summary <- summary(modelGrassbiomassSATVI)
modelGrassbiomassSATVI_summary

overall_r2GrassbiomassSATVI <- modelGrassbiomassSATVI_summary$r.squared
overall_r2GrassbiomassSATVI

#**********************************************************************#
#**********************************************************************#
##4 TotalBiomass
#*1* TotalBiomass VS NDNI with species interaction
modelTotalBiomassNDNI <- lm(TotalBiomass ~ NDNI * Species, data = data)
modelTotalBiomassNDNI
modelTotalBiomassNDNI_summary <- summary(modelTotalBiomassNDNI)
modelTotalBiomassNDNI_summary

overall_r2TotalBiomassNDNI <- modelTotalBiomassNDNI_summary$r.squared
overall_r2TotalBiomassNDNI

#*2* TotalBiomass VS CAI with species interaction
modelTotalBiomassCAI <- lm(TotalBiomass ~ CAI * Species, data = data)
modelTotalBiomassCAI
modelTotalBiomassCAI_summary <- summary(modelTotalBiomassCAI)
modelTotalBiomassCAI_summary

overall_r2TotalBiomassCAI <- modelTotalBiomassCAI_summary$r.squared
overall_r2TotalBiomassCAI

C<- ggplot(data, aes(x = CAI, y = TotalBiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Cellulose Absorption Index (CAI)",  # Replace with your desired title
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
C<-C+ theme(legend.position = "none")
C

library(emmeans)
# Calculate species-specific slopes
emtrends(modelTotalBiomassCAI, ~ Species, var = "CAI")

# Compute R² for each species subset
r2_valuesCC <- data %>%
  group_by(Species) %>%
  summarise(
    r_squared = r2(lm(TotalBiomass ~ CAI))$R2
  )
r2_valuesCC

#*3* TotalBiomass VS PRI with species interaction
modelTotalBiomassPRI <- lm(TotalBiomass ~ PRI * Species, data = data)
modelTotalBiomassPRI
modelTotalBiomassPRI_summary <- summary(modelTotalBiomassPRI)
modelTotalBiomassPRI_summary

overall_r2TotalBiomassPRI<- modelTotalBiomassPRI_summary$r.squared
overall_r2TotalBiomassPRI

#*4* TotalBiomass VS TCARI with species interaction
modelTotalBiomassTCARI<- lm(TotalBiomass ~ TCARI * Species, data = data)
modelTotalBiomassTCARI
modelTotalBiomassTCARI_summary <- summary(modelTotalBiomassTCARI)
modelTotalBiomassTCARI_summary

overall_r2TotalBiomassTCARI<- modelTotalBiomassTCARI_summary$r.squared
overall_r2TotalBiomassTCARI

D<- ggplot(data, aes(x = TCARI, y = Grassbiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # TCARI label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
D<-D+ theme(legend.position = "none")+ annotate("text", 
                                                x = 0.06, y = 235,    # Position (adjust based on your data range)
                                                label = expression(italic(P)[int] == 0.0214), 
                                                color = "black", 
                                                size = 5)
D

library(emmeans)
# Calculate species-specific slopes
emtrends(modelTotalBiomassTCARI, ~ Species, var = "TCARI")

#*5* TotalBiomass VS MCARI2 with species interaction
modelTotalBiomassMCARI2 <- lm(TotalBiomass ~ MCARI2 * Species, data = data)
modelTotalBiomassMCARI2
modelTotalBiomassMCARI2_summary <- summary(modelTotalBiomassMCARI2)
modelTotalBiomassMCARI2_summary

overall_r2TotalBiomassMCARI2<- modelTotalBiomassMCARI2_summary$r.squared
overall_r2TotalBiomassMCARI2

E<- ggplot(data, aes(x = MCARI2, y = Grassbiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # MCARI2 label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
E<-E+ theme(legend.position = "none")
E

library(emmeans)
# Calculate species-specific slopes
emtrends(modelTotalBiomassMCARI2, ~ Species, var = "MCARI2")

#*6* TotalBiomass VS R700_R670 with species interaction
modelTotalBiomassR700_R670 <- lm(TotalBiomass ~ R700_R670 * Species, data = data)
modelTotalBiomassR700_R670
modelTotalBiomassR700_R670_summary <- summary(modelTotalBiomassR700_R670)
modelTotalBiomassR700_R670_summary

overall_r2TotalBiomassR700_R670<- modelTotalBiomassR700_R670_summary$r.squared
overall_r2TotalBiomassR700_R670

F<- ggplot(data, aes(x = R700_R670, y = Grassbiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # R700_R670 label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
F<-F+ theme(legend.position = "none")
F

library(emmeans)
# Calculate species-specific slopes
emtrends(modelTotalBiomassR700_R670, ~ Species, var = "R700_R670")

#*7* TotalBiomass VS RGR with species interaction
modelTotalBiomassRGR <- lm(TotalBiomass ~ RGR * Species, data = data)
modelTotalBiomassRGR
modelTotalBiomassRGR_summary <- summary(modelTotalBiomassRGR)
modelTotalBiomassRGR_summary

overall_r2TotalBiomassRGR<- modelTotalBiomassRGR_summary$r.squared
overall_r2TotalBiomassRGR

FF<- ggplot(data, aes(x = RGR, y = Grassbiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # RGR label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
FF<-FF+ theme(legend.position = "none")
FF

#*8* TotalBiomass VS ARI_550_700 with species interaction
modelTotalBiomassARI_550_700 <- lm(TotalBiomass ~ ARI_550_700 * Species, data = data)
modelTotalBiomassARI_550_700
modelTotalBiomassARI_550_700_summary <- summary(modelTotalBiomassARI_550_700)
modelTotalBiomassARI_550_700_summary

overall_r2TotalBiomassARI_550_700<- modelTotalBiomassARI_550_700_summary$r.squared
overall_r2TotalBiomassARI_550_700

#*9* TotalBiomass VS CRI_510_550 with species interaction
modelTotalBiomassCRI_510_550 <- lm(TotalBiomass ~ CRI_510_550 * Species, data = data)
modelTotalBiomassCRI_510_550
modelTotalBiomassCRI_510_550_summary <- summary(modelTotalBiomassCRI_510_550)
modelTotalBiomassCRI_510_550_summary

overall_r2TotalBiomassCRI_510_550<- modelTotalBiomassCRI_510_550_summary$r.squared
overall_r2TotalBiomassCRI_510_550

#*10* TotalBiomass VS NDVI_682_553 with species interaction
modelTotalBiomassNDVI_682_553 <- lm(TotalBiomass ~ NDVI_682_553 * Species, data = data)
modelTotalBiomassNDVI_682_553
modelTotalBiomassNDVI_682_553_summary <- summary(modelTotalBiomassNDVI_682_553)
modelTotalBiomassNDVI_682_553_summary

overall_r2TotalBiomassNDVI_682_553 <- modelTotalBiomassNDVI_682_553_summary$r.squared
overall_r2TotalBiomassNDVI_682_553

G<- ggplot(data, aes(x = NDVI_682_553, y = Grassbiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # NDVI_682_553 label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
G<-G+ theme(legend.position = "none")
G

library(emmeans)
# Calculate species-specific slopes
emtrends(modelTotalBiomassNDVI_682_553, ~ Species, var = "NDVI_682_553")

#*11* TotalBiomass VS YI with species interaction
modelTotalBiomassYI <- lm(TotalBiomass ~ YI * Species, data = data)
modelTotalBiomassYI
modelTotalBiomassYI_summary <- summary(modelTotalBiomassYI)
modelTotalBiomassYI_summary

overall_r2TotalBiomassYI <- modelTotalBiomassYI_summary$r.squared
overall_r2TotalBiomassYI

#*12* TotalBiomass VS SR680_R430 with species interaction
modelTotalBiomassSR680_R430 <- lm(TotalBiomass ~ SR680_R430 * Species, data = data)
modelTotalBiomassSR680_R430
modelTotalBiomassSR680_R430_summary <- summary(modelTotalBiomassSR680_R430)
modelTotalBiomassSR680_R430_summary

overall_r2TotalBiomassSR680_R430 <- modelTotalBiomassSR680_R430_summary$r.squared
overall_r2TotalBiomassSR680_R430

#*13* TotalBiomass VS SWVI with species interaction
modelTotalBiomassSWVI <- lm(TotalBiomass ~ SWVI * Species, data = data)
modelTotalBiomassSWVI
modelTotalBiomassSWVI_summary <- summary(modelTotalBiomassSWVI)
modelTotalBiomassSWVI_summary

overall_r2TotalBiomassSWVI <- modelTotalBiomassSWVI_summary$r.squared
overall_r2TotalBiomassSWVI

#*14* TotalBiomass VS DWSI1660_550 with species interaction
modelTotalBiomassDWSI1660_550 <- lm(TotalBiomass ~ DWSI1660_550 * Species, data = data)
modelTotalBiomassDWSI1660_550
modelTotalBiomassDWSI1660_550_summary <- summary(modelTotalBiomassDWSI1660_550)
modelTotalBiomassDWSI1660_550_summary

overall_r2TotalBiomassDWSI1660_550 <- modelTotalBiomassDWSI1660_550_summary$r.squared
overall_r2TotalBiomassDWSI1660_550

#*15* TotalBiomass VS SATVI with species interaction
modelTotalBiomassSATVI <- lm(TotalBiomass ~ SATVI * Species, data = data)
modelTotalBiomassSATVI
modelTotalBiomassSATVI_summary <- summary(modelTotalBiomassSATVI)
modelTotalBiomassSATVI_summary

overall_r2TotalBiomassSATVI <- modelTotalBiomassSATVI_summary$r.squared
overall_r2TotalBiomassSATVI

#**********************************************************************#
#**********************************************************************#

##5 LAI
#*1* LAI VS NDNI with species interaction
modelLAINDNI <- lm(LAI ~ NDNI * Species, data = data)
modelLAINDNI
modelLAINDNI_summary <- summary(modelLAINDNI)
modelLAINDNI_summary

overall_r2LAINDNI <- modelLAINDNI_summary$r.squared
overall_r2LAINDNI

H<- ggplot(data, aes(x = NDNI, y = Grassbiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # NDNI label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
H<-H+ theme(legend.position = "none")
H

library(emmeans)
# Calculate species-specific slopes
emtrends(modelLAINDNI, ~ Species, var = "NDNI")

#*2* LAI VS CAI with species interaction
modelLAICAI <- lm(LAI ~ CAI * Species, data = data)
modelLAICAI
modelLAICAI_summary <- summary(modelLAICAI)
modelLAICAI_summary

overall_r2modelLAICAI <- modelLAICAI_summary$r.squared
overall_r2modelLAICAI

I<- ggplot(data, aes(x = CAI, y = LAI, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Cellulose Absorption Index (CAI)",  # Replace with your desired title
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
I<-I+ theme(legend.position = "none")

I

library(emmeans)
# Calculate species-specific slopes
emtrends(modelLAICAI, ~ Species, var = "CAI")

# Compute R² for each species subset
r2_valuesII <- data %>%
  group_by(Species) %>%
  summarise(
    r_squared = r2(lm(LAI ~ CAI))$R2
  )
r2_valuesII

#*3* LAI VS PRI with species interaction
modelLAIPRI <- lm(LAI ~ PRI * Species, data = data)
modelLAIPRI
modelLAIPRI_summary <- summary(modelLAIPRI)
modelLAIPRI_summary

overall_r2modelLAIPRI <- modelLAIPRI_summary$r.squared
overall_r2modelLAIPRI

J<- ggplot(data, aes(x = PRI, y = LAI, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # PRI label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
J<-J+ theme(legend.position = "none")

library(emmeans)
# Calculate species-specific slopes
emtrends(modelLAIPRI, ~ Species, var = "PRI")

#*4* LAI VS TCARI with species interaction
modelLAITCARI<- lm(LAI ~ TCARI * Species, data = data)
modelLAITCARI
modelLAITCARI_summary <- summary(modelLAITCARI)
modelLAITCARI_summary

overall_r2modelLAITCARI <- modelLAITCARI_summary$r.squared
overall_r2modelLAITCARI

#*5* LAI VS MCARI2 with species interaction
modelLAIMCARI2 <- lm(LAI ~ MCARI2 * Species, data = data)
modelLAIMCARI2
modelLAIMCARI2_summary <- summary(modelLAIMCARI2)
modelLAIMCARI2_summary

overall_r2modelLAIMCARI2 <- modelLAIMCARI2_summary$r.squared
overall_r2modelLAIMCARI2


#*6* LAI VS R700_R670 with species interaction
modelLAIR700_R670 <- lm(LAI ~ R700_R670 * Species, data = data)
modelLAIR700_R670
modelLAIR700_R670_summary <- summary(modelLAIR700_R670)
modelLAIR700_R670_summary

overall_r2modelLAIR700_R670<- modelLAIR700_R670_summary$r.squared
overall_r2modelLAIR700_R670

#*7* LAI VS RGR with species interaction
modelLAIRGR <- lm(LAI ~ RGR * Species, data = data)
modelLAIRGR
modelLAIRGR_summary <- summary(modelLAIRGR)
modelLAIRGR_summary

overall_r2modelLAIRGR<- modelLAIRGR_summary$r.squared
overall_r2modelLAIRGR

KK<- ggplot(data, aes(x = RGR, y = LAI, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # RGR label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
KK<-KK+ theme(legend.position = "none")+ annotate("text", 
                                                  x = 1.1, y = 2.85,    # Position (adjust based on your data range)
                                                  label = expression(italic(P)[int] == 0.0299), 
                                                  color = "black", 
                                                  size = 5)
KK

#*8* LAI VS ARI_550_700 with species interaction
modelLAIARI_550_700 <- lm(LAI ~ ARI_550_700 * Species, data = data)
modelLAIARI_550_700
modelLAIARI_550_700_summary <- summary(modelLAIARI_550_700)
modelLAIARI_550_700_summary

overall_r2modelLAIARI_550_700<- modelLAIARI_550_700_summary$r.squared
overall_r2modelLAIARI_550_700

#*9* LAI VS CRI_510_550 with species interaction
modelLAICRI_510_550 <- lm(LAI ~ CRI_510_550 * Species, data = data)
modelLAICRI_510_550
modelLAICRI_510_550_summary <- summary(modelLAICRI_510_550)
modelLAICRI_510_550_summary

overall_r2modelLAICRI_510_550<- modelLAICRI_510_550_summary$r.squared
overall_r2modelLAICRI_510_550

#*10* LAI VS NDVI_682_553 with species interaction
modelLAINDVI_682_553 <- lm(LAI ~ NDVI_682_553 * Species, data = data)
modelLAINDVI_682_553
modelLAINDVI_682_553_summary <- summary(modelLAINDVI_682_553)
modelLAINDVI_682_553_summary

overall_r2modelLAINDVI_682_553<- modelLAINDVI_682_553_summary$r.squared
overall_r2modelLAINDVI_682_553

K<- ggplot(data, aes(x = NDVI_682_553, y = LAI, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # NDVI_682_553 label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
K<-K+ theme(legend.position = "none")

library(emmeans)
# Calculate species-specific slopes
emtrends(modelLAINDVI_682_553, ~ Species, var = "NDVI_682_553")

#*11* LAI VS YI with species interaction
modelLAIYI <- lm(LAI ~ YI * Species, data = data)
modelLAIYI
modelLAIYI_summary <- summary(modelLAIYI)
modelLAIYI_summary

overall_r2LAIYI <- modelLAIYI_summary$r.squared
overall_r2LAIYI

#*12* LAI VS SR680_R430 with species interaction
modelLAISR680_R430 <- lm(LAI ~ SR680_R430 * Species, data = data)
modelLAISR680_R430
modelLAISR680_R430_summary <- summary(modelLAISR680_R430)
modelLAISR680_R430_summary

overall_r2LAISR680_R430 <- modelLAISR680_R430_summary$r.squared
overall_r2LAISR680_R430

#*13* LAI VS SWVI with species interaction
modelLAISWVI <- lm(LAI ~ SWVI * Species, data = data)
modelLAISWVI
modelLAISWVI_summary <- summary(modelLAISWVI)
modelLAISWVI_summary

overall_r2LAISWVI <- modelLAISWVI_summary$r.squared
overall_r2LAISWVI

#*14* LAI VS DWSI1660_550 with species interaction
modelLAIDWSI1660_550 <- lm(LAI ~ DWSI1660_550 * Species, data = data)
modelLAIDWSI1660_550
modelLAIDWSI1660_550_summary <- summary(modelLAIDWSI1660_550)
modelLAIDWSI1660_550_summary

overall_r2LAIDWSI1660_550 <- modelLAIDWSI1660_550_summary$r.squared
overall_r2LAIDWSI1660_550

#*15* LAI VS SATVI with species interaction
modelLAISATVI <- lm(LAI ~ SATVI * Species, data = data)
modelLAISATVI
modelLAISATVI_summary <- summary(modelLAISATVI)
modelLAISATVI_summary

overall_r2LAISATVI <- modelLAISATVI_summary$r.squared
overall_r2LAISATVI

#**********************************************************************#
#**********************************************************************#
##6 ForbsCover
#*1* ForbsCover VS NDNI with species interaction
modelForbsCoverNDNI <- lm(ForbsCover ~ NDNI * Species, data = data)
modelForbsCoverNDNI
modelForbsCoverNDNI_summary <- summary(modelForbsCoverNDNI)
modelForbsCoverNDNI_summary

overall_r2modelForbsCoverNDNI<- modelForbsCoverNDNI_summary$r.squared
overall_r2modelForbsCoverNDNI

L<- ggplot(data, aes(x = NDNI, y = ForbsCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # NDNI label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
L<-L+ theme(legend.position = "none")
L<-L+ annotate("text", 
            x = 0.130, y = 20,    # Position (adjust based on your data range)
            label = expression(italic(P)[int] == 0.00321), 
            color = "black", 
            size = 4)
L

library(emmeans)
# Calculate species-specific slopes
emtrends(modelForbsCoverNDNI, ~ Species, var = "NDNI")

#*2* ForbsCover VS CAI with species interaction
modelForbsCoverCAI <- lm(ForbsCover ~ CAI * Species, data = data)
modelForbsCoverCAI
modelForbsCoverCAI_summary <- summary(modelForbsCoverCAI)
modelForbsCoverCAI_summary

overall_r2modelForbsCoverCAI<- modelForbsCoverCAI_summary$r.squared
overall_r2modelForbsCoverCAI

#*3* ForbsCover VS PRI with species interaction
modelForbsCoverPRI <- lm(ForbsCover ~ PRI * Species, data = data)
modelForbsCoverPRI
modelForbsCoverPRI_summary <- summary(modelForbsCoverPRI)
modelForbsCoverPRI_summary

overall_r2modelForbsCoverPRI<- modelForbsCoverPRI_summary$r.squared
overall_r2modelForbsCoverPRI

#*4* ForbsCover VS TCARI with species interaction
modelForbsCoverTCARI<- lm(ForbsCover ~ TCARI * Species, data = data)
modelForbsCoverTCARI
modelForbsCoverTCARI_summary <- summary(modelForbsCoverTCARI)
modelForbsCoverTCARI_summary

overall_r2modelForbsCoverTCARI<- modelForbsCoverTCARI_summary$r.squared
overall_r2modelForbsCoverTCARI

#*5* ForbsCover VS MCARI2 with species interaction
modelForbsCoverMCARI2 <- lm(ForbsCover ~ MCARI2 * Species, data = data)
modelForbsCoverMCARI2
modelForbsCoverMCARI2_summary <- summary(modelForbsCoverMCARI2)
modelForbsCoverMCARI2_summary

overall_r2modelForbsCoverMCARI2<- modelForbsCoverMCARI2_summary$r.squared
overall_r2modelForbsCoverMCARI2

#*6* ForbsCover VS R700_R670 with species interaction
modelForbsCoverR700_R670 <- lm(ForbsCover ~ R700_R670 * Species, data = data)
modelForbsCoverR700_R670
modelForbsCoverR700_R670_summary <- summary(modelForbsCoverR700_R670)
modelForbsCoverR700_R670_summary

overall_r2modelForbsCoverR700_R670<- modelForbsCoverR700_R670_summary$r.squared
overall_r2modelForbsCoverR700_R670

#*7* ForbsCover VS RGR with species interaction
modelForbsCoverRGR <- lm(ForbsCover ~ RGR * Species, data = data)
modelForbsCoverRGR
modelForbsCoverRGR_summary <- summary(modelForbsCoverRGR)
modelForbsCoverRGR_summary

overall_r2modelForbsCoverRGR<- modelForbsCoverRGR_summary$r.squared
overall_r2modelForbsCoverRGR

#*8* ForbsCover VS ARI_550_700 with species interaction
modelForbsCoverARI_550_700 <- lm(ForbsCover ~ ARI_550_700 * Species, data = data)
modelForbsCoverARI_550_700
modelForbsCoverARI_550_700_summary <- summary(modelForbsCoverARI_550_700)
modelForbsCoverARI_550_700_summary

overall_r2modelForbsCoverARI_550_700<- modelForbsCoverARI_550_700_summary$r.squared
overall_r2modelForbsCoverARI_550_700

M<- ggplot(data, aes(x = ARI_550_700, y = ForbsCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # ARI_550_700 label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
M<-M+ theme(legend.position = "none")+annotate("text", 
                                               x = 4, y = 20,    # Position (adjust based on your data range)
                                               label = expression(italic(P)[int] == 0.00939), 
                                               color = "black", 
                                               size = 5)
M

library(emmeans)
# Calculate species-specific slopes
emtrends(modelForbsCoverARI_550_700, ~ Species, var = "ARI_550_700")

#*9* ForbsCover VS CRI_510_550 with species interaction
modelForbsCoverCRI_510_550 <- lm(ForbsCover ~ CRI_510_550 * Species, data = data)
modelForbsCoverCRI_510_550
modelForbsCoverCRI_510_550_summary <- summary(modelForbsCoverCRI_510_550)
modelForbsCoverCRI_510_550_summary

overall_r2modelForbsCoverCRI_510_550<- modelForbsCoverCRI_510_550_summary$r.squared
overall_r2modelForbsCoverCRI_510_550

#*10* ForbsCover VS NDVI_682_553 with species interaction
modelForbsCoverNDVI_682_553 <- lm(ForbsCover ~ NDVI_682_553 * Species, data = data)
modelForbsCoverNDVI_682_553
modelForbsCoverNDVI_682_553_summary <- summary(modelForbsCoverNDVI_682_553)
modelForbsCoverNDVI_682_553_summary

overall_r2modelForbsCoverNDVI_682_553<- modelForbsCoverNDVI_682_553_summary$r.squared
overall_r2modelForbsCoverNDVI_682_553

#*11* ForbsCover VS YI with species interaction
modelForbsCoverYI <- lm(ForbsCover ~ YI * Species, data = data)
modelForbsCoverYI
modelForbsCoverYI_summary <- summary(modelForbsCoverYI)
modelForbsCoverYI_summary

overall_r2ForbsCoverYI <- modelForbsCoverYI_summary$r.squared
overall_r2ForbsCoverYI

#*12* ForbsCover VS SR680_R430 with species interaction
modelForbsCoverSR680_R430 <- lm(ForbsCover ~ SR680_R430 * Species, data = data)
modelForbsCoverSR680_R430
modelForbsCoverSR680_R430_summary <- summary(modelForbsCoverSR680_R430)
modelForbsCoverSR680_R430_summary

overall_r2ForbsCoverSR680_R430 <- modelForbsCoverSR680_R430_summary$r.squared
overall_r2ForbsCoverSR680_R430

#*13* ForbsCover VS SWVI with species interaction
modelForbsCoverSWVI <- lm(ForbsCover ~ SWVI * Species, data = data)
modelForbsCoverSWVI
modelForbsCoverSWVI_summary <- summary(modelForbsCoverSWVI)
modelForbsCoverSWVI_summary

overall_r2ForbsCoverSWVI <- modelForbsCoverSWVI_summary$r.squared
overall_r2ForbsCoverSWVI

MM<- ggplot(data, aes(x = SWVI, y = ForbsCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # ARI_550_700 label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
MM<-MM+ theme(legend.position = "none")
MM

#*14* ForbsCover VS DWSI1660_550 with species interaction
modelForbsCoverDWSI1660_550 <- lm(ForbsCover ~ DWSI1660_550 * Species, data = data)
modelForbsCoverDWSI1660_550
modelForbsCoverDWSI1660_550_summary <- summary(modelForbsCoverDWSI1660_550)
modelForbsCoverDWSI1660_550_summary

overall_r2ForbsCoverDWSI1660_550 <- modelForbsCoverDWSI1660_550_summary$r.squared
overall_r2ForbsCoverDWSI1660_550

MMM<- ggplot(data, aes(x = DWSI1660_550, y = ForbsCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # ARI_550_700 label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
MMM<-MMM+ theme(legend.position = "none")
MMM

#*15* ForbsCover VS SATVI with species interaction
modelForbsCoverSATVI <- lm(ForbsCover ~ SATVI * Species, data = data)
modelForbsCoverSATVI
modelForbsCoverSATVI_summary <- summary(modelForbsCoverSATVI)
modelForbsCoverSATVI_summary

overall_r2ForbsCoverSATVI <- modelForbsCoverSATVI_summary$r.squared
overall_r2ForbsCoverSATVI

#**********************************************************************#
#**********************************************************************#

##7 BareGroundCover
#*1* BareGroundCover VS NDNI with species interaction
modelBareGroundCoverNDNI <- lm(BareGroundCover ~ NDNI * Species, data = data)
modelBareGroundCoverNDNI
modelBareGroundCoverNDNI_summary <- summary(modelBareGroundCoverNDNI)
modelBareGroundCoverNDNI_summary

overall_r2modelBareGroundCoverNDNI<- modelBareGroundCoverNDNI_summary$r.squared
overall_r2modelBareGroundCoverNDNI

#*2* BareGroundCover VS CAI with species interaction
modelBareGroundCoverCAI <- lm(BareGroundCover ~ CAI * Species, data = data)
modelBareGroundCoverCAI
modelBareGroundCoverCAI_summary <- summary(modelBareGroundCoverCAI)
modelBareGroundCoverCAI_summary

overall_r2modelBareGroundCoverCAI<- modelBareGroundCoverCAI_summary$r.squared
overall_r2modelBareGroundCoverCAI

N <- ggplot(data, aes(x = CAI, y = BareGroundCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  # Add/modify axis titles here
  labs(
    x = "Cellulose Absorption Index (CAI)",  # Replace with your desired title
    y = "Bare Ground (%)"   # Replace with your desired title
  ) +
  
  theme_bw() +  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white"),
    axis.title.x = element_text(size = 18),  # Controls X-axis title size
    axis.title.y = element_text(size = 18),  # Controls Y-axis title size
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  ) +
  theme(legend.position = "none")

# Display the plot
N

library(emmeans)
# Calculate species-specific slopes
emtrends(modelBareGroundCoverCAI, ~ Species, var = "CAI")

# Compute R² for each species subset
r2_valuesNN <- data %>%
  group_by(Species) %>%
  summarise(
    r_squared = r2(lm(BareGroundCover ~ CAI))$R2
  )
r2_valuesNN


#*3* BareGroundCover VS PRI with species interaction
modelBareGroundCoverPRI <- lm(BareGroundCover ~ PRI * Species, data = data)
modelBareGroundCoverPRI
modelBareGroundCoverPRI_summary <- summary(modelBareGroundCoverPRI)
modelBareGroundCoverPRI_summary

overall_r2modelBareGroundCoverPRI<- modelBareGroundCoverPRI_summary$r.squared
overall_r2modelBareGroundCoverPRI

#*4* BareGroundCover VS TCARI with species interaction
modelBareGroundCoverTCARI<- lm(BareGroundCover ~ TCARI * Species, data = data)
modelBareGroundCoverTCARI
modelBareGroundCoverTCARI_summary <- summary(modelBareGroundCoverTCARI)
modelBareGroundCoverTCARI_summary

overall_r2modelBareGroundCoverTCARI<- modelBareGroundCoverTCARI_summary$r.squared
overall_r2modelBareGroundCoverTCARI

O<- ggplot(data, aes(x = TCARI, y = BareGroundCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # TCARI label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
O<-O+ theme(legend.position = "none")

library(emmeans)
# Calculate species-specific slopes
emtrends(modelBareGroundCoverTCARI, ~ Species, var = "TCARI")

#*5* BareGroundCover VS MCARI2 with species interaction
modelBareGroundCoverMCARI2 <- lm(BareGroundCover ~ MCARI2 * Species, data = data)
modelBareGroundCoverMCARI2
modelBareGroundCoverMCARI2_summary <- summary(modelBareGroundCoverMCARI2)
modelBareGroundCoverMCARI2_summary

overall_r2modelBareGroundCoverMCARI2<- modelBareGroundCoverMCARI2_summary$r.squared
overall_r2modelBareGroundCoverMCARI2

P<- ggplot(data, aes(x = MCARI2, y = BareGroundCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # MCARI2 label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
P<-P+ theme(legend.position = "none")

library(emmeans)
# Calculate species-specific slopes
emtrends(modelBareGroundCoverMCARI2, ~ Species, var = "MCARI2")

#*6* BareGroundCover VS R700_R670 with species interaction
modelBareGroundCoverR700_R670 <- lm(BareGroundCover ~ R700_R670 * Species, data = data)
modelBareGroundCoverR700_R670
modelBareGroundCoverR700_R670_summary <- summary(modelBareGroundCoverR700_R670)
modelBareGroundCoverR700_R670_summary

overall_r2modelBareGroundCoverR700_R670<- modelBareGroundCoverR700_R670_summary$r.squared
overall_r2modelBareGroundCoverR700_R670

#*7* BareGroundCover VS RGR with species interaction
modelBareGroundCoverRGR <- lm(BareGroundCover ~ RGR * Species, data = data)
modelBareGroundCoverRGR
modelBareGroundCoverRGR_summary <- summary(modelBareGroundCoverRGR)
modelBareGroundCoverRGR_summary

overall_r2modelBareGroundCoverRGR<- modelBareGroundCoverRGR_summary$r.squared
overall_r2modelBareGroundCoverRGR


PP<- ggplot(data, aes(x = RGR, y = BareGroundCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # RGR label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
PP<-PP+ theme(legend.position = "none")
PP

library(emmeans)
# Calculate species-specific slopes
emtrends(modelBareGroundCoverRGR, ~ Species, var = "RGR")

#*8* BareGroundCover VS ARI_550_700 with species interaction
modelBareGroundCoverARI_550_700 <- lm(BareGroundCover ~ ARI_550_700 * Species, data = data)
modelBareGroundCoverARI_550_700
modelBareGroundCoverARI_550_700_summary <- summary(modelBareGroundCoverARI_550_700)
modelBareGroundCoverARI_550_700_summary

overall_r2modelBareGroundCoverARI_550_700<- modelBareGroundCoverARI_550_700_summary$r.squared
overall_r2modelBareGroundCoverARI_550_700

#*9* BareGroundCover VS CRI_510_550 with species interaction
modelBareGroundCoverCRI_510_550 <- lm(BareGroundCover ~ CRI_510_550 * Species, data = data)
modelBareGroundCoverCRI_510_550
modelBareGroundCoverCRI_510_550_summary <- summary(modelBareGroundCoverCRI_510_550)
modelBareGroundCoverCRI_510_550_summary

overall_r2modelBareGroundCoverCRI_510_550<- modelBareGroundCoverCRI_510_550_summary$r.squared
overall_r2modelBareGroundCoverCRI_510_550

#*10* BareGroundCover VS NDVI_682_553 with species interaction
modelBareGroundCoverNDVI_682_553 <- lm(BareGroundCover ~ NDVI_682_553 * Species, data = data)
modelBareGroundCoverNDVI_682_553
modelBareGroundCoverNDVI_682_553_summary <- summary(modelBareGroundCoverNDVI_682_553)
modelBareGroundCoverNDVI_682_553_summary

overall_r2modelBareGroundCoverNDVI_682_553<- modelBareGroundCoverNDVI_682_553_summary$r.squared
overall_r2modelBareGroundCoverNDVI_682_553

Q<- ggplot(data, aes(x = NDVI_682_553, y = BareGroundCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # NDVI_682_553 label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
Q<-Q+ theme(legend.position = "none")

library(emmeans)
# Calculate species-specific slopes
emtrends(modelBareGroundCoverNDVI_682_553, ~ Species, var = "NDVI_682_553")

#*11* BareGroundCover VS YI with species interaction
modelBareGroundCoverYI <- lm(BareGroundCover ~ YI * Species, data = data)
modelBareGroundCoverYI
modelBareGroundCoverYI_summary <- summary(modelBareGroundCoverYI)
modelBareGroundCoverYI_summary

overall_r2BareGroundCoverYI <- modelBareGroundCoverYI_summary$r.squared
overall_r2BareGroundCoverYI

#*12* BareGroundCover VS SR680_R430 with species interaction
modelBareGroundCoverSR680_R430 <- lm(BareGroundCover ~ SR680_R430 * Species, data = data)
modelBareGroundCoverSR680_R430
modelBareGroundCoverSR680_R430_summary <- summary(modelBareGroundCoverSR680_R430)
modelBareGroundCoverSR680_R430_summary

overall_r2BareGroundCoverSR680_R430 <- modelBareGroundCoverSR680_R430_summary$r.squared
overall_r2BareGroundCoverSR680_R430

#*13* BareGroundCover VS SWVI with species interaction
modelBareGroundCoverSWVI <- lm(BareGroundCover ~ SWVI * Species, data = data)
modelBareGroundCoverSWVI
modelBareGroundCoverSWVI_summary <- summary(modelBareGroundCoverSWVI)
modelBareGroundCoverSWVI_summary

overall_r2BareGroundCoverSWVI <- modelBareGroundCoverSWVI_summary$r.squared
overall_r2BareGroundCoverSWVI

#*14* BareGroundCover VS DWSI1660_550 with species interaction
modelBareGroundCoverDWSI1660_550 <- lm(BareGroundCover ~ DWSI1660_550 * Species, data = data)
modelBareGroundCoverDWSI1660_550
modelBareGroundCoverDWSI1660_550_summary <- summary(modelBareGroundCoverDWSI1660_550)
modelBareGroundCoverDWSI1660_550_summary

overall_r2BareGroundCoverDWSI1660_550 <- modelBareGroundCoverDWSI1660_550_summary$r.squared
overall_r2BareGroundCoverDWSI1660_550

QQ<- ggplot(data, aes(x = DWSI1660_550, y = BareGroundCover, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # DWSI1660_550 label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
QQ<-QQ+ theme(legend.position = "none")+theme(legend.position = "none")+annotate("text", 
                                                                                x = 4.2, y = 15,    # Position (adjust based on your data range)
                                                                                label = expression(italic(P)[int] == 0.0193), 
                                                                                color = "black", 
                                                                                size = 5)
QQ

library(emmeans)
# Calculate species-specific slopes
emtrends(modelBareGroundCoverDWSI1660_550, ~ Species, var = "DWSI1660_550")

#*15* BareGroundCover VS SATVI with species interaction
modelBareGroundCoverSATVI <- lm(BareGroundCover ~ SATVI * Species, data = data)
modelBareGroundCoverSATVI
modelBareGroundCoverSATVI_summary <- summary(modelBareGroundCoverSATVI)
modelBareGroundCoverSATVI_summary

overall_r2BareGroundCoverSATVI <- modelBareGroundCoverSATVI_summary$r.squared
overall_r2BareGroundCoverSATVI

#**********************************************************************#
#**********************************************************************#

##8 DeadBiomass
#*1* DeadBiomass VS NDNI with species interaction
modelDeadBiomassNDNI <- lm(DeadBiomass ~ NDNI * Species, data = data)
modelDeadBiomassNDNI
modelDeadBiomassNDNI_summary <- summary(modelDeadBiomassNDNI)
modelDeadBiomassNDNI_summary

overall_r2modelDeadBiomassNDNI<- modelDeadBiomassNDNI_summary$r.squared
overall_r2modelDeadBiomassNDNI

#*2* DeadBiomass VS CAI with species interaction
modelDeadBiomassCAI <- lm(DeadBiomass ~ CAI * Species, data = data)
modelDeadBiomassCAI
modelDeadBiomassCAI_summary <- summary(modelDeadBiomassCAI)
modelDeadBiomassCAI_summary

overall_r2modelDeadBiomassCAI<- modelDeadBiomassCAI_summary$r.squared
overall_r2modelDeadBiomassCAI

#*3* DeadBiomass VS PRI with species interaction
modelDeadBiomassPRI <- lm(DeadBiomass ~ PRI * Species, data = data)
modelDeadBiomassPRI
modelDeadBiomassPRI_summary <- summary(modelDeadBiomassPRI)
modelDeadBiomassPRI_summary

overall_r2modelDeadBiomassPRI<- modelDeadBiomassPRI_summary$r.squared
overall_r2modelDeadBiomassPRI

#*4* DeadBiomass VS TCARI with species interaction
modelDeadBiomassTCARI<- lm(DeadBiomass ~ TCARI * Species, data = data)
modelDeadBiomassTCARI
modelDeadBiomassTCARI_summary <- summary(modelDeadBiomassTCARI)
modelDeadBiomassTCARI_summary

overall_r2modelDeadBiomassTCARI<- modelDeadBiomassTCARI_summary$r.squared
overall_r2modelDeadBiomassTCARI

#*5* DeadBiomass VS MCARI2 with species interaction
modelDeadBiomassMCARI2 <- lm(DeadBiomass ~ MCARI2 * Species, data = data)
modelDeadBiomassMCARI2
modelDeadBiomassMCARI2_summary <- summary(modelDeadBiomassMCARI2)
modelDeadBiomassMCARI2_summary

overall_r2modelDeadBiomassMCARI2<- modelDeadBiomassMCARI2_summary$r.squared
overall_r2modelDeadBiomassMCARI2

#*6* DeadBiomass VS R700_R670 with species interaction
modelDeadBiomassR700_R670 <- lm(DeadBiomass ~ R700_R670 * Species, data = data)
modelDeadBiomassR700_R670
modelDeadBiomassR700_R670_summary <- summary(modelDeadBiomassR700_R670)
modelDeadBiomassR700_R670_summary

overall_r2modelDeadBiomassR700_R670<- modelDeadBiomassR700_R670_summary$r.squared
overall_r2modelDeadBiomassR700_R670

#*7* DeadBiomass VS RGR with species interaction
modelDeadBiomassRGR <- lm(DeadBiomass ~ RGR * Species, data = data)
modelDeadBiomassRGR
modelDeadBiomassRGR_summary <- summary(modelDeadBiomassRGR)
modelDeadBiomassRGR_summary

overall_r2modelDeadBiomassRGR<- modelDeadBiomassRGR_summary$r.squared
overall_r2modelDeadBiomassRGR

#*8* DeadBiomass VS ARI_550_700 with species interaction
modelDeadBiomassARI_550_700 <- lm(DeadBiomass ~ ARI_550_700 * Species, data = data)
modelDeadBiomassARI_550_700
modelDeadBiomassARI_550_700_summary <- summary(modelDeadBiomassARI_550_700)
modelDeadBiomassARI_550_700_summary

overall_r2modelDeadBiomassARI_550_700<- modelDeadBiomassARI_550_700_summary$r.squared
overall_r2modelDeadBiomassARI_550_700

#*9* DeadBiomass VS CRI_510_550 with species interaction
modelDeadBiomassCRI_510_550 <- lm(DeadBiomass ~ CRI_510_550 * Species, data = data)
modelDeadBiomassCRI_510_550
modelDeadBiomassCRI_510_550_summary <- summary(modelDeadBiomassCRI_510_550)
modelDeadBiomassCRI_510_550_summary

overall_r2modelDeadBiomassCRI_510_550<- modelDeadBiomassCRI_510_550_summary$r.squared
overall_r2modelDeadBiomassCRI_510_550

#*10* DeadBiomass VS NDVI_682_553 with species interaction
modelDeadBiomassNDVI_682_553 <- lm(DeadBiomass ~ NDVI_682_553 * Species, data = data)
modelDeadBiomassNDVI_682_553
modelDeadBiomassNDVI_682_553_summary <- summary(modelDeadBiomassNDVI_682_553)
modelDeadBiomassNDVI_682_553_summary

overall_r2modelDeadBiomassNDVI_682_553<- modelDeadBiomassNDVI_682_553_summary$r.squared
overall_r2modelDeadBiomassNDVI_682_553

#*11* DeadBiomass VS YI with species interaction
modelDeadBiomassYI <- lm(DeadBiomass ~ YI * Species, data = data)
modelDeadBiomassYI
modelDeadBiomassYI_summary <- summary(modelDeadBiomassYI)
modelDeadBiomassYI_summary

overall_r2DeadBiomassYI <- modelDeadBiomassYI_summary$r.squared
overall_r2DeadBiomassYI

#*12* DeadBiomass VS SR680_R430 with species interaction
modelDeadBiomassSR680_R430 <- lm(DeadBiomass ~ SR680_R430 * Species, data = data)
modelDeadBiomassSR680_R430
modelDeadBiomassSR680_R430_summary <- summary(modelDeadBiomassSR680_R430)
modelDeadBiomassSR680_R430_summary

overall_r2DeadBiomassSR680_R430 <- modelDeadBiomassSR680_R430_summary$r.squared
overall_r2DeadBiomassSR680_R430

#*13* DeadBiomass VS SWVI with species interaction
modelDeadBiomassSWVI <- lm(DeadBiomass ~ SWVI * Species, data = data)
modelDeadBiomassSWVI
modelDeadBiomassSWVI_summary <- summary(modelDeadBiomassSWVI)
modelDeadBiomassSWVI_summary

overall_r2DeadBiomassSWVI <- modelDeadBiomassSWVI_summary$r.squared
overall_r2DeadBiomassSWVI

#*14* DeadBiomass VS DWSI1660_550 with species interaction
modelDeadBiomassDWSI1660_550 <- lm(DeadBiomass ~ DWSI1660_550 * Species, data = data)
modelDeadBiomassDWSI1660_550
modelDeadBiomassDWSI1660_550_summary <- summary(modelDeadBiomassDWSI1660_550)
modelDeadBiomassDWSI1660_550_summary

overall_r2DeadBiomassDWSI1660_550 <- modelDeadBiomassDWSI1660_550_summary$r.squared
overall_r2DeadBiomassDWSI1660_550

#*15* DeadBiomass VS SATVI with species interaction
modelDeadBiomassSATVI <- lm(DeadBiomass ~ SATVI * Species, data = data)
modelDeadBiomassSATVI
modelDeadBiomassSATVI_summary <- summary(modelDeadBiomassSATVI)
modelDeadBiomassSATVI_summary

overall_r2DeadBiomassSATVI <- modelDeadBiomassSATVI_summary$r.squared
overall_r2DeadBiomassSATVI

#**********************************************************************#
#**********************************************************************#

##9 ForbsBiomass
#*1* ForbsBiomass VS NDNI with species interaction
modelForbsBiomassNDNI <- lm(ForbsBiomass ~ NDNI * Species, data = data)
modelForbsBiomassNDNI
modelForbsBiomassNDNI_summary <- summary(modelForbsBiomassNDNI)
modelForbsBiomassNDNI_summary

overall_r2modelForbsBiomassNDNI<- modelForbsBiomassNDNI_summary$r.squared
overall_r2modelForbsBiomassNDNI

R<- ggplot(data, aes(x = NDNI, y = ForbsBiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # NDNI label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
R<-R+ theme(legend.position = "none")
R<-R+ annotate("text", 
            x = 0.127, y = 40,    # Position (adjust based on your data range)
            label = expression(italic(P)[int] == 0.00814), 
            color = "black", 
            size = 4)
R

library(emmeans)
# Calculate species-specific slopes
emtrends(modelForbsBiomassNDNI, ~ Species, var = "NDNI")

#*2* ForbsBiomass VS CAI with species interaction
modelForbsBiomassCAI <- lm(ForbsBiomass ~ CAI * Species, data = data)
modelForbsBiomassCAI
modelForbsBiomassCAI_summary <- summary(modelForbsBiomassCAI)
modelForbsBiomassCAI_summary

overall_r2modelForbsBiomassCAI<- modelForbsBiomassCAI_summary$r.squared
overall_r2modelForbsBiomassCAI

#*3* ForbsBiomass VS PRI with species interaction
modelForbsBiomassPRI <- lm(ForbsBiomass ~ PRI * Species, data = data)
modelForbsBiomassPRI
modelForbsBiomassPRI_summary <- summary(modelForbsBiomassPRI)
modelForbsBiomassPRI_summary

overall_r2modelForbsBiomassPRI<- modelForbsBiomassPRI_summary$r.squared
overall_r2modelForbsBiomassPRI

library(emmeans)
# Calculate species-specific slopes
emtrends(modelForbsBiomassPRI, ~ Species, var = "PRI")

#*4* ForbsBiomass VS TCARI with species interaction
modelForbsBiomassTCARI<- lm(ForbsBiomass ~ TCARI * Species, data = data)
modelForbsBiomassTCARI
modelForbsBiomassTCARI_summary <- summary(modelForbsBiomassTCARI)
modelForbsBiomassTCARI_summary

overall_r2modelForbsBiomassTCARI<- modelForbsBiomassTCARI_summary$r.squared
overall_r2modelForbsBiomassTCARI

#*5* ForbsBiomass VS MCARI2 with species interaction
modelForbsBiomassMCARI2 <- lm(ForbsBiomass ~ MCARI2 * Species, data = data)
modelForbsBiomassMCARI2
modelForbsBiomassMCARI2_summary <- summary(modelForbsBiomassMCARI2)
modelForbsBiomassMCARI2_summary

overall_r2modelForbsBiomassMCARI2<- modelForbsBiomassMCARI2_summary$r.squared
overall_r2modelForbsBiomassMCARI2

#*6* ForbsBiomass VS R700_R670 with species interaction
modelForbsBiomassR700_R670 <- lm(ForbsBiomass ~ R700_R670 * Species, data = data)
modelForbsBiomassR700_R670
modelForbsBiomassR700_R670_summary <- summary(modelForbsBiomassR700_R670)
modelForbsBiomassR700_R670_summary

overall_r2modelForbsBiomassR700_R670_summary<- modelForbsBiomassR700_R670_summary$r.squared
overall_r2modelForbsBiomassR700_R670_summary

#*7* ForbsBiomass VS RGR with species interaction
modelForbsBiomassRGR <- lm(ForbsBiomass ~ RGR * Species, data = data)
modelForbsBiomassRGR
modelForbsBiomassRGR_summary <- summary(modelForbsBiomassRGR)
modelForbsBiomassRGR_summary

overall_r2modelForbsBiomassRGR<- modelForbsBiomassRGR_summary$r.squared
overall_r2modelForbsBiomassRGR

#*8* ForbsBiomass VS ARI_550_700 with species interaction
modelForbsBiomassARI_550_700 <- lm(ForbsBiomass ~ ARI_550_700 * Species, data = data)
modelForbsBiomassARI_550_700
modelForbsBiomassARI_550_700_summary <- summary(modelForbsBiomassARI_550_700)
modelForbsBiomassARI_550_700_summary

overall_r2modelForbsBiomassARI_550_700<- modelForbsBiomassARI_550_700_summary$r.squared
overall_r2modelForbsBiomassARI_550_700

T<- ggplot(data, aes(x = ARI_550_700, y = ForbsBiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # ARI_550_700 label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
T<-T+ theme(legend.position = "none")+annotate("text", 
                                              x = 4, y = 40,    # Position (adjust based on your data range)
                                              label = expression(italic(P)[int] == 0.00389), 
                                              color = "black", 
                                              size = 5)
T

library(emmeans)
# Calculate species-specific slopes
emtrends(modelForbsBiomassARI_550_700, ~ Species, var = "ARI_550_700")

#*9* ForbsBiomass VS CRI_510_550 with species interaction
modelForbsBiomassCRI_510_550 <- lm(ForbsBiomass ~ CRI_510_550 * Species, data = data)
modelForbsBiomassCRI_510_550
modelForbsBiomassCRI_510_550_summary <- summary(modelForbsBiomassCRI_510_550)
modelForbsBiomassCRI_510_550_summary

overall_r2modelForbsBiomassCRI_510_550<- modelForbsBiomassCRI_510_550_summary$r.squared
overall_r2modelForbsBiomassCRI_510_550

#*10* ForbsBiomass VS NDVI_682_553 with species interaction
modelForbsBiomassNDVI_682_553 <- lm(ForbsBiomass ~ NDVI_682_553 * Species, data = data)
modelForbsBiomassNDVI_682_553
modelForbsBiomassNDVI_682_553_summary <- summary(modelForbsBiomassNDVI_682_553)
modelForbsBiomassNDVI_682_553_summary

overall_r2modelForbsBiomassNDVI_682_553<- modelForbsBiomassNDVI_682_553_summary$r.squared
overall_r2modelForbsBiomassNDVI_682_553

#*11* ForbsBiomass VS YI with species interaction
modelForbsBiomassYI <- lm(ForbsBiomass ~ YI * Species, data = data)
modelForbsBiomassYI
modelForbsBiomassYI_summary <- summary(modelForbsBiomassYI)
modelForbsBiomassYI_summary

overall_r2ForbsBiomassYI <- modelForbsBiomassYI_summary$r.squared
overall_r2ForbsBiomassYI

#*12* ForbsBiomass VS SR680_R430 with species interaction
modelForbsBiomassSR680_R430 <- lm(ForbsBiomass ~ SR680_R430 * Species, data = data)
modelForbsBiomassSR680_R430
modelForbsBiomassSR680_R430_summary <- summary(modelForbsBiomassSR680_R430)
modelForbsBiomassSR680_R430_summary

overall_r2ForbsBiomassSR680_R430 <- modelForbsBiomassSR680_R430_summary$r.squared
overall_r2ForbsBiomassSR680_R430

#*13* ForbsBiomass VS SWVI with species interaction
modelForbsBiomassSWVI <- lm(ForbsBiomass ~ SWVI * Species, data = data)
modelForbsBiomassSWVI
modelForbsBiomassSWVI_summary <- summary(modelForbsBiomassSWVI)
modelForbsBiomassSWVI_summary

overall_r2ForbsBiomassSWVI <- modelForbsBiomassSWVI_summary$r.squared
overall_r2ForbsBiomassSWVI

TT<- ggplot(data, aes(x = SWVI, y = ForbsBiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # SWVI label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
TT<-TT+ theme(legend.position = "none")
TT

library(emmeans)
# Calculate species-specific slopes
emtrends(modelForbsBiomassSWVI, ~ Species, var = "SWVI")


#*14* ForbsBiomass VS DWSI1660_550 with species interaction
modelForbsBiomassDWSI1660_550 <- lm(ForbsBiomass ~ DWSI1660_550 * Species, data = data)
modelForbsBiomassDWSI1660_550
modelForbsBiomassDWSI1660_550_summary <- summary(modelForbsBiomassDWSI1660_550)
modelForbsBiomassDWSI1660_550_summary

overall_r2ForbsBiomassDWSI1660_550 <- modelForbsBiomassDWSI1660_550_summary$r.squared
overall_r2ForbsBiomassDWSI1660_550

TTT<- ggplot(data, aes(x = DWSI1660_550, y = ForbsBiomass, color = Species)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_bw() +  
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    panel.background = element_rect(fill = "white"),  # Ensure white background
    
    # Adjust axis label text size
    axis.title.x = element_text(size = 12),  # DWSI1660_550 label size
    axis.title.y = element_text(size = 12),  # Height label size
    
    # Adjust tick label text size (optional)
    axis.text.x = element_text(size = 10),   # x-axis numbers
    axis.text.y = element_text(size = 10)    # y-axis numbers
  )
TTT<-TTT+ theme(legend.position = "none")
TTT

library(emmeans)
# Calculate species-specific slopes
emtrends(modelForbsBiomassDWSI1660_550, ~ Species, var = "DWSI1660_550")

#*15* ForbsBiomass VS SATVI with species interaction
modelForbsBiomassSATVI <- lm(ForbsBiomass ~ SATVI * Species, data = data)
modelForbsBiomassSATVI
modelForbsBiomassSATVI_summary <- summary(modelForbsBiomassSATVI)
modelForbsBiomassSATVI_summary

overall_r2ForbsBiomassSATVI <- modelForbsBiomassSATVI_summary$r.squared
overall_r2ForbsBiomassSATVI

#**********************************************************************#
#**********************************************************************#

library(patchwork)
combined <- 
  (A2  | B | C) /      # Row 1
  (I |  N ) 
    +plot_layout(
    nrow = 2, 
    heights = c(1, 1), 
    guides = "collect"  # Shared legend
  )+ theme(
    legend.position = "right",           # Legend above all plots
    legend.justification = "center"    # Centers the legend
  )

combined

combined <- 
  (A2 | B | C) /      # Row 1: 3 plots side-by-side
  (I | N ) +  # Row 2: 2 plots + empty spacer to balance alignment
  plot_layout(
    nrow = 2, 
    heights = c(1, 1), 
    widths = c(1, 1, 1),  # Ensures equal width for all columns
    guides = "collect"     # Shared legend
  ) 
#+ 
#  theme(
#    legend.position = "right", 
#    legend.justification = "center",
#   legend.text = element_text(size = 25)
# )

combined

##############################################################
##############################################################
library(patchwork)
combined <- 
  (A | B | C) /      # Row 1
  (I |  N) /      # Row 2
  (  R | AA)  +  # Row 3 (centered R)
  plot_layout(
    nrow = 3, 
    heights = c(1, 1, 1), 
    guides = "collect"  # Shared legend
  )+ theme(
    legend.position = "top",           # Legend above all plots
    legend.justification = "center"    # Centers the legend
  )

combined

