## Introduction ----
# Author: Jules GRANDVILLEMIN
  # URL link: https://people.epfl.ch/jules.grandvillemin
# Date: May 20, 2025
# Encoding: UTF-8
# Style used for coding and ensure reproducibility: https://style.tidyverse.org/ #Author: Hadley Wickham

## R Version Used & Download Link ----
# R version: 2024.12.1+563
# Download RStudio: https://posit.co/download/rstudio-desktop/

### Load Required Packages and Data ----

# Download necessary packages
install.packages("dplyr")
install.packages("questionr")
install.packages("ade4")
install.packages("factoextra")
install.packages("cowplot")

# Load necessary packages
library(dplyr)
library(questionr)
library(ade4)
library(factoextra)
library(cowplot)


# Load dataset
df <- read.csv("~/Desktop/r_assignment2_jg/data/data_example_JG.csv", sep=";")

#Remark:
  # If the file path does not work, you can import the data manually using RStudio's "Import Dataset" 
  # function accessible via the Environment pane (top-right). This tool allows you to browse and load files 
  # with a GUI, and it even generates the corresponding R code for reproducibility.
  
# To view the data frame in RStudio's viewer:
View(df)


## Code Execution ----

### Data Recoding for Multiple Correspondence Analysis (MCA) ----
# Assess whether public transport is deemed suitable by respondents for various purposes

#### Purpose: Supermarket Shopping ----
# Frequency table
freq(df$Q101_1)

# Recode variable labels
df <- df %>%
  mutate(Q101_1 = case_when(
    Q101_1 == 1 ~ "Not at all suitable",
    Q101_1 == 2 ~ "Rather unsuitable",
    Q101_1 == 3 ~ "Neutral",
    Q101_1 == 4 ~ "Rather suitable",
    Q101_1 == 5 ~ "Completely suitable",
    TRUE ~ NA_character_
  ))

# Verify changes
freq(df$Q101_1)

# Remove NA values
df <- df %>%
  filter(!is.na(Q101_1))

# Rename variable
df <- df %>%
  rename(pt_to_supermarket = Q101_1)

# Convert to factor
df <- df %>%
  mutate(pt_to_supermarket = as.factor(pt_to_supermarket))

#### Purpose: Local Market Shopping ----
# Frequency table
freq(df$Q101_2)

# Recode variable labels
df <- df %>%
  mutate(Q101_2 = case_when(
    Q101_2 == 1 ~ "Not at all suitable",
    Q101_2 == 2 ~ "Rather unsuitable",
    Q101_2 == 3 ~ "Neutral",
    Q101_2 == 4 ~ "Rather suitable",
    Q101_2 == 5 ~ "Completely suitable",
    TRUE ~ NA_character_
  ))

# Verify changes
freq(df$Q101_2)

# Remove NA values
df <- df %>%
  filter(!is.na(Q101_2))

# Rename variable
df <- df %>%
  rename(pt_to_market = Q101_2)

# Convert to factor
df <- df %>%
  mutate(pt_to_market = as.factor(pt_to_market))

#### Purpose: Administrative or Medical Appointments ----
# Frequency table
freq(df$Q101_3)

# Recode variable labels
df <- df %>%
  mutate(Q101_3 = case_when(
    Q101_3 == 1 ~ "Not at all suitable",
    Q101_3 == 2 ~ "Rather unsuitable",
    Q101_3 == 3 ~ "Neutral",
    Q101_3 == 4 ~ "Rather suitable",
    Q101_3 == 5 ~ "Completely suitable",
    TRUE ~ NA_character_
  ))

# Verify changes
freq(df$Q101_3)

# Remove NA values
df <- df %>%
  filter(!is.na(Q101_3))

# Rename variable
df <- df %>%
  rename(pt_to_admin_medical = Q101_3)

# Convert to factor
df <- df %>%
  mutate(pt_to_admin_medical = as.factor(pt_to_admin_medical))

#### Purpose: Cultural Activities ----
# Frequency table
freq(df$Q101_4)

# Recode variable labels
df <- df %>%
  mutate(Q101_4 = case_when(
    Q101_4 == 1 ~ "Not at all suitable",
    Q101_4 == 2 ~ "Rather unsuitable",
    Q101_4 == 3 ~ "Neutral",
    Q101_4 == 4 ~ "Rather suitable",
    Q101_4 == 5 ~ "Completely suitable",
    TRUE ~ NA_character_
  ))

# Verify changes
freq(df$Q101_4)

# Remove NA values
df <- df %>%
  filter(!is.na(Q101_4))

# Rename variable
df <- df %>%
  rename(pt_to_cultural = Q101_4)

# Convert to factor
df <- df %>%
  mutate(pt_to_cultural = as.factor(pt_to_cultural))

#### Purpose: Nightlife Activities ----
# Frequency table
freq(df$Q101_5)

# Recode variable labels
df <- df %>%
  mutate(Q101_5 = case_when(
    Q101_5 == 1 ~ "Not at all suitable",
    Q101_5 == 2 ~ "Rather unsuitable",
    Q101_5 == 3 ~ "Neutral",
    Q101_5 == 4 ~ "Rather suitable",
    Q101_5 == 5 ~ "Completely suitable",
    TRUE ~ NA_character_
  ))

# Verify changes
freq(df$Q101_5)

# Remove NA values
df <- df %>%
  filter(!is.na(Q101_5))

# Rename variable
df <- df %>%
  rename(pt_to_nightlife = Q101_5)

# Convert to factor
df <- df %>%
  mutate(pt_to_nightlife = as.factor(pt_to_nightlife))

### Are cars considered suitable by respondents for various purposes? ----
#### Purpose: Grocery shopping at supermarket ----
# Frequency table
freq(df$Q102_1)

# Recoding response levels
df <- df %>%
  mutate(Q102_1 = case_when(
    Q102_1 == 1 ~ "Not suitable at all",
    Q102_1 == 2 ~ "Rather not suitable",
    Q102_1 == 3 ~ "Neutral",
    Q102_1 == 4 ~ "Rather suitable",
    Q102_1 == 5 ~ "Completely suitable",
    TRUE ~ NA_character_
  ))

# Check recoding results
freq(df$Q102_1)

# Remove NA category
df <- df %>%
  filter(Q102_1 != "NA")

# Verify removal
freq(df$Q102_1)

# Rename variable
df <- df %>%
  rename(car_to_supermarket = Q102_1)

# Check frequencies again
freq(df$car_to_supermarket)

# Convert to factor (note: variable name case corrected)
df <- df %>%
  mutate(car_to_supermarket = as.factor(car_to_supermarket))


#### Purpose: Grocery shopping at small shops, grocers, markets ----
freq(df$Q102_2)

df <- df %>%
  mutate(Q102_2 = case_when(
    Q102_2 == 1 ~ "Not suitable at all",
    Q102_2 == 2 ~ "Rather not suitable",
    Q102_2 == 3 ~ "Neutral",
    Q102_2 == 4 ~ "Rather suitable",
    Q102_2 == 5 ~ "Completely suitable",
    TRUE ~ NA_character_
  ))

freq(df$Q102_2)

df <- df %>%
  filter(Q102_2 != "NA")

freq(df$Q102_2)

df <- df %>%
  rename(car_to_market = Q102_2)

freq(df$car_to_market)

df <- df %>%
  mutate(car_to_market = as.factor(car_to_market))


#### Purpose: Administrative or medical errands (post office, bank, healthcare, etc.) ----
freq(df$Q102_3)

df <- df %>%
  mutate(Q102_3 = case_when(
    Q102_3 == 1 ~ "Not suitable at all",
    Q102_3 == 2 ~ "Rather not suitable",
    Q102_3 == 3 ~ "Neutral",
    Q102_3 == 4 ~ "Rather suitable",
    Q102_3 == 5 ~ "Completely suitable",
    TRUE ~ NA_character_
  ))

freq(df$Q102_3)

df <- df %>%
  filter(Q102_3 != "NA")

freq(df$Q102_3)

df <- df %>%
  rename(car_to_admin_medical = Q102_3)

freq(df$car_to_admin_medical)

df <- df %>%
  mutate(car_to_admin_medical = as.factor(car_to_admin_medical))


#### Purpose: Artistic, cultural, associative or intellectual activities (cinema, theater, museum, exhibitions, sports, language classes, etc.) ----
freq(df$Q102_4)

df <- df %>%
  mutate(Q102_4 = case_when(
    Q102_4 == 1 ~ "Not suitable at all",
    Q102_4 == 2 ~ "Rather not suitable",
    Q102_4 == 3 ~ "Neutral",
    Q102_4 == 4 ~ "Rather suitable",
    Q102_4 == 5 ~ "Completely suitable",
    TRUE ~ NA_character_
  ))

freq(df$Q102_4)

df <- df %>%
  filter(Q102_4 != "NA")

freq(df$Q102_4)

df <- df %>%
  rename(car_to_cultural = Q102_4)

freq(df$car_to_cultural)

df <- df %>%
  mutate(car_to_cultural = as.factor(car_to_cultural))


#### Purpose: Night social activities (bars, restaurants, nightclubs, etc.) ----
freq(df$Q102_5)

df <- df %>%
  mutate(Q102_5 = case_when(
    Q102_5 == 1 ~ "Not suitable at all",
    Q102_5 == 2 ~ "Rather not suitable",
    Q102_5 == 3 ~ "Neutral",
    Q102_5 == 4 ~ "Rather suitable",
    Q102_5 == 5 ~ "Completely suitable",
    TRUE ~ NA_character_
  ))

freq(df$Q102_5)

df <- df %>%
  filter(Q102_5 != "NA")

freq(df$Q102_5)

df <- df %>%
  rename(car_to_nightlife = Q102_5)

freq(df$car_to_nightlife)

df <- df %>%
  mutate(car_to_nightlife = as.factor(car_to_nightlife))


##### Accessibility variable for amenities in a given territory ----
# Example based on the methodology of Frankhauser and Bonin, 2024
# (https://books.google.ch/books?hl=fr&lr=&id=aSUVEQAAQBAJ&oi=fnd&pg=PA277...)
#
# Summary:
# - Points of interest (POIs) from OpenStreetMap were downloaded and classified
#   into 7 categories of human needs (subsistence, protection, affection, understanding,
#   participation, idleness, creation) - see Max-Neef et al., 1991 (https://www.wtf.tw/ref/max-neef.pdf)
# - Distance between respondents' home locations (confidential) and POIs computed via OpenAccess ModAccess
#   (https://thema.univ-fcomte.fr/productions/software/modaccess/fr.html)
# - Only POIs accessible within 15 minutes retained, to model a "15-minute city" scenario,
#   favoring decarbonized transport modes like walking, biking, public transit (see Moreno et al., 2021)
# - Accessibility score reflects the number of accessible needs categories weighted by theoretical
#   daily usage frequency (rare, occasional, frequent, proximity), see Frankhauser and Bonin, 2024 for details

## Accessibility indicator score frequency
freq(df$accessibility_indicator)
# Note:
#   The accessibility indicator scores range from 0 to 0.48,
#   where 0 is the minimum accessibility score,
#   and 0.48 is the maximum score,
#   corresponding to the highest number of amenities accessible within 15 minutes,
#   potentially satisfying the greatest number of needs among the 7 categories mentioned,
#   and likely to be frequently visited by respondents.

## Analyses ----

### Multiple Correspondence Analysis (MCA) ----

# Install ade4 package if not already installed
# install.packages("ade4", dependencies = TRUE)

# Select variables of interest for MCA
d_acm <- df[, c(
  "pt_to_supermarket", 
  "pt_to_market", 
  "pt_to_admin_medical", 
  "pt_to_nightlife", 
  "pt_to_cultural", 
  "car_to_supermarket", 
  "car_to_market", 
  "car_to_admin_medical",
  "car_to_nightlife", 
  "car_to_cultural"
)]

# Check structure of selected data
str(d_acm)

# Run MCA with dudi.acm, keeping 2 principal dimensions (nf = 2)
acm <- dudi.acm(df = d_acm, scannf = FALSE, nf = 2)

# Visualize variables in MCA space with repulsion to avoid overlap
fviz_mca_var(acm, repel = TRUE)
# MCA plot interpretation:
#   This MCA (Multiple Correspondence Analysis) plot displays the distribution of variable categories
#   across the first two dimensions (Dim1 = 12.6% of explained variance, Dim2 = 9.3%).
#   Each point represents a category of transport suitability (car or public transport)
#   for different activities (e.g., supermarket, market, cultural, administrative, nightlife).
#   Categories close to each other are perceived similarly by respondents.
#     For instance:
#     - "Not at all suitable" by public transport for most amenities is clustered on the top-left,
#     - While "Completely suitable" by car is mostly located at the bottom-left.
#     - Moderate or neutral responses are more dispersed and tend to cluster near the origin.
#     This suggests a polarity between transport modes and levels of satisfaction depending on activity type.

# Load cowplot for annotations
library(cowplot)

# Create the MCA variable plot
p <- fviz_mca_var(acm, repel = TRUE)
# Add space at bottom for text
p_shifted <- p + theme(plot.margin = margin(t = 10, r = 10, b = 30, l = 10))
# Add information text at bottom, centered
p_annotated <- ggdraw(p_shifted) +
  draw_text("Author: Jules Grandvillemin   |   Date: May 20, 2025   |   Data: Randomly generated for exercise",
            x = 0.5, y = 0.015, hjust = 0.5, size = 10)
# Print it
print(p_annotated)

## Create another graphic with layout according to accessibility indicator
p <- fviz_mca_ind(acm,
                  geom = "point",
                  alpha.ind = 0.25,
                  habillage = df$accessibility_indicator,
                  addEllipses = TRUE)
# Add space at bottom for text
p_shifted <- p + theme(plot.margin = margin(t = 10, r = 10, b = 40, l = 10))

# Add information text at bottom, centered
p_annotated <- ggdraw(p_shifted) +
  draw_text("Author: Jules Grandvillemin   |   Date: May 20, 2025   |   Data: Randomly generated for exercise",
            x = 0.5, y = 0.015, hjust = 0.5, size = 10)
# Print it
print(p_annotated)
# Interpretation: 
# MCA plot of individuals according to their 15-minute accessibility score.
#   The individuals are grouped by quantiles of their accessibility score, ranging from 0 (lowest accessibility) to 0.48 (highest).
#   Each color ellipse represents the confidence region (group centroid + dispersion) for one accessibility group.
#   The two first dimensions of the MCA explain 12.6% and 9.3% of the variance respectively.
#   Individuals with higher accessibility scores (e.g., 0.40–0.48, shown in pink) tend to be located towards the right side of Dim1,
#   suggesting associations with variables (e.g., amenities) considered more suitable or better accessible.
#   Conversely, individuals with lower scores (e.g., 0–0.10, red) are more spread on the left, associated with less suitable or inaccessible amenities.
