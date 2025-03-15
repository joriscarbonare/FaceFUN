################################################################################
# RANDOMIZED SAMPLING CELL & SPECIES SELECTION SCRIPT                         #
#                                                                              #
# This script integrates vegetation survey data with TERN and TRY databases to  #
# randomly select target understory species (herb, graminoid, fern) and their   #
# corresponding sampling cells on a per-plot basis. Species selection is         #
# determined by the frequency of occurrence (i.e., the number of cells where a    #
# species is present). Specifically, 3 C3 species (including microlaena_stipoides, #
# if available) and 3 C4 species are chosen for each plot.                     #
################################################################################
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(ggplot2)

# -------------------------------------------------------------------------
# STEP 0: Load the data files
# veg_data: Vegetation survey data with columns for Census_Date, Ring, Plot, Quadrant, Cell, and species columns (0/1 presence)
# tern_data: TERN data providing photosynthetic pathway (e.g., C3 or C4) for each species
# df_try: TRY database containing additional photosynthetic pathway and PlantGrowthForm

# Load raw vegetation data (last survey date)
load("DATA/veg_survey_lastdate.RData")
veg_data <- veg_survey_lastdate
# Load TERN data (photosynthetic pathways)
tern_data <- read.csv("DATA/Photosynthetic_Pathways_of_Plants_TERN_v2_19092024.csv", 
                      stringsAsFactors = FALSE)

# Load the TRY database (photosynthetic pathway and PlantGrowthForm)
df_try <- read.xlsx("DATA/TRY_Categorical_Traits_Lookup_Table_2012_03_17_TestRelease.xlsx", sheet = 1)

# -------------------------------------------------------------------------
# STEP 1: Create a unique cell identifier (CellID) by combining Ring, Plot, Quadrant, and Cell.
# Also, exclude rings 5 and 6 because we want to focus only on rings 1,2,3,4,7,8.
veg_data <- veg_data %>%
  filter(!(Ring %in% c(5, 6))) %>%  # Exclude rings 5 and 6
  mutate(CellID = paste(Ring, Plot, Quadrant, Cell, sep = "_"))

# -------------------------------------------------------------------------
# STEP 2: Transform the wide vegetation data into long format.
# We retain the first 5 columns (Census_Date, Ring, Plot, Quadrant, Cell) and the new CellID.
# All remaining columns represent species (with values 0/1 for absence/presence).
df_long <- veg_data %>%
  pivot_longer(
    cols = -c(1:5, "CellID"),  # Exclude the first 5 columns and the already-created CellID column
    names_to = "Species",
    values_to = "Presence"      # Presence/absence (0 or 1)
  ) %>%
  # Reassign CellID to ensure it is included in every row
  mutate(CellID = paste(Ring, Plot, Quadrant, Cell, sep = "_"))

# -------------------------------------------------------------------------
# STEP 3: Prepare a simplified TERN dataset.
# TERN data contains species names and photosynthetic pathways.
# We convert species names to lowercase with underscores for consistency.
tern_simple <- tern_data %>%
  select(speciesName, photosyntheticPathway_combined) %>%
  mutate(
    speciesName = iconv(speciesName, from = "UTF-8", to = "ASCII//TRANSLIT"),
    Species = tolower(gsub(" ", "_", speciesName))
  ) %>%
  select(Species, PhotosyntheticPathway = photosyntheticPathway_combined)

# -------------------------------------------------------------------------
# STEP 4: Merge the long-format vegetation data with the TERN dataset to add PhotosyntheticPathway.
df_long <- df_long %>%
  mutate(Species = tolower(Species)) %>%
  left_join(tern_simple, by = "Species")

# -------------------------------------------------------------------------
# STEP 5: Prepare the TRY dataset.
# TRY provides additional plant traits, including PlantGrowthForm.
# We create a key 'Species' based on AccSpeciesName and retain the columns for PhotosyntheticPathway (from TRY)
# and PlantGrowthForm.
try_simple <- df_try %>%
  mutate(Species = tolower(gsub(" ", "_", AccSpeciesName))) %>%
  select(Species, PhotosyntheticPathway_TRY = PhotosyntheticPathway, PlantGrowthForm) %>%
  distinct()

# Merge TRY data into df_long.
# Use the TRY photosynthetic pathway when the TERN value is missing.
df_long <- df_long %>%
  left_join(try_simple, by = "Species") %>%
  mutate(PhotosyntheticPathway = coalesce(PhotosyntheticPathway, PhotosyntheticPathway_TRY))
# Note: The PlantGrowthForm column (e.g., herb, graminoid, fern) is retained from TRY.

# -------------------------------------------------------------------------
# STEP 6: Filter df_long to retain only understory vegetation.
# We want to work only with species classified as "herb", "graminoid", or "fern" (i.e., typical understory plants).
df_long <- df_long %>%
  filter(PlantGrowthForm %in% c("herb", "graminoid", "fern"))

# Check for species still missing PhotosyntheticPathway information.
missing_species <- df_long %>% 
  filter(is.na(PhotosyntheticPathway)) %>% 
  distinct(Species)
if(nrow(missing_species) > 0){
  cat("Species missing photosynthetic pathway info in TERN or TRY (complete manually):\n")
  print(missing_species)
} else {
  cat("All species have an assigned photosynthetic pathway.\n")
}

# -------------------------------------------------------------------------
# STEP 7: Calculate the frequency of each species in each plot.
# Frequency is defined as the number of distinct cells in which the species is present.
# We filter to count only rows where Presence > 0.
species_freq <- df_long %>%
  filter(as.numeric(Presence) > 0) %>%   # Only count cells where the species is present
  group_by(Ring, Plot, Species, PhotosyntheticPathway, PlantGrowthForm) %>%
  summarise(Freq = n_distinct(CellID), .groups = "drop")

# -------------------------------------------------------------------------
# STEP 8: Select target species for each plot.
# For each plot, we select 3 C4 species and 3 C3 species based on their frequency.
# 8a. For C4 species: select the 3 most frequent C4 species per plot.
top_C4 <- species_freq %>%
  filter(PhotosyntheticPathway == "C4") %>%
  group_by(Ring, Plot) %>%
  arrange(desc(Freq)) %>%
  slice_head(n = 3) %>%
  ungroup()

# 8b. For C3 species: select the 3 most frequent C3 species per plot,
# ensuring that "microlaena_stipoides" is included if present.
top_C3 <- species_freq %>%
  filter(PhotosyntheticPathway == "C3") %>%
  group_by(Ring, Plot) %>%
  arrange(desc(Freq)) %>%
  summarise(
    selected_C3 = list({
      sp_all <- unique(Species)
      if("microlaena_stipoides" %in% sp_all){
        # If "microlaena_stipoides" is present, include it and add the 2 most frequent other C3 species.
        other_sp <- species_freq %>% 
          filter(Ring == first(Ring), Plot == first(Plot),
                 PhotosyntheticPathway == "C3", Species != "microlaena_stipoides") %>%
          arrange(desc(Freq)) %>%
          pull(Species)
        sp_selected <- unique(c("microlaena_stipoides", head(other_sp, 2)))
      } else {
        sp_selected <- head(sp_all, 3)
      }
      sp_selected
    }),
    .groups = "drop"
  ) %>%
  unnest(cols = c(selected_C3)) %>%
  rename(Species = selected_C3)

# (Optional) Ensure that top_C3 has PhotosyntheticPathway and PlantGrowthForm via a join if needed.
top_C3 <- top_C3 %>%
  left_join(tern_simple, by = "Species")

# 8c. Combine the selected C3 and C4 species to obtain 6 target species per plot.
selected_species <- bind_rows(top_C3, top_C4)

# -------------------------------------------------------------------------
# STEP 9: For each plot and each selected species, randomly select one cell where the species is present.
selected_cells <- df_long %>%
  semi_join(selected_species, by = c("Ring", "Plot", "Species")) %>%
  group_by(Ring, Plot, Species, PhotosyntheticPathway, PlantGrowthForm) %>%
  summarise(
    Selected_CellID = sample(unique(CellID), 1),
    .groups = "drop"
  )

# -------------------------------------------------------------------------
# STEP 10: Verify that each plot has exactly 6 cells selected.
check_counts <- selected_cells %>%
  group_by(Ring, Plot) %>%
  summarise(n = n(), .groups = "drop")
print(check_counts)

# -------------------------------------------------------------------------
# STEP 11: Organize the final result.
# The final result contains, for each plot, 6 rows with the following columns:
# Ring, Plot, Species, PhotosyntheticPathway, PlantGrowthForm, and the randomly selected cell ID.
final_selection <- selected_cells %>%
  arrange(Ring, Plot, Species)

# Print the final selection.
print(final_selection)