################################################################################
# BACKTRACKING CELL & SPECIES SELECTION SCRIPT
################################################################################
# This script integrates vegetation survey data (veg_data) with TERN and TRY
# databases. We focus on understory species (herb, graminoid, fern) and select
# specific target species in each plot based on frequency of occurrence (3 C3
# species, ensuring microlaena_stipoides is included if present, plus 3 C4
# species). We then pick one cell per selected species in each plot, ensuring
# that no two chosen cells are within a specified minimum distance (the
# “threshold”). If no valid arrangement is found at the current threshold, we
# relax it step by step until a solution is found or we reach a lower limit.

# Libraries used for data manipulation, pivoting, string processing, reading
# files, and plotting.
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(ggplot2)
library(purrr)

################################################################################
# STEP 1: LOAD DATA AND BUILD SPECIES METADATA
################################################################################
# 1a) Load the vegetation data (veg_data) in wide format, TERN data (tern_data),
#     and TRY data (df_try). Each source contains partial information about
#     species, such as their names, photosynthetic pathways, and growth forms.

load("DATA/veg_survey_lastdate.RData")
veg_data <- veg_survey_lastdate

tern_data <- read.csv(
  "DATA/Photosynthetic_Pathways_of_Plants_TERN_v2_19092024.csv",
  stringsAsFactors = FALSE
)

df_try <- read.xlsx(
  "DATA/TRY_Categorical_Traits_Lookup_Table_2012_03_17_TestRelease.xlsx",
  sheet = 1
)

# 1b) From the vegetation dataset, extract all species actually present
#     (i.e., those with presence > 0 in at least one cell).
veg_species <- veg_data %>%
  pivot_longer(
    cols = -c(1:5),          # The first 5 columns are assumed to be metadata
    names_to = "Species",
    values_to = "Presence"
  ) %>%
  filter(Presence > 0) %>%   # Keep only species that appear in at least one cell
  distinct(Species) %>%
  mutate(Species = tolower(Species))

# 1c) Clean up TERN data to extract species name and photosynthetic pathway
#     under a consistent naming format (lowercase, underscores).
tern_clean <- tern_data %>%
  mutate(
    speciesName = iconv(speciesName, from = "UTF-8", to = "ASCII//TRANSLIT"),
    Species = tolower(gsub(" ", "_", speciesName))
  ) %>%
  select(Species, PhotosyntheticPathway = photosyntheticPathway_combined)

# 1d) Clean up TRY data, focusing on photosynthetic pathway and growth form.
df_try_clean <- df_try %>%
  mutate(
    Species = tolower(gsub(" ", "_", AccSpeciesName))
  ) %>%
  select(Species, PhotosyntheticPathway_TRY = PhotosyntheticPathway, PlantGrowthForm) %>%
  distinct()

# 1e) Merge TERN and TRY into one metadata table. We coalesce the TERN
#     photosynthetic pathway with the TRY pathway if TERN’s is missing.
species_metadata <- full_join(tern_clean, df_try_clean, by = "Species") %>%
  mutate(
    PhotosyntheticPathway = coalesce(PhotosyntheticPathway, PhotosyntheticPathway_TRY)
  ) %>%
  select(Species, PhotosyntheticPathway, PlantGrowthForm) %>%
  distinct() %>%
  semi_join(veg_species, by = "Species")  # Keep only species found in veg_data

################################################################################
# STEP 2: PREPARE THE VEGETATION DATA AND TRANSFORM IT FROM WIDE TO LONG
################################################################################
# 2a) Exclude certain rings (5 and 6 here) and create a unique CellID by
#     combining Ring, Plot, Quadrant, and Cell. This helps us reference each
#     cell unambiguously.
veg_data <- veg_data %>%
  filter(!(Ring %in% c(5, 6))) %>%
  mutate(CellID = paste(Ring, Plot, Quadrant, Cell, sep = "_"))

# 2b) Pivot the vegetation data from wide (species in columns) to long (one row
#     per species per cell). We then merge with the species metadata to attach
#     photosynthetic pathway and growth form information.
df_long <- veg_data %>%
  pivot_longer(
    cols = -c(1:5, "CellID"),
    names_to = "Species",
    values_to = "Presence"
  ) %>%
  mutate(Species = tolower(Species)) %>%
  left_join(species_metadata, by = "Species")

################################################################################
# STEP 3: FILTER FOR UNDERSTORY SPECIES AND CHECK FOR MISSING METADATA
################################################################################
# We focus on understory species (herb, graminoid, fern). Check if any species
# are missing photosynthetic pathway or growth form data.
df_long <- df_long %>%
  filter(PlantGrowthForm %in% c("herb", "graminoid", "fern"))

missing_species <- df_long %>%
  filter(is.na(PhotosyntheticPathway) | is.na(PlantGrowthForm)) %>%
  distinct(Species)

if(nrow(missing_species) > 0) {
  cat("Warning: Some species have incomplete metadata:\n")
  print(missing_species)
} else {
  cat("All understory species have complete metadata.\n")
}

################################################################################
# STEP 4: COMPUTE SPATIAL COORDINATES FOR EACH CELL
################################################################################
# Each quadrant is 100 x 100 cm, subdivided into 25 cells (5x5), each 20 cm.
# We store the center coordinates (X, Y) of each cell, using ring-plot quadrant
# layout. This is needed for the backtracking procedure that checks distances.
df_long <- df_long %>%
  mutate(
    QuadrantLabel = str_extract(CellID, "(?<=_)[A-D](?=_|$)"),
    CellNum = as.numeric(str_extract(CellID, "[0-9]+$"))
  ) %>%
  mutate(
    local_x = ((CellNum - 1) %% 5) * 20 + 10,  # horizontal offset
    local_y = 100 - ((CellNum - 1) %/% 5) * 20 - 10,  # vertical offset
    X = case_when(
      QuadrantLabel %in% c("A", "C") ~ local_x,
      QuadrantLabel %in% c("B", "D") ~ 100 + local_x
    ),
    Y = case_when(
      QuadrantLabel %in% c("A", "B") ~ 200 - local_y,
      QuadrantLabel %in% c("C", "D") ~ 100 - local_y
    )
  )

################################################################################
# STEP 5: CALCULATE FREQUENCY OF SPECIES PER PLOT & SELECT TARGET SPECIES
################################################################################
# 5a) Calculate how often each species appears in each plot. Frequency is the
#     number of distinct cells in which that species is present.
species_freq <- df_long %>%
  filter(as.numeric(Presence) > 0) %>%
  group_by(Ring, Plot, Species, PhotosyntheticPathway, PlantGrowthForm) %>%
  summarise(Freq = n_distinct(CellID), .groups = "drop")

# 5b) Select 3 C4 species per plot (highest frequency).
top_C4 <- species_freq %>%
  filter(PhotosyntheticPathway == "C4") %>%
  group_by(Ring, Plot) %>%
  arrange(desc(Freq)) %>%
  slice_head(n = 3) %>%
  ungroup()

# 5c) Select 3 C3 species per plot (highest frequency), ensuring microlaena_stipoides is included if it exists.
top_C3 <- species_freq %>%
  filter(PhotosyntheticPathway == "C3") %>%
  group_by(Ring, Plot) %>%
  arrange(desc(Freq)) %>%
  slice_head(n = 3) %>%
  ungroup()

ms_data <- species_freq %>%
  filter(Species == "microlaena_stipoides", PhotosyntheticPathway == "C3")

top_C3_final <- top_C3 %>%
  group_by(Ring, Plot) %>%
  mutate(included_ms = any(Species == "microlaena_stipoides")) %>%
  ungroup()

# Identify plot combos missing microlaena_stipoides but it is actually present in ms_data.
plot_ms_missing <- ms_data %>%
  semi_join(top_C3_final %>% filter(!included_ms), by = c("Ring", "Plot"))

if(nrow(plot_ms_missing) > 0) {
  # Remove the least frequent C3 species and add microlaena_stipoides
  top_C3_final <- top_C3_final %>%
    group_by(Ring, Plot) %>%
    arrange(Freq) %>%
    mutate(remove_me = if_else(!included_ms & row_number() == 1, TRUE, FALSE)) %>%
    filter(!remove_me) %>%
    ungroup()
  top_C3_final <- bind_rows(top_C3_final, plot_ms_missing)
}

top_C3_final <- top_C3_final %>% select(-included_ms)
selected_species <- bind_rows(top_C3_final, top_C4)

################################################################################
# STEP 6: DEFINE A RECURSIVE BACKTRACKING FUNCTION FOR NON-ADJACENT SELECTION
################################################################################
# We want to choose exactly one cell per species, ensuring that any chosen cell
# is at least 'threshold' distance from all previously chosen cells. If it fails,
# we return a row of NA to signal the procedure must try a smaller threshold.

select_cells_backtracking <- function(
    species_candidates,
    threshold,
    idx = 1,
    chosen_cells = data.frame(
      CellID = character(),
      X = numeric(),
      Y = numeric(),
      stringsAsFactors = FALSE
    )
) {
  # If we have assigned cells for all species, return the chosen set.
  if(idx > length(species_candidates)) {
    return(chosen_cells)
  }
  
  # Shuffle the candidate rows for species 'idx' to try them in random order.
  cand <- species_candidates[[idx]]
  cand <- cand[sample(nrow(cand)), ]
  
  # Iterate through each candidate cell in random order, checking distances.
  for(r in seq_len(nrow(cand))) {
    candidate_try <- cand[r, ]
    
    # If no cells chosen yet, pick the first candidate directly.
    if(nrow(chosen_cells) == 0) {
      chosen_cells_new <- bind_rows(chosen_cells, candidate_try)
      result <- select_cells_backtracking(species_candidates, threshold, idx + 1, chosen_cells_new)
      if(!any(is.na(result$CellID))) {
        return(result)
      }
    } else {
      # Calculate distances to already chosen cells.
      dist_to_chosen <- sqrt((chosen_cells$X - candidate_try$X)^2 +
                               (chosen_cells$Y - candidate_try$Y)^2)
      # If this candidate is >= threshold from all chosen cells, select it.
      if(min(dist_to_chosen) >= threshold) {
        chosen_cells_new <- bind_rows(chosen_cells, candidate_try)
        result <- select_cells_backtracking(species_candidates, threshold, idx + 1, chosen_cells_new)
        if(!any(is.na(result$CellID))) {
          return(result)
        }
      }
    }
  }
  
  # If we fail to find a valid cell for this species at the current threshold, return NAs.
  return(data.frame(CellID = NA_character_, X = NA_real_, Y = NA_real_))
}

################################################################################
# STEP 7: PER-PLOT BACKTRACKING WITH THRESHOLD RELAXATION
################################################################################
# We'll attempt to select cells for the chosen species in each plot. We start
# with an 'initial_threshold' and reduce it in increments ('relax_step') until
# we reach a 'min_threshold' or succeed.

plot_ids <- selected_species %>%
  distinct(Ring, Plot)

final_selected_list <- list()

initial_threshold <- 80
relax_step <- 10
min_threshold <- 10

for(j in seq_len(nrow(plot_ids))) {
  ring_j <- plot_ids$Ring[j]
  plot_j <- plot_ids$Plot[j]
  
  # Subset the species to be sampled in this ring-plot.
  sp_j <- selected_species %>%
    filter(Ring == ring_j, Plot == plot_j)
  
  # Build a list of candidate cells for each species.
  candidate_list <- list()
  valid_plot <- TRUE
  for(k in seq_len(nrow(sp_j))) {
    sp_name <- sp_j$Species[k]
    cands <- df_long %>%
      filter(Ring == ring_j, Plot == plot_j, Species == sp_name, as.numeric(Presence) > 0) %>%
      distinct(CellID, X, Y)
    if(nrow(cands) == 0) {
      warning(paste("No candidate cells for species", sp_name, "in plot", plot_j, "ring", ring_j))
      valid_plot <- FALSE
      break
    }
    candidate_list[[k]] <- cands
  }
  if(!valid_plot) next
  
  # Try backtracking at decreasing thresholds until we find a valid arrangement.
  threshold <- initial_threshold
  selected_result <- data.frame(CellID = NA, X = NA, Y = NA)
  
  while(threshold >= min_threshold) {
    res <- select_cells_backtracking(candidate_list, threshold)
    # If we have a valid solution (one cell per species, no NAs).
    if(nrow(res) == length(candidate_list) && !any(is.na(res$CellID))) {
      selected_result <- res
      break
    }
    threshold <- threshold - relax_step
  }
  
  # If we succeeded, store the final selection. Otherwise, issue a warning.
  if(nrow(selected_result) == length(candidate_list) && !any(is.na(selected_result$CellID))) {
    selected_result$Ring <- ring_j
    selected_result$Plot <- plot_j
    selected_result$Species <- sp_j$Species
    selected_result$PhotosyntheticPathway <- sp_j$PhotosyntheticPathway
    selected_result$PlantGrowthForm <- sp_j$PlantGrowthForm
    
    final_selected_list[[length(final_selected_list) + 1]] <- selected_result
  } else {
    warning(paste("Backtracking failed for ring", ring_j, "plot", plot_j))
  }
}

# Combine all per-plot results.
final_selected_cells <- bind_rows(final_selected_list)

################################################################################
# STEP 8: VISUALIZATION WITH C3 AND C4 COLOR PALETTES
################################################################################
# We'll define a function that creates distinct color gradients for C3 vs C4
# species, then apply it to the final selected cells. We also build a background
# grid for each ring-plot, so we can see the entire set of cells.

species_pal <- function(species, pathway) {
  c3_species <- unique(species[pathway == "C3"])
  c4_species <- unique(species[pathway == "C4"])
  
  # Define separate gradients: warm for C3, cool for C4.
  c3_cols <- colorRampPalette(c("red", "orange", "yellow"))(length(c3_species))
  c4_cols <- colorRampPalette(c("blue", "cyan", "purple"))(length(c4_species))
  
  c3_map <- setNames(c3_cols, c3_species)
  c4_map <- setNames(c4_cols, c4_species)
  
  # Combine into a single named vector.
  full_map <- c(c3_map, c4_map)
  out <- full_map[species]
  return(out)
}

final_selected_cells$Species <- factor(final_selected_cells$Species,
                                       levels = unique(final_selected_cells$Species))
final_selected_cells$Color <- species_pal(
  final_selected_cells$Species,
  final_selected_cells$PhotosyntheticPathway
)

cell_grid <- df_long %>%
  distinct(Ring, Plot, CellID, X, Y)

ggplot() +
  geom_tile(
    data = cell_grid,
    aes(x = X, y = Y),
    fill = "grey90",
    color = "grey80",
    width = 20,
    height = 20
  ) +
  geom_point(
    data = final_selected_cells,
    aes(x = X, y = Y, color = Color),
    size = 2
  ) +
  scale_color_identity("Species (C3 vs C4)",
                       labels = final_selected_cells$Species,
                       guide = "legend") +
  facet_grid(Ring ~ Plot) +
  theme_minimal() +
  labs(
    title = "Spatial Distribution of Selected Sampling Cells (Backtracking)",
    x = "X (cm)",
    y = "Y (cm)"
  )

################################################################################
# STEP 9: SUMMARY TABLE OF SELECTED SPECIES
################################################################################
# Finally, we create a table showing how many times each species was selected
# across all ring-plot combinations, providing an overview of the final sampling.

species_summary <- final_selected_cells %>%
  group_by(Species, PhotosyntheticPathway, PlantGrowthForm) %>%
  summarise(SelectionCount = n(), .groups = "drop") %>%
  arrange(desc(SelectionCount))

print(species_summary)