# load libraries and data files 
# uncomment below if necessary
# source('R/00_loadpackages.R', 'R/01_load_LIMS.R)
# meant to be run after sources 00-03 but can be run after 01

# 01 get min and max MDLs for all components -----------------------------------

# get min
mdl_min <- lims3 %>%
          dplyr::select(component_long, mdl, units) %>%
          dplyr::group_by(component_long) %>%
          dplyr::summarise(mdl_min = min(mdl)) 

# get max
mdl_max <- lims3 %>%
  dplyr::select(component_long, mdl, units) %>%
  dplyr::group_by(component_long) %>%
  dplyr::summarise(mdl_max = max(mdl))

# merge columns into one df
mdl_range <- dplyr::left_join(mdl_min, mdl_max, by = "component_long")

# view results
View(mdl_range)

## clean up environment ---
rm(mdl_max, mdl_min)

# Write as .csv file. Change file name to match yearly data file
write.csv(mdl_range, here::here('output', 'metadata', 'mdl_range_2023.csv'), row.names = FALSE)
