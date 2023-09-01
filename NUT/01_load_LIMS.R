# load libraries and data files 
# uncomment below if necessary
# source('R/00_loadpackages.R')

# 01 load data --------------------------------------------------
## load LIMS File
## LIMS file must be in the format used in the GTM_LIMS_V2 template
## If running code and encounters an error then there is probably a mistake in the LIMS file.
## this file should be in the 'data/2023' folder
## file needs to be named 'LIMS_Download.xlsx', but if using file as is
## edit file name in import code below
## EDIT and/or REVIEW LIMS file prior to loading it
## correct all mistakes in the LIMS file
## if new parameters are added or removed the code will need to be edited
## code is currently based on the 2023 LIMS file format
lims <- readxl::read_xlsx(here::here('data', 
                                     '2023',
                                     'LIMS_Download.xlsx'), # this is where you'd want to rename the file
                          sheet = "BrowseReportPage") %>% 
        janitor::clean_names()

# inspect the data
dplyr::glimpse(lims)

# load CDMO names file
# prior to loading, add all new parameters to the componentnames file
names <- readxl::read_xlsx(here::here('data', 
                                      '2023',
                                      'componentnames_2023.xlsx')) %>%
         janitor::clean_names()

# 02 wrangle-tidy data ------------------------------------------------------
## rename some columns in lims to what we use in SWMP
## convert datetimes into POSIXct format
## make all entries in station_code and component_long columns lowercase (easier coding)
## remove any white/blank spaces in station_code that were inserted during laboratory data entry
## remove field blanks
lims2 <- lims %>%
          dplyr::rename(station_code = field_id,
                        component_long = component,
                        datetimestamp = date_sampled) %>% 
          dplyr::mutate(datetimestamp = as.POSIXct(strptime(datetimestamp, 
                                                           "%d-%b-%Y %H:%M", tz='EST')),
                        date_analyzed = as.POSIXct(strptime(date_analyzed, 
                                                            "%d-%b-%Y %H:%M", tz='EST')),
                        station_code = tolower(station_code),
                        component_long = tolower(component_long),
                        (across(.cols = 4, str_remove_all, pattern = fixed(" ")))) %>% # remove spaces in between station_codes to make them all the same
          dplyr::filter(station_code != "fieldblank")

# check datetimestamp
dplyr::glimpse(lims2$datetimestamp)

# Double check field IDs/station code to see if the lab misspelled anything or something was added
# There should be 19 station code observations
stationCodes <- lims2 %>%
  dplyr::distinct(as.data.frame(lims2$station_code))

# uncomment, run and modify if there are added analytes or misspellings that need to be removed
# lims2 <- lims2 %>%
#   dplyr::filter(station_code != "fb" & station_code != "disystemcheck1" & station_code != "disystemcheck2" & station_code != "disystemcheck3") #this may have to be changed to fieldblank (no spaces)

# uncomment, run to double check field IDs/station code (again)
# stationCodes2 <- lims2 %>%
#   dplyr::distinct(as.data.frame(lims2$station_code))

# correct so that TKN and TKN-F are different 
# fixing the LIMS entry so that kjeldahl nitrogen, dissolved 
#  is different from kjeldahl nitrogen since they have the same component name in LIMS
tkn_f <- lims2 %>% 
          dplyr::filter(analysis == "W-TKN-F") %>% 
          dplyr::mutate(component_long = "kjeldahl nitrogen, dissolved") 

# fixing the LIMS entry so that total-p, filtered 
#  is different from total-p since they have the same component name
tp_f <- lims2 %>% 
  dplyr::filter(analysis == "W-S-A-TP-F") %>% 
  dplyr::mutate(component_long = "total-p, dissolved") 

# merge the renamed TKNF data with all the other data 
# join with the `names` df to get the CDMO format names
lims3 <- lims2 %>% 
          dplyr::filter(analysis != "W-TKN-F") %>% 
          dplyr::filter(analysis != "W-S-A-TP-F") %>%
          dplyr::bind_rows(tkn_f) %>%  
          dplyr::bind_rows(tp_f) %>%
          dplyr::left_join(names, by = "component_long", multiple = "all") %>% 
          dplyr::mutate(cdmo_name = forcats::as_factor(cdmo_name)) 

## clean up environment ---
rm(tkn_f, tp_f, lims2)