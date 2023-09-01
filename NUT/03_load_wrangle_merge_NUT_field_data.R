# load libraries and data files 
# uncomment below if necessary
# source('R/00_loadpackages.R', 'R/01_load_LIMS.R', 'R/04_wrangle_NUT_LIMS.R)

# 01 load data --------------------------------------------------
## 01.1 2021 field data loop ------------------------------------------------------

# initialize readin listing
mysheets_fromexcel <- list()

# Edit excel file name to match field data file
# get the list of all the sheet names
mysheetlist <- readxl::excel_sheets(path = here::here('data', 
                                                      '2023',
                                                      '2023_FIELDDATA.xlsx'))

# Edit excel file name to match field data file name
# create loop for the sheets
i = 1

for (i in 1:length(mysheetlist)){
  
  tempdf <- readxl::read_excel(path = here::here('data', 
                                                 '2023',
                                                 '2023_FIELDDATA.xlsx'), 
                               sheet = mysheetlist[i])
  
  tempdf$sheetname <- mysheetlist[i]
  
  mysheets_fromexcel[[i]] <- tempdf 
}

mysheets_fromexcel


# merge all the lists into one tibble using dplyr::bind_rows()
env <- purrr::reduce(mysheets_fromexcel, dplyr::bind_rows) %>% 
  janitor::clean_names()

# inspect the data
glimpse(env)

# clear environment

rm(mysheets_fromexcel, 
   tempdf, i, mysheetlist)

# 02 wrangle-tidy field data ---------------------------------------------
env2 <- env %>%
  dplyr::mutate(date_sampled = paste(date_time),
                site = tolower(site),
                component_long = tolower(component_long),
                cdmo_name = forcats::as_factor(component_short)
  ) 

## 02.2 wide-format for export ---------------------------------------------

env_wide_result <- env2 %>% 
  dplyr::select(station_code, date_sampled, cdmo_name, component_long, result) %>%
  tidyr::pivot_wider(id_cols = c("station_code", "date_sampled"),
                     names_from = cdmo_name,
                     values_from = result)
  
env_wide_remark <- env2 %>% 
  dplyr::select(station_code, date_sampled, cdmo_name, component_long, remark) %>%
  dplyr::mutate(cdmo_name = paste0("F_", cdmo_name)) %>% 
  tidyr::pivot_wider(id_cols = c("station_code", "date_sampled"),
                     names_from = cdmo_name,
                     values_from = remark) 
# Output all data
env_wide <- env_wide_result %>% 
  dplyr::left_join(env_wide_remark, by = c("station_code", "date_sampled")) %>% 
  dplyr::mutate(fullstationname = station_code) %>% 
  dplyr::rename(datetimestamp = date_sampled) %>% 
  tidyr::separate(station_code, 
                  into = c("station_code", "num"), 
                  sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  tidyr::separate(num,
                  into = c("monitoringprogram", "rep"),
                  sep = "[.]") %>% 
  dplyr::select(station_code, fullstationname, datetimestamp, monitoringprogram, rep,
                Depth, F_Depth,
                Depth_S, F_Depth_S,
                SECCHI, F_SECCHI,
                WIND_S, F_WIND_S,
                WIND_D, F_WIND_D,
                ATEMP, F_ATEMP,
                WTEM_T, F_WTEM_T,
                SpCond_T, F_SpCond_T,
                pH_T, F_pH_T,
                DO_T, F_DO_T,
                SALT_T, F_SALT_T,
                WTEM_B, F_WTEM_B,
                SpCond_B, F_SpCond_B,
                pH_B, F_pH_B,
                DO_B, F_DO_B,
                SALT_B, F_SALT_B)

## clean up environment
rm(env_wide_remark, env_wide_result)

##Output only SWMP submitted parameters
swmp_field <- env_wide %>% 
  dplyr::select(station_code, fullstationname, datetimestamp, monitoringprogram, rep,
                WTEM_B, F_WTEM_B,
                SALT_B, F_SALT_B,
                DO_B, F_DO_B,
                pH_B, F_pH_B,
                SECCHI, F_SECCHI
                )

## Rename field parameters
swmp_field <- swmp_field %>%
  rename(WTEM_N = WTEM_B,
         F_WTEM_N = F_WTEM_B,
         SALT_N = SALT_B,
         F_SALT_N = F_SALT_B,
         DO_N = DO_B, 
         F_DO_N = F_DO_B,
         PH_N = pH_B, 
         F_PH_N = F_pH_B
        )

# inspect the data
dplyr::glimpse(swmp_field)

# 03 merge field data with LIMS file for QAQC --------------------------------
## Merge field data with LIMS, create new dataframe

CDMO_format <- lims_wide_final %>% left_join(swmp_field, by = c("station_code", "fullstationname", "datetimestamp", "monitoringprogram", "rep"))

# 04 reorder merged dataframe so it matches CDMO format in new dataframe
CDMO_format_Final <- CDMO_format %>% 
   dplyr::select(1:5, # reorder everything, 1:5 are the station code, etc. columns. Second number is one more than the total number of columns i.e. number of data columns +1
                F_Record,
                PO4F,
                F_PO4F,
                TP,
                F_TP,
                TDP,
                F_TDP,
                NH4F,
                F_NH4F,
                NO23F,
                F_NO23F,
                TKN,
                F_TKN,
                TKNF,
                F_TKNF,
                CHLA_N,
                F_CHLA_N,
                UncCHLa_N,
                F_UncCHLa_N,
                PHEA,
                F_PHEA,
                TSS,
                F_TSS,
                # TURB_N,
                # F_TURB_N,
                color,
                F_color,
                FECCOL_CFU,
                F_FECCOL_CFU,
                ENTERO_MPN,
                F_ENTERO_MPN,
                WTEM_N,
                F_WTEM_N,
                SALT_N,
                F_SALT_N,
                DO_N,
                F_DO_N,
                PH_N,
                F_PH_N,
                SECCHI,
                F_SECCHI,
                DOC,
                F_DOC,
                IRR0_N,
                F_IRR0_N,
                IRR1_N,
                F_IRR1_N,
                Kd_N,
                F_Kd_N,
                adjustedtimestamp
                )

# 05 export merged data frame for QAQC ----------------------------------------------

# remove the NAs that will come through on file export for all field parameters
# uncomment if all field data parameters are wanted
# env_wide <- sapply(env_wide, as.character)
# env_wide[is.na(env_wide)] <- ""
# env_wide <- as.data.frame(env_wide)

# remove the NAs that will come through on file export for SWMP submitted parameters
CDMO_format_Final <- sapply(CDMO_format_Final, as.character)
CDMO_format_Final[is.na(CDMO_format_Final)] <- ""
CDMO_format_Final <- as.data.frame(CDMO_format_Final)

# write as .csv file. USE THIS ONE. 
# file exports as .csv 
# when imported into Excel it reformats the columns to numbers 
#  and the date columns as dates
# careful, if running this code twice in the same day, you will get a warning 
#  that a sheet of that name already exists. 
# may need to delete sheet of same date if you ran into error
write.csv(CDMO_format_Final, here::here('output', 'data', 'gtmnut2023_wide.csv'), row.names = FALSE)

# Output merged Excel file (if needed)
# xlsx::write.xlsx(CDMO_format_Final, here::here('output', 'data','gtmnut2023_wide.xlsx'),
#                  sheetName = paste(Sys.Date()), # change this date
#                  append = TRUE)

# Output .csv file for all parameters (if needed)
# write.csv(env_wide, here::here('output', 'data', 'all_field_data_wide.csv'), row.names = FALSE)

## clean up environment
rm(CDMO_format, CDMO_format_Final, env, env_wide, env2, swmp_field)