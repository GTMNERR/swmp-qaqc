# load libraries and data files 
# uncomment below if necessary
# source('R/00_loadpackages.R', 'R/01_load_LIMS.R')

# ## 01 make LIMS data wide for data file --------------------------------------------------
# ## Modify LIMS data from long to wide format for entry into in-house data file

lims_wide_results <- lims3 %>%
  dplyr::select(station_code, datetimestamp, cdmo_name, component_long, result) %>%
  tidyr::pivot_wider(id_cols = c('station_code',
                                 'datetimestamp'),
                     names_from = cdmo_name,
                     values_from = result)

lims_wide_remarks <- lims3 %>%
  dplyr::select(station_code, datetimestamp, cdmo_name, remark) %>%
  dplyr::mutate(cdmo_name = paste0('F_', cdmo_name)) %>%
  tidyr::pivot_wider(id_cols = c('station_code',
                                 'datetimestamp'),
                     names_from = cdmo_name,
                     values_from = remark)

lims_wide <- lims_wide_results %>%
  left_join(lims_wide_remarks,
            by = c("station_code", "datetimestamp")) %>%
  dplyr::mutate(fullstationname = station_code) %>%
  tidyr::separate(station_code,
                  into = c("station_code", "num"),
                  sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  tidyr::separate(num,
                  into = c("monitoringprogram", "rep"),
                  sep = "[.]") %>%
  dplyr::select(1, 103, 4, 2:3, 5:102) # may need to modify last number
                                       # this depends on number of analytes in data set

# to look up column numbers for easier reordering used for `select()` above
# data.frame(colnames(lims_wide))
# data.frame(colnames(lims_wide_results)). Reordering columns. Station Code =1, full station name = 101, datetime = 2, monitoring program = 2
# rep = 3, Data starts in column 5 and data ends in column 100

## clean up environment
rm(lims_wide_remarks, lims_wide_results)

# env variables in LIMS file if wanted ----------------------------------------------

# env <- lims3 %>%
#         select(station_code, temperature, specific_conductance, salinity, ph, dissolved_o2) %>%
#         tidyr::separate(temperature,
#                         into = c("WTEM_N", "tempunits"),
#                         sep = "(\\s+)") %>%
#         tidyr::separate(specific_conductance,
#                         into = c("specificconductance", "spcunits"),
#                         sep = "(\\s+)") %>%
#         tidyr::separate(dissolved_o2,
#                         into = c("DO_N", "o2units"),
#                         sep = "(\\s+)") %>%
#         dplyr::select(-spcunits, -tempunits, -o2units)
#

# 02 prep lims-wide file for export --------------------------------------------
# build in blank columns for easier copy & paste
# make file the same parameters and order as the CDMO in-house qaqc file
# add new parameters or uncomment parameters below
lims_wide2 <- lims_wide %>%
  dplyr::mutate(F_Record = '', 
                IRR0_N = '',
                F_IRR0_N = '',
                IRR1_N = '',
                F_IRR1_N = '',
                Kd_N = '',
                F_Kd_N = '',
                # DOP = '',
                # F_DOP = '',
                # PHOSP	= '',
                # F_PHOSP= '',
                # PN = '',
                # F_PN = '',
                TURB_N = '',
                F_TURB_N = '',
                # DIN = '',
                # F_DIN = '',
                # TN = '',
                # F_TN = '',
                # TON = '',
                # F_TON = '',
                # PON = '',
                # F_PON = '',
                # TDN = '',
                # F_TDN = '',
                # DON = '',
                # F_DON = '',
                # SECCHI = '',
                # F_SECCHI = '',
                # WTEM_N = '',
                # F_WTEM_N = '',
                # SALT_N = '',
                # F_SALT_N = '',
                # PH_N = '',
                # F_PH_N = '',
                # DO_N = '',
                # F_DO_N = ''
  ) %>%
  dplyr::select(1:5, 104, # reorder everything, 1:5 are the station code, etc. columns
                PO4F, F_PO4F,
                TP, F_TP,
                TDP, F_TDP, #Uncomment for 2022+
                NH4F, F_NH4F,
                NO23F, F_NO23F,
                TKN, F_TKN,
                TKNF, F_TKNF,
                CHLA_N, F_CHLA_N,
                UncCHLa_N, F_UncCHLa_N,
                PHEA, F_PHEA,
                TSS, F_TSS,
                TURB_N, F_TURB_N,
                color, F_color, #Uncomment for 2022+
                FECCOL_CFU, F_FECCOL_CFU,
                ENTERO_MPN, F_ENTERO_MPN,
                # WTEM_N, F_WTEM_N, # GTM collected field parameters. Exclude these for now. 
                # SALT_N, F_SALT_N, # They get added in field data script
                # DO_N, F_DO_N,
                # PH_N, F_PH_N,
                # SECCHI,	F_SECCHI,
                DOC, F_DOC,
                IRR0_N,	F_IRR0_N,
                IRR1_N,	F_IRR1_N,
                Kd_N,	F_Kd_N,
                # DIN, F_DIN, # uncomment if you want calc parameters
                # DON, F_DON,
                # DOP, F_DOP,
                # PHOSP, F_PHOSP,
                # PON, F_PON,
                # TDN, F_TDN,
                # TN, F_TN,
                # PN,	F_PN,
                # TON, F_TON
  )

# reformat datetimestamp to CDMO format. Column will be last
# NOTE: If exporting as .csv you don't need to reformat datetimestamp
# create Date objects using base R. Uncomment if needed
#lims_wide2$timestamp <- strptime(lims_wide2$datetimestamp, "%Y-%m-%d %H:%M")

## format them to spec. Uncomment if needed
#lims_wide2$CDMO_dates <- format(lims_wide2$timestamp, "%m/%d/%Y %H:%M")

## Add column that converts datetimestamp from EDT to EST
## Datetimestamp has to have a class of POSIXct/POSIXt.
## EDIT Dates to match daylight savings times
lims_wide_final <- lims_wide2 %>%
  mutate(adjustedtimestamp = case_when(
    datetimestamp >= "2023-03-12" &     # When daylight savings starts
      datetimestamp <= "2023-11-05" ~ datetimestamp - (1 * 60 * 60),     # When daylight savings ends
    TRUE ~ datetimestamp))

# remove the NAs that will come through on file export
lims_wide_final <- sapply(lims_wide_final, as.character)
lims_wide_final[is.na(lims_wide_final)] <- ""
lims_wide_final <- as.data.frame(lims_wide_final)

# 03 write as .csv file. USE THIS ONE ------------------------------------------
# file exports as .csv 
# when imported into Excel it reformats the columns to numbers 
#  and the date columns as dates
# file does not contain GTM field data only LIMS data
write.csv(lims_wide_final, here::here('output', 'data', 'lims_wide_2023.csv'), row.names = FALSE)

# write Excel file (if needed)
# careful, if running this code twice in the same day, you will get a warning that a sheet of that name already exists.
# may need to delete sheet of same date if you ran into error
##xlsx::write.xlsx(lims_wide_final, here::here('output', 'data', lims_wide_2023.xlsx'),
#               sheetName = paste(Sys.Date()), # change this date
#               append = TRUE)

## clean up environment
rm(lims, lims_wide, lims_wide2)