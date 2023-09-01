# load libraries and data files 
# uncomment below if necessary
# source('R/00_loadpackages.R', 'R/01_load_LIMS.R')

# 01 select LIMS data for hold time tables -------------------------------------
holdTimes <- lims3 %>% 
  dplyr::select(station_code, datetimestamp, date_analyzed, cdmo_name, component_long) %>% 
  tidyr::pivot_wider(id_cols = c('station_code',
                                 'datetimestamp'), 
                     names_from = cdmo_name,
                     values_from = date_analyzed)
                    # values_fn = length) # use this to help identify duplicate datetimestamps

# create a new column called Program_Type to help complete the sample hold tables
# Get information for Program_Type
holdTimes$ProgramType<-substr(holdTimes$station_code, 9, 9)

# view table
View(holdTimes)

# create diel, grab, and adjusted datetimestamp column for sample collection times only
# code does NOT adjust datetimestamps for sample hold times
# visually inspect output tables and modify datetimestamp from EDT to EST (if necessary)
holdTimes2<-holdTimes %>%
  mutate(across('ProgramType', str_replace, '2','Diel')) %>%
  mutate(across('ProgramType', str_replace, '1','Grab')) %>%
  mutate(adjustedtimestamp = case_when(
      datetimestamp >= "2023-03-12" & #When daylight savings starts
      datetimestamp <= "2023-11-05" ~ datetimestamp - (1 * 60 * 60), #When daylight savings ends
           TRUE ~ datetimestamp))

# view table
## compare datetimestamp column with adjustedtimestamp column
## confirm adjustedtimestamp is EST
View(holdTimes2)

# reorder output to match metadata document
tier1<- holdTimes2 %>% 
  dplyr::select(1, # reorder everything, 1 is the first column
                ProgramType,
                adjustedtimestamp,
                PO4F,
               # NO2F, # not in 2022+ data set
                NO23F,
                NH4F,
                CHLA_N)

# modify datetimestamp for each parameter to EST
# reformat all analytes to display date only
# tier 1 parameters
tier1Final<- tier1 %>% 
  mutate(PO4F = case_when(
    PO4F >= "2023-03-12" & #When daylight savings starts
    PO4F <= "2023-11-05" ~ PO4F - (1 * 60 * 60), #When daylight savings ends
      TRUE ~ PO4F)) %>% 
    mutate(PO4F = date(PO4F)) %>%
  mutate(NO23F = case_when(
    NO23F >= "2023-03-12" & #When daylight savings starts
      NO23F <= "2023-11-05" ~ NO23F - (1 * 60 * 60), #When daylight savings ends
      TRUE ~ NO23F)) %>%
  mutate(NO23F = date(NO23F)) %>%
  mutate(NH4F = case_when(
    NH4F >= "2023-03-12" & #When daylight savings starts
      NH4F <= "2023-11-05" ~ NH4F - (1 * 60 * 60), #When daylight savings ends
      TRUE ~ NH4F)) %>%
  mutate(NH4F = date(NH4F)) %>%
  mutate(CHLA_N = case_when(
    CHLA_N >= "2023-03-12" & #When daylight savings starts
      CHLA_N <= "2023-11-05" ~ CHLA_N - (1 * 60 * 60), #When daylight savings ends
      TRUE ~ CHLA_N)) %>%
  mutate(CHLA_N = date(CHLA_N)) 

# reorder output to match metadata document
tier2a<- holdTimes2 %>% 
  dplyr::select(1, # reorder everything, 1 is the first column
                ProgramType,
                adjustedtimestamp,
                DOC,            
                TP,
                TDP, #Uncomment for >= 2022 
                TKN,
                TKNF)

# modify datetimestamp for each parameter to EST
# reformat all analytes to display date only
# tier 2 parameters
tier2aFinal<- tier2a %>% 
  mutate(DOC = case_when(
    DOC >= "2023-03-12" & #When daylight savings starts
      DOC <= "2023-11-05" ~ DOC - (1 * 60 * 60), #When daylight savings ends
    TRUE ~ DOC)) %>% 
  mutate(DOC = date(DOC)) %>%
  mutate(TP = case_when(
    TP >= "2023-03-12" & #When daylight savings starts
      TP <= "2023-11-05" ~ TP - (1 * 60 * 60), #When daylight savings ends
    TRUE ~ TP)) %>% 
  mutate(TP = date(TP)) %>%
  mutate(TDP = case_when(
    TDP >= "2023-03-12" & #When daylight savings starts
      TDP <= "2023-11-05" ~ TDP - (1 * 60 * 60), #When daylight savings ends
    TRUE ~ TDP)) %>% 
  mutate(TDP = date(TDP)) %>%
  mutate(TKN = case_when(
    TKN >= "2023-03-12" & #When daylight savings starts
      TKN <= "2023-11-05" ~ TKN - (1 * 60 * 60), #When daylight savings ends
    TRUE ~ TKN)) %>% 
  mutate(TKN = date(TKN)) %>%
  mutate(TKNF = case_when(
    TKNF >= "2023-03-12" & #When daylight savings starts
      TKNF <= "2023-11-05" ~ TKNF - (1 * 60 * 60), #When daylight savings ends
    TRUE ~ TKNF)) %>% 
  mutate(TKNF = date(TKNF))

# reorder output to match metadata document
  tier2b<- holdTimes2 %>% 
  dplyr::select(1, # reorder everything, 1 is the first column
                ProgramType,
                adjustedtimestamp,
                UncCHLa_N,
                PHEA,
                TSS,
                color, #Included in >= 2022. Uncomment
                ENTERO_MPN,
                FECCOL_CFU)
                #TURB_N) #Not included in 2023

  # modify datetimestamp for each parameter to EST
  # reformat all analytes to display date only
  # tier 2b parameters
  tier2bFinal<- tier2b %>% 
    mutate(UncCHLa_N = case_when(
      UncCHLa_N >= "2023-03-12" & #When daylight savings starts
        UncCHLa_N <= "2023-11-05" ~ UncCHLa_N - (1 * 60 * 60), #When daylight savings ends
      TRUE ~ UncCHLa_N)) %>% 
    mutate(UncCHLa_N = date(UncCHLa_N)) %>%
    mutate(PHEA = case_when(
      PHEA >= "2023-03-12" & #When daylight savings starts
        PHEA <= "2023-11-05" ~ PHEA - (1 * 60 * 60), #When daylight savings ends
      TRUE ~ PHEA)) %>% 
    mutate(PHEA = date(PHEA)) %>%
    mutate(TSS = case_when(
      TSS >= "2023-03-12" & #When daylight savings starts
        TSS <= "2023-11-05" ~ TSS - (1 * 60 * 60), #When daylight savings ends
      TRUE ~ TSS)) %>% 
    mutate(TSS = date(TSS)) %>%
    mutate(color = case_when(
      color >= "2023-03-12" & #When daylight savings starts
        color <= "2023-11-05" ~ color - (1 * 60 * 60), #When daylight savings ends
      TRUE ~ color)) %>% 
    mutate(color = date(color)) %>%
    mutate(ENTERO_MPN = case_when(
      ENTERO_MPN >= "2023-03-12" & #When daylight savings starts
        ENTERO_MPN <= "2023-11-05" ~ ENTERO_MPN - (1 * 60 * 60), #When daylight savings ends
      TRUE ~ ENTERO_MPN)) %>% 
    mutate(ENTERO_MPN = date(ENTERO_MPN)) %>%
    mutate(FECCOL_CFU = case_when(
      FECCOL_CFU >= "2023-03-12" & #When daylight savings starts
        FECCOL_CFU <= "2023-11-05" ~ FECCOL_CFU - (1 * 60 * 60), #When daylight savings ends
      TRUE ~ FECCOL_CFU)) %>% 
    mutate(FECCOL_CFU = date(FECCOL_CFU))

# Write as .csv files
# KEEP as .csv. 
# If exported as Excel, it reformats the columns to numbers and the date columns as dates
write.csv(tier1Final, here::here('output', 'metadata', 'tier1_holdtimes_2023.csv'), row.names = FALSE)
write.csv(tier2aFinal, here::here('output', 'metadata', 'tier2a_holdtimes_2023.csv'), row.names = FALSE)
write.csv(tier2bFinal, here::here('output', 'metadata', 'tier2b_holdtimes_2023.csv'), row.names = FALSE)

## clean up environment ---
rm(holdTimes, holdTimes2, mdl_range, tier1, tier2a, tier2b, tier1Final, tier2aFinal, tier2bFinal)