# load libraries and data files 
# uncomment below if necessary
# source('R/00_loadpackages.R', R/01_load_LIMS.R', 'R/02_wrangle_NUT_LIMS.R')

# 01 modify LIMS data --------------------------------------------------
#Separate date and time in adjustedtimestamp column
limsTimeAdjusted <- lims_wide_final %>%
              dplyr::mutate(adjustedtimestamp = as.POSIXct(strptime(adjustedtimestamp, 
                                                    "%Y-%m-%d %H:%M", tz='EST'))) 
class(limsTimeAdjusted$adjustedtimestamp)

limsTimeAdjusted$adjusteddate <- as.Date(limsTimeAdjusted$adjustedtimestamp)
limsTimeAdjusted$adjustedmonth <- month(limsTimeAdjusted$adjustedtimestamp)
limsTimeAdjusted$adjustedtime<- format(limsTimeAdjusted$adjustedtimestamp,"%H:%M")

# inspect the data
dplyr::glimpse(limsTimeAdjusted)

#Select columns for output

grabOrder <- limsTimeAdjusted %>% 
  dplyr::select(station_code, adjusteddate, adjustedtime, monitoringprogram, rep)
dielOrder <- limsTimeAdjusted %>% 
  dplyr::select(station_code, adjustedmonth, adjusteddate, adjustedtime, monitoringprogram, rep)

# 02 filter data and join tables by adjusteddate for grabs and adjustedmonth for diel--------------------------

## Grab samples
grabSamples1 <- dplyr::filter(grabOrder, monitoringprogram == '1' & rep == '1')
grabSamples2 <- dplyr::filter(grabOrder, monitoringprogram == '1' & rep == '2')

### Pine Island
# Filter by station code and join tables
PIgrab1<-dplyr::filter(grabSamples1, station_code == 'gtmpinut')
PIgrab2<-dplyr::filter(grabSamples2, station_code == 'gtmpinut')
PIcombined <- dplyr::full_join(PIgrab1, PIgrab2, by = "adjusteddate")

#Create a duplicate date column for final output order and rename columns to match metadata
PI_final <- PIcombined %>% 
  dplyr::mutate(adjusteddate2 = adjusteddate) %>%
  dplyr::select(station_code.x, adjusteddate, adjustedtime.x, adjusteddate2, adjustedtime.y) %>%
  dplyr::rename(Station.Code = station_code.x, Start.Date = adjusteddate, Start.Time = adjustedtime.x, End.Date = adjusteddate2, End.Time = adjustedtime.y)

## clean up environment 
rm(PIcombined, PIgrab1, PIgrab2)

### San Sebastian
# Filter by station code and join tables
SSgrab1<-dplyr::filter(grabSamples1, station_code == 'gtmssnut') 
SSgrab2<-dplyr::filter(grabSamples2, station_code == 'gtmssnut')
SScombined <- dplyr::full_join(SSgrab1, SSgrab2, by = "adjusteddate")

#Create a duplicate date column for final output order and rename columns to match metadata
SS_final <- SScombined %>% 
  dplyr::mutate(adjusteddate2 = adjusteddate) %>%
  dplyr::select(station_code.x, adjusteddate, adjustedtime.x, adjusteddate2, adjustedtime.y) %>%
  dplyr::rename(Station.Code = station_code.x, Start.Date = adjusteddate, Start.Time = adjustedtime.x, End.Date = adjusteddate2, End.Time = adjustedtime.y)

## clean up environment 
rm(SScombined, SSgrab1, SSgrab2)

### Fort Matanzas
# Filter by station code and join tables
FMgrab1<-dplyr::filter(grabSamples1, station_code == 'gtmfmnut') 
FMgrab2<-dplyr::filter(grabSamples2, station_code == 'gtmfmnut')
FMcombined <- dplyr::full_join(FMgrab1, FMgrab2, by = "adjusteddate")

#Create a duplicate date column for final output order and rename columns to match metadata
FM_final <- FMcombined %>% 
  dplyr::mutate(adjusteddate2 = adjusteddate) %>%
  dplyr::select(station_code.x, adjusteddate, adjustedtime.x, adjusteddate2, adjustedtime.y) %>%
  dplyr::rename(Station.Code = station_code.x, Start.Date = adjusteddate, Start.Time = adjustedtime.x, End.Date = adjusteddate2, End.Time = adjustedtime.y)

## clean up environment 
rm(FMcombined, FMgrab1, FMgrab2)

### Pellicer Creek
# Filter by station code and join tables
PCgrab1<-dplyr::filter(grabSamples1, station_code == 'gtmpcnut') 
PCgrab2<-dplyr::filter(grabSamples2, station_code == 'gtmpcnut')
PCcombined <- dplyr::full_join(PCgrab1, PCgrab2, by = "adjusteddate")

#Create a duplicate date column for final output order and rename columns to match metadata
PC_final <- PCcombined %>% 
  dplyr::mutate(adjusteddate2 = adjusteddate) %>%
  dplyr::select(station_code.x, adjusteddate, adjustedtime.x, adjusteddate2, adjustedtime.y) %>%
  dplyr::rename(Station.Code = station_code.x, Start.Date = adjusteddate, Start.Time = adjustedtime.x, End.Date = adjusteddate2, End.Time = adjustedtime.y)

## clean up environment 
rm(PCcombined, PCgrab1, PCgrab2)


## Diel samples

# Filter diel stations to get first and last sample times and join tables by month
dielSamples1 <- dplyr::filter(dielOrder, monitoringprogram == '2' & rep == '1')
dielSamples2 <- dplyr::filter(dielOrder, monitoringprogram == '2' & rep == '11')

dielCombined <- dplyr::full_join(dielSamples1, dielSamples2, by = "adjustedmonth")

#Create a duplicate date column for final output order and rename columns to match metadata
diel <- dielCombined %>% 
  dplyr::select(station_code.x, adjusteddate.x, adjustedtime.x, adjusteddate.y, adjustedtime.y) %>%
  dplyr::rename(Station.Code = station_code.x, Start.Date = adjusteddate.x, Start.Time = adjustedtime.x, End.Date = adjusteddate.y, End.Time = adjustedtime.y)


#04 Output tables--------------------------------------------------------------------------------------
write.csv(PI_final, here::here('output', 'metadata', 'PI_dataCollection_2023.csv'), row.names = FALSE)
write.csv(SS_final, here::here('output', 'metadata', 'SS_dataCollection_2023.csv'), row.names = FALSE)
write.csv(FM_final, here::here('output', 'metadata', 'FM_dataCollection_2023.csv'), row.names = FALSE)
write.csv(PC_final, here::here('output', 'metadata', 'PC_dataCollectionGrabs_2023.csv'), row.names = FALSE)
write.csv(diel, here::here('output', 'metadata', 'PC_dataCollectionDiel_2023.csv'), row.names = FALSE)
