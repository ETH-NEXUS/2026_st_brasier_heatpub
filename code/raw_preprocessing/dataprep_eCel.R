# ARCHIVED, NOT RUN. See README.md in this folder.
# Extract of dataprep_eCel.R by the earlier statisticians, kept as found: the raw exports
# are not available, so this code could not be run or verified. The plotting
# section moved to code/supplement_timeseries.R. Visit dates and clock times
# used as cut-offs, and a colleague's name, are redacted.

###
### Project:     HeatSTAR
### File:        Data preparation: eCelsius data
### Description: 
###  


# Collected at Visits 1, 2, 3, 4 (need 2-4)

# Notes
# ID5 V2 data not usable (set aside in different folder to avoid use)
# KAR manually changed some file names because there were several variations:
#      Most are names with ID#_V#_e-Cel
#      (except PIDs: 1, 3, 5, 11, 12, 22) which I changed
#      Also had to add column headers to ID 5 and 22
# KAR manually removed extra "Sample number;Date(dd/mm/yyyy)" from header of P6-V3
# KAR manually added "Sample number;Date(dd/mm/yyyy)" to P5-V3


# Issues (KAR !!!)
# Export errors for
#   P2-V4
#   P8-V4
#   P15-V2


# Read in the data for each Patient-Visit pair ---------------------------------
loc_ecel <- paste(data_path, "/e-Celsius/", sep = "")
loc_ecel_id <- paste(loc_ecel, list.files(path = loc_ecel), sep = "")
list_ecel <- list()

ecel_colnames <- c("sample", "date", "time", "temp_core_ecel", "id", "visit")

# data only available for patient 1 so far
#loc_ecel_id <- loc_ecel_id[-16] # delete me

for(i in 1:length(loc_ecel_id)){
  # get list of csvs available for patient (and remove home)
  tmp <- paste(
    loc_ecel_id[i],
    list.files(path = loc_ecel_id[i], pattern = "*.csv"),
    sep = "/")
  list_tmp <- list()
  # read in all the patient-specific data
  for(z in 1:length(tmp)){
    tmp_df <- fread(tmp[z], skip = "emperatur", fill = TRUE, select = 1:4)
    list_tmp[[z]] <- tmp_df
  } # end ID-specific file grabbing loop
  # format the patient=specific list data
  names(list_tmp) <-  tmp
  # below broken
  list_tmp <- mapply(cbind, list_tmp,
                     "id" = sub(".*\\_", "", loc_ecel_id[i]), # change / to _
                     "visit" = gsub(".*[V]([^.]+)[_].*", "\\1",  names(list_tmp)),
                     SIMPLIFY = FALSE)
  list_tmp <- lapply(list_tmp, setNames, ecel_colnames)
  list_ecel[[i]] <- rbindlist(list_tmp)
  names(list_ecel)[i] <- sub(".*\\/", "", loc_ecel_id[i])
} # end ID folder loop


# Fix individual files ---------------------------------------------------------

# P4-V3:  Extra leading columns
# first remove bad version from results
list_ecel[["ID_4"]] <- list_ecel[["ID_4"]][!(list_ecel[["ID_4"]]$visit==3),]
# then read in the specific file and fix it
id_p4 <- grep("ID_4", loc_ecel_id)
x <- readLines(paste(loc_ecel_id[id_p4], "ID4_V3_e-Cel.csv", sep = "/"), encoding = "UTF-8")
x <- gsub('^;+','', x)
tmp_df <- fread(text = x, skip = "emperatur", fill = TRUE, header = TRUE, select = 1:4)
tmp_df <- tmp_df[!(is.na(tmp_df$`Sample number`)),]
# then format corrected result
tmp_df$id <- 4
tmp_df$visit <- 3
colnames(tmp_df) <- ecel_colnames
# then add back to the full results
new4 <- rbind(list_ecel$ID_4, tmp_df)
list_ecel[["ID_4"]] <- new4

# P8-V3:  Extra leading columns
# first remove bad version from results
list_ecel[["ID_8"]] <- list_ecel[["ID_8"]][!(list_ecel[["ID_8"]]$visit==3),]
# then read in the specific file and fix it
id_p8 <- grep("ID_8", loc_ecel_id)
x <- readLines(paste(loc_ecel_id[id_p8], "ID8_V3_e-Cel.csv", sep = "/"))
x <- gsub('^;+','', x)
tmp_df <- fread(text = x, skip = "emperatur", fill = TRUE, header = TRUE, select = 1:4)
# then format corrected result
tmp_df$id <- 8
tmp_df$visit <- 3
colnames(tmp_df) <- ecel_colnames
# then add back to the full results
new8 <- rbind(list_ecel$ID_8, tmp_df)
list_ecel[["ID_8"]] <- new8

# P6-V3:  double-up on Sample and Date columns
# first remove bad version from results
list_ecel[["ID_6"]] <- list_ecel[["ID_6"]][!(list_ecel[["ID_6"]]$visit==3),]
# then read in the specific file and fix it
id_p6 <- grep("ID_6", loc_ecel_id)
tmp_df <- fread(paste(loc_ecel_id[id_p6], "ID6_V3_e-Cel.csv", sep = "/"),
                skip = "emperatur", fill = TRUE, header = TRUE, select = 3:6)
# then format corrected result
tmp_df$id <- 6
tmp_df$visit <- 3
colnames(tmp_df) <- ecel_colnames
# then add back to the full results
list_ecel[["ID_6"]] <- rbind(list_ecel$ID_6, tmp_df)


# Work on combined results -----------------------------------------------------

# combine into single df_ecel
df_ecel <- rbindlist(list_ecel)
df_ecel <- df_ecel[!is.na(df_ecel$temp_core_ecel), ]
# 20795

# clean up
rm(list_ecel, loc_ecel, loc_ecel_id, list_tmp, tmp_df, tmp, i, z)

# reformatting
df_ecel$visit <- as.factor(df_ecel$visit)
df_ecel$time_posix <- as.POSIXct(strptime(df_ecel$time, format="%H:%M:%S"))
#df_ecel$time_K <- format(df_ecel$time, format = '%T')
df_ecel$lubtime_ecel <- lubridate::hms(df_ecel$time)
#df_ecel$seconds <- period_to_seconds(df_ecel$times)
df_ecel$temp_core_ecel <- as.numeric(df_ecel$temp_core_ecel)

rm(ecel_colnames)

# remove extra data from P2-V4, P8-V4, P15-V2
# table(df_ecel$id, df_ecel$visit, useNA = "always")
# P2-V4 seems to have a lot of missing, or visit started later?
# P8-V4 - <name> to clean
# P15-V2 - <name> to clean

# Remove extraneous data from "long" visits
# KAR: repeat with actual start and end times!!!
# P2-V4
# subset data for p-v
df_ecel_p2v4 <- df_ecel[(df_ecel$id == 2 & df_ecel$visit == 4),]
# remove from real data
df_ecel <- df_ecel[!(df_ecel$id == 2 & df_ecel$visit == 4),]
# correct the subset
df_ecel_p2v4 <- df_ecel_p2v4[(df_ecel_p2v4$date >"<redacted>" &
                                df_ecel_p2v4$time > "<redacted>"), ]
# P8-V4
# subset data for p-v
df_ecel_p8v4 <- df_ecel[(df_ecel$id == 8 & df_ecel$visit == 4),]
# remove from real data
df_ecel <- df_ecel[!(df_ecel$id == 8 & df_ecel$visit == 4),]
# correct the subset
df_ecel_p8v4 <- df_ecel_p8v4[(df_ecel_p8v4$date > "<redacted>" &
                                df_ecel_p8v4$time > "<redacted>"), ]
# P15-V2
# subset data for p-v
df_ecel_p15v2 <- df_ecel[(df_ecel$id == 15 & df_ecel$visit == 2),]
# remove from real data
df_ecel <- df_ecel[!(df_ecel$id == 15 & df_ecel$visit == 2),]
# correct the subset
df_ecel_p15v2 <- df_ecel_p15v2[(df_ecel_p15v2$date > "<redacted>" &
                                  df_ecel_p15v2$time > "<redacted>"), ]

# add all corrected subsets back to main data
df_ecel <- rbind(df_ecel, df_ecel_p2v4, df_ecel_p8v4, df_ecel_p15v2)

# format
colnames(df_ecel)[colnames(df_ecel) == "id"] <- "pid"
df_ecel$pid <- as.numeric(df_ecel$pid)
df_ecel <- as.data.frame(merge(df_ecel,
                               df_times[ , c("pid", "visit", "heatstress_type",
                                             "start_acclim", "start_heat",
                                             "start_heat_cloth", "start_cool")],
                               by = c("pid", "visit"), all = TRUE))
df_ecel$pid <- as.numeric(df_ecel$pid)
df_ecel$visit <- factor(df_ecel$visit)
df_ecel$temp_core_ecel <- as.numeric(df_ecel$temp_core_ecel)

# compute time since acclimation phase start
df_ecel$t_min_ecel <- as.numeric(difftime(df_ecel$time_posix,
                                          df_ecel$start_acclim, units = "mins"))
#a <- aggregate(df_ecel$tdiff_study, list(df_ecel$pid, df_ecel$visit), FUN=mean)

# remove placeholder for P5-V2
df_ecel <- df_ecel[!is.na(df_ecel$t_min_ecel), ]

# add in phase
df_ecel$phase <- NA
for(i in 1:nrow(df_ecel)){
  if(df_ecel$t_min_ecel[i] >= 90){df_ecel$phase[i] <- "Cool"}
  else if(df_ecel$t_min_ecel[i] < 90 &
          df_ecel$t_min_ecel[i] >= 60){df_ecel$phase[i] <- "Heat+Cloth"}
  else if(df_ecel$t_min_ecel[i] < 60 &
          df_ecel$t_min_ecel[i] >= 30){df_ecel$phase[i] <- "Heat"}
  else{df_ecel$phase[i] <- "Acclim"}
} # end loop

# check
#table(df_ecel$pid, df_ecel$visit)
df_ecel <- df_ecel[ , c("pid", "visit", "heatstress_type",
                        "time_posix",   "start_acclim", "t_min_ecel",
                        "phase", "temp_core_ecel")]

# save it
save(df_ecel, file = paste(Rproj_path, "/data/ts_ecel.RData", sep = ""))
# export it
writexl::write_xlsx(df_ecel,
                    path = paste(mytabs, "/ecel_formatted.xlsx", sep = ""))

# clean up
rm(df_ecel_p2v4, df_ecel_p8v4, df_ecel_p15v2, new4, new8, id_p4, id_p6, id_p8)
