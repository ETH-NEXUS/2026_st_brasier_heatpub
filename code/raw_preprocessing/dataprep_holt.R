# ARCHIVED, NOT RUN. See README.md in this folder.
# Extract of dataprep_holt.R by the earlier statisticians, kept as found: the raw exports
# are not available, so this code could not be run or verified. The plotting
# section moved to code/supplement_timeseries.R. Visit dates and clock times
# used as cut-offs are redacted.

###
### Project:     HeatSTAR
### File:        Data preparation: Holt monitor data
### Description: 
###  


# Collected at Visits 2, 3, 4

# Get data from individual folders ---------------------------------------------
loc_holt <- paste(data_path, "/EKG/CSV files", sep = "")
loc_holt_id <- paste(loc_holt, list.files(path = loc_holt), sep = "/")
loc_holt_id <- loc_holt_id[grep("dropout", loc_holt_id, invert = TRUE)]
list_holt <- list()

for(i in 1:length(loc_holt_id)){
  # get list of csvs available for patient (and remove home)
  tmp <- paste(loc_holt_id[i],
               list.files(path = loc_holt_id[i], pattern = "*.csv"),
               sep = "/")
  #tmp <- tmp[-grep("V1", tmp)] # I don't need
  list_tmp <- list()
  # read in all the patient-specific data
  for(z in 1:length(tmp)){
    tmp_df <- fread(tmp[z], skip = 2, fill = TRUE, header = FALSE,
                    col.names = c("zeit", "rMSSD", "SDNN", "hr_holt"))
    #read_excel(tmp[z], col_names = FALSE, skip = 3)
    list_tmp[[z]] <- tmp_df
  } # end ID-specific file grabbing loop
  # format the patient=specific list data
  names(list_tmp) <-  tmp
  # below broken
  list_tmp <- mapply(cbind, list_tmp,
                     "id" = sub(".*\\_", "", loc_holt_id[i]),
                     "visit" = gsub(".*[V]([^.]+)[_].*", "\\1",  names(list_tmp)),
                     SIMPLIFY = FALSE)
  list_holt[[i]] <- rbindlist(list_tmp)
  names(list_holt)[i] <- sub(".*\\/", "", loc_holt_id[i])
} # end ID folder loop


# Work with combind results ----------------------------------------------------

df_holt <- rbindlist(list_holt)

# reformatting
df_holt$visit <- as.factor(df_holt$visit)
df_holt$time_posix <- as.POSIXct(strptime(df_holt$zeit, format="%H:%M:%S"))
df_holt$rMSSD <- as.numeric(gsub(",", ".", df_holt$rMSSD))
df_holt$SDNN <- as.numeric(gsub(",", ".", df_holt$SDNN))
df_holt$hr_holt <- as.numeric(df_holt$hr_holt)
df_holt$id <- as.numeric(df_holt$id)

df_holt <- df_holt[!(df_holt$hr_holt == Inf),] # removes Inf and NA
# checked - they are missing the other data too

# clean up
rm(list_holt, loc_holt, loc_holt_id, list_tmp, tmp_df, tmp)

# rename id to pid
colnames(df_holt)[colnames(df_holt) == "id"] <- "pid"
df_holt <- as.data.frame(merge(df_holt, df_times[, c("pid", "visit",
                                                     "heatstress_type",
                                                     "start_acclim")],
                               by = c("pid", "visit"), all = TRUE))

# put first visit data off to side
df_holt_full <- df_holt
df_holt <- df_holt[df_holt$visit != "1", ]

# check
#table(df_holt$pid, df_holt$visit)

# format
df_holt$visit <- factor(df_holt$visit)

# compute time since acclimation phase start
df_holt$t_min_holt <- as.numeric(difftime(df_holt$time_posix,
                                          df_holt$start_acclim, units = "mins"))

# remove placeholder for P5-V2
df_holt <- df_holt[!is.na(df_holt$t_min_holt), ]

# add in phase
df_holt$phase <- NA
for(i in 1:nrow(df_holt)){
  if(df_holt$t_min_holt[i] >= 90){df_holt$phase[i] <- "Cool"}
  else if(df_holt$t_min_holt[i] < 90 &
          df_holt$t_min_holt[i] >= 60){df_holt$phase[i] <- "Heat+Cloth"}
  else if(df_holt$t_min_holt[i] < 60 &
          df_holt$t_min_holt[i] >= 30){df_holt$phase[i] <- "Heat"}
  else{df_holt$phase[i] <- "Acclim"}
} # end loop

df_holt <- df_holt[ , c("pid", "visit", "heatstress_type",
                        "time_posix",   "start_acclim", "t_min_holt",
                        "phase", "rMSSD", "SDNN", "hr_holt")]

# save it
save(df_holt, file = paste(Rproj_path, "/data/ts_holt.RData", sep = ""))
# export it
writexl::write_xlsx(df_holt,
                    path = paste(mytabs, "/holt_formatted.xlsx", sep = ""))
