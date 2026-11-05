# ARCHIVED, NOT RUN. See README.md in this folder.
# Extract of dataprep_baspo.R by the earlier statisticians, kept as found: the raw exports
# are not available, so this code could not be run or verified. The plotting
# section moved to code/supplement_timeseries.R. Visit dates and clock times
# used as cut-offs are redacted.

###
### Project:     HeatSTAR
### File:        Data preparation: baspo (WBGT)
### Description: 
###  

# Collected at Visits 2, 3, 4

# Get data from individual folders ---------------------------------------------
loc_baspo <- paste(data_path, "/BASPO-Temp-rel.Hum", sep = "")
loc_baspo_id <- paste(loc_baspo, list.files(path = loc_baspo), sep = "/") 
loc_baspo_idvisit <- c(paste(loc_baspo_id, "/V2", sep = ""),
                       paste(loc_baspo_id, "/V3", sep = ""),
                       paste(loc_baspo_id, "/V4", sep = ""))
list_baspo <- list()

for(i in 1:length(loc_baspo_idvisit)){
  print(loc_baspo_idvisit[i])
  # get list of csvs available for patient (and remove home)
  tmp <- paste(loc_baspo_idvisit[i],
               list.files(path = loc_baspo_idvisit[i], pattern = "*.xlsx"),
               sep = "/")
  tmp <- tmp[grep("bersicht", tmp)] 
  # read in the patient/visit-specific data
  df_baspo_idvis <- readxl::read_xlsx(tmp, skip = 1, col_names = TRUE)
  df_baspo_idvis <- df_baspo_idvis[ , 1:which(colnames(df_baspo_idvis)== "Humidity (%)")]
  # format data
  tmp2 <- sub(".*\\_", "", loc_baspo_idvisit[i])
  df_baspo_idvis$pid <- sub("\\/.*", "", tmp2)
  df_baspo_idvis$visit <- sub(".*\\V", "", tmp2)
  list_baspo[[i]] <- df_baspo_idvis
  #names(list_holt)[i] <- sub(".*\\/", "", loc_holt_id[i])
} # end ID folder loop


# Work with combind results ----------------------------------------------------

df_baspo <- data.frame(rbindlist(list_baspo))
colnames(df_baspo) <- c("Date", "Time", "WBGT_in", "WBGT_out", "TA", 
                        "TG", "WET", "DEW", "Humidity", "pid", "visit")

# reformatting 
df_baspo <- df_baspo[!is.na(df_baspo$Date), ]
df_baspo$visit <- as.factor(df_baspo$visit)
df_baspo$pid <- as.numeric(df_baspo$pid)
#df_holt$time_posix <- as.POSIXct(strptime(df_holt$zeit, format="%H:%M:%S"))

# clean up
rm(list_baspo, loc_baspo, loc_baspo_id, loc_baspo_idvisit, tmp2, tmp,
   df_baspo_idvis)

df_baspo <- as.data.frame(merge(df_baspo, df_times[, c("pid", "visit",
                                                       "heatstress_type", 
                                                       "start_acclim")],
                                by = c("pid", "visit"), all = TRUE))

# 
df_baspo$t_clock <- as.POSIXct(sub("^\\S+\\s+", '', df_baspo$Time),format="%H:%M:%S")
df_baspo$t_min_baspo <- as.numeric(difftime(df_baspo$t_clock, df_baspo$start_acclim, units = "mins"))

# remove pid5-visit2
df_baspo <- df_baspo[ !(df_baspo$pid == 5 & df_baspo$visit == "2"), ]

# check
#table(df_baspo$pid, df_baspo$visit)

# add in phase
df_baspo$phase <- NA
for(i in 1:nrow(df_baspo)){
  if(df_baspo$t_min_baspo[i] >= 90){df_baspo$phase[i] <- "Cool"}
  else if(df_baspo$t_min_baspo[i] < 90 & 
          df_baspo$t_min_baspo[i] >= 60){df_baspo$phase[i] <- "Heat+Cloth"}
  else if(df_baspo$t_min_baspo[i] < 60 & 
          df_baspo$t_min_baspo[i] >= 30){df_baspo$phase[i] <- "Heat"}
  else{df_baspo$phase[i] <- "Acclim"}
} # end loop

# save it
save(df_baspo, file = paste(Rproj_path, "/data/ts_baspo.RData", sep = ""))
# export it
writexl::write_xlsx(df_baspo,
                    path = paste(mytabs, "/baspo_formatted.xlsx", sep = ""))
