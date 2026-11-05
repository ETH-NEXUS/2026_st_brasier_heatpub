# ARCHIVED, NOT RUN. See README.md in this folder.
# Extract of dataprep_ibutton.R by the earlier statisticians, kept as found: the raw exports
# are not available, so this code could not be run or verified. The plotting
# section moved to code/supplement_timeseries.R. Visit dates and clock times
# used as cut-offs, and a colleague's name, are redacted.

###
### Project:     HeatSTAR
### File:        Data preparation: iButton
### Description: 
###  


# Collected at Visits Home, 2, 3, 4

# Issues
# ID5 missing V2
# ID18 has V4 but in a separate folder?
# ID20 missing V4

# KAR: discuss w <name>!!!
# for almost all but pid 1, the files are a mix of , and ; separated
# i can't work with inconsistent data

# Get data for all patients and visits -----------------------------------------
loc_ib <- paste(data_path, "/iButton Logger/", sep = "")
loc_ib_id <- paste(loc_ib, list.files(path = loc_ib), sep = "")
list_ibutton <- list()

for(i in 1:length(loc_ib_id)){
  # get list of csvs available for patient (and remove home)
  tmp <- paste(loc_ib_id[i], list.files(path = loc_ib_id[i], pattern = "*.csv"),
               sep = "/")
  tmp <- tmp[-grep("Home", tmp)] # this is ;-sep instead of ,-sep and I don't need
  list_tmp <- list()
  # read in all the patient-specific data
  for(z in 1:length(tmp)){
    #tmp_df <- read.csv(tmp[z], row.names = NULL)
    #x <- grep("Date", tmp_df[,1])
    #list_tmp[[z]] <- tmp_df[(x+1):nrow(tmp_df), ]
    ##x <- readLines(tmp[z])
    ##x <- gsub('\"','', x); x <- gsub(' ','', x)
    tmp_df <- fread(text = tmp[z],
                    skip = "emperatur", fill = TRUE, header = TRUE, 
                    sep = ",", 
                    quote = "")
    list_tmp[[z]] <- tmp_df
  } # end ID-specific file grabbing loop
  # format the patient=specific list data
  names(list_tmp) <- gsub("_iBu.csv", "", sub(".*\\/", "", tmp))
  # below broken
  list_tmp <- mapply(cbind, list_tmp,
                     "id" = sub(".*\\_", "", loc_ib_id[i]) , SIMPLIFY = FALSE , # / to _
                     "visit" = sub(".*\\_", "", names(list_tmp)))
  list_tmp <- lapply(list_tmp, setNames, c("date", "temp_ib", "hum_ib", "id", "visit"))
  list_ibutton[[i]] <- rbindlist(list_tmp)
  names(list_ibutton)[i] <- sub(".*\\/", "", loc_ib_id[i])
} # end ID folder loop


# Work with combind results ----------------------------------------------------

df_ib <- rbindlist(list_ibutton)
df_ib <- df_ib[grep("end", df_ib$date, invert = TRUE), ]
df_ib <- df_ib[!is.na(df_ib$temp_ib), ]

# reformatting
df_ib$visit <- gsub("V", "", df_ib$visit)
df_ib$visit <- gsub(".csv", "", df_ib$visit)
df_ib$visit <- factor(df_ib$visit, levels = c(#"Home", 
  "2", "3", "4"))
df_ib$hum_ib <- as.numeric(gsub(";", "", df_ib$hum_ib))

df_ib$time <- gsub("^\\S+\\s+", '', df_ib$date)
df_ib$time <- gsub('[^[:alnum:] ]', '', df_ib$time)
df_ib$time <- gsub('^(.{2})(.*)$', '\\1:\\2', df_ib$time)

df_ib$time_posix <- as.POSIXct(strptime(df_ib$time, format="%H:%M"))
df_ib$pid <- as.numeric(df_ib$id)

# clean up
rm(tmp, loc_ib, loc_ib_id, tmp_df, list_tmp, list_ibutton)

# time for ibutton is over a week for Home, so separate
#df_ib_home <- df_ib[df_ib$visit == "Home",]
#df_ib <- df_ib[df_ib$visit != "Home",]

# format home data
#df_ib_home$id <- factor(df_ib_home$id)
#df_ib_home$date2 <- as.POSIXct(strptime(
#  paste(substr(df_ib_home$date, 1, 10), substring(df_ib_home$date, 11)),
#  format="%d.%m.%Y %H:%M"))

# format rest of data
# add in heat stress type
df_ib <- as.data.frame(merge(df_ib, df_times[ , c("pid", "visit",
                                                  "heatstress_type",
                                                  "start_acclim")],
                             by = c("pid", "visit"), all = TRUE))
df_ib <- df_ib[ , !(colnames(df_ib) %in% c("id"))]
df_ib$visit <- factor(df_ib$visit)

# check
#table(df_ib$pid,df_ib$visit)

# compute time since acclimation phase start
df_ib$t_min_ib <- as.numeric(difftime(df_ib$time_posix, df_ib$start_acclim,
                                      units = "mins"))

# remove placeholder for P5-V2
df_ib <- df_ib[!is.na(df_ib$t_min_ib), ]

# add in phase
df_ib$phase <- NA
for(i in 1:nrow(df_ib)){
  if(df_ib$t_min_ib[i] >= 90){df_ib$phase[i] <- "Cool"}
  else if(df_ib$t_min_ib[i] < 90 &
          df_ib$t_min_ib[i] >= 60){df_ib$phase[i] <- "Heat+Cloth"}
  else if(df_ib$t_min_ib[i] < 60 &
          df_ib$t_min_ib[i] >= 30){df_ib$phase[i] <- "Heat"}
  else{df_ib$phase[i] <- "Acclim"}
} # end loop

df_ib <- df_ib[ , c("pid", "visit", "heatstress_type",
                    "time_posix",   "start_acclim", "t_min_ib",
                    "phase", "temp_ib", "hum_ib")]

# save it
save(df_ib, file = paste(Rproj_path, "/data/ts_ibutton.RData", sep = ""))
# export it
writexl::write_xlsx(df_ib,
                    path = paste(mytabs, "/ibutton_formatted.xlsx", sep = ""))
