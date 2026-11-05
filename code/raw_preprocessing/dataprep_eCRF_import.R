# ARCHIVED, NOT RUN. See README.md in this folder.
# Extract of dataprep_eCRF.R by the earlier statisticians, kept as found: the raw exports
# are not available, so this code could not be run or verified. The plotting
# section moved to code/supplement_timeseries.R. Visit dates and clock times
# used as cut-offs are redacted.

###
### Project:     HeatSTAR
### File:        Data preparation: eCRF (RedCap) data
### Description: 
###              



# get eCRF data and metadata
data <- read.csv(Rproj_path %&% "/data/HEATSTAR_Export_2023-02-27/HEATSTAR_DATA_2023-02-27_0948.csv")

# apply CTU data prep script (skip first few lines)
source2(Rproj_path %&% "/data/HEATSTAR_Export_2023-02-27/HEATSTAR_R_2023-02-27_0948_nodoubles.r",
        start = 9, end = 450)


# Basic formatting -------------------------------------------------------------
data$pid <- as.numeric(sub("\\D*(\\d+).*", "\\1", data$pid))

# create visit number variable
data$visit <- sub("\\D*(\\d+).*", "\\1", data$redcap_event_name)


# Clean up ---------------------------------------------------------------------

# The result of these steps is stored as `data_OG` in data_eCRFbased.RData.
# code/prepare_data.R continues from there.
