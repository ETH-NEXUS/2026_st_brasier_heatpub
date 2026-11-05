# Prepare the analysis data from the earliest available inputs (see
# data/README.md).
#
# The eCRF tables are regenerated from `data_OG`, the REDCap export stored
# before any correction. The stored copies of these tables serve as a check.
# The device files already contain cleaned recordings: their raw exports are
# not available, so only their derived columns can be verified here.
#
# Adapted from dataprep_eCRF.R by the earlier statisticians. The step reading
# the REDCap export is documented in code/raw_preprocessing/.

suppressMessages(here::i_am("code/prepare_data.R"))
source(here::here("code/setup.R"))
# Hmisc is needed even though no function is called: its subsetting method
# keeps the variable labels of the eCRF columns, as in the stored tables.
load_packages(c("Hmisc", "readxl", "sessioninfo", "tidyr"))

out_dir <- create_dir(dirname(prepared_data_file))

stored <- new.env()
load(file.path(data_dir, "data_eCRFbased.RData"), envir = stored)
data_OG <- stored$data_OG

dropouts <- c(14, 16, 17)

# The stored tables were built on 2023-10-13. Their clock times carry that
# date, because strptime() uses the current date when none is given. Stating
# it keeps the results independent of the day of the run. Only differences
# between these times are used in the analyses.
clock_time <- function(x) {
    as.POSIXct(strptime(paste("2023-10-13", x), format = "%Y-%m-%d %H:%M"))
}

# ── eCRF: corrections and removal of the serious event records ───────────────

df_eCRF <- data_OG
drop <- c("redcap_event_name", "redcap_repeat_instrument",
          "redcap_repeat_instance")
df_eCRF <- df_eCRF[, !(colnames(df_eCRF) %in% drop)]

# Correct the misclassified stress sequence of participant 5 (decided at the
# meeting of 10.05.2023). The sequence is defined at visit 1 only.
df_eCRF$randseq[df_eCRF$pid == 5 & df_eCRF$visit == 1] <- "ETH"
df_eCRF$heatstress_type[df_eCRF$pid == 5 & df_eCRF$visit == 3] <- "temperature"
df_eCRF$heatstress_type[df_eCRF$pid == 5 & df_eCRF$visit == 4] <-
    "relative humidity"

df_eCRF_SE <- df_eCRF[df_eCRF$visit == "Serious Events", ]
df_eCRF <- df_eCRF[df_eCRF$visit != "Serious Events", ]

se_start <- which(colnames(df_eCRF_SE) == "se_version")
se_end <- which(colnames(df_eCRF_SE) == "serious_event_report_complete")
df_eCRF_SE <- df_eCRF_SE[, se_start:se_end]
df_eCRF <- df_eCRF[, -(se_start:se_end)]

# ── eCRF: screening data ─────────────────────────────────────────────────────

screen_start <- which(colnames(df_eCRF) == "incl_consent")
screen_end <- which(colnames(df_eCRF) == "screening_complete")
df_screen <- df_eCRF[, 1:screen_end]
df_screen <- df_screen[!is.na(df_screen$incl_consent), ]
df_eCRF <- df_eCRF[, -(screen_start:screen_end)]

# ── eCRF: participant characteristics ────────────────────────────────────────

base_vars <- c(
    "pid", "randseq", "age", "sex", "height", "weight", "bmi", "bsa",
    "maxheartrate", "bp_base_sys", "bp_base_dia", "v02max", "stress_cause",
    "stress_symptoms", "stress_coping", "stress_stabilisation"
)
df_base <- df_eCRF[df_eCRF$visit == 1, colnames(df_eCRF) %in% base_vars]
df_base <- df_base[!(df_base$pid %in% dropouts), ]
colnames(df_base) <- gsub("stress", "DSI", colnames(df_base))

# ── eCRF: visit timing ───────────────────────────────────────────────────────

time_vars <- c(
    "pid", "visit", "visit_date", "visit_time", "acclim_time",
    "heatstress_time", "cooldown_time", "heatstress_type"
)
df_times <- df_eCRF[df_eCRF$visit %in% c("2", "3", "4"),
                    colnames(df_eCRF) %in% time_vars]
df_times <- df_times[!(df_times$pid %in% dropouts), ]

# Empty cells of the visit without usable timing
empty_visit <- df_times$pid == 5 & df_times$visit == 2
for (time_var in c("visit_time", "acclim_time", "heatstress_time",
                   "cooldown_time")) {
    df_times[[time_var]][empty_visit] <- NA
}

start_acclim <- clock_time(df_times$acclim_time)
start_heat <- clock_time(df_times$heatstress_time)
start_cool <- clock_time(df_times$cooldown_time)

df_times$start_min <- difftime(start_acclim, start_acclim)
df_times$heatstart_min <- difftime(start_heat, start_acclim)
df_times$coolstart_min <- difftime(start_cool, start_acclim, units = "mins")
df_times$start_acclim <- start_acclim
df_times$start_heat <- start_heat
df_times$start_cool <- start_cool
df_times$start_heat_cloth <- start_heat + 30 * 60

# ── eCRF: local sweat rate from the absorbent pads ───────────────────────────

df_psweat <- df_eCRF[, c("pid", "visit", "sweatrate_12", "sweatrate_22",
                         "sweatrate_32", "sweatrate_42")]
colnames(df_psweat)[3:6] <- c("sweatrate_30", "sweatrate_60", "sweatrate_90",
                              "sweatrate_120")
df_psweat <- df_psweat[df_psweat$visit %in% c("2", "3", "4"), ]
df_psweat <- df_psweat[!(df_psweat$pid %in% dropouts), ]

# Convert from g/(30 min * 15 cm^2) to g/(h * m^2)
df_psweat[, 3:6] <- df_psweat[, 3:6] * 1333.3

df_psweat <- merge(df_psweat, df_times[c("pid", "visit", "heatstress_type")],
                   by = c("pid", "visit"))

# The sweat rate columns have different labels, which gather() drops.
df_psweat_long <- suppressWarnings(
    gather(df_psweat, min, sweatrate, sweatrate_30:sweatrate_120)
)
df_psweat_long$min <- as.numeric(gsub(".*_", "", df_psweat_long$min))
df_psweat_long$phase <- unname(
    c(`30` = "Acclim", `60` = "Heat", `90` = "Heat+Cloth",
      `120` = "Cool")[as.character(df_psweat_long$min)]
)

# ── Check: regenerated eCRF tables against their stored copies ───────────────

# Columns are compared by their values and their labels. Date-times are
# compared as instants, read in the time zone fixed in setup.R.
comparable <- function(column) {
    label <- attr(column, "label", exact = TRUE)
    values <- if (is.factor(column)) as.character(column) else
        as.vector(unclass(column))
    list(values = values, label = label)
}

differing_columns <- function(new, old) {
    if (!identical(dim(new), dim(old)) ||
        !identical(names(new), names(old))) {
        return("<dimensions or column names>")
    }
    same <- vapply(
        names(old),
        \(col) identical(comparable(new[[col]]), comparable(old[[col]])),
        logical(1)
    )
    names(old)[!same]
}

ecrf_tables <- c("df_eCRF", "df_eCRF_SE", "df_screen", "df_base", "df_times",
                 "df_psweat", "df_psweat_long")
for (table_name in ecrf_tables) {
    differing <- differing_columns(get(table_name), stored[[table_name]])
    if (length(differing) > 0) {
        stop(table_name, " differs from its stored copy: ",
             paste(differing, collapse = ", "), call. = FALSE)
    }
    cat("Regenerated", table_name, "matches its stored copy\n")
}

# ── Device files: load and verify the derived columns ────────────────────────

for (file_name in c("ts_ecel", "ts_holt", "ts_ibutton", "ts_baspo")) {
    load(file.path(data_dir, paste0(file_name, ".RData")))
}

# Records before minute 0 count as acclimatization and records from minute 120
# as cool-down, as in the published analysis.
phase_from_minutes <- function(t_min) {
    ifelse(t_min >= 90, "Cool",
           ifelse(t_min >= 60, "Heat+Cloth",
                  ifelse(t_min >= 30, "Heat", "Acclim")))
}

check_device <- function(name, data, t_min, clock) {
    recomputed <- as.numeric(difftime(data[[clock]], data$start_acclim,
                                      units = "mins"))
    times <- unique(data[c("pid", "visit", "heatstress_type", "start_acclim")])
    times <- merge(times, df_times[c("pid", "visit", "heatstress_type",
                                     "start_acclim")],
                   by = c("pid", "visit"), suffixes = c("", "_ecrf"))
    stopifnot(
        isTRUE(all.equal(recomputed, data[[t_min]])),
        identical(as.character(data$phase), phase_from_minutes(data[[t_min]])),
        nrow(times) == 59,
        all(as.character(times$heatstress_type) ==
            as.character(times$heatstress_type_ecrf)),
        all(format(times$start_acclim, "%H:%M") ==
            format(times$start_acclim_ecrf, "%H:%M")),
        !any(data$pid == 5 & data$visit == 2)
    )
    cat("Verified time, phase and stressor in", name, "\n")
}

check_device("df_ecel", df_ecel, "t_min_ecel", "time_posix")
check_device("df_holt", df_holt, "t_min_holt", "time_posix")
check_device("df_ib", df_ib, "t_min_ib", "time_posix")
check_device("df_baspo", df_baspo, "t_min_baspo", "t_clock")

# ── Close-body conditions at home ────────────────────────────────────────────

# One column per participant, one row per measure
home_raw <- as.data.frame(read_excel(
    file.path(data_dir, "Measures at home_iButton.xlsx")
))
stopifnot(identical(home_raw[[1]], c("Temperature at home (°C)",
                                     "Relative Humdity at home (%)")))
df_home <- data.frame(
    home_temperature = as.numeric(home_raw[1, -1]),
    home_humidity = as.numeric(home_raw[2, -1])
)

# ── Save ─────────────────────────────────────────────────────────────────────

save(
    data_OG, df_eCRF, df_base, df_times, df_psweat, df_psweat_long,
    df_ecel, df_holt, df_ib, df_baspo, df_home,
    file = prepared_data_file
)
write_session_info(out_dir)
cat("Prepared data saved to", prepared_data_file, "\n")
