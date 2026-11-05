# Descriptive results: Table 1, Table 2, whole-body sweat loss, and the core
# temperature, heart rate and local sweat rate means by stressor and phase.
#
# Adapted from the report heatSTAR_2022_webreport_v6 (Table 1), desc_WBGT.R
# (Table 2), summarize_waterloss.R (sweat loss) and po_descriptiveanalysis.R
# (means by stressor and phase) by the earlier statisticians.

suppressMessages(here::i_am("code/descriptives.R"))
source(here::here("code/setup.R"))
load_packages(c("MASS", "dplyr", "lme4", "lmerTest", "sessioninfo", "tibble",
                "writexl"))

out_dir <- create_dir(here::here("output/descriptives"))
load(prepared_data_file)

# ── Table 1: participant characteristics ─────────────────────────────────────

summarise_numeric <- function(variable, unit, x) {
    tibble(
        Variable = variable,
        Unit = unit,
        Mean = mean(x, na.rm = TRUE),
        SD = sd(x, na.rm = TRUE),
        N = sum(!is.na(x)),
        Missing = sum(is.na(x))
    )
}

table1 <- bind_rows(
    summarise_numeric("Age", "years", as.numeric(df_base$age)),
    summarise_numeric("Height", "cm", as.numeric(df_base$height)),
    summarise_numeric("Weight", "kg", as.numeric(df_base$weight)),
    summarise_numeric("BMI", "kg/m2", as.numeric(df_base$bmi)),
    summarise_numeric("BSA", "m2", as.numeric(df_base$bsa)),
    summarise_numeric("VO2 max", "ml/(kg*min)", as.numeric(df_base$v02max)),
    summarise_numeric("Max HR", "n/min", as.numeric(df_base$maxheartrate)),
    summarise_numeric("Close-body temperature at home", "degrees C",
                      df_home$home_temperature),
    summarise_numeric("Close-body relative humidity at home", "%",
                      df_home$home_humidity)
)

table1_sex <- as.data.frame(table(Sex = as.character(df_base$sex))) |>
    mutate(Percent = 100 * Freq / sum(Freq))

# ── Whole-body sweat loss ────────────────────────────────────────────────────

# Water drunk during the visit and urine produced are accounted for. As in the
# published analysis, every visit with weight data contributes: participant 18,
# visit 4 stays in, because the unusable recording is that of the e-Celsius.
visit_rows <- data_OG$visit %in% c("2", "3", "4")
weights <- data.frame(
    pid = data_OG$pid[visit_rows],
    visit = as.character(data_OG$visit[visit_rows]),
    start = as.numeric(data_OG$bodyweight_nude_start[visit_rows]),
    end = as.numeric(data_OG$bodyweight_nude_end[visit_rows]),
    drunk = as.numeric(data_OG$bottleweight_before[visit_rows] -
                       data_OG$bottleweight_after[visit_rows]) / 1000,
    urine = as.numeric(data_OG$urination_volume[visit_rows]) / 1000
)
weights$loss <- weights$start + weights$drunk - weights$end
weights$loss_sweat <- ifelse(!is.na(weights$urine),
                             weights$loss - weights$urine, weights$loss)

sweat_loss <- merge(weights[c("pid", "visit", "loss_sweat")],
                    df_times[c("pid", "visit", "heatstress_type")],
                    by = c("pid", "visit"), all.y = TRUE)
sweat_loss <- merge(sweat_loss, df_base[c("pid", "bsa")], by = "pid",
                    all.x = TRUE)
sweat_loss$loss_per_m2 <- sweat_loss$loss_sweat / as.numeric(sweat_loss$bsa)

whole_body_sweat_loss <- sweat_loss |>
    mutate(Stressor = as.character(heatstress_type)) |>
    group_by(Stressor) |>
    summarise(
        Mean_L_per_m2 = mean(loss_per_m2, na.rm = TRUE),
        SD_L_per_m2 = sd(loss_per_m2, na.rm = TRUE),
        N = sum(!is.na(loss_per_m2))
    )

# ── Means by stressor and phase ──────────────────────────────────────────────

# Means and SDs pool all records of a stressor and phase, as in the published
# analysis. For the devices these records include those before minute 0 and
# from minute 120 (see phase_from_minutes() in prepare_data.R).
summarise_by_phase <- function(data, variable, measure) {
    data[!is_excluded_visit(data$pid, data$visit), ] |>
        filter(!is.na(.data[[measure]])) |>
        mutate(
            Stressor = as.character(heatstress_type),
            Phase = as.character(phase)
        ) |>
        group_by(Stressor, Phase) |>
        summarise(
            Mean = mean(.data[[measure]]),
            SD = sd(.data[[measure]]),
            N = n(),
            .groups = "drop"
        ) |>
        mutate(Variable = variable, .before = 1)
}

means_by_phase <- bind_rows(
    summarise_by_phase(df_ecel, "CBT", "temp_core_ecel"),
    summarise_by_phase(df_holt, "HR", "hr_holt"),
    summarise_by_phase(df_psweat_long, "LSR", "sweatrate")
)

max_lsr <- df_psweat_long[
    !is_excluded_visit(df_psweat_long$pid, df_psweat_long$visit),
] |>
    filter(!is.na(sweatrate)) |>
    slice_max(sweatrate, n = 1, with_ties = FALSE) |>
    transmute(
        Stressor = as.character(heatstress_type),
        Phase = as.character(phase),
        Max_LSR = sweatrate
    )

# ── Table 2: WBGT by stressor and phase ──────────────────────────────────────

# Laboratory conditions: participant 18, visit 4 stays in, as in the published
# analysis, because the WBGT recording of that visit is usable.
wbgt <- df_baspo[!is.na(df_baspo$WBGT_in), ] |>
    group_by(pid, heatstress_type, phase) |>
    summarise(mean = mean(WBGT_in), .groups = "drop") |>
    as.data.frame()

wbgt$stress <- ifelse(grepl("temp", wbgt$heatstress_type), "temp",
                      ifelse(grepl("hum", wbgt$heatstress_type), "hum",
                             "extert"))
wbgt$group <- paste(wbgt$stress, wbgt$phase, sep = "_")

# The contrasts and labels below rely on the alphabetical order of the groups.
stopifnot(identical(
    sort(unique(wbgt$group)),
    paste(rep(c("extert", "hum", "temp"), each = 4),
          rep(c("Acclim", "Cool", "Heat", "Heat+Cloth"), times = 3), sep = "_")
))

# Each phase against acclimatization, within each stressor
phase_contrasts <- rbind(
    c(-1, 1, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0),
    c(-1, 0, 1, 0,  0, 0, 0, 0,  0, 0, 0, 0),
    c(-1, 0, 0, 1,  0, 0, 0, 0,  0, 0, 0, 0),
    c(0, 0, 0, 0,  -1, 1, 0, 0,  0, 0, 0, 0),
    c(0, 0, 0, 0,  -1, 0, 1, 0,  0, 0, 0, 0),
    c(0, 0, 0, 0,  -1, 0, 0, 1,  0, 0, 0, 0),
    c(0, 0, 0, 0,  0, 0, 0, 0,  -1, 1, 0, 0),
    c(0, 0, 0, 0,  0, 0, 0, 0,  -1, 0, 1, 0),
    c(0, 0, 0, 0,  0, 0, 0, 0,  -1, 0, 0, 1)
)

model_estimates <- function(model) {
    # The first two rows of the profile intervals are the variance parameters.
    intervals <- suppressMessages(confint(model))[-c(1, 2), ]
    data.frame(
        Estimate = coef(summary(model))[, "Estimate"],
        Lower = intervals[, 1],
        Upper = intervals[, 2],
        p_value = coef(summary(model))[, "Pr(>|t|)"]
    )
}

stressors <- c("Exertion", "Humidity", "Temperature")

group_means <- model_estimates(
    lmer(mean ~ -1 + group + (1 | pid), data = wbgt)
)
group_means <- tibble(
    Stressor = rep(stressors, each = 4),
    Phase = rep(c("Acclimation", "Cool", "Stress", "Stress+Cl"), times = 3),
    Measured = group_means$Estimate
)

phase_differences <- model_estimates(
    lmer(mean ~ 1 + group + (1 | pid), data = wbgt,
         contrasts = list(group = ginv(phase_contrasts)))
)[-1, ]
phase_differences <- tibble(
    Stressor = rep(stressors, each = 3),
    Phase = rep(c("Cool", "Stress", "Stress+Cl"), times = 3),
    Difference = phase_differences$Estimate,
    Lower = phase_differences$Lower,
    Upper = phase_differences$Upper,
    p_value = phase_differences$p_value
)

table2 <- left_join(group_means, phase_differences,
                    by = c("Stressor", "Phase")) |>
    mutate(
        Stressor = factor(Stressor, c("Temperature", "Humidity", "Exertion")),
        Phase = factor(Phase,
                       c("Acclimation", "Stress", "Stress+Cl", "Cool"))
    ) |>
    arrange(Stressor, Phase) |>
    mutate(across(c(Stressor, Phase), as.character))

# ── Export ───────────────────────────────────────────────────────────────────

write_xlsx(path = file.path(out_dir, "results.xlsx"), list(
    `Table 1` = table1,
    `Table 1 Sex` = table1_sex,
    `Table 2` = table2,
    `Whole-Body Sweat Loss` = whole_body_sweat_loss,
    `Means by Phase` = means_by_phase,
    `Maximum LSR` = max_lsr
))
write_session_info(out_dir)
cat("Descriptive results saved to", out_dir, "\n")
