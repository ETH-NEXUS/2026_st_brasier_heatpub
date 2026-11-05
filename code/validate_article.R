# Compare the regenerated results with the values reported in the accepted
# article, listed in code/article_values.csv.
#
# Each result is rounded once to the precision reported in the article. A value
# differing from the article stops the pipeline, unless the difference has been
# examined and is recorded as a known discrepancy in code/article_values.csv.
# Values that a script cannot regenerate are not part of this comparison.

suppressMessages(here::i_am("code/validate_article.R"))
source(here::here("code/setup.R"))
load_packages(c("dplyr", "purrr", "readr", "readxl", "sessioninfo", "tidyr"))

out_dir <- create_dir(here::here("output/validation"))

results_file <- function(...) here::here("output", ..., "results.xlsx")
read_sheet <- function(path, sheet) as.data.frame(read_excel(path, sheet))

# Named vector of results from the rows of a table
values_from <- function(keys, values) setNames(as.numeric(values), keys)

# ── Descriptive results ──────────────────────────────────────────────────────

descriptives_file <- results_file("descriptives")

table1 <- read_sheet(descriptives_file, "Table 1") |>
    mutate(
        # Results in cm, article in m
        Mean = ifelse(Variable == "Height", Mean / 100, Mean),
        SD = ifelse(Variable == "Height", SD / 100, SD),
        Variable = ifelse(Variable == "Height", "Height_m", Variable)
    )
table1_sex <- read_sheet(descriptives_file, "Table 1 Sex")

table2 <- read_sheet(descriptives_file, "Table 2") |>
    pivot_longer(c(Measured, Difference, Lower, Upper, p_value),
                 values_drop_na = TRUE)

sweat_loss <- read_sheet(descriptives_file, "Whole-Body Sweat Loss")
means_by_phase <- read_sheet(descriptives_file, "Means by Phase")
max_lsr <- read_sheet(descriptives_file, "Maximum LSR")

descriptive_values <- c(
    `table1|n` = sum(table1_sex$Freq),
    `table1|male_n` = table1_sex$Freq[table1_sex$Sex == "male"],
    `table1|male_pct` = table1_sex$Percent[table1_sex$Sex == "male"],
    values_from(paste0("table1|", table1$Variable, "|mean"), table1$Mean),
    values_from(paste0("table1|", table1$Variable, "|sd"), table1$SD),
    values_from(
        paste("table2", table2$Stressor, table2$Phase, table2$name, sep = "|"),
        table2$value
    ),
    values_from(paste0("text|sweat_loss|", sweat_loss$Stressor, "|mean"),
                sweat_loss$Mean_L_per_m2),
    values_from(paste0("text|sweat_loss|", sweat_loss$Stressor, "|sd"),
                sweat_loss$SD_L_per_m2),
    values_from(
        paste("text", means_by_phase$Variable, means_by_phase$Stressor,
              means_by_phase$Phase, "mean", sep = "|"),
        means_by_phase$Mean
    ),
    values_from(
        paste("text", means_by_phase$Variable, means_by_phase$Stressor,
              means_by_phase$Phase, "sd", sep = "|"),
        means_by_phase$SD
    ),
    `text|LSR|max` = max_lsr$Max_LSR
)

# ── Heart rate slope ─────────────────────────────────────────────────────────

slope_model <- read_sheet(results_file("hr_slope_vo2max"), "Slope Model")
slope_row <- slope_model[slope_model$Term == "v02max", ]
slope_values <- c(
    `text|hr_slope|estimate` = slope_row$Estimate,
    `text|hr_slope|p_value` = slope_row$`Pr(>|t|)`
)

# ── Participant flow ─────────────────────────────────────────────────────────

flow_file <- results_file("participant_flow")
participants <- read_sheet(flow_file, "Participants")
flow <- read_sheet(flow_file, "Flow by Sequence")
visits <- read_sheet(flow_file, "Visits Analysed")

flow_values <- c(
    `figure1|screened` = participants$Screened,
    `figure1|baseline` = participants$With_baseline_visit,
    `figure1|withdrew` = participants$Withdrew,
    values_from(paste0("figure1|", flow$Sequence, "|Allocated"),
                flow$Allocated),
    values_from(paste0("figure1|", flow$Sequence, "|Dropped"), flow$Dropped),
    values_from(paste0("figure1|", flow$Sequence, "|Completed"),
                flow$Completed),
    values_from(paste0("figure1|visits|", visits$Stressor), visits$Visits)
)

# ── Prediction ───────────────────────────────────────────────────────────────

primary_file <- results_file("so_prediction_rev/without_vo2max")
normal_file <- results_file("so_prediction_rev/with_vo2max_normal")
forced_file <- results_file("so_prediction_rev/with_vo2max_forced")

performance_values <- function(path, key_prefix) {
    read_sheet(path, "Performance Metrics") |>
        pivot_longer(
            -c(Class, Metric),
            names_to = c("statistic", "model"),
            names_sep = "_"
        ) |>
        with(values_from(
            paste(key_prefix, model, Class, Metric, statistic, sep = "|"),
            value
        ))
}

confusion_values <- function(path, sheet, model) {
    confusion <- read_sheet(path, sheet)
    values_from(
        paste("table3", model, confusion$observed, confusion$predicted,
              sep = "|"),
        confusion$Freq
    )
}

performance <- function(path, model, metric, statistic = "Estimate") {
    metrics <- read_sheet(path, "Performance Metrics")
    metrics[metrics$Class == "Overall" & metrics$Metric == metric,
            paste(statistic, model, sep = "_")]
}

predictions <- read_sheet(primary_file, "Predictions")
predictions_normal <- read_sheet(normal_file, "Predictions")
stopifnot(identical(predictions[c("pid", "visit")],
                    predictions_normal[c("pid", "visit")]))

n_correct <- function(data) sum(data$predicted_rf == data$observed)

nogueira <- read_sheet(primary_file, "Nogueira Stability Index")
jaccard <- read_sheet(primary_file, "Jaccard Overlap")
missingness <- read_sheet(primary_file, "Predictor Missingness")
rf_tuning <- read_sheet(primary_file, "RF Tuning Profile")
lasso_tuning <- read_sheet(primary_file, "LASSO Tuning Profile")
lasso_primary <- read_sheet(primary_file, "LASSO Variable Importance")
lasso_normal <- read_sheet(normal_file, "LASSO Variable Importance")

prediction_values <- c(
    performance_values(primary_file, "table4"),
    confusion_values(primary_file, "RF Confusion Matrix", "rf"),
    confusion_values(primary_file, "LASSO Confusion Matrix", "lasso"),
    values_from(paste0("table5|", nogueira$Class, "|RF_Stability"),
                nogueira$RF_Stability),
    values_from(paste0("table5|", nogueira$Class, "|LASSO_Stability"),
                nogueira$LASSO_Stability),
    values_from(paste0("table5|", jaccard$Class, "|Jaccard"),
                jaccard$Estimate),
    values_from(paste0("table5|", jaccard$Class, "|Jaccard_Lower"),
                jaccard$Lower),
    values_from(paste0("table5|", jaccard$Class, "|Jaccard_Upper"),
                jaccard$Upper),
    `text|missing|predictor_values` = missingness$Predictor_values,
    `text|missing|missing_values` = missingness$Missing_values,
    `text|missing|visits_with_missing` =
        missingness$Visits_with_missing_values,
    `text|predictors` = missingness$Predictors,
    `text|visits` = missingness$Visits,
    `text|mtry_min` = min(rf_tuning$mtry),
    `text|mtry_max` = max(rf_tuning$mtry),
    `text|lambda_n` = nrow(lasso_tuning),
    `text|lambda_min` = min(lasso_tuning$lambda),
    `text|lambda_max` = max(lasso_tuning$lambda),
    `text|primary|rf_lasso_classification_differences` =
        sum(predictions$predicted_rf != predictions$predicted_lasso),
    `text|temperature_selected_predictors` = sum(
        lasso_primary$class == "temperature" & lasso_primary$selected
    ),
    `text|vo2max_normal|lasso_classification_changes` =
        sum(predictions$predicted_lasso != predictions_normal$predicted_lasso),
    `text|vo2max_normal|lasso_nonzero_coefficients` = sum(
        lasso_normal$predictor == "vo2max" & lasso_normal$selected
    ),
    `text|vo2max_normal|rf|accuracy` =
        performance(normal_file, "rf", "Accuracy"),
    `text|vo2max_normal|rf|additional_correct` =
        n_correct(predictions_normal) - n_correct(predictions),
    `text|vo2max_forced|lasso|accuracy` =
        performance(forced_file, "lasso", "Accuracy"),
    `text|vo2max_forced|lasso|Multiclass Brier Score|Estimate` =
        performance(forced_file, "lasso", "Multiclass Brier Score"),
    `text|vo2max_forced|lasso|Multiclass Brier Score|Lower` =
        performance(forced_file, "lasso", "Multiclass Brier Score", "Lower"),
    `text|vo2max_forced|lasso|Multiclass Brier Score|Upper` =
        performance(forced_file, "lasso", "Multiclass Brier Score", "Upper"),
    `text|vo2max_forced|lasso|Multiclass Log Loss|Estimate` =
        performance(forced_file, "lasso", "Multiclass Log Loss"),
    `text|vo2max_forced|lasso|Multiclass Log Loss|Lower` =
        performance(forced_file, "lasso", "Multiclass Log Loss", "Lower"),
    `text|vo2max_forced|lasso|Multiclass Log Loss|Upper` =
        performance(forced_file, "lasso", "Multiclass Log Loss", "Upper")
)

# ── Comparison ───────────────────────────────────────────────────────────────

results <- c(descriptive_values, slope_values, flow_values, prediction_values)
stopifnot(!anyDuplicated(names(results)))

article_values <- read_csv(
    here::here("code/article_values.csv"),
    col_types = cols(key = "c", article_value = "d", digits = "i",
                     comparison = "c", known_discrepancy = "c")
)

comparison <- article_values |>
    mutate(
        result = unname(results[key]),
        result_rounded = round(result, digits),
        agrees = ifelse(
            comparison == "lt",
            result < article_value,
            abs(result_rounded - article_value) < 1e-9
        ),
        status = case_when(
            is.na(result) ~ "not regenerated",
            agrees & !is.na(known_discrepancy) ~
                "matches, but listed as a discrepancy",
            agrees ~ "matches",
            !is.na(known_discrepancy) ~ "known discrepancy",
            TRUE ~ "DIFFERS"
        )
    ) |>
    select(key, article_value, result, result_rounded, comparison, status,
           known_discrepancy)

write_csv(comparison, file.path(out_dir, "article_comparison.csv"), na = "")
write_session_info(out_dir)

cat("Comparison with the article:\n")
print(as.data.frame(count(comparison, status)), row.names = FALSE)

known <- filter(comparison, status == "known discrepancy")
if (nrow(known) > 0) {
    cat("\nKnown discrepancies:\n")
    print(as.data.frame(select(known, key, article_value, result_rounded)),
          row.names = FALSE)
}

unexplained <- filter(comparison, status != "matches",
                      status != "known discrepancy")
if (nrow(unexplained) > 0) {
    print(as.data.frame(select(unexplained, key, article_value, result_rounded,
                               status)), row.names = FALSE)
    stop(nrow(unexplained), " article values need attention", call. = FALSE)
}
