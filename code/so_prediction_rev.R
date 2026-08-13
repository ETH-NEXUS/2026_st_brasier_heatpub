# ── Packages ─────────────────────────────────────────────────────────────────

pkgs <- c(
    "boot", "caret", "dplyr", "forcats", "ggplot2", "glmnet", "here", "knitr",
    "purrr", "randomForest", "recipes", "stringr", "tibble", "tidyr",
    "tidytext", "writexl"
)

missing_pkgs <- setdiff(pkgs, rownames(installed.packages()))

if (length(missing_pkgs) > 0) {
    cat("The following packages are missing: ",
        paste(missing_pkgs, collapse = ", "), "\n")

    choice <- menu(
        c("Yes", "No"),
        title = "Do you want to install the missing packages?"
    )

    switch(choice,
        # 1 = "Yes"
        {
            install.packages(missing_pkgs)
            cat("Missing packages installed.\n")
        },
        # 2 = "No"
        {
            cat("Please install the missing packages manually and then run",
                "the script again.\n")
            invokeRestart("abort")
        }
    )

    if (choice == 0) {
        cat("No selection made. Exiting script.\n")
        invokeRestart("abort")
    }
}

.null <- lapply(pkgs, \(pkg) suppressPackageStartupMessages(
    require(pkg, character.only = TRUE)))

# ── Set project root directory using the `here` package ──────────────────────

suppressMessages(i_am("code/so_prediction_rev.R"))

# ── Data location ────────────────────────────────────────────────────────────

data_dir = list(
    mipr = file.path(
        "C:/Users/mprummer",
        "polybox - Michael Prummer (prummer@nexus.ethz.ch)@polybox.ethz.ch",
        "Projects/brasier_HEATSTAR_2022/data"
    ),
    jpas = file.path(
        "/run/user/1002/gvfs",
        "onedrive:host=ethz.ch,user=jpasquier/Dokumente",
        "NEXUS - Documents/08_groups/02_st/Projects/_old projects",
        "2023_st_goldhahn_heatstarext/data/2022"
    )
)

# ── Output directory ─────────────────────────────────────────────────────────

out_dir <- here("results/so_prediction_rev")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ── Load data ────────────────────────────────────────────────────────────────

load_data <- function(data_dir, envir = .GlobalEnv) {
    t0 <- Sys.time()
    cat("Loading data from:", data_dir, "\n")
    datasets <- c("data_eCRFbased", "ts_ecel", "ts_holt", "ts_ibutton")
    for (dataset in datasets) {
        file_path <- file.path(data_dir, paste0(dataset, ".RData"))
        load(file_path, envir = envir)
    }
    cat("Data loading completed in", round(Sys.time() - t0, 1), "seconds\n")
    cat("\n", paste(rep("─", 70), collapse = ""), "\n")
}

# If running on jpas Linux workstation, cache data in /dev/shm (RAM) for
# faster reloading (fetching from onedrive is slow)
is_jpas_desktop <- Sys.info()[["nodename"]] == "nexus-u02"

if (is_jpas_desktop) {
    data_cache_file <- "/dev/shm/2026_st_brasier_biomarker_data.rda"
    if (file.exists(data_cache_file)) {
        load(data_cache_file)
    } else {
        if (!dir.exists(data_dir$jpas)) {
            system("gio mount onedrive://jpasquier@ethz.ch/")
        }
        .env <- new.env()
        load_data(data_dir = data_dir$jpas, envir = .env)
        save(list = ls(envir = .env), envir = .env, file = data_cache_file)
        list2env(as.list(.env), envir = .GlobalEnv)
        rm(.env)
    }
    rm(data_cache_file)
} else {
    load_data(data_dir = data_dir$mipr)
}

# ── Functions ────────────────────────────────────────────────────────────────

# to subset to the last X minutes of each phase
getLastXmin <- function(data, tvar, min, cool) {
    b <- ((data[[tvar]] >= (30-min)) & (data[[tvar]] < 30)) |
         ((data[[tvar]] >= (60-min)) & (data[[tvar]] < 60)) |
         ((data[[tvar]] >= (90-min)) & (data[[tvar]] < 90))

    if (isTRUE(cool)) {
        b <- b | ((data[[tvar]] >= (120-min)) & (data[[tvar]] < 120))
    }

    return(data[b, ])
}

# to summarize the data per phase (mean, sd, min, max)
getSummaryStats <- function(data, voi) {
    data |>
        group_by(pid, visit, phase) |>
        summarise_at(
            vars(all_of(voi)),
            list(
                mean = function(x) mean(x, na.rm = TRUE),
                sd   = function(x) sd(x,   na.rm = TRUE),
                min  = function(x) min(x,  na.rm = TRUE),
                max  = function(x) max(x,  na.rm = TRUE)
            )
        )
}

# to make summary wide
getWideSumm <- function(data) {
    data |>
        pivot_wider(
            names_from  = phase,
            values_from = c(mean, sd, min, max)
        )
}

# to get differences in means and maxes
getDiffz <- function(data) {
    data |>
        mutate(
            diff_mean_accheat  = mean_Heat - mean_Acclim,
            diff_mean_accheatc = `mean_Heat+Cloth` - mean_Acclim,
            diff_max_accheat   = max_Heat - max_Acclim,
            diff_max_accheatc  = `max_Heat+Cloth` - max_Acclim
        )
}

# Combine the preceding functions in a function that
#   - subset
#   - get mean, sd, min, max
#   - make wide then get differences between heat/heat+ and acclim
#   - diff in mean between acclimation and heat phase
#   - add datastream to name
createPredictors <- function(data, tvar, min, cool, voi, prefix) {
    data |>
        getLastXmin(tvar = tvar, min = min, cool = cool) |>
        getSummaryStats(voi) |>
        getWideSumm() |>
        getDiffz() |>
        rename_with(~ sub("^", prefix, .x), !matches("pid|visit"))
}

# ── Creating predictors ──────────────────────────────────────────────────────

# data structure (data should be per id-visit)
data_pred <- df_times[, c("pid", "visit", "heatstress_type")]

# from sweat rate plasters
df_psweat_pred <- df_psweat |>
    rename(
        sr_Acclim = sweatrate_30,
        sr_Heat = sweatrate_60,
        `sr_Heat+Cloth` = sweatrate_90,
        sr_Cool = sweatrate_120
    ) |>
    select(pid, visit, sr_Acclim, sr_Heat, `sr_Heat+Cloth`) |>
    mutate(
        sr_diff_accheat = sr_Heat - sr_Acclim,
        sr_diff_accheatc = `sr_Heat+Cloth` - sr_Acclim
    )

# from eCelsius data, Holt monitor data and iButton data
summ_wide <- list(
    c(df = "df_ecel", tvar = "t_min_ecel", voi = "temp_core_ecel",
      prefix = "cbt_"),
    c(df = "df_holt", tvar = "t_min_holt", voi = "hr_holt", prefix = "hhr_"),
    c(df = "df_holt", tvar = "t_min_holt", voi = "rMSSD", prefix = "rmssd_"),
    c(df = "df_holt", tvar = "t_min_holt", voi = "SDNN", prefix = "sdnn_"),
    c(df = "df_ib", tvar = "t_min_ib", voi = "temp_ib", prefix = "ibtemp_"),
    c(df = "df_ib", tvar = "t_min_ib", voi = "hum_ib", prefix = "ibhum_")
) |>
    map(~ createPredictors(get(.x[["df"]]), tvar = .x[["tvar"]], min = 5,
                           cool = FALSE, voi = .x[["voi"]],
                           prefix = .x[["prefix"]]))

# combine the predictors into analysis data set
data_pred <- append(list(data_pred, df_psweat_pred), summ_wide) |>
    reduce(full_join, by=c("pid", "visit"))

# remove "excluded" visits
data_pred <- data_pred |>
    filter(!(pid == 5 & visit == 2), !(pid == 18 & visit == 4))

# ── Prepare data for 3-class analysis ────────────────────────────────────────

# select variables, rename heatstress type, set variable types
data_pred_3c <- data_pred |>
    select(!matches("_(Acclim|Heat\\+Cloth)$")) |>
    rename(class = heatstress_type) |>
    mutate(
        pid = factor(pid),
        class = factor(gsub(" +", ".", class)),
        across(where(is.numeric), as.numeric)
    )

# check missingness
#   -> limited, we'll use mean imputation
stopifnot(
    !anyNA(data_pred_3c$pid),
    !anyNA(data_pred_3c$class)
)
#Amelia::missmap(data_pred_3c)

# predictor names
predictor_names <- setdiff(
    names(data_pred_3c),
    c("pid", "visit", "class")
)

# class levels
class_levels <- levels(data_pred_3c$class)
class_indices <- setNames(seq_along(class_levels), class_levels)

# ── Leave-one-subject-out resampling ─────────────────────────────────────────

subjects <- levels(droplevels(data_pred_3c$pid))

# In caret:
#   - index    = rows used for model fitting
#   - indexOut = rows used for validation
loso_index <- setNames(
    map(subjects, ~ which(data_pred_3c$pid != .x)),
    paste0("Subject_", subjects)
)

loso_index_out <- setNames(
    map(subjects, ~ which(data_pred_3c$pid == .x)),
    paste0("Subject_", subjects)
)

# Recipe with fold-specific imputation (and normalization for LASSO)
rf_recipe <- recipe(
    class ~ .,
    data = data_pred_3c
) |>
    update_role(pid, visit, new_role = "id") |>
    step_impute_mean(all_numeric_predictors())

lasso_recipe <- recipe(
    class ~ .,
    data = data_pred_3c
) |>
    update_role(pid, visit, new_role = "id") |>
    step_impute_mean(all_numeric_predictors()) |>
    step_normalize(all_numeric_predictors())

# Train control for leave-one-subject-out cross-validation
loso_control <- trainControl(
    method = "cv",
    number = length(subjects),
    index = loso_index,
    indexOut = loso_index_out,
    search = "grid",
    savePredictions = "final",
    classProbs = TRUE,
    allowParallel = FALSE
)

# Explicit tuning grids (for RF and LASSO)
mtry_grid <- expand.grid(
    mtry = unique(pmax(
        1L,
        round(seq(
            from = 2,
            to = sqrt(length(predictor_names)) * 2
        ))
    ))
)

lambda_grid <- expand.grid(
    alpha = 1,
    lambda = 10^seq(-4, 1, length.out = 50)
)

# Train models. If running on jpas Linux workstation, cache model object in
# /dev/shm (RAM) for faster reruns (fitting the model takes a while).
train_rf <- function() {
    set.seed(72929)
    cat("Fitting random forest using leave-one-subject-out cross-validation\n")
    cat(paste0("mtry tuning grid: ", paste(mtry_grid$mtry, collapse = ", ")),
        "\n")
    cat("May take a while... Please be patient\n")
    t0 <- Sys.time()
    fit <- train(
        rf_recipe,
        data = data_pred_3c,
        method = "rf",
        metric = "Accuracy",
        trControl = loso_control,
        tuneGrid = mtry_grid,
        ntree = 1000,
        importance = TRUE,
        keep.forest = TRUE
    )
    cat("Model fitting completed in", round(Sys.time() - t0, 1), "seconds\n")
    cat("\n", paste(rep("─", 70), collapse = ""), "\n")
    return(fit)
}

train_lasso <- function() {
    set.seed(9047549)
    cat("Fitting multinomial LASSO logistic regression using",
        "leave-one-subject-out cross-validation\n")
    cat(paste0("lambda tuning grid: ",
               paste(sprintf("%.5f", lambda_grid$lambda), collapse = ", "),
                "\n"))
    cat("May take a while... Please be patient\n")
    t0 <- Sys.time()
    fit <- train(
        lasso_recipe,
        data = data_pred_3c,
        method = "glmnet",
        metric = "Accuracy",
        trControl = loso_control,
        tuneGrid = lambda_grid,
        family = "multinomial"
    )
    cat("Model fitting completed in", round(Sys.time() - t0, 1), "seconds\n")
    cat("\n", paste(rep("─", 70), collapse = ""), "\n")
    return(fit)
}

train_model <- function() {
    rf <- train_rf()
    lasso <- train_lasso()
    list(rf = rf, lasso = lasso)
}

model_cache_file <- "/dev/shm/2026_st_brasier_models.rds"
if (is_jpas_desktop) {
    if (file.exists(model_cache_file)) {
        model <- readRDS(model_cache_file)
    } else {
        model <- train_model()
        saveRDS(model, model_cache_file)
    }
} else {
    model <- train_model()
}

# Selected hyperparameters
cat("Selected mtry:", model$rf$bestTune$mtry, "\n")
cat("Selected lambda:", model$lasso$bestTune$lambda, "\n")
cat("\n", paste(rep("─", 70), collapse = ""), "\n")

# Note: mtry=2 is selected even if we try a larger grid (2,3,...,51)

# ── Results of the 3-class model ─────────────────────────────────────────────

# Extract predictions and arrange them in the original order
loso_predictions <- map(names(model), \(model_name) {
    model[[model_name]]$pred |>
        arrange(rowIndex) |>
        mutate(
            pid = data_pred_3c$pid[rowIndex],
            visit = data_pred_3c$visit[rowIndex]
        ) |>
        select(
            pid,
            visit,
            observed = obs,
            predicted = pred,
            all_of(class_levels)
        ) |>
        rename_with(~ paste0(.x, "_", model_name), -c(pid, visit, observed))
}) |>
    reduce(full_join, by = c("pid", "visit", "observed"))

loso_predictions_long <- loso_predictions |>
    pivot_longer(
        cols = -c(pid, visit, observed),
        names_to = c(".value", "model"),
        names_pattern = "(.*)_(rf|lasso)"
    )

# Both models must contribute exactly one prediction for every original row.
stopifnot(
    nrow(loso_predictions) == nrow(data_pred_3c),
    nrow(loso_predictions_long) == 2L * nrow(data_pred_3c),
    !anyDuplicated(loso_predictions[c("pid", "visit", "observed")]),
    all(table(loso_predictions_long$model) == nrow(data_pred_3c))
)

# Confusion matrices
confusion_matrix <- map(setNames(nm = names(model)), \(model_name) {
    loso_predictions_long |>
        filter(model == model_name) |>
        with(table(observed, predicted))
})

cat("Confusion matrix - Random Forest:")
kable(confusion_matrix$rf)
cat("\nConfusion matrix - Multinomial LASSO logistic regression:")
kable(confusion_matrix$lasso)
cat("\n", paste(rep("─", 70), collapse = ""), "\n")

# Row indices for each subject (for bootstrapping)
subject_ids <- unique(loso_predictions$pid)

prediction_rows_by_subject <- split(
    seq_len(nrow(loso_predictions)),
    loso_predictions$pid
)[as.character(subject_ids)]

# Matrices for computing performance metrics
response_matrix <- outer(loso_predictions$observed, class_levels, `==`)
colnames(response_matrix) <- class_levels
prediction_matrix <- map(setNames(nm = names(model)), ~ {
    predicted <- loso_predictions_long |>
        filter(model == .x) |>
        pull(predicted)
    m <- outer(predicted, class_levels, `==`)
    colnames(m) <- class_levels
    return(m)
})
probability_matrix <- map(setNames(nm = names(model)), ~ {
        loso_predictions_long |>
            filter(model == .x) |>
            select(all_of(class_levels)) |>
            as.matrix()
})

# Function to calculate the AUC using rank-based method (Mann–Whitney U
# statistic)
auc <- function(response, probability) {
    n1 <- sum(response)
    n0 <- sum(!response)
    if (n1 == 0L || n0 == 0L) return(NA_real_)
    r <- rank(probability, ties.method = "average")
    sum_r_pos <- sum(r[response])
    (sum_r_pos - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# Function to calculate performance metrics. This function will be used for
# bootstrapping and is much more efficient than using caret::confusionMatrix()
# repeatedly.
# Arguments:
#   rows: indices of selected rows from loso_predictions
# Output:
#   named vector of performance metrics
metrics_from_predictions <- function(rows) {
    do.call(base::c, map(names(model), \(model_name) {
        response <- response_matrix[rows, ]
        prediction <- prediction_matrix[[model_name]][rows, ]
        probability <- probability_matrix[[model_name]][rows, ]

        confusion <- t(prediction) %*% response

        total <- sum(confusion)
        true_positive <- diag(confusion)
        false_positive <- rowSums(confusion) - true_positive
        false_negative <- colSums(confusion) - true_positive
        true_negative <- total - true_positive - false_positive - false_negative

        # A zero denominator produces NaN, matching caret for undefined metrics.
        sensitivity <- true_positive / (true_positive + false_negative)
        specificity <- true_negative / (true_negative + false_positive)
        pos_pred_value <- true_positive / (true_positive + false_positive)
        neg_pred_value <- true_negative / (true_negative + false_negative)
        # caret defines F1 from precision and recall. In particular, F1 is
        # undefined (rather than zero) when both are zero.
        f1 <- 2 * pos_pred_value * sensitivity /
            (pos_pred_value + sensitivity)
        balanced_accuracy <- (sensitivity + specificity) / 2

        eps <- 1e-15
        clipped_probability <- pmin(pmax(probability, eps), 1 - eps)

        overall_brier <- mean(rowSums((response - probability)^2))
        overall_logloss <- -mean(log(clipped_probability[response]))

        auc <- sapply(class_indices, \(j) {
            auc(response[, j], probability[, j])
        })
        brier <- sapply(class_indices, \(j) {
            mean((response[, j] - probability[, j])^2)
        })
        logloss <- sapply(class_indices, \(j) {
            -mean(response[, j] * log(clipped_probability[, j]) +
                  (!response[, j]) * log(1 - clipped_probability[, j]))
        })

        by_class <- cbind(
            "Sensitivity" = sensitivity,
            "Specificity" = specificity,
            "Pos Pred Value" = pos_pred_value,
            "Neg Pred Value" = neg_pred_value,
            "F1" = f1,
            "Balanced Accuracy" = balanced_accuracy,
            "AUC" = auc,
            "Brier Score" = brier,
            "Log Loss" = logloss
        )

        macro <- colMeans(by_class, na.rm = TRUE)

        result <- c(
            sum(true_positive) / total,
            overall_brier,
            overall_logloss,
            as.vector(t(by_class)),
            macro
        )

        metric_names <- c(
            "Overall | Accuracy",
            "Overall | Multiclass Brier Score",
            "Overall | Multiclass Log Loss",
            as.vector(t(outer(
                paste("Class:", class_levels),
                colnames(by_class),
                paste,
                sep = " | "
            ))),
            paste("Macro Average", colnames(by_class), sep = " | ")
        )
        metric_names <- paste(model_name, metric_names, sep = " | ")

        set_names(result, metric_names)
    }))
}

# Bootstrap statistic: resampling subjects ID
set.seed(46742309)

B <- 10000

boot_all_metrics <- function(data, indices) {
    sampled_subjects <- as.character(data[indices])

    rows <- unlist(
        prediction_rows_by_subject[sampled_subjects],
        use.names = FALSE
    )

    metrics_from_predictions(rows)
}

run_bootstrap <- function() {
    cat("Bootstrapping performance metrics with", B, "replicates...\n")
    t0 <- Sys.time()
    boot_out <- boot(
        data = subject_ids,
        statistic = boot_all_metrics,
        R = B
    )
    cat("Bootstrapping completed in", round(Sys.time() - t0, 1), "seconds\n\n")
    return(boot_out)
}

metric_bs_cache_file <- "/dev/shm/2026_st_brasier_metric_bs.rds"
if (is_jpas_desktop) {
    if (file.exists(metric_bs_cache_file)) {
        boot_out <- readRDS(metric_bs_cache_file)
    } else {
        boot_out <- run_bootstrap()
        saveRDS(boot_out, metric_bs_cache_file)
    }
} else {
    boot_out <- run_bootstrap()
}

# Calculate percentile interval for each metric
metrics_table <- map_dfr(
    seq_along(boot_out$t0),
    function(i) {
        ci <- if (length(unique(boot_out$t[, i])) == 1L) {
            # If the bootstrap distribution is degenerate, return a pointwise
            # interval at the single value.
            list(perc = matrix(c(rep(NA, 3), rep(boot_out$t0[i], 2)),
                               nrow = 1))
        } else {
            boot.ci(
                boot_out,
                type = "perc",
                index = i
            )
        }

        metric_parts <- str_split_fixed(
            names(boot_out$t0)[i],
            fixed(" | "),
            n = 3
        )

        tibble(
            Model = metric_parts[1],
            Class = metric_parts[2],
            Metric = metric_parts[3],
            Estimate = unname(boot_out$t0[i]),
            Lower = ci$perc[1, 4],
            Upper = ci$perc[1, 5]
        )
    }
) |>
    pivot_wider(
        names_from = Model,
        values_from = c(Estimate, Lower, Upper),
        names_vary = "slowest"
    )

# Paired differences use the same held-out observations and the same bootstrap
# subject samples for both models. Positive differences favour RF for metrics
# where higher is better; negative differences favour RF for loss metrics.
rf_metric_indices <- grep("^rf \\| ", names(boot_out$t0))
metric_keys <- sub("^rf \\| ", "", names(boot_out$t0)[rf_metric_indices])
lasso_metric_indices <- match(
    paste("lasso", metric_keys, sep = " | "),
    names(boot_out$t0)
)

stopifnot(!anyNA(lasso_metric_indices))

model_difference_table <- map_dfr(seq_along(metric_keys), function(i) {
    bootstrap_difference <-
        boot_out$t[, rf_metric_indices[i]] -
        boot_out$t[, lasso_metric_indices[i]]
    interval <- quantile(
        bootstrap_difference,
        probs = c(0.025, 0.975),
        na.rm = TRUE,
        names = FALSE,
        type = 6   # corresponds to boot.ci(type = "perc")
    )
    metric_parts <- str_split_fixed(metric_keys[i], fixed(" | "), n = 2)

    tibble(
        Class = metric_parts[1],
        Metric = metric_parts[2],
        Difference_RF_minus_LASSO =
            unname(boot_out$t0[rf_metric_indices[i]]) -
            unname(boot_out$t0[lasso_metric_indices[i]]),
        Lower = interval[1],
        Upper = interval[2]
    )
})

cat("Performance metrics:")
kable(metrics_table, digits = 4)
cat("\nPaired performance differences (RF minus LASSO):")
kable(model_difference_table, digits = 4)
cat("\n", paste(rep("─", 70), collapse = ""), "\n")

# ── Variable importance ──────────────────────────────────────────────────────

# Extract class-specific importance (long format)
varimp_class <- varImp(
    model$rf,
    scale = TRUE
)$importance |>
    rownames_to_column("predictor") |>
    pivot_longer(
        cols = -predictor,
        names_to = "class",
        values_to = "importance"
    ) |>
    group_by(class) |>
    arrange(desc(importance), .by_group = TRUE) |>
    ungroup()

# Top variables per class
cat("Top 10 variables per class:")
varimp_class |>
    group_by(class) |>
    arrange(desc(importance), .by_group = TRUE) |>
    slice_head(n = 10) |>
    ungroup() |>
    kable()
cat("\n", paste(rep("─", 70), collapse = ""), "\n")

# Figure 3
figure3 <- varimp_class |>
    mutate(predictor_plot = reorder_within(predictor, importance, class)) |>
    ggplot(aes(x = importance, y = predictor_plot)) +
    geom_point() +
    scale_y_reordered() +
    facet_wrap(vars(class), scales = "free_y", nrow = 1) +
    labs(x = "Variable importance (scaled)", y = "Predictor") +
    theme_bw()
figure3

# ── Export results ───────────────────────────────────────────────────────────

write_xlsx(path = file.path(out_dir, "results.xlsx"), list(
    `Predictions` = loso_predictions,
    `Tuning Profile (RF)` = model$rf$results,
    `Tuning Profile (LASSO)` = model$lasso$results,
    `Confusion Matrix (RF)` = as.data.frame(confusion_matrix$rf),
    `Confusion Matrix (LASSO)` = as.data.frame(confusion_matrix$lasso),
    `Performance Metrics` = metrics_table,
    `Paired Model Differences` = model_difference_table,
    `Variable Importance` = varimp_class
))

ggsave(file.path(out_dir, "figure3.pdf"), figure3, width = 12, height = 8)
