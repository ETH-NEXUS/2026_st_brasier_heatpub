# ── Packages ─────────────────────────────────────────────────────────────────

pkgs <- c(
    "boot", "caret", "dplyr", "forcats", "ggplot2", "here", "knitr", "purrr",
    "randomForest", "recipes", "stringr", "tibble", "tidyr", "tidytext",
    "writexl"
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

.null <- lapply(pkgs, require, character.only = TRUE)

# ── Set project root directory using the `here` package ──────────────────────

i_am("code/so_prediction_rev.R")

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
    datasets <- c("data_eCRFbased", "ts_ecel", "ts_holt", "ts_ibutton")
    for (dataset in datasets) {
        file_path <- file.path(data_dir, paste0(dataset, ".RData"))
        load(file_path, envir = envir)
    }
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
        class = factor(class),
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

# Recipe with fold-specific imputation
rf_recipe <- recipe(
    class ~ .,
    data = data_pred_3c
) |>
    update_role(pid, visit, new_role = "id") |>
    step_impute_mean(all_numeric_predictors())

# Train control for leave-one-subject-out cross-validation
loso_control <- trainControl(
    method = "cv",
    number = length(subjects),
    index = loso_index,
    indexOut = loso_index_out,
    search = "grid",
    savePredictions = "final",
    allowParallel = FALSE
)

# Explicit tuning grid
mtry_grid <- expand.grid(
    mtry = unique(pmax(
        1L,
        round(seq(
            from = 2,
            to = sqrt(length(predictor_names)) * 2
        ))
    ))
)

# Train model. If running on jpas Linux workstation, cache model object in
# /dev/shm (RAM) for faster reruns (fitting the model takes a while).
train_model <- function() {
    set.seed(72929)
    cat("Fitting model using leave-one-subject-out cross-validation\n")
    cat(paste0("mtry tuning grid: ", paste(mtry_grid$mtry, collapse = ", ")), "\n")
    cat("May take a while... Please be patient\n")
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
    cat("Model fitting completed\n")
    return(fit)
}

rf_model_cache_file <- "/dev/shm/2026_st_brasier_rf_model_loso.rds"
if (is_jpas_desktop) {
    if (file.exists(rf_model_cache_file)) {
        model_rf_loso <- readRDS(rf_model_cache_file)
    } else {
        model_rf_loso <- train_model()
        saveRDS(model_rf_loso, rf_model_cache_file)
    }
} else {
    model_rf_loso <- train_model()
}

# Selected mtry (hyperparameter that controls the number of variables to
# consider at each split)
cat("Selected mtry:", model_rf_loso$bestTune$mtry, "\n")

# Note: mtry=2 is selected even if we try a larger grid (2,3,...,51)

# ── Results of the 3-class model ─────────────────────────────────────────────

# Extract predictions and arrange them in the original order
loso_predictions <- model_rf_loso$pred |>
    arrange(rowIndex) |>
    mutate(
        pid = data_pred_3c$pid[rowIndex],
        visit = data_pred_3c$visit[rowIndex]
    ) |>
    select(
        pid,
        visit,
        rowIndex,
        Resample,
        observed = obs,
        predicted = pred,
        everything()
    )

# Store each subject's predictions once (we will resample subjects to calculate
# bootstrap confidence intervals for performance metrics)
predictions_by_subject <- split(
    loso_predictions,
    loso_predictions$pid
)

# Precompute the confusion counts for each subject. Rows of a confusion matrix
# are predicted classes and columns are observed classes, as in caret.
subject_ids <- unique(loso_predictions$pid)
class_levels <- levels(loso_predictions$observed)
n_classes <- length(class_levels)

subject_confusions <- predictions_by_subject[as.character(subject_ids)] |>
    map(function(x) {
        table(
            factor(x$predicted, levels = class_levels),
            factor(x$observed, levels = class_levels)
        ) |>
            as.vector()
    }) |>
    do.call(what = rbind)

# Confusion matrix
confusion_matrix <- matrix(
    colSums(subject_confusions),
    nrow = n_classes,
    ncol = n_classes,
    dimnames = list(
        predicted = class_levels,
        observed = class_levels
    )
)

cat("\n", paste(rep("─", 70), collapse = ""), "\n")
cat("Confusion matrix:")
kable(confusion_matrix)

# Function to calculate performance metrics directly from a confusion matrix.
# This function will be used for bootstrapping and is much more efficient than
# using caret::confusionMatrix() repeatedly.
metrics_from_confusion <- function(confusion) {
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

    by_class <- cbind(
        "Sensitivity" = sensitivity,
        "Specificity" = specificity,
        "Pos Pred Value" = pos_pred_value,
        "Neg Pred Value" = neg_pred_value,
        "F1" = f1,
        "Balanced Accuracy" = balanced_accuracy
    )

    macro <- colMeans(by_class, na.rm = TRUE)

    result <- c(
        sum(true_positive) / total,
        as.vector(t(by_class)),
        macro
    )

    metric_names <- c(
        "Overall | Accuracy",
        as.vector(t(outer(
            paste("Class:", class_levels),
            colnames(by_class),
            paste,
            sep = " | "
        ))),
        paste("Macro Average", colnames(by_class), sep = " | ")
    )

    set_names(result, metric_names)
}

# Bootstrap statistic: resampling subjects ID
set.seed(46742309)

B <- 10000

boot_all_metrics <- function(data, indices) {
    confusion <- matrix(
        colSums(subject_confusions[indices, , drop = FALSE]),
        nrow = n_classes,
        ncol = n_classes,
        dimnames = list(
            predicted = class_levels,
            observed = class_levels
        )
    )

    metrics_from_confusion(confusion)
}

boot_out <- boot(
    data = subject_ids,
    statistic = boot_all_metrics,
    R = B
)

# Calculate percentile interval for each metric
bootstrap_metrics <- map_dfr(
    seq_along(boot_out$t0),
    function(i) {
        ci <- if (length(unique(boot_out$t[, i])) == 1L) {
            # If the bootstrap distribution is degenerate, return a pointwise
            # interval at the single value.
            list(perc = matrix(c(rep(NA, 3), rep(boot_out$t0[i], 2)), nrow = 1))
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
            n = 2
        )

        tibble(
            Class = metric_parts[1],
            Metric = metric_parts[2],
            Estimate = unname(boot_out$t0[i]),
            Lower = ci$perc[1, 4],
            Upper = ci$perc[1, 5]
        )
    }
)
cat("\n", paste(rep("─", 70), collapse = ""), "\n")
cat("Performance metrics:")
kable(bootstrap_metrics, digits = 4)

# ── Variable importance ──────────────────────────────────────────────────────

# Extract class-specific importance (long format)
varimp_class <- varImp(
    model_rf_loso,
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
cat("\n", paste(rep("─", 70), collapse = ""), "\n")
cat("Top 10 variables per class:")
varimp_class |>
    group_by(class) |>
    arrange(desc(importance), .by_group = TRUE) |>
    slice_head(n = 10) |>
    ungroup() |>
    kable()

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
    `Confusion Matrix` = as.data.frame(confusion_matrix),
    `Overall Accuracy` = as.data.frame(t(overall_accuracy)),
    `Performance Metrics` = metrics_table,
    `Variable Importance` = varimp_class
))

ggsave(file.path(out_dir, "figure3.pdf"), figure3, width = 12, height = 8)
