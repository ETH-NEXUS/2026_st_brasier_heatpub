# ── Packages ─────────────────────────────────────────────────────────────────

pkgs <- c(
    "boot", "caret", "dplyr", "forcats", "ggplot2", "glmnet", "here", "knitr",
    "parallel", "purrr", "randomForest", "recipes", "stabm", "stringr",
    "tibble", "tidyr", "tidytext", "writexl"
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

# ── Parallel bootstraping ────────────────────────────────────────────────────

# Set RNG type to L'Ecuyer-CMRG for reproducibility in parallel processing
RNGkind("L'Ecuyer-CMRG")

# Parallel bootstrap function
parallel_boot <- function(data, statistic, R, seed, n_cores) {

    stopifnot(RNGkind()[1] == "L'Ecuyer-CMRG")

    set.seed(seed)

    # Save the RNG state at the start, as boot() does
    boot_seed <- .Random.seed

    n <- NROW(data)

    # Observed statistic
    t0 <- statistic(data, seq_len(n))

    # Generate bootstrap samples deterministically in the master process
    bootstrap_indices <- replicate(
        R,
        sample.int(
            n = n,
            size = n,
            replace = TRUE
        ),
        simplify = FALSE
    )

    # Generate one independent RNG stream per bootstrap replicate
    rng_streams <- vector("list", R)

    rng_streams[[1]] <- .Random.seed

    if (R > 1) {
        for (r in 2:R) {
            rng_streams[[r]] <- nextRNGStream(rng_streams[[r - 1]])
        }
    }

    boot_function <- function(r) {

        # Important when mc.cores = 1:
        # avoid leaving the caller's RNG state changed
        old_seed <- .Random.seed
        on.exit(
            assign(".Random.seed", old_seed, envir = .GlobalEnv),
            add = TRUE
        )

        assign(
            ".Random.seed",
            rng_streams[[r]],
            envir = .GlobalEnv
        )

        statistic(
            data,
            bootstrap_indices[[r]]
        )
    }

    results <- mclapply(
        X = seq_len(R),
        FUN = boot_function,
        mc.cores = n_cores,
        mc.set.seed = FALSE   # we manage the RNG ourselves
    )

    t <- do.call(rbind, results)

    out <- list(
        t0 = t0,
        t = t,
        R = R,
        data = data,
        seed = boot_seed,
        statistic = statistic,
        sim = "ordinary",
        stype = "i",
        call = match.call(),
        strata = rep(1L, n),
        weights = rep(1 / n, n),

        # Extra information
        bootstrap_indices = bootstrap_indices
    )

    class(out) <- "boot"

    out
}

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

out_dir <- here("output/so_prediction_rev")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ── Load data ────────────────────────────────────────────────────────────────

load_data <- function(data_dir, envir = .GlobalEnv) {
    cat("Loading data from:", data_dir, "\n")
    datasets <- c("data_eCRFbased", "ts_ecel", "ts_holt", "ts_ibutton")
    t0 <- Sys.time()
    for (dataset in datasets) {
        file_path <- file.path(data_dir, paste0(dataset, ".RData"))
        load(file_path, envir = envir)
    }
    elapsed <- difftime(Sys.time(), t0, units = "secs")
    cat("Data loading completed in", round(elapsed, 1), "seconds\n")
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
    elapsed <- difftime(Sys.time(), t0, units = "secs")
    cat("Model fitting completed in", round(elapsed, 1), "seconds\n")
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
    elapsed <- difftime(Sys.time(), t0, units = "secs")
    cat("Model fitting completed in", round(elapsed, 1), "seconds\n")
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
boot_all_metrics <- function(data, indices) {
    sampled_subjects <- as.character(data[indices])

    rows <- unlist(
        prediction_rows_by_subject[sampled_subjects],
        use.names = FALSE
    )

    metrics_from_predictions(rows)
}

run_metric_bootstrap <- function(
    B = 10000,
    seed = 46742309,
    n_cores = detectCores()
) {
    cat("Bootstrapping performance metrics with", B, "replicates...\n")
    cat("May take a while... Please be patient\n")
    t0 <- Sys.time()
    boot_out <- parallel_boot(
        data = subject_ids,
        statistic = boot_all_metrics,
        R = B,
        seed = seed,
        n_cores = n_cores
    )
    elapsed <- difftime(Sys.time(), t0, units = "secs")
    cat("Bootstrapping completed in", round(elapsed, 1), "seconds using",
        n_cores, "cores\n\n")
    return(boot_out)
}

metric_bs_cache_file <- "/dev/shm/2026_st_brasier_metric_bs.rds"
if (is_jpas_desktop) {
    if (file.exists(metric_bs_cache_file)) {
        boot_metric <- readRDS(metric_bs_cache_file)
    } else {
        boot_metric <- run_metric_bootstrap()
        saveRDS(boot_metric, metric_bs_cache_file)
    }
} else {
    boot_metric <- run_metric_bootstrap()
}

# Calculate percentile interval for each metric
metrics_table <- map_dfr(
    seq_along(boot_metric$t0),
    function(i) {
        ci <- if (length(unique(boot_metric$t[, i])) == 1L) {
            # If the bootstrap distribution is degenerate, return a pointwise
            # interval at the single value.
            list(perc = matrix(c(rep(NA, 3), rep(boot_metric$t0[i], 2)),
                               nrow = 1))
        } else {
            boot.ci(
                boot_metric,
                type = "perc",
                index = i
            )
        }

        metric_parts <- str_split_fixed(
            names(boot_metric$t0)[i],
            fixed(" | "),
            n = 3
        )

        tibble(
            Model = metric_parts[1],
            Class = metric_parts[2],
            Metric = metric_parts[3],
            Estimate = unname(boot_metric$t0[i]),
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
rf_metric_indices <- grep("^rf \\| ", names(boot_metric$t0))
metric_keys <- sub("^rf \\| ", "", names(boot_metric$t0)[rf_metric_indices])
lasso_metric_indices <- match(
    paste("lasso", metric_keys, sep = " | "),
    names(boot_metric$t0)
)

stopifnot(!anyNA(lasso_metric_indices))

model_difference_table <- map_dfr(seq_along(metric_keys), function(i) {
    bootstrap_difference <-
        boot_metric$t[, rf_metric_indices[i]] -
        boot_metric$t[, lasso_metric_indices[i]]
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
            unname(boot_metric$t0[rf_metric_indices[i]]) -
            unname(boot_metric$t0[lasso_metric_indices[i]]),
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

# Extract class-specific importance from the RF model (long format)
rf_importance <- model$rf$finalModel |>
    importance(scale = FALSE) |>
    as.data.frame() |>
    rownames_to_column("predictor") |>
    select(predictor, all_of(class_levels)) |>
    pivot_longer(
        cols = -predictor,
        names_to = "class",
        values_to = "importance"
    ) |>
    arrange(class, desc(importance))

# Top variables per class
cat("Top 5 variables per class:")
rf_importance |>
    group_by(class) |>
    arrange(desc(importance), .by_group = TRUE) |>
    slice_head(n = 5) |>
    ungroup() |>
    kable()
cat("\n", paste(rep("─", 70), collapse = ""), "\n")

# Figure 3
fig3_cls_lvls <- c(
    Temperature = "temperature",
    Humidity = "relative.humidity",
    Exertion = "exertion"
)
figure3 <- rf_importance |>
    mutate(
        class = factor(class, fig3_cls_lvls, names(fig3_cls_lvls)),
        predictor_plot = reorder_within(predictor, importance, class)
    ) |>
    ggplot(aes(x = importance, y = predictor_plot)) +
    geom_point() +
    scale_y_reordered() +
    facet_wrap(vars(class), scales = "free_y", nrow = 1) +
    labs(x = "Variable importance (unscaled)", y = "Predictor") +
    theme_bw()
figure3

# Coefficients of the LASSO model
lasso_importance <- coef(
    model$lasso$finalModel,
    s = model$lasso$bestTune$lambda
) |>
    imap_dfr(\(coef_matrix, class_name) {
        tibble(
            class = class_name,
            predictor = rownames(coef_matrix),
            coefficient = as.numeric(coef_matrix)
        )
    }) |>
    filter(predictor != "(Intercept)") |>
    mutate(
        importance = abs(coefficient),
        selected = coefficient != 0
    ) |>
    arrange(class, desc(importance))

cat("LASSO variable importance:")
lasso_importance |>
    filter(selected) |>
    select(!selected) |>
    kable(digits = 4)
cat("\n", paste(rep("─", 70), collapse = ""), "\n")

# ── Selection stability ──────────────────────────────────────────────────────

# k per class = size of LASSO's original selected set for that class
lambda_fixed <- model$lasso$bestTune$lambda
orig_coefs <- coef(model$lasso$finalModel, s = lambda_fixed)
k_by_class <- sapply(class_levels, function(cl) {
    cf <- as.matrix(orig_coefs[[cl]])[predictor_names, 1]
    sum(cf != 0)
})

# Bootstrap the variable selection
run_variable_selection_bootstrap <- function(
    B = 2000,
    seed = 135790,
    n_cores = detectCores()
){
    mtry_fixed   <- model$rf$bestTune$mtry
    ntree_fixed  <- 1000

    rows_by_subject <- split(seq_len(nrow(data_pred_3c)), data_pred_3c$pid)

    boot_variable_selection <- function(data, indices) {
        sampled_subjects <- as.character(data[indices])
        rows <- unlist(rows_by_subject[sampled_subjects], use.names = FALSE)
        boot_data <- data_pred_3c[rows, ]

        # RF
        rf_baked <- bake(prep(rf_recipe, training = boot_data),
                         new_data = NULL)
        rf_fit <- randomForest(
            x = rf_baked[, predictor_names],
            y = rf_baked$class,
            mtry = mtry_fixed,
            ntree = ntree_fixed,
            importance = TRUE
        )
        rf_importance <- importance(rf_fit, scale = FALSE)[, class_levels]
        rf_sel <- sapply(class_levels, \(cl) {
            rank(-rf_importance[, cl]) <= k_by_class[cl]
        })

        # LASSO
        lasso_baked <- bake(prep(lasso_recipe, training = boot_data),
                            new_data = NULL)
        x_mat <- as.matrix(lasso_baked[, predictor_names])
        glmnet_fit <- glmnet(x_mat, lasso_baked$class, family = "multinomial",
                              alpha = 1, lambda = lambda_fixed)
        coefs <- coef(glmnet_fit, s = lambda_fixed)
        lasso_sel <- sapply(class_levels, function(cl) {
            as.matrix(coefs[[cl]])[predictor_names, 1] != 0
        })

        c(rf = as.vector(rf_sel), lasso = as.vector(lasso_sel))
    }

    cat("Bootstrapping variable selection with", B, "replicates...\n")
    cat("May take a while... (Up to 5 minutes on a single core computer)",
        "Please be patient\n")
    t0 <- Sys.time()
    boot_varsel <- parallel_boot(
        data = subject_ids,
        statistic = boot_variable_selection,
        R = B,
        seed = seed,
        n_cores = n_cores
    )
    elapsed <- Sys.time() - t0
    cat("Bootstrapping completed in", round(elapsed, 1),
        attr(elapsed, "units"), "using", n_cores, "cores\n\n")

    return(boot_varsel)
}

selection_bs_cache_file <- "/dev/shm/2026_st_brasier_selection_bs.rds"
if (is_jpas_desktop) {
    if (file.exists(selection_bs_cache_file)) {
        boot_varsel <- readRDS(selection_bs_cache_file)
    } else {
        boot_varsel <- run_variable_selection_bootstrap()
        saveRDS(boot_varsel, selection_bs_cache_file)
    }
} else {
    boot_varsel <- run_variable_selection_bootstrap()
}

# Reshape boot_varsel$t into predictor × class × replicate
selection_array <- local({
    R     <- nrow(boot_varsel$t)       # number of bootstrap replicates
    p     <- length(predictor_names)   # number of predictor variables
    ncl   <- length(class_levels)      # number of class levels
    block <- p * ncl                   # number of columns in boot_varsel$t
                                       # for each model
    rf_block    <- boot_varsel$t[, 1:block, drop = FALSE]
    lasso_block <- boot_varsel$t[, (block + 1):(2 * block), drop = FALSE]

    # each row is a column-major-flattened (p x ncl) matrix
    # -> rebuild as p x ncl x R
    to_array <- function(block_mat) {
        array(
            t(block_mat),
            dim = c(p, ncl, R),
            dimnames = list(predictor_names, class_levels, NULL)
        )
    }

    list(rf = to_array(rf_block), lasso = to_array(lasso_block))
})

# Selection frequencies + paired differences (with CI)
freq <- map(selection_array, \(ary) apply(ary, c(1, 2), mean))

diff_array <- with(selection_array, rf - lasso)
diff_mean <- apply(diff_array, c(1, 2), mean)
diff_ci <- apply(diff_array, c(1, 2), quantile, c(0.025, 0.975))

selection_comparison_table <- expand.grid(
        Predictor = predictor_names,
        Class     = class_levels,
        stringsAsFactors = FALSE
    ) |>
    as_tibble() |>
    mutate(
        RF_Frequency    = as.vector(freq$rf),
        LASSO_Frequency = as.vector(freq$lasso),
        Difference_RF_minus_LASSO = as.vector(diff_mean),
        Diff_Lower = as.vector(diff_ci[1, , ]),
        Diff_Upper = as.vector(diff_ci[2, , ])
    ) |>
    arrange(Class, desc(RF_Frequency))

cat("Selection frequency and paired RF-LASSO difference:")
kable(selection_comparison_table, digits = 3)
cat("\n", paste(rep("─", 70), collapse = ""), "\n")


# Note: Diff_Lower/Diff_Upper excluding 0 flags a predictor where one model
# reliably selects it more often than the other — real disagreement rather than
# resampling noise.

# Jaccard overlap: bootstrap distribution + point estimate
jaccard <- function(a, b) {
    inter <- sum(a & b)
    uni   <- sum(a | b)
    if (uni == 0) return(NA_real_)
    inter / uni
}

# bootstrap distribution, per class (matrix R x ncl)
jaccard_boot <- sapply(class_levels, function(cl) {
    R <- dim(selection_array$rf)[3]
    sapply(seq_len(R), \(r) {
        with(selection_array, jaccard(rf[, cl, r] == 1, lasso[, cl, r] == 1))
    })
})

# point estimate from the original (non-bootstrapped) fit
jaccard_point <- local({
    rf_imp <- importance(model$rf$finalModel, scale = FALSE)[, class_levels]
    rf_sel <- sapply(class_levels, \(cl) {
        rank(-rf_imp[, cl]) <= k_by_class[cl]
    })

    lasso_sel <- sapply(class_levels, function(cl) {
        as.matrix(orig_coefs[[cl]])[predictor_names, 1] != 0
    })

    sapply(class_levels, \(cl) jaccard(rf_sel[, cl], lasso_sel[, cl]))
})

jaccard_table <- tibble(
    Class    = class_levels,
    Estimate = jaccard_point,
    Lower    = apply(jaccard_boot, 2, quantile, 0.025),
    Upper    = apply(jaccard_boot, 2, quantile, 0.975)
)

cat("Jaccard overlap between RF and LASSO selected predictors, per class:")
kable(jaccard_table, digits = 3)
cat("\n", paste(rep("─", 70), collapse = ""), "\n")


# Nogueira stability index per model per class
sel_matrix_to_list <- function(sel_matrix) {
    # sel_matrix is a p x r matrix `selection_array[[model]][, class, ]` where
    # p is the number of predictors and r is the number of bootstrap
    # replicates; the output is a list of length r, where each element is a
    # vector of selected predictors for model/class
    map(seq_len(ncol(sel_matrix)), \(r) {
        rownames(sel_matrix)[sel_matrix[, r] == 1]
    })
}

nogueira_table <- map_dfr(class_levels, function(cl) {
    p = length(predictor_names)
    tibble(
        Class = cl,
        RF_Stability = stabilityNogueira(
            features = sel_matrix_to_list(selection_array$rf[, cl, ]),
            p = p
        ),
        LASSO_Stability = stabilityNogueira(
            features = sel_matrix_to_list(selection_array$lasso[, cl, ]),
            p = p
        )
    )
})

cat("Nogueira stability index (intrinsic, per model per class):")
kable(nogueira_table, digits = 3)
cat("\n", paste(rep("─", 70), collapse = ""), "\n")

# ── Export results ───────────────────────────────────────────────────────────

write_xlsx(path = file.path(out_dir, "results.xlsx"), list(
    `Predictions` = loso_predictions,
    `RF Tuning Profile` = model$rf$results,
    `LASSO Tuning Profile` = model$lasso$results,
    `RF Confusion Matrix` = as.data.frame(confusion_matrix$rf),
    `LASSO Confusion Matrix` = as.data.frame(confusion_matrix$lasso),
    `Performance Metrics` = metrics_table,
    `Paired Model Differences` = model_difference_table,
    `RF Variable Importance` = rf_importance,
    `LASSO Variable Importance` = lasso_importance,
    `Selection Frequencies` = selection_comparison_table,
    `Jaccard Overlap` = jaccard_table,
    `Nogueira Stability Index` = nogueira_table
))

ggsave(file.path(out_dir, "figure3.pdf"), figure3, width = 12, height = 8)
ggsave(file.path(out_dir, "figure3.png"), figure3, width = 12, height = 8,
       dpi = 200)
