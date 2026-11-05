# Association between the heart rate slope during exertion and VO2max
# (Figure 3 and the corresponding results text).
#
# Adapted from so_slopes.R and the report heatSTAR_2022_webreport_v6 by the
# earlier statisticians.

suppressMessages(here::i_am("code/hr_slope_vo2max.R"))
source(here::here("code/setup.R"))
load_packages(c("dplyr", "ggplot2", "sessioninfo", "tibble", "writexl"))

out_dir <- create_dir(here::here("output/hr_slope_vo2max"))
load(prepared_data_file)

# Slope of heart rate over time, from minute 25 to minute 85 of each visit
slopes <- df_holt |>
    filter(heatstress_type == "exertion",
           t_min_holt >= 25, t_min_holt <= 85) |>
    group_by(pid) |>
    summarise(slope = coef(lm(hr_holt ~ t_min_holt))[["t_min_holt"]]) |>
    left_join(
        df_base |>
            transmute(pid, v02max = as.numeric(v02max)),
        by = "pid"
    )

stopifnot(nrow(slopes) == 19, !anyNA(slopes$v02max))

slope_model <- lm(slope ~ v02max, data = slopes)

slope_results <- cbind(
    coef(summary(slope_model)),
    confint(slope_model)
) |>
    as.data.frame() |>
    rownames_to_column("Term") |>
    mutate(N = nobs(slope_model))

figure3 <- ggplot(slopes, aes(x = v02max, y = slope)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
    ylab("Heart rate slope") +
    xlab(bquote(VO[2] ~ "max (ml/min/kg)")) +
    theme_bw()

ggsave(file.path(out_dir, "figure3.pdf"), figure3, width = 6, height = 6)
ggsave(file.path(out_dir, "figure3.png"), figure3, width = 6, height = 6,
       dpi = 200)

write_xlsx(path = file.path(out_dir, "results.xlsx"),
           list(`Slope Model` = slope_results))
write_session_info(out_dir)
cat("Heart rate slope results saved to", out_dir, "\n")
