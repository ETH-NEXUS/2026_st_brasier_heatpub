# Time-series plots per participant and stressor, which appear to correspond to
# Supplementary Figures 1.1, 1.4 to 1.7 and 1.9. The supplement itself is not in
# this repository, so this correspondence has not been verified.
#
# Adapted from the plotting sections of dataprep_eCel.R, dataprep_holt.R,
# dataprep_ibutton.R and dataprep_eCRF.R by the earlier statisticians.

suppressMessages(here::i_am("code/supplement_timeseries.R"))
source(here::here("code/setup.R"))
load_packages(c("dplyr", "ggplot2", "purrr", "sessioninfo"))

out_dir <- create_dir(here::here("output/supplement_timeseries"))
load(prepared_data_file)

plot_series <- function(data, x, y, y_label, y_limits, x_limits, label_y,
                        x_breaks = c(0, 30, 60, 90, 120)) {
    phase_label <- function(x_position, label, colour) {
        annotate("text", x = x_position, y = label_y, label = label,
                 color = colour, size = 2)
    }
    phase_line <- function(x_position, colour) {
        geom_vline(xintercept = x_position, linetype = "dashed",
                   color = colour, linewidth = 0.2)
    }
    data[!is_excluded_visit(data$pid, data$visit), ] |>
        ggplot() +
        geom_line(aes(x = .data[[x]], y = .data[[y]],
                      color = heatstress_type)) +
        facet_wrap(~pid, scales = "fixed") +
        theme_bw() +
        theme(axis.ticks.x = element_blank()) +
        coord_cartesian(ylim = y_limits, xlim = x_limits) +
        scale_x_continuous(breaks = x_breaks) +
        geom_vline(xintercept = 0, color = "grey") +
        phase_label(15, "Acclim", "grey") +
        phase_line(30, "red") +
        phase_label(45, "Stress", "red") +
        phase_line(60, "darkred") +
        phase_label(75, "+Clothing", "darkred") +
        phase_line(90, "blue") +
        phase_label(105, "Cool", "blue") +
        geom_vline(xintercept = 120, color = "grey") +
        labs(x = "Time (minutes)", y = y_label, color = "Heat stress")
}

plots <- list(
    core_body_temperature = plot_series(
        df_ecel, "t_min_ecel", "temp_core_ecel", "Core Body Temperature (°C)",
        c(36, 39), c(-15, 135), 38.75
    ),
    heart_rate = plot_series(
        df_holt, "t_min_holt", "hr_holt", "Heart rate (bpm)",
        c(40, 160), c(-10, 130), 158
    ),
    rmssd = plot_series(
        df_holt, "t_min_holt", "rMSSD", "rMSSD (ms)",
        c(0, 200), c(-10, 130), 180
    ),
    sdnn = plot_series(
        df_holt, "t_min_holt", "SDNN", "SDNN (ms)",
        c(0, 200), c(-10, 128), 195
    ),
    local_sweat_rate = plot_series(
        df_psweat_long, "min", "sweatrate",
        bquote("Local sweat rate" ~ (g / h * m^2)),
        c(0, 1500), NULL, 1400, x_breaks = c(30, 60, 90, 120)
    ),
    close_body_temperature = plot_series(
        df_ib, "t_min_ib", "temp_ib", "Close Body Temperature (°C)",
        c(26, 42), c(-8, 128), 26.25
    ),
    close_body_humidity = plot_series(
        df_ib, "t_min_ib", "hum_ib", "Close Body Relative Humidity (%)",
        c(20, 100), c(-8, 128), 22
    )
)

iwalk(plots, \(plot, name) {
    ggsave(file.path(out_dir, paste0(name, ".pdf")), plot, width = 14,
           height = 8, units = "in")
})

write_session_info(out_dir)
cat("Time-series plots saved to", out_dir, "\n")
