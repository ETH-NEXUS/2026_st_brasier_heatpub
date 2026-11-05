# Participant flow (Figure 1) and the number of visits analysed per stressor.
#
# The published figure was redrawn by the authors. This script produces the
# counts behind it and a CONSORT-like diagram from the same data.
#
# Adapted from consort.R by the earlier statisticians.

suppressMessages(here::i_am("code/participant_flow.R"))
source(here::here("code/setup.R"))
load_packages(c("consort", "dplyr", "sessioninfo", "tidyr", "writexl"))

out_dir <- create_dir(here::here("output/participant_flow"))
load(prepared_data_file)

# The sequence is recorded at visit 1 only, and the eCRF rows are sorted by
# participant and visit. Filling downwards gives it to the later visits.
flow <- df_eCRF[, c("pid", "randseq", "completion_yn", "withdrawal_reason",
                    "dropout_reason", "dropout_other")]
flow$randseq <- ifelse(flow$randseq == "", NA, as.character(flow$randseq))
flow <- fill(flow, randseq)
flow <- flow[!is.na(flow$completion_yn), ]

# Each participant has one study completion record.
stopifnot(!anyDuplicated(flow$pid))

flow$dropout_other[grepl("cont", flow$dropout_other)] <- "Unwilling"
flow$exc <- NA
flow$exit <- trimws(gsub("NA", "", paste(flow$withdrawal_reason,
                                         flow$dropout_other)))
flow$exit[flow$exit == ""] <- NA

# The original diagram ended with a side box for the two visits missing because
# of the e-Celsius. The published figure does not show it, and consort 1.2.4
# does not allow a diagram to end with a side box, so it is left out here. The
# visits are counted in `visits_analysed` below.
diagram <- consort_plot(
    flow,
    orders = c(
        pid = "Screened",
        exc = "Excluded",
        randseq = "At least baseline visit",
        exit = "Dropped",
        completion_yn = "Participated"
    ),
    side_box = c("exc", "exit"),
    allocation = "randseq"
)

cairo_pdf(file.path(out_dir, "figure1_diagram.pdf"), width = 16, height = 9)
grid::grid.draw(build_grid(diagram))
dev.off()

# ── Counts ───────────────────────────────────────────────────────────────────

flow_counts <- flow |>
    group_by(Sequence = randseq) |>
    summarise(
        Allocated = n(),
        Dropped = sum(!is.na(exit)),
        Completed = sum(as.character(completion_yn) == "Yes")
    )

visits_analysed <- df_times |>
    filter(!is_excluded_visit(pid, visit)) |>
    count(Stressor = as.character(heatstress_type), name = "Visits")

participants <- tibble::tibble(
    Screened = length(unique(data_OG$pid)),
    With_baseline_visit = nrow(flow),
    Withdrew = sum(as.character(flow$completion_yn) != "Yes"),
    Analysed = nrow(df_base)
)

write_xlsx(path = file.path(out_dir, "results.xlsx"), list(
    Participants = participants,
    `Flow by Sequence` = flow_counts,
    `Visits Analysed` = visits_analysed
))
write_session_info(out_dir)
cat("Participant flow saved to", out_dir, "\n")
