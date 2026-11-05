# Settings and helpers shared by the analysis scripts. Each script first calls
# here::i_am() and then sources this file.

# Clock times in the data are local times. Fixing the time zone makes date-time
# conversions independent of the computer running the code.
Sys.setenv(TZ = "Europe/Zurich")

data_dir <- here::here("data")
prepared_data_file <- here::here("output/prepared_data/prepared_data.rda")

# Visits excluded from the analyses because their e-Celsius recordings were
# unusable. Participant 5, visit 2 is absent from the device files; participant
# 18, visit 4 is present and has to be removed where the article excludes it.
is_excluded_visit <- function(pid, visit) {
    (pid == 5 & visit == 2) | (pid == 18 & visit == 4)
}

load_packages <- function(pkgs) {
    missing_pkgs <- setdiff(pkgs, rownames(installed.packages()))
    if (length(missing_pkgs) > 0) {
        stop("Please install the missing packages: ",
             paste(missing_pkgs, collapse = ", "), call. = FALSE)
    }
    for (pkg in pkgs) {
        suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    }
}

create_dir <- function(path) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    path
}

write_session_info <- function(out_dir) {
    sink(file.path(out_dir, "session_info.txt"))
    on.exit(sink())
    print(sessioninfo::session_info())
}
