# ╭──────────────────────────────────────────────────────────╮
# │ ⚠ vO2max is spelled v02max (with a zero) in the datasets │
# ╰──────────────────────────────────────────────────────────╯

library(dplyr)
library(purrr)
library(tidyr)

data <- new.env()
here::here("data") |>
    list.files(pattern = ".RData", full.names = TRUE) |>
    map(~ load(.x, envir = data))
vars <- map_dfr(ls(envir = data), ~ {
    tibble(dataset = .x, variable = names(data[[.x]]))
})

filter(vars, grepl("v?2max", tolower(variable)))

filter(vars, grepl("v?2max", tolower(variable))) |>
    pull(dataset) |>
    map_dfr( ~ {
        data[[.x]] |>
            select(pid, v02max) |>
            drop_na() |>
            mutate(dataset = .x)
    }) |>
    pivot_wider(names_from = dataset, values_from = v02max) |>
    select(-pid) |>
    mutate(match = if_all(everything(), ~ .x == pick(1)[[1]])) |>
    print(n = Inf)
