# Reproducible statistical analysis of a heat stress digital biomarker study

This repository contains the code that reproduces the statistical results of
the following article, from the study data:

> Schüpfer L, Niederberger C, Matos Gonçalves V, Goldhahn J, Brasier N.
> *Multimodal machine learning for comprehensive health monitoring in heat
> stress through digital biomarkers.* Accepted for publication.

A single command, `make`, prepares the data, regenerates the results and
compares them with the values reported in the article.

## Contents

| Path                     | Contents                                                       |
| ------------------------ | -------------------------------------------------------------- |
| `code/`                  | Analysis scripts, run in order by the `Makefile`               |
| `code/article_values.csv`| Values reported in the article, compared with the results      |
| `code/raw_preprocessing/`| Archived processing of the raw exports (not run, see below)    |
| `data/`                  | Input data (not in the repository), described in its README    |
| `revision/`              | Record of the revision of the manuscript                       |
| `output/`                | Generated results (not in the repository)                      |
| `renv.lock`              | Versions of R and of the R packages used                       |

## Requirements

- R 4.6.2 and the R packages recorded in `renv.lock`, with the
  [renv](https://rstudio.github.io/renv/) package to install them.
- GNU Make and Bash.
- A C, C++ and Fortran compiler if packages are installed from source.
- [Quarto](https://quarto.org/), only for the revision report.

The article reports R 4.6.1, randomForest 4.7-1.2 and glmnet 5.0. The versions
in `renv.lock` (R 4.6.2, glmnet 5.2) gave results identical to those behind the
article. Other versions may not.

## Installation

```bash
git clone https://github.com/ETH-NEXUS/2026_st_brasier_heatpub.git
cd 2026_st_brasier_heatpub
```

To install the recorded package versions in a library specific to the project,
run these two commands from the project root. They must run in separate R
sessions: the project library is used from the second session onwards.

```bash
Rscript -e 'renv::activate()'
```

```bash
Rscript -e 'renv::restore(prompt = FALSE)'
```

`renv::activate()` creates `.Rprofile` and `renv/`, which git ignores. Every R
session started from the project root then uses the project library.

## Data

The data contain personal health information and are not public, even
pseudonymized. They are available on request from the corresponding author of
the article (see its data availability statement).

The pipeline reads the files listed in [`data/README.md`](data/README.md),
which also gives their checksums. There are two ways to provide them:

1. **On LeoMed**, the secure computing platform of ETH Zurich for sensitive
   research data
   ([documentation](https://unlimited.ethz.ch/pages/viewpage.action?pageId=398920057)),
   where the data are stored in
   `/cluster/work/nexus/2026_st_brasier_heatpub/data`. Run the pipeline there
   after linking the data into `data/`:

   ```bash
   data/make_symlinks
   ```

   The script can be run again: it refreshes the links, and stops without
   changing anything if `data/` already holds a copy of a file.

2. **Elsewhere**, without access to LeoMed, copy the data files into `data/`.

## Usage

From the project root:

```bash
make
```

The full run takes about five minutes on 22 cores. The bootstraps of the
prediction analysis use all available cores.

| Target                 | Action                                                        |
| ---------------------- | ------------------------------------------------------------- |
| `make` or `make all`   | Run everything and compare the results with the article       |
| `make prepare`         | Prepare the analysis data                                     |
| `make descriptives`    | Tables 1 and 2 and the descriptive results of the text        |
| `make figures`         | Figures 1 and 3 and the supplementary time-series plots       |
| `make prediction`      | Tables 3 to 5, Figure 4 and the vO₂max sensitivity analyses   |
| `make validate`        | Compare the results with the article                          |
| `make revision-report` | Render the revision report (not part of the reproduction)     |
| `make clean`           | Remove the cache of fitted models                             |
| `make purge`           | Also remove all generated results                             |

`output/` contains participant-level results, such as the held-out predictions,
so it must not be shared.

The prediction analysis caches its fitted models in `tmp/`. The cache is keyed
on the command-line arguments and the analysis data, not on the code: run `make
clean` after changing the code or its settings.

## Steps

1. `code/prepare_data.R` regenerates the eCRF tables from the REDCap export and
   stops unless they match the copies stored with the data. It also verifies
   the derived time and phase columns of the device files.
2. These scripts write their results to `output/`:
   - `code/descriptives.R`: Table 1, Table 2, whole-body sweat loss, and the
     means by stressor and phase quoted in the text.
   - `code/participant_flow.R`: the counts of Figure 1 and a CONSORT-like
     diagram. The published figure was redrawn by the authors.
   - `code/hr_slope_vo2max.R`: Figure 3 and the association between the heart
     rate slope and vO₂max.
   - `code/supplement_timeseries.R`: time-series plots per participant, which
     appear to correspond to the supplementary figures.
   - `code/so_prediction_rev.R`: Random Forest and multinomial LASSO
     classification of the heat stressor with leave-one-subject-out
     cross-validation (Tables 3 to 5 and Figure 4), for the primary analysis
     and the two vO₂max sensitivity analyses.
3. `code/validate_article.R` rounds each result once to the precision of the
   article, compares it with `code/article_values.csv`, writes
   `output/validation/article_comparison.csv`, and stops if a value differs
   without being a known discrepancy.

## Results of the comparison

A clean run matched 412 of the 413 values compared. The remaining value is a
known discrepancy in the article: the standard deviation of baseline heart rate
under exertion, given as 14.01 instead of 14.69 (the article repeats the value
of the temperature visits).

The supplementary tables are not reproduced, and the correspondence of the
time-series plots with the supplementary figures has not been verified.

## Limitations

The pipeline starts from the earliest data available, not from the raw device
and REDCap exports. As a result, these steps cannot be verified:

- the reading and cleaning of the device recordings, including manual edits to
  the raw files and the trimming of recordings to each visit;
- the summary of the home iButton recordings, whose code was not found;
- the 90% sinus-rhythm rule for heart rate variability described in the
  article, which no available code applies.

`code/raw_preprocessing/` documents the processing of the raw exports. It is
archived for reference and was not run.
