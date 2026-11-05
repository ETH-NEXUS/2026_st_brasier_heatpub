# Project context

The manuscript **"Toward comprehensive multimodal health monitoring in heat: A
digital biomarker exploration study"** has been accepted. The goal of this
repository is now to reproduce the statistical results presented in the
accepted article, from the study data, using the code in `code/`.

The user is the biostatistician responsible for this work. They wrote the
analyses of the revision (`code/so_prediction_rev.R`). The earlier analyses
were written by statisticians who have since left the institute, and which of
their scripts produced which published result is not documented. Establishing
this is part of the work. Assistance in this repository is limited to the
statistical aspects of the article and its code.

The repository was kept, rather than restarted, to preserve the development
history of `code/so_prediction_rev.R`. Do not rewrite git history. Make renames
with `git mv`, in commits that change nothing else, so that `git log --follow`
keeps working.

## Sources

| Path                   | Role                                            | In git |
| ---------------------- | ----------------------------------------------- | ------ |
| `article_rev_2/*.docx` | Accepted article: the reference for all targets | No     |
| `original_codebase/`   | Earlier scripts: raw material to migrate        | No     |
| `code/`                | The reproducible analysis code                  | Yes    |
| `output/`              | Generated results                               | No     |
| `revision/`            | Record of the completed revision                | Yes    |

- The article is read-only. Its Word file contains tracked changes: read it
  with all changes accepted (`pandoc --track-changes=accept`, the default). The
  supplementary material is not in the repository. Ask the user for it before
  checking a supplementary target.
- `original_codebase/` is read-only and will not be shared. Code in `code/`
  must never source, read from, or refer the reader to it.
- `revision/` is a historical record. Do not update it unless asked. It remains
  useful for the reasoning behind the revised analysis, and
  `revision/revised_manuscript_review.qmd` lists corrections requested from the
  authors.
- The study data are outside the repository and may be unavailable in a given
  session. If so, say which steps could not be run.

## Reproduction targets

Keep this table current as targets are migrated and verified. “Candidate” means
that the script appears to match by its content, not that it is known to have
produced the published result.

| Article result | Starting point |
| -------------- | -------------- |
| Prediction: Tables 3–5, Figure 3, performance text, vO₂max sensitivity analyses | `code/so_prediction_rev.R` (in place) |
| Missing predictor values (16 of 2,958) | Hard-coded in `revision/reviewer_comments.qmd`; not exported by the code |
| Figure 1a, participant flow | Candidate: `original_codebase/consort.R` |
| Table 2 and WBGT comparisons | Candidate: `original_codebase/desc_WBGT.R` |
| Figure 2 and the HR slope–VO₂max association | Candidate: `original_codebase/so_slopes.R` |
| Supplementary time-series plots (CBT, HR, rMSSD, SDNN, LSR, CloBT, CloBH) | Candidates: the plots in `original_codebase/dataprep_{eCel,holt,eCRF,ibutton}.R` |
| CBT, HR and LSR means by stressor and phase | Candidate: `original_codebase/po_descriptiveanalysis.R` |
| Table 1 | No script located; baseline variables are in `df_base` (`dataprep_eCRF.R`) |
| CloBT and CloBH at home | No script located; `dataprep_ibutton.R` discards the home recordings |
| Whole-body sweat loss by stressor | No script located |

The article attributes Table 1, Figure 1a and the whole-body sweat loss results
to Brasier et al., at least in part, so they may have been produced for that
publication. If their code cannot be found, report this to the user rather than
reconstructing an analysis and presenting it as the original.

The article states that the preprocessing workflow and code are available at a
link. The pipeline therefore has two stages:

1. **Preprocessing**: raw exports to derived `.RData` files, starting from
   `original_codebase/dataprep_eCRF.R` (which creates `df_times`, needed by the
   others), then `dataprep_{eCel,holt,ibutton,baspo}.R`.
2. **Analysis**: derived `.RData` files to the article's results.

Not in scope unless the user asks, because the article does not report them:
`dataprep_epy.R`, `eo_*.R`, `so_stroop_v3.R` and `so_therm.R`.
`original_codebase/so_prediction.R` is superseded by
`code/so_prediction_rev.R`, and the `origin/nested-cv` branch is not the
published analysis; use them only to understand provenance.

### Known gaps in the earlier code

- The setup script is missing: `Rproj_path`, `data_path`, `myfigs`, `mytabs`,
  `%&%` and `source2` are used but never defined, and nothing in
  `original_codebase/` sets `dd` and `measure` for `po_descriptiveanalysis.R`.
- `dataprep_eCRF.R` sources a REDCap/CTU script stored with the data export.
- Comments in `dataprep_eCel.R` record manual edits to raw files. Find out
  whether the available raw files include these edits, and document any manual
  step that the code cannot perform.
- Figure 2's published axis label differs from the label in `so_slopes.R`, so
  the published figure did not come directly from this version of the script.

Ask the user about missing inputs and undocumented decisions. Do not guess
them.

### Known discrepancies in the accepted article

Do not treat these as reproduction failures, and do not change the code to
match them:

- Table 4, RF macro specificity: article 0.983, results 0.982 (0.98246).
- Table 4, LASSO temperature F1 lower limit: article 0.849, results 0.848
  (0.84848).
- Table 1, height: an SD of 1.02 m is implausible. The user's review assumed a
  unit error (10.2 cm); this has not been checked against the data.

Add further discrepancies here once confirmed.

## Analysis variants

The prediction script has three variants, all reported in the article:

- `--vo2max none`, `output/so_prediction_rev/without_vo2max/`: the primary
  analysis.
- `--vo2max normal`, `output/so_prediction_rev/with_vo2max_normal/`: vO₂max
  penalized like the other predictors.
- `--vo2max forced`, `output/so_prediction_rev/with_vo2max_forced/`: vO₂max
  unpenalized in LASSO.

## Working principles

- **Reproduce what was done.** Migration must preserve the published analysis,
  even where a different method would now be preferred. Report any suspected
  error or methodological concern to the user instead of correcting it
  silently.
- **Never force agreement.** Do not hard-code results, search for seeds, or
  adjust settings merely to match a number. Any change made because of a
  mismatch needs a justification independent of that match.
- **Keep provenance honest.** Do not infer undocumented decisions by the
  earlier statisticians, or describe a script as the source of a result until
  the result has been reproduced with it.
- **Migrate selectively.** Bring only what a target needs into `code/`. Remove
  dead code, exploratory branches and personal paths, but keep each analytical
  step. Verify results before refactoring further. State the originating script
  in the commit message.
- **Make it runnable by someone else.** Use paths relative to the project root
  (`here`), make the data location configurable rather than tied to a named
  machine or user (`code/so_prediction_rev.R` still needs this), run scripts
  non-interactively, and define every dependency explicitly. The `Makefile`
  must grow into the single entry point; its `report` targets still render
  `revision/reviewer_comments.qmd`.
- **Protect the data.** The article states that the data, even pseudonymized,
  are not public because they contain personal health information. Never commit
  data or participant-level results. This includes the `Predictions` sheet of
  `results.xlsx`, and participant-level values in code, comments, commit
  messages or reports. Exact exclusions already in the analysis code, such as
  participant and visit identifiers, are the exception.
- **Keep reproducibility controls.** Preserve random seeds, analysis settings
  and the distinction between variants. Generate results only through the code,
  and never edit result files by hand.
- **Stay in scope.** Do not edit the article or expand into unrelated
  scientific or editorial work. Use precise, measured statistical language, and
  keep exploratory findings distinct from confirmatory evidence.

## Validation

A target is reproduced only when its result has been regenerated from the code
in `code/` and compared with the article. Report each target as one of:
matches, differs (with both values and the likely cause), or not run (with the
reason).

- Compare unrounded values, rounded once to the article's precision. Rounding
  already-rounded values caused errors during the revision.
- Treat the current `output/so_prediction_rev/` as the reference for the
  revised analysis (`session_info.txt` dated 2026-09-08). `make analysis`
  overwrites it and `make purge` deletes it, and it cannot be recovered from
  git. Copy it outside `output/` before regenerating.
- Run `make clean` before validating `code/so_prediction_rev.R`. Its cache in
  `tmp/` or `/dev/shm` is keyed only on the command-line arguments, so a cached
  run can ignore code and data changes.
- Compare environments before interpreting a difference. The article reports R
  4.6.1, randomForest 4.7-1.2 and glmnet 5.0. On 2026-11-05 the installed
  versions included R 4.6.2 and glmnet 5.2. Record `session_info.txt` for every
  analysis, and keep the versions used for the article restorable.
- After migrating a script, check that a clean run does not depend on objects
  left in the R session or on `original_codebase/`.

## Coding conventions

- Use four spaces for indentation; do not use tabs.
- Limit code lines to 79 characters.
