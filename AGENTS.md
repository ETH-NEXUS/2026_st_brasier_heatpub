# Project context

This project supports the second revision round of the manuscript **“Toward
comprehensive multimodal health monitoring in heat: A digital biomarker
exploration study.”**

The statistician who performed the initial analysis has left the institute. The
user is the biostatistician responsible for addressing the reviewers’
statistical requests. Their remit, and the scope of assistance in this
repository, is limited to the statistical aspects of the revision.

## Key files

- `revision/reviewer_comments.qmd`: the statistical reviewer comments, the
  user's current draft responses, results from the revised analyses, and
  suggested modifications to the manuscript. Read the relevant comment and
  current response before making changes.
- `code/so_prediction_rev.R`: the new analyses supporting the revision
  responses.
- `output/so_prediction_rev/`: generated results of those revised analyses,
  used and reported in `revision/reviewer_comments.qmd`.

The current analysis variants are:

- `without_vo2max/`: the primary analysis, corresponding to `--vo2max none`.
- `with_vo2max_normal/`: vO2max included and penalized like the other
  predictors, corresponding to `--vo2max normal`.
- `with_vo2max_forced/`: vO2max included without penalization, corresponding to
  `--vo2max forced`.

## Working principles

- Keep work focused on statistical methods, analyses, interpretation,
  reporting, and the corresponding reviewer responses and manuscript edits. Do
  not expand into unrelated scientific or editorial changes unless requested.
- Preserve the reviewers’ comments and distinguish them clearly from draft
  responses and proposed manuscript text.
- Treat the existing responses as work in progress. Preserve the user’s
  reasoning where supported, and flag inconsistencies or unsupported claims
  rather than silently perpetuating them.
- Trace statistical claims and reported numbers to the analysis code and its
  generated results. Do not invent results, infer undocumented decisions by the
  original statistician, or describe an analysis as completed when it has not
  been run.
- Keep the analysis code, generated outputs, reviewer responses, and proposed
  manuscript changes consistent. When a change affects results, identify which
  outputs and passages require updating.
- Generate revised results through the analysis code rather than manually
  editing result files. Preserve source data and unrelated existing work.
- Maintain reproducibility, including random seeds, analysis settings, and the
  distinction between analysis variants. Explain substantive methodological
  changes and their implications.
- Use precise, measured statistical language. Distinguish exploratory findings
  from confirmatory evidence and ensure conclusions reflect the analysis design
  and uncertainty.

## Validation

For analysis changes, perform checks appropriate to the affected methods and
regenerate the relevant outputs when feasible. For changes to
`revision/reviewer_comments.qmd`, verify that referenced results and files
exist and render the document when needed to check executable content and
presentation. Report what was checked and explicitly identify any analyses or
rendering steps that were not run.

Use the existing result lookups in `revision/reviewer_comments.qmd` where
possible rather than duplicating numerical results as hard-coded text. Check
that analysis settings stated in the document remain consistent with
`code/so_prediction_rev.R`.
