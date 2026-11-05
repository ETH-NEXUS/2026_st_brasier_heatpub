# Preprocessing of the raw exports (archived, not run)

These scripts document how the raw exports became the files described in
`data/README.md`. The raw exports were not available when this repository was
assembled, so **this code was not run and its results were not verified**. The
pipeline starts from the files in `data/` instead.

| Script                    | Raw input                                    | Output                                         |
| ------------------------- | -------------------------------------------- | ---------------------------------------------- |
| `dataprep_eCRF_import.R`  | REDCap export (CSV) and its CTU label script | `data_OG`, then see `code/prepare_data.R`      |
| `dataprep_eCel.R`         | `e-Celsius/`                                 | `ts_ecel.RData`                                |
| `dataprep_holt.R`         | `EKG/CSV files/`                             | `ts_holt.RData`                                |
| `dataprep_ibutton.R`      | `iButton Logger/`                            | `ts_ibutton.RData`                             |
| `dataprep_baspo.R`        | `BASPO-Temp-rel.Hum/`                        | `ts_baspo.RData`                               |

The scripts are extracts kept as found, including the original comments and
formatting, so they do not follow the project's coding conventions. Only the
plotting sections were removed, and visit dates, clock times used as cut-offs
and a colleague's name in comments were redacted.

Points to keep in mind when reading them:

- They expect objects defined in the earlier report (`Rproj_path`, `data_path`,
  `mytabs`, `%&%`, `source2`), and `df_times` from the eCRF preparation.
- The available versions were edited after the data files were produced. The
  clock times stored in the data indicate that the device files were built on
  2023-08-09 and the eCRF file on 2023-10-13, whereas the scripts contain edits
  from 2025. They may therefore differ from the code that produced the files.
- `dataprep_ibutton.R` reads its files with `fread(text = <path>, ...)`. As
  written, this fails with data.table 1.18.6.1.
- Comments in `dataprep_eCel.R` record manual edits to raw files, which the
  code does not perform.
- The code that reduced the home iButton recordings to the means in `Measures
  at home_iButton.xlsx` was not found.
