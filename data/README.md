# Study data

The data files in this folder are not in the repository. Git ignores everything
here except this file. The data contain personal health information and are
available on request from the corresponding author of the article (see its data
availability statement).

To run the pipeline, provide the files below in this folder, without renaming
them, and run `make` from the project root:

- On LeoMed, where the data are stored in
  `/cluster/work/nexus/2026_st_brasier_heatpub/data`, run `make_symlinks` to
  link them here. Running it again refreshes the links.
- Elsewhere, copy the files here.

## Files

These are the earliest versions of the data available when this repository was
assembled. The raw device exports and the original REDCap export were not. The
code that produced these files from them is documented, but was not run, in
`code/raw_preprocessing/`.

| File                            | Contents                               | Use in the pipeline                                 |
| ------------------------------- | -------------------------------------- | --------------------------------------------------- |
| `data_eCRFbased.RData`          | REDCap data, raw and derived           | `data_OG` is the input; the rest serves as a check  |
| `ts_ecel.RData`                 | `df_ecel`: core body temperature       | Input                                               |
| `ts_holt.RData`                 | `df_holt`: heart rate, rMSSD and SDNN  | Input                                               |
| `ts_ibutton.RData`              | `df_ib`: close-body temperature and RH | Input                                               |
| `ts_baspo.RData`                | `df_baspo`: laboratory WBGT            | Input                                               |
| `Measures at home_iButton.xlsx` | Close-body conditions at home          | Input                                               |

### `data_eCRFbased.RData`

- `data_OG`: the REDCap export with one row per participant and event, after
  the variable labels were applied and `pid` and `visit` were formatted, and
  before any correction.
- `df_eCRF`, `df_eCRF_SE`, `df_screen`, `df_base`, `df_times`, `df_psweat`,
  `df_psweat_long`: tables derived from `data_OG`. `code/prepare_data.R`
  regenerates them and stops if the result differs from these stored copies.
  The clock times in `df_times` carry the date of the run that built the file
  (2023-10-13), which has no meaning for the study.

### Device files

Each file has one row per recording, already cleaned, for the 59 visits with
device data. All four files share these columns:

- `pid`, `visit`: participant and visit.
- `heatstress_type`: the stressor of the visit.
- `start_acclim`: clock time at which acclimatization started. As in `df_times`,
  the date is that of the run that built the file (2023-08-09). Differences
  between clock times are valid only within a file.
- `t_min_*`: minutes since the start of acclimatization.
- `phase`: `Acclim` (before minute 30, including records before minute 0),
  `Heat` (from minute 30), `Heat+Cloth` (from minute 60) or `Cool` (from minute
  90, including records from minute 120).

Their clock time column is `time_posix`, or `t_clock` in `df_baspo`.
`code/prepare_data.R` verifies `t_min_*`, `phase`, the stressor and the start
time, but the rest of the cleaning cannot be verified without the raw exports.

Participant 5, visit 2 is absent from these files. Participant 18, visit 4 is
present, and the analyses exclude it where the article does.

### `Measures at home_iButton.xlsx`

One column per participant and two rows: the mean close-body temperature and
relative humidity at home. The original recordings and the code that summarized
them were not available.

## Checksums

SHA-256 checksums of the files used for the article's results. To verify a
copy, run this from this folder:

```bash
sha256sum --check <<'EOF'
fb8efae708f31a61a4566fa2d3f4bbb6e4ce7735316a3c5be27fcdb34eff7c72  data_eCRFbased.RData
71d34822787b4b5104cad805205e54788cf821e685f545e66f31b05d6ad53c78  Measures at home_iButton.xlsx
66a0b562e9ab24f6610f659c2919d0364fe20db2cf8f7353072bd50cb200681b  ts_baspo.RData
31291583618adc3cc280b426a13a9aa3e7bf4b9822581f2b60b6439a016227d7  ts_ecel.RData
3a8791a243adb5f24b3ffb529d112214d794e31e99eb3d04ef2c3a4fbd814f54  ts_holt.RData
81f50af1ca9eb49d6f1d3f62c72c109b621f9baa95949cdc30453637772f012b  ts_ibutton.RData
EOF
```
