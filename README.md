
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blsa\_mims

<!-- badges: start -->

<!-- badges: end -->

The goal of `blsa_mims` is to:

1.  We wish to determine if alternative measures of physical activity,
    derived from the raw acceleration signals, track similarly with
    health outcomes as activity counts. Namely, we focus on open-source
    pipelines and implementations to transform raw files into activity
    summaries.  
2.  As activity counts have been used in a number of studies and
    activity-level thresholds have been derived, we wish to estimate
    equivalent thresholds in these open-source measures to allow for
    similar analysis done with activity counts.
3.  We describe the advantages of using these open source tools and
    present an R package that can aid in this analysis, while also
    providing the source code to generate our analysis.

# Manuscript draft

***\[REMOVE before making the repo public\!\]***

  - [BLSA\_MIMS
    draft](https://docs.google.com/document/d/1fRP6zrzATz7mky-c44kSXnRAMXghd-DEhVb-YokLtS4/edit?usp=sharing)

(based on [John’s first
draft](https://docs.google.com/document/d/11ULWQlMqsx4NHarXwDPZHmrTHVNL51ZUNEitQhYVF68/edit?usp=sharing))

# Repo documentation

## code

  - `add_nonwear.R` – runs WearNonWear function; reads files with suffix
    `60sec.csv.gz` from `/csv`, runs WearNonWear algorithm, saves as
    file with suffix `_nonwear.csv.gz`\[MK: TODO JM was it actually
    saved? I do not see any such files in `/csv`\]
  - `ag_read.R` – contains code to read gt3x files with
    `AGread::read_gt3x`
  - `check_dims.R` – runs `read_xi_dims` function from
    `/code/helper_functions` to check for correctness of the dimensions
    of files from `mats` dir
  - `check_gt3x_vs_csv.R` – runs checks related to open-source reading
    of acc data files
  - `check_gt3x_vs_mat.R` – cruns hecks related to open-source reading
    of acc data files
  - `compare_nonwear.R` – TODO JM
  - `get_metadata.R` – runs extraction of meta data (Acceleration Max,
    Acceleration Min, Serial Number, Device Type) for all files in
    `/gt3x` and saves summary at `results/device_info.rds`
  - `gt3x_read.R` – contains code to read gt3x files with
    `AGread::read_gt3x` \[TODO JM : how it differs from `ag_read.R` ? \]
  - `helper_functions.R` – contains set of util functions, including:
      - `round_away_zero` – TODO JM
      - `quick_ai` – TODO JM
      - `full_measures` – gets all measures to compare other than MIMS
      - `mad` – short function for MAD
      - `get_dynamic_range` – get the dynamic range in g from header
        (from Jacek header)
      - `read_mat` – general function to read in matlab .mat
      - `read_acc_mat` – specific function to read in Jacek
        accelerometry .mat
      - `sub_thing` – util for parsing the header from RAW csv from
        Actilife
      - `WearNonWear` – from Jacek - implementation of Choi
      - `tabber` – quick tabulation function of long logicals
      - `cwa_mims` – get MIMS from a CWA file (for Biobank)
      - `read_xi_dims` – TODO JM
  - `make_mims.R` – runs `SummarizedActigraphy::read_acc_csv` function
    to derive MIMS for all files at `/gt3x`; saves result at
    `/open_measures/` as `_MIMS.csv.gz` suffix files \[TODO JM : am I
    right it used /gt3x?\]
  - `make_thresholds.R` – runs code to estimate mapping between values
    of metrics with gam models; note comments for any data pre-filtering
    done
  - `mat_make_mims.R` – TODO JM
  - `plot_comparison.R` – generates data plots
  - `resample_data.R` – TODO JM
  - `rsync_gt3x.R` – script to upload `/gt3x` files from external
    location
  - `WearNonWear.R` – wear/non-wear algorithm; likely former version of
    [arctools/R/get\_wear\_flag.R](https://github.com/martakarass/arctools/blob/master/R/get_wear_flag.R)

Also:

  - files with prefix `CSV.e4069838.` – TODO JM \[MK: looks like job
    logs, to be deleted?\]

## covariates

This directory contains covariates data files.

The `mastervisit` files (`.sas7bdat`, `.rdata`, `.dta`) are last
modified on 2020-08-18 are up to date as of 2020-12-30 (clarified with
Jennifer Schrack on 2020-12-30).

## csv

This directory contains `.csv.gz` files with name suffix `60sec.csv.gz`.
These are the raw output from ActiLife giving activity counts –
proprietary minute-level summary statistic of accelerometry data.

Specifically, these are “vector magnitude of counts”, i.e. a square root
of the sum of squares of X, Y, and Z axis-specific activity counts
(clarified with Jacek Urbanek on 2020-12-10).

The files in `csv` directory are last modified on 2020-08-14 are up to
date as of 2020-12-28 (clarified with Jacek Urbanek on 2020-12-28).

## gt3x

  - `gt3x.gz` suffix files – TODO JM
  - `RAW.csv.gz` suffix files (few) – TODO JM
  - `_MIMS.csv.gz` suffix files (few) – TODO JM

Also:

  - `gzip.sh` – TODO JM
  - `helper_functions.R` – TODO JM \[MK: are these some older copy of up
    to date code file of similar name at
    blsa\_mims/code/helper\_functions.R ? \]
  - `make_mims.R` – TODO JM \[MK: are these some older copy of up to
    date code file of similar name at blsa\_mims/code/make\_mims.R ? \]
  - `synced.txt` – TODO JM \[MK: looks empty \]

## mats

This directory contains Matlab `.mat` files. These are the raw output
from ActiLife giving acceleration measurements \[*g*\] along three
orthogonal axes (subsecond-level accelerometry data). These were
received from Jacek Urbanek between 2020-12-27 and 2020-12-29.

These are “new” MATLAB files, which means they are essentially HDF5
files. Old `.mat` files can be read using `R.matlab::readMat`, these
cannot. Since they are `hdf5` files, we can use `rhdf5` to read them in.
In `code/helper_functions.R` the `read_acc_mat` function can read it in
as a data.frame.

## mats\_old

Previous (outdated) version of `mats` directory.

## open\_measures

  - `_MIMS.csv.gz` suffix files – files with MIMS computed for all files
    at `/gt3x`; derived with `code/make_mims.R` file \[TODO JM : am I
    right it used /gt3x?\]

## qc

  - `_read.gt3x.txt` suffix files – TODO JM
  - `_AGread.txt` suffix files – TODO JM

## resampled

  - `.rds` files – TODO JM

## results

  - `comparison_data.rds` – TODO JM
  - `device_info.rds` – summary of meta data (Acceleration Max,
    Acceleration Min, Serial Number, Device Type) extracted for all
    files in `/gt3x`; derived with `/code/get_metadata.R`
