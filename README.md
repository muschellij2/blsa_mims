
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BLSA MIMS project

<!-- badges: start -->
<!-- badges: end -->

The project uses raw accelerometry data from \~700 participants in
Baltimore Longitudinal Study of Aging (BLSA), each monitored for aporox.
1 week with a wrist-worn ActiGraph sensor.

The goal of the project is to:

1.  Develop and use R software to aggregate raw data at minute-level
    with open-source measures: MIMS, ENMO, MAD, AI.

2.  Quantify association between AC and open-source measures marginally
    and conditionally on age, sex and BMI.

3.  Harmonize minute-level AC with open-source measures via one-to-one
    mapping.

4.  Reproduce some of the published BLSA results that used AC with the
    use of the open-source measures.

# Repository navigation

For practitioners, potentially most useful R code scripts and result
files are referenced below.

### Methods

-   R code script to generate raw data quality check flags:
    [code/data\_preprocessing/mat\_to\_minute\_quality\_flag.R](https://github.com/muschellij2/blsa_mims/blob/master/code/data_preprocessing/mat_to_minute_quality_flag.R)

-   R code script to compute MIMS, MAD, AI:
    [code/data\_preprocessing/mat\_to\_open\_source\_measures.R](https://github.com/muschellij2/blsa_mims/blob/master/code/data_preprocessing/mat_to_open_source_measures.R)

-   R code script to compute ENMO (with data calibration step):
    [code/data\_preprocessing/mat\_calibrated\_to\_open\_source\_measures.R](https://github.com/muschellij2/blsa_mims/blob/master/code/data_preprocessing/mat_calibrated_to_open_source_measures.R)

-   R code script to compute valid minute and valid day flags and to
    filter the participants:
    [code/data\_preprocessing/prepare\_measures\_masterfile.R](https://github.com/muschellij2/blsa_mims/blob/master/code/data_preprocessing/prepare_measures_masterfile.R)

-   R code script to perform data imputation:
    [code/data\_preprocessing/prepare\_measures\_masterfile\_winsorized\_imp.R](https://github.com/muschellij2/blsa_mims/blob/master/code/data_preprocessing/prepare_measures_masterfile_winsorized_imp.R)

### Results

-   CSV table with model-fitted values of MIMS, ENMO, MAD, AI for a
    range of AC values:
    [results\_public/mapping\_between\_measures\_FITTED.txt](https://raw.githubusercontent.com/muschellij2/blsa_mims/master/results_public/mapping_between_measures_FITTED.txt)

-   R code script with fast mapping functions between AC and MIMS, ENMO,
    MAD, AI:
    [code/data\_preprocessing/measures\_mapping\_FUNC.R](https://github.com/muschellij2/blsa_mims/blob/master/code/data_preprocessing/measures_mapping_FUNC.R)
