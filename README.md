
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

# Data

In the data folders, the `mats` folder contains: 1. Matlab `.mat` files.
These are “new” MATLAB files, which means they are essentially HDF5
files. Old `.mat` files can be read using `R.matlab::readMat`. These
cannot. Since they are `hdf5` files, we can use `rhdf5` to read them in.
In `code/helper_functions.R` the `read_acc_mat` function can read it in
as a data.frame. These are the raw output from ActiLife giving
accelerations. - we want to compare these to the output of the open
source tools. 2.
