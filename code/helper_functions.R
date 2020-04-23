mad = function(df, epoch = "1 min") {
  require("magrittr")
  mad = df %>% 
    dplyr::mutate(         
      r = sqrt(X^2 + Y^2 + Z^2),
      HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP, epoch)) %>% 
    dplyr::group_by(HEADER_TIME_STAMP) %>% 
    dplyr::summarise(
      MAD = mean(abs(r - mean(r)))
    )
}

get_dynamic_range = function(header) {
  hdr = strsplit(header, "---")[[1]]
  hdr = gsub("-", "", hdr)
  hdr = hdr[ !hdr %in% ""]
  ACTIGRAPH_SERIALNUM_PATTERN <- "SerialNumber:([A-Za-z0-9]+)StartTime.*"
  sn = sub(ACTIGRAPH_SERIALNUM_PATTERN, "\\1", hdr[[2]])
  at <- substr(sn, 1, 3)
  gr <- switch(at, MAT = "3", CLE = "6", MOS = "8", TAS = "8")
  if (stringr::str_detect(hdr[[1]], "IMU")) {
    gr <- "16"
  }
  gr = as.numeric(gr)
  gr = c(-gr, gr)
  gr
}

read_mat = function(mat, verbose = TRUE) {
  L = try({
    R.matlab::readMat(mat)
  }, silent = TRUE)
  if (!inherits(L, "try-error")) {
    return(L)
  }
  
  info = rhdf5::h5ls(mat, all = TRUE)
  names = info$name
  convert_mat_string = function(x) {
    rawToChar(as.raw(x))
  }
  L = lapply(names, function(x) {
    # print(x)
    if (verbose) {
      xinfo = info$dim[info$name %in% x][[1]]
      msg = paste0("Reading in ", x, ", dimensions: ", xinfo)
      message(msg)
    }
    out = rhdf5::h5read(file = mat, name = x)
    out_attrs = rhdf5::h5readAttributes(mat, name = x)
    MATLAB_class = out_attrs$MATLAB_class
    if (!is.null(MATLAB_class)) {
      if (MATLAB_class == "char") {
        out = convert_mat_string(out)
      }
    }
    out
  })
  names(L) = names
  return(L)
}

read_acc_mat = function(mat, ..., check_names = TRUE) {
  names = rhdf5::h5ls(mat)$name
  if (check_names) {
    stopifnot(all(c("Xi", "fs", "hed", "startdate", "starttime") %in%
                    names))
  }
  L = read_mat(mat, ...)
  colnames(L$Xi) = c("X", "Y", "Z")
  L$Xi = tibble::as_tibble(L$Xi)
  L$startdate = gsub("_", "/", L$startdate)
  L$starttime = gsub("_", ":", L$starttime)
  start_date = lubridate::mdy_hms(paste0(L$startdate, " ", L$starttime))
  L$fs = c(L$fs)
  srate = L$fs
  L$Xi$HEADER_TIME_STAMP = start_date + (seq(0, nrow(L$Xi) - 1)/srate)
  L
}

sub_thing = function(hdr, string) {
  x = hdr[grepl(string, hdr)]
  x = gsub(string, "", x)
  x = trimws(x)
}

fix_zeros = function(gt3x) {
  zero = rowSums(gt3x[, c("X", "Y", "Z")] == 0) == 3
  names(zero) = NULL
  gt3x$X[zero] = NA
  gt3x$Y[zero] = NA
  gt3x$Z[zero] = NA
  gt3x$X = zoo::na.locf(gt3x$X, na.rm = FALSE)
  gt3x$Y = zoo::na.locf(gt3x$Y, na.rm = FALSE)
  gt3x$Z = zoo::na.locf(gt3x$Z, na.rm = FALSE)
  
  gt3x$X[ is.na(gt3x$X)] = 0
  gt3x$Y[ is.na(gt3x$Y)] = 0
  gt3x$Z[ is.na(gt3x$Z)] = 0
  gt3x
}

WearNonWear = function(ac, win = 90){
  ### Create WNW flags
  # 0 - non-wear
  # 1 - wear
  
  ac.ones = rep(0, length(ac))
  ac.ones[ac > 0] = 1
  
  meanw = runstats::RunningMean(x = ac.ones, W = win)
  meanw[meanw < 1/win] = 0
  
  out = rep(1, length(ac))
  ind = which(meanw == 0)
  
  if(length(ind) != 0){ # only run when there in as non-wear time
    for(i in 1:length(ind)){
      out[ind[i]:(ind[i] + win - 1)] = 0
    }
  }
  
  out[is.na(ac)] = NA
  return(out)
}


tabber = function(
  x, 
  y, 
  dnames=c("x", "y")) {
  x = as.numeric(x)
  y = as.numeric(y)
  stopifnot(all(unique(c(x,y)) %in% c(0, 1, NA)))
  tt = sum(x * y)
  t1 = sum(x)
  t2 = sum(y)
  tab = matrix(c(length(x) - t1 - t2 + tt,  t1 - tt, t2 - tt, tt), 2, 2)
  n = list(c("FALSE", "TRUE"), c("FALSE", "TRUE"))
  names(n) = dnames
  dimnames(tab) = n
  tab = as.table(tab)
  tab = c(tab)
  return(tab) 
}

cwa_mims = function(file, epoch = "1 min", ...) {
  require("magrittr")
  acc = biobankr::read_cwa(file)
  hdr = acc$header
  dynamic_range = c(-hdr$accrange, hdr$accrange)
  acc = acc$data
  acc = as.data.frame(acc)
  acc = acc %>% 
    dplyr::rename(X = x,
           Y = y,
           Z = z,
           HEADER_TIME_STAMP = time) %>% 
    dplyr::select(HEADER_TIME_STAMP, X, Y, Z)
  mims = MIMSunit::mims_unit(acc, 
                             dynamic_range = dynamic_range, 
                             epoch = epoch,
                             ...)
  mims
}
