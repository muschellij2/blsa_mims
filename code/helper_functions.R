read_mat = function(mat) {
  L = try({
    R.matlab::readMat(mat)
  })
  if (!inherits(L, "try-error")) {
    return(L)
  }
  
  names = rhdf5::h5ls(mat)$name
  convert_mat_string = function(x) {
    rawToChar(as.raw(x))
  }
  L = lapply(names, function(x) {
    print(x)
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

read_acc_mat = function(mat, check_names = TRUE) {
  names = rhdf5::h5ls(mat)$name
  if (check_names) {
    stopifnot(all(c("Xi", "fs", "hed", "startdate", "starttime") %in%
                    names))
  }
  L = read_mat(mat)
  colnames(L$Xi) = c("X", "Y", "Z")
  L$Xi = tibble::as_tibble(L$Xi)
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
