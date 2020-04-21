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



# win = 5
# datai = read.csv(paste0(file_path,files[1]),skip = 10)
# ac = datai$vectormagnitude
# 
# ac = rep(1, 100)
# ac[40:60] = 0


