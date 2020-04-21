setwd("~/Johns Hopkins/Jacek Urbanek - BLSA Accelerometry/gt3x")
ids = list.files(pattern = "[.]csv")
x = readLines("synced.txt")
x = unique(x)
iid = ids[1]
for (iid in ids) {
  print(iid)
  if (!iid %in% x) {
    cmd = paste0("rsync --progress ", shQuote(iid), " $jtenig:~/gt3x")
    system(cmd)
    cat(iid, file = "synced.txt", append = TRUE)
  }
}

# stopifnot(isTRUE(all.equal(gt3x$time, csv$time)))
