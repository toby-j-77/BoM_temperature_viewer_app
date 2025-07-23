library(zoo)   # for na.approx()
library(TTR)   # for SMA()

split_fill <- function(x,
                       na_length = 5,
                       ra_length = 3) {
  n <- length(x)
  # vector of chunk IDs (0 means “skip – big NA run”)
  chunk_id <- integer(n)
  id <- 1L
  i <- 1L
  
  while(i <= n) {
    start <- i
    j     <- start
    
    # advance j until we hit a “big” NA run
    while(j <= n) {
      if (is.na(x[j])) {
        # measure the length of this NA run
        k <- j
        while(k <= n && is.na(x[k])) k <- k + 1L
        if ((k - j) >= na_length) {
          # this is a splitter—stop before it
          break
        } else {
          # small NA run: include it in the chunk
          j <- k
        }
      } else {
        j <- j + 1L
      }
    }
    
    end <- j - 1L
    if (end >= start) {
      # assign this interval to chunk `id`
      chunk_id[start:end] <- id
      id <- id + 1L
    }
    
    # skip the big NA run entirely
    if (j <= n && is.na(x[j])) {
      k <- j
      while(k <= n && is.na(x[k])) k <- k + 1L
      i <- k
    } else {
      i <- j
    }
  }
  
  # prepare output
  ra <- rep(NA_real_, n)
  
  # process each chunk
  for (g in unique(chunk_id[chunk_id > 0])) {
    idx <- which(chunk_id == g)
    if (length(idx) >= ra_length) {
      seg <- x[idx]
      # 1) interpolate short NAs
      seg_filled <- na.approx(seg, na.rm = FALSE)
      # 2) running average
      ra_vals <- SMA(seg_filled, n = ra_length)
      ra[idx] <- ra_vals
    }
  }
  
  ra
}

#testing

test <- read_csv("Input_data/test_data.csv")

test_filled <- test %>%
  mutate(RA = split_fill(test_2, na_length = 5, ra_length = 3))

test_filled |> print(n = 48)
