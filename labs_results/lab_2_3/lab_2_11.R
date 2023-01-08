df <- read.csv('data/RH_T.csv')

find_max_diff <- function(df) {
  max_diff <- 0
  max_i <- 2
  for (i in 2:nrow(df)) {
    dt <- abs(as.numeric(df$T2M[i - 1]) - as.numeric(df$T2M[i]))
    if (dt > max_diff) {
      max_diff <- dt
      max_i = i
    }
  }
  return(df[c(max_i-1, max_i), ])
}

find_max_diff(df)
