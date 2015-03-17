corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  corr_vec <- vector(mode='numeric')
    
  for (file_name in list.files(directory)) {
    full_path <- paste(directory, "\\", file_name, sep="")
    raw_data <- read.table(full_path, header=TRUE, sep=",")
    df <- subset(raw_data, !is.na(raw_data[,2]) & !is.na(raw_data[,3]))
    if (nrow(df) > threshold) {
      corr_vec <- c(corr_vec, cor(df$sulfate, df$nitrate))
    }
  }
  
  corr_vec
}



