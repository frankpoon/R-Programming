complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  id_list = list()
  nobs_list = list()

  
  for (file_id in id) {
    file_name <- sprintf("%0.3d.csv", file_id)
    full_path <- paste(directory, "\\", file_name, sep="")
    raw_data <- read.table(full_path, header=TRUE, sep=",")
    fd <- subset(raw_data, !is.na(raw_data[,2]) & !is.na(raw_data[,3]))
    id_list <- c(id_list, file_id)
    nobs_list <- c(nobs_list, nrow(fd))
  }
  
  nobs_df <- data.frame(cbind(id=id_list, nobs=nobs_list))
  nobs_df
}