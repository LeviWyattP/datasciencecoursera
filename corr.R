corr <- function(directory, threshold = 0) {
  

  # get number of observed cases for each file
  nobs_df <- complete("specdata")
  
  outData <- nobs_df$nobs > threshold
  ids <- c()
  
  # loop to get ids above threshold
  for (iter in seq(length(nobs_df$id)))  {

    # if above threshold
    if (nobs_df$nobs[iter]>threshold) {
      
      # append id to master list
      ids <- c(ids, nobs_df$id[iter])
      
    }
    
  }

  correlations <- c()
  # now we can use the correlation
  for (i in ids) {
    
    ## LOAD THE FILES
      # form filename - then add padding 0's
      file_name <- as.character(i)
      while (nchar(file_name)<3)
      {
        file_name <- paste("0", file_name, sep="")
      }
      
      #combine for full filename
      filename <- paste(directory, "\\",file_name, ".csv", sep = "")
      
      
      #load csv file
      csv_file <- read.csv(filename, header = TRUE, na.strings = c("NA","NaN", " "))
      
      #remove bad rows
      csv_file <- na.omit(csv_file)
      
      # calculate correlations and append to vector
      correlations <- c(correlations, cor(csv_file$sulfate, csv_file$nitrate))
      
  }
  
  #now calculate the covariance & return

  return(correlations)
}


