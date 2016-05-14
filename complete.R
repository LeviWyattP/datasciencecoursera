complete <- function(directory, id = 1:332) {
  
  
  #first we are going to load the data
  df <- data.frame()

  # for each id
  for (i in id)
  {

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
    
      
    #add the data to the combined data
    df <- rbind(df, data.frame(id = i, nobs = nrow(csv_file)))
    
    
  }
  
  return(df)
  
}