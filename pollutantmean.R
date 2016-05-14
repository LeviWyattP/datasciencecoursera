pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  
  #first we are going to load the data
  csv_data <- rbind()
  
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
    csv_data <- rbind(csv_data, csv_file)
    
    
  }
  
  #now lets calculate
  if (pollutant == 'sulfate')
  {
    total <- sum(csv_data$sulfate)
    count <- length(csv_data$sulfate)
  }
  if (pollutant == 'nitrate')
  {
    total <- sum(csv_data$nitrate)  
    count <- length(csv_data$nitrate)
  }
  #print(total)
  
  #return(csv_data)
  return(total/count)
  
}