pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  ## initialise total to hold all the sums and count to hold the number
  ##    of observations
  total<-numeric(length(id))
  count<-numeric(length(id))
  ##Loop through monitors in id
  for(i in seq_along(id)){
    filename <- paste0(formatC(id[i], width=3, flag="0"),".csv") 
    fullpath <- paste(getwd(),directory,filename,sep="/")
    data <- read.csv(fullpath)
    total[i] <- sum(data[[pollutant]], na.rm = TRUE)
    count[i] <- sum(!is.na(data[[pollutant]]))
  }
  sum(total)/sum(count)
}