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
  ## out<-data.frame(id=numeric(length(id)),nobs=numeric(length(id)))
  nobs <- numeric(length(id))
  for(i in seq_along(id)){
    filename <- paste0(formatC(id[i], width=3, flag="0"),".csv") 
    fullpath <- paste(getwd(),directory,filename,sep="/")
    sample <- read.csv(fullpath)
    vars<-names(sample)
    for(j in vars){
           sample<-subset(sample,!is.na(sample[j]))
      }
    nobs[i]<-length(sample[,1])
  }
  out<-data.frame(id,nobs)
  out
  
}