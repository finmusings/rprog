corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ok_list<-complete(directory,1:332)
  ok_list<-subset(ok_list,nobs>threshold)
  if(length(ok_list[,1])==0){
    return
  }else{
  correl<-numeric(length(ok_list[,1]))
  for(i in 1:length(ok_list[,1])){
      filename <- paste0(formatC(ok_list[i,"id"], width=3, flag="0"),".csv") 
      fullpath <- paste(getwd(),directory,filename,sep="/")
      sample <- read.csv(fullpath)
      vars<-names(sample)
      for(j in vars){
        sample<-subset(sample,!is.na(sample[j]))
      }
      correl[i]<-cor(sample["nitrate"],sample["sulfate"])
  }
  correl
}
}