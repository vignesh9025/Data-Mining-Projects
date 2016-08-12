complete <- function(directory, id = 1:332)
{
 ## num <- c()
  vect <- c()
  for(i in id)
  {
    ## Initialize location
    if(i<=9)
    {
      loc <- paste(directory,"/00",i,".csv", sep="")
    }
    if(i>=10 && i<=99)
    {
      loc <- paste(directory,"/0",i,".csv", sep="")
    }
    if(i >= 100)
    {
      loc <- paste(directory,"/",i,".csv", sep="")
    }
    
    ##Reading from directory
    x <- read.csv(loc)
    
    ##Cleaning the data
    x <- x[complete.cases(x), ]
    
    num <- nrow(x)
    vect <- c(vect, num)
  }
  df <- data.frame(id = id, nobs = vect)
  df <- df[complete.cases(df),]
  df
}