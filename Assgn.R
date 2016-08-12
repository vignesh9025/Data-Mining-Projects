pollutantmean <- function(directory = "specdata", pollutant, id = 001:332)
{
  vec2 <- c()
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

    ## Reading from directory
    x <- read.csv(loc)
    
    ##Cleaning the dataset
    x <- x[complete.cases(x), 1:4]
    
    ##Storing "pollutant" data in a vector
    vec <- x[,pollutant]
    vec2 <- c(vec, vec2)
  }
  mean(vec2)
}