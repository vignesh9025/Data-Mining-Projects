corr <- function(directory, threshold = 0)
{
    flag <- 0
    corvect <- c()
    corvect <- as.numeric(corvect)
    sulfate <- c()
    nitrate <- c()
    for (i in 1:332)
    {
        ## Directory init
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
        
        x <- read.csv(loc)
        
        ## Cleaning
        
        x <- x[complete.cases(x), ]
        
        ## Checking threshold
        
        if(nrow(x) > threshold)
        {
            ## Getting values for Nitrate and Sulfate
            sulfate <- x[, "sulfate"]
            nitrate <- x[, "nitrate"]
            corvect <- c(corvect, cor(sulfate, nitrate))
        }
        
    }
    
    round(corvect, 5)
    
}