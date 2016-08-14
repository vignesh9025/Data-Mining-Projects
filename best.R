best <- function(state, on)
{
      outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      ## Attributes: [2], [7], [11]
      
      outcome <- outcome[complete.cases(outcome), ]
      
      outcome <- outcome[, c(2, 7, 11, 17, 23)]
      if(on == "heart attack")
      {
            on <- paste("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", sep = "")
      }
      if(on == "heart failure")
      {
            on <- paste("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", sep = "")
      }
      if(on == "pneumonia")
      {
            on <- paste("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia", sep = "")
      }
      
      ## Constricting using state and on
      outcome <- outcome[outcome$State == state, c("Hospital.Name", on)]
      outcome <- outcome[outcome[,on]!="Not Available",]
      ## Computing minimum value
      
      m <- min(as.numeric(outcome[,on]))
      
      outcome[as.numeric(outcome[,on]) == m,"Hospital.Name"]
}
