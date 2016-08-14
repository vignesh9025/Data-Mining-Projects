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
      else if(on == "heart failure")
      {
            on <- paste("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", sep = "")
      }
      else if(on == "pneumonia")
      {
            on <- paste("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia", sep = "")
      }
      else
      {
            stop("invalid outcome")
      }
      
      ## Constricting using state and on
      outcome <- outcome[outcome$State == state, c("Hospital.Name", on)]
      if(length(outcome[,"Hospital.Name"]) == 0)
      {
            stop("invalid state")
      }
      
      outcome <- outcome[outcome[ ,on]!="Not Available", ]
      ## Computing minimum value
      
      m <- min(as.numeric(outcome[ ,on]))
      
      o <- outcome[as.numeric(outcome[ ,on]) == m,"Hospital.Name"]
      if(length(o) > 1)
      {
            o <- o[order(o)]
            o[1]
      }
      else
      {
            o
      }
      
}
