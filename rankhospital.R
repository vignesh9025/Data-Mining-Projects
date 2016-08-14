rankhospital <- function(state, on, num = "best") 
{
      ## Read outcome data
      outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      ## Check that state and outcome are valid
      
      ##CLEANING THE DATA
      
      outcome <- outcome[complete.cases(outcome), ]
      
      ##Reducing the table
      
      outcome <- outcome[, c(2, 7, 11, 17, 23)]
      
      ##Ranking can only be done after reducing the outcome
      ##Reducing the outcome and state
      
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
      
      outcome <- outcome[outcome$State == state, c("Hospital.Name", on)]
      if(length(outcome[,"Hospital.Name"]) == 0)
      {
            stop("invalid state")
      }
      ##Cleaning the data further
      
      outcome <- outcome[outcome[,on]!="Not Available",]
      
      ##RANKING THE TABLE WITH 2 COLS - NAME AND DEATH RATE
      
      outcome[,on] <- as.numeric(outcome[,on])
      outcome[,"Hospital.Name"] <- as.character(outcome[,"Hospital.Name"])
      namerank <- outcome[order(outcome[,on], outcome[,"Hospital.Name"]), "Hospital.Name"]
      
   ##   namerank2 <- namerank[order(namerank)]
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
      if(num == "best")
      {
            namerank[1]
      }
      else if(num == "worst")
      {
            namerank[length(namerank)]
      }
      else if(num >= 1 && num <= length(namerank))
      {
            namerank[num]
      }
      else
      {
            "NA"
      }
      
}