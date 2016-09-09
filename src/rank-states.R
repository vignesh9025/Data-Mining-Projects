rankall <- function(on, num = "best") 
{
      ## Read outcome data
      outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      ## Check that state and outcome are valid
      
      ##CLEANING THE DATA
      
      outcome <- outcome[complete.cases(outcome), ]
      
      ## GET LIST OF STATES WITHOUT REP.
      
      states <- unique(outcome[,"State"])
      Hospital_Name_per_State <- c()
      
      
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
      
      state2 <- c()
      
      for(state in states)
      {
            
            outcome2 <- outcome[outcome$State == state, c("Hospital.Name", on)]
            if(length(outcome2[,"Hospital.Name"]) == 0)
            {
                  stop("invalid state")
            }
            ##Cleaning the data further
            
            outcome2 <- outcome2[outcome2[,on]!="Not Available",]
            
            ##RANKING THE TABLE WITH 2 COLS - NAME AND DEATH RATE
            
            outcome2[,on] <- as.numeric(outcome2[,on])
            outcome2[,"Hospital.Name"] <- as.character(outcome2[,"Hospital.Name"])
            namerank <- outcome2[order(outcome2[,on], outcome2[,"Hospital.Name"]), "Hospital.Name"]
            
            ##   namerank2 <- namerank[order(namerank)]
            ## Return hospital name in that state with the given rank
            ## 30-day death rate
            if(num == "best")
            {
                  Hospital_Name_per_State <- c(Hospital_Name_per_State, namerank[1])
                  state2 <- c(state2, state)
            }
            else if(num == "worst")
            {
                  Hospital_Name_per_State <- c(Hospital_Name_per_State, namerank[length(namerank)])
                  state2 <- c(state2, state)
            }
            else if(num >= 1 && num <= length(namerank))
            {
                  Hospital_Name_per_State <- c(Hospital_Name_per_State ,namerank[num])
                  state2 <- c(state2, state)
            }
            else
           {
                 "NA"
           }
      }
      
      d <- data.frame(State = state2, Name = Hospital_Name_per_State)
      d
      ## For each state, find the hospital of the given rank
      ## Return a data frame with the hospital names and the
      ## (abbreviated) state name
}