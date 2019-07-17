# This is the start of a new file
best <- function(state, outcome) {
        
        setwd("/Users/lingwei/Desktop/coursera/rprog_data_ProgAssignment3-data")
        
        ## Read outcome data
        outcome_by_state <- read.csv("outcome-of-care-measures.csv", header = TRUE, stringsAsFactors = FALSE)
        
        ## Check that state and outcome are valid
        if (state %in% outcome_by_state$State == FALSE){
                stop("invalid state")
        }
        else if (outcome %in% c("heart attack","heart failure","pneumonia") == FALSE){
                stop("invalid outcome")
        }
        ## Return hospital name in that state with lowest 30-day death
        else{
                if(outcome == "heart attack"){
                        index <- 11
                }
                else if(outcome == "heart failure"){
                        index <- 17
                }
                else {
                        index <- 23
                }
                state_data <- outcome_by_state[outcome_by_state$State == state, ]
                hospital_lowest_death <- state_data[order(as.numeric(state_data[, index]), state_data$Hospital.Name), c(2,index) ]
                hospital_lowest_death <- hospital_lowest_death$Hospital.Name[1]
        }
        return(hospital_lowest_death)
        ## rate
        
}
