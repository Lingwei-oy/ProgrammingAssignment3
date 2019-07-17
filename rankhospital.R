# This is the start of a new file
rankhospital <- function(state, outcome, num = "best") {
        
        ## set working directory
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
        ## Return hospital name in that state with the given rank
        
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
                        if (num == "worst"){
                                num <- sum(complete.cases(as.numeric(state_data[, index])))
                        }
                        if (num == "best"){
                                num <- 1
                        }
                        if (num > nrow(outcome_by_state[outcome_by_state$State == state, ])){
                                stop("NA")
                        }
                        
        }
        ## 30-day death rate
        return(state_data[order(as.numeric(state_data[, index]), state_data[, 2]), 2][num])
        
}
