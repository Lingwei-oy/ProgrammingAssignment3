# read data into R
setwd("/Users/lingwei/Desktop/Coursera/rprog_data_ProgAssignment3-data")
find <- function(s, x, num){
    result[ , x] <- as.numeric(result[ , x])
    # select the data to be ordered
    select <- result[state ==s, ]
    newdata <- result[order(result[result$State == s ,x]), ] 
    name <- newdata[num, "Hospital.Name"]
    return (name)
}
## Read outcome data
result <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
## Check that state and outcome are valid
rankhospital<- function(state, outcome, num = "best"){
    # check whether num is more than hospital numbers
    if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop ("invalid outcome")
    else if(!(state %in% unique(result$State))) stop ("invalid state")
    else if(is.numeric(num) & num > length(unique(result[result$State == state, "Hospital.Name"]))) return(NA)
    # give rank 
    else{
        if(num == "best") num <- 1
        else if (num == "worst") num <- length(unique(result[result$State == state, "Hospital.Name"]))
        else if (outcome == "heart attack"){
            find(s = state, x = 14, num)
        }
        else if(outcome == "heart failure"){
            find(s = state, x= 17, num)
        }
        else{
            find(s = state, x= 23, num)
        }
        
        
        
    }
}          

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
        else if (num > nrow(outcome_by_state[outcome_by_state$State == state, ])){
                stop("NA")
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
                        state_data <- outcome_by_state[outcome_by_state$State == state, c(2, index)]
                        if (num == "worst"){
                                num <- nrow(outcome_by_state$State)
                        }
                        if (num == "best"){
                                num <- 1
                        }
                        
        }
        ## 30-day death rate
        return(state_data[order(state_data[, index], state_data[, 2]), 2][num])
        
}
