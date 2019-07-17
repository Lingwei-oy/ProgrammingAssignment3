# this is the start of a new file
# should remember to add output to every branch
rankall <- function(outcome, num = "best") {
        
                ## set working directory
                setwd("/Users/lingwei/Desktop/coursera/rprog_data_ProgAssignment3-data")
                
                ## Read outcome data
                ## add na.strings = "Not Available", to avoid the problem of not getting NA
                outcome_by_state <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", header = TRUE, stringsAsFactors = FALSE)
                
                
                ## Check that state and outcome are valid
                
                if (outcome %in% c("heart attack","heart failure","pneumonia") == FALSE){
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
                        if (num == "best"){
                                num <- 1
                        }
                        outcome_all <- data.frame()
                        i <- 0
                ## For each state, find the hospital of the given rank
                        for (state in unique(outcome_by_state$State)){
                                i <- i + 1
                                state_data <- outcome_by_state[outcome_by_state$State == state, ]
                        # sort here
                                state_data<- state_data[order(state_data[, index], state_data[, 2]), c(2, index, 7) ]
                                colnames(state_data) <- c("hospital", "outcome", "state")
                                if (num == "worst"){
                                #        num_1 <- sum(complete.cases(as.numeric(state_data[, index])))
                                        num_1 <- sum(complete.cases(state_data[, index]))
                                #        s <-  state_data[order(as.numeric(state_data[, index]), state_data[, 2]), c(2,7)][num_1, ]
                                        s <-  state_data[num_1, "outcome"]
                                }
                               # else if (!(num > nrow(state_data))){
                                else {
                               #  s <-  state_data[order(as.numeric(state_data[, index]), state_data[, 2]), c(2,7)][num, ]
                                        s <-  state_data[num, "outcome"]
                                }
                               # else{
                                #        s <- c(NA, state)
                               # }
                       # if (is.na(s[1])){ print (s)}
                       # print (i)
                        outcome_all <- rbind (outcome_all, cbind(state_data[num, "hospital"],state, outcome, s))
                        }
                ## Return a data frame with the hospital names and the
                ## 30-day death rate
                        
                ## (abbreviated) state name
                        return(outcome_all[order(outcome_all$state), ])
                
        }
        
}

# This is another way to loop over states

rankall_2 <- function(outcome, num = "best") {
        
        ## set working directory
        setwd("/Users/lingwei/Desktop/coursera/rprog_data_ProgAssignment3-data")
        
        ## Read outcome data
        ## add na.strings = "Not Available", to avoid the problem of not getting NA
        outcome_by_state <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", header = TRUE, stringsAsFactors = FALSE)
        
        ## filter out data of three outcomes and shuffle data by hospital names and mortality 
        ## Return hospital name in that state with the given rank
        data_mortality <- outcome_by_state[, c(2, 7, 11, 17, 23)]
        names(data_mortality) <- c("hospital", "state", "heart attack", "heart failure", "pheumonia")

        
        ## Check that state and outcome are valid
        
        if (outcome %in% c("heart attack","heart failure","pneumonia") == FALSE){
                stop("invalid outcome")
        }
        
        else{
                
                ## For each state, find the hospital of the given rank
                lapply(split(outcome_by_state, State), )

                find_hospital <- function(state, num, outcome){
                        # shuffle state data 
                        state_data<- state_data[order(state_data[, outcome], state_data[, "hospital"]), ]
        
                        if (num == "worst"){
                                
                                num_1 <- sum(complete.cases(state_data[, index]))
                                
                                s <-  state_data[num_1, outcome]
                        }
                        
                        else {
                                
                                s <-  state_data[num, outcome]
                        }
                        
                        
                }
                ## Return a data frame with the hospital names and the
                ## 30-day death rate
                
                ## (abbreviated) state name
                return(outcome_all[order(outcome_all$state), ])
                
        }
        
}

