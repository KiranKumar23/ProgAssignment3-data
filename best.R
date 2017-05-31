#C:/H116501/Learningmaterials/R/RProgramming/Simulation/ProgAssignment3-data
setwd("C:/H116501/Learningmaterials/R/RProgramming/Simulation/ProgAssignment3-data")
outcome <- read.csv("C:/H116501/Learningmaterials/R/RProgramming/Simulation/ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  fd   <- as.data.frame(cbind(data[, 2],   # hospital
                              data[, 7],   # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in% fd[, "state"]){
    stop('invalid state')
  } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else {
    si <- which(fd[, "state"] == state)
    ts <- fd[si, ]    # extracting data for the called state
    oi <- as.numeric(ts[, eval(outcome)])
    min_val <- min(oi, na.rm = TRUE)
    result  <- ts[, "hospital"][which(oi == min_val)]
    output  <- result[order(result)]
  }
  return(output)
}

# example output:
#> best("MD", "pneumonia")
#[1] "GREATER BALTIMORE MEDICAL CENTER"
#> best("NY", "hert attack")
#Error in best("NY", "hert attack") : invalid outcome
#> best("MD", "heart attack")
#[1] "JOHNS HOPKINS HOSPITAL, THE"



