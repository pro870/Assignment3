
rankhospital<-function(state,outcome,num = "best"){
        list.outcomes<- c("heart attack", "heart failure", "pneumonia")
        outcome.data<-read.csv("outcome-of-care-measures.csv"
                               , colClasses="character")
        
        if (state %in% outcome.data[,7] == "FALSE") {
                stop("invalid state")             
        }
        if (outcome %in% list.outcomes == "FALSE") {
                stop("invalid outcome")
        }
        subset.outcome<-subset(outcome.data,outcome.data$State == state)
         
        if (outcome == "heart attack") {
                x<- 11 
        } else if (outcome == "heart failure") {
                 x<- 17 
               } else {
                 x<- 23
        }
        if (num == "best") {
                y<-which.min(subset.outcome[,x])
        subset.outcome[y,2] 
        } else if ( num == "worst") {
                y<-which.max(subset.outcome[,x])
        subset.outcome[y,2]
        } else {
                y<-subset.outcome[order(subset.outcome[,x],decreasing=TRUE
                                        ,na.last=NA),]
                y[num,2]
        }
        
}