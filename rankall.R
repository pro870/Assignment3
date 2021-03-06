
rankall<- function(outcome,num = "best"){
        list.outcomes<- c("heart attack", "heart failure", "pneumonia")
        outcome.data<-read.csv("outcome-of-care-measures.csv"
                               , colClasses="character")
        hospital.list<-data.frame()

        if (outcome %in% list.outcomes == "FALSE") {
                stop("invalid outcome")
        }
         
        if (outcome == "heart attack") {
                x<- 11 
                outcome.data[,x]<-as.numeric(outcome.data[,x])
        } else if (outcome == "heart failure") {
                 x<- 17 
                 outcome.data[,x]<-as.numeric(outcome.data[,x])   
               } else {
                 x<- 23
                 outcome.data[,x]<-as.numeric(outcome.data[,x])   
        }
        
        data<-split(outcome.data,outcome.data$State)
        
        if (num == "best") {
                for (i in 1:54){
                        a<-data[[i]]
                        y<-which.min(a[,x])
                        b<-a[y,2]
                        c<-cbind(b,a[y,7])
                        hospital.list<-rbind(hospital.list,c)
                }
                        
        } else if ( num == "worst") {
                for (i in 1:54){
                        a<-data[[i]]
                        y<-which.max(a[,x])
                        b<-a[y,2]
                        c<-cbind(b,a[y,7])
                        hospital.list<-rbind(hospital.list,c)
                }
                
        } else {
                for (i in 1:length(data)){
                        a<-data[[i]]
                             a<-a[order(a[,7],a[,x],a[,2]),]
                             c<-cbind(a[num,2],a[num,7])
                         
                        hospital.list<-rbind(hospital.list,c)
                }
        }
    
        colnames(hospital.list)<-c("hospital","state")
      hospital.list      
}


