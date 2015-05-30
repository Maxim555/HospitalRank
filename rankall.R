rankall <- function(outcomes,num){
  data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available" , stringsAsFactors=FALSE)
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (outcomes%in%valid_outcomes){
    
    if (outcomes == "heart attack") {
      outcomes = 11
    }
    if (outcomes == "heart failure") {
      outcomes = 17
    }
    if (outcomes == "pneumonia") {
      outcomes = 23
    }
    df <- data.frame
    df <- data[,c(2, 7, outcomes)] # get a data frame with all the states
    df <- na.omit(df)
    ordered_df <- df[with(df, order(State)),]
    name_vec <- c("Name","State","Outcome")
    name_vec2 <- c("hospital","state","Outcome")
    colnames(ordered_df) <- name_vec
    state_vector <- unique(ordered_df$State)
    ass3 <- data.frame(hospital=character(),state=character())
    ass <- data.frame(hospital=character(),state=character())
    ordered_df2 <- data.frame()
    print("OK1")
    #for (i in state_vector){
      if(num == "best"){
        for (i in state_vector){
        #print("OK2")
        state_subset <- subset(ordered_df,State == i) # get a dataframe for a particular state
        min_outcome <- min(state_subset['Outcome']) # get minimum outcome value
        hospital_data <- subset(state_subset,Outcome == min_outcome) # get a line from dataframe with min outcome value
              
        ass <- data.frame(hospital_data)
        ass3 <- rbind(ass3,ass)
        
      }
      ass3_ord <- ass3[with(ass3, order(State,Name)),]
      ass33 <- ass3_ord[!duplicated(ass3_ord['State']),]
      colnames(ass33) <- name_vec2
      ass33
      hospital_list <- subset(ass33,select = c("hospital","state"))
      }
      else if(num == "worst"){
        for (i in state_vector){
        state_subset <- subset(ordered_df,State == i)
        max_outcome <- max(state_subset['Outcome'])
        hospital_data <- subset(state_subset,Outcome == max_outcome)
        
        ass <- data.frame(hospital_data)
        ass3 <- rbind(ass3,ass)
      }
      ass3_ord <- ass3[with(ass3, order(State,Name)),]
      ass33 <- ass3_ord[!duplicated(ass3_ord['State']),]
      colnames(ass33) <- name_vec2
      ass33
      hospital_list <- subset(ass33,select = c("hospital","state"))
      }
      else{
        for (i in state_vector){
          state_subset <- subset(ordered_df,State == i)
          state_subset1 <- state_subset[with(state_subset, order(Outcome,Name)),]
        
        hospital_data <- state_subset1[num,]  # get hospital data with needed num
        
        ass <- data.frame(hospital_data)
        ass3 <- rbind(ass3,ass)
      }
      ass3_ord <- ass3[with(ass3, order(State,Name)),]
      ass33 <- ass3_ord[!duplicated(ass3_ord['State']),]
      colnames(ass33) <- name_vec2
      ass33
      hospital_list <- subset(ass33,select = c("hospital","state"))
      
      }
     
    }
}
    

