rankhospital <- function(state,outcomes,num){
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
    df <- data[,c(2, 7, outcomes)]
    
    st.sub <- subset(df,State == state)
    stvec <- c("Name","State","Outcome")
    colnames(st.sub) <- stvec
    stsub <- na.omit(st.sub)
    if (state%in%stsub$State){ # calculating the number
      if(num == "best"){
        min_outcome <- min(stsub['Outcome'])
        hospital_data <- subset(stsub,Outcome == min_outcome)
        resu <- as.character(character_vector <- c(hospital_data['Name']))
      }
      else if(num == "worst"){
        max_outcome <- max(stsub['Outcome'])
        hospital_data <- subset(stsub,Outcome == max_outcome)
        resu <- as.character(character_vector <- c(hospital_data['Name']))
      }
      else{
      ordered_df <- stsub[with(stsub, order(Outcome,Name)),]# get ordered on outcome dataframe      
      hospital_data <- ordered_df[num,]  # get hospital data with needed num
      hospital_data <- as.character(character_vector <- c(hospital_data['Name'])) #get the hospital name
      }
      
    }
    else{
      stop("invalid state")
    }
  }
  else{
    stop("invalid outcome")
  }
}
