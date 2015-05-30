best <- function(state,outcomes){
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
st.sub <- na.omit(st.sub)
if (state%in%st.sub$State){
min_outcome <- min(st.sub['Outcome'])
hospital_data <- subset(st.sub,Outcome == min_outcome)
resu <- as.character(character_vector <- c(hospital_data['Name']))
}
else{
  stop("invalid state")
}
}
else{
  stop("invalid outcome")
}
}
