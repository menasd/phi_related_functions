library(tidyverse)

# Words of interest:
int.words <- paste0("word",1:50)
incidents <- 1:1000 #ID for each text unique, in this case participants' descriptions of an incident.

total.words <-length(int.words)

#Total text units (observations, people, tweets, etc.)
total.indicents <- length(incidents)
total.pairs <- total.words*(total.words-1)/2 #Expected word number given the number of words. 

#Getting individual totals for each word first:
# Word info is a sparse matrix with columns corresponding to each word
# and values of zeros and ones, depending on whether that word is present
# for that corresponding incident.
word.info <- data.frame(incidents = incidents)

for(curr.word in int.words){
  curr.data <- clean.firsts %>% group_by(studyid) %>%
    summarise(present = as.numeric(curr.word %in% description))
  colnames(curr.data)[2] <- curr.word

  word.info <- cbind(word.info,
                     curr.data[,2])
}


