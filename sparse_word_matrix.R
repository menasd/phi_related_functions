# Words of interest:
int.words <- paste0("word",1:50)

total.words <-length(int.words)

#Total text units (observations, people, tweets, etc.)
total.indicents <- length(1:1000)
total.pairs <- total.words*(total.words-1)/2 #Expected word number given the number of words. 

