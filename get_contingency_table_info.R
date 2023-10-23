library(tidyverse)

#This file creates a long format matrix with contingency information about each word aggregated across documents (tweets, descriptions, etc.). 

#Creat a dataframe: 

# Putting data information together:
pop.dat <- data.frame(item1 = rep(NA,total.pairs),
                      item2 = rep(NA,total.pairs),
                      item1.id = rep(NA,total.pairs),
                      item2.id = rep(NA,total.pairs),
                      both=rep(NA,total.pairs),
                      only1=rep(NA,total.pairs),
                      only2=rep(NA,total.pairs),
                      neither=rep(NA,total.pairs))


curr.row <- 1
for(i in 1:(total.words-1)){
  word1 <- int.words[i]
  for(j in 2:total.words){
    word2 <- int.words[j]
    off.diagonal <- i != j

    # We only want non redundant items:
    if(j > i & off.diagonal){
      #  cat(". YES, i = ",i," j = ",j)
      # cat("\n", word1,
      #     " - ",word2)
    #  print(curr.row)
      pop.dat$item1[curr.row] <- word1
      pop.dat$item2[curr.row] <- word2

      pop.dat$item1.id[curr.row] <- i
      pop.dat$item2.id[curr.row] <- j

      rel.data <- word.info[,c(word1,word2)]

      pop.dat$only1[curr.row] <- sum(rel.data[,1] == 1 &
                                       rel.data[,2] == 0)

      pop.dat$only2[curr.row] <- sum(rel.data[,1] == 0 &
                                       rel.data[,2] == 1)

      pop.dat$both[curr.row] <- sum(rel.data[,1] == 1 &
                                      rel.data[,2] == 1)

      pop.dat$neither[curr.row] <- sum(rel.data[,1] == 0 &
                                         rel.data[,2] == 0)


      curr.row <- curr.row + 1
    }

  }
}

#Sanity check:
mean(rowSums(pop.dat[,c("both","neither","only1","only2")])) #should be 20711
sd(rowSums(pop.dat[,c("both","neither","only1","only2")])) #should be 0


#Continue adding to the contingency tables:
pop.dat$row1 <- pop.dat$both + pop.dat$only1
pop.dat$row2 <- pop.dat$only2 + pop.dat$neither

pop.dat$col1 <- pop.dat$both + pop.dat$only2
pop.dat$col2 <- pop.dat$only1 + pop.dat$neither

#Sanity check :
sum((pop.dat$row1+pop.dat$row2)==(pop.dat$col1+pop.dat$col2)) #should be total.pairs

pop.dat$phi.num <- ((pop.dat$both*pop.dat$neither)-(pop.dat$only1*pop.dat$only2))
pop.dat$row.denum <- sqrt(pop.dat$row1*pop.dat$row2)
pop.dat$col.denum <- sqrt(pop.dat$col1*pop.dat$col2)
pop.dat$phi <- pop.dat$phi.num / (pop.dat$row.denum*pop.dat$col.denum)

#Transforming into proportions:
prop.dat <- cbind(pop.dat[,c("item1",     "item2",     "item1.id",  "item2.id")],
                  pop.dat[,c("both",     "only1",     "only2",  "neither")]/total.youth,
                  pop.dat[,c("row1",     "row2")]/total.youth,
                  phi = pop.dat$phi)



