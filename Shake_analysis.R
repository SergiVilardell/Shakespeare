library(tidytext)
library(tidyverse)

shakespeare <-read.csv("shakespeare.csv", stringsAsFactors = F)

#List of words. One per row
shake.total.words <- shakespeare%>% 
  unnest_tokens(word, text) %>% 
  group_by(title) %>% 
  count(title)

shake.unique.words <- shakespeare%>% 
  unnest_tokens(word, text) %>% 
  group_by(year) %>% 
  count(word) %>% 
  count(year) 


years <-  as.numeric(shake.unique.words$year)
unique.words <- list()

for( i in 1:length(years)){
  
  shake.unique.words <- shakespeare%>% 
    unnest_tokens(word, text) %>% 
    filter(year <= years[i]) %>% 
    count(word)
  
  unique.words[i] <- dim(shake.unique.words)[1]
  
}

shakespeare.words <- data.frame(matrix(unlist(unique.words), nrow=20, byrow=T))
colnames(shakespeare.words) = "u.words"
shakespeare.words$year <- as.numeric(years)

plot(shakespeare.words$year, shakespeare.words$u.word, 
     xlab = "year", ylab = "Unique words", 
     main = "Shakespeare Vocabulary")

plot(shake.unique.words$year, shake.unique.words$nn, 
     xlab = "year", ylab = "Unique words", 
     main = "Shakespeare Vocabulary")

# By title
unique.words <- list()
total.words <- list()

for( i in 1:38){
  
  shake.total.words <- shakespeare[1:i,]%>% 
    unnest_tokens(word, text)
  
  shake.unique.words <- shakespeare[1:i,]%>% 
    unnest_tokens(word, text) %>% 
    count(word)
  
  unique.words[i]= dim(shake.unique.words)[1]
  
  total.words[i]= dim(shake.total.words)[1]
} 

shakespeare$N.t <- unlist(total.words)
shakespeare$V.t <- unlist(unique.words)

plot(total.words, unique.words, xlab = "N", ylab = "V")

ggplot(shakespeare)+
  geom_point(aes(x = N.t, y = V.t, colour = as.factor(year)), size = 2.5)



# Analyze each title 

a <-  shakespeare[1,] %>% 
  unnest_tokens(word, text)

words <- a[,"word"]
vocab <- c()
V <- list()

for(i in 1:length(words)){
  if(!(words[i] %in% vocab )){
    vocab <- append(vocab, words[i])
   
  }
  V[i] = length(vocab)
}

plot(unlist(V))








write.csv(shakespeare, file = "shakespeare.csv", row.names = F)
