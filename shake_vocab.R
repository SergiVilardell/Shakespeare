library(tidytext)
library(tidyverse)

shakespeare <-read.csv("shakespeare.csv", stringsAsFactors = F)

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



