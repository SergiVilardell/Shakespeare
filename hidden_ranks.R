library(tidytext)
library(tidyverse)
library(pracma)

shakespeare <-read.csv("shakespeare.csv", stringsAsFactors = F)

#Count words in a book
w <-  shakespeare[1,] %>% 
  unnest_tokens(word, text) %>% 
  count(word) %>% 
  arrange(desc(n))

#Compute the probabilities p
N <-  dim(w)[1]
w <- w %>% 
  mutate(p = n/N)

sum <-  0
p <- w[["p"]]
for( i in 1:N){
  sum = sum + p[i]*log(p[i])
}


