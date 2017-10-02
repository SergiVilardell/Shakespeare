library(tidytext)
library(tidyverse)

#Read txt file with complete shakespeare works

TEXTFILE <-  "pg100.txt"
if (!file.exists(TEXTFILE)) {
       download.file("http://www.gutenberg.org/cache/epub/100/pg100.txt", destfile = TEXTFILE)
}

shakespeare = readLines(TEXTFILE,encoding = "UTF-8")

#Separate by book and year

shakespeare <-  shakespeare[-(1:173)]
shakespeare <- shakespeare[-(124195:length(shakespeare))]
shakespeare <-  paste(shakespeare, collapse = " ")
nchar(shakespeare)
shakespeare <- gsub("<<[^>]*>>", "", shakespeare)
shakespeare <- gsub("\"", "", shakespeare)
shakespeare <-  strsplit(shakespeare, "THE END")[[1]]
year <- regmatches(shakespeare, regexpr('[0-9]+',perl = T, shakespeare))
title <- regmatches(shakespeare, regexpr("[\\d].*(?=by William)",perl = T, shakespeare))
title <- gsub("[0-9]+", "", title)
shakespeare <-  tolower(shakespeare)
length(shakespeare)

shakespeare <- as.data.frame(shakespeare, stringsAsFactors = FALSE)
colnames(shakespeare) <-  "text"
shakespeare$text <-  as.character((shakespeare$text))
shakespeare$year <-  as.numeric(year)
shakespeare$title <-  as.character(title)
shakespeare <- shakespeare %>% 
  arrange(year)

write.csv(shakespeare, file = "shakespeare.csv", row.names = F)


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


