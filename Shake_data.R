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
