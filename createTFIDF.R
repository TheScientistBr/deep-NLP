library(dplyr)
library(tidytext)
library(ggplot2)
library(tm)
library(dtplyr)
library("data.table")

myDataset <- data.frame(stringsAsFactors = FALSE)
nRead <- Inf     # number files to read by class

deepNLP_Dataset <- read.csv(file = "data/Sheet_1.csv",header = TRUE,sep = ",",
                            stringsAsFactors = FALSE)
datasetTemp <- deepNLP_Dataset[,1:3]
ct <- 0         # counter to read files
for(ind in 1:length(datasetTemp$response_id)) {
        texto <- datasetTemp$response_text[ind]
        texto <- paste(texto,collapse = " ")
        texto <- gsub("<.*?>", "", texto)
        documents <- Corpus(VectorSource(texto))
        documents = tm_map(documents, tolower)
        documents = tm_map(documents, removePunctuation)
        texto  = tm_map(documents, removeNumbers)$content
        #texto <- tm_map(documents, removeWords,stopwords("en"))$content
        myDataset <- rbind(myDataset,c(datasetTemp$response_id[ind],datasetTemp$class[ind], 
                                       t(texto)),deparse.level = 0, stringsAsFactors =  FALSE)

}

myDataset <- data.frame(lapply(myDataset,as.character), stringsAsFactors = FALSE)
colnames(myDataset) <- c("file","class","text")
book_words <- myDataset %>%
        unnest_tokens(word, text,to_lower = TRUE) %>%
        count(file,word, sort = TRUE) %>%
        ungroup()

total_words <- book_words %>% group_by(file) %>% summarize(total = sum(n))
book_words <- left_join(book_words, total_words)
head(book_words)

# Create matrix with TF-IDF
book_words <- book_words %>% bind_tf_idf(word, file, n)
book_words <- as.data.table(book_words)

book_words$class <- myDataset[match(book_words$file,myDataset$file),"class"]

setkey(book_words,file,word,class)

write.csv(book_words,file = "data/deepNLP.csv")

