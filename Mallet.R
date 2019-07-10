library(dplyr)
library(mallet)
library(stopwords)
library(corpus)
library(rJava)

sw<-c(data_stopwords_stopwordsiso$es, data_stopwords_stopwordsiso$en)
write.table(sw, file = "sw.txt", sep = "\t",
            row.names = FALSE, quote = FALSE)

noticias.id.df <- data.frame(rownames(noticias.df), noticias.df$articulo)
colnames(noticias.id.df) <- c("id", "text")
noticias.id.df$id <- as.character(noticias.id.df$id)
noticias.id.df$text <- as.character(noticias.id.df$text)


mallet.instances <- mallet.import(noticias.id.df$id, noticias.id.df$text, "sw.txt")


topic.model <- MalletLDA(num.topics = 37)
topic.model$loadDocuments(mallet.instances)

vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)

topic.model$setAlphaOptimization(20, 50)
topic.model$train(10000)

topic.model$maximize(20)

doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)

mallet.top.words(topic.model, topic.words[7,])

topics <- vector()
for (i in 1:37){
  topics[i] <- mallet.top.words(topic.model, topic.words[i,], num.top.words = 15)
    }

temas <- vector()
temporal <- vector()
for (i in 1:37){
  temporal <- unlist(topics[i])
  temas[i] <- paste(temporal, collapse = ", ", sep = ", ")
}

write.table(temas, "temasMALLET cuarto intento.txt", quote = FALSE)


temas.pesos <- list()
for (i in 1:37) {
  temas.pesos[[i]] <- mallet.top.words(topic.model, topic.words[i,], num.top.words = 138774)
}

## Necesito acceder a la segunda columna de cada uno de los data frames 
## guardados en la lista


