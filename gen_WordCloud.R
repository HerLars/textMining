# Librerias
library(readr)
library(tidyverse)
library(NLP)
library(wordcloud)

# tm: Principal para el analisis de texo
library(tm)

# slam: Para que no se el error de memoria  "Error: cannot allocate vector of size 182.7" Gb con los muchos tweets haciendo m <- as.matrix(dtm)
library(slam)

# archivo
# Para lectura de parametros de linea de comandos. Archivo a procesar.
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("Debe especificar un archivo.n", call.=FALSE)
} else if (length(args)==1) {
  archivo = args[1]
}
#archivo="cntmU_FHo_gz.csv"
fileTweets <- read_csv(archivo, locale = locale())

# Selecciono solo los tweets
fileTweets <- fileTweets[fileTweets$Tipo == "TWEET",]

mydata <- Corpus(VectorSource(fileTweets$Text))

# ##########################
# Cleaning
# ##########################
# convert to lower case
print("Limpiado datos.")
mydata <- tm_map(mydata, content_transformer(tolower))

# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
mydata <- tm_map(mydata, content_transformer(removeURL) )

# remove ?????????????????? what would be emojis
mydata<-tm_map(mydata, content_transformer(gsub), pattern="\\W",replace=" ")

# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
mydata <- tm_map(mydata, content_transformer(removeNumPunct))

# Para tweets del barcelona
cambiaBarca <- function(x) gsub("barca", "barça", x)
mydata <- tm_map(mydata, content_transformer(cambiaBarca))

# Seleccion de stopwords (español, ingles)
miListaStopWords <- iconv(stopwords("spanish"), 'UTF-8', 'latin1', 'byte')
miListaStopWords <- c(miListaStopWords, iconv(stopwords("english"), 'UTF-8', 'latin1', 'byte') )

# Archivo de palabras en catalan
aFile <- read.csv("stopWords_catalan.csv", header=FALSE)
miListaStopWords <- c(as.character(aFile$V1), miListaStopWords)

# Archivo con palabras personalizadas
aFile <- read.csv("lista_stopWords.csv", header=FALSE)
miListaStopWords <- c(as.character(aFile$V1), miListaStopWords)

# remove stopwords
print("Quitando stopwords.")
mydata <- tm_map(mydata, removeWords, miListaStopWords)

# remove extra whitespace
print("Quitando espacios extra.")
mydata <- tm_map(mydata, stripWhitespace)

# Remove numbers
print("Quitando numeros.")
mydata <- tm_map(mydata, removeNumbers)

# keep a copy for stem completion
mydataCopy <- mydata


# ####################################
# Analisis, generacion de nube
# ####################################

# build a term document matrix. The TDM is often the matrix used for language analysis.
tdm <- TermDocumentMatrix(mydata)

# Comentar linea de abajo para usar el nuevo sort
#       as.matrix no sirve para grandes volumenes de informacion. (Se comenta la linea)
# m <- as.matrix(tdm)

# row_sums de slam
# v <- sort(rowSums(m),decreasing=TRUE)
v <- sort(row_sums(tdm),decreasing=TRUE)

d <- data.frame(word = names(v),freq=v)
head(d, 10)

# ####################################
# Draw the word cloud
# ####################################

set.seed(142)
pdf("WordCloud_Dendrogram_BarPlot.pdf")

#wordcloud(words = d$word, freq = d$freq, min.freq = 10,max.words=50, random.order=FALSE, scale = c(4, 0.5), colors = brewer.pal(6, "Dark2"))
wordcloud(words = d$word, freq = d$freq, max.words=70, random.order=FALSE, colors = brewer.pal(6, "Dark2"))

# ####################################
# Plot a dendrogram. Cluster
# ####################################
tdms <- removeSparseTerms(tdm, sparse = 0.98)
hc <- hclust(d = dist(tdms, method = "euclidean"), method = "complete")
plot(hc)

# ####################################
# Barplot con las palabras mas usadas
# ####################################
barplot(v[2:30], col = "tan", las = 2)

dev.off()

# ####################################
# Limpio variables.
# ####################################
rm(args, tdm, v, d, mydataCopy, fileTweets, mydata, removeURL, removeNumPunct,miListaStopWords, aFile, hc, tdms)
rm(archivo, cambiaBarca)
