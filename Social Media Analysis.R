install.packages("rvest")
install.packages("purrr")
install.packages("dplyr")
library(rvest)
library(purrr)
library(dplyr)

glassdoor_base <- "https://www.glassdoor.co.in/Reviews/Tesla-Reviews-E43129_P%d.htm"
css_glassdoor_base <-".mt-md:nth-child(6) > p:nth-child(2)"

map_dfr(1:10, function(i){
  page <- read_html(sprintf(glassdoor_base,i))
  data.frame(Pros = html_text(html_nodes(page, css_glassdoor_base)))
  })-> TeslaPros
View(TeslaPros)
?read_html
?sprintf
?list
write.table(TeslaPros,"TeslaPros.txt", sep='\n',row.names = FALSE, col.names = FALSE)
library(syuzhet)
# Get data from file
text <- readLines("TeslaPros.txt")
text

# Fetch sentiment words from texts 
Sentiment <- get_nrc_sentiment(text)
View(Sentiment)

# Count the sentiment words from texts
TotalSenti <- data.frame(colSums(Sentiment[,c(1:10)]))
View(TotalSenti)

# To name the column as "Count"
names(TotalSenti)
names(TotalSenti) <- "count"
View(TotalSenti)

# cbind is used for adding more columns 
# To add a column named as "sentiment" for bar graph 
TotalSenti <- cbind("sentiment" = rownames(TotalSenti), TotalSenti)
View(TotalSenti)
rownames(TotalSenti) <- NULL

install.packages("ggplot2")
library(ggplot2)

# Total sentiment score 
ggplot(data = TotalSenti,aes(x = sentiment , y = count))+ 
  geom_col(aes(fill= sentiment))+ geom_text(aes(label= count))+ 
  xlab("Sentiment") + ggtitle("Total Sentiment Score")

library(tm)
library(SnowballC)

#get the data from text file
text <- readLines("TeslaPros.txt")
text
#let us create the corpus (corpus takes input in the vector form)
docs <- Corpus(VectorSource(text))
?readLines
?Corpus

#inspect documents of corpus
inspect(docs)
?inspect

#creating content transformer function for global substitution
trans <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

#Clean our data
docs <- tm_map(docs, trans, "@")
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("en"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)

inspect(docs)
?TermDocumentMatrix
#create  the document term matrix
dtm <- TermDocumentMatrix(docs)
inspect(dtm)
mat <- as.matrix(dtm)
View(mat)
v <-sort(rowSums(mat),decreasing = TRUE)
View(v)
data <- data.frame(word =names(v),freq =v)
View(data)

install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)

wordcloud(words =data$word, freq=data$freq, min.freq =2,max.words=500, scale = c(2,0.2), random.order= FALSE, rot.per=0.50, colors =brewer.pal(8, "Dark2") )
?wordcloud


