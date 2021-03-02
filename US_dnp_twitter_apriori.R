#Install and loading the required packagesR
#  Install Requried Packages
install.packages("SnowballC")
install.packages("tm")
install.packages("twitteR")
install.packages("syuzhet")
install.packages("rlang")
install.packages("tibble")
install.packages("lattice")
install.packages("plyr")
install.packages("stringr")
install.packages("ggmap")
install.packages("wordcloud")
install.packages("ROAuth")
install.packages("ggplot2")
install.packages('arules')
install.packages('arulesViz')

# Load Requried Packages
library("SnowballC")
library("tm")
library("twitteR")
library("syuzhet")
library("rlang")
library("tibble")
library("ggplot2")
library("lattice")
library("plyr")
library("stringr")
library("ggmap")
library("wordcloud")
library("dplyr")
library("ROAuth")
library("stringr")
library('arules')
library('data.table')
library('arulesViz')

install.packages("sos")
library("sos")
findFn("laply")


#AUTHENTICATION ON TWITTER
#Invoke Twitter API using the app we have created and using the 
#keys and access tokens we got through the app.
consumer_key <- 'oQdLvfbAoTOtCgtlxsULvljR5'
consumer_secret <- 'GGPfjDVDM6UcNqF0DEnLwqA7iSwiU7cDCWsaygwCWCeacny6X1'
access_token <- '15269885-D1y1KmdiPtEOEa1pr6wY9jmUY3uKoTieky5x7gOeP'
access_secret <- 'u0dFPoiX6T2Z6NJzYjVoyLlj7z12woi04NO3yBHGQsPXL'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#list of the twitter accounts of the top10 newspaper of USA
list_of_ta<-c("nytimes","USATODAY","WSJ","washingtonpost","Newsday","latimes","StarTribune","chicagotribune", "BostonGlobe", "nypostbiz")
list_of_ta
length(list_of_ta)
list_of_ta[1]

seq <- c(1:length(list_of_ta))
seq
i=1
combined_tl <- data.frame()

for (i in seq) {
  tweets<-twListToDF(userTimeline(list_of_ta[i], n=1000))
  combined_tl <- rbind(combined_tl, tweets)
}
print( tail(combined_tl))
head(combined_tl$text)
tail(combined_tl$text)

n.combined_tl<- length(combined_tl)
n.combined_tl

original_combined_tl<-combined_tl
typeof(original_combined_tl)
original_combined_tl
write.csv(original_combined_tl,"c:\\Users\\malejin\\Documents\\Rworkinglibrary\\_beadando_nagyvallalati_adatelemzes\\original_combined_tl.csv", row.names = FALSE)


#removing unwanted characters
combined_tl <- gsub("http.*","",combined_tl$text)
combined_tl <- gsub("https.*","",combined_tl)
combined_tl <- gsub("#.*","",combined_tl)
combined_tl <- gsub("@.*","",combined_tl)
head(combined_tl)

#SIMPLE SCORING
word.df <- as.vector(combined_tl)
head(word.df)
length(word.df)
#get_nrc_sentiment(word.df, cl = NULL, language = "english")
#??get_nrc_sentiment
emotion.df <- get_nrc_sentiment(word.df)
emotion.df
emotion.df2 <- cbind(combined_tl, emotion.df) 
head(emotion.df2)
sent.value <- get_sentiment(word.df)
most.positive <- word.df[sent.value == max(sent.value)]
most.positive
most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative 
sent.value

#Segregating positive and negative tweets
positive.combined_tl <- word.df[sent.value > 0]
head(positive.combined_tl)
length(positive.combined_tl)
negative.combined_tl <- word.df[sent.value < 0]
head(negative.combined_tl)
length(negative.combined_tl)
neutral.combined_tl <- word.df[sent.value == 0]
head(neutral.combined_tl)
length(neutral.combined_tl)

#alternative way to categorize pos, neg, neutral 
category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
head(category_senti)
length(category_senti)

category_senti2 <- cbind(combined_tl,category_senti)
category_senti2
table(category_senti)
print(category_senti)

#segregation grouped by screenName/newspaper account
category_senti4<-data.frame(original_combined_tl$screenName, category_senti)
category_senti4
sum_category_senti_by_npa<-table(category_senti4)
sum_category_senti_by_npa
summary(sum_category_senti_by_npa)

df_sum_category_senti_by_npa<-data.frame(sum_category_senti_by_npa)
df_sum_category_senti_by_npa
write.csv(df_sum_category_senti_by_npa,"c:\\Users\\malejin\\Documents\\Rworkinglibrary\\_beadando_nagyvallalati_adatelemzes\\phillybikes_sentiment_summary.csv", row.names = FALSE)

summary(df_sum_category_senti_by_npa)
summary(df_sum_category_senti_by_npa[df_sum_category_senti_by_npa$category_senti=="Negative",])
summary(df_sum_category_senti_by_npa[df_sum_category_senti_by_npa$category_senti=="Positive",])
summary(df_sum_category_senti_by_npa[df_sum_category_senti_by_npa$category_senti=="Neutral",])


#visualization of results
#boxplot[]
cols = c("#7CAE00", "#00BFC4", "#F8766D", "#C77CFF")
names(cols) = c("negative", "neutral", "positive")

ggplot(df_sum_category_senti_by_npa, aes(x=category_senti, y=Freq, group=category_senti)) +
  geom_boxplot(aes(fill=category_senti)) +
  scale_fill_manual(values=cols) +
  geom_jitter(colour="gray40",
              position=position_jitter(width=0.2), alpha=0.3)+
  ggtitle("Boxplot of catgeory_senti") 


#Grouped barchart with lattice
library(lattice)

colors = c("lightsalmon3", "lightgoldenrod2", "pink")

df_sum_category_senti_by_npa

barchart(
  data = df_sum_category_senti_by_npa,
  origin = 0,
  Freq ~ original_combined_tl.screenName,
  groups = category_senti,
  xlab = list (
    label = "Twitter accounts of top10 US newspapers",
    font = 2,
    cex = 1),
  ylab= list (
    label = "Number of tweets",
    font = 2,
    cex = 1),
  ylim=c(0,500),
   scales=list(x=list(rot=90) ),
  labels = TRUE,
   auto.key = list(space="top", columns= 3),
  par.settings = list(superpose.polygon = list(col = colors)))   

###################
#DETAILED SCORING 
# function score.sentiment
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}


# import positive and negative words
getwd()
setwd("C:\\Users\\malejin\\Documents\\Rworkinglibrary\\_beadando_HIT\\twitter")
pos = readLines("positive-words.txt")
neg = readLines("negative-words.txt")

# apply function score.sentiment
scores <- score.sentiment(original_combined_tl$text, pos, neg, .progress='text')
head(scores)
tail(scores)
scores
saving_scores_with_original_combined_tl<-original_combined_tl
saving_scores_with_original_combined_tl$scores<-scores$score
colnames(saving_scores_with_original_combined_tl)
length(saving_scores_with_original_combined_tl)
write.csv(saving_scores_with_original_combined_tl,"c:\\Users\\malejin\\Documents\\Rworkinglibrary\\_beadando_nagyvallalati_adatelemzes\\saving_scores_with_original_combined_tl.csv", row.names = FALSE)

min(scores$score)
max(scores$score)

head(original_combined_tl)

# add variables to data frame
scores$npa<-original_combined_tl$screenName
scores$very.pos = as.numeric(scores$score >= 2)
scores$very.neg = as.numeric(scores$score <= -2)
tail(scores)


# how many very positives and very negatives
numpos = sum(scores$very.pos)
numpos
numneg = sum(scores$very.neg)
numneg

# global score
global_score = round( 100 * numpos / (numpos + numneg) )
global_score

unique(scores$npa)
scores$score
############TO CORRECT AND USE LATTICE
# barplot of average score
meanscore = tapply(scores$score, scores$npa, mean)
head(meanscore)
df = data.frame(newspaper_account=names(meanscore), meanscore=meanscore)
df$newspaper_account <- reorder(df$newspaper_account, df$meanscore)
df$newspaper_account
df$meanscore
head(df)

ggplot(data=df, aes(x=newspaper_account, y=meanscore, fill=newspaper_account)) +
  geom_bar(stat="identity", color="black")+theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("AVG scores of tweets")+
  xlab("Twitter accounts ")+
  ylab("Mean scores of tweets")


# barplot of average very positive
npa_pos = ddply(scores, .(npa), summarise, mean_pos=mean(very.pos))
npa_pos$npa <- reorder(npa_pos$npa, npa_pos$mean_pos)
npa_pos$npa
npa_pos$mean_pos
head(npa_pos)

ggplot(data=npa_pos, aes(x=npa, y=mean_pos, fill=npa)) +
  geom_bar(stat="identity", color="black")+theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("AVG of very positive")+
  xlab("Newspapers' accounts ")+
  ylab("Mean of very positive tweets")


# barplot of average very negative
npa_neg = ddply(scores, .(npa), summarise, mean_neg=mean(very.neg))
npa_neg$npas <- reorder(npa_neg$npa, npa_neg$mean_neg)
head(npa_neg$npas)
head(npa_neg)

ggplot(data=npa_neg, aes(x=npa, y=mean_neg, fill=npa)) +
  geom_bar(stat="identity", color="black")+theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("AVG of very negative")+
  xlab("Newspapers' accounts ")+
  ylab("Mean of very negative tweets")

#MOST FREQ WORDS AND WORDCLOUD ANALISYS
#create corpus
myCorpus <- Corpus(VectorSource(original_combined_tl$text))
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpusCopy <- myCorpus
myCorpusCopy
myCorpus <- tm_map(myCorpus, stemDocument)
myCorpus <- Corpus(VectorSource(myCorpus))
head(myCorpus)

#cleaning the corpus
corpus_tosave<-myCorpus[[1]]$content
corpus_tosave
corpus_tosave<-gsub("^c\\(|\\)$", "", corpus_tosave)
corpus_tosave
corpus_tosave<-gsub(pattern = "<.*>", replacement = "", x = corpus_tosave)
corpus_tosave

words <- strsplit(corpus_tosave, ",")[[1]]
words<-c(words)
words

words<-str_replace_all(words, "[[:punct:]]", " ")
words

words<-str_trim(words, "left") 
words<-str_trim(words, "right") 
words

words<-gsub("\n", "", words, fixed = TRUE)
words

words[words==""]<-NA
words

#saving the corpus
saving_scores_with_original_combined_tl$corpus<-words
typeof(saving_scores_with_original_combined_tl$corpus)
write.csv(saving_scores_with_original_combined_tl,"c:\\Users\\malejin\\Documents\\Rworkinglibrary\\_beadando_nagyvallalati_adatelemzes\\phillybikes_twitter_data.csv", row.names = FALSE)
typeof(saving_scores_with_original_combined_tl)
length(saving_scores_with_original_combined_tl)

#create term document matrix
wordFreq <- function(corpus, word) {
  results <- lapply(corpus,
                    function(x) { grep(as.character(x), pattern=paste0("\\<",word)) }
  )
  sum(unlist(results))
}
tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
tdm

#find the most frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq = 100))
freq.terms

#removing stopwords
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),"and", "when", "what", "to", "this","the","that","so","of","it","is","in","at","a","be","by","for","have","on","our","are","i","will","with","you", "the","via", "like", "from", '"",','",','"')
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus[[1]]$content

#most frequent words without stopwords
tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
tdm
(freq.terms <- findFreqTerms(tdm, lowfreq = 4000))
freq.terms 

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 5)
df2 <- data.frame(term = names(term.freq), freq = term.freq)
df2
df2 <- df2[-1, ]
df2 <- df2[-1, ]
df2

ggplot(df2, aes(x=term, y=freq, fill=term)) + geom_bar(stat="identity")+theme_minimal()+
  xlab("Terms") + ylab("Count") + coord_flip() +theme(axis.text=element_text(size=7))+
  ggtitle("Most frequent words")+
  xlab("Terms")+
  ylab("Frquency")

#create word cloud
wordcloud(myCorpus ,max.words =100,min.freq=3,scale=c(4,.5),colors=palette())


#most frequent words without stopwords
tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
tdm
(freq.terms <- findFreqTerms(tdm, lowfreq = 5))

#plot most frequent words
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 5)
df2 <- data.frame(term = names(term.freq), freq = term.freq)
df2
df2 <- df2[-1, ]
df2 <- df2[-1, ]
df2$term
df2$term[1]
df2$term<-gsub("[^[:alnum:] ]", "", df2$term)
df2

ggplot(df2, aes(x=term, y=freq, fill=term)) + geom_bar(stat="identity")+theme_minimal()+
  xlab("Terms") + ylab("Count") + coord_flip() +theme(axis.text=element_text(size=7))+
  ggtitle("Most frequent words")+
  xlab("Terms")+
  ylab("Frquency")

#check saving_scores_with_original_combined_tl$corpus which most freq words contains
write.csv(df2,"c:\\Users\\malejin\\Documents\\Rworkinglibrary\\_beadando_nagyvallalati_adatelemzes\\phillybikes_twitter_most_frequent_words.csv", row.names = FALSE)
write.csv(myCorpus[[1]]$content,"c:\\Users\\malejin\\Documents\\Rworkinglibrary\\_beadando_nagyvallalati_adatelemzes\\phillybikes_twitter_corpus.csv", row.names = FALSE)


nrow(df2)
df2$term[1]
df2

#check which words of the corpus is in the most freq wordslist and take the 1st element
mf1<-list()
mf2<-list()
for (row in 1:nrow(saving_scores_with_original_combined_tl)) {
  corpus_temp <- saving_scores_with_original_combined_tl[row, "corpus"]
  #print(typeof(corpus_temp))
  corpus_temp_array<-c(strsplit(corpus_temp, "\\s+")[[1]])
  #print(corpus_temp_array)
  #print(typeof(corpus_temp_array))
  fl1<-list()
  for ( i in corpus_temp_array){ 
    #print(i)
    if(!is.na(i)){
      #print(i)
      fl2<-list()
      for (row_ in 1:nrow(df2)){
        #print(df2$term[row_])
        if (i==df2$term[row_]){
          #print(i)
          fl2<-c(fl2,i)
          #print(fl2)
        }
      }
      fl1<-c(fl1,fl2)
    }
  }
  print(length(fl1))
  mf2<-c(mf2,fl1[1])
}
length(mf2)
length(saving_scores_with_original_combined_tl)
saving_scores_with_original_combined_tl$mf2<-mf2
colnames(saving_scores_with_original_combined_tl)
saving_scores_with_original_combined_tl$mf2

saving_scores_with_original_combined_tl$mf2 <- vapply(saving_scores_with_original_combined_tl$mf2, paste, collapse = ", ", character(1L))
saving_scores_with_original_combined_tl$mf2

write.csv(saving_scores_with_original_combined_tl,"c:\\Users\\malejin\\Documents\\Rworkinglibrary\\_beadando_nagyvallalati_adatelemzes\\phillybikes_twitter_most_frequent_words_per_tweet.csv", row.names = FALSE)

nrow(saving_scores_with_original_combined_tl)
#check whcih word of the corpus is in the most freq words list and take the one with highest frequency
#mf2<- data.frame(term=character(), frequency=integer())
mf2_t<- data.frame(term=character(), frequency=integer())
for (row in 1:nrow(saving_scores_with_original_combined_tl)) {
  corpus_temp <- saving_scores_with_original_combined_tl[row, "corpus"]
  #print(typeof(corpus_temp))
  corpus_temp_array<-c(strsplit(corpus_temp, "\\s+")[[1]])
  corpus_temp_array<-unique(corpus_temp_array)
    #print(corpus_temp_array)
  #print(length(corpus_temp_array))
  #print(typeof(corpus_temp_array))
  fl1<- data.frame(term=character(), frequency=integer())
  for ( i in corpus_temp_array){ 
    #print(i)
    fl2df <- data.frame(corpus_temp=character(),term=character(), frequency=integer())
    if(!is.na(i)){
      #print(i)
      #print(colnames(fl2df))
      for (row_ in 1:nrow(df2)){
        #print(df2$term[row_])
        if (i==df2$term[row_]){
          #print(i)
          fl2df <- rbind(fl2df, data.frame(corpus_temp=saving_scores_with_original_combined_tl[row, "corpus"],term = i, frequency=row_))
          #fl2df<-fl2df[0:1,]
          #print(fl2df)
        }
      }
      fl1<-rbind(fl1, data.frame(term=fl2df$term, frequency =fl2df$frequency))
      #print(fl1)
      mf2_t<-rbind(mf2, data.frame(term =fl1$term, frequency =fl1$frequency))
    } else{
      fl1<-rbind(fl1, data.frame(term=i, frequency=0))
      mf2_t<-rbind(mf2, data.frame(term =fl1$term, frequency =fl1$frequency))
    }
  }
  fl1_max<-subset(fl1, frequency==max(frequency))
  #print(fl1_max)
  mf2<-rbind(mf2, data.frame(term =fl1_max$term, frequency =fl1_max$frequency))
}

print(nrow(mf2_t))
print(nrow(mf2))

print(nrow(saving_scores_with_original_combined_tl))
saving_scores_with_original_combined_tl$mf2_term<-mf2$term
saving_scores_with_original_combined_tl$mf2_freq<-mf2$frequency
colnames(saving_scores_with_original_combined_tl)
saving_scores_with_original_combined_tl

#saving_scores_with_original_combined_tl$mf2_term <- vapply(saving_scores_with_original_combined_tl$mf2_term, paste, collapse = ", ", character(1L))
#saving_scores_with_original_combined_tl$mf2_term
#saving_scores_with_original_combined_tl$mf2_freq <- vapply(saving_scores_with_original_combined_tl$mf2_freq, paste, collapse = ", ", character(1L))
#saving_scores_with_original_combined_tl$mf2_freq

write.csv(saving_scores_with_original_combined_tl,"c:\\Users\\malejin\\Documents\\Rworkinglibrary\\_beadando_nagyvallalati_adatelemzes\\phillybikes_twitter_most_frequent_words_per_tweet_dtls.csv", row.names = FALSE)

#run apriori to see most freq word combinations
saving_scores_with_original_combined_tl$corpus_split<-strsplit(saving_scores_with_original_combined_tl$corpus, " ")
saving_scores_with_original_combined_tl$corpus_split

df_temp_list<- list()
for (row in 1:nrow(saving_scores_with_original_combined_tl)) {
  df_temp<-transpose(tibble(unlist(saving_scores_with_original_combined_tl$corpus_split[row])))
  df_temp_list[[row]]<-df_temp
}
df_temp_list

df_temp_list_merged<-bind_rows(df_temp_list, .id = "column_label")
df_temp_list_merged
df_temp_list_merged<-df_temp_list_merged %>%   select(-1)
df_temp_list_merged

df_temp_list_merged_t <- as(df_temp_list_merged, "transactions")
df_temp_list_merged_t

# Training Apriori on the dataset
rules = apriori(data = df_temp_list_merged_t, parameter = list(support = 0.04, confidence = 0.2))

#Check Apriori rules
detach(package:tm, unload=TRUE)#The rason of this error is that both arules and tm library has inspect() method and so the order in which they are loaded affects how this method is implemented.
inspect(head(rules, n = 20, by ="lift"))


# Visualising the results
inspect(sort(rules, by = 'lift')[1:10])
plot(rules, measure = c("support", "lift"), shading = "confidence", jitter=0)
plot(rules, method = "two-key plot", jitter=0)
plot(rules, method = "grouped", control = list(k = 50))
plot(rules, method = "paracoord")

#Visualesing results based on confidence
subrules <- rules[quality(rules)$confidence > 0.8]
subrules

plot(subrules, method = "matrix", measure = "lift")
plot(subrules, measure = c("support", "lift"), shading = "confidence", jitter=0)
plot(subrules, method = "two-key plot", jitter=0)
plot(subrules, method = "grouped", control = list(k = 50))
plot(subrules, method = "paracoord")
