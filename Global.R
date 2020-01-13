# ALLOCATE MEMORY
options(java.parameters = "-Xmx10g")

# CLEAR CONSOLE
cat("\014")

# CLEAR GLOBAL VARIABLES
rm(list=ls())

# list of packages required ###########################################################################################################
list.of.packages = c("stringr","syuzhet","tm","NLP","wordcloud","dplyr","twitteR","igraph","ggthemes", 'lubridate',
                     'forcats','reshape2','widyr', 'ggraph','plyr','plotly','shinydashboard','DT','jsonlite','base64enc')

new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# LOAD LIBRARIES ########################################################################################################################
library(dplyr)
library(plyr)
library(reshape2)
library(widyr)
library(shinydashboard)
library(igraph)
library(ggraph)
library(ggthemes)
library(plotly)
library(wordcloud)
library(NLP)
library(lubridate)
library(stringr)
library(forcats)
library(tm)
library(syuzhet)
library(twitteR)
library(DT)
library(jsonlite)
library(base64enc)

# TWITTER AUTHENTICATION KEY #########################################################################################################

oauth <- setup_twitter_oauth(consumer_key = jsonlite::read_json('keys.json')$key,
                            consumer_secret = jsonlite::read_json('keys.json')$c_secret,
                             access_token = jsonlite::read_json('keys.json')$acc_token,
                             access_secret = jsonlite::read_json('keys.json')$acc_secret)

cat("\014")
# GETTING TREND LOCATIONS ############################################################################################################
trendloc <- function(x){
  x<- as.String(x)
  tloc <- availableTrendLocations()
  tloc <- tloc %>% filter(tloc$country == x)
  return(tloc)
}

# GETTING TRENDS ######################################################################################################################
trends<- function (LocT){
  trend<-getTrends(LocT)
  return ( trend)
} 

# CREATING SELECTION LIST FOR TREND ID INPUT #########################################################################################
tid <- as.data.frame(availableTrendLocations())
tid <- tid[with(tid, order(country)), ]

# PREPARING THE DATA FILE THE TWEETS #################################################################################################
prepDataFile <- function (df,searchterm){
  
  df <- twListToDF(df)
  
  # REMOVING UNWANTED COLUMNS
  del <- c("profileImageURL","retweeted",'favorited','replyToSID','replyToUID','id',"longitude",'latitude','language','language') 
  df <- df[,!(names(df)%in% del)]
  # DATE CLEANING
  df$day <- day(df$created)
  df$hour <- hour(df$created)
  df$day_n_hour <- paste(df$day,df$hour)
  df$created <- strftime(df$created, '%Y-%m-%d')
  # ADDING THE P-RETWEETED COLUMN
  for(i in 1:nrow(df)){ 
    df$pretweeted[i] <- if (df$isRetweet[i] == "TRUE"){ regmatches(df$text[i], gregexpr('RT @\\K\\w+', df$text[i], perl=T))} else 
    {df$screenName[i]}
    
  }
  df$pretweeted <- sapply( df$pretweeted, paste0, collapse="")
  return(df)
}

# CLEAN TWEET TEXT ####################################################################################################################
cleanTweetText <- function (df){
  df <- sapply(df, function (row) iconv(row, "latin1", "ASCII", sub = ""))#Remove non-ASCII characters
  df <- gsub("?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", df)
  df = str_replace_all(df,"[^[:graph:]]", " ")
  df = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", df) # Removes RT
  df = gsub("@\\w+", "", df)# Removes @<twitter handle>
  df = gsub("http\\w+", "", df) # Removes html links
  df = gsub("[ \t]{2,}", "", df) # Removes unnecessary spaces
  df = gsub("^\\s+|\\s+$", "", df)
  df = str_replace_all(df,"[^[:graph:]]", " ") # Fix for error related to formatting 'utf8towcs'"
  return(df)
}  

# TERM DOCUMENT MATRIX ###############################################################################################################
tdm <- function (data) {
  # Getting the corpus
  doc <- Corpus(VectorSource(data))
  # FURTHER CLEANING TEXT
  doc <- tm_map(doc, tolower)
  doc.ng <- tm_map(doc,removeWords,c(stopwords("english")))
  doc.ng <- tm_map(doc.ng, removePunctuation)
  
  # strip white spaces
  doc.ng <- tm_map(doc.ng, stripWhitespace)
  
  # Term document matrix
  tdm <- TermDocumentMatrix(doc.ng)
  tdm <- as.matrix(tdm)
  
  return (tdm)
}

# MELTING THE TERM DOCUMENT MATRIX FOR VARIANCE COMPUTATION #########################################################################
melttdm <- function(tdm){
  tdm <- melt(tdm, variable.name = "Term", value.name = "Appearance count")
  tdm <- subset(tdm, tdm$`Appearance count` !=0)
  return(tdm)
}

# WORD CLOUD GENERATOR ##############################################################################################################
wordcloudgen <- function(tdm,minfreq, maxnum){
  
  dt1 = sort(rowSums(tdm),decreasing = TRUE)
  freq.dt1 = data.frame(word=names(dt1), freq=dt1)
  
  set.seed(222)
  wordcloud(words = freq.dt1$word, freq.dt1$freq, max.words = maxnum, random.order = F, min.freq = minfreq, 
            colors = brewer.pal(8, 'Dark2'), rot.per = 0.2, scale = c(6,0.8))
}

# STORY NETWORK #####################################################################################################################
story <- function (meltedtdm,filter,correltn) { 
  
  meltedtdm %>% group_by(Terms) %>% filter(n() >= filter) %>% pairwise_cor(Terms, Docs) %>% 
    filter(!is.na(correlation), correlation > correltn) %>% graph_from_data_frame() %>% 
    
    ggraph(layout = 'linear', circular = TRUE) +  #"fr"
    geom_edge_link(aes(edge_alpha = correlation, color = 'grey'), show.legend = T) +
    geom_node_point(color = "lightblue", size = 6) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
}

# WORD CORRELATION PLOT #############################################################################################################
wordcorr <- function(meltedtdm, topic, correlatn2){
  
  word_cor <- meltedtdm %>% group_by(Terms) %>% filter(n() >= 1) %>% pairwise_cor(Terms, Docs) %>% filter(!is.na(correlation),correlation > correlatn2)
  word_cor %>% filter(item1 == topic) %>% arrange(desc(correlation)) -> df
  
  g <- ggplot(data = df, aes(x = reorder(factor(df$item2),-correlation), y = df$correlation))+
    geom_bar(stat = "identity", colour="black", fill="steelblue")+
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="bottom",panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank()) +
    labs(x="Terms", y="Correlation", title=paste0("Word correlation with the term :",topic))
  (gg <- ggplotly(g))
}

# WORD COUNT BAR PLOT #############################################################################################################
wordbarplot <- function (tdm,minfreq){
  
  #tdm[tdm>1] <- 1  
  
  w1 <- rowSums(tdm)
  w1 <- sort(rowSums(tdm), decreasing = TRUE)
  w1 <- subset(w1, w1>=minfreq)
  
  barplot(w1, las = 2, col = rainbow(20),main ="Word Frequencies", ylab = "frequencies")
  
}

# SCORE SENTIMENT #################################################################################################################
scoreSentiment = function(tab) {
  tab$syuzhet = get_sentiment(tab$text, method="syuzhet")
  tab$bing = get_sentiment(tab$text, method="bing")
  tab$afinn = get_sentiment(tab$text, method="afinn")
  tab$nrc = get_sentiment(tab$text, method="nrc")
  emotions = get_nrc_sentiment(tab$text)
  n = names(emotions)
  for (nn in n) tab[, nn] = emotions[nn]
  return(tab)
}

# EMOTION ANALYSIS ##############################################################################################################
emotionanalysis <- function(df,threshold) {
  all = c(
    paste(df$text[df$anger > threshold], collapse=" "),
    paste(df$text[df$anticipation > threshold], collapse=" "),
    paste(df$text[df$disgust > threshold], collapse=" "),
    paste(df$text[df$fear > threshold], collapse=" "),
    paste(df$text[df$joy > threshold], collapse=" "),
    paste(df$text[df$sadness > threshold], collapse=" "),
    paste(df$text[df$surprise > threshold], collapse=" "),
    paste(df$text[df$trust > threshold], collapse=" ")
  )
  all = removeWords(all, c(stopwords("english")))
  #
  # create corpus
  corpus = Corpus(VectorSource(all))
  #
  # create term-document matrix
  tdm = TermDocumentMatrix(corpus)
  #
  # convert as matrix
  tdm = as.matrix(tdm)
  #
  # add column names
  colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
  #
  # Plot comparison wordcloud
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, 'Emotion Comparison Word Cloud')
  comparison.cloud(tdm, random.order=FALSE,
                   colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                   title.size=1.5, max.words=250)
  
}


