#LOAD LIBRARIES
library(tm) #A framework for text mining applications within R.
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tidytext)
library(widyr) # for function pairwise_count
library(magrittr) # for function %<>%
library(reshape2) # for function acast
library(tidyr)
library(igraph) # for graph_from_data_frame function
library(ggraph)
library(spacyr) #needed for the lemmatization in German
##spacyr runs with Anaconda, which must be installed before we can use language sets: https://cran.r-project.org/web/packages/spacyr/readme/README.html#:~:text=The%20easiest%20way%20to%20install,.io%2Fminiconda.html.
#spacy_install()
#spacy_download_langmodel(model="de_core_news_lg") #Is needed to install the language set for the first time
spacy_initialize(model="de_core_news_lg")
library(ggthemes)
library(readxl)
library(gridExtra)
library(topicmodels)
library(stringr)
library(quanteda)
library(cowplot)

#LOAD DATA FROM EXCEL
text1 <- read_excel("df.xlsx")

#DATA PRE-PROCESSING
text1$Text <- as.character(text1$Text)

##delete symbols etc. from text
text1$Text = gsub("B:", "", text1$Text)
text1$Text = gsub(":", "", text1$Text)
text1$Text = gsub("-", "", text1$Text)
text1$Text = gsub("#\\w+", "", text1$Text)
text1$Text = gsub("#", "", text1$Text)
text1$Text = gsub("\r?\n|\r", " ", text1$Text)

##transform to tibble with columns
text2<- text1 %>% unnest_tokens(word, Text)

##lemmatization of the words in the generated tibble "text2"
text.1.lemma.1 <- spacy_parse(text2$word)

##This is to extract only the variable with the lemmatized words from lemmatization output
text.1.lemma.2 <- text.1.lemma.1$lemma

##The text has Umlauts in it, which do not work well in R. With this function "swiss2" You can exchange the Umlauts in the data frame as define in the lines 23 - 28.
df<-data.frame(text=text.1.lemma.2)
a <- c("ä", "ae") #small a with colon
A <- c("Ä", "Ae") #big A with colon
u <- c("ü", "ue") #small u with colon
U <- c("Ü", "Ue") #big U with colon
o <- c("ö", "oe") #small o with colon
O <- c("Ö", "Oe") #big O with colon
m <- cbind(a,A,u,U,o,O, deparse.level=0)
swiss2 <- function(m, data){
  for (i in seq_len(ncol(m))){
    data <- gsub(paste(m[,i], sep=",")[1],paste(m[,i], sep=",")[2], data,
                 ignore.case=F, perl = F, fixed = F, useBytes = F)
  }
  data
}

##This runs the function from above
df1<-swiss2(m, df$text)

##This exports the revised text to a data frame
df2<-data.frame(text=df1)
text3<-df2 %>% unnest_tokens(word, text)

##This generates a corpus and transforms all words to lower case, deletes stopwords, removes words (not included in the stopwords sets of package tm), removes punctuation and numbers.
text.prep <- VCorpus(VectorSource(text3))
text.prep <- tm_map(text.prep, content_transformer(tolower))
text.prep <- tm_map(text.prep, removeWords, tm::stopwords(kind= "de"))
text.prep <- tm_map(text.prep, removeWords, c("add_your_word_here_1","add_your_word_here_2"))
text.prep <- tm_map(text.prep, removePunctuation)
text.prep <- tm_map(text.prep, removeNumbers)

##The lemmatizer does not work 100% for German. You need to manually add the correct lemmatized word.
text.prep<-tm_map(text.prep, content_transformer(function(x) gsub(x, pattern = "jed", replacement = "jeder"))) #example for "everyone" [jeder]
text.prep <- tm_map(text.prep, stripWhitespace)

##This builds a term matrix and generates a frequency table of all words.
dtm.prep<-TermDocumentMatrix(text.prep)
rowTotals<-apply(dtm.prep,1,sum)
dtm.prep1 <- dtm.prep[rowTotals > 0, ] 
m.prep <- as.matrix(dtm.prep1)
v.prep <- sort(rowSums(m.prep), decreasing =TRUE)
text.prep.df <- data.frame(word = names(v.prep), freq =v.prep)
n_sum <- sum(text.prep.df$freq)
text.prep.df<-data.frame(word =names(v.prep),freq=v.prep,n_sum)

##This shows the top10 used words
head(text.prep.df, 10)

#DATA ANALYSIS
##This prepares the needed dataframes and tibbles.
text.testing <- data.frame(text=unlist(sapply(text.prep, `[`, "content")), 
                        stringsAsFactors=F)

text.testing<-data.frame(text.testing,text2$Interview,text2$sex,text2$education,text2$age,text2$profession)
text.testing.def<- text.testing %>% unnest_tokens(words, text)

text.ngram<-text.testing.def%>% 
  group_by(text2.Interview) %>% 
  summarize(text = str_c(words, collapse = " ")) %>%
  ungroup()

##This loads the sentiments: http://wortschatz.uni-leipzig.de/de/download
##Ref.: R. Remus, U. Quasthoff & G. Heyer: SentiWS - a Publicly Available German-language Resource for Sentiment Analysis.In: Proceedings of the 7th International Language Ressources and Evaluation (LREC'10), pp. 1168-1171, 2010
sent<- c(
  #positive words
  readLines(paste0(file.path("SentiWS_v2.0_Positive.txt")),
            encoding = "UTF-8"),
  #negative words
  readLines(paste0(file.path( "SentiWS_v2.0_Negative.txt")),
            encoding = "UTF-8")
) %>% lapply(function(x) {
  #extract single rows
  res <- strsplit(x, "\t", fixed = TRUE)[[1]]
  return(data.frame(words =res[1], value = res[2],
                    stringsAsFactors=FALSE))
}) %>%
  bind_rows %>%
  mutate(words = gsub("\\|.*", "", words) %>% tolower,
         value = as.numeric(value)) %>% 
  group_by(words) %>% summarise(value = mean(value)) %>% ungroup

text.sent <- left_join(text.testing.def, sent, by = c("words")) %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(!is.na(value))

text.sent %<>% 
  mutate(sent = ifelse(value >= 0, "positive", "negative")) 


##Bigram analysis
text.bigrams <- text.ngram %>%
  unnest_tokens(output = bigram, input = text, token = "ngrams", n = 2)
text.bigrams %>%
  count(bigram, sort = TRUE) #The stopwords are still included
text.brigrams.sep <- text.bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
text.brigrams.filter <- text.brigrams.sep %>%
  filter(!word1 %in% tm::stopwords(kind= "de"))%>%
  filter(!word2 %in% tm::stopwords(kind= "de"))
text.brigrams.count <- text.brigrams.filter %>%
  count(word1, word2, sort=TRUE)

##example for: Bigram Electronic Health Record

KIS.words <- text.brigrams.sep %>%
  filter(word1 == "klinikinformationssystem") %>%
  inner_join(sent, by = c(word2 = "words"))%>%
  count(word2, value, sort = TRUE) %>%
  ungroup()

plot1<-KIS.words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Electronic Health Record") +
  ylab("polarity * frequency") + theme(text = element_text(size=12))+
  coord_flip()+ylim(-0.8,0.8)

##n-gram analysis (n= 5)
text.bigrams.5 <- text.ngram %>%
  unnest_tokens(output = bigram, input = text, token = "ngrams", n = 5)
text.bigrams.5 %>%
  count(bigram, sort = TRUE)
text.brigrams.sep.5 <- text.bigrams.5 %>%
  separate(bigram, c("word1", "word2","word3","word4","word5"), sep = " ")
text.brigrams.filter.5 <- text.brigrams.sep.5
text.brigrams.count.5 <- text.brigrams.filter.5 %>%
  count(word1, word2,word3,word4,word5, sort=TRUE)

##5-gram analysis for Laptop
laptop.words.5.2 <- text.brigrams.sep.5 %>%
  filter(word1 == "laptop")%>%
  inner_join(sent, by = c(word2 = "words"))
laptop.words.5.3 <- text.brigrams.sep.5 %>%
  filter(word1 == "laptop")%>%
  inner_join(sent, by = c(word3 = "words"))
laptop.words.5.4 <- text.brigrams.sep.5 %>%
  filter(word1 == "laptop")%>%
  inner_join(sent, by = c(word4 = "words"))
laptop.words.5.5 <- text.brigrams.sep.5 %>%
  filter(word1 == "laptop")%>%
  inner_join(sent, by = c(word5 = "words"))
laptop.words.total.5<-bind_rows(laptop.words.5.2,laptop.words.5.3,laptop.words.5.4,laptop.words.5.5)

#SENTIMENT ANALYSIS
text.sent%>%summarise (mean = mean(value))
pos<-text.sent%>%filter(value>=0) #to check the proportion of pos
neg<-text.sent%>%filter(value<=0) #to check the proportion of neg

#Generates plot SentiWS sentiments from the interviews ordered by their frequency. The left bar chart displays the sentiments with negative polarization. The right bar chart displays the sentiments with positive polarization.

text.sent %>% 
  count(words, sent, sort = TRUE) %>%
  ungroup %>% 
  group_by(sent) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(words, n), words = reorder_within(words, n, sent)) %>%
  ggplot(aes(words, n)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sent, scales = "free_y") +
  labs(x = NULL, y = "frequency") +
  coord_flip()+
  scale_x_reordered()+
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(label = n, hjust=1.2), color= "white")+theme_light()


#Calculate the average proportion per interview and overall
pos0<-text.sent %>% filter (value>=0)
pos0<-c((mean(pos0$value)))
neg0<-text.sent %>% filter (value<=0)
neg0<-c((mean(neg0$value)))
posNorm0<- pos0 / (pos0 - neg0)
negNorm0 <- neg0 / (pos0 - neg0)
posNorm0
negNorm0

##example for interview 1
iv1 <- text.sent %>% filter(text2.Interview==1)
pos1<-iv1%>%filter(value>=0) #to check the proportion of pos
pos1 <- c((mean(pos1$value)))
neg1<-iv1%>%filter(value<=0) #to check the proportion of neg
neg1 <- c((mean(neg1$value)))

posNorm1<- pos1 / (pos1 - neg1)
negNorm1 <- neg1 / (pos1 - neg1)
posNorm1
negNorm1

df.iv1<- data.frame(posNorm=posNorm1,
                   negNorm=negNorm1,
                     sex= var_sex_level,
                     edu=var_edu_level,
                     age=var_age_number,
                     prof=var_prof_level)

df.prob <- rbind(df.iv1,df.iv2,df.iv3,df.iv4,df.iv5,df.iv6,df.iv7,df.iv8,df.iv9,df.iv10,df.iv11,df.iv12,df.iv13,df.iv14,df.iv15,df.iv16,df.iv17,df.iv18,df.iv19,df.iv20)

df.prob<-cbind(df.prob,id=(1:20))

df.prob1<-data.frame(negNorm = df.prob$negNorm*(-100),
                     id=df.prob$id)

df.prob1.percent<-data.frame(negNorm = df.prob$negNorm*(-100)/100,
                     id=df.prob$id)

#This generates the scatterplot for visualizing the probabilities of negative sentences.
ggplot(data=df.prob1.percent, aes(id,negNorm))+geom_point()+scale_y_reverse(lim=c(1,0),labels = scales::percent)+geom_hline(yintercept=mean(df.prob1.percent$negNorm), color = "red")+ylab("proportion of negative\n sentences about technology")+xlab("Interview number")+geom_text(aes(label= paste0(round(100*negNorm, digits = 0),"%")),hjust = 0.5, vjust = 2)
                               
