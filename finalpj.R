library(RedditExtractoR)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# DATA LOADING
#####################
# get the list of the web addresses of the most recent posts
load(file.choose()) # pop-up window, choose objects.RData
CGThreads.df <- data.frame(CGThreads)
CGThreads <- find_thread_urls(subreddit = "careerguidance", sort_by = "new", period = "day")
# write.csv(CGThreads, "/Users/User/Desktop/R practice/finalpj_data/redditThreads.csv")

# access each URL and grab information about the comments
CGComments <- get_thread_content(CGThreads$url)
CGComments.df <- data.frame(CGComments$comments)

write.csv(CGComments.df, "/Users/User/Desktop/R practice/finalpj_data/redditComments.csv")

# combine by "url"
require(dplyr)
merged_df <- full_join(CGThreads.df, CGComments.df, by = "url")
colnames(merged_df)[colnames(merged_df) == "comments"] <- "count_comments"
write.csv(merged_df, "/Users/User/Desktop/R practice/finalpj_data/redditMerged.csv")



num_zero_comments <- sum(CGThreads.df$comments == "0") # 351 threads with no comments
num_zero_comments
num_unique_urls <- length(unique(merged_df$url)) # equal number of threads; no prob in merge process!
num_unique_urls
#num_comments <- sum(CGThreads.df$comments) # 2609 comments (we have 8350 comments in total)
#num_comments

#####################
# SENTIMENT ANALYSIS METHOD 1
#####################
library(SentimentAnalysis)
sentiment <- analyzeSentiment(merged_df$comment)
# output is sentiment scores based on different dictionary (dictGI, dictHE, dictLM, dictQDAP)
## positive - negative = neutral score
## https://datascience.stackexchange.com/questions/25782/what-do-all-the-column-names-mean-in-the-sentimentanalysis-packages-analyzesent

# the different in scores based on the choice of dictionary
sentimentData <- sentiment$SentimentGI
sentimentData <- sentiment$SentimentHE
sentimentData <- sentiment$SentimentLE
sentimentData <- sentiment$SentimentQDAP
plotSentiment(sentimentData)


#####################
# SENTIMENT ANALYSIS METHOD 2
#####################
# basic info about the dictionary that we apply on our analysis: https://quanteda.io/reference/data_dictionary_LSD2015.html
data_dictionary_LSD2015 
summary(data_dictionary_LSD2015)
merged_df_copy <- merged_df
library(quanteda)

additional_stopwords <- c("feel", "like")

cal_sentiment <- function(mydata) {
  # Step 1: Create corpus
  mydata <- cbind(doc_id = seq_len(nrow(mydata)), mydata)
  mydata_corpus <- corpus(mydata, docid_field = "doc_id", text_field = "text")
  
  # Step 2: Clean raw data
  dfm_mydata_corpus <- mydata_corpus %>%
    tokens(remove_punct = TRUE, remove_symbols = TRUE) %>%
    tokens_wordstem() %>%
    tokens_remove(stopwords("english"), additional_stopwords) %>%
    dfm()
  
  # Step 3: Tokenize and Apply dictionary
  toks_dict <- mydata_corpus %>%
    tokens() %>%
    tokens_lookup(dictionary = data_dictionary_LSD2015)
  dfm_mydata_LSD2015 <- dfm(toks_dict)
  
  # Step 4: Calculate sentiment
  dict_output <- convert(dfm_mydata_LSD2015, to = "data.frame")
  dict_output$sent_score <- log(
    (dict_output[, 3] + dict_output[, 5] + 0.5) /
      (dict_output[, 2] + dict_output[, 4] + 0.5)
  )
  dict_output <- cbind(dict_output, docvars(mydata_corpus))
  
  # Return the final dictionary output
  return(dict_output)
}

thread_sentiment_df <- cal_sentiment(CGThreads.df)
# head(thread_sentiment_score)

# change the name of "comment" column to "text" so that the function works:
colnames(CGComments.df)[colnames(CGComments.df) == "comment"] <- "text"

comment_sentiment_df <- cal_sentiment(CGComments.df)
#head(comment_sentiment_score)

# Step 5: merge sent_score column of Thread dataset
## with the merged_df dataset
thread_sent_score <- thread_sentiment_df %>% 
  select(url, sent_score) %>% 
  as.data.frame

comment_sent_score <- comment_sentiment_df %>% 
  select(url, sent_score) %>% 
  as.data.frame

# Step 6: explore the covariance (direction) and correlation (strength) between threads and comments
analysis.df <- left_join(thread_sent_score, comment_sent_score, by="url") 
cov(analysis.df$sent_score.x, analysis.df$sent_score.y, na.rm=T)
inner_join(merged_df, by="url")

print(cov(analysis.df$sent_score.x, analysis.df$sent_score.y, use="complete"))
cor(na.omit(analysis.df[,-1]))


#####################
# TOPIC MODELLING
#####################
require(stm)
additional_stopwords <- c("feel", "like", "just")
processed <- textProcessor(CGThreads.df$text, metadata = CGThreads,
                           lowercase = TRUE, #*
                           removestopwords = TRUE, #*
                           removenumbers = TRUE, #*
                           removepunctuation = TRUE, #*
                           stem = TRUE, #*
                           wordLengths = c(3,Inf), #*
                           sparselevel = 1, #*
                           language = "en", #*
                           verbose = TRUE, #*
                           onlycharacter = TRUE, # not def
                           striphtml = FALSE, #*
                           customstopwords = additional_stopwords, #*
                           v1 = FALSE) #*

stm_model <- stm(processed$documents, processed$vocab, K = 5,
               data = processed$meta, verbose = FALSE,
               LDAbeta = T, 
               interactions = F, 
               reportevery = 10, 
               init.type = c("Spectral"), seed = 123)
plot(stm_model)
labelTopics(stm_model)
cloud(stm_model, topic=5)








# change number of topics to 10
stm_model2 <- stm(processed$documents, processed$vocab, K = 10,
                  data = processed$meta, verbose = FALSE,
                  LDAbeta = T, 
                  interactions = F, 
                  reportevery = 10, 
                  init.type = c("Spectral"), seed = 123)
plot(stm_model2)
set.seed(837)


library(quanteda)
library(stm)
require(stm)
additional_stopwords <- c("feel", "like")
model_topic <- function(mydata, num_topics) {
  # Step 1: Create a quanteda corpus
  mydata_corpus <- corpus(mydata, text_field = "text")
  
  # Step 2: Clean the text data
  my_data_dfm <- mydata_corpus %>%
    tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) %>%
    tokens_wordstem() %>%
    tokens_remove(stopwords("english"), additional_stopwords) %>%
    dfm()


  # Step 3: Convert quanteda DFM to STM input format
  stm_input <- convert(mydata_dfm, to = "stm")

  
  # Return the fitted STM model
  return(ctmodel)
}

stmodel <- model_topic(CGThreads.df, 5)

# Visualization
plot(stmodel, n = 10)
cloud(stmodel, topic = 1)
cloud(stmodel, topic = 2)
cloud(stmodel, topic = 3)
cloud(stmodel, topic = 4)
cloud(stmodel, topic = 5)

labelTopics(ctmodel, topic = 1, n = 20)


# Conclusion