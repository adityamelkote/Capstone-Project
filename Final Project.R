setwd("c:/Users/adyme/Documents/R/Workspace/")
student_data <- read.csv("IDC4U_Module 5_UNGoal Exercise_20161221.csv",header = T, row.names = NULL, stringsAsFactors = FALSE)


#See how much pass and how much fail, and percentages using cross tabulations and concatenating the representations
xtabs(~ student_data$PassFail +student_data$PassFail, data = student_data)
cat("Percentage of Pass grades: ", response_rate <- mean(student_data$PassFail == " Pass "), "\n")
cat("Percentage of Fail grades: ", response_rate <- mean(student_data$PassFail == " No Pass "), "\n")

#Graphing functions that are used throughout 
#Graph for age and gender function using inherent R barplot and legend capabilities
Graph_gender <- function(x,y,z){
counts <- table(x, y)
barplot(counts, main=z,
        xlab="Age", ylab="Frequency", col=c("pink","lightblue"),
)
legend("top", 
       legend = c("Male", "Female"), 
       col = c("lightblue","pink"),
       pch = c(15,15), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
)
}
#Graphing evalutation ratings function using GGplot2
library(ggplot2)
Graph_eval <- function(x,y){
        pf <- data.frame(Evaluation = c(x))
        pf_graph<- ggplot(data = pf, aes(y=Evaluation)) 
        
        pf_graph + geom_bar(stat = "count", color = "black", fill = "darkgrey") + ggtitle(y)
        
}   


#Graphically show distribution by age and gender for all students
Graph_gender(student_data$Gender, student_data$Age, "Students by Age and Gender")
#Graphically show evalutation ratings
library(ggplot2)
pass_student <- subset(student_data, student_data$PassFail == " Pass ")
Graph_eval(pass_student$Course.Evaluation,"Course Evaluation Ratings from Passing Students")

#Find out gender and age distribution exact numbers for overall stats using cross tabulations
xtabs(~ student_data$Gender + student_data$Gender, data = student_data)
xtabs(~ student_data$Age + student_data$Age, data = student_data)
xtabs(~ student_data$Age + student_data$Age + student_data$Gender, data = student_data)
        

#Find out median overall score (using median function because of outliers of ppl that drop the course partway through)
#NA results omitted using na.omit
student_data_no_na = na.omit(student_data)
median(student_data_no_na$Overall)



#Filtering pass w/ distinction using subsetting
pass_distinction <- subset(pass_student, pass_student$Overall > 85)


#Find out gender and age distribution numerically using cross tabulation
xtabs(~ pass_distinction$Gender + pass_distinction$Gender, data = pass_distinction)
xtabs(~ pass_distinction$Age + pass_distinction$Age, data = pass_distinction)
#Create stacked bargraph of male female distribution by age using functions defined previously
Graph_gender(pass_distinction$Gender, pass_distinction$Age, "High Distinction Students by Age and Gender")
#Representing student evaluation ratings using GGplot2
Graph_eval(pass_distinction$Course.Evaluation,"Frequency of Evalutions for Pass With Distinction Students")

#Check to see if they do noticebly better or worse in any modules
#Getting relevant columns only
distinguished_mean <- pass_distinction[-c(1,2,3,26,27,28)]
distinguished_mean <- as.data.frame(apply(distinguished_mean, 2, as.numeric))  # Convert all variable types to numeric
#Apply a function over df
sapply(distinguished_mean, class)      
#Omitting NA entries
distinguished_mean<- na.omit(distinguished_mean)
#Converting to data frame
distinguished_mean <- as.data.frame(apply(distinguished_mean, 2,mean,na.omit=TRUE))
barplot(t(as.matrix(distinguished_mean)), xlab  = "Modules",
        ylab  = "Score",
        main  = "Barplot of mean score per module for High Distinction Students",
        col   = "tan",ylim = c(0,100), beside=TRUE)

#Check to see if there is a pattern in their course evaluations
xtabs(~ pass_distinction$Course.Evaluation + pass_distinction$Course.Evaluation, data = pass_distinction)



#Filtering pass w/o distinction using subsetting
pass_reg <- subset(pass_student, !(pass_student$Overall > 85))    

#Find out gender and age distribution using cross tabulations
xtabs(~ pass_reg$Gender + pass_reg$Gender, data = pass_reg)
xtabs(~ pass_reg$Age + pass_reg$Age, data = pass_reg)


#Cross tabulating and create stacked bargraph of male female distribution by age for pass w/o high dist. students
xtabs(~ pass_reg$Course.Evaluation + pass_reg$Course.Evaluation, data = pass_reg)
Graph_gender(pass_reg$Gender, pass_reg$Age, "Pass w/o High Distinction Students by Age and Gender")


#Create graph of evaluation ratings
Graph_eval(pass_reg$Course.Evaluation,"Frequency of Evalutions for Pass w/o Distinction Students")

#Check to see if they do noticebly better or worse in any modules
reg_pass_mean <- pass_reg[-c(1,2,3,26,27,28)]
reg_pass_mean <- as.data.frame(apply(reg_pass_mean, 2, as.numeric))  # Convert all variable types to numeric
#Applying function over df
sapply(reg_pass_mean, class)      
reg_pass_mean<- na.omit(reg_pass_mean)
reg_pass_mean <- as.data.frame(apply(reg_pass_mean, 2,mean,na.omit=TRUE))
#Graphing with barplot function
barplot(t(as.matrix(reg_pass_mean)), xlab  = "Modules",
        ylab  = "Score",
        main  = "Barplot of mean score per module for Pass w/o High Distinction Students  ",
        col   = "lightgreen",ylim = c(0,100), beside=TRUE)

#Check to see if there is a pattern in their course evaluations
xtabs(~ pass_distinction$Course.Evaluation + pass_distinction$Course.Evaluation, data = pass_distinction)


#Running similar tests on no pass

#Find out gender and age distribution using subsetting and cross tabulation 
fail_student <- subset(student_data, student_data$PassFail == " No Pass ")
xtabs(~ fail_student$Gender + fail_student$Gender, data = fail_student)
xtabs(~ fail_student$Age + fail_student$Age, data = fail_student)

#Create stacked bargraph of male female distribution by age using user created functions
Graph_gender(fail_student$Gender, fail_student$Age, "No Pass Students by Age and Gender")


#Check to see if they do noticebly better or worse in any modules
fail_student_mean <- fail_student[-c(1,2,3,26,27,28)]
fail_student_mean <- as.data.frame(apply(fail_student_mean, 2, as.numeric))  # Convert all variable types to numeric
#Applying function over df
sapply(fail_student_mean, class)  
#Amitting NA values
fail_student_mean<- na.omit(fail_student_mean)
fail_student_mean <- as.data.frame(apply(fail_student_mean, 2,mean,na.omit=TRUE))
#Graphing via barplot function
barplot(t(as.matrix(fail_student_mean)), xlab  = "Modules",
        ylab  = "Score",
        main  = "Barplot of mean score per module for No Pass Students  ",
        col   = "lightgreen",ylim = c(0,100), beside=TRUE)



#Mean avg score per module graphed
#First, subset the modules
modules <- student_data[-c(1,2,3,26,27,28)]
#Subset average columns
avg_modules <- modules[c(5,10,15,20,22)]
avg_modules <- as.data.frame(apply(avg_modules, 2, as.numeric))  # Convert all variable types to numeric
#Apply function over df
sapply(avg_modules, class)      
#Omit na values
avg_modules<- na.omit(avg_modules)
avg_modules <- as.data.frame(apply(avg_modules, 2,mean,na.omit=TRUE))
#Graph with barplot functions
barplot(t(as.matrix(avg_modules)), xlab  = "Modules",
        ylab  = "Score",
        main  = "Barplot of mean score per module",
        col   = "lightgreen",ylim = c(0,100), beside=TRUE)





#Sentiment analysis
#Install + load necessary libraries
install.packages("ROAuth")
install.packages("twitteR")
library(twitteR)
library("ROAuth")
library(NLP)
library(tm)
library(quanteda)
library(ggplot2)
#Omitting keys ( as it is linked to my twitter)
api_key = "xxxx" # your api_key
api_secret = "xxxx" # your api_secret 
access_token = "xxxx" # your access_token 
access_token_secret = "xxxx" # your access_token_sceret 
credential<-OAuthFactory$new(consumerKey=api_key,
                             consumerSecret=api_secret,
                             requestURL="https://api.twitter.com/oauth/request_token",
                             accessURL="https://api.twitter.com/oauth/access_token",
                             authURL="https://api.twitter.com/oauth/authorize")

credential$handshake()

setup_twitter_oauth(api_key,api_secret,access_token,
                    access_token_secret)
save(key, file="twitterKey.Rdata")


Sentiment <- searchTwitter('Sentiment Analysis', n = 1000, since = '2012-01-01', retryOnRateLimit = 1e3)
Sentiment_df = twListToDF(Sentiment)
colnames(Sentiment_df)
Sentiment_df <- Sentiment_df$text
Sentiment_df <- gsub("@\\w+", "", Sentiment_df)
Sentiment_df <- gsub("[[:punct:]]", "", Sentiment_df)
Sentiment_df <- gsub("http\\w+", "", Sentiment_df)
Sentiment_df <- gsub("RT", "", Sentiment_df)
Sentiment_df <- gsub("rt", "", Sentiment_df)
cat(head(Sentiment_df))
Sentiment_df <- iconv(Sentiment_df, "latin1", "ASCII",sub="")
Sentiment_df <- tolower(Sentiment_df)
#Tokenizing each tweet by word
Sentiment_df_token <- tokens(Sentiment_df, what = "word", remove_punct = TRUE, remove_numbers = TRUE)
Sentiment_df_token
#Creating document term matrix
Sentiment_df_tdmCreator <- function(Sentiment_df_token, stemDoc = T, rmStopwords = T){
        
        tdm <- Corpus(VectorSource(Sentiment_df))
        if (isTRUE(rmStopwords)) {
                tdm <- tm_map(tdm, removeWords, stopwords())
        }
        if (isTRUE(stemDoc)) {
                tdm <- tm_map(tdm, stemDocument)
        }
        tdm <- TermDocumentMatrix(tdm,
                                  control = list(wordLengths = c(1, Inf)))
        tdm <- rowSums(as.matrix(tdm))
        tdm <- sort(tdm, decreasing = T)
        df <- data.frame(term = names(tdm), freq = tdm)
        return(df)
}
library(tm)
library(tmap)
library(wordcloud)        
Sentiment_df_tdmCreator <- Sentiment_df_tdmCreator(Sentiment_df_token)
dim(Sentiment_df_tdmCreator)
Sentiment_df_top25<- Sentiment_df_tdmCreator[1:25,]
#Graphing via ggplot2
Sentiment_df_plot <- ggplot(Sentiment_df_top25, aes(x = reorder(term, freq), y = freq)) +
        geom_bar(stat = "identity", fill = "red") +
        xlab("Most Used") + ylab("How Often") +
        coord_flip() + theme(text=element_text(size=10,face="bold"))

Sentiment_df_plot
#Matching words using AFINN list
afinn_list <- read.delim(file='AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')



Sentiment_df_2 <- data.frame(Sentiment_df_tdmCreator$term)

matched_sentiment <- unique(rbind(merge(Sentiment_df_2, afinn_list, by.x = "Sentiment_df_tdmCreator.term", by.y = "word", all.x = FALSE)))

#See how many words matched
NROW(matched_sentiment)

#List total amount of words
NROW(Sentiment_df_tdmCreator$term)


#Percentage of words matched
round(NROW(matched_sentiment)/NROW(Sentiment_df_tdmCreator$term), 2) * 100

#Average score for matched words
sum(matched_sentiment$score)
NROW(matched_sentiment$score)
sum(matched_sentiment$score) / NROW(matched_sentiment$score)

#Rounding average to 2 decimal places
round(mean(matched_sentiment$score),2)

