library(readr)    # to import and read flat/tabulated format
library(stringr)  # to use regx and other string functions
library(tidyverse)  # to manipulate data
library(dplyr)      # to manipulate data
library(ggplot2)    # to plot graph
library(lubridate)  # to manipulate as date
library(tm)         # to perform text mining operations
library(caret)      # to split data and and select featured data
library(wordcloud)  # to write text mining in cloud form
library(gridExtra)  # to arrange multiple grid based plots on a page
library(RColorBrewer)# to have nicer color palettes
library(tibble)
library(NLP)
library(lattice)

AirplaneCrashData <- read.csv2('./airplane-crashes-since-1908/Airplane_Crashes_and_Fatalities_Since_1908.csv', sep=",", header=TRUE, stringsAsFactors = FALSE ) 
print(as_tibble(AirplaneCrashData))

summary(AirplaneCrashData)

AirplaneCrashData <- na.omit(AirplaneCrashData)
summary(AirplaneCrashData)

AirplaneCrashData <- AirplaneCrashData %>% separate(Date, into = c("Month","Day","Year"))
print(as_tibble(AirplaneCrashData))

 AirplaneCrashData$Location <- sapply(AirplaneCrashData$Location, as.character)
    AirplaneCrashData$Location <- gsub(".*,", "", AirplaneCrashData$Location)
    #remove white space at beginning
    AirplaneCrashData$Location <- str_trim(AirplaneCrashData$Location, side = "both")
    #Convert string back to factors
    AirplaneCrashData$Location <- sapply(AirplaneCrashData$Location, as.factor)
print(as_tibble(AirplaneCrashData))

#Monthly
months <- as.data.frame(table(AirplaneCrashData$Month))
A2 <- ggplot(months, aes(Var1, Freq)) + 
      geom_bar(stat = "identity", fill = "Navy", width = 0.3) + 
      xlab("Month") + ylab("Crashes") +
      ggtitle("Total number of crashes per month")

#Yearly
years <- as.data.frame(table(AirplaneCrashData$Year))
A1 <- ggplot(years, aes(y = Freq, x = Var1, group = 1))  + 
      geom_line(size = 1, linetype = 1, color = "Navy") + 
      geom_point(size = 3, shape = 20)+ 
      geom_smooth(stat = 'smooth', color = 'Red', method = 'gam', formula = y ~ s(x, bs = "cs")) +
      xlab("Years") + ylab("Crashes") + 
      scale_x_discrete(breaks = seq(from = 1908, to = 2009, by = 10)) + 
      ggtitle("Total number of crashes per year")

grid.arrange(A1, A2, nrow = 2, heights=2:1)

Fatalities <- AirplaneCrashData %>% group_by(Year) %>% 
              summarise(total_fatalities = sum(Fatalities), total_passengers = sum(Aboard))

f1 <- ggplot(Fatalities, aes(y = (total_fatalities/total_passengers)*100, x = Year, group = 10))  + 
      geom_line(size = 1, linetype = 1, color = "Red") + 
      geom_point(size = 3, shape = 20) + 
      geom_smooth() +
      xlab("Years") + ylab("% Fatalities") + 
      scale_x_discrete(breaks = seq(from = 1908, to = 2009, by = 10)) +
      ggtitle("Percent of fatalities per year")
f1

Location_Crash <-   AirplaneCrashData %>% group_by(Location) %>% 
                    summarise(total_fatalities = sum(Fatalities)) %>% arrange(desc(total_fatalities))

L1 <- ggplot(Location_Crash[1:10,], aes(x = reorder(Location, -total_fatalities), y = total_fatalities, alpha = total_fatalities)) + 
      geom_bar(stat = "identity", fill = "purple", width = 0.5) +
      xlab("Countries") + ylab("Number of Casualties") + 
      ggtitle("Top 10 Countries with Highest Flight Crash Casualties")
L1

crash_operator <-   AirplaneCrashData %>% group_by(Operator) %>% 
                    summarise(Freq = n()) %>% arrange(desc(Freq))

operator <- ggplot(crash_operator[1:10,], aes(x = reorder(factor(Operator), Freq), y = Freq, alpha = Freq)) + 
            geom_bar(stat = "identity", fill = "red", width = 0.10) + geom_point(stat = "identity") + 
            xlab("Aircraft Operators") + ylab("Crashes") + ggtitle("Top 10 Aircraft Operator involved in Crash") + 
            coord_flip() 
operator

crash_type <- AirplaneCrashData %>% group_by(Type) %>% 
              summarise(Freq = n()) %>% arrange(desc(Freq))

type <- ggplot(crash_type[1:10,], aes(x = reorder(factor(Type), Freq), y = Freq, alpha = Freq)) + 
        geom_bar(stat = "identity", fill = "maroon", width = 0.10) + geom_point(stat = "identity") + 
        xlab("Types") + ylab("Crashes") + ggtitle("Top 10 Aircraft Type That Crashed") +
        coord_flip() 
type  



print(tibble(AirplaneCrashData$Summary))

data <- VCorpus(VectorSource(AirplaneCrashData$Summary))

corpus_clean <- tm_map(data, tolower)
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removeWords, "flight")
corpus_clean <- tm_map(corpus_clean, removeWords, "crashed")
corpus_clean <- tm_map(corpus_clean, removeWords, "plane")
corpus_clean <- tm_map(corpus_clean, removeWords, "aircraft")


tdm <- TermDocumentMatrix(corpus_clean)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]

wordcloud(corpus_clean, max.words = 100, min.freq = 30, random.order = FALSE)

# data <- VCorpus(VectorSource(AirplaneCrashData$Location))

# corpus_clean <- tm_map(data, tolower)
# corpus_clean <- tm_map(corpus_clean, removePunctuation)
# corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
# corpus_clean <- tm_map(corpus_clean, removeNumbers)
# corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())


# tdm <- TermDocumentMatrix(corpus_clean)
# m <- as.matrix(tdm)
# v <- sort(rowSums(m),decreasing=TRUE)
# d <- data.frame(word = names(v),freq=v)
# pal <- brewer.pal(9, "BuGn")
# pal <- pal[-(1:2)]

# wordcloud(corpus_clean, max.words = 100, min.freq = 30, random.order = FALSE,)


