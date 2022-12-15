pacman::p_load(tidyverse, rio, party, ggplot2, dplyr, vcd, webr, 
               plotrix, gginference,corrplot,ggplot2,ggpubr,BSDA, psych, naniar
               ,gridExtra, rpart, rpart.plot, rattle, caret, forcats)
library(dplyr)
install.packages("assertive")
library(assertive)
library(lubridate)
library(readr)

install.packages("ggfortify")
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)

airbnb <- read_csv("train.csv")
View(airbnb)

priceperday <- exp(airbnb$log_price)
airbnb$priceperday <- priceperday

sum(is.na(airbnb))
airbnb <- na.omit(airbnb) 

quartiles <- quantile(airbnb$priceperday, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(airbnb$priceperday)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
Lower
Upper


data_no_outlier <- subset(airbnb$priceperday, airbnb$priceperday > Lower & airbnb$priceperday < Upper)
length(data_no_outlier)


airbnbnew <- airbnb[-c(which(airbnb$priceperday > 350)), ]
summary(airbnb$priceperday)
str(airbnb)
nrow(airbnb) - sum(complete.cases(airbnb))
airbnb$thumbnail_url <- NULL
airbnb$description <- NULL

view(airbnb)
summary(airbnb$log_price)
priceperday <- exp(airbnb$log_price)
airbnb$priceperday <- priceperday
boxplot(exp(airbnb$log_price), ylab="Price", main= "Boxplot of Price")
summary(exp(airbnb$log_price))
length(airbnb$priceperday)


quartiles <- quantile(airbnb$priceperday, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(airbnb$priceperday)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(airbnb$priceperday, airbnb$priceperday > Lower & airbnb$priceperday < Upper)
length(data_no_outlier)


airbnbnew <- airbnb[-c(which(airbnb$priceperday > 350)), ]

airbnbnew <- airbnbnew[complete.cases(airbnbnew[11:14]),]
airbnbnew <- airbnbnew[complete.cases(airbnbnew[22:23]),]
airbnbnew <- airbnbnew[complete.cases(airbnbnew[15:24]),]
view(airbnbnew)

airbnbnew$property_type <- as.factor(airbnbnew$property_type)
summary(airbnbnew$property_type)

class(airbnbnew$last_review)
airbnbnew <- airbnbnew %>%
  mutate(last_review = as.Date(last_review))

airbnbnew <- airbnbnew %>%
  mutate(first_review = as.Date(first_review))

airbnbnew <- airbnbnew %>%
  mutate(host_since = as.Date(host_since))

airbnbnew <- airbnbnew %>%
  filter(first_review <= today())

airbnbnew <- airbnbnew %>%
  filter(last_review <= today())

airbnbnew <- airbnbnew %>%
  filter(host_since <= today())


airbnbnew %>%
  count(property_type)


airbnbnew$property_type <- fct_collapse(airbnbnew$property_type, "Other"=c("Bed & Breakfast","Boat","Boutique hotel","Bungalow","Cabin",
                                                                           "Camper/RV","Casa particular","Castle",
                                                                           "Cave","Chalet",
                                                                           "Dorm","Earth House",
                                                                           "Guest suite","Guesthouse","Hostel","Hut", "In-law",
                                                                           "Island","Lighthouse",
                                                                           "Loft","Parking Space", "Serviced apartment",
                                                                           "Tent","Timeshare",
                                                                           "Tipi","Train",
                                                                           "Treehouse","Vacation home","Villa","Yurt"))

summary(airbnbnew$property_type)


glimpse(airbnb)

sum(duplicated(airbnb))

airbnbnew %>%
  count(property_type) %>% 
  ggplot(aes(forcats::fct_reorder(property_type, desc(n)), n)) + 
  geom_col(fill="lightblue") + labs(title = "Barplot of property types") + xlab("Property Type")

airbnbnew <- airbnbnew %>% 
  mutate(price_range =
           case_when(priceperday >= 1 & priceperday <= 151 ~ "Affordable",
                     priceperday >= 151 & priceperday <= 350 ~ "Expensive")
  )
view(airbnbnew)
airbnbnew$price_range <- as.factor(airbnbnew$price_range)
summary(airbnbnew$price_range)


airbnbnew$amenities <- airbnbnew$amenities %>%
  str_remove_all(fixed("{")) %>%
  str_remove_all(fixed("}"))


airbnbnew[c('Firstamenity', 'Secondamenity', 'Thirdamenity', 'Additional')] <- str_split_fixed(airbnbnew$amenities, ',', 4)
View(airbnbnew)

class(airbnbnew$Firstamenity)
airbnbnew$Firstamenity <- as.factor(airbnbnew$Firstamenity)
summary(airbnbnew$Firstamenity)  



# Clustring 

mydata <- select(airbnbnew, c(23,24,28))
mydata
mydata12 <- tail(mydata,900)
view(mydata12)

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(mydata12)

KM <- kmeans(mydata12,3)

autoplot(KM,mydata12,frame=TRUE)

KM$centers
KM$size

airbnbnew$Additional <- NULL
airbnbnew$amenities<- NULL
sum(is.na(airbnbnew))
airbnbnew <- na.omit(airbnbnew) 

airbnb12 <- airbnbnew[-c(1,2,11,12,15,17,18,19,20,21,24,24,27,31,32)]





str(airbnb12)

number.perfect.splits <- apply(X=airbnb12[-1], MARGIN = 2, FUN = 
                                 function(col){
                                   t <- table(airbnb12$price_range,col)
                                   sum(t == 0)
                                 })
# Descending order of perfect splits
order <- order(number.perfect.splits,decreasing = TRUE)
number.perfect.splits <- number.perfect.splits[order]

par(mar=c(10,2,2,2))
barplot(number.perfect.splits,
        main="Number of perfect splits vs feature",
        xlab="",ylab="Feature",las=2,col="wheat")

airbnb12 <- as.data.frame(unclass(airbnb12),                    
                          stringsAsFactors = TRUE)

str(airbnb12)
set.seed(12345)
traindata <- sample(1:nrow(airbnb12),size = 
                      ceiling(0.80*nrow(airbnb12)),replace = FALSE)
# training set
airbnb_train <- airbnb12[traindata,]
# test set
airbnb_test <- airbnb12[-traindata,]

library(rpart)
library(rpart.plot)
dtree <- rpart(price_range~.,
               data=airbnb_train,
               method = "class")

rpart.plot(dtree, nn=TRUE)

cp.optim <- dtree$cptable[which.min(dtree$cptable[,"xerror"]),"CP"]

dtree <- prune(dtree, cp=cp.optim)
#Testing the model
pred <- predict(object=dtree,airbnb_test,type="class")
#Calculating accuracy
t <- table(airbnb_test$price_range,pred)
confusionMatrix(t)


table(airbnbnew$room_type, airbnbnew$price_range)

ggplot(airbnbnew, aes(x=city, fill=priceperday)) + geom_bar(position="dodge")

summary(airbnbnew$city)

airbnbnew <- as.factor(airbnbnew$city)

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
docs <- Corpus(VectorSource(amenities))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", 
                                                            x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


docs <- tm_map(docs, content_transformer(tolower))


# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
view(d)
head(d, 10)
par(mar=c(1,1,1,1))
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per = 0.35,
          colors=brewer.pal(8, "Dark2"))

par(mar=c(7, 4, 4, 2))
barplot(d[1:5,]$freq, las = 2, names.arg = d[1:5,]$word,
        col ="pink", main ="Most frequent words",
        ylab = "Word frequencies")



summary(airbnbnew$city)

airbnbnew$city<-as.factor(airbnbnew$city)

ggplot(airbnbnew, 
       aes(x = city, 
           y = priceperday, 
           fill = city))+ geom_boxplot(alpha = .6, outlier.alpha = .4) +
  scale_y_continuous(name = "Price") + 
  scale_x_discrete(name = "City") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        legend.position = "right") + 
  theme(legend.text = element_text(size = 8)) + 
  labs(fill = "Cities", title = "Prices per cities")

ggplot(airbnbnew, 
       aes(x = as.factor(property_type), 
           y = priceperday, 
           fill = property_type))+ geom_boxplot(alpha = .6, outlier.alpha = .4) +
  scale_y_continuous(name = "Price") + 
  scale_x_discrete(name = "Property Types") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        legend.position = "right") + 
  theme(legend.text = element_text(size = 8)) + 
  labs(fill = "", title = "Prices per property type")



ggplot(airbnbnew, 
       aes(x = as.factor(room_type), 
           y = priceperday, 
           fill = room_type))+ geom_boxplot(alpha = .6, outlier.alpha = .4) +
  scale_y_continuous(name = "Price") + 
  scale_x_discrete(name = "Room type") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        legend.position = "right") + 
  theme(legend.text = element_text(size = 8)) + 
  labs(fill = "", title = "Prices per room type")



ggplot(airbnbnew, 
       aes(y = priceperday))+ geom_boxplot() +
  scale_y_continuous(name = "Price") +
  labs(title = "Boxplot of Price")

class(airbnbnew$host_since)

view(airbnbnew)

ggplot(head(airbnbnew,500), aes(x = host_since, y = number_of_reviews)) +         
  geom_point() + 
  scale_x_date(date_labels = "%Y-%m")
