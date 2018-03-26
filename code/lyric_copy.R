##########tidy file##########
lyric = fread("raw Lyric with Period.csv")
words = lyric %>% 
  transmute(serial = 1:5200,
            song = Song,
            year = rep(1965:2016, each = 100),
            text = Lyrics) %>%
  unnest_tokens(word, text)
data(stop_words)
words <- words %>% anti_join(stop_words)

##########frequency##########
words %>% count(word, sort = T)
library(ggplot2)
words %>%
  count(word, sort = TRUE) %>%
  filter(n > 2900) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

##########words group by year##########
words_year = lyric %>% 
  transmute(serial = 1:5200,
            year = rep(1965:2016, each = 100),
            text = Lyrics) %>%
  unnest_tokens(word, text) %>%
  group_by(year,
           word) 
data(stop_words)
words_year <- words_year %>% anti_join(stop_words)
year_frequency <- words_year %>% count(word, sort = T)
saveGIF({
  for (i in 1965:2016) {
    pic <- year_frequency %>%
      filter(year == i) %>%
      filter(n > 50) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col() +
      xlab(NULL) +
      coord_flip() +
      ggtitle(i)
    print (pic)
  }},
  interval = 0.5, movie.name = "yearfrequency.gif"
)

##########sentiments##########
data(sentiments)
nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")
nrcanger <- get_sentiments("nrc") %>%
  filter(sentiment == "anger")
nrcanticipation <- get_sentiments("nrc") %>%
  filter(sentiment == "anticipation")
nrcdisgust <- get_sentiments("nrc") %>%
  filter(sentiment == "disgust")
nrcfear <- get_sentiments("nrc") %>%
  filter(sentiment == "fear")
nrcnegative <- get_sentiments("nrc") %>%
  filter(sentiment == "negative")
nrcpositive <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")
nrcsadness <- get_sentiments("nrc") %>%
  filter(sentiment == "sadness")
nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")
nrctrust <- get_sentiments("nrc") %>%
  filter(sentiment == "trust")
angerwords <- words %>%
  inner_join(nrcanger)
anticipationwords <- words %>% 
  inner_join(nrcanticipation)
disgustwords <- words %>%
  inner_join(nrcdisgust)
fearwords <- words %>%
  inner_join(nrcfear)
joywords <- words %>%
  inner_join(nrcjoy)
sadnesswords <- words %>%
  inner_join(nrcsadness)
surprisewords <- words %>%
  inner_join(nrcsurprise)
trustwords <- words %>%
  inner_join(nrctrust)
##########wordcloud##########
library(wordcloud)
#totalcloud
words %>% 
  count(word, sort = T) %>%
  with(wordcloud(word, n, max.words = 500,random.order=FALSE, 
                 colors=brewer.pal(8, "Dark2")))
#2016cloud
words %>% 
  filter(year == 2016) %>%
  count(word, sort = T) %>%
  with(wordcloud(word, n, max.words = 500,random.order=FALSE,
                 colors=brewer.pal(8, "Dark2")))
#CloudBySentimentGIF
saveGIF({
  for (i in 1965:2016) {
    words %>% 
      filter(year == i) %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = T) %>%
      acast(word ~ sentiment,  value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                       random.order=FALSE,
                       max.words = 50)
  }},
  interval = 0.5, movie.name = "wordcloudbyyear50.gif"
)

##########rhyme##########
Score.list1=create.Scorelist(Lyrics.list,sample=c(1:1250));
Score.data1=Score.df(Score.list1);

PlotRhyme=function(data,by="Year"){
  if(by=="Year"){
    data.plot=data %>%
      filter(Rank!="Tie") %>%
      group_by(Year) %>%
      summarise(pctRhyme=sum(Rhyme)/n())
    
    ggplot(data=data.plot)+
      geom_col(aes(x=Year,y=pctRhyme),alpha=0.25,fill="steelblue1")+
      geom_path(aes(x=Year,y=pctRhyme),color="slateblue1",size=1.25)+
      geom_smooth(aes(x=Year,y=pctRhyme),method="lm",color="slateblue1");
  }else if(by=="Rank"){
    data.plot=data %>%
      group_by(Rank) %>%
      summarise(pctRhyme=sum(Rhyme)/n()) %>%
      arrange(as.numeric(Rank))
    
    ggplot(data=data.plot)+
      geom_area(aes(x=as.numeric(Rank),y=pctRhyme),fill="slateblue1")
  }else{}
  
}
##########sentiment & rhyme##########
#情感得分和押韵得分
load("score.data.RData")
lyrics_senti_score <- words %>% 
  inner_join(get_sentiments("nrc")) %>%
  group_by(serial,
           song) %>%
  filter(!(sentiment == "negative"|sentiment == "positive")) %>%
  count(sentiment)
  spread(sentiment,value = value)# sentiment score of each song
senti_n_rhyme <- lyrics_senti_score %>%
  mutate(Song = song) %>%
  select(serial, Song, senti_score) %>%
  inner_join(Score.data$score.songs)# sentiment score and rhyme score
summary(glm(Rhyme ~ senti_score, data = senti_n_rhyme, family = "binomial"))
cor.test(senti_n_rhyme$senti_score, senti_n_rhyme$RhymeScore) # relationship?
for (i in 1:nrow(senti_n_rhyme)) {
  if (senti_n_rhyme$senti_score[i] > 0) {
    senti_n_rhyme$pon[i] = 1
  } else {
    senti_n_rhyme$pon[i] = 0
  }
}
table(senti_n_rhyme$Rhyme, senti_n_rhyme$pon)
#什么情感押什么韵
xscore <- Score.data$score.dist %>%
  mutate(value = 1) %>%
  select(-score) %>% 
  spread(key = phnm, value = value) # sparse matrix of x
songsent=lyric %>% 
  transmute(serial = 1:5200,
            year = rep(1965:2016, each = 100),
            song = Song,
            text = Lyrics) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("nrc") %>%
               filter(!(sentiment == "positive"|sentiment == "negative"))) %>%
  group_by(serial,
           year,
           song) %>%
  count(sentiment) %>%
  mutate(perc=n/sum(n)) %>%
  select(-n) %>%
  filter(perc>0.2)
yscore <- songsent %>%
  select(-c(perc)) %>%
  mutate(value = 1) %>%
  spread(key = sentiment, value = value)# sparse matrix of y
score <- xscore %>%
  mutate(song = Song) %>% 
  select(-Song) %>%
  right_join(yscore) %>%
  select(-year) %>%
  select(-serial) %>%
  unique() %>%
  filter(!(Year == 0))
score[is.na(score)] <- 0 

angerscore <- cbind(score[,71],score[,3:69])
anticipationscore <- cbind(score[,72],score[,3:69])
disgustscore <- cbind(score[,73],score[,3:69])
fearscore <- cbind(score[,74],score[,3:69])
joyscore <- cbind(score[,75],score[,3:69])
sadnessscore <- cbind(score[,76],score[,3:69])
surprisescore <- cbind(score[,77],score[,3:69])
trustscore <- cbind(score[,78],score[,3:69])
# anger
cv.fitanger <- cv.glmnet(as.matrix(angerscore[,-1]), as.matrix(angerscore[,1]), alpha=1, family = "binomial")
plot(cv.fitanger)
best.lambda <- cv.fitanger$lambda.min
angermodel <- glmnet(as.matrix(angerscore[,-1]), as.matrix(angerscore[,1]),alpha = 1,lambda = best.lambda, family = "binomial")
anger_rhyme <- angermodel$beta 
# anticipation
cv.fitanticipation <- cv.glmnet(as.matrix(anticipationscore[,-1]), as.matrix(angerscore[,1]), alpha=1, family = "binomial")
plot(cv.fitanticipation)
best.lambda <- cv.fitanticipation$lambda.min
anticipationmodel <- glmnet(as.matrix(anticipationscore[,-1]), as.matrix(anticipationscore[,1]),alpha = 1,lambda = best.lambda, family = "binomial")
anticipation_rhyme <- anticipationmodel$beta
# disgust
cv.fitdisgust <- cv.glmnet(as.matrix(disgustscore[,-1]), as.matrix(disgustscore[,1]), alpha=1, family = "binomial")
plot(cv.fitdisgust)
best.lambda <- cv.fitdisgust$lambda.min
disgustmodel <- glmnet(as.matrix(disgustscore[,-1]), as.matrix(disgustscore[,1]),alpha = 1,lambda = best.lambda, family = "binomial")
disgust_rhyme <- disgustmodel$beta 
# fear
cv.fitfear <- cv.glmnet(as.matrix(fearscore[,-1]), as.matrix(fearscore[,1]), alpha=1, family = "binomial")
plot(cv.fitfear)
best.lambda <- cv.fitfear$lambda.min
fearmodel <- glmnet(as.matrix(fearscore[,-1]), as.matrix(fearscore[,1]),alpha = 1,lambda = best.lambda, family = "binomial")
fear_rhyme <- fearmodel$beta
# joy
library(sampling)
n=round(9/10*nrow(joyscore))
set.seed(1)
sub_train=sample(1:nrow(joyscore),n)
joy_train=joyscore[sub_train,]
joy_test=joyscore[-sub_train,]
fitjoy <- glmnet(as.matrix(joy_train[,-1]), as.matrix(joy_train[,1]), alpha=1, family = "binomial")
plot(fitjoy, xvar = "lambda")
cv.fitjoy <- cv.glmnet(as.matrix(joy_train[,-1]), as.matrix(joy_train[,1]), alpha=1, family = "binomial")
plot(cv.fitjoy)
best.lambda <- cv.fitjoy$lambda.min
joymodel <- glmnet(as.matrix(joy_train[,-1]), as.matrix(joy_train[,1]),alpha = 1,lambda = best.lambda, family = "binomial")
joy_rhyme <- joymodel$beta 
pre.joy <- predict(joymodel, as.matrix(joy_train[,-1]), s = best.lambda, type = "response")
preprepre <- rep(0,n)
for (i in 1:length(pre.joy)) {
  if (pre.joy[i] > 0.5){
    preprepre[i] <- 2 
  } else {
    preprepre[i] <- 1
  }
}
preprepre <- as.factor(preprepre)
table(preprepre,as.matrix(joy_train[,1]+1))
sum(preprepre==as.matrix(joy_train[,1]+1))/length(as.matrix(joy_train[,1])) # 64.17%

pre.joy <- predict(joymodel, as.matrix(joy_test[,-1]), s = best.lambda, type = "response")
preprepre <- rep(0,nrow(joyscore)-n)
for (i in 1:length(preprepre)) {
  if (pre.joy[i] > 0.5){
    preprepre[i] <- 2 
  } else {
    preprepre[i] <- 1
  }
}
preprepre <- as.factor(preprepre)
table(preprepre,as.matrix(joy_test[,1]+1))
sum(preprepre==as.matrix(joy_test[,1]+1))/length(as.matrix(joy_test[,1]))

library(ROCR)
pred <- prediction(pre.joy,as.matrix(joy_test[,1]+1))
perf <- performance(pred,"tpr","fpr")
plot(perf,lty=1)


# sadness
cv.fitsadness <- cv.glmnet(as.matrix(sadnessscore[,-1]), as.matrix(sadnessscore[,1]), alpha=1, family = "binomial")
plot(cv.fitsadness)
best.lambda <- cv.fitsadness$lambda.min
sadnessmodel <- glmnet(as.matrix(sadnessscore[,-1]), as.matrix(sadnessscore[,1]),alpha = 1,lambda = best.lambda, family = "binomial")
sadness_rhyme <- sadnessmodel$beta
# surprise
cv.fitsurprise <- cv.glmnet(as.matrix(surprisescore[,-1]), as.matrix(surprisescore[,1]), alpha=1, family = "binomial")
plot(cv.fitsurprise)
best.lambda <- cv.fitsurprise$lambda.min
surprisemodel <- glmnet(as.matrix(surprisescore[,-1]), as.matrix(surprisescore[,1]),alpha = 1,lambda = best.lambda, family = "binomial")
surprise_rhyme <- surprisemodel$beta 
# trust
cv.fittrust <- cv.glmnet(as.matrix(trustscore[,-1]), as.matrix(trustscore[,1]), alpha=1, family = "binomial")
plot(cv.fittrust)
best.lambda <- cv.fittrust$lambda.min
trustmodel <- glmnet(as.matrix(trustscore[,-1]), as.matrix(trustscore[,1]),alpha = 1,lambda = best.lambda, family = "binomial")
trust_rhyme <- trustmodel$beta 
### coef_matrix
coef_matrix <- cbind(anger_rhyme, anticipation_rhyme, disgust_rhyme, fear_rhyme,
                     joy_rhyme, sadness_rhyme, surprise_rhyme, trust_rhyme)
colnames(coef_matrix) <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")
coef_matrix <- as.matrix(coef_matrix)
lasso_coef <- as.data.frame(coef_matrix)
lasso_coef <- lasso_coef %>%
  mutate(rhyme = c("AA","AA -N","AA L","AA R","AE","AE -N","AE L","AH",
                  "AH -N","AH L","AO","AO -N","AO L","AO R","AW","AW -N",
                  "AY","AY -N","AY L","B","CH","D","DH","EH","EH -N","EH L",
                  "EH R","ER","ER -N","ER L","ER R","EY","EY -N","F","G",
                  "HH","IH","IH _N","IH L","IH R","IY","IY -N","IY L","IY R",
                  "JH","K","L","M","N","OW","OW -N","OW L","OY","P","R","S",
                  "SH","T","TH","UH","UW","UW -N","V","W","Y","Z","ZH"))
anger_coef <- lasso_coef %>%
  arrange(desc(abs(anger))) %>%
  select(anger,rhyme)
anger_top10_coef <- anger_coef[1:10,]

anticipation_coef <- lasso_coef %>%
  arrange(desc(abs(anticipation))) %>%
  select(anticipation,rhyme)
anticipation_top10_coef <- anticipation_coef[1:10,]

disgust_coef <- lasso_coef %>%
  arrange(desc(abs(disgust))) %>%
  select(disgust,rhyme)
disgust_top10_coef <- disgust_coef[1:10,]

fear_coef <- lasso_coef %>%
  arrange(desc(abs(fear))) %>%
  select(fear,rhyme)
fear_top10_coef <- fear_coef[1:10,]

joy_coef <- lasso_coef %>%
  arrange(desc(abs(joy))) %>%
  select(joy,rhyme)
joy_top10_coef <- joy_coef[1:10,]

sadness_coef <- lasso_coef %>%
  arrange(desc(abs(sadness))) %>%
  select(sadness,rhyme)
sadness_top10_coef <- sadness_coef[1:10,]

surprise_coef <- lasso_coef %>%
  arrange(desc(abs(surprise))) %>%
  select(surprise,rhyme)
surprise_top10_coef <- surprise_coef[1:10,]

trust_coef <- lasso_coef %>%
  arrange(desc(abs(trust))) %>%
  select(trust,rhyme)
trust_top10_coef <- trust_coef[1:10,]


##########randomForest##########
xscore <- Score.data$score.dist %>%
  mutate(value = 1) %>%
  spread(key = phnm, value = value) # sparse matrix of x
yscore <- songsent %>%
  mutate(value = 1) %>%
  spread(key = sentiment, value = value)# sparse matrix of y
score <- xscore %>%
  mutate(song = Song) %>% 
  select(-Song) %>%
  inner_join(yscore) %>%
  select(-year) %>%
  select(-serial) %>%
  unique()
score[is.na(score)] <- 0 

angerscore.rf <- cbind(score.rf[,72],score.rf[,3:69])
anticipationscore.rf <- cbind(score.rf[,73],score.rf[,3:69])
disgustscore.rf <- cbind(score.rf[,74],score.rf[,3:69])
fearscore.rf <- cbind(score.rf[,75],score.rf[,3:69])
joyscore.rf <- cbind(score.rf[,76],score.rf[,3:69])
sadnessscore.rf <- cbind(score.rf[,77],score.rf[,3:69])
surprisescore.rf <- cbind(score.rf[,78],score.rf[,3:69])
trustscore.rf <- cbind(score.rf[,79],score.rf[,3:69])
# anger
anger.rf = randomForest(x = as.matrix(angerscore.rf[,-1]), y = as.matrix(angerscore.rf[,1]), ntree = 50)
plot(anger.rf)
anger_imp <- round(importance(anger.rf),2)
# anticipation
anticipation.rf = randomForest(x = as.matrix(anticipationscore.rf[,-1]), y = as.matrix(anticipationscore.rf[,1]), ntree = 50)
plot(anticipation.rf)
anticipation_imp <- round(importance(anticipation.rf),2)
# disgust
disgust.rf = randomForest(x = as.matrix(disgustscore.rf[,-1]), y = as.matrix(disgustscore.rf[,1]), ntree = 50)
plot(disgust.rf)
disgust_imp <- round(importance(disgust.rf),2)
# fear
fear.rf = randomForest(x = as.matrix(fearscore.rf[,-1]), y = as.matrix(fearscore.rf[,1]), ntree = 50)
plot(fear.rf)
fear_imp <- round(importance(fear.rf),2)
# joy
joy.rf = randomForest(x = as.matrix(joyscore.rf[,-1]), y = as.matrix(joyscore.rf[,1]), ntree = 50)
plot(joy.rf)
joy_imp <- round(importance(joy.rf),2)
# sadness
sadness.rf = randomForest(x = as.matrix(sadnessscore.rf[,-1]), y = as.matrix(sadnessscore.rf[,1]), ntree = 50)
plot(sadness.rf)
sadness_imp <- round(importance(sadness.rf),2)
# surprise
surprise.rf = randomForest(x = as.matrix(surprisescore.rf[,-1]), y = as.matrix(surprisescore.rf[,1]), ntree = 50)
plot(surprise.rf)
surprise_imp <- round(importance(surprise.rf),2)
# trust
trust.rf = randomForest(x = as.matrix(trustscore.rf[,-1]), y = as.matrix(trustscore.rf[,1]), ntree = 50)
plot(trust.rf)
trust_imp <- round(importance(trust.rf),2)
###imp_rf
imp_rf <- cbind(anger_imp, anticipation_imp, disgust_imp, fear_imp, 
                joy_imp, sadness_imp, surprise_imp, trust_imp)
colnames(imp_rf) <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")
imp_rf <- as.data.frame(imp_rf)
imp_rf <- imp_rf %>%
  mutate(phnm = c("AA","AA -N","AA L","AA R","AE","AE -N","AE L","AH",
                  "AH -N","AH L","AO","AO -N","AO L","AO R","AW","AW -N",
                  "AY","AY -N","AY L","B","CH","D","DH","EH","EH -N","EH L",
                  "EH R","ER","ER -N","ER L","ER R","EY","EY -N","F","G",
                  "HH","IH","IH -N","IH L","IH R","IY","IY -N","IY L","IY R",
                  "JH","K","L","M","N","OW","OW -N","OW L","OY","P","R","S",
                  "SH","T","TH","UH","UW","UW -N","V","W","Y","Z","ZH"))
#top10 phnm for each sentiment
anger_desc <- imp_rf %>%
  arrange(desc(anger)) %>%
  select(anger,phnm)
anger_top10 <- anger_desc[1:10,]
anticipation_desc <- imp_rf %>%
  arrange(desc(anticipation)) %>%
  select(anticipation,phnm)
anticipation_top10 <- anticipation_desc[1:10,]
disgust_desc <- imp_rf %>%
  arrange(desc(disgust)) %>%
  select(disgust,phnm)
disgust_top10 <- disgust_desc[1:10,]
fear_desc <- imp_rf %>%
  arrange(desc(fear)) %>%
  select(fear,phnm)
fear_top10 <- fear_desc[1:10,]
joy_desc <- imp_rf %>%
  arrange(desc(joy)) %>%
  select(joy,phnm)
joy_top10 <- joy_desc[1:10,]
sadness_desc <- imp_rf %>%
  arrange(desc(sadness)) %>%
  select(sadness,phnm)
sadness_top10 <- sadness_desc[1:10,]
surprise_desc <- imp_rf %>%
  arrange(desc(surprise)) %>%
  select(surprise,phnm)
surprise_top10 <- surprise_desc[1:10,]
trust_desc <- imp_rf %>%
  arrange(desc(trust)) %>%
  select(trust,phnm)
trust_top10 <- trust_desc[1:10,]
