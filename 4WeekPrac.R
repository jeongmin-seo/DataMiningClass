library(e1071)
train.ds = iris[,1:4]
train.cl = iris[,5]
test.ds = iris[,1:4]
test.cl = iris[,5]
model = naiveBayes(train.ds, train.cl)
pr = predict(model, test.ds)
acc = mean(pr==test.cl) # accuracy
print(acc)

# spam
setwd("/Users/jm/Desktop/datamining")
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)
dim(sms_raw)
str(sms_raw)

# 팩터로 spam/ham으로 변환
sms_raw$type <- factor(sms_raw$type)
# 변수형 확인
str(sms_raw$type)
table(sms_raw$type)
# 텍스트 마이닝(tm) 패키지를 사용하여 말뭉치(corpus) 생성
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))
# sms 말뭉치 확인
print(sms_corpus)
inspect(sms_corpus[1:3]) # 안됨
sms_corpus[[1]]$content
sms_corpus[[2]]$content

## tm_map() 사용하여 말뭉치 정리 -----------------
# 대문자는 소문자로
corpus_clean <- tm_map(sms_corpus,
                       content_transformer(tolower))
# 숫자 제거
corpus_clean <- tm_map(corpus_clean, removeNumbers)
# 불용어(to, and, but,…) 제거
corpus_clean <- tm_map(corpus_clean, removeWords,
                       stopwords())
# 마침표 제거
corpus_clean <- tm_map(corpus_clean,
                       removePunctuation)
# 보이지 않는 공백 제거
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

# 말뭉치 정리 확인
inspect(sms_corpus[1:3]) # 안됨
inspect(corpus_clean[1:3]) # 안됨
sms_corpus[[1]]$content
corpus_clean[[1]]$content
sms_corpus[[2]]$content
corpus_clean[[2]]$content
sms_corpus[[3]]$content
corpus_clean[[3]]$content

# 문서-용어 희소 매트릭스 생성
sms_dtm <- DocumentTermMatrix(corpus_clean)
sms_dtm

inspect(sms_dtm[1:10,20:30])

sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test <- sms_raw[4170:5559, ]
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]
sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5559]
# 스팸 비율 확인
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

library(wordcloud)
wordcloud(sms_corpus_train, min.freq = 30,
          random.order = FALSE)

# 훈련 데이터를 스팸과 햄으로 구분
spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_train, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3,
                                               0.5))
wordcloud(ham$text, max.words = 40, scale = c(3,
                                              0.5))


findFreqTerms(sms_dtm_train, 5) # 출현빈도 5회 이상 단어
#sms_dict <- Dictionary(findFreqTerms(sms_dtm_train,5)) ## Error
sms_dict <- findFreqTerms(sms_dtm_train, 5)
sms_train <- DocumentTermMatrix(sms_corpus_train,
                                list(dictionary = sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test,
                               list(dictionary = sms_dict))

# 개수를 팩터로 변환
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No",
                                              "Yes"))
}
# apply() convert_counts()를 사용한 훈련/테스트 데이터
추출
sms_train <- apply(sms_train, MARGIN = 2,
                   convert_counts)
sms_test <- apply(sms_test, MARGIN = 2,
                  convert_counts)
sms_train <- data.frame(sms_train)
sms_test <- data.frame(sms_test)

library(e1071)
sms_model <- naiveBayes(sms_train,
                        factor(sms_raw_train$type))
sms_model


sms_test_pred <- predict(sms_model, sms_test)
library(gmodels)
CrossTable(sms_test_pred, factor(sms_raw_test$type),
           prop.chisq = FALSE, prop.t = FALSE,
           prop.r = FALSE,
           dnn = c('predicted', 'actual'))
acc <- mean(sms_test_pred==factor(sms_raw_test$type))
acc

#---
sms_model2 <- naiveBayes(sms_train,
                         factor(sms_raw_train$type), laplace = 1)
sms_test_pred2 <- predict(sms_model2, sms_test)
CrossTable(sms_test_pred2, factor(sms_raw_test$type),
           prop.chisq = FALSE, prop.t = FALSE, prop.r
           = FALSE,dnn = c('predicted', 'actual'))
acc <- mean(sms_test_pred2==factor(sms_raw_test$type))
acc
