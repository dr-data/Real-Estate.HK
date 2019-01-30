RE <- read.csv('RealEstate_merged.csv')          # Real_Estate 데이터에 City변수 추가한 data set

RE <- RE[,-c(1,2,3,4,5,6)]
summary(RE)                                      # n=573, p=13


#### City가 'Metro City', 'Hang Hau', 'Tseung Kwan O', 'Tiu Keng Wan', 'Yau Yue Wan' 인 행 선택
library(dplyr)
RE1 <- RE %>% filter(city == 'Metro City')
RE2 <- RE %>% filter(city == 'Hang Hau')
RE3 <- RE %>% filter(city == 'Tseung Kwan O')                     
RE4 <- RE %>% filter(city == 'Tiu Keng Wan')  
RE5 <- RE %>% filter(city == 'Yau Yue Wan')  

RE.f <- rbind(RE1, RE2, RE3, RE4, RE5)      
RE.f$sea <- as.factor(RE.f$sea)
RE.f$street <- as.factor(RE.f$street)
RE.f$mountain <- as.factor(RE.f$mountain)
RE.f$landscape <- as.factor(RE.f$landscape)
RE.f$bridge <- as.factor(RE.f$bridge)
RE.f <- RE.f %>% select(-Saleable.area, -Gross.area, -city) 

# 최종 분석을 위한 데이터 셋 - n:502, p=10
summary(RE.f)


#### RE.f에 대한 예측모델 ####
## train set, test set 분리
train <- sample(1:nrow(RE.f), nrow(RE.f)*0.8)
test <- -train
RE.train <- RE.f[train,]
RE.test <- RE.f[test,]
HK.test <- RE.f$HK[test]

## 회귀트리 Bagging 적합
library(randomForest)
bag.RE <- randomForest(HK~., data=RE.train, mtry=10, imporance=TRUE)
yhat.bag <- predict(bag.RE, newdata = RE.test)
mean((yhat.bag - HK.test)^2)               # test MSE = 0.05506751 --- 상당히 낮다!!

importance(bag.RE)                         # gross.area -> saleable.area -> efficiency.ratio -> sea -> floor  순의 변수 중요도

## 회귀트리 랜덤 포레스트  적합
rf.RE <- randomForest(HK~., data=RE.train, mtry=4, imporance=TRUE)
yhat.rf <- predict(rf.RE, newdata = RE.test)
mean((yhat.rf - HK.test)^2)               # test MSE = 0.04980042 --- 괜찮은 test MSE라고 판단!     

importance(rf.RE)                         # gross.area -> saleable.area -> room -> sea -> efficiency.ratio -> floor  순의 변수중요도
