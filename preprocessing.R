## 불러오기
library(readxl)
RE <- read_excel("RealEstate.xlsx")
View(RE)

## ID 앞 숫자 제거
head(RE$ID)
library(stringr)
for (i in 0:9){
  RE$ID <- str_remove_all(RE$ID,as.character(i))
}
RE$ID 

## ID, block, floor 좌우 빈칸 없애기
library(stringr)
RE$ID <- str_trim(RE$ID, side = "right")
RE$ID <- str_trim(RE$ID, side = "left")
RE$block <- str_trim(RE$block, side = "right")
RE$floor <- str_trim(RE$floor, side = "right")
RE$ID
RE$block
RE$floor

## block 숫자 표현 맞추기
RE$block <- gsub('Block 1', 'Block 01',RE$block)
RE$block <- gsub('Block 2', 'Block 02',RE$block)
RE$block <- gsub('Block 3', 'Block 03',RE$block)
RE$block <- gsub('Block 4', 'Block 04',RE$block)
RE$block <- gsub('Block 5', 'Block 05',RE$block)
RE$block <- gsub('Block 6', 'Block 06',RE$block)
RE$block <- gsub('Block 7', 'Block 07',RE$block)
RE$block <- gsub('Block 8', 'Block 08',RE$block)
RE$block <- gsub('Block 9', 'Block 09',RE$block)
RE$block <- gsub('Block 3', 'Block 03',RE$block)
RE$block[1:15]

## 변수 이름 바꾸기
names(RE)[8] <- 'HK'
names(RE)[9] <- 'Saleable.area'
names(RE)[4] <- 'saleable.area'
names(RE)[5] <- 'gross.area'
names(RE)[6] <- 'efficiency.ratio'
names(RE)[10] <- 'Gross.area'
names(RE)[17] <- 'full.sea'
names(RE)

## factor로 변수 바꾸기
RE$sea <- as.factor(RE$sea)
RE$street <- as.factor(RE$street)
RE$factory <- as.factor(RE$factory)
RE$mountain <- as.factor(RE$mountain)
RE$landscape <- as.factor(RE$landscape)
RE$full.sea <- as.factor(RE$full.sea)
RE$bridge <- as.factor(RE$bridge)


## floor 변수를 factor로 변환
RE$floor <- as.factor(RE$floor)
levels(RE$floor) = c('Low Floor', 'Middle Floor', 'High Floor')
str(RE$floor)

## 변수별 결측치 개수 및 제거 -> date, factory 제거
na.sum <- rep(NA, 18)
names(na.sum) <- names(RE)
for (i in 1:18){
  na.sum[i] = sum(is.na(RE[,i]))
}
na.sum                  # block 7, floor 13, room 18

RE <- RE[,-14]               # factory 제거
RE <- RE[,-11]               # date 제거
RE <- na.omit(RE)            # n=579, p=16

summary(RE)                  

## full sea 와 sea 변수 합쳐서 sea로
a <-as.numeric(RE$sea)
b <-as.numeric(RE$full.sea)
sea <- as.factor(a+b-2)
RE$sea <- sea
RE <- RE[,-15]
RE$sea                      # n-579, p = 15
rm(a,b,sea)

## room 1,2,3 과 4개 이상으로 변환
RE$room[RE$room>3]=4
RE$room <- as.numeric(RE$room)
str(RE$room)
summary(RE)


## clustering : ID, block 변수와 가격변수 제외한 클러스터링 실시
library(dplyr)
library(cluster)
RE2 <- RE %>% select(-ID, -block, -HK, -Saleable.area, - Gross.area)
d <- daisy(RE2, metric='gower')
cl <- hclust(d)
plot(cl)
cluster <- cutree(cl,2)        # 2개 군집으분 분류
table(cluster)
cluster <- as.factor(cluster)
RE.c <- cbind(RE,cluster)      # data set에 cluster열 추가


#### 시각화 -> 군집별 특성 파악 ####
library(ggplot2)
library(gridExtra)
RE.c$room <- as.factor(RE.c$room)           # 시각화를 편하게 하기 위해 room 변수를 factor로 변환

par(mfrow=c(3,2))
## 범주형 변수 시각화
p1<-ggplot(RE.c, aes(x=cluster, fill=floor))+ geom_bar(position='fill')+ theme_bw()     # 군집별 floor 변수 비교
p2<-ggplot(RE.c, aes(x=cluster, fill=room))+ geom_bar(position='fill')+ theme_bw()      # 군집별 room 변수 비교
p3<-ggplot(RE.c, aes(x=cluster, fill=sea))+ geom_bar(position='fill')+ theme_bw()       # 군집별 sea 변수 비교
p4<-ggplot(RE.c, aes(x=cluster, fill=street))+ geom_bar(position='fill')+ theme_bw()    # 군집별 street 변수 비교
p5<-ggplot(RE.c, aes(x=cluster, fill=mountain))+ geom_bar(position='fill')+ theme_bw()  # 군집별 mountain 변수 비교
p6<-ggplot(RE.c, aes(x=cluster, fill=landscape))+ geom_bar(position='fill')+ theme_bw() # 군집별 landscape 변수 비교
p7<-ggplot(RE.c, aes(x=cluster, fill=bridge))+ geom_bar(position='fill')+ theme_bw()    # 군집별 bridge 변수 비교


## 연속형 변수 시각화
p8<-ggplot(RE.c, aes(x=cluster,y=saleable.area ,group=cluster)) +                       # 군집별 saleable.area 변수 비교
  geom_boxplot() + theme_bw()

p9<-ggplot(RE.c, aes(x=cluster,y=gross.area ,group=cluster)) +                           # 군집별 gross.area 변수 비교
  geom_boxplot() + theme_bw()

p10<-ggplot(RE.c, aes(x=cluster,y=efficiency.ratio ,group=cluster)) +                     # 군집별 efficiency.ratio 변수 비교
  geom_boxplot() + theme_bw()

p11<-ggplot(RE.c, aes(x=cluster,y=HK,group=cluster)) +                                    # 군집별 HK 변수 비교
  geom_boxplot() + theme_bw()

grid.arrange(p1, p2, p3, p4, p5, p6)                          # 군집 1과 군집 2는 모든 변수에서 확실한 차이가 난다.
grid.arrange(p7, p8, p9, p10, p11)


#### RE.c 에서 분석에 사용하지 않을 변수 제거 ####
# RE.c 는 HK까지 제거한 cluster 예측하기 위한 데이터 셋
# RE.c1은 HK 포함한 회귀를 위한 cluster 1 데이터 셋
# RE.c2는 HK 포함한 회귀를 위한 cluster 2 데이터 셋
RE.c1 <- RE.c %>% select(-ID, -block, -Saleable.area, - Gross.area) %>% filter(cluster==1)
RE.c2 <- RE.c %>% select(-ID, -block, -Saleable.area, - Gross.area) %>% filter(cluster==2)
RE.c <- RE.c %>% select(-ID, -block, -Saleable.area, - Gross.area, -HK)


#### RE.c data set을 이용해 클러스터 예측 모델 ####

# train set : test set 8:2 나누기
train <- sample(1:nrow(RE.c), nrow(RE.c)*0.8)
test <- -train
RE.c.train <- RE.c[train,]             # RE.c train set
RE.c.test <- RE.c[test,]               # RE.c test set
cluster.test <- RE.c$cluster[test]


# Classification Tree Model 사용 
library(tree)
tree.RE.c <- tree(cluster~., data=RE.c.train)
summary(tree.RE.c)
plot(tree.RE.c)
text(tree.RE.c, pretty=0)

# 모델의 test MSE 확인
tree.pred <- predict(tree.RE.c, newdata = RE.c.test, type='class')
table(tree.pred, cluster.test)
mean(tree.pred == cluster.test)    # 100% 분류


## 혹시 모르니 tree pruning -> pruning을 하지 않는 것이 좋다.
cv.RE.c <- cv.tree(tree.RE.c, FUN=prune.misclass)
cv.RE.c                  # 2개 터미널노드(size)를 가진 모델이 가장 낮은 cv error -> pruning 하지 않는 게 낫다.




#### RE.c1 과 RE.c2 각각에 대한 HK 예측모델 ####
summary(RE.c1)   # sea, full.sea 모두 0 
summary(RE.c2)   # sea모두 1, street, mountain,landscape,bridge 모두 0 

#### 군집 1에 대한 예측모델 적합 

# train set : test set 8:2 나누기
train <- sample(1:nrow(RE.c1), nrow(RE.c1)*0.8)
test <- -train
RE.c1.train <- RE.c1[train,]             # RE.c train set
RE.c1.test <- RE.c1[test,]               # RE.c test set
HK.test <- RE.c1$HK[test]

### RE.c1에 대한 회귀트리(배깅)
library(randomForest)
bag.RE.c1 <- randomForest(HK~., data=RE.c1.train, mytry=11, importance=TRUE)
bag.RE.c1
yhat.bag <- predict(bag.RE.c1, newdata=RE.c1.test)
plot(yhat.bag, HK.test)
abline(0,1, col='red', cex=3)
mean((yhat.bag - HK.test)^2)           # test MSE = 0.1677281

#### RE.c1에 대한 회귀트리(랜덤 포레스트)####
rf.RE.c1 <- randomForest(HK~., data=RE.c1.train, mtry=4, importance=TRUE)   # split별 고려할 변수 4개
yhat.rf <- predict(rf.RE.c1, newdata = RE.c1.test)
mean((yhat.rf - HK.test)^2)         # test MSE = 0.1568142  

rf.RE.c1 <- randomForest(HK~., data=RE.c1.train, mtry=5, importance=TRUE)   # split별 고려할 변수 5개
yhat.rf <- predict(rf.RE.c1, newdata = RE.c1.test)
mean((yhat.rf - HK.test)^2)         # 0.1589204 배깅보다 낫다. -> 트리 간 상관성이 제거되었기 때문


#### RE.c1에 대한 Lasso
library(glmnet)
x <- model.matrix(HK~., RE.c1)[,-1]
y <- RE.c1$HK
lasso.mod <- glmnet(x[train,], y[train], alpha=1)
plot(lasso.mod)

# CV 수행
cv.out <- cv.glmnet(x[train,],y[train], alpha=1)
plot(cv.out)
bestlambda <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s=bestlambda, newx = x[test,])
mean((lasso.pred - y[test])^2)      # 0.3229733

## 군집 1에 대하여 Random Forest 사용하기로 결정



#### 군집 2에 대한 예측모델 적합 


# train set : test set 7:3 나누기  --- 관측치 수가 너무 적다
train <- sample(1:nrow(RE.c2), nrow(RE.c2)*0.7)
test <- -train
RE.c2.train <- RE.c2[train,]             # RE.c train set
RE.c2.test <- RE.c2[test,]               # RE.c test set
HK.test <- RE.c2$HK[test]

#### RE.c2에 대한 회귀트리(배깅)####
bag.RE.c2 <- randomForest(HK~., data=RE.c2.train, mytry=11, importance=TRUE)
bag.RE.c2
yhat.bag <- predict(bag.RE.c2, newdata=RE.c2.test)
plot(yhat.bag, HK.test)
abline(0,1, col='red', cex=3)
mean((yhat.bag - HK.test)^2)           # test MSE = 6.620505 너무 높다

## RE.c2에 대한 회귀트리(랜덤 포레스트)
rf.RE.c2 <- randomForest(HK~., data=RE.c2.train, mtry=4, importance=TRUE)
yhat.rf <- predict(rf.RE.c2, newdata = RE.c2.test)
mean((yhat.rf - HK.test)^2)         # test MSE = 5.358317

rf.RE.c2 <- randomForest(HK~., data=RE.c2.train, mtry=5, importance=TRUE)
yhat.rf <- predict(rf.RE.c2, newdata = RE.c2.test)
mean((yhat.rf - HK.test)^2)         # test MSE = 5.462702  

rf.RE.c2 <- randomForest(HK~., data=RE.c2.train, mtry=8, importance=TRUE)
yhat.rf <- predict(rf.RE.c2, newdata = RE.c2.test)
mean((yhat.rf - HK.test)^2)         # 5.277079   배깅보다 낮은 성능

summary(RE.c2$HK)


## RE.c2는 회귀트리 배깅, 랜덤 포레스트가 잘 안맞는다. -> 부스팅해봄
install.packages('gbm')
library(gbm)
boost.RE.c2 <- gbm(HK~.-cluster-sea-street-mountain-landscape-bridge, data=RE.c2.train, distribution = 'gaussian', n.tree=100, interaction.depth = 4)
summary(boost.RE.c2)
yhat.boost <- predict(boost.RE.c2, newdata=RE.c2.test, n.trees=100)
mean((yhat.boost - HK.test)^2)      # 9.132027   배깅, 랜덤 포레스트 보다 낮은 성능


#### RE.c2에 대한 Lasso
library(glmnet)
x <- model.matrix(HK~., RE.c2)[,-1]
y <- RE.c2$HK
lasso.mod <- glmnet(x[train,], y[train], alpha=1)
plot(lasso.mod)

# CV 수행
cv.out <- cv.glmnet(x[train,],y[train], alpha=1)
plot(cv.out)
bestlambda <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s=bestlambda, newx = x[test,])
mean((lasso.pred - y[test])^2)      # tset MSE = 6.612063
out = glmnet(x,y,alpha=1)
lasso.coef <- predict(out, type='coefficients', s= bestlambda)
lasso.coef

#### 군집1에 대한 예측모델을 잘 맞으나, 군집 2에 대한 예측이 너무 부정확하다.
#### 데이터를 다시 살펴볼 필요가 있다.
write.csv(RE.c, file = 'preRE.csv', row.names=T)

RE