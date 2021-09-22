## snsdata.csv 필요
## 미국 10대 학생들의 소셜 네트워크 프로파일 데이터를 이용한 군집분석
## 30000명, 40개 변수

##0. setwd 경로 설정
setwd('/Users/kimsojeong/DA')

##1. 데이터 구조 확인

#데이터 읽기
teens <- read.csv("snsdata.csv")

#구조 확인
str(teens)

#gender 변수의 결측 데이터 확인
table(teens$gender)

#결측값을 포함할 수 있도록 ifany 작성
table(teens$gender, useNA = "ifany")

#age 변수의 결측 데이터 확인
#10대라고 했는데 Min과 Max가 이상함
summary(teens$age)

#age 이상치(outliers) 제거
teens$age <- ifelse(teens$age >= 13 & teens$age < 20,
                    teens$age, NA)

#age 변수의 데이터 확인
summary(teens$age)

#"unknown"인 성별값에 재부여
teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender),
                       1,0) #-> gender가 NA인 애들이랑 남자는 0이 됨

teens$no_gender <- ifelse(is.na(teens$gender),
                          1, 0)

#재지정한 작업에 대한 확인
table(teens$gender, useNA="ifany")
table(teens$female, useNA="ifany")
table(teens$no_gender, useNA="ifany")

#집단(cohort)별 나이 평균
mean(teens$age) #NA가 있어서 평균도 NA로 나옴
mean(teens$age, na.rm=TRUE)

#집단별 나이
aggregate(data=teens, age~gradyear, mean, na.rm=TRUE)

#각 개인에 대한 예측된 나이 계산
#ave 함수는 각 원소 값을 서브그룹의 평균으로 대체해주는 함수
#ave_age <- ave(teens$age, teens$gradyear) 
#얘도 다 NA나옴-> 따로 function 작성 필요
ave_age <- ave(teens$age, teens$gradyear,
               FUN=function(x) mean(x, na.rm=TRUE))
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

#제거한 결측치에 대한 요약 결과 확인
summary(teens$age)

## 2. Kmeans 모형 구축

#모델 만들기: myclusters <- kmeans(mydata, k) : mydata를 k개로 나눔

#다양한 관심사의 횟수를 표현하는 36개의 특징 추출
intrests <- teens[5:40]

#Data scaling -> 단위를 일정하게 맞춰준다는 느낌
interests_z <- as.data.frame(lapply(intrests, scale))

#시드 설정 -> 랜덤 값 부여할 때 코드 공유 시에도 fix된 값이 나옴 
set.seed(2345)

#자료에 대해 k=5로 나눠줌
teen_clusters <- kmeans(interests_z, 5)

#군집의 크기 확인
teen_clusters$size

## 3. 분석 결과 확인

#군집의 중앙점(centers) 확인
teen_clusters$centers

#본래 데이터 프레임에 군집ID 적용
teens$cluster <- teen_clusters$cluster

#처음 5개 데이터 확인
teens[1:5, c("cluster", "gender", "age", "friends")]

#군집별 평균 나이
aggregate(data=teens, age~cluster, mean)

#군집별 여성 비율
aggregate(data=teens, female~cluster, mean) 

#군집별 친구 수의 평균
aggregate(data=teens, friends~cluster, mean)


