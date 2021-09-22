## utilities.csv 필요
## 미국 전력회사의 데이터
## 9개 변수, 22개 데이터

## 0. setwd 경로 설정
setwd('/Users/kimsojeong/DA')

## 1. 데이터 구조 확인 및 전처리

#데이터 읽기
utilities.df <- read.csv("Utilities.csv")
View(utilities.df)

#Company 벡터 내용 복제하기
row.names(utilities.df) <- utilities.df[,1]
View(utilities.df)

#기존의 company 벡터 삭제
#1열(기존 company)를 제외하고 나머지를 다시 utilities.df에 넣겠다
utilities.df <- utilities.df[,-1]    
View(utilities.df)

#compute Euclidean distance
d <- dist(utilities.df, method="euclidean")

#normalize input variables (정규화)
utilities.df.norm <- sapply(utilities.df, scale)

#add row names: utilites (정규화 시킨 벡터에 추가)
row.names(utilities.df.norm) <- row.names(utilities.df)

#compute normalized distance based on variables sales and fuelcost
d.norm <- dist(utilities.df.norm[,c(6,8)], method = "euclidean")

## 2. 모델 구축

#compute normalized distance based on all 8 variables
d.norm <- dist(utilities.df.norm, method = "euclidean")

#hcluster를 이용하여 덴드로그램 설정 (hclust(data, method=""))
#거리 계산 방식: single(단일연결법) / complete(완전연결법)
#                average(평균연결법) / median(중심연결법)
hc1 <- hclust(d.norm, method = "single")
hc2 <- hclust(d.norm, method = "complete")
hc3 <- hclust(d.norm, method = "average")
hc4 <- hclust(d.norm, method = "median")

#하지만 그냥 hc1을 보면 뭐가 뭔지 알 수 없음(방법만 보임)
#plot으로 표현해줘야 함
#hang: variable name의 위치
#ann: 각주의 여부
plot(hc1, hang=-1, ann=FALSE)
plot(hc2, hang=-1, ann=FALSE)
plot(hc3, hang=-1, ann=FALSE)
plot(hc4, hang=-1, ann=FALSE)

plot(hc2, hang=-1, ann=FALSE)
#군집을 4개로 나누고 싶을 때 어디서 잘라야 할지 보여주는 함수 
rect.hclust(hc2,k=6)

#덴드로그램을 절단하여 멤버쉽(변수)으로 할당
memb <- cutree(hc2, k=6)
memb

## 3. 분석 결과 확인

#set labels as cluster membership and utility name
#"City" -> "memb: City" (ex. Florida를 1: Florida 이렇게 바꾼다)

#paste(문자열, 문자열, 문자열 ...) : 문자열을 합쳐서 하나의 문자열로 만드는 함수
paste("오늘","수업은","재미가","있다.", sep="!")
paste("오늘","수업은","재미가","있다.", sep="")

row.names(utilities.df.norm) <- paste(memb,": ",row.names(utilities.df.norm), sep="")

#Heatmap 설정
#heatmap 함수는 matrix만을 매개변수로 받기 때문에 as.matrix(데이터프레임) 필요
#Colv: heatmap 함수는 자동 클러스터링이 가능함/ NA: 쓰지 않겠다
heatmap(as.matrix(utilities.df.norm), Colv = NA, 
        hclustfun = hclust,
        col = rev(paste("grey",1:99,sep="")) #색깔을 grey1, grey2 이렇게 하겠다
        )
#히트맵을 통해서 클러스터 한 내용에 대해 어느 곳에서는 어떤 변수가 어느 정도고
#이런 걸 알 수 있음

