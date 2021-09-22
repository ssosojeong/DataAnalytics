# 벡터
subject_name <- c("John", "Dunpy", "Steve")
temperature <- c(33.2, 33.0, 37.9)
flu_status <- c(FALSE, FALSE, TRUE)

# 벡터 구조 확인
is(temperature)
is(flu_status)
is(subject_name)

# 팩터
gender <- factor(c("MALE", "FEMALE", "MALE"), ordered = TRUE)
gender

blood <- factor(c("O", "AB", "A" ),
                levels = c("A", "B", "AB", "O"))
blood

# 리스트
John <- list(fullname = subject_name[1],
             temperature = temperature[1],
             flu_status = flu_status[1],
             gender = gender[1],
             blood = blood[1])
John

# 데이터프레임
people <- data.frame(subject_name,
                     temperature,
                     flu_status,
                     gender,
                     blood,
                     stringsAsFactors = FALSE
                     )
people

# 데이터 프레임 실습
people$subject_name
people[c("temperature", "flu_status")]

#2번째 열과 3번째 열의 예제 출력
people[2:3]

#1번째 행과 2번째 열
people[1,2]

#1,3번째 행 / 2,4번째 열
people[c(1,3), c(2,4)]

#1행에 대한 모든 정보 출력
people[1,]

#1열에 대한 모든 정보 출력
people[,1]

#모든 행과 열
people[,]

#2행과 3,5열의 정보 제외
people[-2, c(-3,-5)]

#2행과 3,5열의 정보 제외 view
View(people[-2, c(-3,-5)])


##### R데이터를 csv 저장하고 불러오기!!!!
write.csv(people, 
          file="first.csv")

#csv 불러오기
first <- read.csv(file = "first.csv", stringsAsFactors = FALSE)
first <- first[,-1]

#read.csv의 header 비교
first1 <- read.csv(file = "first.csv", stringsAsFactors = FALSE,
                   header=TRUE)
first2 <- read.csv(file = "first.csv", stringsAsFactors = FALSE,
                   header=FALSE)























