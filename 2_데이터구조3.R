m1 <- matrix(c('a','b','c','d'), nrow = 2)
m1
m2 <- matrix(c('a','b','c','d'), ncol = 2)
m2
m3 <- matrix(c('a','b','c','d','e','f'), nrow = 3)
m3
m4 <- matrix(c('a','b','c','d','e','f'), ncol = 3)
m4

m4[1,1]
m4[2,3]
m3[1,]
m3[,1]

#matrix를 단일 파일로 저장하기
save(m1,m2,m3,m4,
     file = "mydata.RData")

load("mydata.RData")
