usedcars <- read.csv(file="usedcars.csv", stringsAsFactors = FALSE)
str(usedcars)

summary(usedcars$year)
summary(usedcars[c("year","mileage")])

(36000+44000+56000)/3
mean(c(36000, 44000, 56000))
median(c(36000, 44000, 56000))

range(usedcars$price)
diff(range(usedcars$price))

IQR(usedcars$price) #Q3-Q1

quantile(usedcars$price)
quantile(usedcars$price, probs = c(0.01, 0.99)) #0.01%와 0.99%의 값 
quantile(usedcars$price, seq(from=0, to=1, by=0.2)) #0 0.2, 0.4, 0.6, 0.8, 1.0의 값

boxplot(usedcars$price,
        main = "Boxplot of used Car Price",
        ylab="price($)")

boxplot(usedcars$mileage,
        main = "Boxplot of used Car Mileage",
        ylab="Odometer(mi.)")

hist(usedcars$price,
        main = "Histogram of used Car Price",
        ylab="price($)")

hist(usedcars$mileage,
     main = "Histogram of used Car Mileage",
     ylab="Odometer(mi.)")

var(usedcars$price)
sd(usedcars$price)
var(usedcars$mileage)
sd(usedcars$mileage)

table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

mt <- table(usedcars$model)
prop.table(mt)

rmt <- prop.table(mt) *100
rmt
round(rmt, digits = 1)

plot(x = usedcars$mileage,
     y = usedcars$price,
     main = "Scatterplot of Price vs Mileage",
     xlab = "used Cars Odometer(mi.)",
     ylab = "used Cars Price($)")

#install.packages("gmodels") -> 과제 제출할 때 반드시 주석처리하기 
library(gmodels)

usedcars$conservative <- usedcars$color %in% c("Black", "Gray",
                                                "Silver", "White")
table(usedcars$conservative)
CrossTable(x = usedcars$model,
           y = usedcars$conservative)





























