# 한국인의 삶 분석 - 나이와 월급의 관계

library(foreign)
library(dplyr)
library(readxl)

# welfare 불러오기
load('welfare.rda')

#=== 1. 나이 변수
class(welfare$birth)
summary(welfare$birth)
# 결측치 처리
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
welfare <- welfare %>% filter(!is.na(birth))
# 나이 구하기
welfare$age <- 2018 - welfare$birth + 1
# 그래프로 시각화
welfare_age <- table(welfare$age)
barplot(welfare_age)

#=== 2. 나이 월급 평균 분석
age_income <- welfare %>% filter(!is.na(income)) %>% group_by(age) %>% summarise(mean_income = mean(income))
age_income
# 그래프로 시각화
plot(age_income$mean_income, xlab = "AGE", ylab = "MEAN_INCOME")

# welfare 저장하기
save(welfare, file="welfare.rda")

