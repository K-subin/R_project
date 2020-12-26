#======== 연령대에 따른 월급차이 분석 ========
library(descr)
library(dplyr)

# welfare 불러오기
load('welfare.rda')
table(welfare$age)

#=== 1. 나이변수로 연령대 변수 만들기
# 연령대별 분류
welfare$ageg <- ifelse(welfare$age < 30, 'young', ifelse(welfare$age <= 59, 'middle', 'old'))
# 그래프로 시각화 
welfare_age <- table(welfare$ageg)
barplot(welfare_age)

#=== 2. 연령대에 따른 월급 분석
ageg_income <- welfare %>% filter(!is.na(income)) %>% group_by(ageg) %>% summarise(mean_income = mean(income))
# 그래프로 시각화
ggplot(ageg_income, aes(x=ageg, y=mean_income))+geom_col() + scale_x_discrete(limits=c('young','middle','old'))

save(welfare, file = 'welfare.rda')
