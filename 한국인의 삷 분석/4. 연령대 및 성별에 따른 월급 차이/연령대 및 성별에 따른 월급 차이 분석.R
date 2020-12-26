#======== 연령대 및 성별에 따른 월급 차이 분석 ========

library(descr)
library(dplyr)

# welfare 불러오기
load('welfare.rda')

# 연령대 및 성별 월급 차이
# ageg와 sex로 2단계 그룹화
ageg_sex_income <- welfare %>% filter(!is.na(income)) %>% group_by(ageg, sex) %>% summarise(mean_income = mean(income))
ageg_sex_income
# 그래프로 시각화
ggplot(ageg_sex_income, aes(x=ageg, y=mean_income, fill=sex))+geom_col(position = 'dodge')+scale_x_discrete(limits=c('young', 'middle','old'))

# 나이 및 성별 월급 차이 분석
sex_age <- welfare %>% filter(!is.na(income)) %>% group_by(age, sex) %>% summarise(mean_income = mean(income))
sex_age
# 그래프로 시각화
ggplot(sex_age, aes(x=age, y=mean_income, col=sex))+geom_line()

save(welfare, file = 'welfare.rda')
