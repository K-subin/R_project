# 한국인의 삶 분석 - 성별에 따른 월급차이

library(foreign)
library(dplyr)
library(readxl)

# welfare 불러오기
raw_welfare <- read.spss(file = 'koweps_hpc10_2018_beta1.sav', to.data.frame = T)
welfare <- raw_welfare
View(welfare)

# codebook 불러오기
codebook<-read_excel('C:/Rstudy/Koweps_Codebook.xlsx')
View(codebook)

# 변수이름 바꾸기
welfare <- rename(welfare, sex = h10_g3, birth = h10_g4, marriage = h10_g10, religion = h10_g11, income = p1002_8aq1, code_job = h10_eco9, code_region = h10_reg7)

#1. 성별 변수
class(welfare$sex)
table(welfare$sex) # 1 남자 2 여자 9 무응답
# 결측치 처리
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)
welfare <- welfare %>% filter(!is.na(sex))
# 변수 이름 변경
welfare$sex <- ifelse(welfare$sex == 1, 'male', 'female')
# 그래프로 시각화
welfare_sex <- table(welfare$sex)
barplot(welfare_sex)

#2. 월급 변수
class(welfare$income)
summary(welfare$income)
# 결측치 처리
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)
income_tr <- welfare %>% filter(!is.na(income))

#3. 성별 월급 평균 분석 
sex_income <- income_tr %>% group_by(sex) %>% summarise(mean_income = mean(income))
sex_income
# 그래프로 시각화
barplot(sex_income$mean_income, xlab = "SEX", ylab = "MEAN_INCOME")

# welfare 저장
save(welfare, file="welfare.rda")
