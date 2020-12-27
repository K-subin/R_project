# 한국인의 삶 분석 - 직업에 따른 월급 차이

library(dplyr)
library(ggplot2)
library(descr)
library(readxl)

# welfare 불러오기
load('welfare.rda')

#======== 직업별 월급 차이 분석
class(welfare$code_job)
table(welfare$code_job)

# 직업별 코드 확인
list_job <- read_excel("c:/Rstudy/Koweps_Codebook.xlsx", col_names = T, sheet = 2)
head(list_job)
dim(list_job)

# 직업분류코드의 job변수를 welfare에 결합
welfare <- left_join(welfare, list_job, by='code_job')
table(welfare$job)

# code_job, job 상위 10개 확인
welfare %>% filter(!is.na(code_job)) %>% select(code_job, job) %>% head(10)

# job과 income 묶기
job_income <- welfare %>% filter(!is.na(job) & !is.na(income)) %>% group_by(job) %>% summarise(mean_income = mean(income))
job_income

# 월급 내림차순 정렬, 상위 10개
top10 <- job_income %>% arrange(desc(mean_income)) %>% head(10)
top10
# 월급 상위 10위에 해당하는 직업에 대한 시각화
ggplot(top10, aes(x=job, y=mean_income)) + geom_col() + coord_flip()
# 월급 하위 10위에 해당하는 직업에 대한 시각화
bottom10 <- job_income %>% arrange(mean_income) %>% head(10)
bottom10
ggplot(bottom10, aes(x=job, y=mean_income, )) + geom_col() + coord_flip() + ylim(0, 850)

# welfare 저장하기
save(welfare, file="welfare.rda")
