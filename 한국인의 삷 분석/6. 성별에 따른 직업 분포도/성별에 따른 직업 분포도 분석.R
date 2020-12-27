library(dplyr)
# 한국인의 삶 분석 - 성별에 따른 직업 분포도 분석

library(ggplot2)
library(descr)

# welfare 불러오기
load('welfare.rda')

# 남자 직업 분포도 상위 10개
job_male <- welfare %>% filter(!is.na(job) & sex=='male') %>% group_by(job) %>% summarise(n=n()) %>% arrange(desc(n)) %>% head(10)
job_male
# 남자 직업 분포도 상위 10개 그래프로 시각화
ggplot(job_male, aes(x=reorder(job,n), y=n)) + geom_col() + coord_flip()

# 여자 직업 분포도 상위 10개
job_female <- welfare %>% filter(!is.na(job) & sex=='female') %>% group_by(job) %>% summarise(n=n()) %>% arrange(desc(n)) %>% head(10)
job_female
# 여자 직업 분포도 상위 10개 그래프로 시각화
ggplot(job_female, aes(x=reorder(job,n), y=n)) + geom_col() + coord_flip()

# welfare 저장하기
save(welfare, file="welfare.rda")
