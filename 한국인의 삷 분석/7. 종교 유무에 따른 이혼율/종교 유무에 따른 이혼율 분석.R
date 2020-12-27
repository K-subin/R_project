#====== 가설 : 종교 유무에 따라 이혼율은 다를 것이다. =======

library(dplyr)
library(ggplot2)
library(descr)
library(readxl)
library(foreign)

# 연령대 및 종교유무
load('welfare.rda')

# 데이터 형태 파악하기
str(welfare)
View(welfare)
View(codebook)

# 종교 - 1:있음 / 2:없음
welfare$religion <- ifelse(welfare$religion == 1, 'have_religion', ifelse(welfare$religion == 2, 'no_religion', NA))

# 혼인상태 - 1:결혼 / 3:이혼
welfare$group_marriage <- ifelse(welfare$marriage == 1, 'marriage', ifelse(welfare$marriage == 3, 'divorce', NA))

# 결측치 제거
rel_marriage <- welfare %>% select(group_marriage, religion) %>% filter(!is.na(group_marriage) & !is.na(religion))
View(rel_marriage)

# 이혼한 사람들을 종교유무에 따라 그룹화 한 후 그 수를 카운트
rel_div <- rel_marriage %>% filter(group_marriage == 'divorce') %>% group_by(religion) %>% summarise(divorce=n())
rel_div

# 배우가 있는 사람들을 종교유무에 따라 그룹화한 후 그 수를 카운트
rel_mar <- rel_marriage %>% filter(group_marriage == 'marriage') %>% group_by(religion) %>% summarise(marriage=n())
rel_mar

# 종교를 기준으로 가로 결합한다.
rel_mm <- full_join(rel_div, rel_mar, by='religion')
rel_mm

# 이혼율 구하기
rel_mm$rate <- rel_mm$divorce / (rel_mm$divorce + rel_mm$marriage) * 100
rel_mm

# 그래프를 통해 시각화
ggplot(rel_mm, aes(x=religion, y=rate)) + geom_col() + ylab('divorce rate (%)')

# welfare 저장하기
save(welfare, file="welfare.rda")

#===========다른 방법==========
# mutate를 사용해 바로 합치기
religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage) & !is.na(religion)) %>% 
  group_by(religion, group_marriage) %>%
  summarise(n=n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))
religion_marriage

divorce <- religion_marriage %>% filter(group_marriage == 'divorce')
divorce

ggplot(divorce, aes(x=religion, y=pct))+geom_col()
