#====== 가설 : 종교 유무에 따라 이혼율은 다를 것이다. =======

library(dplyr)
library(ggplot2)
library(descr)
library(readxl)
library(foreign)

# welfare 불러오기
welfare <- read.spss(file = "Koweps_hpc10_2018_beta1.sav", to.data.frame = T)

# codebook 불러오기
codebook <- read_excel("c:/Rstudy/Koweps_Codebook.xlsx")

# 데이터 형태 파악하기
str(welfare)
View(welfare)
View(codebook)

# 이름 변경하기
welfare <- rename(welfare, marriage = h10_g10, religion = h10_g11)

# 종교와 결혼 선택
rel_marriage <- welfare %>% select(religion, marriage)
View(rel_marriage)

# 종교 - 1:있음 / 2:없음
rel_marriage$religion <- ifelse(rel_marriage$religion == 1, 'have_religion', ifelse(rel_marriage$religion == 2, 'no_religion', NA))

# 혼인상태 - 3:이혼 / 1,2,4:이혼안함
rel_marriage$marriage <- ifelse(rel_marriage$marriage == 3, 'divorce', ifelse(rel_marriage$marriage >= 1 & rel_marriage$marriage <= 4, 'no_divorce', NA))

# 결측치 제거
rel_marriage <- rel_marriage %>% filter(!is.na(marriage) & !is.na(religion))
View(rel_marriage)

# 이혼한 사람들을 종교유무에 따라 그룹화 한 후 그 수를 카운트
rel_div <- rel_marriage %>% filter(marriage == 'divorce') %>% group_by(religion) %>% summarise(divorce=n())
rel_div

# 배우가 있는 사람들을 종교유무에 따라 그룹화한 후 그 수를 카운트
rel_noDiv <- rel_marriage %>% filter(marriage == 'no_divorce') %>% group_by(religion) %>% summarise(no_divorce=n())
rel_noDiv

# 종교를 기준으로 가로 결합한다.
rel_mar <- full_join(rel_div, rel_noDiv, by='religion')
rel_mar

# 이혼율 구하기
rel_mar$rate <- rel_mar$divorce / (rel_mar$divorce + rel_mar$no_divorce) * 100
rel_mar

# 그래프를 통해 시각화
ggplot(rel_mar, aes(x=religion, y=rate)) + geom_col() + ylab('divorce rate (%)')


#===========다른 방법==========
# mutate를 사용해 바로 합치기
rel_mm <- rel_marriage %>% 
  filter(!is.na(marriage)) %>% 
  group_by(religion, marriage) %>% 
  summarise(count = n())  %>% 
  mutate(sum = sum(count))%>% 
  filter(marriage=='divorce') %>% 
  mutate(rate = count / sum) 
rel_mm

ggplot(rel_mm, aes(x=religion, y=rate)) + geom_col() + ylab('rate(%)')