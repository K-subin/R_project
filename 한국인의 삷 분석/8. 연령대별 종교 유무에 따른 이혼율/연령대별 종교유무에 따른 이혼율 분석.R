# 연령대별 종교유무에 따른 이혼율 분석

library(dplyr)
library(ggplot2)
library(descr)

# welfare 불러오기
load('welfare.rda')

table(welfare$ageg)
table(welfare$group_marriage)

# 연령대별 이혼율 구하기 
ageg_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  count(ageg, group_marriage) %>%
  group_by(ageg) %>%
  mutate(pct = round(n/sum(n)*100, 1))
ageg_marriage

# 이혼한 사람 선택
# young은 너무 자료가 부족하기 때문에 제외
ageg_divorce <- ageg_marriage %>%
  filter(ageg != 'young' & group_marriage == 'divorce')
ageg_divorce

# 그래프로 시각화
ggplot(ageg_divorce, aes(x=ageg, y=pct))+geom_col()


# 연령대 및 종교유무에 따른 이혼율 구하기
ageg_religion_marriage <- welfare %>%
  filter(!is.na(group_marriage) & ageg != 'young') %>%
  count(ageg, religion, group_marriage) %>%
  group_by(ageg, religion) %>%
  mutate(pct = round(n/sum(n)*100, 1))
ageg_religion_marriage

# 이혼한 사람 선택
df_divorce <- ageg_religion_marriage %>%
  filter(group_marriage == 'divorce')
df_divorce

# 그래프로 시각화
ggplot(df_divorce, aes(x=ageg, y=pct, fill=religion))+geom_col(position = 'dodge')

# welfare 저장하기
save(welfare, file="welfare.rda")
