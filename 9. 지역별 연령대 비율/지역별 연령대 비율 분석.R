# 지역별 연령대 비율 분석
# 가설 : 노년층이 많은 지역은 어디일까?

library(dplyr)
library(ggplot2)
library(descr)

# welfare 불러오기
load('welfare.rda')

# 지역 이름 변경
table(welfare$code_region)
list_region <- data.frame(code_region = c(1:7),
                          region = c('서울', '수도권(인천/경기)', '부산/경남/울산', '대구/경북', '대전/충남', '강원/충북', '광주/전남/전북/제주도'))
list_region

# welfare와 list_region 합치기
welfare <- left_join(welfare, list_region, id='code_region')

# 지역별 연령대 비율 구하기
region_ageg <- welfare %>% 
  count(region, ageg) %>%
  group_by(region) %>%
  mutate(pct = round(n/sum(n)*100, 2))
region_ageg

# old가 많은 순으로 정렬
list_order_old <- region_ageg %>% filter(ageg=='old') %>% arrange(pct)
order <- list_order_old$region

# old, middle, young 순서로 정렬
region_ageg$ageg <- factor(region_ageg$ageg, levels = c('old', 'middle', 'young'))
levels(region_ageg$ageg)

# 그래프로 시각화
ggplot(region_ageg, aes(x=region, y=pct, fill=ageg))+geom_col()+coord_flip()+scale_x_discrete(limits=order)

