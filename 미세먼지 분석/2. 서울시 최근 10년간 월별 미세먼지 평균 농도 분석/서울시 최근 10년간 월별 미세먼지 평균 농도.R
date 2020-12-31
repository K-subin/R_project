#=== 서울시 최근 10년간 월별 미세먼지 평균 농도 현황 ===
library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2)

# seoul_y10_finedust 불러오기
se_y10_dust <- read_excel("c:/Rstudy/seoul_y10_finedust.xlsx")
View(se_y10_dust)

# 월별 가로행 자료를 세로열로 변환
se_y10_dust2 <- melt(se_y10_dust, id.vars = 'year',
                     measure.vars = c('1월','2월','3월','4월','5월','6월','7월','8월','9월','10월','11월','12월'))
View(se_y10_dust2)

# 변수명 변경
se_y10_dust2 <- rename(se_y10_dust2, mon = variable, f_dust = value)
str(se_y10_dust2)

# 년도별로 정렬
se_y10_dust2$year <- factor(se_y10_dust2$year)
se_y10_dust3 <- se_y10_dust2 %>% arrange(year)
View(se_y10_dust3)

# 그래프로 시각화
ggplot(se_y10_dust3, aes(mon, f_dust, color=year, group=year))+geom_line() + ggtitle("서울시 최근 10년간 월별 미세먼지 평균 농도 현황") + xlab("날짜") + ylab("미세먼지 평균 농도")
