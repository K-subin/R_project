#===== 지역별 미세먼지 농도 비교하기 ===== 

library(readxl)
library(dplyr)
library(psych)

# dustdata 불러오기
dustdata <- read_excel("c:/Rstudy/dustdata.xlsx")

# 성북구, 중구만 추출
dustdata_anal <- dustdata %>% filter(area %in% c("성북구", "중구"))
View(dustdata_anal)

# 추출한 지역의 2달간 데이터가 모두 포함되어 있는지 확인
count(dustdata_anal, yyyymmdd) %>% arrange(desc(n))
# 두 지역의 미세먼지 수치가 모두 포함되어 있는지 확인
count(dustdata_anal, area) %>% arrange(desc(n))

# 성북구와 중구로 나누기
dustdata_anal_area_sb <- subset(dustdata_anal, area == '성북구')
dustdata_anal_area_jg <- subset(dustdata_anal, area == '중구')

# 구별 미세먼지 기초 통계량 분석
describe(dustdata_anal_area_sb$finedust)
describe(dustdata_anal_area_jg$finedust)

# 성북구, 중구의 미세먼지 분포 차이 시각화
boxplot(dustdata_anal_area_sb$finedust,
        dustdata_anal_area_jg$finedust,
        main = "finedust_compare", xlab="AREA",
        names = c("성북구", "중구"),
        ylab="FINEDUST_PM", col=c("blue", "green"))
