# 서울에서 치킨집이 가장 많은 5개구의 최근 20년간 인허가 현황을 분석

library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2)

# 서울치킨인허가 불러오기
se_ck <- read_excel("c:/Rstudy/서울치킨인허가.xlsx")
View(se_ck_ft)

# 결측치 제거
se_ck <- se_ck %>% filter(!is.na(소재지전체주소))
# 영업/정상인 치킨집의 인허가일자, 소재지전체주소 추출
se_ck_ft <- subset(se_ck, 영업상태명=='영업/정상', select=c(인허가일자, 소재지전체주소))

# 구만 남기기
se_ck_ft$addr <- substr(se_ck_ft$소재지전체주소, 7, 10)
se_ck_ft$addr <- gsub("중구 \\w", "중구", se_ck_ft$addr)
se_ck_ft$addr <- gsub(" ", "", se_ck_ft$addr)
table(se_ck_ft$addr)

# 년도만 남기기
se_ck_ft$year <- substr(se_ck_ft$인허가일자, 1, 4)
table(se_ck_ft$year)

# 치킨집이 가장 많은 5개구 구하기
chicken_s_5 <- se_ck_ft %>% group_by(addr) %>% summarise(n=n()) %>% arrange(desc(n)) %>% head(5)
chicken_s_5 <- chicken_s_5$addr
chicken_s_5

# 상위 5개구 중 2000년도 이상만 추출
se_ck_5_year <- subset(se_ck_ft, addr %in% chicken_s_5 & year >= 2000)

# 년도별,구별로 개수 세기
se_ck_year <- se_ck_5_year %>%
  group_by(year, addr) %>%
  summarise(인허가현황=n())
View(se_ck_year)

# 상위 5개 순서대로 정렬
se_ck_year$addr <- factor(se_ck_year$addr, levels = chicken_s_5)

# 그래프로 시각화
ggplot(se_ck_year, aes(year, 인허가현황, color=addr, group=addr)) + geom_line() + ggtitle("서울시 치킨집 개수 상위 5개구의 최근 20년간 인허가 현황")

