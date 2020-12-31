# 서울시 최근 10년간 구별 당구장 인가 분포

library(readxl)
library(dplyr)
library(ggplot2)

# 서울 당구장업 불러오기
dangu <- read_excel("c:/Rstudy/서울_당구장업.xlsx")
View(dangu)

# 결측치 제거
table(is.na(dangu$소재지전체주소))
table(is.na(dangu$인허가일자))
table(is.na(dangu$영업상태명))
dangu <- dangu %>% filter(!is.na(소재지전체주소)) 

# 영업/정상인 당구장의 소재지전체주소와 인허가일자 선택
dangu_ft <- subset(dangu, 영업상태='영업/정상', select=c(소재지전체주소, 인허가일자))

# 구주소만 남기기
dangu_ft$addr <- substr(dangu_ft$소재지전체주소, 7, 10)
dangu_ft$addr <- gsub("중구 \\w", "중구", dangu_ft$addr)
dangu_ft$addr <- gsub(" ", "", dangu_ft$addr)
table(dangu_ft$addr)

# 2010년 이상 연도만 남기기
dangu_ft$year <- substr(dangu_ft$인허가일자, 1, 4)
dangu_ft <- subset(dangu_ft, year >= '2010')
table(dangu_ft$year)

# 년도별로 정렬
dangu_ft$year <- factor(dangu_ft$year, levels = c(2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010))

# 연도별,구별 개수 구하기
dangu_ft_as <- dangu_ft %>% group_by(addr, year) %>% summarise(n=n())
dangu_ft_as

# 그래프로 시각화
ggplot(dangu_ft, aes(x=addr, fill=year)) + geom_bar() + ggtitle('서울시 최근 10년간 구별 당구장 인가현황') + xlab("당구장 소재지주소") + ylab("인가 현황")


ggplot(dangu_ft_as, aes(x=reorder(addr, -n), y=n, fill=year)) + geom_col() + ggtitle('서울시 최근 10년간 구별 당구장 인가현황') + xlab("당구장 소재지주소") + ylab("인가 분포 현황")

ggplot(dangu_ft_as, aes(x=n, y=reorder(addr, n), fill=year)) + geom_col() + ggtitle('서울시 최근 10년간 구별 당구장 인가현황') + xlab("인가 분포 현황") + ylab("당구장 소재지주소")
