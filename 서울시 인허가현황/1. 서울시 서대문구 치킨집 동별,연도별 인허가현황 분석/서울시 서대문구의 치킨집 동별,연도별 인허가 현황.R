# 서울시 서대문구의 치킨집 동별,연도별 인허가 현황

library(readxl)
library(dplyr)
library(ggplot2)

# 치킨집 가공 불러오기
ck <- read_excel("c:/Rstudy/치킨집_가공2.xlsx")
head(ck)
table(is.na(ck$소재지전체주소))
table(is.na(ck$인허가일자))

# 동주소만 남기기
ck$소재지전체주소 <- substr(ck$소재지전체주소, 12, 16)
ck$소재지전체주소 <- gsub("[0-9]", "", ck$소재지전체주소)
ck$소재지전체주소 <- gsub(" ", "", ck$소재지전체주소)

# 인허가 년도만 남기기
ck$인허가일자 <- substr(ck$인허가일자, 1, 4)
head(ck)

# 연도별,동별 개수 구하기
seodaemon_year_count <- ck %>% group_by(소재지전체주소, 인허가일자) %>% summarise(n=n())
seodaemon_year_count

# 그래프로 시각화하기
ggplot(seodaemon_year_count, aes(x=n, y=reorder(소재지전체주소, n), fill=인허가일자)) + geom_col() + ggtitle("치킨집 연도별,동별 인허가 현황") + ylab("치킨집 소재지주소") + xlab("인허가 현황")
