#===== 2019년의 월별 지하철역(1~9호선)이용객 수 상위 5개역 분석 =====

library(dplyr)
library(ggplot2)
library(descr)

# 2019년 데이터 불러오기
subway1 <- read.csv('c:/Rstudy/subway/CARD_SUBWAY_MONTH_201901.csv')
subway2 <- read.csv('c:/Rstudy/subway/CARD_SUBWAY_MONTH_201902.csv')
subway3 <- read.csv('c:/Rstudy/subway/CARD_SUBWAY_MONTH_201903.csv')
subway4 <- read.csv('c:/Rstudy/subway/CARD_SUBWAY_MONTH_201904.csv')
subway5 <- read.csv('c:/Rstudy/subway/CARD_SUBWAY_MONTH_201905.csv')
subway6 <- read.csv('c:/Rstudy/subway/CARD_SUBWAY_MONTH_201906.csv')
subway7 <- read.csv('c:/Rstudy/subway/CARD_SUBWAY_MONTH_201907.csv')
subway8 <- read.csv('c:/Rstudy/subway/CARD_SUBWAY_MONTH_201908.csv')
subway9 <- read.csv('c:/Rstudy/subway/CARD_SUBWAY_MONTH_201909.csv')
subway10 <- read.csv('c:/Rstudy/subway/CARD_SUBWAY_MONTH_201910.csv')
subway11 <- read.csv('c:/Rstudy/subway/CARD_SUBWAY_MONTH_201911.csv')
subway12 <- read.csv('c:/Rstudy/subway/CARD_SUBWAY_MONTH_201912.csv')

# 데이터 형태 파악하기
head(subway)
View(subway1)
str(subway1$노선명)

# 데이터 결합하기
subway <- bind_rows(subway1, subway2, subway3, subway4, subway5, subway6, subway7, subway8, subway9, subway10, subway11, subway12)

# 1~9호선 추출하기
subway <- subset(subway, 노선명 %in% c('1호선','2호선','3호선','4호선','5호선','6호선','7호선','8호선','9호선'))

# 상위 5개역 추출하기
# 이용객 수 = 승차총승객수 + 하차총승객수
subway$tot <- subway$승차총승객수 + subway$하차총승객수
st5 <- subway %>% group_by(역명) %>% summarise(tot=sum(tot)) %>% arrange(desc(tot)) %>% head(5)
st5

# 상위 5개역명 구하기
st5_name <- st5$역명
st5_name

# 사용일자 데이터 이름 변경
subway$사용일자 <- substr(subway$사용일자, 5, 6)

subway$사용일자 <- ifelse(subway$사용일자 == '01', 'Jan', 
                      ifelse(subway$사용일자 == '02', 'Feb', 
                      ifelse(subway$사용일자 == '03', 'Mar', 
                      ifelse(subway$사용일자 == '04', 'Apr', 
                      ifelse(subway$사용일자 == '05', 'May', 
                      ifelse(subway$사용일자 == '06', 'Jun', 
                      ifelse(subway$사용일자 == '07', 'Jul', 
                      ifelse(subway$사용일자 == '08', 'Aug', 
                      ifelse(subway$사용일자 == '09', 'Sep', 
                      ifelse(subway$사용일자 == '10', 'Oct', 
                      ifelse(subway$사용일자 == '11', 'Nov', 'Dec')))))))))))

# 월별 이용객 수를 구하기 위해 사용일자, 역명별로 그룹화한 후 데이터를 요약한다.
month_st5 <- subway %>% filter(역명 %in% st5_name) %>% group_by(사용일자, 역명) %>% summarise(tot=sum(tot))
View(month_st5)
str(month_st5)

# 역명은 이용객수가 많은 순으로, 사용일자는 날짜 순으로 정렬한다.
month_st5$역명 <- factor(month_st5$역명, levels = st5_name)
month_st5$사용일자 <- factor(month_st5$사용일자, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

# 그래프를 통해 분석결과 시각화
ggplot(month_st5, aes(x=사용일자, y=tot, fill=역명)) + geom_col() + xlab('month') + scale_y_continuous(labels = scales::comma)