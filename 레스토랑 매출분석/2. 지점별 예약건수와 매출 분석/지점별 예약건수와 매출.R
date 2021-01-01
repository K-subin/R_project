#===== 지점별 예약건수와 매출 분석 =====

library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2)

# 파일 불러오기
customer_r <- read_excel("c:/Rstudy/customer_r.xlsx")
reservation_r <- read_excel("c:/Rstudy/reservation_r.xlsx")
order_info_r <- read_excel("c:/Rstudy/order_info_r.xlsx")
item_r <- read_excel("c:/Rstudy/item_r.xlsx")

# 주문 취소되지 않은 예약건의 부서별 빈도표
no_cancel_data <- reservation_r %>% filter(CANCEL == 'N')
temp <- table(no_cancel_data$BRANCH) %>% data.frame()
arrange(temp, desc(Freq))

# 주문예약/취소 정보와 주문상세정보와 메뉴상품정보 결합
df_f_join_1 <- full_join(reservation_r, order_info_r, by='RESERV_NO')
df_f_join_2 <- full_join(df_f_join_1, item_r, by='ITEM_ID')

# 강남, 마포, 강서의 부서별 메뉴이름별 매출 합산 구하기
df_branch_sales <- df_f_join_2 %>% filter(BRANCH %in% c('강남', '마포', '강서')) %>% group_by(BRANCH, PRODUCT_NAME) %>% summarise(sales_amt = sum(SALES)/1000)
df_branch_sales

# 누적 막대 그래프로 시각화
ggplot(df_branch_sales, aes(x="", y=sales_amt, fill=PRODUCT_NAME)) + geom_bar(stat='identity') + facet_grid(facets = . ~ BRANCH) + ggtitle("지점별 예약건수와 매출 분석") + xlab("부서별") + ylab("매출 합산")

# 파이 차트로 시각화
ggplot(df_branch_sales, aes(x="", y=sales_amt, fill=PRODUCT_NAME)) + geom_bar(stat='identity') + facet_grid(facets = . ~ BRANCH) + coord_polar(theta='y') + ggtitle("지점별 예약건수와 매출 분석") + xlab("매출 합산") + ylab("부서별") 