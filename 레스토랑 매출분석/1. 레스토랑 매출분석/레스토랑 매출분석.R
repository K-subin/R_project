# 레스토랑 매출분석

library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2)

# customer_r.xlsx : 고객정보
# reservation_r.xlsx : 주문예약/취소 정보
# order_info_r.xlsx : 주문상세정보
# item_r.xlsx : 메뉴상품정보
customer_r <- read_excel("c:/Rstudy/customer_r.xlsx")
reservation_r <- read_excel("c:/Rstudy/reservation_r.xlsx")
order_info_r <- read_excel("c:/Rstudy/order_info_r.xlsx")
item_r <- read_excel("c:/Rstudy/item_r.xlsx")

#=== 1. 고객별 방문횟수와 총 매출액 관계 분석 ===

# 주문예약/취소 정보와 주문상세정보 결합하기
cfm_order <- full_join(reservation_r, order_info_r, by='RESERV_NO')
# 필요한 변수 선택하기
df_cfm_order <- subset(cfm_order, select = c(CUSTOMER_ID, RESERV_NO, VISITOR_CNT, CANCEL, ORDER_NO, ITEM_ID, SALES))

# 고객별 방문횟수, 총 매출액 요약
df_sct_graph <- df_cfm_order %>% 
  filter(!is.na(SALES)) %>%
  group_by(CUSTOMER_ID) %>% 
  summarise(vst_cnt = sum(VISITOR_CNT), cust_amt = sum(SALES/1000))

# 그래프로 시각화
ggplot(df_sct_graph, aes(x=vst_cnt, y=cust_amt)) + geom_point() + xlim(0, 70) + ylim(0, 900) + ggtitle("레스토랑 매출분석") + xlab("고객별 방문횟수") + ylab("총 매출액")

#=== 2. 고객 성별 자료 추가하기 ===

# 고객정보와 결합하기
df_sct_graph2 <- left_join(df_sct_graph, customer_r, by='CUSTOMER_ID')
# 필요한 변수 선택하기
df_sct_graph2 <- subset(df_sct_graph2, !is.na(SEX_CODE), select=c(vst_cnt, cust_amt, SEX_CODE))

ggplot(df_sct_graph2, aes(x=vst_cnt, y=cust_amt, color=SEX_CODE)) + geom_point() + xlim(0, 50) + ylim(0, 600) + ggtitle("레스토랑 매출분석") + xlab("고객별 방문횟수") + ylab("총 매출액")
