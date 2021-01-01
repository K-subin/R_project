#===== 지점별 메뉴 아이템 주문 비율 =====

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

# 강남, 마포, 강서 추출
df_branch_items <- df_f_join_2 %>% filter(BRANCH %in% c('강남', '마포', '강서') & !is.na(PRODUCT_NAME))

# 교차빈도표 만들기
df_branch_items_table <-table(df_branch_items$BRANCH, df_branch_items$PRODUCT_NAME) %>% data.frame()

# 변수 이름 변경
df_branch_items_table <- rename(df_branch_items_table, '지점' = Var1, '메뉴아이템' = Var2)

# 주문 비율을 계산해서 백분율로 만들기
df_branch_items_group <- df_branch_items_table %>%
  group_by(지점) %>%
  mutate(비율 = (Freq/sum(Freq)) * 100) 

# 지점별 각 메뉴 아이템의 주문비율
ggplot(df_branch_items_group, aes(x=지점, y=비율, fill=메뉴아이템)) + geom_bar(stat='identity') + ggtitle("지점별 주문 건수 그래프") + ylab("메뉴 아이템 판매 비율")