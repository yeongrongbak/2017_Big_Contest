setwd('C:/Users/user/Desktop/Bigcon')

data_set <- fread('data_set.csv', stringsAsFactors = T)

##library
library(data.table)  #data loading
library(ggplot2)     #data EDA


##factor type data
levels(data_set$LT1Y_PEOD_RATE)           #최근1년보험료연체율 0,10미만,...,90미만,90이상
levels(data_set$AGE)                      #나이정보 *,20,25,...,65,70
levels(data_set$SEX)                      #성별정보 *,1,2
levels(data_set$TEL_MBSP_GRAD)            #멤버쉽등급 "","E","Q","R","W"
levels(data_set$PAYM_METD)                #납부방법 "","G","K","O","R"


##data type check
str(data_set)
data_set$TARGET=as.factor(data_set$TARGET)


##credit data description

#BNK_LNIF_CNT(대출정보 현재 총 건수[은행])
summary(data_set$BNK_LNIF_CNT)                       #mean: 0.8402, median: 1, max=5, min=0
ggplot(data=data_set,aes(x=BNK_LNIF_CNT,fill=TARGET)) + geom_bar(stat='count')  #1회 대출자가 가장 많고 default비율은 0회에서 가장 높음
table(data_set$BNK_LNIF_CNT)


#CPT_LNIF_CNT(대출정보 현재 총 건수[카드사/할부사/캐피탈])
summary(data_set$CPT_LNIF_CNT)                       #mean: 0.5009, median: 0, max=5, min=0
ggplot(data=data_set,aes(x=CPT_LNIF_CNT,fill=TARGET)) + geom_bar(stat='count')  #0회 대출자가 절반 이상
table(data_set$CPT_LNIF_CNT)

#SPART_LNIF_CNT(대출정보 현재 총 건수[2산업분류])    #2금융권의 대출금액은 카드사 및 캐피탈사 등등 2금융권기관의 대출금액을 모두 포함한 금액
summary(data_set$SPART_LNIF_CNT)                     #mean: 0.953, med: 1, max=7, min=0
ggplot(data=data_set,aes(x=SPART_LNIF_CNT,fill=TARGET)) + geom_bar(stat='count')  #0회 대출자가 가장 많고 default는 1,2회에서 상대적으로 많이 발생 
table(data_set$SPART_LNIF_CNT)

#ECT_LNIF_CNT(대출정보 현재 총 건수[기타])
summary(data_set$ECT_LNIF_CNT)                       #mean: 0.478, med: 0, max=6, min=0
ggplot(data=data_set,aes(x=ECT_LNIF_CNT,fill=TARGET)) + geom_bar(stat='count')  #0회 대출자가 절반 이상
table(data_set$ECT_LNIF_CNT)

#TOT_LNIF_AMT(대출정보 현재 총 금액)
summary(data_set$TOT_LNIF_AMT)                       #mean: 84043, med: 39001 max=994001, min=1   (min값으로 범주화해서 min=1이됨)
ggplot(data=data_set,aes(x=TARGET,y=TOT_LNIF_AMT,fill=TARGET)) + geom_boxplot()  #상환을 한 사람들의 금액이 평균적으로 더 높음
table(data_set$TOT_LNIF_AMT)

#TOT_CLIF_AMT(대출정보 현재 총 금액[신용대출])
summary(data_set$TOT_CLIF_AMT)                       #mean: 33202, med: 9001, max=994001, min=0 
ggplot(data=data_set,aes(x=TARGET,y=TOT_CLIF_AMT,fill=TARGET)) + geom_boxplot()
table(data_set$TOT_CLIF_AMT)

#BNK_LNIF_AMT(대출정보 현재 총 금액[은행])
summary(data_set$BNK_LNIF_AMT)                       #mean: 51649, med: 9001, max=994001, min=0    
ggplot(data=data_set,aes(x=TARGET,y=BNK_LNIF_AMT,fill=TARGET)) + geom_boxplot()  #상환을 한 사람들의 금액이 평균적으로 더 높음

#CPT_LNIF_AMT(대출정보 현재 총 금액[카드사/할부사/캐피탈])
summary(data_set$CPT_LNIF_AMT)                       #mean: 4186, med: 0, max=301001, min=0
ggplot(data=data_set,aes(x=TARGET,y=CPT_LNIF_AMT,fill=TARGET)) + geom_boxplot()  #default 사람들의 금액이 더 높음

#CRDT_OCCR_MDIF(대출정보 최근 개설일로부터 현재까지 유지기간[신용대출])               #비식별화로 범주화 시킨 것 같음
summary(data_set$CRDT_OCCR_MDIF)                     #mean: 18.08, med: 1, max=121, min=0
ggplot(data=data_set,aes(x=CRDT_OCCR_MDIF,fill=TARGET)) + geom_bar(stat='count')

#SPTCT_OCCR_MDIF(대출정보 최근 개설일로부터 현재까지 유지기간[2산업분류-신용대출])    #비식별화로 범주화 시킨 것 같음
summary(data_set$SPTCT_OCCR_MDIF)                     #mean: 13.99, med: 0, max=121, min=0
ggplot(data=data_set,aes(x=SPTCT_OCCR_MDIF,fill=TARGET)) + geom_bar(stat='count')

#CRDT_CARD_CNT(개설정보 현재 신용개설 총 건수[신용카드])	
summary(data_set$CRDT_CARD_CNT)                       #mean: 3.092, med: 3, max=11, min=0
ggplot(data=data_set,aes(x=CRDT_CARD_CNT,fill=TARGET)) + geom_bar(stat='count')       #3개를 개설하는 비율이 가장 높았지만 default 사람들은 linear 감소 형태 보임

for (i in (0:10)) {
  a<-nrow(subset(data_set,TARGET=="1" & CRDT_CARD_CNT==i))/length(which(data_set$CRDT_CARD_CNT==i))
  print(a)
}

#신용카드가 0~1개인 경우 평균 이상의 연체율을 보임 -> 범주형으로 변수 변환?
#보험의 신용등급 데이터와 연관해서 인과관계 알아보

#CTCD_OCCR_MDIF(개설정보 최초 개설일로부터 현재까지 유지기간[신용카드])	              #비식별화로 범주화 시킨 것 같음
summary(data_set$CTCD_OCCR_MDIF)                       #mean: 91, med: 121, max=121, min=0
ggplot(data=data_set,aes(x=CTCD_OCCR_MDIF,fill=TARGET)) + geom_bar(stat='count')      #121(120초과의 모든값들의 집합으로 추정)값의 count가 과반수 이상

#CB_GUIF_CNT(보증정보 현재 보증 총 건수)
summary(data_set$CB_GUIF_CNT)                       #mean: 0.09626, med: 0, max=10, min=0
ggplot(data=data_set,aes(x=CB_GUIF_CNT,fill=TARGET)) + geom_bar(stat='count')         #보증을 하지 않은 경우가 대부분임(90000건이상), default 사람이면 0인경우가 많음(역은 성립x) 

#CB_GUIF_AMT(보증정보 현재 보증 총 금액)	
summary(data_set$CB_GUIF_AMT)                       #mean: 9183, med: 0, max=980001, min=0
ggplot(data=data_set,aes(x=TARGET,y=CB_GUIF_AMT,fill=TARGET)) + geom_boxplot()

length(which(data_set$CB_GUIF_AMT=="0"))/nrow(data_set)                 #전체 인원 중 보증 총 금액이 0인 비유  (0.944)

zero_true=which(data_set$TARGET=="1" & data_set$CB_GUIF_AMT==0)
length(zero_true)/length(which(data_set$TARGET=="1"))                   #default 중 보증 총 금액이 0인 비율    (0.9097)

zero_false=which(data_set$TARGET=="0" & data_set$CB_GUIF_AMT==0)
length(zero_false)/length(which(data_set$TARGET=="0"))                  #non-default 중 보증 총 금액이 0인 비율(0.9455)

length(which(data_set$TARGET=="1"))/length(which(data_set$CB_GUIF_AMT=="0"))  #보증 총 금액이 0인 사람들 중 default 비율 (0.045) -> 데이터 셋의 def비율이랑 비슷함

#보증 총 금액 비교(non-def vs def)
t.test(CB_GUIF_AMT~TARGET, data=data_set)                 #평균적으로 default의 보증 금액이 높다.  
tt1<-data_set[which(data_set$CB_GUIF_AMT!=0),]
t.test(CB_GUIF_AMT~TARGET, data=tt1)                      #보증을 한(CB_GUIF_AMT!=0) 사람들의 보증 금액 평균은 차이가 없다 


##신용 평가 컬럼의 상관관계
corr.cre.matrix = cor(data_set[, 3:16])

## 상관관계가 높은 feature 찾기                                         # 신용평가 테이블에서는 변수의 상관관계 높은게 없음
corr.cre.list = for (i in 1:nrow(corr.cre.matrix)) {
  rownames(corr.cre.matrix[abs(corr.cre.matrix[,i]) > 0.5,])
}