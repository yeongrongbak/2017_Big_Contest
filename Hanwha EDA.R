setwd("C:/Users/user/Desktop/Bigcon")

library(data.table)  # data loading

library(ggplot2)     # data EDA

## data loading
data_set=fread("Data_set.csv",stringsAsFactors = T)

## data check
dim(data_set)
str(data_set)
View(head(data_set))
data_set$TARGET=as.factor(data_set$TARGET)

## Insurance data description (#16~#51)

## Factor type data 

# OCCP_NAME_G (직업정보) 
levels(data_set$OCCP_NAME_G)           # "*", "1차산업 종사자", "2차산업 종사자", "3차산업 종사자", "NULL", "고소득 전문직" 등 18개 값
sort(table(data_set$OCCP_NAME_G), decreasing = T)

# LAST_CHLD_AGE (마지막 자녀 나이)
levels(data_set$LAST_CHLD_AGE)         # "0"(NULL), "14", "19", "24", "29" 등 15개 값
sort(table(data_set$LAST_CHLD_AGE), decreasing = T)

# MATE_OCCP_NAME_G (배우자 직업 정보)
levels(data_set$MATE_OCCP_NAME_G)      # "*", "1차산업 종사자" 등  직업 정보와 동일한 범주
sort(table(data_set$MATE_OCCP_NAME_G), decreasing = T)

# LT1Y_PEOD_RATE (최근 1년 보험료 연체율)
levels(data_set$LT1Y_PEOD_RATE)        # "0","10미만","20미만",...,"90미만","90이상"
sort(table(data_set$LT1Y_PEOD_RATE), decreasing = T)

## int type data 


boxplot(data_set$CUST_JOB_INCM~data_set$OCCP_NAME_G) #주부, 학생, 기타 추정치 낮음
boxplot(data_set$CUST_JOB_INCM[-which(data_set$CUST_JOB_INCM == 0)]~data_set$OCCP_NAME_G[-which(data_set$CUST_JOB_INCM == 0)])

# CUS_JOB_INCM (추정소득)
summary(data_set$CUST_JOB_INCM)                       # min:0 , median: 3600, mean: 2788, max:10000
boxplot(data_set$CUST_JOB_INCM)
table(data_set$CUST_JOB_INCM)                         # 소득 0인 사람 40338명

# 소득 0인 사람 제외
summary(data_set$CUST_JOB_INCM[-which(data_set$CUST_JOB_INCM == 0)]) # min:2400, median:4500, mean:4666, max:10000 
boxplot(data_set$CUST_JOB_INCM[-which(data_set$CUST_JOB_INCM == 0)]) 
table(data_set$CUST_JOB_INCM[-which(data_set$CUST_JOB_INCM == 0)])

  # 디폴트의 비율 확인
ggplot(data=data_set,aes(x=CUST_JOB_INCM,fill=TARGET)) + geom_bar(stat='count')   

  # 디폴트 비율 (소득 0 제외)
attach(data_set)
my.table <- xtabs(~ TARGET[-which(CUST_JOB_INCM==0)]+CUST_JOB_INCM[-which(CUST_JOB_INCM==0)])
my.table
barplot(my.table, xlab="INCOME", ylab="Frequency", legend.text=T, col= c("skyblue","red")) # 비율을 어떻게?


# HSHD_INFR_INCM(가구추정소득)
summary(data_set$HSHD_INFR_INCM)                       # min:0 , median: 6600, mean: 6922, max:20000
boxplot(data_set$HSHD_INFR_INCM)
table(data_set$HSHD_INFR_INCM)                         # 소득 0인 가구 3633가구

ggplot(data=data_set,aes(x=HSHD_INFR_INCM,fill=TARGET)) + geom_bar(stat='count')  # 가구 추정소득도 0인 경우가 가장 많고 2500~12500 사이에 대부분 분포 


# ACTL_FMLY_NUM(실가족원수)   
summary(data_set$ACTL_FMLY_NUM)                       # min:1 , median: 3, mean: 2.758, max:8
table(data_set$ACTL_FMLY_NUM)
barplot(table(data_set$ACTL_FMLY_NUM))                # 1~4인 가구가 대부분

ggplot(data=data_set,aes(x=ACTL_FMLY_NUM,fill=TARGET)) + geom_bar(stat='count')  # Default 비율이 거의 비슷


# CUST_FMLY_NUM(보험가입가족원수)
summary(data_set$CUST_FMLY_NUM)                       # min:1 , median: 1, mean: 1.393, max:5
table(data_set$CUST_FMLY_NUM)
barplot(table(data_set$CUST_FMLY_NUM))                # 실가족원수가 1~4에 골고루 분포하는데에 비해 보험가입가족원수는 1에 60%이상 분포
ggplot(data=data_set,aes(x=CUST_FMLY_NUM,fill=TARGET)) + geom_bar(stat='count')  


# MATE_JOB_INCM(배우자 추정소득)
summary(data_set$MATE_JOB_INCM)                       # min:0 , median: 0, mean: 1720, max:10000
boxplot(data_set$MATE_JOB_INCM)
table(data_set$MATE_JOB_INCM)                         # 소득이 0인 경우 빈도수 65774

  # 디폴트 비율
ggplot(data=data_set,aes(x=MATE_JOB_INCM,fill=TARGET)) + geom_bar(stat='count')  

  # 디폴트 비율(소득 0 제외)
my.table2 <- xtabs(~ TARGET[-which(MATE_JOB_INCM==0)]+MATE_JOB_INCM[-which(MATE_JOB_INCM==0)])
my.table2
barplot(my.table2, xlab="MATE INCOME", ylab="Frequency", legend.text=T, col= c("skyblue","red")) # 비율을 어떻게?


# CRDT_LOAN_CNT(신용대출건수)
summary(data_set$CRDT_LOAN_CNT)                       # min:0 , median: 0, mean: 0.1622, max:11
table(data_set$CUST_FMLY_NUM)
barplot(table(data_set$CUST_FMLY_NUM))                
ggplot(data=data_set,aes(x=CRDT_LOAN_CNT,fill=TARGET)) + geom_bar(stat='count') # 신용 대출 건수가 0인 경우ㅇ Default가 많음?

  # 디폴트 비율 (신용대출건수 0 제외)
my.table3 <- xtabs(~ TARGET[-which(CRDT_LOAN_CNT==0)]+CRDT_LOAN_CNT[-which(CRDT_LOAN_CNT==0)])
my.table3
barplot(my.table3, xlab="LOAN", ylab="Frequency", legend.text=T, col= c("skyblue","red")) # 비율을 어떻게?

#아래 값들의 경우 신용 대출 건수가 있는 값에 한하여 값이 존재함 (최초대출날짜~최근신용등급)

# MIN_CNTT_DATE(최초대출날짜) 

# TOT_CRLN_AMT (한화생명신용대출금액)
summary(data_set$TOT_CRLN_AMT[-which(data_set$TOT_CRLN_AMT == 0)]) # min:1000000, median:8000000, mean:12054420, max:101000000
boxplot(data_set$TOT_CRLN_AMT[-which(data_set$TOT_CRLN_AMT == 0)]) 
table(data_set$TOT_CRLN_AMT[-which(data_set$TOT_CRLN_AMT == 0)])
ggplot(data=data_set,aes(x=TOT_CRLN_AMT,fill=TARGET)) + geom_bar(stat='count') # 신용 대출 건수가 0인 경우에 Default가 많음?

# 디폴트 비율 (신용대출건수 0 제외)
my.table4 <- xtabs(~ TARGET[-which(TOT_CRLN_AMT==0)]+TOT_CRLN_AMT[-which(TOT_CRLN_AMT==0)])
my.table4
barplot(my.table4, xlab="HLOAN", ylab="Frequency", legend.text=T, col= c("skyblue","red")) 

# TOT_REPY_AMT (한화생명신용상환금액)

summary(data_set$TOT_REPY_AMT[-which(data_set$TOT_REPY_AMT == 0)]) # min:1000000, median:6000000, mean:10361424, max:101000000
boxplot(data_set$TOT_REPY_AMT[-which(data_set$TOT_REPY_AMT == 0)]) 
table(data_set$TOT_REPY_AMT[-which(data_set$TOT_REPY_AMT == 0)])
ggplot(data=data_set,aes(x=TOT_REPY_AMT,fill=TARGET)) + geom_bar(stat='count') 
  
  # 디폴트 비율 (신용상환건수 0 제외)
my.table5 <- xtabs(~ TARGET[-which(TOT_REPY_AMT==0)]+TOT_REPY_AMT[-which(TOT_REPY_AMT==0)])
my.table5
barplot(my.table5, xlab="HLOAN", ylab="Frequency", legend.text=T, col= c("skyblue","red")) # 비율을 어떻게?

# CRLN_OVDU_RATE (신용대출연체율)
summary(data_set$CRLN_OVDU_RATE[-which(data_set$CRLN_OVDU_RATE == 0)]) # min:1, median:40, mean:43.24, max:100
boxplot(data_set$CRLN_OVDU_RATE[-which(data_set$CRLN_OVDU_RATE == 0)]) 
table(data_set$CRLN_OVDU_RATE[-which(data_set$CRLN_OVDU_RATE == 0)])
ggplot(data=data_set,aes(x=CRLN_OVDU_RATE,fill=TARGET)) + geom_bar(stat='count') 

  # 디폴트 비율 (신용대출연체율 0 제외)
my.table6 <- xtabs(~ TARGET[-which(CRLN_OVDU_RATE==0)]+CRLN_OVDU_RATE[-which(CRLN_OVDU_RATE==0)])
my.table6
barplot(my.table6, xlab="DRATE", ylab="Frequency", legend.text=T, col= c("skyblue","red")) 

# CRLN_30OVDU_RATE (30일이내 신용대출연체율)
summary(data_set$CRLN_30OVDU_RATE[-which(data_set$CRLN_30OVDU_RATE == 0)]) # min:1, median:14, mean:22.51, max:100
boxplot(data_set$CRLN_30OVDU_RATE[-which(data_set$CRLN_30OVDU_RATE == 0)]) 
table(data_set$CRLN_30OVDU_RATE[-which(data_set$CRLN_30OVDU_RATE == 0)])
ggplot(data=data_set,aes(x=CRLN_30OVDU_RATE,fill=TARGET)) + geom_bar(stat='count') 

   # 디폴트 비율 (신용대출연체율 0 제외)
my.table7 <- xtabs(~ TARGET[-which(CRLN_30OVDU_RATE==0)]+CRLN_30OVDU_RATE[-which(CRLN_30OVDU_RATE==0)])
my.table7
barplot(my.table7, xlab="30DRATE", ylab="Frequency", legend.text=T, col= c("skyblue","red"))

# LT1Y_CLOD_RATE (최근 1년 신용대출연체율)
summary(data_set$LT1Y_CLOD_RATE[-which(data_set$LT1Y_CLOD_RATE == 0)]) # min:10, median:20, mean:31.53, max:100
boxplot(data_set$LT1Y_CLOD_RATE[-which(data_set$LT1Y_CLOD_RATE == 0)]) 
table(data_set$LT1Y_CLOD_RATE[-which(data_set$LT1Y_CLOD_RATE == 0)])
ggplot(data=data_set,aes(x=LT1Y_CLOD_RATE,fill=TARGET)) + geom_bar(stat='count') 

  # 디폴트 비율 (신용대출연체율 0 제외)
my.table8 <- xtabs(~ TARGET[-which(LT1Y_CLOD_RATE==0)]+LT1Y_CLOD_RATE[-which(LT1Y_CLOD_RATE==0)])
my.table8
barplot(my.table8, xlab="1YDRATE", ylab="Frequency", legend.text=T, col= c("skyblue","red"))

# STRT_CRDT_GRAD (최초신용등급)
summary(data_set$STRT_CRDT_GRAD[-which(data_set$STRT_CRDT_GRAD == 0)]) # min:1, median:4, mean:3.905, max:7
boxplot(data_set$STRT_CRDT_GRAD[-which(data_set$STRT_CRDT_GRAD == 0)]) 
table(data_set$STRT_CRDT_GRAD)
ggplot(data=data_set,aes(x=STRT_CRDT_GRAD,fill=TARGET)) + geom_bar(stat='count') 

  # 디폴트 비율 (최초신용등급 0 제외)
my.table9 <- xtabs(~ TARGET[-which(STRT_CRDT_GRAD==0)]+STRT_CRDT_GRAD[-which(STRT_CRDT_GRAD==0)])
my.table9
barplot(my.table9, xlab="STCRDT", ylab="Frequency", legend.text=T, col= c("skyblue","red"))

# LTST_CRDT_GRAD (최근신용등급)
summary(data_set$LTST_CRDT_GRAD[-which(data_set$LTST_CRDT_GRAD == 0)]) # min:1, median:6, mean:5.572, max:10
boxplot(data_set$LTST_CRDT_GRAD[-which(data_set$LTST_CRDT_GRAD == 0)]) 
table(data_set$LTST_CRDT_GRAD)
ggplot(data=data_set,aes(x=LTST_CRDT_GRAD,fill=TARGET)) + geom_bar(stat='count') 

  # 디폴트 비율 (최근신용등급 0 제외)
my.table10 <- xtabs(~ TARGET[-which(LTST_CRDT_GRAD==0)]+LTST_CRDT_GRAD[-which(LTST_CRDT_GRAD==0)])
my.table10
barplot(my.table10, xlab="LTCRDT", ylab="Frequency", legend.text=T, col= c("skyblue","red"))


#PREM_OVDU_RATE(보험료 연체율)
summary(data_set$PREM_OVDU_RATE) # min:0, median:3, mean:8.01, max:100
boxplot(data_set$PREM_OVDU_RATE) 
table(data_set$PREM_OVDU_RATE)
ggplot(data=data_set,aes(x=PREM_OVDU_RATE,fill=TARGET)) + geom_bar(stat='count') 

# AVG_STLN_RATE(평균약대율 : 약관 대출 가능 금액 중 약관 대출 받은 금액의 비율의 연중 평균)
summary(data_set$AVG_STLN_RATE) # min:0, median:0, mean:16.95, max:100
boxplot(data_set$AVG_STLN_RATE) 
table(data_set$AVG_STLN_RATE)
ggplot(data=data_set,aes(x=AVG_STLN_RATE,fill=TARGET)) + geom_bar(stat='count') # 두 그룹으로 나뉘어 있어 보임

  # 디폴트 비율 (평균약대율 0 제외)
my.table11 <- xtabs(~ TARGET[-which(AVG_STLN_RATE==0)]+AVG_STLN_RATE[-which(AVG_STLN_RATE==0)])
my.table11
barplot(my.table11, xlab="AVG_STLN_RATE", ylab="Frequency", legend.text=T, col= c("skyblue","red"))


# STLN_REMN_AMT(약관대출가능잔액)   
summary(data_set$STLN_REMN_AMT) # min:0, median:0, mean:1603045, max:101000000
boxplot(data_set$STLN_REMN_AMT) 
table(data_set$STLN_REMN_AMT)
ggplot(data=data_set,aes(x=STLN_REMN_AMT,fill=TARGET)) + geom_bar(stat='count') 

  # 디폴트 비율 (평균약대율 0 제외)
my.table12 <- xtabs(~ TARGET[-which(STLN_REMN_AMT==0)]+STLN_REMN_AMT[-which(STLN_REMN_AMT==0)])
my.table12
barplot(my.table12, xlab="STLN_REMN_AMT", ylab="Frequency", legend.text=T, col= c("skyblue","red"))

# LT1Y_STLN_AMT(최근1년약대금액)
summary(data_set$LT1Y_STLN_AMT) # min:0, median:0, mean:92579, max:101000000
boxplot(data_set$LT1Y_STLN_AMT) 
table(data_set$LT1Y_STLN_AMT)
ggplot(data=data_set,aes(x=LT1Y_STLN_AMT,fill=TARGET)) + geom_bar(stat='count') 

  # 디폴트 비율 (최근1년약대금액 0 제외)
my.table13 <- xtabs(~ TARGET[-which(LT1Y_STLN_AMT==0)]+LT1Y_STLN_AMT[-which(LT1Y_STLN_AMT==0)])
my.table13
barplot(my.table13, xlab="LT1Y_STLN_AMT", ylab="Frequency", legend.text=T, col= c("skyblue","red"))

# LT1Y_SLOD_RATE(최근1약대연체율)
summary(data_set$LT1Y_SLOD_RATE) # min:0, median:0, mean:2.584, max:100
boxplot(data_set$LT1Y_SLOD_RATE) 
table(data_set$LT1Y_SLOD_RATE)
ggplot(data=data_set,aes(x=LT1Y_SLOD_RATE,fill=TARGET)) + geom_bar(stat='count') 

  # 디폴트 비율 (최근1년약대금액 0 제외)
my.table14 <- xtabs(~ TARGET[-which(LT1Y_SLOD_RATE==0)]+LT1Y_SLOD_RATE[-which(LT1Y_SLOD_RATE==0)])
barplot(my.table14, xlab="LT1Y_SLOD_RATE", ylab="Frequency", legend.text=T, col= c("skyblue","red"))

# GDINS_MON_PREM(非연금저축상품월납입보험료)
summary(data_set$GDINS_MON_PREM) # min:0, median:50000, mean:127653, max:4000000
boxplot(data_set$GDINS_MON_PREM) 
table(data_set$GDINS_MON_PREM)
ggplot(data=data_set,aes(x=GDINS_MON_PREM,fill=TARGET)) + geom_bar(stat='count')

  # 디폴트 비율 (0 제외)
my.table15 <- xtabs(~ TARGET[-which(GDINS_MON_PREM==0)]+GDINS_MON_PREM[-which(GDINS_MON_PREM==0)])
barplot(my.table15, xlab="GDINS_MON_PREM", ylab="Frequency", legend.text=T, col= c("skyblue","red"))

#SVINS_MON_PREM(연금저축상품월납입보험료)	
summary(data_set$SVINS_MON_PREM) # min:0, median:0, mean:92047, max:4000000
boxplot(data_set$SVINS_MON_PREM) 
table(data_set$SVINS_MON_PREM)
ggplot(data=data_set,aes(x=SVINS_MON_PREM,fill=TARGET)) + geom_bar(stat='count')

  # 디폴트 비율 (0 제외)
my.table16 <- xtabs(~ TARGET[-which(SVINS_MON_PREM==0)]+SVINS_MON_PREM[-which(SVINS_MON_PREM==0)])
barplot(my.table16, xlab="SVINS_MON_PREM", ylab="Frequency", legend.text=T, col= c("skyblue","red"))

# FMLY_GDINS_MNPREM(非가구연금저축상품월납입보험료)	
summary(data_set$FMLY_GDINS_MNPREM) # min:0, median:80000, mean:184012, max:4000000
boxplot(data_set$FMLY_GDINS_MNPREM) 
table(data_set$FMLY_GDINS_MNPREM)
ggplot(data=data_set,aes(x=FMLY_GDINS_MNPREM,fill=TARGET)) + geom_bar(stat='count')

  # 디폴트 비율 (0 제외)
my.table17 <- xtabs(~ TARGET[-which(FMLY_GDINS_MNPREM==0)]+FMLY_GDINS_MNPREM[-which(FMLY_GDINS_MNPREM==0)])
barplot(my.table17, xlab="FMLY_GDINS_MNPREM", ylab="Frequency", legend.text=T, col= c("skyblue","red"))

# FMLY_SVINS_MNPREM(가구非연금저축상품월납입보험료)
summary(data_set$FMLY_SVINS_MNPREM) # min:0, median:0, mean:136436, max:10000000
boxplot(data_set$FMLY_SVINS_MNPREM) 
table(data_set$FMLY_SVINS_MNPREM)
ggplot(data=data_set,aes(x=FMLY_SVINS_MNPREM,fill=TARGET)) + geom_bar(stat='count')

  # 디폴트 비율 (0 제외)
my.table18 <- xtabs(~ TARGET[-which(FMLY_SVINS_MNPREM==0)]+FMLY_SVINS_MNPREM[-which(FMLY_SVINS_MNPREM==0)])
barplot(my.table18, xlab="FMLY_SVINS_MNPREM", ylab="Frequency", legend.text=T, col= c("skyblue","red"))

# MAX_MON_PREM(최대월납입보험료)
summary(data_set$MAX_MON_PREM) # min:0, median:190000, mean:373421, max:10000000
boxplot(data_set$MAX_MON_PREM) 
table(data_set$MAX_MON_PREM)
ggplot(data=data_set,aes(x=MAX_MON_PREM,fill=TARGET)) + geom_bar(stat='count')

# TOT_PREM(기납입보험료)
summary(data_set$TOT_PREM) # min:0, median:11000000, mean:20720000, max:1000000000
boxplot(data_set$TOT_PREM) 
table(data_set$TOT_PREM)
ggplot(data=data_set,aes(x=TOT_PREM,fill=TARGET)) + geom_bar(stat='count')

# FMLY_TOT_PREM(가구기납입보험료)
summary(data_set$FMLY_TOT_PREM) # min:0, median:15000000, mean:30770000, max:1000000000
boxplot(data_set$FMLY_TOT_PREM) 
table(data_set$FMLY_TOT_PREM)
ggplot(data=data_set,aes(x=FMLY_TOT_PREM,fill=TARGET)) + geom_bar(stat='count')

# CNTT_LAMT_CNT(실효해지건수)
summary(data_set$CNTT_LAMT_CNT) # min:0, median:0, mean:0.1154, max:7
boxplot(data_set$CNTT_LAMT_CNT) 
table(data_set$CNTT_LAMT_CNT)
ggplot(data=data_set,aes(x=CNTT_LAMT_CNT,fill=TARGET)) + geom_bar(stat='count')

  # 디폴트 비율 (0 제외)
my.table19 <- xtabs(~ TARGET[-which(CNTT_LAMT_CNT==0)]+CNTT_LAMT_CNT[-which(CNTT_LAMT_CNT==0)])
barplot(my.table19, xlab="CNTT_LAMT_CNT", ylab="Frequency", legend.text=T, col= c("skyblue","red"))

# LT1Y_CTLT_CNT(최근1년 실효해지건수)
summary(data_set$LT1Y_CTLT_CNT) # min:0, median:0, mean:0.02276, max:6
boxplot(data_set$LT1Y_CTLT_CNT) 
table(data_set$LT1Y_CTLT_CNT)
ggplot(data=data_set,aes(x=LT1Y_CTLT_CNT,fill=TARGET)) + geom_bar(stat='count')

  # 디폴트 비율 (0 제외)
my.table20 <- xtabs(~ TARGET[-which(LT1Y_CTLT_CNT==0)]+LT1Y_CTLT_CNT[-which(LT1Y_CTLT_CNT==0)])
barplot(my.table19, xlab="LT1Y_CTLT_CNT", ylab="Frequency", legend.text=T, col= c("skyblue","red"))

# AUTR_FAIL_MCNT(자동이체실패월수:산출일 기준 총 자동이체실패월수)
summary(data_set$AUTR_FAIL_MCNT) # min:0, median:0, mean:2.716, max:61
boxplot(data_set$AUTR_FAIL_MCNT) 
table(data_set$AUTR_FAIL_MCNT) # 자동이체 실패 월수가 0인 경우가 60%
ggplot(data=data_set,aes(x=AUTR_FAIL_MCNT,fill=TARGET)) + geom_bar(stat='count')

  # 디폴트 비율 (0 제외)
my.table21 <- xtabs(~ TARGET[-which(AUTR_FAIL_MCNT==0)]+AUTR_FAIL_MCNT[-which(AUTR_FAIL_MCNT==0)])
barplot(my.table21, xlab="AUTR_FAIL_MCNT", ylab="Frequency", legend.text=T, col= c("skyblue","red"))

# FYCM_PAID_AMT(가구총지급보험금액:가계 합산 보험금지급 총액)
summary(data_set$FYCM_PAID_AMT) # min:0, median:300000, mean:4039838, max:300100000
boxplot(data_set$FYCM_PAID_AMT) 
table(data_set$FYCM_PAID_AMT) # 0인 경우가 40%, 금액에 대한 범주화 필요?
ggplot(data=data_set,aes(x=FYCM_PAID_AMT,fill=TARGET)) + geom_bar(stat='count')

# FMLY_CLAM_CNT(가구총보험금청구건수:가계 합산 총 보험금청구 건수)
summary(data_set$FMLY_CLAM_CNT) # min:0, median:1, mean:4.218, max:171
boxplot(data_set$FMLY_CLAM_CNT) 
table(data_set$FMLY_CLAM_CNT) # 보험금 청구건수가 0인 가구가 45%이상
ggplot(data=data_set,aes(x=FMLY_CLAM_CNT,fill=TARGET)) + geom_bar(stat='count')

  # 디폴트 비율 (0 제외)
my.table22 <- xtabs(~ TARGET[-which(FMLY_CLAM_CNT==0)]+FMLY_CLAM_CNT[-which(FMLY_CLAM_CNT==0)])
barplot(my.table22, xlab="FMLY_CLAM_CNT", ylab="Frequency", legend.text=T, col= c("skyblue","red"))

# FMLY_PLPY_CNT (가구만기완납경험횟수:가구단위 만기까지 보험료를 완납한 증번의 갯수)
summary(data_set$FMLY_PLPY_CNT) # min:0, median:1, mean:1.235, max:22
boxplot(data_set$FMLY_PLPY_CNT) 
table(data_set$FMLY_PLPY_CNT)
ggplot(data=data_set,aes(x=FMLY_PLPY_CNT,fill=TARGET)) + geom_bar(stat='count')

