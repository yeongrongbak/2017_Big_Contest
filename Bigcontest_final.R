#package
library(h2o)
library(caret)
library(ROCR)

#setwd("C:\\Users\\lifebloom\\Desktop\\loan payment")
setwd("C:\\Users\\kis\\Desktop\\lib")

################################################
####################NEW E D A #####################
################################################

### 데이터 불러오기 

origin_data = read.csv('challenge_data\\Data_set.csv',fileEncoding='euc-kr',stringsAsFactors = TRUE)
origin_data <- origin_data[origin_data$BNK_LNIF_CNT+origin_data$CPT_LNIF_CNT+origin_data$SPART_LNIF_CNT+origin_data$ECT_LNIF_CNT!=0,] # 대출 합계가 0인 행삭제
origin_data <- origin_data[-which(origin_data$ARPU==-1),]
data_set <- origin_data
### 데이터 종류별 분할

sci_data <- origin_data[,c(2,3:16)]
in_data <- origin_data[,c(1,2,53,54,17:52)]
ph_data <- origin_data[,c(1,2,53,54,55:69)]

### SCI_DATA EDA
print(colnames(sci_data))

## BNK_LNIF_CNT
BNK_LNIF_CNT <- sci_data$BNK_LNIF_CNT
table(sci_data$BNK_LNIF_CNT)
plot(prop.table(table(sci_data$BNK_LNIF_CNT)),t="l")
# 1일 0.5의 비율로 가장많고 1부터 커질 때 마다 비율이 줄어듬.

plot(prop.table(table(sci_data$BNK_LNIF_CNT[sci_data$TARGET==1])),t="l")
points(prop.table(table(sci_data$BNK_LNIF_CNT[sci_data$TARGET==0])),t="l",col="red")
prop.table(table(sci_data$BNK_LNIF_CNT,sci_data$TARGET),1)
prop.table(table(sci_data$BNK_LNIF_CNT,sci_data$TARGET),2)

# TARGET==1 일 때는 전체와 다르게 0일 때 약 0.6에 가까운 값을 나타냄
#CPT_LNIF_CNT
CPT_LNIF_CNT <- sci_data$CPT_LNIF_CNT
table(CPT_LNIF_CNT)
plot(prop.table(table(CPT_LNIF_CNT)),t="l")

# 1일 0.5의 비율로 가장많고 1부터 커질 때 마다 비율이 줄어듬.
plot(prop.table(table(CPT_LNIF_CNT[sci_data$TARGET==1])),t="l",ylim=c(0,0.8)) # TARGET==1일 때 0인 비율이 낮다
points(prop.table(table(CPT_LNIF_CNT[sci_data$TARGET==0])),t="l",col="red")




# SPART_LNIF_CNT
SPART_LNIF_CNT <- sci_data$SPART_LNIF_CNT
plot(prop.table(table(SPART_LNIF_CNT)),t="l")
plot(prop.table(table(SPART_LNIF_CNT[sci_data$TARGET==1])),t="l",ylim=c(0,0.6))
points(prop.table(table(SPART_LNIF_CNT[sci_data$TARGET==0])),t="l",col="red")

table(SPART_LNIF_CNT,sci_data$TARGET)
prop.table(table(SPART_LNIF_CNT,sci_data$TARGET),1)

# ECT_LNIF_CNT
ECT_LNIF_CNT <- sci_data$ECT_LNIF_CNT
plot(prop.table(table(ECT_LNIF_CNT)),t="l")
plot(prop.table(table(ECT_LNIF_CNT[sci_data$TARGET==1])),t="l",ylim=c(0,0.8))
points(prop.table(table(ECT_LNIF_CNT[sci_data$TARGET==0])),t="l",col="red")

table(ECT_LNIF_CNT,sci_data$TARGET)
prop.table(table(ECT_LNIF_CNT,sci_data$TARGET),1)

# ALL_CNT

# TOT_LNIF_AMT
TOT_LNIF_AMT <-ifelse(sci_data$TOT_LNIF_AMT==0,0,sci_data$TOT_LNIF_AMT+3000)
TOT_LNIF_AMT_nom <-  as.character(log(ifelse(sci_data$TOT_LNIF_AMT==0,0,sci_data$TOT_LNIF_AMT+3000)/1000))

# TOT_LNIF_AMT / ALL_CNT
ratio_LNIF <- ifelse(sci_data$TOT_LNIF_AMT==0,0,sci_data$TOT_LNIF_AMT+3000)/1000/(BNK_LNIF_CNT+CPT_LNIF_CNT+SPART_LNIF_CNT+ECT_LNIF_CNT)
# TOT_CLIF_AMT
TOT_CLIF_AMT <- (ifelse(sci_data$TOT_CLIF_AMT==0,0,sci_data$TOT_CLIF_AMT+3000)/1000)

TOT_CLIF_AMT_nom <- log(TOT_CLIF_AMT+1)

#CLIF_TOT
CLIF <- TOT_CLIF_AMT/TOT_LNIF_AMT
hist(CLIF[sci_data$TARGET==1])
hist(CLIF[sci_data$TARGET==0])
sci_data$CLIF <- CLIF

## BNK_LNIF_AMT
BNK_LNIF_AMT <- ifelse(sci_data$BNK_LNIF_AMT==0,0,sci_data$BNK_LNIF_AMT+3000)
# 단위를 1000으로 바꾼다.
BNK_LNIF_AMT <- round(BNK_LNIF_AMT/1000)
hist(BNK_LNIF_AMT) 
# 0인데이터가 대부분이고 값이 클 수록 Frequency가 줄어듬을 알 수 있다.
hist(BNK_LNIF_AMT[sci_data$TARGET==1]) # TARGET==1일 때 0인 비율이 더 높다
hist(BNK_LNIF_AMT[sci_data$TARGET==0]) 

# log를 취하면 0을 제외하고는 정규분포가 된다. (좋을지는 모르겠음)
BNK_LNIF_AMT_nom <- log(BNK_LNIF_AMT+1)
hist(BNK_LNIF_AMT_nom)
hist(BNK_LNIF_AMT_nom[sci_data$TARGET==1])
hist(BNK_LNIF_AMT_nom[sci_data$TARGET==0])
sci_data$BNK_LNIF_AMT <- BNK_LNIF_AMT_nom

# BNK_TOT
BNK <- BNK_LNIF_AMT/TOT_LNIF_AMT

hist(BNK[sci_data$TARGET==1])
hist(BNK[sci_data$TARGET==0])
sci_data$BNK <- BNK

# CPT_LNIF_AMT
CPT_LNIF_AMT <- ifelse(sci_data$CPT_LNIF_AMT==0,0,sci_data$CPT_LNIF_AMT+3000)
plot(table(CPT_LNIF_AMT))
plot(table(CPT_LNIF_AMT[sci_data$TARGET==1])) 
plot(table(CPT_LNIF_AMT[sci_data$TARGET==0]) )

CPT_LNIF_AMT_nom <- log(CPT_LNIF_AMT+1)
hist(CPT_LNIF_AMT_nom)
hist(CPT_LNIF_AMT_nom[sci_data$TARGET==1])
hist(CPT_LNIF_AMT_nom[sci_data$TARGET==0])

sci_data$CPT_LNIF_AMT <- CPT_LNIF_AMT_nom

# CRDT_OCCR_MDIF
CRDT_OCCR_MDIF <- sci_data$CRDT_OCCR_MDIF

hist(CRDT_OCCR_MDIF,probability = TRUE)
hist(CRDT_OCCR_MDIF[sci_data$TARGET==1],probability = TRUE)
hist(CRDT_OCCR_MDIF[sci_data$TARGET==0],probability = TRUE)
table(CRDT_OCCR_MDIF,sci_data$TARGET)
prop.table(table(CRDT_OCCR_MDIF,sci_data$TARGET),1)

# SPTCT_OCCR_MDIF
SPTCT_OCCR_MDIF <- sci_data$SPTCT_OCCR_MDIF
hist(SPTCT_OCCR_MDIF[sci_data$TARGET==1],probability = TRUE)
hist(SPTCT_OCCR_MDIF[sci_data$TARGET==0],probability = TRUE)

table(SPTCT_OCCR_MDIF,sci_data$TARGET)
round(prop.table(table(SPTCT_OCCR_MDIF,sci_data$TARGET),1),4)

new_CRDT <- ifelse(CRDT_OCCR_MDIF%in%c(13,25),2,ifelse(CRDT_OCCR_MDIF%in%c(37,49,61,73),3,ifelse(CRDT_OCCR_MDIF%in%c(85,97,109),4,ifelse(CRDT_OCCR_MDIF==121,5,CRDT_OCCR_MDIF))))
new_SPTCT <- ifelse(SPTCT_OCCR_MDIF%in%c(13,25),2,ifelse(SPTCT_OCCR_MDIF%in%c(37,49,61,73),3,ifelse(SPTCT_OCCR_MDIF%in%c(85,97,109),4,ifelse(SPTCT_OCCR_MDIF==121,5,SPTCT_OCCR_MDIF))))


a <- paste0(new_CRDT,"-",new_SPTCT)
table(a,sci_data$TARGET)
round(prop.table(table(a,sci_data$TARGET),1),4)
# CRDT_CARD_CNT
CRDT_CARD_CNT <- as.numeric(as.factor(sci_data$CRDT_CARD_CNT))

plot(prop.table(table(CRDT_CARD_CNT[sci_data$TARGET==1])),ylim=c(0,0.5),t="l")
points(prop.table(table(CRDT_CARD_CNT[sci_data$TARGET==0])),t="l",col="red")
table(CRDT_CARD_CNT,sci_data$TARGET)
prop.table(table(as.numeric(CRDT_CARD_CNT),sci_data$TARGET),2)

# CTCD_OCCR_MDIF
CTCD_OCCR_MDIF <- as.numeric(as.factor(sci_data$CTCD_OCCR_MDIF))

plot(prop.table(table(CTCD_OCCR_MDIF[sci_data$TARGET==1])),ylim=c(0,0.5),t="l")
points(prop.table(table(CTCD_OCCR_MDIF[sci_data$TARGET==0])),t="l",col="red")
table(CTCD_OCCR_MDIF,sci_data$TARGET)
prop.table(table(as.numeric(CTCD_OCCR_MDIF),sci_data$TARGET),1)


# 

# 카드 CRDT_CARD_CNT_cat # 
data_set$CRDT_CARD_CNT <- ifelse(data_set$CRDT_CARD_CNT>7,7,data_set$CRDT_CARD_CNT)
data_set$CRDT_CARD_CNT <- as.numeric(as.factor(data_set$CRDT_CARD_CNT))

plot(prop.table(table(data_set$CRDT_CARD_CNT[data_set$TARGET==1])),ylim=c(0,0.5),t="l")
points(prop.table(table(data_set$CRDT_CARD_CNT[data_set$TARGET==0])),t="l",col="red")
table(data_set$CRDT_CARD_CNT,data_set$TARGET)
prop.table(table(as.numeric(data_set$CRDT_CARD_CNT),data_set$TARGET),1)
data_set$CRDT_CARD_CNT <- as.factor(data_set$CRDT_CARD_CNT)
# 연체율 숫자로 #
# LT1Y_PEOD_RATE <- ifelse(as.character(data_set$LT1Y_PEOD_RATE)=="90이상",95,as.numeric(substr(as.character(data_set$LT1Y_PEOD_RATE),1,2))-5)
# LT1Y_PEOD_RATE <- as.character(ifelse(LT1Y_PEOD_RATE<0,0,LT1Y_PEOD_RATE))
# plot(prop.table(table(data_set$LT1Y_PEOD_RATE[data_set$TARGET==1])),ylim=c(0,1),t="l")
# points(prop.table(table(data_set$LT1Y_PEOD_RATE[data_set$TARGET==0])),t="l",col="red")
# table(data_set$LT1Y_PEOD_RATE,data_set$TARGET)
# prop.table(table(as.numeric(data_set$LT1Y_PEOD_RATE),data_set$TARGET),1)

data_set$LT1Y_PEOD_RATE <- as.factor(substr(LT1Y_PEOD_RATE,1,2))

# ratio_LNIF는 전체 금액을 전체 건수로 나눈 비율
data_set$ratio_LNIF =data_set$TOT_LNIF_AMT/(data_set$BNK_LNIF_CNT+data_set$CPT_LNIF_CNT+data_set$SPART_LNIF_CNT+data_set$ECT_LNIF_CNT)
######################################################################

# 대출급액 대비 소득
mean(data_set$CUST_JOB_INCM[data_set$TARGET==1])
mean(data_set$CUST_JOB_INCM[data_set$TARGET==0])
table(data_set$CUST_JOB_INCM==0,data_set$TARGET)
prop.table(table(data_set$CUST_JOB_INCM==0,data_set$TARGET),1)
level <- unique(data_set$OCCP_NAME_G)

# 직업별로 표준화 (한게 좋은지 안한게 좋은지 판단하기)
for(i in 1:length(level)){
  job <- level[i]
  if(sd(data_set$CUST_JOB_INCM[data_set$OCCP_NAME_G==job])!=0){
    data_set$CUST_JOB_INCM[data_set$OCCP_NAME_G==job] <- (data_set$CUST_JOB_INCM[data_set$OCCP_NAME_G==job]-mean(data_set$CUST_JOB_INCM[data_set$OCCP_NAME_G==job]))/sd(data_set$CUST_JOB_INCM[data_set$OCCP_NAME_G==job])
  }
}

cor(data_set$CUST_JOB_INCM,data_set$TARGET)

# 아내직업별 표준화
level <- unique(data_set$MATE_OCCP_NAME_G)
# 직업별로 표준화 (한게 좋은지 안한게 좋은지 판단하기)
for(i in 1:length(level)){
  job <- level[i]
  if(sd(data_set$MATE_JOB_INCM[data_set$MATE_OCCP_NAME_G==job])!=0){
    data_set$MATE_JOB_INCM[data_set$MATE_OCCP_NAME_G==job] <- (data_set$MATE_JOB_INCM[data_set$MATE_OCCP_NAME_G==job]-mean(data_set$MATE_JOB_INCM[data_set$MATE_OCCP_NAME_G==job]))/sd(data_set$MATE_JOB_INCM[data_set$MATE_OCCP_NAME_G==job])
  }
}

###가족수
b <- data_set$CUST_FMLY_NUM
prop.table(table(b,data_set$TARGET),1)
table(b,data_set$TARGET)

# 가족소득
# 가구별로 표준화
level <- unique(data_set$CUST_FMLY_NUM)
data_set$HSHD_INFR_INCM <- data_set$HSHD_INFR_INCM/data_set$TOT_LNIF_AMT

#가구수별 표준화
for(i in 1:length(level)){
  fnum <- level[i]
  a <- data_set$HSHD_INFR_INCM[data_set$CUST_FMLY_NUM==fnum]/fnum
  b <- (a-mean(a))/sd(a)
  data_set$HSHD_INFR_INCM[data_set$CUSTL_FMLY_NUM==fnum] <- b
}


d <- data_set$SPTCT_OCCR_MDIF
e <- data_set$BNK_LNIF_CNT/(d+1)
data_set$per_CLIF_2 <- e

# #CRDT_OCCR_MDIF & SPTCT_OCCR_MDIF
# sp <- data_set$SPTCT_OCCR_MDIF
# table(sp,data_set$TARGET)
# cr <- data_set$CRDT_OCCR_MDIF
# table(cr,data_set$TARGET)
# data_set$CR_SP_MDIF <- as.factor(ifelse(cr>sp,1,ifelse(cr==0&sp==0,2,ifelse(cr==sp,3,4))))



#신용대출 금액/전체금액
a <- data_set$TOT_CLIF_AMT/data_set$TOT_LNIF_AMT
data_set$ratio_CLIF_TOT_AMT <- a

#은행대출 금액/전체금액
a <- data_set$BNK_LNIF_AMT/data_set$TOT_LNIF_AMT
data_set$ratio_BNK_TOT_AMT <- a

#카드사할부사 / 전체금액
a <- data_set$CPT_LNIF_AMT/data_set$TOT_LNIF_AMT
data_set$ratio_CPT_TOT_AMT <- a

# 은행대출금액/은행대출건수
data_set$ratio_BNK <- data_set$BNK_LNIF_AMT/(1+data_set$BNK_LNIF_CNT)

#카드사 캐피탈사/건수
data_set$ratio_CPT <- data_set$CPT_LNIF_AMT/(1+data_set$CPT_LNIF_CNT)

#대출정보 
a <- data_set$BNK_LNIF_CNT
b <- data_set$CPT_LNIF_CNT
c <- data_set$SPART_LNIF_CNT
d <- data_set$ECT_LNIF_CNT

#신용카드
a <- data_set$CTCD_OCCR_MDIF
b <- data_set$CRDT_CARD_CNT
table(b,data_set$TARGET)
c <- as.numeric(b)/(a+1)
data_set$per_CARD <- c

## #CRDT_OCCR_MDIF & SPTCT_OCCR_MDIF
CTCD_OCCR_MDIF <- data_set$CTCD_OCCR_MDIF
CTCD_OCCR_MDIF <- ifelse(CTCD_OCCR_MDIF%in%c(1,13),1,ifelse(CTCD_OCCR_MDIF%in%c(25,37,49,61,73),2,ifelse(CTCD_OCCR_MDIF%in%c(85,97,109),3,ifelse(CTCD_OCCR_MDIF==121,4,0))))
data_set$CTCD_OCCR_MDIF <- as.factor(CTCD_OCCR_MDIF)
new_CRDT <- ifelse(data_set$CRDT_OCCR_MDIF%in%c(13,25),2,ifelse(data_set$CRDT_OCCR_MDIF%in%c(37,49,61,73),3,ifelse(data_set$CRDT_OCCR_MDIF%in%c(85,97,109),4,ifelse(data_set$CRDT_OCCR_MDIF==121,5,data_set$CRDT_OCCR_MDIF))))
new_SPTCT <- ifelse(data_set$SPTCT_OCCR_MDIF%in%c(13,25),2,ifelse(data_set$SPTCT_OCCR_MDIF%in%c(37,49,61,73),3,ifelse(data_set$SPTCT_OCCR_MDIF%in%c(85,97,109),4,ifelse(data_set$SPTCT_OCCR_MDIF==121,5,data_set$SPTCT_OCCR_MDIF))))
data_set$CRDT_OCCR_MDIF <- as.factor(new_CRDT)
data_set$SPTCT_OCCR_MDIF <- as.factor(new_SPTCT)
data_set$cross_CRDT_SPTCT <- as.factor(paste0(CRDT_OCCR_MDIF,"-",SPTCT_OCCR_MDIF))

#신용대출
a <- data_set$TOT_CLIF_AMT
b <- as.numeric(as.character(data_set$CRDT_CARD_CNT))
c <- a/(b+1)
data_set$per_CLIF <- c



#SPART_ECT
a <- data_set$TOT_LNIF_AMT
b <- data_set$BNK_LNIF_AMT
c <- data_set$CPT_LNIF_AMT
d <- a-b-c
data_set$TOT_SP_EC <- ifelse(d<0,0,d)
data_set$per_SP_EC <- data_set$TOT_SP_E/(data_set$SPART_LNIF_CNT+data_set$ECT_LNIF_CNT+1)
# data_set$per_SP_EC <- ifelse(is.nan(data_set$per_SP_EC),0,data_set$per_SP_EC)
# data_set$per_SP_EC <- ifelse(is.infinite(data_set$per_SP_EC),0,data_set$per_SP_EC)


#한화생명신용상환금액/한화생명신용대출금액
data_set$TOT_CRLN_ratio<-data_set$TOT_REPY_AMT/data_set$TOT_CRLN_AMT
data_set$TOT_CRLN_ratio <- ifelse(is.nan(data_set$TOT_CRLN_ratio),0,data_set$TOT_CRLN_ratio)



######################################################################
# 전체 대출 횟수
data_set$all_loan <- data_set$BNK_LNIF_CNT+data_set$CPT_LNIF_CNT+data_set$SPART_LNIF_CNT+data_set$ECT_LNIF_CNT
######################################################################

######################################################################
# 은행대출 / 전체 대출 횟수
data_set$BNK_d_all_loan <- data_set$BNK_LNIF_CNT/data_set$all_loan
######################################################################

######################################################################
# 카드사/할부사/캐피탈 / 전체 대출 횟수
data_set$CPT_d_all_loan <- data_set$CPT_LNIF_CNT/data_set$all_loan
######################################################################

######################################################################
# [2산업분류]/ 전체 대출 횟수
data_set$SPART_d_all_loan <- data_set$SPART_LNIF_CNT/data_set$all_loan
######################################################################

# ######################################################################
# # ETC_d_all_loan
# data_set$ETC_d_all_loan <- data_set$ECT_LNIF_CNT/ data_set$all_loan
# ######################################################################
# 
# ######################################################################
# # loan_type
# data_set$loan_type <- data_set$ratio_BNK_d_LNIF
# data_set$loan_type[which(data_set$ratio_BNK_d_LNIF>0.5 &  data_set$ratio_BNK_d_LNIF!=1)] <- 2
# data_set$loan_type[(data_set$ratio_BNK_d_LNIF<=0.5 & data_set$ratio_BNK_d_LNIF!=0)] <- 3
# ######################################################################
# 
# ######################################################################
# # ratio_except_loan_d_LNIF
# data_set$ratio_except_loan_d_LNIF <- data_set$except_loan / data_set$TOT_LNIF_AMT
# ######################################################################
# 
# ######################################################################
# # per_income
# data_set$perincome <- data_set$HSHD_INFR_INCM/data_set$ACTL_FMLY_NUM
# ######################################################################
# 
# 
# 
# ######################################################################
# # ratio_CNT_d_AMT
# data_set$ratio_CNT_d_AMT <- data_set$all_loan / data_set$TOT_LNIF_AMT



######2017.09.03 Precision79   -1.50 0.55520254 0.4205776 0.47860322  
data_set <- data_set[,-which(colnames(data_set)=="TEL_CNTT_QTR")]
########### 추가 변수 표준화 #####################3
normv <- function(data){
  if(sum(data==0)>0){
    y <- log(data+1)
  }else{
    y <- log(data)
  }
  return(y)
}
str(data_set)
#
data_set$TOT_LNIF_AMT <- normv(data_set$TOT_LNIF_AMT)
#
data_set$TOT_CLIF_AMT <- normv(data_set$TOT_CLIF_AMT)
#
data_set$BNK_LNIF_AMT <- normv(data_set$BNK_LNIF_AMT)
#
data_set$CPT_LNIF_AMT <- normv(data_set$BNK_LNIF_AMT)
#
data_set$CB_GUIF_AMT <- normv(data_set$CB_GUIF_AMT)

#
data_set$STLN_REMN_AMT <- normv(data_set$STLN_REMN_AMT)
#
data_set$LT1Y_STLN_AMT <- normv(data_set$LT1Y_STLN_AMT)
#
data_set$GDINS_MON_PREM <- normv(data_set$GDINS_MON_PREM )
#
data_set$FMLY_SVINS_MNPREM <- normv(data_set$FMLY_SVINS_MNPREM)
#
data_set$MAX_MON_PREM <- normv(data_set$MAX_MON_PREM)
#
data_set$TOT_PREM <- normv(data_set$TOT_PREM )
#
data_set$FMLY_TOT_PREM <- normv(data_set$FMLY_TOT_PREM)
#
data_set$FYCM_PAID_AMT <- normv(data_set$FYCM_PAID_AMT)
#
data_set$ARPU <- normv(data_set$ARPU)
#
data_set$MON_TLFE_AMT <- normv(data_set$MON_TLFE_AMT)
#
data_set$MOBL_FATY_PRC <- normv(data_set$MOBL_FATY_PRC)
#
data_set$CRMM_OVDU_AMT <- normv(data_set$CRMM_OVDU_AMT) 
#
data_set$LT1Y_MXOD_AMT <- normv(data_set$LT1Y_MXOD_AMT)
#
data_set$MOBL_PRIN <- normv(data_set$MOBL_PRIN)
#
data_set$ratio_LNIF <- normv(data_set$ratio_LNIF)
#
data_set$ratio_BNK <- normv(data_set$ratio_BNK)
#
data_set$ratio_CPT <- normv(data_set$ratio_CPT)
#
data_set$per_CLIF <- normv(data_set$per_CLIF)
#
data_set$per_CLIF_2 <- normv(data_set$per_CLIF_2)
#
data_set$TOT_SP_EC <- normv(data_set$TOT_SP_EC)
#
data_set$per_SP_EC <- normv(data_set$per_SP_EC)
#
data_set <- data_set[,-which(colnames(data_set)=="MIN_CNTT_DATE")]
#
data_set$TOT_CRLN_AMT <- normv(data_set$TOT_CRLN_AMT)
#
data_set$TOT_REPY_AMT <- normv(data_set$TOT_REPY_AMT)
#
data_set$SVINS_MON_PREM <- normv(data_set$SVINS_MON_PREM )
#
data_set$FMLY_GDINS_MNPREM <- normv(data_set$FMLY_GDINS_MNPREM)

######################################
data_set$MOBL_REPAY <- ((data_set$MOBL_FATY_PRC - data_set$MOBL_PRIN) / data_set$MOBL_FATY_PRC)
inf <- which(is.infinite(data_set$MOBL_REPAY)) #x/0 할부금 존재, 단말기 출고가 0원 -> 사기당한사람..? -> -1
nanski <- which(is.na(data_set$MOBL_REPAY)) #0/0 , 할부원금/단말기  모두 0원 -> 공짜폰 (1로 처리)
length(inf) #5744 (-1)
length(nanski) #22167 (1)
length(which(data_set$MOBL_REPAY == 1)) #모두 상환한 사람 16383
data_set$MOBL_REPAY[inf] <- c(-1)
data_set$MOBL_REPAY[nanski] <- c(1)

plot(prop.table(table(data_set$MOBL_REPAY[which(data_set$TARGET == 1)])),type="l",xlim=c(-0.005,0.02),ylim = c(0.0,0.02),col="red")
lines(prop.table(table(data_set$MOBL_REPAY[which(data_set$TARGET == 0)])),type="l",xlim=c(-0.005,0.02),ylim = c(0.0,0.02))

plot(prop.table(table(data_set$MOBL_REPAY[which(data_set$TARGET == 1)])),type="l",xlim=c(0.2,0.33),ylim = c(0.0,0.02),col="red")
lines(prop.table(table(data_set$MOBL_REPAY[which(data_set$TARGET == 0)])),type="l",xlim=c(0.2,0.33),ylim = c(0.0,0.02))
######################################






for(i in 1:ncol(data_set)){
  if(is.factor(data_set[,i])){
    len <- as.numeric(data_set[,i])
    data_set[,i] <- as.factor(paste0("a",len))
  }
}
data_set$mon_year <- data_set$CRMM_OVDU_AMT / data_set$LT1Y_MXOD_AMT
length(which(is.infinite(data_set$mon_year))) #1544명 당월 값이 존재 , 년간 최대 연체금액 0 -> +1
length(which(is.nan(data_set$mon_year))) #84337명 당월 연체 0, 최대연체금액 0 -> -1로 부여

inf3 <- which(is.infinite(data_set$mon_year)) #1544명 당월 값이 존재 , 년간 최대 연체금액 0 -> +1
nanski3 <- which(is.nan(data_set$mon_year)) #84337명 당월 연체 0, 최대연체금액 0 -> -1로 부여

data_set$mon_year[inf3] <- c(1)
data_set$mon_year[nanski3] <- c(-1)





#install.packages("h2o")
library(h2o)
library(caret)
h2o.init(nthreads = -1,min_mem_size= '8G')
#h2o.no_progress() # Don't show progress bars in RMarkdown output

# data split
intrain<-createDataPartition(y=data_set$TARGET, p=0.8, list=FALSE) 
train<-data_set[intrain,-1]
test<-data_set[-intrain,-1]

train <- as.h2o(train)
test <- as.h2o(test)
# Identify predictors and response
y <- "TARGET"
x <- setdiff(names(train), y)

# For binary classification, response should be a factor
train[,y] <- as.factor(train[,y])
test[,y] <- as.factor(test[,y])

# Number of CV folds (to generate level-one data for stacking)
nfolds <- 5
# Train & Cross-validate a GBM
my_gbm <- h2o.gbm(x = x,
                  y = y,
                  training_frame = train,
                  distribution = "bernoulli",
                  max_depth = 6,
                  ntrees = 1261,
                  min_rows = 2,
                  learn_rate = 0.2,
                  nfolds = nfolds,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

perf <- h2o.performance(my_gbm, newdata = test)
pred <- h2o.predict(my_gbm,newdata=test)
my_gbm
a <- as.data.frame((pred$predict))
pt<- data.frame(table(as.numeric(a$predict),data_set$TARGET[-intrain]))
pt[,3][2]
pt[,3][3]
pt[,3][4]
p <- pt[,3][4]/(pt[,3][2]+pt[,3][4])
r <- pt[,3][4]/(pt[,3][3]+pt[,3][4])
f_score<-2*p*r/(p+r)
f_score

# Train & Cross-validate a RF
my_rf8 <- h2o.randomForest(x = x,
                           y = y,
                           training_frame = train,
                           ntrees = 1000,
                           max_depth = 14,
                           nfolds = nfolds,
                           fold_assignment = "Modulo",
                           keep_cross_validation_predictions = TRUE,
                           seed = 1)

# Train & Cross-validate a DNN
rate=c(0.0001,0.005,0.01)
l2=c(0.001,0.005,0.01)
hidden=list(c(200,200),c(200,100,100),c(300,300),c(300,300,300),c(300,200,200))

dnn_result

dnn_result<-data.frame()
for (i in c(1:length(rate))){
  for (j in c(1:length(l2))){
    for (k in c(1:length(hidden))){
      my_dl <- h2o.deeplearning(x = x,
                                y = y,
                                training_frame = train,
                                rate=rate[i],
                                l1=0.001,
                                l2=l2[j],
                                hidden=unlist(hidden[k]),
                                nfolds = nfolds,
                                fold_assignment = "Modulo",
                                keep_cross_validation_predictions = TRUE,
                                seed = 1)
      
      perf <- h2o.performance(my_dl, newdata = test)
      pred <- h2o.predict(my_dl,newdata=test)
      
      a <- as.data.frame((pred$predict))
      pt<- data.frame(table(as.numeric(a$predict),data_set$TARGET[-intrain]))
      pt[,3][2]
      pt[,3][3]
      pt[,3][4]
      p <- pt[,3][4]/(pt[,3][2]+pt[,3][4])
      r <- pt[,3][4]/(pt[,3][3]+pt[,3][4])
      f_score<-2*p*r/(p+r)
      
      res_data<-data.frame(rateA=rate[i],l2A=l2[j],hiddenA=k,f_scoreA=f_score)
      dnn_result<-rbind(dnn_result,res_data)
      
      
      
    }
  }
}


my_dl <- h2o.deeplearning(x = x,
                          y = y,
                          training_frame = train,
                          l1 = 0.001,
                          l2 = 0.001,
                          hidden = c(200, 200, 200),
                          nfolds = nfolds,
                          fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)
my_dl
perf <- h2o.performance(my_dl, newdata = test)
pred <- h2o.predict(my_dl,newdata=test)
a <- as.data.frame((pred$predict))
pt<- data.frame(table(as.numeric(a$predict),data_set$TARGET[-intrain]))
pt[,3][2]
pt[,3][3]
pt[,3][4]
p <- pt[,3][4]/(pt[,3][2]+pt[,3][4])
r <- pt[,3][4]/(pt[,3][3]+pt[,3][4])
f_score<-2*p*r/(p+r)
f_score


# Train & Cross-validate a (shallow) XGB-GBM
my_xgb1 <- h2o.xgboost(x = x,
                       y = y,
                       training_frame = train,
                       distribution = "bernoulli",
                       ntrees = 50,
                       max_depth = 3,
                       min_rows = 2,
                       learn_rate = 0.2,
                       nfolds = nfolds,
                       fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,
                       seed = 1)


# Train & Cross-validate another (deeper) XGB-GBM
my_xgb2 <- h2o.xgboost(x = x,
                       y = y,
                       training_frame = train,
                       distribution = "bernoulli",
                       ntrees = 50,
                       max_depth = 8,
                       min_rows = 1,
                       learn_rate = 0.1,
                       sample_rate = 0.7,
                       col_sample_rate = 0.9,
                       nfolds = nfolds,
                       fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,
                       seed = 1)

# Train a stacked ensemble using the H2O and XGBoost models from above
base_models <- list(my_gbm@model_id,
                    my_rf@model_id,
                    my_dl@model_id  
                    #my_xgb1@model_id,
                    #my_xgb2@model_id
)
metalearner <- "h2o.randomForest.wrapper"
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                #metalearner=metalearner,
                                base_models = base_models)
?h2o.stackedEnsemble
# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test)
pred <- h2o.predict(ensemble,newdata=test)



# Compare to base learner performance on the test set
get_auc <- function(mm) h2o.auc(h2o.performance(h2o.getModel(mm), newdata = test))
baselearner_aucs <- sapply(base_models, get_auc)
baselearner_best_auc_test <- max(baselearner_aucs)
ensemble_auc_test <- h2o.auc(perf)

print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))


str(pred)
a <- as.data.frame((pred$predict))
pt<- data.frame(table(as.numeric(a$predict),data_set$TARGET[-intrain]))
pt[,3][2]
pt[,3][3]
pt[,3][4]
p <- pt[,3][4]/(pt[,3][2]+pt[,3][4])
r <- pt[,3][4]/(pt[,3][3]+pt[,3][4])
2*p*r/(p+r)
d12_1000<-2*p*r/(p+r)


summary(my_xgb1)
test <-  read.csv('/home/snu/Bigcontest/challenge_data/Test_set.csv',fileEncoding='euc-kr',stringsAsFactors = TRUE)
result <- data_set[,c(-1,-2)]
result <- as.h2o(result)
pred <- h2o.predict(ensemble,result)
c <- as.data.frame(pred$predict)
table(c$predict)
preresult$today <- c$predict
cor(as.numeric(preresult$TARGET),as.numeric(preresult$today))
mean(preresult$TARGET==preresult$today)
#####################################33
y="TARGET"
stack <- h2o.stack(models = base_models,
                   response_frame = train[,y],
                   metalearner = metalearner, 
                   seed = 1,
                   keep_levelone_data = TRUE)
?h2o.stack
h2o.stackedEnsemble
# Compute test set performance:
perf <- h2o.ensemble_performance(stack, newdata = test)




