library(lattice)

# 데이터 파일 불러오기
data_set = read.csv('/home/snu/Bigcontest/challenge_data/Data_set.csv',fileEncoding='euc-kr',stringsAsFactors = FALSE)
size = nrow(data_set)
# 54	AVG_CALL_TIME	월통화시간_분	월평균 통화시간 분단위
# 55	AVG_CALL_FREQ	월통화빈도	월평균 통화횟수
# 56	TEL_MBSP_GRAD	멤버쉽등급	SKT멤버쉽 등급
# 57	ARPU	가입자매출_원	월기준 회선당 평균 수익금
# 58	MON_TLFE_AMT	납부요금_원	월기준 서비스 납부요금
# 59	CBPT_MBSP_YN	결합상품가입여부	인터넷, TV등 결합상품가입 여부: Y(가입) , N(미가입)
# 60	MOBL_FATY_PRC	단말기가격_원	사용중인 핸드폰단말기 출고가액
# 61	TEL_CNTT_QTR	가입년월_분기	SKT가입년월_분기단위: YYYYQ
# 62	NUM_DAY_SUSP	정지일수	회선의 사용정지일수
# 63	CRMM_OVDU_AMT	당월연체금액_원	해당월 납부요금의 연체금액
# 64	TLFE_UNPD_CNT	납부일미준수횟수	핸드폰 납부요금의 납입일 미준수한 횟수
# 65	LT1Y_MXOD_AMT	년간최대연체금액_원	산출일 기준 최근1년이내 납부요금 연체금액 中 최대 연체금액
# 66	PAYM_METD	납부방법	납부요금의 납부 방법
# 67	LINE_STUS	회선상태	산출일 기준 회선의 상태: S(정지), U(사용)
# 68	MOBL_PRIN	남은할부금_원	산출일 기준 남아있는 핸드폰 단말기 할부원금


################ 휴대폰 정보 데이터 및 사용자 정보 ########################
###########################################################################

# 해당 column의 index
Phone_and_user_idx <- c(1,2,17,53:69)

# 해당 data set
Phone_data = data_set[,Phone_and_user_idx, with=F]

# 전체적인 탐색
str(Phone_data)

### 범주형 자료와 연속형 자료 구분

Phone_col <- colnames(Phone_data)
discrete_idx <- which(Phone_col %in% c('TEL_MBSP_GRAD','CBPT_MBSP_YN','PAYM_METD','LINE_STUS','TEL_CNTT_QTR','TLFE_UNPD_CNT','MOBL_FATY_PRC'))

Phone_data_discrete <- Phone_data[,discrete_idx, with=F]
Phone_data_continous <-  Phone_data[,-c(discrete_idx,1,2,3,4,5), with=F]
Phone_data_user <- Phone_data[,c(1,2,3,4,5)]

#### 범주형 자료 탐색
# TEL_MBSP_GRAD : 멤버쉽 등급
# CBTP_MBSP_YN :결합상품가입여부
# PAYM_METD : 납부방법
# LINE_STUS : 회선상태
# TEL_CNTT_QTR	가입년월_분기	SKT가입년월_분기단위: YYYYQ
# TLFE_UNPD_CNT	납부일미준수횟수	핸드폰 납부요금의 납입일 미준수한 횟수
# MOBL_FATY_PRC	단말기가격_원	사용중인 핸드폰단말기 출고가액

# table 
table(Phone_data_discrete)
table(Phone_data_discrete$TEL_MBSP_GRAD)/size # 결측치 45% : 아마 멤버쉽을 가입하지 않은 사람으로 보인다.
table(Phone_data_discrete$CBPT_MBSP_YN)/size # 결측치 0%
table(Phone_data_discrete$PAYM_METD)/size # 결측치 3% ???
table(Phone_data_discrete$LINE_STUS)/size # 결측치 0% 2%를 제외하곤 모두 휴대폰을 사용하고 있다.
table(Phone_data_discrete$TEL_CNTT_QTR) /size
table(Phone_data_discrete$TLFE_UNPD_CNT)/size
table(Phone_data_discrete$MOBL_FATY_PRC)/size

plot(table(Phone_data_discrete$TEL_MBSP_GRAD)/size)
plot(table(Phone_data_discrete$CBPT_MBSP_YN)/size)
plot(table(Phone_data_discrete$PAYM_METD)/size)
plot(table(Phone_data_discrete$LINE_STUS)/size)
plot(table(Phone_data_discrete$TEL_CNTT_QTR)/size)
plot(table(Phone_data_discrete$TLFE_UNPD_CNT)/size) # 납부일 미준수 횟수가 있는것은 아주 드물다.
plot(table(Phone_data_discrete$MOBL_FATY_PRC)/size) 

#### 연속형 자료 탐색
# AVG_CALL_TIME	월통화시간_분	월평균 통화시간 분단위
# AVG_CALL_FREQ	월통화빈도	월평균 통화횟수
# ARPU	가입자매출_원	월기준 회선당 평균 수익금
# MON_TLFE_AMT	납부요금_원	월기준 서비스 납부요금
# NUM_DAY_SUSP	정지일수	회선의 사용정지일수
# CRMM_OVDU_AMT	당월연체금액_원	해당월 납부요금의 연체금액
# LT1Y_MXOD_AMT	년간최대연체금액_원	산출일 기준 최근1년이내 납부요금 연체금액 中 최대 연체금액
# MOBL_PRIN	남은할부금_원	산출일 기준 남아있는 핸드폰 단말기 할부원금

# range
summary(Phone_data_continous$AVG_CALL_TIME)
summary(Phone_data_continous$AVG_CALL_FREQ)
summary(Phone_data_continous$ARPU)
summary(Phone_data_continous$MON_TLFE_AMT)
summary(Phone_data_continous$NUM_DAY_SUSP)
summary(Phone_data_continous$CRMM_OVDU_AMT)
summary(Phone_data_continous$LT1Y_MXOD_AMT)
summary(Phone_data_continous$MOBL_PRIN)
table(Phone_data_continous$LT1Y_MXOD_AMT)

hist(Phone_data_continous$AVG_CALL_TIME)
hist(Phone_data_continous$AVG_CALL_FREQ)
hist(Phone_data_continous$ARPU)
hist(Phone_data_continous$MON_TLFE_AMT)
hist(Phone_data_continous$NUM_DAY_SUSP) #정지일수로 대부분이 0이다
hist(Phone_data_continous$CRMM_OVDU_AMT)
hist(Phone_data_continous$LT1Y_MXOD_AMT)
hist(Phone_data_continous$MOBL_PRIN)

# 공분산 행렬 시각화
cor = cor(Phone_data_continous)
cor(Phone_data_continous$ARPU,Phone_data_continous$AVG_CALL_TIME)

rgb.palette <- colorRampPalette(c("yellow", "blue"), space = "rgb")
levelplot(cor, main="Continous data of Phone data correlation matrix", xlab="", ylab="", col.regions=rgb.palette(120), cuts=300, at=seq(-1,1,0.05))
# 대부분 양의 관계를 보인다. NUM_DAY_SUSP는 음의 관계를 나타낸다.


### 연속인 데이터의 대부분이 0으로 보인다.
zero_AVG_CALL_TIME = as.numeric(Phone_data_continous$AVG_CALL_TIME==0)
zero_AVG_CALL_FREQ = Phone_data_continous$AVG_CALL_FREQ==0
zero_ARPU = Phone_data_continous$ARPU==0
zero_MON_TLFE_AMT = Phone_data_continous$MON_TLFE_AMT==0
zero_NUM_DAY_SUSP = Phone_data_continous$NUM_DAY_SUSP==0
zero_CRMM_OVDU_AMT = Phone_data_continous$CRMM_OVDU_AMT==0
zero_LT1Y_MXOD_AMT = Phone_data_continous$LT1Y_MXOD_AMT==0
zero_MOBL_PRIN =Phone_data_continous$MOBL_PRIN==0

mean(zero_AVG_CALL_TIME==0)
mean(zero_AVG_CALL_FREQ==0)
mean(zero_ARPU==0)
mean(zero_MON_TLFE_AMT==0)
mean(zero_NUM_DAY_SUSP==0)
mean(zero_CRMM_OVDU_AMT==0)
mean(zero_LT1Y_MXOD_AMT==0)
mean(zero_MOBL_PRIN==0)

# zero면 1 아니면 0인 matrix생성
zero_matrix = data.frame(zero_AVG_CALL_TIME,zero_AVG_CALL_FREQ,zero_ARPU,zero_MON_TLFE_AMT,zero_NUM_DAY_SUSP,zero_CRMM_OVDU_AMT,zero_LT1Y_MXOD_AMT,zero_MOBL_PRIN)

# zero의 상관계수행렬
cor_zero = cor(zero_matrix)
rgb.palette <- colorRampPalette(c("yellow", "blue"), space = "rgb")
levelplot(cor_zero, main="Continous data of Phone data correlation matrix", xlab="", ylab="", col.regions=rgb.palette(120), cuts=300, at=seq(-1,1,0.05))
# AVG_CALL_FREQ, AVG_CALL_FREQ, ARPU, MON_TLFE_AMT, PRIN 들 끼리는 높은 관계를 보인다. 그리고 맥스 월연체와 해당 월연체가 높은 관계를 보인다.