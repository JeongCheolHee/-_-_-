install.packages("haven")
install.packages("reshape2")
library(haven)
library(reshape2)
# 데이터 불러오기
spss_data_2014 = read_spss("C:/Users/USER/Desktop/회귀분석2/Project/11조.sav")
# 필요한 변수선택
spss_data <- spss_data_2014[,c("EMPLY","SEX","AGE","EDUC")]
# 성별(SEX), 종교(RELIG), 연령(AGE)
# 결측치 제거
spss_data = na.omit(spss_data)
# 결측치 제거 확인
colSums(is.na(spss_data))

# 미취업이 2로 설정되어있기 때문에, pi가 취업확률이 되게 설정
spss_data$EMPLY = relevel(factor(spss_data$EMPLY),ref = 2)

glm.fit <- glm(EMPLY ~ factor(SEX) + AGE + factor(EDUC) ,family = binomial(link=logit), data=spss_data)
summary(glm.fit)
qchisq(0.95,1360)

write.csv(spss_data,"11조.csv",quote=F,row.names = F) #산점도 그리기
spss<-read.csv("C:/Users/USER/Desktop/회귀분석2/Project/11조.csv")
par(mfrow=c(2,2))
library(reshape2)
for (i in 2:4){
  a <- table(spss[,i], spss$EMPLY); a <- as.data.frame(a)
  a <- dcast(a, Var1~Var2,value.var = "Freq")
  data <- c()
  data <- colnames(spss)[i]
  colnames(a) <- c("data", "no", "yes")
  a$prop <- round(a$yes/(a$no+a$yes),2)
  cat("=====",i,colnames(spss)[i],"=====\n")
  print(a)
  cat("\n")
  plot(prop~data, a, xlab=colnames(spss)[i])
  with(a, lines(lowess(data,prop),lty=2,col=2 ) ) 
}

glm.fit2 <- glm(EMPLY ~ factor(SEX) + AGE + factor(EDUC) + I(AGE^2) ,family = binomial(link=logit), data=spss_data)
summary(glm.fit2)
qchisq(0.95,1365)

anova(glm.fit,glm.fit2,test="Chisq")

glm.fit3 <- glm(EMPLY ~ factor(SEX) + AGE + factor(EDUC) + I(AGE^2) ,family = binomial(link=probit), data=spss_data)
summary(glm.fit3)

glm.fit4 <- glm(EMPLY ~ factor(SEX) + AGE + factor(EDUC) + I(AGE^2) ,family = binomial(link=cloglog), data=spss_data)
summary(glm.fit4)

