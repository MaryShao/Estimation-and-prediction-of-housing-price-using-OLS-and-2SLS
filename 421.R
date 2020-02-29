#install.packages("Hmisc")
#install.packages("car")
#install.packages("psych")
#install.packages("MASS")
#install.packages("leaps")
#install.packages('gvlma')
#install.packages('plm')
#install.packages('systemfit')
options(warn=-1)
library(car) 
library(ggplot2)
library(Hmisc)
library(psych)
library(MASS)
library(leaps)
library(gvlma)
library(reshape2)
library(rugarch)
## install.packages('quantmod')
library(quantmod)
# install.packages('fBasics')
library(fBasics)
#install.packages('forecast')
library(forecast)
#install.packages('aTSA')
library(aTSA)
#install.packages('PerformanceAnalytics')
library(PerformanceAnalytics) 
library(tseries)
library(stats)
library(plm)
library(systemfit)


data <- read.csv(file.choose(),header=T)

my_data <- data[, c(4,5,6,7,8,9,16)]
# correlation
res <- cor(my_data)
round(res, 2)
res2 <- rcorr(as.matrix(my_data))
# Visualize correlation matrix
col<- colorRampPalette(c( "white", "black"))(20)
heatmap(x = res, col = col, symm = TRUE)

price<-data$price
price_AP<-price[1:129]
price_UWP<-price[130:132]
# exogenous variable
bathroom<-data$'bathroom'
bathroom_AP<-bathroom[1:129]
bathroom_UWP<-bathroom[130:132]
bedroom<-data$'bedroom'
bedroom_AP<-bedroom[1:129]
bedroom_UWP<-bedroom[130:132]
area<-data$'sqrt'
area_AP<-area[1:129]
area_UWP<-area[130:132]
distance_UW<-data$'dc'
distance_UW_AP<-distance_UW[1:129]
distance_UW_UWP<-distance_UW[130:132]
distance_UL<-data$'lu'
distance_UL_AP<-distance_UL[1:129]
distance_UL_UWP<-distance_UL[130:132]
distance_mart<-data$'mart'
distance_mart_AP<-distance_mart[1:129]
distance_mart_UWP<-distance_mart[130:132]
# dummy variable
parking<-data$'parking'
parking_AP<-parking[1:129]
parking_UWP<-parking[130:132]
gym<-data$'gym'
gym_AP<-gym[1:129]
gym_UWP<-gym[130:132]
utility<-data$'utility'
utility_AP<-utility[1:129]
utility_UWP<-utility[130:132]
insuit_laundry<-data$'insuit_laundry'
insuit_laundry_AP<-insuit_laundry[1:129]
insuit_laundry_UWP<-insuit_laundry[130:132]
# endougenous variable
web_value<-data$'web_value'
web_value_AP<-web_value[1:129]
web_value_UWP<-web_value[130:132]
hit<-data$'web_hits'
hit_AP<-hit[1:129]
hit_UWP<-hit[130:132]
building<-data$'buildings'
building_AP<-building[1:129]
building_UWP<-building[130:132]
year<-data$'years'
year_AP<-year[1:129]
year_UWP<-year[130:132]

distance_U_AP<-rep(0,length(distance_UW_AP))
distance_U_UWP<-rep(0,length(distance_UW_UWP))
for (i in (1:length(distance_UW_AP))){
  distance_U_AP[i] = min(distance_UW_AP[i],distance_UL_AP[i])
}
for (i in (1:length(distance_UW_UWP))){
  distance_U_UWP[i] = min(distance_UW_UWP[i],distance_UL_UWP[i])
}


# OLS

m11<-lm(formula=price_AP~bathroom_AP+bedroom_AP+area_AP
        +distance_U_AP+distance_mart_AP
        +parking_AP+gym_AP+utility_AP+insuit_laundry_AP
        +web_value_AP)
summary(m11)


leaps=regsubsets(price_AP~bathroom_AP+bedroom_AP+area_AP
                +distance_U_AP+distance_mart_AP+web_value_AP
                 +parking_AP+gym_AP+utility_AP+insuit_laundry_AP,
                 data = data,nbest=2)
m12<-lm(formula=price_AP~bathroom_AP+bedroom_AP+area_AP
        +distance_U_AP+distance_mart_AP+gym_AP
        +utility_AP+insuit_laundry_AP)
plot(leaps,scale='adjr2')

m13<-stepAIC(m11,direction="backward",steps=1)
summary(m13)
std.r2 <- rstandard(m12) #extracting the standardized residuals from the fitted model
hist(std.r2)
mean(std.r2)
#plot(m12)
# correlation test

c1<-cor.test(std.r2,bathroom_AP, method=c("pearson", "kendall", "spearman"))
c2<-cor.test(std.r2,bedroom_AP, method=c("pearson", "kendall", "spearman"))
c3<-cor.test(std.r2,area_AP, method=c("pearson", "kendall", "spearman"))
c4<-cor.test(std.r2,distance_U_AP, method=c("pearson", "kendall", "spearman"))
c5<-cor.test(std.r2,distance_mart_AP, method=c("pearson", "kendall", "spearman"))
c6<-cor.test(std.r2,gym_AP, method=c("pearson", "kendall", "spearman"))
c7<-cor.test(std.r2,utility_AP, method=c("pearson", "kendall", "spearman"))
c8<-cor.test(std.r2,insuit_laundry_AP, method=c("pearson", "kendall", "spearman"))
c9<-cor.test(std.r2,web_value_AP, method=c("pearson", "kendall", "spearman"))

bathroom_test <- c(c1$estimate,c1$p.value)
bedroom_test <- c(c2$estimate,c2$p.value)
area_test <- c(c3$estimate,c3$p.value)
distance_U_test <- c(c4$estimate,c4$p.value)
distance_mart_test <- c(c5$estimate,c5$p.value)
gym_test <- c(c6$estimate,c6$p.value)
utility_test <- c(c7$estimate,c7$p.value)
insuit_laundry_test <- c(c8$estimate,c8$p.value)
web_value_test <- c(c9$estimate,c9$p.value)
names(bathroom_test) <- c('correlation','p-value')
rbind(bathroom_test,bedroom_test,area_test,distance_U_test,
      distance_mart_test,gym_test,utility_test,
      insuit_laundry_test,web_value_test)


# 2SLS regression
# first stage

c10<-cor.test(std.r2,year_AP, method=c("pearson", "kendall", "spearman"))
c11<-cor.test(std.r2,building_AP, method=c("pearson", "kendall", "spearman"))
c12<-cor.test(std.r2,hit_AP, method=c("pearson", "kendall", "spearman"))
year_test <- c(c10$estimate,c10$p.value)
building_test <- c(c11$estimate,c11$p.value)
hit_test <- c(c12$estimate,c12$p.value)
rbind(year_test,building_test,hit_test)

m2<-lm(formula=web_value~hit+building)
summary(m2)
est_web_value<-fitted(m2)
# second stage
m31<-lm(formula=price_AP~bathroom_AP+bedroom_AP+area_AP
        +distance_U_AP+distance_mart_AP
        +gym_AP+utility_AP+insuit_laundry_AP
        +est_web_value[1:129])
summary(m31)

#plot(m31)

#outlierTest(m32)
#influence.measures(m32)scatterplotMatrix(m32,spread = FALSE,lty = 2,main = "Scatter Plot Matrix")


#predict
UWP_p<-m31$coef[1]+m31$coef[2]*bathroom_UWP+m31$coef[3]*bedroom_UWP+
  m31$coef[4]*area_UWP+m31$coef[5]*distance_U_UWP+m31$coef[6]*distance_mart_UWP+
  m31$coef[7]*gym_UWP+m31$coef[8]*utility_UWP+m31$coef[9]*insuit_laundry_UWP+
  m31$coef[10]*est_web_value[130:132] 

new_p<- m31$coef[1]+m31$coef[2]*1+m31$coef[3]*5+
  m31$coef[4]*10+m31$coef[5]*500+m31$coef[6]*1500+
  m31$coef[7]*1+m31$coef[8]*0+m31$coef[9]*0+
  m31$coef[10]*est_web_value[132]

# RSS for OLS and 2SLS
anova(m13,m31)

# hausman test
hausman.systemfit( m13, m31 )
