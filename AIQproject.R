#############################################
############Air Quality Forecast#############
#############################################
AirQualityUCI<- readxl::read_xlsx("AirQualityUCI.xlsx")
library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)
library(reshape2)
library(Amelia)
library(imputeTS)
library(dplyr)
library(zoo)
library(TSA)
library(forecast)
library(tseries)
#dealing with time variable
Time <- as.POSIXct(paste(date(AirQualityUCI$Date), substr(AirQualityUCI$Time,12,19)), format="%Y-%m-%d %H:%M:%S",tz="UTC")
#to get whole day information delete 2004-03-10 and 2005-04-04
AirQualityUCI_final <- AirQualityUCI %>% 
  mutate(Date=as.Date(Date),Time=as.POSIXct(paste(date(AirQualityUCI$Date), substr(AirQualityUCI$Time,12,19)), format="%Y-%m-%d %H:%M:%S",tz="UTC"),
         year=year(AirQualityUCI$Date),month=month(AirQualityUCI$Date),day=day(AirQualityUCI$Date),hour=hour(AirQualityUCI$Time),) %>% 
  filter(Date!="2004-03-10"&Date!="2005-04-04") %>% 
  select(Date,Time,year,month,day,hour,`CO(GT)`:AH) %>% 
  as.data.frame()

#dealing with NA values
#In this dataset, missing values are tagged with -200 value
AirQualityUCI_final[AirQualityUCI_final==-200] <- NA
round(colSums(is.na(AirQualityUCI_final))/nrow(AirQualityUCI_final),3)
na_df <- data.frame(pollutant=names(round(colSums(is.na(AirQualityUCI_final))/nrow(AirQualityUCI_final),3))[7:19],
                    percent=round(colSums(is.na(AirQualityUCI_final))/nrow(AirQualityUCI_final),3)[7:19]*100)
ggplot(na_df, aes(reorder(pollutant,-percent), percent,fill=pollutant))+
  geom_bar(stat="identity",show.legend = F)+
  geom_text(data=na_df, aes(label=paste0(percent,"%"),
                            y=percent+1.3), size=4)+
  labs(x = "Vaeiables", y = "Percentage", 
       title = "Percentage of NA values (total 9357 records)")
#check the position of NA values
missmap(AirQualityUCI_final[,-1:-4],col = c("black","light blue"),y.labels = seq(1000,9000,1000),y.at = seq(0,9000,1000),
        x.cex = 0.7,main = "Missingness Map for AirQualityUCI dataset")

#由於缺失值的位置集中，選擇直接刪除NA，並直接刪除NMHC(GT)欄位
AirQualityUCI_naomit <- AirQualityUCI_final %>% 
  select(-`NMHC(GT)`) %>% 
  na.omit()
AirQualityUCI_naomit$Date <- as.Date(AirQualityUCI_naomit$Date)  
AirQualityUCI_final <- AirQualityUCI_final %>% 
  select(-`NMHC(GT)`) #delete NMHC(GT) column
#convert CO to ppm and NO2 to ppb
CO_ppm <- (AirQualityUCI_final$`CO(GT)`/28)*24.45
NO2_ppb <- (AirQualityUCI_final$`NO2(GT)`/46)*24.45
df1 <- data.frame(date=AirQualityUCI_final$Time,CO_ppm=CO_ppm,NO2_ppb=NO2_ppb)

g1 <- ggplot(data.frame(date=AirQualityUCI_final$Time,CO_ppm=CO_ppm),aes(x=date,y=CO_ppm))+
  geom_line(size=0.7,color="steelblue")+
  geom_hline(yintercept =c(4.5,9.5),linetype="dashed",color=c("yellow","orange"),size=0.8)

g2 <- ggplot(data.frame(date=AirQualityUCI_final$Time,NO2_ppb=NO2_ppb),aes(x=date,y=NO2_ppb))+
  geom_line(size=0.7,color="red4")+
  geom_hline(yintercept =c(54,101),linetype="dashed",color=c("yellow","orange"),size=0.8)

grid.arrange(g1,g2,nrow=2,top = "Concentration of CO & NO2 ")
cbind(CO=sum(AirQualityUCI_final$`CO(GT)` > 9.5, na.rm = T),NO2=sum(AirQualityUCI_final$`NO2(GT)` > 9.5, na.rm = T))

#pollution of NO2 in this city is the more serious problem

#取NO2的即時濃度值作為空氣品質指標

########About data
NO2_dailymax <- AirQualityUCI_final %>% 
  group_by(year,month,day) %>% 
  summarise(Date=as.Date(first(Date)),NO2=mean(`NO2(GT)`,na.rm=T)) %>% 
  select(Date,year,month,day,NO2)
NO2_dailymax$NO2[NO2_dailymax$NO2==-Inf] <- NA
#daily series
g7 <- ggplot(NO2_dailymax,aes(x=Date,y=NO2))+
  geom_line(size=0.8,color="red4")+
  ylab("NO2(microg/m^3)")+
  ggtitle("Daily Mean of NO2")
#theme(plot.title = element_text(hjust = 0.5))
#by time(hourly series)
NO2_hourly <- AirQualityUCI_final %>% 
  group_by(hour) %>% 
  summarise(NO2=mean(`NO2(GT)`,na.rm=T)) %>% 
  select(hour,NO2)
g8 <- ggplot(NO2_hourly,aes(x=hour,y=NO2))+
  geom_line(size=0.8,color="red4")+
  ylab("NO2(microg/m^3)")+
  ggtitle("Hourly Mean of NO2")
#theme(plot.title = element_text(hjust = 0.5))
grid.arrange(g8,g7,nrow=2)

#trend for a day
df_NO2 <- AirQualityUCI_final %>% 
  select(Date=Date,Time=Time,year=year,month=month,day=day,hour=hour,NO2=`NO2(GT)`)
df_NO2$Date <- as.factor(df_NO2$Date)
#monthly series
df_g5 <- df_NO2 %>% 
  group_by(month) %>% 
  summarise(NO2=mean(NO2,na.rm=T)) 
g5 <- ggplot(df_g5,aes(x=as.factor(month),y=NO2,fill=NO2))+
  geom_bar(stat = "identity")+
  ylab("NO2(microg/m^3)")+
  ggtitle("Mean of NO2 for Each Month")+
  xlab("month")

ggplot(df_NO2,aes(x=hour,y=NO2,color=Date))+
  geom_line(size=0.7,show.legend = F)+
  xlab("hourly trend")+ylab("NO2(microg/m^3)")+
  geom_point(show.legend = F)

#trend for each month
df_g4 <- df_NO2 %>% 
  group_by(month,hour) %>% 
  summarise(NO2=mean(NO2,na.rm=T))   
df_g4$month <- as.factor(df_g6$month)
g4 <- ggplot(df_g6,aes(x=hour,y=NO2,color=month))+
  geom_line(size=0.8)+
  ylab("NO2(microg/m^3)")+
  xlab('hour')+
  ggtitle("Hourly Trend for Each Month")+
  scale_colour_hue(l=45)+
  geom_point()
#trend for each year
grid.arrange(g4,g5,nrow=2)

#######model fitting
ts.plot(AirQualityUCI_final$`NO2(GT)`)
#使用Kalmen seasonal filter填補遺失值
NO2_Kalmen <- na.StructTS(ts(AirQualityUCI_final$`NO2(GT)`,frequency = 24))
#split traing and testing dataset 
NO2_train <- ts(NO2_Kalmen[1:(length(NO2_Kalmen)-24*5)],frequency = 24)
NO2_test <- ts(NO2_Kalmen[(length(NO2_Kalmen)-24*5+1):length(NO2_Kalmen)],frequency = 24)
#由ACF和PACF觀察自相關和季節性變化
par(mfrow=c(1,2))
acf(NO2_train)
pacf(NO2_train)
par(mfrow=c(1,1))#cancel mfrow
#一階差分
ts.plot(diff(diff(NO2_train,1),24))
par(mfrow=c(1,2))
acf(diff(diff(NO2_train,24),1),100)
pacf(diff(diff(NO2_train,24),1),100)
par(mfrow=c(1,1))
#進行一階差分和季節性差分(24)  
ts.plot(diff(diff(NO2_train,1),24))
par(mfrow=c(1,2))
acf(diff(diff(NO2_train,24),1),100)
pacf(diff(diff(NO2_train,24),1),100)
par(mfrow=c(1,1))
#單根檢定(Augmented Dickey-Fuller Test)
adf.test(diff(diff(NO2_train,1),24))
#ARIMA
NO2_fit_manual <- stats::arima(NO2_train,order = c(4,1,0),seasonal = list(order=c(3,1,1),period=24))
NO2_fit_manual2 <- stats::arima(NO2_train,order = c(5,1,0),seasonal = list(order=c(4,1,1),period=24))
NO2_fit_autoarima <- auto.arima(NO2_train)
#ETS
NO2_fit_ETS <- ets(NO2_train)

#model validation
MAPE<-function(actual,pred){
  mean(abs(actual-pred)/actual)
}
MAPE(NO2_test,as.vector(predict(NO2_fit_manual,120)$pred))
MAPE(NO2_test,as.vector(predict(NO2_fit_manual2,120)$pred))
MAPE(NO2_test,as.vector(predict(NO2_fit_autoarima,120)$pred))
MAPE(NO2_test,data.frame(predict(NO2_fit_ETS,120))$Point.Forecast)
#choose NO2_fit_manual2 for final model(lowest MAPE)
#Training and Testing plot
train_forecast_arima_df <- data.frame(date=AirQualityUCI_final$Time,NO2_actual=AirQualityUCI_final$`NO2(GT)`,
                                      NO2_forecast=c(rep(NA,9336-120),as.vector(predict(NO2_fit_manual2,120)$pred)))

ggplot(melt(train_forecast_arima_df[9000:9336,],"date"),aes(x=date,y=value,color=variable))+
  geom_line(size=0.75)+
  ylab("NO2(microg/m^3)")+
  scale_colour_manual(values = c("dark red","skyblue4"), 
                      labels=c("Actual", "Forecast"))

########
par(mfrow=c(1,2))
plot(scale(as.numeric(NO2_fit_manual$residuals)),ylab="")
acf(NO2_fit_manual$residuals,main="")
par(mfrow=c(1,1))
Box.test(NO2_fit_manual$residuals, type="Ljung-Box")
#for final model
par(mfrow=c(1,2))
qqnorm(NO2_fit_manual$residuals)
qqline(NO2_fit_manual$residuals)
r <- as.numeric(NO2_fit_manual$residuals)
hist(r,col = "grey",freq = F,main="Histogram of Resuduals",xlim = c(-75,75))
curve(dnorm(x,mean=mean(r),sd=sd(r)),add = T,col="red")
par(mfrow=c(1,1))
#merge training and testing data to fit the final model
NO2_final_arima <- stats::arima(ts(c(NO2_train,NO2_test),frequency = 24),order = c(5,1,0),seasonal = list(order=c(4,1,1),period=24))
#predict NO2 concentration for the next 5 days(2005/04/04~2005/04/08,120 values)
seq_5days <- seq(as.POSIXlt("2005-04-04 00:00:00",tz = "UTC"),by="hour",length.out = 120)

predict(NO2_final_arima,120)$pred
final_forecast_arima_df <- data.frame(date=as.POSIXct(c(as.character(AirQualityUCI_final$Time),as.character(seq_5days)),tz="UTC"),
                                      NO2_actual=c(AirQualityUCI_final$`NO2(GT)`,rep(NA,120)),
                                      NO2_forecast=c(rep(NA,9336),as.vector(predict(NO2_final_arima,120)$pred)))
ggplot(melt(final_forecast_arima_df[9000:9456,],"date"),aes(x=date,y=value,color=variable))+
  geom_line(size=0.75)+
  ylab("NO2(microg/m^3)")+
  scale_colour_manual(values = c("dark red","skyblue3"), 
                      labels=c("Actual", "Forecast"))

forecast_table <- data.frame(final_forecast_arima_df[9337:9456,c(1,3)])
forecast_table <- data.frame(Date=forecast_table$date,
                             NO2_ugm3=round(forecast_table$NO2_forecast,3),
                             NO2_ppb=round((forecast_table$NO2_forecast/46)*24.45,3))
forecast_table

#ACF PACF after diff
par(mfrow=c(1,2))
acf(diff(NO2_Kalmen),main="ACF after differencing")
pacf(diff(NO2_Kalmen),main="PACF after differencing")
par(mfrow=c(1,1))

qqnorm(as.vector(NO2_test)-as.vector(predict(NO2_fit_manual2,120)$pred))
qqline(as.vector(NO2_test)-as.vector(predict(NO2_fit_manual2,120)$pred),col="red")
plot(as.vector(NO2_test)-as.vector(predict(NO2_fit_manual2,120)$pred))
r <- as.numeric(NO2_fit_manual$residuals)
hist(scale(r),col = "grey",freq = F,main="Histogram of Resuduals",xlim = c(-7.5,7.5))
curve(dnorm(x,mean=0,sd=1),add = T,col="red")

######relation between chemicals
#correlation heatmap 
cor_matrix <- AirQualityUCI_final %>% 
  select(`NO2(GT)`,`PT08.S4(NO2)`,`CO(GT)`,`C6H6(GT)`,`NOx(GT)`,`PT08.S5(O3)`) %>% 
  na.omit() %>% 
  cor(method="pearson")
round(cor_matrix,3)
ggplot(melt(cor_matrix, varnames=c("x", "y"), value.name="correlation"), 
       aes(x=x, y=y)) +
  geom_tile(aes(fill=correlation)) +
  scale_fill_gradient2(low="blue", mid="yellow", high="red",
                       guide=guide_colorbar(ticks=FALSE, barheight = 5),
                       limits=c(-1,1)) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(title="Heatmap of Correlation Matrix", 
       x=NULL, y=NULL)
#散布圖
sca1 <- ggplot(AirQualityUCI_final)+
  geom_point(aes(x=`NO2(GT)`,y=`CO(GT)`),col="#F8766D")
  
sca2 <- ggplot(AirQualityUCI_final)+
  geom_point(aes(x=`NO2(GT)`,y=`C6H6(GT)`),col="#B79F00")

sca3 <- ggplot(AirQualityUCI_final)+
  geom_point(aes(x=`NO2(GT)`,y=`NOx(GT)`),col="#00BA38")

sca4 <- ggplot(AirQualityUCI_final)+
  geom_point(aes(x=`NO2(GT)`,y=`PT08.S5(O3)`),col="#00BFC4")

sca5 <- ggplot(AirQualityUCI_final)+
  geom_point(aes(x=`NO2(GT)`,y=`PT08.S4(NO2)`),col="#619CFF")

grid.arrange(sca1,sca2,sca3,sca4,sca5,nrow=2,ncol=3,top = "Relation between NO2 and other chemicals")

#line plot for all chemicals
cb7 <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","red")
cbind(AirQualityUCI_final[505:1224,1:5],as.data.frame(apply(AirQualityUCI_final[505:1224,7:15],2,scale))) %>% 
  melt(c("Date","year","month","day","Time")) %>% 
  group_by(Date,variable) %>% 
  summarise(value=mean(value,na.rm=T)) %>% 
  ggplot(aes(x=Date,y=value,color=variable))+
  geom_line(size=0.75)+
  ylab("value after scaling")+
  scale_color_manual(values=cb7)
 

#linear regression
lm_CO <- lm(`NO2(GT)`~`CO(GT)`,data=AirQualityUCI_final)
plot(lm_CO$residuals,ylab="residuals")
adf.test(lm_CO$residuals)
acf(lm_CO$residuals)
pacf(lm_CO$residuals)

lm_C6H6 <- lm(`NO2(GT)`~`C6H6(GT)`,data=AirQualityUCI_final)
plot(lm_C6H6$residuals,ylab="residuals")
adf.test(lm_C6H6$residuals)
acf(lm_C6H6$residuals) #殘差有高度自相關
pacf(lm_C6H6$residuals)
#linear regression after differencing
lm_CO_diff <- lm(diff(diff(NO2_Kalmen,24),1)~diff(diff(CO_GT_Kalmen,24),1))
plot(lm_CO_diff$residuals,ylab="residuals")
adf.test(lm_CO_diff$residuals)
acf(lm_CO_diff$residuals)
pacf(lm_CO_diff$residuals)

hist(scale(lm_CO_diff$residuals),col = "grey",freq = F,main="Histogram of Resuduals",xlim = c(-7.5,7.5))
curve(dnorm(x,mean=0,sd=1),add = T,col="red")

#fitting residual from ARIMA with other chemicals (dynamic regression)
#先進行差補
NO2_Kalmen
CO_GT_Kalmen <- na.StructTS(ts(AirQualityUCI_final$`CO(GT)`,frequency = 24))
C6H6_GT_Kalmen <- na.StructTS(ts(AirQualityUCI_final$`C6H6(GT)`,frequency = 24))
NOx_GT_Kalmen <- na.StructTS(ts(AirQualityUCI_final$`NOx(GT)`,frequency = 24))
O3_PT_Kalmen <- na.StructTS(ts(AirQualityUCI_final$`PT08.S5(O3)`,frequency = 24))
NO2_PT_Kalmen <- na.StructTS(ts(AirQualityUCI_final$`PT08.S4(NO2)`,frequency = 24))
chemicals_df_train <- data.frame(CO_GT_Kalmen,C6H6_GT_Kalmen,NOx_GT_Kalmen,O3_PT_Kalmen,
                           NO2_PT_Kalmen)[1:(9336-120),]
#chemicals_df_train_diff <- apply(chemicals_df_train,2,function(x){diff(diff(x,24),1)})

chemicals_df_test <- data.frame(CO_GT_Kalmen,C6H6_GT_Kalmen,NOx_GT_Kalmen,O3_PT_Kalmen,
           NO2_PT_Kalmen)[(9336-120+1):9336,]  
#chemicals_df_test_diff <- apply(chemicals_df_test,2,function(x){diff(diff(x,24),1)})

NO2_arima_xreg <- stats::arima(NO2_train,order = c(5,1,0),seasonal = list(order=c(4,1,1),period=24),xreg = chemicals_df_train)
#NO2_arima_xreg_diff <- stats::arima(diff(diff(NO2_train,24),1),order = c(5,0,0),seasonal = list(order=c(4,0,1),period=24),xreg = chemicals_df_train_diff)
MAPE(NO2_test,as.vector(predict(NO2_arima_xreg,newxreg =chemicals_df_test ,120)$pred))
#perform better!
ts.plot(as.vector(predict(NO2_arima_xreg,newxreg =chemicals_df_test ,120)$pred),col="red",xlab="index",
        ylab="")
par(new=T)
plot(NO2_test,xlab="",ylab="",xaxt='n',yaxt="n")
