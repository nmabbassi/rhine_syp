#DON'T RUN FOLLOWING - JUST FOR NOTES#
######################################
install.packages("tidyverse")
install.packages("readxl")
install.packages("ca")
install.packages("forecast")
install.packages("MLmetrics")
install.packages("fpp2")
install.packages("dplyr")
######################################


library(ggplot2)
library(readxl)
library(carData)
library(ggthemes)

#below used for forecasting counterfactual
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)
library(MLmetrics)


#load data
rh_data <- read_excel("~/Desktop/Rhine/Rhine_Chloride_Total.xlsx",
                      na="NA") 

rh_avg <- read_excel("~/Desktop/Rhine/Rhine_Chloride_Avg.xlsx",
                     na="NA") 
ad <- read_excel("argumentdata.xlsx")

rh_forecast <- read_excel("~/Desktop/Rhine/rhine_forecast.xlsx",
                          na="NA") 

rh_avgflow <- read_excel("~/Desktop/Rhine/rhine_avgflow.xlsx",
                         na="NA")


#ORGANIZE DATA

###create lines and labels for each country
####data frame for countries w  colors
labels <- c(
            `Switzerland` = "red4", 
            `Netherlands` = "dodgerblue4", 
            `Germany` = "slateblue",
            `France` = "darkgreen")

####treaty lines and data frame
treatyrat <- c(xintercept = 1983)
treatyimp <- c(xintercept = 1986)
protocolrat <- c(xintercept = 1991)
protocolimp <- c(xintercept = 1994)
dfone <- c(treatyrat,
           treatyimp,
           protocolimp,
           protocolrat)

dftest <- c(treatyrat = "gray1",
            treatysig = "gray2",
            protocolrat = "gray29",
            protocolimp = "gray30")

#fig. 1: total average flow rate
##code ggplot
ggplot(rh_avgflow, aes(x=`Year`))+
  geom_line(aes(y=`Switzerland`, 
                color="Switzerland"))+
  geom_line(aes(y=`Netherlands`, 
                color="Netherlands"))+
  geom_line(aes(y=`Germany`, 
                color="Germany"))+
  geom_line(aes(y=`France`, 
                color="France"))+
  labs(x=element_blank(), 
       y="Average annual flow rate in Rhine River (m3/s)", 
       title=element_blank(),
       fill=element_blank())+
  theme_gray()+ 
  theme(axis.title=element_text(size=12))+
  scale_color_manual(name=element_blank(),
                     values=c(labels))+
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=.15, color=1))


#fig. 2(?) cooperation conflict simple scatterplot
plot(ad, 
     xlab=expression("greater treaty cooperation" * symbol('\256')),
     ylab=expression("more costly for downstream states" * symbol('\256')))


#fig.3 - avg. chloride pollution in Rhine
#using both chloride ion pollution and chloride concentration
#concentration variable
rh_data$totalconc <- 100*(rh_data$`Total Averages`/rh_data$ConcentrationAvg)

#code ggplot
ggplot(rh_data, aes(x=`Year`, 
                    y=`Total Averages`,
                    color="     Chloride Ion Averages (kg/s)"))+
  geom_line(aes(y=totalconc, 
                color="      Chloride Concentration (kg/m3)"))+
  
  geom_line()+
  labs(x=element_blank(), 
       y="Chloride Pollution in Rhine River", 
       title=element_blank(),
       fill=element_blank())+
  theme_gray()+ 
  theme(axis.title=element_text(size=12))+
  
  #add vert lines for treaty introduction
  geom_vline(aes(xintercept=treatyrat,
                 color="   Treaty Ratified"),
             linetype = "solid",
             size=.5)+
  geom_vline(aes(xintercept = treatyimp,
                 color="  Treaty Implemented"),
             linetype = "longdash",
             size=.5)+
  geom_vline(aes(xintercept = protocolrat,
                 color=" Protocol Ratified"),
             linetype = "dotdash",
             size=.5)+
  geom_vline(aes(xintercept = protocolimp,
                 color="Protocol Implemented"),
             linetype = "twodash",
             size=.5)+
  #create legend
  scale_color_manual(name=element_blank(), 
                     values = c("blue3", "steelblue4", "gray1", "gray2", "gray29", "gray30"))+
              
  
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=0.15, color=1))


#####fig. 3 alternate#####
ggplot(rh_data, aes(x=`Year`, 
                    y=`Total Averages`,
                    color="    Chloride Ion Averages (kg/s)"))+
  geom_line()+
  labs(x=element_blank(), 
       y= "Chloride Pollution in Rhine River", 
       title=element_blank(),
       fill=element_blank())+
  theme_gray()+ 
  theme(axis.title=element_text(size=12))+
  #add vert lines for treaty introduction
  geom_vline(aes(xintercept = treatyrat,
                 color="   Treaty Ratified"),
             linetype = "solid",
             size=.5)+
  geom_vline(aes(xintercept = treatyimp,
                 color="  Treaty Implemented"),
             linetype = "longdash",
             size=.5)+
  geom_vline(aes(xintercept = protocolrat,
                 color=" Protocol Ratified"),
             linetype = "dotdash",
             size=.5)+
  geom_vline(aes(xintercept = protocolimp,
                 color="Protocol Implemented"),
             linetype = "twodash",
             size=.5)+
  #create legend
  scale_color_manual(name=element_blank(), 
                     values = c("steelblue4", "gray1", "gray2", "gray29", "gray30"))+
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=0.15, color=1))



#fig. 4 - avg. chloride pollution in rhine per state
#code ggplot
ggplot(rh_data, aes(x=`Year`))+
  geom_line(aes(y=`Switzerland`, 
                color="      Switzerland"))+
  geom_line(aes(y=`Netherlands`, 
                color="        Netherlands"))+
  geom_line(aes(y=`Germany`, 
                color="         Germany"))+
  geom_line(aes(y=`France`, 
                color="          France"))+
  
  labs(x=element_blank(), 
       y="Average Chloride Ion Pollution 
in Rhine River (kg/s) per State", 
       title=element_blank(),
       fill=element_blank(),
       color=element_blank())+
  
  #add horizontal lines for treaty info
  geom_vline(aes(xintercept=1983,
                 color="   Treaty Ratified"),
             linetype = "solid",
             size=.5)+
  geom_vline(aes(xintercept=1986,
                 color="  Treaty Implemented"),
             linetype = "longdash",
             size=.5)+
  geom_vline(aes(xintercept=1991,
                 color=" Protocol Ratified"),
             linetype = "dotdash",
             size=.5)+
  geom_vline(aes(xintercept=1994,
                 color="Protocol Implemented"),
             linetype = "twodash",
             size=.5)+
  
  
  #create legend
  scale_color_manual(name=element_blank(),
                     values=c(`          France` = "darkgreen",
                              `         Germany` = "slateblue",
                              `        Netherlands` = "dodgerblue4",
                              `      Switzerland` = "red4",
                              `   Treaty Ratified` = "gray1",
                              `  Treaty Implemented` = "gray1",
                              ` Protocol Ratified` = "gray30",
                              `Protocol Implemented` = "gray30"))+
                             
  theme_gray()+
  theme(axis.title=element_text(size=12))+
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=.15, color=1))


#fig.5 - chloride concentration in Rhine
#note flow rate data is limited - both in years and from monitoring stations
#calculating concentrations (kg/s) divided by (m3/s) = kg/m3
rh_data$swiss_conc <- 100*(rh_data$Switzerland/rh_data$FR_Switzerland)
rh_data$neth_conc <- 100*(rh_data$Netherlands/rh_data$FR_Netherlands)
rh_data$germ_conc <- 100*(rh_data$Germany/rh_data$FR_Germany)
rh_data$france_conc <- 100*(rh_data$France/rh_data$FR_France)

#code ggplot
ggplot(rh_data, aes(x=`Year`))+
  geom_line(aes(y=swiss_conc, 
                color="      Switzerland"))+
  geom_line(aes(y=neth_conc, 
                color="        Netherlands"))+
  geom_line(aes(y=germ_conc, 
                color="         Germany"))+
  geom_line(aes(y=france_conc, 
                color="          France"))+
  
  labs(x=element_blank(), 
       y="Chloride concentrations in Rhine River per state (kg/m3)", 
       title=element_blank(),
       fill=element_blank(),
       color=element_blank())+
  
  #add horizontal lines for treaty info
  geom_vline(aes(xintercept=1983,
                 color="   Treaty Ratified"),
             linetype = "solid",
             size=.5)+
  geom_vline(aes(xintercept=1986,
                 color="  Treaty Implemented"),
             linetype = "longdash",
             size=.5)+
  geom_vline(aes(xintercept=1991,
                 color=" Protocol Ratified"),
             linetype = "dotdash",
             size=.5)+
  geom_vline(aes(xintercept=1994,
                 color="Protocol Implemented"),
             linetype = "twodash",
             size=.5)+
  #code legend
  scale_color_manual(name=element_blank(),
                     values=c(`          France` = "darkgreen",
                              `         Germany` = "slateblue",
                              `        Netherlands` = "dodgerblue4",
                              `      Switzerland` = "red4",
                              `   Treaty Ratified` = "gray1",
                              `  Treaty Implemented` = "gray1",
                              ` Protocol Ratified` = "gray30",
                              `Protocol Implemented` = "gray30"))+
  theme_gray()+
  theme(axis.title=element_text(size=12))+
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=.15, color=1))

#fig. 6 - avg. chloride pollution at selected monitoring stations
ggplot(rh_data, aes(x=`Year`))+
  geom_line(aes(y=`Before AM`, 
                color="        Before AM (CH)"))+
  geom_line(aes(y=`After AM 1`, 
                color="       After AM 1 (FR)"))+
  geom_line(aes(y=`After AM 2`, 
                color="      After AM 2 (DE)"))+
  geom_line(aes(y=`After AM 3`, 
                color="     After AM 3 (NL)"))+
  geom_line(aes(y=`After AM 4`, 
                color="    After AM 4 (NL)"))+
  
  labs(x=element_blank(), 
       y="Chloride  pollution in Rhine at select monitoring stations (kg/s)", 
       title=element_blank(),
       fill=element_blank(),
       color=element_blank())+
  
  #add horizontal lines for treaty info
  geom_vline(aes(xintercept=1983,
                 color="   Treaty Ratified"),
             linetype = "solid",
             size=.5)+
  geom_vline(aes(xintercept=1986,
                 color="  Treaty Implemented"),
             linetype = "longdash",
             size=.5)+
  geom_vline(aes(xintercept=1991,
                 color=" Protocol Ratified"),
             linetype = "dotdash",
             size=.5)+
  geom_vline(aes(xintercept=1994,
                 color="Protocol Implemented"),
             linetype = "twodash",
             size=.5)+
  
  #code legend
  scale_color_manual(name=element_blank(),
                     values=c(`        Before AM (CH)` = "red4",
                              `       After AM 1 (FR)` = "darkgreen",
                              `      After AM 2 (DE)` = "slateblue",
                              `     After AM 3 (NL)` = "dodgerblue4",
                              `    After AM 4 (NL)` = "dodgerblue1",
                              `   Treaty Ratified` = "gray1",
                              `  Treaty Implemented` = "gray1",
                              ` Protocol Ratified` = "gray30",
                              `Protocol Implemented` = "gray30"))+
  theme_gray()+
  theme(axis.title=element_text(size=12))+
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=.15, color=1))

#fig. 7 naive forecasting model
glimpse(rh_forecast)

  #time series data object (ts)
  #frequency is annual
rh_ts <- ts(rh_forecast[, 2], start = c(1954, 1), end = c(1975, 1), frequency = 1)
    #mean absolute percentage error (MAPE), used to evaluate performance of forecasting models
    ##lower MAPE value = better model
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
               return (mape)
}

      #reads in time series object (rh_ts)
      ##h is number of values forecast (2003-1975)
      ###3 predictive intervals: 80%, 95%, 99%
        naive_mod <- naive(rh_ts, h = 28, level=c(80, 95, 99))
        summary(naive_mod)

        rh_forecast$naive = 106.77
        mape(rh_forecast$`Total Averages`, rh_forecast$naive) ##13.7%
              ###MAPE error is 13.7% - not terrible!
  
  #code ggplot 
        forecast <- read_excel("~/Desktop/Rhine/naive.xlsx",
                                 na="NA")       
        ggplot(forecast, aes(x=`Year`))+
          geom_line(aes(y=Lo80, 
                        color="     Low 80%"))+
          geom_line(aes(y=Hi80, 
                        color="    High 80%"))+
          geom_line(aes(y=Lo95, 
                        color="   Low 95%"))+
          geom_line(aes(y=Hi95, 
                        color="  High 95%"))+
          geom_line(aes(y=Lo99, 
                        color=" Low 99%"))+
          geom_line(aes(y=Hi99, 
                        color="High 99%"))+
          labs(x=element_blank(), 
               y="Naive forecast of chloride pollution in Rhine (kg/s)", 
               title=element_blank(),
               fill=element_blank(),
               color=element_blank())+
          scale_color_manual(name=element_blank(),
                             values=c(`     Low 80%` = "tomato4",
                                      `    High 80%` = "tomato3",
                                      `   Low 95%` = "dodgerblue4",
                                      `  High 95%` = "dodgerblue3",
                                      ` Low 99%` = "gray4",
                                      `High 99%` = "gray3"))+
          theme_gray()+
          theme(axis.title=element_text(size=12))+
          theme(legend.position="bottom", legend.background=element_rect
                (fill="gray95", linetype=1, size=.15, color=1))
        
#simple exponential smoothing
        se_model <- ses(rh_ts, h=28, level=c(80, 95, 99))
        summary(se_model)
        rh_se <- as.data.frame(se_model)
        summary(rh_se)
  #alpha shows this might not be the best model to use
        se_excel <- read_excel("~/Desktop/Rhine/semodel.xlsx",
                                  na="NA") 
        
        ggplot(se_excel, aes(x=Year))+ 
          geom_line(aes(y=lo80, 
                        color="     Low 80%"))+
          geom_line(aes(y=hi80, 
                        color="    High 80%"))+
          geom_line(aes(y=lo95, 
                        color="   Low 95%"))+
          geom_line(aes(y=hi95, 
                        color="  High 95%"))+
          geom_line(aes(y=lo99, 
                        color=" Low 99%"))+
          geom_line(aes(y=hi99, 
                        color="High 99%"))+
          labs(x=element_blank(), 
               y="Simple exponential smoothing forecast of 
chloride pollution in Rhine (kg/s)", 
               title=element_blank(),
               fill=element_blank(),
               color=element_blank())+
          scale_color_manual(name=element_blank(),
                             values=c(`     Low 80%` = "tomato4",
                                      `    High 80%` = "tomato3",
                                      `   Low 95%` = "dodgerblue4",
                                      `  High 95%` = "dodgerblue3",
                                      ` Low 99%` = "black",
                                      `High 99%` = "black"))+
          theme_gray()+
          theme(axis.title=element_text(size=12))+
          theme(legend.position="bottom", legend.background=element_rect
                (fill="gray95", linetype=1, size=.15, color=1))

#holt's trend method
holt_model <- holt(rh_ts, h=28, level=c(80, 95, 99))
rh_holt <- as.data.frame(holt_model)
summary(rh_holt)

holt_excel <- read_excel("~/Desktop/Rhine/holt.xlsx",
                          na="NA") 

ggplot(holt_excel, aes(x=`Year`))+ 
  geom_line(aes(y=lo80, 
                color="     Low 80%"))+
  geom_line(aes(y=hi80, 
                color="    High 80%"))+
  geom_line(aes(y=lo95, 
                color="   Low 95%"))+
  geom_line(aes(y=hi95, 
                color="  High 95%"))+
  geom_line(aes(y=lo99, 
                color=" Low 99%"))+
  geom_line(aes(y=hi99, 
                color="High 99%"))+
  labs(x=element_blank(), 
       y="Holt forecast of chloride pollution in Rhine (kg/s)", 
       title=element_blank(),
       fill=element_blank(),
       color=element_blank())+
  scale_color_manual(name=element_blank(),
                     values=c(`     Low 80%` = "tomato4",
                              `    High 80%` = "tomato3",
                              `   Low 95%` = "dodgerblue4",
                              `  High 95%` = "dodgerblue3",
                              ` Low 99%` = "black",
                              `High 99%` = "black"))+
  theme_gray()+
  theme(axis.title=element_text(size=12))+
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=.15, color=1))




#ARIMA model
arima_model <- auto.arima(rh_ts)
summary(arima_model)
    #MAPE is 5.3%

fore_arima = forecast::forecast(arima_model, h=28, level=c(80, 95, 99))
rh_arima = as.data.frame(fore_arima)
summary(rh_arima)


arima_excel <- read_excel("~/Desktop/Rhine/arima.xlsx",
                       na="NA") 

ggplot(arima_excel, aes(x=Year))+ 
  geom_line(aes(y=lo80, 
                color="80% CI"))+
  geom_line(aes(y=lo95, 
                color="95% CI"))+
  geom_line(aes(y=lo99, 
                color="99% CI"))+
  geom_line(aes(y=counter_ivs, 
                color="Predicted Counterfactual"))+
  geom_line(aes(y=actual, 
                color="Actual"))+
  labs(x=element_blank(), 
       y="Arima forecast of chloride pollution in Rhine (kg/s)", 
       title=element_blank(),
       fill=element_blank(),
       color=element_blank())+
  scale_color_manual(name=element_blank(),
                     values=c(`80% CI` = "tomato4",
                              `95% CI` = "tomato3",
                              `99% CI` = "indianred",
                              `Predicted Counterfactual` = "cyan4",
                              `Actual` = "black"))+
  theme_gray()+
  theme(axis.title=element_text(size=12))+
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=.15, color=1))


#TBATS
model_tbats <- tbats(rh_ts)
summary(model_tbats)
for_tbats <- forecast::forecast(model_tbats, h=28, level=c(80, 95, 99))
df_tbats = as.data.frame(for_tbats)

tbats_excel <- read_excel("~/Desktop/Rhine/tbats.xlsx",
                          na="NA") 

ggplot(tbats_excel, aes(x=Year))+ 
  geom_line(aes(y=lo80, 
                color="     Low 80%"))+
  geom_line(aes(y=hi80, 
                color="    High 80%"))+
  geom_line(aes(y=lo95, 
                color="   Low 95%"))+
  geom_line(aes(y=hi95, 
                color="  High 95%"))+
  geom_line(aes(y=lo99, 
                color=" Low 99%"))+
  geom_line(aes(y=hi99, 
                color="High 99%"))+
  labs(x=element_blank(), 
       y="TBATS forecast of chloride pollution in Rhine (kg/s)", 
       title=element_blank(),
       fill=element_blank(),
       color=element_blank())+
  scale_color_manual(name=element_blank(),
                     values=c(`     Low 80%` = "tomato4",
                              `    High 80%` = "tomato3",
                              `   Low 95%` = "dodgerblue4",
                              `  High 95%` = "dodgerblue3",
                              ` Low 99%` = "black",
                              `High 99%` = "black"))+
  theme_gray()+
  theme(axis.title=element_text(size=12))+
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=.15, color=1))





