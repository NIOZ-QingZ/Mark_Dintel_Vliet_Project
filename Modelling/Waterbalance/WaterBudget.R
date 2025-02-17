
# dVol = Qin-Qout

require(lubridate)
library(readr)
require(dplyr)

setwd("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet_Project_Shared\\Modelling\\Waterbalance")# Set the working directory

#### Data preparation ####
# WL: Water depths
# Up: Upstream of sluice
# Down: Downstream of sluice

WL_Down_Vliet <- read_csv("WL_Down_Vliet.csv",col_types = cols(Datum = col_datetime(format = "%d-%m-%Y %H:%M:%S"), Eenheid = col_skip(),Serie = col_skip(), X5 = col_skip()))
WL_Down_Dintel <- read_csv("WL_Down_Dintel.csv",col_types = cols(Datum = col_datetime(format = "%d-%m-%Y %H:%M:%S"), Eenheid = col_skip(),Serie = col_skip(), X5 = col_skip()))
names(WL_Down_Vliet) <- c("Datum", "WL_Down_Vliet")
names(WL_Down_Dintel) <- c("Datum", "WL_Down_Dintel")
# Water depth measurements at downstream of Dintel and Vliet as indicator of Water depth dynamics in Volkerakmeer

WL_Up_Vliet <- read_csv("WL_Up_Vliet.csv",col_types = cols(Datum = col_datetime(format = "%d-%m-%Y %H:%M:%S"), Eenheid = col_skip(),Serie = col_skip(), X5 = col_skip()))
WL_Up_Dintel <- read_csv("WL_Up_Dintel.csv",col_types = cols(Datum = col_datetime(format = "%d-%m-%Y %H:%M:%S"), Eenheid = col_skip(),Serie = col_skip(), X5 = col_skip()))
names(WL_Up_Vliet) <- c("Datum", "WL_Up_Vliet")
names(WL_Up_Dintel) <- c("Datum", "WL_Up_Dintel")
# Water depth measurements at upstream of Dintel and Vliet as indicator of Water depth dynamics in Mark-Dintel-Vliet system

# Gastel (Molenbeak) water depth measurements
WL_Gastel <- read_csv("WL_Gastel.csv",col_types = cols(Datum = col_datetime(format = "%d-%m-%Y %H:%M:%S"), Eenheid = col_skip(),Serie = col_skip(), X5 = col_skip()))
names(WL_Gastel) <- c("Datum", "WL_Gastel")

# Inflow discharges:
HF_Qin_MarkCanal <- read_csv("HF_Qin_MarkCanal.csv",col_types = cols(Datum = col_datetime(format = "%d-%m-%Y %H:%M:%S"), Eenheid = col_skip(),Serie = col_skip(), X5 = col_skip()))
names(HF_Qin_MarkCanal) <- c("Datum","Qin_MarkCanal")

HF_Qin_Oranjeboombrug <- read_csv("HF_Qin_Oranjeboombrug.csv",col_types = cols(Datum = col_datetime(format = "%d-%m-%Y %H:%M:%S"), Eenheid = col_skip(),Serie = col_skip(), X5 = col_skip()))
names(HF_Qin_Oranjeboombrug) <- c("Datum","Qin_Oranjeboombrug")

HF_Qin_BlauweKamer <- read_csv("HF_Qin_BlauweKamer.csv",
                               col_types = cols(Datum = col_datetime(format = "%d-%m-%Y %H:%M:%S"), Eenheid = col_skip(),Serie = col_skip(), X5 = col_skip()))
names(HF_Qin_BlauweKamer) <- c("Datum","Qin_BlauweKamer")

HF_Qin_Molenbeak <- read_csv("HF_Qin_Molenbeak.csv",
                             col_types = cols(Datum = col_datetime(format = "%d-%m-%Y %H:%M:%S"), Eenheid = col_skip(),Serie = col_skip(), X5 = col_skip()))
names(HF_Qin_Molenbeak) <- c("Datum","Qin_Molenbeak")

# Outflow discharges:
HF_Qout_Dintelsas <- read_csv("HF_Qout_Dintelsas.csv",
                              col_types = cols(Datum = col_datetime(format = "%d-%m-%Y %H:%M:%S"), Eenheid = col_skip(),Serie = col_skip(), X5 = col_skip()))
names(HF_Qout_Dintelsas) <- c("Datum", "Qout_Dintelsas")

HF_Qout_Vliet <- read_csv("HF_Qout_Vliet.csv",
                          col_types = cols(Datum = col_datetime(format = "%d-%m-%Y %H:%M:%S"), Eenheid = col_skip(),Serie = col_skip(), X5 = col_skip()))
names(HF_Qout_Vliet) <- c("Datum", "Qout_Vliet")

# Integrating dataset #
sTime_numeric <- seq(HF_Qin_MarkCanal$Datum[1]%>%as.numeric,HF_Qin_MarkCanal$Datum[length(HF_Qin_MarkCanal$Datum)]%>%as.numeric, by=3600) #hourly interval

WaterBudget_MDV <- data.frame(Datum = ymd_hms("1970-01-01 00:00:00")+sTime_numeric,WL_Down_Dintel=NA,WL_Down_Vliet=NA,WL_Up_Dintel=NA,WL_Up_Vliet=NA,WL_Gastel=NA,Qin_MarkCanal=NA,Qin_BlauweKamer=NA,Qin_Molenbeak=NA,Qin_Oranjeboombrug=NA,Qout_Dintelsas=NA,Qout_Vliet=NA)

WaterBudget_MDV$WL_Down_Dintel <- approx(WL_Down_Dintel$Datum%>%as.numeric,WL_Down_Dintel$WL_Down_Dintel,xout=WaterBudget_MDV$Datum%>%as.numeric)$y
WaterBudget_MDV$WL_Down_Vliet <- approx(WL_Down_Vliet$Datum%>%as.numeric,WL_Down_Vliet$WL_Down_Vliet,xout=WaterBudget_MDV$Datum%>%as.numeric)$y

WaterBudget_MDV$WL_Up_Dintel <- approx(WL_Up_Dintel$Datum%>%as.numeric,WL_Up_Dintel$WL_Up_Dintel,xout=WaterBudget_MDV$Datum%>%as.numeric)$y
WaterBudget_MDV$WL_Up_Vliet <- approx(WL_Up_Vliet$Datum%>%as.numeric,WL_Up_Vliet$WL_Up_Vliet,xout=WaterBudget_MDV$Datum%>%as.numeric)$y
WaterBudget_MDV$WL_Gastel <- approx(WL_Gastel$Datum%>%as.numeric,WL_Gastel$WL_Gastel, xout = WaterBudget_MDV$Datum%>%as.numeric)$y

WaterBudget_MDV$Qin_MarkCanal <- approx(HF_Qin_MarkCanal$Datum%>%as.numeric,HF_Qin_MarkCanal$Qin_MarkCanal,xout = WaterBudget_MDV$Datum%>%as.numeric)$y
WaterBudget_MDV$Qin_BlauweKamer <- approx(HF_Qin_MarkCanal$Datum%>%as.numeric,HF_Qin_MarkCanal$Qin_MarkCanal,xout = WaterBudget_MDV$Datum%>%as.numeric)$y
WaterBudget_MDV$Qin_Molenbeak <- approx(HF_Qin_Molenbeak$Datum%>%as.numeric,HF_Qin_Molenbeak$Qin_Molenbeak,xout = WaterBudget_MDV$Datum%>%as.numeric)$y
WaterBudget_MDV$Qin_Oranjeboombrug <- approx(HF_Qin_Oranjeboombrug$Datum%>%as.numeric,HF_Qin_Oranjeboombrug$Qin_Oranjeboombrug,xout = WaterBudget_MDV$Datum%>%as.numeric)$y

WaterBudget_MDV$Qout_Vliet <- approx(HF_Qout_Vliet$Datum%>%as.numeric,HF_Qout_Vliet$Qout_Vliet,xout = WaterBudget_MDV$Datum%>%as.numeric)$y
WaterBudget_MDV$Qout_Dintelsas <- approx(HF_Qout_Dintelsas$Datum%>%as.numeric,HF_Qout_Dintelsas$Qout_Dintelsas,xout = WaterBudget_MDV$Datum%>%as.numeric)$y

#### Compare the measurements at Dintel and Vliet downsteram as indicator of water depth dynamics at Volkerakmeer ####
plot(WaterBudget_MDV$Datum, WaterBudget_MDV$WL_Down_Dintel)
lines(WaterBudget_MDV$Datum, WaterBudget_MDV$WL_Down_Vliet, col=2)
plot(WaterBudget_MDV$WL_Down_Dintel, WaterBudget_MDV$WL_Down_Vliet)
lines(-2:2,-2:2,col=2) # 1:1 line

#### Compare depth measurements inside MDV system
plot(WaterBudget_MDV$Datum, WaterBudget_MDV$WL_Up_Dintel)
lines(WaterBudget_MDV$Datum, WaterBudget_MDV$WL_Up_Vliet, col=2)
lines(WaterBudget_MDV$Datum, WaterBudget_MDV$WL_Gastel,col=3)

#### Closing the water balance by adjusting the inflows at Gastel (Molenbeak)
Surface <- 50*(114-0)*40+50*(640-114)*50+50*(666-640)*100+50*(743-666)*40+50*(760-743)*26+50*(1117-760)*30 # m^2
WaterBudget_MDV$dt <- c(1, diff(as.numeric(WaterBudget_MDV$Datum))) # seconds
range(WaterBudget_MDV$dt)
WaterBudget_MDV <- WaterBudget_MDV[which(WaterBudget_MDV$dt!=0),]
WaterBudget_MDV$dV <- c(0, diff(WaterBudget_MDV$WL_Up_Dintel))*Surface # m^3

#### Calculting the rest discharge to close the water budget ####
WaterBudget_MDV$Q_diff <- WaterBudget_MDV$dV/WaterBudget_MDV$dt # m3/s

#### Compare it with Measurements at Molenbeak ####
plot(WaterBudget_MDV$Datum,WaterBudget_MDV$Q_diff, type = "l",ylim = c(-100,100))  
lines(WaterBudget_MDV$Datum,WaterBudget_MDV$Qin_Molenbeak, col=2)
plot(WaterBudget_MDV$Q_diff, WaterBudget_MDV$Qin_Molenbeak)
lines(-100:100,-100:100,col="red") # add 1:1 line

#### Output the balanced water budget data
#write.csv(WaterBudget_MDV,"WaterBudget_MDV.csv")
