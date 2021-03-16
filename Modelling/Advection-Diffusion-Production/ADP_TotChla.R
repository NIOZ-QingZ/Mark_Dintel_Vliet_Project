require(lubridate)
require(dplyr)

### Water Budget ####
WaterBudget <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\Waterbalance\\WaterBudget.csv")

sTime=WaterBudget$sTime

### Input data preparation ####
HF_BlueCh_MarkCanal <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\HF_BlueCh_MarkCanal.csv")
HF_BlueCh_DintelCanal <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\HF_BlueCh_DintelCanal.csv")
HF_BlueCh_Vliet <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\HF_BlueCh_Vliet.csv")

HF_Chla_MarkCanal <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\HF_Chla_MarkCanal.csv")
HF_Chla_DintelCanal <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\HF_Chla_DintelCanal.csv")
HF_Chla_Vliet <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\HF_Chla_Vliet.csv")

mL <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\data\\MeteorologicalData\\Radiance.csv")
mL$Time <- paste(mL$YYYYMMDD, mL$HH)%>%ymd_h%>%as.numeric
mL <- approx(x=mL$Time,y=mL$Q,xout=WaterBudget$sTime)%>%as.data.frame()

MDV_data <- data.frame(sTime=WaterBudget$sTime, Date=WaterBudget$Date%>%ymd_hms, Temp=NA, Qin_MarkCanal=WaterBudget$Qin_MarkCanal, Qin_Oranjeboombrug=WaterBudget$Qin_Oranjeboombrug, Qin_BlauweKamer=WaterBudget$Qin_BlauweKamer, Qin_Molenbeak=WaterBudget$Qin_Molenbeak, Qout_Dintelsas=WaterBudget$Qout_Dintelsas, Qout_Vliet=WaterBudget$Qout_Vliet, Qout_Polder=WaterBudget$Qout_Polder, BlueCh_MarkCanal=NA, BlueCh_DintelCanal=NA, BlueCh_Vliet=NA, TotChla_MarkCanal=NA, TotChla_DintelCanal=NA, TotChla_Vliet=NA, mL=NA)

# global radiance:
MDV_data$mL <- mL$y[which(mL$x%in%MDV_data$sTime)]

# Temperature data
HF_Temp <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\HF_Temp_MarkCanal.csv")
MDV_data$Temp <- approx(HF_Temp$Datum%>%dmy_hms%>%as.numeric, HF_Temp$Waarde, xout=MDV_data$sTime)$y
  
# Cyanobacteria measurements
MDV_data$BlueCh_MarkCanal <- approx(HF_BlueCh_MarkCanal$Datum%>%dmy_hms%>%as.numeric, HF_BlueCh_MarkCanal$Waarde, xout=WaterBudget$sTime)$y
MDV_data$BlueCh_MarkCanal[which(MDV_data$BlueCh_MarkCanal<0)]<-0
MDV_data$BlueCh_DintelCanal <- approx(HF_BlueCh_DintelCanal$Datum%>%dmy_hms%>%as.numeric, HF_BlueCh_DintelCanal$Waarde, xout=sTime)$y
MDV_data$BlueCh_DintelCanal[which(MDV_data$BlueCh_DintelCanal<0)]<-0
MDV_data$BlueCh_Vliet <- approx(HF_BlueCh_Vliet$Datum%>%dmy_hms%>%as.numeric, HF_BlueCh_Vliet$Waarde, xout=sTime)$y
MDV_data$BlueCh_Vliet[which(MDV_data$BlueCh_Vliet<0)]<-0

# Chla measurements
MDV_data$TotChla_MarkCanal <- approx(HF_Chla_MarkCanal$Datum%>%dmy_hms%>%as.numeric, HF_Chla_MarkCanal$Waarde, xout = sTime)$y
MDV_data$TotChla_MarkCanal[which(MDV_data$Chla_MarkCanal<0)]<-0
MDV_data$TotChla_DintelCanal <- approx(HF_Chla_DintelCanal$Datum%>%dmy_hms%>%as.numeric, HF_Chla_DintelCanal$Waarde, xout = sTime)$y
MDV_data$TotChla_DintelCanal[which(MDV_data$Chla_DintelCanal<0)]<-0
MDV_data$TotChla_Vliet <- approx(HF_Chla_Vliet$Datum%>%dmy_hms%>%as.numeric, HF_Chla_Vliet$Waarde, xout = sTime)$y
MDV_data$TotChla_Vliet[which(MDV_data$Chla_Vliet<0)]<-0

MDV_data$dChla_MarkCanal <- MDV_data$TotChla_MarkCanal-MDV_data$BlueCh_MarkCanal
MDV_data$dChla_MarkCanal[which(MDV_data$dChla_MarkCanal<0)]<-0
MDV_data$dChla_DintelCanal <- MDV_data$TotChla_DintelCanal-MDV_data$BlueCh_DintelCanal
MDV_data$dChla_DintelCanal[which(MDV_data$dChla_DintelCanal<0)]<-0
MDV_data$dChla_Vliet <- MDV_data$TotChla_Vliet-MDV_data$BlueCh_Vliet
MDV_data$dChla_Vliet[which(MDV_data$dChla_Vliet<0)] <- 0

MDV_data <- MDV_data%>%na.omit

#sTime=seq(mL2018$Time[2],mL2018$Time[length(mL2018$Time)],by=60*30)


# Nutrient data ####
PO4_MarkCanal <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\LF_PO4_MarkCanal.csv")
PO4_DintelCanal <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\LF_PO4_DintelCanal.csv")
PO4_MVCanal <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\LF_PO4_MVCanal.csv")
PO4_Vliet <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\LF_PO4_Vliet.csv")

MDV_data$PO4_MarkCanal <- NA
MDV_data$PO4_DintelCanal <- NA
MDV_data$PO4_MVCanal <- NA
MDV_data$PO4_Vliet <- NA

PO4_MarkCanal<-approx(PO4_MarkCanal$Datum%>%dmy_hms%>%as.numeric,PO4_MarkCanal$Waarde, xout=sTime)%>%as.data.frame()
MDV_data$PO4_MarkCanal[which(MDV_data$sTime%in%PO4_MarkCanal$x)]<- PO4_MarkCanal$y[which(PO4_MarkCanal$x%in%MDV_data$sTime)]
PO4_DintelCanal <- approx(PO4_DintelCanal$Datum%>%dmy_hms%>%as.numeric,PO4_DintelCanal$Waarde, xout = sTime)%>%as.data.frame()
MDV_data$PO4_DintelCanal[which(MDV_data$sTime%in%PO4_DintelCanal$x)] <- PO4_DintelCanal$y[which(PO4_DintelCanal$x%in%MDV_data$sTime)]
PO4_MVCanal <- approx(PO4_MVCanal$Datum%>%dmy_hms%>%as.numeric,PO4_MVCanal$Waarde, xout = sTime)%>%as.data.frame()
MDV_data$PO4_MVCanal[which(MDV_data$sTime%in%PO4_MVCanal$x)] <- PO4_MVCanal$y[which(PO4_MVCanal$x%in%MDV_data$sTime)]
PO4_Vliet <- approx(PO4_Vliet$Datum%>%dmy_hms%>%as.numeric,PO4_Vliet$Waarde,xout=sTime)%>%as.data.frame()
MDV_data$PO4_Vliet[which(MDV_data$sTime%in%PO4_Vliet$x)] <- PO4_Vliet$y[which(PO4_Vliet$x%in%MDV_data$sTime)]

MDV_data <- na.omit(MDV_data)




# PO4 concentration (mg/L)####
plot(MDV_data$Date, MDV_data$PO4_MarkCanal, col=1, ylim = c(0,0.12), ylab = "PO4 (mg/L)", type="l")
lines(MDV_data$Date, MDV_data$PO4_DintelCanal, col=2)
lines(MDV_data$Date, MDV_data$PO4_MVCanal, col=3)
lines(MDV_data$Date, MDV_data$PO4_Vliet, col=4)
legend("topright",legend = c("MarkCanal","DintelCanal","MV_Canal","Vliet"),col=1:4,text.col = 1:4,lty=1)

####
plot(MDV_data$mL)
lines(cLDayAve-cLDayVar*cos(2*pi*(MDV_data$sTime+TenDays)/DaysPerYear/SecondPerDay),col="red")

legend("topright",legend = c("measurements","default"),text.col=c("black","red"),col=c("black","red"),lty=1)

#### Parameter set-up in model ####

Num_Place_Steps <- 6
Num_Time_Steps <- length(MDV_data$sTime)#1e5
Time_step <- 60*30 # delta t, at which derivative will be calculated

# Coversion parameters
SecondPerDay <- 86400 # seconds per day
TenDays <- 10*SecondPerDay # s
DaysPerYear <- 365 # d
gPermg <- 0.001

#Place_step <- 50 # meter
Dispersion <- 7.5 # m2/s

# Chlorophyll/C
cChDDiat <- (0.004+0.012)/2
cChDGren <- (0.01+0.02)/2
cChDBlue <- (0.005+0.015)/2
cChD.vec <- c(BlueCh=cChDBlue, GreenCh=cChDGren, DiatCh=cChDDiat)
#(cChDDiat+cChDGren+cChDBlue)

# maximum growth rate
cMuMaxDiat <- 2 #(day^-1)
cMuMaxGren <- 1.5 #(day^-1)
cMuMaxBlue <- 0.6 #(day^-1)
cMuMax.vec <- c(BlueCh=cMuMaxBlue, GreenCh=cChDGren, DiatCh=cChDDiat)
#((cMuMaxDiat+cMuMaxGren+cMuMaxBlue)/3) # /delta t

# maintenance respiration constant rate
kDRespDiat <- 0.1 #(day^-1)
kDRespGren <- 0.075 #(day^-1)
kDRespBlue <- 0.03 #(day^-1)
kDResp.vec <- c(BlueCh=kDRespBlue, GreenCh=kDRespGren, DiatCh=kDRespDiat)
#((kDRespDiat+kDRespGren+kDRespBlue)/3) # /delta t

# mortality constant
kMortDiat <- 0.01 #(day^-1)
kMortGren <- 0.01 #(day^-1)
kMortBlue <- 0.01 #(day^-1)
kMort.vec <- c(BlueCh=kMortBlue, GreenCh=kMortGren, DiatCh=kMortDiat)
#((kMortDiat+kMortGren+kMortBlue)/3) /delta t

# grazing loss rate
kLossDiat <- 0.25 #(day^-1)
kLossGren <- 0.25 #(day^-1)
kLossBlue <- 0.03 #(day^-1)
kLoss.vec <- c(BlueCh=kLossBlue, GreenCh=kLossGren, DiatCh=kLossDiat)
#((kLossDiat+kLossBlue+kLossGren)/3) # /delta t

# Temperature function
cTmRef <- 20

cTmOptDiat <- 18
cTmOptGren <- 25
cTmOptBlue <- 25
cTmOpt.vec <- c(BlueCh=cTmOptBlue, GreenCh=cTmOptGren, DiatCh=cTmOptDiat)

cSigTmDiat <- 20 # sigma in Gaussian curve
cSigTmGren <- 15
cSigTmBlue <- 12
cSigTm.vec <- c(BlueCh=cSigTmBlue, GreenCh=cSigTmGren, DiatCh=cSigTmDiat)
  
# radiation
cLDayAve <- 1e7/SecondPerDay #J/m2/s = W/m2
cLDayVar <- 8e6/SecondPerDay #J/m2/s = W/m2
fPAR <- 0.48 # fraction photosynthesically active radiation (PAR)
fRefl <- 0.2 # the fraction photosynthetically active radiation reflected at the surface

# Day length
cfDayAve <- 0.5
cfDayVar <- 0.2

# light extinction coeffcients
cExtWat <- 0.5 # m^-1
cExtSpDet <- 0.15 # m2/gDW
cExtSpIM <- 0.05 # m2/gDW

# half-sat PAR at 20 degrees
hLRefDiat <- 1000 # Fake value, W/m2 = J/m2/s
hLRefGren <- 17 # W/m2
hLRefBlue <- 1000 # Fake value, W/m2
hLRef.vec <- c(BlueCh=hLRefBlue, GreenCh=hLRefGren, DiatCh=hLRefDiat)

cLOptRefDiat <- 54 # W/m2
cLOptRefGren <- 1000 # W/m2, Fake value
cLOptRefBlue <- 13.6 # W/m2
cLOptRef.vec <- c(BlueCh=cLOptRefBlue, GreenCh=cLOptRefGren, DiatCh=cLOptRefDiat)

# half saturation constant of resource
hR <- 0.18 # mg/l


# functions ####
## Temperature function
TmFunc = function(uTm, cSigTm, cTmOpt){
  return(exp(-0.5/(cSigTm^2)*((uTm-cTmOpt)^2-(cTmRef-cTmOpt))))
} 

## Light limitation
SteeleFunc <- function(aExtCoef, sDepthW, ISurf, uTm, cSigTm, cTmOpt, cLOptRef,TmFunc_value){
  IBottom = ISurf*exp(-aExtCoef*sDepthW)
  uLOptRef = cLOptRef*TmFunc_value
  return(exp(1)/(aExtCoef*sDepthW)*(exp(-IBottom/uLOptRef)-exp(-ISurf/uLOptRef)))
} # Tilman (1982), Huisman and Weissing (1994, 1995)

LehmanFunc <- function(aExtCoef,sDepthW,ISurf,uTm, cSigTm, cTmOpt,hLRef,TmFunc_value){
  IBottom = ISurf*exp(-aExtCoef*sDepthW)
  uhLRef = hLRef*TmFunc_value
  return(1/(aExtCoef*sDepthW)*log((uhLRef*TmFunc_value+ISurf)/(uhLRef*TmFunc_value+IBottom)))
}

## Nutrient limitation
PLimFunc = function(P){return(P/(P+hR))}


#### Morphological size ####

#Conc0 <- 50 # ug/l
#Dim TotChla(1117,10000)
#Dim Flowrate(1117)
#Too_big_t_step <- 0
#starting_growth <- 1/24 # ug/l/s: numer ug/l that's added every second
#Monod_Speed <- 0.05
#Monod_Conc <- 50

#num <- (0.1/2)*(Place_step-0.1*Time_step)

Begin_Mark_canal <- 1
Width_Mark_canal <- 40
Depth_Mark_canal <- 2
Length_Mark_canal <- 114*50

Begin_Dintel <- 2 # Place step 114 transition Mark canal to Dintel
Width_Dintel <- 50
Depth_Dintel <- 3
Length_Dintel <- (640-114)*50

Zevenbergen <- 401

Begin_Dintel_wide <- 3 # Here the Dintel becomes much wider
Width_Dintel_wide <- 100
Depth_Dintel_wide <- 3.3
Length_Dintel_wide <- (666-640)*50

Begin_MV_canal <- 4 # place step 666 water disappears in the direction of the non-modelled branch to the discharge sluice_ Transition to the Mark-Vliet canal profile
Width_MV_canal <- 40
Depth_MV_canal <- 2.8
Length_MV_canal <- (743-666)*50

Begin_Vliet_narrow <- 5
Width_Vliet_narrow <- 26
Depth_Vliet_narrow <- 1.53
Length_Vliet_narrow <- (760-743)*50

Begin_Vliet <- 6
Width_Vliet <- 30
Depth_Vliet <- 2.4
Length_Vliet <- (1117-760)*50

#### Flowrate simulation based on mass conservation: ####
Flowrate <- matrix(data=NA, nrow=Num_Place_Steps,ncol = Num_Time_Steps,dimnames = list(paste("Place_step",0:(Num_Place_Steps-1)),paste("Time_step",0:(Num_Time_Steps-1))))

for (t in 1:Num_Time_Steps) {
  for (x in Begin_Mark_canal) {
    Flowrate[x,t] = MDV_data$Qin_MarkCanal[t]/(Width_Mark_canal*Depth_Mark_canal) # m/s
  }
  
  for (x in Begin_Dintel){
    Flowrate[x,t]=(MDV_data$Qin_MarkCanal[t]+MDV_data$Qin_Oranjeboombrug[t]+MDV_data$Qin_BlauweKamer[t]-MDV_data$Qout_Polder[t])/(Width_Dintel*Depth_Dintel) # m/s
  }
  
  for (x in Begin_Dintel_wide){
    Flowrate[x,t]=(MDV_data$Qin_MarkCanal[t]+MDV_data$Qin_Oranjeboombrug[t]+MDV_data$Qin_BlauweKamer[t]-MDV_data$Qout_Polder[t])/(Width_Dintel_wide*Depth_Dintel_wide) # m/s
  }
  
  for (x in Begin_MV_canal){
    Flowrate[x,t]=(MDV_data$Qin_MarkCanal[t]+MDV_data$Qin_Oranjeboombrug[t]+MDV_data$Qin_BlauweKamer[t]-MDV_data$Qout_Dintelsas[t]-MDV_data$Qout_Polder[t])/(Width_MV_canal*Depth_MV_canal) # m/s
  }
  
  for (x in Begin_Vliet_narrow) {
    Flowrate[x,t]=(MDV_data$Qin_MarkCanal[t]+MDV_data$Qin_BlauweKamer[t]+MDV_data$Qin_Oranjeboombrug[t]-MDV_data$Qout_Dintelsas[t]-MDV_data$Qout_Polder[t])/(Width_Vliet_narrow*Depth_Vliet_narrow) # m/s
  }
  
  for (x in Begin_Vliet){
    Flowrate[x,t]=(MDV_data$Qin_MarkCanal[t]+MDV_data$Qin_BlauweKamer[t]+MDV_data$Qin_Oranjeboombrug[t]-MDV_data$Qout_Dintelsas[t]-MDV_data$Qout_Polder[t])/(Width_Vliet*Depth_Vliet) # m/s
  }
}

for (i in 1:nrow(Flowrate)) {
  Flowrate[i,which(Flowrate[i,]<0)]=0
}
plot(Flowrate[,30],t="l",xlab="place_step")
#axis(side = 1,at=c(114,640,666,743,760,1117),labels = c(114,640,666,743,760,1117),col.ticks = "red",las=2,cex=0.7)
plot(Flowrate[2,],t="l",xlab="time_step")

## Chlorophyll simulation ####


## Initial values: ####
TotChla <- list(
  BlueCh=matrix(data=NA,nrow=Num_Time_Steps,ncol=Num_Place_Steps,dimnames = list(paste("Time_step",0:(Num_Time_Steps-1)),paste("Place_step",0:(Num_Place_Steps-1)))),
  GreenCh=matrix(data=NA,nrow=Num_Time_Steps,ncol=Num_Place_Steps,dimnames = list(paste("Time_step",0:(Num_Time_Steps-1)),paste("Place_step",0:(Num_Place_Steps-1)))),
  DiatCh=matrix(data=NA,nrow=Num_Time_Steps,ncol=Num_Place_Steps,dimnames = list(paste("Time_step",0:(Num_Time_Steps-1)),paste("Place_step",0:(Num_Place_Steps-1))))
)

TotChla[["BlueCh"]]["Time_step 0",] =c(
  MDV_data$BlueCh_MarkCanal[1],
  MDV_data$BlueCh_DintelCanal[1],
  MDV_data$BlueCh_DintelCanal[1],
  MDV_data$BlueCh_Vliet[1],
  MDV_data$BlueCh_Vliet[1],
  MDV_data$BlueCh_Vliet[1]
)
TotChla[["GreenCh"]]["Time_step 0",] =c(
  MDV_data$BlueCh_MarkCanal[1],
  MDV_data$BlueCh_DintelCanal[1],
  MDV_data$BlueCh_DintelCanal[1],
  MDV_data$BlueCh_Vliet[1],
  MDV_data$BlueCh_Vliet[1],
  MDV_data$BlueCh_Vliet[1]
)
TotChla[["DiatCh"]]["Time_step 0",] =c(
  MDV_data$BlueCh_MarkCanal[1],
  MDV_data$BlueCh_DintelCanal[1],
  MDV_data$BlueCh_DintelCanal[1],
  MDV_data$BlueCh_Vliet[1],
  MDV_data$BlueCh_Vliet[1],
  MDV_data$BlueCh_Vliet[1]
)

TotChla[["BlueCh"]]["Time_step 0",]=c(MDV_data$BlueCh_MarkCanal[1], MDV_data$BlueCh_DintelCanal[1], MDV_data$BlueCh_DintelCanal[1], MDV_data$BlueCh_DintelCanal[1], MDV_data$BlueCh_Vliet[1], MDV_data$BlueCh_Vliet[1])

TotChla[["GreenCh"]]["Time_step 0",]=c(MDV_data$dChla_MarkCanal[1], MDV_data$dChla_DintelCanal[1], MDV_data$dChla_DintelCanal[1], MDV_data$dChla_DintelCanal[1], MDV_data$dChla_Vliet[1], MDV_data$dChla_Vliet[1])

TotChla[["DiatCh"]]["Time_step 0",]=c(MDV_data$dChla_MarkCanal[1], MDV_data$dChla_DintelCanal[1], MDV_data$dChla_DintelCanal[1], MDV_data$dChla_DintelCanal[1], MDV_data$dChla_Vliet[1], MDV_data$dChla_Vliet[1])
  
TotChla[["BlueCh"]][,"Place_step 0"]=MDV_data$BlueCh_MarkCanal
TotChla[["GreenCh"]][,"Place_step 0"]=MDV_data$dChla_MarkCanal
TotChla[["DiatCh"]][,"Place_step 0"]=MDV_data$dChla_MarkCanal

plot(TotChla[[1]][,1])

Place_step <- c(Length_Mark_canal, Length_Dintel, Length_Dintel_wide, Length_MV_canal, Length_Vliet_narrow, Length_Vliet)
Place_Depths <- c(Depth_Mark_canal, Depth_Dintel, Depth_Dintel_wide, Depth_MV_canal, Depth_Vliet_narrow, Depth_Vliet)

Too_big_t_step <- list(BlueCh=rep(NA,Num_Place_Steps),GreenCh=rep(NA,Num_Place_Steps),DiatCh=rep(NA,Num_Place_Steps))
Too_big_t_step[["BlueCh"]][1] <- 0
Too_big_t_step[["GreenCh"]][1] <- 0
Too_big_t_step[["DiatCh"]][1] <- 0

### State variables to be outputted: ####
#sGrowth <- rep(NA,ncol(TotChla))
#sDegradation <- rep(NA,ncol(TotChla))
#sLDay <- rep(NA,ncol(TotChla))
#sLLim <- rep(NA,ncol(TotChla))
#sPLim <- rep(NA,ncol(TotChla))
#sMuMaxTm <- rep(NA,ncol(TotChla))



## Simulation ####
Chla_vec <- c("BlueCh","GreenCh","DiatCh")

for (Chla in Chla_vec) {
  for(t in 2:Num_Time_Steps){
    for(x in 2:Num_Place_Steps){#
      Time = MDV_data$sTime[t]
      sDepthW = Place_Depths[x]
      #LDay = mLDay[t]
      #P = P_Conc[[x]][t]
      
      # Calculate the time step requirement
      Egg_time_step <-(Place_step[x])^2/(Flowrate[x,t]*Place_step[x]+2*Dispersion) # s
      if(Time_step>Egg_time_step){Too_big_t_step[[Chla]][x]=1}else{Too_big_t_step[[Chla]][x]=0}
      
      # Advection and Diffusion coefficient (unitless):
      One=Flowrate[x,t]*Time_step/Place_step[x]
      Two=Dispersion*Time_step/(Place_step[x])^2
      
      # Parameters:
      uTm = MDV_data$Temp[t]
      cSigTm = cSigTm.vec[Chla]
      cTmOpt = cTmOpt.vec[Chla]
      cMuMax = cMuMax.vec[Chla]
      cChD = cChD.vec[Chla]
      cSigTm = cSigTm.vec[Chla]
      cTmOpt = cTmOpt.vec[Chla]
      cLOptRef = cLOptRef.vec[Chla]
      kDResp = kDResp.vec[Chla]
      kLoss = kLoss.vec[Chla]
      kMort = kMort.vec[Chla]
      hLRef = hLRef.vec[Chla]
      
      # Temperature function
      TmFunc_value = TmFunc(uTm, cSigTm, cTmOpt)
      # growth rate:
      
      uMuMaxTm = cMuMax*TmFunc_value
      
      ## light limitation
      uLDay = cLDayAve-cLDayVar*cos(2*pi*(Time+TenDays)/DaysPerYear/SecondPerDay)#LDay
      ISurf = uLDay*fPAR*(1-fRefl)
      aExtCoef = (cExtWat+cExtSpDet*TotChla[[Chla]][t-1,x]/cChD*gPermg)
      
      aLLim = ifelse(Chla=="GreenCh",
                     LehmanFunc(aExtCoef=aExtCoef,sDepthW=sDepthW,ISurf=ISurf,uTm=uTm,cSigTm=cSigTm,cTmOpt=cTmOpt,hLRef=hLRef,TmFunc_value=TmFunc_value),
                     SteeleFunc(aExtCoef=aExtCoef,sDepthW=sDepthW,ISurf=ISurf,uTm=uTm,cSigTm=cSigTm,cTmOpt=cTmOpt,cLOptRef=cLOptRef,TmFunc_value=TmFunc_value)) 
      
      #aPLim = PLimFunc(P)
      aMuTmL = uMuMaxTm*aLLim #min(aLLim,aPLim(P),na.rm = T) # Growth rate at current light and temp
      
      growth = aMuTmL #*aNutLim
      #starting_growth*(1-(Flowrate[x,t]/(Monod_Speed+Flowrate[x,t])))*(1-TotChla[x,(t-1)]/(TotChla[x,(t-1)]+Monod_Conc))
      
      # Respiration
      ukDRespTm  = kDResp*TmFunc_value
      
      # Grazing Loss
      ukLossTm = kLoss*TmFunc_value
      degradation = ukDRespTm + kMort + ukLossTm
      
      # State variable output
      #sGrowth[t] = growth
      #sDegradation[t] = degradation
      #sLDay[t] = uLDay  
      
      #sLLim[t] = aLLim
      #sPLim[t] = aPLim
      #sMuMaxTm[t] = uMuMaxTm
      
      if(x<Num_Place_Steps){#
        TotChla[[Chla]][t,x]=TotChla[[Chla]][(t-1),(x-1)]*(One+Two)+TotChla[[Chla]][(t-1),x]*((1+growth-degradation)^(Time_step/SecondPerDay)-One-2*Two)+TotChla[[Chla]][(t-1),(x+1)]*Two
      }else{
        TotChla[[Chla]][t,x]=TotChla[[Chla]][(t-1),(x-1)]*(One+Two)+TotChla[[Chla]][(t-1),x]*((1+growth-degradation)^(Time_step/SecondPerDay)-One-2*Two)+TotChla[[Chla]][(t-1),(x-1)]*Two # mirrored edge
      }
    }
  }
}

Too_big_t_step
# dx = 8787 m

#par(mfrow=c(2,1),mar=c(2,4,1,0))
#library(dplyr)

# TotChla
#plot(TotChla[,3500],xlab = "Place.step (*50m)",ylab = "TotChlabaceteria (ug/L)",t="l")
#lines(TotChla[,1000],col=2)
#lines(TotChla[,350],col=3)
#lines(TotChla[,35],col=4)
#lines(sTime,TotChla[2,],col=2, lty=2, lwd=3)
#lines(sTime,TotChla[3,],col=3, lty=2, lwd=3)
#lines(sTime,TotChla[4,],col=4, lty=2, lwd=3)
#lines(sTime,TotChla[5,],col=5, lty=2, lwd=3)
#lines(sTime,TotChla[6,],col=6, lty=2, lwd=3)

Chla_DintelCanal <- approx(HF_BlueCh_DintelCanal$Datum%>%dmy_hms%>%as.numeric,HF_BlueCh_DintelCanal$Waarde, xout=sTime) # ug/l
loessChla_DintelCanal <- loess(Chla_DintelCanal$y~Chla_DintelCanal$x,span = SecondPerDay/Time_step/length(sTime))

Chla_Vliet <- approx(HF_BlueCh_Vliet$Datum%>%dmy_hms%>%as.numeric,HF_BlueCh_Vliet$Waarde, xout=sTime) # ug/l
loessChla_Vliet <- loess(Chla_Vliet$y~Chla_Vliet$x,span = SecondPerDay/Time_step/length(sTime))

# Visualization
## Time series
plot(sTime,TotChla[1,],xlab = "time.step (*day)",ylab = "Blue algae Ch (ug/L)",t="l",ylim = c(0,100),main = "advection-diffusion + MuTm",xaxt="n")
axis(1,at=as.numeric(ymd_hms(c("2018-01-01 01:00:00","2019-01-01 01:00:00","2020-01-01 01:00:00"))),labels = c("2018-01-01 01:00:00","2019-01-01 01:00:00","2020-01-01 01:00:00"))


plot(loessChla_DintelCanal$x, predict(loessChla_DintelCanal), col="lightgreen",ylim = c(0,70))#3
lines(sTime,TotChla[2,],col=2,lwd=3,lty=2)


lines(sTime,TotChla[3,],col=3,lwd=3,lty=2)
lines(sTime,TotChla[4,],col=4,lwd=3,lty=2)

plot(loessChla_Vliet$x, predict(loessChla_Vliet), col="orange")#6
lines(sTime,TotChla[5,],col=5,lwd=3,lty=2)
lines(sTime,TotChla[6,],col="darkorange",lwd=3,lty=2)
legend("topright",
       legend = c("Mark canal","Dintel canal","Dintel wide", "MV_canal","Vliet_narrow","Vliet"),col=c(1:5,"darkorange"),text.col==c(1:5,"darkorange"),lty=1)

# State variables:
plot(sTime, sGrowth,xaxt="n",ylim = range(c(sGrowth,sDegradation),na.rm = T),col="green",ylab = "Blue algae")
lines(sTime, sDegradation,col="red")
axis(1,at=as.numeric(ymd_hms(c("2018-01-01 01:00:00","2019-01-01 01:00:00","2020-01-01 01:00:00"))),labels = c("2018-01-01 01:00:00","2019-01-01 01:00:00","2020-01-01 01:00:00"))
legend("topright",legend = c("Growthrate", "Degradationrate"),col=c("green","red"),text.col = c("green","red"),lty = 1)

plot(sTime, sLDay)
plot(sTime, sLLim)
plot(sTime, sMuMaxTm)

anarep <- TotChla%>%t%>%as.data.frame()
anarep$Time <- sTime

Obs_DintelCanal <- data.frame(Time=loessChla_DintelCanal$x%>%as.numeric(), Obs.Chla.DintelCanal=predict(loessChla_DintelCanal))
Obs_Vliet <- data.frame(Time=loessChla_Vliet$x%>%as.numeric(), Obs.Chla.Vliet=predict(loessChla_Vliet))

CompaDintelCanal <- merge(anarep,Obs_DintelCanal,by.x = "Time",by.y = "Time")
CompaVlietCanal <- merge(anarep,Obs_Vliet,by.x = "Time",by.y = "Time")

plot(CompaDintelCanal$Obs.Chla.DintelCanal,CompaDintelCanal$`Place_step 5`)
plot(CompaVlietCanal$Obs.Chla.Vliet,CompaVlietCanal$`Place_step 5`)

# Flowrate
plot(Flowrate_Oosterhout,Chla_Oosterhout, xlab = "Discharge [m3/s]", ylab = "Chla [ug/L]",main="Oosterhout measurement")
cor(Flowrate_Oosterhout,Chla_Oosterhout)
plot(Flowrate[6,],TotChla[6,], xlab = "Flowrate [m/s]", ylab = "Chla [ug/L]",main="Vliet simulation")
cor(Flowrate[6,],TotChla[6,])

plot(TotChla[6,],approx(HF_BlueCh_Vliet$Datum%>%dmy_hms%>%as.numeric(), HF_BlueCh_Vliet$Waarde,xout = sTime)$y)

# Flowrate
#plot(Flowrate[,30],t="l",xlab="place_step")

plot(Flowrate[1,],t="l",xlab="time_step",col=1, ylim=c(0,0.6),ylab="Flowrate (m/s)")
lines(Flowrate[2,],t="l",xlab="time_step",col=2)
lines(Flowrate[3,],t="l",xlab="time_step",col=3)
lines(Flowrate[4,],t="l",xlab="time_step",col=4)
lines(Flowrate[5,],t="l",xlab="time_step",col=5)
lines(Flowrate[6,],t="l",xlab="time_step",col=5)

# Trend check
plot(sLLim*100,ylim = c(0,120))
lines(sPLim*100,col=2)
lines(TotChla[6,],col=3,lwd=3,lty=2)
lines(P_Vliet*1000, col=4)
lines(predict(loessChla_Vliet), col="lightgreen")#6
legend("topright",legend = c("Light limitation","P limitation","Chla","PO4"),lty = 1,col = 1:4,text.col = 1:4)
