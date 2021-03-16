require(lubridate)
require(dplyr)

### Input data preparation ####
HF_BlueCh_MarkCanal <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\HF_BlueCh_MarkCanal.csv")
HF_BlueCh_DintelCanal <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\HF_BlueCh_DintelCanal.csv")
HF_BlueCh_Vliet <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\HF_BlueCh_Vliet.csv")

HF_Chla_MarkCanal <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\HF_Chla_MarkCanal.csv")
HF_Chla_DintelCanal <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\HF_Chla_DintelCanal.csv")
HF_Chla_Vliet <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\HF_Chla_Vliet.csv")

HF_Q_MarkCanal <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\HF_Q_MarkCanal.csv")
HF_Q_Vliet <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Mark_Dintel_Vliet\\Modelling\\Advection-Diffusion-Production\\HF_Q_Vliet.csv")

mL2018$Time <- paste(mL2018$Date, mL2018$day)%>%dmy_h%>%as.numeric()
mL2018 <- mL2018%>%na.omit

sTime=seq(HF_Q_MarkCanal$Datum[1]%>%dmy_hms%>%as.numeric,HF_Q_MarkCanal$Datum[length(HF_Q_MarkCanal$Datum)]%>%dmy_hms%>%as.numeric,by=60*30)
#sTime=seq(mL2018$Time[2],mL2018$Time[length(mL2018$Time)],by=60*30)

Q_MarkCanal<- approx(HF_Q_MarkCanal$Datum%>%dmy_hms%>%as.numeric,HF_Q_MarkCanal$Waarde, xout=sTime) # 10 # m3/s
loessQ_MarkCanal <- loess(Q_MarkCanal$y~Q_MarkCanal$x,span=SecondPerDay/Time_step/length(sTime))
Flowrate_Oosterhout <- predict(loessQ_MarkCanal)

Chla_MarkCanal <- approx(HF_BlueCh_MarkCanal$Datum%>%dmy_hms%>%as.numeric,HF_BlueCh_MarkCanal$Waarde, xout=sTime) # ug/l
loessChla_Oosterhout <- loess(Chla_MarkCanal$y~Chla_MarkCanal$x,span = SecondPerDay/Time_step/length(sTime))
Chla_Oosterhout <- predict(loessChla_Oosterhout)

# Nutrient data
PO4_MarkCanal <- approx(LF_PO4_MarkCanal$Datum%>%dmy_hms%>%as.numeric,LF_PO4_MarkCanal$Waarde, xout=sTime) # ug/l
loessPO4_MarkCanal <- loess(PO4_MarkCanal$y~PO4_MarkCanal$x,span = Time_step/length(sTime))
P_MarkCanal <- predict(loessPO4_MarkCanal)

PO4_DintelCanal <- approx(LF_PO4_DintelCanal$Datum%>%dmy_hms%>%as.numeric,LF_PO4_DintelCanal$Waarde, xout=sTime) # ug/l
loessPO4_DintelCanal <- loess(PO4_DintelCanal$y~PO4_DintelCanal$x,span = Time_step/length(sTime))
P_DintelCanal <- predict(loessPO4_DintelCanal)

PO4_MVCanal <- approx(LF_PO4_MVCanal$Datum%>%dmy_hms%>%as.numeric,LF_PO4_MVCanal$Waarde, xout=sTime) # ug/l
loessPO4_MVCanal <- loess(PO4_MVCanal$y~PO4_MVCanal$x,span = Time_step/length(sTime))
P_MVCanal <- predict(loessPO4_MVCanal)

PO4_Vliet <- approx(LF_PO4_Vliet$Datum%>%dmy_hms%>%as.numeric,LF_PO4_Vliet$Waarde, xout=sTime) # ug/l
loessPO4_Vliet <- loess(PO4_Vliet$y~PO4_Vliet$x,span = Time_step/length(sTime))
P_Vliet <- predict(loessPO4_Vliet)

# reading data from Modelling/Advection-Diffusion/Folder
plot(HF_BlueCh_MarkCanal$Datum%>%dmy_hms, HF_BlueCh_MarkCanal$Waarde, col=1,t="l",ylim=c(0,300),ylab = "Blue Ch (ug/L)")
lines(HF_BlueCh_DintelCanal$Datum%>%dmy_hms, HF_BlueCh_DintelCanal$Waarde, col=2)
lines(HF_BlueCh_Vliet$Datum%>%dmy_hms, HF_BlueCh_Vliet$Waarde, col=3)
legend("topright", legend = c("MarkCanal","DintelCanal","Vliet"),col=1:3,text.col = 1:3,lty=1)

# PO4 concentration (mg/L)
plot(P_MarkCanal, col=1, ylim = c(0,0.12),ylab = "PO4 (mg/L)")
lines(P_DintelCanal, col=2)
lines(P_MVCanal, col=3)
lines(P_Vliet, col=4)
legend("topright",legend = c("MarkCanal","DintelCanal","MV_Canal","Vliet"),col=1:4,text.col = 1:4)

P_Conc <- list(P_MarkCanal=P_MarkCanal, P_DintelCanal=P_DintelCanal, P_Dintelwide=P_DintelCanal, P_MVCanal=P_MVCanal, P_Vlietnarrow=P_Vliet, P_Vlietwide=P_Vliet)

### Flowrate data
Flowrate_Singels <- approx(HF_Q_MarkCanal$Datum[1]%>%dmy_hms%>%as.numeric+FlowrateForRiverModel$dTime*SecondPerDay,FlowrateForRiverModel$Singels, xout=sTime)$y # 4.1 # m3/s

Flowrate_Dintelsas <- approx(HF_Q_MarkCanal$Datum[1]%>%dmy_hms%>%as.numeric+FlowrateForRiverModel$dTime*SecondPerDay,FlowrateForRiverModel$Dintelsas, xout=sTime)$y # 4 # m3/s

T_Flowrate_Polders <- approx(HF_Q_MarkCanal$Datum[1]%>%dmy_hms%>%as.numeric+FlowrateForRiverModel$dTime*SecondPerDay,FlowrateForRiverModel$Polders, xout=sTime)$y # 3.3 # m3/s 

Temp <- approx(HF_Temp_MarkCanal$Datum%>%dmy_hms%>%as.numeric,HF_Temp_MarkCanal$Waarde, xout=sTime)$y

#mLDay <- approx(mL2018$Time,mL2018$in_shortwave+mL2018$in_longwave, xout=sTime)$y


#### Parameter set-up in model ####

Num_Place_Steps <- 6
Num_Time_Steps <- length(sTime)#1e5
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
cChD <- cChDBlue
#(cChDDiat+cChDGren+cChDBlue)

# maximum growth rate
cMuMaxDiat <- 2 #(day^-1)
cMuMaxGren <- 1.5 #(day^-1)
cMuMaxBlue <- 0.6 #(day^-1)
cMuMax <- cMuMaxBlue
#((cMuMaxDiat+cMuMaxGren+cMuMaxBlue)/3) # /delta t

# maintenance respiration constant rate
kDRespDiat <- 0.1 #(day^-1)
kDRespGren <- 0.075 #(day^-1)
kDRespBlue <- 0.03 #(day^-1)
kDResp <- kDRespBlue
#((kDRespDiat+kDRespGren+kDRespBlue)/3) # /delta t

# mortality constant
kMortDiat <- 0.01 #(day^-1)
kMortGren <- 0.01 #(day^-1)
kMortBlue <- 0.01 #(day^-1)
kMort <- kMortBlue
#((kMortDiat+kMortGren+kMortBlue)/3) /delta t

# grazing loss rate
kLossDiat <- 0.25 #(day^-1)
kLossGren <- 0.25 #(day^-1)
kLossBlue <- 0.03 #(day^-1)
kLoss <- kLossBlue
#((kLossDiat+kLossBlue+kLossGren)/3) # /delta t

# Temperature function
cTmRef <- 20

cTmOptDiat <- 18
cTmOptGren <- 25
cTmOptBlue <- 25

cSigTmDiat <- 20 # sigma in Gaussian curve
cSigTmGren <- 15
cSigTmBlue <- 12

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
hL <- hLRefBlue

cLOptRefDiat <- 54 # W/m2
cLOptRefGren <- 1000 # W/m2, Fake value
cLOptRefBlue <- 13.6 # W/m2
cLOptRef <- cLOptRefBlue

# half saturation constant of resource
hR <- 0.18 # mg/l


#### Morphological size ####

#Conc0 <- 50 # ug/l
#Dim Cyano(1117,10000)
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
    Flowrate[x,t] = Flowrate_Oosterhout[t]/(Width_Mark_canal*Depth_Mark_canal) # m/s
  }
  
  for (x in Begin_Dintel){
    Flowrate_Polders = (Length_Dintel/(Length_Dintel+Length_Dintel_wide+Length_MV_canal+Length_Vliet_narrow+Length_Vliet))*T_Flowrate_Polders[t]
    Flowrate[x,t]=(Flowrate_Oosterhout[t]+Flowrate_Singels[t]-Flowrate_Polders)/(Width_Dintel*Depth_Dintel) # m/s
  }
  
  for (x in Begin_Dintel_wide){
    Flowrate_Polders=((Length_Dintel+Length_Dintel_wide)/(Length_Dintel+Length_Dintel_wide+Length_MV_canal+Length_Vliet_narrow+Length_Vliet))*T_Flowrate_Polders[t]
    Flowrate[x,t]=(Flowrate_Oosterhout[t]+Flowrate_Singels[t]-Flowrate_Polders)/(Width_Dintel_wide*Depth_Dintel_wide) # m/s
  }
  
  for (x in Begin_MV_canal){
    Flowrate_Polders=((Length_Dintel+Length_Dintel_wide+Length_MV_canal)/(Length_Dintel+Length_Dintel_wide+Length_MV_canal+Length_Vliet_narrow+Length_Vliet))*T_Flowrate_Polders[t]
    Flowrate[x,t]=(Flowrate_Oosterhout[t]+Flowrate_Singels[t]-Flowrate_Dintelsas[t]-Flowrate_Polders)/(Width_MV_canal*Depth_MV_canal) # m/s
  }
  
  for (x in Begin_Vliet_narrow) {
    Flowrate_Polders=((Length_Dintel+Length_Dintel_wide+Length_MV_canal+Length_Vliet_narrow)/(Length_Dintel+Length_Dintel_wide+Length_MV_canal+Length_Vliet_narrow+Length_Vliet))*T_Flowrate_Polders[t]
    Flowrate[x,t]=(Flowrate_Oosterhout[t]+Flowrate_Singels[t]-Flowrate_Dintelsas[t]-Flowrate_Polders)/(Width_Vliet_narrow*Depth_Vliet_narrow) # m/s
  }
  
  for (x in Begin_Vliet){
    Flowrate_Polders = ((Length_Dintel+Length_Dintel_wide+Length_MV_canal+Length_Vliet_narrow+Length_Vliet)/(Length_Dintel+Length_Dintel_wide+Length_MV_canal+Length_Vliet_narrow+Length_Vliet))*T_Flowrate_Polders[t]
    Flowrate[x,t]=(Flowrate_Oosterhout[t]+Flowrate_Singels[t]-Flowrate_Dintelsas[t]-Flowrate_Polders)/(Width_Vliet*Depth_Vliet) # m/s
  }
}

for (i in 1:nrow(Flowrate)) {
  Flowrate[i,which(Flowrate[i,]<0)]=0
}
plot(Flowrate[,30],t="l",xlab="place_step")
#axis(side = 1,at=c(114,640,666,743,760,1117),labels = c(114,640,666,743,760,1117),col.ticks = "red",las=2,cex=0.7)
plot(Flowrate[2,],t="l",xlab="time_step")

## Chlorophyll simulation ####

Cyano <- matrix(data = NA,nrow=Num_Place_Steps,ncol = Num_Time_Steps,dimnames = list(paste("Place_step",0:(Num_Place_Steps-1)),paste("Time_step",0:(Num_Time_Steps-1))))

#for (x in 1:Num_Place_Steps){
#  Cyano[x,"Time_step 0"] =Conc0
#}
#for (t in 2:Num_Time_Steps){
#  Cyano["Place_step 0",t]=Conc0#+sin(t/Num_Time_Steps*2*pi)*Conc0/2
#}

Cyano[,"Time_step 0"] =c(HF_BlueCh_MarkCanal$Waarde[1],HF_BlueCh_DintelCanal$Waarde[1],HF_BlueCh_DintelCanal$Waarde[1],HF_BlueCh_DintelCanal$Waarde[1],HF_BlueCh_Vliet$Waarde[1],HF_BlueCh_Vliet$Waarde[1])
Cyano["Place_step 0",]=Chla_Oosterhout

plot(Cyano[1,])

Place_step <- c(Length_Mark_canal, Length_Dintel, Length_Dintel_wide, Length_MV_canal, Length_Vliet_narrow, Length_Vliet)
Place_Depths <- c(Depth_Mark_canal, Depth_Dintel, Depth_Dintel_wide, Depth_MV_canal, Depth_Vliet_narrow, Depth_Vliet)

Too_big_t_step <- rep(NA,nrow(Cyano))
Too_big_t_step[1] <- 0


# State variables to be outputted:
sGrowth <- rep(NA,ncol(Cyano))
sDegradation <- rep(NA,ncol(Cyano))
sLDay <- rep(NA,ncol(Cyano))

sLLim <- rep(NA,ncol(Cyano))
#sPLim <- rep(NA,ncol(Cyano))
sMuMaxTm <- rep(NA,ncol(Cyano))

# functions
## Temperature function
TmFunc = function(uTm){
  return(exp(-0.5/(cSigTm^2)*((uTm-cTmOpt)^2-(cTmRef-cTmOpt))))
} 

## Light limitation
SteeleFunc <- function(aExtCoef,sDepthW,ISurf,uTm){
  IBottom = ISurf*exp(-aExtCoef*sDepthW)
  uLOptRef = cLOptRef*TmFunc(uTm)
  return(exp(1)/(aExtCoef*sDepthW)*(exp(-IBottom/uLOptRef)-exp(-ISurf/uLOptRef)))
} # Tilman (1982), Huisman and Weissing (1994, 1995)

LehmanFunc <- function(aExtCoef,sDepthW,ISurf,uTm){
  IBottom = ISurf*exp(-aExtCoef*sDepthW)
  uhLRef = hLRef*TmFunc(uTm)
  return(1/(aExtCoef*sDepthW)*log((uhLRef+ISurf)/(uhLRef+IBottom)))
}

## Nutrient limitation
PLimFunc = function(P){return(P/(P+hR))}


## Simulation ####
for(t in 2:ncol(Cyano)){
  for(x in 2:nrow(Cyano)){#
    Time = sTime[t]
    sDepthW = Place_Depths[x]
    #LDay = mLDay[t]
    #P = P_Conc[[x]][t]
    
    # Calculate the time step requirement
    Egg_time_step <-(Place_step[x])^2/(Flowrate[x,t]*Place_step[x]+2*Dispersion) # s
    if(Time_step>Egg_time_step){Too_big_t_step[x]=1}else{Too_big_t_step[x]=0}
    
    # Advection and Diffusion coefficient (unitless):
    One=Flowrate[x,t]*Time_step/Place_step[x]
    Two=Dispersion*Time_step/(Place_step[x])^2
    
    # growth rate:
    ## Temperature function
    uTm = Temp[t]
    uMuMaxTm = cMuMax*TmFunc(uTm)
    
    ## light limitation
    uLDay = cLDayAve-cLDayVar*cos(2*pi*(Time+TenDays)/DaysPerYear/SecondPerDay)#LDay
    ISurf = uLDay*fPAR*(1-fRefl)
    aExtCoef = (cExtWat+cExtSpDet*Cyano[x,t-1]/cChD*gPermg)
    
    aLLim = SteeleFunc(aExtCoef=aExtCoef,sDepthW=sDepthW,ISurf=ISurf,uTm=uTm) 
    
    aPLim = PLimFunc(P)
    aMuTmL = uMuMaxTm*aLLim #min(aLLim,aPLim(P),na.rm = T) # Growth rate at current light and temp
    
    growth = aMuTmL #*aNutLim
    #starting_growth*(1-(Flowrate[x,t]/(Monod_Speed+Flowrate[x,t])))*(1-Cyano[x,(t-1)]/(Cyano[x,(t-1)]+Monod_Conc))
    
    # Respiration
    ukDRespTm  = kDResp*TmFunc(uTm)
    
    # Grazing Loss
    ukLossTm = kLoss*TmFunc(uTm)
    degradation = ukDRespTm + kMort + ukLossTm
    
    # State variable output
    sGrowth[t] = growth
    sDegradation[t] = degradation
    sLDay[t] = uLDay  
    
    sLLim[t] = aLLim
    sPLim[t] = aPLim
    sMuMaxTm[t] = uMuMaxTm
    
    if(x<Num_Place_Steps){#
      Cyano[x,t]=Cyano[(x-1),(t-1)]*(One+Two)+Cyano[x,(t-1)]*((1+growth-degradation)^(Time_step/SecondPerDay)-One-2*Two)+Cyano[(x+1),(t-1)]*Two
    }else{
      Cyano[x,t]=Cyano[(x-1),(t-1)]*(One+Two)+Cyano[x,(t-1)]*((1+growth-degradation)^(Time_step/SecondPerDay)-One-2*Two)+Cyano[(x-1),(t-1)]*Two # mirrored edge
    }

    #if(x<Num_Place_Steps){#
    #  Cyano[x,t]=Cyano[(x-1),(t-1)]*(One+Two)+Cyano[x,(t-1)]*(1-One-2*Two-Degradation_coef*Time_step)+Cyano[(x+1),(t-1)]*Two+growth*Time_step
    #}else{
    #  Cyano[x,t]=Cyano[(x-1),(t-1)]*(One+Two)+Cyano[x,(t-1)]*(1-One-2*Two-Degradation_coef*Time_step)+Cyano[(x-1),(t-1)]*Two+growth*Time_step # mirrored edge
    #}
  }
}
Too_big_t_step
# dx = 8787 m

#par(mfrow=c(2,1),mar=c(2,4,1,0))
#library(dplyr)

# Cyano
#plot(Cyano[,3500],xlab = "Place.step (*50m)",ylab = "Cyanobaceteria (ug/L)",t="l")
#lines(Cyano[,1000],col=2)
#lines(Cyano[,350],col=3)
#lines(Cyano[,35],col=4)
#lines(sTime,Cyano[2,],col=2, lty=2, lwd=3)
#lines(sTime,Cyano[3,],col=3, lty=2, lwd=3)
#lines(sTime,Cyano[4,],col=4, lty=2, lwd=3)
#lines(sTime,Cyano[5,],col=5, lty=2, lwd=3)
#lines(sTime,Cyano[6,],col=6, lty=2, lwd=3)

Chla_DintelCanal <- approx(HF_BlueCh_DintelCanal$Datum%>%dmy_hms%>%as.numeric,HF_BlueCh_DintelCanal$Waarde, xout=sTime) # ug/l
loessChla_DintelCanal <- loess(Chla_DintelCanal$y~Chla_DintelCanal$x,span = SecondPerDay/Time_step/length(sTime))

Chla_Vliet <- approx(HF_BlueCh_Vliet$Datum%>%dmy_hms%>%as.numeric,HF_BlueCh_Vliet$Waarde, xout=sTime) # ug/l
loessChla_Vliet <- loess(Chla_Vliet$y~Chla_Vliet$x,span = SecondPerDay/Time_step/length(sTime))

# Visualization
## Time series
plot(sTime,Cyano[1,],xlab = "time.step (*day)",ylab = "Blue algae Ch (ug/L)",t="l",ylim = c(0,100),main = "advection-diffusion + MuTm",xaxt="n")
axis(1,at=as.numeric(ymd_hms(c("2018-01-01 01:00:00","2019-01-01 01:00:00","2020-01-01 01:00:00"))),labels = c("2018-01-01 01:00:00","2019-01-01 01:00:00","2020-01-01 01:00:00"))


plot(loessChla_DintelCanal$x, predict(loessChla_DintelCanal), col="lightgreen",ylim = c(0,70))#3
lines(sTime,Cyano[2,],col=2,lwd=3,lty=2)


lines(sTime,Cyano[3,],col=3,lwd=3,lty=2)
lines(sTime,Cyano[4,],col=4,lwd=3,lty=2)

plot(loessChla_Vliet$x, predict(loessChla_Vliet), col="orange")#6
lines(sTime,Cyano[5,],col=5,lwd=3,lty=2)
lines(sTime,Cyano[6,],col="darkorange",lwd=3,lty=2)
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

anarep <- Cyano%>%t%>%as.data.frame()
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
plot(Flowrate[6,],Cyano[6,], xlab = "Flowrate [m/s]", ylab = "Chla [ug/L]",main="Vliet simulation")
cor(Flowrate[6,],Cyano[6,])

plot(Cyano[6,],approx(HF_BlueCh_Vliet$Datum%>%dmy_hms%>%as.numeric(), HF_BlueCh_Vliet$Waarde,xout = sTime)$y)

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
lines(Cyano[6,],col=3,lwd=3,lty=2)
lines(P_Vliet*1000, col=4)
lines(predict(loessChla_Vliet), col="lightgreen")#6
legend("topright",legend = c("Light limitation","P limitation","Chla","PO4"),lty = 1,col = 1:4,text.col = 1:4)
