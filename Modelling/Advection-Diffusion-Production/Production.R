require(lubridate)
require(dplyr)


################################################################
# Parameter set-up in model



##############
Num_Place_Steps <- 1
Num_Time_Steps <- length(sTime)#1e5
Time_step <- 60*30 # delta t, at which derivative will be calculated

sTime=seq(HF_Q_MarkCanal$Datum[1]%>%dmy_hms%>%as.numeric,HF_Q_MarkCanal$Datum[length(HF_Q_MarkCanal$Datum)]%>%dmy_hms%>%as.numeric,by=Time_step)

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
cMuMax <- cMuMaxBlue #(day^-1)
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
#cTmRef <- 20
cTmOpt <- 25
cSigTm <- 12 # sigma in Gaussian curve

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
hLRefDiat <- 17#1000 # W/m2 = J/m2/s
hLRefGren <- 17 # W/m2
hLRefBlue <- 17#1000 # W/m2
hL <- hLRefBlue
#(hLRefDiat+hLRefBlue+hLRefGren)/3 # W/m2

# half saturation constant of resource
hR <- 0.18 # mg/l

############################################################

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


#################################################
# Input data preparation
#################################################

sTime=seq(HF_Q_MarkCanal$Datum[1]%>%dmy_hms%>%as.numeric,HF_Q_MarkCanal$Datum[length(HF_Q_MarkCanal$Datum)]%>%dmy_hms%>%as.numeric,by=Time_step)

Temp <- approx(HF_Temp_MarkCanal$Datum%>%dmy_hms%>%as.numeric,HF_Temp_MarkCanal$Waarde, xout=sTime)$y

#mLDay <- approx(mL2018$Time,mL2018$in_shortwave+mL2018$in_longwave, xout=sTime)$y

Chla_MarkCanal <- approx(HF_BlueCh_MarkCanal$Datum%>%dmy_hms%>%as.numeric,HF_BlueCh_MarkCanal$Waarde, xout=sTime) # ug/l
loessChla_Oosterhout <- loess(Chla_MarkCanal$y~Chla_MarkCanal$x,span = Time_step/length(sTime))
Chla_Oosterhout <- predict(loessChla_Oosterhout)

PO4_MarkCanal <- approx(LF_PO4_MarkCanal$Datum%>%dmy_hms%>%as.numeric,LF_PO4_MarkCanal$Waarde, xout=sTime) # ug/l
loessPO4_MarkCanal <- loess(PO4_MarkCanal$y~PO4_MarkCanal$x,span = Time_step/length(sTime))
P_MarkCanal <- predict(loessPO4_MarkCanal)

####################################################################################
Cyano <- matrix(data = NA,nrow=Num_Place_Steps,ncol = Num_Time_Steps,dimnames = list(paste("Place_step",0:(Num_Place_Steps-1)),paste("Time_step",0:(Num_Time_Steps-1))))

#for (x in 1:Num_Place_Steps){
#  Cyano[x,"Time_step 0"] =Conc0
#}
#for (t in 2:Num_Time_Steps){
#  Cyano["Place_step 0",t]=Conc0#+sin(t/Num_Time_Steps*2*pi)*Conc0/2
#}

#Cyano[,"Time_step 0"] =c(HF_BlueCh_MarkCanal$Waarde[1],HF_BlueCh_DintelCanal$Waarde[1],HF_BlueCh_DintelCanal$Waarde[1],HF_BlueCh_DintelCanal$Waarde[1],HF_BlueCh_Vliet$Waarde[1],HF_BlueCh_Vliet$Waarde[1])
Cyano["Place_step 0",1]=Chla_Oosterhout[1]

plot(Cyano[1,])

Place_Depths <- c(Depth_Mark_canal, Depth_Dintel, Depth_Dintel_wide, Depth_MV_canal, Depth_Vliet_narrow, Depth_Vliet)

# State variables to be outputted:
sGrowth <- rep(NA,ncol(Cyano))
sDegradation <- rep(NA,ncol(Cyano))
sLDay <- rep(NA,ncol(Cyano))

sLLim <- rep(NA,ncol(Cyano))
sPLim <- rep(NA,ncol(Cyano))
sMuMaxTm <- rep(NA,ncol(Cyano))

for(t in 2:ncol(Cyano)){
#  for(x in 2:nrow(Cyano)){#
    Time = sTime[t]
    sDepthW = Place_Depths[1]
    #LDay = mLDay[t]
    P = P_MarkCanal[t]
    
    # Advection and Diffusion coefficient (unitless):
    One=0#Flowrate[x,t]*Time_step/Place_step[x]
    Two=0#Dispersion*Time_step/(Place_step[x])^2
    
    # growth rate:
    ## Temperature function
    uTm = Temp[t]
    uFunTm = exp(-0.5/(cSigTm^2)*((uTm-cTmOpt)^2))#-(cTmRef-cTmOpt)^2
    uMuMaxTm = cMuMax*uFunTm
    
    ## light limitation
    uLDay = cLDayAve-cLDayVar*cos(2*pi*(Time+TenDays)/DaysPerYear/SecondPerDay)#LDay#
    ufDay = cfDayAve-cfDayVar*cos(2*pi*(Time+TenDays)/DaysPerYear/SecondPerDay)#LDay#
    
    uLOut = uLDay/ufDay # Light intensity during daytime
    ISurf = uLOut*fPAR*(1-fRefl) # PAR at zero depth
    aExtCoef = (cExtWat+cExtSpDet*Cyano[1,t-1]/cChD*gPermg)
    IBottom=ISurf*exp(-aExtCoef*sDepthW)
    
    aLLim =(1/(aExtCoef*sDepthW)*log((hL+ISurf)/(hL+IBottom))) # Tilman (1982), Huisman and Weissing (1994, 1995)
    
    aPLim = P/(P+hR)
    #aNutLim = min(c(aPLim,aNLim)) # Nutrient limitation
    aMuTmL = uMuMaxTm*min(aLLim,aPLim,na.rm = T) # Growth rate at current light and temp
    
    growth = aMuTmL #*aNutLim
    #starting_growth*(1-(Flowrate[x,t]/(Monod_Speed+Flowrate[x,t])))*(1-Cyano[x,(t-1)]/(Cyano[x,(t-1)]+Monod_Conc))
    
    # Respiration
    ukDRespTm  = kDResp*uFunTm
    
    # Grazing Loss
    ukLossTm = kLoss*uFunTm
    degradation = ukDRespTm + kMort + ukLossTm
    
    # State variable output
    sGrowth[t] = growth
    sDegradation[t] = degradation
    sLDay[t] = uLDay  
    
    sLLim[t] = aLLim
    sPLim[t] = aPLim
    sMuMaxTm[t] = uMuMaxTm
    
    Cyano[1,t]=Cyano[1,(t-1)]*(1+growth-degradation)^(Time_step/SecondPerDay)
}

plot(sTime,Cyano[1,],xlab = "time.step (*day)",ylab = "Blue algae Ch (ug/L)",t="l",ylim = c(0,50),main = "advection-diffusion + MuTm",xaxt="n",col=1)
axis(1,at=as.numeric(ymd_hms(c("2018-01-01 01:00:00","2019-01-01 01:00:00","2020-01-01 01:00:00"))),labels = c("2018-01-01 01:00:00","2019-01-01 01:00:00","2020-01-01 01:00:00"))

lines(sTime, Chla_Oosterhout,col=2)

# State variables:
plot(sTime, sGrowth,xaxt="n",ylim = range(c(sGrowth,sDegradation),na.rm = T),col="green",ylab = "Blue algae")
lines(sTime, sDegradation,col="red")
axis(1,at=as.numeric(ymd_hms(c("2018-01-01 01:00:00","2019-01-01 01:00:00","2020-01-01 01:00:00"))),labels = c("2018-01-01 01:00:00","2019-01-01 01:00:00","2020-01-01 01:00:00"))
legend("topright",legend = c("Growthrate", "Degradationrate"),col=c("green","red"),text.col = c("green","red"),lty = 1)

plot(sTime, sLDay)

plot(sTime, sLLim,ylim = c(0,1))
lines(sTime,sPLim,col=2)

# Trend check
plot(sLLim*100,ylim = c(0,100))
lines(sPLim*100,col=2)
lines(Chla_Oosterhout,col=3)
lines(P_MarkCanal*1000, col=4)

legend("topleft",legend = c("Light limitation","P limitation","Chla","PO4"),lty = 1,col = 1:4,text.col = 1:4)

plot(sTime, sMuMaxTm)






