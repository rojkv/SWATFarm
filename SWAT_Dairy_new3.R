##########################################################################################################
#------------------This script is for autofertilization without user defined fert rate and date--------------------
# the model spreads fertilizer automatically when the manure stored in the barn reaches its maximum capacity
##########################################################################################################
# First, the users need to initialize their SWAT models in ArcSWAT and 
# uploading the TxtInOut file to an R directory and set the R directory to there 
# The steps in the code:
#----------1. gettin Date from SWAT initialization file.cio, and HRUs areas from output.hru aby running standard swat: areas are needed to calculate kgs of applied manure
#----------2. getting information from user:
# 2.1. the number of HRU with the farm (that fertilizers will be applied on them)
# 2.2. The code will open the google sheets that need to be filled by user, the info include:
# 2.2.a) Barn_number = The user might have several farms across the watershed that can be identified by
# a character string start with B and following by 2 digit farm number : "B01","B02", "B03",.....,"B99".
# 2.2.b) HRU_number = The number of HRU that has been assigned by ArcSWAT: "10111","10112",...
# then R code will read the table from google sheets and get it back to R global Environment ("mgt_dataframe")
#----------3. automatically added other information to "mgt_dataframe" dataframe than user-defined info
# 3.1. Area_ha= area of each HRU in ha
#----------4. the code gets Dairy  Farm information 
# 4.1. source "Dairy_df" function that generates two main dataframes in GE, 1) has daily and stored manure and nutrient information
# 2) updated "mgt_datafram_farm(B01,B02,..)" dataframe with the case if autofertilization is needed (stored manure> max capacity)
# 4.2.  the code opens the google sheets that need to be filled by the user including the input parametesr of dairy model
# 4.3. getting aurofertilization rate and date the dataframe will be generated (mgt_datafram) that has information inclusing:
# 4.3.a) Barn_number = The user might have several farms across the watershed that can be identified by
# a character string start with B and following by 2 digit farm number : "B01","B02", "B03",.....,"B99".
# 4.3.b) HRU_number = The number of HRU that has been assigned by ArcSWAT: "10111","10112",...
# 4.3.c) year_app= year of manure application: 2010,2011,2012,....
# 4.3.d) month_app= month of manure application: 1,2,...,12
# 4.3.e) day_app= day of month for manure application: 1,2,......30
# 4.3.f) MGT_OP= mgt operation number in SWAT, 3 is default for fert application
# 4.3.g) FRT_KG= the amount of fertilizer applied to HRU (kg/ha) for HRU: e.g. 100kg/ha
# 4.3.h) Area_ha= area of each HRU in ha
# 4.3.i) Date_app= date of application
# 4.3.j) FERT_NAME= Fert/Manure name in fert.dat file, the forat wil be "BbbYYJJJ", Bbb=Barn_number and YY=two last digit of year, JJJ= Julian date
# 4.3.k) FRT_SURFACE= fraction of fertilizer applied to top 10mm of soil, 0, means the model applies 20% of fertilizer to top 10mm and the remainder to the 1st soil layer
# 4.3.l) FER_ID= fertilizer ID in fert.dat file , 1,2,.....,110, etc
#----------5. it updates *.mgt files for all HRUs that have fertilizer/manure application (using "mgt_datafram" dataframe)
#----------6. update FERT.DAT (fertilizer database), using dynamic nutrient fraction for the specific date that we have fertilizer application
#----------7. Rerun SWAT model with updated input files
#############################################################################################################
Sys.unsetenv("http_proxy"); Sys.unsetenv("https_proxy")
#setwd("~/NewScenarios/TxtInOutLOC/") #set wd to the SWAT initialization
setwd("~/DairyModelModify/Models/test/")
#-------------------------------------Setp 1 --------------------------------------------------------
# 1.1. getting Date from file.cio of SWAT initialization
#setwd("~/SWATFARM/Scenario2/TxtInOutLOC/") # Note= set your directory to the directory of SWAT TxtInOut
pathtofile="."
cfilename=paste0(pathtofile,"/file.cio")
SWATnbyr = read.fortran(textConnection(readLines(cfilename)[8]), "f20")[1,]
SWATiyr = read.fortran(textConnection(readLines(cfilename)[9]), "f20")[1,]
SWATidaf = read.fortran(textConnection(readLines(cfilename)[10]), "f20")[1,]
SWATidal = read.fortran(textConnection(readLines(cfilename)[11]), "f20")[1,]
SWATnsky=read.fortran(textConnection(readLines(cfilename)[60]), "f20")[1,]
startdate=as_date(paste0(SWATiyr,"-01-01")) + SWATidaf -1
enddate=as_date(paste0(SWATiyr+SWATnbyr -1,"-01-01")) + SWATidal -1
AllDays=data.frame(date=seq(startdate, by = "day", length.out = enddate-startdate+1))
Date=as.array(AllDays$date)
# 1.2. we run swat model for standard one to get the area of each HRU
file.remove(list.files(pattern="output.")) #removing output files
runSWAT2012() # run SWAT model 
outdata_rch_STAN = readSWAT("rch",".") # output.rch 
outdata_sub_STAN=readSWAT("sub",".") #output.sub
###remember to check the columns merge two heders such as output in TxtInOutLOC
cfilename_hruoutA=paste0(pathtofile,"/output.hru")
cfilename_hruout=readLines(paste0(pathtofile,"/output.hru"))
textreplace=gsub(pattern = "WTAB CLIm WTAB SOLm", replace="WTABclimwtabsolm",x=cfilename_hruout)
writeLines(textreplace, con=cfilename_hruoutA)
outdata_hru_STAN=readSWAT("hru",".") # output.hru
#-------------------------Step 2---------------------------------------------------------------------
# how many lines are in fert.dat?
pathtofile="."
linefertdat=readLines(paste0(pathtofile,"/fert.dat"))
row_fert_dat=length(linefertdat)
# -------------------------------based on user-defined application rates and dates ---
#2.1
totalnumber_HRU=7 # this is the total number of hrus that we want to do mgt application and equals to MGT_DATAFRAME nrow 
#-----2.2
Make_MGT_DATAFRAME= function(totalnumber_HRU){
  if(missing(totalnumber_HRU)){
    totalnumber_HRU=0
  }
  MGT_DATAFRAME=data.frame(
    Barn_number=character(),
    HRU_number=numeric()
  )
  MGT_DATAFRAME[1:totalnumber_HRU,] = NA 
  return(MGT_DATAFRAME)
}
mgt_datafram= Make_MGT_DATAFRAME(totalnumber_HRU)
#---here the code tries to open the data fram in google sheet so that the user can modify it manually
pacman::p_load(googlesheets4)
gsheet = gs4_create(
  "mgt_dataframe",
  sheets=mgt_datafram
)
# check console, enter your gmaile and go to your google sheets (there should be a file named mgt_dataframe) and fill it out
mgt_datafram=read_sheet(unclass(gsheet)) # now by running this command you will have filled mgt_dataframe in global environment
#-----------------------------------------Step 3--------------------------------------
mgt_datafram$Area_ha=NA
for (i in 1:nrow(mgt_datafram)){
  mgt_datafram$Area_ha[i]=outdata_hru_STAN$AREAkm2[outdata_hru_STAN$GIS==mgt_datafram$HRU_number[i]][1]*100
}
#----------------------------------------Step 4------------------------------------------
#source("https://raw.githubusercontent.com/Rojakaveh/SWAT_DAIRY/main/Autodairymodel.R")

Dairy_farm_input= function(numberofbarn){
  if(missing(numberofbarn)){
    numberofbarn=0
  }
  Dairy_farm_DATAFRAME=data.frame(
    Barn_number=character(),
    kMortality_calf=numeric(),
    kMortality_yearling=numeric(),
    kMortality_bredHeifer=numeric(),
    kMortality_LH=numeric(),
    kMortality_BLH=numeric(),
    kMortality_DBH=numeric(),
    kMortality_SecondLC=numeric(),
    kMortality_BredSecLC=numeric(),
    kMortality_DSecondLC=numeric(),
    kMortality_ThirdLC=numeric(),
    kMortality_BredThirdLC=numeric(),
    kMortality_DThirdLC=numeric(),
    kMortality_LC=numeric(),
    kMotality_BredLC=numeric(),
    kMortality_DC=numeric(),
    HFCR=numeric(),
    HSCR=numeric(),
    C2CR= numeric(),
    C3CR = numeric(),
    C4CR= numeric(),
    PropF= numeric(),
    PropKeep = numeric(),
    iCalf=integer(),
    iHeifer_first_lact=integer(),
    iHeifer_second_lact=integer(),
    iHeifer_third_lact=integer(),
    iHeifer_first_dry=integer(),
    iHeifer_second_dry=integer(),
    iHeifer_third_dry=integer(),
    iLact=integer(),
    iDry=integer(),
    iyearling=integer(),
    ibredHeifer=integer(),
    iBLH=integer(),
    iBredSecLC=integer(),
    iBredThirdLC=integer(),
    iBredLC=integer(),
    Barn_capacity=integer(),
    Max_capacity=integer(),
    Breed =integer(),
    FCM=integer(),
    calf_ME=numeric(),
    calf_CP=numeric(),
    fed_calf_P=numeric(),
    heifer_ME=numeric(),
    heifer_CP=numeric(),
    fed_heifer_P=numeric(),
    lact_CP=numeric(),
    fed_lact_P=numeric(),
    dry_CP=numeric(),
    fed_dry_P=numeric(),
    HRS=numeric(),
    Temp=numeric(),
    RHMD=numeric(),
    WS=numeric()
  )
  Dairy_farm_DATAFRAME[1:numberofbarn,] = NA 
  return(Dairy_farm_DATAFRAME)
}
Dairy_farm_datafram= Dairy_farm_input(length(unique(mgt_datafram$Barn_number))) # here you will have Dairy_farm_datafram in global environment
#here the code opens the data fram (Dairy_farm_datafram) in google sheet so that the user can modify it manually
gsheet = gs4_create(
  "Dairy_farm_datafram",
  sheets=Dairy_farm_datafram
)
#Getting back the dataframe into global environment
Dairy_farm_datafram=read_sheet(unclass(gsheet))
#------------------------------------------------- Step 5--------------------------------------------------------------
for (j in unique(mgt_datafram$Barn_number)){
  nam=paste0("Farm_df",j)
  FARMarea=sum(mgt_datafram$Area_ha[mgt_datafram$Barn_number==j]) #ha area of all hru in the farm j
  assign(nam, Dairy_df(Dairy_farm_datafram$kMortality_calf[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$kMortality_yearling[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$kMortality_bredHeifer[Dairy_farm_datafram$Barn_number==j],
                       Dairy_farm_datafram$kMortality_LH[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$kMortality_BLH[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$kMortality_DBH[Dairy_farm_datafram$Barn_number==j],
                       Dairy_farm_datafram$kMortality_SecondLC[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$kMortality_BredSecLC[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$kMortality_DSecondLC[Dairy_farm_datafram$Barn_number==j],
                       Dairy_farm_datafram$kMortality_ThirdLC[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$kMortality_BredThirdLC[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$kMortality_DThirdLC[Dairy_farm_datafram$Barn_number==j],
                       Dairy_farm_datafram$kMortality_LC[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$kMotality_BredLC[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$kMortality_DC[Dairy_farm_datafram$Barn_number==j],
                       Dairy_farm_datafram$HFCR[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$HSCR[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$C2CR[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$C3CR[Dairy_farm_datafram$Barn_number==j],
                       Dairy_farm_datafram$C4CR[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$PropF[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$PropKeep[Dairy_farm_datafram$Barn_number==j],
                       Dairy_farm_datafram$iCalf[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$iHeifer_first_lact[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$iHeifer_second_lact[Dairy_farm_datafram$Barn_number==j],
                       Dairy_farm_datafram$iHeifer_third_lact[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$iHeifer_first_dry[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$iHeifer_second_dry[Dairy_farm_datafram$Barn_number==j],
                       Dairy_farm_datafram$iHeifer_third_dry[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$iLact[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$iDry[Dairy_farm_datafram$Barn_number==j],
                       Dairy_farm_datafram$iyearling[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$ibredHeifer[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$iBLH[Dairy_farm_datafram$Barn_number==j],
                       Dairy_farm_datafram$iBredSecLC[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$iBredThirdLC[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$iBredLC[Dairy_farm_datafram$Barn_number==j],
                       Dairy_farm_datafram$Barn_capacity[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$Max_capacity[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$Breed[Dairy_farm_datafram$Barn_number==j],
                       Dairy_farm_datafram$FCM[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$calf_ME[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$calf_CP[Dairy_farm_datafram$Barn_number==j],
                       Dairy_farm_datafram$fed_calf_P[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$heifer_ME[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$heifer_CP[Dairy_farm_datafram$Barn_number==j],
                       Dairy_farm_datafram$fed_heifer_P[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$lact_CP[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$fed_lact_P[Dairy_farm_datafram$Barn_number==j],
                       Dairy_farm_datafram$dry_CP[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$fed_dry_P[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$HRS[Dairy_farm_datafram$Barn_number==j],
                       Dairy_farm_datafram$Temp[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$RHMD[Dairy_farm_datafram$Barn_number==j],Dairy_farm_datafram$WS[Dairy_farm_datafram$Barn_number==j]))
}

mgt_datafram=do.call(rbind, mget(ls(pattern ="mgt_dataframe_Farm*" )))
mgt_datafram$FERT_ID=c((row_fert_dat+1):(nrow(mgt_datafram)+row_fert_dat))

# modify *.mgt files
pathtofile="."
for (i in unique(mgt_datafram$HRU_number)){
  b=formatC(i, width = 9, format = "d", flag = "0")
  assign(paste0("cfilename_mgtA",i,sep=""), paste0(pathtofile,"/",b,".mgt"))
  assign(paste0("cfilename_mgt",i,sep=""), readLines(paste0(pathtofile,"/",b,".mgt")))
  c=formatC(mgt_datafram$month_app[mgt_datafram$HRU_number==i], width = 2, format = "d", flag = "")
  d=formatC(mgt_datafram$day_app[mgt_datafram$HRU_number==i], width = 2, format = "d", flag = "")
  e=formatC(mgt_datafram$FERT_ID[mgt_datafram$HRU_number==i], width = 4, format = "d", flag = "")
  f=formatC(format(round(mgt_datafram$FRT_KG[mgt_datafram$HRU_number==i],5),nsmall=5), width = 12, format = "d", flag = "")
  g=formatC(mgt_datafram$FRT_SURFACE[mgt_datafram$HRU_number==i], width = 6, format = "d", flag = "")
  assign(paste0("mgt_fert",i,sep=""), matrix(paste0(" ",c," ",d,"           ",mgt_datafram$MGT_OP[mgt_datafram$HRU_number==i]," ",e,"        ",f," ",g)))
  textreplace=readLines(get(paste0("cfilename_mgtA",i,sep="")))
  textreplace=c(textreplace[1:30],get(paste0("mgt_fert",i,sep="")),textreplace[31])
  writeLines(textreplace,get(paste0("cfilename_mgtA",i,sep="")))
}
#-----------------------------Step 6 --------------------------------------------------  
for (l in 1:nrow(mgt_datafram)){
  #l=1
  i=mgt_datafram$HRU_number[l]
  Farm_df=get(paste0("Farm_df",mgt_datafram$Barn_number[mgt_datafram$HRU_number==i][1]))
  assign(paste0("FERT_DAT_datafram",i,l,sep=""), data.frame(IFNUM=mgt_datafram$FERT_ID[l],FERTNUM=mgt_datafram$FERT_NAME[l],FMINN=format(round(Farm_df$Nmin_frac[Farm_df$Date==mgt_datafram$Date_app[l]], 3),nsmall=3),
                                                            FMINP=format(round(Farm_df$Pmin_frac[Farm_df$Date==mgt_datafram$Date_app[l]], 3),nsmall=3),
                                                            FORGN=format(round(Farm_df$Norg_frac[Farm_df$Date==mgt_datafram$Date_app[l]], 3),nsmall=3),
                                                            FORGP=format(round(Farm_df$Porg_frac[Farm_df$Date==mgt_datafram$Date_app[l]],3),nsmall=3),
                                                            FNH3N=format(round(0.990, 3), nsmall = 3),
                                                            BACTPDB=format(round(0.000, 3), nsmall = 3),
                                                            BACTLPDB=format(round(0.000, 3), nsmall = 3),
                                                            BACTKDDB=format(round(0.000, 3), nsmall = 3),
                                                            X=format(round(0.600, 3), nsmall = 3),
                                                            Y=2))
  
}
for (l in 1:nrow(mgt_datafram)){
  i=mgt_datafram$HRU_number[l]
  k=formatC(get(paste0("FERT_DAT_datafram",i,l,sep= ""))$IFNUM, width = 4, format = "d", flag = "")
  assign(paste0("new_fert",i,l,sep=""), paste0(k," ",get(paste0("FERT_DAT_datafram",i,l,sep= ""))$FERTNUM,"   ",get(paste0("FERT_DAT_datafram",i,l,sep= ""))$FMINN,"   ",get(paste0("FERT_DAT_datafram",i,l,sep= ""))$FMINP,
                                               "   ",get(paste0("FERT_DAT_datafram",i,l,sep= ""))$FORGN,"   ",get(paste0("FERT_DAT_datafram",i,l,sep= ""))$FORGP,"   ",get(paste0("FERT_DAT_datafram",i,l,sep= ""))$FNH3N,"   ",get(paste0("FERT_DAT_datafram",i,l,sep= ""))$BACTPDB,
                                               "      ",get(paste0("FERT_DAT_datafram",i,l,sep= ""))$BACTLPDB,"      ",get(paste0("FERT_DAT_datafram",i,l,sep= ""))$BACTKDDB,"   ",get(paste0("FERT_DAT_datafram",i,l,sep= ""))$X,"   ",get(paste0("FERT_DAT_datafram",i,l,sep= ""))$Y) )
  write(get(paste0("new_fert",i,l,sep= "")),file=list.files(pattern = "fert.dat"), sep = "\n",append = TRUE)
}
# CHECK IF THE NEW LINES ADDED CORRECTLY to fert.dat
file.edit("fert.dat")
#------------------------------------------------- Step 7------------------------------------------------------
##now run SWAT model with modified input files
file.remove(list.files(pattern="output."))
runSWAT2012()
source("https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/SWATmodel/R/readSWAT.R?root=ecohydrology")
save(readSWAT,file="readSWAT.R")
load("readSWAT.R")
outdata_rch_IncreaseTN = readSWAT("rch",".")
outdata_sub_IncreaseTN=readSWAT("sub",".")
cfilename_hruoutA=paste0(pathtofile,"/output.hru")
cfilename_hruout=readLines(paste0(pathtofile,"/output.hru"))
textreplace=gsub(pattern = "WTAB CLIm WTAB SOLm", replace="WTABclimwtabsolm",x=cfilename_hruout)
writeLines(textreplace, con=cfilename_hruoutA)
outdata_hru_IncreaseTN=readSWAT("hru",".") 

###########################---------------FARM--------------------------

outdata_hru_IncreaseTN$P_APPkg_per_ha[(outdata_hru_IncreaseTN$mdate=="2019-05-17")&(outdata_hru_IncreaseTN$LULC=="HYAF")]
outdata_hru_IncreaseTN$N_APPkg_per_ha[(outdata_hru_IncreaseTN$mdate=="2018-10-04")&(outdata_hru_IncreaseTN$LULC=="HYAF")]
#outdata_hru_IncreaseTN=subset(outdata_hru_IncreaseTN, outdata_hru_IncreaseTN$LULC=="HYAF")

#roja=matrix(NA, ncol = 7, nrow = 365)
#roja=data.frame(roja)
#colnames(roja)=c("Date","ORGN","ORGP","MINP","MINN","N_APP","P_APP")



for (i in 1:365){
  roja$Date[i]=i
  roja$ORGN[i]=sum(outdata_hru_IncreaseTN$ORGNkg_per_ha[outdata_hru_IncreaseTN$MON==i])
  roja$ORGP[i]=sum(outdata_hru_IncreaseTN$ORGPkg_per_ha[outdata_hru_IncreaseTN$MON==i])
  roja$MINP[i]=sum(outdata_hru_IncreaseTN$SEDPkg_per_ha[outdata_hru_IncreaseTN$MON==i])
  roja$MINN[i]=sum(outdata_hru_IncreaseTN$NSURQkg_per_ha[outdata_hru_IncreaseTN$MON==i])
  roja$N_APP[i]=sum(outdata_hru_IncreaseTN$N_APPkg_per_ha[outdata_hru_IncreaseTN$MON==i])
  roja$P_APP[i]=sum(outdata_hru_IncreaseTN$P_APPkg_per_ha[outdata_hru_IncreaseTN$MON==i])
}
outdata_rch_IncreaseTN=subset(outdata_rch_IncreaseTN,outdata_rch_IncreaseTN$RCH==1)
sum(outdata_rch_IncreaseTN$TOT_Nkg)
sum(outdata_rch_IncreaseTN$TOT_Pkg)
#----------------------------------plot the Total N +Total P in your farms farms-------------------------------
p1 <- ggplot(Farm_dfB01[c(1:365),], aes(x=Date, y=TotalNP_barn)) +
  geom_line(color="red", size=2) +
  ggtitle("Farm1")
p2 <- ggplot(Farm_dfB02[c(1:365),], aes(x=Date, y=TotalNP_barn)) +
  geom_line(color="black",size=2) +
  ggtitle("Farm2") 
p3 <-ggplot(Farm_dfB03[c(1:365),], aes(x=Date, y=TotalNP_barn)) +
  geom_line(color="black",size=2) +
  ggtitle("Farm3")


p1 + p2 + p3 + plot_layout(ncol = 1, widths = c(1, 1))
#----------------------------------plot the Stored manure in your farms farms-------------------------------
p1 <- ggplot(Farm_dfB01) +
  geom_line(aes(x=Date, y=stored_manure),color="blue", size=2) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  scale_y_continuous(labels =function(x) format(x, scientific = FALSE))+
  geom_point(data=subset(Farm_dfB01,Farm_dfB01$Date==c("2019-04-07")),aes(Date,stored_manure),shape=1,size=8,col="red",stroke=3) +
  geom_point(data=subset(Farm_dfB01,Farm_dfB01$Date==c("2019-08-25")),aes(Date,stored_manure),shape=1,size=8, col="red",stroke=3) +
  geom_point(data=subset(Farm_dfB01,Farm_dfB01$Date==c("2019-12-03")),aes(Date,stored_manure),shape=1,size=8, col="red",stroke=3) +
  geom_point(data=subset(Farm_dfB01,Farm_dfB01$Date==c("2019-05-17")),aes(Date,stored_manure),shape=1,size=8, col="green",stroke=3) +
  labs(y = 'Stored Manure (kg)')+
  ggtitle("Allandra Farm")+
  theme(axis.text.y = element_text(size = 12,colour = "black"),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(size = 15, face = "bold"))
p2 <- ggplot(Farm_dfB02) +
  geom_line( aes(x=Date, y=stored_manure),color="blue",size=2) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  scale_y_continuous(labels =function(x) format(x, scientific = FALSE))+
  geom_point(data=subset(Farm_dfB02,Farm_dfB02$Date==c("2019-04-16")),aes(Date,stored_manure),shape=1,size=8,col="red",stroke=3) +
  geom_point(data=subset(Farm_dfB02,Farm_dfB02$Date==c("2019-09-27")),aes(Date,stored_manure),shape=1,size=8,col="red",stroke=3) +
  geom_point(data=subset(Farm_dfB02,Farm_dfB02$Date==c("2019-06-15")),aes(Date,stored_manure),shape=1,size=8,col="green",stroke=3) +
  labs(y = 'Stored Manure (kg)')+
  ggtitle("Four Hills Farm")+
  theme(axis.text.y = element_text(size = 12,colour = "black"),
        axis.title.y=element_text(size = 16,face = "bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(size = 15, face = "bold"))
p3 <- ggplot(Farm_dfB01) +
  geom_line( aes(x=Date, y=stored_manure),color="blue",size=2) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  scale_y_continuous(labels =function(x) format(x, scientific = FALSE))+
  geom_point(data=subset(Farm_dfB01,Farm_dfB01$Date==c("2019-04-01")),aes(Date,stored_manure),shape=1,size=8,col="red",stroke=3) +
  geom_point(data=subset(Farm_dfB01,Farm_dfB01$Date==c("2019-07-04")),aes(Date,stored_manure),shape=1,size=8,col="red",stroke=3) +
  geom_point(data=subset(Farm_dfB01,Farm_dfB01$Date==c("2019-10-11")),aes(Date,stored_manure),shape=1,size=8,col="red",stroke=3) +
  geom_point(data=subset(Farm_dfB01,Farm_dfB01$Date==c("2019-07-12")),aes(Date,stored_manure),shape=1,size=8,col="green",stroke=3) +
  labs(y = 'Stored Manure (kg)')+
  ggtitle("Allandra Farm")+
  theme(axis.text.y = element_text(size = 11,colour = "black"),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 12,colour = "black",face = "bold",angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        plot.title=element_text(size = 15, face="bold"))

p1 + p2 + p3+ plot_layout(ncol = 1, widths = c(1, 1))
#----------------------------------plot the dry matter manure in your farms farms-------------------------------
p1 <- ggplot(Farm_dfB01, aes(x=Date, y=TotalNP_barn)) +
  geom_line(color="red", size=2) +
  ggtitle("Farm1")


plot(Farm_dfB02$Date[730:4380],Farm_dfB01$calves[730:4380],ylim=c(0,175))

plot(Farm_dfB01$Date,Farm_dfB01$yearling)

####Farm 1
mean(Farm_dfB01$total_N_barn)
mean(Farm_dfB01$total_P_barn)
mean(Farm_dfB01$Nmin)
mean(Farm_dfB01$Norg)
mean(Farm_dfB01$Pmin)
mean(Farm_dfB01$Porg)
#######fractions
mean(Farm_dfB01$Nmin_frac)
mean(Farm_dfB01$Pmin_frac)
mean(Farm_dfB01$Norg_frac)
mean(Farm_dfB01$Porg_frac)

####Farm 2
mean(Farm_dfB02$total_N_barn)
mean(Farm_dfB02$total_P_barn)
mean(Farm_dfB02$Nmin)
mean(Farm_dfB02$Norg)
mean(Farm_dfB02$Pmin)
mean(Farm_dfB02$Porg)
#######fractions
mean(Farm_dfB02$Nmin_frac)*10^4
mean(Farm_dfB02$Pmin_frac)*10^4
mean(Farm_dfB02$Norg_frac)*10^4
mean(Farm_dfB02$Porg_frac)*10^4

#####Farm 3
mean(Farm_dfB03$total_N_barn)
mean(Farm_dfB03$total_P_barn)
mean(Farm_dfB03$Nmin)
mean(Farm_dfB03$Norg)
mean(Farm_dfB03$Pmin)
mean(Farm_dfB03$Porg)

#######fractions
mean(Farm_dfB03$Nmin_frac)*10^4
mean(Farm_dfB03$Pmin_frac)*10^4
mean(Farm_dfB03$Norg_frac)*10^4
mean(Farm_dfB03$Porg_frac)*10^4

df2 <-averagenutrients
print(df2)
colnames(df2)[2]="Roja"
colnames(df2)[3]="val"
# Plot "len" by "dose" and change color by a second group: "supp"
# Add labels inside bars
p1=ggbarplot(df2,"Roja", "val",
             fill = "Farm", color = "Farm", palette = "Paired",
             label = FALSE,
             position = position_dodge(0.9))+
  labs(y = 'kg/day')+
  ggtitle("(a)") +
  theme(legend.direction='vertical',
        legend.position = c(0.85,0.4),
        axis.text.y = element_text(size = 15),
        axis.title.y=element_text(size = 15),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 15),
        axis.ticks.x=element_blank(),
        legend.text = element_text(size = 15),
        plot.title=element_text(margin=margin(t=10,b=-20),size = 20))


df3 <-FRACTIONNUTRIENT
colnames(df3)[2]="Roja"
colnames(df3)[3]="val"
p2=ggbarplot(df3,"Roja", "val",
             fill = "Farm", color = "Farm", palette = "Paired",
             label = TRUE,
             position = position_dodge(0.9))+
  labs(y = expression('(kg/kg)x'*10^4))+
  ggtitle("(b)") +
  theme(#legend.direction='vertical',
    legend.position = "none",
    axis.text.y = element_text(size = 15),
    axis.title.y=element_text(size = 15),
    axis.title.x=element_blank(),
    axis.text.x=element_text(size = 15),
    axis.ticks.x=element_blank(),
    legend.text = element_blank(),
    legend.title = element_blank(),
    plot.title=element_text(margin=margin(t=10,b=-20),size = 20))

p1 + p2 + plot_layout(ncol = 1, widths = c(1, 1))


