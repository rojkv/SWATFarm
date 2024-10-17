
flowgage_id="04282650" #Little Otter Creek at Ferrisburg, VT.
flowgage=get_usgs_gage(flowgage_id,begin_date = "2010-01-01",end_date= "2021-12-31")
flowgage$flowdata$Qmm=(flowgage$flowdata$flow)/(flowgage$area*1000)#mm

#dir.create("Dan_VSA_Calib")
setwd("Dan_VSA_Calib/")
setwd("TxtInOut_nofert/")
TItoFile=data.frame(filepre=substr(list.files(pattern="[:0:].*\\.hru",path="."),0,9))
length(TItoFile[,1])
TItoFile$tmpline=""
TItoFile$TI=""
for (i in 1:length(TItoFile$filepre)){
  #i=1
  tmpline=readLines(paste0(TItoFile[i,1],".hru"),n=1)
  TItoFile$tmpline[i]=tmpline
  TItoFile$TI[i]=sub(".*TI([0-9][0-9])A.*","t\\1",tmpline)
  filepre=TItoFile$filepre[i]
  filepost=paste0(TItoFile$TI[i],substr(filepre,4,10))
  for(swatpost in substr(list.files(pattern=TItoFile$filepre[i]),10,20)){
    print(swatpost)
    oldname=paste0(TItoFile$filepre[i],swatpost)
    print(filepre)
    print(filepost)
    print(oldname)
    newname=paste0(filepost,swatpost)
    file.rename(oldname,newname)
  }
  subbasinfile=paste0(substr(filepre,0,5),"0000.sub")
  print(subbasinfile)
  tmpfile=readLines(subbasinfile)
  tmpfile=gsub(filepre,filepost,tmpfile)
  print(tmpfile)
  writeLines(tmpfile,subbasinfile)
}




change_params=""
rm(change_params)
load(paste(path.package("EcoHydRology"), "data/change_params.rda", sep = "/"))

filetype <-c("t01*.mgt","t02*.mgt","t03*.mgt","t04*.mgt","t05*.mgt","t06*.mgt","t07*.mgt","t08*.mgt","t09*.mgt","t10*.mgt")
parameter<-c("CN2","CN2","CN2","CN2","CN2","CN2","CN2","CN2","CN2","CN2")
alter_type <-c("new","new","new","new","new","new","new","new","new","new")
min <- c(30,50,65,70,75,85,90,93,95,97)
max<-c(35,65,70,75,85,90,93,95,97,100)
current <-c(33,55,67,72,80,87,91,94,96,98)
multi <- c("FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE")
frformat <- c("F20,A80","F20,A80","F20,A80","F20,A80","F20,A80","F20,A80","F20,A80","F20,A80","F20,A80","F20,A80")
fwformat <- c("%20.5f,%s","%20.5f,%s","%20.5f,%s","%20.5f,%s","%20.5f,%s","%20.5f,%s","%20.5f,%s","%20.5f,%s","%20.5f,%s","%20.5f,%s")
newdataframe <- data.frame(filetype,parameter,alter_type,min,max,current,multi,frformat,fwformat)
change_params <- rbind(change_params,newdataframe)
change_params$filetype=as.character(change_params$filetype)
change_params$filetype[1:6]="t*.gw"
change_params$filetype[21:28]="t*.sol"
change_params$filetype[29:34]="t*.hru"
change_params$filetype=as.factor(change_params$filetype)


#########################################################
#  Load updated functions
source("https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/SWATmodel/R/readSWAT.R?root=ecohydrology")
save(readSWAT,file="readSWAT.R")
source("https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/EcoHydRology/R/setup_swatcal.R?root=ecohydrology")
source("https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/EcoHydRology/R/swat_objective_function_rch.R?root=ecohydrology")
calib_range=c("1999-12-31","2021-12-31")

######################################
#####here we should enter the parameters that are sensitive (not all of the params that are below) !!!be carful
########################################
params_select=c(1,2,3,4,5,6,7,8,9,10,11,14,21,23,24,32,33,42,43,44,45,46,47,48,49,50,51)
calib_params=change_params[params_select,]
View(calib_params)


calib_params[1:7]
setup_swatcal(calib_params)
rch=1
dir.create("/dev/shm/rojakaveh")
setwd("/dev/shm/rojakaveh")
dir.create("VSACalib")
file.copy(list.files("~/Dan_VSA_Calib/TxtInOut_nofert/",full.names = TRUE,recursive = TRUE),"/dev/shm/rojakaveh/VSACalib/",recursive = TRUE)
setwd("VSACalib/")
x=calib_params$current

swat_objective_function_rch=function (x, calib_range, calib_params, flowgage, rch,save_results=F)
{
  pacman::p_load(SWATmodel,dplyr,EcoHydRology,topmodel,utils)
  calib_params$current <- x
  tmpdir=as.character(as.integer((runif(1)+1)*10000))
  tmpdir=paste(c(format(Sys.time(), "%s"),tmpdir,Sys.getpid()),sep="",collapse="")
  print(tmpdir)
  dir.create(tmpdir)
  file.copy(list.files(),tmpdir)
  setwd(tmpdir)
  file.remove(list.files(pattern="output."))
  alter_files(calib_params)
  libarch = if (nzchar(base::version$arch)) paste("libs", base::version$arch, sep = "/") else "libs"
  swatbin <- "rswat2012.exe"
  junkout=system(shQuote(paste(path.package("SWATmodel"), libarch, swatbin, sep = "/")),intern = TRUE)
  start_year = read.fortran(textConnection(readLines("file.cio")[9]), "f20")
  load("readSWAT.R")
  outdata = readSWAT("rch",".")
  outdata$FLOW_OUTmm=(outdata$FLOW_OUTcms*24*3600)/(outdata$AREAkm2*1000)
  test2 = subset(outdata, outdata$RCH == rch)
  test3 = merge(flowgage$flowdata, test2, all = F)
  NS = topmodel::NSeff(test3$Qmm, test3$FLOW_OUTmm)
  print(NS)
  if(save_results){file.copy(list.files(),"../")}
  file.remove(list.files())
  setwd("../")
  file.remove(tmpdir)
  return(abs(NS - 1))
}

swat_objective_function_rch(x,calib_range,calib_params,flowgage,rch,save_results=F)
cl <- parallel::makeCluster(16)
outDEoptim<-DEoptim(swat_objective_function_rch,calib_params$min,calib_params$max,
                    DEoptim.control(cluster=cl,strategy = 6,NP = 16,itermax=300,parallelType = 1,
                                    packages = c("SWATmodel","dplyr","EcoHydRology","base","topmodel","utils","cl"),parVar=c("%<%","NSeff","read.fortran","readSWAT","alter_files")),calib_range,calib_params,flowgage,rch)
