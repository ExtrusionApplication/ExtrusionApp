#This R code is used to load csv or xls file into R. And clean the raw data based on demand.
#And we need to add more code to update our database daily
library(shiny)
library(bootstrap)
library(jpeg)
library(ggplot2)
library(DT)
library(stringr)
library(gsubfn)
library(proto)
library(sqldf)
library(plyr)


#LOADING DATA


#Testing the appstats data
test1 <- "//Mgrovef1/shared/Operations/EXTRUSIO/Felipe Correa Netto/Extrusion Application/Data/LaserLinc Data/AppStats Data_14986-01 LaserLinc.csv"
test2 <- "//Mgrovef1/shared/Operations/EXTRUSIO/Felipe Correa Netto/Extrusion Application/Data/LaserLinc Data/AppStats Data_14986-01 Nexiv.csv"
test3 <- "//Mgrovef1/shared/Operations/EXTRUSIO/Felipe Correa Netto/Extrusion Application/Data/LaserLinc Data/AppStats Data_14986-03 LaserLinc.csv"
test4 <- "//Mgrovef1/shared/Operations/EXTRUSIO/Felipe Correa Netto/Extrusion Application/Data/LaserLinc Data/AppStats Data_14986-03 Nexiv.csv"
ll1 <- read.csv(test1, header = TRUE, stringsAsFactors = FALSE, 
                check.names = FALSE)
n1 <- read.csv(test2, header = TRUE, stringsAsFactors = FALSE, 
               check.names = FALSE)
ll2 <- read.csv(test3, header = TRUE, stringsAsFactors = FALSE, 
                check.names = FALSE)
n2 <- read.csv(test4, header = TRUE, stringsAsFactors = FALSE, 
               check.names = FALSE)


#Creating variables across all sessions
path <- "//Mgrovef1/shared/Operations/EXTRUSIO/Felipe Correa Netto/Extrusion Application/Data/UI Data"
single_pps_file <- "Single PPS Data_UI_30 August 2017.csv"
#single_tari_file <- "Single Tari Data.csv"
single_tari_file <- "MES Data_Single Tari Data.csv"
multi_pps_file <- "Multi-Layered PPS Data_UI_30 August 2017.csv"
tapered_pps_file <- "Tapered PPS Data_UI_30 August 2017.csv"
resin_file <- "Resin Information.csv"
screw_file <- "Screw Properties.csv"
parameter_file <- "Parameters and Yield.csv"
time_file <- "Tari Time.csv"
submitter_file <- "Tari Submitter.csv"
total_file <- "Tari Total.csv"
scrapcode_file <- "Scrap Codes.csv"

single_pps_pathfile <- paste(path, single_pps_file, sep = "/")
single_tari_pathfile <- paste(path, single_tari_file, sep = "/")
multi_pps_pathfile <- paste(path, multi_pps_file, sep = "/")
tapered_pps_pathfile <- paste(path, tapered_pps_file, sep = "/")
resin_pathfile <- paste(path, resin_file, sep = "/")
screw_pathfile <- paste(path, screw_file, sep = "/")
single_parameter_filepath <- paste(path, parameter_file, sep = "/")
single_time_filepath <- paste(path, time_file, sep = "/")
single_submitter_filepath <- paste(path, submitter_file, sep = "/")
single_total_filepath <- paste(path, total_file, sep = "/")
scrapcode_filepath <- paste(path, scrapcode_file, sep = "/")

single_pps_data <- read.csv(single_pps_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                            check.names = FALSE)
single_pps_names <- names(single_pps_data)
single_tari_data <- read.csv(single_tari_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                             check.names = FALSE)
multi_pps_data <- read.csv(multi_pps_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                           check.names = FALSE)
tapered_pps_data <- read.csv(tapered_pps_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                             check.names = FALSE)
resin_data <- read.csv(resin_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                       check.names = FALSE)
screw_data <- read.csv(screw_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                       check.names = FALSE)
single_tari_parameter_data <- read.csv(single_parameter_filepath, header = TRUE, stringsAsFactors = FALSE, 
                       check.names = FALSE)
single_tari_time_data <- read.csv(single_time_filepath, header = TRUE, stringsAsFactors = FALSE, 
                       check.names = FALSE)
single_tari_submitter_data <- read.csv(single_submitter_filepath, header = TRUE, stringsAsFactors = FALSE, 
                       check.names = FALSE)
single_tari_total_data <- read.csv(single_total_filepath, header = TRUE, stringsAsFactors = FALSE, 
                       check.names = FALSE)
scrapcodes_data <- read.csv(scrapcode_filepath, header = TRUE, stringsAsFactors = FALSE, 
                       check.names = FALSE)



#getting the tapered buttons
count <- 1
tapered_buttons_vector <- c(rep(0,nrow(tapered_pps_data)))
while (count < nrow(tapered_pps_data) + 1){
  #runs through the single PPS and creates a vector of html entries for action buttons for the
  #single PPS data table.
  tapered_buttons_vector[count] <- as.character(
    actionButton(inputId = paste0("button_", tapered_pps_data[count,1]),
                 label = "Add Part",
                 onclick = 'Shiny.onInputChange(\"taperedadd_button\",  this.id)')
  )
  
  count <- count + 1
}#end tapered_pps_data buttons



##### DATA CLEANING For all Data Tables ####

#Convert all char to numeric
for (i in 6:9){
  single_pps_data[,i]<-as.numeric(single_pps_data[,i],na.rm=T)
}
for (i in 11:25){
  single_pps_data[,i]<-as.numeric(single_pps_data[,i],na.rm=T)
}

for (i in 8:11){
  multi_pps_data[,i]<-as.numeric(multi_pps_data[,i])
}
for (i in 13:20){
  multi_pps_data[,i]<-as.numeric(multi_pps_data[,i])
}
for (i in 22:32){
  multi_pps_data[,i]<-as.numeric(multi_pps_data[,i])
}

for (i in 6:9){
  tapered_pps_data[,i]<-as.numeric(tapered_pps_data[,i],na.rm=T)
}
for (i in 11:33){
  tapered_pps_data[,i]<-as.numeric(tapered_pps_data[,i],na.rm=T)
}



#obtain min and max for all length and temperature from single,multi,tapered pps data
#Create blank matrix to store min and max
single_pps_range=matrix(0,2,19)
colnames(single_pps_range)<-c("DS","DLL","TS","TLL","FT","BZT1","BZT2","BZT3","CT","AT","DT1","DT2","IDI","ODI","WT","OR","CCT","Length","PPD")
rownames(single_pps_range)<-c("min","max")


multi_pps_range=matrix(0,2,23)
colnames(multi_pps_range)<-c("DS","DLL","TS","TLL","FT","BZT1","BZT2","BZT3","CT","AT","DT1","DT2","IDI","ODI",
                             "IWT","MWT","OWT","TWT","OR","CCT","Length","ToLength","PPD")
rownames(multi_pps_range)<-c("min","max")

tapered_pps_range=matrix(0,2,27)
rownames(tapered_pps_range)<-c("min","max")
colnames(tapered_pps_range)<-c("DS","DLL","TS","TLL","FT","BZT1","BZT2","BZT3","CT","AT","DT1","DT2",
                              "PIDI","PODI","PWT","POR","PCCT","DIDI","DODI","DWT","DOR","DCCT",
                              "PLength","TLength","DLength","ToLength","PPD")
#obtain min values and max values
for (i in 1:4){
  single_pps_range[1,i]<-min(single_pps_data[,i+5],na.rm=T)
  single_pps_range[2,i]<-max(single_pps_data[,i+5],na.rm=T)
}
for (i in 5:19){
  single_pps_range[1,i]<-min(single_pps_data[,i+6],na.rm=T)
  single_pps_range[2,i]<-max(single_pps_data[,i+6],na.rm=T)
}
#multi_pps_data
for (i in 1:4){
  multi_pps_range[1,i]<-min(multi_pps_data[,i+7],na.rm=T)
  multi_pps_range[2,i]<-max(multi_pps_data[,i+7],na.rm=T)
}
for (i in 5:12){
  multi_pps_range[1,i]<-min(multi_pps_data[,i+8],na.rm=T)
  multi_pps_range[2,i]<-max(multi_pps_data[,i+8],na.rm=T)
}
for (i in 13:23){
  multi_pps_range[1,i]<-min(multi_pps_data[,i+9],na.rm=T)
  multi_pps_range[2,i]<-max(multi_pps_data[,i+9],na.rm=T)
}
#tapered
for (i in 1:4){
  tapered_pps_range[1,i]<-min(tapered_pps_data[,i+5],na.rm=T)
  tapered_pps_range[2,i]<-max(tapered_pps_data[,i+5],na.rm=T)
}
for (i in 5:27){
  tapered_pps_range[1,i]<-min(tapered_pps_data[,i+6],na.rm=T)
  tapered_pps_range[2,i]<-max(tapered_pps_data[,i+6],na.rm=T)
}


#Get the range of all tabs---single Extrusion PPS DATA
PCSDSmin=single_pps_range[1,1];PCSDSmax=single_pps_range[2,1];
PCSTSmin=single_pps_range[1,3];PCSTSmax=single_pps_range[2,3];
PCSFTmin=single_pps_range[1,5];PCSFTmax=single_pps_range[2,5];
PCSBZT1min=single_pps_range[1,6];PCSBZT1max=single_pps_range[2,6];
PCSBZT2min=single_pps_range[1,7];PCSBZT2max=single_pps_range[2,7];
PCSBZT3min=single_pps_range[1,8];PCSBZT3max=single_pps_range[2,8];
PCSCTmin=single_pps_range[1,9];PCSCTmax=single_pps_range[2,9];
PCSATmin=single_pps_range[1,10];PCSATmax=single_pps_range[2,10];
PCSDT1min=single_pps_range[1,11];PCSDT1max=single_pps_range[2,11];
PCSDT2min=single_pps_range[1,12];PCSDT2max=single_pps_range[2,12];
PCSIDImin=single_pps_range[1,13];PCSIDImax=single_pps_range[2,13];
PCSODImin=single_pps_range[1,14];PCSODImax=single_pps_range[2,14];
PCSWTmin=single_pps_range[1,15];PCSWTmax=single_pps_range[2,15];
PCSORmin=single_pps_range[1,16];PCSORmax=single_pps_range[2,16];
PCSCCTmin=single_pps_range[1,17];PCSCCTmax=single_pps_range[2,17];
PCSLengthmin=single_pps_range[1,18];PCSLengthmax=single_pps_range[2,18];
#Get the range of all tabs---Multi Extrusion PPS DATA
PCMDSmin=multi_pps_range[1,1];PCMDSmax=multi_pps_range[2,1];
PCMTSmin=multi_pps_range[1,3];PCMTSmax=multi_pps_range[2,3];
PCMFTmin=multi_pps_range[1,5];PCMFTmax=multi_pps_range[2,5];
PCMBZT1min=multi_pps_range[1,6];PCMBZT1max=multi_pps_range[2,6];
PCMBZT2min=multi_pps_range[1,7];PCMBZT2max=multi_pps_range[2,7];
PCMBZT3min=multi_pps_range[1,8];PCMBZT3max=multi_pps_range[2,8];
PCMCTmin=multi_pps_range[1,9];PCMCTmax=multi_pps_range[2,9];
PCMATmin=multi_pps_range[1,10];PCMATmax=multi_pps_range[2,10];
PCMDT1min=multi_pps_range[1,11];PCMDT1max=multi_pps_range[2,11];
PCMDT2min=multi_pps_range[1,12];PCMDT2max=multi_pps_range[2,12];
PCMIDImin=multi_pps_range[1,13];PCMIDImax=multi_pps_range[2,13];
PCMODImin=multi_pps_range[1,14];PCMODImax=multi_pps_range[2,14];
PCMIWTmin=multi_pps_range[1,15];PCMIWTmax=multi_pps_range[2,15];
PCMMWTmin=multi_pps_range[1,16];PCMMWTmax=multi_pps_range[2,16];
PCMOWTmin=multi_pps_range[1,17];PCMOWTmax=multi_pps_range[2,17];
PCMTWTmin=multi_pps_range[1,18];PCMTWTmax=multi_pps_range[2,18];
PCMORmin=multi_pps_range[1,19];PCMORmax=multi_pps_range[2,19];
PCMCCTmin=multi_pps_range[1,20];PCMCCTmax=multi_pps_range[2,20];
PCMLengthmin=multi_pps_range[1,21];PCMLengthmax=multi_pps_range[2,21];
PCMToLengthmin=multi_pps_range[1,22];PCMToLengthmax=multi_pps_range[2,22];
#Get the range of all tabs---Tapered Extrusion PPS DATA
PCTDSmin=tapered_pps_range[[1,1]];PCTDSmax=tapered_pps_range[[2,1]]
PCTTSmin=tapered_pps_range[[1,3]];PCTTSmax=tapered_pps_range[[2,3]]
PCTFTmin=tapered_pps_range[[1,5]];PCTFTmax=tapered_pps_range[[2,5]]
PCTBZT1min=tapered_pps_range[[1,6]];PCTBZT1max=tapered_pps_range[[2,6]]
PCTBZT2min=tapered_pps_range[[1,7]];PCTBZT2max=tapered_pps_range[[2,7]]
PCTBZT3min=tapered_pps_range[[1,8]];PCTBZT3max=tapered_pps_range[[2,8]]
PCTCTmin=tapered_pps_range[[1,9]];PCTCTmax=tapered_pps_range[[2,9]]
PCTATmin=tapered_pps_range[[1,10]];PCTATmax=tapered_pps_range[[2,10]]
PCTDT1min=tapered_pps_range[[1,11]];PCTDT1max=tapered_pps_range[[2,11]]
PCTDT2min=tapered_pps_range[[1,12]];PCTDT2max=tapered_pps_range[[2,12]]
PCTPIDImin=tapered_pps_range[[1,13]];PCTPIDImax=tapered_pps_range[[2,13]]
PCTPODImin=tapered_pps_range[[1,14]];PCTPODImax=tapered_pps_range[[2,14]]
PCTPWTmin=tapered_pps_range[[1,15]];PCTPWTmax=tapered_pps_range[[2,15]]
PCTPORmin=tapered_pps_range[[1,16]];PCTPORmax=tapered_pps_range[[2,16]]
PCTPCCTmin=tapered_pps_range[[1,17]];PCTPCCTmax=tapered_pps_range[[2,17]]
PCTDIDImin=tapered_pps_range[[1,18]];PCTDIDImax=tapered_pps_range[[2,18]]
PCTDODImin=tapered_pps_range[[1,19]];PCTDODImax=tapered_pps_range[[2,19]]
PCTDWTmin=tapered_pps_range[[1,20]];PCTDWTmax=tapered_pps_range[[2,20]]
PCTDORmin=tapered_pps_range[[1,21]];PCTDORmax=tapered_pps_range[[2,21]]
PCTDCCTmin=tapered_pps_range[[1,22]];PCTDCCTmax=tapered_pps_range[[2,22]]
PCTPLengthmin=tapered_pps_range[[1,23]];PCTPLengthmax=tapered_pps_range[[2,23]]
PCTTLengthmin=tapered_pps_range[[1,24]];PCTTLengthmax=tapered_pps_range[[2,24]]
PCTDLengthmin=tapered_pps_range[[1,25]];PCTDLengthmax=tapered_pps_range[[2,25]]
PCTToLengthmin=tapered_pps_range[[1,26]];PCTToLengthmax=tapered_pps_range[[2,26]]
#End obtaining Min value and Max Values

#convert NA to blank for all length and temperature values
single_pps_data[is.na(single_pps_data)]<-""
single_tari_data[is.na(single_tari_data)]<-""
multi_pps_data[is.na(multi_pps_data)]<-""
tapered_pps_data[is.na(tapered_pps_data)]<-""
resin_data[is.na(resin_data)]<-""
screw_data[is.na(screw_data)]<-""



#Output--MES--get the start date from Start Time
temp=as.data.frame(matrix(0,nrow=nrow(single_tari_data),ncol=2))
colnames(temp)=c("Start Date","Start Time")
temp[,1:2]=str_split_fixed(single_tari_data$`Start Time`,' ',2)
temp[,1]=as.Date(temp[,1],"%m/%d/%Y",origin="1970-01-01")
single_tari_data=cbind(single_tari_data[,1:which(colnames(single_tari_data)=="Start Time")-1],
                       temp,single_tari_data[,(which(colnames(single_tari_data)=="Start Time")+1):ncol(single_tari_data)])
Time_Start=sqldf("select Min([Start Date]) from single_tari_data")
Time_Start<-as.numeric(Time_Start)
Time_Start<-as.Date(Time_Start,origin="1970-01-01")
Time_End<-sqldf("select Max([Start Date]) from single_tari_data")
Time_End<-as.numeric(Time_End)
Time_End<-as.Date(Time_End,origin="1970-01-01")




#Catalog--Multi Extrusion PPS Table---Fill the Partnumber and PPS number for each single row in the table
for (i in 1:nrow(multi_pps_data)){
  if(multi_pps_data[i,"Part Number"]==""){
    multi_pps_data[i,"Part Number"]=multi_pps_data[i-1,"Part Number"]
  }
  if(multi_pps_data[i,"PPS Number"]==""){
    multi_pps_data[i,"PPS Number"]=multi_pps_data[i-1,"PPS Number"]
  }
}

#### Button vectors for PPS documents ####

#getting the single buttons
count <- 1
single_buttons_vector <- c(rep(0,nrow(single_pps_data)))
while (count < nrow(single_pps_data) + 1){
  #runs through the single PPS and creates a vector of html entries for action buttons for the
  #single PPS data table.
  single_buttons_vector[count] <- as.character(
    actionButton(inputId = paste0("button_", single_pps_data[count,1]),
                 label = "Add Part",
                 onclick = 'Shiny.onInputChange(\"singleadd_button\",  this.id)')
  )
  
  count <- count + 1
}#end single_pps_data buttons

#this then adds the html to the table
single_pps_data$"" <- single_buttons_vector
single_pps_data <- single_pps_data[,c(ncol(single_pps_data), 1:(ncol(single_pps_data)-1))]




#getting the multi-layer buttons
count <- 1
multi_buttons_vector <- c(rep(0,nrow(multi_pps_data)))
while (count < nrow(multi_pps_data) + 1){
  #runs through the multi-layer PPS and creates a vector of html entries for action buttons for the
  #multi-layer PPS data table.
  multi_buttons_vector[count] <- as.character(
    actionButton(inputId = paste0("button_", multi_pps_data[count,1]),
                 label = "Add Part",
                 onclick = 'Shiny.onInputChange(\"multiadd_button\",  this.id)')
  )
  
  count <- count + 1
}#end multi_pps_data buttons