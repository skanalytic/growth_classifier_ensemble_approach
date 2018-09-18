
## DATE: 20170804
## COMBINE MONTHLY AND ANNUAL DATA TOGETHER FOR TRAINNING AND PREDICTION

rm(list=ls()[substring(ls(),1,2)!="FZ"])
#FZlistdata(dirname="Datafiles")
#ls()
gc()
library("VSURF")
library("caret")
library("e1071")
library("zoo")
library("abind")
library("xts")
library("quantmod")
library("TTR")
library("ROCR")
library("DMwR")
library("randomForest")
library("methods")
library("pls")
library("glmnet")
library("moments")
library("installr") #load
library("MASS")
library("cvTools")

################################################    CHANGE NUMBERS HERE    ############################################################

Scolfortrain <- 48
Ecolfortrain <- 192
shiftcol <- 3           #new

# use TRUE if new file to be created !!!!!!!!!!!!!!!!!!!!!!!!!!!!
writenewfile <- TRUE

TRUE################################################ Start Reading Data (New Version) ############################################
source("C001BaseFunctions.R")
source("C002PredictionFunctions.R")

ttmanualclass <- FZreaddata(dataobj="D_CALCtrainingclass092017", dirname ="Datafiles")
ttmanualclass[ttmanualclass=="NA"] <- NA
colnames(ttmanualclass) <- colnames(FZreaddata(dataobj="D_PLSALESANN", dirname ="Datafiles"))

ttmanualclass_ANNREP <- matrix(NA, nrow = nrow(FZreaddata(dataobj="D_PLSALESLTM", dirname ="Datafiles")),ncol = ncol(FZreaddata(dataobj="D_PLSALESLTM", dirname ="Datafiles")),dimnames=dimnames(FZreaddata(dataobj="D_PLSALESLTM", dirname ="Datafiles")))
ttmanualclass_ANNREP[,colnames(ttmanualclass)] <- ttmanualclass[,colnames(ttmanualclass)]

for (colpos in 2:ncol(ttmanualclass_ANNREP)){
	ttmanualclass_ANNREP[,colpos] <- ifelse(!is.na(ttmanualclass_ANNREP[,colpos]),ttmanualclass_ANNREP[,colpos] <- ttmanualclass_ANNREP[,colpos], ttmanualclass_ANNREP[,colpos] <- ttmanualclass_ANNREP[,(colpos-1)])
}


fulllistname <- FZlistdata(dirname="Datafiles")
fulllistname <- gsub("Datafiles/","",fulllistname)

listRF <- fulllistname[substr(fulllistname,nchar(fulllistname)-8,nchar(fulllistname)-4) == "RFANN"]
fulllistname <- fulllistname[!gsub("ANN","",fulllistname) %in% gsub("RFANN","",listRF)]
fulllistname <- fulllistname[substr(fulllistname,3,5) != "NEW"]



listname <- sapply(fulllistname,FUN=function(x){
			substr(x,1,nchar(x)-4)
		})


listname <- gsub("^.*?_","",listname)
listRF <- listname[substr(listname,nchar(listname)-4,nchar(listname)) == "RFANN"]
listname <- listname[!gsub("ANN","",listname) %in% gsub("RFANN","",listRF)]

listname <- sapply(listname,FUN=function(x){
			if(substr(x,1,3) != "NEW"){
				subfront <- substr(x,1,nchar(x)-3)
				subback <- substr(x,nchar(x)-2,nchar(x))
				if(subback %in% c("ANN","LTM")){
					paste(subfront,"_",subback,sep="")
				}else{x}
			}
		})
listname <- unlist(listname)
listname <- gsub("RF","",listname)


for(nthdt in 1:length(fulllistname)){
	tempname <- fulllistname[nthdt]
	tempname <- substr(tempname,1,nchar(tempname)-4)
	tempdt <- FZreaddata(dataobj=tempname, dirname ="Datafiles")
	tempcolnames <- colnames(tempdt)
	temprownames <- rownames(tempdt)
	tempsubback <- substr(tempname,nchar(tempname)-2,nchar(tempname))

	if (length(which(!is.na(tempdt)))==0){
		ANNname <- paste(substr(tempname,1,nchar(tempname)-3),"ANN",sep="")
		tempdt1 <- FZreaddata(dataobj=ANNname, dirname ="Datafiles")

#change so that last col is only rep at current month in to year 
		tempdt <- FZgetmonthlyfromannual(ANNdata=tempdt1, LTMdata=tempdt, shiftper=3)
	}
	if (tempsubback=="ANN"){
		LTMname <- paste(substr(tempname,1,nchar(tempname)-3),"LTM",sep="")
		LTMname <- gsub("RF","",LTMname)
		tempdt1 <- FZreaddata(dataobj=LTMname, dirname ="Datafiles")

#change so that last col is only rep at current month in to year 
		tempdt <- FZgetmonthlyfromannual(ANNdata=tempdt, LTMdata=tempdt1, shiftper=3)
	}
	assign(listname[nthdt],tempdt)
}


# READ IN MCAP DATA (ANNUAL) AND REP
MCAP_ANN <- FZreaddata(dataobj="D_marketcap",dirname="Datafiles")
MCAP_LTM <- FZgetmonthlyfromannual(ANNdata=MCAP_ANN, LTMdata=PLSALES_ANN, shiftper=3)
MCAP_ANN <- FZgetmonthlyfromannual(ANNdata=MCAP_ANN, LTMdata=PLSALES_ANN, shiftper=3)

## READ IN INFLATION DATA (ANNUAL) AND REP
Inflationraw <- t(FZreaddata(dataobj="inflation",dirname="Datafiles"))
colnames(Inflationraw) <- colnames(FZreaddata(dataobj="D_PLSALESANN", dirname ="Datafiles"))
Inflationraw_LTM <- matrix(NA, nrow=nrow(Inflationraw),ncol=ncol(PLSALES_ANN))#
colnames(Inflationraw_LTM) <- colnames(PLSALES_ANN)
rownames(Inflationraw_LTM) <- rownames(Inflationraw)
Inflationraw_LTM[,colnames(Inflationraw)] <- Inflationraw[,colnames(Inflationraw)]
Inflationraw_LTM <- FZfillwithlast(input=Inflationraw_LTM)
Inflationraw_LTM <- FZnazero(input=Inflationraw_LTM)
Inflationraw_LTM <- FZshiftmx(input=Inflationraw_LTM, shiftcol=-(3+1))

Inflationraw_ANN <- matrix(NA, nrow=nrow(Inflationraw),ncol=ncol(PLSALES_ANN))#
colnames(Inflationraw_ANN) <- colnames(PLSALES_ANN)
rownames(Inflationraw_ANN) <- rownames(Inflationraw)
Inflationraw_ANN[,colnames(Inflationraw)] <- Inflationraw[,colnames(Inflationraw)]
Inflationraw_ANN <- FZfillwithlast(input=Inflationraw_ANN)
Inflationraw_ANN <- FZnazero(input=Inflationraw_ANN)
Inflationraw_ANN <- FZshiftmx(input=Inflationraw_ANN, shiftcol=-(3+1))




#####################################################################################################
####################################   CREATE VARIABLES    ##########################################
RETENTIONRATIO_ANN <- 1-(FZzerona(input=FSPAYOUTRATIO_ANN)/100) # 1 - Payout Ratio from FS
RETENTIONRATIO_LTM <- 1-(FZzerona(input=FSPAYOUTRATIO_LTM)/100) # 1 - Payout Ratio from FS

#(EBIT*(1-T))+ D&A - WCR - CAPEX
PLTAXRATE_ANN <- PLTAX_ANN/PLPRETAX_ANN
PLTAXRATE_ANN[!is.na(PLTAXRATE_ANN) & PLTAXRATE_ANN > 0.5] <- 0.5
PLTAXRATE_ANN[!is.na(PLTAXRATE_ANN) & PLTAXRATE_ANN < 0.1] <- 0.1
PLGROSSINC_ANN <- PLSALES_ANN-PLCOGSXDEP_ANN

ROE_ANN <- PLNETINC_ANN/BSEQTOT_ANN
ROEMomentum_ANN <- (FZEVAMAvgSKIP(input=ROE_ANN, maper = 3,chgper=12)-FZEVAMAvgSKIP(input=ROE_ANN, maper = 5,chgper=12))

#(EBIT*(1-T))+ D&A - WCR - CAPEX
PLTAXRATE_LTM <- PLTAX_LTM/PLPRETAX_LTM
PLTAXRATE_LTM[!is.na(PLTAXRATE_LTM) & PLTAXRATE_LTM > 0.5] <- 0.5
PLTAXRATE_LTM[!is.na(PLTAXRATE_LTM) & PLTAXRATE_LTM < 0.1] <- 0.1
PLGROSSINC_LTM <- PLSALES_LTM-PLCOGSXDEP_LTM
ROE_LTM <- PLNETINC_LTM/BSEQTOT_LTM
ROEMomentum_LTM <- (FZEVAMAvgLTM(input=ROE_LTM, maper = 3,chgper=12)-FZEVAMAvgLTM(input=ROE_LTM, maper = 5,chgper=12))


salesgrowth <- (PLSALES_ANN/FZshiftmx(input=PLSALES_ANN,shiftcol=-12))-1
SalesGrowthfull <- salesgrowth
SalesGrowthfull[is.na(SalesGrowthfull)|is.nan(SalesGrowthfull)|!is.finite(SalesGrowthfull)] <- NA
SalesGrowthfullt <- SalesGrowthfull[,Scolfortrain:Ecolfortrain]
salesgrowth_LTM <- (PLSALES_LTM/FZshiftmx(input=PLSALES_LTM,shiftcol=-12))-1
SalesGrowthfull_LTM <- salesgrowth_LTM
SalesGrowthfull_LTM[is.na(SalesGrowthfull_LTM)|is.nan(SalesGrowthfull_LTM)|!is.finite(SalesGrowthfull_LTM)] <- NA



lasttoHistMaxfull <- PLSALES_ANN/t(apply(PLSALES_ANN,1,FUN=function(x){
							x[is.na(x)] <- 0
							cummax(x)}))  # Historical Sales -> Last v.s Previous max
lasttoHistMaxfull[is.na(lasttoHistMaxfull)|is.nan(lasttoHistMaxfull)|!is.finite(lasttoHistMaxfull)] <- NA
lasttoHistMaxfullt <- lasttoHistMaxfull[,Scolfortrain:Ecolfortrain]

lasttoHistMaxfull_LTM <- PLSALES_LTM/t(apply(PLSALES_LTM,1,FUN=function(x){
							x[is.na(x)] <- 0
							cummax(x)}))
lasttoHistMaxfull_LTM[is.na(lasttoHistMaxfull_LTM)|is.nan(lasttoHistMaxfull_LTM)|!is.finite(lasttoHistMaxfull_LTM)] <- NA



ROEfull <- ROE_ANN
ROEfull[is.na(ROEfull)|is.nan(ROEfull)|!is.finite(ROEfull)] <- NA
ROEfullt <- ROEfull[,Scolfortrain:Ecolfortrain]

ROEfull_LTM <- ROE_LTM
ROEfull_LTM[is.na(ROEfull_LTM)|is.nan(ROEfull_LTM)|!is.finite(ROEfull_LTM)] <- NA

BSWCR_ANN <- (BSCURRASSETS_ANN-FZzerona(input=BSCASH_ANN)) - (BSCURRLIABS_ANN-FZzerona(input=BSDEBTSTCURRPORT_ANN))
ROIC_ANN <- (PLEBITOPER_ANN*(1-PLTAXRATE_ANN))/(BSEQTOT_ANN+FZzerona(BSDEBTLT_ANN)+FZzerona(BSDEBTST_ANN)+BSWCR_ANN)
ROICfull <- ROIC_ANN
ROICfull[is.na(ROICfull)|is.nan(ROICfull)|!is.finite(ROICfull)] <- NA
ROICfullt <- ROICfull[,Scolfortrain:Ecolfortrain]

BSWCR_LTM <- (BSCURRASSETS_LTM-FZzerona(input=BSCASH_LTM)) - (BSCURRLIABS_LTM-FZzerona(input=BSDEBTSTCURRPORT_LTM))
ROIC_LTM <- (PLEBITOPER_LTM*(1-PLTAXRATE_LTM))/(BSEQTOT_LTM+FZzerona(BSDEBTLT_LTM)+FZzerona(BSDEBTST_LTM)+BSWCR_LTM)
ROICfull_LTM <- ROIC_LTM
ROICfull_LTM[is.na(ROICfull_LTM)|is.nan(ROICfull_LTM)|!is.finite(ROICfull_LTM)] <- NA



RetentionRatiofull <- RETENTIONRATIO_ANN
RetentionRatiofull[is.na(RetentionRatiofull)|is.nan(RetentionRatiofull)|!is.finite(RetentionRatiofull)] <- NA
RetentionRatiofullt <- RetentionRatiofull[,Scolfortrain:Ecolfortrain]

RetentionRatiofull_LTM <- RETENTIONRATIO_LTM
RetentionRatiofull_LTM[is.na(RetentionRatiofull_LTM)|is.nan(RetentionRatiofull_LTM)|!is.finite(RetentionRatiofull_LTM)] <- NA

ROE_ANN <- PLNETINC_ANN/BSEQTOT_ANN
growthE_ANN <- ROE_ANN * RETENTIONRATIO_ANN
ROEnRRfull <- growthE_ANN
ROEnRRfull[is.na(ROEnRRfull)|is.nan(ROEnRRfull)|!is.finite(ROEnRRfull)] <- NA
ROEnRRfullt <- ROEnRRfull[,Scolfortrain:Ecolfortrain]

ROE_LTM <- PLNETINC_LTM/BSEQTOT_LTM
growthE_LTM <- ROE_LTM * RETENTIONRATIO_LTM
ROEnRRfull_LTM <- growthE_LTM
ROEnRRfull_LTM[is.na(ROEnRRfull_LTM)|is.nan(ROEnRRfull_LTM)|!is.finite(ROEnRRfull_LTM)] <- NA



Leveragefull <- (FZzerona(BSDEBTST_ANN)+FZzerona(BSDEBTLT_ANN))/(FZzerona(BSDEBTST_ANN)+FZzerona(BSDEBTLT_ANN)+BSEQTOT_ANN)
Leveragefull[is.na(Leveragefull)|is.nan(Leveragefull)|!is.finite(Leveragefull)] <- NA
Leveragefullt <- Leveragefull[,Scolfortrain:Ecolfortrain]

Leveragefull_LTM <- (FZzerona(BSDEBTST_LTM)+FZzerona(BSDEBTLT_LTM))/(FZzerona(BSDEBTST_LTM)+FZzerona(BSDEBTLT_LTM)+BSEQTOT_LTM)
Leveragefull_LTM[is.na(Leveragefull_LTM)|is.nan(Leveragefull_LTM)|!is.finite(Leveragefull_LTM)] <- NA

assetturnover_ANN <- (PLSALES_ANN/BSASSETS_ANN)
assetturnover_ANN <- FZEVAMAvgSKIP(input=assetturnover_ANN, maper = 3,chgper=12) ############# smooth assetturnover
growthS_ANN <- RETENTIONRATIO_ANN*assetturnover_ANN
ASSETTOnRRfull <- growthS_ANN
ASSETTOnRRfull[is.na(ASSETTOnRRfull)|is.nan(ASSETTOnRRfull)|!is.finite(ASSETTOnRRfull)] <- NA
ASSETTOnRRfullt <- ASSETTOnRRfull[,Scolfortrain:Ecolfortrain]

assetturnover_LTM <- (PLSALES_LTM/BSASSETS_LTM)
assetturnover_LTM <- FZEVAMAvgLTM(input=assetturnover_LTM, maper = 3,chgper=12) ############# smooth assetturnover
growthS_LTM <- RETENTIONRATIO_LTM*assetturnover_LTM
ASSETTOnRRfull_LTM <- growthS_LTM
ASSETTOnRRfull_LTM[is.na(ASSETTOnRRfull_LTM)|is.nan(ASSETTOnRRfull_LTM)|!is.finite(ASSETTOnRRfull_LTM)] <- NA


ulASSETTOnRRfull <- ASSETTOnRRfull/Leveragefull
ulASSETTOnRRfull[is.na(ulASSETTOnRRfull)|is.nan(ulASSETTOnRRfull)|!is.finite(ulASSETTOnRRfull)] <- NA
ulASSETTOnRRfullt <- ulASSETTOnRRfull[,Scolfortrain:Ecolfortrain]

ulASSETTOnRRfull_LTM <- ASSETTOnRRfull_LTM/Leveragefull_LTM
ulASSETTOnRRfull_LTM[is.na(ulASSETTOnRRfull_LTM)|is.nan(ulASSETTOnRRfull_LTM)|!is.finite(ulASSETTOnRRfull_LTM)] <- NA

OpMarginfull <- (PLEBITOPER_ANN/PLSALES_ANN)
OpMarginfull[is.na(OpMarginfull)|is.nan(OpMarginfull)|!is.finite(OpMarginfull)] <- NA
OpMarginfullt <- OpMarginfull[,Scolfortrain:Ecolfortrain]

OpMarginfull_LTM <- (PLEBITOPER_LTM/PLSALES_LTM)
OpMarginfull_LTM[is.na(OpMarginfull_LTM)|is.nan(OpMarginfull_LTM)|!is.finite(OpMarginfull_LTM)] <- NA


netmarginfull <- (PLNETINC_ANN/PLSALES_ANN)
netmarginfull[is.na(netmarginfull)|is.nan(netmarginfull)|!is.finite(netmarginfull)] <- NA
netmarginfullt <- netmarginfull[,Scolfortrain:Ecolfortrain]

netmarginfull_LTM <- (PLNETINC_LTM/PLSALES_LTM)
netmarginfull_LTM[is.na(netmarginfull_LTM)|is.nan(netmarginfull_LTM)|!is.finite(netmarginfull_LTM)] <- NA

quickratiofull <- (BSCURRASSETS_ANN/BSCURRLIABS_ANN)
quickratiofull[is.na(quickratiofull)|is.nan(quickratiofull)|!is.finite(quickratiofull)] <- NA
quickratiofullt <- quickratiofull[,Scolfortrain:Ecolfortrain]

quickratiofull_LTM <- (BSCURRASSETS_LTM/BSCURRLIABS_LTM)
quickratiofull_LTM[is.na(quickratiofull_LTM)|is.nan(quickratiofull_LTM)|!is.finite(quickratiofull_LTM)] <- NA

DebttEquityfull <- ((FZzerona(input=BSDEBTLT_ANN)+FZzerona(input=BSDEBTST_ANN))/BSEQTOT_ANN)
DebttEquityfull[is.na(DebttEquityfull)|is.nan(DebttEquityfull)|!is.finite(DebttEquityfull)] <- NA
DebttEquityfullt <- DebttEquityfull[,Scolfortrain:Ecolfortrain]

DebttEquityfull_LTM <- ((FZzerona(input=BSDEBTLT_LTM)+FZzerona(input=BSDEBTST_LTM))/BSEQTOT_LTM)
DebttEquityfull_LTM[is.na(DebttEquityfull_LTM)|is.nan(DebttEquityfull_LTM)|!is.finite(DebttEquityfull_LTM)] <- NA


CAPEXtoSalesfull <- (CFCAPEX_ANN/PLSALES_ANN)
CAPEXtoSalesfull[is.na(CAPEXtoSalesfull)|is.nan(CAPEXtoSalesfull)|!is.finite(CAPEXtoSalesfull)] <- NA
CAPEXtoSalesfullt <- CAPEXtoSalesfull[,Scolfortrain:Ecolfortrain]

CAPEXtoSalesfull_LTM <- (CFCAPEX_LTM/PLSALES_LTM)
CAPEXtoSalesfull_LTM[is.na(CAPEXtoSalesfull_LTM)|is.nan(CAPEXtoSalesfull_LTM)|!is.finite(CAPEXtoSalesfull_LTM)] <- NA

WCRchangefull <- (BSWCR_ANN-FZshiftmx(input=BSWCR_ANN,shiftcol=-12))-1
WCRchangefull[is.na(WCRchangefull)|is.nan(WCRchangefull)|!is.finite(WCRchangefull)] <- NA
WCRchangefullt <- WCRchangefull[,Scolfortrain:Ecolfortrain]

WCRchangefull_LTM <- (BSWCR_LTM-FZshiftmx(input=BSWCR_LTM,shiftcol=-12))-1
WCRchangefull_LTM[is.na(WCRchangefull_LTM)|is.nan(WCRchangefull_LTM)|!is.finite(WCRchangefull_LTM)] <- NA

SGAtoSales_ANN <- (PLSGA_ANN/PLSALES_ANN)
ChangeSGAtoSalesfull <- (SGAtoSales_ANN/FZshiftmx(input=SGAtoSales_ANN, shiftcol=-12))-1
ChangeSGAtoSalesfull[is.na(ChangeSGAtoSalesfull)|is.nan(ChangeSGAtoSalesfull)|!is.finite(ChangeSGAtoSalesfull)] <- NA
ChangeSGAtoSalesfullt <- ChangeSGAtoSalesfull[,Scolfortrain:Ecolfortrain]

SGAtoSales_LTM <- (PLSGA_LTM/PLSALES_LTM)
ChangeSGAtoSalesfull_LTM <- (SGAtoSales_LTM/FZshiftmx(input=SGAtoSales_LTM, shiftcol=-12))-1
ChangeSGAtoSalesfull_LTM[is.na(ChangeSGAtoSalesfull_LTM)|is.nan(ChangeSGAtoSalesfull_LTM)|!is.finite(ChangeSGAtoSalesfull_LTM)] <- NA

MAvgSalesGrowthfull <- FZEVAMAvgSKIP(input=salesgrowth,maper=3,chgper=12)
MAvgSalesGrowthfull[is.na(MAvgSalesGrowthfull)|is.nan(MAvgSalesGrowthfull)|!is.finite(MAvgSalesGrowthfull)] <- NA
MAvgSalesGrowthfullt <- MAvgSalesGrowthfull[,Scolfortrain:Ecolfortrain]


MAvgSalesGrowthfull_LTM <- FZEVAMAvgLTM(input=salesgrowth_LTM,maper=3,chgper=12)
MAvgSalesGrowthfull_LTM[is.na(MAvgSalesGrowthfull_LTM)|is.nan(MAvgSalesGrowthfull_LTM)|!is.finite(MAvgSalesGrowthfull_LTM)] <- NA

MAvglasttoHistMaxfull <- FZEVAMAvgSKIP(input=lasttoHistMaxfull,maper=3,chgper=12)
MAvglasttoHistMaxfull[is.na(MAvglasttoHistMaxfull)|is.nan(MAvglasttoHistMaxfull)|!is.finite(MAvglasttoHistMaxfull)] <- NA
MAvglasttoHistMaxfullt <- MAvglasttoHistMaxfull[,Scolfortrain:Ecolfortrain]

MAvglasttoHistMaxfull_LTM <- FZEVAMAvgLTM(input=lasttoHistMaxfull_LTM,maper=3,chgper=12)
MAvglasttoHistMaxfull_LTM[is.na(MAvglasttoHistMaxfull_LTM)|is.nan(MAvglasttoHistMaxfull_LTM)|!is.finite(MAvglasttoHistMaxfull_LTM)] <- NA

MAvgROEfull <- FZEVAMAvgSKIP(input=ROE_ANN, maper = 3,chgper=12)
MAvgROEfull[is.na(MAvgROEfull)|is.nan(MAvgROEfull)|!is.finite(MAvgROEfull)] <- NA
MAvgROEfullt <- MAvgROEfull[,Scolfortrain:Ecolfortrain]

MAvgROEfull_LTM <- FZEVAMAvgLTM(input=ROE_LTM, maper = 3,chgper=12)
MAvgROEfull_LTM[is.na(MAvgROEfull_LTM)|is.nan(MAvgROEfull_LTM)|!is.finite(MAvgROEfull_LTM)] <- NA

MAvgROICfull <- FZEVAMAvgSKIP(input=ROIC_ANN,maper=3,chgper=12)
MAvgROICfull[is.na(MAvgROICfull)|is.nan(MAvgROICfull)|!is.finite(MAvgROICfull)] <- NA
MAvgROICfullt <- MAvgROICfull[,Scolfortrain:Ecolfortrain]

MAvgROICfull_LTM <- FZEVAMAvgLTM(input=ROIC_LTM,maper=3,chgper=12)
MAvgROICfull_LTM[is.na(MAvgROICfull_LTM)|is.nan(MAvgROICfull_LTM)|!is.finite(MAvgROICfull_LTM)] <- NA

MAvgRRfull <- FZEVAMAvgSKIP(input=RetentionRatiofull,maper=3,chgper=12)
MAvgRRfull[is.na(MAvgRRfull)|is.nan(MAvgRRfull)|!is.finite(MAvgRRfull)] <- NA
MAvgRRfullt <- MAvgRRfull[,Scolfortrain:Ecolfortrain]

MAvgRRfull_LTM <- FZEVAMAvgLTM(input=RetentionRatiofull_LTM,maper=3,chgper=12)
MAvgRRfull_LTM[is.na(MAvgRRfull_LTM)|is.nan(MAvgRRfull_LTM)|!is.finite(MAvgRRfull_LTM)] <- NA

MAvgROEnRRfull <- FZEVAMAvgSKIP(input=ROEnRRfull,maper=3,chgper=12)
MAvgROEnRRfull[is.na(MAvgROEnRRfull)|is.nan(MAvgROEnRRfull)|!is.finite(MAvgROEnRRfull)] <- NA
MAvgROEnRRfullt <- MAvgROEnRRfull[,Scolfortrain:Ecolfortrain]

MAvgROEnRRfull_LTM <- FZEVAMAvgLTM(input=ROEnRRfull_LTM,maper=3,chgper=12)
MAvgROEnRRfull_LTM[is.na(MAvgROEnRRfull_LTM)|is.nan(MAvgROEnRRfull_LTM)|!is.finite(MAvgROEnRRfull_LTM)] <- NA

MAvgLeveragefull <- FZEVAMAvgSKIP(input=Leveragefull,maper=3,chgper=12)
MAvgLeveragefull[is.na(MAvgLeveragefull)|is.nan(MAvgLeveragefull)|!is.finite(MAvgLeveragefull)] <- NA
MAvgLeveragefullt <- MAvgLeveragefull[,Scolfortrain:Ecolfortrain]

MAvgLeveragefull_LTM <- FZEVAMAvgLTM(input=Leveragefull_LTM,maper=3,chgper=12)
MAvgLeveragefull_LTM[is.na(MAvgLeveragefull_LTM)|is.nan(MAvgLeveragefull_LTM)|!is.finite(MAvgLeveragefull_LTM)] <- NA

MAvgASSETTOnRRfull <- FZEVAMAvgSKIP(input=ASSETTOnRRfull,maper=3,chgper=12)
MAvgASSETTOnRRfull[is.na(MAvgASSETTOnRRfull)|is.nan(MAvgASSETTOnRRfull)|!is.finite(MAvgASSETTOnRRfull)] <- NA
MAvgASSETTOnRRfullt <- MAvgASSETTOnRRfull[,Scolfortrain:Ecolfortrain]

MAvgASSETTOnRRfull_LTM <- FZEVAMAvgLTM(input=ASSETTOnRRfull_LTM,maper=3,chgper=12)
MAvgASSETTOnRRfull_LTM[is.na(MAvgASSETTOnRRfull_LTM)|is.nan(MAvgASSETTOnRRfull_LTM)|!is.finite(MAvgASSETTOnRRfull_LTM)] <- NA

MAvgulASSETTOnRRfull <- FZEVAMAvgSKIP(input=ulASSETTOnRRfull,maper=3,chgper=12)
MAvgulASSETTOnRRfull[is.na(MAvgulASSETTOnRRfull)|is.nan(MAvgulASSETTOnRRfull)|!is.finite(MAvgulASSETTOnRRfull)] <- NA
MAvgulASSETTOnRRfullt <- MAvgulASSETTOnRRfull[,Scolfortrain:Ecolfortrain]

MAvgulASSETTOnRRfull_LTM <- FZEVAMAvgLTM(input=ulASSETTOnRRfull_LTM,maper=3,chgper=12)
MAvgulASSETTOnRRfull_LTM[is.na(MAvgulASSETTOnRRfull_LTM)|is.nan(MAvgulASSETTOnRRfull_LTM)|!is.finite(MAvgulASSETTOnRRfull_LTM)] <- NA

MAvgOpMarginfull <- FZEVAMAvgSKIP(input=OpMarginfull,maper=3,chgper=12)
MAvgOpMarginfull[is.na(MAvgOpMarginfull)|is.nan(MAvgOpMarginfull)|!is.finite(MAvgOpMarginfull)] <- NA
MAvgOpMarginfullt <- MAvgOpMarginfull[,Scolfortrain:Ecolfortrain]

MAvgOpMarginfull_LTM <- FZEVAMAvgLTM(input=OpMarginfull_LTM,maper=3,chgper=12)
MAvgOpMarginfull_LTM[is.na(MAvgOpMarginfull_LTM)|is.nan(MAvgOpMarginfull_LTM)|!is.finite(MAvgOpMarginfull_LTM)] <- NA

MAvgnetmarginfull <- FZEVAMAvgSKIP(input=netmarginfull,maper=3,chgper=12)
MAvgnetmarginfull[is.na(MAvgnetmarginfull)|is.nan(MAvgnetmarginfull)|!is.finite(MAvgnetmarginfull)] <- NA
MAvgnetmarginfullt <- MAvgnetmarginfull[,Scolfortrain:Ecolfortrain]

MAvgnetmarginfull_LTM <- FZEVAMAvgLTM(input=netmarginfull_LTM,maper=3,chgper=12)
MAvgnetmarginfull_LTM[is.na(MAvgnetmarginfull_LTM)|is.nan(MAvgnetmarginfull_LTM)|!is.finite(MAvgnetmarginfull_LTM)] <- NA

MAvgquickratiofull <- FZEVAMAvgSKIP(input=quickratiofull,maper=3,chgper=12)
MAvgquickratiofull[is.na(MAvgquickratiofull)|is.nan(MAvgquickratiofull)|!is.finite(MAvgquickratiofull)] <- NA
MAvgquickratiofullt <- MAvgquickratiofull[,Scolfortrain:Ecolfortrain]

MAvgquickratiofull_LTM <- FZEVAMAvgLTM(input=quickratiofull_LTM,maper=3,chgper=12)
MAvgquickratiofull_LTM[is.na(MAvgquickratiofull_LTM)|is.nan(MAvgquickratiofull_LTM)|!is.finite(MAvgquickratiofull_LTM)] <- NA

MAvgDebttEquityfull <- FZEVAMAvgSKIP(input=DebttEquityfull,maper=3,chgper=12)
MAvgDebttEquityfull[is.na(MAvgDebttEquityfull)|is.nan(MAvgDebttEquityfull)|!is.finite(MAvgDebttEquityfull)] <- NA
MAvgDebttEquityfullt <- MAvgDebttEquityfull[,Scolfortrain:Ecolfortrain]

MAvgDebttEquityfull_LTM <- FZEVAMAvgLTM(input=DebttEquityfull_LTM,maper=3,chgper=12)
MAvgDebttEquityfull_LTM[is.na(MAvgDebttEquityfull_LTM)|is.nan(MAvgDebttEquityfull_LTM)|!is.finite(MAvgDebttEquityfull_LTM)] <- NA

MAvgCAPEXtoSalesfull <- FZEVAMAvgSKIP(input=CAPEXtoSalesfull,maper=3,chgper=12)
MAvgCAPEXtoSalesfull[is.na(MAvgCAPEXtoSalesfull)|is.nan(MAvgCAPEXtoSalesfull)|!is.finite(MAvgCAPEXtoSalesfull)] <- NA
MAvgCAPEXtoSalesfullt <- MAvgCAPEXtoSalesfull[,Scolfortrain:Ecolfortrain]

MAvgCAPEXtoSalesfull_LTM <- FZEVAMAvgLTM(input=CAPEXtoSalesfull_LTM,maper=3,chgper=12)
MAvgCAPEXtoSalesfull_LTM[is.na(MAvgCAPEXtoSalesfull_LTM)|is.nan(MAvgCAPEXtoSalesfull_LTM)|!is.finite(MAvgCAPEXtoSalesfull_LTM)] <- NA

MAvgWCRchangefull <- FZEVAMAvgSKIP(input=WCRchangefull,maper=3,chgper=12)
MAvgWCRchangefull[is.na(MAvgWCRchangefull)|is.nan(MAvgWCRchangefull)|!is.finite(MAvgWCRchangefull)] <- NA
MAvgWCRchangefullt <- MAvgWCRchangefull[,Scolfortrain:Ecolfortrain]

MAvgWCRchangefull_LTM <- FZEVAMAvgLTM(input=WCRchangefull_LTM,maper=3,chgper=12)
MAvgWCRchangefull_LTM[is.na(MAvgWCRchangefull_LTM)|is.nan(MAvgWCRchangefull_LTM)|!is.finite(MAvgWCRchangefull_LTM)] <- NA

MAvgChangeSGAtoSalesfull <- FZEVAMAvgSKIP(input=ChangeSGAtoSalesfull,maper=3,chgper=12)
MAvgChangeSGAtoSalesfull[is.na(MAvgChangeSGAtoSalesfull)|is.nan(MAvgChangeSGAtoSalesfull)|!is.finite(MAvgChangeSGAtoSalesfull)] <- NA
MAvgChangeSGAtoSalesfullt <- MAvgChangeSGAtoSalesfull[,Scolfortrain:Ecolfortrain]

MAvgChangeSGAtoSalesfull_LTM  <- FZEVAMAvgLTM(input=ChangeSGAtoSalesfull_LTM,maper=3,chgper=12)
MAvgChangeSGAtoSalesfull_LTM[is.na(MAvgChangeSGAtoSalesfull_LTM)|is.nan(MAvgChangeSGAtoSalesfull_LTM)|!is.finite(MAvgChangeSGAtoSalesfull_LTM)] <- NA

ChglasttoHistMaxfull <- (lasttoHistMaxfull/FZshiftmx(input=lasttoHistMaxfull, shiftcol=-12))-1
ChglasttoHistMaxfull[is.na(ChglasttoHistMaxfull)|is.nan(ChglasttoHistMaxfull)|!is.finite(ChglasttoHistMaxfull)] <- NA
ChglasttoHistMaxfullt <- ChglasttoHistMaxfull[,Scolfortrain:Ecolfortrain]

ChglasttoHistMaxfull_LTM <- (lasttoHistMaxfull_LTM/FZshiftmx(input=lasttoHistMaxfull_LTM, shiftcol=-12))-1
ChglasttoHistMaxfull_LTM[is.na(ChglasttoHistMaxfull_LTM)|is.nan(ChglasttoHistMaxfull_LTM)|!is.finite(ChglasttoHistMaxfull_LTM)] <- NA

ChgROEfull <- (ROEfull/FZshiftmx(input=ROEfull, shiftcol=-12))-1
ChgROEfull[is.na(ChgROEfull)|is.nan(ChgROEfull)|!is.finite(ChgROEfull)] <- NA
ChgROEfullt <- ChgROEfull[,Scolfortrain:Ecolfortrain]

ChgROEfull_LTM <- (ROEfull_LTM/FZshiftmx(input=ROEfull_LTM, shiftcol=-12))-1
ChgROEfull_LTM[is.na(ChgROEfull_LTM)|is.nan(ChgROEfull_LTM)|!is.finite(ChgROEfull_LTM)] <- NA

ChgROICfull <- (ROICfull/FZshiftmx(input=ROICfull, shiftcol=-12))-1
ChgROICfull[is.na(ChgROICfull)|is.nan(ChgROICfull)|!is.finite(ChgROICfull)] <- NA
ChgROICfullt <- ChgROICfull[,Scolfortrain:Ecolfortrain]

ChgROICfull_LTM <- (ROICfull_LTM/FZshiftmx(input=ROICfull_LTM, shiftcol=-12))-1
ChgROICfull_LTM[is.na(ChgROICfull_LTM)|is.nan(ChgROICfull_LTM)|!is.finite(ChgROICfull_LTM)] <- NA

ChgRetentionRatiofull <- (RetentionRatiofull/FZshiftmx(input=RetentionRatiofull, shiftcol=-12))-1
ChgRetentionRatiofull[is.na(ChgRetentionRatiofull)|is.nan(ChgRetentionRatiofull)|!is.finite(ChgRetentionRatiofull)] <- NA
ChgRetentionRatiofullt <- ChgRetentionRatiofull[,Scolfortrain:Ecolfortrain]

ChgRetentionRatiofull_LTM <- (RetentionRatiofull_LTM/FZshiftmx(input=RetentionRatiofull_LTM, shiftcol=-12))-1
ChgRetentionRatiofull_LTM[is.na(ChgRetentionRatiofull_LTM)|is.nan(ChgRetentionRatiofull_LTM)|!is.finite(ChgRetentionRatiofull_LTM)] <- NA

ChgROEnRRfull <- (ROEnRRfull/FZshiftmx(input=ROEnRRfull, shiftcol=-12))-1
ChgROEnRRfull[is.na(ChgROEnRRfull)|is.nan(ChgROEnRRfull)|!is.finite(ChgROEnRRfull)] <- NA
ChgROEnRRfullt <- ChgROEnRRfull[,Scolfortrain:Ecolfortrain]

ChgROEnRRfull_LTM <- (ROEnRRfull_LTM/FZshiftmx(input=ROEnRRfull_LTM, shiftcol=-12))-1
ChgROEnRRfull_LTM[is.na(ChgROEnRRfull_LTM)|is.nan(ChgROEnRRfull_LTM)|!is.finite(ChgROEnRRfull_LTM)] <- NA

ChgLeveragefull <- (Leveragefull/FZshiftmx(input=Leveragefull, shiftcol=-12))-1
ChgLeveragefull[is.na(ChgLeveragefull)|is.nan(ChgLeveragefull)|!is.finite(ChgLeveragefull)] <- NA
ChgLeveragefullt <- ChgLeveragefull[,Scolfortrain:Ecolfortrain]

ChgLeveragefull_LTM <- (Leveragefull_LTM/FZshiftmx(input=Leveragefull_LTM, shiftcol=-12))-1
ChgLeveragefull_LTM[is.na(ChgLeveragefull_LTM)|is.nan(ChgLeveragefull_LTM)|!is.finite(ChgLeveragefull_LTM)] <- NA

ChgASSETTOnRRfull <- (ASSETTOnRRfull/FZshiftmx(input=ASSETTOnRRfull, shiftcol=-12))-1
ChgASSETTOnRRfull[is.na(ChgASSETTOnRRfull)|is.nan(ChgASSETTOnRRfull)|!is.finite(ChgASSETTOnRRfull)] <- NA
ChgASSETTOnRRfullt <- ChgASSETTOnRRfull[,Scolfortrain:Ecolfortrain]

ChgASSETTOnRRfull_LTM <- (ASSETTOnRRfull_LTM/FZshiftmx(input=ASSETTOnRRfull_LTM, shiftcol=-12))-1
ChgASSETTOnRRfull_LTM[is.na(ChgASSETTOnRRfull_LTM)|is.nan(ChgASSETTOnRRfull_LTM)|!is.finite(ChgASSETTOnRRfull_LTM)] <- NA

ChgulASSETTOnRRfull <- (ulASSETTOnRRfull/FZshiftmx(input=ulASSETTOnRRfull, shiftcol=-12))-1
ChgulASSETTOnRRfull[is.na(ChgulASSETTOnRRfull)|is.nan(ChgulASSETTOnRRfull)|!is.finite(ChgulASSETTOnRRfull)] <- NA
ChgulASSETTOnRRfullt <- ChgulASSETTOnRRfull[,Scolfortrain:Ecolfortrain]

ChgulASSETTOnRRfull_LTM <- (ulASSETTOnRRfull_LTM/FZshiftmx(input=ulASSETTOnRRfull_LTM, shiftcol=-12))-1
ChgulASSETTOnRRfull_LTM[is.na(ChgulASSETTOnRRfull_LTM)|is.nan(ChgulASSETTOnRRfull_LTM)|!is.finite(ChgulASSETTOnRRfull_LTM)] <- NA

ChgOpMarginfull <- (OpMarginfull/FZshiftmx(input=OpMarginfull, shiftcol=-12))-1
ChgOpMarginfull[is.na(ChgOpMarginfull)|is.nan(ChgOpMarginfull)|!is.finite(ChgOpMarginfull)] <- NA
ChgOpMarginfullt <- ChgOpMarginfull[,Scolfortrain:Ecolfortrain]

ChgOpMarginfull_LTM <- (OpMarginfull_LTM/FZshiftmx(input=OpMarginfull_LTM, shiftcol=-12))-1
ChgOpMarginfull_LTM[is.na(ChgOpMarginfull_LTM)|is.nan(ChgOpMarginfull_LTM)|!is.finite(ChgOpMarginfull_LTM)] <- NA

Chgnetmarginfull <- (netmarginfull/FZshiftmx(input=netmarginfull, shiftcol=-12))-1
Chgnetmarginfull[is.na(Chgnetmarginfull)|is.nan(Chgnetmarginfull)|!is.finite(Chgnetmarginfull)] <- NA
Chgnetmarginfullt <- Chgnetmarginfull[,Scolfortrain:Ecolfortrain]

Chgnetmarginfull_LTM <- (netmarginfull_LTM/FZshiftmx(input=netmarginfull_LTM, shiftcol=-12))-1
Chgnetmarginfull_LTM[is.na(Chgnetmarginfull_LTM)|is.nan(Chgnetmarginfull_LTM)|!is.finite(Chgnetmarginfull_LTM)] <- NA

Chgquickratiofull <- (quickratiofull/FZshiftmx(input=quickratiofull, shiftcol=-12))-1
Chgquickratiofull[is.na(Chgquickratiofull)|is.nan(Chgquickratiofull)|!is.finite(Chgquickratiofull)] <- NA
Chgquickratiofullt <- Chgquickratiofull[,Scolfortrain:Ecolfortrain]

Chgquickratiofull_LTM <- (quickratiofull_LTM/FZshiftmx(input=quickratiofull_LTM, shiftcol=-12))-1
Chgquickratiofull_LTM[is.na(Chgquickratiofull_LTM)|is.nan(Chgquickratiofull_LTM)|!is.finite(Chgquickratiofull_LTM)] <- NA

ChgDebttEquityfull <- (DebttEquityfull/FZshiftmx(input=DebttEquityfull, shiftcol=-12))-1
ChgDebttEquityfull[is.na(ChgDebttEquityfull)|is.nan(ChgDebttEquityfull)|!is.finite(ChgDebttEquityfull)] <- NA
ChgDebttEquityfullt <- ChgDebttEquityfull[,Scolfortrain:Ecolfortrain]

ChgDebttEquityfull_LTM <- (DebttEquityfull_LTM/FZshiftmx(input=DebttEquityfull_LTM, shiftcol=-12))-1
ChgDebttEquityfull_LTM[is.na(ChgDebttEquityfull_LTM)|is.nan(ChgDebttEquityfull_LTM)|!is.finite(ChgDebttEquityfull_LTM)] <- NA

ChgCAPEXtoSalesfull <- (CAPEXtoSalesfull/FZshiftmx(input=CAPEXtoSalesfull, shiftcol=-12))-1
ChgCAPEXtoSalesfull[is.na(ChgCAPEXtoSalesfull)|is.nan(ChgCAPEXtoSalesfull)|!is.finite(ChgCAPEXtoSalesfull)] <- NA
ChgCAPEXtoSalesfullt <- ChgCAPEXtoSalesfull[,Scolfortrain:Ecolfortrain]

ChgCAPEXtoSalesfull_LTM <- (CAPEXtoSalesfull_LTM/FZshiftmx(input=CAPEXtoSalesfull_LTM, shiftcol=-12))-1
ChgCAPEXtoSalesfull_LTM[is.na(ChgCAPEXtoSalesfull_LTM)|is.nan(ChgCAPEXtoSalesfull_LTM)|!is.finite(ChgCAPEXtoSalesfull_LTM)] <- NA

ChgWCRchangefull <- (WCRchangefull/FZshiftmx(input=WCRchangefull, shiftcol=-12))-1
ChgWCRchangefull[is.na(ChgWCRchangefull)|is.nan(ChgWCRchangefull)|!is.finite(ChgWCRchangefull)] <- NA
ChgWCRchangefullt <- ChgWCRchangefull[,Scolfortrain:Ecolfortrain]

ChgWCRchangefull_LTM <- (WCRchangefull_LTM/FZshiftmx(input=WCRchangefull_LTM, shiftcol=-12))-1
ChgWCRchangefull_LTM[is.na(ChgWCRchangefull_LTM)|is.nan(ChgWCRchangefull_LTM)|!is.finite(ChgWCRchangefull_LTM)] <- NA

ChgChangeSGAtoSalesfull <- (ChangeSGAtoSalesfull/FZshiftmx(input=ChangeSGAtoSalesfull, shiftcol=-12))-1
ChgChangeSGAtoSalesfull[is.na(ChgChangeSGAtoSalesfull)|is.nan(ChgChangeSGAtoSalesfull)|!is.finite(ChgChangeSGAtoSalesfull)] <- NA
ChgChangeSGAtoSalesfullt <- ChgChangeSGAtoSalesfull[,Scolfortrain:Ecolfortrain]

ChgChangeSGAtoSalesfull_LTM <- (ChangeSGAtoSalesfull_LTM/FZshiftmx(input=ChangeSGAtoSalesfull_LTM, shiftcol=-12))-1
ChgChangeSGAtoSalesfull_LTM[is.na(ChgChangeSGAtoSalesfull_LTM)|is.nan(ChgChangeSGAtoSalesfull_LTM)|!is.finite(ChgChangeSGAtoSalesfull_LTM)] <- NA





###################### PCv1 ######################
#1st
PCv1 <- BSPPEGROSS_ANN+FZcapplitemBSitemGROSSLTM(input=PLRDEXP_ANN, capper = 3)
PCv1[is.na(PCv1)|is.nan(PCv1)|!is.finite(PCv1)] <- NA
PCv1t <- PCv1[,Scolfortrain:Ecolfortrain]

PCv1trailingmax <- FZtrailingmax(input=PCv1)
PCv1Growth1perchg <- PCv1/PCv1trailingmax
PCv1Growth1perchg[is.na(PCv1Growth1perchg)|is.nan(PCv1Growth1perchg)|!is.finite(PCv1Growth1perchg)] <- NA
PCv1Growth1perchgt <- PCv1Growth1perchg[,Scolfortrain:Ecolfortrain]

PCv1Growthlastvs3perma <- PCv1/FZEVAMAvgSKIP(input=PCv1trailingmax,maper=3,chgper=12)
PCv1Growthlastvs3perma[is.na(PCv1Growthlastvs3perma)|is.nan(PCv1Growthlastvs3perma)|!is.finite(PCv1Growthlastvs3perma)] <- NA
PCv1Growthlastvs3permat <- PCv1Growthlastvs3perma[,Scolfortrain:Ecolfortrain]

PCv1Growth3permachg <- FZEVAMAvgSKIP(input=PCv1Growth1perchg,maper=3,chgper=12)
PCv1Growth3permachg[is.na(PCv1Growth3permachg)|is.nan(PCv1Growth3permachg)|!is.finite(PCv1Growth3permachg)] <- NA
PCv1Growth3permachgt <- PCv1Growth3permachg[,Scolfortrain:Ecolfortrain]

PCv1Growth1pchgvs3pmachg <- PCv1Growth1perchg/PCv1Growth3permachg
PCv1Growth1pchgvs3pmachg[is.na(PCv1Growth1pchgvs3pmachg)|is.nan(PCv1Growth1pchgvs3pmachg)|!is.finite(PCv1Growth1pchgvs3pmachg)] <- NA
PCv1Growth1pchgvs3pmachgt <- PCv1Growth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]


#### 1st_LTM
PCv1_LTM <- BSPPEGROSS_LTM+FZcapplitemBSitemGROSSLTM(input=PLRDEXP_LTM,capper=4)
PCv1_LTM[is.na(PCv1_LTM)|is.nan(PCv1_LTM)|!is.finite(PCv1_LTM)] <- NA

PCv1trailingmax_LTM <- FZtrailingmax(input=PCv1_LTM)
PCv1Growth1perchg_LTM <- PCv1_LTM/PCv1trailingmax_LTM
PCv1Growth1perchg_LTM[is.na(PCv1Growth1perchg_LTM)|is.nan(PCv1Growth1perchg_LTM)|!is.finite(PCv1Growth1perchg_LTM)] <- NA

PCv1Growthlastvs3perma_LTM <- PCv1_LTM/FZEVAMAvgLTM(input=PCv1trailingmax_LTM,maper=3,chgper=12)
PCv1Growthlastvs3perma_LTM[is.na(PCv1Growthlastvs3perma_LTM)|is.nan(PCv1Growthlastvs3perma_LTM)|!is.finite(PCv1Growthlastvs3perma_LTM)] <- NA

PCv1Growth3permachg_LTM <- FZEVAMAvgLTM(input=PCv1Growth1perchg_LTM,maper=3,chgper=12)
PCv1Growth3permachg_LTM[is.na(PCv1Growth3permachg_LTM)|is.nan(PCv1Growth3permachg_LTM)|!is.finite(PCv1Growth3permachg_LTM)] <- NA

PCv1Growth1pchgvs3pmachg_LTM <- PCv1Growth1perchg_LTM/PCv1Growth3permachg_LTM
PCv1Growth1pchgvs3pmachg_LTM[is.na(PCv1Growth1pchgvs3pmachg_LTM)|is.nan(PCv1Growth1pchgvs3pmachg_LTM)|!is.finite(PCv1Growth1pchgvs3pmachg_LTM)] <- NA


###################### PCv2 ######################
#2nd
GrPPEexCIP_ANN <- BSPPEGROSS_ANN-FZzerona(BSPPEGROSSCONSTR_ANN)
InfladjGrPPEexCIP_ANN <- FZEVAinflAdjGrossPPELTM(GrossPPEmx=GrPPEexCIP_ANN,AccmuDepnmx=BSPPEDEP_ANN,Depnmx=PLDEPAMORTEXP_ANN,Inflationmx=Inflationraw_ANN,Countrymx=Country)
PCv2 <- InfladjGrPPEexCIP_ANN+FZcapplitemBSitemGROSSLTM(input=PLRDEXP_ANN, capper = 3)+FZzerona(BSPPEGROSSCONSTR_ANN)

PCv2[is.na(PCv2)|is.nan(PCv2)|!is.finite(PCv2)] <- NA
PCv2t <- PCv2[,Scolfortrain:Ecolfortrain]

PCv2trailingmax <- FZtrailingmax(input=PCv2)
PCv2Growth1perchg <- PCv2/PCv2trailingmax
PCv2Growth1perchg[is.na(PCv2Growth1perchg)|is.nan(PCv2Growth1perchg)|!is.finite(PCv2Growth1perchg)] <- NA
PCv2Growth1perchgt <- PCv2Growth1perchg[,Scolfortrain:Ecolfortrain]

PCv2Growthlastvs3perma <- PCv2/FZEVAMAvgLTM(input=PCv2trailingmax,maper=3,chgper=12)
PCv2Growthlastvs3perma[is.na(PCv2Growthlastvs3perma)|is.nan(PCv2Growthlastvs3perma)|!is.finite(PCv2Growthlastvs3perma)] <- NA
PCv2Growthlastvs3permat <- PCv2Growthlastvs3perma[,Scolfortrain:Ecolfortrain]

PCv2Growth3permachg <- FZEVAMAvgLTM(input=PCv2Growth1perchg,maper=3,chgper=12)
PCv2Growth3permachg[is.na(PCv2Growth3permachg)|is.nan(PCv2Growth3permachg)|!is.finite(PCv2Growth3permachg)] <- NA
PCv2Growth3permachgt <- PCv2Growth3permachg[,Scolfortrain:Ecolfortrain]

PCv2Growth1pchgvs3pmachg <- PCv2Growth1perchg/PCv2Growth3permachg
PCv2Growth1pchgvs3pmachg[is.na(PCv2Growth1pchgvs3pmachg)|is.nan(PCv2Growth1pchgvs3pmachg)|!is.finite(PCv2Growth1pchgvs3pmachg)] <- NA
PCv2Growth1pchgvs3pmachgt <- PCv2Growth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

#### 2nd_LTM
GrPPEexCIP_LTM <- BSPPEGROSS_LTM-FZzerona(BSPPEGROSSCONSTR_LTM)
InfladjGrPPEexCIP_LTM <- FZEVAinflAdjGrossPPELTM(GrossPPEmx=GrPPEexCIP_LTM,AccmuDepnmx=BSPPEDEP_LTM,Depnmx=PLDEPAMORTEXP_LTM,Inflationmx=Inflationraw_LTM,Countrymx=Country)
PCv2_LTM <- InfladjGrPPEexCIP_LTM+FZcapplitemBSitemGROSSLTM(input=PLRDEXP_LTM,capper=4)+FZzerona(BSPPEGROSSCONSTR_LTM)
PCv2_LTM[is.na(PCv2_LTM)|is.nan(PCv2_LTM)|!is.finite(PCv2_LTM)] <- NA

PCv2trailingmax_LTM <- FZtrailingmax(input=PCv2_LTM)
PCv2Growth1perchg_LTM <- PCv2_LTM/PCv2trailingmax_LTM
PCv2Growth1perchg_LTM[is.na(PCv2Growth1perchg_LTM)|is.nan(PCv2Growth1perchg_LTM)|!is.finite(PCv2Growth1perchg_LTM)] <- NA

PCv2Growthlastvs3perma_LTM <- PCv2_LTM/FZEVAMAvgLTM(input=PCv2trailingmax_LTM,maper=3,chgper=12)
PCv2Growthlastvs3perma_LTM[is.na(PCv2Growthlastvs3perma_LTM)|is.nan(PCv2Growthlastvs3perma_LTM)|!is.finite(PCv2Growthlastvs3perma_LTM)] <- NA

PCv2Growth3permachg_LTM <- FZEVAMAvgLTM(input=PCv2Growth1perchg_LTM,maper=3,chgper=12)
PCv2Growth3permachg_LTM[is.na(PCv2Growth3permachg_LTM)|is.nan(PCv2Growth3permachg_LTM)|!is.finite(PCv2Growth3permachg_LTM)] <- NA

PCv2Growth1pchgvs3pmachg_LTM <- PCv2Growth1perchg_LTM/PCv2Growth3permachg_LTM
PCv2Growth1pchgvs3pmachg_LTM[is.na(PCv2Growth1pchgvs3pmachg_LTM)|is.nan(PCv2Growth1pchgvs3pmachg_LTM)|!is.finite(PCv2Growth1pchgvs3pmachg_LTM)] <- NA



###################### PCv3 ######################
#3rd
PCv3 <- PCv2+FZzerona(BSACCREC_ANN)+FZzerona(BSINVENTORY_ANN)-FZzerona(BSACCPAY_ANN)
PCv3[is.na(PCv3)|is.nan(PCv3)|!is.finite(PCv3)] <- NA
PCv3t <- PCv3[,Scolfortrain:Ecolfortrain]

PCv3trailingmax <- FZtrailingmax(input=PCv3)
PCv3Growth1perchg <- PCv3/PCv3trailingmax
PCv3Growth1perchg[is.na(PCv3Growth1perchg)|is.nan(PCv3Growth1perchg)|!is.finite(PCv3Growth1perchg)] <- NA
PCv3Growth1perchgt <- PCv3Growth1perchg[,Scolfortrain:Ecolfortrain]

PCv3Growthlastvs3perma <- PCv3/FZEVAMAvgSKIP(input=PCv3trailingmax,maper=3,chgper=12)
PCv3Growthlastvs3perma[is.na(PCv3Growthlastvs3perma)|is.nan(PCv3Growthlastvs3perma)|!is.finite(PCv3Growthlastvs3perma)] <- NA
PCv3Growthlastvs3permat <- PCv3Growthlastvs3perma[,Scolfortrain:Ecolfortrain]

PCv3Growth3permachg <- FZEVAMAvgSKIP(input=PCv3Growth1perchg,maper=3,chgper=12)
PCv3Growth3permachg[is.na(PCv3Growth3permachg)|is.nan(PCv3Growth3permachg)|!is.finite(PCv3Growth3permachg)] <- NA
PCv3Growth3permachgt <- PCv3Growth3permachg[,Scolfortrain:Ecolfortrain]

PCv3Growth1pchgvs3pmachg <- PCv3Growth1perchg/PCv3Growth3permachg
PCv3Growth1pchgvs3pmachg[is.na(PCv3Growth1pchgvs3pmachg)|is.nan(PCv3Growth1pchgvs3pmachg)|!is.finite(PCv3Growth1pchgvs3pmachg)] <- NA
PCv3Growth1pchgvs3pmachgt <- PCv3Growth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

#### 3rd_LTM
PCv3_LTM <- PCv2_LTM+FZzerona(BSACCREC_LTM)+FZzerona(BSINVENTORY_LTM)-FZzerona(BSACCPAY_LTM)
PCv3_LTM[is.na(PCv3_LTM)|is.nan(PCv3_LTM)|!is.finite(PCv3_LTM)] <- NA

PCv3trailingmax_LTM <- FZtrailingmax(input=PCv3_LTM)
PCv3Growth1perchg_LTM <- PCv3_LTM/PCv3trailingmax_LTM
PCv3Growth1perchg_LTM[is.na(PCv3Growth1perchg_LTM)|is.nan(PCv3Growth1perchg_LTM)|!is.finite(PCv3Growth1perchg_LTM)] <- NA

PCv3Growthlastvs3perma_LTM <- PCv3_LTM/FZEVAMAvgLTM(input=PCv3trailingmax_LTM,maper=3,chgper=12)
PCv3Growthlastvs3perma_LTM[is.na(PCv3Growthlastvs3perma_LTM)|is.nan(PCv3Growthlastvs3perma_LTM)|!is.finite(PCv3Growthlastvs3perma_LTM)] <- NA

PCv3Growth3permachg_LTM <- FZEVAMAvgLTM(input=PCv3Growth1perchg_LTM,maper=3,chgper=12)
PCv3Growth3permachg_LTM[is.na(PCv3Growth3permachg_LTM)|is.nan(PCv3Growth3permachg_LTM)|!is.finite(PCv3Growth3permachg_LTM)] <- NA

PCv3Growth1pchgvs3pmachg_LTM <- PCv3Growth1perchg_LTM/PCv3Growth3permachg_LTM
PCv3Growth1pchgvs3pmachg_LTM[is.na(PCv3Growth1pchgvs3pmachg_LTM)|is.nan(PCv3Growth1pchgvs3pmachg_LTM)|!is.finite(PCv3Growth1pchgvs3pmachg_LTM)] <- NA




###################### PCv4 ######################
#4th
PCv4 <- PCv3+FZzerona(BSINTANGIBLE_ANN)
PCv4[is.na(PCv4)|is.nan(PCv4)|!is.finite(PCv4)] <- NA
PCv4t <- PCv4[,Scolfortrain:Ecolfortrain]

PCv4trailingmax <- FZtrailingmax(input=PCv4)
PCv4Growth1perchg <- PCv4/PCv4trailingmax
PCv4Growth1perchg[is.na(PCv4Growth1perchg)|is.nan(PCv4Growth1perchg)|!is.finite(PCv4Growth1perchg)] <- NA
PCv4Growth1perchgt <- PCv4Growth1perchg[,Scolfortrain:Ecolfortrain]

PCv4Growthlastvs3perma <- PCv4/FZEVAMAvgSKIP(input=PCv4trailingmax,maper=3,chgper=12)
PCv4Growthlastvs3perma[is.na(PCv4Growthlastvs3perma)|is.nan(PCv4Growthlastvs3perma)|!is.finite(PCv4Growthlastvs3perma)] <- NA
PCv4Growthlastvs3permat <- PCv4Growthlastvs3perma[,Scolfortrain:Ecolfortrain]

PCv4Growth3permachg <- FZEVAMAvgSKIP(input=PCv4Growth1perchg,maper=3,chgper=12)
PCv4Growth3permachg[is.na(PCv4Growth3permachg)|is.nan(PCv4Growth3permachg)|!is.finite(PCv4Growth3permachg)] <- NA
PCv4Growth3permachgt <- PCv4Growth3permachg[,Scolfortrain:Ecolfortrain]

PCv4Growth1pchgvs3pmachg <- PCv4Growth1perchg/PCv4Growth3permachg
PCv4Growth1pchgvs3pmachg[is.na(PCv4Growth1pchgvs3pmachg)|is.nan(PCv4Growth1pchgvs3pmachg)|!is.finite(PCv4Growth1pchgvs3pmachg)] <- NA
PCv4Growth1pchgvs3pmachgt <- PCv4Growth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

#### 4th
PCv4_LTM <- PCv3_LTM+FZzerona(BSINTANGIBLE_LTM)
PCv4_LTM[is.na(PCv4_LTM)|is.nan(PCv4_LTM)|!is.finite(PCv4_LTM)] <- NA
PCv4_LTMt <- PCv4_LTM[,Scolfortrain:Ecolfortrain]

PCv4trailingmax_LTM <- FZtrailingmax(input=PCv4_LTM)
PCv4Growth1perchg_LTM <- PCv4_LTM/PCv4trailingmax_LTM
PCv4Growth1perchg_LTM[is.na(PCv4Growth1perchg_LTM)|is.nan(PCv4Growth1perchg_LTM)|!is.finite(PCv4Growth1perchg_LTM)] <- NA

PCv4Growthlastvs3perma_LTM <- PCv4_LTM/FZEVAMAvgLTM(input=PCv4trailingmax_LTM,maper=3,chgper=12)
PCv4Growthlastvs3perma_LTM[is.na(PCv4Growthlastvs3perma_LTM)|is.nan(PCv4Growthlastvs3perma_LTM)|!is.finite(PCv4Growthlastvs3perma_LTM)] <- NA

PCv4Growth3permachg_LTM <- FZEVAMAvgLTM(input=PCv4Growth1perchg_LTM,maper=3,chgper=12)
PCv4Growth3permachg_LTM[is.na(PCv4Growth3permachg_LTM)|is.nan(PCv4Growth3permachg_LTM)|!is.finite(PCv4Growth3permachg_LTM)] <- NA

PCv4Growth1pchgvs3pmachg_LTM <- PCv4Growth1perchg_LTM/PCv4Growth3permachg_LTM
PCv4Growth1pchgvs3pmachg_LTM[is.na(PCv4Growth1pchgvs3pmachg_LTM)|is.nan(PCv4Growth1pchgvs3pmachg_LTM)|!is.finite(PCv4Growth1pchgvs3pmachg_LTM)] <- NA




# Productive Assets
###################### GrossPPE ######################
MAvgGrossPPE <- FZEVAMAvgSKIP(input=BSPPEGROSS_ANN,maper=3,chgper=12)
MAvgGrossPPE[is.na(MAvgGrossPPE)|is.nan(MAvgGrossPPE)|!is.finite(MAvgGrossPPE)] <- NA
MAvgGrossPPEt <- MAvgGrossPPE[,Scolfortrain:Ecolfortrain]

ChgGrossPPEfull <- (BSPPEGROSS_ANN/FZshiftmx(input=BSPPEGROSS_ANN, shiftcol=-12))-1
ChgGrossPPEfull[is.na(ChgGrossPPEfull)|is.nan(ChgGrossPPEfull)|!is.finite(ChgGrossPPEfull)] <- NA
ChgGrossPPEfullt <- ChgGrossPPEfull[,Scolfortrain:Ecolfortrain]

BSPPEGROSStrailingmax <- FZtrailingmax(input=BSPPEGROSS_ANN)
BSPPEGROSS1perchg <- BSPPEGROSS_ANN/BSPPEGROSStrailingmax
BSPPEGROSS1perchg[is.na(BSPPEGROSS1perchg)|is.nan(BSPPEGROSS1perchg)|!is.finite(BSPPEGROSS1perchg)] <- NA
BSPPEGROSS1perchgt <- BSPPEGROSS1perchg[,Scolfortrain:Ecolfortrain]

BSPPEGROSSlastvs3perma <- BSPPEGROSS_ANN/FZEVAMAvgSKIP(input=BSPPEGROSStrailingmax,maper=3,chgper=12)
BSPPEGROSSlastvs3perma[is.na(BSPPEGROSSlastvs3perma)|is.nan(BSPPEGROSSlastvs3perma)|!is.finite(BSPPEGROSSlastvs3perma)] <- NA
BSPPEGROSSlastvs3permat <- BSPPEGROSSlastvs3perma[,Scolfortrain:Ecolfortrain]

BSPPEGROSS3permachg <- FZEVAMAvgSKIP(input=BSPPEGROSS1perchg,maper=3,chgper=12)
BSPPEGROSS3permachg[is.na(BSPPEGROSS3permachg)|is.nan(BSPPEGROSS3permachg)|!is.finite(BSPPEGROSS3permachg)] <- NA
BSPPEGROSS3permachgt <- BSPPEGROSS3permachg[,Scolfortrain:Ecolfortrain]

BSPPEGROSS1pchgvs3pmachg <- BSPPEGROSS1perchg/BSPPEGROSS3permachg
BSPPEGROSS1pchgvs3pmachg[is.na(BSPPEGROSS1pchgvs3pmachg)|is.nan(BSPPEGROSS1pchgvs3pmachg)|!is.finite(BSPPEGROSS1pchgvs3pmachg)] <- NA
BSPPEGROSS1pchgvs3pmachgt <- BSPPEGROSS1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
MAvgGrossPPE_LTM <- FZEVAMAvgLTM(input=BSPPEGROSS_LTM,maper=3,chgper=12)
MAvgGrossPPE_LTM[is.na(MAvgGrossPPE_LTM)|is.nan(MAvgGrossPPE_LTM)|!is.finite(MAvgGrossPPE_LTM)] <- NA

ChgGrossPPEfull_LTM <- (BSPPEGROSS_LTM/FZshiftmx(input=BSPPEGROSS_LTM, shiftcol=-12))-1
ChgGrossPPEfull_LTM[is.na(ChgGrossPPEfull_LTM)|is.nan(ChgGrossPPEfull_LTM)|!is.finite(ChgGrossPPEfull_LTM)] <- NA

BSPPEGROSStrailingmax_LTM <- FZtrailingmax(input=BSPPEGROSS_LTM)
BSPPEGROSS1perchg_LTM <- BSPPEGROSS_LTM/BSPPEGROSStrailingmax_LTM
BSPPEGROSS1perchg_LTM[is.na(BSPPEGROSS1perchg_LTM)|is.nan(BSPPEGROSS1perchg_LTM)|!is.finite(BSPPEGROSS1perchg_LTM)] <- NA

BSPPEGROSSlastvs3perma_LTM <- BSPPEGROSS_LTM/FZEVAMAvgLTM(input=BSPPEGROSStrailingmax_LTM,maper=3,chgper=12)
BSPPEGROSSlastvs3perma_LTM[is.na(BSPPEGROSSlastvs3perma_LTM)|is.nan(BSPPEGROSSlastvs3perma_LTM)|!is.finite(BSPPEGROSSlastvs3perma_LTM)] <- NA

BSPPEGROSS3permachg_LTM <- FZEVAMAvgLTM(input=BSPPEGROSS1perchg_LTM,maper=3,chgper=12)
BSPPEGROSS3permachg_LTM[is.na(BSPPEGROSS3permachg_LTM)|is.nan(BSPPEGROSS3permachg_LTM)|!is.finite(BSPPEGROSS3permachg_LTM)] <- NA

BSPPEGROSS1pchgvs3pmachg_LTM <- BSPPEGROSS1perchg_LTM/BSPPEGROSS3permachg_LTM
BSPPEGROSS1pchgvs3pmachg_LTM[is.na(BSPPEGROSS1pchgvs3pmachg_LTM)|is.nan(BSPPEGROSS1pchgvs3pmachg_LTM)|!is.finite(BSPPEGROSS1pchgvs3pmachg_LTM)] <- NA


###################### GrossPPEexCIP ######################
GrPPEexCIP <- BSPPEGROSS_ANN-FZzerona(BSPPEGROSSCONSTR_ANN)
GrPPEexCIP[is.na(GrPPEexCIP)|is.nan(GrPPEexCIP)|!is.finite(GrPPEexCIP)] <- NA
GrPPEexCIPt <- GrPPEexCIP[,Scolfortrain:Ecolfortrain]

MAvgGrPPEexCIP <- FZEVAMAvgSKIP(input=GrPPEexCIP,maper=3,chgper=12)
MAvgGrPPEexCIP[is.na(MAvgGrPPEexCIP)|is.nan(MAvgGrPPEexCIP)|!is.finite(MAvgGrPPEexCIP)] <- NA
MAvgGrPPEexCIPt <- MAvgGrPPEexCIP[,Scolfortrain:Ecolfortrain]

ChgGrPPEexCIP <- (GrPPEexCIP/FZshiftmx(input=GrPPEexCIP, shiftcol=-12))-1
ChgGrPPEexCIP[is.na(ChgGrPPEexCIP)|is.nan(ChgGrPPEexCIP)|!is.finite(ChgGrPPEexCIP)] <- NA
ChgGrPPEexCIPt <- ChgGrPPEexCIP[,Scolfortrain:Ecolfortrain]

GrPPEexCIPtrailingmax <- FZtrailingmax(input=GrPPEexCIP)
GrPPEexCIP1perchg <- GrPPEexCIP/GrPPEexCIPtrailingmax
GrPPEexCIP1perchg[is.na(GrPPEexCIP1perchg)|is.nan(GrPPEexCIP1perchg)|!is.finite(GrPPEexCIP1perchg)] <- NA
GrPPEexCIP1perchgt <- GrPPEexCIP1perchg[,Scolfortrain:Ecolfortrain]

GrPPEexCIPlastvs3perma <- GrPPEexCIP/FZEVAMAvgSKIP(input=GrPPEexCIPtrailingmax,maper=3,chgper=12)
GrPPEexCIPlastvs3perma[is.na(GrPPEexCIPlastvs3perma)|is.nan(GrPPEexCIPlastvs3perma)|!is.finite(GrPPEexCIPlastvs3perma)] <- NA
GrPPEexCIPlastvs3permat <- GrPPEexCIPlastvs3perma[,Scolfortrain:Ecolfortrain]

GrPPEexCIP3permachg <- FZEVAMAvgSKIP(input=GrPPEexCIP1perchg,maper=3,chgper=12)
GrPPEexCIP3permachg[is.na(GrPPEexCIP3permachg)|is.nan(GrPPEexCIP3permachg)|!is.finite(GrPPEexCIP3permachg)] <- NA
GrPPEexCIP3permachgt <- GrPPEexCIP3permachg[,Scolfortrain:Ecolfortrain]

GrPPEexCIP1pchgvs3pmachg <- GrPPEexCIP1perchg/GrPPEexCIP3permachg
GrPPEexCIP1pchgvs3pmachg[is.na(GrPPEexCIP1pchgvs3pmachg)|is.nan(GrPPEexCIP1pchgvs3pmachg)|!is.finite(GrPPEexCIP1pchgvs3pmachg)] <- NA
GrPPEexCIP1pchgvs3pmachgt <- GrPPEexCIP1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
GrPPEexCIP_LTM <- BSPPEGROSS_LTM-FZzerona(BSPPEGROSSCONSTR_LTM)
GrPPEexCIP_LTM[is.na(GrPPEexCIP_LTM)|is.nan(GrPPEexCIP_LTM)|!is.finite(GrPPEexCIP_LTM)] <- NA

MAvgGrPPEexCIP_LTM <- FZEVAMAvgLTM(input=GrPPEexCIP_LTM,maper=3,chgper=12)
MAvgGrPPEexCIP_LTM[is.na(MAvgGrPPEexCIP_LTM)|is.nan(MAvgGrPPEexCIP_LTM)|!is.finite(MAvgGrPPEexCIP_LTM)] <- NA

ChgGrPPEexCIP_LTM <- (GrPPEexCIP_LTM/FZshiftmx(input=GrPPEexCIP_LTM, shiftcol=-12))-1
ChgGrPPEexCIP_LTM[is.na(ChgGrPPEexCIP_LTM)|is.nan(ChgGrPPEexCIP_LTM)|!is.finite(ChgGrPPEexCIP_LTM)] <- NA

GrPPEexCIPtrailingmax_LTM <- FZtrailingmax(input=GrPPEexCIP_LTM)
GrPPEexCIP1perchg_LTM <- GrPPEexCIP_LTM/GrPPEexCIPtrailingmax_LTM
GrPPEexCIP1perchg_LTM[is.na(GrPPEexCIP1perchg_LTM)|is.nan(GrPPEexCIP1perchg_LTM)|!is.finite(GrPPEexCIP1perchg_LTM)] <- NA

GrPPEexCIPlastvs3perma_LTM <- GrPPEexCIP_LTM/FZEVAMAvgLTM(input=GrPPEexCIPtrailingmax_LTM,maper=3,chgper=12)
GrPPEexCIPlastvs3perma_LTM[is.na(GrPPEexCIPlastvs3perma_LTM)|is.nan(GrPPEexCIPlastvs3perma_LTM)|!is.finite(GrPPEexCIPlastvs3perma_LTM)] <- NA

GrPPEexCIP3permachg_LTM <- FZEVAMAvgLTM(input=GrPPEexCIP1perchg_LTM,maper=3,chgper=12)
GrPPEexCIP3permachg_LTM[is.na(GrPPEexCIP3permachg_LTM)|is.nan(GrPPEexCIP3permachg_LTM)|!is.finite(GrPPEexCIP3permachg_LTM)] <- NA

GrPPEexCIP1pchgvs3pmachg_LTM <- GrPPEexCIP1perchg_LTM/GrPPEexCIP3permachg_LTM
GrPPEexCIP1pchgvs3pmachg_LTM[is.na(GrPPEexCIP1pchgvs3pmachg_LTM)|is.nan(GrPPEexCIP1pchgvs3pmachg_LTM)|!is.finite(GrPPEexCIP1pchgvs3pmachg_LTM)] <- NA



###################### Inflation Adjusted GrossPPEexCIP ######################
InfladjGrPPEexCIP <- InfladjGrPPEexCIP_ANN
InfladjGrPPEexCIP[is.na(InfladjGrPPEexCIP)|is.nan(InfladjGrPPEexCIP)|!is.finite(InfladjGrPPEexCIP)] <- NA
InfladjGrPPEexCIPt <- InfladjGrPPEexCIP[,Scolfortrain:Ecolfortrain]

MAvgInfladjGrPPEexCIP <- FZEVAMAvgSKIP(input=InfladjGrPPEexCIP,maper=3,chgper=12)
MAvgInfladjGrPPEexCIP[is.na(MAvgInfladjGrPPEexCIP)|is.nan(MAvgInfladjGrPPEexCIP)|!is.finite(MAvgInfladjGrPPEexCIP)] <- NA
MAvgInfladjGrPPEexCIPt <- MAvgInfladjGrPPEexCIP[,Scolfortrain:Ecolfortrain]

ChgInfladjGrPPEexCIP <- (InfladjGrPPEexCIP/FZshiftmx(input=InfladjGrPPEexCIP, shiftcol=-12))-1
ChgInfladjGrPPEexCIP[is.na(ChgInfladjGrPPEexCIP)|is.nan(ChgInfladjGrPPEexCIP)|!is.finite(ChgInfladjGrPPEexCIP)] <- NA
ChgInfladjGrPPEexCIPt <- ChgInfladjGrPPEexCIP[,Scolfortrain:Ecolfortrain]

####
InfladjGrPPEexCIP_LTM <- InfladjGrPPEexCIP_LTM
InfladjGrPPEexCIP_LTM[is.na(InfladjGrPPEexCIP_LTM)|is.nan(InfladjGrPPEexCIP_LTM)|!is.finite(InfladjGrPPEexCIP_LTM)] <- NA

MAvgInfladjGrPPEexCIP_LTM <- FZEVAMAvgLTM(input=InfladjGrPPEexCIP_LTM,maper=3,chgper=12)
MAvgInfladjGrPPEexCIP_LTM[is.na(MAvgInfladjGrPPEexCIP_LTM)|is.nan(MAvgInfladjGrPPEexCIP_LTM)|!is.finite(MAvgInfladjGrPPEexCIP_LTM)] <- NA

ChgInfladjGrPPEexCIP_LTM <- (InfladjGrPPEexCIP_LTM/FZshiftmx(input=InfladjGrPPEexCIP_LTM, shiftcol=-12))-1
ChgInfladjGrPPEexCIP_LTM[is.na(ChgInfladjGrPPEexCIP_LTM)|is.nan(ChgInfladjGrPPEexCIP_LTM)|!is.finite(ChgInfladjGrPPEexCIP_LTM)] <- NA




###################### SGA Expense ######################
PLSGA <- PLSGA_ANN
PLSGA[is.na(PLSGA)|is.nan(PLSGA)|!is.finite(PLSGA)] <- NA
PLSGAt <- PLSGA[,Scolfortrain:Ecolfortrain]

MAvgPLSGA <- FZEVAMAvgSKIP(input=PLSGA,maper=3,chgper=12)
MAvgPLSGA[is.na(MAvgPLSGA)|is.nan(MAvgPLSGA)|!is.finite(MAvgPLSGA)] <- NA
MAvgPLSGAt <- MAvgPLSGA[,Scolfortrain:Ecolfortrain]

ChgPLSGA <- (PLSGA/FZshiftmx(input=PLSGA, shiftcol=-12))-1
ChgPLSGA[is.na(ChgPLSGA)|is.nan(ChgPLSGA)|!is.finite(ChgPLSGA)] <- NA
ChgPLSGAt <- ChgPLSGA[,Scolfortrain:Ecolfortrain]

PLSGAtrailingmax <- FZtrailingmax(input=PLSGA)
PLSGAGrowth1perchg <- PLSGA/PLSGAtrailingmax
PLSGAGrowth1perchg[is.na(PLSGAGrowth1perchg)|is.nan(PLSGAGrowth1perchg)|!is.finite(PLSGAGrowth1perchg)] <- NA
PLSGAGrowth1perchgt <- PLSGAGrowth1perchg[,Scolfortrain:Ecolfortrain]

PLSGAGrowthlastvs3perma <- PLSGA/FZEVAMAvgSKIP(input=PLSGAtrailingmax,maper=3,chgper=12)
PLSGAGrowthlastvs3perma[is.na(PLSGAGrowthlastvs3perma)|is.nan(PLSGAGrowthlastvs3perma)|!is.finite(PLSGAGrowthlastvs3perma)] <- NA
PLSGAGrowthlastvs3permat <- PLSGAGrowthlastvs3perma[,Scolfortrain:Ecolfortrain]

PLSGAGrowth3permachg <- FZEVAMAvgSKIP(input=PLSGAGrowth1perchg,maper=3,chgper=12)
PLSGAGrowth3permachg[is.na(PLSGAGrowth3permachg)|is.nan(PLSGAGrowth3permachg)|!is.finite(PLSGAGrowth3permachg)] <- NA
PLSGAGrowth3permachgt <- PLSGAGrowth3permachg[,Scolfortrain:Ecolfortrain]

PLSGAGrowth1pchgvs3pmachg <- PLSGAGrowth1perchg/PLSGAGrowth3permachg
PLSGAGrowth1pchgvs3pmachg[is.na(PLSGAGrowth1pchgvs3pmachg)|is.nan(PLSGAGrowth1pchgvs3pmachg)|!is.finite(PLSGAGrowth1pchgvs3pmachg)] <- NA
PLSGAGrowth1pchgvs3pmachgt <- PLSGAGrowth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
PLSGA_LTM[is.na(PLSGA_LTM)|is.nan(PLSGA_LTM)|!is.finite(PLSGA_LTM)] <- NA

MAvgPLSGA_LTM <- FZEVAMAvgLTM(input=PLSGA_LTM,maper=3,chgper=12)
MAvgPLSGA_LTM[is.na(MAvgPLSGA_LTM)|is.nan(MAvgPLSGA_LTM)|!is.finite(MAvgPLSGA_LTM)] <- NA

ChgPLSGA_LTM <- (PLSGA_LTM/FZshiftmx(input=PLSGA_LTM, shiftcol=-12))-1
ChgPLSGA_LTM[is.na(ChgPLSGA_LTM)|is.nan(ChgPLSGA_LTM)|!is.finite(ChgPLSGA_LTM)] <- NA

PLSGAtrailingmax_LTM <- FZtrailingmax(input=PLSGA_LTM)
PLSGAGrowth1perchg_LTM <- PLSGA_LTM/PLSGAtrailingmax_LTM
PLSGAGrowth1perchg_LTM[is.na(PLSGAGrowth1perchg_LTM)|is.nan(PLSGAGrowth1perchg_LTM)|!is.finite(PLSGAGrowth1perchg_LTM)] <- NA

PLSGAGrowthlastvs3perma_LTM <- PLSGA_LTM/FZEVAMAvgLTM(input=PLSGAtrailingmax_LTM,maper=3,chgper=12)
PLSGAGrowthlastvs3perma_LTM[is.na(PLSGAGrowthlastvs3perma_LTM)|is.nan(PLSGAGrowthlastvs3perma_LTM)|!is.finite(PLSGAGrowthlastvs3perma_LTM)] <- NA

PLSGAGrowth3permachg_LTM <- FZEVAMAvgLTM(input=PLSGAGrowth1perchg_LTM,maper=3,chgper=12)
PLSGAGrowth3permachg_LTM[is.na(PLSGAGrowth3permachg_LTM)|is.nan(PLSGAGrowth3permachg_LTM)|!is.finite(PLSGAGrowth3permachg_LTM)] <- NA

PLSGAGrowth1pchgvs3pmachg_LTM <- PLSGAGrowth1perchg_LTM/PLSGAGrowth3permachg_LTM
PLSGAGrowth1pchgvs3pmachg_LTM[is.na(PLSGAGrowth1pchgvs3pmachg_LTM)|is.nan(PLSGAGrowth1pchgvs3pmachg_LTM)|!is.finite(PLSGAGrowth1pchgvs3pmachg_LTM)] <- NA





###################### R&D Expense ######################
PLRDEXP <- PLRDEXP_ANN
PLRDEXP[is.na(PLRDEXP)|is.nan(PLRDEXP)|!is.finite(PLRDEXP)] <- NA
PLRDEXPt <- PLRDEXP[,Scolfortrain:Ecolfortrain]

MAvgPLRDEXP <- FZEVAMAvgSKIP(input=PLRDEXP,maper=3,chgper=12)
MAvgPLRDEXP[is.na(MAvgPLRDEXP)|is.nan(MAvgPLRDEXP)|!is.finite(MAvgPLRDEXP)] <- NA
MAvgPLRDEXPt <- MAvgPLRDEXP[,Scolfortrain:Ecolfortrain]

ChgPLRDEXP <- (PLRDEXP/FZshiftmx(input=PLRDEXP, shiftcol=-12))-1
ChgPLRDEXP[is.na(ChgPLRDEXP)|is.nan(ChgPLRDEXP)|!is.finite(ChgPLRDEXP)] <- NA
ChgPLRDEXPt <- ChgPLRDEXP[,Scolfortrain:Ecolfortrain]

PLRDEXPtrailingmax <- FZtrailingmax(input=PLRDEXP)
PLRDEXPGrowth1perchg <- PLRDEXP/PLRDEXPtrailingmax
PLRDEXPGrowth1perchg[is.na(PLRDEXPGrowth1perchg)|is.nan(PLRDEXPGrowth1perchg)|!is.finite(PLRDEXPGrowth1perchg)] <- NA
PLRDEXPGrowth1perchgt <- PLRDEXPGrowth1perchg[,Scolfortrain:Ecolfortrain]

PLRDEXPGrowthlastvs3perma <- PLRDEXP/FZEVAMAvgSKIP(input=PLRDEXPtrailingmax,maper=3,chgper=12)
PLRDEXPGrowthlastvs3perma[is.na(PLRDEXPGrowthlastvs3perma)|is.nan(PLRDEXPGrowthlastvs3perma)|!is.finite(PLRDEXPGrowthlastvs3perma)] <- NA
PLRDEXPGrowthlastvs3permat <- PLRDEXPGrowthlastvs3perma[,Scolfortrain:Ecolfortrain]

PLRDEXPGrowth3permachg <- FZEVAMAvgSKIP(input=PLRDEXPGrowth1perchg,maper=3,chgper=12)
PLRDEXPGrowth3permachg[is.na(PLRDEXPGrowth3permachg)|is.nan(PLRDEXPGrowth3permachg)|!is.finite(PLRDEXPGrowth3permachg)] <- NA
PLRDEXPGrowth3permachgt <- PLRDEXPGrowth3permachg[,Scolfortrain:Ecolfortrain]

PLRDEXPGrowth1pchgvs3pmachg <- PLRDEXPGrowth1perchg/PLRDEXPGrowth3permachg
PLRDEXPGrowth1pchgvs3pmachg[is.na(PLRDEXPGrowth1pchgvs3pmachg)|is.nan(PLRDEXPGrowth1pchgvs3pmachg)|!is.finite(PLRDEXPGrowth1pchgvs3pmachg)] <- NA
PLRDEXPGrowth1pchgvs3pmachgt <- PLRDEXPGrowth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
PLRDEXP_LTM[is.na(PLRDEXP_LTM)|is.nan(PLRDEXP_LTM)|!is.finite(PLRDEXP_LTM)] <- NA

MAvgPLRDEXP_LTM <- FZEVAMAvgLTM(input=PLRDEXP_LTM,maper=3,chgper=12)
MAvgPLRDEXP_LTM[is.na(MAvgPLRDEXP_LTM)|is.nan(MAvgPLRDEXP_LTM)|!is.finite(MAvgPLRDEXP_LTM)] <- NA

ChgPLRDEXP_LTM <- (PLRDEXP_LTM/FZshiftmx(input=PLRDEXP_LTM, shiftcol=-12))-1
ChgPLRDEXP_LTM[is.na(ChgPLRDEXP_LTM)|is.nan(ChgPLRDEXP_LTM)|!is.finite(ChgPLRDEXP_LTM)] <- NA

PLRDEXPtrailingmax_LTM <- FZtrailingmax(input=PLRDEXP_LTM)
PLRDEXPGrowth1perchg_LTM <- PLRDEXP_LTM/PLRDEXPtrailingmax_LTM
PLRDEXPGrowth1perchg_LTM[is.na(PLRDEXPGrowth1perchg_LTM)|is.nan(PLRDEXPGrowth1perchg_LTM)|!is.finite(PLRDEXPGrowth1perchg_LTM)] <- NA

PLRDEXPGrowthlastvs3perma_LTM <- PLRDEXP_LTM/FZEVAMAvgLTM(input=PLRDEXPtrailingmax_LTM,maper=3,chgper=12)
PLRDEXPGrowthlastvs3perma_LTM[is.na(PLRDEXPGrowthlastvs3perma)|is.nan(PLRDEXPGrowthlastvs3perma)|!is.finite(PLRDEXPGrowthlastvs3perma)] <- NA

PLRDEXPGrowth3permachg_LTM <- FZEVAMAvgLTM(input=PLRDEXPGrowth1perchg_LTM,maper=3,chgper=12)
PLRDEXPGrowth3permachg_LTM[is.na(PLRDEXPGrowth3permachg_LTM)|is.nan(PLRDEXPGrowth3permachg_LTM)|!is.finite(PLRDEXPGrowth3permachg_LTM)] <- NA

PLRDEXPGrowth1pchgvs3pmachg_LTM <- PLRDEXPGrowth1perchg_LTM/PLRDEXPGrowth3permachg_LTM
PLRDEXPGrowth1pchgvs3pmachg_LTM[is.na(PLRDEXPGrowth1pchgvs3pmachg_LTM)|is.nan(PLRDEXPGrowth1pchgvs3pmachg_LTM)|!is.finite(PLRDEXPGrowth1pchgvs3pmachg_LTM)] <- NA



###################### Accounts Payable ######################
BSACCPAY <- BSACCPAY_ANN
BSACCPAY[is.na(BSACCPAY)|is.nan(BSACCPAY)|!is.finite(BSACCPAY)] <- NA
BSACCPAYt <- BSACCPAY[,Scolfortrain:Ecolfortrain]

MAvgBSACCPAY <- FZEVAMAvgSKIP(input=BSACCPAY,maper=3,chgper=12)
MAvgBSACCPAY[is.na(MAvgBSACCPAY)|is.nan(MAvgBSACCPAY)|!is.finite(MAvgBSACCPAY)] <- NA
MAvgBSACCPAYt <- MAvgBSACCPAY[,Scolfortrain:Ecolfortrain]

ChgBSACCPAY <- (BSACCPAY/FZshiftmx(input=BSACCPAY, shiftcol=-12))-1
ChgBSACCPAY[is.na(ChgBSACCPAY)|is.nan(ChgBSACCPAY)|!is.finite(ChgBSACCPAY)] <- NA
ChgBSACCPAYt <- ChgBSACCPAY[,Scolfortrain:Ecolfortrain]

BSACCPAYtrailingmax <- FZtrailingmax(input=BSACCPAY)
BSACCPAYGrowth1perchg <- BSACCPAY/BSACCPAYtrailingmax
BSACCPAYGrowth1perchg[is.na(BSACCPAYGrowth1perchg)|is.nan(BSACCPAYGrowth1perchg)|!is.finite(BSACCPAYGrowth1perchg)] <- NA
BSACCPAYGrowth1perchgt <- BSACCPAYGrowth1perchg[,Scolfortrain:Ecolfortrain]

BSACCPAYGrowthlastvs3perma <- BSACCPAY/FZEVAMAvgSKIP(input=BSACCPAYtrailingmax,maper=3,chgper=12)
BSACCPAYGrowthlastvs3perma[is.na(BSACCPAYGrowthlastvs3perma)|is.nan(BSACCPAYGrowthlastvs3perma)|!is.finite(BSACCPAYGrowthlastvs3perma)] <- NA
BSACCPAYGrowthlastvs3permat <- BSACCPAYGrowthlastvs3perma[,Scolfortrain:Ecolfortrain]

BSACCPAYGrowth3permachg <- FZEVAMAvgSKIP(input=BSACCPAYGrowth1perchg,maper=3,chgper=12)
BSACCPAYGrowth3permachg[is.na(BSACCPAYGrowth3permachg)|is.nan(BSACCPAYGrowth3permachg)|!is.finite(BSACCPAYGrowth3permachg)] <- NA
BSACCPAYGrowth3permachgt <- BSACCPAYGrowth3permachg[,Scolfortrain:Ecolfortrain]

BSACCPAYGrowth1pchgvs3pmachg <- BSACCPAYGrowth1perchg/BSACCPAYGrowth3permachg
BSACCPAYGrowth1pchgvs3pmachg[is.na(BSACCPAYGrowth1pchgvs3pmachg)|is.nan(BSACCPAYGrowth1pchgvs3pmachg)|!is.finite(BSACCPAYGrowth1pchgvs3pmachg)] <- NA
BSACCPAYGrowth1pchgvs3pmachgt <- BSACCPAYGrowth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
BSACCPAY_LTM[is.na(BSACCPAY_LTM)|is.nan(BSACCPAY_LTM)|!is.finite(BSACCPAY_LTM)] <- NA

MAvgBSACCPAY_LTM <- FZEVAMAvgLTM(input=BSACCPAY_LTM,maper=3,chgper=12)
MAvgBSACCPAY_LTM[is.na(MAvgBSACCPAY_LTM)|is.nan(MAvgBSACCPAY_LTM)|!is.finite(MAvgBSACCPAY_LTM)] <- NA

ChgBSACCPAY_LTM <- (BSACCPAY_LTM/FZshiftmx(input=BSACCPAY_LTM, shiftcol=-12))-1
ChgBSACCPAY_LTM[is.na(ChgBSACCPAY_LTM)|is.nan(ChgBSACCPAY_LTM)|!is.finite(ChgBSACCPAY_LTM)] <- NA

BSACCPAYtrailingmax_LTM <- FZtrailingmax(input=BSACCPAY_LTM)
BSACCPAYGrowth1perchg_LTM <- BSACCPAY_LTM/BSACCPAYtrailingmax_LTM
BSACCPAYGrowth1perchg_LTM[is.na(BSACCPAYGrowth1perchg_LTM)|is.nan(BSACCPAYGrowth1perchg_LTM)|!is.finite(BSACCPAYGrowth1perchg_LTM)] <- NA

BSACCPAYGrowthlastvs3perma_LTM <- BSACCPAY_LTM/FZEVAMAvgLTM(input=BSACCPAYtrailingmax_LTM,maper=3,chgper=12)
BSACCPAYGrowthlastvs3perma_LTM[is.na(BSACCPAYGrowthlastvs3perma_LTM)|is.nan(BSACCPAYGrowthlastvs3perma_LTM)|!is.finite(BSACCPAYGrowthlastvs3perma_LTM)] <- NA

BSACCPAYGrowth3permachg_LTM <- FZEVAMAvgLTM(input=BSACCPAYGrowth1perchg_LTM,maper=3,chgper=12)
BSACCPAYGrowth3permachg_LTM[is.na(BSACCPAYGrowth3permachg_LTM)|is.nan(BSACCPAYGrowth3permachg_LTM)|!is.finite(BSACCPAYGrowth3permachg_LTM)] <- NA

BSACCPAYGrowth1pchgvs3pmachg_LTM <- BSACCPAYGrowth1perchg_LTM/BSACCPAYGrowth3permachg_LTM
BSACCPAYGrowth1pchgvs3pmachg_LTM[is.na(BSACCPAYGrowth1pchgvs3pmachg_LTM)|is.nan(BSACCPAYGrowth1pchgvs3pmachg_LTM)|!is.finite(BSACCPAYGrowth1pchgvs3pmachg_LTM)] <- NA


###################### Accounts Receivable ######################
BSACCREC <- BSACCREC_ANN
BSACCREC[is.na(BSACCREC)|is.nan(BSACCREC)|!is.finite(BSACCREC)] <- NA
BSACCRECt <- BSACCREC[,Scolfortrain:Ecolfortrain]

MAvgBSACCREC <- FZEVAMAvgSKIP(input=BSACCREC,maper=3,chgper=12)
MAvgBSACCREC[is.na(MAvgBSACCREC)|is.nan(MAvgBSACCREC)|!is.finite(MAvgBSACCREC)] <- NA
MAvgBSACCRECt <- MAvgBSACCREC[,Scolfortrain:Ecolfortrain]

ChgBSACCREC <- (BSACCREC/FZshiftmx(input=BSACCREC, shiftcol=-12))-1
ChgBSACCREC[is.na(ChgBSACCREC)|is.nan(ChgBSACCREC)|!is.finite(ChgBSACCREC)] <- NA
ChgBSACCRECt <- ChgBSACCREC[,Scolfortrain:Ecolfortrain]

BSACCRECtrailingmax <- FZtrailingmax(input=BSACCREC)
BSACCRECGrowth1perchg <- BSACCREC/BSACCRECtrailingmax
BSACCRECGrowth1perchg[is.na(BSACCRECGrowth1perchg)|is.nan(BSACCRECGrowth1perchg)|!is.finite(BSACCRECGrowth1perchg)] <- NA
BSACCRECGrowth1perchgt <- BSACCRECGrowth1perchg[,Scolfortrain:Ecolfortrain]

BSACCRECGrowthlastvs3perma <- BSACCREC/FZEVAMAvgSKIP(input=BSACCRECtrailingmax,maper=3,chgper=12)
BSACCRECGrowthlastvs3perma[is.na(BSACCRECGrowthlastvs3perma)|is.nan(BSACCRECGrowthlastvs3perma)|!is.finite(BSACCRECGrowthlastvs3perma)] <- NA
BSACCRECGrowthlastvs3permat <- BSACCRECGrowthlastvs3perma[,Scolfortrain:Ecolfortrain]

BSACCRECGrowth3permachg <- FZEVAMAvgSKIP(input=BSACCRECGrowth1perchg,maper=3,chgper=12)
BSACCRECGrowth3permachg[is.na(BSACCRECGrowth3permachg)|is.nan(BSACCRECGrowth3permachg)|!is.finite(BSACCRECGrowth3permachg)] <- NA
BSACCRECGrowth3permachgt <- BSACCRECGrowth3permachg[,Scolfortrain:Ecolfortrain]

BSACCRECGrowth1pchgvs3pmachg <- BSACCRECGrowth1perchg/BSACCRECGrowth3permachg
BSACCRECGrowth1pchgvs3pmachg[is.na(BSACCRECGrowth1pchgvs3pmachg)|is.nan(BSACCRECGrowth1pchgvs3pmachg)|!is.finite(BSACCRECGrowth1pchgvs3pmachg)] <- NA
BSACCRECGrowth1pchgvs3pmachgt <- BSACCRECGrowth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
BSACCREC_LTM[is.na(BSACCREC_LTM)|is.nan(BSACCREC_LTM)|!is.finite(BSACCREC_LTM)] <- NA

MAvgBSACCREC_LTM <- FZEVAMAvgLTM(input=BSACCREC_LTM,maper=3,chgper=12)
MAvgBSACCREC_LTM[is.na(MAvgBSACCREC_LTM)|is.nan(MAvgBSACCREC_LTM)|!is.finite(MAvgBSACCREC_LTM)] <- NA

ChgBSACCREC_LTM <- (BSACCREC_LTM/FZshiftmx(input=BSACCREC_LTM, shiftcol=-12))-1
ChgBSACCREC_LTM[is.na(ChgBSACCREC_LTM)|is.nan(ChgBSACCREC_LTM)|!is.finite(ChgBSACCREC_LTM)] <- NA

BSACCRECtrailingmax_LTM <- FZtrailingmax(input=BSACCREC_LTM)
BSACCRECGrowth1perchg_LTM <- BSACCREC_LTM/BSACCRECtrailingmax_LTM
BSACCRECGrowth1perchg_LTM[is.na(BSACCRECGrowth1perchg_LTM)|is.nan(BSACCRECGrowth1perchg_LTM)|!is.finite(BSACCRECGrowth1perchg_LTM)] <- NA

BSACCRECGrowthlastvs3perma_LTM <- BSACCREC_LTM/FZEVAMAvgLTM(input=BSACCRECtrailingmax_LTM,maper=3,chgper=12)
BSACCRECGrowthlastvs3perma_LTM[is.na(BSACCRECGrowthlastvs3perma_LTM)|is.nan(BSACCRECGrowthlastvs3perma_LTM)|!is.finite(BSACCRECGrowthlastvs3perma_LTM)] <- NA

BSACCRECGrowth3permachg_LTM <- FZEVAMAvgLTM(input=BSACCRECGrowth1perchg_LTM,maper=3,chgper=12)
BSACCRECGrowth3permachg_LTM[is.na(BSACCRECGrowth3permachg_LTM)|is.nan(BSACCRECGrowth3permachg_LTM)|!is.finite(BSACCRECGrowth3permachg_LTM)] <- NA

BSACCRECGrowth1pchgvs3pmachg_LTM <- BSACCRECGrowth1perchg_LTM/BSACCRECGrowth3permachg_LTM
BSACCRECGrowth1pchgvs3pmachg_LTM[is.na(BSACCRECGrowth1pchgvs3pmachg_LTM)|is.nan(BSACCRECGrowth1pchgvs3pmachg_LTM)|!is.finite(BSACCRECGrowth1pchgvs3pmachg_LTM)] <- NA





###################### CIP ######################
BSPPEGROSSCONSTR <- BSPPEGROSSCONSTR_ANN
BSPPEGROSSCONSTR[is.na(BSPPEGROSSCONSTR)|is.nan(BSPPEGROSSCONSTR)|!is.finite(BSPPEGROSSCONSTR)] <- NA
BSPPEGROSSCONSTRt <- BSPPEGROSSCONSTR[,Scolfortrain:Ecolfortrain]

MAvgBSPPEGROSSCONSTR <- FZEVAMAvgSKIP(input=BSPPEGROSSCONSTR,maper=3,chgper=12)
MAvgBSPPEGROSSCONSTR[is.na(MAvgBSPPEGROSSCONSTR)|is.nan(MAvgBSPPEGROSSCONSTR)|!is.finite(MAvgBSPPEGROSSCONSTR)] <- NA
MAvgBSPPEGROSSCONSTRt <- MAvgBSPPEGROSSCONSTR[,Scolfortrain:Ecolfortrain]

ChgBSPPEGROSSCONSTR <- (BSPPEGROSSCONSTR/FZshiftmx(input=BSPPEGROSSCONSTR, shiftcol=-12))-1
ChgBSPPEGROSSCONSTR[is.na(ChgBSPPEGROSSCONSTR)|is.nan(ChgBSPPEGROSSCONSTR)|!is.finite(ChgBSPPEGROSSCONSTR)] <- NA
ChgBSPPEGROSSCONSTRt <- ChgBSPPEGROSSCONSTR[,Scolfortrain:Ecolfortrain]

BSPPEGROSSCONSTRtrailingmax <- FZtrailingmax(input=BSPPEGROSSCONSTR)
BSPPEGROSSCONSTRGrowth1perchg <- BSPPEGROSSCONSTR/BSPPEGROSSCONSTRtrailingmax
BSPPEGROSSCONSTRGrowth1perchg[is.na(BSPPEGROSSCONSTRGrowth1perchg)|is.nan(BSPPEGROSSCONSTRGrowth1perchg)|!is.finite(BSPPEGROSSCONSTRGrowth1perchg)] <- NA
BSPPEGROSSCONSTRGrowth1perchgt <- BSPPEGROSSCONSTRGrowth1perchg[,Scolfortrain:Ecolfortrain]

BSPPEGROSSCONSTRGrowthlastvs3perma <- BSPPEGROSSCONSTR/FZEVAMAvgSKIP(input=BSPPEGROSSCONSTRtrailingmax,maper=3,chgper=12)
BSPPEGROSSCONSTRGrowthlastvs3perma[is.na(BSPPEGROSSCONSTRGrowthlastvs3perma)|is.nan(BSPPEGROSSCONSTRGrowthlastvs3perma)|!is.finite(BSPPEGROSSCONSTRGrowthlastvs3perma)] <- NA
BSPPEGROSSCONSTRGrowthlastvs3permat <- BSPPEGROSSCONSTRGrowthlastvs3perma[,Scolfortrain:Ecolfortrain]

BSPPEGROSSCONSTRGrowth3permachg <- FZEVAMAvgSKIP(input=BSPPEGROSSCONSTRGrowth1perchg,maper=3,chgper=12)
BSPPEGROSSCONSTRGrowth3permachg[is.na(BSPPEGROSSCONSTRGrowth3permachg)|is.nan(BSPPEGROSSCONSTRGrowth3permachg)|!is.finite(BSPPEGROSSCONSTRGrowth3permachg)] <- NA
BSPPEGROSSCONSTRGrowth3permachgt <- BSPPEGROSSCONSTRGrowth3permachg[,Scolfortrain:Ecolfortrain]

BSPPEGROSSCONSTRGrowth1pchgvs3pmachg <- BSPPEGROSSCONSTRGrowth1perchg/BSPPEGROSSCONSTRGrowth3permachg
BSPPEGROSSCONSTRGrowth1pchgvs3pmachg[is.na(BSPPEGROSSCONSTRGrowth1pchgvs3pmachg)|is.nan(BSPPEGROSSCONSTRGrowth1pchgvs3pmachg)|!is.finite(BSPPEGROSSCONSTRGrowth1pchgvs3pmachg)] <- NA
BSPPEGROSSCONSTRGrowth1pchgvs3pmachgt <- BSPPEGROSSCONSTRGrowth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
BSPPEGROSSCONSTR_LTM[is.na(BSPPEGROSSCONSTR_LTM)|is.nan(BSPPEGROSSCONSTR_LTM)|!is.finite(BSPPEGROSSCONSTR_LTM)] <- NA

MAvgBSPPEGROSSCONSTR_LTM <- FZEVAMAvgLTM(input=BSPPEGROSSCONSTR_LTM,maper=3,chgper=12)
MAvgBSPPEGROSSCONSTR_LTM[is.na(MAvgBSPPEGROSSCONSTR_LTM)|is.nan(MAvgBSPPEGROSSCONSTR_LTM)|!is.finite(MAvgBSPPEGROSSCONSTR_LTM)] <- NA

ChgBSPPEGROSSCONSTR_LTM <- (BSPPEGROSSCONSTR_LTM/FZshiftmx(input=BSPPEGROSSCONSTR_LTM, shiftcol=-12))-1
ChgBSPPEGROSSCONSTR_LTM[is.na(ChgBSPPEGROSSCONSTR_LTM)|is.nan(ChgBSPPEGROSSCONSTR_LTM)|!is.finite(ChgBSPPEGROSSCONSTR_LTM)] <- NA

BSPPEGROSSCONSTRtrailingmax_LTM <- FZtrailingmax(input=BSPPEGROSSCONSTR_LTM)
BSPPEGROSSCONSTRGrowth1perchg_LTM <- BSPPEGROSSCONSTR_LTM/BSPPEGROSSCONSTRtrailingmax_LTM
BSPPEGROSSCONSTRGrowth1perchg_LTM[is.na(BSPPEGROSSCONSTRGrowth1perchg_LTM)|is.nan(BSPPEGROSSCONSTRGrowth1perchg_LTM)|!is.finite(BSPPEGROSSCONSTRGrowth1perchg_LTM)] <- NA

BSPPEGROSSCONSTRGrowthlastvs3perma_LTM <- BSPPEGROSSCONSTR_LTM/FZEVAMAvgLTM(input=BSPPEGROSSCONSTRtrailingmax_LTM,maper=3,chgper=12)
BSPPEGROSSCONSTRGrowthlastvs3perma_LTM[is.na(BSPPEGROSSCONSTRGrowthlastvs3perma_LTM)|is.nan(BSPPEGROSSCONSTRGrowthlastvs3perma_LTM)|!is.finite(BSPPEGROSSCONSTRGrowthlastvs3perma_LTM)] <- NA

BSPPEGROSSCONSTRGrowth3permachg_LTM <- FZEVAMAvgLTM(input=BSPPEGROSSCONSTRGrowth1perchg_LTM,maper=3,chgper=12)
BSPPEGROSSCONSTRGrowth3permachg_LTM[is.na(BSPPEGROSSCONSTRGrowth3permachg_LTM)|is.nan(BSPPEGROSSCONSTRGrowth3permachg_LTM)|!is.finite(BSPPEGROSSCONSTRGrowth3permachg_LTM)] <- NA

BSPPEGROSSCONSTRGrowth1pchgvs3pmachg_LTM <- BSPPEGROSSCONSTRGrowth1perchg_LTM/BSPPEGROSSCONSTRGrowth3permachg_LTM
BSPPEGROSSCONSTRGrowth1pchgvs3pmachg_LTM[is.na(BSPPEGROSSCONSTRGrowth1pchgvs3pmachg_LTM)|is.nan(BSPPEGROSSCONSTRGrowth1pchgvs3pmachg_LTM)|!is.finite(BSPPEGROSSCONSTRGrowth1pchgvs3pmachg_LTM)] <- NA



###################### P/S ######################
PricetoSalesfull <- (MCAP_ANN/PLSALES_ANN)

PricetoSalesfull[is.na(PricetoSalesfull)|is.nan(PricetoSalesfull)|!is.finite(PricetoSalesfull)] <- NA
PricetoSalesfullt <- PricetoSalesfull[,Scolfortrain:Ecolfortrain]

MAvgPricetoSalesfull <- FZEVAMAvgSKIP(input=PricetoSalesfull,maper=3,chgper=12)
MAvgPricetoSalesfull[is.na(MAvgPricetoSalesfull)|is.nan(MAvgPricetoSalesfull)|!is.finite(MAvgPricetoSalesfull)] <- NA
MAvgPricetoSalesfullt <- MAvgPricetoSalesfull[,Scolfortrain:Ecolfortrain]

ChgPricetoSalesfull <- (PricetoSalesfull/FZshiftmx(input=PricetoSalesfull, shiftcol=-12))-1
ChgPricetoSalesfull[is.na(ChgPricetoSalesfull)|is.nan(ChgPricetoSalesfull)|!is.finite(ChgPricetoSalesfull)] <- NA
ChgPricetoSalesfullt <- ChgPricetoSalesfull[,Scolfortrain:Ecolfortrain]

PricetoSalesfulltrailingmax <- FZtrailingmax(input=PricetoSalesfull)
PricetoSalesfullGrowth1perchg <- PricetoSalesfull/PricetoSalesfulltrailingmax
PricetoSalesfullGrowth1perchg[is.na(PricetoSalesfullGrowth1perchg)|is.nan(PricetoSalesfullGrowth1perchg)|!is.finite(PricetoSalesfullGrowth1perchg)] <- NA
PricetoSalesfullGrowth1perchgt <- PricetoSalesfullGrowth1perchg[,Scolfortrain:Ecolfortrain]

PricetoSalesfullGrowthlastvs3perma <- PricetoSalesfull/FZEVAMAvgSKIP(input=PricetoSalesfulltrailingmax,maper=3,chgper=12)
PricetoSalesfullGrowthlastvs3perma[is.na(PricetoSalesfullGrowthlastvs3perma)|is.nan(PricetoSalesfullGrowthlastvs3perma)|!is.finite(PricetoSalesfullGrowthlastvs3perma)] <- NA
PricetoSalesfullGrowthlastvs3permat <- PricetoSalesfullGrowthlastvs3perma[,Scolfortrain:Ecolfortrain]

PricetoSalesfullGrowth3permachg <- FZEVAMAvgSKIP(input=PricetoSalesfullGrowth1perchg,maper=3,chgper=12)
PricetoSalesfullGrowth3permachg[is.na(PricetoSalesfullGrowth3permachg)|is.nan(PricetoSalesfullGrowth3permachg)|!is.finite(PricetoSalesfullGrowth3permachg)] <- NA
PricetoSalesfullGrowth3permachgt <- PricetoSalesfullGrowth3permachg[,Scolfortrain:Ecolfortrain]

PricetoSalesfullGrowth1pchgvs3pmachg <- PricetoSalesfullGrowth1perchg/PricetoSalesfullGrowth3permachg
PricetoSalesfullGrowth1pchgvs3pmachg[is.na(PricetoSalesfullGrowth1pchgvs3pmachg)|is.nan(PricetoSalesfullGrowth1pchgvs3pmachg)|!is.finite(PricetoSalesfullGrowth1pchgvs3pmachg)] <- NA
PricetoSalesfullGrowth1pchgvs3pmachgt <- PricetoSalesfullGrowth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
PricetoSalesfull_LTM <- (MCAP_LTM/PLSALES_LTM)
PricetoSalesfull_LTM[is.na(PricetoSalesfull_LTM)|is.nan(PricetoSalesfull_LTM)|!is.finite(PricetoSalesfull_LTM)] <- NA

MAvgPricetoSalesfull_LTM <- FZEVAMAvgLTM(input=PricetoSalesfull_LTM,maper=3,chgper=12)
MAvgPricetoSalesfull_LTM[is.na(MAvgPricetoSalesfull_LTM)|is.nan(MAvgPricetoSalesfull_LTM)|!is.finite(MAvgPricetoSalesfull_LTM)] <- NA

ChgPricetoSalesfull_LTM <- (PricetoSalesfull_LTM/FZshiftmx(input=PricetoSalesfull_LTM, shiftcol=-12))-1
ChgPricetoSalesfull_LTM[is.na(ChgPricetoSalesfull_LTM)|is.nan(ChgPricetoSalesfull_LTM)|!is.finite(ChgPricetoSalesfull_LTM)] <- NA

PricetoSalesfulltrailingmax_LTM <- FZtrailingmax(input=PricetoSalesfull_LTM)
PricetoSalesfullGrowth1perchg_LTM <- PricetoSalesfull_LTM/PricetoSalesfulltrailingmax_LTM
PricetoSalesfullGrowth1perchg_LTM[is.na(PricetoSalesfullGrowth1perchg_LTM)|is.nan(PricetoSalesfullGrowth1perchg_LTM)|!is.finite(PricetoSalesfullGrowth1perchg_LTM)] <- NA

PricetoSalesfullGrowthlastvs3perma_LTM <- PricetoSalesfull_LTM/FZEVAMAvgLTM(input=PricetoSalesfulltrailingmax_LTM,maper=3,chgper=12)
PricetoSalesfullGrowthlastvs3perma_LTM[is.na(PricetoSalesfullGrowthlastvs3perma_LTM)|is.nan(PricetoSalesfullGrowthlastvs3perma_LTM)|!is.finite(PricetoSalesfullGrowthlastvs3perma_LTM)] <- NA

PricetoSalesfullGrowth3permachg_LTM <- FZEVAMAvgLTM(input=PricetoSalesfullGrowth1perchg_LTM,maper=3,chgper=12)
PricetoSalesfullGrowth3permachg_LTM[is.na(PricetoSalesfullGrowth3permachg_LTM)|is.nan(PricetoSalesfullGrowth3permachg_LTM)|!is.finite(PricetoSalesfullGrowth3permachg_LTM)] <- NA

PricetoSalesfullGrowth1pchgvs3pmachg_LTM <- PricetoSalesfullGrowth1perchg_LTM/PricetoSalesfullGrowth3permachg_LTM
PricetoSalesfullGrowth1pchgvs3pmachg_LTM[is.na(PricetoSalesfullGrowth1pchgvs3pmachg_LTM)|is.nan(PricetoSalesfullGrowth1pchgvs3pmachg_LTM)|!is.finite(PricetoSalesfullGrowth1pchgvs3pmachg_LTM)] <- NA



###################### ROE*RR ######################
ROEnRRfull <- growthE_ANN
ROEnRRfull[is.na(ROEnRRfull)|is.nan(ROEnRRfull)|!is.finite(ROEnRRfull)] <- NA
ROEnRRfullt <- ROEnRRfull[,Scolfortrain:Ecolfortrain]

MAvgROEnRRfull <- FZEVAMAvgSKIP(input=ROEnRRfull,maper=3,chgper=12)
MAvgROEnRRfull[is.na(MAvgROEnRRfull)|is.nan(MAvgROEnRRfull)|!is.finite(MAvgROEnRRfull)] <- NA
MAvgROEnRRfullt <- MAvgROEnRRfull[,Scolfortrain:Ecolfortrain]

ChgROEnRRfull <- (ROEnRRfull/FZshiftmx(input=ROEnRRfull, shiftcol=-12))-1
ChgROEnRRfull[is.na(ChgROEnRRfull)|is.nan(ChgROEnRRfull)|!is.finite(ChgROEnRRfull)] <- NA
ChgROEnRRfullt <- ChgROEnRRfull[,Scolfortrain:Ecolfortrain]

ROEnRRfulltrailingmax <- FZtrailingmax(input=ROEnRRfull)
ROEnRRfullGrowth1perchg <- ROEnRRfull/ROEnRRfulltrailingmax
ROEnRRfullGrowth1perchg[is.na(ROEnRRfullGrowth1perchg)|is.nan(ROEnRRfullGrowth1perchg)|!is.finite(ROEnRRfullGrowth1perchg)] <- NA
ROEnRRfullGrowth1perchgt <- ROEnRRfullGrowth1perchg[,Scolfortrain:Ecolfortrain]

ROEnRRfullGrowthlastvs3perma <- ROEnRRfull/FZEVAMAvgSKIP(input=ROEnRRfulltrailingmax,maper=3,chgper=12)
ROEnRRfullGrowthlastvs3perma[is.na(ROEnRRfullGrowthlastvs3perma)|is.nan(ROEnRRfullGrowthlastvs3perma)|!is.finite(ROEnRRfullGrowthlastvs3perma)] <- NA
ROEnRRfullGrowthlastvs3permat <- ROEnRRfullGrowthlastvs3perma[,Scolfortrain:Ecolfortrain]

ROEnRRfullGrowth3permachg <- FZEVAMAvgSKIP(input=ROEnRRfullGrowth1perchg,maper=3,chgper=12)
ROEnRRfullGrowth3permachg[is.na(ROEnRRfullGrowth3permachg)|is.nan(ROEnRRfullGrowth3permachg)|!is.finite(ROEnRRfullGrowth3permachg)] <- NA
ROEnRRfullGrowth3permachgt <- ROEnRRfullGrowth3permachg[,Scolfortrain:Ecolfortrain]

ROEnRRfullGrowth1pchgvs3pmachg <- ROEnRRfullGrowth1perchg/ROEnRRfullGrowth3permachg
ROEnRRfullGrowth1pchgvs3pmachg[is.na(ROEnRRfullGrowth1pchgvs3pmachg)|is.nan(ROEnRRfullGrowth1pchgvs3pmachg)|!is.finite(ROEnRRfullGrowth1pchgvs3pmachg)] <- NA
ROEnRRfullGrowth1pchgvs3pmachgt <- ROEnRRfullGrowth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
ROEnRRfull_LTM <- growthE_LTM
ROEnRRfull_LTM[is.na(ROEnRRfull_LTM)|is.nan(ROEnRRfull_LTM)|!is.finite(ROEnRRfull_LTM)] <- NA

MAvgROEnRRfull_LTM <- FZEVAMAvgLTM(input=ROEnRRfull_LTM,maper=3,chgper=12)
MAvgROEnRRfull_LTM[is.na(MAvgROEnRRfull_LTM)|is.nan(MAvgROEnRRfull_LTM)|!is.finite(MAvgROEnRRfull_LTM)] <- NA

ChgROEnRRfull_LTM <- (ROEnRRfull_LTM/FZshiftmx(input=ROEnRRfull_LTM, shiftcol=-12))-1
ChgROEnRRfull_LTM[is.na(ChgROEnRRfull_LTM)|is.nan(ChgROEnRRfull_LTM)|!is.finite(ChgROEnRRfull_LTM)] <- NA

ROEnRRfulltrailingmax_LTM <- FZtrailingmax(input=ROEnRRfull_LTM)
ROEnRRfullGrowth1perchg_LTM <- ROEnRRfull_LTM/ROEnRRfulltrailingmax_LTM
ROEnRRfullGrowth1perchg_LTM[is.na(ROEnRRfullGrowth1perchg_LTM)|is.nan(ROEnRRfullGrowth1perchg_LTM)|!is.finite(ROEnRRfullGrowth1perchg_LTM)] <- NA

ROEnRRfullGrowthlastvs3perma_LTM <- ROEnRRfull_LTM/FZEVAMAvgLTM(input=ROEnRRfulltrailingmax_LTM,maper=3,chgper=12)
ROEnRRfullGrowthlastvs3perma_LTM[is.na(ROEnRRfullGrowthlastvs3perma_LTM)|is.nan(ROEnRRfullGrowthlastvs3perma_LTM)|!is.finite(ROEnRRfullGrowthlastvs3perma_LTM)] <- NA

ROEnRRfullGrowth3permachg_LTM <- FZEVAMAvgLTM(input=ROEnRRfullGrowth1perchg_LTM,maper=3,chgper=12)
ROEnRRfullGrowth3permachg_LTM[is.na(ROEnRRfullGrowth3permachg_LTM)|is.nan(ROEnRRfullGrowth3permachg_LTM)|!is.finite(ROEnRRfullGrowth3permachg_LTM)] <- NA

ROEnRRfullGrowth1pchgvs3pmachg_LTM <- ROEnRRfullGrowth1perchg_LTM/ROEnRRfullGrowth3permachg_LTM
ROEnRRfullGrowth1pchgvs3pmachg_LTM[is.na(ROEnRRfullGrowth1pchgvs3pmachg_LTM)|is.nan(ROEnRRfullGrowth1pchgvs3pmachg_LTM)|!is.finite(ROEnRRfullGrowth1pchgvs3pmachg_LTM)] <- NA



###################### Operating Margin ######################
OpMarginfull <- (PLEBITOPER_ANN/PLSALES_ANN)
OpMarginfull[is.na(OpMarginfull)|is.nan(OpMarginfull)|!is.finite(OpMarginfull)] <- NA
OpMarginfullt <- OpMarginfull[,Scolfortrain:Ecolfortrain]

MAvgOpMarginfull <- FZEVAMAvgSKIP(input=OpMarginfull,maper=3,chgper=12)
MAvgOpMarginfull[is.na(MAvgOpMarginfull)|is.nan(MAvgOpMarginfull)|!is.finite(MAvgOpMarginfull)] <- NA
MAvgOpMarginfullt <- MAvgOpMarginfull[,Scolfortrain:Ecolfortrain]

ChgOpMarginfull <- (OpMarginfull/FZshiftmx(input=OpMarginfull, shiftcol=-12))-1
ChgOpMarginfull[is.na(ChgOpMarginfull)|is.nan(ChgOpMarginfull)|!is.finite(ChgOpMarginfull)] <- NA
ChgOpMarginfullt <- ChgOpMarginfull[,Scolfortrain:Ecolfortrain]

OpMarginfulltrailingmax <- FZtrailingmax(input=OpMarginfull)
OpMarginfullGrowth1perchg <- OpMarginfull/OpMarginfulltrailingmax
OpMarginfullGrowth1perchg[is.na(OpMarginfullGrowth1perchg)|is.nan(OpMarginfullGrowth1perchg)|!is.finite(OpMarginfullGrowth1perchg)] <- NA
OpMarginfullGrowth1perchgt <- OpMarginfullGrowth1perchg[,Scolfortrain:Ecolfortrain]

OpMarginfullGrowthlastvs3perma <- OpMarginfull/FZEVAMAvgSKIP(input=OpMarginfulltrailingmax,maper=3,chgper=12)
OpMarginfullGrowthlastvs3perma[is.na(OpMarginfullGrowthlastvs3perma)|is.nan(OpMarginfullGrowthlastvs3perma)|!is.finite(OpMarginfullGrowthlastvs3perma)] <- NA
OpMarginfullGrowthlastvs3permat <- OpMarginfullGrowthlastvs3perma[,Scolfortrain:Ecolfortrain]

OpMarginfullGrowth3permachg <- FZEVAMAvgSKIP(input=OpMarginfullGrowth1perchg,maper=3,chgper=12)
OpMarginfullGrowth3permachg[is.na(OpMarginfullGrowth3permachg)|is.nan(OpMarginfullGrowth3permachg)|!is.finite(OpMarginfullGrowth3permachg)] <- NA
OpMarginfullGrowth3permachgt <- OpMarginfullGrowth3permachg[,Scolfortrain:Ecolfortrain]

OpMarginfullGrowth1pchgvs3pmachg <- OpMarginfullGrowth1perchg/OpMarginfullGrowth3permachg
OpMarginfullGrowth1pchgvs3pmachg[is.na(OpMarginfullGrowth1pchgvs3pmachg)|is.nan(OpMarginfullGrowth1pchgvs3pmachg)|!is.finite(OpMarginfullGrowth1pchgvs3pmachg)] <- NA
OpMarginfullGrowth1pchgvs3pmachgt <- OpMarginfullGrowth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
OpMarginfull_LTM <- (PLEBITOPER_LTM/PLSALES_LTM)
OpMarginfull_LTM[is.na(OpMarginfull_LTM)|is.nan(OpMarginfull_LTM)|!is.finite(OpMarginfull_LTM)] <- NA

MAvgOpMarginfull_LTM <- FZEVAMAvgLTM(input=OpMarginfull_LTM,maper=3,chgper=12)
MAvgOpMarginfull_LTM[is.na(MAvgOpMarginfull_LTM)|is.nan(MAvgOpMarginfull_LTM)|!is.finite(MAvgOpMarginfull_LTM)] <- NA

ChgOpMarginfull_LTM <- (OpMarginfull_LTM/FZshiftmx(input=OpMarginfull_LTM, shiftcol=-12))-1
ChgOpMarginfull_LTM[is.na(ChgOpMarginfull_LTM)|is.nan(ChgOpMarginfull_LTM)|!is.finite(ChgOpMarginfull_LTM)] <- NA

OpMarginfulltrailingmax_LTM <- FZtrailingmax(input=OpMarginfull_LTM)
OpMarginfullGrowth1perchg_LTM <- OpMarginfull_LTM/OpMarginfulltrailingmax_LTM
OpMarginfullGrowth1perchg_LTM[is.na(OpMarginfullGrowth1perchg_LTM)|is.nan(OpMarginfullGrowth1perchg_LTM)|!is.finite(OpMarginfullGrowth1perchg_LTM)] <- NA

OpMarginfullGrowthlastvs3perma_LTM <- OpMarginfull_LTM/FZEVAMAvgLTM(input=OpMarginfulltrailingmax_LTM,maper=3,chgper=12)
OpMarginfullGrowthlastvs3perma_LTM[is.na(OpMarginfullGrowthlastvs3perma_LTM)|is.nan(OpMarginfullGrowthlastvs3perma_LTM)|!is.finite(OpMarginfullGrowthlastvs3perma_LTM)] <- NA

OpMarginfullGrowth3permachg_LTM <- FZEVAMAvgLTM(input=OpMarginfullGrowth1perchg_LTM,maper=3,chgper=12)
OpMarginfullGrowth3permachg_LTM[is.na(OpMarginfullGrowth3permachg_LTM)|is.nan(OpMarginfullGrowth3permachg_LTM)|!is.finite(OpMarginfullGrowth3permachg_LTM)] <- NA

OpMarginfullGrowth1pchgvs3pmachg_LTM <- OpMarginfullGrowth1perchg_LTM/OpMarginfullGrowth3permachg_LTM
OpMarginfullGrowth1pchgvs3pmachg_LTM[is.na(OpMarginfullGrowth1pchgvs3pmachg_LTM)|is.nan(OpMarginfullGrowth1pchgvs3pmachg_LTM)|!is.finite(OpMarginfullGrowth1pchgvs3pmachg_LTM)] <- NA



###################### Net Margin ######################
netmarginfull <- (PLNETINC_ANN/PLSALES_ANN)
netmarginfull[is.na(netmarginfull)|is.nan(netmarginfull)|!is.finite(netmarginfull)] <- NA
netmarginfullt <- netmarginfull[,Scolfortrain:Ecolfortrain]

MAvgnetmarginfull <- FZEVAMAvgSKIP(input=netmarginfull,maper=3,chgper=12)
MAvgnetmarginfull[is.na(MAvgnetmarginfull)|is.nan(MAvgnetmarginfull)|!is.finite(MAvgnetmarginfull)] <- NA
MAvgnetmarginfullt <- MAvgnetmarginfull[,Scolfortrain:Ecolfortrain]

Chgnetmarginfull <- (netmarginfull/FZshiftmx(input=netmarginfull, shiftcol=-12))-1
Chgnetmarginfull[is.na(Chgnetmarginfull)|is.nan(Chgnetmarginfull)|!is.finite(Chgnetmarginfull)] <- NA
Chgnetmarginfullt <- Chgnetmarginfull[,Scolfortrain:Ecolfortrain]

netmarginfulltrailingmax <- FZtrailingmax(input=netmarginfull)
netmarginfullGrowth1perchg <- netmarginfull/netmarginfulltrailingmax
netmarginfullGrowth1perchg[is.na(netmarginfullGrowth1perchg)|is.nan(netmarginfullGrowth1perchg)|!is.finite(netmarginfullGrowth1perchg)] <- NA
netmarginfullGrowth1perchgt <- netmarginfullGrowth1perchg[,Scolfortrain:Ecolfortrain]

netmarginfullGrowthlastvs3perma <- netmarginfull/FZEVAMAvgSKIP(input=netmarginfulltrailingmax,maper=3,chgper=12)
netmarginfullGrowthlastvs3perma[is.na(netmarginfullGrowthlastvs3perma)|is.nan(netmarginfullGrowthlastvs3perma)|!is.finite(netmarginfullGrowthlastvs3perma)] <- NA
netmarginfullGrowthlastvs3permat <- netmarginfullGrowthlastvs3perma[,Scolfortrain:Ecolfortrain]

netmarginfullGrowth3permachg <- FZEVAMAvgSKIP(input=netmarginfullGrowth1perchg,maper=3,chgper=12)
netmarginfullGrowth3permachg[is.na(netmarginfullGrowth3permachg)|is.nan(netmarginfullGrowth3permachg)|!is.finite(netmarginfullGrowth3permachg)] <- NA
netmarginfullGrowth3permachgt <- netmarginfullGrowth3permachg[,Scolfortrain:Ecolfortrain]

netmarginfullGrowth1pchgvs3pmachg <- netmarginfullGrowth1perchg/netmarginfullGrowth3permachg
netmarginfullGrowth1pchgvs3pmachg[is.na(netmarginfullGrowth1pchgvs3pmachg)|is.nan(netmarginfullGrowth1pchgvs3pmachg)|!is.finite(netmarginfullGrowth1pchgvs3pmachg)] <- NA
netmarginfullGrowth1pchgvs3pmachgt <- netmarginfullGrowth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
netmarginfull_LTM <- (PLNETINC_LTM/PLSALES_LTM)
netmarginfull_LTM[is.na(netmarginfull_LTM)|is.nan(netmarginfull_LTM)|!is.finite(netmarginfull_LTM)] <- NA

MAvgnetmarginfull_LTM <- FZEVAMAvgLTM(input=netmarginfull_LTM,maper=3,chgper=12)
MAvgnetmarginfull_LTM[is.na(MAvgnetmarginfull_LTM)|is.nan(MAvgnetmarginfull_LTM)|!is.finite(MAvgnetmarginfull_LTM)] <- NA

Chgnetmarginfull_LTM <- (netmarginfull_LTM/FZshiftmx(input=netmarginfull_LTM, shiftcol=-12))-1
Chgnetmarginfull_LTM[is.na(Chgnetmarginfull_LTM)|is.nan(Chgnetmarginfull_LTM)|!is.finite(Chgnetmarginfull_LTM)] <- NA

netmarginfulltrailingmax_LTM <- FZtrailingmax(input=netmarginfull_LTM)
netmarginfullGrowth1perchg_LTM <- netmarginfull_LTM/netmarginfulltrailingmax_LTM
netmarginfullGrowth1perchg_LTM[is.na(netmarginfullGrowth1perchg_LTM)|is.nan(netmarginfullGrowth1perchg_LTM)|!is.finite(netmarginfullGrowth1perchg_LTM)] <- NA

netmarginfullGrowthlastvs3perma_LTM <- netmarginfull_LTM/FZEVAMAvgLTM(input=netmarginfulltrailingmax_LTM,maper=3,chgper=12)
netmarginfullGrowthlastvs3perma_LTM[is.na(netmarginfullGrowthlastvs3perma_LTM)|is.nan(netmarginfullGrowthlastvs3perma_LTM)|!is.finite(netmarginfullGrowthlastvs3perma_LTM)] <- NA

netmarginfullGrowth3permachg_LTM <- FZEVAMAvgLTM(input=netmarginfullGrowth1perchg_LTM,maper=3,chgper=12)
netmarginfullGrowth3permachg_LTM[is.na(netmarginfullGrowth3permachg_LTM)|is.nan(netmarginfullGrowth3permachg_LTM)|!is.finite(netmarginfullGrowth3permachg_LTM)] <- NA

netmarginfullGrowth1pchgvs3pmachg_LTM <- netmarginfullGrowth1perchg_LTM/netmarginfullGrowth3permachg_LTM
netmarginfullGrowth1pchgvs3pmachg_LTM[is.na(netmarginfullGrowth1pchgvs3pmachg_LTM)|is.nan(netmarginfullGrowth1pchgvs3pmachg_LTM)|!is.finite(netmarginfullGrowth1pchgvs3pmachg_LTM)] <- NA



###################### Intangible ######################
BSINTANGIBLE <- BSINTANGIBLE_ANN
BSINTANGIBLE[is.na(BSINTANGIBLE)|is.nan(BSINTANGIBLE)|!is.finite(BSINTANGIBLE)] <- NA
BSINTANGIBLEt <- BSINTANGIBLE[,Scolfortrain:Ecolfortrain]

MAvgBSINTANGIBLE <- FZEVAMAvgSKIP(input=BSACCREC,maper=3,chgper=12)
MAvgBSINTANGIBLE[is.na(MAvgBSINTANGIBLE)|is.nan(MAvgBSINTANGIBLE)|!is.finite(MAvgBSINTANGIBLE)] <- NA
MAvgBSINTANGIBLEt <- MAvgBSINTANGIBLE[,Scolfortrain:Ecolfortrain]

ChgBSINTANGIBLE <- (BSINTANGIBLE/FZshiftmx(input=BSINTANGIBLE, shiftcol=-12))-1
ChgBSINTANGIBLE[is.na(ChgBSINTANGIBLE)|is.nan(ChgBSINTANGIBLE)|!is.finite(ChgBSINTANGIBLE)] <- NA
ChgBSINTANGIBLEt <- ChgBSINTANGIBLE[,Scolfortrain:Ecolfortrain]

BSINTANGIBLEtrailingmax <- FZtrailingmax(input=BSINTANGIBLE)
BSINTANGIBLEGrowth1perchg <- BSINTANGIBLE/BSINTANGIBLEtrailingmax
BSINTANGIBLEGrowth1perchg[is.na(BSINTANGIBLEGrowth1perchg)|is.nan(BSINTANGIBLEGrowth1perchg)|!is.finite(BSINTANGIBLEGrowth1perchg)] <- NA
BSINTANGIBLEGrowth1perchgt <- BSINTANGIBLEGrowth1perchg[,Scolfortrain:Ecolfortrain]

BSINTANGIBLEGrowthlastvs3perma <- BSINTANGIBLE/FZEVAMAvgSKIP(input=BSINTANGIBLEtrailingmax,maper=3,chgper=12)
BSINTANGIBLEGrowthlastvs3perma[is.na(BSINTANGIBLEGrowthlastvs3perma)|is.nan(BSINTANGIBLEGrowthlastvs3perma)|!is.finite(BSINTANGIBLEGrowthlastvs3perma)] <- NA
BSINTANGIBLEGrowthlastvs3permat <- BSINTANGIBLEGrowthlastvs3perma[,Scolfortrain:Ecolfortrain]

BSINTANGIBLEGrowth3permachg <- FZEVAMAvgSKIP(input=BSINTANGIBLEGrowth1perchg,maper=3,chgper=12)
BSINTANGIBLEGrowth3permachg[is.na(BSINTANGIBLEGrowth3permachg)|is.nan(BSINTANGIBLEGrowth3permachg)|!is.finite(BSINTANGIBLEGrowth3permachg)] <- NA
BSINTANGIBLEGrowth3permachgt <- BSINTANGIBLEGrowth3permachg[,Scolfortrain:Ecolfortrain]

BSINTANGIBLEGrowth1pchgvs3pmachg <- BSINTANGIBLEGrowth1perchg/BSINTANGIBLEGrowth3permachg
BSINTANGIBLEGrowth1pchgvs3pmachg[is.na(BSINTANGIBLEGrowth1pchgvs3pmachg)|is.nan(BSINTANGIBLEGrowth1pchgvs3pmachg)|!is.finite(BSINTANGIBLEGrowth1pchgvs3pmachg)] <- NA
BSINTANGIBLEGrowth1pchgvs3pmachgt <- BSINTANGIBLEGrowth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
BSINTANGIBLE_LTM[is.na(BSINTANGIBLE_LTM)|is.nan(BSINTANGIBLE_LTM)|!is.finite(BSINTANGIBLE_LTM)] <- NA

MAvgBSINTANGIBLE_LTM <- FZEVAMAvgLTM(input=BSINTANGIBLE_LTM,maper=3,chgper=12)
MAvgBSINTANGIBLE_LTM[is.na(MAvgBSINTANGIBLE_LTM)|is.nan(MAvgBSINTANGIBLE_LTM)|!is.finite(MAvgBSINTANGIBLE_LTM)] <- NA

ChgBSINTANGIBLE_LTM <- (BSINTANGIBLE_LTM/FZshiftmx(input=BSINTANGIBLE_LTM, shiftcol=-12))-1
ChgBSINTANGIBLE_LTM[is.na(ChgBSINTANGIBLE_LTM)|is.nan(ChgBSINTANGIBLE_LTM)|!is.finite(ChgBSINTANGIBLE_LTM)] <- NA

BSINTANGIBLEtrailingmax_LTM <- FZtrailingmax(input=BSINTANGIBLE_LTM)
BSINTANGIBLEGrowth1perchg_LTM <- BSINTANGIBLE_LTM/BSINTANGIBLEtrailingmax_LTM
BSINTANGIBLEGrowth1perchg_LTM[is.na(BSINTANGIBLEGrowth1perchg_LTM)|is.nan(BSINTANGIBLEGrowth1perchg_LTM)|!is.finite(BSINTANGIBLEGrowth1perchg_LTM)] <- NA

BSINTANGIBLEGrowthlastvs3perma_LTM <- BSINTANGIBLE_LTM/FZEVAMAvgLTM(input=BSINTANGIBLEtrailingmax_LTM,maper=3,chgper=12)
BSINTANGIBLEGrowthlastvs3perma_LTM[is.na(BSINTANGIBLEGrowthlastvs3perma_LTM)|is.nan(BSINTANGIBLEGrowthlastvs3perma_LTM)|!is.finite(BSINTANGIBLEGrowthlastvs3perma_LTM)] <- NA

BSINTANGIBLEGrowth3permachg_LTM <- FZEVAMAvgLTM(input=BSINTANGIBLEGrowth1perchg_LTM,maper=3,chgper=12)
BSINTANGIBLEGrowth3permachg_LTM[is.na(BSINTANGIBLEGrowth3permachg_LTM)|is.nan(BSINTANGIBLEGrowth3permachg_LTM)|!is.finite(BSINTANGIBLEGrowth3permachg_LTM)] <- NA

BSINTANGIBLEGrowth1pchgvs3pmachg_LTM <- BSINTANGIBLEGrowth1perchg_LTM/BSINTANGIBLEGrowth3permachg_LTM
BSINTANGIBLEGrowth1pchgvs3pmachg_LTM[is.na(BSINTANGIBLEGrowth1pchgvs3pmachg_LTM)|is.nan(BSINTANGIBLEGrowth1pchgvs3pmachg_LTM)|!is.finite(BSINTANGIBLEGrowth1pchgvs3pmachg_LTM)] <- NA


###################### Inventory ######################
BSINVENTORY <- BSINVENTORY_ANN
BSINVENTORY[is.na(BSINVENTORY)|is.nan(BSINVENTORY)|!is.finite(BSINVENTORY)] <- NA
BSINVENTORYt <- BSINVENTORY[,Scolfortrain:Ecolfortrain]

MAvgBSINVENTORY <- FZEVAMAvgSKIP(input=BSINVENTORY,maper=3,chgper=12)
MAvgBSINVENTORY[is.na(MAvgBSINVENTORY)|is.nan(MAvgBSINVENTORY)|!is.finite(MAvgBSINVENTORY)] <- NA
MAvgBSINVENTORYt <- MAvgBSINVENTORY[,Scolfortrain:Ecolfortrain]

ChgBSINVENTORY <- (BSINVENTORY/FZshiftmx(input=BSINVENTORY, shiftcol=-12))-1
ChgBSINVENTORY[is.na(ChgBSINVENTORY)|is.nan(ChgBSINVENTORY)|!is.finite(ChgBSINVENTORY)] <- NA
ChgBSINVENTORYt <- ChgBSINVENTORY[,Scolfortrain:Ecolfortrain]

BSINVENTORYtrailingmax <- FZtrailingmax(input=BSINVENTORY)
BSINVENTORYGrowth1perchg <- BSINVENTORY/BSINVENTORYtrailingmax
BSINVENTORYGrowth1perchg[is.na(BSINVENTORYGrowth1perchg)|is.nan(BSINVENTORYGrowth1perchg)|!is.finite(BSINVENTORYGrowth1perchg)] <- NA
BSINVENTORYGrowth1perchgt <- BSINVENTORYGrowth1perchg[,Scolfortrain:Ecolfortrain]

BSINVENTORYGrowthlastvs3perma <- BSINVENTORY/FZEVAMAvgSKIP(input=BSINVENTORYtrailingmax,maper=3,chgper=12)
BSINVENTORYGrowthlastvs3perma[is.na(BSINVENTORYGrowthlastvs3perma)|is.nan(BSINVENTORYGrowthlastvs3perma)|!is.finite(BSINVENTORYGrowthlastvs3perma)] <- NA
BSINVENTORYGrowthlastvs3permat <- BSINVENTORYGrowthlastvs3perma[,Scolfortrain:Ecolfortrain]

BSINVENTORYGrowth3permachg <- FZEVAMAvgSKIP(input=BSINVENTORYGrowth1perchg,maper=3,chgper=12)
BSINVENTORYGrowth3permachg[is.na(BSINVENTORYGrowth3permachg)|is.nan(BSINVENTORYGrowth3permachg)|!is.finite(BSINVENTORYGrowth3permachg)] <- NA
BSINVENTORYGrowth3permachgt <- BSINVENTORYGrowth3permachg[,Scolfortrain:Ecolfortrain]

BSINVENTORYGrowth1pchgvs3pmachg <- BSINVENTORYGrowth1perchg/BSINVENTORYGrowth3permachg
BSINVENTORYGrowth1pchgvs3pmachg[is.na(BSINVENTORYGrowth1pchgvs3pmachg)|is.nan(BSINVENTORYGrowth1pchgvs3pmachg)|!is.finite(BSINVENTORYGrowth1pchgvs3pmachg)] <- NA
BSINVENTORYGrowth1pchgvs3pmachgt <- BSINVENTORYGrowth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
BSINVENTORY_LTM[is.na(BSINVENTORY_LTM)|is.nan(BSINVENTORY_LTM)|!is.finite(BSINVENTORY_LTM)] <- NA

MAvgBSINVENTORY_LTM <- FZEVAMAvgLTM(input=BSINVENTORY_LTM,maper=3,chgper=12)
MAvgBSINVENTORY_LTM[is.na(MAvgBSINVENTORY_LTM)|is.nan(MAvgBSINVENTORY_LTM)|!is.finite(MAvgBSINVENTORY_LTM)] <- NA

ChgBSINVENTORY_LTM <- (BSINVENTORY_LTM/FZshiftmx(input=BSINVENTORY_LTM, shiftcol=-12))-1
ChgBSINVENTORY_LTM[is.na(ChgBSINVENTORY_LTM)|is.nan(ChgBSINVENTORY_LTM)|!is.finite(ChgBSINVENTORY_LTM)] <- NA

BSINVENTORYtrailingmax_LTM <- FZtrailingmax(input=BSINVENTORY_LTM)
BSINVENTORYGrowth1perchg_LTM <- BSINVENTORY_LTM/BSINVENTORYtrailingmax_LTM
BSINVENTORYGrowth1perchg_LTM[is.na(BSINVENTORYGrowth1perchg_LTM)|is.nan(BSINVENTORYGrowth1perchg_LTM)|!is.finite(BSINVENTORYGrowth1perchg_LTM)] <- NA

BSINVENTORYGrowthlastvs3perma_LTM <- BSINVENTORY_LTM/FZEVAMAvgLTM(input=BSINVENTORYtrailingmax_LTM,maper=3,chgper=12)
BSINVENTORYGrowthlastvs3perma_LTM[is.na(BSINVENTORYGrowthlastvs3perma_LTM)|is.nan(BSINVENTORYGrowthlastvs3perma_LTM)|!is.finite(BSINVENTORYGrowthlastvs3perma_LTM)] <- NA

BSINVENTORYGrowth3permachg_LTM <- FZEVAMAvgLTM(input=BSINVENTORYGrowth1perchg_LTM,maper=3,chgper=12)
BSINVENTORYGrowth3permachg_LTM[is.na(BSINVENTORYGrowth3permachg_LTM)|is.nan(BSINVENTORYGrowth3permachg_LTM)|!is.finite(BSINVENTORYGrowth3permachg_LTM)] <- NA

BSINVENTORYGrowth1pchgvs3pmachg_LTM <- BSINVENTORYGrowth1perchg_LTM/BSINVENTORYGrowth3permachg_LTM
BSINVENTORYGrowth1pchgvs3pmachg_LTM[is.na(BSINVENTORYGrowth1pchgvs3pmachg_LTM)|is.nan(BSINVENTORYGrowth1pchgvs3pmachg_LTM)|!is.finite(BSINVENTORYGrowth1pchgvs3pmachg_LTM)] <- NA


###################### AssetTOMomentum ######################
AssetTOMomentum <- FZEVAMAvgSKIP(input=assetturnover_ANN, maper = 3,chgper=12)-FZEVAMAvgSKIP(input=assetturnover_ANN, maper = 5,chgper=12)
AssetTOMomentum[is.na(AssetTOMomentum)|is.nan(AssetTOMomentum)|!is.finite(AssetTOMomentum)] <- NA
AssetTOMomentumt <- AssetTOMomentum[,Scolfortrain:Ecolfortrain]

MAvgAssetTOMomentum <- FZEVAMAvgSKIP(input=AssetTOMomentum,maper=3,chgper=12)
MAvgAssetTOMomentum[is.na(MAvgAssetTOMomentum)|is.nan(MAvgAssetTOMomentum)|!is.finite(MAvgAssetTOMomentum)] <- NA
MAvgAssetTOMomentumt <- MAvgAssetTOMomentum[,Scolfortrain:Ecolfortrain]

ChgAssetTOMomentum <- (AssetTOMomentum/FZshiftmx(input=AssetTOMomentum, shiftcol=-12))-1
ChgAssetTOMomentum[is.na(ChgAssetTOMomentum)|is.nan(ChgAssetTOMomentum)|!is.finite(ChgAssetTOMomentum)] <- NA
ChgAssetTOMomentumt <- ChgAssetTOMomentum[,Scolfortrain:Ecolfortrain]

AssetTOMomentumtrailingmax <- FZtrailingmax(input=ChgAssetTOMomentum)
AssetTOMomentumGrowth1perchg <- AssetTOMomentum/AssetTOMomentumtrailingmax
AssetTOMomentumGrowth1perchg[is.na(AssetTOMomentumGrowth1perchg)|is.nan(AssetTOMomentumGrowth1perchg)|!is.finite(AssetTOMomentumGrowth1perchg)] <- NA
AssetTOMomentumGrowth1perchgt <- AssetTOMomentumGrowth1perchg[,Scolfortrain:Ecolfortrain]

AssetTOMomentumGrowthlastvs3perma <- AssetTOMomentum/FZEVAMAvgSKIP(input=AssetTOMomentumtrailingmax,maper=3,chgper=12)
AssetTOMomentumGrowthlastvs3perma[is.na(AssetTOMomentumGrowthlastvs3perma)|is.nan(AssetTOMomentumGrowthlastvs3perma)|!is.finite(AssetTOMomentumGrowthlastvs3perma)] <- NA
AssetTOMomentumGrowthlastvs3permat <- AssetTOMomentumGrowthlastvs3perma[,Scolfortrain:Ecolfortrain]

AssetTOMomentumGrowth3permachg <- FZEVAMAvgSKIP(input=AssetTOMomentumGrowth1perchg,maper=3,chgper=12)
AssetTOMomentumGrowth3permachg[is.na(AssetTOMomentumGrowth3permachg)|is.nan(AssetTOMomentumGrowth3permachg)|!is.finite(AssetTOMomentumGrowth3permachg)] <- NA
AssetTOMomentumGrowth3permachgt <- AssetTOMomentumGrowth3permachg[,Scolfortrain:Ecolfortrain]

AssetTOMomentumGrowth1pchgvs3pmachg <- AssetTOMomentumGrowth1perchg/AssetTOMomentumGrowth3permachg
AssetTOMomentumGrowth1pchgvs3pmachg[is.na(AssetTOMomentumGrowth1pchgvs3pmachg)|is.nan(AssetTOMomentumGrowth1pchgvs3pmachg)|!is.finite(AssetTOMomentumGrowth1pchgvs3pmachg)] <- NA
AssetTOMomentumGrowth1pchgvs3pmachgt <- AssetTOMomentumGrowth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
AssetTOMomentum_LTM <- FZEVAMAvgLTM(input=assetturnover_LTM, maper = 3,chgper=12)-FZEVAMAvgLTM(input=assetturnover_LTM, maper = 5,chgper=12)
AssetTOMomentum_LTM[is.na(AssetTOMomentum_LTM)|is.nan(AssetTOMomentum_LTM)|!is.finite(AssetTOMomentum_LTM)] <- NA

MAvgAssetTOMomentum_LTM <- FZEVAMAvgLTM(input=AssetTOMomentum_LTM,maper=3,chgper=12)
MAvgAssetTOMomentum_LTM[is.na(MAvgAssetTOMomentum_LTM)|is.nan(MAvgAssetTOMomentum_LTM)|!is.finite(MAvgAssetTOMomentum_LTM)] <- NA

ChgAssetTOMomentum_LTM <- (AssetTOMomentum_LTM/FZshiftmx(input=AssetTOMomentum_LTM, shiftcol=-12))-1
ChgAssetTOMomentum_LTM[is.na(ChgAssetTOMomentum_LTM)|is.nan(ChgAssetTOMomentum_LTM)|!is.finite(ChgAssetTOMomentum_LTM)] <- NA

AssetTOMomentumtrailingmax_LTM <- FZtrailingmax(input=ChgAssetTOMomentum_LTM)
AssetTOMomentumGrowth1perchg_LTM <- AssetTOMomentum_LTM/AssetTOMomentumtrailingmax_LTM
AssetTOMomentumGrowth1perchg_LTM[is.na(AssetTOMomentumGrowth1perchg_LTM)|is.nan(AssetTOMomentumGrowth1perchg_LTM)|!is.finite(AssetTOMomentumGrowth1perchg_LTM)] <- NA

AssetTOMomentumGrowthlastvs3perma_LTM <- AssetTOMomentum_LTM/FZEVAMAvgLTM(input=AssetTOMomentumtrailingmax_LTM,maper=3,chgper=12)
AssetTOMomentumGrowthlastvs3perma_LTM[is.na(AssetTOMomentumGrowthlastvs3perma_LTM)|is.nan(AssetTOMomentumGrowthlastvs3perma_LTM)|!is.finite(AssetTOMomentumGrowthlastvs3perma_LTM)] <- NA

AssetTOMomentumGrowth3permachg_LTM <- FZEVAMAvgLTM(input=AssetTOMomentumGrowth1perchg_LTM,maper=3,chgper=12)
AssetTOMomentumGrowth3permachg_LTM[is.na(AssetTOMomentumGrowth3permachg_LTM)|is.nan(AssetTOMomentumGrowth3permachg_LTM)|!is.finite(AssetTOMomentumGrowth3permachg_LTM)] <- NA

AssetTOMomentumGrowth1pchgvs3pmachg_LTM <- AssetTOMomentumGrowth1perchg_LTM/AssetTOMomentumGrowth3permachg_LTM
AssetTOMomentumGrowth1pchgvs3pmachg_LTM[is.na(AssetTOMomentumGrowth1pchgvs3pmachg_LTM)|is.nan(AssetTOMomentumGrowth1pchgvs3pmachg_LTM)|!is.finite(AssetTOMomentumGrowth1pchgvs3pmachg_LTM)] <- NA



###################### ROIC ######################
ROIC <- ROIC_ANN
ROIC[is.na(ROIC)|is.nan(ROIC)|!is.finite(ROIC)] <- NA
ROICt <- ROIC[,Scolfortrain:Ecolfortrain]

MAvgROIC <- FZEVAMAvgSKIP(input=ROIC,maper=3,chgper=12)
MAvgROIC[is.na(MAvgROIC)|is.nan(MAvgROIC)|!is.finite(MAvgROIC)] <- NA
MAvgROICt <- MAvgROIC[,Scolfortrain:Ecolfortrain]

ChgROIC <- (ROIC/FZshiftmx(input=ROIC, shiftcol=-12))-1
ChgROIC[is.na(ChgROIC)|is.nan(ChgROIC)|!is.finite(ChgROIC)] <- NA
ChgROICt <- ChgROIC[,Scolfortrain:Ecolfortrain]

ROICtrailingmax <- FZtrailingmax(input=ROIC)
ROICGrowth1perchg <- ROIC/ROICtrailingmax
ROICGrowth1perchg[is.na(ROICGrowth1perchg)|is.nan(ROICGrowth1perchg)|!is.finite(ROICGrowth1perchg)] <- NA
ROICGrowth1perchgt <- ROICGrowth1perchg[,Scolfortrain:Ecolfortrain]

ROICGrowthlastvs3perma <- ROIC/FZEVAMAvgSKIP(input=ROICtrailingmax,maper=3,chgper=12)
ROICGrowthlastvs3perma[is.na(ROICGrowthlastvs3perma)|is.nan(ROICGrowthlastvs3perma)|!is.finite(ROICGrowthlastvs3perma)] <- NA
ROICGrowthlastvs3permat <- ROICGrowthlastvs3perma[,Scolfortrain:Ecolfortrain]

ROICGrowth3permachg <- FZEVAMAvgSKIP(input=ROICGrowth1perchg,maper=3,chgper=12)
ROICGrowth3permachg[is.na(ROICGrowth3permachg)|is.nan(ROICGrowth3permachg)|!is.finite(ROICGrowth3permachg)] <- NA
ROICGrowth3permachgt <- ROICGrowth3permachg[,Scolfortrain:Ecolfortrain]

ROICGrowth1pchgvs3pmachg <- ROICGrowth1perchg/ROICGrowth3permachg
ROICGrowth1pchgvs3pmachg[is.na(ROICGrowth1pchgvs3pmachg)|is.nan(ROICGrowth1pchgvs3pmachg)|!is.finite(ROICGrowth1pchgvs3pmachg)] <- NA
ROICGrowth1pchgvs3pmachgt <- ROICGrowth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
ROIC_LTM[is.na(ROIC_LTM)|is.nan(ROIC_LTM)|!is.finite(ROIC_LTM)] <- NA

MAvgROIC_LTM <- FZEVAMAvgLTM(input=ROIC_LTM,maper=3,chgper=12)
MAvgROIC_LTM[is.na(MAvgROIC_LTM)|is.nan(MAvgROIC_LTM)|!is.finite(MAvgROIC_LTM)] <- NA

ChgROIC_LTM <- (ROIC_LTM/FZshiftmx(input=ROIC_LTM, shiftcol=-12))-1
ChgROIC_LTM[is.na(ChgROIC_LTM)|is.nan(ChgROIC_LTM)|!is.finite(ChgROIC_LTM)] <- NA

ROICtrailingmax_LTM <- FZtrailingmax(input=ROIC_LTM)
ROICGrowth1perchg_LTM <- ROIC_LTM/ROICtrailingmax_LTM
ROICGrowth1perchg_LTM[is.na(ROICGrowth1perchg_LTM)|is.nan(ROICGrowth1perchg_LTM)|!is.finite(ROICGrowth1perchg_LTM)] <- NA

ROICGrowthlastvs3perma_LTM <- ROIC_LTM/FZEVAMAvgLTM(input=ROICtrailingmax_LTM,maper=3,chgper=12)
ROICGrowthlastvs3perma_LTM[is.na(ROICGrowthlastvs3perma_LTM)|is.nan(ROICGrowthlastvs3perma_LTM)|!is.finite(ROICGrowthlastvs3perma_LTM)] <- NA

ROICGrowth3permachg_LTM <- FZEVAMAvgLTM(input=ROICGrowth1perchg_LTM,maper=3,chgper=12)
ROICGrowth3permachg_LTM[is.na(ROICGrowth3permachg_LTM)|is.nan(ROICGrowth3permachg_LTM)|!is.finite(ROICGrowth3permachg_LTM)] <- NA

ROICGrowth1pchgvs3pmachg_LTM <- ROICGrowth1perchg_LTM/ROICGrowth3permachg_LTM
ROICGrowth1pchgvs3pmachg_LTM[is.na(ROICGrowth1pchgvs3pmachg_LTM)|is.nan(ROICGrowth1pchgvs3pmachg_LTM)|!is.finite(ROICGrowth1pchgvs3pmachg_LTM)] <- NA



###################### P/E ######################
earningpower_ANN <- ((FZEVAMAvgSKIP(input=ROIC_ANN, maper = 3,chgper=12)*(BSEQTOT_ANN+FZzerona(BSDEBTLT_ANN)+FZzerona(BSDEBTST_ANN)+BSWCR_ANN))-PLINTEXPNET_ANN)*(1-PLTAXRATE_ANN)
PricetoEarningsfull <- (MCAP_ANN/earningpower_ANN)
PricetoEarningsfull[is.na(PricetoEarningsfull)|is.nan(PricetoEarningsfull)|!is.finite(PricetoEarningsfull)] <- NA
PricetoEarningsfullt <- PricetoEarningsfull[,Scolfortrain:Ecolfortrain]

MAvgPricetoEarningsfull <- FZEVAMAvgSKIP(input=PricetoEarningsfull,maper=3,chgper=12)
MAvgPricetoEarningsfull[is.na(MAvgPricetoEarningsfull)|is.nan(MAvgPricetoEarningsfull)|!is.finite(MAvgPricetoEarningsfull)] <- NA
MAvgPricetoEarningsfullt <- MAvgPricetoEarningsfull[,Scolfortrain:Ecolfortrain]

ChgPricetoEarningsfull <- (PricetoEarningsfull/FZshiftmx(input=PricetoEarningsfull, shiftcol=-12))-1
ChgPricetoEarningsfull[is.na(ChgPricetoEarningsfull)|is.nan(ChgPricetoEarningsfull)|!is.finite(ChgPricetoEarningsfull)] <- NA
ChgPricetoEarningsfullt <- ChgPricetoEarningsfull[,Scolfortrain:Ecolfortrain]

PricetoEarningsfulltrailingmax <- FZtrailingmax(input=PricetoEarningsfull)
PricetoEarningsfull1perchg <- PricetoEarningsfull/PricetoEarningsfulltrailingmax
PricetoEarningsfull1perchg[is.na(PricetoEarningsfull1perchg)|is.nan(PricetoEarningsfull1perchg)|!is.finite(PricetoEarningsfull1perchg)] <- NA
PricetoEarningsfull1perchgt <- PricetoEarningsfull1perchg[,Scolfortrain:Ecolfortrain]

PricetoEarningsfulllastvs3perma <- PricetoEarningsfull/FZEVAMAvgSKIP(input=PricetoEarningsfulltrailingmax,maper=3,chgper=12)
PricetoEarningsfulllastvs3perma[is.na(PricetoEarningsfulllastvs3perma)|is.nan(PricetoEarningsfulllastvs3perma)|!is.finite(PricetoEarningsfulllastvs3perma)] <- NA
PricetoEarningsfulllastvs3permat <- PricetoEarningsfulllastvs3perma[,Scolfortrain:Ecolfortrain]

PricetoEarningsfull3permachg <- FZEVAMAvgSKIP(input=PricetoEarningsfull1perchg,maper=3,chgper=12)
PricetoEarningsfull3permachg[is.na(PricetoEarningsfull3permachg)|is.nan(PricetoEarningsfull3permachg)|!is.finite(PricetoEarningsfull3permachg)] <- NA
PricetoEarningsfull3permachgt <- PricetoEarningsfull3permachg[,Scolfortrain:Ecolfortrain]

PricetoEarningsfull1pchgvs3pmachg <- PricetoEarningsfull1perchg/PricetoEarningsfull3permachg
PricetoEarningsfull1pchgvs3pmachg[is.na(PricetoEarningsfull1pchgvs3pmachg)|is.nan(PricetoEarningsfull1pchgvs3pmachg)|!is.finite(PricetoEarningsfull1pchgvs3pmachg)] <- NA
PricetoEarningsfull1pchgvs3pmachgt <- PricetoEarningsfull1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
earningpower_LTM <- ((FZEVAMAvgLTM(input=ROIC_LTM, maper = 3,chgper=12)*(BSEQTOT_LTM+FZzerona(BSDEBTLT_LTM)+FZzerona(BSDEBTST_LTM)+BSWCR_LTM))-PLINTEXPNET_LTM)*(1-PLTAXRATE_LTM)
PricetoEarningsfull_LTM <- (MCAP_LTM/earningpower_LTM)
PricetoEarningsfull_LTM[is.na(PricetoEarningsfull_LTM)|is.nan(PricetoEarningsfull_LTM)|!is.finite(PricetoEarningsfull_LTM)] <- NA

MAvgPricetoEarningsfull_LTM <- FZEVAMAvgLTM(input=PricetoEarningsfull_LTM,maper=3,chgper=12)
MAvgPricetoEarningsfull_LTM[is.na(MAvgPricetoEarningsfull_LTM)|is.nan(MAvgPricetoEarningsfull_LTM)|!is.finite(MAvgPricetoEarningsfull_LTM)] <- NA

ChgPricetoEarningsfull_LTM <- (PricetoEarningsfull_LTM/FZshiftmx(input=PricetoEarningsfull_LTM, shiftcol=-12))-1
ChgPricetoEarningsfull_LTM[is.na(ChgPricetoEarningsfull_LTM)|is.nan(ChgPricetoEarningsfull_LTM)|!is.finite(ChgPricetoEarningsfull_LTM)] <- NA

PricetoEarningsfulltrailingmax_LTM <- FZtrailingmax(input=PricetoEarningsfull_LTM)
PricetoEarningsfull1perchg_LTM <- PricetoEarningsfull_LTM/PricetoEarningsfulltrailingmax_LTM
PricetoEarningsfull1perchg_LTM[is.na(PricetoEarningsfull1perchg_LTM)|is.nan(PricetoEarningsfull1perchg_LTM)|!is.finite(PricetoEarningsfull1perchg_LTM)] <- NA

PricetoEarningsfulllastvs3perma_LTM <- PricetoEarningsfull_LTM/FZEVAMAvgLTM(input=PricetoEarningsfulltrailingmax_LTM,maper=3,chgper=12)
PricetoEarningsfulllastvs3perma_LTM[is.na(PricetoEarningsfulllastvs3perma_LTM)|is.nan(PricetoEarningsfulllastvs3perma_LTM)|!is.finite(PricetoEarningsfulllastvs3perma_LTM)] <- NA

PricetoEarningsfull3permachg_LTM <- FZEVAMAvgLTM(input=PricetoEarningsfull1perchg_LTM,maper=3,chgper=12)
PricetoEarningsfull3permachg_LTM[is.na(PricetoEarningsfull3permachg_LTM)|is.nan(PricetoEarningsfull3permachg_LTM)|!is.finite(PricetoEarningsfull3permachg_LTM)] <- NA

PricetoEarningsfull1pchgvs3pmachg_LTM <- PricetoEarningsfull1perchg_LTM/PricetoEarningsfull3permachg_LTM
PricetoEarningsfull1pchgvs3pmachg_LTM[is.na(PricetoEarningsfull1pchgvs3pmachg_LTM)|is.nan(PricetoEarningsfull1pchgvs3pmachg_LTM)|!is.finite(PricetoEarningsfull1pchgvs3pmachg_LTM)] <- NA




###################### lasttoHistMaxfull ######################
lasttoHistMaxfull <- PLSALES_ANN/t(apply(PLSALES_ANN,1,cummax))
lasttoHistMaxfull[is.na(lasttoHistMaxfull)|is.nan(lasttoHistMaxfull)|!is.finite(lasttoHistMaxfull)] <- NA
lasttoHistMaxfullt <- lasttoHistMaxfull[,Scolfortrain:Ecolfortrain]

MAvglasttoHistMaxfull <- FZEVAMAvgSKIP(input=lasttoHistMaxfull,maper=3,chgper=12)
MAvglasttoHistMaxfull[is.na(MAvglasttoHistMaxfull)|is.nan(MAvglasttoHistMaxfull)|!is.finite(MAvglasttoHistMaxfull)] <- NA
MAvglasttoHistMaxfullt <- MAvglasttoHistMaxfull[,Scolfortrain:Ecolfortrain]

ChglasttoHistMaxfull <- (lasttoHistMaxfull/FZshiftmx(input=lasttoHistMaxfull, shiftcol=-12))-1
ChglasttoHistMaxfull[is.na(ChglasttoHistMaxfull)|is.nan(ChglasttoHistMaxfull)|!is.finite(ChglasttoHistMaxfull)] <- NA
ChglasttoHistMaxfullt <- ChglasttoHistMaxfull[,Scolfortrain:Ecolfortrain]

lasttoHistMaxfulltrailingmax <- FZtrailingmax(input=lasttoHistMaxfull)
lasttoHistMaxfull1perchg <- lasttoHistMaxfull/lasttoHistMaxfulltrailingmax
lasttoHistMaxfull1perchg[is.na(lasttoHistMaxfull1perchg)|is.nan(lasttoHistMaxfull1perchg)|!is.finite(lasttoHistMaxfull1perchg)] <- NA
lasttoHistMaxfull1perchgt <- lasttoHistMaxfull1perchg[,Scolfortrain:Ecolfortrain]

lasttoHistMaxfulllastvs3perma <- lasttoHistMaxfull/FZEVAMAvgSKIP(input=lasttoHistMaxfulltrailingmax,maper=3,chgper=12)
lasttoHistMaxfulllastvs3perma[is.na(lasttoHistMaxfulllastvs3perma)|is.nan(lasttoHistMaxfulllastvs3perma)|!is.finite(lasttoHistMaxfulllastvs3perma)] <- NA
lasttoHistMaxfulllastvs3permat <- lasttoHistMaxfulllastvs3perma[,Scolfortrain:Ecolfortrain]

lasttoHistMaxfull3permachg <- FZEVAMAvgSKIP(input=lasttoHistMaxfull1perchg,maper=3,chgper=12)
lasttoHistMaxfull3permachg[is.na(lasttoHistMaxfull3permachg)|is.nan(lasttoHistMaxfull3permachg)|!is.finite(lasttoHistMaxfull3permachg)] <- NA
lasttoHistMaxfull3permachgt <- lasttoHistMaxfull3permachg[,Scolfortrain:Ecolfortrain]

lasttoHistMaxfull1pchgvs3pmachg <- lasttoHistMaxfull1perchg/lasttoHistMaxfull3permachg
lasttoHistMaxfull1pchgvs3pmachg[is.na(lasttoHistMaxfull1pchgvs3pmachg)|is.nan(lasttoHistMaxfull1pchgvs3pmachg)|!is.finite(lasttoHistMaxfull1pchgvs3pmachg)] <- NA
lasttoHistMaxfull1pchgvs3pmachgt <- lasttoHistMaxfull1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
lasttoHistMaxfull_LTM <- PLSALES_LTM/t(apply(PLSALES_LTM,1,cummax))
lasttoHistMaxfull_LTM[is.na(lasttoHistMaxfull_LTM)|is.nan(lasttoHistMaxfull_LTM)|!is.finite(lasttoHistMaxfull_LTM)] <- NA
lasttoHistMaxfull_LTMt <- lasttoHistMaxfull_LTM[,Scolfortrain:Ecolfortrain]

MAvglasttoHistMaxfull_LTM <- FZEVAMAvgLTM(input=lasttoHistMaxfull_LTM,maper=3,chgper=12)
MAvglasttoHistMaxfull_LTM[is.na(MAvglasttoHistMaxfull_LTM)|is.nan(MAvglasttoHistMaxfull_LTM)|!is.finite(MAvglasttoHistMaxfull_LTM)] <- NA
MAvglasttoHistMaxfull_LTMt <- MAvglasttoHistMaxfull_LTM[,Scolfortrain:Ecolfortrain]

ChglasttoHistMaxfull_LTM <- (lasttoHistMaxfull_LTM/FZshiftmx(input=lasttoHistMaxfull_LTM, shiftcol=-12))-1
ChglasttoHistMaxfull_LTM[is.na(ChglasttoHistMaxfull_LTM)|is.nan(ChglasttoHistMaxfull_LTM)|!is.finite(ChglasttoHistMaxfull_LTM)] <- NA
ChglasttoHistMaxfull_LTMt <- ChglasttoHistMaxfull_LTM[,Scolfortrain:Ecolfortrain]

lasttoHistMaxfulltrailingmax_LTM <- FZtrailingmax(input=lasttoHistMaxfull_LTM)
lasttoHistMaxfull1perchg_LTM <- lasttoHistMaxfull_LTM/lasttoHistMaxfulltrailingmax_LTM
lasttoHistMaxfull1perchg_LTM[is.na(lasttoHistMaxfull1perchg_LTM)|is.nan(lasttoHistMaxfull1perchg_LTM)|!is.finite(lasttoHistMaxfull1perchg_LTM)] <- NA
lasttoHistMaxfull1perchg_LTMt <- lasttoHistMaxfull1perchg_LTM[,Scolfortrain:Ecolfortrain]

lasttoHistMaxfulllastvs3perma_LTM <- lasttoHistMaxfull_LTM/FZEVAMAvgLTM(input=lasttoHistMaxfulltrailingmax_LTM,maper=3,chgper=12)
lasttoHistMaxfulllastvs3perma_LTM[is.na(lasttoHistMaxfulllastvs3perma_LTM)|is.nan(lasttoHistMaxfulllastvs3perma_LTM)|!is.finite(lasttoHistMaxfulllastvs3perma_LTM)] <- NA
lasttoHistMaxfulllastvs3perma_LTMt <- lasttoHistMaxfulllastvs3perma_LTM[,Scolfortrain:Ecolfortrain]

lasttoHistMaxfull3permachg_LTM <- FZEVAMAvgLTM(input=lasttoHistMaxfull1perchg_LTM,maper=3,chgper=12)
lasttoHistMaxfull3permachg_LTM[is.na(lasttoHistMaxfull3permachg_LTM)|is.nan(lasttoHistMaxfull3permachg_LTM)|!is.finite(lasttoHistMaxfull3permachg_LTM)] <- NA
lasttoHistMaxfull3permachg_LTMt <- lasttoHistMaxfull3permachg_LTM[,Scolfortrain:Ecolfortrain]

lasttoHistMaxfull1pchgvs3pmachg_LTM <- lasttoHistMaxfull1perchg_LTM/lasttoHistMaxfull3permachg_LTM
lasttoHistMaxfull1pchgvs3pmachg_LTM[is.na(lasttoHistMaxfull1pchgvs3pmachg_LTM)|is.nan(lasttoHistMaxfull1pchgvs3pmachg_LTM)|!is.finite(lasttoHistMaxfull1pchgvs3pmachg_LTM)] <- NA
lasttoHistMaxfull1pchgvs3pmachg_LTMt <- lasttoHistMaxfull1pchgvs3pmachg_LTM[,Scolfortrain:Ecolfortrain]





########################## Debt ###########################
Debt <- FZzerona(input=BSDEBTLT_ANN)+FZzerona(input=BSDEBTST_ANN)
Debt[is.na(Debt)|is.nan(Debt)|!is.finite(Debt)] <- NA
Debtt <- Debt[,Scolfortrain:Ecolfortrain]

MAvgDebt <- FZEVAMAvgSKIP(input=Debt,maper=3,chgper=12)
MAvgDebt[is.na(MAvgDebt)|is.nan(MAvgDebt)|!is.finite(MAvgDebt)] <- NA
MAvgDebtt <- MAvgDebt[,Scolfortrain:Ecolfortrain]

ChgDebt <- (Debt/FZshiftmx(input=Debt, shiftcol=-12))-1
ChgDebt[is.na(ChgDebt)|is.nan(ChgDebt)|!is.finite(ChgDebt)] <- NA
ChgDebtt <- ChgDebt[,Scolfortrain:Ecolfortrain]

Debttrailingmax <- FZtrailingmax(input=Debt)
DebtGrowth1perchg <- Debt/Debttrailingmax
DebtGrowth1perchg[is.na(DebtGrowth1perchg)|is.nan(DebtGrowth1perchg)|!is.finite(DebtGrowth1perchg)] <- NA
DebtGrowth1perchgt <- DebtGrowth1perchg[,Scolfortrain:Ecolfortrain]

DebtGrowthlastvs3perma <- Debt/FZEVAMAvgSKIP(input=Debttrailingmax,maper=3,chgper=12)
DebtGrowthlastvs3perma[is.na(DebtGrowthlastvs3perma)|is.nan(DebtGrowthlastvs3perma)|!is.finite(DebtGrowthlastvs3perma)] <- NA
DebtGrowthlastvs3permat <- DebtGrowthlastvs3perma[,Scolfortrain:Ecolfortrain]

DebtGrowth3permachg <- FZEVAMAvgSKIP(input=DebtGrowth1perchg,maper=3,chgper=12)
DebtGrowth3permachg[is.na(DebtGrowth3permachg)|is.nan(DebtGrowth3permachg)|!is.finite(DebtGrowth3permachg)] <- NA
DebtGrowth3permachgt <- DebtGrowth3permachg[,Scolfortrain:Ecolfortrain]

DebtGrowth1pchgvs3pmachg <- DebtGrowth1perchg/DebtGrowth3permachg
DebtGrowth1pchgvs3pmachg[is.na(DebtGrowth1pchgvs3pmachg)|is.nan(DebtGrowth1pchgvs3pmachg)|!is.finite(DebtGrowth1pchgvs3pmachg)] <- NA
DebtGrowth1pchgvs3pmachgt <- DebtGrowth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
Debt_LTM <- FZzerona(input=BSDEBTLT_LTM)+FZzerona(input=BSDEBTST_LTM)
Debt_LTM[is.na(Debt_LTM)|is.nan(Debt_LTM)|!is.finite(Debt_LTM)] <- NA

MAvgDebt_LTM <- FZEVAMAvgLTM(input=Debt_LTM,maper=3,chgper=12)
MAvgDebt_LTM[is.na(MAvgDebt_LTM)|is.nan(MAvgDebt_LTM)|!is.finite(MAvgDebt_LTM)] <- NA

ChgDebt_LTM <- (Debt_LTM/FZshiftmx(input=Debt_LTM, shiftcol=-12))-1
ChgDebt_LTM[is.na(ChgDebt_LTM)|is.nan(ChgDebt_LTM)|!is.finite(ChgDebt_LTM)] <- NA

Debttrailingmax_LTM <- FZtrailingmax(input=Debt_LTM)
DebtGrowth1perchg_LTM <- Debt_LTM/Debttrailingmax_LTM
DebtGrowth1perchg_LTM[is.na(DebtGrowth1perchg_LTM)|is.nan(DebtGrowth1perchg_LTM)|!is.finite(DebtGrowth1perchg_LTM)] <- NA

DebtGrowthlastvs3perma_LTM <- Debt_LTM/FZEVAMAvgLTM(input=Debttrailingmax_LTM,maper=3,chgper=12)
DebtGrowthlastvs3perma_LTM[is.na(DebtGrowthlastvs3perma_LTM)|is.nan(DebtGrowthlastvs3perma_LTM)|!is.finite(DebtGrowthlastvs3perma_LTM)] <- NA

DebtGrowth3permachg_LTM <- FZEVAMAvgLTM(input=DebtGrowth1perchg_LTM,maper=3,chgper=12)
DebtGrowth3permachg_LTM[is.na(DebtGrowth3permachg_LTM)|is.nan(DebtGrowth3permachg_LTM)|!is.finite(DebtGrowth3permachg_LTM)] <- NA

DebtGrowth1pchgvs3pmachg_LTM <- DebtGrowth1perchg_LTM/DebtGrowth3permachg_LTM
DebtGrowth1pchgvs3pmachg_LTM[is.na(DebtGrowth1pchgvs3pmachg_LTM)|is.nan(DebtGrowth1pchgvs3pmachg_LTM)|!is.finite(DebtGrowth1pchgvs3pmachg_LTM)] <- NA





###################### RetainedEarningstoEquity ######################
RetainedEarningstoEquityfull <- (BSRETAINEDEARNINGS_ANN/BSEQTOT_ANN)
RetainedEarningstoEquityfull[is.na(RetainedEarningstoEquityfull)|is.nan(RetainedEarningstoEquityfull)|!is.finite(RetainedEarningstoEquityfull)] <- NA
RetainedEarningstoEquityfullt <- RetainedEarningstoEquityfull[,Scolfortrain:Ecolfortrain]

MAvgRetainedEarningstoEquityfull <- FZEVAMAvgSKIP(input=RetainedEarningstoEquityfull,maper=3,chgper=12)
MAvgRetainedEarningstoEquityfull[is.na(MAvgRetainedEarningstoEquityfull)|is.nan(MAvgRetainedEarningstoEquityfull)|!is.finite(MAvgRetainedEarningstoEquityfull)] <- NA
MAvgRetainedEarningstoEquityfullt <- MAvgRetainedEarningstoEquityfull[,Scolfortrain:Ecolfortrain]

ChgRetainedEarningstoEquityfull <- (RetainedEarningstoEquityfull/FZshiftmx(input=RetainedEarningstoEquityfull, shiftcol=-12))-1
ChgRetainedEarningstoEquityfull[is.na(ChgRetainedEarningstoEquityfull)|is.nan(ChgRetainedEarningstoEquityfull)|!is.finite(ChgRetainedEarningstoEquityfull)] <- NA
ChgRetainedEarningstoEquityfullt <- ChgRetainedEarningstoEquityfull[,Scolfortrain:Ecolfortrain]

RetainedEarningstoEquityfulltrailingmax <- FZtrailingmax(input=RetainedEarningstoEquityfull)
RetainedEarningstoEquityfullGrowth1perchg <- RetainedEarningstoEquityfull/RetainedEarningstoEquityfulltrailingmax
RetainedEarningstoEquityfullGrowth1perchg[is.na(RetainedEarningstoEquityfullGrowth1perchg)|is.nan(RetainedEarningstoEquityfullGrowth1perchg)|!is.finite(RetainedEarningstoEquityfullGrowth1perchg)] <- NA
RetainedEarningstoEquityfullGrowth1perchgt <- RetainedEarningstoEquityfullGrowth1perchg[,Scolfortrain:Ecolfortrain]

RetainedEarningstoEquityfullGrowthlastvs3perma <- RetainedEarningstoEquityfull/FZEVAMAvgSKIP(input=RetainedEarningstoEquityfulltrailingmax,maper=3,chgper=12)
RetainedEarningstoEquityfullGrowthlastvs3perma[is.na(RetainedEarningstoEquityfullGrowthlastvs3perma)|is.nan(RetainedEarningstoEquityfullGrowthlastvs3perma)|!is.finite(RetainedEarningstoEquityfullGrowthlastvs3perma)] <- NA
RetainedEarningstoEquityfullGrowthlastvs3permat <- RetainedEarningstoEquityfullGrowthlastvs3perma[,Scolfortrain:Ecolfortrain]

RetainedEarningstoEquityfullGrowth3permachg <- FZEVAMAvgSKIP(input=RetainedEarningstoEquityfullGrowth1perchg,maper=3,chgper=12)
RetainedEarningstoEquityfullGrowth3permachg[is.na(RetainedEarningstoEquityfullGrowth3permachg)|is.nan(RetainedEarningstoEquityfullGrowth3permachg)|!is.finite(RetainedEarningstoEquityfullGrowth3permachg)] <- NA
RetainedEarningstoEquityfullGrowth3permachgt <- RetainedEarningstoEquityfullGrowth3permachg[,Scolfortrain:Ecolfortrain]

RetainedEarningstoEquityfullGrowth1pchgvs3pmachg <- RetainedEarningstoEquityfullGrowth1perchg/RetainedEarningstoEquityfullGrowth3permachg
RetainedEarningstoEquityfullGrowth1pchgvs3pmachg[is.na(RetainedEarningstoEquityfullGrowth1pchgvs3pmachg)|is.nan(RetainedEarningstoEquityfullGrowth1pchgvs3pmachg)|!is.finite(RetainedEarningstoEquityfullGrowth1pchgvs3pmachg)] <- NA
RetainedEarningstoEquityfullGrowth1pchgvs3pmachgt <- RetainedEarningstoEquityfullGrowth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
RetainedEarningstoEquityfull_LTM <- (BSRETAINEDEARNINGS_LTM/BSEQTOT_LTM)
RetainedEarningstoEquityfull_LTM[is.na(RetainedEarningstoEquityfull_LTM)|is.nan(RetainedEarningstoEquityfull_LTM)|!is.finite(RetainedEarningstoEquityfull_LTM)] <- NA

MAvgRetainedEarningstoEquityfull_LTM <- FZEVAMAvgLTM(input=RetainedEarningstoEquityfull_LTM,maper=3,chgper=12)
MAvgRetainedEarningstoEquityfull_LTM[is.na(MAvgRetainedEarningstoEquityfull_LTM)|is.nan(MAvgRetainedEarningstoEquityfull_LTM)|!is.finite(MAvgRetainedEarningstoEquityfull_LTM)] <- NA

ChgRetainedEarningstoEquityfull_LTM <- (RetainedEarningstoEquityfull_LTM/FZshiftmx(input=RetainedEarningstoEquityfull_LTM, shiftcol=-12))-1
ChgRetainedEarningstoEquityfull_LTM[is.na(ChgRetainedEarningstoEquityfull_LTM)|is.nan(ChgRetainedEarningstoEquityfull_LTM)|!is.finite(ChgRetainedEarningstoEquityfull_LTM)] <- NA

RetainedEarningstoEquityfulltrailingmax_LTM <- FZtrailingmax(input=RetainedEarningstoEquityfull_LTM)
RetainedEarningstoEquityfullGrowth1perchg_LTM <- RetainedEarningstoEquityfull_LTM/RetainedEarningstoEquityfulltrailingmax_LTM
RetainedEarningstoEquityfullGrowth1perchg_LTM[is.na(RetainedEarningstoEquityfullGrowth1perchg_LTM)|is.nan(RetainedEarningstoEquityfullGrowth1perchg_LTM)|!is.finite(RetainedEarningstoEquityfullGrowth1perchg_LTM)] <- NA

RetainedEarningstoEquityfullGrowthlastvs3perma_LTM <- RetainedEarningstoEquityfull_LTM/FZEVAMAvgLTM(input=RetainedEarningstoEquityfulltrailingmax_LTM,maper=3,chgper=12)
RetainedEarningstoEquityfullGrowthlastvs3perma_LTM[is.na(RetainedEarningstoEquityfullGrowthlastvs3perma_LTM)|is.nan(RetainedEarningstoEquityfullGrowthlastvs3perma_LTM)|!is.finite(RetainedEarningstoEquityfullGrowthlastvs3perma_LTM)] <- NA

RetainedEarningstoEquityfullGrowth3permachg_LTM <- FZEVAMAvgLTM(input=RetainedEarningstoEquityfullGrowth1perchg_LTM,maper=3,chgper=12)
RetainedEarningstoEquityfullGrowth3permachg_LTM[is.na(RetainedEarningstoEquityfullGrowth3permachg_LTM)|is.nan(RetainedEarningstoEquityfullGrowth3permachg_LTM)|!is.finite(RetainedEarningstoEquityfullGrowth3permachg_LTM)] <- NA

RetainedEarningstoEquityfullGrowth1pchgvs3pmachg_LTM <- RetainedEarningstoEquityfullGrowth1perchg_LTM/RetainedEarningstoEquityfullGrowth3permachg_LTM
RetainedEarningstoEquityfullGrowth1pchgvs3pmachg_LTM[is.na(RetainedEarningstoEquityfullGrowth1pchgvs3pmachg_LTM)|is.nan(RetainedEarningstoEquityfullGrowth1pchgvs3pmachg_LTM)|!is.finite(RetainedEarningstoEquityfullGrowth1pchgvs3pmachg_LTM)] <- NA




###################### RETENTION RATIO ######################
RetentionRatiofull <- RETENTIONRATIO_ANN
RetentionRatiofull[is.na(RetentionRatiofull)|is.nan(RetentionRatiofull)|!is.finite(RetentionRatiofull)] <- NA
RetentionRatiofullt <- RetentionRatiofull[,Scolfortrain:Ecolfortrain]

MAvgRetentionRatiofull <- FZEVAMAvgSKIP(input=RetentionRatiofull,maper=3,chgper=12)
MAvgRetentionRatiofull[is.na(MAvgRetentionRatiofull)|is.nan(MAvgRetentionRatiofull)|!is.finite(MAvgRetentionRatiofull)] <- NA
MAvgRetentionRatiofullt <- MAvgRetentionRatiofull[,Scolfortrain:Ecolfortrain]

ChgRetentionRatiofull <- (RetentionRatiofull/FZshiftmx(input=RetentionRatiofull, shiftcol=-12))-1
ChgRetentionRatiofull[is.na(ChgRetentionRatiofull)|is.nan(ChgRetentionRatiofull)|!is.finite(ChgRetentionRatiofull)] <- NA
ChgRetentionRatiofullt <- ChgRetentionRatiofull[,Scolfortrain:Ecolfortrain]

RetentionRatiofulltrailingmax <- FZtrailingmax(input=RetentionRatiofull)
RetentionRatiofull1perchg <- RetentionRatiofull/RetentionRatiofulltrailingmax
RetentionRatiofull1perchg[is.na(RetentionRatiofull1perchg)|is.nan(RetentionRatiofull1perchg)|!is.finite(RetentionRatiofull1perchg)] <- NA
RetentionRatiofull1perchgt <- RetentionRatiofull1perchg[,Scolfortrain:Ecolfortrain]

RetentionRatiofulllastvs3perma <- RetentionRatiofull/FZEVAMAvgSKIP(input=RetentionRatiofulltrailingmax,maper=3,chgper=12)
RetentionRatiofulllastvs3perma[is.na(RetentionRatiofulllastvs3perma)|is.nan(RetentionRatiofulllastvs3perma)|!is.finite(RetentionRatiofulllastvs3perma)] <- NA
RetentionRatiofulllastvs3permat <- RetentionRatiofulllastvs3perma[,Scolfortrain:Ecolfortrain]

RetentionRatiofull3permachg <- FZEVAMAvgSKIP(input=RetentionRatiofull1perchg,maper=3,chgper=12)
RetentionRatiofull3permachg[is.na(RetentionRatiofull3permachg)|is.nan(RetentionRatiofull3permachg)|!is.finite(RetentionRatiofull3permachg)] <- NA
RetentionRatiofull3permachgt <- RetentionRatiofull3permachg[,Scolfortrain:Ecolfortrain]

RetentionRatiofull1pchgvs3pmachg <- RetentionRatiofull1perchg/RetentionRatiofull3permachg
RetentionRatiofull1pchgvs3pmachg[is.na(RetentionRatiofull1pchgvs3pmachg)|is.nan(RetentionRatiofull1pchgvs3pmachg)|!is.finite(RetentionRatiofull1pchgvs3pmachg)] <- NA
RetentionRatiofull1pchgvs3pmachgt <- RetentionRatiofull1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
RetentionRatiofull_LTM <- RETENTIONRATIO_LTM
RetentionRatiofull_LTM[is.na(RetentionRatiofull_LTM)|is.nan(RetentionRatiofull_LTM)|!is.finite(RetentionRatiofull_LTM)] <- NA

MAvgRetentionRatiofull_LTM <- FZEVAMAvgLTM(input=RetentionRatiofull_LTM,maper=3,chgper=12)
MAvgRetentionRatiofull_LTM[is.na(MAvgRetentionRatiofull_LTM)|is.nan(MAvgRetentionRatiofull_LTM)|!is.finite(MAvgRetentionRatiofull_LTM)] <- NA

ChgRetentionRatiofull_LTM <- (RetentionRatiofull_LTM/FZshiftmx(input=RetentionRatiofull_LTM, shiftcol=-12))-1
ChgRetentionRatiofull_LTM[is.na(ChgRetentionRatiofull_LTM)|is.nan(ChgRetentionRatiofull_LTM)|!is.finite(ChgRetentionRatiofull_LTM)] <- NA

RetentionRatiofulltrailingmax_LTM <- FZtrailingmax(input=RetentionRatiofull_LTM)
RetentionRatiofull1perchg_LTM <- RetentionRatiofull_LTM/RetentionRatiofulltrailingmax_LTM
RetentionRatiofull1perchg_LTM[is.na(RetentionRatiofull1perchg_LTM)|is.nan(RetentionRatiofull1perchg_LTM)|!is.finite(RetentionRatiofull1perchg_LTM)] <- NA

RetentionRatiofulllastvs3perma_LTM <- RetentionRatiofull_LTM/FZEVAMAvgLTM(input=RetentionRatiofulltrailingmax_LTM,maper=3,chgper=12)
RetentionRatiofulllastvs3perma_LTM[is.na(RetentionRatiofulllastvs3perma_LTM)|is.nan(RetentionRatiofulllastvs3perma_LTM)|!is.finite(RetentionRatiofulllastvs3perma_LTM)] <- NA

RetentionRatiofull3permachg_LTM <- FZEVAMAvgLTM(input=RetentionRatiofull1perchg_LTM,maper=3,chgper=12)
RetentionRatiofull3permachg_LTM[is.na(RetentionRatiofull3permachg_LTM)|is.nan(RetentionRatiofull3permachg_LTM)|!is.finite(RetentionRatiofull3permachg_LTM)] <- NA

RetentionRatiofull1pchgvs3pmachg_LTM <- RetentionRatiofull1perchg_LTM/RetentionRatiofull3permachg_LTM
RetentionRatiofull1pchgvs3pmachg_LTM[is.na(RetentionRatiofull1pchgvs3pmachg_LTM)|is.nan(RetentionRatiofull1pchgvs3pmachg_LTM)|!is.finite(RetentionRatiofull1pchgvs3pmachg_LTM)] <- NA



###################### ROIC ######################
BSWCR_ANN <- (BSCURRASSETS_ANN-FZzerona(input=BSCASH_ANN)) - (BSCURRLIABS_ANN-FZzerona(input=BSDEBTSTCURRPORT_ANN))
ROIC_ANN <- (PLEBITOPER_ANN*(1-PLTAXRATE_ANN))/(BSEQTOT_ANN+FZzerona(BSDEBTLT_ANN)+FZzerona(BSDEBTST_ANN)+BSWCR_ANN)
ROICVOL_ANN <- FZhiSDSKIP(input=ROIC_ANN, maper=5,chgper=12)
ROICVOL_ANN <- apply(ROICVOL_ANN,2,function(x){
	outval <- (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
	outval
	})
ROICVOLfull <- ROICVOL_ANN
ROICVOLfull[is.na(ROICVOLfull)|is.nan(ROICVOLfull)|!is.finite(ROICVOLfull)] <- NA
ROICVOLfullt <- ROICVOLfull[,Scolfortrain:Ecolfortrain]

MAvgROICVOLfull <- FZEVAMAvgSKIP(input=ROICVOLfull,maper=3,chgper=12)
MAvgROICVOLfull[is.na(MAvgROICVOLfull)|is.nan(MAvgROICVOLfull)|!is.finite(MAvgROICVOLfull)] <- NA
MAvgROICVOLfullt <- MAvgROICVOLfull[,Scolfortrain:Ecolfortrain]

ChgROICVOLfull <- (ROICVOLfull/FZshiftmx(input=ROICVOLfull, shiftcol=-12))-1
ChgROICVOLfull[is.na(ChgROICVOLfull)|is.nan(ChgROICVOLfull)|!is.finite(ChgROICVOLfull)] <- NA
ChgROICVOLfullt <- ChgROICVOLfull[,Scolfortrain:Ecolfortrain]

ROICVOLfulltrailingmax <- FZtrailingmax(input=ROICVOLfull)
ROICVOLfull1perchg <- ROICVOLfull/ROICVOLfulltrailingmax
ROICVOLfull1perchg[is.na(ROICVOLfull1perchg)|is.nan(ROICVOLfull1perchg)|!is.finite(ROICVOLfull1perchg)] <- NA
ROICVOLfull1perchgt <- ROICVOLfull1perchg[,Scolfortrain:Ecolfortrain]

ROICVOLfulllastvs3perma <- ROICVOLfull/FZEVAMAvgSKIP(input=ROICVOLfulltrailingmax,maper=3,chgper=12)
ROICVOLfulllastvs3perma[is.na(ROICVOLfulllastvs3perma)|is.nan(ChgROICVOLfull)|!is.finite(ChgROICVOLfull)] <- NA
ROICVOLfulllastvs3permat <- ROICVOLfulllastvs3perma[,Scolfortrain:Ecolfortrain]

ROICVOLfull3permachg <- FZEVAMAvgSKIP(input=ROICVOLfull1perchg,maper=3,chgper=12)
ROICVOLfull3permachg[is.na(ROICVOLfull3permachg)|is.nan(ROICVOLfull3permachg)|!is.finite(ROICVOLfull3permachg)] <- NA
ROICVOLfull3permachgt <- ROICVOLfull3permachg[,Scolfortrain:Ecolfortrain]

ROICVOLfull1pchgvs3pmachg <- ROICVOLfull1perchg/ROICVOLfull3permachg
ROICVOLfull1pchgvs3pmachg[is.na(ROICVOLfull1pchgvs3pmachg)|is.nan(ROICVOLfull1pchgvs3pmachg)|!is.finite(ROICVOLfull1pchgvs3pmachg)] <- NA
ROICVOLfull1pchgvs3pmachgt <- ROICVOLfull1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
BSWCR_LTM <- (BSCURRASSETS_LTM-FZzerona(input=BSCASH_LTM)) - (BSCURRLIABS_LTM-FZzerona(input=BSDEBTSTCURRPORT_LTM))
ROIC_LTM <- (PLEBITOPER_LTM*(1-PLTAXRATE_LTM))/(BSEQTOT_LTM+FZzerona(BSDEBTLT_LTM)+FZzerona(BSDEBTST_LTM)+BSWCR_LTM)

ROICVOL_LTM <- FZmovingSD(input=ROIC_LTM, sdper=5)
ROICVOL_LTM <- apply(ROICVOL_LTM,2,function(x){
	outval <- (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
	outval
	})
ROICVOLfull_LTM <- ROICVOL_LTM
ROICVOLfull_LTM[is.na(ROICVOLfull_LTM)|is.nan(ROICVOLfull_LTM)|!is.finite(ROICVOLfull_LTM)] <- NA

MAvgROICVOLfull_LTM <- FZEVAMAvgLTM(input=ROICVOLfull_LTM,maper=3,chgper=12)
MAvgROICVOLfull_LTM[is.na(MAvgROICVOLfull_LTM)|is.nan(MAvgROICVOLfull_LTM)|!is.finite(MAvgROICVOLfull_LTM)] <- NA

ChgROICVOLfull_LTM <- (ROICVOLfull_LTM/FZshiftmx(input=ROICVOLfull_LTM, shiftcol=-12))-1
ChgROICVOLfull_LTM[is.na(ChgROICVOLfull_LTM)|is.nan(ChgROICVOLfull_LTM)|!is.finite(ChgROICVOLfull_LTM)] <- NA

ROICVOLfulltrailingmax_LTM <- FZtrailingmax(input=ROICVOLfull_LTM)
ROICVOLfull1perchg_LTM <- ROICVOLfull_LTM/ROICVOLfulltrailingmax_LTM
ROICVOLfull1perchg_LTM[is.na(ROICVOLfull1perchg_LTM)|is.nan(ROICVOLfull1perchg_LTM)|!is.finite(ROICVOLfull1perchg_LTM)] <- NA

ROICVOLfulllastvs3perma_LTM <- ROICVOLfull_LTM/FZEVAMAvgLTM(input=ROICVOLfulltrailingmax_LTM,maper=3,chgper=12)
ROICVOLfulllastvs3perma_LTM[is.na(ROICVOLfulllastvs3perma_LTM)|is.nan(ROICVOLfulllastvs3perma_LTM)|!is.finite(ROICVOLfulllastvs3perma_LTM)] <- NA

ROICVOLfull3permachg_LTM <- FZEVAMAvgLTM(input=ROICVOLfull1perchg_LTM,maper=3,chgper=12)
ROICVOLfull3permachg_LTM[is.na(ROICVOLfull3permachg_LTM)|is.nan(ROICVOLfull3permachg_LTM)|!is.finite(ROICVOLfull3permachg_LTM)] <- NA

ROICVOLfull1pchgvs3pmachg_LTM <- ROICVOLfull1perchg_LTM/ROICVOLfull3permachg_LTM
ROICVOLfull1pchgvs3pmachg_LTM[is.na(ROICVOLfull1pchgvs3pmachg_LTM)|is.nan(ROICVOLfull1pchgvs3pmachg_LTM)|!is.finite(ROICVOLfull1pchgvs3pmachg_LTM)] <- NA





###################### SALESGROWTH SD ######################
SALESSD_ANN <- FZhiSDSKIP(input=(PLSALES_ANN/FZshiftmx(input=PLSALES_ANN,shiftcol=-12))-1, maper=5,chgper=12)
SALESSD_ANN <- FZtrimvals(input=SALESSD_ANN,trimval=3,naval=TRUE)
SALESSD_ANN <- apply(SALESSD_ANN,2,function(x){
	outval <- (x-median(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
	outval
	})
SALESSD_ANN <- FZadjustvarbypara(inputvar=SALESSD_ANN,paramx=Country, Adjfac=0.5)
SALESSDfull <- SALESSD_ANN
SALESSDfull[is.na(SALESSDfull)|is.nan(SALESSDfull)|!is.finite(SALESSDfull)] <- NA
SALESSDfullt <- SALESSDfull[,Scolfortrain:Ecolfortrain]

MAvgSALESSDfull <- FZEVAMAvgSKIP(input=SALESSDfull,maper=3,chgper=12)
MAvgSALESSDfull[is.na(MAvgSALESSDfull)|is.nan(MAvgSALESSDfull)|!is.finite(MAvgSALESSDfull)] <- NA
MAvgSALESSDfullt <- MAvgSALESSDfull[,Scolfortrain:Ecolfortrain]

ChgSALESSDfull <- (SALESSDfull/FZshiftmx(input=SALESSDfull, shiftcol=-12))-1
ChgSALESSDfull[is.na(ChgSALESSDfull)|is.nan(ChgSALESSDfull)|!is.finite(ChgSALESSDfull)] <- NA
ChgSALESSDfullt <- ChgSALESSDfull[,Scolfortrain:Ecolfortrain]

SALESSDfulltrailingmax <- FZtrailingmax(input=SALESSDfull)
SALESSDfull1perchg <- SALESSDfull/SALESSDfulltrailingmax
SALESSDfull1perchg[is.na(SALESSDfull1perchg)|is.nan(SALESSDfull1perchg)|!is.finite(SALESSDfull1perchg)] <- NA
SALESSDfull1perchgt <- SALESSDfull1perchg[,Scolfortrain:Ecolfortrain]

SALESSDfulllastvs3perma <- SALESSDfull/FZEVAMAvgSKIP(input=SALESSDfulltrailingmax,maper=3,chgper=12)
SALESSDfulllastvs3perma[is.na(SALESSDfulllastvs3perma)|is.nan(SALESSDfulllastvs3perma)|!is.finite(SALESSDfulllastvs3perma)] <- NA
SALESSDfulllastvs3permat <- SALESSDfulllastvs3perma[,Scolfortrain:Ecolfortrain]

SALESSDfull3permachg <- FZEVAMAvgSKIP(input=SALESSDfull1perchg,maper=3,chgper=12)
SALESSDfull3permachg[is.na(SALESSDfull3permachg)|is.nan(SALESSDfull3permachg)|!is.finite(SALESSDfull3permachg)] <- NA
SALESSDfull3permachgt <- SALESSDfull3permachg[,Scolfortrain:Ecolfortrain]

SALESSDfull1pchgvs3pmachg <- SALESSDfull1perchg/SALESSDfull3permachg
SALESSDfull1pchgvs3pmachg[is.na(SALESSDfull1pchgvs3pmachg)|is.nan(SALESSDfull1pchgvs3pmachg)|!is.finite(SALESSDfull1pchgvs3pmachg)] <- NA
SALESSDfull1pchgvs3pmachgt <- SALESSDfull1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
SALESSD_LTM <- FZmovingSD(input=(PLSALES_LTM/FZshiftmx(input=PLSALES_LTM,shiftcol=-12))-1, sdper=5)
SALESSD_LTM <- FZtrimvals(input=SALESSD_LTM,trimval=3,naval=TRUE)
SALESSD_LTM <- apply(SALESSD_LTM,2,function(x){
	outval <- (x-median(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
	outval
	})
SALESSD_LTM <- FZadjustvarbypara(inputvar=SALESSD_LTM,paramx=Country, Adjfac=0.5)
SALESSDfull_LTM <- SALESSD_LTM
SALESSDfull_LTM[is.na(SALESSDfull_LTM)|is.nan(SALESSDfull_LTM)|!is.finite(SALESSDfull_LTM)] <- NA

MAvgSALESSDfull_LTM <- FZEVAMAvgLTM(input=SALESSDfull_LTM,maper=3,chgper=12)
MAvgSALESSDfull_LTM[is.na(MAvgSALESSDfull_LTM)|is.nan(MAvgSALESSDfull_LTM)|!is.finite(MAvgSALESSDfull_LTM)] <- NA

ChgSALESSDfull_LTM <- (SALESSDfull_LTM/FZshiftmx(input=SALESSDfull_LTM, shiftcol=-12))-1
ChgSALESSDfull_LTM[is.na(ChgSALESSDfull_LTM)|is.nan(ChgSALESSDfull_LTM)|!is.finite(ChgSALESSDfull_LTM)] <- NA

SALESSDfulltrailingmax_LTM <- FZtrailingmax(input=SALESSDfull_LTM)
SALESSDfull1perchg_LTM <- SALESSDfull_LTM/SALESSDfulltrailingmax_LTM
SALESSDfull1perchg_LTM[is.na(SALESSDfull1perchg_LTM)|is.nan(SALESSDfull1perchg_LTM)|!is.finite(SALESSDfull1perchg_LTM)] <- NA

SALESSDfulllastvs3perma_LTM <- SALESSDfull_LTM/FZEVAMAvgLTM(input=SALESSDfulltrailingmax_LTM,maper=3,chgper=12)
SALESSDfulllastvs3perma_LTM[is.na(SALESSDfulllastvs3perma_LTM)|is.nan(SALESSDfulllastvs3perma_LTM)|!is.finite(SALESSDfulllastvs3perma_LTM)] <- NA

SALESSDfull3permachg_LTM <- FZEVAMAvgLTM(input=SALESSDfull1perchg_LTM,maper=3,chgper=12)
SALESSDfull3permachg_LTM[is.na(SALESSDfull3permachg_LTM)|is.nan(SALESSDfull3permachg_LTM)|!is.finite(SALESSDfull3permachg_LTM)] <- NA

SALESSDfull1pchgvs3pmachg_LTM <- SALESSDfull1perchg_LTM/SALESSDfull3permachg_LTM
SALESSDfull1pchgvs3pmachg_LTM[is.na(SALESSDfull1pchgvs3pmachg_LTM)|is.nan(SALESSDfull1pchgvs3pmachg_LTM)|!is.finite(SALESSDfull1pchgvs3pmachg_LTM)] <- NA



###################### OperatingLeverage ######################
OperatingLeveragefull <- ((PLEBITOPER_ANN-PLGROSSINC_ANN)/PLCOGSXDEP_ANN)
OperatingLeveragefull[is.na(OperatingLeveragefull)|is.nan(OperatingLeveragefull)|!is.finite(OperatingLeveragefull)] <- NA
OperatingLeveragefullt <- OperatingLeveragefull[,Scolfortrain:Ecolfortrain]

MAvgOperatingLeveragefull <- FZEVAMAvgSKIP(input=OperatingLeveragefull,maper=3,chgper=12)
MAvgOperatingLeveragefull[is.na(MAvgOperatingLeveragefull)|is.nan(MAvgOperatingLeveragefull)|!is.finite(MAvgOperatingLeveragefull)] <- NA
MAvgOperatingLeveragefullt <- MAvgOperatingLeveragefull[,Scolfortrain:Ecolfortrain]

ChgOperatingLeveragefull <- (OperatingLeveragefull/FZshiftmx(input=OperatingLeveragefull, shiftcol=-12))-1
ChgOperatingLeveragefull[is.na(ChgOperatingLeveragefull)|is.nan(ChgOperatingLeveragefull)|!is.finite(ChgOperatingLeveragefull)] <- NA
ChgOperatingLeveragefullt <- ChgOperatingLeveragefull[,Scolfortrain:Ecolfortrain]

OperatingLeveragefulltrailingmax <- FZtrailingmax(input=OperatingLeveragefull)
OperatingLeveragefullGrowth1perchg <- OperatingLeveragefull/OperatingLeveragefulltrailingmax
OperatingLeveragefullGrowth1perchg[is.na(OperatingLeveragefullGrowth1perchg)|is.nan(OperatingLeveragefullGrowth1perchg)|!is.finite(OperatingLeveragefullGrowth1perchg)] <- NA
OperatingLeveragefullGrowth1perchgt <- OperatingLeveragefullGrowth1perchg[,Scolfortrain:Ecolfortrain]

OperatingLeveragefullGrowthlastvs3perma <- OperatingLeveragefull/FZEVAMAvgSKIP(input=OperatingLeveragefulltrailingmax,maper=3,chgper=12)
OperatingLeveragefullGrowthlastvs3perma[is.na(OperatingLeveragefullGrowthlastvs3perma)|is.nan(OperatingLeveragefullGrowthlastvs3perma)|!is.finite(OperatingLeveragefullGrowthlastvs3perma)] <- NA
OperatingLeveragefullGrowthlastvs3permat <- OperatingLeveragefullGrowthlastvs3perma[,Scolfortrain:Ecolfortrain]

OperatingLeveragefullGrowth3permachg <- FZEVAMAvgSKIP(input=OperatingLeveragefullGrowth1perchg,maper=3,chgper=12)
OperatingLeveragefullGrowth3permachg[is.na(OperatingLeveragefullGrowth3permachg)|is.nan(OperatingLeveragefullGrowth3permachg)|!is.finite(OperatingLeveragefullGrowth3permachg)] <- NA
OperatingLeveragefullGrowth3permachgt <- OperatingLeveragefullGrowth3permachg[,Scolfortrain:Ecolfortrain]

OperatingLeveragefullGrowth1pchgvs3pmachg <- OperatingLeveragefullGrowth1perchg/OperatingLeveragefullGrowth3permachg
OperatingLeveragefullGrowth1pchgvs3pmachg[is.na(OperatingLeveragefullGrowth1pchgvs3pmachg)|is.nan(OperatingLeveragefullGrowth1pchgvs3pmachg)|!is.finite(OperatingLeveragefullGrowth1pchgvs3pmachg)] <- NA
OperatingLeveragefullGrowth1pchgvs3pmachgt <- OperatingLeveragefullGrowth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
PLGROSSINC_LTM <- PLSALES_LTM - PLCOGSXDEP_LTM
OperatingLeveragefull_LTM <- ((PLEBITOPER_LTM-PLGROSSINC_LTM)/PLCOGSXDEP_LTM)
OperatingLeveragefull_LTM[is.na(OperatingLeveragefull_LTM)|is.nan(OperatingLeveragefull_LTM)|!is.finite(OperatingLeveragefull_LTM)] <- NA

MAvgOperatingLeveragefull_LTM <- FZEVAMAvgLTM(input=OperatingLeveragefull_LTM,maper=3,chgper=12)
MAvgOperatingLeveragefull_LTM[is.na(MAvgOperatingLeveragefull_LTM)|is.nan(MAvgOperatingLeveragefull_LTM)|!is.finite(MAvgOperatingLeveragefull_LTM)] <- NA

ChgOperatingLeveragefull_LTM <- (OperatingLeveragefull_LTM/FZshiftmx(input=OperatingLeveragefull_LTM, shiftcol=-12))-1
ChgOperatingLeveragefull_LTM[is.na(ChgOperatingLeveragefull_LTM)|is.nan(ChgOperatingLeveragefull_LTM)|!is.finite(ChgOperatingLeveragefull_LTM)] <- NA

OperatingLeveragefulltrailingmax_LTM <- FZtrailingmax(input=OperatingLeveragefull_LTM)
OperatingLeveragefullGrowth1perchg_LTM <- OperatingLeveragefull_LTM/OperatingLeveragefulltrailingmax_LTM
OperatingLeveragefullGrowth1perchg_LTM[is.na(OperatingLeveragefullGrowth1perchg_LTM)|is.nan(OperatingLeveragefullGrowth1perchg_LTM)|!is.finite(OperatingLeveragefullGrowth1perchg_LTM)] <- NA

OperatingLeveragefullGrowthlastvs3perma_LTM <- OperatingLeveragefull_LTM/FZEVAMAvgLTM(input=OperatingLeveragefulltrailingmax_LTM,maper=3,chgper=12)
OperatingLeveragefullGrowthlastvs3perma_LTM[is.na(OperatingLeveragefullGrowthlastvs3perma_LTM)|is.nan(OperatingLeveragefullGrowthlastvs3perma_LTM)|!is.finite(OperatingLeveragefullGrowthlastvs3perma_LTM)] <- NA

OperatingLeveragefullGrowth3permachg_LTM <- FZEVAMAvgLTM(input=OperatingLeveragefullGrowth1perchg_LTM,maper=3,chgper=12)
OperatingLeveragefullGrowth3permachg_LTM[is.na(OperatingLeveragefullGrowth3permachg_LTM)|is.nan(OperatingLeveragefullGrowth3permachg_LTM)|!is.finite(OperatingLeveragefullGrowth3permachg_LTM)] <- NA

OperatingLeveragefullGrowth1pchgvs3pmachg_LTM <- OperatingLeveragefullGrowth1perchg_LTM/OperatingLeveragefullGrowth3permachg_LTM
OperatingLeveragefullGrowth1pchgvs3pmachg_LTM[is.na(OperatingLeveragefullGrowth1pchgvs3pmachg_LTM)|is.nan(OperatingLeveragefullGrowth1pchgvs3pmachg_LTM)|!is.finite(OperatingLeveragefullGrowth1pchgvs3pmachg_LTM)] <- NA





###################### Reinvestment ######################
# Reinvestment
#(capex + capitalised R&d (cum sum of R&D/3)+ change in WCR())/(NetPPE+FZcapplitemBSitemEVA+WC(CA-CL)
CapitalisedRnD_ANN <- FZcummx(input=FZcummx(input=PLRDEXP_ANN/3))
WCRchange_ANN <- ((BSWCR_ANN-FZshiftmx(input=BSWCR_ANN,shiftcol=-12))-1)
CapitalisedBSRnD_ANN <- FZcapplitemBSitem(input=PLRDEXP_ANN, capper = 3)
Reinvestmentfull <- ((CFCAPEX_ANN + CapitalisedRnD_ANN + WCRchange_ANN)/(BSPPENET_ANN + CapitalisedBSRnD_ANN + BSWCR_ANN))
Reinvestmentfull[is.na(Reinvestmentfull)|is.nan(Reinvestmentfull)|!is.finite(Reinvestmentfull)] <- NA
Reinvestmentfullt <- Reinvestmentfull[,Scolfortrain:Ecolfortrain]

MAvgReinvestmentfull <- FZEVAMAvgSKIP(input=Reinvestmentfull,maper=3,chgper=12)
MAvgReinvestmentfull[is.na(MAvgReinvestmentfull)|is.nan(MAvgReinvestmentfull)|!is.finite(MAvgReinvestmentfull)] <- NA
MAvgReinvestmentfullt <- MAvgReinvestmentfull[,Scolfortrain:Ecolfortrain]

ChgReinvestmentfull <- (Reinvestmentfull/FZshiftmx(input=Reinvestmentfull, shiftcol=-12))-1
ChgReinvestmentfull[is.na(ChgReinvestmentfull)|is.nan(ChgReinvestmentfull)|!is.finite(ChgReinvestmentfull)] <- NA
ChgReinvestmentfullt <- ChgReinvestmentfull[,Scolfortrain:Ecolfortrain]

Reinvestmentfulltrailingmax <- FZtrailingmax(input=Reinvestmentfull)
ReinvestmentfullGrowth1perchg <- Reinvestmentfull/Reinvestmentfulltrailingmax
ReinvestmentfullGrowth1perchg[is.na(ReinvestmentfullGrowth1perchg)|is.nan(ReinvestmentfullGrowth1perchg)|!is.finite(ReinvestmentfullGrowth1perchg)] <- NA
ReinvestmentfullGrowth1perchgt <- ReinvestmentfullGrowth1perchg[,Scolfortrain:Ecolfortrain]

ReinvestmentfullGrowthlastvs3perma <- Reinvestmentfull/FZEVAMAvgSKIP(input=Reinvestmentfulltrailingmax,maper=3,chgper=12)
ReinvestmentfullGrowthlastvs3perma[is.na(ReinvestmentfullGrowthlastvs3perma)|is.nan(ReinvestmentfullGrowthlastvs3perma)|!is.finite(ReinvestmentfullGrowthlastvs3perma)] <- NA
ReinvestmentfullGrowthlastvs3permat <- ReinvestmentfullGrowthlastvs3perma[,Scolfortrain:Ecolfortrain]

ReinvestmentfullGrowth3permachg <- FZEVAMAvgSKIP(input=ReinvestmentfullGrowth1perchg,maper=3,chgper=12)
ReinvestmentfullGrowth3permachg[is.na(ReinvestmentfullGrowth3permachg)|is.nan(ReinvestmentfullGrowth3permachg)|!is.finite(ReinvestmentfullGrowth3permachg)] <- NA
ReinvestmentfullGrowth3permachgt <- ReinvestmentfullGrowth3permachg[,Scolfortrain:Ecolfortrain]

ReinvestmentfullGrowth1pchgvs3pmachg <- ReinvestmentfullGrowth1perchg/ReinvestmentfullGrowth3permachg
ReinvestmentfullGrowth1pchgvs3pmachg[is.na(ReinvestmentfullGrowth1pchgvs3pmachg)|is.nan(ReinvestmentfullGrowth1pchgvs3pmachg)|!is.finite(ReinvestmentfullGrowth1pchgvs3pmachg)] <- NA
ReinvestmentfullGrowth1pchgvs3pmachgt <- ReinvestmentfullGrowth1pchgvs3pmachg[,Scolfortrain:Ecolfortrain]

####
CapitalisedRnD_LTM <- FZhicummxLTM(input=FZhicummxLTM(input=PLRDEXP_LTM/3,per=12),per=12)
WCRchange_LTM <- ((BSWCR_LTM-FZshiftmx(input=BSWCR_LTM,shiftcol=-12))-1)
CapitalisedBSRnD_LTM <- FZcapplitemBSitemLTM(input=PLRDEXP_LTM, capper = 3,shiftper=12)
Reinvestmentfull_LTM <- ((CFCAPEX_LTM + CapitalisedRnD_LTM + WCRchange_LTM)/(BSPPENET_LTM + CapitalisedBSRnD_LTM + BSWCR_LTM))
Reinvestmentfull_LTM[is.na(Reinvestmentfull_LTM)|is.nan(Reinvestmentfull_LTM)|!is.finite(Reinvestmentfull_LTM)] <- NA

MAvgReinvestmentfull_LTM <- FZEVAMAvgLTM(input=Reinvestmentfull_LTM,maper=3,chgper=12)
MAvgReinvestmentfull_LTM[is.na(MAvgReinvestmentfull_LTM)|is.nan(MAvgReinvestmentfull_LTM)|!is.finite(MAvgReinvestmentfull_LTM)] <- NA

ChgReinvestmentfull_LTM <- (Reinvestmentfull_LTM/FZshiftmx(input=Reinvestmentfull_LTM, shiftcol=-12))-1
ChgReinvestmentfull_LTM[is.na(ChgReinvestmentfull_LTM)|is.nan(ChgReinvestmentfull_LTM)|!is.finite(ChgReinvestmentfull_LTM)] <- NA

Reinvestmentfulltrailingmax_LTM <- FZtrailingmax(input=Reinvestmentfull_LTM)
ReinvestmentfullGrowth1perchg_LTM <- Reinvestmentfull_LTM/Reinvestmentfulltrailingmax_LTM
ReinvestmentfullGrowth1perchg_LTM[is.na(ReinvestmentfullGrowth1perchg_LTM)|is.nan(ReinvestmentfullGrowth1perchg_LTM)|!is.finite(ReinvestmentfullGrowth1perchg_LTM)] <- NA

ReinvestmentfullGrowthlastvs3perma_LTM <- Reinvestmentfull_LTM/FZEVAMAvgLTM(input=Reinvestmentfulltrailingmax_LTM,maper=3,chgper=12)
ReinvestmentfullGrowthlastvs3perma_LTM[is.na(ReinvestmentfullGrowthlastvs3perma_LTM)|is.nan(ReinvestmentfullGrowthlastvs3perma_LTM)|!is.finite(ReinvestmentfullGrowthlastvs3perma_LTM)] <- NA

ReinvestmentfullGrowth3permachg_LTM <- FZEVAMAvgLTM(input=ReinvestmentfullGrowth1perchg_LTM,maper=3,chgper=12)
ReinvestmentfullGrowth3permachg_LTM[is.na(ReinvestmentfullGrowth3permachg_LTM)|is.nan(ReinvestmentfullGrowth3permachg_LTM)|!is.finite(ReinvestmentfullGrowth3permachg_LTM)] <- NA

ReinvestmentfullGrowth1pchgvs3pmachg_LTM <- ReinvestmentfullGrowth1perchg_LTM/ReinvestmentfullGrowth3permachg_LTM
ReinvestmentfullGrowth1pchgvs3pmachg_LTM[is.na(ReinvestmentfullGrowth1pchgvs3pmachg_LTM)|is.nan(ReinvestmentfullGrowth1pchgvs3pmachg_LTM)|!is.finite(ReinvestmentfullGrowth1pchgvs3pmachg_LTM)] <- NA


##############################################
mxinf_ANN <- matrix(NA, nrow=nrow(Inflationraw_ANN), ncol=ncol(Inflationraw_ANN), dimnames = dimnames(Inflationraw_ANN))
INFLATION_ANN <- sapply(1:ncol(mxinf_ANN),function(x){
			tempinf <- Inflationraw_ANN[match(Country,gsub("[.]", " ",rownames(Inflationraw_ANN))),x]
			tempinf
			})
rownames(INFLATION_ANN) <- rownames(Country)
PCv1nInflation <- PCv1+INFLATION_ANN
PCv1nInflation[is.na(PCv1nInflation)|is.nan(PCv1nInflation)|!is.finite(PCv1nInflation)] <- NA
PCv1nInflationt <- PCv1nInflation[,Scolfortrain:Ecolfortrain]

BSWCR_ANN <- (BSCURRASSETS_ANN-FZzerona(input=BSCASH_ANN)) - (BSCURRLIABS_ANN-FZzerona(input=BSDEBTSTCURRPORT_ANN))

PCv1nInflationWCR <- PCv1nInflation+BSWCR_ANN
PCv1nInflationWCR[is.na(PCv1nInflationWCR)|is.nan(PCv1nInflationWCR)|!is.finite(PCv1nInflationWCR)] <- NA
PCv1nInflationWCRt <- PCv1nInflationWCR[,Scolfortrain:Ecolfortrain]

SalesvsPCv1 <- PLSALES_ANN/PCv1
SalesvsPCv1[is.na(SalesvsPCv1)|is.nan(SalesvsPCv1)|!is.finite(SalesvsPCv1)] <- NA
SalesvsPCv1t <- SalesvsPCv1[,Scolfortrain:Ecolfortrain]

SalesvsPCv1nInflation <- PLSALES_ANN/PCv1nInflation
SalesvsPCv1nInflation[is.na(SalesvsPCv1nInflation)|is.nan(SalesvsPCv1nInflation)|!is.finite(SalesvsPCv1nInflation)] <- NA
SalesvsPCv1nInflationt <- SalesvsPCv1nInflation[,Scolfortrain:Ecolfortrain]

SalesvsPCv1nInflationWCR <- PLSALES_ANN/PCv1nInflationWCR
SalesvsPCv1nInflationWCR[is.na(SalesvsPCv1nInflationWCR)|is.nan(SalesvsPCv1nInflationWCR)|!is.finite(SalesvsPCv1nInflationWCR)] <- NA
SalesvsPCv1nInflationWCRt <- SalesvsPCv1nInflationWCR[,Scolfortrain:Ecolfortrain]

MAvgSalesvsPCv1 <- FZEVAMAvgSKIP(input=SalesvsPCv1,maper=3,chgper=12)
MAvgSalesvsPCv1[is.na(MAvgSalesvsPCv1)|is.nan(MAvgSalesvsPCv1)|!is.finite(MAvgSalesvsPCv1)] <- NA
MAvgSalesvsPCv1t <- MAvgSalesvsPCv1[,Scolfortrain:Ecolfortrain]

MAvgSalesvsPCv1nInflation <- FZEVAMAvgSKIP(input=SalesvsPCv1nInflation,maper=3,chgper=12)
MAvgSalesvsPCv1nInflation[is.na(MAvgSalesvsPCv1nInflation)|is.nan(MAvgSalesvsPCv1nInflation)|!is.finite(MAvgSalesvsPCv1nInflation)] <- NA
MAvgSalesvsPCv1nInflationt <- MAvgSalesvsPCv1nInflation[,Scolfortrain:Ecolfortrain]

MAvgSalesvsPCv1nInflationWCR <- FZEVAMAvgSKIP(input=SalesvsPCv1nInflationWCR,maper=3,chgper=12)
MAvgSalesvsPCv1nInflationWCR[is.na(MAvgSalesvsPCv1nInflationWCR)|is.nan(MAvgSalesvsPCv1nInflationWCR)|!is.finite(MAvgSalesvsPCv1nInflationWCR)] <- NA
MAvgSalesvsPCv1nInflationWCRt <- MAvgSalesvsPCv1nInflationWCR[,Scolfortrain:Ecolfortrain]

####
mxinf_LTM <- matrix(NA, nrow=nrow(Inflationraw_LTM), ncol=ncol(Inflationraw_LTM), dimnames = dimnames(Inflationraw_LTM))
INFLATION_LTM <- sapply(1:ncol(mxinf_LTM),function(x){
			tempinf <- Inflationraw_LTM[match(Country,gsub("[.]", " ",rownames(Inflationraw_ANN))),x]
			tempinf
			})

rownames(INFLATION_LTM) <- rownames(Country)
PCv1nInflation_LTM <- PCv1_LTM+INFLATION_LTM
PCv1nInflation_LTM[is.na(PCv1nInflation_LTM)|is.nan(PCv1nInflation_LTM)|!is.finite(PCv1nInflation_LTM)] <- NA

BSWCR_LTM <- (BSCURRASSETS_LTM-FZzerona(input=BSCASH_LTM)) - (BSCURRLIABS_LTM-FZzerona(input=BSDEBTSTCURRPORT_LTM))

PCv1nInflationWCR_LTM <- PCv1nInflation_LTM+BSWCR_LTM
PCv1nInflationWCR_LTM[is.na(PCv1nInflationWCR_LTM)|is.nan(PCv1nInflationWCR_LTM)|!is.finite(PCv1nInflationWCR_LTM)] <- NA

SalesvsPCv1_LTM <- PLSALES_LTM/PCv1_LTM
SalesvsPCv1_LTM[is.na(SalesvsPCv1_LTM)|is.nan(SalesvsPCv1_LTM)|!is.finite(SalesvsPCv1_LTM)] <- NA

SalesvsPCv1nInflation_LTM <- PLSALES_LTM/PCv1nInflation_LTM
SalesvsPCv1nInflation_LTM[is.na(SalesvsPCv1nInflation_LTM)|is.nan(SalesvsPCv1nInflation_LTM)|!is.finite(SalesvsPCv1nInflation_LTM)] <- NA

SalesvsPCv1nInflationWCR_LTM <- PLSALES_LTM/PCv1nInflationWCR_LTM
SalesvsPCv1nInflationWCR_LTM[is.na(SalesvsPCv1nInflationWCR_LTM)|is.nan(SalesvsPCv1nInflationWCR_LTM)|!is.finite(SalesvsPCv1nInflationWCR_LTM)] <- NA

MAvgSalesvsPCv1_LTM <- FZEVAMAvgLTM(input=SalesvsPCv1_LTM,maper=3,chgper=12)
MAvgSalesvsPCv1_LTM[is.na(MAvgSalesvsPCv1_LTM)|is.nan(MAvgSalesvsPCv1_LTM)|!is.finite(MAvgSalesvsPCv1_LTM)] <- NA

MAvgSalesvsPCv1nInflation_LTM <- FZEVAMAvgLTM(input=SalesvsPCv1nInflation_LTM,maper=3,chgper=12)
MAvgSalesvsPCv1nInflation_LTM[is.na(MAvgSalesvsPCv1nInflation_LTM)|is.nan(MAvgSalesvsPCv1nInflation_LTM)|!is.finite(MAvgSalesvsPCv1nInflation_LTM)] <- NA

MAvgSalesvsPCv1nInflationWCR_LTM <- FZEVAMAvgLTM(input=SalesvsPCv1nInflationWCR_LTM,maper=3,chgper=12)
MAvgSalesvsPCv1nInflationWCR_LTM[is.na(MAvgSalesvsPCv1nInflationWCR_LTM)|is.nan(MAvgSalesvsPCv1nInflationWCR_LTM)|!is.finite(MAvgSalesvsPCv1nInflationWCR_LTM)] <- NA





#############################################################################################
# Retention Ratio
RETENTIONRATIO_ANN <- 1-(FZzerona(input=FSPAYOUTRATIO_ANN)/100) # 1 - Payout Ratio from FS
RETENTIONRATIO_ANN[!is.na(RETENTIONRATIO_ANN) & RETENTIONRATIO_ANN < 0] <- NA
RETENTIONRATIO_ANN[!is.na(RETENTIONRATIO_ANN) & RETENTIONRATIO_ANN > 1] <- 1

# Leverage
Leverage_ANN <- (FZzerona(BSDEBTST_ANN)+FZzerona(BSDEBTLT_ANN))/(FZzerona(BSDEBTST_ANN)+FZzerona(BSDEBTLT_ANN)+BSEQTOT_ANN)

RSalesvsPCv1 <- SalesvsPCv1*RETENTIONRATIO_ANN*Leverage_ANN
RSalesvsPCv1[is.na(RSalesvsPCv1)|is.nan(RSalesvsPCv1)|!is.finite(RSalesvsPCv1)] <- NA
RSalesvsPCv1t <- RSalesvsPCv1[,Scolfortrain:Ecolfortrain]

RSalesvsPCv1nInflation <- SalesvsPCv1nInflation*RETENTIONRATIO_ANN*Leverage_ANN
RSalesvsPCv1nInflation[is.na(RSalesvsPCv1nInflation)|is.nan(RSalesvsPCv1nInflation)|!is.finite(RSalesvsPCv1nInflation)] <- NA
RSalesvsPCv1nInflationt <- RSalesvsPCv1nInflation[,Scolfortrain:Ecolfortrain]

RSalesvsPCv1nInflationWCR <- SalesvsPCv1nInflationWCR*RETENTIONRATIO_ANN*Leverage_ANN
RSalesvsPCv1nInflationWCR[is.na(RSalesvsPCv1nInflationWCR)|is.nan(RSalesvsPCv1nInflationWCR)|!is.finite(RSalesvsPCv1nInflationWCR)] <- NA
RSalesvsPCv1nInflationWCRt <- RSalesvsPCv1nInflationWCR[,Scolfortrain:Ecolfortrain]

RMAvgPCv1 <- MAvgSalesvsPCv1*RETENTIONRATIO_ANN*Leverage_ANN
RMAvgPCv1[is.na(RMAvgPCv1)|is.nan(RMAvgPCv1)|!is.finite(RMAvgPCv1)] <- NA
RMAvgPCv1t <- RMAvgPCv1[,Scolfortrain:Ecolfortrain]

RMAvgPCv1nInflation <- MAvgSalesvsPCv1nInflation*RETENTIONRATIO_ANN*Leverage_ANN
RMAvgPCv1nInflation[is.na(RMAvgPCv1nInflation)|is.nan(RMAvgPCv1nInflation)|!is.finite(RMAvgPCv1nInflation)] <- NA
RMAvgPCv1nInflationt <- RMAvgPCv1nInflation[,Scolfortrain:Ecolfortrain]

RMAvgPCv1nInflationWCR <- MAvgSalesvsPCv1nInflationWCR*RETENTIONRATIO_ANN*Leverage_ANN
RMAvgPCv1nInflationWCR[is.na(RMAvgPCv1nInflationWCR)|is.nan(RMAvgPCv1nInflationWCR)|!is.finite(RMAvgPCv1nInflationWCR)] <- NA
RMAvgPCv1nInflationWCRt <- RMAvgPCv1nInflationWCR[,Scolfortrain:Ecolfortrain]

####
# Retention Ratio
RETENTIONRATIO_LTM <- 1-(FZzerona(input=FSPAYOUTRATIO_LTM)/100) # 1 - Payout Ratio from FS

#apply(RETENTIONRATIO_LTM,2,FUN=function(x){length(x[!is.na(x) & x>1])})

RETENTIONRATIO_LTM[!is.na(RETENTIONRATIO_LTM) & RETENTIONRATIO_LTM < 0] <- NA
RETENTIONRATIO_LTM[!is.na(RETENTIONRATIO_LTM) & RETENTIONRATIO_LTM > 1] <- 1

# Leverage
Leverage_LTM <- (FZzerona(BSDEBTST_LTM)+FZzerona(BSDEBTLT_LTM))/(FZzerona(BSDEBTST_LTM)+FZzerona(BSDEBTLT_LTM)+BSEQTOT_LTM)

RSalesvsPCv1_LTM <- SalesvsPCv1_LTM*RETENTIONRATIO_LTM*Leverage_LTM
RSalesvsPCv1_LTM[is.na(RSalesvsPCv1_LTM)|is.nan(RSalesvsPCv1_LTM)|!is.finite(RSalesvsPCv1_LTM)] <- NA

RSalesvsPCv1nInflation_LTM <- SalesvsPCv1nInflation_LTM*RETENTIONRATIO_LTM*Leverage_LTM
RSalesvsPCv1nInflation_LTM[is.na(RSalesvsPCv1nInflation_LTM)|is.nan(RSalesvsPCv1nInflation_LTM)|!is.finite(RSalesvsPCv1nInflation_LTM)] <- NA

RSalesvsPCv1nInflationWCR_LTM <- SalesvsPCv1nInflationWCR_LTM*RETENTIONRATIO_LTM*Leverage_LTM
RSalesvsPCv1nInflationWCR_LTM[is.na(RSalesvsPCv1nInflationWCR_LTM)|is.nan(RSalesvsPCv1nInflationWCR_LTM)|!is.finite(RSalesvsPCv1nInflationWCR_LTM)] <- NA

RMAvgPCv1_LTM <- MAvgSalesvsPCv1_LTM*RETENTIONRATIO_LTM*Leverage_LTM
RMAvgPCv1_LTM[is.na(RMAvgPCv1_LTM)|is.nan(RMAvgPCv1_LTM)|!is.finite(RMAvgPCv1_LTM)] <- NA

RMAvgPCv1nInflation_LTM <- MAvgSalesvsPCv1nInflation_LTM*RETENTIONRATIO_LTM*Leverage_LTM
RMAvgPCv1nInflation_LTM[is.na(RMAvgPCv1nInflation_LTM)|is.nan(RMAvgPCv1nInflation_LTM)|!is.finite(RMAvgPCv1nInflation_LTM)] <- NA

RMAvgPCv1nInflationWCR_LTM <- MAvgSalesvsPCv1nInflationWCR_LTM*RETENTIONRATIO_LTM*Leverage_LTM
RMAvgPCv1nInflationWCR_LTM[is.na(RMAvgPCv1nInflationWCR_LTM)|is.nan(RMAvgPCv1nInflationWCR_LTM)|!is.finite(RMAvgPCv1nInflationWCR_LTM)] <- NA




##############################################################################
# EBIT
EBITrescale_ANN <- PLEBITOPER_ANN - FZcapplitemPLitemLTM(input=PLRDEXP_ANN, capper = 3)
# SMOOTH TAX
PLTAXRATE_ANN <- PLTAX_ANN/PLPRETAX_ANN
PLTAXRATE_ANN[!is.na(PLTAXRATE_ANN) & PLTAXRATE_ANN > 0.5] <- 0.5
PLTAXRATE_ANN[!is.na(PLTAXRATE_ANN) & PLTAXRATE_ANN < 0.1] <- 0.1
TAXsmooth_ANN <- FZEVAMAvgSKIP(input=PLTAXRATE_ANN,maper=3,chgper=12)

# CAPITAL
CAPITAL_ANN <- (FZzerona(BSDEBTST_ANN)+FZzerona(BSDEBTLT_ANN)+BSEQTOT_ANN)
remove_ANN <- BSCASH_ANN-((BSCURRLIABS_ANN-BSDEBTSTCURRPORT_ANN)-(BSCURRASSETS_ANN-BSCASH_ANN))
CAPITALrescale_ANN <- CAPITAL_ANN + INFLATION_ANN + FZEVAcapplitemBSitemNET(input=PLRDEXP_ANN, capper = 3) - remove_ANN

ratio_ANN <- (EBITrescale_ANN*(1-TAXsmooth_ANN))/CAPITALrescale_ANN
MAvgratio_ANN <- FZEVAMAvgSKIP(input=ratio_ANN,maper=3,chgper=12)

Rratio <- ratio_ANN*RETENTIONRATIO_ANN*Leverage_ANN
Rratio[is.na(Rratio)|is.nan(Rratio)|!is.finite(Rratio)] <- NA
Rratiot <- Rratio[,Scolfortrain:Ecolfortrain]

RMAvgratio <- MAvgratio_ANN*RETENTIONRATIO_ANN*Leverage_ANN
RMAvgratio[is.na(RMAvgratio)|is.nan(RMAvgratio)|!is.finite(RMAvgratio)] <- NA
RMAvgratiot <- RMAvgratio[,Scolfortrain:Ecolfortrain]

####
# EBIT
EBITrescale_LTM <- PLEBITOPER_LTM - FZcapplitemPLitemLTM(input=PLRDEXP_LTM, capper = 3)
# SMOOTH TAX
PLTAXRATE_LTM <- PLTAX_LTM/PLPRETAX_LTM
PLTAXRATE_LTM[!is.na(PLTAXRATE_LTM) & PLTAXRATE_LTM > 0.5] <- 0.5
PLTAXRATE_LTM[!is.na(PLTAXRATE_LTM) & PLTAXRATE_LTM < 0.1] <- 0.1
TAXsmooth_LTM <- FZEVAMAvgLTM(input=PLTAXRATE_LTM,maper=3,chgper=12)

# CAPITAL
CAPITAL_LTM <- (FZzerona(BSDEBTST_LTM)+FZzerona(BSDEBTLT_LTM)+BSEQTOT_LTM)
remove_LTM <- BSCASH_LTM-((BSCURRLIABS_LTM-BSDEBTSTCURRPORT_LTM)-(BSCURRASSETS_LTM-BSCASH_LTM))
CAPITALrescale_LTM <- CAPITAL_LTM + INFLATION_LTM + FZEVAcapplitemBSitemNET(input=PLRDEXP_LTM, capper = 3) - remove_LTM

ratio_LTM <- (EBITrescale_LTM*(1-TAXsmooth_LTM))/CAPITALrescale_LTM
MAvgratio_LTM <- FZEVAMAvgLTM(input=ratio_LTM,maper=3,chgper=12)

Rratio_LTM <- ratio_LTM*RETENTIONRATIO_LTM*Leverage_LTM
Rratio_LTM[is.na(Rratio_LTM)|is.nan(Rratio_LTM)|!is.finite(Rratio_LTM)] <- NA

RMAvgratio_LTM <- MAvgratio_LTM*RETENTIONRATIO_LTM*Leverage_LTM
RMAvgratio_LTM[is.na(RMAvgratio_LTM)|is.nan(RMAvgratio_LTM)|!is.finite(RMAvgratio_LTM)] <- NA




#######################################################################
ROICVOL <- FZhiSDSKIP(input=ROIC_ANN, maper=5,chgper=12)
#ROICVOL <- log(ROICVOL)
ROICVOL <- apply(ROICVOL,2,function(x){
				outval <- (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
				outval
			})
ROICVOL[is.na(ROICVOL)|is.nan(ROICVOL)|!is.finite(ROICVOL)] <- NA
ROICVOLt <- ROICVOL[,Scolfortrain:Ecolfortrain]

SALESSD <- FZhiSDSKIP(input=(PLSALES_ANN/FZshiftmx(input=PLSALES_ANN,shiftcol=-12))-1, maper=5,chgper=12)
SALESSD <- FZtrimvals (input=SALESSD,trimval=3,naval=TRUE)
SALESSD <- apply(SALESSD,2,function(x){
			outval <- (x-median(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
			outval
		})
SALESSD[is.na(SALESSD)|is.nan(SALESSD)|!is.finite(SALESSD)] <- NA
SALESSDt <- SALESSD[,Scolfortrain:Ecolfortrain]

SALESrange <- FZtrailingmax(input=PLSALES_ANN)-FZtrailingmin(input=PLSALES_ANN)
SALESrange[is.na(SALESrange)|is.nan(SALESrange)|!is.finite(SALESrange)] <- NA
SALESranget <- SALESrange[,Scolfortrain:Ecolfortrain]

ROE <- PLNETINC_ANN/BSEQTOT_ANN
#ROEhistVOL <- FZpriceVolatilitywithper(input=ROE_ANN, volper=3)
#ROEhistVOL[is.na(ROEhistVOL)|is.nan(ROEhistVOL)|!is.finite(ROEhistVOL)] <- NA
#ROEhistVOLt <- ROEhistVOL[,Scolfortrain:Ecolfortrain]

ROErange <- FZtrailingmax(input=ROE)-FZtrailingmin(input=ROE)
ROErange[is.na(ROErange)|is.nan(ROErange)|!is.finite(ROErange)] <- NA
ROEranget <- ROErange[,Scolfortrain:Ecolfortrain]

ROEMomentum <- (FZEVAMAvgSKIP(input=ROE_ANN, maper = 3,chgper=12)-FZEVAMAvgSKIP(input=ROE_ANN, maper = 5,chgper=12))
ROEMomentum[is.na(ROEMomentum)|is.nan(ROEMomentum)|!is.finite(ROEMomentum)] <- NA
ROEMomentumt <- ROEMomentum[,Scolfortrain:Ecolfortrain]

#ROIChistVOL <- FZpriceVolatilitywithper(input=ROIC_ANN, volper=3)
#ROIChistVOL[is.na(ROIChistVOL)|is.nan(ROIChistVOL)|!is.finite(ROIChistVOL)] <- NA
#ROIChistVOLt <- ROIChistVOL[,Scolfortrain:Ecolfortrain]

ROICrange <- FZtrailingmax(input=ROIC_ANN)-FZtrailingmin(input=ROIC_ANN)
ROICrange[is.na(ROICrange)|is.nan(ROICrange)|!is.finite(ROICrange)] <- NA
ROICranget <- ROICrange[,Scolfortrain:Ecolfortrain]

####
ROICVOL <- FZmovingSD (input=ROIC_LTM, sdper=5)
#ROICVOL <- log(ROICVOL)
ROICVOL <- apply(ROICVOL,2,function(x){
				outval <- (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
				outval
			})
ROICVOL_LTM[is.na(ROICVOL_LTM)|is.nan(ROICVOL_LTM)|!is.finite(ROICVOL_LTM)] <- NA

SALESSD_LTM <- FZmovingSD (input=(PLSALES_LTM/FZshiftmx(input=PLSALES_LTM,shiftcol=-12))-1, sdper=5)
SALESSD_LTM <- FZtrimvals (input=SALESSD_LTM,trimval=3,naval=TRUE)
SALESSD_LTM <- apply(SALESSD_LTM,2,function(x){
			outval <- (x-median(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
			outval
		})
SALESSD_LTM[is.na(SALESSD_LTM)|is.nan(SALESSD_LTM)|!is.finite(SALESSD_LTM)] <- NA

SALESrange_LTM <- FZtrailingmax(input=PLSALES_LTM)-FZtrailingmin(input=PLSALES_LTM)
SALESrange_LTM[is.na(SALESrange_LTM)|is.nan(SALESrange_LTM)|!is.finite(SALESrange_LTM)] <- NA

ROE_LTM <- PLNETINC_LTM/BSEQTOT_LTM
#ROEhistVOL_LTM <- FZpriceVolatilitywithper(input=ROE_LTM, volper=3)
#ROEhistVOL_LTM[is.na(ROEhistVOL_LTM)|is.nan(ROEhistVOL_LTM)|!is.finite(ROEhistVOL_LTM)] <- NA

ROErange_LTM <- FZtrailingmax(input=ROE_LTM)-FZtrailingmin(input=ROE_LTM)
ROErange_LTM[is.na(ROErange_LTM)|is.nan(ROErange_LTM)|!is.finite(ROErange_LTM)] <- NA

ROEMomentum_LTM <- (FZEVAMAvgLTM(input=ROE_LTM, maper = 3,chgper=12)-FZEVAMAvgLTM(input=ROE_LTM, maper = 5,chgper=12))
ROEMomentum_LTM[is.na(ROEMomentum_LTM)|is.nan(ROEMomentum_LTM)|!is.finite(ROEMomentum_LTM)] <- NA

#ROIChistVOL_LTM <- FZpriceVolatilitywithper(input=ROIC_LTM, volper=3)
#ROIChistVOL_LTM[is.na(ROIChistVOL_LTM)|is.nan(ROIChistVOL_LTM)|!is.finite(ROIChistVOL_LTM)] <- NA

ROICrange_LTM <- FZtrailingmax(input=ROIC_LTM)-FZtrailingmin(input=ROIC_LTM)
ROICrange_LTM[is.na(ROICrange_LTM)|is.nan(ROICrange_LTM)|!is.finite(ROICrange_LTM)] <- NA



########################################################################################
PLGROSSINC_ANN <- PLSALES_ANN - PLCOGSXDEP_ANN
GROSSMARGIN_ANN <- PLGROSSINC_ANN/PLSALES_ANN
GROSSMARGINchg <- (GROSSMARGIN_ANN/FZshiftmx(input=GROSSMARGIN_ANN, shiftcol=-12))-1
GROSSMARGINchg[is.na(GROSSMARGINchg)|is.nan(GROSSMARGINchg)|!is.finite(GROSSMARGINchg)] <- NA
GROSSMARGINchgt <- GROSSMARGINchg[,Scolfortrain:Ecolfortrain]

MAvgGROSSMARGIN <-  FZEVAMAvgSKIP(input=GROSSMARGIN_ANN, maper = 3,chgper=12)
MAvgGROSSMARGIN[is.na(MAvgGROSSMARGIN)|is.nan(MAvgGROSSMARGIN)|!is.finite(MAvgGROSSMARGIN)] <- NA
MAvgGROSSMARGINt <- MAvgGROSSMARGIN[,Scolfortrain:Ecolfortrain]

#GROSSMARGINhistVOL <- FZpriceVolatilitywithper(input=GROSSMARGIN_ANN, volper=3)
#GROSSMARGINhistVOL[is.na(GROSSMARGINhistVOL)|is.nan(GROSSMARGINhistVOL)|!is.finite(GROSSMARGINhistVOL)] <- NA
#GROSSMARGINhistVOLt <- GROSSMARGINhistVOL[,Scolfortrain:Ecolfortrain]

GROSSMARGINrange <- FZtrailingmax(input=GROSSMARGIN_ANN)-FZtrailingmin(input=GROSSMARGIN_ANN)
GROSSMARGINrange[is.na(GROSSMARGINrange)|is.nan(GROSSMARGINrange)|!is.finite(GROSSMARGINrange)] <- NA
GROSSMARGINranget <- GROSSMARGINrange[,Scolfortrain:Ecolfortrain]

EBITvGROSSINC <- PLEBITOPER_ANN/PLGROSSINC_ANN
EBITvGROSSINCchg <- (EBITvGROSSINC/FZshiftmx(input=EBITvGROSSINC, shiftcol=-12))-1
EBITvGROSSINCchg[is.na(EBITvGROSSINCchg)|is.nan(EBITvGROSSINCchg)|!is.finite(EBITvGROSSINCchg)] <- NA
EBITvGROSSINCchgt <- EBITvGROSSINCchg[,Scolfortrain:Ecolfortrain]

MAvgEBITvGROSSINC <- FZEVAMAvgSKIP(input=EBITvGROSSINC, maper = 3,chgper=12)
MAvgEBITvGROSSINC[is.na(MAvgEBITvGROSSINC)|is.nan(MAvgEBITvGROSSINC)|!is.finite(MAvgEBITvGROSSINC)] <- NA
MAvgEBITvGROSSINCt <- MAvgEBITvGROSSINC[,Scolfortrain:Ecolfortrain]

EBITvGROSSINCrange  <- FZtrailingmax(input=EBITvGROSSINC)-FZtrailingmin(input=EBITvGROSSINC)
EBITvGROSSINCrange[is.na(EBITvGROSSINCrange)|is.nan(EBITvGROSSINCrange)|!is.finite(EBITvGROSSINCrange)] <- NA
EBITvGROSSINCranget <- EBITvGROSSINCrange[,Scolfortrain:Ecolfortrain]

ASSETO_ANN <- (PLSALES_ANN/BSASSETS_ANN)
ASSETOchg <- (ASSETO_ANN/FZshiftmx(input=ASSETO_ANN, shiftcol=-12))-1
ASSETOchg[is.na(ASSETOchg)|is.nan(ASSETOchg)|!is.finite(ASSETOchg)] <- NA
ASSETOchgt <- ASSETOchg[,Scolfortrain:Ecolfortrain]

MAvgASSETO <- FZEVAMAvgSKIP(input=assetturnover_ANN, maper = 3,chgper=12)
MAvgASSETO[is.na(MAvgASSETO)|is.nan(MAvgASSETO)|!is.finite(MAvgASSETO)] <- NA
MAvgASSETOt <- MAvgASSETO[,Scolfortrain:Ecolfortrain]

#ASSETOhistVOL <- FZpriceVolatilitywithper(input=ASSETO_ANN, volper=3)
#ASSETOhistVOL[is.na(ASSETOhistVOL)|is.nan(ASSETOhistVOL)|!is.finite(ASSETOhistVOL)] <- NA
#ASSETOhistVOLt <- ASSETOhistVOL[,Scolfortrain:Ecolfortrain]

ASSETOrange <- FZtrailingmax(input=ASSETO_ANN)-FZtrailingmin(input=ASSETO_ANN)
ASSETOrange[is.na(ASSETOrange)|is.nan(ASSETOrange)|!is.finite(ASSETOrange)] <- NA
ASSETOranget <- ASSETOrange[,Scolfortrain:Ecolfortrain]

####
PLGROSSINC_LTM <- PLSALES_LTM - PLCOGSXDEP_LTM
GROSSMARGIN_LTM <- PLGROSSINC_LTM/PLSALES_LTM
GROSSMARGINchg_LTM <- (GROSSMARGIN_LTM/FZshiftmx(input=GROSSMARGIN_LTM, shiftcol=-12))-1
GROSSMARGINchg_LTM[is.na(GROSSMARGINchg_LTM)|is.nan(GROSSMARGINchg_LTM)|!is.finite(GROSSMARGINchg_LTM)] <- NA

MAvgGROSSMARGIN_LTM <-  FZEVAMAvgLTM(input=GROSSMARGIN_LTM, maper = 3,chgper=12)
MAvgGROSSMARGIN_LTM[is.na(MAvgGROSSMARGIN_LTM)|is.nan(MAvgGROSSMARGIN_LTM)|!is.finite(MAvgGROSSMARGIN_LTM)] <- NA

#GROSSMARGINhistVOL_LTM <- FZpriceVolatilitywithper(input=GROSSMARGIN_LTM, volper=3)
#GROSSMARGINhistVOL_LTM[is.na(GROSSMARGINhistVOL_LTM)|is.nan(GROSSMARGINhistVOL_LTM)|!is.finite(GROSSMARGINhistVOL_LTM)] <- NA

GROSSMARGINrange_LTM <- FZtrailingmax(input=GROSSMARGIN_LTM)-FZtrailingmin(input=GROSSMARGIN_LTM)
GROSSMARGINrange_LTM[is.na(GROSSMARGINrange_LTM)|is.nan(GROSSMARGINrange_LTM)|!is.finite(GROSSMARGINrange_LTM)] <- NA

EBITvGROSSINC_LTM <- PLEBITOPER_LTM/PLGROSSINC_LTM
EBITvGROSSINCchg_LTM <- (EBITvGROSSINC_LTM/FZshiftmx(input=EBITvGROSSINC_LTM, shiftcol=-12))-1
EBITvGROSSINCchg_LTM[is.na(EBITvGROSSINCchg_LTM)|is.nan(EBITvGROSSINCchg_LTM)|!is.finite(EBITvGROSSINCchg_LTM)] <- NA

MAvgEBITvGROSSINC_LTM <- FZEVAMAvgLTM(input=EBITvGROSSINC_LTM, maper = 3,chgper=12)
MAvgEBITvGROSSINC_LTM[is.na(MAvgEBITvGROSSINC_LTM)|is.nan(MAvgEBITvGROSSINC_LTM)|!is.finite(MAvgEBITvGROSSINC_LTM)] <- NA

EBITvGROSSINCrange_LTM  <- FZtrailingmax(input=EBITvGROSSINC_LTM)-FZtrailingmin(input=EBITvGROSSINC_LTM)
EBITvGROSSINCrange_LTM[is.na(EBITvGROSSINCrange_LTM)|is.nan(EBITvGROSSINCrange_LTM)|!is.finite(EBITvGROSSINCrange_LTM)] <- NA

ASSETO_LTM <- (PLSALES_LTM/BSASSETS_LTM)
ASSETOchg_LTM <- (ASSETO_LTM/FZshiftmx(input=ASSETO_LTM, shiftcol=-12))-1
ASSETOchg_LTM[is.na(ASSETOchg_LTM)|is.nan(ASSETOchg_LTM)|!is.finite(ASSETOchg_LTM)] <- NA

MAvgASSETO_LTM <- FZEVAMAvgLTM(input=assetturnover_LTM, maper = 3,chgper=12)
MAvgASSETO_LTM[is.na(MAvgASSETO_LTM)|is.nan(MAvgASSETO_LTM)|!is.finite(MAvgASSETO_LTM)] <- NA

#ASSETOhistVOL_LTM <- FZpriceVolatilitywithper(input=ASSETO_LTM, volper=3)
#ASSETOhistVOL_LTM[is.na(ASSETOhistVOL_LTM)|is.nan(ASSETOhistVOL_LTM)|!is.finite(ASSETOhistVOL_LTM)] <- NA

ASSETOrange_LTM <- FZtrailingmax(input=ASSETO_LTM)-FZtrailingmin(input=ASSETO_LTM)
ASSETOrange_LTM[is.na(ASSETOrange_LTM)|is.nan(ASSETOrange_LTM)|!is.finite(ASSETOrange_LTM)] <- NA



##############################################################################################################
################################## WEIGHTED RANDOM FOREST TEST ######################################################

# uses annaul rep to full year, using an in built 3 month lag
# pred uses LTM (so more up to date info) but its ok
# also have a pred annual using same 3 month lag and structure as input for training

inputtraingrowthEVAtemp <- data.frame(as.vector(ttmanualclass_ANNREP[,Scolfortrain:Ecolfortrain]),
						as.vector(SalesGrowthfullt),
						as.vector(ROEfullt),
						as.vector(ROICfullt),
						as.vector(Leveragefullt),
						as.vector(ASSETTOnRRfullt),
						as.vector(ulASSETTOnRRfullt),
						as.vector(quickratiofullt),
						as.vector(DebttEquityfullt),
						as.vector(CAPEXtoSalesfullt),
						as.vector(WCRchangefullt),
						as.vector(ChangeSGAtoSalesfullt),
						as.vector(MAvgSalesGrowthfullt),
						as.vector(MAvgROEfullt),
						as.vector(MAvgROICfullt),
						as.vector(MAvgRRfullt),
						as.vector(MAvgLeveragefullt),
						as.vector(MAvgASSETTOnRRfullt),
						as.vector(MAvgulASSETTOnRRfullt),
						as.vector(MAvgquickratiofullt),
						as.vector(MAvgDebttEquityfullt),
						as.vector(MAvgCAPEXtoSalesfullt),
						as.vector(MAvgWCRchangefullt),
						as.vector(MAvgChangeSGAtoSalesfullt),
						as.vector(ChgROEfullt),
						as.vector(ChgROICfullt),
						as.vector(ChgLeveragefullt),
						as.vector(ChgASSETTOnRRfullt),
						as.vector(ChgulASSETTOnRRfullt),
						as.vector(ChgOpMarginfullt),
						as.vector(Chgnetmarginfullt),
						as.vector(Chgquickratiofullt),
						as.vector(ChgDebttEquityfullt),
						as.vector(ChgCAPEXtoSalesfullt),
						as.vector(ChgWCRchangefullt),
						as.vector(ChgChangeSGAtoSalesfullt),
						as.vector(ChgGrossPPEfullt),
						as.vector(PCv1t),
						as.vector(PCv1Growth1perchgt),
						as.vector(PCv1Growthlastvs3permat),
						as.vector(PCv1Growth3permachgt),
						as.vector(PCv1Growth1pchgvs3pmachgt),
						as.vector(PCv2t),
						as.vector(PCv2Growth1perchgt),
						as.vector(PCv2Growthlastvs3permat),
						as.vector(PCv2Growth3permachgt),
						as.vector(PCv2Growth1pchgvs3pmachgt),
						as.vector(PCv3t),
						as.vector(PCv3Growth1perchgt),
						as.vector(PCv3Growthlastvs3permat),
						as.vector(PCv3Growth3permachgt),
						as.vector(PCv3Growth1pchgvs3pmachgt),
						as.vector(PCv4t),
						as.vector(PCv4Growth1perchgt),
						as.vector(PCv4Growthlastvs3permat),
						as.vector(PCv4Growth3permachgt),
						as.vector(PCv4Growth1pchgvs3pmachgt),
						as.vector(MAvgGrossPPEt),
						#as.vector(ChgGrossPPEt),
						as.vector(BSPPEGROSS1perchgt),
						as.vector(BSPPEGROSSlastvs3permat),
						as.vector(BSPPEGROSS3permachgt),
						as.vector(BSPPEGROSS1pchgvs3pmachgt),
						as.vector(GrPPEexCIPt),
						as.vector(MAvgGrPPEexCIPt),
						as.vector(ChgGrPPEexCIPt),
						as.vector(GrPPEexCIP1perchgt),
						as.vector(GrPPEexCIPlastvs3permat),
						as.vector(GrPPEexCIP3permachgt),
						as.vector(GrPPEexCIP1pchgvs3pmachgt),
						as.vector(InfladjGrPPEexCIPt),
						as.vector(MAvgInfladjGrPPEexCIPt),
						as.vector(ChgInfladjGrPPEexCIPt),
						as.vector(PLSGAt),
						as.vector(MAvgPLSGAt),
						as.vector(ChgPLSGAt),
						as.vector(PLSGAGrowth1perchgt),
						as.vector(PLSGAGrowthlastvs3permat),
						as.vector(PLSGAGrowth3permachgt),
						as.vector(PLSGAGrowth1pchgvs3pmachgt),
						as.vector(PLRDEXPt),
						as.vector(MAvgPLRDEXPt),
						as.vector(ChgPLRDEXPt),
						as.vector(PLRDEXPGrowth1perchgt),
						as.vector(PLRDEXPGrowthlastvs3permat),
						as.vector(PLRDEXPGrowth3permachgt),
						as.vector(PLRDEXPGrowth1pchgvs3pmachgt),
						as.vector(BSACCPAYt),
						as.vector(MAvgBSACCPAYt),
						as.vector(BSACCPAYGrowth1perchgt),
						as.vector(BSACCPAYGrowthlastvs3permat),
						as.vector(BSACCPAYGrowth3permachgt),
						as.vector(BSACCPAYGrowth1pchgvs3pmachgt),
						as.vector(BSACCRECt),
						as.vector(MAvgBSACCRECt),
						as.vector(ChgBSACCRECt),
						as.vector(BSACCRECGrowth1perchgt),
						as.vector(BSACCRECGrowthlastvs3permat),
						as.vector(BSACCRECGrowth3permachgt),
						as.vector(BSACCRECGrowth1pchgvs3pmachgt),
						as.vector(BSPPEGROSSCONSTRt),
						as.vector(MAvgBSPPEGROSSCONSTRt),
						as.vector(ChgBSPPEGROSSCONSTRt),
						as.vector(BSPPEGROSSCONSTRGrowth1perchgt),
						as.vector(BSPPEGROSSCONSTRGrowthlastvs3permat),
						as.vector(BSPPEGROSSCONSTRGrowth3permachgt),
						as.vector(BSPPEGROSSCONSTRGrowth1pchgvs3pmachgt),
						as.vector(PricetoSalesfullt),
						as.vector(MAvgPricetoSalesfullt),
						as.vector(ChgPricetoSalesfullt),
						as.vector(PricetoSalesfullGrowth1perchgt),
						as.vector(PricetoSalesfullGrowthlastvs3permat),
						as.vector(PricetoSalesfullGrowth3permachgt),
						as.vector(PricetoSalesfullGrowth1pchgvs3pmachgt),
						as.vector(ROEnRRfullt),
						as.vector(MAvgROEnRRfullt),
						as.vector(ROEnRRfullGrowth1perchgt),
						as.vector(ROEnRRfullGrowthlastvs3permat),
						as.vector(ROEnRRfullGrowth3permachgt),
						as.vector(ROEnRRfullGrowth1pchgvs3pmachgt),
						as.vector(OpMarginfullt),
						as.vector(MAvgOpMarginfullt),
						as.vector(OpMarginfullGrowth1perchgt),
						as.vector(OpMarginfullGrowthlastvs3permat),
						as.vector(OpMarginfullGrowth3permachgt),
						as.vector(OpMarginfullGrowth1pchgvs3pmachgt),
						as.vector(netmarginfullt),
						as.vector(MAvgnetmarginfullt),
						as.vector(netmarginfullGrowth1perchgt),
						as.vector(netmarginfullGrowthlastvs3permat),
						as.vector(netmarginfullGrowth3permachgt),
						as.vector(netmarginfullGrowth1pchgvs3pmachgt),
						as.vector(BSINTANGIBLEt),
						as.vector(MAvgBSINTANGIBLEt),
						as.vector(ChgBSINTANGIBLEt),
						as.vector(BSINTANGIBLEGrowth1perchgt),
						as.vector(BSINTANGIBLEGrowthlastvs3permat),
						as.vector(BSINTANGIBLEGrowth3permachgt),
						as.vector(BSINTANGIBLEGrowth1pchgvs3pmachgt),
						as.vector(BSINVENTORYt),
						as.vector(MAvgBSINVENTORYt),
						as.vector(ChgBSINVENTORYt),
						as.vector(BSINVENTORYGrowth1perchgt),
						as.vector(BSINVENTORYGrowthlastvs3permat),
						as.vector(BSINVENTORYGrowth3permachgt),
						as.vector(BSINVENTORYGrowth1pchgvs3pmachgt),
						as.vector(AssetTOMomentumt),
						as.vector(MAvgAssetTOMomentumt),
						as.vector(ChgAssetTOMomentumt),
						as.vector(AssetTOMomentumGrowth1perchgt),
						as.vector(AssetTOMomentumGrowthlastvs3permat),
						as.vector(AssetTOMomentumGrowth3permachgt),
						as.vector(AssetTOMomentumGrowth1pchgvs3pmachgt),
						as.vector(ROICt),
						as.vector(MAvgROICt),
						as.vector(ChgROICt),
						as.vector(ROICGrowth1perchgt),
						as.vector(ROICGrowthlastvs3permat),
						as.vector(ROICGrowth3permachgt),
						as.vector(ROICGrowth1pchgvs3pmachgt),
						as.vector(PricetoEarningsfullt),
						as.vector(MAvgPricetoEarningsfullt),
						as.vector(ChgPricetoEarningsfullt),
						as.vector(PricetoEarningsfull1perchgt),
						as.vector(PricetoEarningsfulllastvs3permat),
						as.vector(PricetoEarningsfull3permachgt),
						as.vector(PricetoEarningsfull1pchgvs3pmachgt),
						as.vector(lasttoHistMaxfullt),
						as.vector(MAvglasttoHistMaxfullt),
						as.vector(ChglasttoHistMaxfullt),
						as.vector(lasttoHistMaxfull1perchgt),
						as.vector(lasttoHistMaxfulllastvs3permat),
						as.vector(lasttoHistMaxfull3permachgt),
						as.vector(lasttoHistMaxfull1pchgvs3pmachgt),
						as.vector(Debtt),
						as.vector(MAvgDebtt),
						as.vector(ChgDebtt),
						as.vector(DebtGrowth1perchgt),
						as.vector(DebtGrowthlastvs3permat),
						as.vector(DebtGrowth3permachgt),
						as.vector(DebtGrowth1pchgvs3pmachgt),
						as.vector(Reinvestmentfullt),
						as.vector(MAvgReinvestmentfullt),
						as.vector(ChgReinvestmentfullt),
						as.vector(ReinvestmentfullGrowth1perchgt),
						as.vector(ReinvestmentfullGrowthlastvs3permat),
						as.vector(ReinvestmentfullGrowth3permachgt),
						as.vector(ReinvestmentfullGrowth1pchgvs3pmachgt),
						as.vector(RetainedEarningstoEquityfullt),
						as.vector(MAvgRetainedEarningstoEquityfullt),
						as.vector(ChgRetainedEarningstoEquityfullt),
						as.vector(RetainedEarningstoEquityfullGrowth1perchgt),
						as.vector(RetainedEarningstoEquityfullGrowthlastvs3permat),
						as.vector(RetainedEarningstoEquityfullGrowth3permachgt),
						as.vector(RetainedEarningstoEquityfullGrowth1pchgvs3pmachgt),
						#as.vector(RetentionRatiofullt),
						#as.vector(MAvgRetentionRatiofullt),
						#as.vector(ChgRetentionRatiofullt),
						#as.vector(RetentionRatiofull1perchgt),
						#as.vector(RetentionRatiofulllastvs3permat),
						#as.vector(RetentionRatiofull3permachgt),
						#as.vector(RetentionRatiofull1pchgvs3pmachgt),
						as.vector(ROICVOLfullt),
						as.vector(MAvgROICVOLfullt),
						as.vector(ChgROICVOLfullt),
						as.vector(ROICVOLfull1perchgt),
						as.vector(ROICVOLfulllastvs3permat),
						as.vector(ROICVOLfull3permachgt),
						as.vector(ROICVOLfull1pchgvs3pmachgt),
						as.vector(SALESSDfullt),
						as.vector(MAvgSALESSDfullt),
						as.vector(ChgSALESSDfullt),
						as.vector(SALESSDfull1perchgt),
						as.vector(SALESSDfulllastvs3permat),
						as.vector(SALESSDfull3permachgt),
						as.vector(SALESSDfull1pchgvs3pmachgt),
						as.vector(OperatingLeveragefullt),
						as.vector(MAvgOperatingLeveragefullt),
						as.vector(ChgOperatingLeveragefullt),
						as.vector(OperatingLeveragefullGrowth1perchgt),
						as.vector(OperatingLeveragefullGrowthlastvs3permat),
						as.vector(OperatingLeveragefullGrowth3permachgt),
						as.vector(OperatingLeveragefullGrowth1pchgvs3pmachgt),
						as.vector(PCv1nInflationt),
						as.vector(PCv1nInflationWCRt),
						as.vector(SalesvsPCv1t),
						as.vector(SalesvsPCv1nInflationt),
						as.vector(SalesvsPCv1nInflationWCRt),
						as.vector(MAvgSalesvsPCv1t),
						as.vector(MAvgSalesvsPCv1nInflationt),
						as.vector(MAvgSalesvsPCv1nInflationWCRt),
						as.vector(RSalesvsPCv1t),
						as.vector(RSalesvsPCv1nInflationt),
						as.vector(RSalesvsPCv1nInflationWCRt),
						as.vector(RMAvgPCv1t),
						as.vector(RMAvgPCv1nInflationt),
						as.vector(RMAvgPCv1nInflationWCRt),
						as.vector(Rratiot),
						as.vector(RMAvgratiot),
						as.vector(ROICVOLt),
						as.vector(SALESSDt),
						as.vector(SALESranget),
						as.vector(ROEranget),
						as.vector(ROEMomentumt),
						as.vector(ROICranget),
						as.vector(GROSSMARGINchgt),
						as.vector(MAvgGROSSMARGINt),
						as.vector(GROSSMARGINranget),
						as.vector(EBITvGROSSINCchgt),
						as.vector(MAvgEBITvGROSSINCt),
						as.vector(EBITvGROSSINCranget),
						as.vector(ASSETOchgt),
						as.vector(MAvgASSETOt),
						as.vector(ASSETOranget)
)





predvalsgrowthEVAtemp_ANN <- data.frame(
						as.vector(SalesGrowthfull),
						as.vector(ROEfull),
						as.vector(ROICfull),
						as.vector(Leveragefull),
						as.vector(ASSETTOnRRfull),
						as.vector(ulASSETTOnRRfull),
						as.vector(quickratiofull),
						as.vector(DebttEquityfull),
						as.vector(CAPEXtoSalesfull),
						as.vector(WCRchangefull),
						as.vector(ChangeSGAtoSalesfull),
						as.vector(MAvgSalesGrowthfull),
						as.vector(MAvgROEfull),
						as.vector(MAvgROICfull),
						as.vector(MAvgRRfull),
						as.vector(MAvgLeveragefull),
						as.vector(MAvgASSETTOnRRfull),
						as.vector(MAvgulASSETTOnRRfull),
						as.vector(MAvgquickratiofull),
						as.vector(MAvgDebttEquityfull),
						as.vector(MAvgCAPEXtoSalesfull),
						as.vector(MAvgWCRchangefull),
						as.vector(MAvgChangeSGAtoSalesfull),
						as.vector(ChgROEfull),
						as.vector(ChgROICfull),
						as.vector(ChgLeveragefull),
						as.vector(ChgASSETTOnRRfull),
						as.vector(ChgulASSETTOnRRfull),
						as.vector(ChgOpMarginfull),
						as.vector(Chgnetmarginfull),
						as.vector(Chgquickratiofull),
						as.vector(ChgDebttEquityfull),
						as.vector(ChgCAPEXtoSalesfull),
						as.vector(ChgWCRchangefull),
						as.vector(ChgChangeSGAtoSalesfull),
						as.vector(ChgGrossPPEfull),
						as.vector(PCv1),
						as.vector(PCv1Growth1perchg),
						as.vector(PCv1Growthlastvs3perma),
						as.vector(PCv1Growth3permachg),
						as.vector(PCv1Growth1pchgvs3pmachg),
						as.vector(PCv2),
						as.vector(PCv2Growth1perchg),
						as.vector(PCv2Growthlastvs3perma),
						as.vector(PCv2Growth3permachg),
						as.vector(PCv2Growth1pchgvs3pmachg),
						as.vector(PCv3),
						as.vector(PCv3Growth1perchg),
						as.vector(PCv3Growthlastvs3perma),
						as.vector(PCv3Growth3permachg),
						as.vector(PCv3Growth1pchgvs3pmachg),
						as.vector(PCv4),
						as.vector(PCv4Growth1perchg),
						as.vector(PCv4Growthlastvs3perma),
						as.vector(PCv4Growth3permachg),
						as.vector(PCv4Growth1pchgvs3pmachg),
						as.vector(MAvgGrossPPE),
						#as.vector(ChgGrossPPE),
						as.vector(BSPPEGROSS1perchg),
						as.vector(BSPPEGROSSlastvs3perma),
						as.vector(BSPPEGROSS3permachg),
						as.vector(BSPPEGROSS1pchgvs3pmachg),
						as.vector(GrPPEexCIP),
						as.vector(MAvgGrPPEexCIP),
						as.vector(ChgGrPPEexCIP),
						as.vector(GrPPEexCIP1perchg),
						as.vector(GrPPEexCIPlastvs3perma),
						as.vector(GrPPEexCIP3permachg),
						as.vector(GrPPEexCIP1pchgvs3pmachg),
						as.vector(InfladjGrPPEexCIP),
						as.vector(MAvgInfladjGrPPEexCIP),
						as.vector(ChgInfladjGrPPEexCIP),
						as.vector(PLSGA),
						as.vector(MAvgPLSGA),
						as.vector(ChgPLSGA),
						as.vector(PLSGAGrowth1perchg),
						as.vector(PLSGAGrowthlastvs3perma),
						as.vector(PLSGAGrowth3permachg),
						as.vector(PLSGAGrowth1pchgvs3pmachg),
						as.vector(PLRDEXP),
						as.vector(MAvgPLRDEXP),
						as.vector(ChgPLRDEXP),
						as.vector(PLRDEXPGrowth1perchg),
						as.vector(PLRDEXPGrowthlastvs3perma),
						as.vector(PLRDEXPGrowth3permachg),
						as.vector(PLRDEXPGrowth1pchgvs3pmachg),
						as.vector(BSACCPAY),
						as.vector(MAvgBSACCPAY),
						as.vector(BSACCPAYGrowth1perchg),
						as.vector(BSACCPAYGrowthlastvs3perma),
						as.vector(BSACCPAYGrowth3permachg),
						as.vector(BSACCPAYGrowth1pchgvs3pmachg),
						as.vector(BSACCREC),
						as.vector(MAvgBSACCREC),
						as.vector(ChgBSACCREC),
						as.vector(BSACCRECGrowth1perchg),
						as.vector(BSACCRECGrowthlastvs3perma),
						as.vector(BSACCRECGrowth3permachg),
						as.vector(BSACCRECGrowth1pchgvs3pmachg),
						as.vector(BSPPEGROSSCONSTR),
						as.vector(MAvgBSPPEGROSSCONSTR),
						as.vector(ChgBSPPEGROSSCONSTR),
						as.vector(BSPPEGROSSCONSTRGrowth1perchg),
						as.vector(BSPPEGROSSCONSTRGrowthlastvs3perma),
						as.vector(BSPPEGROSSCONSTRGrowth3permachg),
						as.vector(BSPPEGROSSCONSTRGrowth1pchgvs3pmachg),
						as.vector(PricetoSalesfull),
						as.vector(MAvgPricetoSalesfull),
						as.vector(ChgPricetoSalesfull),
						as.vector(PricetoSalesfullGrowth1perchg),
						as.vector(PricetoSalesfullGrowthlastvs3perma),
						as.vector(PricetoSalesfullGrowth3permachg),
						as.vector(PricetoSalesfullGrowth1pchgvs3pmachg),
						as.vector(ROEnRRfull),
						as.vector(MAvgROEnRRfull),
						as.vector(ROEnRRfullGrowth1perchg),
						as.vector(ROEnRRfullGrowthlastvs3perma),
						as.vector(ROEnRRfullGrowth3permachg),
						as.vector(ROEnRRfullGrowth1pchgvs3pmachg),
						as.vector(OpMarginfull),
						as.vector(MAvgOpMarginfull),
						as.vector(OpMarginfullGrowth1perchg),
						as.vector(OpMarginfullGrowthlastvs3perma),
						as.vector(OpMarginfullGrowth3permachg),
						as.vector(OpMarginfullGrowth1pchgvs3pmachg),
						as.vector(netmarginfull),
						as.vector(MAvgnetmarginfull),
						as.vector(netmarginfullGrowth1perchg),
						as.vector(netmarginfullGrowthlastvs3perma),
						as.vector(netmarginfullGrowth3permachg),
						as.vector(netmarginfullGrowth1pchgvs3pmachg),
						as.vector(BSINTANGIBLE),
						as.vector(MAvgBSINTANGIBLE),
						as.vector(ChgBSINTANGIBLE),
						as.vector(BSINTANGIBLEGrowth1perchg),
						as.vector(BSINTANGIBLEGrowthlastvs3perma),
						as.vector(BSINTANGIBLEGrowth3permachg),
						as.vector(BSINTANGIBLEGrowth1pchgvs3pmachg),
						as.vector(BSINVENTORY),
						as.vector(MAvgBSINVENTORY),
						as.vector(ChgBSINVENTORY),
						as.vector(BSINVENTORYGrowth1perchg),
						as.vector(BSINVENTORYGrowthlastvs3perma),
						as.vector(BSINVENTORYGrowth3permachg),
						as.vector(BSINVENTORYGrowth1pchgvs3pmachg),
						as.vector(AssetTOMomentum),
						as.vector(MAvgAssetTOMomentum),
						as.vector(ChgAssetTOMomentum),
						as.vector(AssetTOMomentumGrowth1perchg),
						as.vector(AssetTOMomentumGrowthlastvs3perma),
						as.vector(AssetTOMomentumGrowth3permachg),
						as.vector(AssetTOMomentumGrowth1pchgvs3pmachg),
						as.vector(ROIC),
						as.vector(MAvgROIC),
						as.vector(ChgROIC),
						as.vector(ROICGrowth1perchg),
						as.vector(ROICGrowthlastvs3perma),
						as.vector(ROICGrowth3permachg),
						as.vector(ROICGrowth1pchgvs3pmachg),
						as.vector(PricetoEarningsfull),
						as.vector(MAvgPricetoEarningsfull),
						as.vector(ChgPricetoEarningsfull),
						as.vector(PricetoEarningsfull1perchg),
						as.vector(PricetoEarningsfulllastvs3perma),
						as.vector(PricetoEarningsfull3permachg),
						as.vector(PricetoEarningsfull1pchgvs3pmachg),
						as.vector(lasttoHistMaxfull),
						as.vector(MAvglasttoHistMaxfull),
						as.vector(ChglasttoHistMaxfull),
						as.vector(lasttoHistMaxfull1perchg),
						as.vector(lasttoHistMaxfulllastvs3perma),
						as.vector(lasttoHistMaxfull3permachg),
						as.vector(lasttoHistMaxfull1pchgvs3pmachg),
						as.vector(Debt),
						as.vector(MAvgDebt),
						as.vector(ChgDebt),
						as.vector(DebtGrowth1perchg),
						as.vector(DebtGrowthlastvs3perma),
						as.vector(DebtGrowth3permachg),
						as.vector(DebtGrowth1pchgvs3pmachg),
						as.vector(Reinvestmentfull),
						as.vector(MAvgReinvestmentfull),
						as.vector(ChgReinvestmentfull),
						as.vector(ReinvestmentfullGrowth1perchg),
						as.vector(ReinvestmentfullGrowthlastvs3perma),
						as.vector(ReinvestmentfullGrowth3permachg),
						as.vector(ReinvestmentfullGrowth1pchgvs3pmachg),
						as.vector(RetainedEarningstoEquityfull),
						as.vector(MAvgRetainedEarningstoEquityfull),
						as.vector(ChgRetainedEarningstoEquityfull),
						as.vector(RetainedEarningstoEquityfullGrowth1perchg),
						as.vector(RetainedEarningstoEquityfullGrowthlastvs3perma),
						as.vector(RetainedEarningstoEquityfullGrowth3permachg),
						as.vector(RetainedEarningstoEquityfullGrowth1pchgvs3pmachg),
						#as.vector(RetentionRatiofull),
						#as.vector(MAvgRetentionRatiofull),
						#as.vector(ChgRetentionRatiofull),
						#as.vector(RetentionRatiofull1perchg),
						#as.vector(RetentionRatiofulllastvs3perma),
						#as.vector(RetentionRatiofull3permachg),
						#as.vector(RetentionRatiofull1pchgvs3pmachg),
						as.vector(ROICVOLfull),
						as.vector(MAvgROICVOLfull),
						as.vector(ChgROICVOLfull),
						as.vector(ROICVOLfull1perchg),
						as.vector(ROICVOLfulllastvs3perma),
						as.vector(ROICVOLfull3permachg),
						as.vector(ROICVOLfull1pchgvs3pmachg),
						as.vector(SALESSDfull),
						as.vector(MAvgSALESSDfull),
						as.vector(ChgSALESSDfull),
						as.vector(SALESSDfull1perchg),
						as.vector(SALESSDfulllastvs3perma),
						as.vector(SALESSDfull3permachg),
						as.vector(SALESSDfull1pchgvs3pmachg),
						as.vector(OperatingLeveragefull),
						as.vector(MAvgOperatingLeveragefull),
						as.vector(ChgOperatingLeveragefull),
						as.vector(OperatingLeveragefullGrowth1perchg),
						as.vector(OperatingLeveragefullGrowthlastvs3perma),
						as.vector(OperatingLeveragefullGrowth3permachg),
						as.vector(OperatingLeveragefullGrowth1pchgvs3pmachg),
						as.vector(PCv1nInflation),
						as.vector(PCv1nInflationWCR),
						as.vector(SalesvsPCv1),
						as.vector(SalesvsPCv1nInflation),
						as.vector(SalesvsPCv1nInflationWCR),
						as.vector(MAvgSalesvsPCv1),
						as.vector(MAvgSalesvsPCv1nInflation),
						as.vector(MAvgSalesvsPCv1nInflationWCR),
						as.vector(RSalesvsPCv1),
						as.vector(RSalesvsPCv1nInflation),
						as.vector(RSalesvsPCv1nInflationWCR),
						as.vector(RMAvgPCv1),
						as.vector(RMAvgPCv1nInflation),
						as.vector(RMAvgPCv1nInflationWCR),
						as.vector(Rratio),
						as.vector(RMAvgratio),
						as.vector(ROICVOL),
						as.vector(SALESSD),
						as.vector(SALESrange),
						as.vector(ROErange),
						as.vector(ROEMomentum),
						as.vector(ROICrange),
						as.vector(GROSSMARGINchg),
						as.vector(MAvgGROSSMARGIN),
						as.vector(GROSSMARGINrange),
						as.vector(EBITvGROSSINCchg),
						as.vector(MAvgEBITvGROSSINC),
						as.vector(EBITvGROSSINCrange),
						as.vector(ASSETOchg),
						as.vector(MAvgASSETO),
						as.vector(ASSETOrange)
)

predvalsgrowthEVAtemp_LTM <- data.frame(
						as.vector(SalesGrowthfull_LTM),
						as.vector(ROEfull_LTM),
						as.vector(ROICfull_LTM),
						as.vector(Leveragefull_LTM),
						as.vector(ASSETTOnRRfull_LTM),
						as.vector(ulASSETTOnRRfull_LTM),
						as.vector(quickratiofull_LTM),
						as.vector(DebttEquityfull_LTM),
						as.vector(CAPEXtoSalesfull_LTM),
						as.vector(WCRchangefull_LTM),
						as.vector(ChangeSGAtoSalesfull_LTM),
						as.vector(MAvgSalesGrowthfull_LTM),
						as.vector(MAvgROEfull_LTM),
						as.vector(MAvgROICfull_LTM),
						as.vector(MAvgRRfull_LTM),
						as.vector(MAvgLeveragefull_LTM),
						as.vector(MAvgASSETTOnRRfull_LTM),
						as.vector(MAvgulASSETTOnRRfull_LTM),
						as.vector(MAvgquickratiofull_LTM),
						as.vector(MAvgDebttEquityfull_LTM),
						as.vector(MAvgCAPEXtoSalesfull_LTM),
						as.vector(MAvgWCRchangefull_LTM),
						as.vector(MAvgChangeSGAtoSalesfull_LTM),
						as.vector(ChgROEfull_LTM),
						as.vector(ChgROICfull_LTM),
						as.vector(ChgLeveragefull_LTM),
						as.vector(ChgASSETTOnRRfull_LTM),
						as.vector(ChgulASSETTOnRRfull_LTM),
						as.vector(ChgOpMarginfull_LTM),
						as.vector(Chgnetmarginfull_LTM),
						as.vector(Chgquickratiofull_LTM),
						as.vector(ChgDebttEquityfull_LTM),
						as.vector(ChgCAPEXtoSalesfull_LTM),
						as.vector(ChgWCRchangefull_LTM),
						as.vector(ChgChangeSGAtoSalesfull_LTM),
						as.vector(ChgGrossPPEfull_LTM),
						as.vector(PCv1_LTM),
						as.vector(PCv1Growth1perchg_LTM),
						as.vector(PCv1Growthlastvs3perma_LTM),
						as.vector(PCv1Growth3permachg_LTM),
						as.vector(PCv1Growth1pchgvs3pmachg_LTM),
						as.vector(PCv2_LTM),
						as.vector(PCv2Growth1perchg_LTM),
						as.vector(PCv2Growthlastvs3perma_LTM),
						as.vector(PCv2Growth3permachg_LTM),
						as.vector(PCv2Growth1pchgvs3pmachg_LTM),
						as.vector(PCv3_LTM),
						as.vector(PCv3Growth1perchg_LTM),
						as.vector(PCv3Growthlastvs3perma_LTM),
						as.vector(PCv3Growth3permachg_LTM),
						as.vector(PCv3Growth1pchgvs3pmachg_LTM),
						as.vector(PCv4_LTM),
						as.vector(PCv4Growth1perchg_LTM),
						as.vector(PCv4Growthlastvs3perma_LTM),
						as.vector(PCv4Growth3permachg_LTM),
						as.vector(PCv4Growth1pchgvs3pmachg_LTM),
						as.vector(MAvgGrossPPE_LTM),
						#as.vector(ChgGrossPPE_LTM),
						as.vector(BSPPEGROSS1perchg_LTM),
						as.vector(BSPPEGROSSlastvs3perma_LTM),
						as.vector(BSPPEGROSS3permachg_LTM),
						as.vector(BSPPEGROSS1pchgvs3pmachg_LTM),
						as.vector(GrPPEexCIP_LTM),
						as.vector(MAvgGrPPEexCIP_LTM),
						as.vector(ChgGrPPEexCIP_LTM),
						as.vector(GrPPEexCIP1perchg_LTM),
						as.vector(GrPPEexCIPlastvs3perma_LTM),
						as.vector(GrPPEexCIP3permachg_LTM),
						as.vector(GrPPEexCIP1pchgvs3pmachg_LTM),
						as.vector(InfladjGrPPEexCIP_LTM),
						as.vector(MAvgInfladjGrPPEexCIP_LTM),
						as.vector(ChgInfladjGrPPEexCIP_LTM),
						as.vector(PLSGA_LTM),
						as.vector(MAvgPLSGA_LTM),
						as.vector(ChgPLSGA_LTM),
						as.vector(PLSGAGrowth1perchg_LTM),
						as.vector(PLSGAGrowthlastvs3perma_LTM),
						as.vector(PLSGAGrowth3permachg_LTM),
						as.vector(PLSGAGrowth1pchgvs3pmachg_LTM),
						as.vector(PLRDEXP_LTM),
						as.vector(MAvgPLRDEXP_LTM),
						as.vector(ChgPLRDEXP_LTM),
						as.vector(PLRDEXPGrowth1perchg_LTM),
						as.vector(PLRDEXPGrowthlastvs3perma_LTM),
						as.vector(PLRDEXPGrowth3permachg_LTM),
						as.vector(PLRDEXPGrowth1pchgvs3pmachg_LTM),
						as.vector(BSACCPAY_LTM),
						as.vector(MAvgBSACCPAY_LTM),
						as.vector(BSACCPAYGrowth1perchg_LTM),
						as.vector(BSACCPAYGrowthlastvs3perma_LTM),
						as.vector(BSACCPAYGrowth3permachg_LTM),
						as.vector(BSACCPAYGrowth1pchgvs3pmachg_LTM),
						as.vector(BSACCREC_LTM),
						as.vector(MAvgBSACCREC_LTM),
						as.vector(ChgBSACCREC_LTM),
						as.vector(BSACCRECGrowth1perchg_LTM),
						as.vector(BSACCRECGrowthlastvs3perma_LTM),
						as.vector(BSACCRECGrowth3permachg_LTM),
						as.vector(BSACCRECGrowth1pchgvs3pmachg_LTM),
						as.vector(BSPPEGROSSCONSTR_LTM),
						as.vector(MAvgBSPPEGROSSCONSTR_LTM),
						as.vector(ChgBSPPEGROSSCONSTR_LTM),
						as.vector(BSPPEGROSSCONSTRGrowth1perchg_LTM),
						as.vector(BSPPEGROSSCONSTRGrowthlastvs3perma_LTM),
						as.vector(BSPPEGROSSCONSTRGrowth3permachg_LTM),
						as.vector(BSPPEGROSSCONSTRGrowth1pchgvs3pmachg_LTM),
						as.vector(PricetoSalesfull_LTM),
						as.vector(MAvgPricetoSalesfull_LTM),
						as.vector(ChgPricetoSalesfull_LTM),
						as.vector(PricetoSalesfullGrowth1perchg_LTM),
						as.vector(PricetoSalesfullGrowthlastvs3perma_LTM),
						as.vector(PricetoSalesfullGrowth3permachg_LTM),
						as.vector(PricetoSalesfullGrowth1pchgvs3pmachg_LTM),
						as.vector(ROEnRRfull_LTM),
						as.vector(MAvgROEnRRfull_LTM),
						as.vector(ROEnRRfullGrowth1perchg_LTM),
						as.vector(ROEnRRfullGrowthlastvs3perma_LTM),
						as.vector(ROEnRRfullGrowth3permachg_LTM),
						as.vector(ROEnRRfullGrowth1pchgvs3pmachg_LTM),
						as.vector(OpMarginfull_LTM),
						as.vector(MAvgOpMarginfull_LTM),
						as.vector(OpMarginfullGrowth1perchg_LTM),
						as.vector(OpMarginfullGrowthlastvs3perma_LTM),
						as.vector(OpMarginfullGrowth3permachg_LTM),
						as.vector(OpMarginfullGrowth1pchgvs3pmachg_LTM),
						as.vector(netmarginfull_LTM),
						as.vector(MAvgnetmarginfull_LTM),
						as.vector(netmarginfullGrowth1perchg_LTM),
						as.vector(netmarginfullGrowthlastvs3perma_LTM),
						as.vector(netmarginfullGrowth3permachg_LTM),
						as.vector(netmarginfullGrowth1pchgvs3pmachg_LTM),
						as.vector(BSINTANGIBLE_LTM),
						as.vector(MAvgBSINTANGIBLE_LTM),
						as.vector(ChgBSINTANGIBLE_LTM),
						as.vector(BSINTANGIBLEGrowth1perchg_LTM),
						as.vector(BSINTANGIBLEGrowthlastvs3perma_LTM),
						as.vector(BSINTANGIBLEGrowth3permachg_LTM),
						as.vector(BSINTANGIBLEGrowth1pchgvs3pmachg_LTM),
						as.vector(BSINVENTORY_LTM),
						as.vector(MAvgBSINVENTORY_LTM),
						as.vector(ChgBSINVENTORY_LTM),
						as.vector(BSINVENTORYGrowth1perchg_LTM),
						as.vector(BSINVENTORYGrowthlastvs3perma_LTM),
						as.vector(BSINVENTORYGrowth3permachg_LTM),
						as.vector(BSINVENTORYGrowth1pchgvs3pmachg_LTM),
						as.vector(AssetTOMomentum_LTM),
						as.vector(MAvgAssetTOMomentum_LTM),
						as.vector(ChgAssetTOMomentum_LTM),
						as.vector(AssetTOMomentumGrowth1perchg_LTM),
						as.vector(AssetTOMomentumGrowthlastvs3perma_LTM),
						as.vector(AssetTOMomentumGrowth3permachg_LTM),
						as.vector(AssetTOMomentumGrowth1pchgvs3pmachg_LTM),
						as.vector(ROIC_LTM),
						as.vector(MAvgROIC_LTM),
						as.vector(ChgROIC_LTM),
						as.vector(ROICGrowth1perchg_LTM),
						as.vector(ROICGrowthlastvs3perma_LTM),
						as.vector(ROICGrowth3permachg_LTM),
						as.vector(ROICGrowth1pchgvs3pmachg_LTM),
						as.vector(PricetoEarningsfull_LTM),
						as.vector(MAvgPricetoEarningsfull_LTM),
						as.vector(ChgPricetoEarningsfull_LTM),
						as.vector(PricetoEarningsfull1perchg_LTM),
						as.vector(PricetoEarningsfulllastvs3perma_LTM),
						as.vector(PricetoEarningsfull3permachg_LTM),
						as.vector(PricetoEarningsfull1pchgvs3pmachg_LTM),
						as.vector(lasttoHistMaxfull_LTM),
						as.vector(MAvglasttoHistMaxfull_LTM),
						as.vector(ChglasttoHistMaxfull_LTM),
						as.vector(lasttoHistMaxfull1perchg_LTM),
						as.vector(lasttoHistMaxfulllastvs3perma_LTM),
						as.vector(lasttoHistMaxfull3permachg_LTM),
						as.vector(lasttoHistMaxfull1pchgvs3pmachg_LTM),
						as.vector(Debt_LTM),
						as.vector(MAvgDebt_LTM),
						as.vector(ChgDebt_LTM),
						as.vector(DebtGrowth1perchg_LTM),
						as.vector(DebtGrowthlastvs3perma_LTM),
						as.vector(DebtGrowth3permachg_LTM),
						as.vector(DebtGrowth1pchgvs3pmachg_LTM),
						as.vector(Reinvestmentfull_LTM),
						as.vector(MAvgReinvestmentfull_LTM),
						as.vector(ChgReinvestmentfull_LTM),
						as.vector(ReinvestmentfullGrowth1perchg_LTM),
						as.vector(ReinvestmentfullGrowthlastvs3perma_LTM),
						as.vector(ReinvestmentfullGrowth3permachg_LTM),
						as.vector(ReinvestmentfullGrowth1pchgvs3pmachg_LTM),
						as.vector(RetainedEarningstoEquityfull_LTM),
						as.vector(MAvgRetainedEarningstoEquityfull_LTM),
						as.vector(ChgRetainedEarningstoEquityfull_LTM),
						as.vector(RetainedEarningstoEquityfullGrowth1perchg_LTM),
						as.vector(RetainedEarningstoEquityfullGrowthlastvs3perma_LTM),
						as.vector(RetainedEarningstoEquityfullGrowth3permachg_LTM),
						as.vector(RetainedEarningstoEquityfullGrowth1pchgvs3pmachg_LTM),
						#as.vector(RetentionRatiofull_LTM),
						#as.vector(MAvgRetentionRatiofull_LTM),
						#as.vector(ChgRetentionRatiofull_LTM),
						#as.vector(RetentionRatiofull1perchg_LTM),
						#as.vector(RetentionRatiofulllastvs3perma_LTM),
						#as.vector(RetentionRatiofull3permachg_LTM),
						#as.vector(RetentionRatiofull1pchgvs3pmachg_LTM),
						as.vector(ROICVOLfull_LTM),
						as.vector(MAvgROICVOLfull_LTM),
						as.vector(ChgROICVOLfull_LTM),
						as.vector(ROICVOLfull1perchg_LTM),
						as.vector(ROICVOLfulllastvs3perma_LTM),
						as.vector(ROICVOLfull3permachg_LTM),
						as.vector(ROICVOLfull1pchgvs3pmachg_LTM),
						as.vector(SALESSDfull_LTM),
						as.vector(MAvgSALESSDfull_LTM),
						as.vector(ChgSALESSDfull_LTM),
						as.vector(SALESSDfull1perchg_LTM),
						as.vector(SALESSDfulllastvs3perma_LTM),
						as.vector(SALESSDfull3permachg_LTM),
						as.vector(SALESSDfull1pchgvs3pmachg_LTM),
						as.vector(OperatingLeveragefull_LTM),
						as.vector(MAvgOperatingLeveragefull_LTM),
						as.vector(ChgOperatingLeveragefull_LTM),
						as.vector(OperatingLeveragefullGrowth1perchg_LTM),
						as.vector(OperatingLeveragefullGrowthlastvs3perma_LTM),
						as.vector(OperatingLeveragefullGrowth3permachg_LTM),
						as.vector(OperatingLeveragefullGrowth1pchgvs3pmachg_LTM),
						as.vector(PCv1nInflation_LTM),
						as.vector(PCv1nInflationWCR_LTM),
						as.vector(SalesvsPCv1_LTM),
						as.vector(SalesvsPCv1nInflation_LTM),
						as.vector(SalesvsPCv1nInflationWCR_LTM),
						as.vector(MAvgSalesvsPCv1_LTM),
						as.vector(MAvgSalesvsPCv1nInflation_LTM),
						as.vector(MAvgSalesvsPCv1nInflationWCR_LTM),
						as.vector(RSalesvsPCv1_LTM),
						as.vector(RSalesvsPCv1nInflation_LTM),
						as.vector(RSalesvsPCv1nInflationWCR_LTM),
						as.vector(RMAvgPCv1_LTM),
						as.vector(RMAvgPCv1nInflation_LTM),
						as.vector(RMAvgPCv1nInflationWCR_LTM),
						as.vector(Rratio_LTM),
						as.vector(RMAvgratio_LTM),
						as.vector(ROICVOL_LTM),
						as.vector(SALESSD_LTM),
						as.vector(SALESrange_LTM),
						as.vector(ROErange_LTM),
						as.vector(ROEMomentum_LTM),
						as.vector(ROICrange_LTM),
						as.vector(GROSSMARGINchg_LTM),
						as.vector(MAvgGROSSMARGIN_LTM),
						as.vector(GROSSMARGINrange_LTM),
						as.vector(EBITvGROSSINCchg_LTM),
						as.vector(MAvgEBITvGROSSINC_LTM),
						as.vector(EBITvGROSSINCrange_LTM),
						as.vector(ASSETOchg_LTM),
						as.vector(MAvgASSETO_LTM),
						as.vector(ASSETOrange_LTM)
)

rmlistinput <- c(names(which(apply(inputtraingrowthEVAtemp[,2:ncol(inputtraingrowthEVAtemp)],2,function(x){length(which(!is.na(x)))/length(x)}) < 0.7)))
rmlistpred_ANN <- c(paste(substr(rmlistinput, 1, nchar(rmlistinput)-2),".",sep=""))
rmlistpred_LTM <- c(paste(substr(rmlistinput, 1, nchar(rmlistinput)-2),"_LTM.",sep=""))
#length(rmlistinput)
#length(rmlistpred_ANN)
#length(rmlistpred_LTM)
#rmlistinput %in% colnames(inputtraingrowthEVAtemp)
#rmlistpred_ANN %in% colnames(predvalsgrowthEVAtemp_ANN)
#rmlistpred_LTM %in% colnames(predvalsgrowthEVAtemp_LTM)

inputtraingrowthEVAtemptrim <- inputtraingrowthEVAtemp[ ,-which(colnames(inputtraingrowthEVAtemp) %in% rmlistinput)]
predvalsgrowthEVAtemp_ANNtrim <- predvalsgrowthEVAtemp_ANN[ ,-which(colnames(predvalsgrowthEVAtemp_ANN) %in% rmlistpred_ANN)]
predvalsgrowthEVAtemp_LTMtrim <- predvalsgrowthEVAtemp_LTM[ ,-which(colnames(predvalsgrowthEVAtemp_LTM) %in% rmlistpred_LTM)]



#### testing of process
predvalsgrowthEVAtemp_ANNtrim <- cbind(rep(rownames(BSASSETS_ANN),ncol(BSASSETS_ANN)),
					rep(colnames(BSASSETS_ANN),each = nrow(BSASSETS_ANN)),
					predvalsgrowthEVAtemp_ANNtrim)

predvalsgrowthEVAtemp_LTMtrim <- cbind(rep(rownames(ROEfull_LTM),ncol(ROEfull_LTM)),
					rep(colnames(ROEfull_LTM),each = nrow(ROEfull_LTM)),
					predvalsgrowthEVAtemp_LTMtrim)

ttdatamx <- inputtraingrowthEVAtemptrim
ttdatamx[,1] <- as.factor(ttdatamx[,1])
ttlogitvals <- FZ_logit_pvalue(input=ttdatamx)
rmlistlogitinput <- c(names(which(ttlogitvals<0.05)))
rmlistlogitpred_ANN <- c(paste(substr(rmlistlogitinput,1,nchar(rmlistlogitinput)-2),".",sep=""))
rmlistlogitpred_LTM <- c(paste(substr(rmlistlogitinput,1,nchar(rmlistlogitinput)-2),"_LTM.",sep=""))
#length(rmlistlogitinput)
#length(rmlistlogitpred)

inputtraingrowthEVAlogit <- inputtraingrowthEVAtemptrim[ ,-which(colnames(inputtraingrowthEVAtemptrim) %in% rmlistlogitinput)]
predvalsgrowthEVAlogit_ANN <- predvalsgrowthEVAtemp_ANNtrim[ ,-which(colnames(predvalsgrowthEVAtemp_ANNtrim) %in% rmlistlogitpred_ANN)]
predvalsgrowthEVAlogit_LTM <- predvalsgrowthEVAtemp_LTMtrim[ ,-which(colnames(predvalsgrowthEVAtemp_LTMtrim) %in% rmlistlogitpred_LTM)]
#ncol(inputtraingrowthEVAtemptrim)
#ncol(inputtraingrowthEVAlogit)
#ncol(predvalsgrowthEVAtemptrim)
#ncol(predvalsgrowthEVAlogit)

ttmanualclass_LTM <- ttmanualclass[,rep(1:ncol(ttmanualclass), times = rep(12,ncol(ttmanualclass)))]
colnames(ttmanualclass_LTM) <- rep(colnames(ttmanualclass),each=12)
ttmanualclass_LTM <- cbind(ttmanualclass_LTM,ttmanualclass_LTM[,ncol(ttmanualclass_LTM)])





########### For back test
ttsales <- FZreaddata(dataobj="D_PLSALESANN", dirname = "Datafiles")
tempnames <- colnames(ttsales)
ttsales <- ttsales %x% t(rep(1, 12))
colnames(ttsales) <- rep(tempnames,each=12)
ttsalesgrowth <- FZshiftmx(ttsales,shiftcol=36)/FZtrailingmax(ttsales) -1 
ttsalesgrowth <- cbind(ttsalesgrowth,ttsalesgrowth[,ncol(ttsalesgrowth)])
ttsalesgrowthoneper <- ttsales/FZshiftmx(input=ttsales,shiftcol=-12) - 1
ttsalesgrowthoneper <- FZmovingSD(ttsalesgrowthoneper,3)
ttsalesgrowthoneper <- cbind(ttsalesgrowthoneper,ttsalesgrowthoneper[,ncol(ttsalesgrowthoneper)])

ttbeta <- FZreaddata(dataobj="D_Marketbeta", dirname = "Datafiles")
ttbeta <- ttbeta %x% t(rep(1, 12))
colnames(ttbeta) <- rep(colnames(ttbeta),each=12)
ttbeta <- FZshiftmx(input=ttbeta,shiftcol=-3)
ttbeta <- cbind(ttbeta,ttbeta[,ncol(ttbeta)])

factors <- list(ttbeta,ttsalesgrowth,ttsalesgrowthoneper)
names(factors) <- c("Beta","Future Sales Growth","SD of Sales Growth")

###################### kNN
#ttBTkNNalldata_ANN <- FZhibacktestnonode(trainingmx = inputtraingrowthEVAlogit,predictionmx = predvalsgrowthEVAlogit_ANN,class_method1 = "knn",class_method2 = "knn",
#							ANA-EStrain_method = "repeatedcv",useweights = FALSE,binary=TRUE)


#ttincprediction <- ifelse(ttBTkNNalldata_ANN == "GROWTH",1,0)
#ttincprediction <- FZmovingavg(ttincprediction,maper = 12)
#ttBTkNNalldata_ANN3 <- ifelse(ttincprediction != 0, "GROWTH",ttBTkNNalldata_ANN)
#apply(ttBTkNNalldata_ANN3, 2, function(x){length(which(!is.na(x) & x =="GROWTH"))})

########################   NEW TESTS
ttBTkNNalldata_ANN <- FZhibacktestnonode(trainingmx = inputtraingrowthEVAlogit,predictionmx = predvalsgrowthEVAlogit_ANN,class_method1 = "knn",class_method2 = "knn",train_method = "repeatedcv",useweights = FALSE,binary=TRUE)
#write.csv(ttBTkNNalldata_ANN,"ttBTkNNalldata.csv")


ttincprediction_ANN <- ifelse(ttBTkNNalldata_ANN == "GROWTH",1,0)
ttincprediction_ANN  <- FZmovingavg(ttincprediction_ANN ,maper = 12)
ttBTkNNalldata_ANN_ma <- ifelse(ttincprediction_ANN  != 0, "GROWTH",ttBTkNNalldata_ANN)
write.csv(ttBTkNNalldata_ANN_ma,"ttBTkNNalldataMARF.csv")

# Gives charts on output 
FZhiClassPerf(training=ttmanualclass_ANNREP,prediction=ttBTkNNalldata_ANN_ma,factors=factors,title=NULL,Growthper=0)










######################################### SAM TAKE OVER ###############################################################

########## UNDERSTAND WHERE WE GOT TO

# input with Class split by time (but time column removed) - 31755 * 89 (removed when over 70% missing values) - TARGET, BUT TIME OR ID
nrow(inputtraingrowthEVAlogit)

# prediction (split by time with time columns and variables but no target) - 51903 * 90 - NO TARGET, BUT TIME AND ID 
nrow(predvalsgrowthEVAlogit_ANN)

# prediction output (prediction in each year for each stock - unqiue stock as row, time as each column) - 219 * 237 
View(ttBTkNNalldata_ANN)

# actual output (prediction in each year) - 219 * 237
ncol(ttmanualclass_ANNREP)

# build confusion matrix from previous models
ttBTkNNalldata_ANN
ttmanualclass_ANNREP


confusionMatrix(ttBTkNNalldata_ANN,ttmanualclass_ANNREP)
bmatrix = matrix(c(1543,76+115,36+490,19623), nrow = 2, ncol = 2)
bmatrix
(bmatrix[1] + bmatrix[4]) / sum(bmatrix)
# 0.967 <- for G/NG to beat!  

result <- confusionMatrix(ttBTkNNalldata_ANN,ttmanualclass_ANNREP)
precision <- result$byClass['Pos Pred Value']    
recall <- result$byClass['Sensitivity']
precision

### CREATE FRESH DATA
s_df = inputtraingrowthEVAlogit
s_target = names(inputtraingrowthEVAlogit)[[1]]
s_variables = names(inputtraingrowthEVAlogit)[c(2:length(names(inputtraingrowthEVAlogit)))]

################################### BUSINESS UNDERSTANDING 
# we want to pull G out of NG, looking for the non-linear line that divides G from not G
# G is currently defined (binned) by looking at metrics like change in sales - which are also in the model (time-series basically) 
# First thing to do then is explore the categories 

################################### DATA UNDERSTANDING 

# What is the distribution of target variable?
df = s_df
colnames(df)[which(names(df) == "as.vector.ttmanualclass_ANNREP...Scolfortrain.Ecolfortrain..")] <- "TARGET"

nrow(df)
dftail = tail(df,100)

summary(df$TARGET)
ggplot(df,aes(x=df$TARGET)) + geom_bar(aes(fill=TARGET)) + scale_color_brewer(palette="Dark2") + ggtitle("Distribution of Target Variable") +
  xlab("Target Variable") + ylab("Count") +
  theme_bw() + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5)) + geom_text(stat='count',aes(label=..count..),vjust=-1) 

# We can see there is a massive imbalance with Growth compared to the others - 8.12%
growth_target_percentage = paste(round(summary(df$TARGET)[1:1] / sum(summary(df$TARGET)[1:5]) * 100,2),"%")
growth_target_percentage

# even if we remove the NaS - 10.51%
df_nona = df[!is.na(df$TARGET),]
growth_target_percentage_nona = paste(round(summary(df_nona$TARGET)[1:1] / sum(summary(df_nona$TARGET)[1:4]) * 100,2),"%")
growth_target_percentage_nona

# Looks like we will have to do something to solve this imbalance problem
# ideas:
## Can we collect more data? 
## Can we change our performance metrics?
    # instead of accuracy paradox - use ConMat, precision, Recall, F1
    # could also look into Kappa (or Cohen's kappa) - normalises acc by imbalance of the classes in the data
    # ROC Curves - sensitivity and specificity balance 
## Can we try resampling our dataset? 
    # oversampling / sampling with replacement (better if not a lot of data)
    # undersampling (better if we had a lot of data)
      # we should try both approaches just to try 
      # also test random and non-random (stratified) sampling schemas 
      # considering testing different resampled ratios 
## Can we try generate synthetic samples 
    # Generate new rows based on stat methods of existing ones
      # pros more/different data, cons non-linear relationship between attributes may not be preserved
      # Try SMOTE from DMwR package 
## Can we try different algorithms for imbalanced data?
    # decision trees often perform well - C4.5, C5.0, CART, and Random Forest.
## Can we try penalized Models? 
    # add additional cost for making mistaken on the minor class - penalized-SVM and penalized-LDA
    # could try WEKA has a CostSensitiveClassifier to wrap any matrix 
    # good as can be tested on the algorithm already developed but compelx to set up
## Can we just try a different perspective?
    # Research anomaly detection / change detection literature 
    # could we use a one-class classified and treat G as 'outlier detection'
## Other/creative options?
    # Other ways to split this into smaller problems?
    # Is the way G/NG are classed right? should we relook at this? 
    # resample the set into SEVERAL balanced. run ensemble of classifiers 



# variable exploration
# what is the distribution of each class across some key numeric variables?
#remove nas
df = df_nona
ggplot(df, aes(df$as.vector.SalesGrowthfullt., fill = TARGET)) + geom_density(alpha = 0.2) + xlim(c(0,mean(df$as.vector.SalesGrowthfullt.,na.rm=TRUE)*2)) + ggtitle("as.vector.SalesGrowthfullt.")

# group G / NG
require("car")    
df$TARGET = recode(df$TARGET, "c('MATURECYCLICAL','MATUREDECLINER','MATURESTABLE')='NOT GROWTH'; else='GROWTH'")

summary(df$TARGET)


ggplot(df, aes(df$as.vector.SalesGrowthfullt., fill = TARGET)) + geom_density(alpha = 0.2) + xlim(c(0,mean(df$as.vector.SalesGrowthfullt.,na.rm=TRUE)*2)) + ggtitle("as.vector.SalesGrowthfullt.")

names(df)

for (name in names(df)[2:89]){
  print(name)
  var = df[name]
  plot = ggplot(df, aes(var, fill = df$TARGET)) + geom_density(alpha = 0.2) + xlim(c(0,2)) + ggtitle(name)
  print(plot)
}

ggplot(df, aes(df$as.vector.BSACCPAYt., fill = df$TARGET)) + geom_density(alpha = 0.2)# + xlim(c(0,2)) #+ ggtitle(name)

dev.off()

ggplot(df, aes(df$as.vector.BSACCPAYt., fill = df$TARGET)) + geom_density(alpha = 0.2) + xlim(c(0,500))

## THESE ARE THE MOST INTERESTING WITH DISTINCTIVE / DIFFERENTIATING PATTERNS BETWEEN G/NG 
# Operatingleveragefullgrowth
# MAvgBSACCPAYt
# MAVGRetainedearnings toequityfull
# BSPPEGROSS1PCHgvs3phelgt 


############## DATA PREP
# For now it looks like it is fine (until you go back and try balancing and algorithm specific methods)

############# MODELLING
# First create a baseline model to see how well it does 

# Create Logistic Regression as baseline model
require(nnet)

# remove all NAs 
df = df[complete.cases(df),]
sum(is.na(df))

df_save = df[complete.cases(df),]
sum(is.na(df))

log_results_list = NA
tuneresults_log <- data.frame()
seed_list = c(1:10)

for (j in seed_list){
  seed = j
  set.seed(seed)
  splitn = 0.8
  smp_size_80 <- floor(splitn * nrow(df))
  train_ind <- sample(seq_len(nrow(df)), size = smp_size_80)
  train_log <- df[train_ind, ]
  test_log <- df[-train_ind, ]
  
  logistic_model = multinom(data =  test_log, TARGET~ .)
  
  pred_log = predict(logistic_model, newdata = test_log)
  
  conmatrix_log = table(pred_log,test_log$TARGET)
  confusionMatrix(pred_log,test_log$TARGET)
  acc_log = (conmatrix_log[1] + conmatrix_log[4]) / sum(conmatrix_log)
  log_results_list <- append(log_results_list, acc_log)
  print("ACC:")
  print(conmatrix_log)
  
  tempresults_log <- data.frame()
  tempresults_log <- rbind(c(seed,acc_log))
  colnames(tempresults_log) <- c("seed","acc")
  
  tuneresults_log <- rbind(tuneresults_log,tempresults_log)
  
}

results_log = log_results_list[2:length(log_results_list)]
#write.csv(tuneresults_log,"logistic_regression_results_v1.csv")
results_log

ggplot(data=tuneresults_log, aes(x =tuneresults_log$seed, y=tuneresults_log$acc)) + geom_point(alpha=0.2)+ geom_hline(yintercept = mean(tuneresults_log$acc), color="blue") +  annotate("text", size = 5, color = "red", x = mean(tuneresults_log$seed), y = mean(tuneresults_log$acc + 0.005), label = paste("Average Acc: ", mean(tuneresults_log$acc))) + geom_hline(yintercept = max(tuneresults_log$acc), color="blue") +  annotate("text", size = 5, color = "red", x = mean(tuneresults_log$seed), y = max(tuneresults_log$acc - 0.005), label = paste("Max Acc: ", max(tuneresults_log$acc))) + ggtitle("Logistic Regression Results")

max(tuneresults_log$acc)
# MAX 0.9, # AVG 0.89 (SEED 1:30)

confusionMatrix(pred_log,test_log$TARGET)

# SO WHAT WE CAN CLEARLY SEE HERE IS AN EXAMPLE OF THE 'ACCURACY PARADOX'
# A BINARY MODEL SCORES WELL BECAUSE IT PUTS ALMOST EVERYTHING (99%) INTO THE MAJORITY CLASS 


# Try again with a bit more of a sophisticated model (RF) and then dive into iterating through BALANCING APPROACHES experiments 
require(dplyr)
tuneresults <- data.frame()

seed_list = c(1)
#seed_list = c(162)

runs = seq(500,600,100)

for (i in seed_list){
  seed = i
  set.seed(seed)
  splitn = 0.8
  smp_size_80 <- floor(splitn * nrow(df))
  train_ind <- sample(seq_len(nrow(df)), size = smp_size_80)
  train_1 <- df[train_ind, ]
  test_1 <- df[-train_ind, ]
  
  for (j in runs){
    
    fit <- randomForest(TARGET ~ .,  
                        method="class",
                        #na.action = 'remove',
                        minsplit=10, 
                        data=train_1, 
                        importance = TRUE, 
                        localImp=TRUE,
                        replace = TRUE,
                        #cutoff=0.7,
                        #classwt=c(0.33,0.33,0.33),
                        mtry=3,
                        ncores=TRUE,
                        ntree= j)
    
    pred = predict(fit, test_1 %>% select(-TARGET))
    conmatrix = table(pred,test_1$TARGET)
    acc = (conmatrix[1] + conmatrix[4]) / sum(conmatrix)
    acc
    
    tempresults <- data.frame()
    tempresults <- rbind(c(seed,splitn,fit$ntree,fit$mtry,acc))
    colnames(tempresults) <- c("seed","splitn","ntree","mtry","acc")
    
    tuneresults <- rbind(tuneresults,tempresults)
  }
}


ncol(predvalsgrowthEVAlogit_ANN)
ncol(test_1)

pred2 = predict(fit, predvalsgrowthEVAlogit_ANN[3:length(predvalsgrowthEVAlogit_ANN)])
pred2



View(predvalsgrowthEVAlogit_ANN[3:length(predvalsgrowthEVAlogit_ANN)])
View(test_1)

tuneresults_RF = tuneresults
tuneresults_RF_first = tuneresults_RF
#View(tuneresults_RF)
#write.csv(tuneresults_RF, 'RF_ntree_results_v3.csv')

ggplot(data=tuneresults_RF, aes(x =tuneresults_RF$seed, y=tuneresults_RF$acc)) + geom_point(alpha=0.1)+ geom_hline(yintercept = mean(tuneresults_RF$acc), color="blue") +  annotate("text", size = 5, color = "red", x = mean(tuneresults_RF$seed), y = mean(tuneresults_RF$acc + 0.005), label = paste("Average Acc: ", mean(tuneresults_RF$acc))) + geom_hline(yintercept = max(tuneresults_RF$acc), color="blue") +  annotate("text", size = 5, color = "red", x = mean(tuneresults_RF$seed), y = max(tuneresults_RF$acc - 0.005), label = paste("Max Acc: ", max(tuneresults_RF$acc))) + ggtitle("Random Forest Regression Results")
# get results then set up system to check BALANCE METHODS 

library(caret)

confusionMatrix(pred,test_1$TARGET)

result <- confusionMatrix(pred,test_1$TARGET)
precision <- result$byClass['Pos Pred Value']    
recall <- result$byClass['Sensitivity']

precision
recall

f_measure <- 2 * ((precision * recall) / (precision + recall))
f_measure

summary(df$TARGET)
ggplot(df,aes(x=df$TARGET)) + geom_bar(aes(fill=TARGET)) + scale_color_brewer(palette="Dark2") + ggtitle("Distribution of Target Variable") +
  xlab("Target Variable") + ylab("Count") +
  theme_bw() + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5)) + geom_text(stat='count',aes(label=..count..),vjust=-1) 






# SET UP EXPERIMENT SPACE WITH RESULT SAVES (COFMAT + ALL PERFORMANCE METRICS)
# Try different performance metrics - precision, Recall, F1 + KAPPA/Cohen's Kappa + ROC curve sensitivity/specificity
# Try oversampling
# Try undersampling 
# Try random / non-random stratified sampling 
# Try generative synthetic sample (SMOTE in DMwR)
# Try different algorithms for imbalanced - C4.5, C5.0, CART, and Random Forest
# Try penalized models - penalized-SVM and penalized-LDA + WEKA has a CostSensitiveClassifier to wrap any matrix 
# Try 'outlier detection' anomaly detection
# Try other creative options (smaller problems, resample the set into SEVERAL balanced run ensemble of classified - e.g. G/MS , G/MC)


# TEST WITH OUT OF SAMPLE 
 

