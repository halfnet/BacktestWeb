library(shiny)
library(rhandsontable)
library(shinyjs)
library(shinyBS)
library(dplyr)
library(stringr)
library(mondate)
library(DataCombine)
library(data.table)
library(openxlsx)
library(TTR)
library(tryCatchLog)
options(keep.source = TRUE)
options(tryCatchLog.write.error.dump.file = FALSE)
options(tryCatchLog.silent.messages       = TRUE)
options(tryCatchLog.silent.warnings       = TRUE)

########## global variables ##########
CS_MaxRange_Q = 0
CS_MaxRange_SA = 0
CS_AdjEntryDate_123Q = 0
CS_AdjEntryDate_1S = 0
CS_AdjEntryDate_4Q = 0
CS_AdjEntryDate_2S = 0
CS_Opinion_NA = 0
CS_TableOption = 0

BuyM_RatioCutOff = 0
HoldM_RatioCutOff = 0
ConstructModelPortDtSPHold = 0
ConstructModelPortDtSPBuy = 0

TScoreHoldFilter = 0
TScoreBuyFilter = 0
TScoreCutOff = 0
TScoreNA = 0
OutPerformMultiple = 0
MaxWeightPerName = 0
MaxLimit = 0

IndexDataFolder = ""
IndexLvlDataFolder = ""
Ind_Lvl1 = ""
Ind_Lvl2 = ""
Ind_Lvl3 = ""
Ind_Lvl4 = ""

TH_MinWght = 0
TH_MA = 0
TransactionCost = 0

params=NULL
TH.Limits=NULL
Indices=NULL
Periods=NULL

trans=NULL
scores=NULL


init_var <- function() {
    

    ########## initialize variables ##########
    params <<- read.csv("parameters.csv")
    TH.Limits <<- read.csv("indices.csv")
    TH.Limits <<- subset(TH.Limits, TH.Limits$Include)
    for(i in 1:nrow(TH.Limits)) {
        if (grepl("-", TH.Limits$StartFrom[i])) {
            TH.Limits$StartFrom = as.Date(TH.Limits$StartFrom, "%Y-%m-%d")
            break
        }
        if (grepl("/", TH.Limits$StartFrom[i])) {
            TH.Limits$StartFrom = as.Date(TH.Limits$StartFrom, "%m/%d/%Y")
            break
        }
    }
    
    Indices <<- as.vector(TH.Limits$Indices)

    Periods <<- read.csv("periods.csv")
    for(i in 1:nrow(Periods)) {
        if (grepl("-", Periods$Start[i])) {
            Periods$Start = as.Date(Periods$Start, "%Y-%m-%d")
            Periods$End = as.Date(Periods$End, "%Y-%m-%d")
        }
        if (grepl("/", Periods$Start[i])) {
            Periods$Start = as.Date(Periods$Start, "%m/%d/%Y")
            Periods$End = as.Date(Periods$End, "%m/%d/%Y")
        }
    }
    
    ########## setting parameters ##########
    CS_MaxRange_Q <<- as.numeric(as.vector(params$value[params$name=="CS_MaxRange_Q"]))
    CS_MaxRange_SA <<- as.numeric(as.vector(params$value[params$name=="CS_MaxRange_SA"]))
    CS_AdjEntryDate_123Q <<- as.numeric(as.vector(params$value[params$name=="CS_AdjEntryDate_123Q"]))
    CS_AdjEntryDate_1S <<- as.numeric(as.vector(params$value[params$name=="CS_AdjEntryDate_1S"]))
    CS_AdjEntryDate_4Q <<- as.numeric(as.vector(params$value[params$name=="CS_AdjEntryDate_4Q"]))
    CS_AdjEntryDate_2S <<- as.numeric(as.vector(params$value[params$name=="CS_AdjEntryDate_2S"]))
    CS_Opinion_NA <<- as.numeric(as.vector(params$value[params$name=="CS_Opinion_NA"]))
    CS_TableOption <<- as.numeric(as.vector(params$value[params$name=="CS_TableOption"]))
    
    BuyM_RatioCutOff <<- as.numeric(as.vector(params$value[params$name=="BuyM_RatioCutOff"]))
    HoldM_RatioCutOff <<- as.numeric(as.vector(params$value[params$name=="HoldM_RatioCutOff"]))
    ConstructModelPortDtSPHold <<- as.numeric(as.vector(params$value[params$name=="ConstructModelPortDtSPHold"]))
    ConstructModelPortDtSPBuy <<- as.numeric(as.vector(params$value[params$name=="ConstructModelPortDtSPBuy"]))
    
    TScoreHoldFilter <<- as.numeric(as.vector(params$value[params$name=="TScoreHoldFilter"]))
    TScoreBuyFilter <<- as.numeric(as.vector(params$value[params$name=="TScoreBuyFilter"]))
    TScoreCutOff <<- as.numeric(as.vector(params$value[params$name=="TScoreCutOff"]))
    TScoreNA <<- as.numeric(as.vector(params$value[params$name=="TScoreNA"]))
    OutPerformMultiple <<- as.numeric(as.vector(params$value[params$name=="OutPerformMultiple"]))
    MaxWeightPerName <<- as.integer(as.vector(params$value[params$name=="MaxWeightPerName"]))
    MaxLimit <<- MaxWeightPerName - 0.05
    
    IndexDataFolder <<- as.character(as.vector(params$value[params$name=="IndexDataFolder"]))
    IndexLvlDataFolder <<- as.character(as.vector(params$value[params$name=="IndexLvlDataFolder"]))
    CompanyScoreFolder <<- as.character(as.vector(params$value[params$name=="CompanyScoreFolder"]))
    Ind_Lvl1 <<- as.character(as.vector(params$value[params$name=="Ind_Lvl1"]))
    Ind_Lvl2 <<- as.character(as.vector(params$value[params$name=="Ind_Lvl2"]))
    Ind_Lvl3 <<- as.character(as.vector(params$value[params$name=="Ind_Lvl3"]))
    Ind_Lvl4 <<- as.character(as.vector(params$value[params$name=="Ind_Lvl4"]))
    
    TH_MinWght <<- as.numeric(as.vector(params$value[params$name=="TH_MinWght"]))
    TH_MA <<- as.numeric(as.vector(params$value[params$name=="TH_MA"]))

    TransactionCost <<- as.numeric(as.vector(params$value[params$name=="TransactionCost"]))
}

# this function loads translation template
loadtrans <- function(RootFolder) {


    ########## load translation file ##########
    trans = read.xlsx(xlsxFile=paste(RootFolder, "Translation template.xlsx", sep = ""),
                      sheet = "Bond translation",
                      startRow = 14,
                      detectDates = TRUE,
                      na.strings = c(""),
                      cols = c(1,5,17,18))
    trans = subset(trans, is.na(trans$Bond.Ticker)==FALSE)
    trans = subset(trans, trimws(trans$Bond.Ticker)!="")
    # remove duplicated date + ticker
    trans = trans %>% distinct(FROM.DATE.LINK,Bond.Ticker, .keep_all = TRUE)
    trans = trans[order(trans$Bond.Ticker, trans$FROM.DATE.LINK),]
    trans$index = seq.int(nrow(trans))-1
    trans$index1 = trans$index + 1
    trans$index = paste(trans$Bond.Ticker, trans$index)
    trans$index1 = paste(trans$Bond.Ticker, trans$index1)
    trans = merge(trans, trans[ , c("index","FROM.DATE.LINK")],
                  by.x="index1", by.y="index", all.x=TRUE)
    names(trans)[names(trans) == "FROM.DATE.LINK.x"] = "FROM.DATE.LINK"
    names(trans)[names(trans) == "FROM.DATE.LINK.y"] = "TO.DATE.LINK"
    trans$TO.DATE.LINK = ifelse(is.na(trans$TO.DATE.LINK), "2020-01-01", trans$TO.DATE.LINK)
    # convert TO.DATE.LINK back to date type
    trans$TO.DATE.LINK =as.Date(trans$TO.DATE.LINK, "%Y-%m-%d")
    # convert FROM.DATE.LINK back to date type
    trans$FROM.DATE.LINK =as.Date(trans$FROM.DATE.LINK, "%Y-%m-%d")
    trans$ID_BB_UNIQUE = gsub("EQ", "", trans$ID_BB_UNIQUE)
    trans = transform(trans, ID_BB_UNIQUE = as.numeric(ID_BB_UNIQUE))
    
    return (trans)
}

# this function loads translation template for equity
loadtransequity <- function(RootFolder) {
    

    ########## load translation file ##########
    trans = read.xlsx(xlsxFile=paste(RootFolder, "Translation template.xlsx", sep = ""),
                      sheet = "Equity translation",
                      startRow = 14,
                      detectDates = TRUE,
                      na.strings = c(""),
                      cols = c(1,2,11,12))
    trans = subset(trans, is.na(trans$ORIGINAL.TICKER)==FALSE)
    trans = subset(trans, trimws(trans$ORIGINAL.TICKER)!="")
    # remove duplicated date + ticker
    trans = trans %>% distinct(FROM.DATE.LINK,ORIGINAL.TICKER, .keep_all = TRUE)
    trans = trans[order(trans$ORIGINAL.TICKER, trans$FROM.DATE.LINK),]
    trans$index = seq.int(nrow(trans))-1
    trans$index1 = trans$index + 1
    trans$index = paste(trans$ORIGINAL.TICKER, trans$index)
    trans$index1 = paste(trans$ORIGINAL.TICKER, trans$index1)
    trans = merge(trans, trans[ , c("index","FROM.DATE.LINK")],
                  by.x="index1", by.y="index", all.x=TRUE)
    names(trans)[names(trans) == "FROM.DATE.LINK.x"] = "FROM.DATE.LINK"
    names(trans)[names(trans) == "FROM.DATE.LINK.y"] = "TO.DATE.LINK"
    trans$TO.DATE.LINK = ifelse(is.na(trans$TO.DATE.LINK), "2020-01-01", trans$TO.DATE.LINK)
    # convert TO.DATE.LINK back to date type
    trans$TO.DATE.LINK =as.Date(trans$TO.DATE.LINK, "%Y-%m-%d")
    # convert FROM.DATE.LINK back to date type
    trans$FROM.DATE.LINK =as.Date(trans$FROM.DATE.LINK, "%Y-%m-%d")
    trans$ID_BB_UNIQUE = gsub("EQ", "", trans$ID_BB_UNIQUE)
    trans = transform(trans, ID_BB_UNIQUE = as.numeric(ID_BB_UNIQUE))
    
    return (trans)
}

# this function returns index level monthly file
indexlvlmonthlydata <- function(RootFolder, IndexLvlDataFolder) {


    file.list <- list.files(path=paste(RootFolder, IndexLvlDataFolder, "Monthly", sep=""),
                            pattern='*.csv',
                            full.names=TRUE)
    df <- lapply(file.list, read.csv, skip=2, sep=";")
    df <- do.call("rbind", df)
    df=subset(df, trimws(df$Index)!="")
    df$Date=as.Date(as.character(df$Date), "%Y%m%d")
    # remove unused columns
    df = df[, !(colnames(df) %in% c("X"))]    
    
    return(df)
}

# this function returns index level file
indexlvldata <- function(RootFolder, IndexLvlDataFolder) {
    

    file.list <- list.files(path=paste(RootFolder, IndexLvlDataFolder, "BOFA index lvl history", sep=""),
                            pattern='*.xlsx',
                            full.names=TRUE)
    df <- lapply(file.list, read.xlsx)
    df <- do.call("rbind", df)
    df$Date=as.Date(as.character(df$Date), "%Y%m%d")
    # converting from excel date to R
    # rename columns
    names(df)[names(df) == "TRR.%.1-month.LOC"] = "TRR...1.month.LOC"
    names(df)[names(df) == "Excess.Rtn.%.1-month"] = "Excess.Rtn...1.month"
    
    # add monthly files    
    df=rbind(df, indexlvlmonthlydata(RootFolder, IndexLvlDataFolder))   
    df$PK=paste(df$Index, df$Date, sep="")
    # remove duplicate PK
    df = df %>% distinct(PK, .keep_all = TRUE)    
    return(df)
}

# this function returns index monthly file
indexmonthlydata <- function(index, RootFolder, IndexDataFolder) {
    

    file.list <- list.files(path=paste(RootFolder, IndexDataFolder, "Monthly", sep=""),
                            pattern='*.csv',
                            full.names=TRUE)
    df <- lapply(file.list, read.csv, skip=3, sep=";")
    df <- do.call("rbind", df)
    df=subset(df, df$Index.Name==index)
    df$As.of.Date=as.Date(df$As.of.Date, "%m/%d/%Y")
    df$Maturity.Date=as.Date(df$Maturity.Date, "%m/%d/%Y")
    colnames(df)[6] = "Bond.Name"
    # remove unused columns
    df = df[, !(colnames(df) %in% c("Description", "X"))]    
    return(df)
}

# this function returns index file
indexdata <- function(index, RootFolder, IndexDataFolder, StartFrom) {
    

    file.list <- list.files(path=paste(RootFolder, IndexDataFolder, "BOFA ", index, " index history", sep=""),
                            pattern='*.xlsx',
                            full.names=TRUE)
    df <- lapply(file.list, read.xlsx)
    df <- do.call("rbind", df)
    df$As.of.Date=as.Date(as.character(df$As.of.Date), "%Y%m%d")
    df = subset(df, df$As.of.Date>=StartFrom)
    # converting from excel date to R
    # surpress warning
    #options(warn = -1)
    df$Maturity.Date=suppressWarnings(as.Date(as.numeric(df$Maturity.Date), origin="1899-12-30"))
    #options(warn = 0)
    
    colnames(df)[6] = "Bond.Name"
    # remove unused columns
    df = df[, !(colnames(df) %in% c("Description"))]
    # rename columns
    names(df)[names(df) == "Mkt.%.Index.Wght"] = "Mkt...Index.Wght"
    names(df)[names(df) == "PrevMend.Mkt.%.Index.Wght"] = "PrevMend.Mkt...Index.Wght"
    names(df)[names(df) == "TRR.%.MTD.LOC"] = "TRR...MTD.LOC"
    names(df)[names(df) == "Excess.Rtn.%.MTD"] = "Excess.Rtn...MTD"
    
    # add monthly files    
    df=rbind(df, indexmonthlydata(index, RootFolder, IndexDataFolder))   
    df=subset(df, df$Ticker!="CASH")
    df$PK=paste(df$Index.Name, df$Cusip, df$As.of.Date, sep="")
    # remove duplicate PK
    df = df %>% distinct(PK, .keep_all = TRUE)    
    df$PrevMend.Mod.Dur.To.Worst[is.na(df$PrevMend.Mod.Dur.To.Worst)]=0
    df$PrevMend.AssetSwp[is.na(df$PrevMend.AssetSwp)]=0
    if (Ind_Lvl1!="ALL") df = subset(df, df$ML.Industry.Lvl.1==Ind_Lvl1)
    if (Ind_Lvl2!="ALL") df = subset(df, df$ML.Industry.Lvl.2==Ind_Lvl2)
    if (Ind_Lvl3!="ALL") df = subset(df, df$ML.Industry.Lvl.3==Ind_Lvl3)
    if (Ind_Lvl4!="ALL") df = subset(df, df$ML.Industry.Lvl.4==Ind_Lvl4)
    return(df)
}

# this function returns equity data file
equitydata <- function(RootFolder) {
    

    file.list <- list.files(path=paste(RootFolder, "Equity raw data", sep=""),
                            pattern='*.xlsx',
                            full.names=TRUE)
    df <- lapply(file.list, read.xlsx)
    df <- do.call("rbind", df)
    # converting from excel date to R
    df$As.of.Date=suppressWarnings(as.Date(as.numeric(df$As.of.Date), origin="1899-12-30"))

    # rename columns
    names(df)[names(df) == "Original.Ticker"] = "Ticker"

    df$PK=paste(df$Index.Name, df$Ticker, df$As.of.Date, sep="")
    # remove duplicate PK
    df = df %>% distinct(PK, .keep_all = TRUE)    
    return(df)
}

# this function returns current holdings file
ch_data <- function(index, RootFolder, IndexDataFolder, StartFrom) {
    

    #print(TH_MA)
    file.list <- list.files(path=paste(RootFolder, IndexDataFolder, "CurrentHoldings", sep=""),
                            pattern='*.csv',
                            full.names=TRUE)
    if (length(file.list) >0) {
        # if projected file exist
        df <- lapply(file.list, read.csv, skip=3, sep=";")
        df <- do.call("rbind", df)
        df=subset(df, df$Index.Name==index)
        df$As.of.Date=as.Date(df$As.of.Date, "%m/%d/%Y")
        df$Maturity.Date=as.Date(df$Maturity.Date, "%m/%d/%Y")
        colnames(df)[6] = "Bond.Name"
        # remove unused columns
        df = df[, !(colnames(df) %in% c("Description", "X"))]    
        df=subset(df, df$Ticker!="CASH")
        df$PK=paste(df$Index.Name, df$Cusip, df$As.of.Date, sep="")
        # remove duplicate PK
        df = df %>% distinct(PK, .keep_all = TRUE)    
        df$PrevMend.Mod.Dur.To.Worst[is.na(df$PrevMend.Mod.Dur.To.Worst)]=0
        df$PrevMend.AssetSwp[is.na(df$PrevMend.AssetSwp)]=0
        if (Ind_Lvl1!="ALL") df = subset(df, df$ML.Industry.Lvl.1==Ind_Lvl1)
        if (Ind_Lvl2!="ALL") df = subset(df, df$ML.Industry.Lvl.2==Ind_Lvl2)
        if (Ind_Lvl3!="ALL") df = subset(df, df$ML.Industry.Lvl.3==Ind_Lvl3)
        if (Ind_Lvl4!="ALL") df = subset(df, df$ML.Industry.Lvl.4==Ind_Lvl4)
        
        # add X more month from regular index file
        df2 = indexdata(index, RootFolder, IndexDataFolder, StartFrom)
        # get rid of the monthly file that is later than the project file
        df2 = subset(df2, df2$As.of.Date<max(df$As.of.Date))
        #print(nrow(df2))
        df2 = subset(df2, df2$As.of.Date<=max(df2$As.of.Date) & df2$As.of.Date>=as.Date(as.mondate(max(df2$As.of.Date))-TH_MA+1))
        df = rbind(df, df2)
    } else {
        # if projected file doesn't exist, return X most recent months
        df = indexdata(index, RootFolder, IndexDataFolder, StartFrom)
        #print(nrow(df))
        df = subset(df, df$As.of.Date<=max(df$As.of.Date) & df$As.of.Date>=as.Date(as.mondate(max(df$As.of.Date))-TH_MA))
    }
    
    # rename columns here
    setnames(df, old=c("PrevMend.AssetSwp", "Asset.Swap", "PrevMend.Mod.Dur.To.Worst", "Semi.Mod.Dur.To.Worst", "PrevMend.Mkt...Index.Wght", "Mkt...Index.Wght"), 
             new=c("Asset.Swap", "PrevMend.AssetSwp", "Semi.Mod.Dur.To.Worst", "PrevMend.Mod.Dur.To.Worst", "Mkt...Index.Wght", "PrevMend.Mkt...Index.Wght"))
    
    return(df)
}

# this function returns the first day of the month
monthStart <- function(x) {


    x <- as.POSIXlt(x)
    x$mday <- 1
    as.Date(x)
}

# this function is one of the 3 score to opinion translation functions
scoreToOp1 <- function(rf, na0, na1, na2, t0, t1, t2) {

    ifelse(rf=="Q",
           ifelse(na0>CS_Opinion_NA,-3,
                  ifelse(t0>=9,1,
                         ifelse(t0==8 & (na1<=CS_Opinion_NA & t1>=9),1,
                                ifelse(t0==8 & (na1<=CS_Opinion_NA & t1==8) & (na2<=CS_Opinion_NA & t2>=8),1, 
                                       ifelse(t0<=3,-1,
                                              ifelse(t0==4 & (na1<=CS_Opinion_NA & t1<=4) & (na2<=CS_Opinion_NA & t2<=4),-1,
                                                     ifelse(t0>=7 & (na1<=CS_Opinion_NA & t1>=7),0,-2))))))),
           ifelse(na0>CS_Opinion_NA,-3,
                  ifelse(t0>=9,1,
                         ifelse(t0==8 & (na1<=CS_Opinion_NA & t1>=8),1,
                                ifelse(t0<=3,-1,
                                       ifelse(t0==4 & (na1<=CS_Opinion_NA & t1<=4),-1,
                                              ifelse(t0>=7,0,-2))))))
    )
    
}

# this function is one of the 3 score to opinion translation functions
scoreToOp2 <- function(rf, na0, na1, na2, t0, t1, t2) {

    ifelse(rf=="Q",
           ifelse(na0>CS_Opinion_NA,-2,
                  ifelse(t0>=9,0,
                         ifelse(t0==8 & (na1<=CS_Opinion_NA & t1>=9),0,
                                ifelse(t0==8 & (na1<=CS_Opinion_NA & t1==8) & (na2<=CS_Opinion_NA & t2>=8),0, 
                                       ifelse(t0<=3,-1,
                                              ifelse(t0==4 & (na1<=CS_Opinion_NA & t1<=4) & (na2<=CS_Opinion_NA & t2<=4),-1,
                                                     ifelse(t0>=7 & (na1<=CS_Opinion_NA & t1>=7),0,NA))))))),
           ifelse(na0>CS_Opinion_NA,-2,
                  ifelse(t0>=9,0,
                         ifelse(t0==8 & (na1<=CS_Opinion_NA & t1>=8),0,
                                ifelse(t0<=3,-1,
                                       ifelse(t0==4 & (na1<=CS_Opinion_NA & t1<=4),-1,
                                              ifelse(t0>=7,0,NA))))))
    )
}

# this function is one of the 3 score to opinion translation functions
scoreToOp <- function(df) {
    

    df = df[order(df$AdjDate),]
    df$M.OP1 = scoreToOp1(df$RF, df$M.NA, df$T1.M.NA, df$T2.M.NA, df$M.SCORE, df$T1.M.SCORE, df$T2.M.SCORE)
    df$M.OP2 = scoreToOp2(df$RF, df$M.NA, df$T1.M.NA, df$T2.M.NA, df$M.SCORE, df$T1.M.SCORE, df$T2.M.SCORE)
    df = FillDown(df, 'M.OP2')
    df$M.OP2[is.na(df$M.OP2)] = -2
    df$M.OP = ifelse(df$M.OP1==-3|df$M.OP1==-1|df$M.OP1==0|df$M.OP1==1, df$M.OP1, df$M.OP2)    
    return (df)
}

# this function turns scores into opinions
scores2opinions <- function(RootFolder) {
    

    ########## scores to opinions ##########
    transRF = trans[, c("ID_BB_UNIQUE","REPORTING_FREQUENCY")]
    transRF = subset(transRF, is.na(transRF$REPORTING_FREQUENCY)==FALSE)
    transRF = subset(transRF, trimws(transRF$REPORTING_FREQUENCY)!="")
    transRF = subset(transRF, trimws(transRF$REPORTING_FREQUENCY)!="A")
    transRF = subset(transRF, trimws(transRF$REPORTING_FREQUENCY)!="0")
    # remove duplicated date + ticker
    transRF = transRF %>% distinct(ID_BB_UNIQUE,REPORTING_FREQUENCY, .keep_all = TRUE)
    
    scores = read.csv(paste(RootFolder, CompanyScoreFolder, "BondsScoresBackT.csv", sep = ""))
    scores = subset(scores, trimws(scores$BBERG_ID)!="")
    scores = subset(scores, nchar(as.character(scores$COMPANY_QUARTER))==6)
    scores = subset(scores, scores$TABLES!="RS")
    names(scores)[names(scores) == "MOMENTUM_11"] = "M.FORMULA_11"
    scores = scores[ ,c("BBERG_ID","TABLES","COMPANY_QUARTER","M.SCORE", "M.NA","T.SCORE","T.NA","L.SCORE","L.NA","M.FORMULA_11","DT_ENTRY","DATE")]
    scores$BBERG_ID = gsub("EQ", "", scores$BBERG_ID)
    scores = transform(scores, BBERG_ID = as.numeric(BBERG_ID))
    
    scores$DATE = as.Date(scores$DATE, "%b %d %Y")
    scores$DT_ENTRY = as.Date(scores$DT_ENTRY, "%b %d %Y")
    scores$COMPANY_QUARTER = substr(scores$COMPANY_QUARTER,1,2)
    scores$AdjDate = ifelse(scores$COMPANY_QUARTER=="4Q",scores$DATE+CS_AdjEntryDate_4Q,
                            ifelse(scores$COMPANY_QUARTER=="1S",scores$DATE+CS_AdjEntryDate_1S,
                                   ifelse(scores$COMPANY_QUARTER=="2S",scores$DATE+CS_AdjEntryDate_2S,
                                          scores$DATE+CS_AdjEntryDate_123Q)))
    scores$AdjDate = ifelse(scores$DT_ENTRY>scores$AdjDate,scores$AdjDate,scores$DT_ENTRY)
    scores$AdjDate =as.Date(scores$AdjDate, origin="1970-01-01")
    
    scores = merge(scores, transRF, by.x="BBERG_ID", by.y="ID_BB_UNIQUE")
    if (CS_TableOption==1) {
        scores$FilterByTable = ifelse(scores$TABLES=="OR" & grepl("M", scores$REPORTING_FREQUENCY),1,
                                      ifelse(scores$TABLES=="MR" & !grepl("M", scores$REPORTING_FREQUENCY),1,0))
    } else if  (CS_TableOption==2) {
        scores$FilterByTable = ifelse(scores$TABLES=="OR",1,0)
    } else if  (CS_TableOption==3) {
        scores$FilterByTable = ifelse(scores$TABLES=="MR",1,0)    
    } else {
        scores$FilterByTable = 1
    }
    scores = subset(scores, scores$FilterByTable==1)
    scores$RF = ifelse(grepl("Q", scores$REPORTING_FREQUENCY), "Q", "SA")
    scores = scores[, !(colnames(scores) %in% c("DT_ENTRY","DATE","FilterByTable","COMPANY_QUARTER","REPORTING_FREQUENCY"))]
    scores = scores[order(scores$BBERG_ID, scores$AdjDate),]
    scores$index = seq.int(nrow(scores))-1
    scores$index1 = scores$index + 1
    scores$index2 = scores$index + 2
    scores$index = paste(scores$BBERG_ID, "#", scores$index)
    scores$index1 = paste(scores$BBERG_ID, "#", scores$index1)
    scores$index2 = paste(scores$BBERG_ID, "#", scores$index2)
    scores = merge(scores, scores[ , c("index1","M.SCORE", "M.NA")],
                   by.x="index", by.y="index1", all.x=TRUE)
    names(scores)[names(scores) == "M.SCORE.x"] = "M.SCORE"
    names(scores)[names(scores) == "M.NA.x"] = "M.NA"
    names(scores)[names(scores) == "M.SCORE.y"] = "T1.M.SCORE"
    names(scores)[names(scores) == "M.NA.y"] = "T1.M.NA"
    scores = merge(scores, scores[ , c("index2","M.SCORE", "M.NA")],
                   by.x="index", by.y="index2", all.x=TRUE)
    names(scores)[names(scores) == "M.SCORE.x"] = "M.SCORE"
    names(scores)[names(scores) == "M.NA.x"] = "M.NA"
    names(scores)[names(scores) == "M.SCORE.y"] = "T2.M.SCORE"
    names(scores)[names(scores) == "M.NA.y"] = "T2.M.NA"
    
    scores$T1.M.SCORE[is.na(scores$T1.M.SCORE)] = -1
    scores$T2.M.SCORE[is.na(scores$T2.M.SCORE)] = -1
    scores$T1.M.NA[is.na(scores$T1.M.NA)] = 99
    scores$T2.M.NA[is.na(scores$T2.M.NA)] = 99
    
    # next group data by BBERG_ID and loop thru each BBERG_ID to determine M.OP
    companies = scores %>% split(f=scores$BBERG_ID)
    companies = lapply(companies, scoreToOp)
    opinions = do.call(rbind,companies)
    
    opinions = opinions[, !(colnames(opinions) %in% c("RF","index","index1","index2","T1.M.SCORE","T2.M.SCORE","T1.M.NA","T2.M.NA","M.OP1","M.OP2"))]
    return(opinions)
}    

# this function returns index file after joining scores
indexop <- function(index, ds, CH, CH_OpinionDate) {
    

    # joining index file to translation file
    ds2 = ds[, c("Ticker","As.of.Date","PK")]
    ds2 = merge(ds2, trans[ , c("Bond.Ticker","FROM.DATE.LINK","TO.DATE.LINK","ID_BB_UNIQUE","REPORTING_FREQUENCY")], 
                by.x="Ticker", by.y="Bond.Ticker", all.x=TRUE)
    ds2 = subset(ds2, ds2$As.of.Date>=ds2$FROM.DATE.LINK 
                 & ds2$As.of.Date<ds2$TO.DATE.LINK)
    ds2 = ds2[, !(colnames(ds2) %in% c("FROM.DATE.LINK","TO.DATE.LINK"))]
    if (CH) {
        # special handling for CH, looking back from the last "As.of.Date"
        ds2$FROM.DATE.LINK = ifelse(grepl("Q", ds2$REPORTING_FREQUENCY), 
                                    max(ds2$As.of.Date)+CS_MaxRange_Q,
                                    max(ds2$As.of.Date)+CS_MaxRange_SA)
    } else {
        ds2$FROM.DATE.LINK = ifelse(grepl("Q", ds2$REPORTING_FREQUENCY), 
                                    ds2$As.of.Date+CS_MaxRange_Q,
                                    ds2$As.of.Date+CS_MaxRange_SA)
    }
    # convert FROM.DATE.LINK back to date type
    ds2$FROM.DATE.LINK =as.Date(ds2$FROM.DATE.LINK, origin="1970-01-01")
    ds2$TO.DATE.LINK = monthStart(ds2$As.of.Date)
    if (CH) {
        # special handling for CH, the last "As.of.Date" to use user set opinion date
        ds2$TO.DATE.LINK[ds2$As.of.Date==max(ds2$As.of.Date)] = CH_OpinionDate
    }
    
    # get rid of columns no longer needed
    ds2 = ds2[, !(colnames(ds2) %in% c("Ticker","As.of.Date"))]
    
    ###### longest running merge #######
    # joining index file to company scores (unequal join using data.table object)
    scores$AdjDate2 = scores$AdjDate
    ds2=setDT(ds2)[setDT(scores), on=.(ID_BB_UNIQUE=BBERG_ID, FROM.DATE.LINK<=AdjDate2, TO.DATE.LINK>AdjDate2),nomatch=0,allow.cartesian=TRUE]
    # convert data.table back to data.frame    
    ds2=as.data.frame(ds2)
    ds2 = ds2[, !(colnames(ds2) %in% c("FROM.DATE.LINK","TO.DATE.LINK"))]
    # keep only the top 1 AdjDate for each PK
    ds2 = ds2[order(ds2$PK, ds2$AdjDate),]
    ds2 = ds2 %>% dplyr::mutate(rn = row_number()) %>% group_by(PK) %>% top_n(1, rn)
    # reformat Company ID back to its original format
    ds2$ID_BB_UNIQUE = paste("EQ", str_pad(ds2$ID_BB_UNIQUE, 16, pad = "0"), sep = "")
    
    # final join
    ds = merge(ds, ds2, by.x="PK", by.y="PK", all.x=TRUE)
    # destroy ds2
    rm(ds2)
    gc()
    
    # clean up M.OP and determine C.OP
    ds$M.OP = ifelse(is.na(ds$M.OP),-4,ds$M.OP)
    # special handling for CurrentHoldings (limited financials)
    # if the last opinion is -3, and 2nd to last opinion is 1, 0 or -2, copy the score data
    if (CH) {
        # ds3 = except for the last month
        ds3 = subset(ds, ds$As.of.Date < max(ds$As.of.Date))
        # ds1 = last month
        ds1 = subset(ds, ds$As.of.Date == max(ds$As.of.Date))
        # ds2 = 2nd to last month
        ds2 = subset(ds3[,c("Cusip","M.OP","M.SCORE","M.NA","T.SCORE","T.NA","L.SCORE","L.NA","M.FORMULA_11")], ds3$As.of.Date == max(ds3$As.of.Date))
        ds = merge(ds1, ds2, by.x="Cusip", by.y="Cusip", all.x=TRUE)
        ds$M.OP = ifelse(ds$M.OP.x==-3 & !is.na(ds$M.OP.y) & (ds$M.OP.y==1 | ds$M.OP.y==0 | ds$M.OP.y==-2),
                         ds$M.OP.y, ds$M.OP.x)
        ds$M.SCORE = ifelse(ds$M.OP.x==-3 & !is.na(ds$M.OP.y) & (ds$M.OP.y==1 | ds$M.OP.y==0 | ds$M.OP.y==-2),
                            ds$M.SCORE.y, ds$M.SCORE.x)
        ds$M.NA = ifelse(ds$M.OP.x==-3 & !is.na(ds$M.OP.y) & (ds$M.OP.y==1 | ds$M.OP.y==0 | ds$M.OP.y==-2 | ds$M.OP.y==-1),
                         ds$M.NA.y, ds$M.NA.x)
        ds$T.SCORE = ifelse(ds$M.OP.x==-3 & !is.na(ds$M.OP.y) & (ds$M.OP.y==1 | ds$M.OP.y==0 | ds$M.OP.y==-2 | ds$M.OP.y==-1),
                            ds$T.SCORE.y, ds$T.SCORE.x)
        ds$T.NA = ifelse(ds$M.OP.x==-3 & !is.na(ds$M.OP.y) & (ds$M.OP.y==1 | ds$M.OP.y==0 | ds$M.OP.y==-2 | ds$M.OP.y==-1),
                         ds$T.NA.y, ds$T.NA.x)
        ds$L.SCORE = ifelse(ds$M.OP.x==-3 & !is.na(ds$M.OP.y) & (ds$M.OP.y==1 | ds$M.OP.y==0 | ds$M.OP.y==-2 | ds$M.OP.y==-1),
                            ds$L.SCORE.y, ds$L.SCORE.x)
        ds$L.NA = ifelse(ds$M.OP.x==-3 & !is.na(ds$M.OP.y) & (ds$M.OP.y==1 | ds$M.OP.y==0 | ds$M.OP.y==-2 | ds$M.OP.y==-1),
                         ds$L.NA.y, ds$L.NA.x)
        ds$M.FORMULA_11 = ifelse(ds$M.OP.x==-3 & !is.na(ds$M.OP.y) & (ds$M.OP.y==1 | ds$M.OP.y==0 | ds$M.OP.y==-2 | ds$M.OP.y==-1),
                                 ds$M.FORMULA_11.y, ds$M.FORMULA_11.x)
        
        ds = ds[, !(colnames(ds) %in% c("M.OP.x","M.SCORE.x","M.NA.x","T.SCORE.x","T.NA.x","L.SCORE.x","L.NA.x","M.FORMULA_11.x",
                                        "M.OP.y","M.SCORE.y","M.NA.y","T.SCORE.y","T.NA.y","L.SCORE.y","L.NA.y","M.FORMULA_11.y"))]
        ds = rbind(ds, ds3)
    }
    ds$C.OP = ifelse(ds$M.OP==-4 | ds$M.OP==-3 | ds$M.OP==-1,ds$M.OP,
                     ifelse((ds$M.OP==-2 | ds$M.OP==-0) & ds$M.FORMULA_11>=HoldM_RatioCutOff,ds$M.OP,
                            ifelse(ds$M.OP==1 & ds$M.FORMULA_11>=BuyM_RatioCutOff,ds$M.OP,-1)))
    
    return(ds)
}

# this function returns threshold file
indexopt <- function(df) {
    

    df = df[order(df$Index,df$Date),]
    indexmonths = split(df, paste(df$Index, df$Date))
    
    MinWght = TH_MinWght
    
    DurationCutOff = 0
    DurationMax = 20
    ASWCutOff = 0
    ASWMax = 10000
    DurationStep = 0.05
    ASWStep = 10
    
    
    thresholds <- data.frame(matrix(ncol = 21, nrow = 0))
    colnames(thresholds) = c("Index", "Date", 
                             "BuyDurationCutOff", "BuyDurationMax", "BuyASWCutOff", "BuyASWMax",
                             "HoldDurationCutOff", "HoldDurationMax", "HoldASWCutOff", "HoldASWMax", 
                             "obsindex", "obsbuy", "obshold",
                             "IndexDuration", "IndexASW", "PortDuration", "PortASW",
                             "BuyDuration", "BuyASW", "HoldDuration", "HoldASW")
    
    PrevHoldDurationCutOff = DurationCutOff
    PrevBuyDurationCutOff = DurationCutOff
    PrevHoldDurationMax = DurationMax
    PrevBuyDurationMax = DurationMax
    PrevHoldASWCutOff = ASWCutOff
    PrevBuyASWCutOff = ASWCutOff
    PrevHoldASWMax = ASWMax
    PrevBuyASWMax = ASWMax
    
    
    for (i in 1:length(indexmonths)){
        im = indexmonths[[i]]
        HoldDurationCutOff = DurationCutOff
        BuyDurationCutOff = DurationCutOff
        HoldDurationMax = DurationMax
        BuyDurationMax = DurationMax
        HoldASWCutOff = ASWCutOff
        BuyASWCutOff = ASWCutOff
        HoldASWMax = ASWMax
        BuyASWMax = ASWMax
        
        
        # Hold
        continue = TRUE
        solution = FALSE
        while (continue) {
            im$OP = ifelse(im$C.OP==1 & im$ASW>=BuyASWCutOff & im$ASW<=BuyASWMax
                           & im$Duration>=BuyDurationCutOff & im$Duration<=BuyDurationMax,1,
                           ifelse((im$C.OP==0 | im$C.OP==-2) & im$ASW>=HoldASWCutOff & im$ASW<=HoldASWMax
                                  & im$Duration>=HoldDurationCutOff & im$Duration<=HoldDurationMax,0,-1))
            im$PortWght = ifelse(im$OP==1, im$IndexWght*3, ifelse(im$OP==0, im$IndexWght,0))
            curPortWght = sum(im$PortWght)
            avgHoldDuration = weighted.mean(im$Duration[im$OP==0], im$IndexWght[im$OP==0])
            avgHoldASW = weighted.mean(im$ASW[im$OP==0], im$IndexWght[im$OP==0])
            if (is.nan(avgHoldDuration) | is.nan(avgHoldASW)) {
                continue = FALSE            
            }
            else if (curPortWght < MinWght) {
                continue = FALSE
            }
            else if (avgHoldDuration < im$HoldDurationMin[1]) {
                HoldDurationCutOff = HoldDurationCutOff + DurationStep
            } else if (avgHoldDuration > im$HoldDurationMax[1]) {
                HoldDurationMax = HoldDurationMax - DurationStep
            } else if (avgHoldASW < im$HoldASWMin[1]) {
                HoldASWCutOff = HoldASWCutOff + ASWStep
            } else if (avgHoldASW > im$HoldASWMax[1]) {
                HoldASWMax = HoldASWMax - ASWStep
            } else {
                solution = TRUE
                continue = FALSE
            }
            
        }
        
        # Buy
        continue = TRUE
        solution = FALSE
        while (continue) {
            im$OP = ifelse(im$C.OP==1 & im$ASW>=BuyASWCutOff & im$ASW<=BuyASWMax
                           & im$Duration>=BuyDurationCutOff & im$Duration<=BuyDurationMax,1,
                           ifelse((im$C.OP==0 | im$C.OP==-2) & im$ASW>=HoldASWCutOff & im$ASW<=HoldASWMax
                                  & im$Duration>=HoldDurationCutOff & im$Duration<=HoldDurationMax,0,-1))
            im$PortWght = ifelse(im$OP==1, im$IndexWght*3, ifelse(im$OP==0, im$IndexWght,0))
            curPortWght = sum(im$PortWght)
            avgBuyDuration = weighted.mean(im$Duration[im$OP==1], im$IndexWght[im$OP==1])
            avgBuyASW = weighted.mean(im$ASW[im$OP==1], im$IndexWght[im$OP==1])
            if (is.nan(avgBuyDuration) | is.nan(avgBuyASW)) {
                continue = FALSE
            }
            else if (curPortWght < MinWght) {
                continue = FALSE
            }
            else if (avgBuyDuration < im$BuyDurationMin[1]) {
                BuyDurationCutOff = BuyDurationCutOff + DurationStep
            } else if (avgBuyDuration > im$BuyDurationMax[1]) {
                BuyDurationMax = BuyDurationMax - DurationStep
            } else if (avgBuyASW < im$BuyASWMin[1]) {
                BuyASWCutOff = BuyASWCutOff + ASWStep
            } else if (avgBuyASW > im$BuyASWMax[1]) {
                BuyASWMax = BuyASWMax - ASWStep
            } else {
                solution = TRUE
                continue = FALSE
            }
            
        }
        
        
        if (solution) {
            # if solution found, save as prev month
            PrevHoldDurationCutOff = HoldDurationCutOff
            PrevBuyDurationCutOff = BuyDurationCutOff
            PrevHoldDurationMax = HoldDurationMax
            PrevBuyDurationMax = BuyDurationMax
            PrevHoldASWCutOff = HoldASWCutOff
            PrevBuyASWCutOff = BuyASWCutOff
            PrevHoldASWMax = HoldASWMax
            PrevBuyASWMax = BuyASWMax
        } else {
            # if solution not found, use prev month
            HoldDurationCutOff = PrevHoldDurationCutOff
            BuyDurationCutOff = PrevBuyDurationCutOff
            HoldDurationMax = PrevHoldDurationMax
            BuyDurationMax = PrevBuyDurationMax
            HoldASWCutOff = PrevHoldASWCutOff
            BuyASWCutOff = PrevBuyASWCutOff
            HoldASWMax = PrevHoldASWMax
            BuyASWMax = PrevBuyASWMax
            im$OP = ifelse(im$C.OP==1 & im$ASW>=BuyASWCutOff & im$ASW<=BuyASWMax
                           & im$Duration>=BuyDurationCutOff & im$Duration<=BuyDurationMax,1,
                           ifelse((im$C.OP==0 | im$C.OP==-2) & im$ASW>=HoldASWCutOff & im$ASW<=HoldASWMax
                                  & im$Duration>=HoldDurationCutOff & im$Duration<=HoldDurationMax,0,-1))
        }
        
        obsindex = nrow(im)
        obsbuy = nrow(im[im$OP==1,])
        obshold = nrow(im[im$OP==0,])
        im$PortWght = ifelse(im$OP==1, im$IndexWght*3, ifelse(im$OP==0, im$IndexWght,0))
        IndexDuration = weighted.mean(im$Duration, im$IndexWght)
        PortDuration = weighted.mean(im$Duration[im$PortWght>0], im$PortWght[im$PortWght>0])
        BuyDuration = weighted.mean(im$Duration[im$OP==1], im$IndexWght[im$OP==1]) 
        HoldDuration = weighted.mean(im$Duration[im$OP==0 | im$OP==-2], im$IndexWght[im$OP==0 | im$OP==-2]) 
        IndexASW = weighted.mean(im$ASW, im$IndexWght)
        PortASW = weighted.mean(im$ASW[im$PortWght>0], im$PortWght[im$PortWght>0])
        BuyASW = weighted.mean(im$ASW[im$OP==1], im$IndexWght[im$OP==1]) 
        HoldASW = weighted.mean(im$ASW[im$OP==0 | im$OP==-2], im$IndexWght[im$OP==0 | im$OP==-2]) 
        
        thresholds<-rbind(thresholds, data.frame(Index=im$Index[1],Date=im$Date[1],
                                                 BuyDurationCutOff=BuyDurationCutOff, BuyDurationMax=BuyDurationMax,
                                                 BuyASWCutOff=BuyASWCutOff, BuyASWMax=BuyASWMax,
                                                 HoldDurationCutOff=HoldDurationCutOff, HoldDurationMax=HoldDurationMax,
                                                 HoldASWCutOff=HoldASWCutOff, HoldASWMax=HoldASWMax,
                                                 obsindex=obsindex,obsbuy=obsbuy,obshold=obshold,
                                                 IndexDuration=IndexDuration,PortDuration=PortDuration,IndexASW=IndexASW,PortASW=PortASW,
                                                 BuyDuration=BuyDuration,HoldDuration=HoldDuration,BuyASW=BuyASW,HoldASW=HoldASW))
        
    }
    return (thresholds)
}

# this function calc thresholds with optimization logic
indexth <- function(index, ds) {
    

    ds$Date = as.character(ds$As.of.Date)
    names(ds)[names(ds) == "Index.Name"] = "Index"
    names(ds)[names(ds) == "PrevMend.Mkt...Index.Wght"] = "IndexWght"
    names(ds)[names(ds) == "PrevMend.Mod.Dur.To.Worst"] = "Duration"
    names(ds)[names(ds) == "PrevMend.AssetSwp"] = "ASW"
    ds = ds[, c("Index", "Date", "IndexWght", "Duration", "ASW", "C.OP")]
    ### try use moving average here ###
    #print(TH_MA)
    if (TH_MA==1) {
        ds$AvgDuration = ave(ds$IndexWght/100*ds$Duration, ds$Date, FUN=sum)
        ds$AvgASW = ave(ds$IndexWght/100*ds$ASW, ds$Date, FUN=sum)
    } else {
        ds_ma = group_by(ds,Index,Date) %>% 
            summarize(AvgASWTemp = sum(IndexWght/100*ASW), AvgDurationTemp = sum(IndexWght/100*Duration))
        ds_ma$AvgASW=runMean(ds_ma$AvgASWTemp, TH_MA)
        ds_ma$AvgDuration=runMean(ds_ma$AvgDurationTemp, TH_MA)
        for (i in (TH_MA-1):2) {
            ds_ma$AvgASW[is.na(ds_ma$AvgASW)]=runMean(subset(ds_ma, is.na(ds_ma$AvgASW))$AvgASWTemp, i)
            ds_ma$AvgDuration[is.na(ds_ma$AvgDuration)]=runMean(subset(ds_ma, is.na(ds_ma$AvgDuration))$AvgDurationTemp, i)
        }
        ds_ma$AvgASW[is.na(ds_ma$AvgASW)]=subset(ds_ma, is.na(ds_ma$AvgASW))$AvgASWTemp
        ds_ma$AvgDuration[is.na(ds_ma$AvgDuration)]=subset(ds_ma, is.na(ds_ma$AvgDuration))$AvgDurationTemp
        ds_ma = ds_ma[, c("Index", "Date", "AvgASW", "AvgDuration")]
        ds = merge(ds, ds_ma, 
                   by.x=c("Index", "Date"), by.y=c("Index", "Date"), all.x=TRUE)
    }
    
    
    ds$BuyDurationMin = ds$AvgDuration * TH.Limits[TH.Limits$Indices==index, "BuyDurationLow"]
    ds$BuyDurationMax = ds$AvgDuration * TH.Limits[TH.Limits$Indices==index, "BuyDurationHigh"]
    ds$HoldDurationMin = ds$AvgDuration * TH.Limits[TH.Limits$Indices==index, "HoldDurationLow"]
    ds$HoldDurationMax = ds$AvgDuration * TH.Limits[TH.Limits$Indices==index, "HoldDurationHigh"]
    ds$BuyASWMin = ds$AvgASW * TH.Limits[TH.Limits$Indices==index, "BuyASWLow"]
    ds$BuyASWMax = ds$AvgASW * TH.Limits[TH.Limits$Indices==index, "BuyASWHigh"]
    ds$HoldASWMin = ds$AvgASW * TH.Limits[TH.Limits$Indices==index, "HoldASWLow"]
    ds$HoldASWMax = ds$AvgASW * TH.Limits[TH.Limits$Indices==index, "HoldASWHigh"]
    
    return(indexopt(ds))
}

# this calc FinalMktWeight for DtS
dts <- function(df) {
    

    df = df[order(-df$Qualify, -df$DtS),]
    
    Multiple = OutPerformMultiple-1
    PercentileHold = ConstructModelPortDtSPHold
    PercentileBuy = ConstructModelPortDtSPBuy
    df$DtSPHold = quantile(df$DtS, PercentileHold)
    df$DtSPBuy = quantile(df$DtS, PercentileBuy)
    df$DtSPHoldDiff = abs(df$DtS - df$DtSPHold)
    df$DtSPBuyDiff = abs(df$DtS - df$DtSPBuy)
    row = df[1,]
    Opinion = row$M.OP
    NumOfBonds = row$NumOfBonds
    NumQualify = row$NumQualify
    TotalTicker = row$TotalTicker
    
    if (Opinion ==1)
    {
        # find the one that's qualified and closest to PHold and use it
        df = df[order(-df$Qualify, df$DtSPHoldDiff),]
        row = df[1,]
        EfficientDtS = row$EfficientDtS
        row$FinalMktWeight = ifelse(EfficientDtS==0,0,TotalTicker/EfficientDtS)
        
        # find the one that's qualified and closest to PBuy and use it
        df = df[order(-df$Qualify, df$DtSPBuyDiff),]
        row = df[1,]
        EfficientDtS = row$EfficientDtS
        row$FinalMktWeight = ifelse(EfficientDtS==0,0,row$FinalMktWeight + Multiple*TotalTicker/EfficientDtS)
    } else
    {
        # find the one that's qualified and closest to PHold and use it
        df = df[order(-df$Qualify, df$DtSPHoldDiff),]
        row = df[1,]
        EfficientDtS = row$EfficientDtS
        row$FinalMktWeight = ifelse(EfficientDtS==0,0,TotalTicker/EfficientDtS)
    }
    
    
    return(df)
    
}

# this function limits weight by index month
limitWeight <- function(df) {
    

    # 1) group by Ticker, calculate sum of ReWeighted2
    # 2) if any ticker with sum of ReWeighted2 > MaxWeightPerName, set it to MaxLimit, 
    #   distribute by CUSIP within that ticker, otherwise DONE
    # 3) re-weight the whole population to 100
    # 4) repeat from 1)
    continue = TRUE
    count = 0
    while (continue) {
        
        df$TickerWeight = ave(df$ReWeighted2, df$Ticker, FUN=sum)
        if (nrow(df[which(df$TickerWeight>MaxWeightPerName),])>0) {
            df$ReWeighted2[df$TickerWeight>MaxWeightPerName] = 
                df$ReWeighted2[df$TickerWeight>MaxWeightPerName] / df$TickerWeight[df$TickerWeight>MaxWeightPerName] * MaxLimit
            df$ReWeighted2 = df$ReWeighted2 * 100 / sum(df$ReWeighted2)
        } else {
            continue = FALSE
        }
        count = count + 1
        if (count>10) continue = FALSE
    }
    
    return (df)
}

# this function writes metrics to a file for a given index and period
writeMetrics <- function(fn, final_index_month, index, period_name, period_start, period_end, gs_params) {
    

    #print(paste(index, period_name, period_start, period_end))
    if (!grepl("ALL", period_name)) {
        final_index_month = subset(final_index_month, final_index_month$Index.Name == index &
                                   final_index_month$As.of.Date>=period_start & final_index_month$As.of.Date<period_end)
    } else {
        final_index_month = subset(final_index_month, final_index_month$Index.Name == index)
    }
    if (nrow(final_index_month) > 0) {
        final_index = group_by(final_index_month, Index.Name) %>% 
            summarize(months = n(),
                      # index
                      idx_trr_prod = prod(idx_trr_tmp),
                      idx_exr_prod = prod(idx_exr_tmp),
                      idx_err_prod = prod(idx_err_tmp),
                      idx_dur = mean(idx_dur),
                      idx_asw = mean(idx_asw),
                      idx_dxs = mean(idx_dxs),
                      idx_stdev = sd(idx_trr)*sqrt(12),
                      idx_dndev = sqrt(sum(idx_trrdn*idx_trrdn)*12/n()),
                      #p1
                      p1_dur = mean(p1_dur),
                      p1_asw = mean(p1_asw),
                      p1_dxs = mean(p1_dxs),
                      # transaction cost
                      p1_trr_prod = prod(p1_trr_tmp),
                      p1_exr_prod = prod(p1_exr_tmp),
                      p1_err_prod = prod(p1_err_tmp),
                      p1_stdev = sd(p1_trr)*sqrt(12),
                      p1_dndev = sqrt(sum(p1_trrdn*p1_trrdn)*12/n()),
                      p1_buy = sum(p1_buy),
                      p1_sell = sum(p1_sell),
                      
                      # no transaction cost
                      p1nx_trr_prod = prod(p1nx_trr_tmp),
                      p1nx_exr_prod = prod(p1nx_exr_tmp),
                      p1nx_err_prod = prod(p1nx_err_tmp),
                      p1nx_stdev = sd(p1nx_trr)*sqrt(12),
                      p1nx_dndev = sqrt(sum(p1nx_trrdn*p1nx_trrdn)*12/n())
            )
        # index
        final_index$idx_trr = (`^`(final_index$idx_trr_prod,12/final_index$months)-1)*100
        final_index$idx_exr = (`^`(final_index$idx_exr_prod,12/final_index$months)-1)*100
        final_index$idx_err = (`^`(final_index$idx_err_prod,12/final_index$months)-1)*100
        final_index$idx_sharpe = final_index$idx_err/final_index$idx_stdev
        final_index$idx_sortino = final_index$idx_trr/final_index$idx_dndev
        #p1
        final_index$p1_trr = (`^`(final_index$p1_trr_prod,12/final_index$months)-1)*100
        final_index$p1_exr = (`^`(final_index$p1_exr_prod,12/final_index$months)-1)*100
        final_index$p1_err = (`^`(final_index$p1_err_prod,12/final_index$months)-1)*100
        final_index$p1_sharpe = final_index$p1_err/final_index$p1_stdev
        final_index$p1_sortino = final_index$p1_trr/final_index$p1_dndev
        
        # p1nx
        final_index$p1nx_trr = (`^`(final_index$p1nx_trr_prod,12/final_index$months)-1)*100
        final_index$p1nx_exr = (`^`(final_index$p1nx_exr_prod,12/final_index$months)-1)*100
        final_index$p1nx_err = (`^`(final_index$p1nx_err_prod,12/final_index$months)-1)*100
        final_index$p1nx_sharpe = final_index$p1nx_err/final_index$p1nx_stdev
        final_index$p1nx_sortino = final_index$p1nx_trr/final_index$p1nx_dndev
        
        final_index$period = period_name
        final_index$var_dur = abs(final_index$p1_dur - final_index$idx_dur)/final_index$idx_dur
        final_index$var_asw = abs(final_index$p1_asw - final_index$idx_asw)/final_index$idx_asw
        final_index$var_trr = final_index$p1_trr - final_index$idx_trr
        final_index$var_exr = final_index$p1_exr - final_index$idx_exr
        final_index$var_err = final_index$p1_err - final_index$idx_err
        final_index$var_sharpe = final_index$p1_sharpe - final_index$idx_sharpe
        final_index$var_sortino = final_index$p1_sortino - final_index$idx_sortino
        
        final_index$p1_TO = with(final_index, pmin(p1_buy, p1_sell))
        final_index$p1_TO = with(final_index, p1_TO*12/months)
        # remove unused columns
        final_index = final_index[, !(colnames(final_index) %in% 
                                          c("idx_trr_prod", "idx_exr_prod", "idx_err_prod", "idx_stdev", "idx_dndev", 
                                            "p1_trr_prod", "p1_exr_prod", "p1_err_prod", "p1_stdev", "p1_dndev", "p1_buy", "p1_sell",
                                            "p1nx_trr_prod", "p1nx_exr_prod", "p1nx_err_prod", "p1nx_stdev", "p1nx_dndev"))]    
        
        if (!is.null(nrow(gs_params))) final_index = cbind(final_index, gs_params)
        
        if (!file.exists(fn))
            write.table(final_index, file = fn, sep = ",", col.names = TRUE, row.names = FALSE)
        else
            write.table(final_index, file = fn, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
    }
    
}

# this function compute portfolio weight
computePortWght <- function(ds, th, ConstructModelPortMethod) {
    # Merge with threshold and determine F.OP
    ds = merge(ds, th[,c("Index", "Date", "BuyASWCutOff", "BuyASWMax", "BuyDurationCutOff", "BuyDurationMax", "HoldASWCutOff", "HoldASWMax", "HoldDurationCutOff", "HoldDurationMax")], 
               by.x=c("Index.Name", "As.of.Date"), by.y=c("Index", "Date"), all.x=TRUE)
    ds$F.OP = ifelse(ds$M.OP==-4 | ds$M.OP==-3 | ds$M.OP==-1,ds$M.OP,
                     ifelse((ds$M.OP==-2 | ds$M.OP==-0) & ds$M.FORMULA_11>=HoldM_RatioCutOff 
                            & ds$PrevMend.AssetSwp >= ds$HoldASWCutOff & ds$PrevMend.AssetSwp <= ds$HoldASWMax
                            & ds$PrevMend.Mod.Dur.To.Worst >= ds$HoldDurationCutOff & ds$PrevMend.Mod.Dur.To.Worst <= ds$HoldDurationMax
                            & (TScoreHoldFilter == 0 | ds$T.SCORE >= TScoreCutOff | ds$T.NA > TScoreNA),
                            ds$M.OP,
                            ifelse(ds$M.OP==1 & ds$M.FORMULA_11>=BuyM_RatioCutOff
                                   & ds$PrevMend.AssetSwp >= ds$BuyASWCutOff & ds$PrevMend.AssetSwp <= ds$BuyASWMax
                                   & ds$PrevMend.Mod.Dur.To.Worst >= ds$BuyDurationCutOff & ds$PrevMend.Mod.Dur.To.Worst <= ds$BuyDurationMax
                                   & (TScoreBuyFilter == 0 | ds$T.SCORE >= TScoreCutOff | ds$T.NA > TScoreNA),
                                   ds$M.OP,ds$M.OP-100)))
    
    # remove columns no longer required to reduce memory usage
    ds = ds[, !(colnames(ds) %in% c("rn","BuyASWCutOff", "BuyASWMax", "BuyDurationCutOff", "BuyDurationMax", 
                                    "HoldASWCutOff", "HoldASWMax", "HoldDurationCutOff", "HoldDurationMax"))]
    
    # add additional columns
    ds$Investment.Date = as.mondate(ds$As.of.Date)-1
    ds$IndexDate = paste(ds$Index.Name, ds$As.of.Date)
    ds$IndexTickerDate = paste(ds$Index.Name, ds$Ticker, ds$As.of.Date)
    ds$ModifiedMktWeight = ifelse(ds$F.OP==1, ds$PrevMend.Mkt...Index.Wght*OutPerformMultiple,
                                  ifelse(ds$F.OP==0 | ds$F.OP==-2,ds$PrevMend.Mkt...Index.Wght,0))
    ds$FinalMktWeight = 0
    ds$DtS = ds$PrevMend.Mod.Dur.To.Worst*ds$PrevMend.AssetSwp
    ds$WghtDtS = ds$PrevMend.Mkt...Index.Wght*ds$DtS
    ds$Qualify = ifelse(((ds$F.OP==1|ds$F.OP==0|ds$F.OP==-2) & ds$DtS>0),1,0)
    ds$QualifyDtS = ifelse(ds$Qualify==1,ds$DtS,0)
    ds$temp = 1
    ds$NumOfBonds = ave(ds$temp, ds$IndexTickerDate, FUN=sum)
    ds$TotalTicker = ave(ds$PrevMend.Mkt...Index.Wght, ds$IndexTickerDate, FUN=sum)
    ds$TotalWghtDtS = ave(ds$WghtDtS, ds$IndexTickerDate, FUN=sum)
    ds$NumQualify = ave(ds$Qualify, ds$IndexTickerDate, FUN=sum)
    ds$MaxDtS = ave(ds$QualifyDtS, ds$IndexTickerDate, FUN=max)
    ds$Qualify[ds$TotalWghtDtS<=0] = 0
    ds$AvgWghtDtSTicker = ifelse(ds$TotalTicker>0,ds$TotalWghtDtS/ds$TotalTicker,0)
    ds$EfficientDtS = ifelse(ds$AvgWghtDtSTicker>0,ds$DtS/ds$AvgWghtDtSTicker,0)
    ds[c("DtSPHold", "DtSPBuy", "DtSPHoldDiff", "DtSPBuyDiff")] <- NA
    
    if (ConstructModelPortMethod == 1) {
        # MV%
        ds$FinalMktWeight = ds$ModifiedMktWeight
    } else if (ConstructModelPortMethod >= 2) {
        # DtS
        # handling simple cases here for efficiency
        ds$FinalMktWeight = ifelse((ds$NumOfBonds<=3 & ds$DtS==ds$MaxDtS & ds$Qualify==1) | (ds$NumQualify==1 & ds$Qualify==1),
                                   ifelse(ds$M.OP==1,
                                          ifelse(ds$EfficientDtS==0,0,OutPerformMultiple*ds$TotalTicker/ds$EfficientDtS),
                                          ifelse(ds$M.OP==0 | ds$M.OP==-2,ifelse(ds$EfficientDtS==0,0,ds$TotalTicker/ds$EfficientDtS),
                                                 ds$FinalMktWeight)),
                                   ds$FinalMktWeight)
        
        # create an empty dts dataframe with same structure of ds
        ds$DtSPHold = NA
        ds$DtSPBuy = NA
        ds$DtSPHoldDiff = NA
        ds$DtSPBuyDiff = NA
        dts_output1=subset(ds, ds$NumQualify>1 & ds$NumOfBonds>3 & ds$TotalTicker>0)
        dts_output2=subset(ds, ds$NumQualify<=1 | ds$NumOfBonds<=3 | ds$TotalTicker<=0)
        # destory ds
        rm(ds)
        gc()
        dts_list = dts_output1 %>% split(f=dts_output1$IndexTickerDate)
        dts_list = lapply(dts_list, dts)
        dts_output1 = do.call(rbind,dts_list)
        ds = rbind(dts_output2, dts_output1)
        # destory dts_output1 & 2
        rm(dts_output1)
        rm(dts_output2)
        rm(dts_list)
        gc()
        
        if (ConstructModelPortMethod == 3) {
            # DtS Cash
            # group by IndexTickerDate
            # TickerModelWeight = sum(FinalMktWeight)
            # TickerIndexWeight = sum(PrevMend.Mkt...Index.Wght)
            # TickerOpinion = max(F.OP)
            ds_cash = group_by(ds,Index.Name, Ticker, As.of.Date) %>% 
                summarize(TickerModelWeight = sum(FinalMktWeight),
                          TickerIndexWeight = sum(PrevMend.Mkt...Index.Wght),
                          TickerOpinion = max(F.OP))
            ds_cash$TickerCashWeight = ifelse(ds_cash$TickerOpinion==1,
                                              ds_cash$TickerIndexWeight*OutPerformMultiple-ds_cash$TickerModelWeight,
                                              ifelse(ds_cash$TickerOpinion==0 | ds_cash$TickerOpinion==-2,
                                                     ds_cash$TickerIndexWeight-ds_cash$TickerModelWeight,0))
            ds_cash = group_by(ds_cash,Index.Name,As.of.Date) %>% 
                summarize(FinalMktWeight = sum(TickerCashWeight))
            ds_cash$FinalMktWeight[ds_cash$FinalMktWeight<0]=0
            
            morecols = colnames(ds)[!(colnames(ds) %in% c("Index.Name","As.of.Date","FinalMktWeight"))]
            ds_cash[morecols] <- NA
            ds_cash$PK=paste(ds_cash$Index.Name, "CASH", ds_cash$As.of.Date, sep="")
            ds_cash$Investment.Date=as.mondate(ds_cash$As.of.Date)-1
            ds_cash$Ticker="CASH"
            ds_cash$Cusip="CASH"
            ds_cash$ISIN.number="CASH"
            ds_cash$IndexDate = paste(ds_cash$Index.Name, ds_cash$As.of.Date)
            ds_cash$IndexTickerDate = paste(ds_cash$Index.Name, ds_cash$Ticker, ds_cash$As.of.Date)
            ds_cash$PrevMend.Mkt...Index.Wght=0
            ds_cash$PrevMend.Mod.Dur.To.Worst=0
            ds_cash$PrevMend.AssetSwp=0
            ds_cash$PrevMend.Price=100
            ds_cash$TRR...MTD.LOC=0
            ds_cash$Excess.Rtn...MTD=0
            ds_cash$Semi.Mod.Dur.To.Worst=0
            ds_cash$Asset.Swap=0
            ds_cash$M.SCORE=10
            ds_cash$M.NA=0
            ds_cash$T.SCORE=10
            ds_cash$T.NA=0
            ds_cash$L.SCORE=10
            ds_cash$L.NA=0
            ds_cash$M.FORMULA_11=1
            ds_cash$M.OP=-100
            ds_cash$C.OP=-100
            ds_cash$F.OP=-100
            ds_cash$ModifiedMktWeight=0
            ds_cash$DtS=0
            ds_cash$WghtDtS=0
            ds_cash$Qualify=0
            ds_cash$QualifyDtS=0
            ds_cash$temp=1
            ds_cash$NumOfBonds=0
            ds_cash$TotalTicker=0
            ds_cash$TotalWghtDtS=0
            ds_cash$NumQualify=0
            ds_cash$MaxDtS=0
            ds_cash$AvgWghtDtSTicker=0
            ds_cash$EfficientDtS=0
            # convert to data.frame before rbind
            ds_cash = data.frame(ds_cash)
            ds=rbind(ds, ds_cash)
        }
        
    } 
    
    # remove columns no longer required to reduce memory usage
    ds = ds[, !(colnames(ds) %in% c("QualifyDtS", "temp", "MaxDtS"))]
    
    return(ds)
}

# this function compute metrics from final dataset
computeMetrics <- function(final, RootFolder) {
    G0O1 = indexlvldata(RootFolder, IndexLvlDataFolder)
    G0O1 = subset(G0O1, G0O1$Index=="G0O1")
    
    
    final$IndexCusip = paste(final$Index.Name, final$Cusip, sep = "")
    final$F.OP.CAT = ifelse(final$F.OP==-2|final$F.OP==0, "neutral", 
                            ifelse(final$F.OP==1, "OutPerform", "UnderPerform"))
    final$DxS = final$PrevMend.Mod.Dur.To.Worst*final$PrevMend.AssetSwp
    
    
    # p1
    final$p1 = ifelse(final$F.OP==-2|final$F.OP==0|final$F.OP==1, 1,0)
    final$p1_trr = ifelse(final$F.OP==-2|final$F.OP==0|final$F.OP==1, final$TRR...MTD.LOC,0)
    final$p1_exr = ifelse(final$F.OP==-2|final$F.OP==0|final$F.OP==1, final$Excess.Rtn...MTD,0)
    final$p1_wght = final$ReWeighted2
    final$p1_dur = ifelse(final$F.OP==-2|final$F.OP==0|final$F.OP==1, final$PrevMend.Mod.Dur.To.Worst,0)
    final$p1_asw = ifelse(final$F.OP==-2|final$F.OP==0|final$F.OP==1, final$PrevMend.AssetSwp,0)
    final$p1_dxs = final$p1_dur*final$p1_asw
    
    
    final$Index1 = final$Index + 1
    final = merge(final, final[ , c("Index1","IndexCusip", "F.OP.CAT", "p1_wght")],
                  by.x="Index", by.y="Index1", all.x=TRUE)
    
    names(final)[names(final) == "IndexCusip.x"] = "IndexCusip"
    names(final)[names(final) == "F.OP.CAT.x"] = "F.OP.CAT"
    names(final)[names(final) == "p1_wght.x"] = "p1_wght"
    final$IndexCusip.y[1]="NONE"
    final$F.OP.CAT.y[1]="NONE"
    final$NewCusip = ifelse(final$IndexCusip==final$IndexCusip.y,0,1)
    final$F.OP.CAT.y = ifelse(final$NewCusip==1, "NONE", final$F.OP.CAT.y)
    final$p1.Op.Chg = ifelse(final$NewCusip==0,ifelse(final$F.OP.CAT!=final$F.OP.CAT.y,1,0),0)
    final$p1_TX = ifelse(final$p1.Op.Chg==1,abs(final$p1_wght-final$p1_wght.y),0) * TransactionCost / 100   #assume price = 100
    final$p1_TX2 = ifelse(final$p1.Op.Chg==1,final$p1_wght-final$p1_wght.y,0) * TransactionCost / 100       #assume price = 100
    final$p1_buy = (final$p1_TX + final$p1_TX2)/2*100/TransactionCost
    final$p1_sell = (final$p1_TX - final$p1_TX2)/2*100/TransactionCost
    
    final_index_month = group_by(final, Index.Name, As.of.Date) %>% 
        summarize(
            # index
            idx_obs = n(),
            idx_trr = sum(TRR...MTD.LOC*Orig.PrevMend.Mkt...Index.Wght),
            idx_exr = sum(Excess.Rtn...MTD*Orig.PrevMend.Mkt...Index.Wght),
            idx_dur = sum(PrevMend.Mod.Dur.To.Worst*Orig.PrevMend.Mkt...Index.Wght),
            idx_asw = sum(PrevMend.AssetSwp*Orig.PrevMend.Mkt...Index.Wght),
            idx_dxs = sum(DxS*Orig.PrevMend.Mkt...Index.Wght),
            idx_wght = sum(Orig.PrevMend.Mkt...Index.Wght),
            # p1
            p1_obs = sum(p1),
            p1nx_trr = sum(p1_trr*p1_wght),
            p1nx_exr = sum(p1_exr*p1_wght),
            p1_dur = sum(p1_dur*p1_wght),
            p1_asw = sum(p1_asw*p1_wght),
            p1_dxs = sum(p1_dxs*p1_wght),
            p1_TX = sum(p1_TX),
            p1_TX2 = abs(sum(p1_TX2)),
            p1_buy = sum(p1_buy),
            p1_sell = sum(p1_sell),
            # this one last
            p1_wght = sum(p1_wght)
        )
    # turn over
    # link risk free rate
    final_index_month$As.of.Date = as.Date(final_index_month$As.of.Date)
    final_index_month = merge(final_index_month, G0O1[ , c("Date", "TRR...1.month.LOC")], 
                              by.x="As.of.Date", by.y="Date", all.x=TRUE)
    
    # index metrics
    final_index_month$idx_trr = ifelse(final_index_month$idx_wght==0,0,final_index_month$idx_trr/final_index_month$idx_wght)
    final_index_month$idx_trr_tmp = 1+final_index_month$idx_trr/100
    final_index_month$idx_exr = ifelse(final_index_month$idx_wght==0,0,final_index_month$idx_exr/final_index_month$idx_wght)
    final_index_month$idx_exr_tmp = 1+final_index_month$idx_exr/100
    final_index_month$idx_dur = ifelse(final_index_month$idx_wght==0,0,final_index_month$idx_dur/final_index_month$idx_wght)
    final_index_month$idx_asw = ifelse(final_index_month$idx_wght==0,0,final_index_month$idx_asw/final_index_month$idx_wght)
    final_index_month$idx_dxs = ifelse(final_index_month$idx_wght==0,0,final_index_month$idx_dxs/final_index_month$idx_wght)
    final_index_month$idx_trrdn = ifelse(final_index_month$idx_trr>0,0,final_index_month$idx_trr)
    final_index_month$idx_err = final_index_month$idx_trr - final_index_month$TRR...1.month.LOC
    final_index_month$idx_err_tmp = 1+final_index_month$idx_err/100
    
    # port1 metrics
    final_index_month$p1_dur = ifelse(final_index_month$p1_wght==0,0,final_index_month$p1_dur/final_index_month$p1_wght)
    final_index_month$p1_asw = ifelse(final_index_month$p1_wght==0,0,final_index_month$p1_asw/final_index_month$p1_wght)
    final_index_month$p1_dxs = ifelse(final_index_month$p1_wght==0,0,final_index_month$p1_dxs/final_index_month$p1_wght)
    # transaction cost
    final_index_month$p1_trr = ifelse(final_index_month$p1_wght==0,0,
                                      final_index_month$p1nx_trr/final_index_month$p1_wght-final_index_month$p1_TX-final_index_month$p1_TX2)
    final_index_month$p1_trr_tmp = 1+final_index_month$p1_trr/100
    final_index_month$p1_exr = ifelse(final_index_month$p1_wght==0,0,
                                      final_index_month$p1nx_exr/final_index_month$p1_wght-final_index_month$p1_TX-final_index_month$p1_TX2)
    final_index_month$p1_exr_tmp = 1+final_index_month$p1_exr/100
    final_index_month$p1_trrdn = ifelse(final_index_month$p1_trr>0,0,final_index_month$p1_trr)
    final_index_month$p1_err = final_index_month$p1_trr - final_index_month$TRR...1.month.LOC
    final_index_month$p1_err_tmp = 1+final_index_month$p1_err/100
    # no transaction cost
    final_index_month$p1nx_trr = ifelse(final_index_month$p1_wght==0,0,final_index_month$p1nx_trr/final_index_month$p1_wght)
    final_index_month$p1nx_trr_tmp = 1+final_index_month$p1nx_trr/100
    final_index_month$p1nx_exr = ifelse(final_index_month$p1_wght==0,0,final_index_month$p1nx_exr/final_index_month$p1_wght)
    final_index_month$p1nx_exr_tmp = 1+final_index_month$p1nx_exr/100
    final_index_month$p1nx_trrdn = ifelse(final_index_month$p1nx_trr>0,0,final_index_month$p1nx_trr)
    final_index_month$p1nx_err = final_index_month$p1nx_trr - final_index_month$TRR...1.month.LOC
    final_index_month$p1nx_err_tmp = 1+final_index_month$p1nx_err/100
    
    return(final_index_month)
}

# this function does a grid search for optimal
gridSearch <- function(RF, CMPM, session) {

    RootFolder = RF
    start_time = Sys.time()
    fn = paste(RootFolder, "gridsearch.csv", sep ="")
    if (file.exists(fn)) file.remove(fn)
    
    init_var()
    trans <<- loadtrans(RootFolder)

    variables = read.csv("variables.csv")    
    
    CS_Opinion_NA.Enabled = variables[variables$Variables=="CS_Opinion_NA",]$Enabled
    if (CS_Opinion_NA.Enabled) {
        CS_Opinion_NA.Min = variables[variables$Variables=="CS_Opinion_NA",]$Min
        CS_Opinion_NA.Max = variables[variables$Variables=="CS_Opinion_NA",]$Max
        CS_Opinion_NA.Step = variables[variables$Variables=="CS_Opinion_NA",]$Step
    } else {
        CS_Opinion_NA.Min = CS_Opinion_NA
        CS_Opinion_NA.Max = CS_Opinion_NA
        CS_Opinion_NA.Step = 10000
    }

    CS_TableOption.Enabled = variables[variables$Variables=="CS_TableOption",]$Enabled
    if (CS_TableOption.Enabled) {
        CS_TableOption.Min = variables[variables$Variables=="CS_TableOption",]$Min
        CS_TableOption.Max = variables[variables$Variables=="CS_TableOption",]$Max
        CS_TableOption.Step = variables[variables$Variables=="CS_TableOption",]$Step
    } else {
        CS_TableOption.Min = CS_TableOption
        CS_TableOption.Max = CS_TableOption
        CS_TableOption.Step = 10000
    }
    
    TH_MA.Enabled = variables[variables$Variables=="TH_MA",]$Enabled
    if (TH_MA.Enabled) {
        TH_MA.Min = variables[variables$Variables=="TH_MA",]$Min
        TH_MA.Max = variables[variables$Variables=="TH_MA",]$Max
        TH_MA.Step = variables[variables$Variables=="TH_MA",]$Step
    } else {
        TH_MA.Min = TH_MA
        TH_MA.Max = TH_MA
        TH_MA.Step = 10000
    }    

    TH_MinWght.Enabled = variables[variables$Variables=="TH_MinWght",]$Enabled
    if (TH_MinWght.Enabled) {
        TH_MinWght.Min = variables[variables$Variables=="TH_MinWght",]$Min
        TH_MinWght.Max = variables[variables$Variables=="TH_MinWght",]$Max
        TH_MinWght.Step = variables[variables$Variables=="TH_MinWght",]$Step
    } else {
        TH_MinWght.Min = TH_MinWght
        TH_MinWght.Max = TH_MinWght
        TH_MinWght.Step = 10000
    }

    OutPerformMultiple.Enabled = variables[variables$Variables=="OutPerformMultiple",]$Enabled
    if (OutPerformMultiple.Enabled) {
        OutPerformMultiple.Min = variables[variables$Variables=="OutPerformMultiple",]$Min
        OutPerformMultiple.Max = variables[variables$Variables=="OutPerformMultiple",]$Max
        OutPerformMultiple.Step = variables[variables$Variables=="OutPerformMultiple",]$Step
    } else {
        OutPerformMultiple.Min = OutPerformMultiple
        OutPerformMultiple.Max = OutPerformMultiple
        OutPerformMultiple.Step = 10000
    }        
    
    for (i in seq(CS_Opinion_NA.Min, CS_Opinion_NA.Max, CS_Opinion_NA.Step)) {
        for (j in seq(CS_TableOption.Min, CS_TableOption.Max, CS_TableOption.Step)) {
            for (k in seq(TH_MA.Min, TH_MA.Max, TH_MA.Step)) {
                for (l in seq(TH_MinWght.Min, TH_MinWght.Max, TH_MinWght.Step)) {
                    for (m in seq(OutPerformMultiple.Min, OutPerformMultiple.Max, OutPerformMultiple.Step)) {
                        print (paste(i, j, k, l, m))     
                        generateMetrics(RF, CMPM, session, fn, i, j, k, l, m)
                    }
                }
            }
        }
    }
    end_time = Sys.time()
    time_took = as.numeric(difftime(end_time, start_time, units=("mins")))
    time_took = format(round(time_took, 1), nsmall = 1)
    result = paste("grid search completed, it took", time_took, "mins.")
    return (result)
}

# generate metrics given input parameters
generateMetrics <- function(RF, CMPM, session, fn, Opinion.NA, TableOption, MA, MinWght, Multiple) {
    

    RootFolder = RF
    # 1 = MV%, 2 = DtS, 3 = DtS Cash
    ConstructModelPortMethod = CMPM
    
    result = ""
    status_msg = ""
    status_obj = "OPT_status"
    final = data.frame(matrix(ncol=0,nrow=0))
    
    tryCatchLog({
        
        # override variable parameters
        CS_Opinion_NA <<- Opinion.NA
        CS_TableOption <<- TableOption
        TH_MA <<- MA
        TH_MinWght <<- MinWght
        OutPerformMultiple <<- Multiple
        gs = paste(Opinion.NA, TableOption, MA, MinWght, Multiple, sep = "-")
        
        
        status_msg = "calculating company opinions..."
        status_msg = paste(gs, status_msg, sep = "-")
        session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
        scores <<- scores2opinions(RootFolder)

        ########## load index file ##########
        for (j in 1:length(Indices)) {
            StartFrom = as.Date(TH.Limits[TH.Limits$Indices==Indices[j], "StartFrom"])
            status_msg = paste("merging", Indices[j], "data with company opinions...")
            status_msg = paste(gs, status_msg, sep = "-")
            session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
            
            ds = indexdata(Indices[j], RootFolder, IndexDataFolder, StartFrom)
            ds = indexop(Indices[j], ds, 0, NA)
            
            # calculate thresholds
            status_msg = paste("calculating", Indices[j], "thresholds...")
            status_msg = paste(gs, status_msg, sep = "-")
            session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
            th = indexth(Indices[j], ds)
            
            ########## MV%/DtS/DtS Cash ##########
            status_msg = paste("calculating portfolio weight for", Indices[j], "...")
            status_msg = paste(gs, status_msg, sep = "-")
            session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
            
            ds = computePortWght(ds, th, ConstructModelPortMethod)
            
            ########## limit max weight per name ##########
            status_msg = paste("limiting portfolio weight for", Indices[j], "...")
            status_msg = paste(gs, status_msg, sep = "-")
            session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
            
            
            ds$TotalMonthMktWgt = ave(ds$FinalMktWeight, ds$IndexDate, FUN=sum)
            ds$ReWeighted = ifelse(ds$TotalMonthMktWgt==0, 0, ds$FinalMktWeight*100/ds$TotalMonthMktWgt)
            ds$TickerWeight = ave(ds$ReWeighted, ds$IndexTickerDate, FUN=sum)
            ds$TickerWeight = ifelse(ds$TickerWeight>MaxWeightPerName,MaxWeightPerName/ds$TickerWeight,1)
            ds$ReWeighted = ds$ReWeighted*ds$TickerWeight
            ds$TotalMonthMktWgt2 = ave(ds$ReWeighted, ds$IndexDate, FUN=sum)
            ds$ReWeighted2 = ifelse(ds$TotalMonthMktWgt2==0, 0, ds$ReWeighted*100/ds$TotalMonthMktWgt2)
            ds$TickerWeight = 0
                
            df = ds %>% split(f=ds$IndexDate)
            df = lapply(df, limitWeight)
            output = do.call(rbind,df)

            # destory ds
            rm(ds)
            gc()
            
            # setting up for dynamic update for powerpivot
            names(output)[names(output) == "PrevMend.Mkt...Index.Wght"] = "Orig.PrevMend.Mkt...Index.Wght"
            output$PrevMend.Mkt...Index.Wght=ifelse(output$F.OP==1,output$ReWeighted2/OutPerformMultiple,output$ReWeighted2)

            output = output[, !(colnames(output) %in% c("TickerWeight", "IndexDate", "IndexTickerDate"))]

            if (nrow(final)==0) {
                # create an empty final dataframe with same structure of output
                final = output[0,]
            }
            final=rbind(final, output)
            
            # destory output
            rm(output)
            gc()
        }
        
        ########## final touches ##########
        final = final[order(final$PK),]
        final$Index = seq.int(nrow(final))
        final$Excess.Rtn...MTD = ifelse(is.na(final$Excess.Rtn...MTD),0,final$Excess.Rtn...MTD)
        

        status_msg = paste("calculating final metrics...")
        status_msg = paste(gs, status_msg, sep = "-")
        session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
        
        final_index_month = computeMetrics(final, RootFolder)

        gs_params = setNames(data.frame(matrix(ncol = 5, nrow = 0)), 
                             c("CS_Opinion_NA", "CS_TableOption", "TH_MA", "TH_MinWght", "Multiple"))
        gs_params[1,] = list(Opinion.NA,TableOption, MA, MinWght, Multiple)
        for (i in 1:nrow(Periods)) {
            period = Periods[i,]
            writeMetrics(fn, final_index_month, 
                         as.character(period$Indices), 
                         as.character(period$Name), 
                         as.character(period$Start), 
                         as.character(period$End),
                         gs_params)
        }
        
    },
    error = function(e)
    {
        result = as.character(e)
    }
    , finally =
    {
    }
    )
    
}

# main backtest data processing function called from UI
processData <- function(RF, CS, TH, CMP, CMPM, CH, CH_OpinionDate, session) {
    
    RootFolder = RF
    CS.UseFile = CS
    TH.UseFile = TH
    ConstructModelPort = CMP
    # 1 = MV%, 2 = DtS, 3 = DtS Cash
    ConstructModelPortMethod = CMPM

    DEBUG = FALSE
    
    if (DEBUG) {
        RootFolder = "C:/MyProjects/Guru/BacktestR/"
        IndexDataFolder = "Bond raw data/Index constituents/"
        IndexLvlDataFolder = "Bond raw data/Index lvl data/"
        CS.UseFile = TRUE
        TH.UseFile = FALSE
        ConstructModelPort = TRUE
        ConstructModelPortMethod = 1
        CH = FALSE
        CH_OpinionDate = as.Date("2018-05-01")
        TH_MA = 6
    }
    
    ############################################################################################################
    ##########################   MAIN CODE STARTS HERE!!!   ####################################################
    ############################################################################################################
    result = ""
    status_msg = ""
    status_obj = ifelse(CH,"CH_status","status")
    start_time = Sys.time()
    final = data.frame(matrix(ncol=0,nrow=0))
    
    tryCatchLog({
        init_var()
        
        ########## load translation and company scores files ##########
        status_msg = "loading translation file..."
        session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
        trans <<- loadtrans(RootFolder)
        
        status_msg = "calculating company opinions..."
        session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
        if (CS.UseFile) {
            scores <<- read.csv(paste(RootFolder, "CompanyScoresBondCalc.csv", sep = ""))
            scores <<- scores[ , c("BBERG_ID","TABLES","M.SCORE", "M.NA","T.SCORE","T.NA","L.SCORE","L.NA","M.FORMULA_11","AdjDate","M.OP")]
            if (grepl("-", scores$AdjDate[1]))
                scores$AdjDate <<- as.Date(scores$AdjDate, "%Y-%m-%d")
            else
                scores$AdjDate <<- as.Date(scores$AdjDate, "%m/%d/%Y")
            
        } else {
            scores <<- scores2opinions(RootFolder)
            write.table(scores, file = paste(RootFolder, "CompanyScoresBondCalc.csv", sep =""), sep = ",", col.names = TRUE, row.names = FALSE)
        }
        
        ########## load index file ##########
        for (j in 1:length(Indices)) {
            if (DEBUG) {
                j = 1
            }
            StartFrom = as.Date(TH.Limits[TH.Limits$Indices==Indices[j], "StartFrom"])
            status_msg = paste("merging", Indices[j], "data with company opinions...")
            session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))

            if (CH) {
                ds = ch_data(Indices[j], RootFolder, IndexDataFolder, StartFrom)
            } else {
                ds = indexdata(Indices[j], RootFolder, IndexDataFolder, StartFrom)
            }
            ds = indexop(Indices[j], ds, CH, CH_OpinionDate)
            
            # calculate thresholds
            status_msg = paste("calculating", Indices[j], "thresholds...")
            session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))

            if (TH.UseFile) {
                if (CH) {
                    th = read.csv(paste(RootFolder,"CH_THRESHOLDS.csv",sep = ""))
                } else {
                    th = read.csv(paste(RootFolder,"THRESHOLDS.csv",sep = ""))
                }
                if (grepl("-", th$Date[1]))
                    th$Date = as.Date(th$Date, "%Y-%m-%d")
                else
                    th$Date = as.Date(th$Date, "%m/%d/%Y")
                
            } else {
                #print(nrow(ds))
                th = indexth(Indices[j], ds)
                if (CH) { ds = subset(ds, ds$As.of.Date == max(ds$As.of.Date))}
                #print(nrow(ds))
                if (j==1) {
                    if (CH) {
                        write.table(th, file = paste(RootFolder, "CH_THRESHOLDS.csv", sep =""), sep = ",", col.names = TRUE, row.names = FALSE)
                    } else {
                        write.table(th, file = paste(RootFolder, "THRESHOLDS.csv", sep =""), sep = ",", col.names = TRUE, row.names = FALSE)
                    }
                }
                else {
                    if (CH) {
                        write.table(th, file = paste(RootFolder, "CH_THRESHOLDS.csv", sep =""), sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
                    } else {
                        write.table(th, file = paste(RootFolder, "THRESHOLDS.csv", sep =""), sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
                    }
                }
            }

            ########## MV%/DtS/DtS Cash ##########
            status_msg = paste("calculating portfolio weight for", Indices[j], "...")
            session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
            
            ds = computePortWght(ds, th, ConstructModelPortMethod)
            
            ########## limit max weight per name ##########
            status_msg = paste("limiting portfolio weight for", Indices[j], "...")
            session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))

            ds$TotalMonthMktWgt = ave(ds$FinalMktWeight, ds$IndexDate, FUN=sum)
            ds$ReWeighted = ifelse(ds$TotalMonthMktWgt==0, 0, ds$FinalMktWeight*100/ds$TotalMonthMktWgt)
            ds$TickerWeight = ave(ds$ReWeighted, ds$IndexTickerDate, FUN=sum)
            if (ConstructModelPort) {
                ds$TickerWeight = ifelse(ds$TickerWeight>MaxWeightPerName,MaxWeightPerName/ds$TickerWeight,1)
                ds$ReWeighted = ds$ReWeighted*ds$TickerWeight
                ds$TotalMonthMktWgt2 = ave(ds$ReWeighted, ds$IndexDate, FUN=sum)
                ds$ReWeighted2 = ifelse(ds$TotalMonthMktWgt2==0, 0, ds$ReWeighted*100/ds$TotalMonthMktWgt2)
                ds$TickerWeight = 0
                
                df = ds %>% split(f=ds$IndexDate)
                df = lapply(df, limitWeight)
                output = do.call(rbind,df)

                
            } else {
                ds$TotalMonthMktWgt2 = ds$TotalMonthMktWgt
                ds$ReWeighted2 = ds$ReWeighted
                output = ds
            }
            
            # destory ds
            rm(ds)
            gc()
            
            # setting up for dynamic update for powerpivot
            names(output)[names(output) == "PrevMend.Mkt...Index.Wght"] = "Orig.PrevMend.Mkt...Index.Wght"
            if (ConstructModelPort) {
                output$PrevMend.Mkt...Index.Wght=ifelse(output$F.OP==1,output$ReWeighted2/OutPerformMultiple,output$ReWeighted2)
            } else {
                output$PrevMend.Mkt...Index.Wght = output$Orig.PrevMend.Mkt...Index.Wght    
            }
            
            output = output[, !(colnames(output) %in% c("TickerWeight", "IndexDate", "IndexTickerDate"))]
            
            
            if (nrow(final)==0) {
                # create an empty final dataframe with same structure of output
                final = output[0,]
            }
            final=rbind(final, output)
            
            # destory output
            rm(output)
            gc()
        }
        
        ########## final touches ##########
        final = final[order(final$PK),]
        final$Index = seq.int(nrow(final))
        final$Excess.Rtn...MTD = ifelse(is.na(final$Excess.Rtn...MTD),0,final$Excess.Rtn...MTD)
        
        # export data
        result_file = ""
        if (CH) {
            result_file = "CurrentHoldings.csv"
        } else {
            result_file = "BondMaster.csv"
        }
        suppressWarnings(write.table(final, file = paste(RootFolder, result_file, sep =""), sep = ",", col.names = TRUE, row.names = FALSE))

        if (!CH) {
            status_msg = paste("calculating final metrics...")
            session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
            
            final_index_month = computeMetrics(final, RootFolder)
            
            fn = paste(RootFolder, "metrics.csv", sep ="")
            if (file.exists(fn)) file.remove(fn)
     
            for (i in 1:nrow(Periods)) {
                period = Periods[i,]
                writeMetrics(fn, final_index_month, 
                             as.character(period$Indices), 
                             as.character(period$Name), 
                             as.character(period$Start), 
                             as.character(period$End),
                             NA)
            }
        }
        
        end_time = Sys.time()
        time_took = as.numeric(difftime(end_time, start_time, units=("mins")))
        time_took = format(round(time_took, 1), nsmall = 1)
        result = paste(result_file, "generated successfully. It contains:", nrow(final), "records and took", time_took, "mins.")
        
    },
    error = function(e)
    {
        result = as.character(e)
    }
    , finally =
    {
    }
    )
    
}

checkData <- function(RF, session) {

    RootFolder = RF
    DEBUG = FALSE

    if (DEBUG) {
        RootFolder = "C:/Users/xliao/Downloads/BacktestR/"
    }
    
    result = ""
    status_msg = ""
    status_obj = "DC_status"
    start_time = Sys.time()
    
    tryCatchLog({
        init_var()
        
        ########## load translation files ##########
        status_msg = "loading translation file..."
        session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
        trans <<- loadtrans(RootFolder)
        transequity <<- loadtransequity(RootFolder)

        status_msg = paste("merging equity data with translation file...")
        session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
        
        ds = equitydata(RootFolder)
        # missing raw data
        missing_months = as.data.frame(matrix(ncol = 2, nrow = 0))
        names(missing_months) = c("Index.Name", "As.of.Date")
        ds_months = ds %>% distinct(As.of.Date, .keep_all = FALSE)
        total_months = year(max(ds_months$As.of.Date)) - year(min(ds_months$As.of.Date))
        #print(total_months)
        for (i in 0:total_months) {
            cur_month = year(min(ds_months$As.of.Date)) + i
            if (nrow(subset(ds_months, year(ds_months$As.of.Date) == cur_month)) == 0) {
                #print(cur_month)
                missing_months[nrow(missing_months)+1,]=c("RAY",as.character(cur_month))
            }
        }
        #print(missing_months)
        suppressWarnings(write.table(missing_months, file = paste(RootFolder, "MissingRawData.csv", sep =""), sep = ",", col.names = TRUE, row.names = FALSE))

        # missing translation
        # joining equity file to translation file
        ds = ds[, c("Name", "Ticker", "As.of.Date")]
        ds$Index.Name = "RAY"
        #print(nrow(ds))
        ds = merge(ds, transequity[ , c("ORIGINAL.TICKER","FROM.DATE.LINK","TO.DATE.LINK","ID_BB_UNIQUE")],
                   by.x="Ticker", by.y="ORIGINAL.TICKER", all.x=TRUE)
        #print(nrow(ds))
        ds = subset(ds, is.na(ds$ID_BB_UNIQUE))
        #print(nrow(ds))
        ds = ds[, !(colnames(ds) %in% c("FROM.DATE.LINK","TO.DATE.LINK","ID_BB_UNIQUE"))]
        # remove duplicates
        ds = ds %>% distinct(Ticker,Name, .keep_all = TRUE)
        
        if (nrow(ds)>0) {
            # add other columns to conform column names with bond file
            ds$Cusip = ""
            ds$ISIN.number = ""
            ds$ISO.Country = ""
            ds$ML.Industry.Lvl.3 = ""
            ds$ML.Industry.Lvl.4 = ""
            names(ds)[names(ds) == "Name"] = "Bond.Name"
            # re-sequence columns
            ds = ds[, c("Cusip","ISIN.number","Bond.Name", "Ticker", "ISO.Country","ML.Industry.Lvl.3","ML.Industry.Lvl.4", "As.of.Date", "Index.Name")]
        }
        else {
            ds = as.data.frame(matrix(ncol = 9, nrow = 0))
            colnames(ds) = c("Cusip","ISIN.number","Bond.Name", "Ticker", "ISO.Country","ML.Industry.Lvl.3","ML.Industry.Lvl.4", "As.of.Date", "Index.Name")
        }
        write.table(ds, file = paste(RootFolder, "MissingTranslation.csv", sep =""), sep = ",", col.names = TRUE, row.names = FALSE)

        
        for (j in 1:length(Indices)) {
            if (DEBUG) {
                j = 1
                StartFrom = as.Date("2002-11-30")
            }
            StartFrom = as.Date(TH.Limits[TH.Limits$Indices==Indices[j], "StartFrom"])
            status_msg = paste("merging", Indices[j], "data with translation file...")
            session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
            
            ds = indexdata(Indices[j], RootFolder, IndexDataFolder, StartFrom)
            # missing raw data
            missing_months = as.data.frame(matrix(ncol = 2, nrow = 0))
            names(missing_months) = c("Index.Name", "As.of.Date")
            ds_months = ds %>% distinct(As.of.Date, .keep_all = FALSE)
            total_months = as.numeric(as.mondate(max(ds_months$As.of.Date)) - as.mondate(min(ds_months$As.of.Date)))
            #print(total_months)
            for (i in 0:total_months) {
                cur_month = as.mondate(min(ds_months$As.of.Date)) + i
                if (nrow(subset(ds_months, ds_months$As.of.Date == as.Date(cur_month))) == 0) {
                    #print(cur_month)
                    missing_months[nrow(missing_months)+1,]=c(Indices[j],as.character(as.Date(cur_month)))
                }
            }
            #print(missing_months)
            write.table(missing_months, file = paste(RootFolder, "MissingRawData.csv", sep =""), sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)

            # missing translation
            # joining index file to translation file
            ds = ds[, c("Cusip","ISIN.number","Bond.Name", "Ticker", "ISO.Country","ML.Industry.Lvl.3","ML.Industry.Lvl.4", "As.of.Date")]
            ds$Index.Name = Indices[j]
            ds = merge(ds, trans[ , c("Bond.Ticker","FROM.DATE.LINK","TO.DATE.LINK","ID_BB_UNIQUE")], 
                        by.x="Ticker", by.y="Bond.Ticker", all.x=TRUE)
            ds = subset(ds, is.na(ds$ID_BB_UNIQUE))
            ds = ds[, !(colnames(ds) %in% c("FROM.DATE.LINK","TO.DATE.LINK","ID_BB_UNIQUE"))]
            # remove duplicates
            ds = ds %>% distinct(Ticker,Cusip,ISIN.number,Bond.Name, .keep_all = TRUE)
            # re-sequence columns
            ds = ds[, c("Cusip","ISIN.number","Bond.Name", "Ticker", "ISO.Country","ML.Industry.Lvl.3","ML.Industry.Lvl.4", "As.of.Date", "Index.Name")]
            write.table(ds, file = paste(RootFolder, "MissingTranslation.csv", sep =""), sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)

        }
        result_file = "MissingRawData.csv and MissingTranslation.csv"
        end_time = Sys.time()
        time_took = as.numeric(difftime(end_time, start_time, units=("mins")))
        time_took = format(round(time_took, 1), nsmall = 1)
        result = paste(result_file, "generated successfully and took", time_took, "mins.")
        
    }, 
    
    error = function(e)
    {
        result = as.character(e)
    }
    , finally =
    {
    }
    )
    
}