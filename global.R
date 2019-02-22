library(shiny)
library(shinyjs)
library(shinyBS)
library(rhandsontable)
library(dplyr)
library(anonymizer)
library(lubridate)
library(stringr)
library(mondate)
library(DataCombine)
library(data.table)
library(openxlsx)
library(TTR)
library(tryCatchLog)
library(ggplot2)
library(plotly)
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyr)

options(keep.source = FALSE)
options(tryCatchLog.write.error.dump.file = FALSE)
options(tryCatchLog.silent.messages       = TRUE)
options(tryCatchLog.silent.warnings       = TRUE)

########## global variables ##########

ui_index_list = c("HPID" = "HPID",
                  "EMNS" = "EMNS",
                  "EJ00" = "EJ00",
                  "HCNF" = "HCNF",
                  "CI00" = "CI00")

CS_MaxRange_Q = 0
CS_MaxRange_SA = 0
CS_AdjEntryDate_123Q = 0
CS_AdjEntryDate_1S = 0
CS_AdjEntryDate_4Q = 0
CS_AdjEntryDate_2S = 0
CS_Opinion_NA = 0
CS_Opinion_NA_Factor = 0
CS_TableOption = 0

ConstructModelPortDtSPHold = 0
ConstructModelPortDtSPBuy = 0

OutPerformMultiple = 0
MaxWeightPerName = 0
MaxLimit = 0

IndexDataFolder = ""
IndexLvlDataFolder = ""

TH_MinWght = 0
TH_MA = 0
TH_RangeDUR = 0
TH_RangeASW = 0

TEST_CA_SCORE = 0
TEST_CA_NA = 0
TEST_LD_SCORE = 0
TEST_LD_NA = 0
TEST_MIN_OBS = 0
TEST_VAL_TH = 0
TAIL_RATIO_PERCENTILE = 0


params=NULL
TH.Limits=NULL
Indices=NULL
Periods=NULL
DtsBuckets=NULL
DtsBuckets2=NULL
DtsBuckets3=NULL
PortRules=NULL
PortWeights_DtS=NULL
PortWeights_Val=NULL
PortWeights_VnD=NULL

trans=NULL
transequity=NULL
scores=NULL


init_var <- function() {
    

    ########## initialize variables ##########
    params <<- read.csv("parameters.csv")
    TH.Limits = read.csv("indices.csv")
    TH.Limits = subset(TH.Limits, TH.Limits$Include)
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
    TH.Limits <<- TH.Limits
    
    Indices <<- sort(as.vector(TH.Limits$Indices))

    Periods = read.csv("periods.csv")
    Periods$Start = as.Date(Periods$Start)
    Periods$End = as.Date(Periods$End)
    Periods = merge(Periods, TH.Limits[ , c("Indices", "Include")], by.x="Indices", by.y="Indices")
    Periods = Periods[order(Periods$Indices, Periods$Name),]
    
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
    #print(Periods)
    Periods <<- Periods
    
    DtsBuckets <<- read.csv("dtsbuckets.csv")
    DtsBuckets2 <<- read.csv("dtsbuckets2.csv")
    DtsBuckets3 <<- read.csv("dtsbuckets3.csv")
    
    PortRules <<- read.csv("portrules.csv")
    #PortRules = subset(PortRules, PortRules$Include)
    #PortRules <<- PortRules

    PortWeights_DtS = read.csv("PortWeights_DtS.csv")
    PortWeights_DtS$F.OP = ifelse(PortWeights_DtS$opinion=="OutPerform",1,ifelse(PortWeights_DtS$opinion=="Neutral",0,-1))
    PortWeights_DtS$op_bucket = paste(PortWeights_DtS$F.OP, PortWeights_DtS$bucket)
    PortWeights_DtS = PortWeights_DtS[,c("op_bucket", "weight")]
    PortWeights_DtS <<- PortWeights_DtS

    PortWeights_Val = read.csv("PortWeights_Val.csv")
    PortWeights_Val$F.OP = ifelse(PortWeights_Val$opinion=="OutPerform",1,ifelse(PortWeights_Val$opinion=="Neutral",0,-1))
    PortWeights_Val$op_bucket = paste(PortWeights_Val$F.OP, PortWeights_Val$bucket)
    PortWeights_Val = PortWeights_Val[,c("op_bucket", "weight")]
    PortWeights_Val <<- PortWeights_Val

    PortWeights_VnD = read.csv("PortWeights_VnD.csv")
    PortWeights_VnD$F.OP = ifelse(PortWeights_VnD$opinion=="OutPerform",1,ifelse(PortWeights_VnD$opinion=="Neutral",0,-1))
    PortWeights_VnD$op_bucket = paste(PortWeights_VnD$F.OP, PortWeights_VnD$bucket)
    PortWeights_VnD = PortWeights_VnD[,c("op_bucket", "weight")]
    PortWeights_VnD <<- PortWeights_VnD
    #print(PortWeights_VnD)
    ########## setting parameters ##########
    CS_MaxRange_Q <<- as.numeric(as.vector(params$value[params$name=="CS_MaxRange_Q"]))
    CS_MaxRange_SA <<- as.numeric(as.vector(params$value[params$name=="CS_MaxRange_SA"]))
    CS_AdjEntryDate_123Q <<- as.numeric(as.vector(params$value[params$name=="CS_AdjEntryDate_123Q"]))
    CS_AdjEntryDate_1S <<- as.numeric(as.vector(params$value[params$name=="CS_AdjEntryDate_1S"]))
    CS_AdjEntryDate_4Q <<- as.numeric(as.vector(params$value[params$name=="CS_AdjEntryDate_4Q"]))
    CS_AdjEntryDate_2S <<- as.numeric(as.vector(params$value[params$name=="CS_AdjEntryDate_2S"]))
    CS_Opinion_NA <<- as.numeric(as.vector(params$value[params$name=="CS_Opinion_NA"]))
    CS_Opinion_NA_Factor <<- as.numeric(as.vector(params$value[params$name=="CS_Opinion_NA_Factor"]))
    CS_TableOption <<- as.numeric(as.vector(params$value[params$name=="CS_TableOption"]))
    
    ConstructModelPortDtSPHold <<- as.numeric(as.vector(params$value[params$name=="ConstructModelPortDtSPHold"]))
    ConstructModelPortDtSPBuy <<- as.numeric(as.vector(params$value[params$name=="ConstructModelPortDtSPBuy"]))
    
    OutPerformMultiple <<- as.numeric(as.vector(params$value[params$name=="OutPerformMultiple"]))
    MaxWeightPerName <<- as.integer(as.vector(params$value[params$name=="MaxWeightPerName"]))
    MaxLimit <<- MaxWeightPerName - 0.05
    
    IndexDataFolder <<- as.character(as.vector(params$value[params$name=="IndexDataFolder"]))
    IndexLvlDataFolder <<- as.character(as.vector(params$value[params$name=="IndexLvlDataFolder"]))
    CompanyScoreFolder <<- as.character(as.vector(params$value[params$name=="CompanyScoreFolder"]))

    TH_MinWght <<- as.numeric(as.vector(params$value[params$name=="TH_MinWght"]))
    TH_MA <<- as.numeric(as.vector(params$value[params$name=="TH_MA"]))
    
    TEST_CA_SCORE <<- as.numeric(as.vector(params$value[params$name=="TEST_CA_SCORE"]))
    TEST_CA_NA <<- as.numeric(as.vector(params$value[params$name=="TEST_CA_NA"]))
    TEST_LD_SCORE <<- as.numeric(as.vector(params$value[params$name=="TEST_LD_SCORE"]))
    TEST_LD_NA <<- as.numeric(as.vector(params$value[params$name=="TEST_LD_NA"]))
    TEST_MIN_OBS <<- as.numeric(as.vector(params$value[params$name=="TEST_MIN_OBS"]))
    TEST_VAL_TH <<- as.numeric(as.vector(params$value[params$name=="TEST_VAL_TH"]))
    TAIL_RATIO_PERCENTILE <<- as.numeric(as.vector(params$value[params$name=="TAIL_RATIO_PERCENTILE"]))
    

}

#this function generates years dataframe in Periods structure
indexyears <- function(index, ds) {
    #Create an empty data frame from another
    #Years <- Periods[0,]
    ds = subset(ds, ds$Index.Name==index)
    ds$As.of.Date=as.Date(ds$As.of.Date)
    year_first = year(min(ds$As.of.Date))
    year_last = year(max(ds$As.of.Date))
    
    start_first = as.Date(paste0(year_first, "-01-01"))
    start_last = as.Date(paste0(year_last, "-01-01")) 

    end_first = as.Date(paste0(year_first+1, "-01-01"))
    end_last = as.Date(paste0(year_last+1, "-01-01")) 
    
    ds = data.frame(Indices=index, 
                         Name=paste0("Y-",c(year_first:year_last)),
                         Start=seq(start_first, start_last, "years"),
                         End=seq(end_first, end_last, "years"),
                         Include=TRUE)
    return(ds)
}


# this function checks if a file is already opened
file.opened <- function(path) {
    con = file(path, open = "w")
    close(con)
    return(FALSE)
}

# this function loads translation template
loadtrans <- function(RootFolder) {


    ########## load translation file ##########
    trans = read.xlsx(xlsxFile=paste(RootFolder, "Translation template.xlsx", sep = ""),
                      sheet = "Bond translation",
                      startRow = 14,
                      detectDates = TRUE,
                      na.strings = c(""),
                      cols = c(1,5,17,18,25))
    trans = subset(trans, is.na(trans$Bond.Ticker)==FALSE)
    trans = subset(trans, is.na(trans$ID_BB_UNIQUE)==FALSE)
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
    trans$TO.DATE.LINK = ifelse(is.na(trans$TO.DATE.LINK), as.Date("2020-01-01"), trans$TO.DATE.LINK)
    # convert TO.DATE.LINK back to date type
    trans$TO.DATE.LINK =as.Date(trans$TO.DATE.LINK, origin="1970-01-01")
    # convert FROM.DATE.LINK back to date type
    #trans$FROM.DATE.LINK =as.Date(trans$FROM.DATE.LINK, "%Y-%m-%d")
    trans$ID_BB_UNIQUE = gsub("EQ", "", trans$ID_BB_UNIQUE)
    trans = transform(trans, ID_BB_UNIQUE = as.numeric(ID_BB_UNIQUE))
    
    fn = paste(RootFolder, "trans.csv", sep = "")
    write.table(trans, file = fn, sep = ",", col.names = TRUE, row.names = FALSE)
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
    df <- do.call(rbind, df)
    df=subset(df, trimws(df$Index)!="")
    if (!is.null(df)) {
        df$Date=as.Date(as.character(df$Date), "%Y%m%d")
        # remove unused columns
        df = df[, !(colnames(df) %in% c("X"))]    
    }
    return(df)
}

# this function returns index level file
indexlvldata <- function(RootFolder, IndexLvlDataFolder) {
    

    file.list <- list.files(path=paste(RootFolder, IndexLvlDataFolder, "BOFA index lvl history", sep=""),
                            pattern='*.xlsx',
                            full.names=TRUE)
    df <- lapply(file.list, read.xlsx)
    df <- do.call(rbind, df)
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
    df <- do.call(rbind, df)
    df=subset(df, df$Index.Name==index)
    if (!is.null(df)) {
        df$As.of.Date=as.Date(df$As.of.Date, "%m/%d/%Y")
        df$Maturity.Date=as.Date(df$Maturity.Date, "%m/%d/%Y")
        colnames(df)[6] = "Bond.Name"
        # remove unused columns
        df = df[, !(colnames(df) %in% c("Description", "X"))]    
    }
    return(df)
}

# this function returns index file
indexdata <- function(index, RootFolder, IndexDataFolder, StartFrom) {
    

    file.list <- list.files(path=paste(RootFolder, IndexDataFolder, "BOFA ", index, " index history", sep=""),
                            pattern='*.xlsx',
                            full.names=TRUE)
    file.list <- subset(file.list, !grepl("~\\$",file.list))
    #print(file.list)
    df <- lapply(file.list, read.xlsx)
    df <- do.call(rbind, df)
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
    # if duration is na, what should we do?
    df$PrevMend.Mod.Dur.To.Worst[is.na(df$PrevMend.Mod.Dur.To.Worst)]=0
    df$PrevMend.AssetSwp[is.na(df$PrevMend.AssetSwp)]=0
    # get rid of excess ret is na
    df$Excess.Rtn...MTD = ifelse(is.na(df$Excess.Rtn...MTD),0,df$Excess.Rtn...MTD)
    # get rid of duration = 0
    df$PrevMend.Mod.Dur.To.Worst = ifelse(df$PrevMend.Mod.Dur.To.Worst<0.001,0.001,df$PrevMend.Mod.Dur.To.Worst)
    
    return(df)
}

# this function returns equity data file
equitydata <- function(RootFolder) {
    

    file.list <- list.files(path=paste(RootFolder, "Equity raw data", sep=""),
                            pattern='*.xlsx',
                            full.names=TRUE)
    df <- lapply(file.list, read.xlsx)
    df <- do.call(rbind, df)
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
        df <- do.call(rbind, df)
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
        # get rid of excess ret is na
        df$Excess.Rtn...MTD = ifelse(is.na(df$Excess.Rtn...MTD),0,df$Excess.Rtn...MTD)
        # get rid of duration = 0
        df$PrevMend.Mod.Dur.To.Worst = ifelse(df$PrevMend.Mod.Dur.To.Worst<0.001,0.001,df$PrevMend.Mod.Dur.To.Worst)
        
        # add X more month from regular index file
        df2 = indexdata(index, RootFolder, IndexDataFolder, StartFrom)
        # get rid of the monthly file that is later than the project file
        df2 = subset(df2, df2$As.of.Date<max(df$As.of.Date))
        #print(nrow(df2))
        #df2 = subset(df2, df2$As.of.Date<=max(df2$As.of.Date) & df2$As.of.Date>=as.Date(as.mondate(max(df2$As.of.Date))-TH_MA+1))
        df2 = subset(df2, df2$As.of.Date>=as.Date(as.mondate(max(df2$As.of.Date))-TH_MA+1))
        df = rbind(df, df2)
    } else {
        # if projected file doesn't exist, return X most recent months
        df = indexdata(index, RootFolder, IndexDataFolder, StartFrom)
        #print(nrow(df))
        #df = subset(df, df$As.of.Date<=max(df$As.of.Date) & df$As.of.Date>=as.Date(as.mondate(max(df$As.of.Date))-TH_MA))
        df = subset(df, df$As.of.Date>=as.Date(as.mondate(max(df$As.of.Date))-TH_MA))
    }
    
    # rename columns here
    # swap prevmend with currmend
    setnames(df, old=c("PrevMend.AssetSwp", "Asset.Swap", 
                       "PrevMend.Mod.Dur.To.Worst", "Semi.Mod.Dur.To.Worst", 
                       "PrevMend.Mkt...Index.Wght", "Mkt...Index.Wght",
                       "PrevMend.Price", "Price"), 
             new=c("Asset.Swap", "PrevMend.AssetSwp", 
                   "Semi.Mod.Dur.To.Worst", "PrevMend.Mod.Dur.To.Worst", 
                   "Mkt...Index.Wght", "PrevMend.Mkt...Index.Wght",
                   "Price", "PrevMend.Price"))
    
    return(df)
}

# this function returns new issues
new_issues <- function(RootFolder, final, final_index_month) {
    # read issue price
    new_issue = read.xlsx(xlsxFile=paste(RootFolder, "ni.xlsx", sep = ""),
                      sheet = "Main sheet",
                      startRow = 1,
                      detectDates = TRUE,
                      na.strings = c(""),
                      cols = c(1:6))
    new_issue = subset(new_issue, !grepl("N.A", new_issue$FIRST_SETTLE_DT))
    new_issue$issue_date=as.Date(new_issue$FIRST_SETTLE_DT, "%d.%m.%Y")
    new_issue$first_index_date = monthStart(new_issue$issue_date) - 1
    new_issue$first_index_date = as.Date(as.mondate(new_issue$first_index_date)+2)
    new_issue$ISINdate = paste(new_issue$ISIN.number, new_issue$first_index_date)
    final$ISINdate = paste(final$ISIN.number, final$As.of.Date)

    new_issue = merge(new_issue,
                      final[ , c("ISINdate","Index.Name","As.of.Date","Investment.Date","Ticker", "Cusip", "ISIN.number",
                                 "Par.Wtd.Coupon", "Face.Value.LOC", "PrevMend.Yld.To.Worst", "PrevMend.Price", 
                                 "PrevMend.Mod.Dur.To.Worst","PrevMend.AssetSwp", "OAS", "C.OP", "F.OP",
                                 "Rating", "ML.Industry.Lvl.1", "ML.Industry.Lvl.2", "ML.Industry.Lvl.3", "ML.Industry.Lvl.4")],
                      by.x="ISINdate", by.y="ISINdate", all.x = TRUE)
    # link index prevmend asw
    new_issue$IndexDate = paste(new_issue$Index.Name, new_issue$As.of.Date)
    final_index_month$IndexDate = paste(final_index_month$Index.Name, final_index_month$As.of.Date)
    new_issue = merge(new_issue, final_index_month[,c("IndexDate","idx_asw")], by.x="IndexDate", by.y="IndexDate", all.x = TRUE)
    new_issue = new_issue[, c("ISIN.number.x", "ISSUE_SPREAD_BNCHMRK", "ISSUE_PX", "issue_date", "Investment.Date",
                              "Index.Name", "Ticker", "Cusip", "Par.Wtd.Coupon", "Face.Value.LOC", 
                              "PrevMend.Yld.To.Worst", "PrevMend.Price", "PrevMend.Mod.Dur.To.Worst", "PrevMend.AssetSwp", "OAS",
                              "idx_asw", "C.OP", "F.OP", "Rating", 
                              "ML.Industry.Lvl.1", "ML.Industry.Lvl.2", "ML.Industry.Lvl.3", "ML.Industry.Lvl.4")]
    # final clean up
    names(new_issue)[names(new_issue) == "ISIN.number.x"] = "ISIN.number"
    #new_issue = new_issue %>% distinct(ISIN.number, .keep_all = TRUE)
    return(new_issue)
}

# this function returns new tickers by index
new_tickers <- function(index, RootFolder, IndexDataFolder, df2) {
    
    df = as.data.frame(matrix(ncol = 8, nrow = 0))
    names(df) = c("Index.Name", "Cusip", "ISIN.number", "Bond.Name", "Ticker", "ISO.Country", "ML.Industry.Lvl.3", "ML.Industry.Lvl.4")
    
    
    file.list <- list.files(path=paste(RootFolder, IndexDataFolder, "CurrentHoldings", sep=""),
                            pattern='*.csv',
                            full.names=TRUE)
    if (length(file.list) >0) {
        # if projected file exist
        df <- lapply(file.list, read.csv, skip=3, sep=";")
        df <- do.call(rbind, df)
        colnames(df)[6] = "Bond.Name"
        df=subset(df, df$Index.Name==index)
        df=subset(df, df$Ticker!="CASH")
        df=df[,c("Index.Name", "Cusip", "ISIN.number", "Bond.Name", "Ticker", "ISO.Country", "ML.Industry.Lvl.3", "ML.Industry.Lvl.4")]
        df$Index.Name = as.character(df$Index.Name)
        df$Ticker = as.character(df$Ticker)
        # remove duplicate Ticker
        df = df %>% distinct(Ticker, .keep_all = TRUE)    

        # get regular index file
        df2=df2[,c("Index.Name", "Cusip", "ISIN.number", "Bond.Name", "Ticker", "ISO.Country", "ML.Industry.Lvl.3", "ML.Industry.Lvl.4")]
        df2$Index.Name = as.character(df2$Index.Name)
        df2$Ticker = as.character(df2$Ticker)
        # remove duplicate Ticker
        df2 = df2 %>% distinct(Ticker, .keep_all = TRUE)
        # get all tickers in the current holding but not in regular file
        
        df = anti_join(df, df2, by=c("Index.Name", "Ticker"))
        
    }
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

    underperformTH = 3
    
    ifelse(rf=="Q",
           ifelse(na0>CS_Opinion_NA,-3,
                  ifelse(t0>=9,1,
                         ifelse(t0==8 & (na1<=CS_Opinion_NA & t1>=9),1,
                                ifelse(t0==8 & (na1<=CS_Opinion_NA & t1==8) & (na2<=CS_Opinion_NA & t2>=8),1, 
                                       ifelse(t0<=underperformTH,-1,
                                              ifelse(t0==4 & (na1<=CS_Opinion_NA & t1<=4) & (na2<=CS_Opinion_NA & t2<=4),-1,
                                                     ifelse(t0>=7 & (na1<=CS_Opinion_NA & t1>=7),0,-2))))))),
           ifelse(na0>CS_Opinion_NA,-3,
                  ifelse(t0>=9,1,
                         ifelse(t0==8 & (na1<=CS_Opinion_NA & t1>=8),1,
                                ifelse(t0<=underperformTH,-1,
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

# this function turns scores into opinions (orig)
scores2opinions <- function(RootFolder, ScoreType) {
    
    
    if (ScoreType=="BOND") {
        scores = read.csv(paste(RootFolder, CompanyScoreFolder, "BondsScoresBackT.csv", sep = ""))
        transRF = trans[, c("ID_BB_UNIQUE","REPORTING_FREQUENCY")]
    }
    else if (ScoreType=="EQUITY") {
        scores = read.csv(paste(RootFolder, CompanyScoreFolder, "EquitiesScoresBackT.csv", sep = ""))
        transRF = transequity[, c("ID_BB_UNIQUE","REPORTING_FREQUENCY")]
    }
    
    transRF = subset(transRF, is.na(transRF$REPORTING_FREQUENCY)==FALSE)
    transRF = subset(transRF, trimws(transRF$REPORTING_FREQUENCY)!="")
    transRF = subset(transRF, trimws(transRF$REPORTING_FREQUENCY)!="A")
    transRF = subset(transRF, trimws(transRF$REPORTING_FREQUENCY)!="0")
    # remove duplicated date + ticker
    transRF = transRF %>% distinct(ID_BB_UNIQUE,REPORTING_FREQUENCY, .keep_all = TRUE)
    
    scores = subset(scores, trimws(scores$BBERG_ID)!="")
    scores = subset(scores, nchar(as.character(scores$COMPANY_QUARTER))==6)
    scores = subset(scores, scores$TABLES!="RS")
    names(scores)[names(scores) == "MOMENTUM_11"] = "M.FORMULA_11"
    scores = scores[ ,c("BBERG_ID","TABLES","COMPANY_QUARTER","M.SCORE", "M.NA","T.SCORE","T.NA","L.SCORE","L.NA","M.FORMULA_11","DT_ENTRY","DATE","REAL_QUARTER")]
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
    scores$AdjDate = as.Date(scores$AdjDate, origin="1970-01-01")
    
    # now adjust MScore by guessing NA
    scores$M.SCORE = scores$M.SCORE + scores$M.SCORE / (11-scores$M.NA) * scores$M.NA * CS_Opinion_NA_Factor
    
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
    scores = scores[, !(colnames(scores) %in% c("DT_ENTRY","DATE","FilterByTable","COMPANY_QUARTER"))]
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
    
    # get rid of M.OP==-2
    opinions$M.OP=ifelse(opinions$M.OP==-2,0,opinions$M.OP)
    
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
        # special handling for CH, use current month end, plus 30 days to look back
        ds2$FROM.DATE.LINK = ifelse(grepl("Q", ds2$REPORTING_FREQUENCY), 
                                    ds2$As.of.Date+CS_MaxRange_Q+30,
                                    ds2$As.of.Date+CS_MaxRange_SA+30)
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
    
    # save from and to date links
    ds2$FROM = ds2$FROM.DATE.LINK
    ds2$TO = ds2$TO.DATE.LINK
    #print(head(ds2))    
    ###### longest running merge #######
    # joining index file to company scores (unequal join using data.table object)
    scores$AdjDate2 = scores$AdjDate
    ds2=setDT(ds2)[setDT(scores), on=.(ID_BB_UNIQUE=BBERG_ID, FROM.DATE.LINK<=AdjDate2, TO.DATE.LINK>AdjDate2),nomatch=0,allow.cartesian=TRUE]
    # convert data.table back to data.frame    
    ds2=as.data.frame(ds2)
    # keep only the top 1 AdjDate for each PK
    ds2 = ds2[order(ds2$PK, ds2$AdjDate),]
    ds2 = ds2 %>% dplyr::mutate(rn = row_number()) %>% group_by(PK) %>% top_n(1, rn)
    ds2 = as.data.frame(ds2)
    #print(head(ds2))    
    
    scoresNoLF = subset(scores, scores$M.OP!=-3)
    #print(nrow(scoresNoLF))
    dsLF = subset(ds2, ds2$M.OP==-3)
    dsNotLF = subset(ds2, ds2$M.OP!=-3)
    
    dsLFIgnore = dsLF[,c("PK","ID_BB_UNIQUE","REPORTING_FREQUENCY"
                         ,"FROM.DATE.LINK","TO.DATE.LINK","FROM","TO")]
    #print(nrow(dsLFIgnore))
    
    # joining index file to company scores (unequal join using data.table object)
    dsLFIgnore=setDT(dsLFIgnore)[setDT(scoresNoLF), on=.(ID_BB_UNIQUE=BBERG_ID, FROM<=AdjDate2, TO>AdjDate2),nomatch=0,allow.cartesian=TRUE]
    # convert data.table back to data.frame    
    dsLFIgnore=as.data.frame(dsLFIgnore)
    #print(nrow(dsLFIgnore))
    # keep only the top 1 AdjDate for each PK
    dsLFIgnore = dsLFIgnore[order(dsLFIgnore$PK, dsLFIgnore$AdjDate),]
    dsLFIgnore = dsLFIgnore %>% dplyr::mutate(rn = row_number()) %>% group_by(PK) %>% top_n(1, rn)
    dsLFIgnore = as.data.frame(dsLFIgnore)
    #print(nrow(dsLFIgnore))
    
    dsLF = subset(dsLF, !(dsLF$PK %in% dsLFIgnore$PK))
    ds2 = rbind(dsLF, dsLFIgnore, dsNotLF)    
    
    # get rid of the date links
    ds2 = ds2[, !(colnames(ds2) %in% c("FROM.DATE.LINK","TO.DATE.LINK","FROM","TO"))]
    # reformat Company ID back to its original format
    ds2$ID_BB_UNIQUE = paste("EQ", str_pad(ds2$ID_BB_UNIQUE, 16, pad = "0"), sep = "")
    
    # final join
    ds = merge(ds, ds2, by.x="PK", by.y="PK", all.x=TRUE)
    # destroy ds2
    rm(ds2)
    gc()
    
    # clean up M.OP and determine C.OP
    ds$M.OP = ifelse(is.na(ds$M.OP),-4,ds$M.OP)
    
    # Solvency
    ds$M11 = as.integer(ifelse(ds$M.FORMULA_11<1,0,1))
    ds$M11 = ifelse(is.na(ds$M11),-4,ds$M11)
    # creative accounting
    ds$CA = as.integer(ifelse(ds$T.SCORE<TEST_CA_SCORE & ds$T.NA<TEST_CA_NA, 0, 1))
    ds$CA = ifelse(is.na(ds$CA),-4,ds$CA)
    # liqudity
    ds$LD = as.integer(ifelse(ds$L.SCORE<TEST_LD_SCORE & ds$L.NA<TEST_LD_NA, 0, 1))
    ds$LD = ifelse(is.na(ds$LD),-4,ds$LD)
    
    # initialize C.OP to -1 unless no data or not enough data
    ds$C.OP = ifelse(ds$M.OP==-4 | ds$M.OP==-3,ds$M.OP,-1)
    
    
    for(i in 1:nrow(PortRules)) {
        #print(paste(PortRules$M.OP[i], PortRules$M11[i], PortRules$CA[i], PortRules$LD[i]))
        if (PortRules$C.OP[i]=="Worst") {
            OP=-2
        } else if  (PortRules$C.OP[i]=="UnderPerform") {
            OP=-1
        } else if  (PortRules$C.OP[i]=="Neutral") {
            OP=0
        } else if  (PortRules$C.OP[i]=="OutPerform") {
            OP=1
        }
        
        ds$C.OP = ifelse(ds$M.OP==PortRules$M.OP[i] 
                             & ds$M11==PortRules$M11[i] 
                             & ds$CA==PortRules$CA[i] 
                             & ds$LD==PortRules$LD[i],OP,ds$C.OP)
        
    }
    
    # old logic
    # for(i in 1:nrow(PortRules)) {
    #     #print(paste(PortRules$M.OP[i], PortRules$M11[i], PortRules$CA[i], PortRules$LD[i]))
    #     if (PortRules$M.OP[i]=="OutPerform") {
    #         ds$C.OP = ifelse(ds$M.OP==1 & ds$M11==PortRules$M11[i] & ds$CA==PortRules$CA[i] & ds$LD==PortRules$LD[i],1,ds$C.OP)
    #     } else if (PortRules$M.OP[i]=="Neutral") {
    #         ds$C.OP = ifelse((ds$M.OP==0 | ds$M.OP==-2) & ds$M11==PortRules$M11[i] & ds$CA==PortRules$CA[i] & ds$LD==PortRules$LD[i],0,ds$C.OP)
    #     } else {
    #         ds$C.OP = ifelse(ds$M.OP==-1 & ds$M11==PortRules$M11[i] & ds$CA==PortRules$CA[i] & ds$LD==PortRules$LD[i],0,ds$C.OP)
    #     }
    # }
        
    # old old logic
    #ds$C.OP = ifelse(ds$M.OP==-4 | ds$M.OP==-3 | ds$M.OP==-1,ds$M.OP,
    #                 ifelse((ds$M.OP==-2 | ds$M.OP==-0) & ds$M.FORMULA_11>=HoldM_RatioCutOff,ds$M.OP,
    #                        ifelse(ds$M.OP==1 & ds$M.FORMULA_11>=BuyM_RatioCutOff,ds$M.OP,-1)))
    
    return(ds)
}

# this function prepares thresholds limits based on user inputs
indexth <- function(index, ds, TH_NL, TH_UPM) {
    

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
        # calc moving avg for the first a few month when you can't go back TH_MA
        if (TH_MA > 2) {
            for (i in (TH_MA-1):2) {
                #print(i)
                ds_ma$AvgASW[is.na(ds_ma$AvgASW)]=runMean(subset(ds_ma, is.na(ds_ma$AvgASW))$AvgASWTemp, i)
                ds_ma$AvgDuration[is.na(ds_ma$AvgDuration)]=runMean(subset(ds_ma, is.na(ds_ma$AvgDuration))$AvgDurationTemp, i)
            }
        }
        ds_ma$AvgASW[is.na(ds_ma$AvgASW)]=subset(ds_ma, is.na(ds_ma$AvgASW))$AvgASWTemp
        ds_ma$AvgDuration[is.na(ds_ma$AvgDuration)]=subset(ds_ma, is.na(ds_ma$AvgDuration))$AvgDurationTemp
        ds_ma = ds_ma[, c("Index", "Date", "AvgASW", "AvgDuration")]
        ds = merge(ds, ds_ma, 
                   by.x=c("Index", "Date"), by.y=c("Index", "Date"), all.x=TRUE)
    }
    
    if (TH_RangeDUR == 0) {
        ds$BuyDurationMin = ds$AvgDuration * TH.Limits[TH.Limits$Indices==index, "BuyDurationLow"]
        ds$BuyDurationMax = ds$AvgDuration * TH.Limits[TH.Limits$Indices==index, "BuyDurationHigh"]
        ds$HoldDurationMin = ds$AvgDuration * TH.Limits[TH.Limits$Indices==index, "HoldDurationLow"]
        ds$HoldDurationMax = ds$AvgDuration * TH.Limits[TH.Limits$Indices==index, "HoldDurationHigh"]
    }
    else {
        ds$BuyDurationMin = ds$AvgDuration * (1 - TH_RangeDUR/100)
        ds$BuyDurationMax = ds$AvgDuration * (1 + TH_RangeDUR/100)
        ds$HoldDurationMin = ds$AvgDuration * (1 - TH_RangeDUR/100)
        ds$HoldDurationMax = ds$AvgDuration * (1 + TH_RangeDUR/100)
    }
    if (TH_RangeASW == 0) {
        ds$BuyASWMin = ds$AvgASW * TH.Limits[TH.Limits$Indices==index, "BuyASWLow"]
        ds$BuyASWMax = ds$AvgASW * TH.Limits[TH.Limits$Indices==index, "BuyASWHigh"]
        ds$HoldASWMin = ds$AvgASW * TH.Limits[TH.Limits$Indices==index, "HoldASWLow"]
        ds$HoldASWMax = ds$AvgASW * TH.Limits[TH.Limits$Indices==index, "HoldASWHigh"]
    }
    else {
        ds$BuyASWMin = ds$AvgASW * (1 - TH_RangeASW/100)
        ds$BuyASWMax = ds$AvgASW * (1 + TH_RangeASW/100)
        ds$HoldASWMin = ds$AvgASW * (1 - TH_RangeASW/100)
        ds$HoldASWMax = ds$AvgASW * (1 + TH_RangeASW/100)
    }
    return(indexopt(ds, TH_NL, TH_UPM))
}

# this function returns threshold file with optimization logic
indexopt <- function(df, TH_NL, TH_UPM) {
    
    
    df = df[order(df$Index,df$Date),]
    indexmonths = split(df, paste(df$Index, df$Date))
    
    MinWght = TH_MinWght
    
    DurationCutOff = 0
    DurationMax = 20
    ASWCutOff = 0
    if (TH_NL) ASWCutOff = -10000
    ASWMax = 10000
    DurationStep = 0.05
    ASWStep = 10
    
    
    thresholds <- data.frame(matrix(ncol = 22, nrow = 0))
    colnames(thresholds) = c("Index", "Date", 
                             "BuyDurationCutOff", "BuyDurationMax", "BuyASWCutOff", "BuyASWMax",
                             "HoldDurationCutOff", "HoldDurationMax", "HoldASWCutOff", "HoldASWMax", 
                             "obsindex", "obsbuy", "obshold",
                             "IndexDuration", "IndexASW", "PortDuration", "PortASW",
                             "BuyDuration", "BuyASW", "HoldDuration", "HoldASW",
                             "PortWght")
    
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
        # if no limits, no need to continue
        continue = !TH_NL
        solutionHold = FALSE
        while (continue) {
            im$OP = ifelse(im$C.OP==1 & im$ASW>=BuyASWCutOff & im$ASW<=BuyASWMax
                           & im$Duration>=BuyDurationCutOff & im$Duration<=BuyDurationMax,1,
                           ifelse(im$C.OP==0 & im$ASW>=HoldASWCutOff & im$ASW<=HoldASWMax
                                  & im$Duration>=HoldDurationCutOff & im$Duration<=HoldDurationMax,0,-1))
            im$PortWght = ifelse(im$OP==1, im$IndexWght*OutPerformMultiple, ifelse(im$OP==0, im$IndexWght,0))
            curPortWght = sum(im$PortWght)
            avgHoldDuration = weighted.mean(im$Duration[im$OP==0], im$IndexWght[im$OP==0])
            avgHoldASW = weighted.mean(im$ASW[im$OP==0], im$IndexWght[im$OP==0])
            if (is.nan(avgHoldDuration) | is.nan(avgHoldASW)) {
                continue = FALSE            
            }
            else if (curPortWght < MinWght) {
                continue = FALSE
            }
            else if (avgHoldASW < im$HoldASWMin[1]) {
                HoldASWCutOff = HoldASWCutOff + ASWStep
            } 
            else if (avgHoldASW > im$HoldASWMax[1]) {
                HoldASWMax = HoldASWMax - ASWStep
            } 
            else if (avgHoldDuration < im$HoldDurationMin[1]) {
                HoldDurationCutOff = HoldDurationCutOff + DurationStep
            } 
            else if (avgHoldDuration > im$HoldDurationMax[1]) {
                HoldDurationMax = HoldDurationMax - DurationStep
            } 
            else {
                solutionHold = TRUE
                continue = FALSE
            }
            
        }
        
        # Buy
        # if no limits, no need to continue
        continue = !TH_NL
        solutionBuy = FALSE
        while (continue) {
            im$OP = ifelse(im$C.OP==1 & im$ASW>=BuyASWCutOff & im$ASW<=BuyASWMax
                           & im$Duration>=BuyDurationCutOff & im$Duration<=BuyDurationMax,1,
                           ifelse(im$C.OP==0 & im$ASW>=HoldASWCutOff & im$ASW<=HoldASWMax
                                  & im$Duration>=HoldDurationCutOff & im$Duration<=HoldDurationMax,0,-1))
            im$PortWght = ifelse(im$OP==1, im$IndexWght*OutPerformMultiple, ifelse(im$OP==0, im$IndexWght,0))
            curPortWght = sum(im$PortWght)
            avgBuyDuration = weighted.mean(im$Duration[im$OP==1], im$IndexWght[im$OP==1])
            avgBuyASW = weighted.mean(im$ASW[im$OP==1], im$IndexWght[im$OP==1])
            if (is.nan(avgBuyDuration) | is.nan(avgBuyASW)) {
                continue = FALSE
            }
            else if (curPortWght < MinWght) {
                continue = FALSE
            }
            else if (avgBuyASW < im$BuyASWMin[1]) {
                BuyASWCutOff = BuyASWCutOff + ASWStep
            } 
            else if (avgBuyASW > im$BuyASWMax[1]) {
                BuyASWMax = BuyASWMax - ASWStep
            } 
            else if (avgBuyDuration < im$BuyDurationMin[1]) {
                BuyDurationCutOff = BuyDurationCutOff + DurationStep
            } 
            else if (avgBuyDuration > im$BuyDurationMax[1]) {
                BuyDurationMax = BuyDurationMax - DurationStep
            } 
            else {
                solutionBuy = TRUE
                continue = FALSE
            }
            
        }
        
        
        if (solutionBuy & solutionHold) {
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
            if (TH_UPM) {
                # if solution not found, use prev month
                HoldDurationCutOff = PrevHoldDurationCutOff
                BuyDurationCutOff = PrevBuyDurationCutOff
                HoldDurationMax = PrevHoldDurationMax
                BuyDurationMax = PrevBuyDurationMax
                HoldASWCutOff = PrevHoldASWCutOff
                BuyASWCutOff = PrevBuyASWCutOff
                HoldASWMax = PrevHoldASWMax
                BuyASWMax = PrevBuyASWMax
            }
        }
        
        
        # calculate metrics for logging
        obsindex = nrow(im)
        obsbuy = nrow(im[im$OP==1,])
        obshold = nrow(im[im$OP==0,])
        # this next line may not be necessary
        im$OP = ifelse(im$C.OP==1 & im$ASW>=BuyASWCutOff & im$ASW<=BuyASWMax
                       & im$Duration>=BuyDurationCutOff & im$Duration<=BuyDurationMax,1,
                       ifelse(im$C.OP==0 & im$ASW>=HoldASWCutOff & im$ASW<=HoldASWMax
                              & im$Duration>=HoldDurationCutOff & im$Duration<=HoldDurationMax,0,-1))
        im$PortWght = ifelse(im$OP==1, im$IndexWght*OutPerformMultiple, ifelse(im$OP==0, im$IndexWght,0))
        IndexDuration = weighted.mean(im$Duration, im$IndexWght)
        PortDuration = weighted.mean(im$Duration[im$PortWght>0], im$PortWght[im$PortWght>0])
        BuyDuration = weighted.mean(im$Duration[im$OP==1], im$IndexWght[im$OP==1]) 
        HoldDuration = weighted.mean(im$Duration[im$OP==0], im$IndexWght[im$OP==0]) 
        IndexASW = weighted.mean(im$ASW, im$IndexWght)
        PortASW = weighted.mean(im$ASW[im$PortWght>0], im$PortWght[im$PortWght>0])
        BuyASW = weighted.mean(im$ASW[im$OP==1], im$IndexWght[im$OP==1]) 
        HoldASW = weighted.mean(im$ASW[im$OP==0], im$IndexWght[im$OP==0]) 
        
        thresholds<-rbind(thresholds, data.frame(Index=im$Index[1],Date=im$Date[1],
                                                 BuyDurationCutOff=BuyDurationCutOff, BuyDurationMax=BuyDurationMax,
                                                 BuyASWCutOff=BuyASWCutOff, BuyASWMax=BuyASWMax,
                                                 HoldDurationCutOff=HoldDurationCutOff, HoldDurationMax=HoldDurationMax,
                                                 HoldASWCutOff=HoldASWCutOff, HoldASWMax=HoldASWMax,
                                                 obsindex=obsindex,obsbuy=obsbuy,obshold=obshold,
                                                 IndexDuration=IndexDuration,PortDuration=PortDuration,IndexASW=IndexASW,PortASW=PortASW,
                                                 BuyDuration=BuyDuration,HoldDuration=HoldDuration,BuyASW=BuyASW,HoldASW=HoldASW,
                                                 PortWght=sum(im$PortWght)))
        
    }
    return (thresholds)
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
    Opinion = row$C.OP
    NumOfBonds = row$NumOfBonds
    NumQualify = row$NumQualify
    TotalTicker = row$TotalTicker
    
    if (Opinion ==1)
    {
        # find the one that's qualified and closest to PHold and use it
        df = df[order(-df$Qualify, df$DtSPHoldDiff),]
        EfficientDtS = df[1,]$EfficientDtS
        df[1,]$FinalMktWeight = ifelse(EfficientDtS==0,0,TotalTicker/EfficientDtS)
        
        # find the one that's qualified and closest to PBuy and use it
        df = df[order(-df$Qualify, df$DtSPBuyDiff),]
        EfficientDtS = df[1,]$EfficientDtS
        df[1,]$FinalMktWeight = ifelse(EfficientDtS==0,0,df[1,]$FinalMktWeight + Multiple*TotalTicker/EfficientDtS)
    } else
    {
        # find the one that's qualified and closest to PHold and use it
        df = df[order(-df$Qualify, df$DtSPHoldDiff),]
        EfficientDtS = df[1,]$EfficientDtS
        df[1,]$FinalMktWeight = ifelse(EfficientDtS==0,0,TotalTicker/EfficientDtS)
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

# this function recast F-OP to reduce transaction cost
# go thru the CUSIP in the chronical order
# if last month it has a position, use C-OP (1, 0, -2) to determine if continue to have a position
# if last month it doesn't have a position, use F-OP (1, 0, -2) to determine if we should have a position
recastOP_A <- function(df) {
    
    #print("using OP_A")
    if (nrow(df) >0) {
        df = df[order(df$As.of.Date),]
        prevPosition = FALSE
        curPosition = FALSE
        for (i in 1:nrow(df)) {
            
            if (prevPosition) {
                curPosition = df[i,]$C.OP == 1 | df[i,]$C.OP == 0
            }
            else {
                curPosition = df[i,]$F.OP == 1 | df[i,]$F.OP == 0
            }
            df[i,]$F.OP = ifelse(curPosition, df[i,]$C.OP, df[i,]$F.OP)
            prevPosition = curPosition
        }
    }
    
    return (df)
}

# this function recast F-OP to reduce transaction cost
# go thru the CUSIP in the chronical order
# if last month it has a position and no C-OP change, use C-OP (1, 0, -2) to determine if continue to have a position
# if last month it doesn't have a position or C-OP changes, use F-OP (1, 0, -2) to determine if we should have a position
recastOP_B <- function(df) {
    
    
    if (nrow(df) >0) {
        #print("using OP_B")
        df = df[order(df$As.of.Date),]
        prevPosition = FALSE
        curPosition = FALSE
        prevOpinion = 10000
        curOpinion = NA
        op_chg = FALSE
        
        for (i in 1:nrow(df)) {
            
            curOpinion = df[i,]$C.OP
            if (curOpinion != prevOpinion) op_chg = TRUE else op_chg = FALSE
            if (prevPosition & !op_chg) {
                curPosition = df[i,]$C.OP == 1 | df[i,]$C.OP == 0
            }
            else {
                curPosition = df[i,]$F.OP == 1 | df[i,]$F.OP == 0
            }
            df[i,]$F.OP = ifelse(curPosition, df[i,]$C.OP, df[i,]$F.OP)
            prevPosition = curPosition
            prevOpinion = curOpinion
        }
    }
    
    return (df)
}

# this function add "DtSBucket" to the dataset
addDtSBucket <- function(ds) {
    
    if (!("DtS" %in% colnames(ds)))
    {
        ds$DtS = ds$PrevMend.Mod.Dur.To.Worst*ds$PrevMend.AssetSwp
    }
    
    ds2=subset(ds, ds$Index.Name=="EJ00")
    ds3=subset(ds, ds$Index.Name=="CI00")
    ds=subset(ds, ds$Index.Name=="EMNS" | ds$Index.Name=="HCNF" | ds$Index.Name=="HPID")
    
    DT1 = data.table(ds)
    DT2 = data.table(DtsBuckets)
    setkey(DT2, Min, Max)
    DT1[, c("Min", "Max") := DtS]
    ds = foverlaps(DT1, DT2)

    DT1 = data.table(ds2)
    DT2 = data.table(DtsBuckets2)
    setkey(DT2, Min, Max)
    DT1[, c("Min", "Max") := DtS]
    ds2 = foverlaps(DT1, DT2)

    DT1 = data.table(ds3)
    DT2 = data.table(DtsBuckets3)
    setkey(DT2, Min, Max)
    DT1[, c("Min", "Max") := DtS]
    ds3 = foverlaps(DT1, DT2)
    
    ds = rbind(ds, ds2, ds3)
    names(ds)[names(ds) == "Name"] = "DtSBucket.Name"
    names(ds)[names(ds) == "Label"] = "DtSBucket.Label"
    
    return(as.data.frame(ds))
}

# this function add "Val" to the dataset
addValuation <- function(ds, TEST_VAL_METRIC) {
    ds$IndexMonthRating = paste(ds$Index.Name, ds$As.of.Date, ds$Rating)
    val_th = group_by(ds, IndexMonthRating) %>% 
        summarize(
            obs = n(),
            asw_med = median(PrevMend.AssetSwp),
            asw.dur_med = median(PrevMend.AssetSwp/PrevMend.Mod.Dur.To.Worst)
        )
    val_th$asw_ul = val_th$asw_med * (1+TEST_VAL_TH/100)
    val_th$asw_ll = val_th$asw_med * (1-TEST_VAL_TH/100)
    val_th$asw.dur_ul = val_th$asw.dur_med * (1+TEST_VAL_TH/100)
    val_th$asw.dur_ll = val_th$asw.dur_med * (1-TEST_VAL_TH/100)
    if (TEST_VAL_METRIC == 1) {
        ds = merge(ds, val_th[ , c("IndexMonthRating", "asw.dur_ul", "asw.dur_ll", "asw.dur_med")], 
                   by.x="IndexMonthRating", by.y="IndexMonthRating", all.x=TRUE)    
        ds$asw.dur = ds$PrevMend.AssetSwp / ds$PrevMend.Mod.Dur.To.Worst
        ds$Val = ifelse(ds$asw.dur>ds$asw.dur_ul,"C",
                        ifelse(ds$asw.dur<ds$asw.dur_ll,"E","N"))
    } else {
        ds = merge(ds, val_th[ , c("IndexMonthRating", "asw_ul", "asw_ll", "asw_med")], 
                   by.x="IndexMonthRating", by.y="IndexMonthRating", all.x=TRUE)    
        ds$Val = ifelse(ds$PrevMend.AssetSwp>ds$asw_ul,"C",
                        ifelse(ds$PrevMend.AssetSwp<ds$asw_ll,"E","N"))
    }
    
    return(ds)
}

# this function compute portfolio weight
computePortWght <- function(ds, th, ConstructModelPortMethod, PortWeightMethod, TEST_VAL_METRIC) {
    # Merge with threshold and determine F.OP
    ds = merge(ds, th[,c("Index", "Date", "BuyASWCutOff", "BuyASWMax", "BuyDurationCutOff", "BuyDurationMax", "HoldASWCutOff", "HoldASWMax", "HoldDurationCutOff", "HoldDurationMax")], 
               by.x=c("Index.Name", "As.of.Date"), by.y=c("Index", "Date"), all.x=TRUE)

    ds$F.OP = ifelse(ds$C.OP==-4 | ds$C.OP==-3 | ds$C.OP==-2 | ds$C.OP==-1,ds$C.OP,
                     ifelse(ds$C.OP==0 
                            & ds$PrevMend.AssetSwp >= ds$HoldASWCutOff & ds$PrevMend.AssetSwp <= ds$HoldASWMax
                            & ds$PrevMend.Mod.Dur.To.Worst >= ds$HoldDurationCutOff & ds$PrevMend.Mod.Dur.To.Worst <= ds$HoldDurationMax,
                            ds$C.OP,
                            ifelse(ds$C.OP==1 
                                   & ds$PrevMend.AssetSwp >= ds$BuyASWCutOff & ds$PrevMend.AssetSwp <= ds$BuyASWMax
                                   & ds$PrevMend.Mod.Dur.To.Worst >= ds$BuyDurationCutOff & ds$PrevMend.Mod.Dur.To.Worst <= ds$BuyDurationMax,
                                   ds$C.OP,ds$C.OP-100)))
    
    if (ConstructModelPortMethod == 2) {
        df = ds %>% split(f=ds$Cusip)
        df = lapply(df, recastOP_A)
        ds = do.call(rbind,df)
    } else if (ConstructModelPortMethod == 3) {
        df = ds %>% split(f=ds$Cusip)
        df = lapply(df, recastOP_B)
        ds = do.call(rbind,df)
    }
    
    # remove columns no longer required to reduce memory usage
    ds = ds[, !(colnames(ds) %in% c("rn","BuyASWCutOff", "BuyASWMax", "BuyDurationCutOff", "BuyDurationMax", 
                                    "HoldASWCutOff", "HoldASWMax", "HoldDurationCutOff", "HoldDurationMax"))]
    
    # add additional columns
    ds$Investment.Date = as.mondate(ds$As.of.Date)-1
    ds$Investment.Date = as.Date(ds$Investment.Date)
    ds$IndexDate = paste(ds$Index.Name, ds$As.of.Date)
    ds$IndexTickerDate = paste(ds$Index.Name, ds$Ticker, ds$As.of.Date)

    ds$FinalMktWeight = 0
    ds$DtS = ds$PrevMend.Mod.Dur.To.Worst*ds$PrevMend.AssetSwp
    ds$WghtDtS = ds$PrevMend.Mkt...Index.Wght*ds$DtS
    ds$Qualify = ifelse(((ds$F.OP==1|ds$F.OP==0) & ds$DtS>0),1,0)
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
    
    # add DtS buckets  
    ds = addDtSBucket(ds)

    # add valuation
    ds = addValuation(ds, TEST_VAL_METRIC)

    # add val & dts
    ds$VnD = paste(ds$Val, ds$DtSBucket.Name)
    
    if (PortWeightMethod == 3 | PortWeightMethod == 4) {
        ds$op_bucket = paste(ds$F.OP, ds$DtSBucket.Name)
        # determine modified weight
        ds = merge(ds, PortWeights_DtS, by.x = "op_bucket", by.y = "op_bucket", all.x = TRUE)
        ds$weight = ifelse(is.na(ds$weight),0,ds$weight)
    } else if (PortWeightMethod == 5 | PortWeightMethod == 6) {
        ds$op_bucket = paste(ds$F.OP, ds$Val)
        # determine modified weight
        ds = merge(ds, PortWeights_Val, by.x = "op_bucket", by.y = "op_bucket", all.x = TRUE)
        ds$weight = ifelse(is.na(ds$weight),0,ds$weight)
    }  else if (PortWeightMethod == 7 | PortWeightMethod == 8) {
        ds$op_bucket = paste(ds$F.OP, ds$VnD)
        # determine modified weight
        ds = merge(ds, PortWeights_VnD, by.x = "op_bucket", by.y = "op_bucket", all.x = TRUE)
        ds$weight = ifelse(is.na(ds$weight),0,ds$weight)
    }
    
    #print(head(ds))
    
    if (PortWeightMethod == 1) {
        ds$ModifiedMktWeight = ifelse(ds$F.OP==1, ds$PrevMend.Mkt...Index.Wght*OutPerformMultiple,
                                      ifelse(ds$F.OP==0,ds$PrevMend.Mkt...Index.Wght,0))
    } else if (PortWeightMethod == 2) {
        ds$ModifiedMktWeight = ifelse(ds$F.OP==1, OutPerformMultiple,
                                      ifelse(ds$F.OP==0,1,0))
    } else if (PortWeightMethod == 3 | PortWeightMethod == 5 | PortWeightMethod == 7) {
        ds$ModifiedMktWeight = ds$PrevMend.Mkt...Index.Wght*ds$weight
    } else if (PortWeightMethod == 4 | PortWeightMethod == 6 | PortWeightMethod == 8) {
        ds$ModifiedMktWeight = ds$weight
    }

    #remove unused columns
    ds = ds[, !(colnames(ds) %in% c("op_bucket", "Min", "Max","i.Min","i.Max","weight",
                                    "DtSBucket.Name","DtSBucket.Label",
                                    "Val","VnD","IndexMonthRating"))]
    
    if (TEST_VAL_METRIC == 1) {
        ds = ds[, !(colnames(ds) %in% c("asw.dur","asw.dur_ul","asw.dur_ll", "asw.dur_med"))]
    } else {
        ds = ds[, !(colnames(ds) %in% c("asw_ul","asw_ll", "asw_med"))]
    }

    
    
    if (ConstructModelPortMethod <= 3) {
        # MV%
        ds$FinalMktWeight = ds$ModifiedMktWeight
    } else {
        # DtS
        # handling simple cases here for efficiency
        ds$FinalMktWeight = ifelse((ds$NumOfBonds<=3 & ds$DtS==ds$MaxDtS & ds$Qualify==1) | (ds$NumQualify==1 & ds$Qualify==1),
                                   ifelse(ds$C.OP==1,
                                          ifelse(ds$EfficientDtS==0,0,OutPerformMultiple*ds$TotalTicker/ds$EfficientDtS),
                                          ifelse(ds$C.OP==0,ifelse(ds$EfficientDtS==0,0,ds$TotalTicker/ds$EfficientDtS),
                                                 ds$FinalMktWeight)),
                                   ds$FinalMktWeight)
        
        # output1 need to pick 50 and 90 percentile
        dts_output1=subset(ds, ds$NumQualify>1 & ds$NumOfBonds>3 & ds$TotalTicker>0)
        # output2 is already done, separate it out
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
        
        if (ConstructModelPortMethod == 5) {
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
                                              ifelse(ds_cash$TickerOpinion==0,
                                                     ds_cash$TickerIndexWeight-ds_cash$TickerModelWeight,0))
            ds_cash = group_by(ds_cash,Index.Name,As.of.Date) %>% 
                summarize(FinalMktWeight = sum(TickerCashWeight))
            ds_cash$FinalMktWeight[ds_cash$FinalMktWeight<0]=0
            
            morecols = colnames(ds)[!(colnames(ds) %in% c("Index.Name","As.of.Date","FinalMktWeight"))]
            ds_cash[morecols] <- NA
            ds_cash$PK=paste(ds_cash$Index.Name, "CASH", ds_cash$As.of.Date, sep="")
            ds_cash$Investment.Date=as.mondate(ds_cash$As.of.Date)-1
            ds_cash$Investment.Date=as.Date(ds_cash$Investment.Date)
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
computeMetrics <- function(final, RootFolder, GS) {
    G0O1 = indexlvldata(RootFolder, IndexLvlDataFolder)
    G0O1 = subset(G0O1, G0O1$Index=="G0O1")
    
    
    final$IndexCusip = paste(final$Index.Name, final$Cusip, sep = "")
    final$F.OP.CAT = ifelse(final$F.OP==0, "Neutral", 
                            ifelse(final$F.OP==1, "OutPerform", "UnderPerform"))
    #final$DxS = final$PrevMend.Mod.Dur.To.Worst*final$PrevMend.AssetSwp
    
    
    # p1
    final$p1 = ifelse(final$F.OP==0|final$F.OP==1, 1,0)
    final$p1_trr = ifelse(final$F.OP==0|final$F.OP==1, final$TRR...MTD.LOC,0)
    final$p1_exr = ifelse(final$F.OP==0|final$F.OP==1, final$Excess.Rtn...MTD,0)
    final$p1_wght = final$ReWeighted2
    final$p1_dur = ifelse(final$F.OP==0|final$F.OP==1, final$PrevMend.Mod.Dur.To.Worst,0)
    final$p1_asw = ifelse(final$F.OP==0|final$F.OP==1, final$PrevMend.AssetSwp,0)
    final$p1_dxs = final$p1_dur*final$p1_asw
    
    
    final$Index1 = final$Index + 1
    final = merge(final, final[ , c("Index1","IndexCusip", "F.OP.CAT", "p1_wght")],
                  by.x="Index", by.y="Index1", all.x=TRUE)
    final = merge(final, TH.Limits[ , c("Indices","TransactionCost")],
                  by.x="Index.Name", by.y="Indices", all.x=TRUE)
    
    names(final)[names(final) == "IndexCusip.x"] = "IndexCusip"
    names(final)[names(final) == "F.OP.CAT.x"] = "F.OP.CAT"
    names(final)[names(final) == "p1_wght.x"] = "p1_wght"
    final$IndexCusip.y[1]="NONE"
    final$F.OP.CAT.y[1]="NONE"
    final$NewCusip = ifelse(final$IndexCusip==final$IndexCusip.y,0,1)
    final$F.OP.CAT.y = ifelse(final$NewCusip==1, "NONE", final$F.OP.CAT.y)
    final$p1.Op.Chg = ifelse(final$NewCusip==0,ifelse(final$F.OP.CAT!=final$F.OP.CAT.y,1,0),0)
    final$p1_TX = ifelse(final$p1.Op.Chg==1,abs(final$p1_wght-final$p1_wght.y),0) * final$TransactionCost / 100   #assume price = 100
    final$p1_TX2 = ifelse(final$p1.Op.Chg==1,final$p1_wght-final$p1_wght.y,0) * final$TransactionCost / 100       #assume price = 100
    final$p1_buy = (final$p1_TX + final$p1_TX2)/2*100/final$TransactionCost
    final$p1_sell = (final$p1_TX - final$p1_TX2)/2*100/final$TransactionCost
    
    # index - working on turnover
    cusip_last = group_by(final, Index.Name, Cusip) %>% 
        summarize(
            cusip_last_date = max(As.of.Date)
        )
    
    index_last = group_by(final, Index.Name) %>% 
        summarize(
            index_last_date = max(As.of.Date)
        )
    
    cusip_last$IndexDate = paste(cusip_last$Index.Name, cusip_last$cusip_last_date)
    index_last$IndexDate = paste(index_last$Index.Name, index_last$index_last_date)
    cusip_last = merge(cusip_last, index_last, by.x = "IndexDate", by.y = "IndexDate", all.x=TRUE)
    cusip_last = subset(cusip_last, is.na(cusip_last$index_last_date))
    cusip_last$IndexCusipDate = paste(cusip_last$Index.Name.x,cusip_last$Cusip,cusip_last$cusip_last_date)
    cusip_last = cusip_last[,c("IndexCusipDate","cusip_last_date")]

    final$IndexCusipDate = paste(final$Index.Name,final$Cusip,final$As.of.Date)
    final = merge(final, cusip_last, by.x="IndexCusipDate", by.y="IndexCusipDate", all.x=TRUE)
    final$idx_tx_wght = ifelse(is.na(final$cusip_last_date),0,final$Orig.PrevMend.Mkt...Index.Wght)
    final$idx_TX = final$idx_tx_wght * final$TransactionCost / 100   #assume price = 100
    final$idx_TX2 = 0
    final$idx_buy = final$idx_tx_wght
    final$idx_sell = final$idx_tx_wght

    # op1
    final$op1 = ifelse(final$F.OP==1, 1,0)
    final$op1_trr = ifelse(final$F.OP==1, final$TRR...MTD.LOC,0)
    final$op1_exr = ifelse(final$F.OP==1, final$Excess.Rtn...MTD,0)
    final$op1_wght = ifelse(final$F.OP==1, final$Orig.PrevMend.Mkt...Index.Wght,0)
    final$op1_dur = ifelse(final$F.OP==1, final$PrevMend.Mod.Dur.To.Worst,0)
    final$op1_asw = ifelse(final$F.OP==1, final$PrevMend.AssetSwp,0)
    final$op1_dxs = final$op1_dur*final$op1_asw
    
    # op0
    final$op0 = ifelse(final$F.OP==0, 1,0)
    final$op0_trr = ifelse(final$F.OP==0, final$TRR...MTD.LOC,0)
    final$op0_exr = ifelse(final$F.OP==0, final$Excess.Rtn...MTD,0)
    final$op0_wght = ifelse(final$F.OP==0, final$Orig.PrevMend.Mkt...Index.Wght,0)
    final$op0_dur = ifelse(final$F.OP==0, final$PrevMend.Mod.Dur.To.Worst,0)
    final$op0_asw = ifelse(final$F.OP==0, final$PrevMend.AssetSwp,0)
    final$op0_dxs = final$op0_dur*final$op0_asw
    
    # op_1
    final$op_1 = ifelse(final$F.OP==-1, 1,0)
    final$op_1_trr = ifelse(final$F.OP==-1, final$TRR...MTD.LOC,0)
    final$op_1_exr = ifelse(final$F.OP==-1, final$Excess.Rtn...MTD,0)
    final$op_1_wght = ifelse(final$F.OP==-1, final$Orig.PrevMend.Mkt...Index.Wght,0)
    final$op_1_dur = ifelse(final$F.OP==-1, final$PrevMend.Mod.Dur.To.Worst,0)
    final$op_1_asw = ifelse(final$F.OP==-1, final$PrevMend.AssetSwp,0)
    final$op_1_dxs = final$op_1_dur*final$op_1_asw

    # op_2
    final$op_2 = ifelse(final$F.OP==-2, 1,0)
    final$op_2_trr = ifelse(final$F.OP==-2, final$TRR...MTD.LOC,0)
    final$op_2_exr = ifelse(final$F.OP==-2, final$Excess.Rtn...MTD,0)
    final$op_2_wght = ifelse(final$F.OP==-2, final$Orig.PrevMend.Mkt...Index.Wght,0)
    final$op_2_dur = ifelse(final$F.OP==-2, final$PrevMend.Mod.Dur.To.Worst,0)
    final$op_2_asw = ifelse(final$F.OP==-2, final$PrevMend.AssetSwp,0)
    final$op_2_dxs = final$op_2_dur*final$op_2_asw
    
    final_index_month = group_by(final, Index.Name, As.of.Date) %>% 
        summarize(
            # index
            idx_obs = n(),
            idx_trr = sum(TRR...MTD.LOC*Orig.PrevMend.Mkt...Index.Wght),
            idx_exr = sum(Excess.Rtn...MTD*Orig.PrevMend.Mkt...Index.Wght),
            idx_dur = sum(PrevMend.Mod.Dur.To.Worst*Orig.PrevMend.Mkt...Index.Wght),
            idx_asw = sum(PrevMend.AssetSwp*Orig.PrevMend.Mkt...Index.Wght),
            idx_dxs = sum(DtS*Orig.PrevMend.Mkt...Index.Wght),
            idx_wght = sum(Orig.PrevMend.Mkt...Index.Wght),
            idx_TX = sum(idx_TX),
            idx_TX2 = abs(sum(idx_TX2)),
            idx_buy = sum(idx_buy),
            idx_sell = sum(idx_sell),
            
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
            p1_wght = sum(p1_wght),
            
            # op1
            op1_obs = sum(op1),
            op1_trr = sum(op1_trr*op1_wght),
            op1_exr = sum(op1_exr*op1_wght),            
            op1_dur = sum(op1_dur*op1_wght),
            op1_asw = sum(op1_asw*op1_wght),
            op1_dxs = sum(op1_dxs*op1_wght),
            # this one last
            op1_wght = sum(op1_wght),
            
            # op0
            op0_obs = sum(op0),
            op0_trr = sum(op0_trr*op0_wght),
            op0_exr = sum(op0_exr*op0_wght),            
            op0_dur = sum(op0_dur*op0_wght),
            op0_asw = sum(op0_asw*op0_wght),
            op0_dxs = sum(op0_dxs*op0_wght),
            # this one last
            op0_wght = sum(op0_wght),
            
            # op_1
            op_1_obs = sum(op_1),
            op_1_trr = sum(op_1_trr*op_1_wght),
            op_1_exr = sum(op_1_exr*op_1_wght),            
            op_1_dur = sum(op_1_dur*op_1_wght),
            op_1_asw = sum(op_1_asw*op_1_wght),
            op_1_dxs = sum(op_1_dxs*op_1_wght),
            # this one last
            op_1_wght = sum(op_1_wght),

            # op_2
            op_2_obs = sum(op_2),
            op_2_trr = sum(op_2_trr*op_2_wght),
            op_2_exr = sum(op_2_exr*op_2_wght),            
            op_2_dur = sum(op_2_dur*op_2_wght),
            op_2_asw = sum(op_2_asw*op_2_wght),
            op_2_dxs = sum(op_2_dxs*op_2_wght),
            # this one last
            op_2_wght = sum(op_2_wght)
            
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
    
    # p1 metrics
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
    
    #add tracking error
    final_index_month$p1_trkerr = final_index_month$p1_trr - final_index_month$idx_trr
    final_index_month$p1nx_trkerr = final_index_month$p1nx_trr - final_index_month$idx_trr
    

    # op1 metrics
    final_index_month$op1_dur = ifelse(final_index_month$op1_wght==0,0,final_index_month$op1_dur/final_index_month$op1_wght)
    final_index_month$op1_asw = ifelse(final_index_month$op1_wght==0,0,final_index_month$op1_asw/final_index_month$op1_wght)
    final_index_month$op1_dxs = ifelse(final_index_month$op1_wght==0,0,final_index_month$op1_dxs/final_index_month$op1_wght)
    
    final_index_month$op1_trr = ifelse(final_index_month$op1_wght==0,0,final_index_month$op1_trr/final_index_month$op1_wght)
    final_index_month$op1_trr_tmp = 1+final_index_month$op1_trr/100
    final_index_month$op1_exr = ifelse(final_index_month$op1_wght==0,0,final_index_month$op1_exr/final_index_month$op1_wght)
    final_index_month$op1_exr_tmp = 1+final_index_month$op1_exr/100
    final_index_month$op1_trrdn = ifelse(final_index_month$op1_trr>0,0,final_index_month$op1_trr)
    final_index_month$op1_err = final_index_month$op1_trr - final_index_month$TRR...1.month.LOC
    final_index_month$op1_err_tmp = 1+final_index_month$op1_err/100
    final_index_month$op1_trkerr = final_index_month$op1_trr - final_index_month$idx_trr

    
    # op0 metrics
    final_index_month$op0_dur = ifelse(final_index_month$op0_wght==0,0,final_index_month$op0_dur/final_index_month$op0_wght)
    final_index_month$op0_asw = ifelse(final_index_month$op0_wght==0,0,final_index_month$op0_asw/final_index_month$op0_wght)
    final_index_month$op0_dxs = ifelse(final_index_month$op0_wght==0,0,final_index_month$op0_dxs/final_index_month$op0_wght)
    
    final_index_month$op0_trr = ifelse(final_index_month$op0_wght==0,0,final_index_month$op0_trr/final_index_month$op0_wght)
    final_index_month$op0_trr_tmp = 1+final_index_month$op0_trr/100
    final_index_month$op0_exr = ifelse(final_index_month$op0_wght==0,0,final_index_month$op0_exr/final_index_month$op0_wght)
    final_index_month$op0_exr_tmp = 1+final_index_month$op0_exr/100
    final_index_month$op0_trrdn = ifelse(final_index_month$op0_trr>0,0,final_index_month$op0_trr)
    final_index_month$op0_err = final_index_month$op0_trr - final_index_month$TRR...1.month.LOC
    final_index_month$op0_err_tmp = 1+final_index_month$op0_err/100
    final_index_month$op0_trkerr = final_index_month$op0_trr - final_index_month$idx_trr
    
    # op_1 metrics
    final_index_month$op_1_dur = ifelse(final_index_month$op_1_wght==0,0,final_index_month$op_1_dur/final_index_month$op_1_wght)
    final_index_month$op_1_asw = ifelse(final_index_month$op_1_wght==0,0,final_index_month$op_1_asw/final_index_month$op_1_wght)
    final_index_month$op_1_dxs = ifelse(final_index_month$op_1_wght==0,0,final_index_month$op_1_dxs/final_index_month$op_1_wght)
    
    final_index_month$op_1_trr = ifelse(final_index_month$op_1_wght==0,0,final_index_month$op_1_trr/final_index_month$op_1_wght)
    final_index_month$op_1_trr_tmp = 1+final_index_month$op_1_trr/100
    final_index_month$op_1_exr = ifelse(final_index_month$op_1_wght==0,0,final_index_month$op_1_exr/final_index_month$op_1_wght)
    final_index_month$op_1_exr_tmp = 1+final_index_month$op_1_exr/100
    final_index_month$op_1_trrdn = ifelse(final_index_month$op_1_trr>0,0,final_index_month$op_1_trr)
    final_index_month$op_1_err = final_index_month$op_1_trr - final_index_month$TRR...1.month.LOC
    final_index_month$op_1_err_tmp = 1+final_index_month$op_1_err/100
    final_index_month$op_1_trkerr = final_index_month$op_1_trr - final_index_month$idx_trr

    # op_2 metrics
    final_index_month$op_2_dur = ifelse(final_index_month$op_2_wght==0,0,final_index_month$op_2_dur/final_index_month$op_2_wght)
    final_index_month$op_2_asw = ifelse(final_index_month$op_2_wght==0,0,final_index_month$op_2_asw/final_index_month$op_2_wght)
    final_index_month$op_2_dxs = ifelse(final_index_month$op_2_wght==0,0,final_index_month$op_2_dxs/final_index_month$op_2_wght)
    
    final_index_month$op_2_trr = ifelse(final_index_month$op_2_wght==0,0,final_index_month$op_2_trr/final_index_month$op_2_wght)
    final_index_month$op_2_trr_tmp = 1+final_index_month$op_2_trr/100
    final_index_month$op_2_exr = ifelse(final_index_month$op_2_wght==0,0,final_index_month$op_2_exr/final_index_month$op_2_wght)
    final_index_month$op_2_exr_tmp = 1+final_index_month$op_2_exr/100
    final_index_month$op_2_trrdn = ifelse(final_index_month$op_2_trr>0,0,final_index_month$op_2_trr)
    final_index_month$op_2_err = final_index_month$op_2_trr - final_index_month$TRR...1.month.LOC
    final_index_month$op_2_err_tmp = 1+final_index_month$op_2_err/100
    final_index_month$op_2_trkerr = final_index_month$op_2_trr - final_index_month$idx_trr
    
    # Return Retracement Ratio (RRR) for all portfolios
    final_index_month$sort = as.numeric(final_index_month$As.of.Date)
    # sort by As.of.Date
    final_index_month = final_index_month[order(final_index_month$Index.Name, final_index_month$sort),]
    # index
    final_index_month$idx_nav = ave(final_index_month$idx_trr_tmp, final_index_month$Index.Name, FUN=cumprod)
    final_index_month$idx_pnh = ave(final_index_month$idx_nav, final_index_month$Index.Name, FUN=cummax)
    # p1
    final_index_month$p1_nav = ave(final_index_month$p1_trr_tmp, final_index_month$Index.Name, FUN=cumprod)
    final_index_month$p1_pnh = ave(final_index_month$p1_nav, final_index_month$Index.Name, FUN=cummax)
    final_index_month$p1nx_nav = ave(final_index_month$p1nx_trr_tmp, final_index_month$Index.Name, FUN=cumprod)
    final_index_month$p1nx_pnh = ave(final_index_month$p1nx_nav, final_index_month$Index.Name, FUN=cummax)
    # op1
    final_index_month$op1_nav = ave(final_index_month$op1_trr_tmp, final_index_month$Index.Name, FUN=cumprod)
    final_index_month$op1_pnh = ave(final_index_month$op1_nav, final_index_month$Index.Name, FUN=cummax)
    # op0
    final_index_month$op0_nav = ave(final_index_month$op0_trr_tmp, final_index_month$Index.Name, FUN=cumprod)
    final_index_month$op0_pnh = ave(final_index_month$op0_nav, final_index_month$Index.Name, FUN=cummax)
    # op_1
    final_index_month$op_1_nav = ave(final_index_month$op_1_trr_tmp, final_index_month$Index.Name, FUN=cumprod)
    final_index_month$op_1_pnh = ave(final_index_month$op_1_nav, final_index_month$Index.Name, FUN=cummax)
    # op_2
    final_index_month$op_2_nav = ave(final_index_month$op_2_trr_tmp, final_index_month$Index.Name, FUN=cumprod)
    final_index_month$op_2_pnh = ave(final_index_month$op_2_nav, final_index_month$Index.Name, FUN=cummax)
    
    # sort by As.of.Date reverse
    final_index_month = final_index_month[order(final_index_month$Index.Name, -final_index_month$sort),]
    # index
    final_index_month$idx_snl = ave(final_index_month$idx_nav, final_index_month$Index.Name, FUN=cummin)
    final_index_month$idx_mrpnh = (1-final_index_month$idx_nav/final_index_month$idx_pnh)*100
    final_index_month$idx_mrsnl = (1-final_index_month$idx_snl/final_index_month$idx_nav)*100
    final_index_month$idx_mr = pmax(final_index_month$idx_mrpnh, final_index_month$idx_mrsnl)
    # p1
    final_index_month$p1_snl = ave(final_index_month$p1_nav, final_index_month$Index.Name, FUN=cummin)
    final_index_month$p1_mrpnh = (1-final_index_month$p1_nav/final_index_month$p1_pnh)*100
    final_index_month$p1_mrsnl = (1-final_index_month$p1_snl/final_index_month$p1_nav)*100
    final_index_month$p1_mr = pmax(final_index_month$p1_mrpnh, final_index_month$p1_mrsnl)
    final_index_month$p1nx_snl = ave(final_index_month$p1nx_nav, final_index_month$Index.Name, FUN=cummin)
    final_index_month$p1nx_mrpnh = (1-final_index_month$p1nx_nav/final_index_month$p1nx_pnh)*100
    final_index_month$p1nx_mrsnl = (1-final_index_month$p1nx_snl/final_index_month$p1nx_nav)*100
    final_index_month$p1nx_mr = pmax(final_index_month$p1nx_mrpnh, final_index_month$p1nx_mrsnl)
    # op1
    final_index_month$op1_snl = ave(final_index_month$op1_nav, final_index_month$Index.Name, FUN=cummin)
    final_index_month$op1_mrpnh = (1-final_index_month$op1_nav/final_index_month$op1_pnh)*100
    final_index_month$op1_mrsnl = (1-final_index_month$op1_snl/final_index_month$op1_nav)*100
    final_index_month$op1_mr = pmax(final_index_month$op1_mrpnh, final_index_month$op1_mrsnl)
    # op0
    final_index_month$op0_snl = ave(final_index_month$op0_nav, final_index_month$Index.Name, FUN=cummin)
    final_index_month$op0_mrpnh = (1-final_index_month$op0_nav/final_index_month$op0_pnh)*100
    final_index_month$op0_mrsnl = (1-final_index_month$op0_snl/final_index_month$op0_nav)*100
    final_index_month$op0_mr = pmax(final_index_month$op0_mrpnh, final_index_month$op0_mrsnl)
    # op_1
    final_index_month$op_1_snl = ave(final_index_month$op_1_nav, final_index_month$Index.Name, FUN=cummin)
    final_index_month$op_1_mrpnh = (1-final_index_month$op_1_nav/final_index_month$op_1_pnh)*100
    final_index_month$op_1_mrsnl = (1-final_index_month$op_1_snl/final_index_month$op_1_nav)*100
    final_index_month$op_1_mr = pmax(final_index_month$op_1_mrpnh, final_index_month$op_1_mrsnl)
    # op_2
    final_index_month$op_2_snl = ave(final_index_month$op_2_nav, final_index_month$Index.Name, FUN=cummin)
    final_index_month$op_2_mrpnh = (1-final_index_month$op_2_nav/final_index_month$op_2_pnh)*100
    final_index_month$op_2_mrsnl = (1-final_index_month$op_2_snl/final_index_month$op_2_nav)*100
    final_index_month$op_2_mr = pmax(final_index_month$op_2_mrpnh, final_index_month$op_2_mrsnl)
    
    # sort by As.of.Date
    final_index_month = final_index_month[order(final_index_month$Index.Name, final_index_month$sort),]
    
    if (!GS) {
        # if not gridsearch, do the following
        # write transaction cost analysis file
        suppressWarnings(write.table(final[,c("Index.Name","As.of.Date","Ticker","Cusip","Orig.PrevMend.Mkt...Index.Wght",
                                              "PrevMend.Mod.Dur.To.Worst", "PrevMend.AssetSwp","p1.Op.Chg",
                                              "F.OP.CAT","M.FORMULA_11","M.OP","C.OP","F.OP","F.OP.CAT.y",
                                              "p1_TX","p1_TX2","p1_buy","p1_sell")], 
                                     file = paste(RootFolder, "TXAnalysis.csv", sep =""), sep = ",", col.names = TRUE, row.names = FALSE))
        
        # generate new issues file
        new_issue = new_issues(RootFolder, final, final_index_month)
        suppressWarnings(write.table(new_issue, 
                                     file = paste(RootFolder, "NewIssues.csv", sep =""), sep = ",", col.names = TRUE, row.names = FALSE))
        

    }
    
    return(final_index_month)
}

# this function writes metrics to a file for a given index and period
writeMetrics <- function(fn, final_index_month, index, period_name, period_start, period_end, gs_params) {
    TAIL_RATIO_PERCENTILE = 0.1
    
    #print(paste(index, period_name, period_start, period_end))
    if (!grepl("FULL", period_name)) {
        final_index_month = subset(final_index_month, final_index_month$Index.Name == index &
                                       final_index_month$As.of.Date>=period_start & final_index_month$As.of.Date<period_end)
    } else {
        final_index_month = subset(final_index_month, final_index_month$Index.Name == index)
        #print(final_index_month)
        #write.table(final_index_month, file = "test.csv", sep = ",", col.names = TRUE, row.names = FALSE)
    }
    if (nrow(final_index_month) > 0) {
        
        
        final_index = group_by(final_index_month, Index.Name) %>% 
            summarize(months = n(),
                      # index
                      idx_obs = sum(idx_obs),
                      idx_trr_prod = prod(idx_trr_tmp),
                      idx_exr_prod = prod(idx_exr_tmp),
                      idx_err_prod = prod(idx_err_tmp),
                      idx_dur = mean(idx_dur),
                      idx_asw = mean(idx_asw),
                      idx_dxs = mean(idx_dxs),
                      idx_stdev = sd(idx_trr)*sqrt(12),
                      idx_dndev = sqrt(sum(idx_trrdn*idx_trrdn)*12/n()),
                      idx_gpr = sum(idx_trr) / sum(abs(idx_trrdn)),
                      # Type 6 = PERCENTILE.EXC
                      # Type 7 = PERCENTILE.INC
                      idx_tr_high = quantile(idx_trr, 1-TAIL_RATIO_PERCENTILE, type = 6),
                      idx_tr_low = quantile(idx_trr, TAIL_RATIO_PERCENTILE, type = 6),
                      idx_amr = mean(idx_mr),
                      idx_buy = sum(idx_buy),
                      idx_sell = sum(idx_sell),
                      
                      #p1
                      p1_obs = sum(p1_obs),
                      p1_dur = mean(p1_dur),
                      p1_asw = mean(p1_asw),
                      p1_dxs = mean(p1_dxs),
                      # transaction cost
                      p1_trr_prod = prod(p1_trr_tmp),
                      p1_exr_prod = prod(p1_exr_tmp),
                      p1_err_prod = prod(p1_err_tmp),
                      p1_stdev = sd(p1_trr)*sqrt(12),
                      p1_dndev = sqrt(sum(p1_trrdn*p1_trrdn)*12/n()),
                      p1_gpr = sum(p1_trr) / sum(abs(p1_trrdn)),
                      # Type 6 = PERCENTILE.EXC
                      # Type 7 = PERCENTILE.INC
                      p1_tr_high = quantile(p1_trr, 1-TAIL_RATIO_PERCENTILE, type = 6),
                      p1_tr_low = quantile(p1_trr, TAIL_RATIO_PERCENTILE, type = 6),
                      p1_amr = mean(p1_mr),
                      p1_buy = sum(p1_buy),
                      p1_sell = sum(p1_sell),
                      
                      # no transaction cost
                      p1nx_trr_prod = prod(p1nx_trr_tmp),
                      p1nx_exr_prod = prod(p1nx_exr_tmp),
                      p1nx_err_prod = prod(p1nx_err_tmp),
                      p1nx_stdev = sd(p1nx_trr)*sqrt(12),
                      p1nx_dndev = sqrt(sum(p1nx_trrdn*p1nx_trrdn)*12/n()),
                      p1nx_gpr = sum(p1nx_trr) / sum(abs(p1nx_trrdn)),
                      # Type 6 = PERCENTILE.EXC
                      # Type 7 = PERCENTILE.INC
                      p1nx_tr_high = quantile(p1nx_trr, 1-TAIL_RATIO_PERCENTILE, type = 6),
                      p1nx_tr_low = quantile(p1nx_trr, TAIL_RATIO_PERCENTILE, type = 6),
                      p1nx_amr = mean(p1nx_mr),
                      
                      #tracking error
                      p1_trkerrSD = sd(p1_trkerr)*sqrt(12),
                      p1nx_trkerrSD = sd(p1nx_trkerr)*sqrt(12)
            )
        #print(final_index$p1_trkerrSD)
        # index
        final_index$idx_trr = (`^`(final_index$idx_trr_prod,12/final_index$months)-1)*100
        final_index$idx_exr = (`^`(final_index$idx_exr_prod,12/final_index$months)-1)*100
        final_index$idx_err = (`^`(final_index$idx_err_prod,12/final_index$months)-1)*100
        final_index$idx_sharpe = final_index$idx_err/final_index$idx_stdev
        final_index$idx_sortino = final_index$idx_trr/final_index$idx_dndev
        final_index$idx_sdrsr = final_index$idx_err/final_index$idx_dndev/sqrt(2)
        final_index$idx_rrr = final_index$idx_err/final_index$idx_amr
        #p1
        final_index$p1_trr = (`^`(final_index$p1_trr_prod,12/final_index$months)-1)*100
        final_index$p1_exr = (`^`(final_index$p1_exr_prod,12/final_index$months)-1)*100
        final_index$p1_err = (`^`(final_index$p1_err_prod,12/final_index$months)-1)*100
        final_index$p1_sharpe = final_index$p1_err/final_index$p1_stdev
        final_index$p1_sortino = final_index$p1_trr/final_index$p1_dndev
        final_index$p1_sdrsr = final_index$p1_err/final_index$p1_dndev/sqrt(2)
        final_index$p1_rrr = final_index$p1_err/final_index$p1_amr
        
        # p1nx
        final_index$p1nx_trr = (`^`(final_index$p1nx_trr_prod,12/final_index$months)-1)*100
        final_index$p1nx_exr = (`^`(final_index$p1nx_exr_prod,12/final_index$months)-1)*100
        final_index$p1nx_err = (`^`(final_index$p1nx_err_prod,12/final_index$months)-1)*100
        final_index$p1nx_sharpe = final_index$p1nx_err/final_index$p1nx_stdev
        final_index$p1nx_sortino = final_index$p1nx_trr/final_index$p1nx_dndev
        final_index$p1nx_sdrsr = final_index$p1nx_err/final_index$p1nx_dndev/sqrt(2)
        final_index$p1nx_rrr = final_index$p1nx_err/final_index$p1nx_amr
        
        final_index$period = period_name
        final_index$var_dur = abs(final_index$p1_dur - final_index$idx_dur)/final_index$idx_dur
        final_index$var_asw = abs(final_index$p1_asw - final_index$idx_asw)/final_index$idx_asw
        final_index$var_trr = final_index$p1nx_trr - final_index$idx_trr
        final_index$var_exr = final_index$p1nx_exr - final_index$idx_exr
        final_index$var_err = final_index$p1nx_err - final_index$idx_err
        final_index$var_sharpe = final_index$p1nx_sharpe - final_index$idx_sharpe
        final_index$var_sortino = final_index$p1nx_sortino - final_index$idx_sortino
        final_index$var_sdrsr = final_index$p1nx_sdrsr - final_index$idx_sdrsr

        final_index$idx_turnover = with(final_index, pmin(idx_buy, idx_sell))
        final_index$idx_turnover = with(final_index, idx_turnover*12/months)
        
        final_index$p1_turnover = with(final_index, pmin(p1_buy, p1_sell))
        final_index$p1_turnover = with(final_index, p1_turnover*12/months)
        final_index$pct_obs = with(final_index, p1_obs/idx_obs)
        
        # next to compute tail ratio
        final_index_month = merge(final_index_month, final_index[ , c("Index.Name","idx_tr_high", "idx_tr_low", 
                                                                      "p1_tr_high", "p1_tr_low",
                                                                      "p1nx_tr_high", "p1nx_tr_low")], 
                                  by.x="Index.Name", by.y="Index.Name", all.x=TRUE)
        final_index_month$idx_tr_high_n = ifelse(final_index_month$idx_trr>final_index_month$idx_tr_high,final_index_month$idx_trr,0)
        final_index_month$idx_tr_high_d = ifelse(final_index_month$idx_trr>final_index_month$idx_tr_high,1,0)
        final_index_month$idx_tr_low_n = ifelse(final_index_month$idx_trr<final_index_month$idx_tr_low,final_index_month$idx_trr,0)
        final_index_month$idx_tr_low_d = ifelse(final_index_month$idx_trr<final_index_month$idx_tr_low,1,0)
        
        final_index_month$p1_tr_high_n = ifelse(final_index_month$p1_trr>final_index_month$p1_tr_high,final_index_month$p1_trr,0)
        final_index_month$p1_tr_high_d = ifelse(final_index_month$p1_trr>final_index_month$p1_tr_high,1,0)
        final_index_month$p1_tr_low_n = ifelse(final_index_month$p1_trr<final_index_month$p1_tr_low,final_index_month$p1_trr,0)
        final_index_month$p1_tr_low_d = ifelse(final_index_month$p1_trr<final_index_month$p1_tr_low,1,0)
        
        final_index_month$p1nx_tr_high_n = ifelse(final_index_month$p1nx_trr>final_index_month$p1nx_tr_high,final_index_month$p1nx_trr,0)
        final_index_month$p1nx_tr_high_d = ifelse(final_index_month$p1nx_trr>final_index_month$p1nx_tr_high,1,0)
        final_index_month$p1nx_tr_low_n = ifelse(final_index_month$p1nx_trr<final_index_month$p1nx_tr_low,final_index_month$p1nx_trr,0)
        final_index_month$p1nx_tr_low_d = ifelse(final_index_month$p1nx_trr<final_index_month$p1nx_tr_low,1,0)
        
        final_index_tr = group_by(final_index_month, Index.Name) %>% 
            summarize(idx_tr = (sum(idx_tr_high_n) / sum(idx_tr_high_d)) /
                          abs((sum(idx_tr_low_n) / sum(idx_tr_low_d))),
                      p1_tr = (sum(p1_tr_high_n) / sum(p1_tr_high_d)) /
                          abs((sum(p1_tr_low_n) / sum(p1_tr_low_d))),
                      p1nx_tr = (sum(p1nx_tr_high_n) / sum(p1nx_tr_high_d)) /
                          abs((sum(p1nx_tr_low_n) / sum(p1nx_tr_low_d)))
            )
        
        final_index = merge(final_index, final_index_tr, 
                            by.x="Index.Name", by.y="Index.Name", all.x=TRUE)
        
        # remove unused columns
        final_index = final_index[, !(colnames(final_index) %in% 
                                          c("idx_trr_prod", "idx_exr_prod", "idx_err_prod", 
                                            "p1_trr_prod", "p1_exr_prod", "p1_err_prod", "p1_buy", "p1_sell",
                                            "p1nx_trr_prod", "p1nx_exr_prod", "p1nx_err_prod",
                                            "idx_tr_high", "idx_tr_low", "p1_tr_high", "p1_tr_low", "p1nx_tr_high", "p1nx_tr_low",
                                            "idx_amr", "p1_amr", "p1nx_amr",
                                            "idx_buy", "idx_sell"))]    
        if (!is.null(nrow(gs_params))) final_index = cbind(final_index, gs_params)
        
        if (!file.exists(fn))
            write.table(final_index, file = fn, sep = ",", col.names = TRUE, row.names = FALSE)
        else
            write.table(final_index, file = fn, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
    }
    
}

# this function writes missing opinions to a file
MissingOpinions <- function(fn, final) {
    mo = subset(final, final$M.OP ==-4 | final$M.OP ==-3)
    mo = group_by(mo, Index.Name, Ticker, ID_BB_UNIQUE, M.OP, REPORTING_FREQUENCY) %>% 
        summarize(obs = n(),
                  Earliest = min(As.of.Date),
                  Latest = max(As.of.Date))
    mo_names = names(mo)
    mo = merge(mo, trans[,c("Bond.Ticker","ID_BB_UNIQUE","REPORTING_FREQUENCY")],
               by.x = "Ticker", by.y = "Bond.Ticker", all.x = TRUE)
    # reformat Company ID back to its original format
    mo$ID_BB_UNIQUE.y = paste("EQ", str_pad(mo$ID_BB_UNIQUE.y, 16, pad = "0"), sep = "")
    
    mo$ID_BB_UNIQUE.x = ifelse(is.na(mo$ID_BB_UNIQUE.x),mo$ID_BB_UNIQUE.y,mo$ID_BB_UNIQUE.x)
    mo$REPORTING_FREQUENCY.x = ifelse(is.na(mo$REPORTING_FREQUENCY.x),mo$REPORTING_FREQUENCY.y,mo$REPORTING_FREQUENCY.x)
    names(mo)[names(mo)=="ID_BB_UNIQUE.x"]="ID_BB_UNIQUE"
    names(mo)[names(mo)=="REPORTING_FREQUENCY.x"]="REPORTING_FREQUENCY"
    mo = mo[,mo_names]
    write.table(mo, file = fn, sep = ",", col.names = TRUE, row.names = FALSE)
}

# this function does a grid search for optimal
gridSearch <- function(RF, TH_NL, TH_UPM, CMPM, PWM, TEST_VAL_METRIC, session) {

    result = ""
    
    tryCatchLog({
    
        RootFolder = RF
        start_time = Sys.time()
        fn = paste(RootFolder, "gridsearch.csv", sep ="")
        if (file.opened(fn)) stop(paste(fn, " is open, pls close it first!"))
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
    
        MaxWeightPerName.Enabled = variables[variables$Variables=="MaxWeightPerName",]$Enabled
        if (MaxWeightPerName.Enabled) {
            MaxWeightPerName.Min = variables[variables$Variables=="MaxWeightPerName",]$Min
            MaxWeightPerName.Max = variables[variables$Variables=="MaxWeightPerName",]$Max
            MaxWeightPerName.Step = variables[variables$Variables=="MaxWeightPerName",]$Step
        } else {
            MaxWeightPerName.Min = MaxWeightPerName
            MaxWeightPerName.Max = MaxWeightPerName
            MaxWeightPerName.Step = 10000
        }  
        
        TH_RangeDUR.Enabled = variables[variables$Variables=="TH_RangeDUR",]$Enabled
        if (TH_RangeDUR.Enabled) {
            TH_RangeDUR.Min = variables[variables$Variables=="TH_RangeDUR",]$Min
            TH_RangeDUR.Max = variables[variables$Variables=="TH_RangeDUR",]$Max
            TH_RangeDUR.Step = variables[variables$Variables=="TH_RangeDUR",]$Step
        } else {
            # use user-defined range in "indices.csv"
            TH_RangeDUR.Min = 0
            TH_RangeDUR.Max = 0
            TH_RangeDUR.Step = 10000
        }
    
        TH_RangeASW.Enabled = variables[variables$Variables=="TH_RangeASW",]$Enabled
        if (TH_RangeASW.Enabled) {
            TH_RangeASW.Min = variables[variables$Variables=="TH_RangeASW",]$Min
            TH_RangeASW.Max = variables[variables$Variables=="TH_RangeASW",]$Max
            TH_RangeASW.Step = variables[variables$Variables=="TH_RangeASW",]$Step
        } else {
            # use user-defined range in "indices.csv"
            TH_RangeASW.Min = 0
            TH_RangeASW.Max = 0
            TH_RangeASW.Step = 10000
        }

        CS_MaxRange_Q.Enabled = variables[variables$Variables=="CS_MaxRange_Q",]$Enabled
        if (CS_MaxRange_Q.Enabled) {
            CS_MaxRange_Q.Min = variables[variables$Variables=="CS_MaxRange_Q",]$Min
            CS_MaxRange_Q.Max = variables[variables$Variables=="CS_MaxRange_Q",]$Max
            CS_MaxRange_Q.Step = variables[variables$Variables=="CS_MaxRange_Q",]$Step
        } else {
            CS_MaxRange_Q.Min = CS_MaxRange_Q
            CS_MaxRange_Q.Max = CS_MaxRange_Q
            CS_MaxRange_Q.Step = 10000
        }

        CS_MaxRange_SA.Enabled = variables[variables$Variables=="CS_MaxRange_SA",]$Enabled
        if (CS_MaxRange_SA.Enabled) {
            CS_MaxRange_SA.Min = variables[variables$Variables=="CS_MaxRange_SA",]$Min
            CS_MaxRange_SA.Max = variables[variables$Variables=="CS_MaxRange_SA",]$Max
            CS_MaxRange_SA.Step = variables[variables$Variables=="CS_MaxRange_SA",]$Step
        } else {
            CS_MaxRange_SA.Min = CS_MaxRange_SA
            CS_MaxRange_SA.Max = CS_MaxRange_SA
            CS_MaxRange_SA.Step = 10000
        }

        CS_AdjEntryDate_1S.Enabled = variables[variables$Variables=="CS_AdjEntryDate_1S",]$Enabled
        if (CS_AdjEntryDate_1S.Enabled) {
            CS_AdjEntryDate_1S.Min = variables[variables$Variables=="CS_AdjEntryDate_1S",]$Min
            CS_AdjEntryDate_1S.Max = variables[variables$Variables=="CS_AdjEntryDate_1S",]$Max
            CS_AdjEntryDate_1S.Step = variables[variables$Variables=="CS_AdjEntryDate_1S",]$Step
        } else {
            CS_AdjEntryDate_1S.Min = CS_AdjEntryDate_1S
            CS_AdjEntryDate_1S.Max = CS_AdjEntryDate_1S
            CS_AdjEntryDate_1S.Step = 10000
        }
        
        CS_AdjEntryDate_2S.Enabled = variables[variables$Variables=="CS_AdjEntryDate_2S",]$Enabled
        if (CS_AdjEntryDate_2S.Enabled) {
            CS_AdjEntryDate_2S.Min = variables[variables$Variables=="CS_AdjEntryDate_2S",]$Min
            CS_AdjEntryDate_2S.Max = variables[variables$Variables=="CS_AdjEntryDate_2S",]$Max
            CS_AdjEntryDate_2S.Step = variables[variables$Variables=="CS_AdjEntryDate_2S",]$Step
        } else {
            CS_AdjEntryDate_2S.Min = CS_AdjEntryDate_2S
            CS_AdjEntryDate_2S.Max = CS_AdjEntryDate_2S
            CS_AdjEntryDate_2S.Step = 10000
        }
        
        for (i in seq(CS_Opinion_NA.Min, CS_Opinion_NA.Max, CS_Opinion_NA.Step)) {
            for (j in seq(CS_TableOption.Min, CS_TableOption.Max, CS_TableOption.Step)) {
                for (k in seq(TH_MA.Min, TH_MA.Max, TH_MA.Step)) {
                    for (l in seq(TH_MinWght.Min, TH_MinWght.Max, TH_MinWght.Step)) {
                        for (m in seq(OutPerformMultiple.Min, OutPerformMultiple.Max, OutPerformMultiple.Step)) {
                            for (n in seq(MaxWeightPerName.Min, MaxWeightPerName.Max, MaxWeightPerName.Step)) {
                                for (o in seq(TH_RangeDUR.Min, TH_RangeDUR.Max, TH_RangeDUR.Step)) {
                                   for (p in seq(TH_RangeASW.Min, TH_RangeASW.Max, TH_RangeASW.Step)) {
                                       for (q in seq(CS_MaxRange_Q.Min, CS_MaxRange_Q.Max, CS_MaxRange_Q.Step)) {
                                           for (r in seq(CS_MaxRange_SA.Min, CS_MaxRange_SA.Max, CS_MaxRange_SA.Step)) {
                                               for (s in seq(CS_AdjEntryDate_1S.Min, CS_AdjEntryDate_1S.Max, CS_AdjEntryDate_1S.Step)) {
                                                   for (t in seq(CS_AdjEntryDate_2S.Min, CS_AdjEntryDate_2S.Max, CS_AdjEntryDate_2S.Step)) {
                                                        print (paste(i, j, k, l, m, n, o, p, q, r, s, t))
                                                        generateMetrics(RF, TH_NL, TH_UPM, CMPM, PWM, session, fn, TEST_VAL_METRIC, i, j, k, l, m, n, o, p, q, r, s, t)
                                                   }
                                               }
                                           }
                                       }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        end_time = Sys.time()
        time_took = as.numeric(difftime(end_time, start_time, units=("mins")))
        time_took = format(round(time_took, 1), nsmall = 1)
        result = paste("grid search completed, it took", time_took, "mins.")
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
processData <- function(RF, CS, TH, TH_NL, TH_UPM, CMP, CMPM, PWM, TEST_VAL_METRIC, CH, CH_OpinionDate, session) {
    
    RootFolder = RF
    CS_UseFile = CS
    TH_UseFile = TH
    ConstructModelPort = CMP
    # 1 = MV%,  2 = MV%_A,  3 = MV%_B, 4 = DtS, 5 = DtS Cash
    ConstructModelPortMethod = CMPM
    # 1 = index wght, 2 = equal wght, 3 = index wght adv, 4 = equal wght adv
    PortWeightMethod = PWM

    DEBUG = FALSE
    
    if (DEBUG) {
        RootFolder = "C:/MyProjects/Guru/BacktestR/"
        IndexDataFolder = "Bond raw data/Index constituents/"
        IndexLvlDataFolder = "Bond raw data/Index lvl data/"
        CS_UseFile = TRUE
        TH_UseFile = FALSE
        #TH_NoLimits = FALSE
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
    final = data.frame(matrix(ncol=0,nrow=0))
    
    tryCatchLog({
        
        start_time = Sys.time()
        if (CH) {
            fn = paste(RootFolder, "CurrentHoldings.csv", sep ="")
            if (file.opened(fn)) stop(paste(fn, " is open, pls close it first!"))
            if (file.exists(fn)) file.remove(fn)
            
            if (!TH_UseFile) {
                fn = paste(RootFolder, "CH_THRESHOLDS.csv", sep ="")
                if (file.opened(fn)) stop(paste(fn, " is open, pls close it first!"))
                if (file.exists(fn)) file.remove(fn)
            }

        }
        else {
            fn = paste(RootFolder, "BondMaster.csv", sep ="")
            if (file.opened(fn)) stop(paste(fn, " is open, pls close it first!"))
            if (file.exists(fn)) file.remove(fn)
            
            fn = paste(RootFolder, "monthlystats.csv", sep ="")
            if (file.opened(fn)) stop(paste(fn, " is open, pls close it first!"))
            if (file.exists(fn)) file.remove(fn)
            
            fn = paste(RootFolder, "metrics.csv", sep ="")
            if (file.opened(fn)) stop(paste(fn, " is open, pls close it first!"))
            if (file.exists(fn)) file.remove(fn)
            
            fn = paste(RootFolder, "TXAnalysis.csv", sep ="")
            if (file.opened(fn)) stop(paste(fn, " is open, pls close it first!"))
            if (file.exists(fn)) file.remove(fn)
            
            fn = paste(RootFolder, "NewIssues.csv", sep ="")
            if (file.opened(fn)) stop(paste(fn, " is open, pls close it first!"))
            if (file.exists(fn)) file.remove(fn)

            fn = paste(RootFolder, "MissingOpinions.csv", sep ="")
            if (file.opened(fn)) stop(paste(fn, " is open, pls close it first!"))
            if (file.exists(fn)) file.remove(fn)

            if (!TH_UseFile) {
                fn = paste(RootFolder, "THRESHOLDS.csv", sep ="")
                if (file.opened(fn)) stop(paste(fn, " is open, pls close it first!"))
                if (file.exists(fn)) file.remove(fn)
                
            }
            
            
        }
        
        init_var()
        
        ########## load translation and company scores files ##########
        status_msg = "loading translation file..."
        session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
        trans <<- loadtrans(RootFolder)
        
        status_msg = "calculating company opinions..."
        session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
        if (CS_UseFile) {
            scores = read.csv(paste(RootFolder, "CompanyScoresBondCalc.csv", sep = ""))
            if (grepl("-", scores$AdjDate[1]))
                scores$AdjDate = as.Date(scores$AdjDate, "%Y-%m-%d")
            else
                scores$AdjDate = as.Date(scores$AdjDate, "%m/%d/%Y")
        } else {
            fn = paste(RootFolder, "CompanyScoresBondCalc.csv", sep ="")
            if (file.opened(fn)) stop(paste(fn, " is open, pls close it first!"))
            if (file.exists(fn)) file.remove(fn)
            
            scores = scores2opinions(RootFolder, "BOND")
            # reformat Company ID back to its original format for export
            scores$ID_BB_UNIQUE = paste("EQ", str_pad(scores$BBERG_ID, 16, pad = "0"), sep = "")
            write.table(scores, file = paste(RootFolder, "CompanyScoresBondCalc.csv", sep =""), 
                        sep = ",", col.names = TRUE, row.names = FALSE)
        }
        # save only required columns to global variable: scores
        scores <<- scores[, c("BBERG_ID","TABLES","M.SCORE", "M.NA","T.SCORE","T.NA",
                            "L.SCORE","L.NA","M.FORMULA_11","AdjDate","M.OP")]

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
            
            ### very important, this next function figures out C.OP
            ds = indexop(Indices[j], ds, CH, CH_OpinionDate)
            
            # calculate thresholds
            status_msg = paste("calculating", Indices[j], "thresholds...")
            session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))

            if (TH_UseFile) {
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
                
                th = indexth(Indices[j], ds, TH_NL, TH_UPM)
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
            
            ds = computePortWght(ds, th, ConstructModelPortMethod, PortWeightMethod, TEST_VAL_METRIC)
            
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

        # adding opinion columns        
        final$Fundamentals = final$M.OP
        #move columns to the first and last positions
        final = final %>%
            select(-M11,-CA,-LD,everything())
        names(final)[names(final) == "M11"] = "Solvency"
        names(final)[names(final) == "CA"] = "Creative.Accounting"
        names(final)[names(final) == "LD"] = "Liquidity"
        final$Opinion = final$C.OP
        final$Positioning = final$F.OP
        
        # export data
        result_file = ""
        if (!CH) {
            result_file = "BondMaster.csv"
        } else {
            result_file = "CurrentHoldings.csv"
        }
        suppressWarnings(write.table(final, file = paste(RootFolder, result_file, sep =""), 
                                     sep = ",", col.names = TRUE, row.names = FALSE))

        if (!CH) {
            status_msg = paste("calculating final metrics...")
            session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
            
            final_index_month = computeMetrics(final, RootFolder, FALSE)
            #print(final_index_month)
            #print(Indices)
            write.table(final_index_month, file = paste(RootFolder, "monthlystats.csv", sep =""), 
                        sep = ",", col.names = TRUE, row.names = FALSE)
            
            fn = paste(RootFolder, "metrics.csv", sep = "")
            
            # add years to the periods
            #print(Periods)
            Years = lapply(Indices, indexyears, ds=final_index_month)
            Years = do.call(rbind,Years)
            Periods = rbind(Periods, Years)
            #print(Periods)
            
            for (i in 1:nrow(Periods)) {
                period = Periods[i,]
                writeMetrics(fn, final_index_month, 
                             as.character(period$Indices), 
                             as.character(period$Name), 
                             as.character(period$Start), 
                             as.character(period$End),
                             NA)
            }
            
            MissingOpinions(paste(RootFolder, "MissingOpinions.csv", sep =""), final)
        }
        else {
            #print(head(scores))
            # get last report quarter
            scores$REAL_QUARTER = paste(substr(scores$REAL_QUARTER, 3, 6), "Q",
                                        substr(scores$REAL_QUARTER, 1, 1), sep = "")
            lrq = group_by(scores, ID_BB_UNIQUE) %>%
                summarize(
                    Last.Reported.Quarter = max(REAL_QUARTER)
                )
            #print(head(lrq))
            
            #### export CurrentPositions.csv ####
            cp = final[,c("ID_BB_UNIQUE","REPORTING_FREQUENCY",
                          "Index.Name","Ticker","Cusip","ML.Industry.Lvl.3","ML.Industry.Lvl.4",
                          "ISO.Country","Rating","PrevMend.Mod.Dur.To.Worst","PrevMend.AssetSwp",
                          "Orig.PrevMend.Mkt...Index.Wght","ReWeighted","ReWeighted2",
                          "AdjDate","Fundamentals","Solvency","Creative.Accounting","Liquidity",
                          "Opinion","Positioning")]
            
            names(cp)[names(cp) == "PrevMend.Mod.Dur.To.Worst"] = "Duration"
            names(cp)[names(cp) == "PrevMend.AssetSwp"] = "AssetSwp"
            names(cp)[names(cp) == "Orig.PrevMend.Mkt...Index.Wght"] = "Bond.Index.Wght"
            names(cp)[names(cp) == "ReWeighted"] = "Init.Systematic.Wght"
            names(cp)[names(cp) == "ReWeighted2"] = "Final.Systematic.Wght"
            names(cp)[names(cp) == "AdjDate"] = "Latest.Opinion.Date"
            
            cp$IndexTicker = paste(cp$Index.Name, cp$Ticker)
            cp$Ticker.Index.Wght = ave(cp$Bond.Index.Wght, cp$IndexTicker, FUN=sum)
            cp$Ticker.Systematic.Wght = ave(cp$Final.Systematic.Wght, cp$IndexTicker, FUN=sum)
            
            cp$Fundamentals = ifelse(cp$Fundamentals==-3,"Limited Financials",
                                     ifelse(cp$Fundamentals==-1,"UnderPerform",
                                            ifelse(cp$Fundamentals==0,"Neutral",
                                                   ifelse(cp$Fundamentals==1,"OutPerform",
                                                          "Missing Data"))))
            cp$Solvency = ifelse(cp$Solvency==0,"Negative",
                                 ifelse(cp$Solvency==1,"Neutral","Missing Data"))
            cp$Creative.Accounting = ifelse(cp$Creative.Accounting==0,"Negative",
                                            ifelse(cp$Creative.Accounting==1,"Neutral","Missing Data"))
            cp$Liquidity = ifelse(cp$Liquidity==0,"Negative",
                                  ifelse(cp$Liquidity==1,"Neutral","Missing Data"))
            cp$Opinion = ifelse(cp$Opinion==-3,"Limited Financials",
                                     ifelse(cp$Opinion==-1,"UnderPerform",
                                            ifelse(cp$Opinion==0,"Neutral",
                                                   ifelse(cp$Opinion==1,"OutPerform",
                                                          ifelse(cp$Opinion==-2,"Worst",
                                                                 "Missing Data")))))
            cp$Positioning = ifelse(cp$Positioning==-2,"Short",
                                     ifelse(cp$Positioning==-1,"Sell",
                                            ifelse(cp$Positioning==0,"Hold",
                                                   ifelse(cp$Positioning==1,"Buy",
                                                          "Sell"))))
            cp = merge(cp, lrq[,c("ID_BB_UNIQUE","Last.Reported.Quarter")],
                       by.x = "ID_BB_UNIQUE", by.y = "ID_BB_UNIQUE", all.x = TRUE)
            
            cp$yr = as.numeric(substr(cp$Last.Reported.Quarter,1,4))
            cp$qtr = as.numeric(substr(cp$Last.Reported.Quarter,6,6))
            cp$REAL_QUARTER = ifelse(grepl("Q", cp$REPORTING_FREQUENCY),
                                     ifelse(cp$qtr==4,
                                            paste(as.character(cp$yr),"Q1",sep = ""),
                                            paste(as.character(cp$yr-1),"Q",as.character(cp$qtr+1),sep = "")),
                                     ifelse(cp$qtr==1,
                                            paste(as.character(cp$yr-1),"Q3",sep = ""),
                                            ifelse(cp$qtr==2,
                                                   paste(as.character(cp$yr-1),"Q4",sep = ""),
                                                   ifelse(cp$qtr==3,
                                                          paste(as.character(cp$yr),"Q1",sep = ""),
                                                          paste(as.character(cp$yr),"Q2",sep = "")))))
            cp$Expected.Upcoming.Quarter = paste(as.character(as.numeric(substr(cp$REAL_QUARTER,1,4))+1),
                                                 substr(cp$REAL_QUARTER,5,6),sep = "")

            cp = merge(cp, scores[,c("ID_BB_UNIQUE","REAL_QUARTER","AdjDate")],
                       by.x = c("ID_BB_UNIQUE","REAL_QUARTER"), by.y = c("ID_BB_UNIQUE","REAL_QUARTER"), all.x = TRUE)
            
            cp$Expected.Earning.Date = as.POSIXlt(cp$AdjDate)
            cp$Expected.Earning.Date$year = cp$Expected.Earning.Date$year + 1
            cp$Expected.Earning.Date = as.Date(cp$Expected.Earning.Date)
            cp = cp[,c("Index.Name","Ticker","Cusip","ML.Industry.Lvl.3","ML.Industry.Lvl.4",
                       "ISO.Country","Rating","Duration","AssetSwp",
                       "Bond.Index.Wght","Ticker.Index.Wght","Init.Systematic.Wght","Final.Systematic.Wght","Ticker.Systematic.Wght",
                       "Latest.Opinion.Date","Fundamentals","Solvency","Creative.Accounting","Liquidity",
                       "Opinion","Positioning",
                       "ID_BB_UNIQUE","REPORTING_FREQUENCY","Last.Reported.Quarter","Expected.Earning.Date","Expected.Upcoming.Quarter")]
            suppressWarnings(write.table(cp, file = paste(RootFolder, "CurrentPositions.csv", sep =""), 
                                         sep = ",", col.names = TRUE, row.names = FALSE))

            #### export CurrentPositions_IndustryExposure.csv ####
            final$Multiple = ifelse(final$Opinion==1,OutPerformMultiple,
                                     ifelse(final$Opinion==0,1,0))
            
            #opinion industry is estimated
            ie_lvl3 = group_by(final, Index.Name, ML.Industry.Lvl.3) %>%
                summarize(
                    obs = n(),
                    Index = sum(DtS * Orig.PrevMend.Mkt...Index.Wght),
                    Opinion = sum(DtS * Orig.PrevMend.Mkt...Index.Wght * Multiple),
                    Positioning = sum(DtS * ReWeighted2)
                )
            ie_lvl3$Index_total = ave(ie_lvl3$Index,ie_lvl3$Index.Name, FUN=sum)
            ie_lvl3$Opinion_total = ave(ie_lvl3$Opinion,ie_lvl3$Index.Name, FUN=sum)
            ie_lvl3$Positioning_total = ave(ie_lvl3$Positioning,ie_lvl3$Index.Name, FUN=sum)
            ie_lvl3$Index_pct = ie_lvl3$Index / ie_lvl3$Index_total
            ie_lvl3$Opinion_pct = ie_lvl3$Opinion / ie_lvl3$Opinion_total
            ie_lvl3$Positioning_pct = ie_lvl3$Positioning / ie_lvl3$Positioning_total
            ie_lvl3$Opinion_rel = ie_lvl3$Opinion_pct - ie_lvl3$Index_pct
            ie_lvl3$Positioning_rel = ie_lvl3$Positioning_pct - ie_lvl3$Index_pct
            ie_lvl3 = ie_lvl3[,c("Index.Name","ML.Industry.Lvl.3","Index_pct",
                                 "Positioning_pct","Positioning_rel",
                                 "Opinion_pct","Opinion_rel")]
            suppressWarnings(write.table(ie_lvl3, file = paste(RootFolder, "CurrentPositions_IndLvl3.csv", sep =""), 
                                         sep = ",", col.names = TRUE, row.names = FALSE))

            #opinion industry is estimated
            ie_lvl4 = group_by(final, Index.Name, ML.Industry.Lvl.4) %>%
                summarize(
                    obs = n(),
                    Index = sum(DtS * Orig.PrevMend.Mkt...Index.Wght),
                    Opinion = sum(DtS * Orig.PrevMend.Mkt...Index.Wght * Multiple),
                    Positioning = sum(DtS * ReWeighted2)
                )
            ie_lvl4$Index_total = ave(ie_lvl4$Index,ie_lvl4$Index.Name, FUN=sum)
            ie_lvl4$Opinion_total = ave(ie_lvl4$Opinion,ie_lvl4$Index.Name, FUN=sum)
            ie_lvl4$Positioning_total = ave(ie_lvl4$Positioning,ie_lvl4$Index.Name, FUN=sum)
            ie_lvl4$Index_pct = ie_lvl4$Index / ie_lvl4$Index_total
            ie_lvl4$Opinion_pct = ie_lvl4$Opinion / ie_lvl4$Opinion_total
            ie_lvl4$Positioning_pct = ie_lvl4$Positioning / ie_lvl4$Positioning_total
            ie_lvl4$Opinion_rel = ie_lvl4$Opinion_pct - ie_lvl4$Index_pct
            ie_lvl4$Positioning_rel = ie_lvl4$Positioning_pct - ie_lvl4$Index_pct
            ie_lvl4 = ie_lvl4[,c("Index.Name","ML.Industry.Lvl.4","Index_pct",
                                 "Positioning_pct","Positioning_rel",
                                 "Opinion_pct","Opinion_rel")]
            suppressWarnings(write.table(ie_lvl4, file = paste(RootFolder, "CurrentPositions_IndLvl4.csv", sep =""), 
                                         sep = ",", col.names = TRUE, row.names = FALSE))
            
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

# generate metrics given input parameters
generateMetrics <- function(RF, TH_NL, TH_UPM, CMPM, PWM, session, fn, TEST_VAL_METRIC,
                            Opinion.NA, TableOption, MA, MinWght, 
                            Multiple, MaxWeight, RangeDUR, RangeASW, 
                            MaxRange_Q, MaxRange_SA, AdjEntryDate_1S, AdjEntryDate_2S) {
    
    
    RootFolder = RF
    # 1 = MV%,  2 = MV%_A,  3 = MV%_B, 4 = DtS, 5 = DtS Cash
    ConstructModelPortMethod = CMPM
    # 1 = index wght, 2 = equal wght, 3 = index wght adv, 4 = equal wght adv
    PortWeightMethod = PWM
    
    
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
        MaxWeightPerName <<- MaxWeight
        TH_RangeDUR <<- RangeDUR
        TH_RangeASW <<- RangeASW
        
        CS_MaxRange_Q <<- MaxRange_Q
        CS_MaxRange_SA <<- MaxRange_SA
        CS_AdjEntryDate_123Q <<- AdjEntryDate_1S
        CS_AdjEntryDate_1S <<- AdjEntryDate_1S
        CS_AdjEntryDate_4Q <<- AdjEntryDate_2S
        CS_AdjEntryDate_2S <<- AdjEntryDate_2S
        
        gs = paste(Opinion.NA, TableOption, MA, MinWght, 
                   Multiple, MaxWeight, RangeDUR, RangeASW, 
                   MaxRange_Q, MaxRange_SA, AdjEntryDate_1S, AdjEntryDate_2S, 
                   sep = "-")
        
        
        status_msg = "calculating company opinions..."
        status_msg = paste(gs, status_msg, sep = "-")
        session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
        scores <<- scores2opinions(RootFolder, "BOND")
        # get rid of -3 for limited finanical handling
        #scores <<- subset(scores, scores$M.OP!=-3)
        
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
            th = indexth(Indices[j], ds, TH_NL, TH_UPM)
            
            ########## MV%/DtS/DtS Cash ##########
            status_msg = paste("calculating portfolio weight for", Indices[j], "...")
            status_msg = paste(gs, status_msg, sep = "-")
            session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
            
            ds = computePortWght(ds, th, ConstructModelPortMethod, PortWeightMethod, TEST_VAL_METRIC)
            
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
        
        final_index_month = computeMetrics(final, RootFolder, TRUE)
        
        gs_params = setNames(data.frame(matrix(ncol = 12, nrow = 0)), 
                             c("CS_Opinion_NA", "CS_TableOption", "TH_MA", "TH_MinWght", "Multiple", 
                               "MaxWeight", "TH_RangeDUR", "TH_RangeASW", 
                               "CS_MaxRange_Q", "CS_MaxRange_SA", "CS_AdjEntryDate_1S", "CS_AdjEntryDate_2S"))
        gs_params[1,] = list(Opinion.NA,TableOption, MA, MinWght, 
                             Multiple, MaxWeight, RangeDUR, RangeASW, 
                             MaxRange_Q, MaxRange_SA, AdjEntryDate_1S, AdjEntryDate_2S)
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

# save CompanyScoresEquityCalc.csv to root folder
generateEquityOpinion <- function(RF, session) {
    
    RootFolder = RF
    DEBUG = FALSE
    
    if (DEBUG) {
        RootFolder = "C:/MyProjects/Guru/BacktestR/"
    }
    
    result = ""
    status_msg = ""
    status_obj = "status"

    tryCatchLog({
        
        start_time = Sys.time()
        
        fn = paste(RootFolder, "CompanyScoresEquityCalc.csv", sep ="")
        if (file.opened(fn)) stop(paste(fn, " is open, pls close it first!"))
        if (file.exists(fn)) file.remove(fn)
        
        init_var()
        
        ########## load translation and company scores files ##########
        status_msg = "loading translation file..."
        session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
        transequity <<- loadtransequity(RootFolder)
        
        status_msg = "calculating company opinions..."
        session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
        
        equity_scores = scores2opinions(RootFolder, "EQUITY")
        
        # Solvency
        equity_scores$M11 = as.integer(ifelse(equity_scores$M.FORMULA_11<1,0,1))
        # creative accounting
        equity_scores$CA = as.integer(ifelse(equity_scores$T.SCORE<TEST_CA_SCORE & equity_scores$T.NA<TEST_CA_NA, 0, 1))
        # liqudity
        equity_scores$LD = as.integer(ifelse(equity_scores$L.SCORE<TEST_LD_SCORE & equity_scores$L.NA<TEST_LD_NA, 0, 1))
        
        equity_scores$C.OP = ifelse(equity_scores$M.OP==-4 | equity_scores$M.OP==-3,equity_scores$M.OP,-1)

        for(i in 1:nrow(PortRules)) {
            #print(paste(PortRules$M.OP[i], PortRules$M11[i], PortRules$CA[i], PortRules$LD[i]))
            if (PortRules$C.OP[i]=="Worst") {
                OP=-2
            } else if  (PortRules$C.OP[i]=="UnderPerform") {
                OP=-1
            } else if  (PortRules$C.OP[i]=="Neutral") {
                OP=0
            } else if  (PortRules$C.OP[i]=="OutPerform") {
                OP=1
            }
            
            equity_scores$C.OP = ifelse(equity_scores$M.OP==PortRules$M.OP[i] 
                             & equity_scores$M11==PortRules$M11[i] 
                             & equity_scores$CA==PortRules$CA[i] 
                             & equity_scores$LD==PortRules$LD[i],OP,equity_scores$C.OP)
            
        }
        
        # reformat Company ID back to its original format for export
        equity_scores$ID_BB_UNIQUE = paste("EQ", str_pad(equity_scores$BBERG_ID, 16, pad = "0"), sep = "")
        
        # remove columns not needed for export
        equity_scores = equity_scores[, !(colnames(equity_scores) %in% c("BBERG_ID", "M.FORMULA_11"))]
        
        write.table(equity_scores, file = fn, sep = ",", col.names = TRUE, row.names = FALSE)
        
        result_file = "CompanyScoresEquityCalc.csv"
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

# check data for missing data and translation
checkData <- function(RF, session) {

    RootFolder = RF
    DEBUG = FALSE

    if (DEBUG) {
        RootFolder = "C:/MyProjects/Guru/BacktestR/"
    }
    
    result = ""
    status_msg = ""
    status_obj = "DC_status"
    final = data.frame(matrix(ncol=0,nrow=0))
    
    tryCatchLog({
        
        start_time = Sys.time()
        
        fn = paste(RootFolder, "MissingRawData.csv", sep ="")
        if (file.opened(fn)) stop(paste(fn, " is open, pls close it first!"))
        if (file.exists(fn)) file.remove(fn)
        
        fn = paste(RootFolder, "NewCompanies.csv", sep ="")
        if (file.opened(fn)) stop(paste(fn, " is open, pls close it first!"))
        if (file.exists(fn)) file.remove(fn)

        fn = paste(RootFolder, "MissingTranslation.csv", sep ="")
        if (file.opened(fn)) stop(paste(fn, " is open, pls close it first!"))
        if (file.exists(fn)) file.remove(fn)
        
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
        write.table(missing_months, file = paste(RootFolder, "MissingRawData.csv", sep =""), sep = ",", col.names = TRUE, row.names = FALSE)

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
            
            if (nrow(ds)==0) {
                # create an empty final dataframe with same structure of output
                final = ds[0,]
            }
            final=rbind(final, ds)
            
            # new tickers
            nt = new_tickers(Indices[j], RootFolder, IndexDataFolder, ds)
            
            if (j==1) {
                write.table(nt, file = paste(RootFolder, "NewCompanies.csv", sep =""), sep = ",", col.names = TRUE, row.names = FALSE)
            }
            else {
                write.table(nt, file = paste(RootFolder, "NewCompanies.csv", sep =""), sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
            }
            
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
        
        
        result_file = "MissingRawData.csv, MissingTranslation.csv, NewCompanies.csv"
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

# run additional tests to determine port rules an port weights
addlTests <- function(RF, session, TEST_VAL_METRIC) {
    
    
    RootFolder = RF
    DEBUG = FALSE
    
    if (DEBUG) {
        RootFolder = "C:/MyProjects/Guru/BacktestR/"
    }
    
    result = ""
    status_msg = ""
    status_obj = "TEST_status"
    

    
    start_time = Sys.time()
    
    init_var()
    
    ########## load bond master files ##########
    status_msg = "loading bond master file..."
    session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
    
    fn = paste(RootFolder, "addltests.csv", sep ="")
    if (file.opened(fn)) stop(paste(fn, " is open, pls close it first!"))
    if (file.exists(fn)) file.remove(fn)

    fn_rawdata = paste(RootFolder, "addltests_rawdata.csv", sep ="")
    if (file.opened(fn_rawdata)) stop(paste(fn_rawdata, " is open, pls close it first!"))
    if (file.exists(fn_rawdata)) file.remove(fn_rawdata)

    fn_riskret_dxs = paste(RootFolder, "addltests_riskret_dxs.csv", sep ="")
    if (file.opened(fn_riskret_dxs)) stop(paste(fn_riskret_dxs, " is open, pls close it first!"))
    if (file.exists(fn_riskret_dxs)) file.remove(fn_riskret_dxs)
    
    fn_riskret_op_dxs = paste(RootFolder, "addltests_riskret_op_dxs.csv", sep ="")
    if (file.opened(fn_riskret_op_dxs)) stop(paste(fn_riskret_op_dxs, " is open, pls close it first!"))
    if (file.exists(fn_riskret_op_dxs)) file.remove(fn_riskret_op_dxs)

    fn_riskret_val = paste(RootFolder, "addltests_riskret_val.csv", sep ="")
    if (file.opened(fn_riskret_val)) stop(paste(fn_riskret_val, " is open, pls close it first!"))
    if (file.exists(fn_riskret_val)) file.remove(fn_riskret_val)
    
    fn_riskret_op_val = paste(RootFolder, "addltests_riskret_op_val.csv", sep ="")
    if (file.opened(fn_riskret_op_val)) stop(paste(fn_riskret_op_val, " is open, pls close it first!"))
    if (file.exists(fn_riskret_op_val)) file.remove(fn_riskret_op_val)

    fn_riskret_vnd = paste(RootFolder, "addltests_riskret_vnd.csv", sep ="")
    if (file.opened(fn_riskret_vnd)) stop(paste(fn_riskret_vnd, " is open, pls close it first!"))
    if (file.exists(fn_riskret_vnd)) file.remove(fn_riskret_vnd)
    
    fn_riskret_op_vnd = paste(RootFolder, "addltests_riskret_op_vnd.csv", sep ="")
    if (file.opened(fn_riskret_op_vnd)) stop(paste(fn_riskret_op_vnd, " is open, pls close it first!"))
    if (file.exists(fn_riskret_op_vnd)) file.remove(fn_riskret_op_vnd)
    
    G0O1 = indexlvldata(RootFolder, IndexLvlDataFolder)
    G0O1 = subset(G0O1, G0O1$Index=="G0O1")
    
    final = read.csv(paste(RootFolder, "BondMaster.csv", sep = ""))
    #final$DxS = final$PrevMend.Mod.Dur.To.Worst*final$PrevMend.AssetSwp
    # creative accounting
    final$CA = ifelse(final$T.SCORE<TEST_CA_SCORE & final$T.NA<TEST_CA_NA, 0, 1)
    # liqudity
    final$LD = ifelse(final$L.SCORE<TEST_LD_SCORE & final$L.NA<TEST_LD_NA, 0, 1)

    ########## running tests ##########
    status_msg = "running tests..."
    session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
    
    # Index
    final$port = 1
    opTest("Index", final, G0O1, fn)
    
    # op=1 -------------------------------------------------------------------------------
    final$port = ifelse(final$M.OP==1, 1,0)
    opTest("M.OP=1", final, G0O1, fn)
    
    # op=1 | M11=1 | CA=1 | LD=1
    final$port = ifelse(final$M.OP==1 & final$M.FORMULA_11==1 & final$CA==1 & final$LD==1, 1, 0)
    opTest("M.OP=1|M11=1|CA=1|LD=1", final, G0O1, fn)
    
    # op=1 | M11=1 | CA=1 | LD=0
    final$port = ifelse(final$M.OP==1 & final$M.FORMULA_11==1 & final$CA==1 & final$LD==0, 1, 0)
    opTest("M.OP=1|M11=1|CA=1|LD=0", final, G0O1, fn)
    
    # op=1 | M11=1 | CA=0 | LD=1
    final$port = ifelse(final$M.OP==1 & final$M.FORMULA_11==1 & final$CA==0 & final$LD==1, 1, 0)
    opTest("M.OP=1|M11=1|CA=0|LD=1", final, G0O1, fn)
    
    # op=1 | M11=1 | CA=0 | LD=0
    final$port = ifelse(final$M.OP==1 & final$M.FORMULA_11==1 & final$CA==0 & final$LD==0, 1, 0)
    opTest("M.OP=1|M11=1|CA=0|LD=0", final, G0O1, fn)
    
    # op=1 | M11=0 | CA=1 | LD=1
    final$port = ifelse(final$M.OP==1 & final$M.FORMULA_11<1 & final$CA==1 & final$LD==1, 1, 0)
    opTest("M.OP=1|M11=0|CA=1|LD=1", final, G0O1, fn)
    
    # op=1 | M11=0 | CA=1 | LD=0
    final$port = ifelse(final$M.OP==1 & final$M.FORMULA_11<1 & final$CA==1 & final$LD==0, 1, 0)
    opTest("M.OP=1|M11=0|CA=1|LD=0", final, G0O1, fn)
    
    # op=1 | M11=0 | CA=0 | LD=1
    final$port = ifelse(final$M.OP==1 & final$M.FORMULA_11<1 & final$CA==0 & final$LD==1, 1, 0)
    opTest("M.OP=1|M11=0|CA=0|LD=1", final, G0O1, fn)
    
    # op=1 | M11=0 | CA=0 | LD=0
    final$port = ifelse(final$M.OP==1 & final$M.FORMULA_11<1 & final$CA==0 & final$LD==0, 1, 0)
    opTest("M.OP=1|M11=0|CA=0|LD=0", final, G0O1, fn)
    
    # op=0 -------------------------------------------------------------------------------
    final$port = ifelse(final$M.OP==0, 1, 0)
    opTest("M.OP=0", final, G0O1, fn)
    
    # op=0 | M11=1 | CA=1 | LD=1
    final$port = ifelse(final$M.OP==0 & final$M.FORMULA_11==1 & final$CA==1 & final$LD==1, 1, 0)
    opTest("M.OP=0|M11=1|CA=1|LD=1", final, G0O1, fn)

    # op=0 | M11=1 | CA=1 | LD=0
    final$port = ifelse(final$M.OP==0 & final$M.FORMULA_11==1 & final$CA==1 & final$LD==0, 1, 0)
    opTest("M.OP=0|M11=1|CA=1|LD=0", final, G0O1, fn)
    
    # op=0 | M11=1 | CA=0 | LD=1
    final$port = ifelse(final$M.OP==0 & final$M.FORMULA_11==1 & final$CA==0 & final$LD==1, 1, 0)
    opTest("M.OP=0|M11=1|CA=0|LD=1", final, G0O1, fn)
    
    # op=0 | M11=1 | CA=0 | LD=0
    final$port = ifelse(final$M.OP==0 & final$M.FORMULA_11==1 & final$CA==0 & final$LD==0, 1, 0)
    opTest("M.OP=0|M11=1|CA=0|LD=0", final, G0O1, fn)

    # op=0 | M11=0 | CA=1 | LD=1
    final$port = ifelse(final$M.OP==0 & final$M.FORMULA_11<1 & final$CA==1 & final$LD==1, 1, 0)
    opTest("M.OP=0|M11=0|CA=1|LD=1", final, G0O1, fn)
    
    # op=0 | M11=0 | CA=1 | LD=0
    final$port = ifelse(final$M.OP==0 & final$M.FORMULA_11<1 & final$CA==1 & final$LD==0, 1, 0)
    opTest("M.OP=0|M11=0|CA=1|LD=0", final, G0O1, fn)
    
    # op=0 | M11=0 | CA=0 | LD=1
    final$port = ifelse(final$M.OP==0 & final$M.FORMULA_11<1 & final$CA==0 & final$LD==1, 1, 0)
    opTest("M.OP=0|M11=0|CA=0|LD=1", final, G0O1, fn)
    
    # op=0 | M11=0 | CA=0 | LD=0
    final$port = ifelse(final$M.OP==0 & final$M.FORMULA_11<1 & final$CA==0 & final$LD==0, 1, 0)
    opTest("M.OP=0|M11=0|CA=0|LD=0", final, G0O1, fn)
    
    # op=-1 -------------------------------------------------------------------------------
    final$port = ifelse(final$M.OP==-1, 1,0)
    opTest("M.OP=-1", final, G0O1, fn)
    
    # op=-1 | M11=1 | CA=1 | LD=1
    final$port = ifelse(final$M.OP==-1 & final$M.FORMULA_11==1 & final$CA==1 & final$LD==1, 1, 0)
    opTest("M.OP=-1|M11=1|CA=1|LD=1", final, G0O1, fn)
    
    # op=-1 | M11=1 | CA=1 | LD=0
    final$port = ifelse(final$M.OP==-1 & final$M.FORMULA_11==1 & final$CA==1 & final$LD==0, 1, 0)
    opTest("M.OP=-1|M11=1|CA=1|LD=0", final, G0O1, fn)
    
    # op=-1 | M11=1 | CA=0 | LD=1
    final$port = ifelse(final$M.OP==-1 & final$M.FORMULA_11==1 & final$CA==0 & final$LD==1, 1, 0)
    opTest("M.OP=-1|M11=1|CA=0|LD=1", final, G0O1, fn)
    
    # op=-1 | M11=1 | CA=0 | LD=0
    final$port = ifelse(final$M.OP==-1 & final$M.FORMULA_11==1 & final$CA==0 & final$LD==0, 1, 0)
    opTest("M.OP=-1|M11=1|CA=0|LD=0", final, G0O1, fn)

    # op=-1 | M11=0 | CA=1 | LD=1
    final$port = ifelse(final$M.OP==-1 & final$M.FORMULA_11<1 & final$CA==1 & final$LD==1, 1, 0)
    opTest("M.OP=-1|M11=0|CA=1|LD=1", final, G0O1, fn)
    
    # op=-1 | M11=0 | CA=1 | LD=0
    final$port = ifelse(final$M.OP==-1 & final$M.FORMULA_11<1 & final$CA==1 & final$LD==0, 1, 0)
    opTest("M.OP=-1|M11=0|CA=1|LD=0", final, G0O1, fn)
    
    # op=-1 | M11=0 | CA=0 | LD=1
    final$port = ifelse(final$M.OP==-1 & final$M.FORMULA_11<1 & final$CA==0 & final$LD==1, 1, 0)
    opTest("M.OP=-1|M11=0|CA=0|LD=1", final, G0O1, fn)
    
    # op=-1 | M11=0 | CA=0 | LD=0
    final$port = ifelse(final$M.OP==-1 & final$M.FORMULA_11<1 & final$CA==0 & final$LD==0, 1, 0)
    opTest("M.OP=-1|M11=0|CA=0|LD=0", final, G0O1, fn)
    
            
    # save raw data file
    final = final[,c("Index.Name","As.of.Date","M.OP","M.FORMULA_11","CA","LD",
                     "PrevMend.Mod.Dur.To.Worst","PrevMend.AssetSwp","DtS",
                     "Rating","ML.Industry.Lvl.3","Orig.PrevMend.Mkt...Index.Wght","C.OP",
                     "TRR...MTD.LOC","Excess.Rtn...MTD")]
    # add DtS buckets
    final = addDtSBucket(final)
    final = final[, c("Index.Name","As.of.Date","M.OP","M.FORMULA_11","CA","LD",
                      "PrevMend.Mod.Dur.To.Worst","PrevMend.AssetSwp","DtS",
                      "Rating","ML.Industry.Lvl.3","Orig.PrevMend.Mkt...Index.Wght","C.OP",
                      "TRR...MTD.LOC","Excess.Rtn...MTD","DtSBucket.Name","DtSBucket.Label")]
    
    # add valuation
    final = addValuation(final, TEST_VAL_METRIC)
    
    # now do val and dts combo
    final$VnD = paste(final$Val, final$DtSBucket.Name)

    # get index level stats to prep for rel tests
    final$IndexDate = paste(final$Index.Name, final$As.of.Date)
    final_month = group_by(final, IndexDate) %>% 
        summarize(
            idx_trr = sum(TRR...MTD.LOC*Orig.PrevMend.Mkt...Index.Wght),
            idx_exr = sum(Excess.Rtn...MTD*Orig.PrevMend.Mkt...Index.Wght),
            idx_wght = sum(Orig.PrevMend.Mkt...Index.Wght)
        )
    final_month$idx_trr = ifelse(final_month$idx_wght==0,0,final_month$idx_trr/final_month$idx_wght)
    final_month$idx_exr = ifelse(final_month$idx_wght==0,0,final_month$idx_exr/final_month$idx_wght)
    
    final = merge(final, final_month[ , c("IndexDate", "idx_trr", "idx_exr")], 
                  by.x="IndexDate", by.y="IndexDate", all.x=TRUE)

    final$trr_rel = final$TRR...MTD.LOC - final$idx_trr
    final$exr_rel = final$Excess.Rtn...MTD - final$idx_exr
    
    final$trrdn = ifelse(final$TRR...MTD.LOC>0,0,final$TRR...MTD.LOC)
    final$exrdn = ifelse(final$Excess.Rtn...MTD>0,0,final$Excess.Rtn...MTD)
    
    final$trr_reldn = ifelse(final$trr_rel>0,0,final$trr_rel)
    final$exr_reldn = ifelse(final$exr_rel>0,0,final$exr_rel)
    
    # run relative tests
    relTestDxS(final, fn_riskret_dxs, fn_riskret_op_dxs)
    relTestVal(final, fn_riskret_val, fn_riskret_op_val)
    relTestVnD(final, fn_riskret_vnd, fn_riskret_op_vnd)
    
    final = final[,c("Index.Name","As.of.Date","M.OP","M.FORMULA_11","CA","LD",
                    "PrevMend.Mod.Dur.To.Worst","PrevMend.AssetSwp","DtS",
                    "Rating","ML.Industry.Lvl.3","Orig.PrevMend.Mkt...Index.Wght","C.OP",
                    "DtSBucket.Name","DtSBucket.Label","Val",
                    "idx_trr", "idx_exr",
                    "TRR...MTD.LOC",
                    "Excess.Rtn...MTD")]

    final = final[order(final$Index.Name, final$C.OP, final$DtSBucket.Name),]
    write.table(final, file = fn_rawdata, sep = ",", col.names = TRUE, row.names = FALSE)
    
    result_file = "addltests.csv, addltests_rawdata.csv"
    end_time = Sys.time()
    time_took = as.numeric(difftime(end_time, start_time, units=("mins")))
    time_took = format(round(time_took, 1), nsmall = 1)
    result = paste(result_file, "generated successfully and took", time_took, "mins.")
    
}

# DxS: this function calc relative trr / exr, their avg, downdev and ratio
relTestDxS <- function(final, fn_riskret_dxs, fn_riskret_op_dxs) {
    final$IndexDxS = paste(final$Index.Name, final$DtSBucket.Name)
    final$IndexOpDxS = paste(final$Index.Name, final$C.OP, final$DtSBucket.Name)

    # metrics by index
    # total_obs is the observations by index
    final_index = group_by(final, Index.Name) %>% 
        summarize(
            total_obs = n()
        )
    
    # metrics by DxS bucket
    final_dxs = group_by(final, Index.Name, IndexDxS) %>% 
        summarize(
            obs = n(),
            idx_trr_avg = mean(TRR...MTD.LOC),
            idx_trr_dndev = sqrt(sum(trrdn*trrdn)/n()),
            idx_exr_avg = mean(Excess.Rtn...MTD),
            idx_exr_dndev = sqrt(sum(exrdn*exrdn)/n())
        )
    
    final_dxs = merge(final_dxs, final_index, 
                         by.x="Index.Name", by.y="Index.Name", all.x=TRUE)
    final_dxs$pct_obs = final_dxs$obs / final_dxs$total_obs
    final_dxs$idx_trr_ratio = final_dxs$idx_trr_avg/final_dxs$idx_trr_dndev
    final_dxs$idx_exr_ratio = final_dxs$idx_exr_avg/final_dxs$idx_exr_dndev
    final_dxs$IndexDxSCopy = final_dxs$IndexDxS
    final_dxs = within(final_dxs, IndexDxSCopy<-data.frame(do.call('rbind', strsplit(as.character(IndexDxSCopy), ' ', fixed=TRUE))))
    # remove unused columns
    final_dxs = final_dxs[, !(colnames(final_dxs) %in% c("Index.Name"))]    
    
    write.table(final_dxs, file = fn_riskret_dxs, sep = ",", col.names = TRUE, row.names = FALSE)
    
    # remove unused columns
    final_dxs = final_dxs[, !(colnames(final_dxs) %in% c("total_obs","obs","pct_obs","idx_trr_ratio","idx_exr_ratio"))]    
    
    # metrics by opinion by DxS bucket
    final_op_dxs = group_by(final, Index.Name, IndexDxS, IndexOpDxS) %>% 
        summarize(
            obs = n(),
            trr_avg = mean(TRR...MTD.LOC),
            trr_dndev = sqrt(sum(trrdn*trrdn)/n()),
            exr_avg = mean(Excess.Rtn...MTD),
            exr_dndev = sqrt(sum(exrdn*exrdn)/n()),
            trr_rel_avg = mean(trr_rel),
            trr_rel_dndev = sqrt(sum(trr_reldn*trr_reldn)/n()),
            exr_rel_avg = mean(exr_rel),
            exr_rel_dndev = sqrt(sum(exr_reldn*exr_reldn)/n())
            
        )
    
    final_op_dxs = merge(final_op_dxs, final_index, 
                         by.x="Index.Name", by.y="Index.Name", all.x=TRUE)
    final_op_dxs$pct_obs = final_op_dxs$obs / final_op_dxs$total_obs
    
    final_op_dxs = merge(final_op_dxs, final_dxs, 
                  by.x="IndexDxS", by.y="IndexDxS", all.x=TRUE)
    
    final_op_dxs$idx_trr_ratio = final_op_dxs$idx_trr_avg/final_op_dxs$idx_trr_dndev
    final_op_dxs$idx_exr_ratio = final_op_dxs$idx_exr_avg/final_op_dxs$idx_exr_dndev
    final_op_dxs$trr_ratio = final_op_dxs$trr_avg/final_op_dxs$trr_dndev
    final_op_dxs$exr_ratio = final_op_dxs$exr_avg/final_op_dxs$exr_dndev
    final_op_dxs$trr_rel_ratio = final_op_dxs$trr_rel_avg/final_op_dxs$trr_rel_dndev
    final_op_dxs$exr_rel_ratio = final_op_dxs$exr_rel_avg/final_op_dxs$exr_rel_dndev
    
    final_op_dxs$IndexOpDxSCopy = final_op_dxs$IndexOpDxS
    final_op_dxs = within(final_op_dxs, IndexOpDxSCopy<-data.frame(do.call('rbind', strsplit(as.character(IndexOpDxSCopy), ' ', fixed=TRUE))))
    # remove unused columns
    final_op_dxs = final_op_dxs[, !(colnames(final_op_dxs) %in% c("Index.Name","IndexDxS"))]    
    
    write.table(final_op_dxs, file = fn_riskret_op_dxs, sep = ",", col.names = TRUE, row.names = FALSE)
    
    return(final)
}

# Val: this function calc relative trr / exr, their avg, downdev and ratio
relTestVal <- function(final, fn_riskret_val, fn_riskret_op_val) {
    final$IndexVal = paste(final$Index.Name, final$Val)
    final$IndexOpVal = paste(final$Index.Name, final$C.OP, final$Val)
    
    # metrics by index
    # total_obs is the observations by index
    final_index = group_by(final, Index.Name) %>% 
        summarize(
            total_obs = n()
        )
    
    # metrics by Val bucket
    final_Val = group_by(final, Index.Name, IndexVal) %>% 
        summarize(
            obs = n(),
            idx_trr_avg = mean(TRR...MTD.LOC),
            idx_trr_dndev = sqrt(sum(trrdn*trrdn)/n()),
            idx_exr_avg = mean(Excess.Rtn...MTD),
            idx_exr_dndev = sqrt(sum(exrdn*exrdn)/n())
        )
    
    final_Val = merge(final_Val, final_index, 
                      by.x="Index.Name", by.y="Index.Name", all.x=TRUE)
    final_Val$pct_obs = final_Val$obs / final_Val$total_obs
    final_Val$idx_trr_ratio = final_Val$idx_trr_avg/final_Val$idx_trr_dndev
    final_Val$idx_exr_ratio = final_Val$idx_exr_avg/final_Val$idx_exr_dndev
    final_Val$IndexValCopy = final_Val$IndexVal
    final_Val = within(final_Val, IndexValCopy<-data.frame(do.call('rbind', strsplit(as.character(IndexValCopy), ' ', fixed=TRUE))))
    # remove unused columns
    final_Val = final_Val[, !(colnames(final_Val) %in% c("Index.Name"))]    
    
    write.table(final_Val, file = fn_riskret_val, sep = ",", col.names = TRUE, row.names = FALSE)
    
    # remove unused columns
    final_Val = final_Val[, !(colnames(final_Val) %in% c("total_obs","obs","pct_obs","idx_trr_ratio","idx_exr_ratio"))]    
    
    # metrics by opinion by Val bucket
    final_op_Val = group_by(final, Index.Name, IndexVal, IndexOpVal) %>% 
        summarize(
            obs = n(),
            trr_avg = mean(TRR...MTD.LOC),
            trr_dndev = sqrt(sum(trrdn*trrdn)/n()),
            exr_avg = mean(Excess.Rtn...MTD),
            exr_dndev = sqrt(sum(exrdn*exrdn)/n()),
            trr_rel_avg = mean(trr_rel),
            trr_rel_dndev = sqrt(sum(trr_reldn*trr_reldn)/n()),
            exr_rel_avg = mean(exr_rel),
            exr_rel_dndev = sqrt(sum(exr_reldn*exr_reldn)/n())
            
        )
    
    final_op_Val = merge(final_op_Val, final_index, 
                         by.x="Index.Name", by.y="Index.Name", all.x=TRUE)
    final_op_Val$pct_obs = final_op_Val$obs / final_op_Val$total_obs
    
    final_op_Val = merge(final_op_Val, final_Val, 
                         by.x="IndexVal", by.y="IndexVal", all.x=TRUE)
    
    final_op_Val$idx_trr_ratio = final_op_Val$idx_trr_avg/final_op_Val$idx_trr_dndev
    final_op_Val$idx_exr_ratio = final_op_Val$idx_exr_avg/final_op_Val$idx_exr_dndev
    final_op_Val$trr_ratio = final_op_Val$trr_avg/final_op_Val$trr_dndev
    final_op_Val$exr_ratio = final_op_Val$exr_avg/final_op_Val$exr_dndev
    final_op_Val$trr_rel_ratio = final_op_Val$trr_rel_avg/final_op_Val$trr_rel_dndev
    final_op_Val$exr_rel_ratio = final_op_Val$exr_rel_avg/final_op_Val$exr_rel_dndev
    
    final_op_Val$IndexOpValCopy = final_op_Val$IndexOpVal
    final_op_Val = within(final_op_Val, IndexOpValCopy<-data.frame(do.call('rbind', strsplit(as.character(IndexOpValCopy), ' ', fixed=TRUE))))
    # remove unused columns
    final_op_Val = final_op_Val[, !(colnames(final_op_Val) %in% c("Index.Name","IndexVal"))]    
    
    write.table(final_op_Val, file = fn_riskret_op_val, sep = ",", col.names = TRUE, row.names = FALSE)
    
    return(final)
}

# VnD: this function calc relative trr / exr, their avg, downdev and ratio
relTestVnD <- function(final, fn_riskret_vnd, fn_riskret_op_vnd) {
    final$IndexVnD = paste(final$Index.Name, final$VnD)
    final$IndexOpVnD = paste(final$Index.Name, final$C.OP, final$VnD)
    
    # metrics by index
    # total_obs is the observations by index
    final_index = group_by(final, Index.Name) %>% 
        summarize(
            total_obs = n()
        )
    
    # metrics by VnD bucket
    final_VnD = group_by(final, Index.Name, IndexVnD) %>% 
        summarize(
            obs = n(),
            idx_trr_avg = mean(TRR...MTD.LOC),
            idx_trr_dndev = sqrt(sum(trrdn*trrdn)/n()),
            idx_exr_avg = mean(Excess.Rtn...MTD),
            idx_exr_dndev = sqrt(sum(exrdn*exrdn)/n())
        )
    
    final_VnD = merge(final_VnD, final_index, 
                      by.x="Index.Name", by.y="Index.Name", all.x=TRUE)
    final_VnD$pct_obs = final_VnD$obs / final_VnD$total_obs
    final_VnD$idx_trr_ratio = final_VnD$idx_trr_avg/final_VnD$idx_trr_dndev
    final_VnD$idx_exr_ratio = final_VnD$idx_exr_avg/final_VnD$idx_exr_dndev
    final_VnD$IndexVnDCopy = final_VnD$IndexVnD
    final_VnD = within(final_VnD, IndexVnDCopy<-data.frame(do.call('rbind', strsplit(as.character(IndexVnDCopy), ' ', fixed=TRUE))))
    # remove unused columns
    final_VnD = final_VnD[, !(colnames(final_VnD) %in% c("Index.Name"))]    
    
    write.table(final_VnD, file = fn_riskret_vnd, sep = ",", col.names = TRUE, row.names = FALSE)
    
    # remove unused columns
    final_VnD = final_VnD[, !(colnames(final_VnD) %in% c("total_obs","obs","pct_obs","idx_trr_ratio","idx_exr_ratio"))]    
    
    # metrics by opinion by VnD bucket
    final_op_VnD = group_by(final, Index.Name, IndexVnD, IndexOpVnD) %>% 
        summarize(
            obs = n(),
            trr_avg = mean(TRR...MTD.LOC),
            trr_dndev = sqrt(sum(trrdn*trrdn)/n()),
            exr_avg = mean(Excess.Rtn...MTD),
            exr_dndev = sqrt(sum(exrdn*exrdn)/n()),
            trr_rel_avg = mean(trr_rel),
            trr_rel_dndev = sqrt(sum(trr_reldn*trr_reldn)/n()),
            exr_rel_avg = mean(exr_rel),
            exr_rel_dndev = sqrt(sum(exr_reldn*exr_reldn)/n())
            
        )
    
    final_op_VnD = merge(final_op_VnD, final_index, 
                         by.x="Index.Name", by.y="Index.Name", all.x=TRUE)
    final_op_VnD$pct_obs = final_op_VnD$obs / final_op_VnD$total_obs
    
    final_op_VnD = merge(final_op_VnD, final_VnD, 
                         by.x="IndexVnD", by.y="IndexVnD", all.x=TRUE)
    
    final_op_VnD$idx_trr_ratio = final_op_VnD$idx_trr_avg/final_op_VnD$idx_trr_dndev
    final_op_VnD$idx_exr_ratio = final_op_VnD$idx_exr_avg/final_op_VnD$idx_exr_dndev
    final_op_VnD$trr_ratio = final_op_VnD$trr_avg/final_op_VnD$trr_dndev
    final_op_VnD$exr_ratio = final_op_VnD$exr_avg/final_op_VnD$exr_dndev
    final_op_VnD$trr_rel_ratio = final_op_VnD$trr_rel_avg/final_op_VnD$trr_rel_dndev
    final_op_VnD$exr_rel_ratio = final_op_VnD$exr_rel_avg/final_op_VnD$exr_rel_dndev
    
    final_op_VnD$IndexOpVnDCopy = final_op_VnD$IndexOpVnD
    final_op_VnD = within(final_op_VnD, IndexOpVnDCopy<-data.frame(do.call('rbind', strsplit(as.character(IndexOpVnDCopy), ' ', fixed=TRUE))))
    # remove unused columns
    final_op_VnD = final_op_VnD[, !(colnames(final_op_VnD) %in% c("Index.Name","IndexVnD"))]    
    
    write.table(final_op_VnD, file = fn_riskret_op_vnd, sep = ",", col.names = TRUE, row.names = FALSE)
    
    return(final)
}


# this function call portTest to get test results and then write to disk
opTest <- function(portname, final, riskfree, fn) {
    
    final$port_trr = ifelse(final$port==1, final$TRR...MTD.LOC,0)
    final$port_exr = ifelse(final$port==1, final$Excess.Rtn...MTD,0)
    final$port_wght = ifelse(final$port==1, final$Orig.PrevMend.Mkt...Index.Wght,0)
    final$port_dur = ifelse(final$port==1, final$PrevMend.Mod.Dur.To.Worst,0)
    final$port_asw = ifelse(final$port==1, final$PrevMend.AssetSwp,0)
    final$port_dxs = final$port_dur*final$port_asw
    final_result = portTest(portname, final, riskfree)
    
    if (nrow(final_result)>0) {
        if (!file.exists(fn))
            write.table(final_result, file = fn, sep = ",", col.names = TRUE, row.names = FALSE)
        else
            write.table(final_result, file = fn, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
    }
}

# this function calc the test results
portTest <- function(portname, final, riskfree) {
    
    
    final_month = group_by(final, Index.Name, As.of.Date) %>% 
        summarize(
            obs = sum(port),
            trr = sum(port_trr*port_wght),
            exr = sum(port_exr*port_wght),
            dur = sum(port_dur*port_wght),
            asw = sum(port_asw*port_wght),
            dxs = sum(port_dxs*port_wght),
            wght = sum(port_wght)
            
        )
    
    # link risk free rate
    if (grepl("/", final_month$As.of.Date[1])) {
        final_month$As.of.Date = as.Date(final_month$As.of.Date, "%m/%d/%Y")
    } else {
        final_month$As.of.Date = as.Date(final_month$As.of.Date)
    }
    final_month = merge(final_month, riskfree[ , c("Date", "TRR...1.month.LOC")], 
                        by.x="As.of.Date", by.y="Date", all.x=TRUE)
    
    # filter out months that don't have enough obs
    #TEST_MIN_OBS = 30
    #print(TEST_MIN_OBS)
    if (TEST_MIN_OBS>0) {
        final_month$wght = ifelse(final_month$obs>=TEST_MIN_OBS, final_month$wght,0)
        final_month$obs = ifelse(final_month$obs>=TEST_MIN_OBS, final_month$obs,0)
    }
    
    # final metrics
    final_month$trr = ifelse(final_month$wght==0,0,final_month$trr/final_month$wght)
    final_month$trr_tmp = 1+final_month$trr/100
    final_month$exr = ifelse(final_month$wght==0,0,final_month$exr/final_month$wght)
    final_month$exr_tmp = 1+final_month$exr/100
    final_month$dur = ifelse(final_month$wght==0,0,final_month$dur/final_month$wght)
    final_month$asw = ifelse(final_month$wght==0,0,final_month$asw/final_month$wght)
    final_month$dxs = ifelse(final_month$wght==0,0,final_month$dxs/final_month$wght)
    final_month$trrdn = ifelse(final_month$trr>0,0,final_month$trr)
    final_month$err = final_month$trr - final_month$TRR...1.month.LOC
    final_month$err_tmp = 1+final_month$err/100
    
    if (nrow(final_month) > 0) {
        final = group_by(final_month, Index.Name) %>% 
            summarize(port = portname,
                      months = n(),
                      obs = sum(obs),
                      trr_prod = prod(trr_tmp),
                      exr_prod = prod(exr_tmp),
                      err_prod = prod(err_tmp),
                      dur = mean(dur),
                      asw = mean(asw),
                      dxs = mean(dxs),
                      stdev = sd(trr)*sqrt(12),
                      dndev = sqrt(sum(trrdn*trrdn)*12/n())
            )
        # index
        final$trr = (`^`(final$trr_prod,12/final$months)-1)*100
        final$exr = (`^`(final$exr_prod,12/final$months)-1)*100
        final$err = (`^`(final$err_prod,12/final$months)-1)*100
        final$sharpe = final$err/final$stdev
        final$sortino = final$trr/final$dndev
        
        # remove unused columns
        final = final[, !(colnames(final) %in% 
                              c("trr_prod", "exr_prod", "err_prod", "stdev", "dndev"))]    

    }
    return(final)
    
}


# this function figures out if a bond is qualify based on dts
qualify4AI <- function(df) {
    
    PercentileHold = ConstructModelPortDtSPHold
    PercentileBuy = ConstructModelPortDtSPBuy
    df$DtSPHold = quantile(df$DtS, PercentileHold)
    df$DtSPBuy = quantile(df$DtS, PercentileBuy)
    df$DtSPHoldDiff = abs(df$DtS - df$DtSPHold)
    df$DtSPBuyDiff = abs(df$DtS - df$DtSPBuy)
    row = df[1,]
    #print(row)
    NumOfBonds = row$NumOfBonds
    #print(paste(NumOfBonds, row$IndexTickerDate))
    if (NumOfBonds>3)
    {
        # find the one that is closest to PHold
        df = df[order(df$DtSPHoldDiff),]
        df[1,]$Qualify = 1

        # find the one that is closest to PBuy
        df = df[order(df$DtSPBuyDiff),]
        df[1,]$Qualify = 1
    }
    
    return(df)
    
}

anonymize4AI <- function(final, col) {
    x = group_by_(final, col) %>% summarize(obs=n())
    y = as.matrix(x[col]) %>% anonymize(.algo = "crc32", .seed = sample(1:100, 1))
    colA = paste(col,"A", sep = ".")
    x[colA] = y
    x = x[,c(col,colA)]
    write.table(x, file = paste(colA,"csv", sep = "."), 
                sep = ",", col.names = TRUE, row.names = FALSE)
    return(x)
}

# generate an output file for AI (AIExport.csv)
generateOutput4AI <- function(RF, session) {
    
    
    RootFolder = RF
    DEBUG = FALSE
    
    if (DEBUG) {
        RootFolder = "C:/MyProjects/Guru/BacktestR/"
    }
    
    result = ""
    status_msg = ""
    status_obj = "status"
    
    
    start_time = Sys.time()
    
    init_var()
    
    ########## load bond master files ##########
    
    fn = paste(RootFolder, "AIExport.csv", sep ="")
    if (file.opened(fn)) stop(paste(fn, " is open, pls close it first!"))
    if (file.exists(fn)) file.remove(fn)

    fn2 = paste(RootFolder, "AIExport_IndexLevel.csv", sep ="")
    if (file.opened(fn2)) stop(paste(fn2, " is open, pls close it first!"))
    if (file.exists(fn2)) file.remove(fn2)
    
    status_msg = "loading bond master file..."
    session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
    final = read.csv(paste(RootFolder, "BondMaster.csv", sep = ""))
    final$IndexTickerDate = paste(final$Index.Name, final$Ticker, final$As.of.Date)
    final$MaxDtS = ave(final$DtS, final$IndexTickerDate, FUN=max)
    final$Qualify = 0
    final$IndexRatingDate = paste(final$Index.Name, final$Rating, final$As.of.Date)
    final$asw.dur = final$PrevMend.AssetSwp / final$PrevMend.Mod.Dur.To.Worst
    final$asw_med = ave(final$PrevMend.AssetSwp, final$IndexRatingDate, FUN=median)
    final$asw.dur_med = ave(final$asw.dur, final$IndexRatingDate, FUN=median)
    
    final = final[,c("Index.Name", "Investment.Date", "Cusip", "Qualify","Ticker", "Rating", 
                     "ML.Industry.Lvl.3","ML.Industry.Lvl.4",
                     "PrevMend.Mod.Dur.To.Worst", "PrevMend.Price", "PrevMend.AssetSwp", "DtS", 
                     "Orig.PrevMend.Mkt...Index.Wght", "TRR...MTD.LOC", "Excess.Rtn...MTD",
                     "M.SCORE", "M.NA", "M.OP", "Solvency", "T.SCORE", "T.NA", "Creative.Accounting", 
                     "L.SCORE", "L.NA", "Liquidity", "C.OP", "F.OP",
                     "IndexTickerDate","NumOfBonds","MaxDtS","DtSPHold","DtSPBuy","DtSPHoldDiff","DtSPBuyDiff",
                     "IndexRatingDate","asw_med","asw.dur","asw.dur_med")]
    names(final)[names(final) == "Orig.PrevMend.Mkt...Index.Wght"] = "Index.Wght"
    names(final)[names(final) == "Solvency"] = "S.OP"
    names(final)[names(final) == "Creative.Accounting"] = "T.OP"
    names(final)[names(final) == "Liquidity"] = "L.OP"
    
    # next steps
    # 1 - qualify
    status_msg = "determining qualify bonds..."
    session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
    
    # handling simple cases here for efficiency
    final$Qualify = ifelse(final$NumOfBonds<=3 & final$DtS==final$MaxDtS,1,0)

    # output1 need to pick 50 and 90 percentile
    dts_output1=subset(final, final$NumOfBonds>3)
    # output2 is already done, separate it out
    dts_output2=subset(final, final$NumOfBonds<=3)
    # destory final
    rm(final)
    gc()
    
    dts_list = dts_output1 %>% split(f=dts_output1$IndexTickerDate)
    dts_list = lapply(dts_list, qualify4AI)
    dts_output1 = do.call(rbind,dts_list)
    final = rbind(dts_output2, dts_output1)
    
    # destory dts_output1 & 2
    rm(dts_output1)
    rm(dts_output2)
    rm(dts_list)
    gc()
    
    # 2 - rating to number
    ratings = read.csv("ratings.csv")
    final = merge(final, ratings, by.x="Rating", by.y="Rating", all.x = TRUE)
    # 3 - DtS Bucket
    final = addDtSBucket(final)
    # 4 - ASW/Rating and ASW/Duration/Rating
    final$ASW.Rating = final$PrevMend.AssetSwp / final$asw_med
    final$ASW.Dur.Rating = final$asw.dur / final$asw.dur_med
    
    status_msg = "anonymizing data..."
    session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
    
    # 5 -next start anonymization for Index.Name, Ticker and Cusip
    indexlvl = indexlvldata(RootFolder, IndexLvlDataFolder)
    Index.A = anonymize4AI(indexlvl, "Index")
    #Index.Name.A = anonymize4AI(final, "Index.Name")
    Ticker.A = anonymize4AI(final, "Ticker")
    Cusip.A = anonymize4AI(final, "Cusip")
    
    final = merge(final, Index.A, by.x = "Index.Name", by.y = "Index", all.x = TRUE)
    indexlvl = merge(indexlvl, Index.A, by.x = "Index", by.y = "Index", all.x = TRUE)
    final = merge(final, Ticker.A, by.x = "Ticker", by.y = "Ticker", all.x = TRUE)
    final = merge(final, Cusip.A, by.x = "Cusip", by.y = "Cusip", all.x = TRUE)
    
    # reorder columns
    final = final[,c("Investment.Date","Index.A","Cusip.A","Ticker.A",
                     "ML.Industry.Lvl.3","ML.Industry.Lvl.4",
                     "NumOfBonds","Qualify","Rating","RatingNum",
                     "PrevMend.Mod.Dur.To.Worst","PrevMend.AssetSwp",
                     "DtS","DtSBucket.Label","DtSBucket.Name",
                     "asw_med","ASW.Rating","asw.dur_med","ASW.Dur.Rating",
                     "Index.Wght","PrevMend.Price","TRR...MTD.LOC","Excess.Rtn...MTD",
                     "M.SCORE","M.NA","M.OP","S.OP",
                     "T.SCORE","T.NA","T.OP",
                     "L.SCORE","L.NA","L.OP",
                     "C.OP","F.OP")]
    # rename columns
    names(final)[names(final) == "PrevMend.Mod.Dur.To.Worst"] = "Begin Month Duration"
    names(final)[names(final) == "PrevMend.AssetSwp"] = "Begin Month ASW"
    names(final)[names(final) == "asw_med"] = "Median Rating ASW"
    names(final)[names(final) == "asw.dur_med"] = "Median Rating ASW/Duration"
    names(final)[names(final) == "Index.Wght"] = "Begin Month Index.Wght%"
    names(final)[names(final) == "PrevMend.Price"] = "Begin Month Price"
    
    write.table(final, file = fn, sep = ",", col.names = TRUE, row.names = FALSE)
    
    # select columns
    indexlvl = indexlvl[, c("Index.A","Date","TRR...1.month.LOC","Excess.Rtn...1.month","Asset.Swap","Mod.Dur.To.Worst")]
    write.table(indexlvl, file = fn2, sep = ",", col.names = TRUE, row.names = FALSE)
    
    result_file = "AIExport.csv and AIExport_IndexLevel.csv"
    end_time = Sys.time()
    time_took = as.numeric(difftime(end_time, start_time, units=("mins")))
    time_took = format(round(time_took, 1), nsmall = 1)
    result = paste(result_file, "generated successfully and took", time_took, "mins.")
    
}

# generate an output file of Bull/Bear Market Cycles
generateBullBearMkt <- function(RF, MarketCycleMetric, session) {
    
    
    RootFolder = RF
    DEBUG = FALSE
    
    if (DEBUG) {
        RootFolder = "C:/MyProjects/Guru/BacktestR/"
    }
    
    result = ""
    status_msg = ""
    status_obj = "status"
    
    
    start_time = Sys.time()
    
    init_var()
    
    fn = paste(RootFolder, "marketcycles.csv", sep ="")
    if (file.opened(fn)) stop(paste(fn, " is open, pls close it first!"))
    if (file.exists(fn)) file.remove(fn)
    
    status_msg = "loading daily stats file..."
    session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
    trr_prr = read.csv(paste(RootFolder, "Bond raw data/Index lvl data/Daily/trr_prr.csv", sep = ""))
    trr_prr$Date = as.Date(trr_prr$Date)
    ########## main logic starts here ##########
    mktcycles = data.frame(matrix(ncol=0,nrow=0))
    
    for (i in 1:nrow(TH.Limits)) {
        index = as.character(TH.Limits$Indices[i])
        startCycle = as.character(TH.Limits$StartCycle[i])
        cycleThreshold = TH.Limits$CycleThreshold[i]
        histdata = subset(trr_prr, trr_prr$Index.Name == index)
        histdata = histdata[,c("Date",MarketCycleMetric)]
        names(histdata)[names(histdata)=="Date"] = "date"
        names(histdata)[names(histdata)==MarketCycleMetric] = "price"
        print(paste("-----",index, MarketCycleMetric, cycleThreshold,"-----"))
        output = findMarketCycles(histdata, index, startCycle, cycleThreshold)
        #if (nrow(mktcycles)==0) {
            # create an empty mktcycles dataframe with same structure of output
        #    mktcycles = output[0,]
        #}
        mktcycles=rbind(mktcycles, output)
        
    }
    
    
    write.table(mktcycles, file = fn, sep = ",", col.names = TRUE, row.names = FALSE)
    
    result_file = "marketcycles.csv"
    end_time = Sys.time()
    time_took = as.numeric(difftime(end_time, start_time, units=("mins")))
    time_took = format(round(time_took, 1), nsmall = 1)
    result = paste(result_file, "generated successfully and took", time_took, "mins.")
    
}
# find bear/bull markets
findMarketCycles <- function(histdata, index, startCycle, cycleThreshold) {
    
    #startCycle = "bull"
    #fn = "SNP500.csv"
    #RF = RootFolder
    #fn = paste(RF, fn, sep = "")
    #histdata = read.csv(fn)
    #histdata$date = as.Date(histdata$date)
    #cycleThreshold = 0.2
    #-----------------------------
    
    # initialize variables
    #df = setNames(data.frame(matrix(ncol = 6, nrow = 0)), 
    #              c("Index.Name", "MarketCycle", "StartDate", "StartValue", "EndDate", "EndValue"))
    
    #df <- data.frame(Doubles=double(),
    #                 Ints=integer(),
    #                 Factors=factor(),
    #                 Logicals=logical(),
    #                 Characters=character(),
    #                 stringsAsFactors=FALSE)
    
    df <- data.frame(Index.Name=character(),
                     MarketCycle=character(),
                     StartDate=as.Date(character()),
                     StartValue=double(),
                     EndDate=as.Date(character()),
                     EndValue=double(),
                     stringsAsFactors=FALSE)
    
    if (startCycle=="bull") {
        mktPrevPeak = histdata$price[1]
        mktPrevPeakDate = histdata$date[1]
        mktPrevTrough = NULL
        mktPrevTroughDate = NULL
    } else {
        mktPrevPeak = NULL
        mktPrevPeakDate = NULL
        mktPrevTrough = histdata$price[1]
        mktPrevTroughDate = histdata$date[1]
    }
    
    mktCycle = startCycle
    mktStart = histdata$date[1]
    mktStartPrice = histdata$price[1]
    mktEnd = NULL
    mktEndPrice = NULL
    
    for(i in 2:nrow(histdata)) {
        if (mktCycle == "bull") {
            # compare the current value with prev peak
            # if the current value is more than 20% lower than the prev peak, we find a bear market
            diffFromPeak = histdata$price[i]/mktPrevPeak - 1
            if (diffFromPeak < -1 * cycleThreshold) {
                # found bear market, record it
                mktEnd = mktPrevPeakDate
                mktEndPrice = mktPrevPeak
                print(paste(mktCycle,mktStart, mktStartPrice, mktEnd, mktEndPrice))
                df[nrow(df)+1,]=list(index, mktCycle,
                                     mktStart, mktStartPrice, 
                                     mktEnd, mktEndPrice)
                # start a new mkt cycle
                mktCycle = "bear"
                mktStart = mktPrevPeakDate
                mktStartPrice = mktPrevPeak
                mktEnd = NULL
                mktEndPrice = NULL
                
                mktPrevTrough = histdata$price[i]
                mktPrevTroughDate = histdata$date[i]
                mktPrevPeak = NULL
                mktPrevPeakDate = NULL
                
            } else {
                # not bear market yet, update prev peak
                if(diffFromPeak>0) {
                    # new peak, record it
                    mktPrevPeak = histdata$price[i]
                    mktPrevPeakDate = histdata$date[i]
                }
            }
        } else {
            # compare the current value with prev trough
            # if the current value is more than 20% higher than the prev trough, we find a bull market
            diffFromTrough = histdata$price[i]/mktPrevTrough - 1
            if (diffFromTrough > cycleThreshold) {
                # found bull market, record it
                mktEnd = mktPrevTroughDate
                mktEndPrice = mktPrevTrough
                print(paste(mktCycle,mktStart, mktStartPrice, mktEnd, mktEndPrice))
                df[nrow(df)+1,]=list(index, mktCycle,
                                     mktStart, mktStartPrice, 
                                     mktEnd, mktEndPrice)
                # start a new mkt cycle
                mktCycle = "bull"
                mktStart = mktPrevTroughDate
                mktStartPrice = mktPrevTrough
                mktEnd = NULL
                mktEndPrice = NULL
                
                mktPrevPeak = histdata$price[i]
                mktPrevPeakDate = histdata$date[i]
                mktPrevTrough = NULL
                mktPrevTroughDate = NULL
                
            } else {
                # not bull market yet, update prev trough
                if(diffFromTrough<0) {
                    # new trough, record it
                    mktPrevTrough = histdata$price[i]
                    mktPrevTroughDate = histdata$date[i]
                }
            }            
        }
    }
    # print the last cycle
    if (mktCycle=="bull") {
        mktEnd = mktPrevPeakDate
        mktEndPrice = mktPrevPeak
    } else {
        mktEnd = mktPrevTroughDate
        mktEndPrice = mktPrevTrough
    }
    print(paste(mktCycle,mktStart, mktStartPrice, mktEnd, mktEndPrice))
    df[nrow(df)+1,]=list(index, mktCycle,
                         mktStart, mktStartPrice, 
                         mktEnd, mktEndPrice)
    
    #df$mktStart = as.Date(df$mktStart)
    #df$mktEnd = as.Date(df$mktEnd)
    
    return(df)
}

# vect is a vector, val is a value
# by using lapply, we can create a dataframe by replicating a vector many times by another vector
createDF <- function(val, vect) 
    { return (data.frame(col1=vect,col2=val))}

recycledTickers <- function(ds) {

    # get a list of tickers
    tickers = group_by(ds, Ticker) %>% 
        summarize(
            obs = n()
        )
    # get a list of dates
    startDate = min(ds$As.of.Date)
    endDate = max(ds$As.of.Date)
    total_months = as.numeric(as.mondate(endDate) - as.mondate(startDate))
    dates = c(startDate)
    for (i in 1:total_months) {
        curDate = as.Date(as.mondate(startDate) + i)
        dates = c(dates, curDate)
    }
    # create a dataframe that's tickers x dates
    df1 = lapply(dates,createDF, vect = tickers$Ticker)
    df1 <- do.call(rbind, df1)
    names(df1)[names(df1)=="col1"] = "Ticker"
    names(df1)[names(df1)=="col2"] = "As.of.Date"
    df1$TickerDate = paste(df1$Ticker, df1$As.of.Date)
    
    # create a dataframe that's ticker x existing dates
    df2 = group_by(ds, Ticker, As.of.Date) %>% 
        summarize(
            obs = n()
        )
    df2$TickerDate = paste(df2$Ticker, df2$As.of.Date)
    
    # merge df1 and df2 together
    df = merge(df1, df2[,c("TickerDate","obs")], by.x = "TickerDate", by.y = "TickerDate", all.x = TRUE)
    df$obs = ifelse(is.na(df$obs),0,1)
    
    # sort df add index
    df = df[order(df$Ticker, df$As.of.Date),]
    df$index = seq.int(nrow(df))-1
    df$index1 = df$index + 1
    df$index = paste(df$Ticker, df$index)
    df$index1 = paste(df$Ticker, df$index1)
    df = merge(df, df[,c("index1","obs")], by.x="index", by.y="index1", all.x=TRUE)
    df = df[order(df$Ticker, df$As.of.Date),]
    df$obs.y = ifelse(is.na(df$obs.y),0,df$obs.y)
    df$pat = paste(as.character(df$obs.x), as.character(df$obs.y))
    df = subset(df, df$pat == "0 1" | df$pat == "1 0" )
    
    # now get each period into one row with start / end
    df = df[,c("Ticker","As.of.Date","pat")]
    df = df[order(df$Ticker, df$As.of.Date),]
    df$index = seq.int(nrow(df))-1
    df$index1 = df$index + 1
    df1 = subset(df, df$pat == "1 0")
    df1$match = paste(df1$Ticker, df1$index1)
    names(df1)[names(df1)=="As.of.Date"]="Period.Start"
    df1 = df1[,c("Ticker","Period.Start","match")]
    df2 = subset(df, df$pat == "0 1")
    df2$match = paste(df2$Ticker, df2$index)
    names(df2)[names(df2)=="As.of.Date"]="Period.End"
    df2 = df2[,c("Period.End","match")]
    
    TickerPeriods = merge(df1, df2, by.x = "match", by.y = "match", all.x = TRUE)
    TickerPeriods = TickerPeriods[,c("Ticker","Period.Start","Period.End")]
    TickerPeriods$Period.End = as.Date(as.mondate(TickerPeriods$Period.End) - 1)
    
    # ticker with more than 1 periods
    TickerGt1Periods = group_by(TickerPeriods, Ticker) %>%
        summarize(
            obs = n()
        )
    TickerGt1Periods = subset(TickerGt1Periods, TickerGt1Periods$obs > 1)
    TickerGt1Periods = TickerGt1Periods[,c("Ticker")]
    
    # ticker with more than 1 bond names
    TickerGt1Names = group_by(ds, Ticker, Bond.Name) %>%
        summarize(
            obs = n()
        )
    TickerGt1Names = group_by(TickerGt1Names, Ticker) %>%
        summarize(
            obs = n()
        )
    TickerGt1Names = subset(TickerGt1Names, TickerGt1Names$obs > 1)
    TickerGt1Names = TickerGt1Names[,c("Ticker")]
    
    # ticker with more than 1 industries
    TickerGt1Inds = group_by(ds, Ticker, ML.Industry.Lvl.4) %>%
        summarize(
            obs = n()
        )
    TickerGt1Inds = group_by(TickerGt1Inds, Ticker) %>%
        summarize(
            obs = n()
        )
    TickerGt1Inds = subset(TickerGt1Inds, TickerGt1Inds$obs > 1)
    TickerGt1Inds = TickerGt1Inds[,c("Ticker")]
    
    #======================================
    # ticker suspected to be recycled
    TickerRecycled = merge(TickerGt1Periods, TickerGt1Names, by.x = "Ticker", by.y = "Ticker")
    TickerRecycled = merge(TickerRecycled, TickerGt1Inds, by.x = "Ticker", by.y = "Ticker")
    
    # add periods to recycled tickers
    TickerRecycled = merge(TickerRecycled, TickerPeriods, by.x = "Ticker", by.y = "Ticker")
    # add industry and company id
    TickerRecycled$TickerDate = paste(TickerRecycled$Ticker, TickerRecycled$Period.Start)
    ds$TickerDate = paste(ds$Ticker, ds$As.of.Date)
    temp = group_by(ds, TickerDate) %>%
        summarize(obs=n(),
                  Bond.Name=first(Bond.Name),
                  Industry=first(ML.Industry.Lvl.4)
        )
    TickerRecycled = merge(TickerRecycled, temp[,c("TickerDate","Bond.Name","Industry")], 
                           by.x = "TickerDate", by.y = "TickerDate")
    
    # joining index file to company scores (unequal join using data.table object)
    
    temp=setDT(TickerRecycled)[setDT(trans[,c("Bond.Ticker","ID_BB_UNIQUE","FROM.DATE.LINK","TO.DATE.LINK","COMMENTS")]), 
                               on=.(Ticker=Bond.Ticker, Period.Start>=FROM.DATE.LINK, Period.Start<TO.DATE.LINK),
                               nomatch=0,allow.cartesian=TRUE]
    # convert data.table back to data.frame    
    temp=as.data.frame(temp)
    # reformat Company ID back to its original format
    temp$ID_BB_UNIQUE = paste("EQ", str_pad(temp$ID_BB_UNIQUE, 16, pad = "0"), sep = "")
    
    TickerRecycled = merge(TickerRecycled, temp[,c("TickerDate","ID_BB_UNIQUE","COMMENTS")], 
                           by.x = "TickerDate", by.y = "TickerDate", all.x = TRUE)
    
    #======================================
    # now detect if multi-IDs has been already added
    temp = group_by(TickerRecycled, Ticker) %>%
        summarize(NumOfBonds=n_distinct(Bond.Name)
        )
    
    temp2 = group_by(TickerRecycled, Ticker) %>%
        summarize(NumOfIDs=n_distinct(na.omit(ID_BB_UNIQUE))
        )
    temp = merge(temp, temp2, by.x = "Ticker", by.y = "Ticker")
    temp = subset(temp, temp$NumOfBonds>temp$NumOfIDs)    
    
    TickerRecycled = TickerRecycled[TickerRecycled$Ticker %in% temp$Ticker,]
    
    TickerRecycled$Index.Name = ds$Index.Name[1]
    return (TickerRecycled)
    
}

# generate an file to find out ticker periods
# can be used to detect recycled tickers
findRecycledTickers <- function(RF, session) {
    
    
    RootFolder = RF
    DEBUG = FALSE
    
    if (DEBUG) {
        RootFolder = "C:/MyProjects/Guru/BacktestR/"
    }
    
    result = ""
    status_msg = ""
    status_obj = "DC_status"
    
    
    start_time = Sys.time()
    
    init_var()

    ########## load translation file ##########
    status_msg = "loading translation file..."
    session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
    trans <<- loadtrans(RootFolder)
    
    ########## load bond master files ##########
    
    fn = paste(RootFolder, "TickerRecycled.csv", sep ="")
    if (file.opened(fn)) stop(paste(fn, " is open, pls close it first!"))
    if (file.exists(fn)) file.remove(fn)

    fn2 = paste(RootFolder, "TickerRecycled2.csv", sep ="")
    if (file.opened(fn2)) stop(paste(fn2, " is open, pls close it first!"))
    if (file.exists(fn2)) file.remove(fn2)
    
    status_msg = "loading bond master file..."
    session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
    final = read.csv(paste(RootFolder, "BondMaster.csv", sep = ""))
    final$As.of.Date=as.Date(final$As.of.Date)
    final$Bond.Name = toupper(trimws(final$Bond.Name))

    final = final %>%
        separate(Bond.Name, c("Bond.Name"), " ")
    
    final = final %>%
        separate(Bond.Name, c("Bond.Name"), "-")

    status_msg = "detecting recycled tickers..."
    session$sendCustomMessage(type = 'print', message = list(selector = status_obj, html = status_msg))
    
    #=====================================
    ds_list = final %>% split(f=final$Index.Name)
    ds_list = lapply(ds_list, recycledTickers)
    TickerRecycled = do.call(rbind,ds_list)

    write.table(TickerRecycled[,c("Index.Name","Ticker","Period.Start","Period.End","Bond.Name","Industry","ID_BB_UNIQUE","COMMENTS")], 
                file = fn, sep = ",", col.names = TRUE, row.names = FALSE)

    TickerRecycled2 = TickerRecycled %>% distinct(Ticker,Bond.Name, Industry, .keep_all = TRUE)

    write.table(TickerRecycled2[,c("Index.Name","Ticker","Period.Start","Period.End","Bond.Name","Industry","ID_BB_UNIQUE","COMMENTS")], 
                file = fn2, sep = ",", col.names = TRUE, row.names = FALSE)
    
    result_file = "TickerRecycled.csv and TickerRecycled2.csv"
    end_time = Sys.time()
    time_took = as.numeric(difftime(end_time, start_time, units=("mins")))
    time_took = format(round(time_took, 1), nsmall = 1)
    result = paste(result_file, "generated successfully and took", time_took, "mins.")
    
}
