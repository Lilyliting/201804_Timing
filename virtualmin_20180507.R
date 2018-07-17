#nc

# initial settings ----------------------

setwd('C:\\Users\\Administrator\\Desktop\\Timing_20180427')

library(RMySQL)
library(lubridate)
library(xts)
library(RcppRoll)
conwind <- dbConnect(MySQL(), host='rm-2zey1z6px42nits51.mysql.rds.aliyuncs.com',
                     port=3306,dbname="wind", username="hulitingali", password="Hu.lt@2018")


# input -------------------

indexcode <- '000300.SH'
stadate <- '20161230' # date included
enddate <- '20180421'
freq <- 'days' # days,weeks,months,quarters,years

gate1=0.55
gate2=0.45
k=0.98

# define function  ----------------

virtualmin <- function(indexcode = '000300.SH', stadate = '20171230',
                 enddate = '20180421',freq = 'days',gate1=0.55,gate2=0.45,k=0.98) {
    # k：遗忘系数
    truestadate <- as.Date(stadate, format="%Y%m%d")-30
    truestadate <- gsub('-','',as.character(truestadate))
    
    query <- paste0('select TRADE_DT,S_DQ_OPEN,S_DQ_HIGH,S_DQ_LOW,S_DQ_CLOSE,S_DQ_VOLUME from aindexeodprices
                    where S_INFO_WINDCODE = \'',indexcode,'\' and TRADE_DT >= \'',truestadate,'\'  
                    and TRADE_DT <= \'',enddate,'\' order by TRADE_DT;')
    indexdata <- dbGetQuery(conwind, query)
    indexdata[,1] <- as.Date(indexdata[,1], format="%Y%m%d")
    
    tradedays <- indexdata[,1]
    indexquote <- xts(indexdata[, -1],indexdata[, 1])
    
    sig <- indexquote[,1]
    sig[] <- NaN
    
    Rlonglist <- sig
    Rshortlist <- sig
    siglonglist <- sig
    sigshortlist <- sig
    
    Rlonglist[1,] <- 0
    Rshortlist[1,] <- 0
    siglonglist[1,] <- 0.5
    sigshortlist[1,] <- 0.5
    
    
    for (ii in 2:length(tradedays)) {
        closediff <- as.numeric(indexquote[ii,4])-as.numeric(indexquote[ii-1,4])
        vh <- siglonglist[ii-1,]*closediff-sigshortlist[ii-1,]*closediff
        Rlonglist[ii,] <- as.numeric(k*Rlonglist[ii-1,] + gate1*closediff - vh)
        Rshortlist[ii,] <- as.numeric(k*Rshortlist[ii-1,] + gate2*closediff - vh)
        siglonglist[ii,] <- Rlonglist[ii,]/(Rlonglist[ii,]+Rshortlist[ii,])
        sigshortlist[ii,] <- Rshortlist[ii,]/(Rlonglist[ii,]+Rshortlist[ii,])
        
    }
    
    
    
    ema <- function(v,n) {
        emav <- v
        for (ii in 2:length(v)) {
            emav[ii] <- (2/(n+1))*v[ii]+((n-1)/(n+1))*as.numeric(emav[ii-1])
        }
        return(emav)
    }
    bolln <- 26
    MA <- ema(indexquote[,4],26)
    MD <- sig
    for (ii in bolln:length(tradedays)) {
        MD[ii] <- sd(as.numeric(indexquote[(ii-bolln+1):ii,4]))
    }
    UP = MB+2*MD
    DN = MB-2*MD
    
    #kdj
    n <- 9
    RSV <- vector(length = length(tradedays))
    for (ii in n:length(tradedays)) {
        Hn <- as.numeric(indexquote[(ii-8):ii,2])
        Ln <- as.numeric(indexquote[(ii-8):ii,3])
        hn <- max(Hn)
        ln <- min(Ln)
        RSV[ii] <- (indexquote[ii,4]-ln)/(hn-ln)*100
    }
    RSV[RSV==0] <- 50
    k <- ema(RSV,3)
    d <- ema(k,3)
    k <- xts(k,tradedays)
    d <- xts(d,tradedays)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    sig1 <- sig
    sig2 <- sig
    sig3 <- sig
    
    #dmi
    dm1 <- indexquote[,2]
    dm1 <- diff(dm1)
    dm1[dm1<0] <- 0
    dm1[is.na(dm1)] <- 0
    
    
    dm2 <- indexquote[,3]
    dm2 <- -diff(dm2)
    dm2[dm2<0] <- 0
    dm2[is.na(dm2)] <- 0
    
    tr <- pmax(abs(indexquote[,2]-indexquote[,3]),
              abs(lag(indexquote[,4])-indexquote[,3]),
              abs(indexquote[,2]-lag(indexquote[,4])),na.rm = T)
   
    mdm1 <- ema(as.vector(dm1[,1]),13)
    mdm2 <- ema(as.vector(dm2[,1]),13)
    mtr <- ema(as.vector(tr[,1]),13)
    
    DI1 <- mdm1/mtr*100
    DI2 <- mdm2/mtr*100
    DX <- abs(DI1-DI2)/(DI1+DI2)*100
    DX[is.na(DX)] <- 0
    
    ADX <- ema(DX,14)
    sig1[ADX>20 & DI1>DI2] <- 1
    sig1[ADX>20 & DI1<DI2] <- 0
    if (is.na(sig1[1])) sig1[1]=0
    sig1 <- na.locf(sig1)
    
    #kdj
    n <- 9
    RSV <- vector(length = length(tradedays))
    for (ii in n:length(tradedays)) {
        Hn <- as.numeric(indexquote[(ii-8):ii,2])
        Ln <- as.numeric(indexquote[(ii-8):ii,3])
        hn <- max(Hn)
        ln <- min(Ln)
        RSV[ii] <- (indexquote[ii,4]-ln)/(hn-ln)*100
    }
    RSV[RSV==0] <- 50
    k <- ema(RSV,3)
    d <- ema(k,3)
    k <- xts(k,tradedays)
    d <- xts(d,tradedays)
    sig2[k>30 & lag(k)<30] <- 1
    sig2[k<70 & lag(k)>70] <- 0
    sig2[k>70] <- 1
    sig2[sig2==1 & k<70 & k<d & lag(k)>lag(d)] <- 0
    sig2[sig2==0 & k>30 & k>d & lag(k)<lag(d)] <- 1
    
    if (is.na(sig2[1])) sig2[1]=0
    sig2 <- na.locf(sig2)
    
    #emv
    n=14
    
    PR <- (indexquote[,2]+indexquote[,3])/2-(lag(indexquote[,2])+lag(indexquote[,3]))/2
    PV <- indexquote[,5]/(indexquote[,2]-indexquote[,3])
    EMV <- PR/PV
    EMV[is.na(EMV)] <- 0
    EMVA <- rollmeanr(EMV,n)
    
    sig3[EMVA>0 & lag(EMVA)<0] <- 1
    sig3[EMVA<0 & lag(EMVA)>0] <- 1
    if (is.na(sig3[1])) sig3[1]=0
    sig3 <- na.locf(sig3)
   
    sigsum <- sig1+sig2+sig3
    sig[sigsum>=2] <- 1
    sig[sigsum<2] <- 0
    
    sig <- sig[paste0(as.Date(stadate, format="%Y%m%d"),'/')]
    colnames(sig) <- 'virtualmin'
    return(sig)
}

timing <- virtualmin(stadate = '20131230',enddate = '20180430')
timingdf <- as.data.frame(timing)
write.csv(timingdf,'C:\\Users\\Administrator\\Desktop\\Periodly_invest_20180502\\Data\\dketiming.csv')
