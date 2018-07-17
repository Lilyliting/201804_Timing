# initial settings ----------------------

setwd('C:\\Users\\Administrator\\Desktop\\Timing_20180427')

library(RMySQL)
library(lubridate)
library(xts)
library(RcppRoll)
library(rJava)
library(xlsx)


conwind <- dbConnect(MySQL(), host='rm-2zey1z6px42nits51.mysql.rds.aliyuncs.com',
                     port=3306,dbname="wind", username="hulitingali", password="Hu.lt@2018")

# input ------------------------

indexcode <- '000300.SH'
stadate <- '20090101' # date included
enddate <- '20180501'
freq <- 'days' # days,weeks,months,quarters,years



Double_MA <- function(indexcode = '000300.SH', stadate = '20171230',
                      enddate = '20180421',freq = 'days',m=10,n=5) {
    truestadate <- as.Date(stadate, format="%Y%m%d")-ceiling(m*2)
    truestadate <- gsub('-','',as.character(truestadate))
    
    query <- paste0('select TRADE_DT, S_DQ_CLOSE from aindexeodprices
                    where S_INFO_WINDCODE = \'',indexcode,'\' and TRADE_DT >= \'',truestadate,'\'  
                    and TRADE_DT <= \'',enddate,'\' order by TRADE_DT;')
    indexdata <- dbGetQuery(conwind, query)
    
    indexdata[,1] <- as.Date(indexdata[,1], format="%Y%m%d")
    
    tradedays <- indexdata[,1]
    indexquote <- xts(indexdata[, -1],indexdata[, 1])
    indexret <- diff(log(indexquote))
    
    indexret <- indexret[!is.na(indexret)]
    indexprdret <- period.sum(indexret,endpoints(indexret,on=freq))
    
    indexMA_m <- rollmeanr(indexprdret,m)
    indexMA_n <- rollmeanr(indexprdret,n)
    
    sig <- indexMA_n-indexMA_m
    sig[sig>0] <- 1
    sig[sig<=0] <- 0
    colnames(sig) <- 'Double_MA'
    
    sig <- sig[paste0(as.Date(stadate, format="%Y%m%d"),'/')]
    return(sig)
}

dmi_kdj_emv <- function(indexcode = '000300.SH', stadate = '20171230',
                        enddate = '20180421',freq = 'days') {
    
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
    ema <- function(v,n) {
        emav <- v
        for (ii in 2:length(v)) {
            emav[ii] <- (2/(n+1))*v[ii]+((n-1)/(n+1))*emav[ii-1]
        }
        return(emav)
    }
    
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
    k <- ema(RSV,2)
    d <- ema(k,2)
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
    colnames(sig) <- 'dmi_kdj_emv'
    return(sig)
}


GFTD <- function(indexcode = '000300.SH', stadate = '20171230',
                 enddate = '20180421',freq = 'days',bn1=4,bn2=4,bn3=4,
                 sn1=5,sn2=5,sn3=5) {
    
    truestadate <- as.Date(stadate, format="%Y%m%d")-30
    truestadate <- gsub('-','',as.character(truestadate))
    
    query <- paste0('select TRADE_DT,S_DQ_CLOSE,S_DQ_HIGH,S_DQ_LOW from aindexeodprices
                    where S_INFO_WINDCODE = \'',indexcode,'\' and TRADE_DT >= \'',truestadate,'\'  
                    and TRADE_DT <= \'',enddate,'\' order by TRADE_DT;')
    indexdata <- dbGetQuery(conwind, query)
    indexdata[,1] <- as.Date(indexdata[,1], format="%Y%m%d")
    
    tradedays <- indexdata[,1]
    indexquote <- xts(indexdata[, -1],indexdata[, 1])
    
    sig <- indexquote[,1]
    sig[] <- NaN
    
    timelen <- dim(indexquote[,1])[1]
    idxdf <- as.data.frame(indexquote)
    # startbuy
    udb <- 0
    for (ii in (bn1+1):timelen) {
        if (idxdf[ii,1]<idxdf[ii-bn1,1]) {
            udb <- udb - 1
            if (udb == -bn2) {
                cnt <- 1
                m <- ii+1
                for (jj in (ii+2):timelen) {
                    if (jj>timelen) break
                    if ((idxdf[jj,1]>=idxdf[jj-2,2]) & 
                        (idxdf[jj,2]>idxdf[jj-1,2]) &
                        (idxdf[jj,1]>idxdf[m,1])) {
                        cnt <- cnt+1
                        m <- jj
                        if (cnt==bn3) {
                            sig[jj] <- 1
                            break
                        }
                    }
                }
            }
        } else {
            udb <- 0
        }
    }
    
    
    # startsell
    uds <- 0
    for (ii in (sn1+1):timelen) {
        if (idxdf[ii,1]>idxdf[ii-sn1,1]) {
            uds <- uds + 1
            if (uds == sn2) {
                cnt <- 1
                m <- ii+1
                for (jj in (ii+2):timelen) {
                    if (jj>timelen) break
                    if ((idxdf[jj,1]<=idxdf[jj-2,3]) & 
                        (idxdf[jj,3]<idxdf[jj-1,3]) &
                        (idxdf[jj,1]<idxdf[m,1])) {
                        cnt <- cnt+1
                        m <- jj
                        if (cnt==sn3) {
                            sig[jj] <- 0
                            break
                        }
                    }
                }
            }
        } else {
            uds <- 0
        }
    }
    
    if (is.nan(sig[1])) {
        sig[1] <- 0
    }
    
    # smooth?
    
    sig <- na.locf(sig)
    sig <- sig[paste0(as.Date(stadate, format="%Y%m%d"),'/')]
    colnames(sig) <- 'GFTD'
    return(sig)
}

LLT <- function(indexcode = '000300.SH', stadate= '20171230',
                enddate = '20180421',freq = 'days',alpha=1/40) {
    
    truestadate <- as.Date(stadate, format="%Y%m%d") - 10
    truestadate <- gsub('-','',as.character(truestadate))
    
    query <- paste0('select TRADE_DT,S_DQ_CLOSE from aindexeodprices
                    where S_INFO_WINDCODE = \'',indexcode,'\' and TRADE_DT >= \'',truestadate,'\'  
                    and TRADE_DT <= \'',enddate,'\' order by TRADE_DT;')
    indexdata <- dbGetQuery(conwind, query)
    indexdata[,1] <- as.Date(indexdata[,1], format="%Y%m%d")
    
    tradedays <- indexdata[,1]
    indexquote <- xts(indexdata[, -1],indexdata[, 1])
    
    llt <- indexquote
    for (ii in 3:length(indexquote)) {
        pricelag0 <- as.numeric(indexquote[ii])
        pricelag1 <- as.numeric(indexquote[ii-1])
        pricelag2 <- as.numeric(indexquote[ii-2])
        llt[ii] <- (alpha-alpha^2/4)*pricelag0+alpha^2/2*pricelag1-(alpha-3*alpha^2/4)*pricelag2+
            2*(1-alpha)*as.numeric(llt[ii-1])-(1-alpha)^2*as.numeric(llt[ii-2])
    }
    
    sig <- (llt>lag(llt))*1
    sig <- sig[paste0(as.Date(stadate, format="%Y%m%d"),'/')]
    colnames(sig) <- 'LLT'
    return(sig)
}

revtm <- function(indexcode = '000300.SH', stadate = '20171230',
                  enddate = '20180421',freq = 'days',m=750,n=15,k=0.56) {
    
    truestadate <- as.Date(stadate, format="%Y%m%d")-ceiling((m+n)*1.5)
    truestadate <- gsub('-','',as.character(truestadate))
    
    query <- paste0('select TRADE_DT,S_DQ_CLOSE from aindexeodprices
                    where S_INFO_WINDCODE = \'',indexcode,'\' and TRADE_DT >= \'',truestadate,'\'  
                    and TRADE_DT <= \'',enddate,'\' order by TRADE_DT;')
    indexdata <- dbGetQuery(conwind, query)
    indexdata[,1] <- as.Date(indexdata[,1], format="%Y%m%d")
    
    tradedays <- indexdata[,1]
    indexquote <- xts(indexdata[,-1],indexdata[, 1])
    
    onret <- diff(indexquote)/lag(indexquote)
    sig <- onret
    sig[] <- NaN
    for (ii in (m+3):length(onret)) {
        sorttemp <- sort(as.numeric(onret[(ii-m-2):(ii-2)]))
        sig[ii] <- which.min(as.numeric(onret[ii-1])>sorttemp)/m
    }
    sig[sig > k] <- 1
    sig[sig <= k] <- 0
    
    sig <- sig[paste0(as.Date(stadate, format="%Y%m%d"),'/')]
    colnames(sig) <- 'revtm'
    return(sig)
}

RSRS <- function(indexcode = '000300.SH', stadate = '20171230',
                 enddate = '20180421',freq = 'days',m=750,n=15,k=1) {
    
    truestadate <- as.Date(stadate, format="%Y%m%d")-ceiling((m+n)*1.5)
    truestadate <- gsub('-','',as.character(truestadate))
    
    query <- paste0('select TRADE_DT,S_DQ_LOW,S_DQ_HIGH from aindexeodprices
                    where S_INFO_WINDCODE = \'',indexcode,'\' and TRADE_DT >= \'',truestadate,'\'  
                    and TRADE_DT <= \'',enddate,'\' order by TRADE_DT;')
    indexdata <- dbGetQuery(conwind, query)
    indexdata[,1] <- as.Date(indexdata[,1], format="%Y%m%d")
    
    tradedays <- indexdata[,1]
    indexquote <- xts(indexdata[, -1],indexdata[, 1])
    
    betaR2 <- indexquote
    betaR2[,] <- NaN
    colnames(betaR2) <- c('beta','R2')
    for (ii in (n+1):dim(indexquote)[1]) {
        indexlm <- lm(S_DQ_HIGH~.,data=indexquote[(ii-15):(ii-1),])
        lmsmy <- summary(indexlm)
        betaR2[ii,1] <- coef(indexlm)[2]
        betaR2[ii,2] <- lmsmy$adj.r.squared
    }
    
    betaR2 <- betaR2[!is.na(betaR2[,1]),]
    adjscore <- betaR2$beta
    adjscore[] <- NaN
    for (ii in (m+1):dim(betaR2)[1]) {
        betamean <- mean(betaR2[(ii-m):(ii-1),1])
        betastd <- sd(betaR2[(ii-m):(ii-1),1])
        adjscore[ii] <- (betaR2[ii,1]-betamean)/betastd*betaR2[ii,2]
    }
    adjscore <- adjscore[!is.na(adjscore)]
    
    sig <- adjscore
    sig[] <- NaN
    sig[1] <- sign(adjscore[1])/2+0.5
    sig[which(adjscore > k)] <- 1
    sig[which(adjscore < -k)] <- 0
    sig <- na.locf(sig)
    sig <- sig[paste0(as.Date(stadate, format="%Y%m%d"),'/')]
    colnames(sig) <- 'RSRS'
    return(sig)
}

totaldf <- cbind(Double_MA(stadate=stadate,enddate = enddate),
                      dmi_kdj_emv(stadate=stadate,enddate = enddate),
                      GFTD(stadate=stadate,enddate = enddate),
                      LLT(stadate=stadate,enddate = enddate),
                      revtm(stadate=stadate,enddate = enddate),
                      RSRS(stadate=stadate,enddate = enddate))
ep1 <- endpoints(totaldf,'weeks')
totaldfweek <- round(period.apply(totaldf,ep1,FUN = mean), digits = 0)

ep2 <- endpoints(totaldf,'months')
totaldfmonth <- round(period.apply(totaldf,ep2,FUN = mean), digits = 0)




write.xlsx(as.data.frame(totaldf),'C:\\Users\\Administrator\\Desktop\\Backtest_index_Matlab\\Data\\daytiming.xlsx')
write.xlsx(as.data.frame(totaldfweek),'C:\\Users\\Administrator\\Desktop\\Backtest_index_Matlab\\Data\\weektiming.xlsx')
write.xlsx(as.data.frame(totaldfmonth),'C:\\Users\\Administrator\\Desktop\\Backtest_index_Matlab\\Data\\monthtiming.xlsx')

# -----------------
query <- 'select TRADE_DT, S_DQ_CLOSE from aindexeodprices
where S_INFO_WINDCODE =\'000300.SH\'
order by TRADE_DT;'
hs300 <- dbGetQuery(conwind, query)
hs300[,1] <- as.Date(hs300[,1],format="%Y%m%d")
write.xlsx(hs300,'C:\\Users\\Administrator\\Desktop\\Backtest_index_Matlab\\Data\\HS300.xlsx',row.names = F)





