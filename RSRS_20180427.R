# initial settings ----------------------

setwd('C:\\Users\\Administrator\\Desktop\\Timing_20180427')

library(RMySQL)
library(lubridate)
library(xts)


conwind <- dbConnect(MySQL(), host='rm-2zey1z6px42nits51.mysql.rds.aliyuncs.com',
                     port=3306,dbname="wind", username="hulitingali", password="Hu.lt@2018")


# input -------------------

indexcode <- '000300.SH'
stadate <- '20090101' # date included
enddate <- '20180421'
freq <- 'days' # days,weeks,months,quarters,years

n <- 15
m <- 750
k <- 1
#参数设置
#A股 n=15,m=750,k=1
#港股 n=10 m=750 k=0.7
#美股 n=10 m=250 k=0.8

# define function  ----------------

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

timingrsrs <- RSRS(stadate = '20091230',enddate = '20180511')
rsrsdf <- as.data.frame(timingrsrs)
write.csv(rsrsdf,file = 'C:\\Users\\Administrator\\Desktop\\Periodly_invest_20180502\\Data\\rsrstiming.csv')
