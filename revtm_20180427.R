# initial settings ----------------------

setwd('C:\\Users\\Administrator\\Desktop\\Timing_20180427')

library(RMySQL)
library(lubridate)
library(xts)


conwind <- dbConnect(MySQL(), host='rm-2zey1z6px42nits51.mysql.rds.aliyuncs.com',
                     port=3306,dbname="wind", username="hulitingali", password="Hu.lt@2018")


# input -------------------

indexcode <- '000300.SH'
stadate <- '20171230' # date included
enddate <- '20180421'
freq <- 'days' # days,weeks,months,quarters,years


# define function  ----------------

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

revtm()

