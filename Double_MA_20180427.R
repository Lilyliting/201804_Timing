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

Double_MA()



