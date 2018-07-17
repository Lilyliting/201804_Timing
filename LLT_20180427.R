# initial settings ----------------------

setwd('C:\\Users\\Administrator\\Desktop\\Timing_20180427')

library(RMySQL)
library(lubridate)
library(xts)


conwind <- dbConnect(MySQL(), host='rm-2zey1z6px42nits51.mysql.rds.aliyuncs.com',
                     port=3306,dbname="wind", username="hulitingali", password="Hu.lt@2018")


# input -------------------

indexcode <- '000300.SH'
stadate <- '20060101' # date included
enddate <- '20180601'
freq <- 'days' # days,weeks,months,quarters,years

# define function  ----------------

LLT <- function(indexcode = '000300.SH', stadate= '20171230',
                  enddate = '20180421',freq = 'days',alpha=1/40,smo=10) {
    alpha = 0.1
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
    plot(merge(llt,indexquote))
    sig <- (llt>lag(llt))*1
    sig <- sig[-1,]
    
    sig[1:smo] <- as.numeric(sig[1])
    sigl <- as.numeric(sig[(smo+1)])
    ii=smo+2
    while (ii < (length(sig))) {
        if (sig[ii] != sigl && sigl==0) {
            print(ii)
            sig[ii:min(length(sig),(ii+smo-1))] <- as.numeric(sig[ii])
            ii <- ii+smo
            if (ii>(length(sig))) {break}
            sigl <- as.numeric(sig[ii])
        } else {
            ii <- ii+1
        }
    }
    ii
    sig <- sig[paste0(as.Date(stadate, format="%Y%m%d"),'/')]
    colnames(sig) <- 'LLT'
    return(sig)
}

# A=LLT(stadate=stadate)
# table(A)

timingllt <- LLT(stadate = '20060101',enddate = '20180601',alpha=1/50,smo = 10)
lltdf <- as.data.frame(timingllt)
write.xlsx(lltdf,'llttimimg.xlsx')

