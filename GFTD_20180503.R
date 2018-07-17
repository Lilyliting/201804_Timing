# initial settings ----------------------

setwd('C:\\Users\\Administrator\\Desktop\\Timing_20180427')

library(RMySQL)
library(lubridate)
library(xts)
library(rJava)
library(xlsx)


conwind <- dbConnect(MySQL(), host='rm-2zey1z6px42nits51.mysql.rds.aliyuncs.com',
                     port=3306,dbname="wind", username="hulitingali", password="Hu.lt@2018")


# input -------------------

indexcode <- '000300.SH'
stadate <- '20060101' # date included
todaydate <- format(today(),'%Y%m%d')
freq <- 'days' # days,weeks,months,quarters,years
bn1=5
bn2=5
bn3=4
sn1=5
sn2=4
sn3=5
smo=10
# define function  ----------------

GFTD <- function(indexcode = '000300.SH', stadate = '20171230',
                 enddate = '20180421',freq = 'days',bn1=5,bn2=5,bn3=4,
                 sn1=5,sn2=4,sn3=5,smo=10) {
    
    truestadate <- as.Date(stadate, format="%Y%m%d")-30
    truestadate <- gsub('-','',as.character(truestadate))
    
    query <- paste0('select TRADE_DT,S_DQ_CLOSE,S_DQ_HIGH,S_DQ_LOW from aindexeodprices
                    where S_INFO_WINDCODE = \'',indexcode,'\' and TRADE_DT >= \'',truestadate,'\'  
                    and TRADE_DT <= \'',enddate,'\' order by TRADE_DT;')
    indexdata <- dbGetQuery(conwind, query)
    indexdata[,1] <- as.Date(indexdata[,1], format="%Y%m%d")
    
    tradedays <- indexdata[,1]
    indexquote <- xts(indexdata[, -1],indexdata[, 1])
    
    # sig <- xts(indexquote[,1],tradedays+1)
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
    
    ii=2
    while (ii < (length(sig))) {
        if (sig[ii] == 1 && sig[ii-1] == 0) {
            sig[ii:min(length(sig),(ii+smo-1))] <- 1
            ii <- ii+smo
            if (ii>(length(sig))) {break}
        } else {
            ii <- ii+1
        }
    }
    sig <- sig[paste0(as.Date(stadate, format="%Y%m%d"),'/')]
    colnames(sig) <- 'GFTD'
    return(sig)
}

totaltiming <- GFTD(indexcode = '399006.SZ',stadate = '20060101',enddate = todaydate,smo=10)
colnames(totaltiming) <- 'GFTD'
# for (ii in 3:5) {
#     for(jj in 3:5) {
#         for (kk in 3:5) {
#             timing <- GFTD(stadate = '20060101',enddate = '20180601',bn1=ii,bn2=jj,bn3=kk,sn2=4,smo=10)
#             colnames(timing) <- paste0(ii,jj,kk)
#             print(colnames(timing))
#             totaltiming <- merge(totaltiming,timing)
#         }
#     }
# }



timingdf <- as.data.frame(totaltiming)
tail(timingdf)
# write.xlsx(timingdf,'gftdtimimg.xlsx')
write.csv(timingdf,'C:\\Users\\Administrator\\Desktop\\StockorIndustry_yo_20180608\\Data\\gftdtimimg.csv')
