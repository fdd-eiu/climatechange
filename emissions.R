# Fishing vessel emissions Analyisis

# Need to access FAOSTAT for emissions of other sectors for comparison
# Need to access ACE database
# Need to define some constants from BEIS reports CO@ and other emissions for fuel
# need to define FUI and refinery constants
rm(list=ls())
library(FAOSTAT)
library("RPostgreSQL")
library("sqldf")
library(tidyverse)
library(flextable)


# Functions

#calculate emissions intensity

emissionsintensity <- function(fui){
     convert <- 0.91 # marine diesel from bbloe
     co2 <-  3.1 #kg CO2e per litre  
     #ei<- convert*fui*co2/1000  # 0.91 l/mt kg cO2e /1000 to convert to tonnes per mt fish caught
     ei<- fui*co2/1000 
  return(ei)
}

#calculate meissions total

emissionstotal <- function(fui,catch){
    convert <- 0.91
    co2 <-   3.1
    #ei<- convert*fui*catch*co2/1000
    ei<- fui*catch*co2/1000
  return(ei)
  
}

#FAOsearch(dataset="emissions",full=FALSE)

fao_metadata <- FAOsearch(dataset='emissions',full=TRUE)

fao_metadata

data_folder <- "data_raw"

dir.create(data_folder)
df.ei <- get_faostat_bulk(code="EI",data_folder=data_folder) #emissions intensity

df.ei

# Extract emissions intensities for each member country

df.ck <- df.ei %>% filter(area == "Cook Islands",na.rm = TRUE)
df.fm <- df.ei %>% filter(area == "Micronesia",na.rm = TRUE)
df.fj <- df.ei %>% filter(area == "Fiji",na.rm = TRUE)
df.ki <- df.ei %>% filter(area == "Kiribati",na.rm = TRUE) #returns no data
# Marshall Islands not found
df.nu <- df.ei %>% filter(area == "Nauru",na.rm = TRUE)
df.ni <- df.ei %>% filter(area == "Niue",na.rm = TRUE)
df.pg <- df.ei %>% filter(area == "Papua New Guinea",na.rm = TRUE) #no data
df.pw <- df.ei %>% filter(area == "Palau",na.rm = TRUE) #no data
df.ws <- df.ei %>% filter(area == "Samoa",na.rm = TRUE) #no data
df.sb <- df.ei %>% filter(area == "Solomon Islands",na.rm = TRUE) #no data
df.tk <- df.ei %>% filter(area == "Tokelau",na.rm = TRUE) #no data
df.to <- df.ei %>% filter(area == "Tongo",na.rm = TRUE) #no data
df.tu <- df.ei %>% filter(area == "Tuvalu",na.rm = TRUE) #no data
df.vu <- df.ei %>% filter(area == "Vanuatu",na.rm = TRUE) #no data

# Calculate some summary statistics and tables

df.world <- df.ei %>% filter(area == "World",na.rm = TRUE)
wld.cattle <- df.world %>% filter(item == 'Meat, cattle')
wld.pig <- df.world %>% filter(item == 'Meat, pig')
wld.chicken <- df.world %>% filter(item == 'Meat, chicken')
wld.goat <- df.world %>% filter(item == 'Meat, goat')

# Members

ck <- df.ck %>% select(year, value, item)
ck.cattle <- ck %>% filter(item == 'Meat, cattle')
ck.pig <- ck %>% filter(item == 'Meat, pig') 
ck.chicken <- ck %>% filter(item == 'Meat, chicken') 
ck.goat <- ck %>% filter(item == 'Meat, goat') 


fm <- df.fm %>% select(year, value, item)
fm.cattle <- fm %>% filter(item == 'Meat, cattle')
fm.pig <- fm %>% filter(item == 'Meat, pig') 
fm.chicken <- fm %>% filter(item == 'Meat, chicken') 
fm.goat <- fm %>% filter(item == 'Meat, goat') 

fj <- df.fj %>% select(year, value, item)
fj.cattle <- fj %>% filter(item == 'Meat, cattle')
fj.pig <- fj %>% filter(item == 'Meat, pig') 
fj.chicken <- fj %>% filter(item == 'Meat, chicken') 
fj.goat <- fj %>% filter(item == 'Meat, goat') 

ki <- df.ki %>% select(year, value, item)
ki.cattle <- ki %>% filter(item == 'Meat, cattle')
ki.pig <- ki %>% filter(item == 'Meat, pig') 
ki.chicken <- ki %>% filter(item == 'Meat, chicken') 
ki.goat <- ki %>% filter(item == 'Meat, goat') 

nu <- df.nu %>% select(year, value, item)
nu.cattle <- nu %>% filter(item == 'Meat, cattle')
nu.pig <- nu %>% filter(item == 'Meat, pig') 
nu.chicken <- nu %>% filter(item == 'Meat, chicken') 
nu.goat <- nu %>% filter(item == 'Meat, goat') 

ni <- df.ni %>% select(year, value, item)
ni.cattle <- ni %>% filter(item == 'Meat, cattle')
ni.pig <- ni %>% filter(item == 'Meat, pig') 
ni.chicken <- ni %>% filter(item == 'Meat, chicken') 
ni.goat <- ni %>% filter(item == 'Meat, goat') 

pg <- df.pg %>% select(year, value, item)
pg.cattle <- pg %>% filter(item == 'Meat, cattle')
pg.pig <- pg %>% filter(item == 'Meat, pig') 
pg.chicken <- pg %>% filter(item == 'Meat, chicken') 
pg.goat <- pg %>% filter(item == 'Meat, goat') 

pw <- df.pw %>% select(year, value, item)
pw.cattle <- pw %>% filter(item == 'Meat, cattle')
pw.pig <- pw %>% filter(item == 'Meat, pig') 
pw.chicken <- pw %>% filter(item == 'Meat, chicken') 
pw.goat <- pw %>% filter(item == 'Meat, goat') 

ws <- df.ws %>% select(year, value, item)
ws.cattle <- ws %>% filter(item == 'Meat, cattle')
ws.pig <- ws %>% filter(item == 'Meat, pig') 
ws.chicken <- ws %>% filter(item == 'Meat, chicken') 
ws.goat <- ws %>% filter(item == 'Meat, goat') 

sb <- df.sb %>% select(year, value, item)
sb.cattle <- sb %>% filter(item == 'Meat, cattle')
sb.pig <- sb %>% filter(item == 'Meat, pig') 
sb.chicken <- sb %>% filter(item == 'Meat, chicken') 
sb.goat <- sb %>% filter(item == 'Meat, goat') 

tk <- df.tk %>% select(year, value, item)
tk.cattle <- tk %>% filter(item == 'Meat, cattle')
tk.pig <- tk %>% filter(item == 'Meat, pig') 
tk.chicken <- tk %>% filter(item == 'Meat, chicken') 
tk.goat <- tk %>% filter(item == 'Meat, goat') 

to <- df.to %>% select(year, value, item)
to.cattle <- to %>% filter(item == 'Meat, cattle')
to.pig <- to %>% filter(item == 'Meat, pig') 
to.chicken <- to %>% filter(item == 'Meat, chicken') 
to.goat <- to %>% filter(item == 'Meat, goat') 

tu <- df.tu %>% select(year, value, item)
tu.cattle <- tu %>% filter(item == 'Meat, cattle')
tu.pig <- tu %>% filter(item == 'Meat, pig') 
tu.chicken <- tu %>% filter(item == 'Meat, chicken') 
tu.goat <- tu %>% filter(item == 'Meat, goat') 

vu <- df.vu %>% select(year, value, item)
vu.cattle <- vu %>% filter(item == 'Meat, cattle')
vu.pig <- vu %>% filter(item == 'Meat, pig') 
vu.chicken <- vu %>% filter(item == 'Meat, chicken') 
vu.goat <- vu %>% filter(item == 'Meat, goat') 

# need to add more countries (world)


flextable(ck,col_keys = c('year', 'value'))

# Import data from the annual catch estimates database
# and calculate CO2 estimates and intensitites per kg caught.
# Break this down by gear and flag
# Is there a difference between particular flags
# For reporting combine all FFA flags to avoid comparisons between members/


#There are approximately 1,184 l marine diesel in one ton oil equivalent, 
#and 6.8412 barrels in one ton of oil . So this gives us 1,150/6.8412 l/bbl 
#marine diesel per barrel. Or 173 l/bbloe (barrels oil equivalent) of marine diesel. 
# One barrel of oil equates to X barrels of marine diesel where X is our conversion factor 
# for converting barrels of oil to barrels of marine diesel. X can be found by dividing 
#bbloe/bblmd which is 159/173 using 2021 conversion factors for l marine diesel per toe. 
# Resulting in an X of 91%.


# Read catch data

pw <- {
  "XXX"
} #password for database 


#library(DBI)
#library(RPostgres)
#con <- dbConnect(RPostgres::Postgres(), host = url$host, port = url$port, dbname = url$dbname, user = url$user, password = url$password#
#)

# loads the PostgreSQL driver

drv <- dbDriver("PostgreSQL")

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database

con = dbConnect(drv = drv, dbname = "xxx", host = "xxx", port = xxxx,
                 user = "xxx", password = pw) #Credentials must be priovided

print(con)

# query the data from postgreSQL 
df_postgres <- dbGetQuery(con, "SELECT * FROM ACE;")

rm(pw)

allcons <- dbListConnections(PostgreSQL())
for (con in allcons) {
  dbDisconnect(con)
}

dbDisconnect(con)
dbUnloadDriver(drv)


# Read in data
# Convert dataframe to time series object

df <- as.data.frame(df_postgres)

#data <- ts(df,start=c(1997),end=c(2021),frequency=1) # need to update end year


# Compare the emissions intensities between agriculture and fisheries by flag.

df2 <- df %>% select(yr, gr_id, flag_id, ez_id, skj_c, yft_c, bet_c, alb_c)

df2

# filter by gear

df.PS <- df2 %>% filter(gr_id == "P")
df.LL <- df2 %>% filter(gr_id == "L")

#emissionsintensity(fui)
#emissionstotal(fui,catch)

# filter by flag

##FFA

# Longline

df.ck  <- df2 %>% filter(flag_id == "CK" &  gr_id == "L")
df.ck_total <- df.ck[,5:8] %>% rowSums() # need to think about unit testing this
ck.total <- cbind(df.ck["yr"],df.ck_total)
df.fm  <- df2 %>% filter(flag_id == "FM" &  gr_id == "L")
df.fm_total <- df.fm[,5:8] %>% rowSums() # need to think about unit testing this
fm.total <- cbind(df.fm["yr"],df.fm_total)
df.fj  <- df2 %>% filter(flag_id == "FJ" &  gr_id == "L")
df.fj_total <- df.fj[,5:8] %>% rowSums() # need to think about unit testing this
fj.total <- cbind(df.fj["yr"],df.fj_total) 
df.ki  <- df2 %>% filter(flag_id == "KI" &  gr_id == "L")
df.ki_total <- df.ki[,5:8] %>% rowSums() # need to think about unit testing this
ki.total <- cbind(df.ki["yr"],df.ki_total)
#df.mi  <- df2 %>% filter(flag_id == "MI" &  gr_id == "L")
#df.mi_total <- df.mi[,5:8] %>% rowSums() # need to think about unit testing this
#mi.total <- cbind(df.mi["yr"],df.mi_total)
df.nu  <- df2 %>% filter(flag_id == "NU" &  gr_id == "L")
df.nu_total <- df.nu[,5:8] %>% rowSums() # need to think about unit testing this
nu.total <- cbind(df.nu["yr"],df.nu_total)
df.ni  <- df2 %>% filter(flag_id == "NI" &  gr_id == "L")
df.ni_total <- df.ni[,5:8] %>% rowSums() # need to think about unit testing this
ni.total <- cbind(df.ni["yr"],df.ni_total)
df.pw  <- df2 %>% filter(flag_id == "PW" &  gr_id == "L")
df.pw_total <- df.pw[,5:8] %>% rowSums() # need to think about unit testing this
pw.total <- cbind(df.pw["yr"],df.pw_total)
df.pg  <- df2 %>% filter(flag_id == "PG" &  gr_id == "L")
df.pg_total <-  df.pg[,5:8] %>% rowSums() # need to think about unit testing this
pg.total <- cbind(df.pg["yr"],df.pg_total)
df.ws  <- df2 %>% filter(flag_id == "WS" &  gr_id == "L")
df.ws_total <- df.ws[,5:8] %>% rowSums() # need to think about unit testing this
ws.total <- cbind(df.ws["yr"],df.ws_total)  
df.sb  <- df2 %>% filter(flag_id == "SB" &  gr_id == "L")
df.sb_total <- df.sb[,5:8] %>% rowSums() # need to think about unit testing this
sb.total <- cbind(df.sb["yr"],df.sb_total)
df.tk  <- df2 %>% filter(flag_id == "TK" &  gr_id == "L")
df.tk_total <- df.tk[,5:8] %>% rowSums() # need to think about unit testing this
tk.total <- cbind(df.tk["yr"],df.tk_total)
df.to  <- df2 %>% filter(flag_id == "TO" &  gr_id == "L")
df.to_total <- df.to[,5:8] %>% rowSums() # need to think about unit testing this
to.total <- cbind(df.to["yr"],df.to_total)
df.tv  <- df2 %>% filter(flag_id == "TV" &  gr_id == "L")
df.tv_total <- df.tv[,5:8] %>% rowSums() # need to think about unit testing this
tv.total <- cbind(df.tv["yr"],df.tv_total)
df.vu  <- df2 %>% filter(flag_id == "VU" &  gr_id == "L")
df.vu_total <- df.vu[,5:8] %>% rowSums() # need to think about unit testing this
vu.total <- cbind(df.vu["yr"],df.vu_total)

#Note there is more than one record per year, because we are calculating emissions by flag 
# and there is variation by zone, so fishing occurs in different zones in a given year. This suggests a question as to how concentrated 
# a particular  fleet is in terms of fishing in particular zones.

# Following computes total emissions for 2019

ck.2019 <- ck.total %>% filter(yr == "2019-01-10")
ck.2019_total <- sum(ck.2019["df.ck_total"])
fm.2019 <- fm.total %>% filter(yr == "2019-01-10")
fm.2019_total <- sum(fm.2019["df.fm_total"])
fj.2019 <- fj.total %>% filter(yr == "2019-01-10")
fj.2019_total <- sum(fj.2019["df.fj_total"])
ki.2019 <- ki.total %>% filter(yr == "2019-01-10")
ki.2019_total <- sum(ki.2019["df.ki_total"])
#mi.2019 <- mi.total %>% filter(yr == "2019-01-10")
#mi.2019_total <- sum(mi.2019["df.mi_total"])
#nu.2019 <- nu.total %>% filter(yr == "2019-01-10")
#nu.2019_total <- sum(nu.2019["df.nu_total"])
#ni.2019 <- ni.total %>% filter(yr == "2019-01-10")
#ni.2019_total <- sum(ni.2019["df.ni_total"])
pw.2019 <- pw.total %>% filter(yr == "2019-01-10")
pw.2019_total <- sum(pw.2019["df.pw_total"])
#pg.2019 <- pg.total %>% filter(yr == "2019-01-10")
#pg.2019_total <- sum(pg.2019["df.pg_total"])
ws.2019 <- ws.total %>% filter(yr == "2019-01-10")
ws.2019_total <- sum(ws.2019["df.ws_total"])
sb.2019 <- sb.total %>% filter(yr == "2019-01-10")
sb.2019_total <- sum(sb.2019["df.sb_total"])
#tk.2019 <- tk.total %>% filter(yr == "2019-01-10")
#tk.2019_total <- sum(tk.2019["df.tk_total"])
to.2019 <- to.total %>% filter(yr == "2019-01-10")
to.2019_total <- sum(to.2019["df.to_total"])
tv.2019 <- tv.total %>% filter(yr == "2019-01-10")
tv.2019_total <- sum(tv.2019["df.tv_total"])
vu.2019 <- vu.total %>% filter(yr == "2019-01-10")
vu.2019_total <- sum(vu.2019["df.vu_total"])

ffamean.2019 <- mean(ck.2019_total,fm.2019_total,fj.2019_total,ki.2019_total,
                                  pw.2019_total,ws.2019_total,sb.2019_total,
                                  to.2019_total,tv.2019_total,vu.2019_total)  

ffatotal_LL.2019 <- sum(ck.2019_total,fm.2019_total,fj.2019_total,ki.2019_total,
                    pw.2019_total,ws.2019_total,sb.2019_total,
                    to.2019_total,tv.2019_total,vu.2019_total)  

globalemissions <- 3.3*10^10 #gigatonness in 2019 source: https://www.iea.org/articles/global-co2-emissions-in-2019

percentage.LL <- (ffatotal_LL.2019/globalemissions)*100 # percentage as percent of global CO@ emissions


## DWFN

# https://stackoverflow.com/questions/5831794/opposite-of-in-exclude-rows-with-values-specified-in-a-vector#5831829
# Flag based

members <- c("CK","FM","FJ","KI","MI","NU","NI","PW","PG","WS","SB","TK","TO","TV","VU")

dwfn0 <-filter(df2,!flag_id %in% members) #select DWFN by removing FFA not including ANZ

ANZ <- c("AU","NZ")

dwfn <- filter(dwfn0,!flag_id %in% ANZ) #select DWFN nations by removing ANZ

#next doesn't work
#for (i in unique(dwfn["flag_id"])){
 # df.tmp  <- dwfn %>% filter(flag_id == i &  gr_id == "L")
#  df.tmp_total<- df.tmp[,5:8] %>% rowSums()  
 # df.tmp_total
#}

dwfn.flags<- unique(dwfn["flag_id"])

#Longline total catches

df.jp  <- dwfn %>% filter(flag_id == "JP" &  gr_id == "L")
df.jp_total <- df.jp[,5:8] %>% rowSums() 
jp.total <- cbind(df.jp["yr"],df.jp_total)
df.tw  <- dwfn %>% filter(flag_id == "TW" &  gr_id == "L")
df.tw_total <- df.tw[,5:8] %>% rowSums() 
tw.total <- cbind(df.tw["yr"],df.tw_total)
df.id  <- dwfn %>% filter(flag_id == "ID" &  gr_id == "L")
df.id_total <- df.id[,5:8] %>% rowSums() 
id.total <- cbind(df.id["yr"],df.id_total)
df.ph  <- dwfn %>% filter(flag_id == "PH" &  gr_id == "L")
df.ph_total <- df.ph[,5:8] %>% rowSums() 
ph.total <- cbind(df.ph["yr"],df.ph_total)
df.bz  <- dwfn %>% filter(flag_id == "BZ" &  gr_id == "L")
df.bz_total <- df.bz[,5:8] %>% rowSums() 
bz.total <- cbind(df.bz["yr"],df.bz_total)
df.cn  <- dwfn %>% filter(flag_id == "CN" &  gr_id == "L")
df.cn_total <- df.cn[,5:8] %>% rowSums() 
cn.total <- cbind(df.cn["yr"],df.cn_total)
df.kr  <- dwfn %>% filter(flag_id == "KR" &  gr_id == "L")
df.kr_total <- df.kr[,5:8] %>% rowSums() 
kr.total <- cbind(df.kr["yr"],df.kr_total)
df.nc  <- dwfn %>% filter(flag_id == "NC" &  gr_id == "L")
df.nc_total <- df.nc[,5:8] %>% rowSums() 
nc.total <- cbind(df.nc["yr"],df.nc_total)
df.pf  <- dwfn %>% filter(flag_id == "PF" &  gr_id == "L")
df.pf_total <- df.pf[,5:8] %>% rowSums() 
pf.total <- cbind(df.pf["yr"],df.pf_total)
df.us  <- dwfn %>% filter(flag_id == "US" &  gr_id == "L")
df.us_total <- df.us[,5:8] %>% rowSums() 
us.total <- cbind(df.us["yr"],df.us_total)
df.su  <- dwfn %>% filter(flag_id == "SU" &  gr_id == "L")
df.su_total <- df.su[,5:8] %>% rowSums() 
su.total <- cbind(df.su["yr"],df.su_total)
df.ca  <- dwfn %>% filter(flag_id == "CA" &  gr_id == "L")
df.ca_total <- df.ca[,5:8] %>% rowSums() 
ca.total <- cbind(df.ca["yr"],df.ca_total)
df.mh  <- dwfn %>% filter(flag_id == "MH" &  gr_id == "L")
df.mh_total <- df.mh[,5:8] %>% rowSums()
mh.total <- cbind(df.mh["yr"],df.mh_total)
df.ep  <- dwfn %>% filter(flag_id == "EP" &  gr_id == "L")
df.ep_total <- df.ep[,5:8] %>% rowSums() 
ep.total <- cbind(df.ep["yr"],df.ep_total)
df.es  <- dwfn %>% filter(flag_id == "ES" &  gr_id == "L")
df.es_total <- df.es[,5:8] %>% rowSums() 
es.total <- cbind(df.es["yr"],df.es_total)
df.nr  <- dwfn %>% filter(flag_id == "NR" &  gr_id == "L")
df.nr_total <- df.nr[,5:8] %>% rowSums() 
nr.total <- cbind(df.nr["yr"],df.nr_total)
df.ec  <- dwfn %>% filter(flag_id == "EC" &  gr_id == "L")
df.ec_total <- df.ec[,5:8] %>% rowSums() 
ec.total <- cbind(df.ec["yr"],df.ec_total)
df.sv  <- dwfn %>% filter(flag_id == "SV" &  gr_id == "L")
df.sv_total <- df.sv[,5:8] %>% rowSums() 
sv.total <- cbind(df.sv["yr"],df.sv_total)
df.sn  <- dwfn %>% filter(flag_id == "SN" &  gr_id == "L")
df.sn_total <- df.sn[,5:8] %>% rowSums() 
sn.total <- cbind(df.sn["yr"],df.sn_total)
df.pt  <- dwfn %>% filter(flag_id == "PT" &  gr_id == "L")
df.pt_total <- df.pt[,5:8] %>% rowSums() 
pt.total <- cbind(df.pt["yr"],df.pt_total)
df.wf  <- dwfn %>% filter(flag_id == "WF" &  gr_id == "L")
df.wf_total <- df.wf[,5:8] %>% rowSums() 
wf.total <- cbind(df.wf["yr"],df.wf_total)

# 2019

jp.2019 <- jp.total %>% filter(yr == "2019-01-10")
jp.2019_total <- sum(jp.2019["df.jp_total"])
tw.2019 <- tw.total %>% filter(yr == "2019-01-10")
tw.2019_total <- sum(tw.2019["df.tw_total"])
#id.2019 <- id.total %>% filter(yr == "2019-01-10")
#id.2019_total <- sum(id.2019["df.id_total"])
ph.2019 <- ph.total %>% filter(yr == "2019-01-10")
ph.2019_total <- sum(ph.2019["df.ph_total"])
bz.2019 <- bz.total %>% filter(yr == "2019-01-10")
bz.2019_total <- sum(bz.2019["df.bz_total"])
cn.2019 <- cn.total %>% filter(yr == "2019-01-10")
cn.2019_total <- sum(cn.2019["df.cn_total"])
kr.2019 <- kr.total %>% filter(yr == "2019-01-10")
kr.2019_total <- sum(kr.2019["df.kr_total"])
nc.2019 <- nc.total %>% filter(yr == "2019-01-10")
nc.2019_total <- sum(nc.2019["df.nc_total"])
pf.2019 <- pf.total %>% filter(yr == "2019-01-10")
pf.2019_total <- sum(pf.2019["df.pf_total"])
us.2019 <- us.total %>% filter(yr == "2019-01-10")
us.2019_total <- sum(us.2019["df.us_total"])
#su.2019 <- su.total %>% filter(yr == "2019-01-10")
#su.2019_total <- sum(su.2019["df.su_total"])
#ca.2019 <- ca.total %>% filter(yr == "2019-01-10")
#ca.2019_total <- sum(ca.2019["df.ca_total"])
mh.2019 <- mh.total %>% filter(yr == "2019-01-10")
mh.2019_total <- sum(mh.2019["df.mh_total"])
#ep.2019 <- ep.total %>% filter(yr == "2019-01-10")
#ep.2019_total <- sum(ep.2019["df.ep_total"])
es.2019 <- es.total %>% filter(yr == "2019-01-10")
es.2019_total <- sum(es.2019["df.es_total"])
nr.2019 <- wf.total %>% filter(yr == "2019-01-10")
nr.2019_total <- sum(nr.2019["df.wf_total"])
#ec.2019 <- ec.total %>% filter(yr == "2019-01-10")
#ec.2019_total <- sum(ec.2019["df.ec_total"])
#sv.2019 <- sv.total %>% filter(yr == "2019-01-10")
#sv.2019_total <- sum(sv.2019["df.sv_total"])
sn.2019 <- sn.total %>% filter(yr == "2019-01-10")
sn.2019_total <- sum(sn.2019["df.sn_total"])
pt.2019 <- pt.total %>% filter(yr == "2019-01-10")
pt.2019_total <- sum(pt.2019["df.pt_total"])
wf.2019 <- wf.total %>% filter(yr == "2019-01-10")
wf.2019_total <- sum(wf.2019["df.wf_total"])


# Aggregate FFA members

#ffamean <- rowMeans(matrix(c(df.ck_total,df.fm_total,df.fj_total,df.ki_total,df.mi_total,df.nu_total,
 # df.ni_total,df.pw_total,df.pg_total,df.ws_total,df.sb_total,df.tk_total,
  #df.to_total,df.tv_total,df.vu_total)))  #calculate total catch mean each year for FFA  LL need to check 


# Call functions
# need to match fui against literature.
fui.ll <- 1438 #midpoint of FUI
fui.ps <- 349 # Tyedmers 2015
ei.ll <- emissionsintensity(fui.ll)
ei.ps <- emissionsintensity(fui.ps)


# Graphs and Tables

# considering a treemap here.see R package treemap.https://www.r-graph-gallery.com/treemap.html

# Plot comparing emssions between DWFN and FFA average

library(RColorBrewer)
coul <- brewer.pal(6, "Set3") 
em <- c(cbind(jp.2019_total,tw.2019_total,kr.2019_total,cn.2019_total,us.2019_total,ffamean.2019))
barplot(em,
        names.arg=c('JP','TW','KR','CN','US','FF'),ylab="Emissions",col=coul)
title('Longline total emissions 2019')

# Plot comparing DWFN and FFA total emissions


coul <- brewer.pal(6, "Set2")
em2 <- c(cbind(jp.2019_total,tw.2019_total,kr.2019_total,cn.2019_total,us.2019_total,ffatotal_LL.2019))
barplot(em2,
        names.arg=c('JP','TW','KR','CN','US','FF'),col= coul)
title('Longline total emissions 2019')


# Plot comparison of fisheries emissions and key agricultural emissions from members


############################
#   Purse-seine            #
############################

df.PS <- df2 %>% filter(gr_id == "P")

df.ck  <- df.PS %>% filter(flag_id == "CK")
df.ck_total <- df.ck[,5:8] %>% rowSums() # need to think about unit testing this
ck.total <- cbind(df.ck["yr"],df.ck_total)
df.fm  <- df.PS %>% filter(flag_id == "FM")
df.fm_total <- df.fm[,5:8] %>% rowSums() # need to think about unit testing this
fm.total <- cbind(df.fm["yr"],df.fm_total)
df.fj  <- df.PS %>% filter(flag_id == "FJ")
df.fj_total <- df.fj[,5:8] %>% rowSums() # need to think about unit testing this
fj.total <- cbind(df.fj["yr"],df.fj_total) 
df.ki  <- df.PS %>% filter(flag_id == "KI")
df.ki_total <- df.ki[,5:8] %>% rowSums() # need to think about unit testing this
ki.total <- cbind(df.ki["yr"],df.ki_total)
#df.mi  <- df.PS %>% filter(flag_id == "MI")
#df.mi_total <- df.mi[,5:8] %>% rowSums() # need to think about unit testing this
#mi.total <- cbind(df.mi["yr"],df.mi_total)
df.nu  <- df.PS %>% filter(flag_id == "NU" )
df.nu_total <- df.nu[,5:8] %>% rowSums() # need to think about unit testing this
nu.total <- cbind(df.nu["yr"],df.nu_total)
df.ni  <- df.PS %>% filter(flag_id == "NI")
df.ni_total <- df.ni[,5:8] %>% rowSums() # need to think about unit testing this
ni.total <- cbind(df.ni["yr"],df.ni_total)
df.pw  <- df.PS %>% filter(flag_id == "PW")
df.pw_total <- df.pw[,5:8] %>% rowSums() # need to think about unit testing this
pw.total <- cbind(df.pw["yr"],df.pw_total)
df.pg  <- df.PS %>% filter(flag_id == "PG")
df.pg_total <-  df.pg[,5:8] %>% rowSums() # need to think about unit testing this
pg.total <- cbind(df.pg["yr"],df.pg_total)
df.ws  <- df.PS %>% filter(flag_id == "WS")
df.ws_total <- df.ws[,5:8] %>% rowSums() # need to think about unit testing this
ws.total <- cbind(df.ws["yr"],df.ws_total)  
df.sb  <- df.PS %>% filter(flag_id == "SB")
df.sb_total <- df.sb[,5:8] %>% rowSums() # need to think about unit testing this
sb.total <- cbind(df.sb["yr"],df.sb_total)
df.tk  <- df.PS %>% filter(flag_id == "TK")
df.tk_total <- df.tk[,5:8] %>% rowSums() # need to think about unit testing this
tk.total <- cbind(df.tk["yr"],df.tk_total)
df.to  <- df.PS %>% filter(flag_id == "TO")
df.to_total <- df.to[,5:8] %>% rowSums() # need to think about unit testing this
to.total <- cbind(df.to["yr"],df.to_total)
df.tv  <- df.PS %>% filter(flag_id == "TV" )
df.tv_total <- df.tv[,5:8] %>% rowSums() # need to think about unit testing this
tv.total <- cbind(df.tv["yr"],df.tv_total)
df.vu  <- df.PS %>% filter(flag_id == "VU" )
df.vu_total <- df.vu[,5:8] %>% rowSums() # need to think about unit testing this
vu.total <- cbind(df.vu["yr"],df.vu_total)

#Note there is more than one record per year, because we are calculating emissions by flag 
# and there is variation by zone, so fishing occurs in different zones in a given year. This suggests a question as to how concentrated 
# a particular  fleet is in terms of fishing in particular zones.

# Following computes total emissions for 2019

ck.2019 <- ck.total %>% filter(yr == "2019-01-10")
#ck.2019_total <- sum(ck.2019["df.ck_total"])
fm.2019 <- fm.total %>% filter(yr == "2019-01-10")
#fm.2019_total <- sum(fm.2019["df.fm_total"])
fj.2019 <- fj.total %>% filter(yr == "2019-01-10")
#fj.2019_total <- sum(fj.2019["df.fj_total"])
ki.2019 <- ki.total %>% filter(yr == "2019-01-10")
ki.2019_total <- sum(ki.2019["df.ki_total"])
#mi.2019 <- mi.total %>% filter(yr == "2019-01-10")
#mi.2019_total <- sum(mi.2019["df.mi_total"])
nu.2019 <- nu.total %>% filter(yr == "2019-01-10")
#nu.2019_total <- sum(nu.2019["df.nu_total"])
ni.2019 <- ni.total %>% filter(yr == "2019-01-10")
#ni.2019_total <- sum(ni.2019["df.ni_total"])
pw.2019 <- pw.total %>% filter(yr == "2019-01-10")
#pw.2019_total <- sum(pw.2019["df.pw_total"])
pg.2019 <- pg.total %>% filter(yr == "2019-01-10")
#pg.2019_total <- sum(pg.2019["df.pg_total"])
ws.2019 <- ws.total %>% filter(yr == "2019-01-10")
#ws.2019_total <- sum(ws.2019["df.ws_total"])
sb.2019 <- sb.total %>% filter(yr == "2019-01-10")
sb.2019_total <- sum(sb.2019["df.sb_total"])
tk.2019 <- tk.total %>% filter(yr == "2019-01-10")
#tk.2019_total <- sum(tk.2019["df.tk_total"])
to.2019 <- to.total %>% filter(yr == "2019-01-10")
#to.2019_total <- sum(to.2019["df.to_total"])
tv.2019 <- tv.total %>% filter(yr == "2019-01-10")
#tv.2019_total <- sum(tv.2019["df.tv_total"])
vu.2019 <- vu.total %>% filter(yr == "2019-01-10")
#vu.2019_total <- sum(vu.2019["df.vu_total"])

ffamean_PS.2019 <- mean(ck.2019_total,fm.2019_total,fj.2019_total,ki.2019_total,
                     pw.2019_total,ws.2019_total,sb.2019_total,
                     to.2019_total,tv.2019_total,vu.2019_total)  

ffatotal_PS.2019 <- sum(ck.2019_total,fm.2019_total,fj.2019_total,ki.2019_total,
                        pw.2019_total,ws.2019_total,sb.2019_total,
                        to.2019_total,tv.2019_total,vu.2019_total)  

globalemissions <- 3.3*10^10 #gigatonness in 2019 source: https://www.iea.org/articles/global-co2-emissions-in-2019

percentage.PS <- (ffatotal_PS.2019/globalemissions)*100 # percentage as percent of global CO@ emissions

#calculate total mean emissions each year for DWFN  PS need to check 

#PS total catches

df.jp  <- dwfn %>% filter(flag_id == "JP" &  gr_id == "P")
df.jp_total <- df.jp[,5:8] %>% rowSums() 
jp.total <- cbind(df.jp["yr"],df.jp_total)
df.tw  <- dwfn %>% filter(flag_id == "TW" &  gr_id == "P")
df.tw_total <- df.tw[,5:8] %>% rowSums() 
tw.total <- cbind(df.tw["yr"],df.tw_total)
df.id  <- dwfn %>% filter(flag_id == "ID" &  gr_id == "P")
df.id_total <- df.id[,5:8] %>% rowSums() 
id.total <- cbind(df.id["yr"],df.id_total)
df.ph  <- dwfn %>% filter(flag_id == "PH" &  gr_id == "P")
df.ph_total <- df.ph[,5:8] %>% rowSums() 
ph.total <- cbind(df.ph["yr"],df.ph_total)
df.bz  <- dwfn %>% filter(flag_id == "BZ" &  gr_id == "P")
df.bz_total <- df.bz[,5:8] %>% rowSums() 
bz.total <- cbind(df.bz["yr"],df.bz_total)
df.cn  <- dwfn %>% filter(flag_id == "CN" &  gr_id == "P")
df.cn_total <- df.cn[,5:8] %>% rowSums() 
cn.total <- cbind(df.cn["yr"],df.cn_total)
df.kr  <- dwfn %>% filter(flag_id == "KR" &  gr_id == "P")
df.kr_total <- df.kr[,5:8] %>% rowSums() 
kr.total <- cbind(df.kr["yr"],df.kr_total)
df.nc  <- dwfn %>% filter(flag_id == "NC" &  gr_id == "P")
df.nc_total <- df.nc[,5:8] %>% rowSums() 
nc.total <- cbind(df.nc["yr"],df.nc_total)
df.pf  <- dwfn %>% filter(flag_id == "PF" &  gr_id == "P")
df.pf_total <- df.pf[,5:8] %>% rowSums() 
pf.total <- cbind(df.pf["yr"],df.pf_total)
df.us  <- dwfn %>% filter(flag_id == "US" &  gr_id == "P")
df.us_total <- df.us[,5:8] %>% rowSums() 
us.total <- cbind(df.us["yr"],df.us_total)
df.su  <- dwfn %>% filter(flag_id == "SU" &  gr_id == "P")
df.su_total <- df.su[,5:8] %>% rowSums() 
su.total <- cbind(df.su["yr"],df.su_total)
df.ca  <- dwfn %>% filter(flag_id == "CA" &  gr_id == "P")
df.ca_total <- df.ca[,5:8] %>% rowSums() 
ca.total <- cbind(df.ca["yr"],df.ca_total)
df.mh  <- dwfn %>% filter(flag_id == "MH" &  gr_id == "P")
df.mh_total <- df.mh[,5:8] %>% rowSums()
mh.total <- cbind(df.mh["yr"],df.mh_total)
df.ep  <- dwfn %>% filter(flag_id == "EP" &  gr_id == "P")
df.ep_total <- df.ep[,5:8] %>% rowSums() 
ep.total <- cbind(df.ep["yr"],df.ep_total)
df.es  <- dwfn %>% filter(flag_id == "ES" &  gr_id == "P")
df.es_total <- df.es[,5:8] %>% rowSums() 
es.total <- cbind(df.es["yr"],df.es_total)
df.nr  <- dwfn %>% filter(flag_id == "NR" &  gr_id == "P")
df.nr_total <- df.nr[,5:8] %>% rowSums() 
nr.total <- cbind(df.nr["yr"],df.nr_total)
df.ec  <- dwfn %>% filter(flag_id == "EC" &  gr_id == "P")
df.ec_total <- df.ec[,5:8] %>% rowSums() 
ec.total <- cbind(df.ec["yr"],df.ec_total)
df.sv  <- dwfn %>% filter(flag_id == "SV" &  gr_id == "P")
df.sv_total <- df.sv[,5:8] %>% rowSums() 
sv.total <- cbind(df.sv["yr"],df.sv_total)
df.sn  <- dwfn %>% filter(flag_id == "SN" &  gr_id == "P")
df.sn_total <- df.sn[,5:8] %>% rowSums() 
sn.total <- cbind(df.sn["yr"],df.sn_total)
df.pt  <- dwfn %>% filter(flag_id == "PT" &  gr_id == "P")
df.pt_total <- df.pt[,5:8] %>% rowSums() 
pt.total <- cbind(df.pt["yr"],df.pt_total)
df.wf  <- dwfn %>% filter(flag_id == "WF" &  gr_id == "P")
df.wf_total <- df.wf[,5:8] %>% rowSums() 
wf.total <- cbind(df.wf["yr"],df.wf_total)

# 2019

jp.2019 <- jp.total %>% filter(yr == "2019-01-10")
jp.2019_total <- sum(jp.2019["df.jp_total"])
tw.2019 <- tw.total %>% filter(yr == "2019-01-10")
#tw.2019_total <- sum(tw.2019["df.tw_total"])
id.2019 <- id.total %>% filter(yr == "2019-01-10")
#id.2019_total <- sum(id.2019["df.id_total"])
ph.2019 <- ph.total %>% filter(yr == "2019-01-10")
#ph.2019_total <- sum(ph.2019["df.ph_total"])
bz.2019 <- bz.total %>% filter(yr == "2019-01-10")
#bz.2019_total <- sum(bz.2019["df.bz_total"])
cn.2019 <- cn.total %>% filter(yr == "2019-01-10")
#cn.2019_total <- sum(cn.2019["df.cn_total"])
kr.2019 <- kr.total %>% filter(yr == "2019-01-10")
#kr.2019_total <- sum(kr.2019["df.kr_total"])
nc.2019 <- nc.total %>% filter(yr == "2019-01-10")
#nc.2019_total <- sum(nc.2019["df.nc_total"])
pf.2019 <- pf.total %>% filter(yr == "2019-01-10")
pf.2019_total <- sum(pf.2019["df.pf_total"])
us.2019 <- us.total %>% filter(yr == "2019-01-10")
us.2019_total <- sum(us.2019["df.us_total"])
#su.2019 <- su.total %>% filter(yr == "2019-01-10")
#su.2019_total <- sum(su.2019["df.su_total"])
#ca.2019 <- ca.total %>% filter(yr == "2019-01-10")
#ca.2019_total <- sum(ca.2019["df.ca_total"])
mh.2019 <- mh.total %>% filter(yr == "2019-01-10")
#mh.2019_total <- sum(mh.2019["df.mh_total"])
#ep.2019 <- ep.total %>% filter(yr == "2019-01-10")
#ep.2019_total <- sum(ep.2019["df.ep_total"])
es.2019 <- es.total %>% filter(yr == "2019-01-10")
#es.2019_total <- sum(es.2019["df.es_total"])
nr.2019 <- wf.total %>% filter(yr == "2019-01-10")
#nr.2019_total <- sum(nr.2019["df.wf_total"])
ec.2019 <- ec.total %>% filter(yr == "2019-01-10")
#ec.2019_total <- sum(ec.2019["df.ec_total"])
#sv.2019 <- sv.total %>% filter(yr == "2019-01-10")
#sv.2019_total <- sum(sv.2019["df.sv_total"])
sn.2019 <- sn.total %>% filter(yr == "2019-01-10")
#sn.2019_total <- sum(sn.2019["df.sn_total"])
pt.2019 <- pt.total %>% filter(yr == "2019-01-10")
#pt.2019_total <- sum(pt.2019["df.pt_total"])
wf.2019 <- wf.total %>% filter(yr == "2019-01-10")
#wf.2019_total <- sum(wf.2019["df.wf_total"])

# Plot comparing emssions between DWFN and FFA average

library(RColorBrewer)
coul <- brewer.pal(6, "Set3") 
em <- c(cbind(jp.2019_total,tw.2019_total,kr.2019_total,cn.2019_total,us.2019_total,ffamean.2019))
barplot(em,
        names.arg=c('JP','TW','KR','CN','US','FF'),ylab="Emissions",col=coul)
title('Purse-seine total emissions 2019')

# Plot comparing DWFN and FFA total emissions


coul <- brewer.pal(6, "Set2")
em2 <- c(cbind(jp.2019_total,tw.2019_total,kr.2019_total,cn.2019_total,us.2019_total,ffatotal_PS.2019))
barplot(em2,
        names.arg=c('JP','TW','KR','CN','US','FF'),col= coul)
title('Purse-seine total emissions 2019')


# Plot to compare agricultural emissions fo Pacific countries (average) vs. Purse-seine and longline average emissions.

# emissions intensities

# world

wld.cattle_2017 <- wld.cattle %>% filter(year == "2017") %>% select(year,value,item)
wld.cattle_ei_2017 <- wld.cattle_2017[1,]
wld.pig_2017 <- wld.pig %>% filter(year == "2017") %>% select(year,value,item)
wld.pig_ei_2017 <- wld.pig_2017[1,]
wld.chicken_2017 <- wld.chicken %>% filter(year == "2017") %>% select(year,value,item)
wld.chicken_ei_2017 <- wld.chicken_2017[1,]
wld.goat_2017 <- wld.cattle %>% filter(year == "2017") %>% select(year,value,item)
wld.goat_ei_2017 <- wld.goat_2017[1,]

# Pacific average by species

# Members
# simplify the following and calculate average by species.

ck.cattle_2017 <- ck.cattle %>% filter(year == "2017")
ck.cattle_2017[1,]
ck.pig_2017 <- ck.pig %>% filter(year == "2017")
ck.pig_2017[1,]
ck.chicken_2017 <- ck.chicken %>% filter(year == "2017")
ck.chicken_2017[1,]
ck.goat_2017 <- ck.goat %>% filter(year == "2017")
ck.goat_2017[1,]

fm.cattle_2017 <- fm.cattle %>% filter(year == "2017")
fm.cattle_2017[1,]
fm.pig_2017 <- fm.pig %>% filter(year == "2017")
fm.pig_2017[1,]
fm.chicken_2017 <- fm.chicken %>% filter(year == "2017")
fm.chicken_2017[1,]
fm.goat_2017 <- fm.goat %>% filter(year == "2017")
fm.goat_2017[1,]

fj.cattle_2017 <- ck.cattle %>% filter(year == "2017")
fj.cattle_2017[1,]
fj.pig_2017 <- fj.pig %>% filter(year == "2017")
fj.pig_2017[1,]
fj.chicken_2017 <- fj.chicken %>% filter(year == "2017")
fj.chicken_2017[1,]
fj.goat_2017 <- fj.goat %>% filter(year == "2017")
fj.goat_2017[1,]

ki.cattle_2017 <- ki.cattle %>% filter(year == "2017")
ki.cattle_2017[1,]
ki.pig_2017 <- ki.pig %>% filter(year == "2017")
ki.pig_2017[1,]
ki.chicken_2017 <- ki.chicken %>% filter(year == "2017")
ki.chicken_2017[1,]
ki.goat_2017 <- ki.goat %>% filter(year == "2017")
ki.goat_2017[1,]

nu.cattle_2017 <- nu.cattle %>% filter(year == "2017")
nu.cattle_2017[1,]
nu.pig_2017 <- nu.pig %>% filter(year == "2017")
nu.pig_2017[1,]
nu.chicken_2017 <- nu.chicken %>% filter(year == "2017")
nu.chicken_2017[1,]
nu.goat_2017 <- nu.goat %>% filter(year == "2017")
nu.goat_2017[1,]

pg.cattle_2017 <- pg.cattle %>% filter(year == "2017")
pg.cattle_2017[1,]
pg.pig_2017 <- pg.pig %>% filter(year == "2017")
pg.pig_2017[1,]
pg.chicken_2017 <- pg.chicken %>% filter(year == "2017")
pg.chicken_2017[1,]
pg.goat_2017 <- pg.goat %>% filter(year == "2017")
pg.goat_2017[1,]

pw.cattle_2017 <- pw.cattle %>% filter(year == "2017")
pw.cattle_2017[1,]
pw.pig_2017 <- pw.pig %>% filter(year == "2017")
pw.pig_2017[1,]
pw.chicken_2017 <- pw.chicken %>% filter(year == "2017")
pw.chicken_2017[1,]
pw.goat_2017 <- pw.goat %>% filter(year == "2017")
pw.goat_2017[1,]

ws.cattle_2017 <- ws.cattle %>% filter(year == "2017")
ws.cattle_2017[1,]
ws.pig_2017 <- ws.pig %>% filter(year == "2017")
ws.pig_2017[1,]
ws.chicken_2017 <- ws.chicken %>% filter(year == "2017")
ws.chicken_2017[1,]
ws.goat_2017 <- ws.goat %>% filter(year == "2017")
ws.goat_2017[1,]

sb.cattle_2017 <- sb.cattle %>% filter(year == "2017")
sb.cattle_2017[1,]
sb.pig_2017 <- sb.pig %>% filter(year == "2017")
sb.pig_2017[1,]
sb.chicken_2017 <- sb.chicken %>% filter(year == "2017")
sb.chicken_2017[1,]
sb.goat_2017 <- sb.goat %>% filter(year == "2017")
sb.goat_2017[1,]

tk.cattle_2017 <- tk.cattle %>% filter(year == "2017")
tk.cattle_2017[1,]
tk.pig_2017 <- tk.pig %>% filter(year == "2017")
tk.pig_2017[1,]
tk.chicken_2017 <- tk.chicken %>% filter(year == "2017")
tk.chicken_2017[1,]
tk.goat_2017 <- tk.goat %>% filter(year == "2017")
tk.goat_2017[1,]


to.cattle_2017 <- to.cattle %>% filter(year == "2017")
to.cattle_2017[1,]
to.pig_2017 <- to.pig %>% filter(year == "2017")
to.pig_2017[1,]
to.chicken_2017 <- to.chicken %>% filter(year == "2017")
to.chicken_2017[1,]
to.goat_2017 <- to.goat %>% filter(year == "2017")
to.goat_2017[1,]

tu.cattle_2017 <- tu.cattle %>% filter(year == "2017")
tu.cattle_2017[1,]
tu.pig_2017 <- tu.pig %>% filter(year == "2017")
tu.pig_2017[1,]
tu.chicken_2017 <- tu.chicken %>% filter(year == "2017")
tu.chicken_2017[1,]
tu.goat_2017 <- tu.goat %>% filter(year == "2017")
tu.goat_2017[1,]

vu.cattle_2017 <- vu.cattle %>% filter(year == "2017")
vu.cattle_2017[1,]
vu.pig_2017 <- vu.pig %>% filter(year == "2017")
vu.pig_2017[1,]
vu.chicken_2017 <- vu.chicken %>% filter(year == "2017")
vu.chicken_2017[1,]
vu.goat_2017 <- vu.goat %>% filter(year == "2017")
vu.goat_2017[1,]

# Agricultural species averages
# These are in kg so need to be multiplied by a 1000 for comparison with tuna which are mt.

cattlemean <- colMeans(matrix(c(ck.cattle_2017[1,"value"],fm.cattle_2017[1,'value'],fj.cattle_2017[1,"value"]
                  #,ki.cattle_2017[1,"value"],nu.cattle_2017[1,"value"]
                  ,pg.cattle_2017[1,"value"]
                  #,pw.cattle_2017[1,"value"]
                  ,ws.cattle_2017[1,'value']
                  ,sb.cattle_2017[1,"value"]
                  #,tk.cattle_2017[1,"value"]
                  #,to.cattle_2017[1,"value"],
                  #,tu.cattle_2017[1,"value"]
                  #,vu.cattle_2017[1,"value"]
                  )))
cattlemean 

pigmean <- colMeans(matrix(c(ck.pig_2017[1,"value"]
                                ,fm.pig_2017[1,'value']
                                ,fj.pig_2017[1,"value"]
                                ,ki.pig_2017[1,"value"]
                                ,nu.pig_2017[1,"value"]
                                ,pg.pig_2017[1,"value"]
                                #,pw.pig_2017[1,"value"]
                                ,ws.pig_2017[1,'value']
                                ,sb.pig_2017[1,"value"]
                                ,tk.pig_2017[1,"value"]
                                #,to.pig_2017[1,"value"]
                                ,tu.pig_2017[1,"value"]
                                ,vu.pig_2017[1,"value"]
)))
pigmean 


chickenmean <- colMeans(matrix(c(ck.chicken_2017[1,"value"]
                             ,fm.chicken_2017[1,'value']
                             ,fj.chicken_2017[1,"value"]
                             ,ki.chicken_2017[1,"value"]
                             ,nu.chicken_2017[1,"value"]
                             ,pg.chicken_2017[1,"value"]
                             #,pw.chicken_2017[1,"value"]
                             ,ws.chicken_2017[1,'value']
                             ,sb.chicken_2017[1,"value"]
                             ,tk.chicken_2017[1,"value"]
                             #,to.chicken_2017[1,"value"]
                             ,tu.chicken_2017[1,"value"]
                             ,vu.chicken_2017[1,"value"]
)))
chickenmean 

goatmean <- colMeans(matrix(c(ck.goat_2017[1,"value"]
                                 ,fm.goat_2017[1,'value']
                                 ,fj.goat_2017[1,"value"]
                                 #,ki.goat_2017[1,"value"]
                                 #,nu.goat_2017[1,"value"]
                                 ,pg.goat_2017[1,"value"]
                                 #,pw.goat_2017[1,"value"]
                                 #,ws.goat_2017[1,'value']
                                 #,sb.goat_2017[1,"value"]
                                 #,tk.goat_2017[1,"value"]
                                 #,to.goat_2017[1,"value"]
                                 #,tu.goat_2017[1,"value"]
                                 ,vu.goat_2017[1,"value"]
)))
goatmean 

# Need to calculate 2017 tuna emissions
# Then plot  abarchchart to compare livestock average enissions intensities against tuna emissions

# LL

df.ck  <- df2 %>% filter(flag_id == "CK" &  gr_id == "L")
df.ck_total <- df.ck[,5:8] %>% rowSums() # need to think about unit testing this
ck.total <- cbind(df.ck["yr"],df.ck_total)
df.fm  <- df2 %>% filter(flag_id == "FM" &  gr_id == "L")
df.fm_total <- df.fm[,5:8] %>% rowSums() # need to think about unit testing this
fm.total <- cbind(df.fm["yr"],df.fm_total)
df.fj  <- df2 %>% filter(flag_id == "FJ" &  gr_id == "L")
df.fj_total <- df.fj[,5:8] %>% rowSums() # need to think about unit testing this
fj.total <- cbind(df.fj["yr"],df.fj_total) 
df.ki  <- df2 %>% filter(flag_id == "KI" &  gr_id == "L")
df.ki_total <- df.ki[,5:8] %>% rowSums() # need to think about unit testing this
ki.total <- cbind(df.ki["yr"],df.ki_total)
#df.mi  <- df2 %>% filter(flag_id == "MI" &  gr_id == "L")
#df.mi_total <- df.mi[,5:8] %>% rowSums() # need to think about unit testing this
#mi.total <- cbind(df.mi["yr"],df.mi_total)
df.nu  <- df2 %>% filter(flag_id == "NU" &  gr_id == "L")
df.nu_total <- df.nu[,5:8] %>% rowSums() # need to think about unit testing this
nu.total <- cbind(df.nu["yr"],df.nu_total)
df.ni  <- df2 %>% filter(flag_id == "NI" &  gr_id == "L")
df.ni_total <- df.ni[,5:8] %>% rowSums() # need to think about unit testing this
ni.total <- cbind(df.ni["yr"],df.ni_total)
df.pw  <- df2 %>% filter(flag_id == "PW" &  gr_id == "L")
df.pw_total <- df.pw[,5:8] %>% rowSums() # need to think about unit testing this
pw.total <- cbind(df.pw["yr"],df.pw_total)
df.pg  <- df2 %>% filter(flag_id == "PG" &  gr_id == "L")
df.pg_total <-  df.pg[,5:8] %>% rowSums() # need to think about unit testing this
pg.total <- cbind(df.pg["yr"],df.pg_total)
df.ws  <- df2 %>% filter(flag_id == "WS" &  gr_id == "L")
df.ws_total <- df.ws[,5:8] %>% rowSums() # need to think about unit testing this
ws.total <- cbind(df.ws["yr"],df.ws_total)  
df.sb  <- df2 %>% filter(flag_id == "SB" &  gr_id == "L")
df.sb_total <- df.sb[,5:8] %>% rowSums() # need to think about unit testing this
sb.total <- cbind(df.sb["yr"],df.sb_total)
df.tk  <- df2 %>% filter(flag_id == "TK" &  gr_id == "L")
df.tk_total <- df.tk[,5:8] %>% rowSums() # need to think about unit testing this
tk.total <- cbind(df.tk["yr"],df.tk_total)
df.to  <- df2 %>% filter(flag_id == "TO" &  gr_id == "L")
df.to_total <- df.to[,5:8] %>% rowSums() # need to think about unit testing this
to.total <- cbind(df.to["yr"],df.to_total)
df.tv  <- df2 %>% filter(flag_id == "TV" &  gr_id == "L")
df.tv_total <- df.tv[,5:8] %>% rowSums() # need to think about unit testing this
tv.total <- cbind(df.tv["yr"],df.tv_total)
df.vu  <- df2 %>% filter(flag_id == "VU" &  gr_id == "L")
df.vu_total <- df.vu[,5:8] %>% rowSums() # need to think about unit testing this
vu.total <- cbind(df.vu["yr"],df.vu_total)

ck.2017 <- ck.total %>% filter(yr == "2017-01-10")
ck.2017_total <- sum(ck.2017["df.ck_total"])
fm.2017 <- fm.total %>% filter(yr == "2017-01-10")
fm.2017_total <- sum(fm.2017["df.fm_total"])
fj.2017 <- fj.total %>% filter(yr == "2017-01-10")
fj.2017_total <- sum(fj.2017["df.fj_total"])
ki.2017 <- ki.total %>% filter(yr == "2017-01-10")
ki.2017_total <- sum(ki.2017["df.ki_total"])
#mi.2019 <- mi.total %>% filter(yr == "2019-01-10")
#mi.2019_total <- sum(mi.2019["df.mi_total"])
#nu.2019 <- nu.total %>% filter(yr == "2019-01-10")
#nu.2019_total <- sum(nu.2019["df.nu_total"])
#ni.2019 <- ni.total %>% filter(yr == "2019-01-10")
#ni.2019_total <- sum(ni.2019["df.ni_total"])
#pw.2017 <- pw.total %>% filter(yr == "2017-01-10")
#pw.2017_total <- sum(pw.2017["df.pw_total"])
#pg.2019 <- pg.total %>% filter(yr == "2019-01-10")
#pg.2019_total <- sum(pg.2019["df.pg_total"])
ws.2017 <- ws.total %>% filter(yr == "2017-01-10")
ws.2017_total <- sum(ws.2017["df.ws_total"])
sb.2017 <- sb.total %>% filter(yr == "2017-01-10")
sb.2017_total <- sum(sb.2017["df.sb_total"])
#tk.2019 <- tk.total %>% filter(yr == "2019-01-10")
#tk.2019_total <- sum(tk.2019["df.tk_total"])
to.2017 <- to.total %>% filter(yr == "2017-01-10")
to.2017_total <- sum(to.2017["df.to_total"])
tv.2017 <- tv.total %>% filter(yr == "2017-01-10")
tv.2017_total <- sum(tv.2017["df.tv_total"])
vu.2017 <- vu.total %>% filter(yr == "2017-01-10")
vu.2017_total <- sum(vu.2017["df.vu_total"])

ffamean_LL.2017 <- mean(ck.2017_total,fm.2017_total,fj.2017_total,ki.2017_total,
                     ws.2017_total,sb.2017_total,
                     to.2017_total,tv.2017_total,vu.2017_total)  

ffatotal_LL.2017 <- sum(ck.2017_total,fm.2017_total,fj.2017_total,ki.2017_total,
                        ws.2017_total,sb.2017_total,
                        to.2017_total,tv.2017_total,vu.2017_total)  

coul <- brewer.pal(6, "Set2")
em5 <- c(cbind(cattlemean,pigmean,chickenmean,goatmean,ei.ll))
barplot(em5,
        names.arg=c('CA','PI','CH','GT','LL'),col= coul)
title('Agriculture vs Long-line Emissions Intensities')

coul <- brewer.pal(6, "Set2")
em6 <- c(cbind(cattlemean,pigmean,chickenmean,goatmean,ei.ps))
barplot(em6,
        names.arg=c('CA','PI','CH','GT','PS'),col= coul)
title('Agriculture vs Purse-seine Emissions Intensities')
