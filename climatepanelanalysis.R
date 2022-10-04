# Impact of climate change on license fee revenues
#Read in prices and catch figures from data abse

rm(list=ls())

# Call libraries
library(tidyverse)
library(readxl)
library(writexl)
library("ggplot2")
library("RPostgreSQL")
options(gsubfn.engine = "R")
library("sqldf")
library("plm")
library("lmtest")
library("stargazer")
library("officer")


pw <- {
  "xxx"} #place passwoprd here
  
  drv <- dbDriver("PostgreSQL")
  
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  
  con2 = dbConnect(drv = drv, dbname = "xxx", host = "xxx", port = xxx,
                   user = "xxx", password = pw)
  
  print(con2)
  
  #Read annual prices and ace catch and days 
  
  df_prices <- dbGetQuery(con2, "SELECT year, skj_p, yft_p, bet_p, alb_p from markets.bkkannualprices")
  df.ace <- dbGetQuery(con2, "SELECT * from ace")
  #df.fees <- dbGetQuery(con2,"SELECT lafr from edis.*")
  
  
  rm(pw)
  allcons <- dbListConnections(PostgreSQL())
  for (con in allcons) {
    dbDisconnect(con)
  }
  
  dbDisconnect(con2)
  dbUnloadDriver(drv)
  
  
  # Read in data
  
  df.climate <- as.data.frame(read_excel("climatedata.xlsx",sheet='Data')) # change this 
  #df <- read_excel("Annual SKJ and YFN Thai import prices (1).xlsx")
  #df.fees <- as.data.frame(df.fees)
  df.ace <- as.data.frame(df.ace)
  
  
  oni12 <- read.table("https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/nino12.long.anom.data",skip=1,nrow=153)
  oni3 <- read.table("https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/nino3.long.anom.data",skip=1,nrows=153)
  oni34 <- read.table("https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/nino34.long.anom.data",skip=1,nrow=153)
  oni4 <- read.table("https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/nino4.long.anom.data",skip=1,nrows=153)
  
  source("enso.R")
  
  # Price data pre-processing
  
  df.prices <- as.data.frame(df_prices)
  
  df.climate$skj_p <- df.prices[12:24,]$skj_p
  df.climate$yft_p <- df.prices[12:24,]$yft_p
  df.climate$bet_p <- df.prices[12:24,]$bet_p
  df.climate$alb_p <- df.prices[12:24,]$alb_p
  #df.climate$days <- df.ace['days'][df.ace['yr'] > '2010-01-10'] # need to filter first
  
  # Catch data
  
  # Sum up the says for each year and by eez so these will be i and t indexed. So this calcualtes effort
  
  df.days_2008 <- df.days <- sum(df.ace[df.ace['yr'] == '2008-01-10',]['days'])
  df.days_2009 <- df.days <- sum(df.ace[df.ace['yr'] == '2009-01-10',]['days'])
  df.days_2010 <- df.days <- sum(df.ace[df.ace['yr'] == '2010-01-10',]['days'])
  df.days_2011 <- df.days <- sum(df.ace[df.ace['yr'] == '2011-01-10',]['days'])
  df.days_2012 <- df.days <- sum(df.ace[df.ace['yr'] == '2012-01-10',]['days'])
  df.days_2013 <- df.days <- sum(df.ace[df.ace['yr'] == '2013-01-10',]['days'])
  df.days_2014 <- df.days <- sum(df.ace[df.ace['yr'] == '2014-01-10',]['days'])
  df.days_2015 <- df.days <- sum(df.ace[df.ace['yr'] == '2015-01-10',]['days'])
  df.days_2016 <- df.days <- sum(df.ace[df.ace['yr'] == '2016-01-10',]['days'])
  df.days_2017 <- df.days <- sum(df.ace[df.ace['yr'] == '2017-01-10',]['days'])
  df.days_2018 <- df.days <- sum(df.ace[df.ace['yr'] == '2018-01-10',]['days'])
  
  totaleffort <- c(df.days_2008,df.days_2009,df.days_2010,df.days_2011,df.days_2012,df.days_2013,df.days_2014,
              df.days_2015,df.days_2016,df.days_2017,df.days_2018) #may need to do this for each zone as an alternative.
  
  df.catch <- df.ace %>% select(yr,ez_id,days,skj_c,yft_c,bet_c)
  
  ck.catch <- df.catch[df.catch$ez_id == 'CK',]
  fj.catch <- df.catch[df.catch$ez_id == 'FJ',]
  fm.catch <- df.catch[df.catch$ez_id == 'FM',]
  ki.catch <- df.catch[df.catch$ez_id == 'KI',]
  mi.catch <- df.catch[df.catch$ez_id == 'MI',]
  nu.catch <- df.catch[df.catch$ez_id == 'NU',]
  ni.catch <- df.catch[df.catch$ez_id == 'NI',]
  pg.catch <- df.catch[df.catch$ez_id == 'PG',]
  pw.catch <- df.catch[df.catch$ez_id == 'PW',]
  sb.catch <- df.catch[df.catch$ez_id == 'SB',]
  
  
  # pre-process nino data 
  
  oni12.df <- as.data.frame(oni12)
  oni12.df$annual <- rowSums(oni12.df[,2:13])
  oni3.df <- as.data.frame(oni3)
  oni3.df$annual <- rowSums(oni3.df[,2:13])
  oni4.df <- as.data.frame(oni4)
  oni4.df$annual <- rowSums(oni4.df[,2:13])
  oni34.df <- as.data.frame(oni34)
  oni34.df$annual <- rowSums(oni34.df[,2:13])

  
    # create some new variables
  
  #colnames(df.climate)[9] <- c("NINO1.2") #rename column because of a formatting error
  
  df.climate$logfee <- log(df.climate$fee) #log the fee
  
  df.climate$NINO12 <- rep(oni12.df$annual[139:151],15)
  df.climate$NINO3 <- rep(oni3.df$annual[139:151],15)
  df.climate$NINO34 <- rep(oni34.df$annual[139:151],15)
  df.climate$NINO4 <- rep(oni4.df$annual[139:151],15)
  df.climate$enso <- c(rep(enso.ann[143:155,2],15)) # check when adding new data
  
  #dummy for CoVID19
  
  cov19 <- rep(c(rep(0,12),1),15)
  
  df.climate$cov19 <- cov19
  
  
  Trend <- rep(c(1:13),15)
  
  fit  <- lm(logfee ~ enso + NINO12 + NINO3 + NINO4 + NINO34 + skj_p + yft_p + bet_p + alb_p + cov19 + Trend,data = df.climate)
  
  summary(fit)
  
  fit2 <- lm(logfee ~ enso + NINO12 + NINO3 + NINO4 + NINO34 + skj_p + yft_p + bet_p + alb_p + cov19,data = df.climate)
  
  summary(fit)
  

# Run plm
  
pdata <- pdata.frame(df.climate, index=c("eez","year"), drop.index=TRUE, row.names=TRUE)  

fit.fe <- plm(logfee ~ enso + NINO12 + NINO3 + NINO4 + NINO34 + skj_p + yft_p + bet_p + alb_p + cov19 + Trend,data = pdata,model="within",
              effect='individual')
fit.fe

fit.fe2 <- plm(logfee ~ enso + NINO12 + NINO3 + NINO4 + NINO34 + skj_p + yft_p + bet_p + alb_p + cov19,data = pdata,model="within",
             effect='individual')
fit.fe2

# do some stargazer here

stargazer(fit.fe,type="text",title="Panel fixed effects model") 

summary(fixef(fit.fe))


# Clustered standard errors

cluster1 <- coeftest(fit.fe, vcov=vcovHC(fit.fe,type="HC0",cluster="group")) #economics matters
cluster2 <-coeftest(fit.fe, vcov=vcovHC(fit.fe,type="HC0",cluster="time")) #economics doesn't matter
stargazer(fit,fit.fe,cluster1,cluster2,type="text",title="Panel fixed effects model with clustered standard errors",align=TRUE)

cluster1 <- coeftest(fit.fe2, vcov=vcovHC(fit.fe,type="HC0",cluster="group")) #economics matters
cluster2 <-coeftest(fit.fe2, vcov=vcovHC(fit.fe,type="HC0",cluster="time")) #economics doesn't matter
table2 <- stargazer(fit2,fit.fe2,cluster1,cluster2,type="text",title="Panel fixed effects model with clustered standard errors",align=TRUE)



## need to try two way clustering

# predictions

statusquo <- df.climate$logfee - residuals(fit.fe) # within sample predictions.

## counterfactuals for climate change

df.climate2 <- df.climate
df.climate2$NINO12 <- 0
df.climate2$NINO3 <- 0
df.climate2$NINO4 <- 0
df.climate2$NINO34 <- 0

counterfactuals <- predict(fit.fe,data=df.climate2)

impact <- ifelse(exp(statusquo) > 0,exp(statusquo),0) - ifelse(exp(counterfactuals) > 0,exp(counterfactuals),0) # positive indicates a benefit negative a loss
print(impact)

impact2 <- df.climate$fee - ifelse(exp(counterfactuals) > 0,exp(counterfactuals),0)#be careful as estiamtes were logged

#following is on the right track but doesn't quite work

ecdf1 <- ecdf(df.climate$fee)
ecdf2 <- ecdf(ifelse(exp(counterfactuals) > 0,exp(counterfactuals),0))

df.impact <- as.data.frame(df.climate$fee)
df.impact$counterfactual <- ifelse(exp(counterfactuals) > 0,exp(counterfactuals),0)
colnames(df.impact[1]) <- "fee"

ll <- Map(f  = stat_function, colour = c('red', 'green'),
          fun = list(ecdf1, ecdf2), geom = 'line')
ggplot(data=df.impact, aes(fee)) + ll

#lines(ecdf(ifelse(exp(counterfactuals) > 0,exp(counterfactuals),0)))
#title("Stochastic dominance climate change impacts")

# Do a covid impact and extract figures for Tonga.

df.covidimpoact <- df.climate
df.covidimpact$covid19 <- 0

nocovid <- predict(fit.fe,data=df.covidimpact)
covidimpact <- df.climate$fee - ifelse(exp(nocovid) > 0,exp(nocovid),0)

## create a new data set for out of sample predictions for forecasting

# 1. forecast ENSO
# 2. forecast each of the NINO variables
# 3. forecast prices these were done for PNAO.
# 4. dummy for cov19 1 for 2021 and 2022
# 5. Trend if used 

#  stohastic dominance analysis to compare status quo and counterdactual


# Still to do

# Run spatial panel



# Predictions



