#Impact analysis for climate change

source("climatepanelanalysis.R")

#Model results objects

# fitjapan_with_trend used japanese prices with a trend added to fit a panel model
# fit_with_japan.fe2 uses japanes pices with no trend to fit a panel model

# predictions


# with trend

 
## counterfactuals for climate change

df.climate2 <- df.climate # create a new data frame
df.climate2$NINO12 <- 0
df.climate2$NINO3 <- 0
df.climate2$NINO4 <- 0
df.climate2$NINO34 <- 0


pdata2 <- pdata.frame(df.climate2, index=c("eez","year"), drop.index=TRUE, row.names=TRUE) 
 
counterfactuals_trend <- predict(fitjapan_with_trend,data=pdata2) - fitjapan_with_trend$residuals #This is correct data frame should be a pdata object.

df.climate$counterfactual_trend <- ifelse(exp(counterfactuals_trend)-1 > 0,exp(counterfactuals_trend)-1,0) # positive indicates a benefit negative a loss

df.climate$statusquo <- ifelse(exp(predict(fitjapan_with_trend,data=pdata))-1 > 0,exp(predict(fitjapan_with_trend,data=pdata))-1,0) # This takes the antilog of the regression line.

df.climate$impact  <- df.climate$statusquo - df.climate$counterfactual_trend

# No trend

counterfactuals_notrend <- predict(fit_with_japan.fe2,data=df.pdata2)  - fit_with_japan.fe2$residuals #This is correct but data frame should be a pdata object
#counterfactuals_trend <- predict(fit,data=df.climate2)

df.climate$counterfactual_notrend <- ifelse(exp(counterfactuals_notrend)-1 > 0, exp(counterfactuals_notrend)-1, 0) 
df.climate$impact2 <- 0

df.climate["impact2"] <- df.climate["fee"] - df.climate["counterfactual_notrend"]
df.climate["impact3"] <- df.climate["fee"] - df.climate["counterfactual_trend"]


# Controlling for VDS

did.predict <- predict(did,data=pdata2) - did$residuals

impact.did <- df.climate$fee - ifelse(exp(did.predict)-1 > 0,exp(did.predict) - 1,0)

df.climate$didimpact <- impact.did

# Fixed effects

#be careful as estimates were logged hence need to take the exponential

#impact = current - counterfactual

#save to excel

impact.df <- df.climate$impact #compared to regression line
impact2.df <- df.climate$impact2 # fee vs predict with  no trend counterfactual
impact3.df <- df.climate$impact3 # fee vs predict with trend counterfactual

# # Calculate total impacts for each country
# Calculate impact breakdowns
## These are for each variable in the analysis basically marginal effects.
# add residuals total.

WS <- sum(exp(fitjapan_with_trend$coefficients[2]*df.climate$NINO4[118:130]))
SB <- sum(exp(fitjapan_with_trend$coefficients[2]*df.climate$NINO4[131:143]))

jwtresiduals<- residuals(fitjapan_with_trend)

ensoImpact <- fitjapan_with_trend$coefficients[1]*df.climate2$enso
NIN04impact <- fitjapan_with_trend$coefficients[2]*df.climate2$NINO4
jbet_freshpImpact <- fitjapan_with_trend$coefficients[3]*df.climate2$jbet_freshp
alb_pImpact <- fitjapan_with_trend$coefficients[4]*df.climate2$alb_p
TrendImpact <- fitjapan_with_trend$coefficients[5]*df.climate2$Trend
fixedeff <- fixef(fitjapan_with_trend)[1]
jwtresiduals <- residuals(fitjapan_with_trend)[1] #These shouldn't be included if residuals have to be nette out above
total <- exp(ensoImpact + NIN04impact + jbet_freshpImpact + alb_pImpact + TrendImpact + fixedeff) - 1#

breakdownImpact <- df.climate$fee - total

df.climate$breakdownImpact <- breakdownImpact

# without trend

ensoImpact2 <- fit_with_japan.fe2$coefficients[1]*df.climate2$enso
NIN04impact2 <-fit_with_japan.fe2$coefficients[2]*df.climate2$NINO4
jbet_freshpImpact2 <- fit_with_japan.fe2$coefficients[3]*df.climate2$jbet_freshp
alb_pImpact2 <- fit_with_japan.fe2$coefficients[4]*df.climate2$alb_p
fixedeff2 <- fixef(fit_with_japan.fe2)
jwtresiduals2 <- residuals(fit_with_japan.fe2)[1]
total2 <- exp(ensoImpact2 + NIN04impact2 + jbet_freshpImpact2 + alb_pImpact2 + fixedeff2) - 1 #

breakdownImpact2 <- df.climate$fee - total2

df.climate$breakdownImpact2 <- breakdownImpact2

# Check consistency of estimates

#detach("package:RPostgreSQL", unload=TRUE) # need to detach RPosgreSQL to use sqldf

CK <- sqldf("SELECT sum(breakdownImpact) FROM 'df.climate' WHERE eez = 'CK'",drv="SQLite")
FJ <- sqldf("SELECT sum(breakdownImpact) FROM 'df.climate' WHERE eez = 'FJ'",drv="SQLite")
FM <- sqldf("SELECT sum(breakdownImpact) FROM 'df.climate' WHERE eez = 'FM'",drv="SQLite")
KI <- sqldf("SELECT sum(breakdownImpact) FROM 'df.climate' WHERE eez = 'KI'",drv="SQLite")
MH <- sqldf("SELECT sum(breakdownImpact) FROM 'df.climate' WHERE eez = 'MH'",drv="SQLite")
NR <- sqldf("SELECT sum(breakdownImpact) FROM 'df.climate' WHERE eez = 'NR'",drv="SQLite")
NU <- sqldf("SELECT sum(breakdownImpact) FROM 'df.climate' WHERE eez = 'NU'",drv="SQLite")
PG <- sqldf("SELECT sum(breakdownImpact) FROM 'df.climate' WHERE eez = 'PG'",drv="SQLite")
PW <- sqldf("SELECT sum(breakdownImpact) FROM 'df.climate' WHERE eez = 'PW'",drv="SQLite")
WS <- sqldf("SELECT sum(breakdownImpact) FROM 'df.climate' WHERE eez = 'WS'",drv="SQLite")
SB <- sqldf("SELECT sum(breakdownImpact) FROM 'df.climate' WHERE eez = 'SB'",drv="SQLite")
TK <- sqldf("SELECT sum(breakdownImpact) FROM 'df.climate' WHERE eez = 'TK'",drv="SQLite")
TO <- sqldf("SELECT sum(breakdownImpact) FROM 'df.climate' WHERE eez = 'TO'",drv="SQLite")
TV <- sqldf("SELECT sum(breakdownImpact) FROM 'df.climate' WHERE eez = 'TV'",drv="SQLite")
VU <- sqldf("SELECT sum(breakdownImpact) FROM 'df.climate' WHERE eez = 'VU'",drv="SQLite")

aggregate.impact <- as.data.frame(c(CK,FJ,FM,KI,MH,NR,NU,PG,PW,WS,SB,TK,TO,TV,VU))
#colnames(aggregate.impact) <- c('CK','FJ','FM','KI','MH','NR','NU','PG','PW','WS','SB','TK','TO','TV','VU')


CK2 <- sqldf("SELECT sum(breakdownImpact2) FROM 'df.climate' WHERE eez = 'CK'",drv="SQLite")
FJ2 <- sqldf("SELECT sum(breakdownImpact2) FROM 'df.climate' WHERE eez = 'FJ'",drv="SQLite")
FM2 <- sqldf("SELECT sum(breakdownImpact2) FROM 'df.climate' WHERE eez = 'FM'",drv="SQLite")
KI2 <- sqldf("SELECT sum(breakdownImpact2) FROM 'df.climate' WHERE eez = 'KI'",drv="SQLite")
MH2 <- sqldf("SELECT sum(breakdownImpact2) FROM 'df.climate' WHERE eez = 'MH'",drv="SQLite")
NR2 <- sqldf("SELECT sum(breakdownImpact2) FROM 'df.climate' WHERE eez = 'NR'",drv="SQLite")
NU2 <- sqldf("SELECT sum(breakdownImpact2) FROM 'df.climate' WHERE eez = 'NU'",drv="SQLite")
PG2 <- sqldf("SELECT sum(breakdownImpact2) FROM 'df.climate' WHERE eez = 'PG'",drv="SQLite")
PW2 <- sqldf("SELECT sum(breakdownImpact2) FROM 'df.climate' WHERE eez = 'PW'",drv="SQLite")
WS2 <- sqldf("SELECT sum(breakdownImpact2) FROM 'df.climate' WHERE eez = 'WS'",drv="SQLite")
SB2 <- sqldf("SELECT sum(breakdownImpact2) FROM 'df.climate' WHERE eez = 'SB'",drv="SQLite")
TK2 <- sqldf("SELECT sum(breakdownImpact2) FROM 'df.climate' WHERE eez = 'TK'",drv="SQLite")
TO2 <- sqldf("SELECT sum(breakdownImpact2) FROM 'df.climate' WHERE eez = 'TO'",drv="SQLite")
TV2 <- sqldf("SELECT sum(breakdownImpact2) FROM 'df.climate' WHERE eez = 'TV'",drv="SQLite")
VU2 <- sqldf("SELECT sum(breakdownImpact) FROM 'df.climate' WHERE eez = 'VU'",drv="SQLite")

aggregate.impact2 <- as.data.frame(c(CK2,FJ2,FM2,KI2,MH2,NR2,NU2,PG2,PW2,WS2,SB2,TK2,TO2,TV2,VU2))
#colnames(aggregate.impact2) <- c('CK','FJ','FM','KI','MH','NR','NU','PG','PW','WS','SB','TK','TO','TV','VU')

CK.fee <- sqldf("SELECT sum(fee) FROM 'df.climate' WHERE eez = 'CK'",drv="SQLite")
FJ.fee <- sqldf("SELECT sum(fee) FROM 'df.climate' WHERE eez = 'FJ'",drv="SQLite")
FM.fee <- sqldf("SELECT sum(fee) FROM 'df.climate' WHERE eez = 'FM'",drv="SQLite")
KI.fee <- sqldf("SELECT sum(fee) FROM 'df.climate' WHERE eez = 'KI'",drv="SQLite")
MH.fee <- sqldf("SELECT sum(fee) FROM 'df.climate' WHERE eez = 'MH'",drv="SQLite")
NR.fee <- sqldf("SELECT sum(fee) FROM 'df.climate' WHERE eez = 'NR'",drv="SQLite")
NU.fee <- sqldf("SELECT sum(fee) FROM 'df.climate' WHERE eez = 'NU'",drv="SQLite")
PG.fee <- sqldf("SELECT sum(fee) FROM 'df.climate' WHERE eez = 'PG'",drv="SQLite")
PW.fee <- sqldf("SELECT sum(fee) FROM 'df.climate' WHERE eez = 'PW'",drv="SQLite")
WS.fee <- sqldf("SELECT sum(fee) FROM 'df.climate' WHERE eez = 'WS'",drv="SQLite")
SB.fee <- sqldf("SELECT sum(fee) FROM 'df.climate' WHERE eez = 'SB'",drv="SQLite")
TK.fee <- sqldf("SELECT sum(fee) FROM 'df.climate' WHERE eez = 'TK'",drv="SQLite")
TO.fee <- sqldf("SELECT sum(fee) FROM 'df.climate' WHERE eez = 'TO'",drv="SQLite")
TV.fee <- sqldf("SELECT sum(fee) FROM 'df.climate' WHERE eez = 'TV'",drv="SQLite")
VU.fee <- sqldf("SELECT sum(fee) FROM 'df.climate' WHERE eez = 'VU'",drv="SQLite")

aggregate.fee <- as.data.frame(c(CK.fee,FJ.fee,FM.fee,KI.fee,MH.fee,NR.fee,NU.fee,PG.fee,PW.fee,WS.fee,SB.fee,
                                 TK.fee,TO.fee,TV.fee,VU.fee))
#colnames(aggregate.fee) <- c('CK','FJ','FM','KI','MH','NR','NU','PG','PW','WS','SB','TK','TO','TV','VU')

CK.did <- sqldf("SELECT sum(didimpact) FROM 'df.climate' WHERE eez = 'CK'",drv="SQLite")
FJ.did <- sqldf("SELECT sum(didimpact) FROM 'df.climate' WHERE eez = 'FJ'",drv="SQLite")
FM.did <- sqldf("SELECT sum(didimpact) FROM 'df.climate' WHERE eez = 'FM'",drv="SQLite")
KI.did <- sqldf("SELECT sum(didimpact) FROM 'df.climate' WHERE eez = 'KI'",drv="SQLite")
MH.did <- sqldf("SELECT sum(didimpact) FROM 'df.climate' WHERE eez = 'MH'",drv="SQLite")
NR.did <- sqldf("SELECT sum(didimpact) FROM 'df.climate' WHERE eez = 'NR'",drv="SQLite")
NU.did <- sqldf("SELECT sum(didimpact) FROM 'df.climate' WHERE eez = 'NU'",drv="SQLite")
PG.did <- sqldf("SELECT sum(didimpact) FROM 'df.climate' WHERE eez = 'PG'",drv="SQLite")
PW.did <- sqldf("SELECT sum(didimpact) FROM 'df.climate' WHERE eez = 'PW'",drv="SQLite")
WS.did <- sqldf("SELECT sum(didimpact) FROM 'df.climate' WHERE eez = 'WS'",drv="SQLite")
SB.did <- sqldf("SELECT sum(didimpact) FROM 'df.climate' WHERE eez = 'SB'",drv="SQLite")
TK.did <- sqldf("SELECT sum(didimpact) FROM 'df.climate' WHERE eez = 'TK'",drv="SQLite")
TO.did <- sqldf("SELECT sum(didimpact) FROM 'df.climate' WHERE eez = 'TO'",drv="SQLite")
TV.did <- sqldf("SELECT sum(didimpact) FROM 'df.climate' WHERE eez = 'TV'",drv="SQLite")
VU.did <- sqldf("SELECT sum(didimpact) FROM 'df.climate' WHERE eez = 'VU'",drv="SQLite")

aggregate.did <- as.data.frame(c(CK.did,FJ.did,FM.did,KI.did,MH.did,NR.did,NU.did,PG.did,PW.did,WS.did,SB.did,
                                 TK.did,TO.did,TV.did,VU.did))

#colnames(aggregate.did) <- c('CK','FJ','FM','KI','MH','NR','NU','PG','PW','WS','SB','TK','TO','TV','VU')

#aggregate from plm runs

CK.impact0 <- sqldf("SELECT sum(impact) FROM 'df.climate' WHERE eez = 'CK'",drv="SQLite")
FJ.impact0 <- sqldf("SELECT sum(impact) FROM 'df.climate' WHERE eez = 'FJ'",drv="SQLite")
FM.impact0 <- sqldf("SELECT sum(impact) FROM 'df.climate' WHERE eez = 'FM'",drv="SQLite")
KI.impact0 <- sqldf("SELECT sum(impact) FROM 'df.climate' WHERE eez = 'KI'",drv="SQLite")
MH.impact0 <- sqldf("SELECT sum(impact) FROM 'df.climate' WHERE eez = 'MH'",drv="SQLite")
NR.impact0 <- sqldf("SELECT sum(impact) FROM 'df.climate' WHERE eez = 'NR'",drv="SQLite")
NU.impact0 <- sqldf("SELECT sum(impact) FROM 'df.climate' WHERE eez = 'NU'",drv="SQLite")
PG.impact0 <- sqldf("SELECT sum(impact) FROM 'df.climate' WHERE eez = 'PG'",drv="SQLite")
PW.impact0 <- sqldf("SELECT sum(impact) FROM 'df.climate' WHERE eez = 'PW'",drv="SQLite")
WS.impact0 <- sqldf("SELECT sum(impact) FROM 'df.climate' WHERE eez = 'WS'",drv="SQLite")
SB.impact0 <- sqldf("SELECT sum(impact) FROM 'df.climate' WHERE eez = 'SB'",drv="SQLite")
TK.impact0 <- sqldf("SELECT sum(impact) FROM 'df.climate' WHERE eez = 'TK'",drv="SQLite")
TO.impact0 <- sqldf("SELECT sum(impact) FROM 'df.climate' WHERE eez = 'TO'",drv="SQLite")
TV.impact0 <- sqldf("SELECT sum(impact) FROM 'df.climate' WHERE eez = 'TV'",drv="SQLite")
VU.impact0 <- sqldf("SELECT sum(impact) FROM 'df.climate' WHERE eez = 'VU'",drv="SQLite")

aggregate.impact0 <- as.data.frame(c(CK.impact0,FJ.impact0,FM.impact0,KI.impact0,MH.impact0,NR.impact0,NU.impact0,PG.impact0,PW.impact0,WS.impact0,
                                  SB.impact0,TK.impact0,TO.impact0,TV.impact0,VU.impact0))

CK.impact2 <- sqldf("SELECT sum(impact2) FROM 'df.climate' WHERE eez = 'CK'",drv="SQLite")
FJ.impact2 <- sqldf("SELECT sum(impact2) FROM 'df.climate' WHERE eez = 'FJ'",drv="SQLite")
FM.impact2 <- sqldf("SELECT sum(impact2) FROM 'df.climate' WHERE eez = 'FM'",drv="SQLite")
KI.impact2 <- sqldf("SELECT sum(impact2) FROM 'df.climate' WHERE eez = 'KI'",drv="SQLite")
MH.impact2 <- sqldf("SELECT sum(impact) FROM 'df.climate' WHERE eez = 'MH'",drv="SQLite")
NR.impact2 <- sqldf("SELECT sum(impact2) FROM 'df.climate' WHERE eez = 'NR'",drv="SQLite")
NU.impact2 <- sqldf("SELECT sum(impact2) FROM 'df.climate' WHERE eez = 'NU'",drv="SQLite")
PG.impact2 <- sqldf("SELECT sum(impact2) FROM 'df.climate' WHERE eez = 'PG'",drv="SQLite")
PW.impact2 <- sqldf("SELECT sum(impact2) FROM 'df.climate' WHERE eez = 'PW'",drv="SQLite")
WS.impact2 <- sqldf("SELECT sum(impact2) FROM 'df.climate' WHERE eez = 'WS'",drv="SQLite")
SB.impact2 <- sqldf("SELECT sum(impact2) FROM 'df.climate' WHERE eez = 'SB'",drv="SQLite")
TK.impact2 <- sqldf("SELECT sum(impact2) FROM 'df.climate' WHERE eez = 'TK'",drv="SQLite")
TO.impact2 <- sqldf("SELECT sum(impact2) FROM 'df.climate' WHERE eez = 'TO'",drv="SQLite")
TV.impact2 <- sqldf("SELECT sum(impact2) FROM 'df.climate' WHERE eez = 'TV'",drv="SQLite")
VU.impact2 <- sqldf("SELECT sum(impact2) FROM 'df.climate' WHERE eez = 'VU'",drv="SQLite")

aggregate.impact_notrend <- as.data.frame(c(CK.impact2,FJ.impact2,FM.impact2,KI.impact2,MH.impact2,NR.impact2,NU.impact2,PG.impact2,PW.impact2,
                                     WS.impact2,SB.impact2,TK.impact2,TO.impact2,TV.impact2,VU.impact2))


# df.climate$impact3 # fee vs predict with trend counterfactual

CK.impact3 <- sqldf("SELECT sum(impact3) FROM 'df.climate' WHERE eez = 'CK'",drv="SQLite")
FJ.impact3 <- sqldf("SELECT sum(impact3) FROM 'df.climate' WHERE eez = 'FJ'",drv="SQLite")
FM.impact3 <- sqldf("SELECT sum(impact3) FROM 'df.climate' WHERE eez = 'FM'",drv="SQLite")
KI.impact3 <- sqldf("SELECT sum(impact3) FROM 'df.climate' WHERE eez = 'KI'",drv="SQLite")
MH.impact3 <- sqldf("SELECT sum(impact3) FROM 'df.climate' WHERE eez = 'MH'",drv="SQLite")
NR.impact3 <- sqldf("SELECT sum(impact3) FROM 'df.climate' WHERE eez = 'NR'",drv="SQLite")
NU.impact3 <- sqldf("SELECT sum(impact3) FROM 'df.climate' WHERE eez = 'NU'",drv="SQLite")
PG.impact3 <- sqldf("SELECT sum(impact3) FROM 'df.climate' WHERE eez = 'PG'",drv="SQLite")
PW.impact3 <- sqldf("SELECT sum(impact3) FROM 'df.climate' WHERE eez = 'PW'",drv="SQLite")
WS.impact3 <- sqldf("SELECT sum(impact3) FROM 'df.climate' WHERE eez = 'WS'",drv="SQLite")
SB.impact3 <- sqldf("SELECT sum(impact3) FROM 'df.climate' WHERE eez = 'SB'",drv="SQLite")
TK.impact3 <- sqldf("SELECT sum(impact3) FROM 'df.climate' WHERE eez = 'TK'",drv="SQLite")
TO.impact3 <- sqldf("SELECT sum(impact3) FROM 'df.climate' WHERE eez = 'TO'",drv="SQLite")
TV.impact3 <- sqldf("SELECT sum(impact3) FROM 'df.climate' WHERE eez = 'TV'",drv="SQLite")
VU.impact3 <- sqldf("SELECT sum(impact3) FROM 'df.climate' WHERE eez = 'VU'",drv="SQLite")


aggregate.impact_trend <- as.data.frame(c(CK.impact3,FJ.impact3,FM.impact3,KI.impact3,MH.impact3,NR.impact3,NU.impact3,PG.impact3,PW.impact3,
                                            WS.impact3,SB.impact3,TK.impact3,TO.impact3,TV.impact3,VU.impact3))



# Consolidate aggregate results

members <- c('CK','FJ','FM','KI','MH','NR','NU','PG','PW','WS','SB','TK','TO','TV','VU')
df.aggregate <- as.data.frame(members)
df.aggregate$fee <- as.data.frame(t(aggregate.fee))["V1"]
#colnames(df.aggregate[,2]) <- c("fee")
df.aggregate$impact <- as.data.frame(t(aggregate.impact))["V1"]
#colnames(df.aggregate[,3]) <- c("Breakdown Trend")
df.aggregate$impact2 <- as.data.frame(t(aggregate.impact2))["V1"]
#colnames(df.aggregate[,4]) <- c("Breakdown No Trend")
df.aggregate$did <- as.data.frame(t(aggregate.did))["V1"]
#colnames(df.aggregate[,5]) <- c("Controlling for VDS")
df.aggregate$impact0 <- as.data.frame(t(aggregate.impact0))["V1"]
#colnames(df.aggregate[,6]) <- c("From plm package reg line comparison")
df.aggregate$impact_notrend <- as.data.frame(t(aggregate.impact_notrend))["V1"]
#colnames(df.aggregate[,7]) <- c("No Trend compare actual")
df.aggregate$impact_trend <- as.data.frame(t(aggregate.impact_trend))["V1"]
#colnames(df.aggregate[,8]) <- c("Trend compare actual")

df.aggregate <- as.data.frame(df.aggregate)

colnames(df.aggregate) <- c("Country","fee","Breakdown Trend","Breakdown No Trend","Controlling for VDS","Relative to Reg Line","No Trend","Trend" )

resRow <- as.data.frame(t(df.aggregate))
write.csv(resRow,file="aggregate_cc_results.csv")

#write_xlsx(impact.df,"impact.xlsx", col_names=TRUE)
#write_xlsx(impact2.df, "impact2.xlsx", col_names=TRUE)

#write.csv(impact.df, file = "impact.csv")
#write.csv(impact2.df, file = "impact2.csv")
#write.csv(df.climate, file = "climateimpactresults.csv")

# output results

write.csv(impact.df, file = "impactLL.csv")
write.csv(impact2.df, file = "impact2LL.csv")
write.csv(impact3.df, file = "impact3LL.csv")
write.csv(df.climate, file = "climateimpactresultsLL.csv")


#following is on the right track but doesn't quite work

#ecdf1 <- ecdf(df.climate$fee) 
#ecdf2 <- ecdf(ifelse(exp(counterfactuals) > 0,exp(counterfactuals),0))

#df.impact <- as.data.frame(df.climate$fee)
#df.impact$counterfactual <- ifelse(exp(counterfactuals) > 0,exp(counterfactuals),0)
#colnames(df.impact[1]) <- "fee"

# The following is broken, not sure what I was trying to do here.

#ll <- Map(f  = stat_function, colour = c('red', 'green'),
#         fun = list(ecdf1, ecdf2), geom = 'line')
#ggplot(data=df.impact, aes(fee)) + ll

#lines(ecdf(ifelse(exp(counterfactuals) > 0,exp(counterfactuals),0)))
#title("Stochastic dominance climate change impacts")

# Do a covid impact and extract figures for Tonga.

# df.covidimpact <- df.climate
# df.covidimpact$cov19 <- 0

# nocovid <- predict(fit.fe,data=df.covidimpact)

# covidimpact <- df.climate$fee - ifelse(exp(nocovid) > 0,exp(nocovid),0)

# create a new data set for out of sample predictions for forecasting

# 1. forecast ENSO
# 2. forecast each of the NINO variables
# 3. forecast prices these were done for PNAO.
# 4. dummy for cov19 1 for 2021 and 2022
# 5. Trend if used 

#  stochastic dominance analysis to compare status quo and counterdactual
# Still to do

# Run spatial panel



