#Scrape ENSO/SOI data
# use Aussie BoM 

# url  www.bom.gov.au /climate/soihtm.shtml

# or use https://crudata.uea.ac.uk/data/soi/soi.dat

library(data.table)

url <- fread('https://crudata.uea.ac.uk/cru/data/soi/soi.dat')

df.soi <- data.frame(url)
colnames(df.soi)  <- c("year","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","annual")

ann <- df.soi[,"annual"]
year <- df.soi[,"year"]
all <- data.frame(df.soi)

enso.ann <- data.frame(cbind(year,ann))

all

#plot(ann,ylim=c(-10,10))

