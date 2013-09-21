
# SETUP WORKSPACE

set.seed(4711)
PERSISTENT_CONSTANTS = c("PERSISTENT_CONSTANTS", "WAHLTAG", "DATE_FORMAT",
                         "ORDERED_PARTY_NAMES", "bigTwoColors", "collectedColors",
                         "resCDU", "resSPD", "resGRUENE", "resFDP", "resLINKE", "resPIRATEN", "resAFD", "resSONST")
DATE_FORMAT = "%d.%m.%y"
WAHLTAG = as.Date("22.09.2013", DATE_FORMAT)
ORDERED_PARTY_NAMES = c("CDU/CSU", "SPD", "B90/Grune", "FDP", "Die Linke", "Piraten", "AfD", "sonstige")
bigTwoColors =    c("black", "red")
collectedColors = c("black", "red", "green", "yellow", "purple", "orange", "grey", "grey")

# clean
rm(list = ls()[!(ls() %in% PERSISTENT_CONSTANTS)])


# constants
PERSISTENT_CONSTANTS = c("PERSISTENT_CONSTANTS")



# INIT DATA

# load trainings data: data
dataLocation = "data\\Hist2013.csv"

rawData = read.csv2(dataLocation, header=FALSE, encoding="ANSI", sep=";", 
                    strip.white=TRUE, na.strings=c("?"))

rm(list=c("dataLocation"))

rawData = as.data.frame(rawData)
names(rawData) = c("index", "cdu", "spd", "fdp", "linke", "gruene", "piraten", "afd", "sonst", "institut", "kommentar", "date")
rawData = rawData[-1,]

rawData = rawData[-c(313:316),]


asPercentage = function(value) {
  if (!is.na(value)) {
    as.numeric(sub(",", ".", sub("%", "", value)))/100
  } else {
    NA
  }
}


random.imp <- function (a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}


rawData$piraten = random.imp(rawData$piraten)
rawData$afd =     random.imp(rawData$afd)


# format rawdata
rawData$institut =  as.character(rawData$institut)
rawData$kommentar = as.character(rawData$kommentar)
rawData$date =      as.Date(rawData$date, DATE_FORMAT)
rawData$index =     as.numeric(rawData$index)

rawData$cdu =       asPercentage(rawData$cdu)
rawData$spd =       asPercentage(rawData$spd)
rawData$gruene =    asPercentage(rawData$gruene)
rawData$fdp =       asPercentage(rawData$fdp)
rawData$linke =     asPercentage(rawData$linke)
rawData$piraten =   asPercentage(rawData$piraten)
rawData$afd =       asPercentage(rawData$afd)
rawData$sonst =     asPercentage(rawData$sonst)

data = rawData

rawData$cdu = scale(rawData$cdu)
rawData$spd = scale(rawData$spd)
rawData$gruene = scale(rawData$gruene)
rawData$fdp = scale(rawData$fdp)
rawData$linke = scale(rawData$linke)
rawData$piraten = scale(rawData$piraten)
rawData$afd = scale(rawData$afd)
rawData$sonst = scale(rawData$sonst)


bigTwoMeans =     c(rawData$cdu, rawData$spd)
collectedMeans =  c(rawData$cdu, rawData$spd, rawData$gruene, rawData$fdp, rawData$linke, rawData$piraten, rawData$afd, rawData$sonst)


# plot(bigTwoMeans, col = bigTwoColors)
# plot(collectedMeans, col = collectedColors)


## aggregated data
quantile(sample(data$cdu,    1000, replace=TRUE), na.rm=TRUE)
quantile(sample(data$spd,    1000, replace=TRUE), na.rm=TRUE)
quantile(sample(data$gruene, 1000, replace=TRUE), na.rm=TRUE)
quantile(sample(data$fdp,    1000, replace=TRUE), na.rm=TRUE)
quantile(sample(data$linke,  1000, replace=TRUE), na.rm=TRUE)
quantile(sample(data$piraten,1000, replace=TRUE), na.rm=TRUE)
quantile(sample(data$afd,    1000, replace=TRUE), na.rm=TRUE)
quantile(sample(data$sonst,  1000, replace=TRUE), na.rm=TRUE)


# Machine Learning
cdu =     as.data.frame(x=data$cdu,     row.names=c(1:312))
spd =     as.data.frame(x=data$spd,     row.names=c(1:312))
gruene =  as.data.frame(x=data$gruene,  row.names=c(1:312))
fdp =     as.data.frame(x=data$fdp,     row.names=c(1:312))
linke =   as.data.frame(x=data$linke,   row.names=c(1:312))
piraten = as.data.frame(x=data$piraten, row.names=c(1:312))
afd =     as.data.frame(x=data$afd,     row.names=c(1:312))
sonst =   as.data.frame(x=data$sonst,   row.names=c(1:312))

cdu$date =     data$date
spd$date =     data$date
gruene$date =  data$date
fdp$date =     data$date
linke$date =   data$date
piraten$date = data$date
afd$date =     data$date
sonst$date =   data$date

names(cdu) =     c("mean", "date")
names(spd) =     c("mean", "date")
names(gruene) =  c("mean", "date")
names(fdp) =     c("mean", "date")
names(linke) =   c("mean", "date")
names(piraten) = c("mean", "date")
names(afd) =     c("mean", "date")
names(sonst) =   c("mean", "date")


emptySet = sonst
emptySet$mean = -1
emptySet$date = WAHLTAG
emptySet = data.frame(emptySet)

formula = mean ~ date

# CDU
lm = glm(formula = formula, data = cdu, na.action = na.omit)
resCDU = predict(lm, newdata = emptySet, type="response")
resCDU = as.numeric(resCDU[1])


# SPD
lm = glm(formula = formula, data = spd, na.action = na.omit)
resSPD = predict(lm, newdata = emptySet, type="response")
resSPD = as.numeric(resSPD[1])


# GRUENE
lm = glm(formula = formula, data = gruene, na.action = na.omit)
resGRUENE = predict(lm, newdata = emptySet, type="response")
resGRUENE = as.numeric(resGRUENE[1])


# FDP
lm = glm(formula = formula, data = fdp, na.action = na.omit)
resFDP = predict(lm, newdata = emptySet, type="response")
resFDP = as.numeric(resFDP[1])


# LINKE
lm = glm(formula = formula, data = linke, na.action = na.omit)
resLINKE = predict(lm, newdata = emptySet, type="response")
resLINKE = as.numeric(resLINKE[1])


# PIRATEN
lm = glm(formula = formula, data = piraten, na.action = na.omit)
resPIRATEN = predict(lm, newdata = emptySet, type="response")
resPIRATEN = as.numeric(resPIRATEN[1])


# AFD
lm = glm(formula = formula, data = afd, na.action = na.omit)
resAFD = predict(lm, newdata = emptySet, type="response")
resAFD = as.numeric(resAFD[1])


# SONSTIGE
lm = glm(formula = formula, data = sonst, na.action = na.omit)
resSONST = predict(lm, newdata = emptySet, type="response")
resSONST = as.numeric(resSONST[1])

SUM = resCDU + resSPD + resGRUENE + resFDP + resLINKE + resPIRATEN + resAFD + resSONST


resCDU =     resCDU     / SUM
resSPD =     resSPD     / SUM
resGRUENE =  resGRUENE  / SUM
resFDP =     resFDP     / SUM
resLINKE =   resLINKE   / SUM
resPIRATEN = resPIRATEN / SUM
resAFD =     resAFD     / SUM
resSONST =   resSONST   / SUM


# rest = 1 - (resSONST)

# resCDU =     resCDU     / rest
# resSPD =     resSPD     / rest
# resGRUENE =  resGRUENE  / rest
# resFDP =     resFDP     / rest
# resLINKE =   resLINKE   / rest
# resPIRATEN = resPIRATEN / rest
# resAFD =     resAFD     / rest
# resSONST =   resSONST   / rest

collectedMeanValues = c(resCDU, resSPD, resGRUENE, resFDP, resLINKE, resPIRATEN, resAFD, resSONST)


barplot(collectedMeanValues, main="glm-Prognose: btw2013", xlab="", ylab="Prozent", names.arg=ORDERED_PARTY_NAMES)

rm(list=c("lm", "formula", "bigTwoColors", "bigTwoMeans", "collectedColors", "collectedMeans", 
          "WAHLTAG", "DATE_FORMAT", "SUM", 
          "cdu", "spd", "gruene", "fdp", "linke", "piraten", "afd", "sonst", 
          "rawData", "data", "emptySet"))
