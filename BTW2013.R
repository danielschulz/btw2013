
# SETUP WORKSPACE

set.seed(4711)
PERSISTENT_CONSTANTS = c("PERSISTENT_CONSTANTS")

# clean
rm(list = ls()[!(ls() %in% PERSISTENT_CONSTANTS)])


# constants
PERSISTENT_CONSTANTS = c("PERSISTENT_CONSTANTS")



# INIT DATA

# load trainings data: data
dataLocation = "data\\Umfragen_Sep21-2013.csv"

rawData = read.csv2(dataLocation, header=FALSE, encoding="ANSI", sep=";", 
                    strip.white=TRUE, na.strings=c("?"))

rm(list=c("dataLocation"))

rawData = t(rawData)
rawData = as.data.frame(rawData)
names(rawData) = c("institut", "cdu", "spd", "gruene", "fdp", "linke", "piraten", "afd", "sonst")
rawData = rawData[-1,]


asPercentage = function(value) {
  if (!is.na(value)) {
    as.numeric(sub(",", ".", sub("%", "", value)))/100
  } else {
    NA
  }
}




# format rawdata
rawData$institut = as.character(rawData$institut)
rawData$cdu =      asPercentage(rawData$cdu)
rawData$spd =      asPercentage(rawData$spd)
rawData$gruene =   asPercentage(rawData$gruene)
rawData$fdp =      asPercentage(rawData$fdp)
rawData$linke =    asPercentage(rawData$linke)
rawData$piraten =  asPercentage(rawData$piraten)
rawData$afd =      asPercentage(rawData$afd)
rawData$sonst =    asPercentage(rawData$sonst)

data = rawData[-8,]

## aggregated data
quantile(sample(data$cdu,    1000, replace=TRUE), na.rm=TRUE)
quantile(sample(data$spd,    1000, replace=TRUE), na.rm=TRUE)
quantile(sample(data$gruene, 1000, replace=TRUE), na.rm=TRUE)
quantile(sample(data$fdp,    1000, replace=TRUE), na.rm=TRUE)
quantile(sample(data$linke,  1000, replace=TRUE), na.rm=TRUE)
quantile(sample(data$piraten,1000, replace=TRUE), na.rm=TRUE)
quantile(sample(data$afd,    1000, replace=TRUE), na.rm=TRUE)
quantile(sample(data$sonst,  1000, replace=TRUE), na.rm=TRUE)

