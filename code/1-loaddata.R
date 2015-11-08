rm(list = ls())

require(car)
require(reshape2)
require(stringr)
require(plyr)
require(xlsx)

data <- read.csv("data/IRQ_IDP_ProtMonitoring_CentreS_2015_10_26_01_44_54.csv", encoding="UTF-8", na.strings="n/a")


#names(data)


### let's re-encode the form using the xlsfrom definition

#aggreagtion method
questions <- read.xlsx("data/IRQ_IDP_ProtMonitoring_CentreS.xls",sheetIndex=1,stringsAsFactors=FALSE,encoding="UTF-8")

## we can now delete the "begin group" and "end group" rows
questions <- subset(questions, type !="begin group" & type !="end group")
##questions <- questions[ which(questions$type!="begin group" & questions$type!="end group"), ]

## extracting multiple choice questions
questions$multiple <- with(questions,  ifelse(grepl("select_multiple", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  questions$type),
                                                                 paste0( substr(questions$type ,
                                                                          (regexpr("select_multiple", questions$type , ignore.case=FALSE, fixed=TRUE))+16,250)),paste0("") ))
questions.multiple <- questions[questions$multiple!="",]
question.multiple <- merge(x=questions.multiple, y=choices, by.x="multiple",by.y="list.name", all.x=TRUE) 

#list of choices from ODK
choices <- read.xlsx("data/IRQ_IDP_ProtMonitoring_CentreS.xls",sheetIndex=2,stringsAsFactors=FALSE,encoding="UTF-8")


## Creating a label conversion table -- with cleaned variable name 


### Kobo exports data vairable with concatenated groups names and dots separator
## The make the data more user friendly we restore the orginal xform defintion
## parsing dots in variable name from kobo assuming that 
# we do not have more than 4 nested groups within the form
rm(names.data)
names.data <- as.data.frame(as.character(names(data))) 
names(names.data) <- "labelkobo"
names.data$labelshort1 <-substring(names.data$labelkobo, regexpr(".", names.data$labelkobo, fixed=TRUE) + 1, length(names.data$labelkobo))
names.data$labelshort2 <-substring(names.data$labelshort1, regexpr(".", names.data$labelshort1, fixed=TRUE) + 1, length(names.data$labelshort1))
names.data$labelshort3 <-substring(names.data$labelshort2, regexpr(".", names.data$labelshort2, fixed=TRUE) + 1, length(names.data$labelshort2))
names.data$name <-substring(names.data$labelshort3, regexpr(".", names.data$labelshort3, fixed=TRUE) + 1, length(names.data$labelshort3))
## This will match all questions name except select_multiple questions

##names.data1 <- merge(x=names.data, y=questions, by.x="labelshort4",by.y="name")
## not using merge because it does not keep row orders.
names.data <- join(x=names.data, y=questions, by="name",type="left",match="first")

## changing the default variable names for coordinates
names.data$name[names.data$name=="_GPS_coordinates_longitude"] <- "longitude"
names.data$name[names.data$name=="_GPS_coordinates_latitude"] <- "latitude"
names.data$name[names.data$name=="_GPS_coordinates_altitude"] <- "altitude"
names.data$name[names.data$name=="_GPS_coordinates_precision"] <- "precision"

##names.data <- within(names.data,  names.data$name <- ifelse(is.na(names.data$label), names.data$labelshort2, names.data$name))
##names.data[is.na(names.data$label),names.data$name] <-  names.data[is.na(names.data$label),names.data$labelshort2] 

## trying to match using fuzzy string match
#source("code/partialmatch.R")
#matches <- partialMatch(names.data$labelshort, questions$name)



## We can start relabelling all questions already
n <- length(names.data)
for (i in 1:n ) {
  attributes(data)$variable.labels[i] <- names.data[i,"label"]
  
}
names(data) <- names.data[,"name"]
names(data)

##

### Eliminate record without coordinates
data.map <-data[!rowSums(is.na(data["longitude"])), ]


## get a total number of persons
# Function that will sum values even if we have NA
psum <- function(..., na.rm=FALSE) {
  x <- list(...)
  rowSums(matrix(unlist(x), ncol=length(x)), na.rm=na.rm)
}

data$total <- psum(data$Male_0_5,
                   data$Female_0_5,
                   data$Male_16_17,
                   data$Female_6_17,
                   data$Male_18_60,
                   data$Female_18_60,
                   data$Male_over_60,
                   data$Female_over_60, na.rm = TRUE) 



