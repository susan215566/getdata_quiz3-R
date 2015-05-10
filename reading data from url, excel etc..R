library(datasets)
data(iris)
?iris

sapply(split(iris$Sepal.Length,iris$Species),mean)

apply(iris[, 1:4], 2, mean)
library(datasets)
data(mtcars)
?mtcars
lapply(mtcars, mean)
sapply(split(mtcars$mpg, mtcars$cyl), mean)
26.66364-15.1

getwd()
if(!file.exists("tidydata")){
        dir.create("tidydata")
}

file<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(file, destfile="./tidydata/survey.csv",method="curl")
list.files("./tidydata")
data=read.csv("./tidydata/survey.csv")

head(data$VAL)
data2=data[data$VAL==24,37:38]
head(data2)
good<-complete.cases(data2)
nrow(data2[good,])
nrow(data3)
data2=data[data$VAL==c("24"),37:38]
nrow(data2)
head(data2)



filex<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(filex,destfile="./tidydata/cloud.xlsx",method="curl")
list.files("./tidydata")
library(xlsx)
install.packages("xlsx")
cloud<-read.xlsx("./tidydata/cloud.xlsx",sheetIndex=1,header=TRUE)
head(cloud)
colIndex<-7:15
rowIndex<-18:23
dat<-read.xlsx("./tidydata/cloud.xlsx",sheetIndex=1,colIndex=colIndex,rowIndex=rowIndex)
sum(dat$Zip*dat$Ext,na.rm=T) 




library(XML)
install.packages("XML")

files<-"http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc<-xmlTreeParse(files,useInternal=T)
rootNote<-xmlRoot(doc)
xmlName(rootNote)
names(rootNote)
head(rootNote)
rootNote[[1]]
rootNote[[1]][[1]]
xmlSApply(rootNote,xmlValue)
zip=xpathSApply(rootNote, "//zipcode",xmlValue)
zip2=zip[zip==21231]
str(zip2)

file3<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(file3,destfile="./tidydata/cfront.csv",method="curl")
DT<-fread("./tidydata/cfront.csv")
head(DT)
?fread
??fread
library(data.table)
install.packages("data.table")
system.time(mean(DT$pwgtp15,by=DT$SEX))
system.time(rowMeans(DT)[DT$SEX==1]& rowMeans(DT)[DT$SEX==2])
system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
system.time(tapply(DT$pwgtp15,DT$SEX,mean))
system.time(DT[,mean(pwgtp15),by=SEX])
system.time(mean(DT[DT$SEX==1,]$pwgtp15)&mean(DT[DT$SEX==2,]$pwgtp15))



x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit<-lm(y~x)
summary(fit)
library(UsingR)
data(mtcars)
fit1<-lm(mpg~wt,data=mtcars)
confint(fit1)
fit2<-lm(mpg~I(wt*2),data=mtcars)
confint(fit2)
summary(fit2)
?mtcars
predict(fit,newdata=data.frame(wt=3),interval="prediction")
fit3<-lm(mpg~I(wt-mean(wt)),data=mtcars)
summary(fit2)
confint(fit3)


library(dplyr)
data1=filter(data, AGS==6 & ACR ==3)
head(data1)
data2=select(data1,AGS,ACR)
head(data2)
data3=mutate(data,agricultureLogical= AGS == 6 $ ACR==3)
agricultureLogical<-c(data$AGS == 6 $ data$ACR == 3)

agricultureLogical<-(data$ACR==3 & data$AGS==6)
which(agricultureLogical)
class(agricultureLogical)

install.packages("jpeg")
library(jpeg)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(fileUrl, destfile = "jeff.jpg", method = "curl")
img.n<-readJPEG("jeff.jpg",TRUE)
quantile(img.n,probs=c(0.3,0.8))

fileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileURL, destfile = "GDP.csv", method= "curl" )
gdp<-read.csv("GDP.csv")
fileURL2<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv "
download.file(fileURL2, destfile ="country.csv",method="curl")
edu<-read.csv("country.csv")
names(gdp)
names(edu)
head(select(edu,Income.Group),n=20)
head(select(edu,CountryCode))
gdpclean<-gdp[5:194,]
mergedData=as.data.frame(merge(gdpclean,edu,by.x="X",by.y="CountryCode"))
arrange(mergedData,desc(Gross.domestic.product.2012))
tapply(mergedData$Gross.domestic.product.2012,mergedData$Income.Group,mean,simplify=T)
194-5+1


quantile(mergedData$Gross.domestic.product.2012,probs=c(0.2,0.4,0.6,0.8,1))
library(Hmisc)
mergedData$gdp=cut2(mergedData$Gross.domestic.product.2012,g=5)
a=table(mergedData$Income.Group,mergedData$gdp)
colSums(a)
a
