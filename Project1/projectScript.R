activityData <- read.table("./activity.csv", sep=",", header=TRUE, na.strings = 'NA')
names(activityData)

intervals <- unique(activityData$interval)
dates <- unique(activityData$date)

dayActivityData <- split(activityData,activityData$date)
names(dayActivityData)

totalStepsPerDay <- sapply(dayActivityData,function(x) sum(x$steps,na.rm=FALSE))
#lapply(dayActivityData,function(x) sum(x$steps,na.rm=FALSE))
class(totalStepsPerDay)

hist(totalStepsPerDay, col="orange")

meanStepsPerDay <- mean(totalStepsPerDay, na.rm=TRUE)
meanStepsPerDay

medianStepsPerDay <- median(totalStepsPerDay, na.rm=TRUE)
medianStepsPerDay

#Misunderstood the instructions -> not mean of the readings for the day
#meanStepsPerDay <- lapply(dayActivityData,function(x) mean(x$steps,na.rm=FALSE))
# sapply(dayActivityData,function(x) mean(x$steps,na.rm=FALSE))
#Misunderstood the instructions -> not median of the readings for the day
#medianStepsPerDay <- lapply(dayActivityData,function(x) median(x$steps,na.rm=FALSE))
# sapply(dayActivityData,function(x) mean(x$steps,na.rm=FALSE))

intervalActivityData <- split(activityData,activityData$interval)

averageIntervalActivityData <- sapply(intervalActivityData,function(x) mean(x$steps,na.rm=TRUE))
averageIntervalActivityData

plot(x=intervals,
     y=averageIntervalActivityData,
     type="l")

max(averageIntervalActivityData)
intervalIndex <- which.max(averageIntervalActivityData)
intervals[intervalIndex]

library(data.table)
DT = data.table(steps=activityData$steps,
                date=activityData$date,
                interval=activityData$interval)
names(DT)
DT[,need_filler:={
        ifelse(is.na(steps),TRUE,FALSE);
}]
DT[,meanStepsByInterval:=mean(steps,na.rm=TRUE),by=interval]
head(DT)
DT[,modified_Steps:={
        ifelse(need_filler,meanStepsByInterval,steps);
}]
numOfNAs <- sum(DT$need_filler)

modDayActivityData <- split(DT,DT$date)

modTotalStepsPerDay <- sapply(modDayActivityData, function(x) sum(x$modified_Steps,na.rm=FALSE))
modMeanStepsPerDay <- mean(modTotalStepsPerDay, na.rm=FALSE)
modMeanStepsPerDay
modMedianStepsPerDay <- median(modTotalStepsPerDay, na.rm=FALSE)
modMedianStepsPerDay

#The average is unchanged
#The median has changed to match the average... I believe this can be accounted for since the values added were the average and would thus move the median to the average
hist(modTotalStepsPerDay, col = 'blue')

dates

DT[, weekday := weekdays(as.Date(date))]
unique(DT$weekday)

daysRecord <- DT$weekday
weekSection <- sapply(daysRecord, function(x) ifelse(x == "Saturday" || x == "Sunday","Weekend","Weekday") )
weekSectionFactor <- as.factor(weekSection)
unique(weekSectionFactor)

DT$weekSection <- weekSectionFactor

weekSectionActivityData <- split(DT,DT$weekSection)

weekendActivityData <- weekSectionActivityData$Weekend
weekdayActivityData <- weekSectionActivityData$Weekday

weekendIntervalActivityData <- split(weekendActivityData,weekendActivityData$interval)
weekdayIntervalActivityData <- split(weekdayActivityData,weekdayActivityData$interval)

weekendAvgIntervalActivityData <- sapply(weekendIntervalActivityData,function(x) mean(x$modified_Steps,na.rm=FALSE))
weekdayAvgIntervalActivityData <- sapply(weekdayIntervalActivityData,function(x) mean(x$modified_Steps,na.rm=FALSE))

weekendAvgIntervalActivityData
weekdayAvgIntervalActivityData

plot(x=intervals,
     y=weekendAvgIntervalActivityData,
     type="l")
plot(x=intervals,
     y=weekdayAvgIntervalActivityData,
     type="l")