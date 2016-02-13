happiness <- read.csv('Happiness Challenge/Happiness.csv')

#Country happiness
countryHappiness <- tapply(happiness$HAPPINESS, happiness$COUNTRY, mean)
sortedHappiness <- sort(countryHappiness)
happinessLength <- length(sortedHappiness)

#Various default variable
yLimit <- c(0, 10)
yLabel = "Average Happiness"
imageDirectory = "Happiness Challenge/Images/"

#Plot Unhappy countries
barplot(sortedHappiness[1:5], col=rainbow(5), ylim=yLimit, ylab=yLabel, main="Unhappy countries")

#Plot Happy Countries
barplot(sortedHappiness[(happinessLength - 4): happinessLength], ylab=yLabel, ylim=yLimit, col=rainbow(5), main="Happy Countries")

#Gender vs Happiness
genderHappiness <- tapply(happiness$HAPPINESS, happiness$GENDER, mean)
barplot(genderHappiness, col=rainbow(2), ylim=yLimit, ylab=yLabel, main="Gender Happiness")

#Age vs Happiness
ageHappiness <- tapply(happiness$HAPPINESS, happiness$AGE, mean)
barplot(ageHappiness, col=rainbow(length(ageHappiness)), ylim=yLimit, ylab=yLabel, main="Happiness At Ages")

#Gender vs. Happiness at different age ranges
youthGender <- subset(happiness, happiness$AGE >= 18 & happiness$AGE < 30)
middleGender <- subset(happiness, happiness$AGE >= 31 & happiness$AGE < 50)
oldGender <- subset(happiness, happiness$AGE >= 51 & happiness$AGE < 75)
reallyOldGender <- subset(happiness, happiness$AGE >= 76)

youthGender.mean <- tapply(youthGender$HAPPINESS, youthGender$GENDER, mean)
middleGender.mean <- tapply(middleGender$HAPPINESS, middleGender$GENDER, mean)
oldGender.mean <- tapply(oldGender$HAPPINESS, oldGender$GENDER, mean)
reallyOldGender.mean <- tapply(reallyOldGender$HAPPINESS, reallyOldGender$GENDER, mean)

means <- c(youthGender.mean, middleGender.mean, oldGender.mean, reallyOldGender.mean)
barplot(means, main="Happiness vs. Gender At Ages", ylab=yLabel, ylim=yLimit, 
        col=rainbow(length(means)), names.arg=c("18-30 M", "18-30 F", "31 - 50M", "31-50F", "51-75M", "51-75F", "76+ M", "76+F"))

genderMeans <- list(youthGender$HAPPINESS, middleGender$HAPPINESS, oldGender$HAPPINESS, reallyOldGender$HAPPINESS)

#For each of the 3 happiest countries
for (i in (length - 3): length) {
  
  #Plot a graph of the happiness with age groups
  plotHappinessVSAgeIntervals(countryIndex = i)
  plotHappinessVSAgeIntervals(countryIndex = i, factorGender=FALSE, showBoxPlot=FALSE)
  
  #Get the correlating unhappiest country and plot a graph of their happiness with age groups
  unhappyIndex = length - i + 1
  plotHappinessVSAgeIntervals(countryIndex=unhappyIndex)
  plotHappinessVSAgeIntervals(countryIndex=unhappyIndex, factorGender=FALSE, showBoxPlot=FALSE)
}

#Plots a graph of a country's happiness with age groups and gender
plotHappinessVSAgeIntervals <- function(countryIndex, factorGender=TRUE, showBoxPlot=TRUE) {
  
  #Country name
  country <- sortedHappiness[countryIndex]
  countryName <- names(country)
  
  #Default variables/text 
  ranking <- length - countryIndex + 1
  ranking <- paste(countryName, "(Happiest Country: #", ranking, ")")
  boxPlotXLabels <- c("18-30", "31-50", "51-75", "76+")
  
  #Get the age groups
  youthGender <- subset(happiness, happiness$AGE >= 18 & happiness$AGE < 30 & happiness$COUNTRY == countryName)
  middleGender <- subset(happiness, happiness$AGE >= 31 & happiness$AGE < 50 & happiness$COUNTRY == countryName)
  oldGender <- subset(happiness, happiness$AGE >= 51 & happiness$AGE < 75 & happiness$COUNTRY == countryName)
  reallyOldGender <- subset(happiness, happiness$AGE >= 76 & happiness$COUNTRY == countryName)
  
  #If we should factor gender
  if (factorGender) {
    
    xTitles <- c("18-30 F", "18-30 M", "31-50 F", "31-50 M", "51-75 F", "51-75 M", "76+ F", "76+ M")
    plotName <- paste("Happiness vs Gender at Different Ages in", ranking)
    
    #Calculate the averages for each gender
    youthGender.mean <- tapply(youthGender$HAPPINESS, youthGender$GENDER, mean)
    middleGender.mean <- tapply(middleGender$HAPPINESS, middleGender$GENDER, mean)
    oldGender.mean <- tapply(oldGender$HAPPINESS, oldGender$GENDER, mean)
    reallyOldGender.mean <- tapply(reallyOldGender$HAPPINESS, reallyOldGender$GENDER, mean)
  }
  
  #If we should not factor gender
  else {
    
    xTitles <- boxPlotXLabels
    plotName <- paste("Happiness at Different Ages in", ranking)
    
    #These averages are simple
    youthGender.mean <- mean(youthGender$HAPPINESS)
    middleGender.mean <- mean(middleGender$HAPPINESS)
    oldGender.mean <- mean(oldGender$HAPPINESS)
    reallyOldGender.mean <- mean(reallyOldGender$HAPPINESS)
  }
  
  #Data storing
  genderMeans <- c(youthGender.mean, middleGender.mean, oldGender.mean, reallyOldGender.mean)
  genderMeansRounded <- lapply(genderMeans, round, 2)
  generalDataList <- list(youthGender$HAPPINESS, middleGender$HAPPINESS, oldGender$HAPPINESS, reallyOldGender$HAPPINESS)
  
  #Plot the graph and add labels
  genderMeansPlot <- barplot(genderMeans, main=plotName, ylab=yLabel, ylim=yLimit, col=rainbow(length(genderMeans)), names.arg=xTitles)
  text(x=genderMeansPlot, y=genderMeans, label=genderMeansRounded, pos=3, cex = 1.2, col="red")
  
  #Show a box plot of the data
  if(showBoxPlot) {
    boxPlotTitle <- paste("Ranges of Happiness at Different Ages in", ranking)
    boxplot(generalDataList, main=boxPlotTitle, ylab=yLabel, col=rainbow(length(generalDataList)), names=boxPlotXLabels)
  }
}

#Saves the plot
saveGraph <- function(imageName) {
  fullName <- paste(imageDirectory, imageName, ".png", sep='')
  dev.copy(png, fullName, width = 4, height = 4, units = 'in', res = 300)
}