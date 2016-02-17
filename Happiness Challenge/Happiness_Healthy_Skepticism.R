happiness <- read.csv('Happiness Challenge/Happiness.csv')

#Various default variable
yLimit <- c(0, 10)
yTitles = c("18-30", "30+")
yLabel = "Happiness"
plotTitle = "Happiness vs. Age in Different Groups"

#Our Young Adults Age Group
youngAdults <- subset(happiness, happiness$AGE >= 18 & happiness$AGE < 30)
youngAdults.happiness <- youngAdults$HAPPINESS

#The rest of the population
restOfPopulation <- subset(happiness, happiness$AGE >= 30)
restOfPopulation.happiness <- restOfPopulation$HAPPINESS

#Plot the data to show distribution
boxplot(youngAdults$HAPPINESS, restOfPopulation$HAPPINESS, names=yTitles, main=plotTitle, ylim=yLimit, col=rainbow(5), ylab=yLabel)

#Calculate the means
youngAdults.mean <- mean(youngAdults$HAPPINESS)
restOfPopulation.mean <- mean(restOfPopulation$HAPPINESS)

#Plot the means as a bar graph
means <- c(youngAdults.mean, restOfPopulation.mean)
barplot(means, main="Happiness vs. Gender At Ages", ylab=yLabel, ylim=yLimit, col=rainbow(length(means)), names.arg=yTitles)

#Run through some simulations
sampleCount <- 1000

#To hold the averages of each sample
youngAdultSample <- vector("numeric", sampleCount)
restAdultSample <- vector("numeric", sampleCount)

#Performs the simulation 
for(i in 1:sampleCount) {
  #Have the vector hold the average of a random sample created from the respective group's data
  youngAdultSample[i] <- mean(youngAdults.happiness[sample(sampleCount, length(youngAdults), replace=TRUE)])
  restAdultSample[i] <- mean(restOfPopulation.happiness[sample(sampleCount, length(restOfPopulation), replace=TRUE)])
}

#Plot a histogram of the distribution of means from the simulation
hist(youngAdultSample, breaks=20, main="Average Happiness from Young Adults Sample", xlab="Happiness", col=rainbow(20))
hist(restAdultSample, breaks=20, main="Average Happiness from Rest of Adults Sample", xlab="Happiness", col=rainbow(20))

#Do a t-test to get the P-Value
#Hypothesis is youth are happier than the rest of the population
#Null hypothesis is youth are not happier than the rest of the population
print(t.test(youngAdultSample, restAdultSample))