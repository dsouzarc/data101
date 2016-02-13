happiness <- read.csv('Happiness Challenge/Happiness.csv')

print(summary(happiness))

#Clean the data
happiness.age.subset <- subset(happiness, happiness$AGE > 0 & happiness$AGE < 120)

#List/Vector with every element as the result of the function being applied
ageMean <- lapply(list(happiness$AGE), mean)
happinessMean <- lapply(list(happiness$HAPPINESS), mean)

#Gets the averages separately
ageGender <- tapply(happiness$AGE, happiness$GENDER, mean)

russianIndian <- subset(happiness, happiness$COUNTRY == 'Russia' | happiness$COUNTRY == 'India')

russianIndian.gender <- factor(russianIndian$GENDER)
russianIndian.country <- factor(russianIndian$COUNTRY)

mosaicplot(table(russianIndian.country, russianIndian.gender), col=c(3:4), main="Gender distribution", xlab="Country", ylab="Gender")

countryHappiness <- tapply(happiness$HAPPINESS, happiness$COUNTRY, mean)
barplot(c(countryHappiness["India"], countryHappiness["China"], countryHappiness["South Korea"], countryHappiness["United States"]), col=c(1:4))

sortedHappiness <- sort(countryHappiness)
barplot(sortedHappiness[1:5], col=c(1:10), main="Unhappy countries")

length <- length(sortedHappiness)
barplot(sortedHappiness[(length - 4): length], col=c(1:10), main="Happy countries")

barplot(c(countryHappiness["India"], countryHappiness["China"], countryHappiness["South Korea"], 
          countryHappiness["United States"]), col=c(1:4), las=1, horiz=TRUE)

genderHappiness <- tapply(happiness$HAPPINESS, happiness$GENDER, mean)
barplot(genderHappiness)