#Data Source Link: http://archive.ics.uci.edu/ml/datasets/Taxi+Service+Trajectory+-+Prediction+Challenge%2C+ECML+PKDD+2015

#Read data from CSV file
taxi.data <- read.csv("Taxi Analyzation/Porto_taxi_data_training.csv")

#The time stamp as a vector
timeStamp = taxi.data$TIMESTAMP

#Convert the time stamp to a date object
class(timeStamp) = c('POSIXt', 'POSIXct')

#Store the hour for each time stamp
hours <- c(strftime(timeStamp, format="%H"))

#Convert the string vector to an int vector
hours <- as.integer(hours)

#Tabulate gets frequencies, max is most frequent value 
maxY <- max(tabulate(hours)) * 1.5; 

#Graph the values as a histogram
hist(hours, xlim = c(0, 25), xlab = 'Hour of day', ylab = 'Number of taxi rides', freq = TRUE, main = 'Number of Taxi Rides Throughout the Day in Portugal In 2014')

#Get the call type 
callType = taxi.data$CALL_TYPE

#Get the frequencies for each call type
callTypeFrequency = tabulate(callType)

#Graph the data
par(mar = c(6, 4.1, 4.1, 2.1))
barplot(callTypeFrequency, names.arg=c("Central", "Stand", "Street"), xlab = '', ylab = 
          'Number of trips', las = 2, ylim = c(0, max(callTypeFrequency) * 1.1), main = 'Number of Trips From Each Dispatch Location')
mtext(text = 'Trip dispatch location', side = 1, line = 4.5)