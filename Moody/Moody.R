moody <- read.csv("Moody/MOODY.csv")

#Converts the categorical values to numeric ones for analysis
convertValues <- function(data) {
  data <- as.matrix(data)
  data[tolower(data) == "always" | tolower(data) == "yes"] <- "0"
  data[tolower(data) == "frequently"] <- "1"
  data[tolower(data) == "sometimes"] <- "2"
  data[tolower(data) == "rarely"] <- "3"
  data[tolower(data) == "never" | tolower(data) == "no"] <- "4"
  return(as.numeric(unlist(as.data.frame(data))))
}

#Get our groups 
goodRange <- subset(moody, moody$GRADE == 'A' | moody$GRADE == 'B')
failRange <- subset(moody, moody$GRADE == 'D' | moody$GRADE == 'F')

#Change our categorical data into quantitative
moody$ON_SMARTPHONE <- convertValues(moody$ON_SMARTPHONE)
moody$ASKS_QUESTIONS <- convertValues(moody$ASKS_QUESTIONS)
moody$LEAVES_EARLY <- convertValues(moody$LEAVES_EARLY)
moody$LATE_IN_CLASS <- convertValues(moody$LATE_IN_CLASS)

#Get our groups
goodRange <- subset(moody, moody$GRADE == 'A' | moody$GRADE == 'B')
failRange <- subset(moody, moody$GRADE == 'D' | moody$GRADE == 'F')

#Plot the relationships between good and bad students
plotDensity(goodRange$ON_SMARTPHONE, failRange$ON_SMARTPHONE, "Good vs Bad Student Phone Usage")
plotDensity(goodRange$ASKS_QUESTIONS, failRange$ASKS_QUESTIONS, "Good vs Bad Student Asks Questions")
plotDensity(goodRange$LEAVES_EARLY, failRange$LEAVES_EARLY, "Good vs Bad Student Leaves Early")
plotDensity(goodRange$LATE_IN_CLASS, failRange$LATE_IN_CLASS, "Good vs Bad Student Late in Class")

#Print some t-test values
print(t.test(goodRange$ON_SMARTPHONE, failRange$ON_SMARTPHONE))
print(t.test(goodRange$ASKS_QUESTIONS, failRange$ASKS_QUESTIONS))
print(t.test(goodRange$LEAVES_EARLY, failRange$LEAVES_EARLY))
print(t.test(goodRange$LATE_IN_CLASS, failRange$LATE_IN_CLASS))

#Plots a density graph of both sets of data
plotDensity <- function(data1, data2, title) {
  density1 <- density(data1)
  density2 <- density(data2)
  
  plot(density(goodRange$ON_SMARTPHONE), main=title, col=rainbow(5))
  lines(density(failRange$ON_SMARTPHONE), col="black")
}
