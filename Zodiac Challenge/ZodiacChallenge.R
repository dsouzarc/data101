zodiac.data <- read.csv("Zodiac Challenge/ZodiacChallenge.csv")

boxplot(zodiac.data$AGE~zodiac.data$ZODIAK, ylab="Age", names.arg = zodiac.data$ZODIAK, col=rainbow(10), las=2)

boxplot(zodiac.data$YEARS~zodiac.data$ZODIAK, ylab="Years", col=rainbow(10), las=2, names.arg = zodiac.data$ZODIAK)

#qqplot(zodiac.data$ZODIAK, zodiac.data$CAPITALGAINS)

mosaicplot(table(zodiac.data$ZODIAK, zodiac.data$EDUCATION), color=rainbow(10), main="Symbol vs education year", las=2)

print(summary(zodiac.data))

mosaicplot(table(zodiac.data$ZODIAK, zodiac.data$NATIVE), color=rainbow(10), main="Symbol vs education year", las=2)

#education <- rep("post-HS",nrow(zodiac.data))
#education[zodiac.data$EDUCATION %in% c(" 1st-4th", " 5th-6th", " 7th-8th", " 9th", " 10th", " 11th", " 12th", " HS-grad")] <- "HS"
#mosaicplot(table(zodiac.data$EDUCATION,zodiac.data$ZODIAK),color=3:4,xlab="Hours per week",main="education v. Salary")