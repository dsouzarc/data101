zodiac.data <- read.csv("Zodiac Challenge/ZodiacChallenge.csv")

#Organize the data we need - capital gains and profession
professionCapitalGains <- tapply(zodiac.data$CAPITALGAINS, zodiac.data$PROFESSION, mean)

#Sort that data
professionCapitalGains.sorted <- sort(professionCapitalGains)
professionCapitalGains.length <- length(professionCapitalGains)

#Plot it
plot <- barplot(professionCapitalGains.sorted, names.arg=names(professionCapitalGains.sorted), 
                col=rainbow(professionCapitalGains.length), ylab="Capital Gains in $", 
                main="Profession vs. Capital Gains", xaxt="n")

#Text formatting to make it look pretty
text(plot, par("usr")[3], labels=names(professionCapitalGains.sorted), srt=65, adj=c(1.0, 1.0), xpd=TRUE, cex=0.7)