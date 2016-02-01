#Read the data in
automobile.data <- read.csv('automobile.csv')

#Plot of price
plot(automobile.data$price)

plot(automobile.data$make,automobile.data$price)
plot(automobile.data$engine.size, automobile.data$horse.power)

r <- tapply(automobile.data$price,automobile.data$body.style,mean)
print(r)
barplot(r,ylim=c(0, max(r)))

stripchart(automobile.data$price)

price <- automobile.data$price
hist(price,ylim=c(0,120),xlim=c(0,50000),xlab='price',ylab='frequence')

#add eference line
abline(h = c(100)) 