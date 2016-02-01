library(lattice)

print("Hello world");

a <- 4
print(a);
print(mode(a))

a <- TRUE
print(mode(a))

b <- "Test string"
print(mode(b))

#comment
v1 <- c(1, 3, 5, 6)
print(mode(v1))

v4 = 1+4i
print(mode(v4))

ls(pattern="a")

#v4 <- 3
print(as.numeric(v4) + 1)

xyplot(Petal.Length-Petal.width, data=iris, groups=Species,panel=panel.superpose, type=c('p', 'smooth').auto.key = list(x=0.15, y=0.95))