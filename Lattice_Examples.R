#examples of lattice, a package for plot
library(lattice)

data("iris")
xyplot(Petal.Length~Petal.Width,data=iris, groups=Species,panel=panel.superpose,type=c('p','smooth'),auto.key = list(x=0.15,y=0.85))
xyplot(Petal.Length~Petal.Width,data=iris, groups=Species,panel=panel.superpose,type=c('p','smooth'),auto.key = list(x=0.45,y=0.85))


#create a simple  object
s1 <- 5
s2 <- 'test'
s3 <- FALSE
s4 <- 1+5i

#check data type
print(mode(s1))

#object type and data type
#create a vector
v1 <- c(1,2,3,4,5)
v2 <- c(1:12)
v3 <- c(1,2,'test',4,TRUE)  # vector can contain different data type


#create a matrix
m1 <- matrix(c(1:9),nrow = 3,ncol=3)
m2 <- matrix(c(1:9),nrow = 3,ncol=3,byrow=TRUE)
m3 <- matrix(c(1:9),nrow = 3,ncol=3,byrow=TRUE,dimnames = list(c('student1','student2','student3'),c('math','algebra','data structure')))
m3[1,1]
m3["student1","math"]
# assign dimension name to a matrix
dimnames(m2) <- list(c('student1','student2','student3'),c('math','algebra','data structure'))


#a list can contain any type of object
l1 <- list(gender='male',grade=list(math=98,algebra=99,data_structure=23))
l1['gender']
l1['grade']
l1$grade$math


#concatenate two character
str1 = "data"
str2 = "myfile.xls"
dir <- paste(str1,str2,sep='/')

#check data type
is.numeric(v1)
is.character(v2)

#define your own function
calculate.days<- function(month){
  # create a object to contain sum
  month.days <- c(31,28,31,30,31,30,31,31,30,31,30,31) 
  sum <- 0
  for(i in 1:length(month)){
    # month can be categorized in different type
    sum = sum + month.days[month[i]]
  }
  return(sum)
}
mon = c(1,3,2,12)

#call the function
calculate.days(month=mon)

print(ls())


