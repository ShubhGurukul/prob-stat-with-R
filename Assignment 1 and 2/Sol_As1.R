# Task: Generate Random number for 750 students of 6 subjects using LCM 
#X(i+1)=(a*X(i)+c)%%m
a=56
c=1
m=101
lcg<-function(x,n)
{
  numbers<-vector(length=n)
  for(i in 1:n)
  {
    x=(a*x+c)%%m
    numbers[i]=x
  }
  return(numbers)
  
}
sub1<-lcg(88,750)
sub2<-lcg(56,750)
sub3<-lcg(32,750)
sub4<-lcg(89,750)
sub5<-lcg(99,750)
sub6<-lcg(77,750)

print(sub1)
print(sub2)
print(sub3)
print(sub4)
print(sub5)
print(sub6)

sum<-sub1+sub2+sub3+sub4+sub5+sub6

hist(sub1)
hist(sub2)
hist(sub3)
hist(sub4)
hist(sub5)
hist(sub6)
hist(sum)

# Task: Find the mean, median and standard deviation of the all the six subjects. 
print(paste("Means of All subjects : ", mean(sub1),mean(sub2),mean(sub3),mean(sub4),mean(sub5),mean(sub6)))
print(paste("Median of All subjects : ", median(sub1),median(sub2),median(sub3),median(sub4),median(sub5),median(sub6)))
print(paste("Standard Deviation of All subjects : ", sd(sub1),sd(sub2),sd(sub3),sd(sub4),sd(sub5),sd(sub6)))


all_marks <- matrix(nrow=750, ncol=6)
all_marks[1,] = c(89, 34, 99, 100, 78, 88)
a <- 44
c <- 89
m <- 101
for(i in 2:750){
  new_z <- (all_marks[i-1,]*a + c)%%m
  all_marks[i,] = new_z
}
sorted_marks <- sort(all_marks)

outlier<-function(data)
{
  q<-as.numeric(quantile(data))
  IQR=q[4]-q[2]
  min=q[2]-1.5*IQR
  max=q[4]+1.5*IQR
  for(val in data)
  {
    #print(val>max)
    if(val<min || val>max)
    {
      print("Outlier")
      print(val)
    }
  }
  return("Done")
}

outlier(all_marks)
outlier(sorted_marks)
