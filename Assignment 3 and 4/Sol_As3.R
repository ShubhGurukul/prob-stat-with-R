library(moments)

randommarks<-function()
{
  num<- 750
  subject<- vector(length = num)
  subject <- floor(runif(750, min = 1, max = 100))
  rmean <- mean(subject)
  cat("\nmean is ",rmean)
  rmedian <- median(subject)
  cat( "\nmedian is ",rmedian)
  cat("\nstandard deviation is ",sd(subject),"\n")
  sumi<-sum(subject)
  hist(subject,xlab="Marks ",ylab = "No. of students",col="green",border="blue")
  
  subject=sort(subject)
  iqr1<-IQR(subject)
  q1<-quantile(subject,prob=c(.25))
  q3<-quantile(subject,prob=c(.75))
  min_range = q1-(1.5*iqr1)
  max_range = q3+(1.5*iqr1)
  
  cat("min range iqr : ",min_range,"\nmax_range iqr : ",max_range,"\nrange : 1,100","\nmean absolute deviation : ",mad(subject),"\nvariance : ",var(subject),"\nskewness : ",skewness(subject),"\nkurtosis : ",kurtosis(subject),"\n\n")
  
  
  return(sumi)
  
}
#subje<-randommarks()
sumi<-vector(length = 6)
for(i in 1:6){
  sumi[i]<-randommarks()
  #cat("SUM : ",sumi,"\n\n")
}

cat("mean of total : ",mean(sumi),"\nmedian of total : ",median(sumi),"\nstandard deviation of total : ",sd(sumi),"\n\n")
hist(sumi,xlab="Total marks in subjects",ylab = "No. of subjects",col="yellow",border="blue")

