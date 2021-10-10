
library(datasets)
data(iris)
summary(iris)
library(datasets)
data(iris)
summary(iris)
print(iris)

# colMeans(iris[sapply(iris,is.numeric)])
# apply(iris[, c(2:ncol(iris))], 2, median)
Dataset <- iris
get <- function(x){
  unq <- unique((x))
  unq[which.max(tabulate(match(x,unq)))]
}
for(i in 2:ncol(Dataset)){
  mod_val<- get(Dataset[,i])
  print(mod_val)
}
apply(Dataset[,c(2:ncol(Dataset))], 2, sd)
hist.data.frame(Dataset)
closest<-function(xv,sv){
  xv[which(abs(xv-sv)==min(abs(xv-sv)))] }
outliers<-function(v){
  dataSet<- sort(v)
  summary(dataSet)
  Q1 <- quantile(dataSet, 0.25)
  print(paste(Q1))
  Q1<-min(unique(closest(dataSet,Q1)))
  print(paste("updated=",Q1))
  Q3 <- quantile(dataSet, 0.75)
  print(paste(Q3))
  Q3<-max(unique(closest(dataSet,Q3)))
  print(paste("updated=",Q3))
  qr_<-Q3-Q1
  print(paste(iqr_))
  iqr<-IQR(dataSet)
  print(paste(iqr))
  low_lim<- (Q1-(1.5*iqr_))
  print(paste("low",low_lim))
  high_lim<- (Q3+(1.5*iqr_))
  print(paste("high",high_lim))
  outlier <- c()
  for (i in dataSet){
    if ((i>high_lim) || (i<low_lim)){
      outlier <- append(outlier, i)
    }
  }
  print(paste("Outliers = " , outlier))
}
for (i in 2:ncol(Dataset)){
  mod_val<- outliers(Dataset[,i])
  print(mod_val)
}
