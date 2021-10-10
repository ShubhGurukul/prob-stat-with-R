# Question 3
fun<- function(b,a)
{
  x=sample(b)
  if(identical(x,b))
    return(1)
  else{
    return(0)
  }
  
}
a<-c("dog","cat","rat","lion","fox","tiger")
b=sort(a)
n=1e4
s<-replicate(n,fun(b,a))
print(sum(s)/n)



# Question 4
fun<- function(b,a)
{
  x=sample(b)
  if(identical(x,b))
    return(1)
  else{
    return(0)
  }
  
}
a<-c("dog","dog","dog","horse","horse","horse")
b=sort(a)
n=1e4
s<-replicate(n,fun(b,a))
print(sum(s)/n)
