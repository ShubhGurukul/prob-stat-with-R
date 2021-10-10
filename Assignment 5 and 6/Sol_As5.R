
# Question 1

res = 0 
for(i in 1:100){ 
  sel = rbinom(n=250, size=1, prob=0.6) 
  prob = sum(rle(sel)$length == 16) 
  print(prob/250) 
  res = res + (prob/250); 
} 
print(paste("Average prob for 100 rounds:", res/100))

# Question 2

#Probability for Sample Size 8 
sel1 = rbinom(n=8,size=1, prob=0.5) 
#Calculating probability for 2 or more occurence and subtracting from 1 to get compliment
a1 = sum(rle(sel1)$length >= 2) 
print(paste("Probability in sample size 8:",1 - a1/8)) 

#Probability for Sample Size 250 
sel2 = rbinom(n=250,size=1, prob=0.5) 

#Calculating probability for 2 or more occurence and subtracting from 1 to get compliment 
a2 = sum(rle(sel2)$length >= 2) 
print(paste("Probability in sample size 250:",1 - a2/250)) 
