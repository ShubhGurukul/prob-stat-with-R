# Question 1

# a) Plot the density of the simulated values
r1 <- rnorm(1000, mean = 1.7, sd = 0.1)
plot(density(r1), col ='Green')

# b) Generate 10000 values with the same parameters and plot the respective density 
#     function on top of the previous plot in red to differentiate it.
r2 <- rnorm(10000, mean = 1.7, sd = 0.1)
lines(density(r2), col='Red')

# c) Find the 90% interval of a population with mean = 1.70 and standard deviation = .1 
#    between 0.05 and 0.95.

left <- qnorm(0.05, mean=1.7, sd=0.1)
right <- qnorm(0.95, mean = 1.7, sd = 0.1)


# d) Calculate the qvalue corresponding to every percentile in standard normal distribution.

percentiles <- seq(0.01, 0.99, 0.01)
qnorm(percentiles)

# e) Calculte the pvalues corresponding to z values ranging from 0 to 1 at an interval of 0.05.

z <- seq(0, 1, 0.05)
pnorm(z)


# Question 2.
# Set the environment 

    # > print(getwd())
    # [1] "C:/Users/admin/Documents"
    # > setwd("C:/Users/admin/Desktop/thapar/5th Sem/2 Prob & Stats(UCS410)/Lab")
    # > print(getwd())
    # [1] "C:/Users/admin/Desktop/thapar/5th Sem/2 Prob & Stats(UCS410)/Lab"
    # 
data <- read.csv('auto.csv')




# a) Calculate simple (linear) correlation between car price and its fuel economy 
#  (measured in miles per gallon, or mpg)
correlation <- cor(data$Price, data$MPG)

# b) Create a correlation matrix by selecting each pair of columns from dataset one by 
# one and calculate correlation between selected pairs. Fill the values in matrix 
# named as correlation matrix.

numerical_columns <- data[, 3:12]

correlation_matrix <- cor(numerical_columns)
correlation_matrix


# c) Create a new dataframe, auto_num, that contains only columns with numeric 
# values from the auto dataframe. You can do this using the Filter function. Use the 
# cor function to create a matrix of correlation coefficients for variables in the 
# auto_num dataframe
install.packages('corrgram')
library('corrgram')
corrgram(numerical_columns, lower.panel= panel.cor,
         upper.panel = panel.pts)



###############################
heights<-rnorm(1000, 1.70, 0.1)
d<-(density(heights))
plot(d, main = 'Density')
polygon(d, border = 'blue')
heights1<-rnorm(10000, 1.70, 0.1)
d1<-(density(heights1))
lines(d1, main = 'Density')
polygon(d1, border = 'red')
c(qnorm(0.05, 1.70, 0.1), qnorm(0.95, 1.70, 0.1))
quantiles<-seq(0, 1, by = .05)
qvalues<-qnorm(quantiles)
plot(qvalues,
     type = 'l',
     xaxt = 'n',
     xlab = 'Probability Density',
     ylab = 'Z-scores')
pvalues<-pnorm(quantiles)
plot(pvalues,
     xaxt = 'l',
     main = 'cdf of the standard normal',
     xlab = 'Quantiles',
     ylab = 'Probability Density')

# Question 2
a<-read.csv("auto.csv")
res1 <- cor.test(a[,3], a[, 4], method = "pearson")
print(res1)

data1<-a[, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12)]
correlation_matrix <- cor(data1)
df_new <- data.frame(data1)

library(corrgram)
corrgram(df_new, order = TRUE,
         lower.panel = panel.shade,
         upper.panel = panel.pie,
         text.panel = panel.txt,
         main = "correlegram")
data2<-a[, c(3, 4, 5, 6)]
df2 <- data.frame(data2)
corrgram(df2, order = TRUE,
         lower.panel = panel.shade,
         upper.panel = panel.pts,
         text.panel = panel.txt,
         main = "correlegram")





