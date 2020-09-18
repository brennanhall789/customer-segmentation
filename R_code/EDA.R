## packages
library(faraway)
library(MASS)

### Data Import ###
data = read.csv('Mall_Customers.csv')
colnames(data) = c('ID', 'Gender', 'Age', 'Income', 'Score')
attach(data)
str(data)
any(is.na(data))


### Age ###
summary(Age)
sd(Age)

hist(Age)

## ?consider age groups?
b = c(17, 30, 50, 70)    #different break points? equal size groups?
names = c('young', 'middle', 'old')
data$age_groups = cut(Age, breaks = b, labels = names)
#head(data)


### Income ###
summary(Income)
hist(Income)

fit.norm = fitdistr(Income,"normal")
fit.norm[1]
qqnorm(Income); qqline(Income,col='red')
x <- rnorm(nrow(data), mean = fit.norm$estimate[1], sd = fit.norm$estimate[2])
boxplot(t(rbind(Income, x)), main = "Boxplot comparison of Normal distribution")

fit.gamma <- fitdistr(Income, "gamma", lower=0.0011)
fit.gamma[1]
qqplot(qgamma(ppoints(Income), shape = fit.gamma$estimate[1],
              rate = fit.gamma$estimate[2]),
       sort(Income), main="Gamma Q-Q plot",
       xlab="Theoretical Quantiles", ylab="Sample Quantiles")
qqline(Income, distribution =
         function(x) qgamma(x, shape = fit.gamma$estimate[1], 
                            rate = fit.gamma$estimate[2]))
x <- rgamma(45, shape = fit.gamma$estimate[1], rate = fit.gamma$estimate[2])
boxplot(t(rbind(Income,x)), main = "Boxplot comparison of Gamma distribution")

# Income could feasibly be Normal or Gamma. 

### Spending Score ###
summary(Score)
hist(Score)

