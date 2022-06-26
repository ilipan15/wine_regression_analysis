### --- importing libraries
library(ISLR)
library(MASS)
library(GGally)
library(dplyr)
library(stats)
library(corrplot)
library(corrgram)
library(ppcor)
library(ggplot2)
library(glm2)
library(psych)
library(car)
library(dbplyr)
library(tidyr)
library(scales)
library(glmnet)

### --- Importing the data 
wine <- read.table("wine.txt", header = TRUE)
View(wine)

#### --- getting the descriptive statistics
describe(wine)

### --- data cleaning in the price column (NA)
wine <- wine %>% drop_na(price)

### --- checking the summary of the every variable
summary(wine$year) 
summary(wine$price) 
summary(wine$h.rain) 
summary(wine$s.temp) 
summary(wine$w.rain)
summary(wine$h.temp)
summary(wine$parker)

### --- plotting of predictors
ggplot(wine, aes(x = year, y = price)) + geom_point(colour = "darkblue",alpha = 0.7) + ggtitle("Price vs Year")

ggplot(wine, aes(x = h.rain, y = price)) + geom_point(colour = "darkblue",alpha = 0.7) + ggtitle("Price vs H.rain")

ggplot(wine, aes(x = w.rain, y = price)) + geom_point(colour = "darkblue",alpha = 0.7) + ggtitle("Price vs W.rain")

ggplot(wine, aes(x = s.temp, y = price)) + geom_point(colour = "darkblue",alpha = 0.7) + ggtitle("Price vs Temperature in °C")

ggplot(wine, aes(x = h.temp, y = price)) + geom_point(colour = "darkblue",alpha = 0.7) + ggtitle("Price vs Temperature in °C at harvest")

ggplot(wine, aes(x = parker, y = price)) + geom_point(colour = "darkblue",alpha = 0.7) + ggtitle("Price vs Ratings") ## 27 rows removed because of missing values

### --- plotting the distribution of variables using histograms

ggplot(wine, aes(x = year)) + geom_histogram(binwidth = 4, color="black", fill="lightblue") + ggtitle("Distribution of year")

ggplot(wine, aes(x = price)) + geom_histogram(binwidth = 3, color="black", fill="purple") + ggtitle("Distribution of price")

ggplot(wine, aes(x = h.rain)) + geom_histogram(binwidth = 8, color="black", fill="lightblue") + ggtitle("Distribution of rain in the harvest month")

ggplot(wine, aes(x = w.rain)) + geom_histogram(binwidth = 8, color="black", fill="lightblue") + ggtitle("Distribution of rain in the winter preceding harvest")

ggplot(wine, aes(x = s.temp)) + geom_histogram(binwidth = 0.5, color="black", fill="lightblue") + ggtitle("Distribution of temperature in °C")

ggplot(wine, aes(x = h.temp)) + geom_histogram(binwidth = 0.5, color="black", fill="lightblue") + ggtitle("Distribution of temperature at harvest in °C")

ggplot(wine, aes(x = parker)) + geom_histogram(binwidth = 1, color="black", fill="lightblue") + ggtitle("Distribution of ratings")

### --- plotting boxplots examining of outliers
ggplot(wine) +  aes(x = "price", y = price)+ geom_boxplot(fill = "white", colour = "#3366FF")

ggplot(wine) +  aes(x = "h.rain", y = h.rain)+ geom_boxplot(fill = "white", colour = "#3366FF")

ggplot(wine) +  aes(x = "w.rain", y = w.rain)+ geom_boxplot(fill = "white", colour = "#3366FF")

ggplot(wine) +  aes(x = "s.temp", y = s.temp)+ geom_boxplot(fill = "white", colour = "#3366FF")

ggplot(wine) +  aes(x = "h.temp", y = h.temp)+ geom_boxplot(fill = "white", colour = "#3366FF")



### --- correlation matrix all variables
cor0 <- round(cor(wine, use = "pairwise.complete.obs"), 2)

### --- visualizing correlation matrix
palette = colorRampPalette(c("green", "white", "red")) (20)
corrplot(cor0)


### --- correlation matrix (without price to compare the rest of the predictors)
without_price = subset(wine, select = -c(2))

### --- visualizing correlation matrix
cor1 <- round(cor(without_price, use = "pairwise.complete.obs"), 2)
corrplot(cor1, method="pie")

corrplot(cor1)

corrplot.mixed(cor1, order = 'AOE')
### --- analysis of residuals for finding the "best" model 
### --- running simple linear regression (all variables, no transformation)
mod0 <- lm(price ~., data=wine)
summary(mod0)

### --- plotting
par(mfrow=c(2,2))
plot(mod0)

### --- log transformed response variable (all variables)
mod1 <- lm(log1p(price) ~., data=wine)
summary(mod1)

### --- plotting
plot(mod1)

### --- log transformed (without h.temp and parker)
mod2 <- lm(log1p(price) ~ year + h.rain + s.temp + w.rain, data=wine)
summary(mod2)

### --- plotting
plot(mod2)

### --- from the GLM models, Gamma regression with all predictors (based on the dataset)
# based on our data set we reject Binomial regression because its for logistics
# and Poisson is rejected because its for count data
wine.glm <- glm(price ~., family = Gamma(link = "log"), data=wine)
summary(wine.glm)


### --- Gamma regression removing two variables (h.temp, parker) # final model
mod4 <- glm(price ~ year + h.rain + s.temp + w.rain, family = Gamma(link = "log"), data=wine)
summary(mod4)

### --- plotting
par(mfrow=c(2,2))
plot(mod4)

### --- using stepAIC to decide significant variables (forwarding method)
mod5 <- glm(price ~ 1, data= wine)
stepAIC(mod5, ~ year + h.rain + s.temp + w.rain, data=wine)

### --- exponentiation of the glm coefficients
options(scipen = 999)
(1-exp(mod4$coefficients))*100

### --- AIC comparison between simple linear model and chosen GLM Gamma model (forwarding method)
stepAIC(mod0)
stepAIC(mod1)
stepAIC(mod2)
stepAIC(wine.glm)
stepAIC(mod4)

### --- VIF (variance inflation factor for detecting multicollinearity)
vif(mod0)
vif(mod1)
vif(mod2)
vif(wine.glm)
vif(mod4)

