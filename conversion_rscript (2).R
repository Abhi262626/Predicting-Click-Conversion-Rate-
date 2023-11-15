rm(list = ls())
library(rio)
df = import("Final SDM Project File.xlsx")
View(df)
str(df)
table(df$CURRENCY)
table(df$BRAND)
table(df$SHOP_LOCATION)
table(df$CATEGORY)


#Check for NULLs
colSums(is.na(df))
#review rating, review count, sold, historical sold, 
#liked count has NULLs
#show_count has lot of NULLS so removing them from analysis
df$SHOW_DISCOUNT = NULL

df <- df[complete.cases(df),]  
colSums(is.na(df))

#subset the data by removing sold = 0 and view_count = 0 from dataset
df = subset(df, df$SOLD!=0 | df$VIEW_COUNT!=0)
df = subset(df, df$DEPARTMENT == "Men Clothes")
table(df$DEPARTMENT)
View(df)

#factors
df$CATEGORY = factor(df$CATEGORY)
df$SHOP_LOCATION = factor(df$SHOP_LOCATION)
df$DEPARTMENT = factor(df$DEPARTMENT)
df$CURRENCY = factor(df$CURRENCY)

#controlling for time, we need to extract year and month from create time variable
df$CREATE_TIME_YEAR = format(df$CREATE_TIME, "%Y")
df$CREATE_TIME_YEAR = as.factor(df$CREATE_TIME_YEAR)
df$CREATE_TIME_MONTH = format(df$CREATE_TIME, "%b")
df$CREATE_TIME_MONTH = as.factor(df$CREATE_TIME_MONTH)

#Predictors

#Sale price
#review rating
#review count
#historical sold
#stock
#discount %
#liked_count
#create_year
#category

#response variable - conversion
df$conversion = df$SOLD/df$VIEW_COUNT
#conversion rate in %
df$conv_rate = df$conversion*100
df$conv_rate = round(df$conv_rate,0)

summary(df$conv_rate)

#histogram of conversion
hist(df$conversion, col = "red", probability = T)
hist(log(df$conversion), col = "red", probability = T)

#histogram of conversion rate
hist(df$conv_rate, col = "red", probability = T)
hist(log(df$conv_rate), col = "red", probability = T)
#log distribution looks even more dispersed


hist(df$VIEW_COUNT, col = "blue", prob = T)
hist(log(df$VIEW_COUNT), col = "blue", prob = T)

plot(df$REVIEW_COUNT,df$conversion)
unique(df$conversion)

plot(df$conversion~df$CREATE_TIME_YEAR, pch = 19)


library(lattice)
bwplot(~df$conversion | df$CATEGORY)
boxplot(df$conv_rate ~ df$CATEGORY, las = 2,NAMES=NULL,cex.axis=0.35,
        xlab = " ", ylab = " ")

##check for collinearity
df_num = c("SALE_PRICE","REVIEW_RATING","REVIEW_COUNT","LIKED_COUNT",
           "DISCOUNT","conv_rate")
library(corrplot)
m = cor(df[,df_num])
corrplot(m, method = "number")

View(df)
str(df)

summary(df$conv_rate)

#conversion rate is a censored data
#use tobit model 

#since DV: Conversion is sold/impression which is count/count
# a percentage, therefore we can use OLS regression 

#base model - linear model withh all predictors

lm1 = lm(conv_rate ~ SALE_PRICE + REVIEW_RATING + REVIEW_COUNT + LIKED_COUNT +
           HISTORICAL_SOLD + STOCK + DISCOUNT + CATEGORY + CREATE_TIME_YEAR, 
         data = df)

summary(lm1)

lm2 = lm(conv_rate~CREATE_TIME_YEAR, data = df)
summary(lm2)

#tobit model 

library(AER)
tobit1 = tobit(conv_rate ~ SALE_PRICE + REVIEW_RATING + REVIEW_COUNT +
              LIKED_COUNT + HISTORICAL_SOLD + DISCOUNT + STOCK ,
              left = 0, right = 100, data = df)
summary(tobit1)

truncreg1 = truncreg(conv_rate ~ SALE_PRICE + REVIEW_RATING + REVIEW_COUNT +
                 LIKED_COUNT + HISTORICAL_SOLD + DISCOUNT + DEPARTMENT + STOCK,
               point = 1000, direction = , data = df)

library(censReg)
censreg1 = censReg(conv_rate ~ SALE_PRICE + REVIEW_RATING + REVIEW_COUNT +
                     LIKED_COUNT + HISTORICAL_SOLD + DISCOUNT + STOCK ,
                   left = 0, right = 100, data = df)


tobit2 = tobit(conv_rate ~ SALE_PRICE*STOCK + REVIEW_RATING + REVIEW_COUNT +
                 LIKED_COUNT + HISTORICAL_SOLD+ DISCOUNT, left = 0, right = 100, 
               data = df)
summary(tobit2)


#assumptions for tobit model
residuals <- residuals(tobit1)
fitted_values <- fitted(tobit1)
length(tobit1$fitted.values)
plot(fitted_values, residuals, xlab = "Predicted values", ylab = "Residuals")
abline(0,0,col = "red", lwd = 3)


vif(tobit1)
#There is no multicollinearity - Pass
library(car)
durbinWatsonTest(residuals(tobit1))
#Fail
library(lmtest)
dwtest(residuals(tobit1))

#interaction between stock and price to check when price is low 
#and stock is high - does conversion improve?
#how different categories affect conversion with discount and higher review rating respectively

library(stargazer)
stargazer(lm1, tobit1, tobit2, type = "text", out = "out.txt", single.row = TRUE)


#calculate psedo R2
library(pscl)
pR2(tobit2)['McFadden']

pR2(tobit2, method = "mckelveyZavoina")

#linearity
residuals(tobit2)
plot(tobit2)
#plot(df$conv_rate, tobit2$fitted.values, pch = 19,main = "Conversion rate Vs Fitted Values", xlab = "Conversion rate", ylab = "Fitted Values")
#abline(0,0,lwd = 3, col = "red")


















