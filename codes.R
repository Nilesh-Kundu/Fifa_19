library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)

setwd("C:\\Users\\ADMIN\\Desktop\\Fifa_19")

data<- read.csv("data2.csv")
str(data)
summary(data)
data$International.Reputations <- as.factor(data$International.Reputation)
data$Weak.Foot <- as.factor(data$Weak.Foot)
data$Skill.Moves <- as.factor(data$Skill.Moves) 
data$Wage <- as.numeric(data$Wage)
data$Value <- as.numeric(data$Value)
data$Release.Clause <- as.numeric(data$Release.Clause)

boxplot(data$Potential )
quantile(data$Potential , c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))
data2 <- data[ data$Potential <62, ]
data2 <- data[data$Potential >62 & data$Potential <= 88 , ]
boxplot(data2$Potential)

quantile(data2$Potential, c(0,0.01,0.02,0.1,0.25,0.5,0.75,0.90,0.95,0.97,0.98,0.985,0.99,0.995,1))


nrow(data)-nrow(data2)


data <- data2


## Check the missing value (if any)
sapply(data, function(x) sum(is.na(x)))

data <- na.omit(data)

nrow(data)
names(data)

##Spliting the data into train and test
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind == 1,]
test <- data[ind == 2,]

fit<- lm(Potential ~ Overall + Value + Wage + Preferred.Foot + International.Reputation + Weak.Foot + Skill.Moves + Work.Rate + Body.Type + Crossing + Finishing + HeadingAccuracy + ShortPassing + Volleys + Dribbling + Curve + FKAccuracy + LongPassing + BallControl + Acceleration + SprintSpeed + Agility + Reactions + Balance + ShotPower + Jumping + Stamina + Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Penalties + Composure + Marking + StandingTackle + SlidingTackle + GKDiving + GKHandling + GKKicking + GKPositioning + GKReflexes + Release.Clause , data=train)
summary(fit)

fit<- lm(Potential ~ Overall + Value + Wage + Preferred.Foot + International.Reputation + Weak.Foot + Skill.Moves + Work.Rate + Crossing + Finishing + HeadingAccuracy + ShortPassing + Volleys + Dribbling + Curve + FKAccuracy + LongPassing + BallControl + Acceleration + SprintSpeed + Agility + Reactions + Balance + ShotPower + Jumping + Stamina + Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Penalties + Composure + Marking + StandingTackle + SlidingTackle + GKDiving + GKHandling + GKKicking + GKPositioning + GKReflexes + Release.Clause , data=train)
summary(fit)

fit<- lm(Potential ~ Overall + Value + Wage + Preferred.Foot + International.Reputation + Weak.Foot + Skill.Moves + Work.Rate + Crossing + Finishing + HeadingAccuracy + ShortPassing + Volleys + Dribbling + Curve + FKAccuracy + LongPassing + BallControl + Acceleration + SprintSpeed + Agility + Reactions + Balance + ShotPower + Jumping + Stamina + Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Composure + Marking + StandingTackle + SlidingTackle + GKDiving + GKHandling + GKKicking + GKPositioning + GKReflexes + Release.Clause , data=train)
summary(fit)

fit<- lm(Potential ~ Overall + Value + Wage + Preferred.Foot + International.Reputation + Weak.Foot + Skill.Moves + Work.Rate + Crossing + Finishing + HeadingAccuracy + ShortPassing + Volleys + Dribbling + Curve + FKAccuracy + LongPassing + BallControl + Acceleration + SprintSpeed + Agility + Reactions + Balance + ShotPower + Jumping + Stamina + Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Composure + Marking + StandingTackle + SlidingTackle + GKDiving + GKKicking + GKPositioning + GKReflexes + Release.Clause , data=train)
summary(fit)

fit<- lm(Potential ~ Overall + Value + Wage + Preferred.Foot + International.Reputation + Weak.Foot + Skill.Moves + Work.Rate + Crossing + HeadingAccuracy + ShortPassing + Volleys + Dribbling + Curve + FKAccuracy + LongPassing + BallControl + Acceleration + SprintSpeed + Agility + Reactions + Balance + ShotPower + Jumping + Stamina + Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Composure + Marking + StandingTackle + SlidingTackle + GKDiving + GKKicking + GKPositioning + GKReflexes + Release.Clause , data=train)
summary(fit)

fit<- lm(Potential ~ Overall + Value + Wage + Preferred.Foot + International.Reputation + Weak.Foot + Skill.Moves + Work.Rate + Crossing + HeadingAccuracy + ShortPassing + Volleys + Dribbling + FKAccuracy + LongPassing + BallControl + Acceleration + SprintSpeed + Agility + Reactions + Balance + ShotPower + Jumping + Stamina + Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Composure + Marking + StandingTackle + SlidingTackle + GKDiving + GKKicking + GKPositioning + GKReflexes + Release.Clause , data=train)
summary(fit)

fit<- lm(Potential ~ Overall + Value + Wage + Preferred.Foot + International.Reputation + Weak.Foot + Skill.Moves + Work.Rate + Crossing + HeadingAccuracy + ShortPassing + Volleys + Dribbling + FKAccuracy + LongPassing + BallControl + Acceleration + SprintSpeed + Agility + Reactions + ShotPower + Jumping + Stamina + Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Composure + Marking + StandingTackle + SlidingTackle + GKDiving + GKKicking + GKPositioning + GKReflexes + Release.Clause , data=train)
summary(fit)

fit<- lm(Potential ~ Overall + Value + Wage + Preferred.Foot + International.Reputation + Weak.Foot + Skill.Moves + Work.Rate + Crossing + HeadingAccuracy + ShortPassing + Volleys + Dribbling + FKAccuracy + LongPassing + BallControl + Acceleration + SprintSpeed + Agility + Reactions + ShotPower + Jumping + Stamina + Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Composure + Marking + StandingTackle + SlidingTackle + GKDiving + GKKicking + GKPositioning + Release.Clause , data=train)
summary(fit)

fit<- lm(Potential ~ Overall + Value + Wage + Preferred.Foot + International.Reputation + Weak.Foot + Skill.Moves + Work.Rate + Crossing + HeadingAccuracy + ShortPassing + Volleys + Dribbling + FKAccuracy + LongPassing + BallControl + Acceleration + SprintSpeed + Agility + Reactions + ShotPower + Jumping + Stamina + Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Composure + Marking + StandingTackle + SlidingTackle + GKKicking + GKPositioning + Release.Clause , data=train)
summary(fit)

fit<- lm(Potential ~ Overall + Value + Wage + Preferred.Foot + International.Reputation + Weak.Foot + Skill.Moves + Work.Rate + Crossing + HeadingAccuracy + ShortPassing + Volleys + Dribbling + FKAccuracy + LongPassing + BallControl + Acceleration + SprintSpeed + Agility + Reactions + ShotPower + Jumping + Stamina + Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Composure + StandingTackle + SlidingTackle + GKKicking + GKPositioning + Release.Clause , data=train)
summary(fit)

fit<- lm(Potential ~ Overall + Value + Wage + Preferred.Foot + Weak.Foot + Skill.Moves + Work.Rate + Crossing + ShortPassing + Volleys + Dribbling + FKAccuracy + LongPassing + BallControl + Acceleration + SprintSpeed + Agility + Reactions + ShotPower + Jumping + Stamina + Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Composure + StandingTackle + SlidingTackle + GKKicking + GKPositioning + Release.Clause , data=train)
summary(fit)

fit<- lm(Potential ~ Overall + Value + Wage + Weak.Foot + Skill.Moves + Work.Rate + Crossing + ShortPassing + Volleys + Dribbling + FKAccuracy + LongPassing + BallControl + Acceleration + SprintSpeed + Agility + Reactions + ShotPower + Jumping + Stamina + Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Composure + StandingTackle + SlidingTackle + GKKicking + GKPositioning + Release.Clause , data=train)
summary(fit)

fit<- lm(Potential ~ Overall + Value + Wage + Weak.Foot + I(Skill.Moves == "2") + I(Skill.Moves == "5") + Work.Rate + Crossing + ShortPassing + Volleys + Dribbling + FKAccuracy + LongPassing + BallControl + Acceleration + SprintSpeed + Agility + Reactions + ShotPower + Jumping + Stamina + Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Composure + StandingTackle + SlidingTackle + GKKicking + GKPositioning + Release.Clause , data=train)
summary(fit)

#Check Vif, vif>2 means presence of multicollinearity
vif(fit)

##Final model 
fit<- lm(Potential ~ Overall + Wage + Weak.Foot + I(Skill.Moves == "2") + I(Skill.Moves == "5") + Work.Rate + Crossing + Volleys + FKAccuracy + LongPassing + Acceleration + SprintSpeed + Agility + Reactions + ShotPower + Jumping + Stamina + Strength + LongShots + Aggression + Interceptions + Positioning + Vision + Composure + SlidingTackle + GKKicking + Release.Clause , data=train)
summary(fit)

fit<- lm(Potential ~ Overall + Wage + Weak.Foot + I(Skill.Moves == "2") + I(Skill.Moves == "5") + I(Work.Rate == "High/ Low") + I(Work.Rate == "Low/ High") + I(Work.Rate == "Low/ Low") + I(Work.Rate == "Low/ High") + I(Work.Rate == "Low/ Medium") + I(Work.Rate == "Medium/ Low") + Crossing + Volleys + FKAccuracy + LongPassing + SprintSpeed + Agility + Reactions + ShotPower + Jumping + Stamina + Strength + Aggression + Vision + Composure + SlidingTackle + GKKicking + Release.Clause , data=train)
summary(fit)

fit<- lm(Potential ~ Overall + Wage + Weak.Foot + SprintSpeed + ShotPower + Jumping + Strength + SlidingTackle , data=train)
summary(fit)

## Get the predicted or fitted values
fitted(fit)

## MAPE


test$pred <- predict(fit, test)

write.csv(data2, "data2result.csv")

#Calculating MAPE
attach(test)
(sum((abs(Potential-pred))/Potential))/nrow(test)


##################################### Checking of Assumption ############################################

# residuals should be uncorrelated ##Autocorrelation
# Null H0: residuals from a linear regression are uncorrelated. Value should be close to 2. 
#Less than 1 and greater than 3 -> concern
## Should get a high p value

dwt(fit)

# Checking multicollinearity
vif(fit) # should be within 2. If it is greater than 10 then serious problem

################ Constant error variance ##########Heteroscedasticity


# Breusch-Pagan test
bptest(fit)  # Null hypothesis -> error is homogenious (p value should be more than 0.05)



## Normality testing Null hypothesis is data is normal.

resids <- fit$residuals


ad.test(resids) #get Anderson-Darling test for normality 