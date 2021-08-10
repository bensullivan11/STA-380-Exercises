install.packages('vioplot')
library(mosaic)
library(vioplot)
green = read.csv('greenbuildings.csv')
#green = subset(green, leasing_rate > 40)
summary(green)

plot(green$leasing_rate, green$Rent)


lm1 = lm(green$Rent ~ green$leasing_rate)
summary(lm1)
hist(green$leasing_rate)


# Extract the buildings with green ratings
green_only = subset(green, green_rating==1)
non_green = subset(green, green_rating!=1)
dim(green_only)
dim(non_green)
# Not a normal distribution at all
hist(green_only$Rent, 25)
mean(green_only$Rent)

hist(non_green$Rent, 25)
mean(non_green$Rent)

# Using R's lm function
model1 = lm(Rent ~ 1, data=green_only)
confint(model1, level=0.95)

model1 = lm(Rent ~ 1, data=non_green)
confint(model1, level=0.95)
### Compare with bootstrapping

# Now repeat 2500 times
boot1 = do(2500)*{
  mean(resample(green_only)$Rent)
}
head(boot1)
hist(boot1$result, 30)
sd(boot1$result)
mean(boot1$result)

boot2 = do(2500)*{
  mean(resample(non_green)$Rent)
}
head(boot2)
hist(boot2$result, 30)
sd(boot2$result)
mean(boot2$result)


# Extract the confidence interval from the bootstrapped samples
confint(boot1, level=0.95)

xbar + c(-1.96,1.96)*se_hat

# Calculate the mean and median of the bootstrap and see the median is much higher
mean(boot1$result)
median(boot1$result)

####
# Bootstrap the median
####

median(green_only$Rent)
# Now repeat 2500 times
boot3 = do(2500)*{
  median(resample(green_only)$Rent)
}
head(boot3)
mean(boot3$result)

boot4 = do(2500)*{
  median(resample(non_green)$Rent)
}
head(boot4)
mean(boot4$result)



# Ugly!
hist(boot2$result, 30)

# But we still get a confidence interval
x1 <- confint(boot3)
x2 <- confint(boot4)

vioplot(x1, x2)
boxplot(boot3,boot4)
boxplot(boot4)
####### Now we begin our own visual analysis ######
###### Start with plots ######

library(tidyverse)

#plot rent vs leasing rate
ggplot(green) + 
  geom_point(aes(x=Rent, y=leasing_rate, color=green_rating))

ggplot(green_only) + 
  geom_point(aes(x=Rent, y=leasing_rate))

#plot the Gas and Electricity costs to rent
#shows weak positive correlation to electricity and rent prices
ggplot(green_only) + 
  geom_point(aes(x=Electricity_Costs, y=Rent))

ggplot(green_only) + 
  geom_point(aes(x=Gas_Costs, y=Rent))


#cluster rent vs rent
#shows a strong relation ship that the rent of a green building
#has to the rent of non-green buildings around it
#will be very difficult to disprove this as a confounding variable
#additionally previous analysis excluded out liers (high rent variables)
ggplot(green_only) + 
  geom_point(aes(x=cluster_rent, y=Rent, color=class_a))

#rent to age and size, no correlation
ggplot(green_only) + 
  geom_point(aes(x=stories, y=Rent))

#rent to total days of heating and cooling needed, no correlation
#could almost argue negative correlation
#in fact one random area of with same high amt of heat/cool shows its probably more abt area
ggplot(green) + 
  geom_point(aes(x=total_dd_07, y=Rent, color=green_rating))

#rent to class A showing if its green or not
ggplot(green) + 
  geom_point(aes(x=total_dd_07, y=Rent, color=green_rating))
