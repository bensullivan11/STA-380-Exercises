library(mosaic)

green = read.csv('../data/greenbuildings.csv')
summary(green)

####Professors Sample code####
#One thing he may be trying to show is that bootstrapping will 
#provide an even higher median rent/sq ft than used in previous calculations?

# Extract the buildings with green ratings
green_only = subset(green, green_rating==1)
dim(green_only)

# Not a normal distribution at all
hist(green_only$Rent, 25)
mean(green_only$Rent)

# Normal-based confidence interval for the sample mean
xbar = mean(green_only$Rent)
sig_hat = sd(green_only$Rent)
se_hat = sig_hat/sqrt(nrow(green_only))
xbar + c(-1.96,1.96)*se_hat

# Using R's lm function
model1 = lm(Rent ~ 1, data=green_only)
confint(model1, level=0.95)


### Compare with bootstrapping

# a single bootstrapped sample (repeat a few times)
green_only_boot = resample(green_only)
mean(green_only_boot$Rent)

# Get a feel for what it is in the green_only_boot object
head(green_only_boot)

# Now repeat 2500 times
boot1 = do(2500)*{
	mean(resample(green_only)$Rent)
}
head(boot1)
hist(boot1$result, 30)
sd(boot1$result)

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
boot2 = do(2500)*{
	median(resample(green_only)$Rent)
}
head(boot2)

# Ugly!
hist(boot2$result, 30)

# But we still get a confidence interval
confint(boot2)

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

