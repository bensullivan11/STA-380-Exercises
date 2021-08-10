library(mosaic)
green = read.csv('greenbuildings.csv')
summary(green)

plot(green$leasing_rate, green$Rent)

ggplot(green_only) + 
  geom_point(aes(x=cluster_rent, y=Rent, color = class_a))+
  labs(x = 'Cluster Rent ($ per sq ft)', y = 'Green Buliding Rent ($ per sq ft)', title = 'Green Building Rent vs Associated Cluseter Rent')

# Extract the buildings with green ratings
green_only = subset(green, green_rating==1)
non_green = subset(green, green_rating!=1)
dim(green_only)
dim(non_green)
# Not a normal distribution at all
hist(green_only$Rent, 25)
mean(green_only$Rent)

rents <- green_only$Rent + non_green$Rent

hist(non_green$Rent, 25)
mean(non_green$Rent)

################################

# finding the mean of the non green and green split by A and B

################################
a_only = subset(green, class_a==1)
b_only = subset(green, class_b==1)

median(a_only$Rent)
median(b_only$Rent)
mean(a_only$Rent)
mean(b_only$Rent)
hist(a_only$Rent)
hist(b_only$Rent)

## Bootstrapping to normalize
boot5 = do(2500)*{
  mean(resample(a_only)$Rent)
}
head(boot5)
hist(boot5$result, 30)
sd(boot5$result)
mean(boot5$result)


boot6 = do(2500)*{
  mean(resample(b_only)$Rent)
}
head(boot6)
hist(boot6$result, 30)
sd(boot6$result)
mean(boot6$result)


############ over all the rent is much higher in A or the premium area

green_onlya = subset(a_only, green_rating==1)
non_greena = subset(a_only, green_rating!=1)

median(green_onlya$Rent)
median(non_greena$Rent)
mean(green_onlya$Rent)
mean(non_greena$Rent)
hist(green_onlya$Rent)
hist(non_greena$Rent)

## Bootstrapping to normalize
boot1 = do(2500)*{
  mean(resample(green_onlya)$Rent)
}
head(boot1)
hist(boot1$result, 30)
sd(boot1$result)
mean(boot1$result)


boot2 = do(2500)*{
  mean(resample(non_greena)$Rent)
}
head(boot2)
hist(boot2$result, 30)
sd(boot2$result)
mean(boot2$result)
###### Non green is worth more in the A area

green_onlyb = subset(b_only, green_rating==1)
non_greenb = subset(b_only, green_rating!=1)

median(green_onlyb$Rent)
median(non_greenb$Rent)
mean(green_onlyb$Rent)
mean(non_greenb$Rent)
hist(green_onlyb$Rent)
hist(non_greenb$Rent)

## Bootstrapping to normalize
boot3 = do(2500)*{
  mean(resample(green_onlyb)$Rent)
}
head(boot3)
hist(boot3$result, 30)
sd(boot3$result)
mean(boot3$result)


boot4 = do(2500)*{
  mean(resample(non_greenb)$Rent)
}
head(boot4)
hist(boot4$result, 30)
sd(boot4$result)
mean(boot4$result)
############ in area be you see that the rent is higher for the green buildings like hypothesized
nrow(green_onlya)/nrow(a_only)*100

nrow(green_onlyb)/nrow(b_only)*100
# A much higher proportion of green are in the A which raises the average price
## plotting


hist(boot6$result, 45, col = 'pink', xlab = 'Rent($ per sqft)', main = 'Green bulidings and non green buildings in the prime location', xlim=c(25,35))
hist(boot5$result, 45, col = 'purple', xlab = 'Rent($ per sqft)', main = 'Green bulidings and non green buildings in the prime location', add = T)
  legend("right", c("Area B", "Area A"), fill=c("pink", "purple"))

hist(boot1$result, 45, col = 'green', xlab = 'Rent($ per sqft)', main = 'Green bulidings and non green buildings in the prime location')
hist(boot2$result, 45, col = 'blue', add = T)+
  legend("right", c("Green", "Non_green"), fill=c("green", "blue"))

hist(boot3$result, 30, col = 'green', xlab = 'Rent($ per sqft)', main = 'Green bulidings and non green buildings in the lesser location')
hist(boot4$result, 30, col = 'blue', add = T)+
  legend("right", c("Green", "Non_green"), fill=c("green", "blue"))


hist(non_greena$Rent, xlab = 'Mean Rent for location A', ylab = 'Frequency', col = 'blue', main = 'Relative frequency of green to non greeen buildings in group A', breaks = 25)
hist(green_onlya$Rent, xlab = 'Mean Rent for location A', ylab = 'Frequency', col = 'green', breaks = 25, add = T)+
  legend("right", c("Green", "Non_green"), fill=c("green", "blue"))

hist(non_greenb$Rent, xlab = 'Mean Rent for location B', ylab = 'Frequency', col = 'blue', main = 'Relative frequency of green to non greeen buildings in group B', breaks = 25)
hist(green_onlyb$Rent, xlab = 'Mean Rent for location B', ylab = 'Frequency', col = 'green',  breaks = 25, add = T)+
  legend("right", c("Green", "Non_green"), fill=c("green", "blue"))


