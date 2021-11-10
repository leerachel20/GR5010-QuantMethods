#############################################
######  Recitation 9         ################
######  Author: Rachel Lee   ################
######  Date: Nov 10, 2021   ################
#############################################

# Install the package for MDK data
# install.packages("AMCP") # "A Model Comparison Perspective"
library(AMCP) # Load the package
?AMCP # Access help on the package

data(chapter_3_table_1)
mean(chapter_3_table_1$IQ)
median(chapter_3_table_1$IQ)
quantile(chapter_3_table_1$IQ, probs=c(.05, .25, .5, .75, .95))
sd(chapter_3_table_1$IQ)
length(chapter_3_table_1$IQ)

library(dplyr)
chapter_3_table_1 %>%
  summarize(
    Sum.Y = sum(IQ),
    n = n(),
    Sum.Squared.Errors = sum((IQ-104)^2))

# Manually calculating for F
Values <- chapter_3_table_1 %>%
  summarize(
    Error_F = sum((IQ-mean(IQ))^2),
    df_F = n()-1,
    Error_R = sum((IQ-98)^2),
    df_R = n(),
    F = ((Error_R - Error_F)/(df_R - df_F))/(Error_F/df_F)
  )

# Define two data frames, IQ.1 and IQ.2
IQ.1 = IQ.2 <- chapter_3_table_1

# Reassign `IQ` from the IQ.2 data set as the IQ scores minus the value of 98, which is 
# the null. 
IQ.2$IQ <- IQ.2$IQ-98

# Fit a linear model in which only the intercept (i.e., the 1) is estimated, which is the 
# mean in this context, using the IQ.1 data set. 
Full <- lm(IQ ~ 1, data=IQ.1)

# Fit a linear model in which there is no intercept term (i.e., the -1). 
Restricted <- lm(IQ ~ -1, data=IQ.2)

# Now, perform the model comparisons. It is important that the two data sets use the same 
# name for the dependent variable. 
anova(Restricted, Full)

# The critical value from an F-distribution can be found using the qf() function
# Generically, qf() is used as qf(1-alpha, df1, df2)
qf(.95, 1, 5)

# The p-value from an F-test can be found using the pf() function
1-pf(9, 1, 5)
# Alternatively
pf(9, 1, 5, lower.tail = FALSE)

# t-test can be done. Why?
t.test(chapter_3_table_1$IQ, mu=98)


# If we have time left...
data(chapter_3_table_3)
chapter_3_table_3

str(chapter_3_table_3)

class(chapter_3_table_3$Condition)

# We can convert Condition into a factor, 
# so that it is explicitly recognized as a grouping variable, 
# using the as.factor() function and redefining the Condition variable.
chapter_3_table_3$Condition <- as.factor(chapter_3_table_3$Condition)
class(chapter_3_table_3$Condition)


# A simple (box and whiskers) plot can be performed using the plot() function 
# (now that Conditionition is treated as a factor).
plot(chapter_3_table_3)


# adding descriptive labels
plot(chapter_3_table_3, ylab="Global Affect Rating", xlab="Group", 
     main="Box and Whiskers Plot of Affect Rating by Group")

# Equivalently, the boxplot() function can be used with the model specified. 
# The "model" is simply DV~IV (here with color added to the box)
boxplot(Rating~Condition, data=chapter_3_table_3, ylab="Global Affect Rating", xlab="Group", 
        main="Box and Whiskers Plot of Affect Rating by Group", col="bisque", 
        names=c("Pleasant", "Neutral", "Unpleasant"))

# Now with points
boxplot(Rating~Condition, data=chapter_3_table_3, ylab="Global Affect Rating", xlab="Group", 
        main="Box and Whiskers Plot of Affect Rating by Group", col="bisque", 
        names=c("Pleasant", "Neutral", "Unpleasant"))
points(Rating~Condition, data=chapter_3_table_3)

# Because there are multiple points at the same value, they cannot be seen. 
# "Jittering" the points (literally, changing their value slightly) can be 
# useful to more effectively understand the distribution of values within 
# each group. Even still, there can be some uncertainty if all of the scores 
# are represented (some still may still lay essentially atop one another).
boxplot(Rating~Condition, data=chapter_3_table_3, ylab="Global Affect Rating", xlab="Group", 
        main="Box and Whiskers Plot of Affect Rating by Group", col="bisque", 
        names=c("Pleasant", "Neutral", "Unpleasant"))
points(jitter(Rating)~Condition, data=chapter_3_table_3)

# Another way we could plot the data is to jitter on the grouping variable 
# by treating it as numeric.
boxplot(Rating~as.numeric(Condition), data=chapter_3_table_3, ylab="Global Affect Rating", 
        xlab="Group", main="Box and Whiskers Plot of Affect Rating by Group", col="bisque", 
        names=c("Pleasant", "Neutral", "Unpleasant"))
points(Rating~jitter(as.numeric(Condition)), data=chapter_3_table_3)

# Because the default level of jittering seemed too much, we reduce it here 
# (by a factor of .5*default).
boxplot(Rating~as.numeric(Condition), data=chapter_3_table_3, ylab="Global Affect Rating", 
        xlab="Group", main="Box and Whiskers Plot of Affect Rating by Group", col="bisque", 
        names=c("Pleasant", "Neutral", "Unpleasant"))
points(Rating~jitter(as.numeric(Condition), .5), data=chapter_3_table_3)


summary(chapter_3_table_3[chapter_3_table_3[,"Condition"]==1, "Rating"])
summary(chapter_3_table_3[chapter_3_table_3[,"Condition"]==2, "Rating"])
summary(chapter_3_table_3[chapter_3_table_3[,"Condition"]==3, "Rating"])
sd(chapter_3_table_3[chapter_3_table_3[,"Condition"]==1, "Rating"])
sd(chapter_3_table_3[chapter_3_table_3[,"Condition"]==2, "Rating"])
sd(chapter_3_table_3[chapter_3_table_3[,"Condition"]==3, "Rating"])


