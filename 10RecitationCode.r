#####################################################
#############    QMSS 5010  #########################
############# Recitation 10 #########################
#############   Rachel Lee  #########################
#####################################################

#reading in the data
g1 <- c(2, 3, 7, 2, 6)
g2 <- c(10, 8, 7, 5, 10)
g3 <- c(10, 13, 14, 13, 15)

# turn them into long forms
g1 <- as.data.frame(g1)
g2 <- as.data.frame(g2)
g3 <- as.data.frame(g3)

library(dplyr)
g1.table <- g1 %>% mutate(mean = mean(g1),
                          value.minus.mean = g1 - mean,
                          ss = value.minus.mean^2)
g2.table <- g2 %>% mutate(mean = mean(g2),
                          value.minus.mean = g2 - mean,
                          ss = value.minus.mean^2)
g3.table <- g3 %>% mutate(mean = mean(g3),
                          value.minus.mean = g3 - mean,
                          ss = value.minus.mean^2)
### ss.within ###
(ss.within = sum(g1.table$ss) + sum(g2.table$ss) + sum(g3.table$ss))



### ss.total ###
combined <- as.data.frame(cbind(g1, g2, g3))
combined <- stack(combined)

combined.table <- combined %>% mutate(mean = mean(values),
                                      ss = (values - mean)^2)

(ss.total = sum(combined.table$ss))


#### ss.between ###
# You can calculate ss.between using the formula:
ss.between = ss.total - ss.within
ss.between

# You can also manually calculate (try!)


## F stat calculation

# ss.within/ df = 54/(15-3) = 4.5
# ss.between / df = 203.3/(3-1) = 101.67
# F stat = 101.67 / 4.5 = 22.59


# Check if built-in aov function gives back the same result
anova_results <- aov(values ~ ind, data = combined)
summary(anova_results)
# F(2,12) = 22.59, pvalue < 0.05
