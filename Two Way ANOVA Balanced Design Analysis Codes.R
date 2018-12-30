###### Two Way ANOVA Hypothesis Test - Balanced Design ####

#### Balanced Design - we have equal sample sizes within levels of our independent grouping levels

### Null Hypothesis ##
##There is no difference in the means of factor A
## There is no difference in the means of factor B
## There is no interaction between factors A and B


#### Import the data set

data <- ToothGrowth

data_1 <- data

## Exploratory Data Analysis

dim(data)

View(data)

labels(data)

head(data)

tail(data)

colSums(is.na(data))

str(data)

data$dose <- factor(data$dose, levels = c(0.5, 1, 2), labels = c("0.5", "1", "2"))

str(data)

table(data$supp,data$dose)

#### We have 2X3 design cells with the factors being supp and dose and 10 subjects in each cell. 
##Here, we have a balanced design. 

###### Vizualization of the data ####

boxplot(data$len~data$supp*data$dose,data = data, frame = FALSE, col= c("#00AFBB", "#E7B800"), ylab = "Tooth Length")

### Two way Interaction Plot

interaction.plot(x.factor = data$dose, trace.factor = data$supp, response = data$len, fun = mean, type = "b", legend = TRUE, xlab = "Dose", ylab = "Tooth Length", pch = c(1,19), col = c("#00AFBB", "#E7B800"))

##### Computing 2 Way ANOVA

test_aov <- aov(data$len~data$supp+data$dose, data = data)

summary(test_aov)


## From the ANOVA table we can conclude that both supp and dose are statistically significant. 
##dose is the most significant factor variable. 
##These results would lead us to believe that changing delivery methods (supp) or the dose of vitamin C, will impact significantly the mean tooth length.

##### Computing 2 way ANOVA with interaction effect

test_aov2 <- aov(data$len~data$supp*data$dose, data = data)

summary(test_aov2)

test_aov3 <- aov(data$len~data$supp+data$dose+data$supp:data$dose, data = data)

summary(test_aov3)

###From the ANOVA results, you can conclude the following, based on the p-values and a significance level of 0.05:

### the p-value of supp is 0.000429 (significant), which indicates that the levels of supp are associated with significant different tooth length.
### the p-value of dose is < 2e-16 (significant), which indicates that the levels of dose are associated with significant different tooth length.
### the p-value for the interaction between supp*dose is 0.02 (significant), which indicates that the relationships between dose and tooth length depends on the supp method.


### Multiple pair wise comparision between means of groups

### Tukey HSD Test

TukeyHSD(test_aov3)

####seen from the output, that all pairwise comparisons are significant with an adjusted p-value < 0.05.

#### Checking ANOVA Assumptions

#### Homogenity of Variance

### Levins Test

### Null Hypothesis - variances are equal

### Alternate Hypotheis - Vairances are not equal

library(car)

leveneTest(data$len~data$supp*data$dose, data = data)

### Residuals Vs Fits Plot

plot(test_aov3,1)


### From the output above we can see that the p-value is not less than the significance level of 0.05. 
## This means that there is no evidence to suggest that the variance across groups is statistically significantly different. 
### Therefore, we can assume the homogeneity of variances in the different treatment groups.

#### Normality of Residuals

### Normality Plot of residuals

plot(test_aov3,2)

##### Shapiro Wilks Test

### Extract the residuals

aov3_residuals <- residuals(object = test_aov3)

### Run Shapiro Wilks test

shapiro.test(x = aov3_residuals)

#### The conclusion above, is supported by the Shapiro-Wilk test on the ANOVA residuals (W = 0.98, p = 0.5) which finds no indication that normality is violated.