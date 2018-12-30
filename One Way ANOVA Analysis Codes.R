##### ANOVA - Study ######

data <- PlantGrowth

data1 <- data

#### Exploratory data analysis

View(data)

dim(data)

labels(data)

str(data)

levels(data$group)

summary(data)

IQR(data$weight)

LL <- 4.550 - (1.5*0.95)
UL <- 5.530 + (1.5*0.95)

LL
UL

##### No Outliers ####

colSums(is.na(data))

#### Visualization of the data

boxplot(weight~group, data = data)

install.packages("gplots")

library(gplots)

plotmeans(weight~group, data = data, frame = FALSE, xlab = "Group", ylab = "Weight")

##### ANOVA Test ####

test.aov <- aov(weight~group, data = data)

summary.aov(test.aov)

## Null Hypothesis - The means of the groups are the same
## Alternate Hypothesis - The means of atleast one of the groups is different

#### since p value is less than 0.05 we reject the Null hypothesis, the means of the population of the three groups are different

#### HSD test to determine which groups are different

TukeyHSD(test.aov)

#### If the p value is less than significane level then the difference between the groups is significant

#### Based on Tukey HSD test the difference between trt2 and trt1 is significant

##### Checking of Assumptions ####

### Homogeneity of Variance

plot(test.aov,1)

library(car)

leveneTest(weight~group, data = data)

## Null Hypothesis - Variances are equal
## Alternate Hypothesis - Variances are Not Equal

## P value is greater than significane level, hence we do not reject the Null Hypothesis

##### Relaxing the Homogenity of Variance Assumption

### Welsh One way test - ANOVA test with no assumption of equal variances

oneway.test(weight~group, data = data)

## Null Hypothesis - No equal variance
## Alternate Hypothesis - Equal variance
## since p value is less than significane level we reject the Null Hypothesis


####### Checking Normality Assumption

## Perform Shapiro- Wilk test on the ANOVA residuals

### Extract the residuals

aov_residuals <- residuals(object = test.aov)

### Run Shapiro Wilks test

shapiro.test(x = aov_residuals)

## Null Hypothesis - Data is normal
## Alternate Hypothesis - Data is Not Normal

## Since p value is greater than significane level we do not rejet the Null Hypothesis

#### NON PARAMETRIC Method for One Way ANOVA when assumptions are not met 

### Kruskal-Wallis rank sum test #####

kruskal.test(weight~group, data = data)

## Null Hypothesis - No difference in mean 

### Alternate Hypotheis - Difference in mean

## As the p value is less than the significane level, we reject the Null Hypothesis.
