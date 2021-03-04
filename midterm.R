library(dplyr)
library(tidyr)
library(tidyverse)
library(ggfortify)
library(MuMIn)
library(car)
library(broom)
library(lme4)

### QUESTION 1
# use the datasets 'pinelands_bees.csv' and 'land_cover.csv' to answer the question:
# Does natural land cover in the surrounding landscape predict the number of individual bees collected at a site?
# follow the analysis workflow that we learned in class, for which there are prompts below
# make sure to complete all 6 prompts
# use comment lines to briefly explain your reasoning at each step (ie, why you are doing what you are doing)
# you will turn in your code (ie, this R file) to your github repo at the end of class

## brief metadata
# the datasets you will use are 'pinelands_bees.csv'  and 'land_cover.csv'
# these are data I collected on wild bees of 117 ish species (ish because the taxonomy of some specimens is not fully sorted out) 
# data were collected at 27 study sites in the New Jersey pinelands
# all sites were in forest habitat, but sites differed in how much of the surrounding landscape was forested
# bees were collected on four days at each site between April and September 2003, with collection events organized into rounds, such that each site was sampled once in round 1 before round 2 was begun, etc
# 'pinelands_bees.csv'contains data on the bees collected, where each row is one individual bee
# 'land_cover.csv' column 'Nat1600' is the percentage of the natural land cover surrounding the site within a 1600m radius (roughly an 800 ha area), where the natural habitat is predominantly forest, with a small amount of other natural habitats such as open wetlands


##  1 Get and format the data
# you will need to create a new dataframe that has the variables you need: site, land cover, number of bees collected
# you may want to use group_by, summarize, n(), left_join

bees_data <- read.csv("pinelands_bees.csv")
head(bees_data)
bees_data <- select(bees_data, "genus_species", "site_name", "round")
bees_data

lc <- read.csv("land_cover.csv")
head(lc)

bees_v2 <- bees_data %>% group_by(site_name) %>% summarise(Nbees = n())
bees_v2 <- left_join(bees_v2, lc, key = "site_name")
head(bees_v2)

## 2 Data picture
# plot the data and figure out what type of model might be best

ggplot(data = bees_v2, aes(x = Nat1600, y = Nbees)) + geom_point()

# A Poisson generalised linear model may be a good choice for these data,  since they are counts that are bounded at zero. However, although linear models with normal error distribution often are not optimal for count data, the counts here are reasonably large so may come closer to being approximately normal.

## 3 Test model assumptions
# you can test the assumptions of more than one type of model here if you aren't sure
# explain your reasoning for the model you choose

bees_lm <- lm(Nbees~Nat1600, data = bees_v2)
hist(bees_lm$residuals)
# ^ This histogram of residuals seems to be quite right-skewed, which is not ideal.
plot(bees_lm)
# Residuals vs Fitted: seems to be scattered somewhat evenly, though there are more positive residuals at low fitted values and negative residuals at high fitted values.
# QQ plot: confirms right skewedness
# Standardized Residuals: somewhat even, slight increase toward higher fitted values isn't ideal
# Residuals vs. Leverage: One point has both a large residual and fairly high leverage

bees_glm <- glm(Nbees~Nat1600, family = poisson, data = bees_v2)
hist(bees_glm$residuals)
plot(bees_glm)
# Residuals vs. Fitted: slightly more even scatter than the lm
# QQ plot: right skew
# Standardized Residuals: slightly more even across predicted values than the lm
# Residuals vs. Leverage: still some points with large residuals and high leverage

AIC(bees_lm)
AIC(bees_glm)

# Tests of assumptions do not strongly support the glm (or at least not more so than the lm) and AIC of that model is much greater than that of the lm, so the lm seems to be the better choice for these data.

## 4 Report and interpret results
# make sure you interpret the following things: coefficients, p value, explanatory value of model
# state your conclusions using numbers and units
# discuss the magnitude and biological significance of your result, not just its significance
# how confident are you in this analysis and why

summary(bees_lm)
# Intercept: 72.60; predicted number of individual bees at a site with no natural land cover within a 1600 meter radius. As the data only cover sites with 25-99% natural land cover, this extrapolation is likely not reliable/meaningful. The t-test p-value provided (1.96 * 10^-5) indicates that this intercept is significantly different from zero.
# Nat1600: -0.4354; this coefficient indicates the slope of change in bee abundance with percent natural land cover. The t-test p-value for this slope (0.0293) indicates that it is significantly different from zero.
# Based on these results, it seems that natural land cover does predict the number of individual bees collected at a given location. For every 1% increase in natural land cover around a site, the number of bees collected decreases by about 0.44 individuals (or decrease of about 4 individuals per 10% increase in natural land cover).

# Although this result suggests that bee abundance decreases significantly as natural land cover in a 1600 meter radius increases, the magnitude of the change is fairly small. Considering the relative weakness of this trend and the presence of one or two outlier points in the data, my confidence in this result is limited. In order to be more confident in this trend, I would consider re-running this analysis without obvious outlier points in order to determine their influence on the outcome.


## 5 Plot model results back onto the data picture
# geom_smooth is the easy way to do this, but you can alternatively do it manually using the model output (coefficients) if you want

bee_coefs <- coef(bees_lm)
ggplot(data = bees_v2, aes(x = Nat1600, y = Nbees)) + geom_point() + 
geom_abline(intercept = bee_coefs[[1]], slope = bee_coefs[[2]])



## 6  If you were to add a random effect to this model, what would it be, what would adding it accomplish?
# please answer this question in comment lines
# you do not need to format any data here or code or run anything
# explain your reasoning for using this random effect
# you will need to go back to the original data file and metadata to answer this question
# extra credit: write out the model code for your model now including the random effect

# I would add a random effect of collection round to this model. Considering that collection took place in multiple rounds, it seems possible that measurements within each of those rounds could be more closely related to one another than to measurements in other rounds (could be correlation within rounds). Adding a random effect of round would account for this extra structure in the data without the same loss of degrees of freedom that would come with adding round as a fixed effect.

head(bees_data)
bees_v3 <- bees_data %>% group_by(round, site_name) %>% summarise(Nbees = n())
bees_v3 <- left_join(bees_v3, lc, key = "site_name")
head(bees_v3)

bees_me <- lmer(Nbees~Nat1600+(Nat1600|round), data = bees_v3)
# ^ both random slope and intercept (random slope to account for the possibility that the overall trend in abundance vs. natural land cover differs by round)

### QUESTION 2
# The file "modSel.csv" contains simulated dataset of observations of a focal species at a series of sites.
# For each site, you have observed abundance, and measurements of environmental variables you hypothesize
# to affect the distribution of this species.
# Specifically, you hypothesize the species is more abundant in warmer, wetter regions,
# and that it prefers core over edge habitat.
# You have measured each of these in a couple ways, as mean annual and summer temperature,
# cumulative annual and summer precipitation, and distance to nearest edge and total edge within 500 m.
# Your goal here is to find the best model you can, given your hypotheses,
# to describe the distribution of this species.
# In doing so, you will also assess the more relevant measure of each environmental condition,
# and weigh their relative importance (or at least predictive power).
# For simplicity, do not consider interactions between variables.
# Please give your models interpretable names.

q2 <- read.csv("modSel.csv")
head(q2)
mean(q2$observedAbundance)

# Step 1. Find the best error structure/error distribution for these data.
# State your conclusion in comment lines
# (Hints: you want to assess model-error distributions, not the data distribution; these are count data.)

normal <- glm(observedAbundance ~ meanSummerTemp, family = gaussian, data = q2)
plot(normal)
pois <- glm(observedAbundance ~ meanSummerTemp, family = poisson, data = q2)
plot(pois)

# More even distribution of residuals across fitted values, slightly less skew in residuals, more even distribution of standardized residuals across predicted values all suggest that a Poisson error structure is more appropriate than normal error. This is not unexpected, considering that these are count data with mostly low numbers (mean abundance is only 1.93)

# Step 2: Having determined the best error structure, determine the more effective method of measuring each variable.
# For each variable, compare methods as a pair of single-variable models (e.g., summer temp vs annual temp).
# State your conclusion in comment lines

annualTemp <- glm(observedAbundance ~ meanAnnualTemp, family = poisson, data = q2)
model.sel(pois, annualTemp)
# The model using mean summer temperature seems to be a better fit for the data; it has both a lower AIC and lower negative log likelihood than the model using mean annual temperature as a predictor.

annualPrecip <- glm(observedAbundance ~ annualPrecipitation, family = poisson, data = q2)
summerPrecip <- glm(observedAbundance ~ summerPrecipitation, family = poisson, data = q2)
model.sel(summerPrecip, annualPrecip)
# The model using summer precipitation seems to be a better fit for the data; it has both a lower AIC and lower negative log likelihood than the model using  annual precipitation as a predictor.

edgeDist <- glm(observedAbundance ~ distance2edge, family = poisson, data = q2)
edgeTotal <- glm(observedAbundance ~ totalEdge, family = poisson, data = q2)
model.sel(edgeDist, edgeTotal)
# The model using total edge seems to be a better fit for the data; it has both a lower AIC and lower negative log likelihood than the model using distance to edge as a predictor.

# Step 3: Having determined which method of measurement for each variable is best,
# determine the most effective combination of predictors;
# run a set of competing models and create a table comparing these models to each other and to a null.
# state your conclusion in comment lines

q2_all <- glm(observedAbundance ~ meanSummerTemp + summerPrecipitation + totalEdge, family = poisson, data = q2, na.action = na.fail)
dredge(q2_all)

# Based on this comparison, mean summer temperature and total edge seem to be the most effective combination of predictors; the model including only these predictors had the lowest AIC of the models compared (although its negative log likelihood was very slightly greater than that of the model that also included summer precipitation as a predictor).

# Step 4: Interpret these results.
# Were your hypotheses supported? What is the relative importance of each predictor?
# What is your general conclusion?

q2_b <- glm(observedAbundance ~ meanSummerTemp + totalEdge, family = poisson, data = q2)
summary(q2_b)

# These results support the hypotheses that this species prefers warmer regions (slope of abundance vs mean summer temperature is positive) and core habitat (the coefficient for totalEdge is negative), but not the hypothesis that this species prefers wetter regions (model comparison suggests that precipitation may not be an important predictor). Based on model comparison (by AIC), it seems that mean summer temperature is the most important predictor (model with this predictor alone has lower AIC than with edge alone), followed by total edge, followed by summer precipitation (of these three predictors, results in the model with the highest AIC when included as the sole predictor). Based on this, I conclude that this species' distribution is primarily predicted by temperature and core habitat, increasing in abundance with summer temperature and decreasing with patch edge length.
