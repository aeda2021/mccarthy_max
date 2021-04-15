#######################################
##AEDA Machine Learning assignment  ##
##Based on scripts from Lucas 2020 ##
####################################

## If you don't have these loaded yet, here are the libraries you will need
## Load general data manipulation / plotting libraries
library(dplyr)
library(ggplot2)

# Load modeling libraries
library(caret)
library(ranger)
library(pdp)
library(traitdata)
library(kernlab)

## Load helper scripts from Lucas 2020 - these are on the Github site as ML_helpers.R
## You can download and then source from this file (change file path to the location on your computer)
source('/Users/winfreelab/Dropbox/Rutgers_/Classes/Spring 2021/AEDA/github/mccarthy_max/ML_helpers.R')

set.seed(100)

## Now let's use similar methods to those we used in the exercise to evaluate covariates of litter/clutch size in reptiles!
## Load a larger dataset for amniotes

data(amniota)

amniote_data<-amniota

names(amniote_data)
dim(amniote_data)

sum(!is.na(amniote_data$litter_or_clutch_size_n))

#The trait names should be pretty self-explanatory. "svl" = snout-vent length 

#Q1: Write some code to clean the data.
#Rename the variable of interest to "y", log transform variable of interest and remove any taxa with missing litter/clutch size data.
#Then, retain only taxa in the class Reptilia and remove any variables with no data (all NA).

amniote_data <- amniote_data %>% 
  filter(Class == "Reptilia") %>% 
  filter(!is.na(litter_or_clutch_size_n)) %>% 
  filter(litter_or_clutch_size_n >= 1) %>% 
  mutate(y = log1p(litter_or_clutch_size_n)) %>% 
  dplyr::select(where(~!all(is.na(.x))))
names(amniote_data)

##Q2: Plot the distribution of log-transformed litter/clutch size in reptiles.
##Histogram or boxplot (or both if you want) are fine.
##Visualizing by order may be useful.

ggplot(amniote_data, aes(y)) + geom_histogram()

ggplot(amniote_data, aes(x = Order, y = y)) + geom_boxplot()

##Q3: Write a little more data-cleaning code!
##Impute missing data and remove taxonomic data, common name, and scientific name.

preprocesses <- preProcess(amniote_data, method = 'medianImpute')
amn_impute <- predict(preprocesses, amniote_data)
names(amn_impute)
cols=c(7, 9:30,32)
amn_impute=amn_impute[,cols]

dim(amn_impute)
head(amn_impute)

##Q4: Visualize the distributions for the predictor variables.
##Identify which variables look like they have a highly non-normal distribution.
##Log-transform these variables and visualize again.
##Which of the four models we will fit need the input variables to be log-transformed?

par(mfrow = c(2, 2))

for(i in 0:6){
  for( j in 1:4){
    
    if(j + 4 * i <= ncol(amn_impute)){
      hist(amn_impute[, j + 4 * i], breaks = 100, ylim = c(0, 80), main = j + 4 * i)
    }
  }
  print(i)
  par(mfrow = c(2, 2))
}

log_cols <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 19, 20, 21, 22, 23)

amn_impute[, log_cols] <- log1p(amn_impute[, log_cols])

##Q5: Fit a linear model relating your response variable to some potential predictors.
##To make this similar to our model for mammals, use adult body mass, age to maturity for females, incubation length, litters/clutches per year, and maximum longevity.
##Visualize model fit and get R2.
##How does this model compare to the mammal model?

folds_r <- createFolds(amniote_data$y, k = 5, returnTrain = TRUE)
trcntrl_r <- trainControl(index = folds_r, savePredictions = TRUE, search = 'random')
names(amn_impute)
apriori_formula_r <- y ~ adult_body_mass_g + female_maturity_d + incubation_d + litters_or_clutches_per_y + maximum_longevity_y
reptile_m0_lm <- train(apriori_formula_r, data = amn_impute, method = 'lm', trControl = trcntrl_r, na.action = na.omit)

plotCV(reptile_m0_lm)

reptile_m0_lm

summary(reptile_m0_lm$finalModel)

# R2 = 0.34; a linear model seems to fit these data only very slightly better than in the mammal dataset.

##Q6: Fit an elastic net to the data. Use the same hyperparameters used for the mammal dataset.
##Visualize model fit and get maximum R2.
##Plot R2 vs lasso/ridge fraction and strength of regularization (lambda).
##Does using the elastic net improve prediction relative to the linear model for this dataset?

enet_gr_r <- expand.grid(lambda = 10 ^ seq(0, -4, length.out = 20), fraction = c(seq(0.01, 1, length.out = 25)))
reptile_m1_enet <- train(y ~ ., data = amn_impute, method = 'enet', tuneGrid = enet_gr_r, trControl = trcntrl_r, na.action = na.omit)

plotCV(reptile_m1_enet)

reptile_m1_enet$results$Rsquared %>% max

# R2 = 0.36; yes, the elastic net model seems to allow for better prediction than the linear model for this dataset.

reptile_m1_enet$results %>%
  ggplot(aes(fraction, Rsquared, colour = lambda, group = factor(lambda))) +
  geom_line() +
  geom_point() + scale_color_viridis_c(trans = 'log10') + xlab('Lasso/Ridge fraction')


##Q7: Fit a Gaussian process model to the data. Use the same range of sigma values used for the mammal dataset. 
##Visualize model fit and get R2.
##Plot R2 vs sigma. How does this plot compare to the plot from the mammal dataset?
##Overall, does the Gaussian process model perform better than the linear model?

gp_gr_r <- data.frame(sigma = c(0.01, 0.02, 0.04, 0.08, 0.16))
reptile_m2_gp <- train(y ~ ., data = amn_impute, method = 'gaussprRadial', tuneGrid = gp_gr_r, trControl = trcntrl_r, na.action = na.omit)

reptile_m2_gp$results %>% ggplot(aes(sigma, Rsquared)) +
  geom_line() + geom_point() + xlab('Sigma')

# The overall difference in R2 values in this plot as sigma changes is not as great as in the Gaussian process R2 vs. sigma plot for the mammal dataset, but the shape of the curve is much more peaked (vs. more of a general decline in R2 with increasing sigma in the mammal GP model).

plotCV(reptile_m2_gp)

reptile_m2_gp
reptile_m2_gp$results$Rsquared %>% max

# R2 ~= 0.44; the Gaussian process model does appear to predict the patterns in the reptile data better than a linear model.

##Q7: Train a random forest on the data. Note - use a lower maximum number of random predictors by setting mtry = c(2, 5, 10, 20).
##Visualize model fit and get R2.
##Plot R2 vs node size and number of random predictors.
##What does the node size selected indicate about the amount of noise in the model?
##What does the number of random predictors selected indicate about interaction depth?

rf_gr_r <- expand.grid(mtry = c(2, 5, 10, 20), splitrule = 'variance', min.node.size = c(5, 10, 20, 50))
reptile_m3_rf <- train(y ~ ., data = amn_impute, method = 'ranger', tuneGrid = rf_gr_r, trControl = trcntrl_r, na.action = na.omit, importance = 'impurity', num.trees = 1000)

reptile_m3_rf$results %>%
  ggplot(aes(mtry, Rsquared, colour = factor(min.node.size), group = factor(min.node.size))) +
  geom_line() +
  geom_point() +
  labs(colour = 'min.node.size')

# A greater amount of noise will require stronger regularization, which can be achieved via smaller node size.

plotCV(reptile_m3_rf)

reptile_m3_rf
reptile_m3_rf$results$Rsquared %>% max

# R2 ~= 0.64

##Q8: Overall, which model(s) perform best at predicting litter/clutch size, and which perform the worst?
##Compare this to the mammal analysis. What does this say about the universality of these methods?

# Much like in the mammal analysis, the random forest model seems to perform best, followed by the Gaussian process model, with the linear and elastic net models performing similarly and relatively poorly. This might suggest that relatively more complex ML methods such as random forest can consistently be expected to provide good predictions of patterns in large and complicated datsets such as these.

##Q9: Evaluate variable importance for the elastic net, gaussian process, and random forest.
##Which variable is most important across models? 

varImp(reptile_m1_enet)
varImp(reptile_m2_gp)
varImp(reptile_m3_rf)

# Adult body mass appears to consistently be the most important variable across all models.

##Q10: Plot functional forms for the relationship between litter/clutch size and the most important variable for the Gaussian Process and the random forest models.
##How do they differ?
##What does this say about the likely relationship between litter/clutch size and the best predictor variable?

partial(reptile_m1_enet,pred.var = c('adult_body_mass_g', 'y'),plot = TRUE)
partial(reptile_m2_gp,pred.var = c('adult_body_mass_g', 'y'),plot = TRUE)
partial(reptile_m3_rf,pred.var = c('adult_body_mass_g', 'y'),plot = TRUE)

# 
