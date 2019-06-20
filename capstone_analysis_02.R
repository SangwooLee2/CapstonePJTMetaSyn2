x_train_std = read.csv("./data/x_train_std.csv", header = TRUE)
# x_train_std_sh = read.csv("./data/x_train_std_sh.csv", header = TRUE)
# y1_train_std = read.csv("./data/y1_train_std.csv", header = TRUE)
y2_train_std = read.csv("./data/y2_train_std.csv", header = TRUE)

xy2 = merge(x_train_std,y2_train_std, by=0)

# reference: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/115-famd-factor-analysis-of-mixed-data-in-r-essentials/

nrow(xy2)
ncol(xy2)
colnames(xy2)
library(dplyr)
xy2 = select(xy2,-c(Row.names, X, X0))
names(xy2)[5]='alcohol'
names(xy2)[11]='Metasyn'

xy3 = select(xy2,-c(hypertension, Diabetes)) # cheating removed

table(xy2$sex)/nrow(xy2)
table(xy2$exercise)/nrow(xy2)
table(xy2$alcohol)/nrow(xy2)
table(xy2$hypertension)/nrow(xy2)
table(xy2$Diabetes)/nrow(xy2)
table(xy2$Metasyn)/nrow(xy2)

# http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/
library(tidyverse)
library(broom)

#Basic numerical EDA for dataset.
summary(xy2)
sapply(xy2, sd)
# cor(xy2)

plot(xy2, col = xy2$Metasyn) #Basic graphical EDA.
plot(xy3, col = xy2$Metasyn) #Basic graphical EDA.

# models generated ####################################
logit.cheat= glm(Metasyn ~ age + sex + smoking + exercise + alcohol + hypertension + Diabetes + Height + Weight + BMI, 
                   family = "binomial",
                   data = xy2)
# logit.modelA = glm(Metasyn ~ age + sex + smoking + exercise + alcohol + BMI, family = "binomial", data = xy2)
# logit.modelB = glm(Metasyn ~ age + sex + smoking + exercise + alcohol + Height + Weight, family = "binomial",data = xy2)
logit.modelC = glm(Metasyn ~ age + sex + smoking + exercise + alcohol + BMI + Height + Weight, family = "binomial",data = xy2)

########################################################
# Residual plot for logistic regression with an added loess smoother; we would
#hope that, on average, the residual values are 0.
scatter.smooth(logit.cheat$fit,
               residuals(logit.cheat, type = "deviance"),
               lpars = list(col = "red"),
               xlab = "Fitted Probabilities",
               ylab = "Deviance Residual Values",
               main = "Residual Plot for\nLogistic Regression of Metasyn")
abline(h = 0, lty = 2)
library(car)
influencePlot(logit.cheat) #Can still inspect the influence plot.
summary(logit.cheat) #Investigating the overall fit of the model.


scatter.smooth(logit.modelC$fit,
               residuals(logit.modelC, type = "deviance"),
               lpars = list(col = "red"),
               xlab = "Fitted Probabilities",
               ylab = "Deviance Residual Values",
               main = "Residual Plot for\nLogistic Regression of Metasyn, modelC")
abline(h = 0, lty = 2)
influencePlot(logit.modelC) #Can still inspect the influence plot.
  # creates a “bubble” plot of Studentized residuals versus hat values, 
  # with the areas of the circles representing the observations proportional 
  # to the value Cook's distance. Vertical reference lines are drawn at twice and three times the average hat value, horizontal reference lines at -2, 0, and 2 on the Studentized-residual scale.

summary(logit.modelC) #Investigating the overall fit of the model.


#Inspecting the relationship between log odds and odds.
cbind("Log Odds" = logit.modelC$coefficients,
      "Odds" = exp(logit.modelC$coefficients))

confint(logit.modelC) #For logistic regression objects, the confint() function
#defaults to using the log likelihood to generate confidence
#intervals; this is similar to inverting the likelihood
#ratio test.

confint.default(logit.modelC) #To generate confidence intervals for logistic
#regression models based on the standard errors
#as we are accustomed to, we can use the
#confint.default() function.

#Generating confidence intervals for the coefficients on the odds scale.
exp(confint(logit.modelC))
exp(confint.default(logit.modelC))

#######################################################
# http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/
## linearity assumptios checked here 
probabilities = predict(logit.modelC, type = "response") # lsw, size: 30952 
predicted.classes = ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

# Remove qualitative variables from the original data frame and bind the logit values to the data
# Select only numeric predictors
xy3_1 = xy3 %>% dplyr::select_if(is.numeric) 
predictors = colnames(xy3_1)
# Bind the logit and tidying the data for plot
xy3_1 = xy3_1 %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
  # lsw) xy3: 30952 by 9 => xy3_1: 278568(=30952*9) by 3 

# Create the scatter plots:
ggplot(xy3_1, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

