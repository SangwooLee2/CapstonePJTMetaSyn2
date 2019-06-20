x_train_std = read.csv("./data/x_train_std.csv", header = TRUE)
x_train_std_sh = read.csv("./data/x_train_std_sh.csv", header = TRUE)
y1_train_std = read.csv("./data/y1_train_std.csv", header = TRUE)
y2_train_std = read.csv("./data/y2_train_std.csv", header = TRUE)

# reference: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/115-famd-factor-analysis-of-mixed-data-in-r-essentials/

nrow(x_train_std)
ncol(x_train_std)
colnames(x_train_std)
library(FactoMineR)
library("factoextra")


# FAMD computation -----------------------------------
res.FAMD = FAMD (x_train_std_sh, ncp = 10, graph = TRUE, sup.var = NULL, 
      ind.sup = NULL, axes = c(1,2), row.w = NULL, tab.comp = NULL)

print(res.FAMD)
#1. Kaiser-Harris criterion suggests retaining PCs with eigenvalues > 1; PCs with
#   eigenvalues < 1 explain less varaince than contained in a single variable.
#2. Cattell Scree test visually inspects the elbow graph for diminishing return;
#   retain PCs before a drastic drop-off.
#3. Run simulations and extract eigenvalues from random data matrices of the same
#   dimension as your data; find where the parallel analysis overshadows real data.


# res.FAMD.test= FAMD (x_train_std[1:5000,1:2], ncp = 10, graph = TRUE, sup.var = NULL, ind.sup = NULL, axes = c(1,2), row.w = NULL, tab.comp = NULL)
# => worked => [1:5000, ] works  => [1:10000, ] works 
# f <- rbind(first, second)
res.FAMD_1 = FAMD (x_train_std[1:10000,], ncp = 10, graph = TRUE, sup.var = NULL, 
                 ind.sup = NULL, axes = c(1,2), row.w = NULL, tab.comp = NULL)
res.FAMD_2 = FAMD (x_train_std[10001:20000,], ncp = 10, graph = TRUE, sup.var = NULL, 
                   ind.sup = NULL, axes = c(1,2), row.w = NULL, tab.comp = NULL)
res.FAMD_3 = FAMD (x_train_std[20001:30000,], ncp = 10, graph = TRUE, sup.var = NULL, 
                   ind.sup = NULL, axes = c(1,2), row.w = NULL, tab.comp = NULL)
res.FAMD_4 = FAMD (x_train_std[30001:40000,], ncp = 10, graph = TRUE, sup.var = NULL, 
                   ind.sup = NULL, axes = c(1,2), row.w = NULL, tab.comp = NULL)
res.FAMD_5 = FAMD (x_train_std[40001:nrow(x_train_std),], ncp = 10, graph = TRUE, sup.var = NULL, 
                   ind.sup = NULL, axes = c(1,2), row.w = NULL, tab.comp = NULL)
res.FAMD_6 = FAMD (x_test_std[1:10000,], ncp = 10, graph = TRUE, sup.var = NULL, 
                   ind.sup = NULL, axes = c(1,2), row.w = NULL, tab.comp = NULL)
res.FAMD_7 = FAMD (x_test_std[10001:nrow(x_test_std),], ncp = 10, graph = TRUE, sup.var = NULL, 
                   ind.sup = NULL, axes = c(1,2), row.w = NULL, tab.comp = NULL)

ind1 = get_famd_ind(res.FAMD_1)
ind2 = get_famd_ind(res.FAMD_2)
ind3 = get_famd_ind(res.FAMD_3)
ind4 = get_famd_ind(res.FAMD_4)
ind5 = get_famd_ind(res.FAMD_5)
ind6 = get_famd_ind(res.FAMD_6)
ind7 = get_famd_ind(res.FAMD_7)

x_train_std_PCA = rbind(rbind( rbind( rbind(ind1, ind2), ind3), ind4), ind5)
x_test_std_PCA = rbind(ind6, ind7)
write.csv(x_train_std_PCA, file='./data/x_train_std_PCA.csv', x=Fail)
write.csv(x_test_std_PCA, file='./data/x_test_std_PCA.csv', x=Fail)

rm(res.FAMD_1, res.FAMD_2, res.FAMD_3, res.FAMD_4, res.FAMD_5, res.FAMD_6, res.FAMD_7)
rm(ind1, ind2, ind3, ind4, ind5, ind6, ind7)
rm(x_train_std_PCA, x_test_std_PCA)

# eigenvalue -------------------------------
  #  Extract the eigenvalues/variances retained by each dimension (axis) ###########
eig.val <- get_eigenvalue(res.FAMD)
eig.val

# fviz_eig() or fviz_screeplot() [factoextra package] can be used to draw the scree plot 
# (the percentages of inertia explained by each FAMD dimensions)
fviz_screeplot(res.FAMD)
fviz_eig(res.FAMD) # Visualize the eigenvalues/variances #############

# graph of variables ####################################
var = get_famd_var(res.FAMD)
var
  # note)
  # get_famd(): Extract the results for variables and individuals
  # get_famd_ind(): Extract the results for individuals only
  # get_famd_var(): Extract the results for quantitative and qualitative variables only

# To extract the results for quantitative variables
quanti.var <- get_famd_var(res.FAMD, "quanti.var") 
quanti.var 

#  results for qualitative variables can be extracted as follow:
quali.var = get_famd_var(res.FAMD, "quali.var")
quali.var 

  # Coordinates of variables
head(var$coord)
  # Cos2: quality of representation on the factore map
head(var$cos2)
  # Contributions to the  dimensions
head(var$contrib)

# The following figure shows t1) he correlation between variables 
# (both quantitative and qualitative variables)
# and the principal dimensions, as well as, 
# 2) the contribution of variables to the dimensions 1 and 2. 
# The following functions [in the factoextra package] are used:
# fviz_famd_var() to plot both quantitative and qualitative variables
# fviz_contrib() to visualize the contribution of variables to the principal dimensions

# Plot of variables:  Visualize the results for (all) variables #######################
fviz_famd_var(res.FAMD, repel = TRUE)

# To visualize quantitative variables only
fviz_famd_var(res.FAMD, "quanti.var", repel = TRUE,  col.var = "black")
  # Briefly, the graph of variables (correlation circle) shows the relationship between variables, the quality of the representation of variables, as well as, the correlation between variables and the dimensions. Read more at PCA (Chapter @ref(principal-component-analysis)), MCA (Chapter @ref(multiple-correspondence-analysis)) and MFA (Chapter @ref(multiple-factor-analysis)).

fviz_famd_var(res.FAMD, "quanti.var", col.var = "contrib", repel = TRUE, gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
  # The most contributing quantitative variables can be highlighted on the scatter plot using the argument col.var = "contrib". This produces a gradient colors, which can be customized using the argument gradient.cols.

# Color by cos2 values: quality on the factor map
fviz_famd_var(res.FAMD, "quanti.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE)
  # Similarly, you can highlight quantitative variables using their cos2 values 
  # representing the quality of representation on the factor map. If a variable is well
  # represented by two dimensions, the sum of the cos2 is closed to one. 
  # For some of the items, more than 2 dimensions might be required to perfectly represent
  # the data.

# To visualize qualitative variables, type this
fviz_famd_var(res.FAMD, "quali.var", col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
  # => this works, but hard to understand 

# fviz_famd_var(res.FAMD, "quali.var", col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel=TRUE)
  # ==> this does not work 

# Visualize the results for individuals ##########################
fviz_famd_ind(res.FAMD, repel = TRUE)
  # lsw) either program is wrong, or, too many records 

#  to color individuals by their cos2 and contribution values:
fviz_famd_ind(res.FAMD, col.ind = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

# Contribution to the first dimension
fviz_contrib(res.FAMD, "var", axes = 1)
  # The red dashed line on the graph above indicates the expected average value, If the contributions were uniform. Read more in chapter (Chapter @ref(principal-component-analysis)).
# Contribution to the second dimension
fviz_contrib(res.FAMD, "var", axes = 2)
# Contribution to the 3rd dimension
fviz_contrib(res.FAMD, "var", axes = 3)
# Contribution to the 4th dimension
fviz_contrib(res.FAMD, "var", axes = 4)
# Contribution to the 5th dimension
fviz_contrib(res.FAMD, "var", axes = 5) # 6~: causes errors

# Extract the results for individuals #####################
ind = get_famd_ind(res.FAMD)
head(ind$coord)
head(ind$cos2)
head(ind$contrib)


# Note that, it’s possible to color the individuals using any of the qualitative variables 
# in the initial data table. To do this, the argument habillage is used in the 
# fviz_famd_ind() function. For example, if you want to color the wines according to 
# the supplementary qualitative variable “Label”, type this:
fviz_mfa_ind(res.FAMD, 
             habillage = "Label", # color by groups 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE # Avoid text overlapping 
             ) 


# If you want to color individuals using multiple categorical variables at the 
# same time, use the function fviz_ellipses() [in factoextra] as follow:
fviz_ellipses(res.FAMD, repel = TRUE)




# library(mice) #Load the multivariate imputation by chained equations library.
# mice::md.pattern(house) #
# mice::md.pattern(house_smpl) #
# mice::md.pattern(house_smpl_std) #
# mice::md.pattern(house_smpl_sh) #
# mice::md.pattern(house_smpl_sh_std) #

# lsw) any patterns in missingness? 
library(VIM)
VIM::aggr(house, bars=TRUE, numbers = TRUE, prop = TRUE, combined = TRUE)
house_new = house[, colSums(is.na(house)) != 0 ]
VIM::aggr(house_new, bars=TRUE, numbers = TRUE, prop = TRUE, combined = TRUE)

#Basic numerical EDA for states dataset.
summary(house)
sapply(house, sd)
cor(house)

summary(house_smpl)
sapply(house_smpl, sd)
cor(house_smpl)

summary(house_smpl_sh)
sapply(house_smpl_sh, sd)
cor(house_smpl_sh)

summary(house_smpl_sh_std)
sapply(house_smpl_sh_std, sd)
cor(house_smpl_sh_std)


#Basic graphical EDA for the states dataset.
plot(house)
plot(house_sh_stand)

# ----------------------------

#Creating a saturated model 
# (a model with all variables included).
# model_smpl.saturated = lm(SalePrice ~ ., data = house_smpl)
# summary(model_smpl.saturated) #Many predictor variables are not significant, yet the
#overall regression is significant.


#Creating a saturated model 
# (a model with all variables included).
model_smpl_sh_std.saturated = lm(SalePrice ~ ., data = house_smpl_sh_std)
summary(model_smpl_sh_std.saturated) #Many predictor variables are not significant, yet the
#overall regression is significant? check by seeing f-test results 
plot(model_smpl_sh_std.saturated) 
#Assessing the assumptions of the model.

# model_smpl_std.saturated = lm(SalePrice ~ ., data = house_smpl_std)
# summary(model_smpl_std.saturated) #Many predictor variables are not significant, yet the
# overall regression is significant.

# -------------------------------

library(car) #Companion to applied regression.
# influencePlot(model.saturated)
influencePlot(model_smpl_sh_std.saturated)

# vif(model.saturated) 
#Assessing the variance inflation factors for the variables
#in our model.

vif(model_smpl_sh_std.saturated)

# -----------------

#Added variable plots for assessing the contribution of each 
# additional variable.
avPlots(model_smpl_sh_std.saturated) #Distinct patterns are indications 
# of good contributions
#to the model; absent patterns usually are pointers to
#variables that could be dropped from the model.

# ------------------------------
#We note that Illiteracy has a large VIF, an insignificant p-value in the overall
#regression, and no strong distinct pattern in the added-variable plot. What
#happens when we remove it from the model?
model2 = lm(SalePrice ~ . - YearBuilt, data = house_smpl_sh_std)# lsw) - Illiteracy: Make a model not using Illiteracy 
summary(model2) #R^2 adjusted went up, model still significant, etc.
plot(model2) #No overt additional violations.
influencePlot(model2) #No overt additional violations; Hawaii actually lowers
#its hat value (leverage).
vif(model2) #VIFs all decrease.

#We can compare these two models using a partial F-test using the anova function.
#Here, the first model we supply is the reduced model, and the second is the full
#model.
anova(model2, model_smpl_sh_std.saturated) #The p-value is quite large, indicating that we
#retain the null hypothesis. Recall that the null
#hypothesis is that the slope coefficients of the
#variables in the subset of interest are all 0.
#We retain this hypothesis and conclude that the
#Illiteracy variable is not informative in our
#model; we move forward with the reduced model.
# =========> lsw) makes no difference 

model3 = lm(SalePrice ~ . - Exterior1st, data = house_smpl_sh_std)# lsw) - Illiteracy: Make a model not using Illiteracy 
summary(model3) #R^2 adjusted went up, model still significant, etc.
plot(model3) #No overt additional violations.
influencePlot(model3) #No overt additional violations; Hawaii actually lowers
vif(model3) #VIFs all decrease.
anova(model3, model_smpl_sh_std.saturated) #Th
# lsw) =========> lsw) makes no diffefence 

model4 = lm(SalePrice ~ . - Exterior2nd, data = house_smpl_sh_std)# lsw) - Illiteracy: Make a model not using Illiteracy 
summary(model4) #R^2 adjusted went up, model still significant, etc.
plot(model4) #No overt additional violations.
influencePlot(model4) #No overt additional violations; Hawaii actually lowers
vif(model4) #VIFs all decrease.
anova(model4, model_smpl_sh_std.saturated) #Th
# lsw) =========> makes no difference

model5 = lm(SalePrice ~ . - TotalBsmtSF, data = house_smpl_sh_std)# lsw) - Illiteracy: Make a model not using Illiteracy 
summary(model5) #R^2 adjusted went up, model still significant, etc.
plot(model5) #No overt additional violations.
influencePlot(model5) #No overt additional violations; Hawaii actually lowers
vif(model5) #VIFs all decrease.
anova(model5, model_smpl_sh_std.saturated) #Th
# lsw) =========> makes no difference

model6 = lm(SalePrice ~ . - GrLivArea, data = house_smpl_sh_std)# lsw) - Illiteracy: Make a model not using Illiteracy 
summary(model6) #R^2 adjusted went up, model still significant, etc.
plot(model6) #No overt additional violations.
influencePlot(model6) #No overt additional violations; Hawaii actually lowers
vif(model6) #VIFs all decrease.
anova(model6, model_smpl_sh_std.saturated) #Th
# lsw) =========> makes difference

model7 = lm(SalePrice ~ . - GarageCars, data = house_smpl_sh_std)# lsw) - Illiteracy: Make a model not using Illiteracy 
summary(model7) #R^2 adjusted went up, model still significant, etc.
plot(model7) #No overt additional violations.
influencePlot(model7) #No overt additional violations; Hawaii actually lowers
vif(model7) #VIFs all decrease.
anova(model7, model_smpl_sh_std.saturated) #Th
# lsw) =========> makes difference

model8 = lm(SalePrice ~ . - GarageArea, data = house_smpl_sh_std)# lsw) - Illiteracy: Make a model not using Illiteracy 
summary(model8) #R^2 adjusted went up, model still significant, etc.
plot(model8) #No overt additional violations.
influencePlot(model8) #No overt additional violations; Hawaii actually lowers
vif(model8) #VIFs all decrease.
anova(model8, model_smpl_sh_std.saturated) #Th
# lsw) =========> makes difference

# ------------------------------


#Let's use the partial F-test to test multiple predictors at once. 
# As compared
#to the saturated model, does the subset of Illiteracy, Area, and Income
# haveany effect on our prediction of Life.Exp?
model.reduced = lm(SalePrice ~ . - YearBuilt - Exterior1st - Exterior2nd - TotalBsmtSF - GrLivArea - GarageCars - GarageArea, 
                   data = house_smpl_sh_std)

anova(model.reduced, model_smpl_sh_std.saturated) 
# ==========> lsw) models are difference 

#Checking the model summary and assumptions of the reduced model.
summary(model.reduced)
plot(model.reduced)
influencePlot(model.reduced)
vif(model.reduced)

# ------------------------------

#We can also inspect the AIC and BIC values to 
# compare various models.
# AIC(model.full,    #Model with all variables.
#     model2,        #Model with all variables EXCEPT Illiteracy.
#     model.reduced) #Model with all variables EXCEPT Illiteracy, Area, and Income.

# BIC(model.full,
#     model2,
#     model.reduced) #Both the minimum AIC and BIC values appear alongside the
#reduced model that we tested above.

#We can use stepwise regression to help automate the 
# variable selection process.
#Here we define the minimal model, the full model, 
# and the scope of the models
#through which to search:
model.empty = lm( SalePrice ~ 1, data = house_smpl_sh_std ) 
#The model with an intercept ONLY.
scope = list(lower = formula(model.empty), upper = formula(model_smpl_sh_std.saturated))


if (0){
library(MASS) #The Modern Applied Statistics library.

#Stepwise regression using AIC as the criteria (the penalty k = 2).
# lsw) 	 the multiple of the number of degrees of freedom used for the penalty. Only k = 2 gives the genuine AIC: k = log(n) is sometimes referred to as BIC or SBC 

forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
backwardAIC = step(model_smpl_sh_std.saturated, scope, direction = "backward", k = 2)
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
bothAIC.full = step(model_smpl_sh_std.saturated, scope, direction = "both", k = 2)

#Stepwise regression using BIC as the criteria (the penalty k = log(n)).
forwardBIC = step(model.empty, scope, direction = "forward", k = log(50))
backwardBIC = step(model_smpl_sh_std.saturated, scope, direction = "backward", k = log(50))
bothBIC.empty = step(model.empty, scope, direction = "both", k = log(50))
bothBIC.full = step(model_smpl_sh_std.saturated, scope, direction = "both", k = log(50))

#In this case, all procedures yield the model with only the Murder, HS.Grad,
#Frost, and Population variables intact.

#Checking the model summary and assumptions of the reduced model.
summary(forwardAIC)
plot(forwardAIC)
influencePlot(forwardAIC)
vif(forwardAIC)
avPlots(forwardAIC)
confint(forwardAIC)

summary(backwardAIC)
summary(bothAIC.empty)
summary(bothAIC.full)

summary(forwardBIC)
summary(backwardBIC)
summary(bothBIC.empty)
summary(bothBIC.full)  
}
