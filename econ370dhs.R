## ECON 370 PROJECT 2
## NAME: Benet Ge & Weiran Jiang
## DATE: 11/6/24

##### LIBRARIES ---------------------------------

# install.packages('gitcreds')
# install.packages("tidyverse")
# install.packages("haven") 
# install.packages("fastDummies")
# install.packages("tree")
# install.packages("randomForest")
# install.packages("gbm")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("webshot")
# webshot::install_phantomjs()


library(tidyverse)
library(haven)
library(tree)
library(randomForest)
library(gbm)
library(fastDummies)
library(rpart)
library(rpart.plot)
library(grf)
library(broom)
library(ggplot2)
library(gridExtra)
library(knitr)
library(kableExtra)

#ridge & lasso
library(glmnet)
library(hdm)

library(gitcreds)
# gitcreds_set()

######### INSERT PCA PACKAGE #######
#pca()

##Benet File Path
mypath <- "~/ECON370/ECON370dhs/econ370 project/"
senegal.dhs <- read_dta(paste0(mypath, "SNBR7IDT/SNBR7IDT/SNBR7IFL.dta")) %>% 
  filter(!is.na(midx)) %>% 
  filter(b5 == 1) %>% # Child is alive
  filter(v135 == 1) # Usual resident or visitor of Senegal

##Weiran File Path
#mypath<- "/Users/weiran/Data\ Science\ in\ Econ/PredictingInfantProject/Econ370Project2/"

#senegal.dhs <- read_dta(paste0(mypath, "SNBR7IFL.DTA")) %>% 
 # filter(!is.na(midx)) %>% 
 # filter(b5 == 1) %>% 
 # filter(v135 == 1)

##Clean Dataset
senegal.data <- senegal.dhs %>% 
  select(hw70, hw1, bord, b0, b1, b2, b4, b11, 
         v012, v024, v025, v113, v116, v119, v133,
         v136, v151, v160, v161, v190, v212, v745a,
         v745b, v367, m15, v152, v153, v213, v218,
         v404, v409, v409a, v410, v411, v411a,
         v412a, v412c, v414e, v414f, v414g, v414h,
         v414i, v414j, v414k, v414l, v414m, v414n,
         v414o, v414p, v416, v504, v505, v506, v508,v511,
         v605, v613, v624, v704, v714, v715, v716, v719, v730,
         v743a, v743b, v743f, v744a, v744b, v744c,
         v744d, v744e, v746, m4, m18, m19, m45, h2,
         h3, h4, s224b, s224ca, s224cb, s224cc,
         s224cd, s224ce, v120, v121, v122, v124, v125,
         v131, v157, v158, ml101, v201, v206, v207,
         v384a, v384b, v384c, v393, v394, v501, v503, v621,
         v627, v628) %>%   # ~ 100 at the moment
  rename(haz = hw70, 
         age_months = hw1, 
         mob = b1, 
         yob = b2, 
         sex = b4, 
         interval = b11, 
         mom_age = v012,
         partner_age = v730,
         region = v024,
         urban_rural = v025,
         water_source = v113,
         sanitation = v116,
         electricity = v119,
         radio = v120,
         television = v121,
         refrigerator = v122, 
         motorcycle = v124,
         car_truck = v125,
         ethnicity = v131,
         mothers_education = v133,
         household_size = v136,
         household_head_sex = v151, # we should group the variables for the slides
         household_head_age = v152,
         telephone = v153,
         reading_newspaper = v157,
         listening_radio = v158,
         mosquito_net = ml101,
         total_children = v201,
         son_died = v206,
         daughter_died = v207,
         familyplanning_radio = v384a,
         familyplanning_tv = v384b,
         familyplanning_newsp = v384c,
         familyplanning_visit = v393,
         healthfacility_visit = v394,
         toilet_shared = v160,
         fuel = v161,
         wealth_index = v190,
         age_first_birth = v212,
         currently_pregnant = v213,
         number_living_children = v218,
         currently_breastfeeding = v404,
         plain_water = v409, # nutrition related variables
         sugar_water = v409a,
         juice = v410,
         milk = v411, 
         formula = v411a,
         fortified_food = v412a,
         soup_broth = v412c,
         grain_foods = v414e,
         tuber_foods = v414f,
         eggs = v414g,
         meat = v414h,
         squash = v414i,
         leafy_vegetables = v414j,
         vitamin_a_fruits = v414k,
         other_fruits = v414l,
         organ_meat = v414m,
         fish = v414n,
         legumes = v414o,
         dairy = v414p,
         oral_hydration = v416,  # knowledge of medical practices
         current_marital = v501,
         number_unions = v503,
         living_with_partner = v504, # household arrangements (attention theory)
         other_wives = v505,
         wife_rank = v506,
         yo_first_cohabitation = v508,
         age_first_cohabitation = v511,
         desire_more_children = v605,
         ideal_num_children = v613,
         husband_desire_children = v621,
         unmet_contraceptive_need = v624,  # family planning/knowledge go together
         ideal_num_boys = v627,
         ideal_num_girls = v628,
         husbands_vocation = v704,
         working = v714, # figure out how to do years of education for the father
         husband_education = v715,
         vocation = v716,
         employer = v719, # fits into the Profs. Jakiela & Ozier paper, self-employment happier
         healthcare_decisionmaker = v743a,
         purchase_decisionmaker = v743b, # big purchases
         money_decisionmaker = v743f,
         beating_leaving = v744a, 
         beating_neglect = v744b, # this seems related to child development in two ways
         beating_argument = v744c,
         beating_refusesex = v744d,
         beating_burnedfood = v744e,
         biggestearner = v746, # plausibly if the woman makes more..
         breastfeeding_duration = m4,
         birth_size = m18,
         birth_weight = m19,
         iron_supplement = m45,
         tuberculosis_vaccine = h2,
         dtap_vaccine = h3,
         polio_vaccine = h4,
         hh_child_development = s224b,
         reading_books = s224ca,
         telling_stories = s224cb,
         singing_songs = s224cc,
         taking_walks = s224cd, 
         counting_drawing_naming = s224ce,
         homeowner = v745a,
         landowner = v745b,
         child_wanted = v367,
         pob = m15
  )

## drop observations with missing height-for-age z-scores
senegal.data$haz <- as.numeric(senegal.data$haz)
senegal.data <- filter(senegal.data, !is.na(haz) & haz!=9996 & haz!=9997 & haz!= 9998)

## idiosyncratic cleaning

## change age at time of survey to age at birth
senegal.data$mom_age <- senegal.data$mom_age - (2014 - senegal.data$yob)

## twin dummy
senegal.data$twin = ifelse(senegal.data$b0 == 0, 0, 1)
senegal.data  <- select(senegal.data, !b0)

## birth interval cannot be NAN (first births), replace with median
senegal.data$interval[is.na(senegal.data$interval)] <- median(senegal.data$interval, na.rm=TRUE)

# make dummies for factor variables:
factor_cols <- c("mob",
                 "yob", 
                 "sex",
                 "bord",
                 "region",
                 "urban_rural",
                 "water_source",
                 "sanitation",
                 "electricity",
                 "radio",
                 "television",
                 "refrigerator",
                 "motorcycle",
                 "car_truck",
                 "ethnicity",
                 "telephone",
                 "reading_newspaper",
                 "listening_radio",
                 "household_head_sex",
                 "toilet_shared",
                 "fuel",
                 "wealth_index",
                 "homeowner",
                 "landowner",
                 "child_wanted",
                 "pob",
                 "twin",
                 "mosquito_net",
                 "familyplanning_radio",
                 "familyplanning_tv",
                 "familyplanning_newsp",
                 "familyplanning_visit",
                 "healthfacility_visit",
                 "currently_pregnant",
                 "currently_breastfeeding",
                 "plain_water",
                 "sugar_water",
                 "juice",
                 "milk",
                 "formula",
                 "fortified_food",
                 "soup_broth",
                 "grain_foods",
                 "tuber_foods",
                 "eggs",
                 "meat",
                 "squash",
                 "leafy_vegetables",
                 "vitamin_a_fruits",
                 "other_fruits",
                 "organ_meat",
                 "fish",
                 "legumes",
                 "dairy",
                 "oral_hydration",
                 "current_marital",
                 "living_with_partner",
                 "yo_first_cohabitation",
                 "husband_desire_children",
                 "unmet_contraceptive_need",
                 "husbands_vocation",
                 "working",
                 "vocation",
                 "employer",
                 "healthcare_decisionmaker",
                 "purchase_decisionmaker",
                 "money_decisionmaker",
                 "beating_leaving",
                 "beating_neglect",
                 "beating_argument",
                 "beating_refusesex",
                 "beating_burnedfood",
                 "biggestearner",
                 "breastfeeding_duration",
                 "birth_size",
                 "iron_supplement",
                 "tuberculosis_vaccine",
                 "dtap_vaccine",
                 "polio_vaccine",
                 "hh_child_development",
                 "reading_books",
                 "telling_stories",
                 "singing_songs",
                 "taking_walks",
                 "counting_drawing_naming"
)

senegal.data[factor_cols] <- lapply(senegal.data[factor_cols], as.character)
senegal.data <- dummy_cols(senegal.data, select_columns = factor_cols, remove_selected_columns = TRUE)
senegal.data[is.na(senegal.data)] <- 0

###### Principal Component Analysis --------------------
#install.packages("prcomp")
senegal.data <- senegal.data[, sapply(senegal.data, function(col) var(col, na.rm = TRUE) > 0)]
pca_result <- prcomp(senegal.data, scale. = TRUE)

# Check the results
summary(pca_result)


var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100
plot(var_explained, type = "b", 
     main = "Scree Plot for Full PCA with Variance Explained", 
     xlab = "Principal Components", 
     ylab = "Percentage of Variance Explained", 
     ylim = c(0, max(var_explained) + 5))  
text(1:length(var_explained), var_explained, 
     labels = round(var_explained, 1), 
     pos = 3, cex = 0.8)

biplot(pca_result, scale = 0, cex = 0.6, 
       col = c("black", "red"), 
       xlabs = rep("•", nrow(pca_result$x)),  
       xlim = c(-5, 5),  
       ylim = c(-5, 5))  


par(mfrow = c(1, 3), mar = c(10, 4, 4, 2) + 0.1)  
for (i in 1:3) {
  barplot(pca_result$rotation[, i], 
          las = 2,                
          cex.names = 0.7,        
          main = paste("Full PC", i, "Loadings"), 
          ylab = "Loadings", 
          xlab = "Variables")
}  

##Most important loadings:
# Extract loadings for the first three principal components
loadings <- pca_result$rotation[, 1:3]

# Find the top 10 variables with the highest absolute loadings for PC1
top_pc1_loadings <- sort(abs(loadings[, 1]), decreasing = TRUE)[1:10]
top_pc1_variables <- names(top_pc1_loadings)

# Find the top 10 variables with the highest absolute loadings for PC2
top_pc2_loadings <- sort(abs(loadings[, 2]), decreasing = TRUE)[1:10]
top_pc2_variables <- names(top_pc2_loadings)

# Find the top 10 variables with the highest absolute loadings for PC3
top_pc3_loadings <- sort(abs(loadings[, 3]), decreasing = TRUE)[1:10]
top_pc3_variables <- names(top_pc3_loadings)

# Print results for each principal component
cat("Top 10 significant variables for PC1:\n")
print(data.frame(Variable = top_pc1_variables, Loading = loadings[top_pc1_variables, 1]))

cat("\nTop 10 significant variables for PC2:\n")
print(data.frame(Variable = top_pc2_variables, Loading = loadings[top_pc2_variables, 2]))

cat("\nTop 10 significant variables for PC3:\n")
print(data.frame(Variable = top_pc3_variables, Loading = loadings[top_pc3_variables, 3]))

## Scatterplot
# Create a data frame with the first two principal components
pca_data <- data.frame(PC1 = pca_result$x[, 1], 
                       PC2 = pca_result$x[, 2])

# Perform k-means clustering on PC1 and PC2 with a chosen number of clusters (e.g., 3)
# You can change the number of clusters (centers) as needed
set.seed(8675309)
kmeans_result <- kmeans(pca_data, centers = 3)  # Adjust centers to your needs

# Add the cluster assignments to the data frame
pca_data$Cluster <- factor(kmeans_result$cluster)

# Plot PC1 vs. PC2 with color representing clusters
library(ggplot2)

ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "PC1 vs. PC2 Scatter Plot with k-means Clustering",
       x = "Principal Component 1 (PC1)",
       y = "Principal Component 2 (PC2)") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_brewer(palette = "Set1")  # Change color palette as desired


##### PREPARE DATA --------------------------------------

Y = senegal.data$haz

X <- senegal.data %>%
  select(!c(haz)) %>%
  as.matrix()




##### OLS BENCHMARK -------------------------------------

ols <- lm(Y ~ X)
summary <- summary(ols)
p_values <- coef(summary)[, "Pr(>|t|)"]
p_value_95 <- p_values <= 0.05 
significant_var <- rownames(coef(summary))[p_value_95] %>%
  print() ## which variables are statistically significant predictors of literacy (95% level)?
coefficients <- coef(summary)[, "Estimate"]
min_p_value <- which.min(p_values)
lowest_p_value <- p_values[min_p_value]
lowest_p_value_var <- rownames(coef(summary))[min_p_value] %>%
  print() ## which variable has the lowest p-value? 
lowest_p_value_coeff <- coefficients[min_p_value] %>% 
  print() ## what is the OLS coefficient associated with that variable? 

ols_df <- ols %>%
  tidy() %>%
  as.data.frame()


##### RIDGE REGRESSION ----------------------------------

grid.lasso <- 10^seq(2, -1, length = 300) # this is optimal for visualizing the lasso cv
grid.ridge <- 10^seq(5, -1, length = 300) # this is optimal for visualizing the ridge cv
set.seed(8675309)

ridge_cv <- cv.glmnet(X, Y, alpha = 0, lambda = grid.ridge)
plot(ridge_cv)

ridge_cv$lambda.min
ridge_cv$lambda.1se
min(ridge_cv$cvm)


##### DOING RIDGE WITH LAMBDA MIN
set.seed(8675309)
ridge_cv_min <- predict(ridge_cv, type = "coefficients", s = ridge_cv$lambda.min) %>%
  as.matrix() 

ridge_cv_vars_min <- rownames(ridge_cv_min)[abs(ridge_cv_min) > mean(abs(ridge_cv_min)) &
                                          rownames(ridge_cv_min) != "(Intercept)"]
ridge_cv_vars_min # so as of right now, selecting those variables which (at the optimal lambda)
              # have a coefficient (abs) greater than the mean of the coefficients (abs)
              # which conceptually makes sense since regularization decreases magnitude of variables

ridge_mse_min <- min(ridge_cv$cvm)

ridge_min_coefficients <- tibble(variables = rownames(ridge_cv_min),
                                 coefficients = ridge_cv_min)
ridge_min_coefficients <- ridge_min_coefficients %>%
  arrange(desc(abs(coefficients)))

ridge_min_coefficients$coefficients <- as.numeric(ridge_min_coefficients$coefficients)

top_decile_threshold <- quantile(abs(ridge_min_coefficients$coefficients), 0.9)

ridge_min_coefficients <- ridge_min_coefficients %>%
  mutate(in_top_decile = abs(coefficients) >= top_decile_threshold)


ridge_min_coefficients_graph <- ggplot(ridge_min_coefficients, aes(x = reorder(variables, abs(coefficients)), y = abs(coefficients), fill = in_top_decile)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("FALSE" = "orange", "TRUE" = "red")) +  # Use orange for non-top decile, red for top decile
  labs(x = "Variables", y = "Absolute Coefficients", title = "Ridge Minimum Coefficients (Absolute Values)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))
ridge_min_coefficients_graph


##### DOING RIDGE WITH LAMBDA 1SE
set.seed(8675309)
ridge_cv_1se <- predict(ridge_cv, type = "coefficients", s = ridge_cv$lambda.1se) %>%
  as.matrix() 

ridge_cv_vars_1se <- rownames(ridge_cv_1se)[abs(ridge_cv_1se) > mean(abs(ridge_cv_1se)) &
                                              rownames(ridge_cv_1se) != "(Intercept)"]
ridge_cv_vars_1se

lambda_1se_index <- which(ridge_cv$lambda == ridge_cv$lambda.1se)
ridge_mse_1se <- ridge_cv$cvm[lambda_1se_index]
ridge_mse_1se

##### LASSO REGRESSION ----------------------------------
set.seed(8675309)
lasso_cv <- cv.glmnet(X, Y, alpha = 1, lambda = grid.lasso)  
plot(lasso_cv)

lasso_cv$lambda.min
lasso_cv$lambda.1se
min(lasso_cv$cvm)

##### DOING LASSO WITH LAMBDA MIN
set.seed(8675309)
lasso_cv_min <- predict(lasso_cv, type = "coefficients", s = lasso_cv$lambda.min) %>%
  as.matrix()
lasso_cv_vars <- rownames(lasso_cv_min)[lasso_cv_min != 0 &
                                          rownames(lasso_cv_min) != "(Intercept)"]
lasso_cv_vars

lasso_mse_min <- min(lasso_cv$cvm)
lasso_mse_min

##### DOING LASSO WITH LAMBDA 1SE
set.seed(8675309)
lasso_cv_1se <- predict(lasso_cv, type = "coefficients", s = lasso_cv$lambda.1se) %>%
  as.matrix()
lasso_cv_vars_1se <- rownames(lasso_cv_1se)[lasso_cv_1se != 0 &
                                              rownames(lasso_cv_1se) != "(Intercept)"]
lasso_cv_vars_1se

lasso_1se_index <- which(lasso_cv$lambda == lasso_cv$lambda.1se)
lasso_mse_1se <- lasso_cv$cvm[lasso_1se_index]
lasso_mse_1se

##### DOING LASSO WITH DATA-DRIVEN PROCESS
set.seed(8675309)
lasso_dd <- rlasso(Y ~ X, post = FALSE)
summary(lasso_dd)
lasso_dd_coeff <- as.matrix(coef(lasso_dd))
lasso_dd_vars <- rownames(lasso_dd_coeff)[lasso_dd_coeff != 0 &
                                            rownames(lasso_dd_coeff) != "(Intercept)"]
lasso_dd_vars

lasso_dd_predict <- predict(lasso_dd)
lasso_dd_mse <- mean((Y - lasso_dd_predict)^2)
lasso_dd_mse

##### REGRESSION TREE -----------------------------------
set.seed(8675309)
train <- sample(1:nrow(senegal.data), 0.4 * (nrow(senegal.data))) ### why 40%


mytree <- tree(haz ~ ., senegal.data, subset = train)
summary(mytree)
plot(mytree)
text(mytree, pretty = 0)

yhat <- predict(mytree, newdata = senegal.data[-train, ])
haz_test <- unlist(senegal.data[-train, "haz"])
tree_mse = mean(as.numeric(unlist((yhat - haz_test)^2)))

##### RANDOM FOREST -------------------------------------

#### YOU HAVE TO RENAME IT TO SENEGAL_RF #####

set.seed(8675309)
senegal.rf <- randomForest(haz ~ .,  
                          data = senegal.data, 
                          subset = train,
                          mtry = 21, # about the sqrt(X = 426) # default 500 trees
                          ntree = 1000, # because X = 426 we want to ensure each is used
                          importance = TRUE) 
senegal.rf
yhat_rf <- predict(senegal.rf, newdata = senegal.data[-train, ])
rf_mse = mean((yhat_rf - haz_test)^2)
rf_mse
rf_mse <= tree_mse # the rf MSE is less than the test MSE from a single tree

senegal_rf_variables <- importance(senegal.rf) %>% 
  as.data.frame() %>%
  arrange(desc(`%IncMSE`))

rf_vars <- rownames(senegal_rf_variables)[senegal_rf_variables$`%IncMSE` > mean(senegal_rf_variables$`%IncMSE`)]
rf_vars


##### GRADIENT-BOOSTED FOREST ---------------------------
set.seed(8675309)
senegal_boost <- gbm(haz ~ ., data = senegal.data[train, ], 
                    distribution = "gaussian", 
                    n.trees = 5000, 
                    interaction.depth = 2, 
                    shrinkage = 0.001, 
                    verbose = F)

gbf <- summary(senegal_boost)
gbf_variables <- tibble(influence = gbf$rel.inf,
                        variable = gbf$var)

gbf_variables_select <- gbf_variables %>%
  filter(influence > mean(influence))

gbf_vars <- gbf_variables_select$variable


yhat_boost <- predict(senegal_boost, 
                      newdata = senegal.data[-train, ], 
                      n.trees = 5000)
boost_mse = mean((yhat_boost - haz_test)^2)
boost_mse

library(gbm)

# Initialize vectors to store results
results <- data.frame()

# Define parameter ranges
n_trees_list <- c(1000, 2000, 3000, 4000, 5000)
interaction_depth_list <- c(1, 2, 3, 4, 5)
shrinkage_list <- c(0.001, 0.01, 0.1)
n_minobsinnode_list <- c(5, 10, 20)

# Nested loops to try each combination
for (n_trees in n_trees_list) {
  for (interaction_depth in interaction_depth_list) {
    for (shrinkage in shrinkage_list) {
      for (n_minobsinnode in n_minobsinnode_list) {
        set.seed(8675309)
        
        # Train the model
        gbm_model <- gbm(haz ~ ., data = senegal.data[train, ], 
                         distribution = "gaussian", n.trees = n_trees,
                         interaction.depth = interaction_depth, 
                         shrinkage = shrinkage, 
                         n.minobsinnode = n_minobsinnode, 
                         verbose = FALSE)
        
        # Calculate MSE on test data
        yhat_boost <- predict(gbm_model, newdata = senegal.data[-train, ], n.trees = n_trees)
        mse <- mean((yhat_boost - haz_test)^2)
        
        # Store the results
        results <- rbind(results, data.frame(n_trees, interaction_depth, shrinkage, n_minobsinnode, mse))
      }
    }
  }
}

# Find the best parameters based on the lowest MSE
best_params <- results[which.min(results$mse), ]
print(best_params)

# Print best parameters and results
print(gbm_tuned)

##### PRINCIPAL COMPONENT ANALYSIS  ---------------------



##### ETHIOPIA MODEL ------------------------------------

# maybe I can try a thing where I see which variables crop up the most

collected_variables <- c(Ridge.Minimum = ridge_cv_vars_min,
                                  Ridge.1se = ridge_cv_vars_1se,
                                  Lasso.Minimum = lasso_cv_vars,
                                  Lasso.1se = lasso_cv_vars_1se,
                                  Lasso.DataDriven = lasso_dd_vars,
                                  Random.Forest = rf_vars,
                                  Gradient.Boosted.Forest = gbf_vars)

frequency_vars <- table(collected_variables) %>% 
  as.data.frame()
frequency_vars <- frequency_vars %>%
  arrange(desc(frequency_vars$Freq))

collected_variables_names <- list(
  Ridge.Minimum = ridge_cv_vars_min,
  Ridge.1se = ridge_cv_vars_1se,
  Lasso.Minimum = lasso_cv_vars,
  Lasso.1se = lasso_cv_vars_1se,
  Lasso.DataDriven = lasso_dd_vars,
  Random.Forest = rf_vars,
  Gradient.Boosted.Forest = gbf_vars
)

collected_MSE <- list(ridge_mse_min,
                      ridge_mse_1se,
                      lasso_mse_min,
                      lasso_mse_1se,
                      lasso_dd_mse,
                      rf_mse,
                      boost_mse
)

mse_comparison <- tibble(Method = names(collected_variables_names),
                         MSE = collected_MSE) ##### data-driven lasso the least

mse_comparison$MSE <- as.numeric(mse_comparison$MSE)
mse_viable <- mse_comparison$Method[mse_comparison$MSE < mean(mse_comparison$MSE)]
mse_viable 

lasso_dd_frequency <- frequency_vars %>%
  filter(collected_variables %in% lasso_dd_vars)


##Benet Filepath
mypath <- "~/ECON370/ECON370dhs/econ370 project/"

ethiopia.dhs <- read_dta(paste0(mypath, "Ethiopia2016/ETBR71FL.dta")) %>% 
  filter(!is.na(midx)) %>% 
  filter(b5 == 1) %>% # Child is alive
  filter(v135 == 1) # Usual resident or visitor of Ethiopia

#Weiran Filepath
#mypath <- "/Users/weiran/Data\ Science\ in\ Econ/PredictingInfantProject/Econ370Project2/Ethiopia2016/"

#ethiopia.dhs <- read_dta(paste0(mypath, "ETBR71FL.DTA")) %>% 
#  filter(!is.na(midx)) %>% 
#  filter(b5 == 1) %>% # Child is alive
 # filter(v135 == 1) # Usual resident or visitor of Ethiopia

ethiopia.data <- ethiopia.dhs %>% 
  select(hw70, hw1, bord, b0, b1, b2, b4, b11, 
         v012, v024, v025, v113, v116, v119, v133,
         v136, v151, v160, v161, v190, v212, v745a,
         v745b, v367, m15, v152, v153, v213, v218,
         v404, v409, v409a, v410, v411, v411a,
         v412a, v412c, v414e, v414f, v414g, v414h,
         v414i, v414j, v414k, v414l, v414m, v414n,
         v414o, v414p, v416, v504, v505, v506, v508,v511,
         v605, v613, v624, v704, v714, v715, v716, v719, v730,
         v743a, v743b, v743f, v744a, v744b, v744c,
         v744d, v744e, v746, m4, m18, m19, m45, h2,
         h3, h4, v120, v121, v122, v124, v125,
         v131, v157, v158, ml101, v201, v206, v207,
         v384a, v384b, v384c, v393, v394, v501, v503, v621,
         v627, v628) %>%   # ~ 100 at the moment
  rename(haz = hw70, 
         age_months = hw1, 
         mob = b1, 
         yob = b2, 
         sex = b4, 
         interval = b11, 
         mom_age = v012,
         partner_age = v730,
         region = v024,
         urban_rural = v025,
         water_source = v113,
         sanitation = v116,
         electricity = v119,
         radio = v120,
         television = v121,
         refrigerator = v122, 
         motorcycle = v124,
         car_truck = v125,
         ethnicity = v131,
         mothers_education = v133,
         household_size = v136,
         household_head_sex = v151, # we should group the variables for the slides
         household_head_age = v152,
         telephone = v153,
         reading_newspaper = v157,
         listening_radio = v158,
         mosquito_net = ml101,
         total_children = v201,
         son_died = v206,
         daughter_died = v207,
         familyplanning_radio = v384a,
         familyplanning_tv = v384b,
         familyplanning_newsp = v384c,
         familyplanning_visit = v393,
         healthfacility_visit = v394,
         toilet_shared = v160,
         fuel = v161,
         wealth_index = v190,
         age_first_birth = v212,
         currently_pregnant = v213,
         number_living_children = v218,
         currently_breastfeeding = v404,
         plain_water = v409, # nutrition related variables
         sugar_water = v409a,
         juice = v410,
         milk = v411, 
         formula = v411a,
         fortified_food = v412a,
         soup_broth = v412c,
         grain_foods = v414e,
         tuber_foods = v414f,
         eggs = v414g,
         meat = v414h,
         squash = v414i,
         leafy_vegetables = v414j,
         vitamin_a_fruits = v414k,
         other_fruits = v414l,
         organ_meat = v414m,
         fish = v414n,
         legumes = v414o,
         dairy = v414p,
         oral_hydration = v416,  # knowledge of medical practices
         current_marital = v501,
         number_unions = v503,
         living_with_partner = v504, # household arrangements (attention theory)
         other_wives = v505,
         wife_rank = v506,
         yo_first_cohabitation = v508,
         age_first_cohabitation = v511,
         desire_more_children = v605,
         ideal_num_children = v613,
         husband_desire_children = v621,
         unmet_contraceptive_need = v624,  # family planning/knowledge go together
         ideal_num_boys = v627,
         ideal_num_girls = v628,
         husbands_vocation = v704,
         working = v714, # figure out how to do years of education for the father
         husband_education = v715,
         vocation = v716,
         employer = v719, # fits into the Profs. Jakiela & Ozier paper, self-employment happier
         healthcare_decisionmaker = v743a,
         purchase_decisionmaker = v743b, # big purchases
         money_decisionmaker = v743f,
         beating_leaving = v744a, 
         beating_neglect = v744b, # this seems related to child development in two ways
         beating_argument = v744c,
         beating_refusesex = v744d,
         beating_burnedfood = v744e,
         biggestearner = v746, # plausibly if the woman makes more..
         breastfeeding_duration = m4,
         birth_size = m18,
         birth_weight = m19,
         iron_supplement = m45,
         tuberculosis_vaccine = h2,
         dtap_vaccine = h3,
         polio_vaccine = h4,
         homeowner = v745a,
         landowner = v745b,
         child_wanted = v367,
         pob = m15
  )

## drop observations with missing height-for-age z-scores
ethiopia.data$haz <- as.numeric(ethiopia.data$haz)
ethiopia.data <- filter(ethiopia.data, !is.na(haz) & haz!=9996 & haz!=9997 & haz!= 9998)

## idiosyncratic cleaning

## change age at time of survey to age at birth
ethiopia.data$mom_age <- ethiopia.data$mom_age - (2014 - ethiopia.data$yob)

## twin dummy
ethiopia.data$twin = ifelse(ethiopia.data$b0 == 0, 0, 1)
ethiopia.data  <- select(ethiopia.data, !b0)

## birth interval cannot be NAN (first births), replace with median
ethiopia.data$interval[is.na(ethiopia.data$interval)] <- median(ethiopia.data$interval, na.rm=TRUE)

# make dummies for factor variables:
factor_cols_ethiopia <- c("mob",
                 "yob", 
                 "sex",
                 "bord",
                 "region",
                 "urban_rural",
                 "water_source",
                 "sanitation",
                 "electricity",
                 "radio",
                 "television",
                 "refrigerator",
                 "motorcycle",
                 "car_truck",
                 "ethnicity",
                 "telephone",
                 "reading_newspaper",
                 "listening_radio",
                 "household_head_sex",
                 "toilet_shared",
                 "fuel",
                 "wealth_index",
                 "homeowner",
                 "landowner",
                 "child_wanted",
                 "pob",
                 "twin",
                 "mosquito_net",
                 "familyplanning_radio",
                 "familyplanning_tv",
                 "familyplanning_newsp",
                 "familyplanning_visit",
                 "healthfacility_visit",
                 "currently_pregnant",
                 "currently_breastfeeding",
                 "plain_water",
                 "sugar_water",
                 "juice",
                 "milk",
                 "formula",
                 "fortified_food",
                 "soup_broth",
                 "grain_foods",
                 "tuber_foods",
                 "eggs",
                 "meat",
                 "squash",
                 "leafy_vegetables",
                 "vitamin_a_fruits",
                 "other_fruits",
                 "organ_meat",
                 "fish",
                 "legumes",
                 "dairy",
                 "oral_hydration",
                 "current_marital",
                 "living_with_partner",
                 "yo_first_cohabitation",
                 "husband_desire_children",
                 "unmet_contraceptive_need",
                 "husbands_vocation",
                 "working",
                 "vocation",
                 "employer",
                 "healthcare_decisionmaker",
                 "purchase_decisionmaker",
                 "money_decisionmaker",
                 "beating_leaving",
                 "beating_neglect",
                 "beating_argument",
                 "beating_refusesex",
                 "beating_burnedfood",
                 "biggestearner",
                 "breastfeeding_duration",
                 "birth_size",
                 "iron_supplement",
                 "tuberculosis_vaccine",
                 "dtap_vaccine",
                 "polio_vaccine"
)

ethiopia.data[factor_cols_ethiopia] <- lapply(ethiopia.data[factor_cols_ethiopia], as.character)
ethiopia.data <- dummy_cols(ethiopia.data, 
                            select_columns = factor_cols_ethiopia, 
                            remove_selected_columns = TRUE)
ethiopia.data[is.na(ethiopia.data)] <- 0


##### PREPARE DATA --------------------------------------

Y.ethiopia = ethiopia.data$haz

X.ethiopia <- ethiopia.data %>% # This is the model... i suppose.
  select(!c(haz)) %>% ### select the variables selected by the lasso-data.driven
  select(any_of(lasso_dd_vars)) %>% #### but... what do we do when regions aren't the same!
  select(!c(region_1, region_10, ethnicity_6)) %>% ##### now add a
  as.matrix() # still need to clean this heavily

X.ethiopia <- ethiopia.data %>%
  select(!c(haz, age_months)) %>%                                # Remove the 'haz' variable
  select(any_of(lasso_dd_vars)) %>%                  # Select variables chosen by lasso
  select(!c(region_1, region_10, ethnicity_6)) %>%   # Remove specific regions and ethnicity
  cbind(ethiopia.data %>% 
          select((c(region_3, region_5, region_7, ethnicity_5, ethnicity_19, ethnicity_28)))) %>%  # Add specified variables from ethiopia.data
  as.matrix()  


X.ethiopia.nomods <- ethiopia.data %>%
  select(!c(haz)) %>%
  as.matrix()

ridge_top_decile <- ridge_min_coefficients %>%
  filter(abs(coefficients) >= top_decile_threshold) %>%
  pull(variables) %>% tibble(Variable = .)

ridge_top_decile <- ridge_top_decile$Variable  # Convert to a character vector if it is a data frame

# Select columns in ethiopia.data based on ridge_top_decile and exclude those starting with "region_" or "ethnicity_"
X.ethiopia.ridge <- ethiopia.data %>%
  select(-haz) %>%                                # Exclude the 'haz' column
  select(any_of(ridge_top_decile)) %>%            # Select only columns in ridge_top_decile
  select(-matches("^(region_|ethnicity_)")) %>%   # Exclude columns starting with "region_" or "ethnicity_"
  cbind(ethiopia.data %>% 
          select((c(region_3, region_5, region_7, ethnicity_5, ethnicity_19, ethnicity_28)))) %>% 
  as.matrix()   


X.ethiopia.lasso <- ethiopia.data %>% 
  select(!c(haz)) %>%
  select(any_of(lasso_cv_vars)) %>% ##### these are the variables chosen by lasso with lambda.min
  select(-matches("^(region_|ethnicity_)")) %>%
  cbind(ethiopia.data %>% 
          select((c(region_3, region_5, region_7, ethnicity_5, ethnicity_19, ethnicity_28)))) %>% 
  as.matrix()


ols.ethiopia <- lm(Y.ethiopia ~ X.ethiopia)
summary.ethiopia <- summary(ols.ethiopia)
p_values.ethiopia <- coef(summary.ethiopia)[, "Pr(>|t|)"]
p_value_95.ethiopia <- p_values.ethiopia <= 0.05 
significant_var.ethiopia <- rownames(coef(summary.ethiopia))[p_value_95.ethiopia] %>%
  print() ## which variables are statistically significant predictors of literacy (95% level)?
coefficients.ethiopia <- coef(summary.ethiopia)[, "Estimate"]
min_p_value.ethiopia <- which.min(p_values.ethiopia)
lowest_p_value.ethiopia <- p_values.ethiopia[min_p_value.ethiopia]
lowest_p_value_var.ethiopia <- rownames(coef(summary.ethiopia))[min_p_value.ethiopia] %>%
  print() ## which variable has the lowest p-value? 
lowest_p_value_coeff.ethiopia <- coefficients.ethiopia[min_p_value.ethiopia] %>% 
  print() ## what is the OLS coefficient associated with that variable? 

ols_df.ethiopia <- ols.ethiopia %>%
  tidy() %>%
  as.data.frame()

ethiopia.predictions <- predict(ols.ethiopia)
ethiopia.ols.mse <- mean((Y.ethiopia - ethiopia.predictions)^2)
ethiopia.ols.mse


##### Final Model--------------------------------------

###### DOING RIDGE.MIN --------------------------------
grid.ridge.ethiopia <- 10^seq(5, 1, length = 300) # this is optimal for visualizing the ridge cv
set.seed(8675309)

ridge.ethiopia <- cv.glmnet(X.ethiopia.ridge, Y.ethiopia, alpha = 0, lambda = grid.ridge.ethiopia)
plot(ridge.ethiopia)

min.ridge.ethiopia.mse <- min(ridge.ethiopia$cvm)
min.ridge.ethiopia.mse

ridge.ethiopia.min <- predict(ridge.ethiopia, type = "coefficients", s = ridge.ethiopia$lambda.min)
ridge.ethiopia.min


ridge.ethiopia.test <- cv.glmnet(X.ethiopia.nomods, Y.ethiopia, alpha = 0, lambda = grid.ridge.ethiopia)
plot(ridge.ethiopia.test)
ridge.ethiopia.nomodel <- predict(ridge.ethiopia.test, type = "coefficients", s = ridge.ethiopia.test$lambda.min)
ridge.nomod.ethiopia.mse <- min(ridge.ethiopia.test$cvm)




##### DOING LASSO.MIN---------------------------------
grid.lasso.ethiopia <- 10^seq(2, -1, length = 300)
set.seed(8675398)

# first doing it with the variables chosen by the model (lasso_cv_vars)
lasso.ethiopia <- cv.glmnet(X.ethiopia.lasso, Y.ethiopia, alpha = 1, lambda = grid.lasso.ethiopia)
plot(lasso.ethiopia)

min.lasso.ethiopia.mse <- min(lasso.ethiopia$cvm)
min.lasso.ethiopia.mse

lasso.ethiopia.min <- predict(lasso.ethiopia, type = "coefficients", s = lasso.ethiopia$lambda.min) %>%
  as.matrix()
lasso.ethiopia.min

lasso.ethiopia.vars <- rownames(lasso.ethiopia.min)[lasso.ethiopia.min != 0 &
                                          rownames(lasso.ethiopia.min) != "(Intercept)"]
lasso.ethiopia.vars
lasso.ethiopia.coef <- as.matrix(coef(lasso.ethiopia))

lasso.ethiopia.dropped <- rownames(lasso.ethiopia.coef)[!rownames(lasso.ethiopia.coef) %in% lasso.ethiopia.vars]
lasso.ethiopia.dropped

# now doing it without the model, just applying it straight up!

lasso.ethiopia.nomodel <- cv.glmnet(X.ethiopia.nomods, Y.ethiopia, alpha = 1, lambda = grid.lasso.ethiopia)
plot(lasso.ethiopia.nomodel)
lasso.ethiopia.model.coef <- predict(lasso.ethiopia.nomodel, type = "coefficients", s = lasso.ethiopia.nomodel$lambda.min) %>%
  as.matrix()
lasso.nomod.ethiopia.mse <- min(lasso.ethiopia.nomodel$cvm)

lasso.ethiopia.nomodel.vars <- rownames(lasso.ethiopia.model.coef)[lasso.ethiopia.model.coef != 0 &
                                                                     rownames(lasso.ethiopia.model.coef) != "(Intercept)"]
lasso.ethiopia.nomodel.vars

###### DOING LASSO WITH DATA-DRIVEN PROCESS-----------------------
set.seed(8675309)
lasso_ddf <- rlasso(Y.ethiopia ~ X.ethiopia, post = FALSE)
summary(lasso_ddf)
lasso_ddf_coeff <- as.matrix(coef(lasso_ddf))
lasso_ddf_vars <- rownames(lasso_ddf_coeff)[lasso_ddf_coeff != 0 &
                                            rownames(lasso_ddf_coeff) != "(Intercept)"]
lasso_ddf_vars
lasso_ddf_vars_dropped <- rownames(lasso_ddf_coeff)[!rownames(lasso_ddf_coeff) %in% lasso_ddf_vars]
lasso_ddf_vars_dropped

lasso_ddf_predict <- predict(lasso_ddf)
lasso_ddf_mse <- mean((Y.ethiopia - lasso_ddf_predict)^2)
lasso_ddf_mse

# now doing it without the model, just applying it straight up!

lasso.ddf.ethiopia.nomodel <- rlasso(Y.ethiopia ~ X.ethiopia.nomods, post = FALSE)
summary(lasso.ddf.ethiopia.nomodel)

lasso_ddf_predict.nomodel <- predict(lasso.ddf.ethiopia.nomodel)
lasso_ddf_mse.nomodel <- mean((Y.ethiopia - lasso_ddf_predict.nomodel)^2)
lasso_ddf_mse.nomodel


common_vars <- intersect(lasso_dd_vars, lasso_ddf_vars)
common_vars_lassomin <- intersect(lasso_cv_vars, lasso.ethiopia.vars)

max_length <- max(length(lasso_dd_vars), length(lasso_cv_vars), length(ridge_top_decile))

# Function to extend a vector to max_length with NAs if needed
pad_with_na <- function(vec, max_length) {
  length(vec) <- max_length
  return(vec)
}

# Pad each vector with NA to ensure they have the same length
comparison_table <- tibble(
  Senegal.Lasso.DD.Variables = pad_with_na(lasso_dd_vars, max_length),
  Ethiopia.Lasso.DD.Variables = pad_with_na(ifelse(lasso_dd_vars %in% common_vars, lasso_dd_vars, NA), max_length),
  Dropped.DD.Variables = pad_with_na(ifelse(lasso_dd_vars %in% lasso_ddf_vars_dropped, lasso_dd_vars, NA), max_length),
  Senegal.Lasso.Min.Variables = pad_with_na(lasso_cv_vars, max_length),
  Ethiopia.Lasso.Min.Variables = pad_with_na(ifelse(lasso_cv_vars %in% common_vars_lassomin, lasso_cv_vars, NA), max_length),
  Dropped.Lasso.Min.Variables = pad_with_na(ifelse(lasso_cv_vars %in% lasso.ethiopia.dropped, lasso_cv_vars, NA), max_length),
  Ridge.TopDecile.Variables = pad_with_na(ridge_top_decile, max_length)
)

table_plot <- tableGrob(comparison_table, theme = ttheme_default(base_size = 16))
ggsave("data_frame_table.png", plot = table_plot, width = 24, height = 50, dpi = 300, limitsize = FALSE)

model_vars <- c(comparison_table$Senegal.Lasso.DD.Variables,
                comparison_table$Ethiopia.Lasso.DD.Variables,
                comparison_table$Dropped.DD.Variables,
                comparison_table$Senegal.Lasso.Min.Variables,
                comparison_table$Ethiopia.Lasso.Min.Variables,
                comparison_table$Dropped.Lasso.Min.Variables,
                comparison_table$Ridge.TopDecile.Variables)

vars_df <- comparison_table %>%
  pivot_longer(
    cols = everything(),
    names_to = "Model",
    values_to = "Variable"
  ) %>%
  drop_na(Variable)  # Remove NA values to keep only actual variables

frequency_table <- vars_df %>%
  count(Model, Variable) %>%
  pivot_wider(names_from = Model, values_from = n, values_fill = 0)

observation_frequency <- vars_df %>%
  count(Variable, name = "Frequency") %>%
  arrange(desc(Frequency))

frequency_table
observation_frequency




# collecting the mse values together:

collected_variables.final <- c(Ridge.Minimum = ridge_cv_vars_min,
                         Ridge.1se = ridge_cv_vars_1se,
                         Lasso.Minimum = lasso_cv_vars,
                         Lasso.1se = lasso_cv_vars_1se,
                         Lasso.DataDriven = lasso_dd_vars,
                         Random.Forest = rf_vars,
                         Gradient.Boosted.Forest = gbf_vars,
                         Model.Ridge = ridge_top_decile,
                         Model.Lasso.Min = lasso.ethiopia.vars,
                         Model.Lasso.DataDriven = lasso_ddf_vars)

collected_variables_names.final <- list(
  Senegal.Ridge.Minimum = ridge_cv_vars_min,
  Senegal.Ridge.1se = ridge_cv_vars_1se,
  Senegal.Lasso.Minimum = lasso_cv_vars,
  Senegal.Lasso.1se = lasso_cv_vars_1se,
  Senegal.Lasso.DataDriven = lasso_dd_vars,
  Senegal.Random.Forest = rf_vars,
  Senegal.Gradient.Boosted.Forest = gbf_vars,
  Ethiopia.Model.Ridge = ridge_top_decile,
  Ethiopia.Model.Lasso.Min = lasso.ethiopia.vars,
  Ethiopia.Model.Lasso.DataDriven = lasso_ddf_vars,
  Ethiopia.No.Model.Ridge = ridge.nomod.ethiopia.mse,
  Ethiopia.No.Model.Lasso.Min = lasso.nomod.ethiopia.mse,
  Ethiopia.No.Model.Lasso.DataDriven = lasso_ddf_mse.nomodel
)

collected_MSE.final <- list(ridge_mse_min,
                      ridge_mse_1se,
                      lasso_mse_min,
                      lasso_mse_1se,
                      lasso_dd_mse,
                      rf_mse,
                      boost_mse,
                      min.ridge.ethiopia.mse,
                      min.lasso.ethiopia.mse,
                      lasso_ddf_mse,
                      ridge.nomod.ethiopia.mse,
                      lasso.nomod.ethiopia.mse,
                      lasso_ddf_mse.nomodel
)

mse_comparison.final <- tibble(Method = names(collected_variables_names.final),
                         MSE = collected_MSE.final) 

mse_comparison.final$MSE <- as.numeric(mse_comparison.final$MSE)

mse_comparison.final

table_html <- mse_comparison.final %>%
  kable("html", table.attr = "style='width:auto; border-collapse: collapse;'") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE)

# Save as HTML or Word
save_kable(table_html, "mse_comparison.html")

# Optionally, convert HTML to a PNG image
webshot::webshot("mse_comparison.html", "mse_comparison.png")
