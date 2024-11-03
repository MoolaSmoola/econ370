# Test
# change
# install.packages('gitcreds')
# install.packages("tidyverse")
# install.packages("haven") 
# install.packages("fastDummies")
# install.packages("tree")
# install.packages("randomForest")
# install.packages("gbm")
# install.packages("rpart")
# install.packages("rpart.plot")

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

#ridge & lasso
library(glmnet)
library(hdm)

######### INSERT PCA PACKAGE #######
#pca()

library(gitcreds)
# gitcreds_set()

mypath <- "~/ECON370/ECON370dhs/econ370 project/"


senegal.dhs <- read_dta(paste0(mypath, "SNBR7IDT/SNBR7IDT/SNBR7IFL.dta")) %>% 
  filter(!is.na(midx)) %>% 
  filter(b5 == 1) %>% # Child is alive
  filter(v135 == 1) # Usual resident or visitor of Senegal


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

##### DOING RIDGE WITH LAMBDA 1SE
set.seed(8675309)
ridge_cv_1se <- predict(ridge_cv, type = "coefficients", s = ridge_cv$lambda.1se) %>%
  as.matrix() 

ridge_cv_vars_1se <- rownames(ridge_cv_1se)[abs(ridge_cv_1se) > mean(abs(ridge_cv_1se)) &
                                              rownames(ridge_cv_1se) != "(Intercept)"]
ridge_cv_vars_1se

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

##### DOING LASSO WITH LAMBDA 1SE
set.seed(8675309)
lasso_cv_1se <- predict(lasso_cv, type = "coefficients", s = lasso_cv$lambda.1se) %>%
  as.matrix()
lasso_cv_vars_1se <- rownames(lasso_cv_1se)[lasso_cv_1se != 0 &
                                              rownames(lasso_cv_1se) != "(Intercept)"]
lasso_cv_vars_1se

##### DOING LASSO WITH DATA-DRIVEN PROCESS


##### RANDOM FOREST -------------------------------------

##### GRADIENT-BOOSTED FOREST ---------------------------

##### PRINCIPAL COMPONENT ANALYSIS  ---------------------






