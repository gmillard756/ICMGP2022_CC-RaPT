library(dplyr)
library(tidyr)

####load, clean and match the MusselWatch data to prep for modeling
MW <- readRDS('data/MusselWatch.Rdata')

#take 5 minutes toexplore the Mussel watch dataset on your own
str(MW)
names(MW)
summary(MW)
head(MW)

#dplyr manipulations with piping (select, filter, group_by, summarize, mutate, arrange)
MW %>% group_by(Parameter) %>% 
  tally() %>% arrange(desc(n)) %>% View()

MW_pivot <- MW %>% 
  filter(Parameter %in% c('Mercury', 'Selenium')) %>% 
  distinct() %>%
  select(NST_Sample_ID, NST_Site, Fiscal_Year, Scientific_Name, Parameter, Result, Unit) %>%
  pivot_wider(id_cols = c('NST_Sample_ID', 'NST_Site', 'Scientific_Name', 'Fiscal_Year'), 
              names_from = Parameter, 
              values_from = Result) %>%
  filter(complete.cases(.)) %>% 
  mutate(Hg_0.3_greater = ifelse(Mercury >= 0.3, TRUE, FALSE)) %>%
  data.frame()

head(MW_pivot)

#Train a random forest model
library(ranger)
set.seed(2022)

#threshold for mercury concentration 0.3 mcg/g
#predictive features:
#  -selenium
#  -NST site
#  -species or matrix

MW_rf <- ranger(Hg_0.3_greater ~ Selenium + Scientific_Name + NST_Site,
                    data = MW_pivot,
                    probability = TRUE,
                    classification = TRUE)


# basic DALEX
library(DALEX)
explain_rf <- explain(model = MW_rf,  
                      data = MW_pivot[c(2,3,5)],
                      y = MW_pivot$Hg_0.3_greater == TRUE, 
                      label = "Random Forest")
# explainer objects
explain_rf$model        # model
explain_rf$model_info   # version of model package
explain_rf$data         # data
explain_rf$predict_function     # predict
explain_rf$y_hat       # predictions
explain_rf$residuals   # residuals
explain_rf$label       # model label


model_performance(explain_rf)

#Exercise: use the help documentation to create a quantile regression forest on Mercury using the features Selenium, Scientific Name, and NST_Site
MW_rf2 <- ranger(Mercury ~ Selenium + Scientific_Name + NST_Site,
                 data = MW_pivot,
                 quantreg = TRUE)

explain_rf2 <- explain(model = MW_rf2,  
                      data = MW_pivot[c(2,3,5)],
                      y = MW_pivot$Mercury, 
                      label = "Regression Forest")
model_performance(explain_rf2)



#shapley values
head(MW_pivot)
MW_shap <- predict_parts_shap(MW_ex, new_observation = head(MW_pivot,1))
plot(MW_shap)

#multiple linear regression
MW_lm <- lm(Mercury ~ Selenium + Scientific_Name + NST_Site,
                data = MW_pivot)
MW_lm_ex <- explain(MW_lm,
                 data = MW_pivot[1:5],
                 y = MW_pivot$Mercury, 
                 label = "Linear Regression")

MW_shap2 <- predict_parts_shap(MW_lm_ex, new_observation = head(MW_pivot,1))
plot(MW_shap2)


