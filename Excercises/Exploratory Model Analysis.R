library(dplyr)
library(tidyr)

####load, clean and match the MusselWatch data to prep for modeling
MW <- readRDS('data/MusselWatch.Rdata')

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
  data.frame()


#Train a random forest model
library(ranger)
set.seed(2022)

#threshold for mercury concentration 0.3 mcg/g
#predictive features:
#  -selenium
#  -NST site
#  -species or matrix

MW_rf <- ranger(Mercury ~ Selenium + Scientific_Name + NST_Site,
                    data = MW_pivot,
                    probability = FALSE,
                    classification = FALSE)

# basic use
library(DALEX)
MW_ex <- explain(MW_rf,
                 data = MW_pivot[1:5],
                 y = MW_pivot$Mercury, 
                 label = "Random Forest")

#shapley values
head(MW_pivot)
MW_shap <- predict_parts_shap(MW_ex, new_observation = head(MW_pivot,1))
plot(MW_shap)

#multiple linear regression
MW_lm <- lm(Mercury ~ Selenium + Scientific_Name + NST_Site + Fiscal_Year,
                data = MW_pivot)
MW_lm_ex <- explain(MW_lm,
                 data = MW_pivot[1:5],
                 y = MW_pivot$Mercury, 
                 label = "Linear Regression")

MW_shap2 <- predict_parts_shap(MW_lm_ex, new_observation = head(MW_pivot,1))
plot(MW_shap2)
