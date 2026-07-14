library(dplyr)
library(tidyr)


#inputs
A23.elecS_subsector_logit <- read.csv('Data/inputs/A23.elecS_subsector_logit.csv')

#outputs
outputs <- c('costs_by_tech.csv','hydrogen_costs.csv','prices_by_sector.csv', 'costs_subsector.csv', 'elec_gen_costs.csv','nonCO2_emissions.csv')


hydrogen_costs <- read.csv('Data/hydrogen_costs.csv') %>% 
  select(-scenario, -X1990, -X2005, -X2010, -X2015, -X2020, -X2035, -X2050, -X2065, -X2080, -X2095, -Units) %>% 
  rename(value_chY = X2021)

hydrogen_costs_i <- hydrogen_costs %>%
  filter(region == 'Albania', 
         sector == 'H2 central production', 
         subsector == 'biomass',
         technology == 'biomass to H2') %>%
  select(iteration, value_chY) 

A23.elecS_subsector_logit_biomass <- A23.elecS_subsector_logit %>%
  filter(subsector == 'gas') %>%
  select(iteration, logit.exponent)

df_merge <- hydrogen_costs_i %>% 
  inner_join(A23.elecS_subsector_logit_biomass, by = 'iteration') %>%
  slice(-23, -48) 

plot(df_merge$logit.exponent, df_merge$value_chY)


#Errors


hydrogen_errors <- read.csv('../3. Error structure/Data/hydrogen costs by tech.csv') %>%
  select(-X, -query, -value_chY, -error, -abs_error, -rel_error, -year)
value_ref <- hydrogen_errors %>%
  filter(region == 'Albania', 
         sector == 'H2 central production', 
         subsector == 'biomass',
         technology == 'biomass to H2') %>% pull(value_ref)

df_merge <- df_merge %>% 
  mutate(error = (value_ref - value_chY))


plot(df_merge$logit.exponent, df_merge$error)

## query's RMSE

hydrogen_costs_RMSE <- hydrogen_costs %>%
  left_join(hydrogen_errors, by = c('region', 'sector', 'subsector', 'technology')) %>% 
  drop_na() %>%
  mutate(error = value_ref - value_chY,
         error_abs = abs(error)) %>% 
  filter(!(iteration %in% c(23, 38, 48)))
  

plot(hydrogen_costs_RMSE$iteration, hydrogen_costs_RMSE$error_abs)


### regision

hydrogen_costs_RMSE$region <- factor(hydrogen_costs_RMSE$region)
hydrogen_costs_RMSE$sector <- factor(hydrogen_costs_RMSE$sector)
hydrogen_costs_RMSE$subsector <- factor(hydrogen_costs_RMSE$subsector)
hydrogen_costs_RMSE$technology <- factor(hydrogen_costs_RMSE$technology)

modelo <- lm(
  error_abs ~ technology,
  data = hydrogen_costs_RMSE
)

summary(modelo)
coefs <- summary(modelo)$coefficients
sig <- rownames(coefs)[coefs[, "Pr(>|t|)"] < 0.05]
tech_sig <- sub("^technology", "", sig)
tech_sig <- c("biomass to H2", tech_sig)

datos2 <- subset(
  hydrogen_costs_RMSE,
  technology %in% tech_sig
)
modelo2 <- lm(error_abs ~ technology, data = datos2)

summary(modelo2)

