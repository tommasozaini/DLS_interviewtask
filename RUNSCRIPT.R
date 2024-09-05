install.packages("here")
library(here) # for easy and clear relative paths

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set work directory to this file
here::i_am("TaskReadme.txt")

source("utils.R") # common utility functions for this repository
source("calculator_utils.R") # specific model utility functions


#' WRITE YOUR CODE BELOW

###### TASK 1 #######

#----- 1.1) Load the 3 datasets [7 code lines] ------ 

data.exercise <- readRDS('Data/projected_dle-total-and-sectoral-scaled_SHAPE-final.RData') %>%
  left_join( readRDS('Data/projected_energy_inequality_SHAPE-final.RData') %>% select(-scenario.mapping) ) %>% #the scenario mapping in the inequality dataset must be removed to let left_join work properly (anyway all SDPs map into SSP1)
  left_join( readRDS('Data/scenario_FE_SHAPE-final.RData') ) %>%
  drop_na(energy.per.capita, energy.gini, dle.threshold.curtech, dle.threshold.adjusted) %>% #keep only rows where I have data for all 3 datasets
  arrange(model, scenario, iso, variable, unit, year) #reorder data

data.exercise <- data.exercise[,c('model','scenario','iso','variable','unit','year','scenario.mapping','pop_mil','energy.per.capita','res.ratio','res.tradbio.ratio',
                              'energy.gini','gini.to.gini.elasticities.by.country.down','gini.to.gini.elasticities.by.country.up','gini','dle.tech.scaler','dle.threshold.curtech','dle.threshold.adjusted')] #reorder columns



# data_dle <- readRDS('Data/projected_dle-total-and-sectoral-scaled_SHAPE-final.RData')
# data_inequality <- readRDS('Data/projected_energy_inequality_SHAPE-final.RData')
# data_finalenergy <- readRDS('Data/scenario_FE_SHAPE-final.RData')

# expected_output <- readRDS('Data/EXAMPLE_EXPECTED_OUTPUT/RESULTS-DLE-emulator-SHAPE-final.RData')

#Preliminary analysis on data
# unique_dle <- list(
#   model = unique(data_dle$model),
#   scenario = unique(data_dle$scenario),
#   iso = unique(data_dle$iso),
#   variable = unique(data_dle$variable),
#   year = unique(data_dle$year)
# )
# 
# unique_inequality <- list(
#   model=unique(data_inequality$model),
#   scenario=unique(data_inequality$scenario),
#   iso = unique(data_inequality$iso),
#   variable = unique(data_inequality$variable),
#   year = unique(data_inequality$year)
# )
# 
# unique_finalenergy <- list(
#   model=unique(data_finalenergy$model),
#   scenario=unique(data_finalenergy$scenario),
#   iso = unique(data_finalenergy$iso),
#   variable = unique(data_finalenergy$variable),
#   year = unique(data_finalenergy$year)
# )


#------- 1.2) Join the data tables, keeping only rows where I have data for all 3 datasets [6 code lines] ------ 

# data_inequality$scenario.mapping <- 'SSP1'  #all the SDP scenarios (EI,MC,RC) map into SSP1
# 
# dle_and_inequality <- inner_join(data_dle, data_inequality, by=c('model', 'scenario', 'iso', 'variable', 'year'))
# joined_data <- inner_join(dle_and_inequality, data_finalenergy, by=c('model', 'scenario', 'iso', 'variable', 'year','scenario.mapping'))
# 
# joined_data <- joined_data[,c('model','scenario','iso','variable','unit','year','scenario.mapping','pop_mil','energy.per.capita','res.ratio','res.tradbio.ratio',
#                               'energy.gini','gini.to.gini.elasticities.by.country.down','gini.to.gini.elasticities.by.country.up','gini','dle.tech.scaler','dle.threshold.curtech','dle.threshold.adjusted')] #reorder columns
# 
# joined_data <- joined_data[order(joined_data$model, joined_data$scenario, joined_data$iso, joined_data$variable, joined_data$year), ]

#------- 1.3) Calculate share of population below dle and avg intensity of poverty of this population, using lognormal distributions of energy consumption within-country [8 code lines] ------ 

data.calculated <- data.exercise %>%
  mutate(sig = GetInverseCDF_lognormal(energy.gini)) %>%
  mutate(nu = log(energy.per.capita) - (sig^2) / 2) %>%
  mutate(share.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.curtech, "share")) %>%
  mutate(depth.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.curtech, "DoD")) %>%
  mutate(share.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.adjusted, "share")) %>%
  mutate(depth.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.adjusted, "DoD")) %>%
  select(-sig, -nu)


#------- 1.4) CODE TESTS ------

install.packages('testthat') #move on top

library(testthat)

#First Unit test: check GetInverseCDF_lognormal function

n_extractions = 200 #how many random extractions to perform the test?

test_that("GetInverseCDF_lognormal returns correct sigma", {
  
  energy.gini.test.values <- sample(data.calculated$energy.gini, n_extractions) # random energy.gini values ectracted from data.calculated
  
  for(energy.gini.test in energy.gini.test.values) {
    expected.sigma = sqrt(2) * qnorm((energy.gini.test + 1) / 2) #instead of using invcdf(), I use qnorm(), which computes the quantile for the standard normal distribution (nu=0,sd=1), for a probability=(en.gini+1)/2
    sigma.to.test = GetInverseCDF_lognormal(energy.gini.test)
    expect_equal(sigma.to.test, expected.sigma, tolerance = 1e-6)   # Test if the result is close to the expected value
  }
})


#TODO:

#TEST GetDepthofDeficit_lognormal

#INTEGRATION TEST

#PERFORMANCE TEST (about code efficiency)



##### TASK 2 #####

#---- 2.1) Calculate energy needs gap ------

#Energy needs gap = deprivation.headcount * depth.of.deficit = total population * share.population.below.threshold * depth.of.deficit

data.calculated <- data.calculated %>%
  mutate(deprivation.headcount.curtech_mil = share.below.projected.curtech*pop_mil) %>%       #deprivation.headcount.curtech_mil = population (millions) below threshold, with current technologies
  mutate(deprivation.headcount.adjusted_mil = share.below.projected.adjusted) %>%             #deprivation.headcount.adjusted_mil = population (millions) below threshold, with technological evolution
  mutate(energy.needs.gap.curtech = deprivation.headcount.curtech_mil * 10^-3 * depth.below.projected.curtech) %>%       #energy.needs.gap.curtech = energy needs gap [EJ/yr], with current technologies
  mutate(energy.needs.gap.adjusted = deprivation.headcount.adjusted_mil * 10^-3 * depth.below.projected.adjusted)        #energy.needs.gap.adjusted = energy needs gap [EJ/yr], with technological evolution

#Remove duplicates (projections from model IMAGE 3.3 have a few duplicates)
data.calculated <- data.calculated %>%
  group_by(model, scenario, variable, iso, year) %>%
  arrange(desc(!is.na(res.ratio)),desc(!is.na(res.tradbio.ratio))) %>% #between the two duplicates, I keep only the one that has values for res.ratio and res.tradbio.ratio
  slice(1) %>%
  ungroup()

data.global <- data.calculated %>%
  group_by(model, scenario, variable, year) %>%
  summarise( pop_mil = sum(pop_mil, na.rm=TRUE),
             deprivation.headcount.curtech_mil = sum(deprivation.headcount.curtech_mil, na.rm=TRUE), 
             deprivation.headcount.adjusted_mil = sum(deprivation.headcount.adjusted_mil, na.rm=TRUE),
             energy.needs.gap.curtech = sum(energy.needs.gap.curtech, na.rm=TRUE),
             energy.needs.gap.adjusted = sum(energy.needs.gap.adjusted, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(region='global') %>%
  select(model,scenario, region, variable, everything())

data.four.nations <- data.calculated %>%
  filter(iso %in% c('CHN','IND','NGA','USA'))

data.seven.nations <- data.calculated %>%
  filter(iso %in% c('BRA','CHN','DEU','IND','NGA','USA','ZAF'))

#--------- CHECKS --------

# NB CHECK DUPLICATES! There seem to be some almost duplicate rows, differing only for energy.per.capita. example: IMAGE, RC-1.5, India, residential, 1040

# check.duplicates <- data.calculated %>%
#   group_by(model, scenario, variable, iso, year) %>%
#   filter(n()>1) %>%
#   ungroup()


# Plot one nation
country = 'NGA'
max_year = 2100
variable = 'dle.threshold.adjusted'

check.data.country <- data.calculated %>%
  filter(iso==country) %>%
  filter(year<=max_year)

ggplot(check.data.country, aes(x = year, y = dle.threshold.adjusted, color = scenario, linetype = model)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~variable, scales = "free_y") + # Separate plots by variable
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  labs(title = paste(country, variable),
       x = "Year",
       y = "(EJ/year)",
       color = "Scenario",
       linetype = "Model") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1))


#Plot many countries

data.seven.nations <- data.seven.nations %>%
  mutate( variable = str_replace(variable, 'Final Energy\\|', ''),
    model = str_replace(model, 'IMAGE 3.3', 'IMAGE'),
    model = str_replace(model, 'REMIND-MAgPIE 3.0-4.4', 'REMIND') )

max_year = 2040
variable = 'dle.tech.scaler'

plot <-
  ggplot(data.seven.nations %>% filter(year<=max_year), aes(x = year, y = dle.tech.scaler, color = scenario, linetype = model)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_grid(variable~iso, scales = "free_y") + # Separate plots by variable
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  labs(title = paste('7 countries -', variable),
       x = "",
       y = "",
       color = "Scenario",
       linetype = "Model") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1))
plot

ggsave(paste('7 countries ',variable,' ',max_year,'.png',sep=''), plot=plot, path='/Users/tommasozaini/Documents/IIASA_DLS/InterviewTask/checks', dpi=400,  width=10, height=7)



#Find ANOMALIES

#Find all the data for which dle.threshold.adjusted is > 50
check.anomalies.dle.threshold <- data.calculated %>%
  filter(dle.threshold.adjusted > 50)

# #Check the global sum of energy needs
# example.df <- data.calculated %>%
#   filter(year==2040, model=='IMAGE 3.3', scenario=='SDP_RC-1p5C', variable=='Final Energy|Residential and Commercial') 
#   
# check.sum=sum(example.df$energy.needs.gap.adjusted)



#----- 2.2) Visualize energy needs gap ----

install.packages('RColorBrewer')
library(ggplot2)
library(scales)
library(RColorBrewer)

#----- Useful functions to make data easier to visualise ----

source('functions_visualization.R')

check.anomalies.dle.threshold$iso <- iso_to_country(check.anomalies.dle.threshold$iso)

#------ Plots ------

#Rearrange data to make them more suitable for visualization
data.global.plot = data.global
data.global.plot$variable = clean_variable_name(data.global.plot$variable)
data.global.plot$model = clean_model_name(data.global.plot$model)


data.global.plot.till2040 = data.global.plot %>% filter(year<=2040)

#Initial plot to check if results are sensible
# Plot global energy needs gap over time for adjusted DLE threshold
ggplot(data.global.plot, aes(x = year, y = global.energy.needs.gap.curtech, color = scenario, linetype = model)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~variable, scales = "free_y") + # Separate plots by variable
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  labs(title = "Global Energy Needs Gap Curtech",
       x = "Year",
       y = "Energy Needs Gap (EJ/year)",
       color = "Scenario",
       linetype = "Model") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1))


#PLOT 1: Global energy needs gap in different scenarios

#1 scenario
years.plot=c(2020, 2030, 2040)
scenario.plot='SDP_RC-1p5C'

data.plot.1 <- data.global.plot %>% 
  filter(year %in% years.plot) %>%
  filter(scenario==scenario.plot)

# plot1 <-
  ggplot(data.plot.1, aes(x=factor(year), y=global.energy.needs.gap.adjusted, fill=variable)) +
  geom_bar(stat='identity', position='stack') + 
    facet_wrap(~model, nrow=2) +
    labs(title=paste('Global Energy Needs Gap in 2020, 2030, 2040 - ',scenario.plot),
         x='',
         y='EJ/year',
         fill='Sector'
         ) +
    theme_minimal() +
    theme(legend.position='bottom',
          panel.grid.major.x=element_blank()) +
    scale_fill_brewer(palette='Set2')


  #multiple scenarios
  
  years.plot=c(2020, 2030, 2040)
  
  data.plot.2 <- data.global.plot %>% 
    filter(year %in% years.plot) #%>%
    # filter(scenario==scenario.plot)
  
# plot1 <-
  ggplot(data.plot.2, aes(x=factor(year), y=global.energy.needs.gap.adjusted, fill=variable)) +
    geom_bar(stat='identity', position='stack') + 
    facet_grid(model ~ scenario) +
    labs(title='Global Energy Needs Gap in 2020, 2030, 2040 - Multiple scenarios',
         x='',
         y='EJ/year',
         fill='Sector'
    ) +
    theme_minimal() +
    theme(legend.position='bottom',
          panel.grid.major.x = element_blank(),
          legend.title = element_blank()) 


