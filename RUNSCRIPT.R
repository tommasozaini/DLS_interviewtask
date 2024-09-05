install.packages("here")
library(here) # for easy and clear relative paths

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set work directory to this file
here::i_am("TaskReadme.txt")

source("utils.R") # common utility functions for this repository
source("calculator_utils.R") # specific model utility functions

additional.packages <- c(
  'testthat', #for unit test
  'profvis',  #for performance test, to visualize running time
  'furrr',    #for performance test, to parallelize operations
  'ggplot2',  #for plotting
  'scales',   #for plotting, to work on axis scales
  'RColorBrewer', #for plotting, for more color palettes
  'Hmisc'     #for plotting, to calculate weighted average and interquartile range
)
install.packages(additional.packages)
lapply(additional.packages, library, character.only=TRUE)

#' WRITE YOUR CODE BELOW

###### TASK 1 #######

#----- 1.1) Load and merge the 3 datasets [7 code lines] ------ 

data.exercise <- readRDS('Data/projected_dle-total-and-sectoral-scaled_SHAPE-final.RData') %>%
  left_join( readRDS('Data/projected_energy_inequality_SHAPE-final.RData') %>% select(-scenario.mapping) ) %>% #the scenario mapping in the inequality dataset must be removed to let left_join work properly (anyway all SDPs map into SSP1)
  left_join( readRDS('Data/scenario_FE_SHAPE-final.RData') ) %>%
  drop_na(energy.per.capita, energy.gini, dle.threshold.curtech, dle.threshold.adjusted) %>% #keep only rows where I have data for all 3 datasets
  arrange(model, scenario, iso, variable, unit, year) %>% #reorder data (rows)
  select(model, scenario, iso, variable, unit, year, scenario.mapping, pop_mil, energy.per.capita, res.ratio, res.tradbio.ratio, energy.gini, 
         gini.to.gini.elasticities.by.country.down, gini.to.gini.elasticities.by.country.up, gini, dle.tech.scaler, dle.threshold.curtech, dle.threshold.adjusted) #reorder columns


#------- 1.2) Calculate share of population below dle and avg intensity of poverty of this population, using lognormal distributions of energy consumption within-country [8 code lines] ------ 

data.calculated <- data.exercise %>%
  mutate(sig = GetInverseCDF_lognormal(energy.gini)) %>%
  mutate(nu = log(energy.per.capita) - (sig^2) / 2) %>%
  mutate(share.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.curtech, "share")) %>%
  mutate(depth.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.curtech, "DoD")) %>%
  mutate(share.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.adjusted, "share")) %>%
  mutate(depth.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.adjusted, "DoD")) %>%
  select(-sig, -nu)

#Export data
if (!file.exists(here('OUTPUT'))) { dir.create(here('OUTPUT'), recursive=TRUE) } # Create the subfolder if it doesn't exist
write.csv(data.calculated, here('OUTPUT','output_file.csv'), row.names=FALSE)


#------- 1.3) CODE TESTS ------

# DUPLICATE CHECK

#First check if there are any duplicates
check.duplicates <- data.calculated %>%
  group_by(model, scenario, variable, iso, year) %>%
  filter(n()>1) %>%
  ungroup()

#Remove the duplicates
if(nrow(check.duplicates)>0) {
  data.calculated <- data.calculated %>%
    group_by(model, scenario, variable, iso, year) %>%
    arrange(desc(!is.na(res.ratio)),desc(!is.na(res.tradbio.ratio))) %>% #between the two duplicates, I keep only the one that has values for res.ratio and res.tradbio.ratio
    slice(1) %>%
    ungroup()
}


#First Unit test: check GetInverseCDF_lognormal function

n_extractions = 200 #how many random extractions to perform the test?

test_that("GetInverseCDF_lognormal returns correct sigma", {
  
  energy.gini.test.values <- sample(data.calculated$energy.gini, n_extractions) # random energy.gini values extracted from data.calculated
  
  expected.sigma = sqrt(2) * qnorm((energy.gini.test.values + 1) / 2) #instead of using invcdf(), I use qnorm(), which computes the quantile for the standard normal distribution (nu=0,sd=1), for a probability=(en.gini+1)/2
  sigma.to.test = GetInverseCDF_lognormal(energy.gini.test.values)
  expect_equal(sigma.to.test, expected.sigma, tolerance = 1e-6)   # Test if the result is close to the expected value
  
})


#Another possible unit test: check GetDepthofDeficit_lognormal

#PERFORMANCE TEST (about code efficiency)

#Calculate the time of the code operation
system.time({
  data.calculated <- data.exercise %>%
    mutate(sig = GetInverseCDF_lognormal(energy.gini)) %>%
    mutate(nu = log(energy.per.capita) - (sig^2) / 2) %>%
    mutate(share.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.curtech, "share")) %>%
    mutate(depth.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.curtech, "DoD")) %>%
    mutate(share.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.adjusted, "share")) %>%
    mutate(depth.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.adjusted, "DoD"))
})

#Visualise the running time
profvis({
  data.calculated <- data.exercise %>%
    mutate(sig = GetInverseCDF_lognormal(energy.gini)) %>%
    mutate(nu = log(energy.per.capita) - (sig^2) / 2) %>%
    mutate(share.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.curtech, "share")) %>%
    mutate(depth.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.curtech, "DoD")) %>%
    mutate(share.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.adjusted, "share")) %>%
    mutate(depth.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.adjusted, "DoD"))
})
#As predictable, the functions that require more time are GetInverseCDF_lognormal (less) and GetDepthofDeficit_lognormal (more)

#A possible solution to speed up the code is parallelization

system.time({
  plan(multisession, workers = 4)  #parallelize into 4 cores
  
  chunks <- split(data.exercise, sort(rep(1:4, length.out = nrow(data.exercise))))   #split data into chunks
  
  results <- future_map(chunks, function(chunk) { #apply the calculations to each chunk in parallel
    chunk %>%
      mutate(sig = GetInverseCDF_lognormal(energy.gini)) %>%
      mutate(nu = log(energy.per.capita) - (sig^2) / 2) %>%
      mutate(share.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.curtech, "share")) %>%
      mutate(depth.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.curtech, "DoD")) %>%
      mutate(share.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.adjusted, "share")) %>%
      mutate(depth.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.adjusted, "DoD"))  })
  
  parallelized.data.calculated <- bind_rows(results)   #combine the results
})




########  TASK 2  #########


#---- 2.1) Calculate energy needs gap ------

#Energy needs gap = deprivation.headcount * depth.of.deficit = total population * share.population.below.threshold * depth.of.deficit

data.calculated <- data.calculated %>%
  mutate(deprivation.headcount.curtech_mil = share.below.projected.curtech*pop_mil) %>%       #deprivation.headcount.curtech_mil = population (millions) below threshold, with current technologies
  mutate(deprivation.headcount.adjusted_mil = share.below.projected.adjusted*pop_mil) %>%             #deprivation.headcount.adjusted_mil = population (millions) below threshold, with technological evolution
  mutate(energy.needs.gap.curtech = deprivation.headcount.curtech_mil * 10^-3 * depth.below.projected.curtech) %>%       #energy.needs.gap.curtech = energy needs gap [EJ/yr], with current technologies
  mutate(energy.needs.gap.adjusted = deprivation.headcount.adjusted_mil * 10^-3 * depth.below.projected.adjusted)        #energy.needs.gap.adjusted = energy needs gap [EJ/yr], with technological evolution


#Aggregate data at global level
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



#-------- 2.2) Visualise results ---------

#Useful functions to make data easier to visualise:
source('functions_visualization.R')


#Rearrange data to make them more suitable for visualization
data.global.plot <- data.global %>%
  mutate( variable = clean_variable_name(variable) ) %>%
  mutate( model = clean_model_name(model) ) %>%
  mutate( scenario = clean_scenario_name(scenario) )


# PLOT 1: Global energy needs gap over time

year_max = 2040
type_plot = 'curtech'

plot1 <-
  ggplot(data.global.plot %>% filter(year<=year_max), aes(x = year, y = energy.needs.gap.curtech, color = scenario, linetype = model)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~variable) + 
  labs(title = "Global Energy Needs Gap with Current Technologies",
       x = NULL,
       y = "EJ/year",
       color = "Scenario",
       linetype = "Model") +
  # ylim(0,max(data.global.plot$deprivation.headcount.adjusted_mil*10^-3)) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank())
plot1

#Save
plot_folder_path <- here('graphs') # Create the full path to the subfolder
if (!file.exists(plot_folder_path)) { dir.create(plot_folder_path, recursive=TRUE) } # Create the subfolder if it doesn't exist

# ggsave(paste('global gap ',type_plot,' ',year_max,'.png',sep=''), plot=plot, path=plot_folder_path, dpi=400,  width=12, height=6)


#Plot many countries

data.seven.nations <- data.calculated %>%
  filter(iso %in% c('BRA','CHN','DEU','IND','USA','ZAF'))

data.seven.nations <- data.seven.nations %>%
  mutate( variable = clean_variable_name(variable) ) %>%
  mutate( model = clean_model_name(model) ) %>%
  mutate( scenario = clean_scenario_name(scenario) )

max_year = 2040
variable = 'energy.needs.gap'

plot <-
  ggplot(data.seven.nations %>% filter(year<=max_year), aes(x = year, y = energy.needs.gap.adjusted, color = scenario, linetype = model)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_grid(variable~iso, scales = "free_y") + # Separate plots by variable
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  labs(title = 'Energy needs gap for major countries',
       x = NULL,
       y = 'EJ/year',
       color = "Scenario",
       linetype = "Model") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())
plot

ggsave(paste('6 countries ',variable,' ',max_year,'.png',sep=''), plot=plot, path=here('graphs'), dpi=400,  width=10, height=7)


#PLOT 2: Global energy needs gap in different scenarios

#1 scenario
years.plot=c(2020, 2030, 2040)
scenario.plot='RC 1p5C'

data.plot.1scen <- data.global.plot %>% 
  filter(year %in% years.plot) %>%
  filter(scenario==scenario.plot)

plot2 <-
  ggplot(data.plot.1scen, aes(x=factor(year), y=energy.needs.gap.adjusted, fill=variable)) +
  geom_bar(stat='identity', position='stack') + 
  facet_wrap(~model, nrow=2) +
  labs(title=paste('Global Energy Needs Gap in 2020, 2030, 2040 - ',scenario.plot),
       x='',
       y='EJ/year',
       fill='Sector'
  ) +
  theme_minimal() +
  theme(legend.position='bottom',
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)) +
  scale_fill_brewer(palette='Set2')
plot2

ggsave(paste('global gap 3sectors stacked adjusted.png',sep=''), plot=plot2, path=plot_folder_path, dpi=400,  width=8, height=6)



#Industry has strangely high values, so I remove it to make the results more visible

data.global.plot.noindustry <- data.global.plot %>%
  filter(variable != 'Industry')

#1 scenario
data.plot.1scen.noindustry <- data.global.plot.noindustry %>% 
  filter(year %in% years.plot) %>%
  filter(scenario==scenario.plot)

plot3 <-
  ggplot(data.plot.1scen.noindustry, aes(x=factor(year), y=energy.needs.gap.adjusted, fill=variable)) +
  geom_bar(stat='identity', position='stack') + 
  facet_wrap(~model, nrow=2) +
  labs(title=paste('Global Energy Needs Gap in 2020, 2030, 2040 - ',scenario.plot),
       x='',
       y='EJ/year',
       fill='Sector'
  ) +
  theme_minimal() +
  theme(legend.position='bottom',
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)) +
  scale_fill_manual(values = c("#FC8D62", "#8DA0CB"))
plot3


# ggsave(paste('global gap 2sectors stacked adjusted.png',sep=''), plot=plot3, path=plot_folder_path, dpi=400,  width=8, height=6)


#multiple scenarios

years.plot=c(2020, 2030, 2040)

data.plot.multiscen <- data.global.plot.noindustry %>% 
  filter(year %in% years.plot) %>%
  filter(grepl('1p5C',scenario))

plot4 <- 
  ggplot(data.plot.multiscen, aes(x=factor(scenario), y=energy.needs.gap.adjusted, fill=variable)) +
  geom_bar(stat='identity', position='stack') + 
  facet_grid(model ~ year) +
  labs(title='Global Energy Needs Gap in 2020, 2030, 2040 - Multiple scenarios',
       x=NULL,
       y='EJ/year',
       fill='Sector'
  ) +
  theme_minimal() +
  theme(legend.position='bottom',
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)) +
  scale_fill_manual(values = c("#FC8D62", "#8DA0CB")) 
plot4


ggsave(paste('global gap 2sectors stacked multiscen adjusted.png',sep=''), plot=plot4, path=plot_folder_path, dpi=400,  width=9, height=6)


#1 scenario, many regions

data.four.nations <- data.calculated %>%
  filter(iso %in% c('CHN','IND','NGA','USA'))


#Rearrange data to make them more suitable for visualization
data.four.nations.plot <- data.four.nations %>%
  mutate( variable = clean_variable_name(variable) ) %>%
  mutate( model = clean_model_name(model) ) %>%
  mutate( scenario = clean_scenario_name(scenario) ) %>%
  mutate( iso = iso_to_country(iso) ) %>%
  rename( country = iso )
  
data.four.nations.plot <- data.four.nations.plot %>% 
  filter(scenario=='RC 1p5C') %>%
  filter(year %in% c(2020,2030, 2040)) %>%
  filter(variable != 'Industry')

plot4 <- 
  ggplot(data.four.nations.plot, aes(x=factor(country), y=energy.needs.gap.adjusted, fill=variable)) +
  geom_bar(stat='identity', position='stack') + 
  facet_grid(model ~ year) +
  labs(title='Global Energy Needs Gap in 2020, 2030, 2040 across countries',
       x=NULL,
       y='EJ/year',
       fill='Sector'
  ) +
  theme_minimal() +
  theme(legend.position='bottom',
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)) +
  scale_fill_manual(values = c("#FC8D62", "#8DA0CB"))
plot4


ggsave(paste('4countries gap 2sectors stacked RC 1 adjusted.png',sep=''), plot=plot4, path=plot_folder_path, dpi=400,  width=9, height=6)



# PLOT 4: Share of population below DLET

#Benchmark years: 2020, 2030 and 2040

data.allcountries.plot <- data.calculated %>%
  mutate( variable = clean_variable_name(variable) ) %>%
  mutate( model = clean_model_name(model) ) %>%
  mutate( scenario = clean_scenario_name(scenario) ) %>%
  mutate( iso = iso_to_country(iso) ) %>%
  rename( country = iso )

data.sharepop.plot <- data.allcountries.plot %>%
  filter(year %in% c(2020,2030,2040)) %>% 
  filter(scenario == 'RC 1p5C') %>%
  filter(model=='IMAGE') 

# Calculate weighted summary statistics for each year and variable (sector)
weighted_summary <- data.sharepop.plot %>%
  group_by(year, variable) %>%
  summarise(
    weighted_median = wtd.quantile(share.below.projected.adjusted, weights = pop_mil, probs = 0.5, na.rm = TRUE),
    weighted_q1 = wtd.quantile(share.below.projected.adjusted, weights = pop_mil, probs = 0.25, na.rm = TRUE),
    weighted_q3 = wtd.quantile(share.below.projected.adjusted, weights = pop_mil, probs = 0.75, na.rm = TRUE)
  )


plot<-
  ggplot(data.sharepop.plot, aes(x = variable, y = share.below.projected.adjusted, size = pop_mil, color = variable)) +
  geom_point(alpha = 0.7) +  
  facet_wrap(~ year) +  
  scale_size_continuous(range = c(3, 15)) +  
  labs(title = "Share of Population Below Threshold - RC 1p5C scenario",
       x = NULL,
       y = "Share Below Threshold",
       size = "Population (Millions)",
       color = NULL) +
  theme_bw() +  
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank()) +
  scale_color_brewer(palette = 'Set2') +
  geom_pointrange(data = weighted_summary,     # Add the weighted median and IQR on top in black
                  aes(x = variable, y = weighted_median, ymin = weighted_q1, ymax = weighted_q3),
                  color = "black", inherit.aes = FALSE, size = 1, position = position_dodge(0.5))
plot

ggsave(paste('share pop below thres.png',sep=''), plot=plot, path=plot_folder_path, dpi=400,  width=12, height=6)


#OTHER POSSIBLE PLOTS (I didn't have the time to do that)

# PLOT: DLET and energy.per.capita over time (2020-2040), for 4/5 selected countries, highlighting energy headroom or energy deprivation

# PLOT: Cumulative energy needs gap 2020-2040 [EJ], by sector, by macro-regions


#--------- Check anomalies with values for Industry --------

#Energy need gaps for Industry are two order of magnitude bigger. Here I check where there results come from 

#Plot many countries

data.seven.nations <- data.calculated %>%
  filter(iso %in% c('BRA','CHN','DEU','IND','NGA','USA','ZAF'))

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
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())
plot

ggsave(paste('7 countries ',variable,' ',max_year,'.png',sep=''), plot=plot, path=here('checks'), dpi=400,  width=10, height=7)



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



#Find ANOMALIES

#Find all the data for which dle.threshold.adjusted is > 50
check.anomalies.dle.threshold <- data.calculated %>%
  filter(dle.threshold.adjusted > 50)

check.anomalies.dle.threshold <- check.anomalies.dle.threshold %>%
  mutate( iso = iso_to_country(iso) ) %>%
  rename( country = iso )

#The anomalies in very high values of energy needs for Industry are due to very high dle.tech.scalers for many African countries, which lead to high DLE thresholds and therefore high energy gaps

