#-----------------HEADER-------------------

# Purpose: Some facts about education and emigration
# Author: Karthik Tadepalli

# load packages, install if not present
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, scales, janitor, readxl)

# set theme for ggplot
theme_set(theme_bw() + 
            theme(plot.title = element_text(hjust=0.5)))

#---------HELPERS-------------

# Function to perform linear interpolation on a dataset, written by Claude
# Since edu is missing for some years in many countries
interpolate_missing_values <- function(data) {
  # Convert the data to a numeric matrix
  numeric_data <- as.matrix(sapply(data, as.numeric))
  
  # Function to interpolate a single row
  interpolate_row <- function(row) {
    # Find indices of non-NA values
    valid_indices <- which(!is.na(row))
    
    # If there are less than 2 non-NA values, we can't interpolate
    if (length(valid_indices) < 2) {
      return(row)
    }
    
    # Perform linear interpolation
    interpolated <- approx(valid_indices, row[valid_indices], 1:length(row))$y
    
    return(interpolated)
  }
  
  # Apply the interpolation function to each row
  interpolated_data <- t(apply(numeric_data, 1, interpolate_row))
  
  # Convert back to a data frame and preserve original column names
  result <- as.data.frame(interpolated_data)
  colnames(result) <- colnames(data)
  
  return(result)
}

#--------------EDUCATION DATA---------------

# import country classifications
income_groups <- read_xlsx("data/wb_income_classifications.xlsx") %>%
  # remove non-countries 
  filter(row_number() <= 218) %>%
  select(country_code = Code, group = `Income group`)

# import education data
# start by skipping filler lines and cleaning names
edu <- read_csv("data/API_SE.TER.ENRR_DS2_en_csv_v2_3403591/API_SE.TER.ENRR_DS2_en_csv_v2_3403591.csv",
                skip = 4) %>% 
  clean_names()

# linear interpolation when data is missing
interpolated <- interpolate_missing_values(edu %>% select(6:69)) %>%
  data.frame()
edu <- edu %>%
  select(1:5) %>%
  bind_cols(interpolated) %>%
  # calculate growth in enrolment across time
  inner_join(income_groups) %>%
  mutate(x1990_smooth = (x1989 + x1990 + x1991)/3,
         x2015_smooth = (x2014 + x2015 + x2016)/3) %>%
  select(country_name, enrol_1990 = x1990_smooth, enrol_2015 = x2015_smooth, group) %>%
  # turn enrolment numbers into proportions
  mutate(enrol_2015 = enrol_2015/100,
         enrol_1990 = enrol_1990/100,
         # calculate the growth in enrolment rate
         change_enrol = enrol_2015 - enrol_1990) %>%
  # keep only countries with data in 1990 and 2015 (after interpolation)
  filter(!is.na(change_enrol),
         # keep only developing countries
         group %in% c("Low income", "Lower middle income")) %>%
  arrange(desc(change_enrol))

#--------------EMIGRATION + MERGING-------------------

# read data on emigration 
net_migration <- read_csv("data/API_SM.POP.NETM_DS2_en_csv_v2_3401672/API_SM.POP.NETM_DS2_en_csv_v2_3401672.csv",
                          skip = 4) %>%
  clean_names() %>%
  # flip signs so that emigration is positive; grab emigration in 2019
  mutate(net_emigration = -x2019) %>%
  select(country_name, country_code, net_emigration)

# merge with edu data on country names 
# this removes non-countries and keeps only developing countries
edu_migration_merge <- net_migration %>%
  inner_join(edu, by = "country_name") 

#------------------RESULTS--------------------

# make list of only top 20 emigrant-senders, save it as output
edu_mig_top20 <- edu_migration_merge %>%
  arrange(desc(net_emigration)) %>%
  filter(row_number() <= 20)
write_csv(edu_mig_top20, "output/top20_emigrant_senders.csv")

# make captions for graphs that describe the data
enrol_cap <- "Enrolment rate is the ratio of total college enrolment to the 18-21 population."
deving_cap <- "Sample is 54 LICs/LMICs for which education + emigration data are available."

# India and Philippines as benchmarks, Nigeria for explicit comparison
india_change <- edu_migration_merge %>%
  filter(country_name == "India") %>%
  pull(change_enrol)
philippines_change <- edu_migration_merge %>%
  filter(country_name == "Philippines") %>%
  pull(change_enrol)
nigeria_change <- edu_migration_merge %>%
  filter(country_name == "Nigeria") %>%
  pull(change_enrol)

# distribution of changes in enrolment, all developing countries
p <- edu_migration_merge %>%
  ggplot(aes(x = change_enrol, y = after_stat(count/sum(count)))) + 
  geom_histogram() + 
  geom_vline(aes(xintercept = india_change),
             linetype = 'dashed', color = 'orange') + 
  geom_vline(aes(xintercept = philippines_change),
             linetype = 'dashed', color = 'green') + 
  geom_vline(aes(xintercept = nigeria_change),
             linetype = 'dashed', color = 'blue') + 
  annotate("text", y = 0.16, x = india_change, label = "India", 
           alpha = 0.8, size = 3) + 
  annotate("text", y = 0.16, x = philippines_change, label = "Philippines", 
           alpha = 0.8, size = 3) +
  annotate("text", y = 0.17, x = nigeria_change, label = "Nigeria", 
           alpha = 0.8, size = 3) + 
  labs(x = "Change in college enrolment rate (1990-2015)",
       y = "Density",
       title = "Distribution of college enrolment growth",
       caption = paste0(enrol_cap, "\n", deving_cap)) + 
  scale_x_continuous(labels = percent)
p
ggsave("output/enrolment_distribution.png", p,
       width = 8, height = 6)

# do the countries with most emigrants have growing college educated workforce?  
p <- edu_mig_top20 %>%
  ggplot(aes(y = net_emigration, x = change_enrol)) + 
  geom_point() + 
  geom_vline(aes(xintercept = india_change),
             linetype = 'dashed', color = 'orange') +
  geom_vline(aes(xintercept = philippines_change),
             linetype = 'dashed', color = 'green') + 
  geom_vline(aes(xintercept = nigeria_change),
             linetype = 'dashed', color = 'blue') +
  labs(y = "Net emigrants (2019)",
       x = "Change in college enrolment rate (1990-2015)",
       title = "Changes in college enrolment in the top 20 emigrant-senders",
       caption = paste0(enrol_cap, "\n", deving_cap)) + 
  annotate("text", y = 900000, x = india_change, label = "India", 
           alpha = 0.8, size = 3) + 
  annotate("text", y = 900000, x = philippines_change, label = "Philippines", 
           alpha = 0.8, size = 3) +
  annotate("text", y = 900000, x = nigeria_change, label = "Nigeria", 
           alpha = 0.8, size = 3) + 
  scale_x_continuous(labels = percent) + 
  scale_y_continuous(labels = comma)
p
ggsave("output/enrolment_growth_emigrators.png", p,
       width = 8, height = 6)
