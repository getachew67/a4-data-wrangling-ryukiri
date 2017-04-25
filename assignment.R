# a4-data-wrangling

################################### Set up ###################################

# Install (if not installed) + load dplyr package 
install.packages("dplyr")
library("dplyr")

# Set your working directory to the appropriate project folder
setwd("C:/Users/austi/Google Drive/College/Sophomore/Info 201/Homework/a4-data-wrangling-ryukiri")

# Read in `any_drinking.csv` data using a relative path
any.drinking <- read.csv("data/any_drinking.csv")

# Read in `binge.drinking.csv` data using a relative path
binge.drinking <- read.csv("data/binge_drinking.csv")

# Create a directory (using R) called "output" in your project directory
dir.create("output")

################################### Any drinking in 2012 ###################################

# For this first section, let's just work with the columns `state`, `location`, and the data from 2012
# (from the *any drinking* dataset)
# Create a data.frame that has the `state` and `location` columns, and all columns from 2012
drinking.2012 <- select(any.drinking, state, location, both_sexes_2012, females_2012, males_2012)

# Using the 2012 data, create a column that has the difference in male and female drinking patterns
drinking.2012 <- mutate(drinking.2012, difference = females_2012-males_2012)

# Write your 2012 data to a .csv file in your `output/` directory with an expressive filename
write.csv(file="output/drinking_2012.csv", x=drinking.2012)

# Are there any locations where females drink more than males?
# Your answer should be a *dataframe* of the locations, states, and differences for all locations (no extra columns)
females.more.than.males.2012 <- drinking.2012 %>% 
  filter(difference > 0) %>% 
  select(state, location, difference)

# What is the location in which male and female drinking rates are most similar (*absolute* difference is smallest)?
# Your answer should be a *dataframe* of the location, state, and value of interest (no extra columns)
male.female.similar <- drinking.2012 %>% 
  filter(difference == max(difference)) %>% 
  select(state, location, difference)

# As you've (hopefully) noticed, the `location` column includes national, state, and county level estimates. 
# However, many audiences may only be interested in the *state* level data. Given that, you should do the following:
# Create a new variable that is only the state level observations in 2012
state.data <- drinking.2012 %>% 
  group_by(state) %>% 
  summarise(differences = mean(difference)) 

# Which state had the **highest** drinking rate for both sexes combined? 
# Your answer should be a *dataframe* of the state and value of interest (no extra columns)
highest.drinking <- drinking.2012 %>%
  filter(both_sexes_2012 == max(both_sexes_2012)) %>%
  select(state, both_sexes_2012)

# Which state had the **lowest** drinking rate for both sexes combined?
# Your answer should be a *dataframe* of the state and value of interest (no extra columns)
lowest.drinking <- drinking.2012 %>%
  filter(both_sexes_2012 == min(both_sexes_2012)) %>%
  select(state, both_sexes_2012)

# What was the difference in (any-drinking) prevalence between the state with the highest level of consumption, 
# and the state with the lowest level of consumption?
# Your answer should be a single value (a dataframe storing one value is fine)
difference <- select(highest.drinking, both_sexes_2012) - select(lowest.drinking, both_sexes_2012)

# Write your 2012 state data to an appropriately named file in your `output/` directory
write.csv(file="output/state_drinking_2012.csv", x=state.data)

# Write a function that allows you to specify a state, then saves a .csv file with only observations from that state
# You should use the entire any.drinking dataset for this function
# Make sure the file you save in the `output` directory indicates the state name, and avoid using rownames.
state.observation <- function(my.state) {
  state.info <- filter(any.drinking, state == my.state)
  write.csv(file = paste("output/", my.state, ".csv", sep = ""), x=state.info )
}

# Demonstrate your function works by writing 3 .csv files of the states of your choice
state.observation("Alabama")
state.observation("California")
state.observation("Washington")

################################### Binge drinking Dataset ###################################
# In this section, we'll ask a variety of questions regarding our binge.drinking dataset. 
# In order to ask these questions, you'll need to first prepare a subset of the data for this section:
  
# Create a dataframe with only the county level observations from the binge_drinking dataset 
# (i.e., exclude state/national estimates)
# This should include "county-like" areas such as parishes and boroughs
county.drinking <- filter(binge.drinking, !(location %in% c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 
                                                            'California', 'Colorado', 'Conecticut', 'Delaware', 
                                                            'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 
                                                            'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 
                                                            'Maine', 'Maryland', 'Massachusetts', 'Michigan', 
                                                            'Minnesota', 'Missouri', 'Montana', 'Nebraska', 
                                                            'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 
                                                            'New York', 'North Carolina', 'North Dakota', 'Ohio', 
                                                            'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 
                                                            'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 
                                                            'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 
                                                            'Wisconsin', 'Wyoming')), state != 'National')

# What is the average county level of binge drinking in 2012 for both sexes?
average.binge.both <- county.drinking %>% 
  summarise(mean = mean(both_sexes_2012))

# What is the minimum county level of binge drinking in each state (in 2012 for both sexes)? 
# Your answer should contain 50 values (one for each state), unless there are two counties in a state with the same value
# Your answer should be a *dataframe* with the value of interest, location, and state
states <- binge.drinking %>%
  group_by(state) %>% 
  filter(both_sexes_2012 == min(both_sexes_2012)) %>%
  select(location, state, both_sexes_2012)

# What is the maximum county level of binge drinking in each state (in 2012 for both sexes)? 
# Your answer should be a *dataframe* with the value of interest, location, and state
county.2012 <- county.drinking %>%
  group_by(state) %>%
  filter(both_sexes_2012 == max(both_sexes_2012)) %>%
  select(both_sexes_2012, location, state)

# What is the county with the largest increase in male binge drinking between 2002 and 2012?
# Your answer should include the county, state, and value of interest
male.largest.increase <- county.drinking %>%
  mutate(difference = males_2012 - males_2002) %>%
  select(location, state, difference) %>%
  filter(difference == max(difference))

# How many counties experienced an increase in male binge drinking between 2002 and 2012?
# Your answer should be an integer (a dataframe with only one value is fine)
increase <- nrow(filter(county.drinking %>%
                          mutate(difference = males_2012 - males_2002) %>%
                          select(location, state, difference), 
                        difference > 0))

# What percentage of counties experienced an increase in male binge drinking between 2002 and 2012?
# Your answer should be a fraction or percent (we're not picky)
increase.percentage <- increase / nrow(county.drinking %>%
                                         mutate(difference = males_2012 - males_2002) %>%
                                         select(location, state, difference))
                    
# How many counties observed an increase in female binge drinking in this time period?
# Your answer should be an integer (a dataframe with only one value is fine)
increase.female <- nrow(filter(county.drinking %>%
                                 mutate(difference = females_2012 - females_2002) %>%
                                 select(location, state, difference), 
                               difference > 0))

# What percentage of counties experienced an increase in male binge drinking between 2002 and 2012?
# Your answer should be a fraction or percent (we're not picky)
increase.percentage.female <- increase.female / nrow(county.drinking %>%
                                         mutate(difference = females_2012 - females_2002) %>%
                                         select(location, state, difference))

# How many counties experienced a rise in female binge drinking *and* a decline in male binge drinking?
# Your answer should be an integer (a dataframe with only one value is fine)
increase.female.decrease.male <- 
                                  
################################### Joining Data ###################################
# You'll often have to join different datasets together in order to ask more involved questions of your dataset. 
# In order to join our datasets together, you'll have to rename their columns to differentiate them

# First, rename all prevalence columns in the any.drinking dataset to the have prefix "any."
# Hint: you can get (and set!) column names using the colnames function. This may take multiple lines of code.
new.any.drinking <- read.csv("data/any_drinking.csv")
colnames(new.any.drinking) = paste("any", colnames(new.any.drinking), sep = ".")
colnames(new.any.drinking)[1] = "state"
colnames(new.any.drinking)[2] = "location"

# Then, rename all prevalence columns in the binge.drinking dataset to the have prefix "binge."
# Hint: you can get (and set!) column names using the colnames function. This may take multiple lines of code.
new.binge.drinking <- read.csv("data/binge_drinking.csv")
colnames(new.binge.drinking) = paste("binge", colnames(new.binge.drinking), sep = ".")
colnames(new.binge.drinking)[1] = "state"
colnames(new.binge.drinking)[2] = "location"

# Then, create a dataframe with all of the columns from both datasets. 
# You can do this by performing a full join on the two datasets by the `location` column
combined <- full_join(new.any.drinking, new.binge.drinking, by=c('state', 'location'))

# Create a column of difference b/w `any` and `binge` drinking for both sexes in 2012
combined <- combined %>% 
  mutate(difference = any.both_sexes_2012 - binge.both_sexes_2012)

# Which location has the greatest *absolute* difference between `any` and `binge` drinking?
# Your answer should be a one row data frame with the state, location, and value of interest (difference)
greatest.difference <- combined %>%
  filter(difference == max(abs(difference))) %>%
  select(state, location, difference)

# Which location has the smallest *absolute* difference between `any` and `binge` drinking?
# Your answer should be a one row data frame with the state, location, and value of interest (difference)
min.difference <- combined %>%
  filter(difference == min(abs(difference))) %>%
  select(state, location, difference)

################################### Write a function to ask your own question(s) ###################################
# Even in an entry level data analyst role, people are expected to come up with their own questions of interest
# (not just answer the questions that other people have). For this section, you should *write a function*
# that allows you to ask the same question on different subsets of data. 
# For example, you may want to ask about the highest/lowest drinking level given a state or year. 
# The purpose of your function should be evident given the input parameters and function name. 
# After writing your function, *demonstrate* that the function works by passing in different parameters to your function.

# Write a function that returns the average binge drinking amount for
# both sexes in 2002 for the given state.
both.binge.2002 <- function(my_state) {
  binge <- binge.drinking %>%
    select(both_sexes_2002, state) %>%
    group_by(state) %>%
    summarise(both_sexes_2002 = mean(both_sexes_2002)) %>%
    filter(state == my_state) %>%
    select(both_sexes_2002)
  return(binge)
}

both.binge.2002("Washington")

################################### Challenge ###################################

# Using your function from part 1 that wrote a .csv file for given a state name, write a file for all 50 states
# You should be able to do this in a *single line of (concise) code*
x <- c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Conecticut', 'Delaware', 
       'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 
       'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Missouri', 'Montana', 'Nebraska', 
       'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 
       'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 
       'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')

for(i in length(x)) {
  state.observation(x[i])
}


# Using a dataframe of your choice from above, write a function that allows you to specify a *year* and *state* of interest, 
# that saves a csv file with observations from that state's counties. 
# It should only write the columns `state`, `location`, and data from the specified year. 
# Before writing the .csv file, you should *sort* the data.frame in descending order
# by the both_sexes drinking rate in the specified year. 
# Again, make sure the file you save in the output directory indicates the year and state. 
# Note, this will force you to confront how dplyr uses *non-standard evaluation*
# Hint: https://cran.r-project.org/web/packages/dplyr/vignettes/nse.html
observations.for.state <- function(my_year, my_state) {
  filtered.state.year <- any.drinking %>%
    select(state, location, 
           paste("both_sexes_", my_year, sep = ""), 
           paste("males", my_year, sep = ""), 
           paste("females", my_year, sep = ""))
  return (filtered.state.year)
}