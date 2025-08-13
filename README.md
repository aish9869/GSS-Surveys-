# GSS-Surveys-
The populations opinion on government expenditures on crime reduction. 
<br>
Author - Aishwarya Srivastava 

<br>
Code: 
<br>
# Install 'gssr' from 'ropensci' universe
install.packages('gssr', repos =
                   c('https://kjhealy.r-universe.dev', 'https://cloud.r-project.org'))

<br>
#load the gssr library and other necessary libraries
library(gssr) 
<br>
library(tidyverse)
<br>
library(haven)
<br>
library(skimr)
<br>
library(stargazer)

#load the gssr data frame (it's big and will take a minute to load)
data(gss_all)
<br>

#select the variables for your project
gss_select<-gss_all%>%
  select(natcrime, fear, courts, tax, age, race, sex, year, educ)%>%             
  mutate(across(everything(), zap_missing)) # Convert all missing codes to NA
<br>

#categorical data clean (convert to factors)
gss_factors<-gss_select%>%
  mutate(sex=as_factor(sex),                
         race=as_factor(race),
         natcrime=as_factor(natcrime),
         fear=as_factor(fear),
         courts=as_factor(courts),
         tax=as_factor(tax))
<br>
# Check your levels
summary(gss_factors$sex)
<br>
summary(gss_factors$race)
<br>
summary(gss_factors$natcrime)
<br>
summary(gss_factors$fear)
<br>
summary(gss_factors$courts)
<br>
summary(gss_factors$tax)
<br>

#show the frequency table 
gss_factors %>%
  count(courts) %>%
  mutate(percentage = n / sum(n) * 100)

<br>
gss_factors %>%
  count(race) %>%
  mutate(percentage = n / sum(n) * 100)
<br>
gss_factors %>%
  count(fear) %>%
  mutate(percentage = n / sum(n) * 100)
<br>
gss_factors %>%
  count(sex) %>%
  mutate(percentage = n / sum(n) * 100)
<br>
gss_factors %>%
  count(tax) %>%
  mutate(percentage = n / sum(n) * 100)
<br>
gss_factors %>%
  count(natcrime) %>%
  mutate(percentage = n / sum(n) * 100)
<br>
gss_factors %>%
  count(year)
<br>
gss_factors %>%
  count(educ)
<br>
#create indicator variables from your factor variables
gss_indicators <- gss_factors %>%
  mutate(sex_male = if_else(sex == "male", 1, 0),  
         sex_female = if_else(sex == "female", 1, 0),
         race_white = if_else(race == "white", 1, 0),
         race_black = if_else(race == "black", 1, 0),
         race_other = if_else(race == "other", 1, 0),
         courts_very_harsh = if_else(courts == "too harshly", 3, 0),
         courts_harsh = if_else(courts == "not harshly enough", 2, 0),
         courts_right = if_else(courts == "about right", 1, 0),
         natcrime_too_lit = if_else(natcrime == "too little", 1, 0),
         natcrime_abt_rit = if_else(natcrime == "about right",2, 0), 
         natcrime_t_much = if_else(natcrime == "too much", 3, 0), 
         tax_high = if_else(tax == "too high", 3, 0),
         tax_abt_rit = if_else(tax == "about right", 2, 0),
         tax_low = if_else(tax == "too low", 1, 0),
         fear_yes = if_else(fear == "yes", 1, 0),
         fear_no = if_else(fear == "no", 1, 0)
  )
<br>

# Load necessary library
library(tibble)
<br>
# Create a table listing qualitative and indicator variables
variable_table <- tibble(
  Qualitative_Variables = c("sex", "race", "courts", "natcrime", "tax", "fear"),
  Indicator_Variables = c("sex_male, sex_female",
                          "race_white, race_black, race_other",
                          "courts_very_harsh, courts_harsh, courts_right",
                          "natcrime_too_lit, natcrime_abt_rit, natcrime_t_much",
                          "tax_high, tax_abt_rit, tax_low",
                          "fear_yes, fear_no")
)
<br>
# Print the table
print(variable_table)

#quantitative variable 
summary(gss_select$age)
<br>
summary(gss_select$year)
<br>
#replace the codes for the age variable 
Gss_new <- data.frame(
  age = c(18, 32, 44, 46.56, 60, 89, NA, NA, NA)  # Example with missing values
)
<br>
# Replace NA with a specific code (e.g., 999)
Gss_2 <- Gss_new %>%
  mutate(age = ifelse(is.na(age), 999, age))
<br>
# View updated dataset
print(Gss_2)
<br>
#Create your summary statistics table using the stargazer package

#step 1. Convert your data frame 
gss_indicators_df<-as.data.frame(gss_indicators) 
<br>
#step 2. set your working directory (where your summary stats table will be saved)
setwd("~/Desktop/econ 108")
<br>
#step 3. use stargazer to make your summary statistics table
stargazer(gss_indicators_df, type = "text", title="Descriptive statistics", digits=3, out="Summary Statistics.htm")
<br>


#create a version of your data frame just for visualization where you drop missing (NA)
gss_visualisation<-gss_indicators%>%
  drop_na() 
<br>
#Visualization Examples (you should do more formatting)
ggplot(data = gss_visualisation, mapping = aes(x = race, fill = natcrime)) +
  geom_bar(position = "stack") +
  labs(title = "Gender opinions on national crime",
       x = "Race",
       y = "Count") +
  scale_fill_brewer(palette = "Set3")
<br>

# Aggregate natcrime by year (mean or count dependitax# Aggregate natcrime by year (mean or count depending on data)
crime_trend <- gss_select %>%
  group_by(year) %>%
  summarise(mean_natcrime = mean(natcrime, na.rm = TRUE))
<br>
# Plot the trend over time
ggplot(crime_trend, aes(x = year, y = mean_natcrime)) +
  geom_line(color = "pink", size = 1) +
  geom_point(color = "black", size = 2) +
  theme_minimal() +
  labs(title = "Spending perceptions over the years",
       x = "Year",
       y = "Mean natcrime")
<br>
#regression 
#save your first regression model to m1
# Convert variables to the correct type
gss_indicators$natcrime <- as.numeric(gss_indicators$natcrime)  # If natcrime should be numeric
gss_indicators$sex <- as.factor(gss_indicators$sex)  # Ensure 'sex' is a factor
gss_indicators$race <- as.factor(gss_indicators$race)  # Ensure 'race' is a factor
gss_indicators$age <- as.factor(gss_indicators$age)
gss_indicators$educ <- as.numeric(gss_indicators$educ)
gss_indicators$year <- as.numeric(gss_indicators$year)
<br>
# Run the linear regression model
m1 <- lm(natcrime ~ courts_very_harsh + courts_harsh + courts_right, data = gss_indicators)#choice variables 
#then in other models add general variables that she said is manadatory to see how is the regression being presented 

#m2: have age as a second order polynomial (age and age squared)
m2<-lm(natcrime ~ courts_very_harsh + courts_harsh + courts_right + sex_female + sex_male, data = gss_indicators)
#change to natcrime too little
<br>
#m3: log 
m3<-lm(natcrime ~ courts_very_harsh + courts_harsh + courts_right + sex_female + sex_male + race_white + race_black , data = gss_indicators)
<br>
#m4: interaction terms
m4<-lm(natcrime ~ courts_very_harsh + courts_harsh + courts_right + sex_female + sex_male + race_white + race_black + tax + age + educ , data = gss_indicators)
<br>
#m5: treat year as a factor variable (year fixed effectsyear()#m5: treat year as a factor variable (year fixed effects)
m5 <- lm(natcrime ~ courts_very_harsh + courts_harsh + courts_right + 
           sex_female + sex_male + race_white + race_black + 
           tax + age + year + educ, data = gss_indicators)

<br>
# Generate html file with regression model output for all three models (output looks ugly in the console)
stargazer(m1, m2, m3, m4, m5, type = "html", title = "Regression Models",
          out = "models.htm")










