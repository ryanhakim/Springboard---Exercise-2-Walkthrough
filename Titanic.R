#--- load packages ---
library(dplyr)
library(tidyr)

#---- load data, create data frame, and view ---
df <- read.csv('titanic_original.csv')
data <- tbl_df(df)
data %>% View()

#--- Deal with missing values on embarked column ---
#see how many missing values exist. It looks like there are three
summary(data$embarked)

#replace missing values with "s" 
data$embarked[data$embarked == ""] <- "S"

#--- Age --- 
#consider what "fill" variable to use. Median value is closer to the central tendancy
data %>% 
  group_by(age) %>% 
  summarise(cnt = n()) %>% 
  arrange(desc(cnt)) %>% 
  print(n = Inf)

#or view it graphically too. Population is skewed right, so we ought to use median age
hist(data$age)

median_age <- median(data$age, na.rm = TRUE)
data$age[is.na(data$age)] <- median_age

#--- Assign "None" if passenger did not make it onto a lifeboat ---
#notice the levels here, there are 28 levels in this factor column
data %>% 
  group_by(boat) %>% 
  summarise(cnt = n()) %>% 
  arrange(desc(cnt)) %>% 
  print(n = Inf)

#assign "None" to first level
levels(data$boat)[1] <- "None"

#--- add cabin number if one exists, bind, then reorder columns ---
has_cabin_number <- as.numeric(data$cabin != "")
data <- cbind(data, has_cabin_number)
data <- data[, c(1:10, 15, 11:14)]
