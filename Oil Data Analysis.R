###############
# Load required libraries
library(tidyverse)
library(readr)

# Read the data from CSV file
general <- read.csv('oil data.csv', stringsAsFactors = FALSE, header = TRUE)

# View the original dataset
View(general)

# Create a new dataset with non-missing values in "Estmated.Quantity" column
new_general <- general %>%
  filter(!is.na(Estmated.Quantity)) %>%
  select(YEAR, Status, Cause, Contaminat, States.Affected, Estmated.Quantity)

# View the new dataset
View(new_general)

# Create another dataset with numeric conversion of "Estmated.Quantity" and non-missing values
new_gen <- general %>%
  mutate(Est_qty = as.numeric(Estmated.Quantity)) %>%
  filter(!is.na(Est_qty)) %>%
  select(YEAR, Status, Cause, Contaminat, States.Affected, Estmated.Quantity)

# View the new dataset
View(new_gen)

# Calculate the percentage of missing values for each variable in new_gen
sapply(new_gen, function(x) sum(is.na(x), na.rm = TRUE) / length(x) * 100)

# Calculate the mean of "Est_qty" column in new_gen
mean(new_gen$Est_qty, na.rm = TRUE)

# Replace missing values in "Est_qty" column with the mean
new_gen$Est_qty[is.na(new_gen$Est_qty)] <- mean(new_gen$Est_qty, na.rm = TRUE)

# View the updated dataset
View(new_gen)

# Read the data from CSV file using read_csv function from the readr package
oil <- read_csv('oil data.csv', col_names = TRUE, na = c("", "NA"))

# Create a new dataset with numeric conversion of "Estmated Quantity" and non-missing values
new_gen <- oil %>%
  mutate(Est_qty = as.numeric(`Estmated Quantity`)) %>%
  filter(!is.na(Est_qty))

# View the new dataset
View(new_gen)

# View the original dataset
View(oil)

# Display the structure of the dataset
str(oil)

# Convert selected columns to factors
oil$Status <- as.factor(oil$Status)
oil$Cause <- as.factor(oil$Cause)
oil$Contaminat <- as.factor(oil$Contaminat)
oil$YEAR <- as.factor(oil$YEAR)
oil$`States Affected` <- as.factor(oil$`States Affected`)

# Create a new dataset with selected columns
new_oil <- oil %>%
  select(YEAR, Status, Cause, Contaminat, `States Affected`, `Estmated Quantity`)

# View the new dataset
View(new_oil)

# Display the structure of the dataset
str(new_oil)

# Calculate the percentage of missing values for each variable in new_oil
sapply(new_oil, function(x) sum(is.na(x)) / length(x) * 100)

# Set the value of Estmated_Quantity to NA and assign it to Estmated Quantity column
new_oil$Estmated_Quantity <- NA
new_oil$`Estmated Quantity` <- new_oil$Estmated_Quantity

# Calculate the percentage of missing values for Estmated Quantity
sapply(new_oil$`Estmated Quantity`, function(x) sum(is.na(x), na.rm = TRUE) / length(x) * 100)

###############################################################

# Replace NA values in 'Estmated Quantity' with the mean of non-NA values
new_oil$`Estmated Quantity`[is.na(new_oil$`Estmated Quantity`)] <- mean(new_oil$`Estmated Quantity`, na.rm = TRUE)

# Calculate the mean of 'Estmated Quantity' after replacing NA values
mean(new_oil$`Estmated Quantity`, na.rm = TRUE)

# Calculate the percentage of NA values in each column of 'new_oil'
sapply(new_oil, function(x) sum(is.na(x)) / length(x) * 100)

# View the modified 'new_oil' dataset
View(new_oil)

########################################################################################33

# Cleaning the 'Cause' variable

# Display the frequency table of 'Cause', including NA values
table(new_oil$Cause, useNA = 'always')

# Replace empty strings with 'unknown' in 'Cause'
new_oil$Cause <- replace(new_oil$Cause, new_oil$Cause == '', 'unknown')

# Replace NA values in 'Cause' with 'unknown'
new_oil$Cause[is.na(new_oil$Cause)] <- 'unknown'

# Create a new column 'CAUSE' and initialize it with NA
new_oil$CAUSE <- NA

# Map specific values of 'Cause' to corresponding values in 'CAUSE'
new_oil$CAUSE[new_oil$Cause %in% c('NA')] <- 'unknown'
new_oil$CAUSE[new_oil$Cause %in% c('other: mystery', 'MYSTERY SPILL', 'other: Mystery Sheen',
                                   'other: mys', 'other: Mystery')] <- 'mystery'
new_oil$CAUSE[new_oil$Cause %in% c('cor')] <- 'corrosion'
new_oil$CAUSE[new_oil$Cause %in% c('ytd')] <- 'yet to determine'
new_oil$CAUSE[new_oil$Cause %in% c('sab')] <- 'sabotage'
new_oil$CAUSE[new_oil$Cause %in% c('ome')] <- 'operational error'
new_oil$CAUSE[new_oil$Cause %in% c('eqf', ' other: STRUCURAL FAILURE')] <- 'equipment failure'
new_oil$CAUSE[new_oil$Cause %in% c('other:natural disaster (flood)', 'other:natural disaster',
                                   'other: Heavy downpour resulted in overflow of oily water',
                                   'other: High Sea Tide')] <- 'Natural Disaster'
new_oil$CAUSE[new_oil$CAUSE %in% c('other: Unknown', 'other:',
                                   'other:Trapped crude as a result of previous spill',
                                   'other:overflow of bundwall')] <- 'other'

# Display the frequency table of 'CAUSE', including NA values
table(new_oil$CAUSE, useNA = 'always')

####################################################################################3
# Calculate the percentage of missing values in each column of the 'new_oil' data frame
sapply(new_oil, function(x) sum(is.na(x))/length(x) * 100)

###########################################
# Cleaning the 'Contaminat' variable

# Display the frequency table of values in the 'Contaminat' column, including missing values
table(new_oil$Contaminat, useNA = 'always')

# Replace NA values in the 'Contaminat' column with 'cr'
new_oil$Contaminat[is.na(new_oil$Contaminat)] <- 'cr'

# Remove the 'contaminant' column (assuming it's a typo since it's not used later)
# remove(new_oil$contaminant)

# Create a new column 'contaminat' and set it as NA
new_oil$contaminat <- NA

# Assign specific values to 'contaminat' based on corresponding values in 'Contaminat'
new_oil$contaminat[new_oil$Contaminat %in% c('cr')] <- 'crude oil'
new_oil$contaminat[new_oil$Contaminat %in% c('gs', 'gas')] <- 'gas'
new_oil$contaminat[new_oil$Contaminat %in% c('co', 'CON', 'co,ga')] <- 'Condense'
new_oil$contaminat[new_oil$Contaminat %in% c('no')] <- 'no spill'
new_oil$contaminat[new_oil$Contaminat %in% c('re (PMS)', 're(pms)', 're', 're(AGO)', 're(PMS)')] <- "refined products"
new_oil$contaminat[new_oil$Contaminat %in% c('ch')] <- "chemical"
# ... and so on, assigning values based on specific conditions

# Replace NA values in 'contaminat' with 'crude oil'
new_oil$contaminat[is.na(new_oil$contaminat)] <- 'crude oil'

# Calculate the percentage of missing values in the 'contaminat' column
sapply(new_oil$contaminat, function(x) sum(is.na(x))/length(x) * 100)

# Display the frequency table of values in the 'contaminat' column, including missing values
table(new_oil$contaminat, useNA = 'always')

# Calculate the percentage of missing values in each column of 'new_oil' data frame
sapply(new_oil, function(x) sum(is.na(x)/length(x)) * 100)

# Remove the 7th column from the 'new_oil' data frame
new_oil <- new_oil[, -7]

# View the modified 'new_oil' data frame
View(new_oil)

################################################################################################
#cleaning state affected
new_oil$state_affected <- NA
# Display table of 'States Affected' column with NA values included
table(new_oil$`States Affected`, useNA = 'always')

# Replace NA values in 'States Affected' column with 'unknown'
new_oil$`States Affected`[is.na(new_oil$`States Affected`)] <- 'unknown'

# Replace specific values in 'States Affected' column with corresponding state names
new_oil$state_affected[new_oil$`States Affected` %in% c('AB')] <- 'ABIA'
new_oil$state_affected[new_oil$`States Affected` %in% c('AK')] <- 'AKURE'
new_oil$state_affected[new_oil$`States Affected` %in% c('BY')] <- 'BAYELSA'
new_oil$state_affected[new_oil$`States Affected` %in% c('DE')] <- 'DELTA'
new_oil$state_affected[new_oil$`States Affected` %in% c('ED')] <- 'EDO'
new_oil$state_affected[new_oil$`States Affected` %in% c('FCT')] <- 'FCT'
new_oil$state_affected[new_oil$`States Affected` %in% c('IM','IM,IM')] <- 'IMO'
new_oil$state_affected[new_oil$`States Affected` %in% c('LA','KADUNA','KADUNA-NORTH','OG','KO','KW')] <- 'OTHER STATES'
new_oil$state_affected[new_oil$`States Affected` %in% c('ON')] <- 'ONDO'
new_oil$state_affected[new_oil$`States Affected` %in% c('RI','RI,RI','RI,undefined','undefined')] <- 'RIVER'
new_oil$state_affected[new_oil$`States Affected` %in% c('C R O S S R I V E R')] <- 'CROSSRIVER'

# Replace remaining NA values in 'state_affected' column with 'unknown'
new_oil$state_affected[is.na(new_oil$state_affected)] <- 'unknown'

# Convert 'CAUSE', 'contaminat', and 'state_affected' columns to factors
new_oil$CAUSE <- as.factor(new_oil$CAUSE)
new_oil$contaminat <- as.factor(new_oil$contaminat)
new_oil$state_affected <- as.factor(new_oil$state_affected)




# Plotting a bar chart of 'CAUSE' variable against 'Status' variable
ggplot(new_oil, aes(x = CAUSE, fill = factor(Status))) +
  geom_bar()

# Plotting a bar chart of 'YEAR' variable against 'contaminat' variable
ggplot(new_oil, aes(x = YEAR, fill = factor(contaminat))) +
  geom_bar()

# Plotting a bar chart of 'YEAR' variable against 'state_affected' variable
# Also adding a density plot with transparency
ggplot(new_oil, aes(x = YEAR, fill = factor(state_affected))) +
  geom_bar() +
  geom_density(alpha = 0.5)

# Plotting a bar chart of 'CAUSE' variable against 'state_affected' variable
ggplot(new_oil, aes(x = CAUSE, fill = factor(state_affected))) +
  geom_bar()

# Plotting a bar chart of 'CAUSE' variable against 'YEAR' variable
ggplot(new_oil, aes(x = CAUSE, fill = factor(YEAR))) +
  geom_bar()

# Plotting a bar chart of 'YEAR' variable against 'CAUSE' variable
ggplot(new_oil, aes(x = YEAR, fill = factor(CAUSE))) +
  geom_bar()

# Plotting a bar chart of 'Status' variable
# Using a black and white theme
ggplot(new_oil, aes(x = Status)) +
  theme_bw() +
  geom_bar()
