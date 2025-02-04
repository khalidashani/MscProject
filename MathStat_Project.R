# 1. Read data

data <- read.csv(file.choose(), header=T)

#######################################################################################

# 2. NA value in any of the column

## Converting any column with " " value to NA
data[data == ""] <- NA

## Inspect how many NA in the data
colSums(is.na(data))

######################################################################################

# 3. Group similar property types into broader categories

unique(data$Type)

data$Type_Grouped <- sapply(data$Type, function(x) {
  if (grepl("Terrace House", x)) {
    "Terrace House"
  } else if (grepl("Cluster House", x)) {
    "Cluster House"
  } else if (grepl("Semi D", x)) {
    "Semi Detached (Semi D)"
  } else if (grepl("Bungalow", x)) {
    "Bungalow"
  } else if (grepl("Town House", x)) {
    "Town House"
  } else if (grepl("Condominium", x)) {
    "Condominium"
  } else if (grepl("Apartment", x)) {
    "Apartment"
  } else if (grepl("Flat", x)) {
    "Flat"
  } else if (grepl("Service Residence", x)) {
    "Service Residence"
  } else {
    "Other"
  }
})

########################################################################################

# 4. Check the new unique values

unique(data$Type_Grouped)

unique(data$Area)

unique(data$Township)

unique(data$Median_Price)

########################################################################################

# 5. Analysis of the Data
library(dplyr)

########################################################################################

# 5.1 In each state, what is the highest median_price?

# Assuming your data frame is called 'data'
highest_median_rows_sorted <- data %>%
  group_by(State) %>%
  filter(Median_Price == max(Median_Price, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Median_Price)) %>%
  select(Township, Area, State, Type, Median_Price, Type_Grouped)

# View the result
highest_median_rows_sorted

#######################################################################################

# 5.2 In each state, what is the lowest median_price?

# Assuming your data frame is called 'data'
lowest_median_rows_sorted <- data %>%
  group_by(State) %>%
  filter(Median_Price == min(Median_Price, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Median_Price) %>%
  select(Township, Area, State, Type, Median_Price)

# View the result
lowest_median_rows_sorted

########################################################################################

# 5.3 In each of the state what is property Type that is most frequent?

# Create separate bar charts for each state
ggplot(data, aes(x = Type_Grouped, fill = Type_Grouped)) +
  geom_bar() +
  facet_wrap(~State, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Property Types by State", x = "Property Type", y = "Frequency") +
  theme(legend.title = element_blank())

#######################################################################################

# 5.4. Which township has the highest median per square foot?

top_10_median_psf <- data %>%
  select(Township, Area, State, Type, Median_Price, Median_PSF) %>%
  arrange(desc(Median_PSF)) %>%
  head(10)

# View the top 10 rows with highest Median_PSF
top_10_median_psf

#######################################################################################

if(!require(gridExtra)) install.packages("gridExtra")
if(!require(grid)) install.packages("grid")

library(dplyr)
library(gridExtra)
library(grid)

# 5.1 In each state, what is the highest median_price?
highest_median_rows_sorted <- data %>%
  group_by(State) %>%
  filter(Median_Price == max(Median_Price, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Median_Price)) %>%
  select(Township, Area, State, Type, Median_Price, Type_Grouped)

# Create a table grob
table_grob <- tableGrob(highest_median_rows_sorted)

# Save the table as a PNG
png("highest_median_table.png", width = 1000, height = 600)
grid.draw(table_grob)
dev.off()

################################################################################

# Install packages if you haven't already
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(grid)) install.packages("grid")

library(dplyr)
library(gridExtra)
library(grid)

# 5.2 In each state, what is the lowest median_price?
lowest_median_rows_sorted <- data %>%
  group_by(State) %>%
  filter(Median_Price == min(Median_Price, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Median_Price) %>%
  select(Township, Area, State, Type, Median_Price)

# Create a table grob
table_grob_low <- tableGrob(lowest_median_rows_sorted)

# Save the table as a PNG
png("lowest_median_table.png", width = 1000, height = 600)
grid.draw(table_grob_low)
dev.off()
