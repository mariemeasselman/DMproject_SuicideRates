#Installing Packages
install.packages("readr")
install.packages("ggplot2")
install.packages("countrycode")
install.packages("rworldmap")

#Loading Libraries 
library(readr)
library(dplyr)
library(ggplot2)
library(countrycode)
library(RColorBrewer)
library(rworldmap)

#Reading Data File
mydata <- read_csv("D:/Downloads/master.csv.zip")

#Showing Dataset Characteristics
#-------------------------------
head(mydata)
str(mydata)
summary(mydata)
(colSums(is.na(mydata)) / nrow(mydata)) * 100 #missing values rates

#Renaming columns for an easier manipulation
#-------------------------------------------
mydata <- mydata %>%
  rename(
    suicides_per_100k_pop = `suicides/100k pop`,
    HDI_for_year = 'HDI for year',  
    gdp_for_year = 'gdp_for_year ($)',
    gdp_per_capita = 'gdp_per_capita ($)'
  )

#Adding the column Continent
#---------------------------
mydata$continent <- countrycode(mydata$country, "country.name", "continent")
mydata$continent <- as.factor(mydata$continent)

#Plotting Boxplots to Show Outliers
#----------------------------------
boxplot(mydata$year, mydata$suicides_no, mydata$population, mydata$HDI_for_year,
        mydata$gdp_for_year, mydata$gdp_per_capita,
        names = c("Year", "Suicides number", "Population","HDI for year", "GDP for year", "GDP per capita"),
        main = "Boxplots for Different Variables")

#Mean of Suicide Rate for Each Year
mean_data_per_year <- mydata %>%
  group_by(year) %>%
  summarize(mean_suicide_rate = mean(suicides_per_100k_pop, na.rm = TRUE))

#Plotting the Trend of Suicide Rate per Year
#-------------------------------------------
#Calculating the global mean for the line plot
global_mean <- mean(mydata$suicides_per_100k_pop, na.rm = TRUE)
#Creating the plot for year-wise average suicides
year_wise_suicide_plot <- mydata %>%
  group_by(year) %>%
  summarise(mean_suicides_per_100K_pop = mean(suicides_per_100k_pop, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_suicides_per_100K_pop, group = 1)) +
  geom_line(col = 'blue', size = 1) +
  geom_point(col = 'blue', size = 2) +
  geom_hline(yintercept = global_mean, linetype = 2, size = 1) +
  scale_x_continuous(breaks = seq(1985, 2016, 2)) +
  labs(
    title = "Global Trend Over Time (1985-2016)",
    x = "Year",
    y = "Average Suicides (per 100k)"
  ) +
  theme(
    plot.title = element_text(size = 20),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
# Display the plot
year_wise_suicide_plot

#Plotting trends over time, continent wise
#-----------------------------------------
#Grouping by year and continent to get the mean suicide rates for each year and continent
continent_trends <- mydata %>%
  group_by(year, continent) %>%
  summarise(mean_suicides_per_100K_pop = mean(suicides_per_100k_pop, na.rm = TRUE))

#Function to create a plot for a given continent
plot_continent_trends <- function(continent_name, data) {
  ggplot(data %>% filter(continent == continent_name), aes(x = year, y = mean_suicides_per_100K_pop)) +
    geom_line(size = 1.5, color = "blue") +
    geom_point(size = 3, color = "blue") +
    labs(
      title = paste("Suicide Rates Over Time for", continent_name),
      x = "Year",
      y = "Mean Suicides (per 100k)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      axis.text.x = element_text(size = 14, angle = 90, hjust = 1),
      axis.text.y = element_text(size = 14)
    )
}
# List of unique continents
continents <- unique(continent_trends$continent)
# Loop through each continent and plot individually
for (continent in continents) {
  plot <- plot_continent_trends(continent, continent_trends)
  print(plot)
}

#Plotting mean of suicide numbers per 100k by continent
#------------------------------------------------------
#Group by continent to get the mean suicides per 100k population
continent_mean <- mydata %>%
  group_by(continent) %>%
  summarise(mean_suicides_per_100K_pop = mean(suicides_per_100k_pop, na.rm = TRUE))

#Reorder the continents by the mean suicides per 100k population
continent_mean <- continent_mean %>%
  mutate(continent = reorder(continent, mean_suicides_per_100K_pop))

#Create a bar chart with gradient blue color scale and ordered bars
bar_chart <- ggplot(continent_mean, aes(x = continent, y = mean_suicides_per_100K_pop, fill = mean_suicides_per_100K_pop)) +
  geom_bar(stat = "identity") +  # Use 'stat = "identity"' to plot actual values
  labs(
    title = "Mean Suicides per 100k Population by Continent",
    x = "Continent",
    y = "Mean of Suicides Cases per 100k"
  ) +
  theme_minimal() +  # Simple theme
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 14)
  ) +
  scale_fill_gradient(low = "lightblue", high = "navyblue")  # Gradient color scale with shades of blue

# Display the bar chart
print(bar_chart)


#To check which continent reported more data
#-------------------------------------------
#We plot a pie chart with percentages of each contient
#Create a frequency table to count the number of entries per continent
continent_counts <- mydata %>%
  group_by(continent) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)  # Calculate the percentage for each continent

# Create a pie chart
pie_chart <- ggplot(continent_counts, aes(x = "", y = percentage, fill = continent)) +
  geom_bar(stat = "identity", width = 1) +  # Use 'width = 1' to create a full pie chart
  coord_polar("y", start = 0) +  # Convert to pie chart with polar coordinates
  theme_void() +  # Remove background and axes
  labs(
    title = "Percentage of Each Continent in the Data"
  ) +
  scale_fill_brewer(palette = "Blues") +  # Color palette for pie slices
  geom_text(aes(label = sprintf("%.1f%%", percentage)),  # Add text labels with percentages
            position = position_stack(vjust = 0.5))  # Center text in the pie slices

# Display the pie chart
print(pie_chart)


#Plotting trends over time, gender wise
#-----------------------------------------
#Grouping by year and sex to get the mean suicide rates for each year and gender
gender_trends <- mydata %>%
  group_by(year, sex) %>%
  summarise(mean_suicides_per_100K_pop = mean(suicides_per_100k_pop, na.rm = TRUE))

#Function to create a plot for a given gender
plot_gender_trends <- function(gender_name, data) {
  ggplot(data %>% filter(sex == gender_name), aes(x = year, y = mean_suicides_per_100K_pop)) +
    geom_line(size = 1.5, color = "blue") +
    geom_point(size = 3, color = "blue") +
    labs(
      title = paste("Suicide Rates Over Time for", gender_name),
      x = "Year",
      y = "Mean Suicides (per 100k)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      axis.text.x = element_text(size = 14, angle = 90, hjust = 1),
      axis.text.y = element_text(size = 14)
    )
}
# List of unique genders
genders <- unique(gender_trends$sex)
# Loop through each gender and plot individually
for (gender in genders) {
  plot_gender <- plot_gender_trends(gender, gender_trends)
  print(plot_gender)
}

#Global Mean of Suicide Rate per Gender
#--------------------------------------
#Calculate the average suicides_per_100k_pop for each sex
gender_averages <- mydata %>%
  group_by(sex) %>%
  summarise(mean_suicides_per_100k = mean(suicides_per_100k_pop, na.rm = TRUE))

#Create a bar plot with custom colors for each gender
bar_chart <- ggplot(gender_averages, aes(x = sex, y = mean_suicides_per_100k, fill = sex)) +
  geom_bar(stat = "identity") +  # Use 'stat = "identity"' to plot actual values
  labs(
    title = "Mean of Suicides Rates per 100k by Sex",
    x = "Sex",
    y = "Mean of Suicides per 100k"
  ) +
  theme_minimal() +  # Clean theme
  scale_fill_manual(values = c("male" = "navyblue", "female" = "lightblue")) +  # Set custom colors
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

# Display the bar plot
print(bar_chart)

#Plotting trends over time, age group wise
#--------------------------------------
#Group by year and age group to get the mean suicide rates for each year and age group
age_group_trends <- mydata %>%
  group_by(year, age) %>%
  summarise(mean_suicides_per_100K_pop = mean(suicides_per_100k_pop, na.rm = TRUE))

#Function to create a plot for a given age group
plot_age_trends <- function(age_group_name, data) {
  ggplot(data %>% filter(age == age_group_name), aes(x = year, y = mean_suicides_per_100K_pop)) +
    geom_line(size = 1.5, color = "blue") +
    geom_point(size = 3, color = "blue") +
    labs(
      title = paste("Suicide Rates Over Time for Age Group:", age_group_name),
      x = "Year",
      y = "Mean Suicides (per 100k)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      axis.text.x = element_text(size = 14, angle = 90, hjust = 1),
      axis.text.y = element_text(size = 14)
    )
}

# List of unique age groups
age_groups <- unique(age_group_trends$age)

# Loop through each age group and plot individually
for (age_group in age_groups) {
  plot_age <- plot_age_trends(age_group, age_group_trends)
  print(plot_age)  
}

#Plot a bar chart of the mean of suicides per age group
#------------------------------------------------------
# Group by age group to get the mean suicide rate per 100k population
age_group_averages <- mydata %>%
  group_by(age) %>%
  summarise(mean_suicides_per_100K_pop = mean(suicides_per_100k_pop, na.rm = TRUE))

# Reorder the age groups by the mean suicide rate per 100k population
age_group_averages <- age_group_averages %>%
  mutate(age = reorder(age, mean_suicides_per_100K_pop))

# Create a bar chart with gradient blue color scale and ordered bars
bar_chart <- ggplot(age_group_averages, aes(x = age, y = mean_suicides_per_100K_pop, fill = mean_suicides_per_100K_pop)) +
  geom_bar(stat = "identity") +  # Use 'stat = "identity"' to plot actual values
  labs(
    title = "Average Suicides per 100k by Age Group",
    x = "Age Group",
    y = "Mean Suicides per 100k"
  ) +
  theme_minimal() +  # Simple theme
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 14)
  ) +
  scale_fill_gradient(low = "lightblue", high = "navyblue")  # Gradient color scale with shades of blue

# Display the bar chart
print(bar_chart)


#Plotting a world map showing the average of suicide cases in each country
#-------------------------------------------------------------------------
country_averages <- mydata %>%
  group_by(country) %>%
  summarize(mean_suicides_per_100k = mean(suicides_per_100k_pop, na.rm = TRUE))

#Joining Data with Country Map
countrydata <- joinCountryData2Map(country_averages, joinCode = "NAME", nameJoinColumn = "country")

#Checking if the data joined successfully
if (is.null(countrydata)) {
  stop("Data join with map failed. Please check country names and join codes.")
}

#Creating a world map with mean suicides per 100k
red_palette <- colorRampPalette(c("#ffcccc", "#ff3333", "#b30000"))(7)
par(mar = c(0, 0, 0, 0))  # Reset margins
mapCountryData(
  countrydata,
  nameColumnToPlot = "mean_suicides_per_100k",  # Column to plot
  mapTitle = "Mean Suicides per 100k Population by Country",
  colourPalette = red_palette,  # Color palette
  oceanCol = "lightblue",
  missingCountryCol = "grey65",
  catMethod = "pretty",  # Method for color categorization
  addLegend = TRUE  # Add a legend
)