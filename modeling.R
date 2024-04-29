#Installing Packages
install.packages("broom")
install.packages("tidyr")
install.packages("pROC")
install.packages("RWeka")
install.packages("OneR")
install.packages("arules")
install.packages("arulesViz")

#Loading Libraries 
library(purrr)
library(broom)
library(tidyr)
library(caret)  # For train(), trainControl, createDataPartition
library(rpart)  # For rpart decision trees
library(rpart.plot)  # For plotting decision trees
library(pROC)
library(RWeka)
library(OneR)
library(arules)
library(dplyr)
library(arulesViz)

#Linear Modeling
------------------
#Calculating the suicide rates per country over time
country_year <- mydata %>%
  group_by(country, year) %>%
  summarize(mean_suicides_per_100k = mean(suicides_per_100k_pop, na.rm = TRUE))

#Fitting linear models for each country
country_year_trends <- country_year %>%
  nest(-country) %>%
  mutate(model = map(data, ~ lm(mean_suicides_per_100k ~ year, data = .)),
         tidied = map(model, tidy)) %>%
  unnest(tidied)

#Extracting trends for the "year" term and identify significant ones
trends <- country_year_trends %>%
  filter(term == "year") %>%
  mutate(p.adjusted = p.adjust(p.value, method = "holm")) %>%
  filter(p.adjusted < 0.05)  # Only significant trends

#Plotting the slopes (estimates) for each country
ggplot(trends, aes(x = reorder(country, estimate), y = estimate, fill = estimate > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Trends in Suicide Rates by Country (Significant Only)",
    x = "Country",
    y = "Trend (Slope of Regression)"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "red", "FALSE" = "blue"),
    labels = c("Decreasing", "Increasing")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

#Extracting the top 12 countries with the steepest increasing trends
top_12_increasing_countries <- trends %>%
  arrange(desc(estimate)) %>%
  head(12) %>%
  pull(country)  # Get the country names

#Plotting the 12 countries with the most increasing trends
ggplot(country_year %>%
         filter(country %in% top_12_increasing_countries),  # Filter for the top 12
       aes(x = year, y = mean_suicides_per_100k, color = country)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression trend lines without confidence intervals
  facet_wrap(~ country) +  # Create individual plots for each country
  labs(
    title = "12 Countries with the Most Increasing Trends in Suicide Rates",
    subtitle = "Based on significant trends (p < 0.05)",
    x = "Year",
    y = "Suicides per 100k"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # No legend, since we're using facets
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 12)  # Text size for facet labels
  )

#Extracting the top 12 countries with the steepest decreasing trends
top_12_decreasing_countries <- trends %>%
  arrange(estimate) %>%
  head(12) %>%
  pull(country)  # Get the country names

#Plotting the 12 countries with the most decreasing trends
ggplot(country_year %>%
         filter(country %in% top_12_decreasing_countries),  # Filter for the top 12
       aes(x = year, y = mean_suicides_per_100k, color = country)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression trend lines without confidence intervals
  facet_wrap(~ country) +  # Create individual plots for each country
  labs(
    title = "12 Countries with the Most Decreasing Trends in Suicide Rates",
    subtitle = "Based on significant trends (p < 0.05)",
    x = "Year",
    y = "Suicides per 100k"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # No legend, since we're using facets
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 12)  # Text size for facet labels
  )


#Influence of economy on suicide rates
#-------------------------------------
#Correlation between GDP per capita and suicide rates
correlation <- cor(mydata$gdp_per_capita, mydata$suicides_per_100k_pop, use = "complete.obs")
print(correlation)  # Output the correlation coefficient

#Scatter plot with linear regression line
ggplot(mydata, aes(x = gdp_per_capita, y = suicides_per_100k_pop)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "GDP per Capita vs. Suicide Rates", x = "GDP per Capita ($)", y = "Suicides per 100k")

#Build a simple linear regression model
lm_model <- lm(suicides_per_100k_pop ~ gdp_per_capita, data = mydata)
summary(lm_model)  # Output the model summary


#Decision Tree
#--------------
# Step 1: Creating a Binary Variable for High Suicide Rates
mydata$high_suicide_rate <- ifelse(mydata$suicides_per_100k_pop > 10, "High", "Low")

# Step 2: Splitting the Data into Training and Testing Sets
set.seed(123)  # For reproducibility
train_indices <- createDataPartition(mydata$high_suicide_rate, p = 0.8, list = FALSE)  # 80% for training

train_data <- mydata[train_indices, ]  # Training set
test_data <- mydata[-train_indices, ]  # Testing set

# Step 3: Setting up Cross-Validation
ctrl <- trainControl(
  method = "cv",  # Cross-validation
  number = 10,  # 10-fold cross-validation
  classProbs = TRUE,  # Return class probabilities
  summaryFunction = twoClassSummary  # Use AUC, sensitivity, specificity
)

# Step 4: Training the Decision Tree
fit.cv <- train(
  high_suicide_rate ~ sex + age + gdp_per_capita + continent,  # Predictors
  data = train_data,  # Training set
  method = "rpart",  # Decision tree
  trControl = ctrl,  # Use cross-validation
  tuneLength = 30,  # Tune 30 different hyperparameters
  metric = "ROC"  # Use ROC (Receiver Operating Characteristic) as the evaluation metric
)

# Step 5: Plotting the Decision Tree
rpart.plot(fit.cv$finalModel, extra = 106, under = TRUE, fallen.leaves = TRUE)  # Plot the decision tree

# Step 6: Evaluating the Model with Testing Data
# Ensure predictions and reference are factors with the same levels
expected_levels <- c("High", "Low")  # Define the expected levels
predictions <- predict(fit.cv, test_data)  # Make predictions
predictions <- factor(predictions, levels = expected_levels)  # Ensure correct levels
reference <- factor(test_data$high_suicide_rate, levels = expected_levels)  # Convert reference to factor

#Calculate the confusion matrix
conf_matrix <- confusionMatrix(predictions, reference)  # Confusion matrix
#Calculate the ROC curve and AUC
roc_curve <- roc(test_data$high_suicide_rate, predict(fit.cv, test_data, type = "prob")[, "High"])

#Printing evaluation metrics
print(conf_matrix)
#Plot the ROC curve
plot(roc_curve, col = "blue", main = "ROC Curve")
#Calculate and display the AUC
auc_value <- auc(roc_curve)
print(auc_value)  # AUC should be between 0 and 1

#Finding Associations
#---------------------
#Build a rule-based classifier to predict high or low suicide rates
oner_model <- OneR(high_suicide_rate ~ sex + age + gdp_per_capita + continent, data = mydata)

#Display the rules generated by OneR
summary(oner_model)

