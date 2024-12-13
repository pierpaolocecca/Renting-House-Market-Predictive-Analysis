### RSCRIPT ###

#### Rscript Final Project Brazilian Houses ####

#### OPPA Group
#### Andrea Contino, Omar Regragui, Piergiorgio Zannella & Pierpaolo Ceccarelli

#-------------------------------------------------------------------------------

# 0. Loading packages

# Function to check and install packages
check_and_install <- function(package){
  if (!package %in% installed.packages()) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

# Using the function to install and load the packages
packages <- c("dplyr", "ggplot2", "cowplot", "corrplot", "GGally", 
              "gridExtra", "glmnet", "MASS", "caret", "randomForest", 
              "mgcv", "purrr","mice", "cowplot","tidyverse", "factoextra", 
              "cluster", "patchwork", "RColorBrewer")
lapply(packages, check_and_install)

# 1. Understanding the Dataset

# Importing the dataset
raw_data <- read.csv("BrazHousesRent.csv")

# Head and structure of the dataset
head(raw_data)
str(raw_data)

# 1.1 Data Cleaning and Preparation

# Floor variable is a character but it should be a number so we convert it
raw_data$floor <- as.numeric(raw_data$floor)

# Looking for missing values
sapply(raw_data, function(x) sum(is.na(x))) # floor variables contains nulls

# Treating null in the floor variable
data <- raw_data

# Removing duplicate values if any
data <- data[!duplicated(data),]  #dataframe with no duplicates

# Creating the imputation object and using a simple pmm to fill floor's nulls
set.seed(123)
imp_obj <- mice(data, method = "pmm")
imputed <- complete(imp_obj)[,"floor"]

# Replacing the null values with the obtained values for the floor variable
data$floor[is.na(data$floor)] <- imputed[is.na(data$floor)]

# Factoring the other categorical columns
data$city <- as.factor(data$city)
data$animal <- as.factor(data$animal)
data$furniture <- as.factor(data$furniture)

# Renaming columns with clearer names
data <- data %>% rename(
  hoa = hoa..R.., rent = rent.amount..R.., 
  proptax = property.tax..R.., fireins = fire.insurance..R..
  )

# Cleaned dataframe summary
summary(data)

#-------------------------------------------------------------------------------

# 2. EDA Analysis

# Boxplot and histograms with density lines for interesting numerical columns
num_cols <- sapply(data, is.numeric)
cat_cols <- sapply(data, is.factor)
numcols1 <- c("area", "hoa", "rent", "proptax", "fireins")

# Storing the plots
p_boxplot <- list()
p_boxplot1 <- list()
p_histogram <- list()
p_histogram1 <- list()

# Creating the plots
for (col in names(data[numcols1])) {
  # Boxplot
  p_boxplot[[col]] <- ggplot(data, aes(y = !!sym(col))) +
    geom_boxplot(fill = "#ADD8E6", alpha = 0.5, outlier.color = "#8B0000",
    outlier.shape = 1) +labs(title = paste0(col," boxplot"), x = "") + 
    theme_bw()
  
  # Histogram
  p_histogram[[col]] <- ggplot(data, aes(x = !!sym(col))) +
    geom_histogram(fill = "#ADD8E6", alpha = 0.5) +
    geom_freqpoly(color = "#ADD8E6", linewidth = 0.05) +
    labs(title = paste0(col," hist"), y = "", x = "") +
    theme_bw()
  
  # Boxplot with log scaled data
  p_boxplot1[[col]] <- ggplot(log(data[,numcols1]), aes(y = !!sym(col))) +
    geom_boxplot(fill = "#ADD8E6", alpha = 0.5, outlier.color = "#8B0000",
    outlier.shape = 1) + labs(title = paste0("log scaled Boxplot ", col), 
    x = "") +theme_bw()
  
  # Histogram with scaled data
  p_histogram1[[col]] <- ggplot(log(data[,numcols1]), aes(x = !!sym(col))) +
    geom_histogram(fill = "#ADD8E6", alpha = 0.5) +
    geom_freqpoly(color = "#ADD8E6", linewidth = 0.05) +
    labs(title = paste0("log scaled hist ", col), y = "", x = "") +
    theme_bw() 
  
  # Extract ggplot objects from lists
  plot1 <- p_boxplot[[col]]
  plot2 <- p_histogram[[col]]
  plot3 <- p_boxplot1[[col]]
  plot4 <- p_histogram1[[col]]
  
  # Use the extracted ggplot objects
  print(plot_grid(plot1, plot2, plot3, plot4, ncol = 4))
}

# Removing the outliers (some houses data are questionable)

# Columns to consider for outlier detection
cols <- c("area", "hoa","rent", "proptax")
data_out <- data[, cols]

# Using z-scores method
z_scores <- apply(data_out, 2, 
            function(x) abs((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)))

# Identify rows with at least one z-score greater than 3 (considered outliers)
outlier_rows <- row.names(data_out)[apply(z_scores, 1, function(x) any(x > 3))]

# Print the number of outliers detected
cat("Number of outliers detected:", length(outlier_rows), "\n")

# Creating the final dataframe without outliers
final_data <- data[!(rownames(data) %in% outlier_rows), ]

# Print which was the most common rent value among the outliers
cat("Most common rent value among the outliers:", 
    names(sort(table(data[rownames(data) %in% outlier_rows, "rent"]), 
               decreasing = TRUE)[1]), "\n")

# Print how many times the most common rent value appeared among the outliers
cat("Number of times the most common rent value appeared among the outliers:", 
    max(table(data[rownames(data) %in% outlier_rows, "rent"]), "\n"))

# To visualize the difference we are creating the boxplots and the q-q plots of the rent distribution with and with no outliers
par(mfrow=c(2,2))
options(repr.plot.width=12, repr.plot.height=8)
boxplot(data$rent, col = "#ADD8E6", horizontal = T, 
        main = "Rent - Before Removing Outliers")
qqnorm(data$rent)

boxplot(final_data$rent, col = "#98FB98", horizontal = T,
        main = "Rent - After Removing Outliers")
qqnorm(final_data$rent)

# Rent ditribution by city
pal <- brewer.pal(7, "Reds")

# Average rent by city
mu <- final_data %>%
  filter(rent < 2000) %>%
  group_by(city) %>%
  summarise(grp.mean = mean(area))

options(repr.plot.width = 12, repr.plot.height = 7)

final_data %>%
  ggplot(aes(x = rent, color = city)) +
  geom_density(lwd = 3) +
  scale_color_manual(values = pal) +
  geom_vline(data = mu, aes(xintercept = grp.mean, color = city),
             linetype = "dashed", lwd = 2) +
  labs(title = "Rent Distributions by City", x = "Rent") +
  theme_light(base_size = 18) +
  theme(legend.position = "bottom")

#-------------------------------------------------------------------------------
  
# 3. Correlation among variables and Fitting only the most correlated variables

# Creating a correlation matrix
  
# Correlation matrix with numerical variables
corr_matrix <- cor(select_if(final_data, is.numeric))

# Plotting the heatmap
layout(matrix(c(1,1), nrow = 2, ncol = 2), widths = 5, heights = 5)
corr <- corrplot(corr_matrix, method ="color", order = "hclust",
            addCoef.col = "#1a1a1a", tl.col = "#1a1a1a",
            col = colorRampPalette(c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", 
                                     "#E0F3F8", "#FFFFBF", "#FEE090", "#FDAE61", 
                                     "#F46D43", "#D73027", "#A50026"))(200))
-
# Check the correlation values for the target variable "rent"
corr_matrix["rent",]

# Fitting a simple regression model to the numerical top 4 correlated variables to the target "rent"

# Find the top 4 correlated variables with rent
top_correlated_vars <- sort(corr_matrix["rent",], decreasing = T)[2:5]
top_correlated_vars_list <- names(top_correlated_vars)
target <- "rent"

# Fitting a simple regression (conf.int. at 95% shown)
p_list <- list() 
for (i in 1:length(top_correlated_vars_list)) {
  p <- ggplot(final_data, aes(x = !!sym(top_correlated_vars_list[i]), 
                              y = !!sym(target))) +
    geom_point(cex = 3, pch = 1, stroke = 2, color="#FFA07A") +
    geom_smooth(method = "lm", color = "#8B0000", lwd = 1, formula = "y~x", 
         se = TRUE, level = 0.95, linewidth = 1) + theme_light(base_size = 16) +
    ggtitle(paste("Scatter plot of", top_correlated_vars_list[i], "vs", target))
  
  p_list[[i]] <- p
}

# Showing plots
grid.arrange(grobs = p_list, ncol = 2) 

# Closer look into the most correlated variable "fireins" VS the target variable
p_list[[1]]

# Categorical variables inspection 

# Boxplots for rental prices by category with red outliers
newcol <- c("furniture", "city","animal")
plots <- list()

for (col in names(final_data[newcol])) {
  p_box <- ggplot(final_data, aes(x = !!sym(col), 
                                  y = rent, fill = factor(.data[[col]]))) +
    geom_boxplot(outlier.shape = 1,outlier.size = 2,outlier.color = "#8B0000") +
    labs(x = col, y = "Rent Amount") +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 55, size = 8, hjust = 1),
                       legend.text = element_text(size = 8), 
                       legend.title = element_text(size = 10))
  
  p_hist <- ggplot(final_data, aes(x = !!sym(col))) +
    geom_bar(fill = "#F46D43", alpha = 0.75) +
    labs(title = "Category Frequency", y = "Frequency", x = "Category") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 55, size = 8, hjust = 1),
          legend.text = element_text(size = 8), 
          legend.title = element_text(size = 10))
  
  plots[[col]] <- arrangeGrob(p_box, p_hist, nrow = 2)
}

# Arranging and displaying the plots
final_plot <- grid.arrange(grobs = plots, ncol = 3)
print(final_plot)

#-------------------------------------------------------------------------------

# 4. Testing with lower-dimentional models and analysing the impact

# To achieve it we used linear models with AIC

set.seed(123) # For reproducibility

# Split the data into training and valid(test) sets (80-20 as usual)
train_indices <- sample(1:nrow(final_data), 0.8*nrow(final_data))
train_data <- final_data[train_indices, ]
test_data <- final_data[-train_indices, ]

# Setting up for the models
predictors <- setdiff(names(final_data),c("rent"))
train_x <- train_data[,predictors]
train_y <- train_data$rent
test_x <- test_data[,predictors]
test_y <- test_data$rent

# Creating a dataframe to store all models performance indicators
performance_df <- data.frame(Model = character(), RMSE = numeric(), 
                                 R2 = numeric(), stringsAsFactors = FALSE)
colnames(performance_df) <- c("Model", "RMSE", "R2")

# Creating a dataframe to store the AIC model performance indicators
AIC_performance_df <- data.frame(Model = character(), RMSE = numeric(), 
                             R2 = numeric(), stringsAsFactors = FALSE)

#AIC criterion

# With the top 2 correlated variables "fireins" and "area"
lm_aic1 <- stepAIC(lm(rent ~ fireins + area, data = train_data), 
                   direction = "both", trace = FALSE)

# Without the variable "fireins"
lm_aic2 <- stepAIC(lm(rent ~ hoa + proptax + area + rooms + city + bathroom + 
                        floor + parking.spaces + furniture + animal, 
                      data = train_data), direction = "both", trace = FALSE)

# With all the variables
lm_aic3 <- stepAIC(lm(rent ~ ., data = train_data), 
                   direction = "both", trace = FALSE)

# With some feauture engineering (multiplying closely related variables)
lm_aic4 <- stepAIC(lm(rent ~ hoa*proptax + area*rooms + city + bathroom + 
                        parking.spaces + fireins + furniture, 
                      data = train_data), direction = "both", trace = FALSE)

# Function to calculate R2
calculate_R2 <- function(y_true, y_pred) {
  y_mean <- mean(y_true)
  ss_total <- sum((y_true - y_mean)^2)
  ss_residual <- sum((y_true - y_pred)^2)
  R2 <- 1 - (ss_residual / ss_total)
  return(R2)
}

# Top 2 Model
predictions_aic1 <- predict(lm_aic1, newdata = test_x)
rmse_aic1 <- sqrt(mean((predictions_aic1 - test_y)^2))
R2_aic1 <- calculate_R2(test_y, predictions_aic1)
AIC_performance_df <- rbind(AIC_performance_df, 
                        c("Top 2 AIC Model", rmse_aic1, R2_aic1))

# No fire insurence Model
predictions_aic2 <- predict(lm_aic2, newdata = test_x)
rmse_aic2 <- sqrt(mean((predictions_aic2 - test_y)^2))
R2_aic2 <- calculate_R2(test_y, predictions_aic2)
AIC_performance_df <- rbind(AIC_performance_df, 
                        c("No fireins AIC Model", rmse_aic2, R2_aic2))

# Complete Model
predictions_aic3 <- predict(lm_aic3, newdata = test_x)
rmse_aic3 <- sqrt(mean((predictions_aic3 - test_y)^2))
R2_aic3 <- calculate_R2(test_y, predictions_aic3)
AIC_performance_df <- rbind(AIC_performance_df, 
                        c("Complete AIC Model", rmse_aic3, R2_aic3))

# Feature Engineered Model
predictions_aic4 <- predict(lm_aic4, newdata = test_x)
rmse_aic4 <- sqrt(mean((predictions_aic4 - test_y)^2))
R2_aic4 <- calculate_R2(test_y, predictions_aic4)
AIC_performance_df <- rbind(AIC_performance_df, 
                        c("FE AIC Model", rmse_aic4, R2_aic4))
performance_df <- rbind(performance_df, 
                      c("LM AIC Model", rmse_aic4, R2_aic4))

# Summary of the models
summary(lm_aic1)
summary(lm_aic2)
summary(lm_aic3)
summary(lm_aic4)

# Showing the performances of the models
colnames(AIC_performance_df) <- c("Model", "RMSE", "R2")
AIC_performance_df <- AIC_performance_df %>% arrange(RMSE)
AIC_performance_df
AIC_performance_df$RMSE <- as.numeric(AIC_performance_df$RMSE)
AIC_performance_df$R2 <- as.numeric(AIC_performance_df$R2)

# Plot for RMSE
p1 <- ggplot(AIC_performance_df, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(RMSE,2)), vjust = -0.5, size = 3) +
  labs(title = "RMSE Model Performances", y = "RMSE", x = "Model") +
  theme_light(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill=FALSE)

# Plot for R2
p2 <- ggplot(AIC_performance_df, aes(x = Model, y = R2, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(R2,5)), vjust = -0.5, size = 3) +
  labs(title = "R-Squared Model Performances", y = "R2", x = "Model") +
  theme_light(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Arranging the plots in a single grid
grid.arrange(p1, p2, ncol = 2)

# Checking if residuals are normally distributed

# Creating the grid for the visualizzation
par(mfrow = c(2, 2))

# Top 2 Model
residuals_aic1 <- residuals(lm_aic1)
hist(residuals_aic1, breaks = 20, 
     main = "Residuals - Top 2 Model", xlab = "Residuals")


# No fire insurence Model
residuals_aic2 <- residuals(lm_aic2)
hist(residuals_aic2, breaks = 20, 
     main = "Residuals -  No fire insurence Model", xlab = "Residuals")


# Complete Model
residuals_aic3 <- residuals(lm_aic3)
hist(residuals_aic3, breaks = 20, 
     main = "Residuals - Complete Model", xlab = "Residuals")

# Feature Engineered Model
residuals_aic4 <- residuals(lm_aic4)
hist(residuals_aic4, breaks = 20, 
     main = "Residuals - Feature Engineered Model", xlab = "Residuals")

# Resetting the layout to default
par(mfrow = c(1, 1))

#-------------------------------------------------------------------------------

# 5. Finding the best model to predict rent cost

# Elastic Net

# Creating a dataframe to store model performance indicators
net_performance_df <- data.frame(Model = character(), RMSE = numeric(), 
                             R2 = numeric(), stringsAsFactors = FALSE)

# We encode cathegorical variables because Elastic Net cannot have factors
train_city_encoded <- model.matrix(~ city - 1, data = train_data)
test_city_encoded <- model.matrix(~ city - 1, data = test_data)

train_animal_encoded <- model.matrix(~ animal - 1, data = train_data)
test_animal_encoded <- model.matrix(~ animal - 1, data = test_data)

train_furniture_encoded <- model.matrix(~ furniture - 1, data = train_data)
test_furniture_encoded <- model.matrix(~ furniture - 1, data = test_data)

# Encoding variables with the remaining predictors
train_xnet <- cbind(train_data[, -which(names(train_data) %in% 
                                        c("rent","city","animal","furniture"))], 
                    train_city_encoded, train_animal_encoded, 
                    train_furniture_encoded)
test_xnet <- cbind(test_data[, -which(names(test_data) %in% 
                                        c("rent","city","animal","furniture"))], 
                    test_city_encoded, test_animal_encoded, 
                    test_furniture_encoded)

# Scaling numerical variables
numeric_vars1 <- c("area","rooms","bathroom","parking.spaces","floor",
                   "hoa","proptax","fireins")

# Scale the numeric variables
scaled_train_x2net <- train_xnet
scaled_train_x2net[, numeric_vars1] <- scale(train_xnet[, numeric_vars1])
scaled_test_x2net <- test_xnet
scaled_test_x2net[, numeric_vars1] <- scale(test_xnet[, numeric_vars1])

# Convert the data frames to matrix format
train_xnet <- as.matrix(scaled_train_x2net)
test_xnet <- as.matrix(scaled_test_x2net)

# Tuning lambda with cv (we don't prioritize L1 or L2, mixed approach is best hence alpha = 0.5)
enet_model <- cv.glmnet(train_xnet, train_y, alpha = 0.5, nfolds = 10)

#optimal lambda value (we are less conservative and we use min)
lambda_min <- enet_model$lambda.min

# Fitting the model
enet_model_fit <- glmnet(train_xnet, train_y, alpha = 0.5, lambda = lambda_min)

# Making the predictions
predictions_enet <- predict(enet_model_fit, newx = test_xnet)

# Elastic Net coefficients
coef(enet_model_fit)

# Appending model performance
r2_enet <- calculate_R2(test_y, predictions_enet)
rmse_enet <- sqrt(mean((predictions_enet - test_y)^2))
performance_df <- rbind(performance_df, c("Elastic Net", rmse_enet, r2_enet))
colnames(net_performance_df) <- c("Model", "RMSE", "R2")
net_performance_df <- rbind(net_performance_df, 
                            c("Elastic Net", rmse_enet, r2_enet))
net_performance_df

#-------------------------------------------------------------------------------

# GAM Splines

gam_performance_df <- data.frame(Model = character(), RMSE = numeric(), 
                                 R2 = numeric(), stringsAsFactors = FALSE)

# Splines functions applied to variables less correlated with target(likely non-linear relationships!))
gam.spline <- gam(rent ~ s(hoa) + s(proptax) + area + rooms + city + 
                      bathroom + floor + parking.spaces + fireins + 
                      furniture, data = train_data)
summary(gam.spline)

#performance GAM complete
predictionsgam <- predict(gam.spline, newdata = test_x)
rmserfgam <- sqrt(mean((predictionsgam - test_y)^2))
r_squaredgam <- cor(predictionsgam, test_y)^2

gam_performance_df<-rbind(gam_performance_df,c("Gam", rmserfgam, r_squaredgam))

#second model aims at mitigating correlation among predictors!
gam.spline1 <- gam(rent ~ s(hoa * proptax) + s(area * rooms) + city + 
                     bathroom + floor + parking.spaces + s(fireins) + 
                     furniture, data = train_data)
summary(gam.spline1)

# predictions gam1
predictionsgam1 <- predict(gam.spline1, newdata = test_x)
rmserfgam1 <- sqrt(mean((predictionsgam1 - test_y)^2))
r_squaredgam1 <- cor(predictionsgam1, test_y)^2
performance_df <- rbind(performance_df, 
                        c("Gam Spline", rmserfgam1, r_squaredgam1))
gam_performance_df <- rbind(gam_performance_df,c("Gam with FE", 
                                          rmserfgam1, r_squaredgam1))
colnames(gam_performance_df) <- c("Model", "RMSE", "R2")
gam_performance_df

# Plot for RMSE
p1 <- ggplot(gam_performance_df, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(as.numeric(RMSE),2)), vjust = -0.5, size = 3) +
  labs(title = "RMSE Model Performances", y = "RMSE", x = "Model") +
  theme_light(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill=FALSE)

# Plot for R2
p2 <- ggplot(gam_performance_df, aes(x = Model, y = R2, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(as.numeric(R2),5)), vjust = -0.5, size = 3) +
  labs(title = "R-Squared Model Performances", y = "R2", x = "Model") +
  theme_light(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Arranging the plots in a single grid
grid.arrange(p1, p2, ncol = 2)

#-------------------------------------------------------------------------------

# Random Forest

# Setting up a dataframe for the metrics
RF_performance_df <- data.frame(Model = character(), RMSE = numeric(), 
                             R2 = numeric(), stringsAsFactors = FALSE)

set.seed(123) # To ensure reproducibility

# increasing mtry to equal all the predictor variables is bagging!
randomForest <- randomForest(x = train_x, y = train_y,
                                   ntrees = 500,
                                   mtry = 4);

# Predictions and measures
predictionsrfbag <- predict(randomForest, newdata = test_x)
rmserfbag <- sqrt(mean((predictionsrfbag - test_data$rent)^2))
r_squaredrf <- cor(predictionsrfbag, test_data$rent)^2
RF_performance_df <- rbind(RF_performance_df, c("Random forest Complete", 
                                          rmserfbag, r_squaredrf))


# Random forest without fireins
predictors_nofireins <- setdiff(names(train_x),c("fireins"))
train_x_nofireins <- train_x[,predictors_nofireins]

randomForest_nofireins <- randomForest(x = train_x_nofireins, y = train_y,
                                    ntrees = 500,
                                    mtry = 4);

# Predictions
predictionsrfbag_nofireins <- predict(randomForest_nofireins, newdata = test_x)
rmserfbag_nofireins <- sqrt(mean((predictionsrfbag_nofireins-test_data$rent)^2))
r_squaredrf_nofireins <- cor(predictionsrfbag_nofireins, test_data$rent)^2
RF_performance_df <- rbind(RF_performance_df, c("Random forest no fireins", 
                                          rmserfbag_nofireins, 
                                          r_squaredrf_nofireins))


# Random forest with feature engineering (multiplying closely related variables)
FE_train_data <- train_data
FE_test_data <- test_data
FE_train_data$area_rooms <- FE_train_data$area * FE_train_data$rooms
FE_train_data$hoa_proptax <- FE_train_data$hoa * FE_train_data$proptax
FE_test_data$area_rooms <- FE_test_data$area * FE_test_data$rooms
FE_test_data$hoa_proptax <- FE_test_data$hoa * FE_test_data$proptax

# Predictors
FE_predictors <- setdiff(names(FE_train_data),
                         c("rent","area","hoa","rooms","proptax"))
FE_train_x <- FE_train_data[,predictors]
FE_train_y <- FE_train_data$rent
FE_test_x <- FE_test_data[,predictors]
FE_test_y <- FE_test_data$rent

# Running the random forest with feature engineering
FE_randomForest <- randomForest(x = FE_train_x, y = FE_train_y,
                                ntrees = 500,
                                mtry = 4);

# Predictions
FE_predictionsrfbag <- predict(FE_randomForest, newdata = FE_test_x)
FE_rmserfbag <- sqrt(mean((FE_predictionsrfbag - FE_test_data$rent)^2))
FE_r_squaredrf <- cor(FE_predictionsrfbag, FE_test_data$rent)^2
RF_performance_df <- rbind(RF_performance_df, c("Random forest with FE", 
                                                FE_rmserfbag, FE_r_squaredrf))


# Random Forest with Hyperparameter Tuning
HT_randomForest <- randomForest(x = FE_train_x, FE_train_y,
                             ntrees = 500,
                             mtry = 8);

# Predictions and measures
HT_predictionsrfbag <- predict(HT_randomForest, newdata = FE_test_x)
HT_rmserfbag <- sqrt(mean((HT_predictionsrfbag - test_data$rent)^2))
HT_r_squaredrf <- cor(HT_predictionsrfbag, test_data$rent)^2
RF_performance_df <- rbind(RF_performance_df, c("Random forest with FE & HT", 
                                                HT_rmserfbag, HT_r_squaredrf))
performance_df <- rbind(performance_df, c("Random forest", 
                                                HT_rmserfbag, HT_r_squaredrf))

# Returning models' performances
colnames(RF_performance_df) <- c("Model", "RMSE", "R2")
RF_performance_df <- RF_performance_df %>% arrange(RMSE)
RF_performance_df
RF_performance_df$RMSE <- as.numeric(RF_performance_df$RMSE)
RF_performance_df$R2 <- as.numeric(RF_performance_df$R2)


# Plot for RMSE
p1 <- ggplot(RF_performance_df, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(as.numeric(RMSE),2)), vjust = -0.5, size = 3) +
  labs(title = "RMSE Model Performances", y = "RMSE", x = "Model") +
  theme_light(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill=FALSE)

# Plot for R2
p2 <- ggplot(RF_performance_df, aes(x = Model, y = R2, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(as.numeric(R2),5)), vjust = -0.5, size = 3) +
  labs(title = "R-Squared Model Performances", y = "R2", x = "Model") +
  theme_light(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Arranging the plots in a single grid
grid.arrange(p1, p2, ncol = 2)

#-------------------------------------------------------------------------------

# 6. Conclusions on the best model for rent prediction

# Our models scores
colnames(performance_df) <- c("Model", "RMSE", "R2")
performance_df <- performance_df %>% arrange(RMSE)
performance_df

# Plot for RMSE
p1 <- ggplot(performance_df, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(as.numeric(RMSE),2)), vjust = -0.5, size = 3) +
  labs(title = "RMSE Model Performances", y = "RMSE", x = "Model") +
  theme_light(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill=FALSE)

# Plot for R2
p2 <- ggplot(performance_df, aes(x = Model, y = R2, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(as.numeric(R2),5)), vjust = -0.5, size = 3) +
  labs(title = "R-Squared Model Performances", y = "R2", x = "Model") +
  theme_light(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Arranging the plots in a single grid
grid.arrange(p1, p2, ncol = 2)

#lists to store performance results
plot1 <- list()
plot2 <- list()
plot3 <- list()

# Bootstrapping for each model and compute RMSE and R^2
num_iterations <- 10  
set.seed(123)  
for (i in 1:num_iterations) {
  
  # Split data into train and test sets (taking 30% sample data and splitting 80/20)
  train_datasample <- final_data[sample(nrow(final_data), 
                                        size = round(0.3 * nrow(final_data))), ]
  train_indices <- sample(nrow(train_datasample), 
                    size = floor(0.8 * nrow(train_datasample)), replace = FALSE)
  train_setcv <- train_datasample[train_indices, ]
  test_setcv <- train_datasample[-train_indices, ]
  
  traincv <- train_setcv[,-which(names(train_datasample) == "rent")]
  test_cv <- test_setcv[,-which(names(train_datasample) == "rent")]
  testy_cv <- test_setcv$rent
  
  # Random Forest with bagging
  randomForest.bagcv <- randomForest(x = traincv, y = train_setcv$rent,
                                       ntrees = 500,
                                       mtry = 11)
  
  predictionsrfbagcv <- predict(randomForest.bagcv, newdata = test_cv)
  rmserfbagcv <- 0
  r_squaredrfcv <- 0
  rmserfbagcv <- sqrt(mean((predictionsrfbagcv - testy_cv)^2))
  r_squaredrfcv <- cor(predictionsrfbagcv, testy_cv)^2
  plot1[i] <- rmserfbagcv
  
  #Best AIC
  lm_aic_cv <- stepAIC(lm(rent ~ hoa*proptax + area*rooms + city + bathroom +
                          parking.spaces + fireins + furniture, 
                          data = train_setcv), direction ="both", trace = FALSE)
  predictions_aic_cv <- predict(lm_aic_cv, newdata = test_cv)
  rmse_aic_cv <- 0
  R2_aic_cv <- 0
  rmse_aic_cv <- sqrt(mean((predictions_aic_cv - testy_cv)^2))
  R2_aic_cv <- calculate_R2(testy_cv, predictions_aic_cv)
  plot2[i] <- rmse_aic_cv
  
  # GAM with interaction terms
  m.gam.spline1cv <- gam(rent ~ s(hoa * proptax) + s(area * rooms) 
                         + city + bathroom + floor + parking.spaces + s(fireins) 
                         + furniture, data = train_setcv)
  predictionsgam1 <- predict(m.gam.spline1cv, newdata = test_cv)
  rmserfgam1cv <- 0
  r_squaredgam1cv <- 0
  rmserfgam1cv <- sqrt(mean((predictionsgam1 - testy_cv)^2))
  r_squaredgam1cv <- cor(predictionsgam1, testy_cv)^2
  plot3[i] <- rmserfgam1cv
  
}

# Plotting the variation of RMSE for each model
# Lists to vector to compute CI
plot1 <- unlist(plot1)
plot2 <- unlist(plot2)
plot3 <- unlist(plot3)

# Calculate mean and standard deviation of predictions for each model
mean_plot1 <- mean(plot1)
sd_plot1 <- sd(plot1)

mean_plot2 <- mean(plot2)
sd_plot2 <- sd(plot2)

mean_plot3 <- mean(plot3)
sd_plot3 <- sd(plot3)

# Calculate confidence intervals (95%)
ci_plot1 <- quantile(plot1, probs = c(0.025, 0.975))
ci_plot2 <- quantile(plot2, probs = c(0.025, 0.975))
ci_plot3 <- quantile(plot3, probs = c(0.025, 0.975))

# Plot mean predictions with confidence intervals
plot_data <- data.frame(
  Model = c("Random Forest", "AIC Linear Model", "GAM spline"),
  Mean = c(mean_plot1, mean_plot2, mean_plot3),
  Lower_CI = c(ci_plot1[1], ci_plot2[1], ci_plot3[1]),
  Upper_CI = c(ci_plot1[2], ci_plot2[2], ci_plot3[2])
)

# Plot
p <- ggplot(plot_data, aes(x = Model, y = Mean)) +
  geom_errorbar(aes(ymin = Lower_CI,ymax=Upper_CI),width = 0.2,color="blue") +
  geom_point(color = "blue") +
  xlab("Model") +
  ylab("Predictions") +
  ggtitle("Average RMSE with Confidence Intervals")

print(p)

#-------------------------------------------------------------------------------

# 7. Clustering : Identifing clusters of similar convenient houses' rents

# k-Means

# Scaling the numerical variables data
numeric_vars1 <- c("area","rooms","bathroom","parking.spaces","floor",
                   "hoa","proptax","fireins")
data_scaled <- scale(final_data[,numeric_vars1])
data_scaled <- as.data.frame(data_scaled)

# Implementing the elbow method
k_values <- 1:15  # Range of k values to consider
withinss <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  kmeans_result <- kmeans(data_scaled, centers = k)
  withinss[i] <- kmeans_result$tot.withinss
}

# Plot the elbow curve
elb <- ggplot() +
  geom_line(aes(x = k_values, y = withinss), color = "#FFA04A") +
  geom_point(aes(x = k_values, y = withinss), color = "#FFA04A") +
  labs(x = "Number of Clusters k", y = "Within-cluster Sum of Squares") +
  ggtitle("Elbow Method for Optimal k") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# Silhouette score by k and elbow printed
silk <- fviz_nbclust(data_scaled, kmeans, method='silhouette') +
        ggtitle("Silhouette Method for Optimal k")

# Plot the two plots in a single grid
grid.arrange(elb, silk, ncol = 2)

# Plotting with 2 clusters

kmeans_model <- kmeans(data_scaled, centers = 2, nstart = 25)
clus2 <- fviz_cluster(kmeans_model, data = data_scaled, geom = "point",
                      main = paste("K-Means Clustering (k =", 2, ")"))

# Plotting with 3 clusters
kmeans_model2 <- kmeans(data_scaled, centers = 3, nstart = 25)
clus3 <- fviz_cluster(kmeans_model2, data = data_scaled, geom = "point",
                      main = paste("K-Means Clustering (k =", 3, ")"))

# Plotting the results
grid.arrange(clus2, clus3, ncol = 2)

# Printing the number of houses in each cluster
table(kmeans_model$cluster)
table(kmeans_model2$cluster)

#-------------------------------------------------------------------------------

# Hierarchical clustering

# By Euclidean distance works the best
dist_euc <- dist(data_scaled, method = "euclidean")
hc_euclidean <- hclust(dist_euc, method = "ward.D2")

# Dendrogram (2 or 3 ideal)
plot(hc_euclidean, cex = 0.6, main = "Dendrogram (Euclidean distance)")

# Assessing the results for different k
silhouette_euclidean <- rep(0, 2)
for (k in 2:3) {
  
  # Compute cluster assignments for euclidean distance
  hc_euclidean_k <- cutree(hc_euclidean, k = k)
  
  # Compute silhouette score for euclidean distance
  sileucscores <- data.frame(silhouette(hc_euclidean_k, dist_euc))
  mean_sil_width <- mean(as.numeric(sileucscores$sil_width))         
  silhouette_euclidean[k - 1] <-  mean_sil_width
  
}
cat("Silhouette scores for euclidean distance:", silhouette_euclidean, "\n")
  
# Compute cluster assignments for euclidean distance
hc_euclidean_k2 <- cutree(hc_euclidean, k = 2)
hc_euclidean_k3 <- cutree(hc_euclidean, k = 3)
  
# Plot clusters for euclidean distance
plot_title <- paste0("Hierarchical Clusters (Euclidean distance) for k = 2")
plot_k2 <- fviz_cluster(list(data = data_scaled, 
                                       cluster = hc_euclidean_k2), 
                                  geom = "point", 
                                  palette = "jco", 
                                  main = plot_title) + theme_bw()

# Plot clusters for euclidean distance
plot_title <- paste0("Hierarchical Clusters (Euclidean distance) for k = 3")
plot_k3 <- fviz_cluster(list(data = data_scaled, 
                                       cluster = hc_euclidean_k3), 
                                  geom = "point", 
                                  palette = "jco", 
                                  main = plot_title) + theme_bw()

# Plot the cluster assignment results for k = 2 and k = 3
grid.arrange(plot_k2, plot_k3, ncol = 2)

# Printing the number of houses in each cluster
table(hc_euclidean_k2)
table(hc_euclidean_k3)

#-------------------------------------------------------------------------------

# 8. Conclusions on the Clustering process

data_new <- final_data

# 3 clusters
kmeans_result3 <- kmeans(data_scaled, centers = 3, nstart = 25)
data_new$clusters3 <- kmeans_result3$cluster

# 2 clusters
kmeans_result2 <- kmeans(data_scaled, centers = 2, nstart = 25)
data_new$clusters2 <- kmeans_result2$cluster

# Create the boxplot for city with regrouped 3 clusters

# By rent
box1 <- ggplot(data_new, aes(x = interaction(clusters3, city), 
                             y = rent, fill = as.factor(clusters3))) +
  geom_boxplot(color = "black") +
  labs(x = "clusters", y = "Rent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        plot.title = element_text(size = 12)) +
  ggtitle("Rental prices by city and cluster") +
  theme(legend.position = "none")

# By area
box2 <- ggplot(data_new, aes(x = interaction(clusters3, city), 
                             y = area, fill = as.factor(clusters3))) +
  geom_boxplot(color = "black") +
  labs(x = "clusters", y = "area") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        plot.title = element_text(size = 12)) +
  ggtitle("Houses' area by cluster and city") +
  theme(legend.position = "none")

# By furniture
box3 <- ggplot(data_new, aes(x = interaction(clusters3, furniture), 
                             y = rent, fill = as.factor(clusters3))) +
  geom_boxplot(color = "black") +
  labs(x = "clusters", y = "rent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        plot.title = element_text(size = 12)) +
  ggtitle("Houses' rent by cluster (furnished or not)") +
  theme(legend.position = "none")

box4 <- ggplot(data_new, aes(x = interaction(clusters3, furniture), 
                             y = area, fill = as.factor(clusters3))) +
  geom_boxplot(color = "black") +
  labs(x = "clusters", y = "area") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        plot.title = element_text(size = 12)) +
  ggtitle("Houses' area by cluster (furnished or not)") +
  theme(legend.position = "none")

box_null <- ggplot(data_new, aes(x = interaction(clusters3, furniture), 
                                 y = area, fill = as.factor(clusters3))) +
  geom_boxplot(color = "black") +
  labs(x = "clusters", y = "area") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(size = 10)) +
  guides(fill = guide_legend(title = "Clusters")) +
  ggtitle("Houses' area by cluster (furnished or not)") 

# Extract legend
legend <- get_legend(box_null)

# Arranging plots
grid.arrange(box1, box2, legend, box3, box4, ncol = 3)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Extra tests that we performed during the coding process that are not included

# This code was only meant to be used for testing and not necessarily in the final code

# Outliers detection

# IQR method to detect outliers
cols <- c("proptax", "area", "hoa", "fireins", "rent")
data_log <- log(data[, cols])
data_log[data_log == -Inf] <- 0
data_out <- data_log

# Using IQR step to identify outliers

# IQR for each numeric column
iqr_values <- apply(data_out, 2, stats::IQR)

# Lower and upper bounds using IQR method
lower_bounds <- apply(data_out, 2, function(x) quantile(x, 0.25) - 1.5 * stats::IQR(x))
upper_bounds <- apply(data_out, 2, function(x) quantile(x, 0.75) + 1.5 * stats::IQR(x))

# Rows with outliers
outlier_rows <- row.names(data_out)[apply(data_out, 1, function(x) any(x < lower_bounds | x > upper_bounds))]

# Print the number of outliers detected (3380) 
cat("Number of outliers detected:", length(outlier_rows), "\n")

# In which city are the most outliers located?
data_out$city <- data$city
outliers_city <- data_out[outlier_rows, "city"]
table(outliers_city)

# RANDOM FOREST TESTING

# Define the range of mtry values to test
mtry_range <- 2:8

# Initialize the best mtry value and its corresponding error
best_mtry_value <- NA
best_error <- Inf

# Perform the grid search
for (mtry in mtry_range) {
  result <- tuneRF(FE_train_x, FE_train_y,
                   stepFactor = 2,
                   plot = TRUE,
                   ntreeTry = 500,
                   trace = TRUE,
                   improve = 0.05,
                   mtryStart = mtry)
  
  # Extract the error for the current mtry value
  current_error <- min(result)
  
  if (current_error < best_error) {
    best_mtry_value <- mtry
    best_error <- current_error
  }
}

# Print the best mtry value
print(paste("Best mtry value: ", best_mtry_value))

# Run the random forest with the best mtry value
best_randomForest <- randomForest(x = FE_train_x, y = FE_train_y,
                                  ntrees = 500,
                                  mtry = best_mtry_value)

# Predictions
best_predictions <- predict(best_randomForest, newdata = FE_test_x)
best_rmse <- sqrt(mean((best_predictions - FE_test_data$rent)^2))
best_r_squared <- cor(best_predictions, FE_test_data$rent)^2
best_rmse
best_r_squared

# Grid search Random forest
train_control <- trainControl(
  method = "cv",
  number = 10,
  search = "grid"
)

# Train the random forest model with hyperparameter tuning (takes ages)
rf_model <- train(train_y~.,
                  data = train_data,
                  method = "rf",
                  trControl = train_control,
                  tuneGrid = NULL
)
# Parameter grid for tuning
param_grid <- expand.grid(
  mtry = seq(2, 10, by = 2),  # Features randomly sampled as candidates at each split
  ntree = c(100, 200, 300, 400, 500),  # Number of trees
  nodesize = c(1, 10, by = 1)  # Minimum size of terminal nodes
)

# Reorder the columns in param_grid
param_grid <- param_grid[, c("mtry", "ntree", "nodesize")]

# Define the train control with cross-validation
train_control <- trainControl(
  method = "cv",
  number = 10,
  search = "grid"
)

# Train the random forest model with hyperparameter tuning
rf_model <- train(train_data$rent~.,
                  data = train_data,
                  method = "rf",
                  trControl = train_control,
                  tuneGrid = "param_grid"
)

# Best model and its performance
print(rf_model$bestTune)
print(rf_model$results)


# Random forest with and without bagging
m.randomForest <- randomForest(x = X_train_matrix, y = y_train_vector,
                               ntrees = 500,
                               mtry = 4); 

# Reduced number of mtry is Random Forest - the model can do either
m.randomForest.bag <- randomForest(x = X_train_matrix, y = y_train_vector,
                                   ntrees = 500,
                                   mtry = 11); # include all X vars

# Train the random forest model with default parameters
rf_model <- randomForest(train_data$rent ~ ., data = train_data)

# Make predictions on the test set
predictionsrf <- predict(rf_model, newdata = final_data)

# CLUSTERING TESTING

# Manhattan
dist_man <- dist(data_scaled, method = "manhattan")

# Euclidean
dist_euc <- dist(data_scaled, method = "euclidean")
hc_euclidean <- hclust(dist_euc, method = "ward.D2")

# 1- row correlation
dist_cor <- as.dist(1 - cor(t(data_scaled)))
hc_correlation <- hclust(dist_cor, method = "ward.D2")

# Plot dendograms
plot(hc_euclidean, cex = 0.6, main = "Dendrogram (Euclidean distance)")
plot(hc_correlation, cex = 0.6, main = "Dendrogram (1 - row correlation)")


# Initialize vectors to store silhouette scores
silhouette_euclidean <- rep(0, 3)
silhouette_correlation <- rep(0, 3)


# Iterate over different values of k
for (k in 2:4) {
  
  # Compute cluster assignments for euclidean distance
  hc_euclidean_k <- cutree(hc_euclidean, k = k)
  
  # Compute silhouette score for euclidean distance
  sileucscores <- data.frame(silhouette(hc_euclidean_k, dist_euc))
  mean_sil_width <- mean(as.numeric(sileucscores$sil_width))         
  silhouette_euclidean[k - 1] <-  mean_sil_width
  
  # Compute cluster assignments for 1 - row correlation
  hc_correlation_k <- cutree(hc_correlation, k = k)
  
  # Compute silhouette score for 1 - row correlation
  sileucscores2 <- data.frame(silhouette(hc_correlation_k, dist_cor))
  mean_sil_width2 <- mean(as.numeric(sileucscores2$sil_width))         
  
  silhouette_correlation[k - 1] <- mean_sil_width2
  
}

cat("Silhouette scores for euclidean distance:", silhouette_euclidean, "\n")
cat("Silhouette scores for 1 - row correlation:", silhouette_correlation, "\n") 

# Inspecting into clusters (either for k = 2/ k = 3, just need to change clusters 3 t clusters 2)
boh <- data_new[data_new$clusters3 == 2,]
boh1 <- data_new[dadata_new$clusters3 == 1,]
boh3 <- data_new[dadata_new$clusters3 == 3,]

# value counts
table(boh$furniture)
names(data_new)

# LINEAR AND PENALIZED MODEL TESTING

# AIC criterion
lm_aic <- stepAIC(lm(rent ~ ., data = final_data), 
                  direction = "both", trace = FALSE)
lm_aic2 <- stepAIC(lm(rent ~ ., data = final_data[,predictors]), 
                   direction = "both", trace = FALSE)

# BIC criterion
lm_bic <- stepAIC(lm(rent ~ ., data = final_data), 
                  direction = "both", k = log(nrow(final_data)), trace = FALSE)
lm_bic2 <- stepAIC(lm(rent ~ ., data = final_data[,predictors]), 
                   direction = "both", k = log(nrow(final_data)), trace = FALSE)

# Print the AIC-selected model
summary(lm_aic)
summary(lm_aic2)
AIC(lm_aic)
AIC(lm_aic2)

# Print the BIC-selected model
summary(lm_bic)
summary(lm_bic2)
BIC(lm_bic)
BIC(lm_bic2)

#alternative linear models (penalized lasso and ridge)

#lambda values 
grid <- seq(10, -3, length=100);
lamVals <- 10 ^ grid

# fit the ridge regression model (alpha = 0 for ridge)
m.ridge <- glmnet(x = train_x, y = train_y, alpha = 0,
                  lambda = lamVals, standardize=TRUE)

#Cross Validation for optimal lambda (for Prediction)  

m_cv_ridge <- cv.glmnet(x = train_x, y = train_y, alpha = 0, 
                        lambda = lamVals, standardize=TRUE);

ridge_fit()

# Clustering data insightfulness with 2 clusters

data_new <- final_data

# 3 clusters
kmeans_result3 <- kmeans(data_scaled, centers = 3, nstart = 25)
data_new$clusters3 <- kmeans_result3$cluster

# 2 clusters
kmeans_result2 <- kmeans(data_scaled, centers = 2, nstart = 25)
data_new$clusters2 <- kmeans_result2$cluster

# Create the boxplot for city with regrouped 3 clusters

# By rent
box1 <- ggplot(data_new, aes(x = interaction(clusters2, city), 
                             y = rent, fill = as.factor(clusters))) +
  geom_boxplot(color = "black") +
  labs(x = "clusters", y = "Rent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        plot.title = element_text(size = 12)) +
  ggtitle("Rental prices by city and cluster") +
  theme(legend.position = "none")

# By area
box2 <- ggplot(data_new, aes(x = interaction(clusters2, city), 
                             y = area, fill = as.factor(clusters2))) +
  geom_boxplot(color = "black") +
  labs(x = "clusters", y = "area") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        plot.title = element_text(size = 12)) +
  ggtitle("Houses' area by cluster and city") +
  theme(legend.position = "none")

# By furniture
box3 <- ggplot(data_new, aes(x = interaction(clusters2, furniture), 
                             y = rent, fill = as.factor(clusters2))) +
  geom_boxplot(color = "black") +
  labs(x = "clusters", y = "rent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        plot.title = element_text(size = 12)) +
  ggtitle("Houses' rent by cluster (furnished or not)") +
  theme(legend.position = "none")

box4 <- ggplot(data_new, aes(x = interaction(clusters2, furniture), 
                             y = area, fill = as.factor(clusters2))) +
  geom_boxplot(color = "black") +
  labs(x = "clusters", y = "area") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        plot.title = element_text(size = 12)) +
  ggtitle("Houses' area by cluster (furnished or not)") +
  theme(legend.position = "none")

box_null <- ggplot(data_new, aes(x = interaction(clusters2, furniture), 
                                 y = area, fill = as.factor(clusters2))) +
  geom_boxplot(color = "black") +
  labs(x = "clusters", y = "area") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(size = 10)) +
  guides(fill = guide_legend(title = "Clusters")) +
  ggtitle("Houses' area by cluster (furnished or not)") 

# Extract legend
legend <- get_legend(box_null)

# Arranging plots
grid.arrange(box1, box2, legend, box3, box4, ncol = 3)

#-------------------------------------------------------------------------------
