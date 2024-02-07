library(ggplot2)
library(ggpubr)
library(factoextra)
library(cluster)
#library(caret)

#Get data
medal_gdp_data <- read.csv("medal_pop_gdp_data_statlearn.csv")

##################### TASK-1 ###########################

#Storing columns data
gdp <- medal_gdp_data$GDP
medal_2012 <- medal_gdp_data$Medal2012
medal_2016 <- medal_gdp_data$Medal2016
population <- medal_gdp_data$Population
country <- medal_gdp_data$Country

#new data frame for prediction
medal_2016_df <- data.frame(GDP = gdp,Population = population)

#Linear regression model
model_2012 <- glm(Medal2012 ~ GDP+Population, data = medal_gdp_data, family = gaussian)
summary(model_2012)$coefficients[, 1:2]

#Predict medal count
predict_model1 <- predict(model_2012,medal_2016_df)

#R squared
rsquare_1 <- 1- (sum((predict_model1 - medal_gdp_data$Medal2016) ^ 2)/sum((medal_gdp_data$Medal2016 - mean(medal_gdp_data$Medal2016))^2))

#Extra
#cor(predict_model1, medal_gdp_data$Medal2016)
#mean(abs((medal_gdp_data$Medal2016-predict_model1)/medal_gdp_data$Medal2016))*100

# Scatter plot of actual vs. predicted medal count
ggplot(data = medal_gdp_data, aes(x = Medal2016, y = predict_model1)) +
  geom_point(size = 2, color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "red") +
  xlab("Actual Medal Count") +
  ylab("Predicted Medal Count")

# Histogram of residuals
ggplot(data = medal_gdp_data, aes(x = model_2012$residuals)) +
  geom_histogram(aes(y=..density..), colour="#E69F00", fill="#E69F00",position="identity", alpha=0.5) +
  geom_density(alpha=.3, fill="#E69F00", color="#E95F00") +
  xlab("Residuals") +
  ylab("Frequency") +
  scale_color_brewer(palette="Accent") + 
  theme_minimal()

# QQ plot of residuals
ggplot(data = medal_gdp_data, aes(sample = model_2012$residuals)) +
  stat_qq(color='#E95F00') +
  stat_qq_line(color='red') +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles")

##################### TASK-2 ###########################

#Store log values
log_gdp <- log(medal_gdp_data$GDP)
log_population <- log(medal_gdp_data$Population)
log_medal12 <- log(medal_2012)

#create data frame of log inputs 
log_medal_DataFrame <- data.frame(log_gdp,log_population,medal_2012,country)

#Create linear regression model from log inputs.
model_2012_log <- glm(medal_2012 ~ log_gdp+log_population, data = log_medal_DataFrame)

#check model
summary(model_2012_log)

#perform predictions and calculate r-square error to check performance
medal_2016_df_log <- data.frame(GDP = log_gdp,Population = log_population)
predict_model2 <- predict(model_2012_log,medal_2016_df_log)

rsquare_2 <- 1- (sum((predict_model2 - medal_gdp_data$Medal2016) ^ 2)/sum((medal_gdp_data$Medal2016 - mean(medal_gdp_data$Medal2016))^2))
cor(predict_model2, medal_2016)
mean(abs((medal_2016-predict_model2)/medal_2016)*100)

#below code shows all the plot for our models, continue hitting enter to see the different plots
plot(model_2012_log)

#plot predicted vs actual model count for our new model
ggplot(medal_gdp_data, aes(x = medal_2016, y = predict_model2)) + 
  geom_point(size = 3, color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  labs(x = "Actual Medal count in 2016 olympics", y = "Predicted Medal count in 2016 olympics") + 
  ggtitle("Actual vs. Predicted Medal Count for 2016")
#add below code for regression line in ggplot
#  +geom_smooth(method = "glm", formula = y ~ x, se = FALSE)

# Scatter plot of actual vs. predicted medal count
# ggplot(data = medal_gdp_data, aes(x = Medal2016, y = predict_model2)) +
#   geom_point(size = 2, color = "blue") +
#   geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "red") +
#   xlab("Actual Medal Count") +
#   ylab("Predicted Medal Count")

# Histogram of residuals
ggplot(data = medal_gdp_data, aes(x = model_2012_log$residuals)) +
  geom_histogram(aes(y=..density..), colour="#E69F00", fill="#E69F00",position="identity", alpha=0.5) +
  geom_density(alpha=.3, fill="#E69F00", color="#E95F00") +
  xlab("Residuals") +
  ylab("Frequency") +
  scale_color_brewer(palette="Accent") + 
  theme_minimal()

# QQ plot of residuals
ggplot(data = medal_gdp_data, aes(sample = model_2012_log$residuals)) +
  stat_qq(color='#E95F00') +
  stat_qq_line(color='red') +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles")

#Equations will be, 
#y = -49.363 + 5.545(GDP)
#y = -49.363 + 1.983(Population)


#####################TASK-3###########################

#To validate the optimal number of clusters, we can use the elbow method, which involves plotting the within-cluster sum of squares (WSS) against the number of clusters and choosing the number of clusters where the decrease in WSS begins to level off (i.e., the "elbow" of the plot). We can calculate the WSS using the kmeans function with the tot.withinss argument:
# Calculate within-cluster sum of squares for different numbers of clusters

# Choose a range of K values to test
K <- 1:10

# Fit K-Means models for each value of K and compute the within-cluster sum of squares (WCSS)
wcss <- sapply(K, function(k) {
  kmeans(medal_2016_df_log, k)$tot.withinss
})

# Plot the WCSS as a function of K
plot(K, wcss, type='b', pch=19, frame=FALSE,
     xlab='Number of clusters (K)', ylab='Within-cluster sum of squares (WCSS)',
     main='Elbow Method for Optimal K')

# Add a vertical line at the elbow point
elbow <- which(diff(wcss) < mean(diff(wcss))) + 1
abline(v=elbow, col='red')

library(NbClust)

nb <- NbClust(medal_2016_df_log, distance = "euclidean", min.nc = 3, max.nc = 10, method = "kmeans")
optimal_k <- nb$Best.nc

# Print the optimal K value
print(optimal_k)

#AIC_Score = data.frame(AIC_3 = matrix(nrow = 10),AIC_4 = matrix(nrow = 10),AIC_5 = matrix(nrow = 10), AIC_6 = matrix(nrow = 10))
set.seed(123)
kmean_model <- kmeans(medal_2016_df_log, centers = 3)


# # Dimension reduction using PCA
# res.pca <- prcomp(medal_gdp_data[,2:6],  scale = TRUE)
# # Coordinates of individuals
# ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# # Add clusters obtained using the K-means algorithm
# ind.coord$cluster <- factor(kmean_model$cluster)
# # Add Species groups from the original data sett
# ind.coord$Country <- medal_gdp_data$Country
# # Data inspection
# head(ind.coord)
# 
# # Percentage of variance explained by dimensions
# eigenvalue <- round(get_eigenvalue(res.pca), 1)
# variance.percent <- eigenvalue$variance.percent
# head(eigenvalue)
# 
# ggscatter(
#   ind.coord, x = "Dim.1", y = "Dim.2", 
#   color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
#   size = 1.5,  legend = "right", ggtheme = theme_bw(),
#   xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
#   ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
# ) +
#   stat_mean(aes(color = cluster), size = 4)

#PLot the clusters
fviz_cluster(kmean_model, data = medal_2016_df_log,
            palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
            geom = "point",
            ellipse.type = "convex", 
            ggtheme = theme_bw()
)

#Another method to plot clusters
# clusplot(medal_2016_df_log, kmean_model$cluster, color = TRUE, shade = TRUE, 
#          labels = 2, lines = 0)

kmean_model$size

modified_medal_2016_df <- data.frame(GDP = medal_2016_df$GDP,Population = medal_2016_df$Population,cluster = kmean_model$cluster,medal_2012,Actual_Medal2016=medal_2016,Medal2016=NA)

for (i in 1:max(kmean_model$cluster)) {
  cluster_i <- subset(modified_medal_2016_df, kmean_model$cluster == i)
  model_i <- glm(medal_2012 ~ GDP + Population, data = cluster_i)
  cat("Writing Summary for cluster ",i)
  print(AIC(model_i))
  #AIC_Score[i,j-2] <- model_i$aic
  modified_medal_2016_df$Medal2016[modified_medal_2016_df$cluster == i] <- predict(model_i, cluster_i)
}

rsquare_3 <- 1- (sum((modified_medal_2016_df$Medal2016 - medal_gdp_data$Medal2016) ^ 2)/sum((medal_gdp_data$Medal2016 - mean(medal_gdp_data$Medal2016))^2))
cor(modified_medal_2016_df$Actual_Medal2016, modified_medal_2016_df$Medal2016)^2
mean(abs((modified_medal_2016_df$Actual_Medal2016-modified_medal_2016_df$Medal2016)/modified_medal_2016_df$Actual_Medal2016)*100)

resid <- modified_medal_2016_df$Actual_Medal2016 - modified_medal_2016_df$Medal2016
# Scatter plot of actual vs. predicted medal count in 2016 for each cluster
ggplot(modified_medal_2016_df, aes(x = Actual_Medal2016, y = Medal2016, color = as.factor(cluster))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "#E69F00") +
  xlab("Actual Medal Count in 2016") +
  ylab("Predicted Medal Count") +
  ggtitle("Scatter plot of Actual vs. Predicted Medal Count by Cluster") +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram of residuals from each linear regression model for each cluster
ggplot(modified_medal_2016_df, aes(x = resid, fill = as.factor(cluster), colour = as.factor(cluster))) +
  geom_histogram(aes(y=..density..),bins = 30, alpha = 0.5, position = "identity") +
  geom_density(alpha=0.3, position = "stack") +
  xlab("Residuals") +
  ylab("Frequency") +
  ggtitle("Histogram of Residuals by Cluster") +
  labs(fill = "Cluster")+
  guides(col = FALSE) +
  #scale_color_brewer(palette="Dark2")+
  #scale_fill_brewer(palette="Dark2")+
  theme_minimal()

# QQ plot of residuals from each linear regression model for each cluster
Cluster <- as.factor(modified_medal_2016_df$cluster)
ggplot(modified_medal_2016_df, aes(sample = resid, color = Cluster)) +
  stat_qq() +
  stat_qq_line() +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  ggtitle("QQ plot of Residuals by Cluster") +
  theme(plot.title = element_text(hjust = 0.5)) 

##################### TASK-4 ###########################

#Equations we get form model in Task-2 are as follows:- 
#y = -49.363 + 5.545(GDP) + 1.983(Population)
#P(X>=1) = P(X=1) + 1 - P(X<1)

# Compute the log-odds of winning a medal for the UK
uk_logodds <- predict(model_2012_log, newdata = data.frame(log_population = log(62262000), log_gdp = log(2431.59)))

lambda <- 1/mean(predict_model2)
# Convert the log-odds to a probability
uk_prob <- exp(-lambda*400000000)

# Print the probability
uk_prob

library(dplyr)
# extracting the log-transformed inputs and output for UK
UK_data <- log_medal_DataFrame %>% filter(country == "Great Britain") %>%
  select(log_population, log_gdp, medal_2012)

# predicting the Medal2012 value for UK
UK_data$pred_Medal2012 <- predict(model_2012_log, newdata = UK_data)

# calculating the probability of winning at least 30 medals
prob_30_medals <- 1 - pnorm(1, mean = UK_data$pred_Medal2012, sd = sigma(model_2012_log))
prob_30_medals

hist(predict_model2,freq = F,breaks = 50)

df <- data.frame(Actual_medal_2016 = exp(log(medal_gdp_data$Medal2016)), predictel_medal_2016 = predict_model2)

predict_model2
residual_log_model <- medal_gdp_data$Medal2016 - predict_model2
hist(residual_log_model)


mean_Uk <- mean(c(predict_model2[]))
predict_model2[predict_model2 < 0] <- 0

UK_data

gb_preds


# Extract residuals from the linear regression model
residuals <- residuals(model_2012_log)

# Create histogram of residuals
ggplot(data.frame(residuals = residuals), aes(x = residuals)) + 
  geom_histogram(aes(y=..density..),bins = 30, alpha = 0.5, position = "identity") +
  geom_density(alpha=0.3) +
  ggtitle("Histogram of Residuals") +
  xlab("Residuals") + ylab("Frequency")

# Extract model parameters
model_params <- coef(model_2012_log)

# Create sequence of values for x-axis
x <- seq(min(residuals), max(residuals), length = 100)

# Compute the PDF of the normal distribution using model parameters
pdf <- dnorm(x, mean = model_params[1], sd = model_params[2])

# Create PDF plot
ggplot(data.frame(x = x, pdf = pdf), aes(x = x, y = pdf)) +
  geom_line(color = "red", linewidth = 1) +
  ggtitle("Probability Density Function (PDF)") +
  xlab("Residuals") + ylab("Density")

# Compute probability of winning at least 1 medal using the cumulative distribution function (CDF)
prob_at_least_1 <- 1 - pnorm(1, mean = model_params[1], sd = model_params[2])

# Create bar plot of the probability
ggplot(data.frame(prob_at_least_1 = prob_at_least_1), aes(x = "", y = prob_at_least_1)) +
  geom_bar(stat = "identity", width = 1, color = "black", fill = "lightblue") +
  ggtitle("Probability of Winning at Least 1 Medal") +
  xlab("") + ylab("Probability")

######################TASK-5########################

par(mfrow = c(2, 2))

# Bar chart of AIC values
aic_values <- c(AIC(model_2012), AIC(model_2012_log), 499.4193)
models <- c("Model 1", "Model 2", "Model 3")
data_aic <- data.frame(models, aic_values)
aic_plot <- ggplot(data_aic, aes(x = models, y = aic_values)) +
  geom_bar(stat = "identity", fill = "#00AFBB") +
  labs(title = "AIC Values for Different Models",
       x = "Model",
       y = "AIC Value")

# Bar chart of AIC values
rsquare_values <- c(rsquare_1, rsquare_2, rsquare_3)
models <- c("Model 1", "Model 2", "Model 3")
data_rsquare <- data.frame(models, rsquare_values)
rsquare_plot <- ggplot(data_rsquare, aes(x = models, y = rsquare_values)) +
  geom_bar(stat = "identity", fill = "#00AFBB") +
  labs(title = "R Square Values for Different Models",
       x = "Model",
       y = "R Square Value")

ggarrange(aic_plot, rsquare_plot, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
