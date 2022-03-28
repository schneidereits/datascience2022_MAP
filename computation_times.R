# Landscape Ecology and Data Science, HU Berlin
# Authors: Shawn Schneidereit, Janis Klug
# Script for: Comparison of computational times of the rapidminer model runs
# Date: 25.03.2022

# Loading required library
library(ggplot2)

# Models: Naive Bayes, Deep Learning, Generalized linear model, Random Forest 
# model type
model_type <- c('NB', 'DL', 'GLM', 'RF')

# Runtimes (s)
runtimes <- c(95, 312, 872, 494)

# Training times (ms)
training_time <- c(4, 674, 847, 292)

# Scoring times (ms)
scoring_time <- c(532, 858, 598, 13000)

# Creating a computation times dataframe
df <- data.frame(model_type, runtimes, training_time, scoring_time)

# Creating bar plots
# Runtimes
p_rt <-ggplot(data=df, aes(x=model_type, y=runtimes)) +
  geom_bar(stat="identity", width = 0.5, fill = "grey")+
  xlab("model type")+
  ylab("runtimes (s)")+
  theme_classic()
p_rt

# Creating bar plots
# training time
p_tt <-ggplot(data=df, aes(x=model_type, y=training_time)) +
  geom_bar(stat="identity", width = 0.5, fill = "grey")+
  xlab("model type")+
  ylab("training time (ms)")+
  theme_classic()
p_tt

# Creating bar plots
# scoring time 
p_st <-ggplot(data=df, aes(x=model_type, y=scoring_time)) +
  geom_bar(stat="identity", width = 0.5, fill = "grey")+
  xlab("model type")+
  ylab("scoring time (ms)")+
  theme_classic()
p_st
