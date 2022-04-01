###############################################################################
# Landscape Ecology and Data Science, HU Berlin
# Authors: Janis Klug
# Script for: Comparison of computational times of the rapidminer model runs
# Date: 25.03.2022

###############################################################################

# Loading required libraries
library(ggplot2)

# Models: Naive Bayes, Deep Learning, Generalized linear model, Random Forest 
# model type
model_type <- c('NB', 'DL', 'GLM', 'RF')

# Runtime (s)
runtime <- c(295.332, 344.315, 304.432, 2426.231)

# Training time (ms)
training_time <- c(64, 595, 1000, 119)

# Scoring time (ms)
scoring_time <- c(2000, 1000, 574, 4000)

###############################################################################

# Creating a computation times dataframe
df <- data.frame(model_type, runtime, 
                 training_time, scoring_time)

###############################################################################

# Creating bar plots
# Runtime
p_rt <-ggplot(data=df, aes(x=model_type, y=runtime)) +
  geom_bar(stat="identity", width = 0.6, fill = c("#000000","#F0E442",
                                                  "#009E73","#999999"))+
  xlab("model type")+
  ylab("runtime (s)")+
  scale_x_discrete(limits=c('NB', 'DL', 'GLM', 'RF'))+
  theme_classic()
p_rt


# Training time
p_trt <-ggplot(data=df, aes(x=model_type, y=training_time)) +
  geom_bar(stat="identity", width = 0.6, fill = c("#000000","#F0E442",
                                                  "#009E73","#999999"))+
  xlab("model type")+
  ylab("training time (ms)")+
  scale_x_discrete(limits=c('NB', 'DL', 'GLM', 'RF'))+
  theme_classic()
p_trt


# Scoring time 
p_st <-ggplot(data=df, aes(x=model_type, y=scoring_time)) +
  geom_bar(stat="identity", width = 0.6, fill = c("#000000","#F0E442",
                                                  "#009E73","#999999"))+
  xlab("model type")+
  ylab("scoring time (ms)")+
  scale_x_discrete(limits=c('NB', 'DL', 'GLM', 'RF'))+
  theme_classic()
p_st




