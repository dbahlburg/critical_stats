#create data for known distributions
library(tidyverse)
library(broom)

t_sim2 <- data_frame(samples = numeric(),
                     s_mean = numeric(),
                     s_sd = numeric(),
                     p_value = numeric())
p_values2 <- numeric()


for (i in 2:50){
  for (k in seq(0.5,10,0.5)){
    for (m in seq(0.5,10,0.5)){
      for (l in 1:50){
        
        gen_data <- data_frame(variable1 = rnorm(i, mean = 0, sd = m),
                               variable2 = rnorm(i, mean = k, sd = m))
        t_test <- t.test(gen_data$variable1, gen_data$variable2, paired = T)
        p_values2 <- rbind(p_values2, t_test$p.value)
        
        
        # gen_data <- data_frame(variable = rep(c('a','b','c'), each = i),
        #                        value = c(rnorm(i, mean = 0, sd = m),rnorm(i, mean = 0, sd = m),
        #                                  rnorm(i, mean = 0, sd = m)))
        # anova_results <- tidy(aov(value~variable, data = gen_data))
        # p_values <- rbind(p_values, anova_results$p.value[1])
        
        #)
      }
      right_decision <- sum(p_values2 < 0.05)/length(p_values2)
      t_sim2 <- rbind(t_sim2, c(i,k,m,right_decision))
      
      p_values2 <- numeric()
    }  
  }
}

write.csv(t_sim3, file = 'diff_ttest_data.csv')
t_sim %>%
  rename_at(vars(colnames(t_sim)), ~ c('samples','diff_mean','diff_sd','correctness')) %>%
  filter(samples == 50) %>%
  ggplot(.,aes(x = diff_mean, y = diff_sd, fill = correctness)) +
  geom_raster() +
  scale_fill_gradient(high = '#6be585', low = '#dd3e54', limits = c(0,1), breaks = c(0,0.5,1),
                      guide = guide_colorbar(title = '', ticks = FALSE,
                                             nbin = 100)) +
  labs(x = expression(Delta~bar(x)),y = expression(Delta~sigma))+
  theme_bw() +
  theme(panel.grid = element_blank())


t_nodiff_data <- read.csv('data/nodiff_ttest_data.csv')
t_diff_data <- read.csv('data/diff_ttest_data.csv')
aov_nodiff_data <- read.csv('data/nodiff_anova_data.csv')
aov_diff_data <- read.csv('data/diff_anova_data.csv')

aov_diff_data <- rename_at(aov_diff_data, vars(colnames(aov_diff_data)), ~ c('X','samples','diff_mean','sd','correctness'))

aov_data <- rename_at(aov_data, vars(colnames(aov_data)), ~ c('X','samples','diff_mean','sd','correctness'))
t_nodiff_data$test <- 't_test'
t_diff_data$test <- 't_test'
t_nodiff_data$diff <- 'no'
t_diff_data$diff <- 'yes'

aov_diff_data$test <- 'anova'
aov_nodiff_data$test <- 'anova'
aov_diff_data$diff <- 'yes'
aov_nodiff_data$diff <- 'no'


test_data <- rbind(t_nodiff_data,t_diff_data,aov_nodiff_data,aov_diff_data)
write.csv(test_data, file = 'data/test_data.csv')
