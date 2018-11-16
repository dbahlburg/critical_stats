#create data for known distributions
library(tidyverse)
library(broom)

t_sim <- data_frame(samples = numeric(),
                    s_mean = numeric(),
                    s_sd = numeric(),
                    p_value = numeric())
p_values <- numeric()
selected_test <- "test_anova"

for (i in 2:50){
  for (k in seq(0.5,10,0.5)){
    for (m in seq(0.5,10,0.5)){
      for (l in 1:50){
        
        #switch(selected_test,
        #       
        #       ttest = {gen_data <- data_frame(variable1 = rnorm(i, mean = 0, sd = 1),
        #                                       variable2 = rnorm(i, mean = 0+k, sd = 1+m))
        #       t_test <- t.test(gen_data$variable1, gen_data$variable2, paired = T)
        #       p_values <- rbind(p_values, t_test$p.value)
        #                },
        #       test_anova = {
        gen_data <- data_frame(variable = rep(c('a','b','c'), each = i),
                                                    value = c(rnorm(i, mean = 0, sd = 1),rnorm(i, mean = 0+k, sd = 1+m),
                                                              rnorm(i, mean = 0+2*k, sd = 1+2*m)))
               anova_results <- tidy(aov(value~variable, data = gen_data))
               p_values <- rbind(p_values, anova_results$p.value[1])
                 
               #}
               #)
        
        
        
        
      }
      right_decision <- sum(p_values < 0.05)/length(p_values)
      t_sim <- rbind(t_sim, c(i,k,m,right_decision))
      
      p_values <- numeric()
    }  
  }
}

write.csv(t_sim, file = 'data/anova_data.csv')

t_sim %>%
  rename_at(vars(colnames(t_sim)), ~ c('samples','diff_mean','diff_sd','correctness')) %>%
  filter(samples == 50) %>%
  ggplot(.,aes(x = diff_mean, y = diff_sd, fill = correctness)) +
  geom_raster() +
  scale_fill_gradient(high = '#6be585', low = '#dd3e54', limits = c(0,1), breaks = c(0,0.5,1),
                      guide = guide_colorbar(title = '', ticks = FALSE,
                                             nbin = 100)) +
  labs(x = 'difference between means',y = 'difference between standard deviations') +
  theme_bw() +
  theme(panel.grid = element_blank())




gen_data <- data_frame(variable = rep(c('a','b','c'), each = 3),
                       value = c(rnorm(3, mean = 0, sd = 1),rnorm(3, mean = 0+1, sd = 1+1),rnorm(3, mean = 0+2*1, sd = 1+2*1)))
a <- tidy(aov(value~variable, data = gen_data))







