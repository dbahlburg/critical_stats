#Weshalb p-values kritisch betrachtet werden sollten...
library(ggplot2)
library(shiny)
library(dplyr)
library(shinydashboard)
library(gridExtra)
library(shinyWidgets)

drawn_distr <- data_frame(approx_mean = numeric(), approx_sd = numeric())
#Effekt von Stichprobengroesse auf Praezision der Verteilungsbeschreibung

test_data <- read.csv('data/test_data.csv')

ui <- dashboardPage(skin = 'blue',
                    
                    
                    dashboardHeader(title = 'Limited bayesian statistics',
                                    titleWidth = '280'),
                    
                    dashboardSidebar(width = '240px',
                                     sidebarMenu(
                                       menuItem('Reconstructing distributions', tabName = 'precision',icon = icon('beer')),
                                       menuItem('Sample size vs. Type I error', tabName = 'type1', icon = icon('beer')),
                                       menuItem('Sample size vs. Type II error', tabName = 'type2', icon = icon('beer'))
                                     )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = 'precision',
                                withMathJax(),
                                box(status = 'primary',
                                    title = 'Reconstructing a known distribution',
                                    width = 12,
                                    solidHeader = T,
                                    plotOutput('distribution'),
                                    chooseSliderSkin('Flat', color = '#5796c5'),
                                    sliderInput('sample_size','Sample Size',min = 2, 
                                                max = 100,value = 2, animate = T),
                                    div(style = 'display:inline-block', numericInput('mean_in',label = '\\(\\bar{x}\\)', value = 0)),#$$\\bar{x}$$
                                    div(style = 'display:inline-block', numericInput('sd_in',label = '\\(\\sigma\\)', value = 1, min = 0.2, step = 0.2)),
                                    div(style = 'display:inline-block', numericInput('iteration_in',label = 'Number of Interations', value = 1, min = 1, step = 1))
                                )
                        ),
                        tabItem(tabName = 'type1',
                                box(status = 'primary',
                                    title = 'How likely is it to make a type I error?',
                                    width = 12,
                                    solidHeader = T,
                                    column(1,
                                           dropdown(pickerInput(
                                             label = 'select test',
                                             inputId = "selected_test", 
                                             choices = c('ANOVA' = 'anova','pairwise t-test' = 't_test'),
                                             selected = 'ANOVA',
                                             options = list(
                                               `actions-box` = F, 
                                               size = 10
                                             ), 
                                             multiple = F
                                           ),
                                           style = 'material-circle',
                                           size = 'sm',
                                           status = 'danger',
                                           icon = icon("gear"),
                                           width = '200px')
                                    ),
                                    column(11,
                                           textOutput('test_name'),
                                           plotOutput('type1_plot'),
                                           chooseSliderSkin('Flat', color = '#5796c5'),
                                           sliderInput('psample_size', 'Sample Size', min = 2, max = 50, value = 2, animate = T)))),
                        tabItem(tabName = 'type2',
                                box(status = 'primary',
                                    title = 'How likely is it to make a type II error?',
                                    width = 12,
                                    solidHeader = T,
                                    column(1,
                                           dropdown(pickerInput(
                                             label = 'select test',
                                             inputId = "selected_test2",
                                             choices = c('ANOVA' = 'anova','pairwise t-test' = 't_test'),
                                             selected = 'ANOVA',
                                             options = list(
                                               `actions-box` = F,
                                               size = 10
                                             ),
                                             multiple = F
                                           ),
                                           style = 'material-circle',
                                           size = 'sm',
                                           status = 'danger',
                                           icon = icon("gear"),
                                           width = '200px')
                                    ),
                                    column(11,
                                           textOutput('test_name2'),
                                           plotOutput('type2_plot'),
                                           chooseSliderSkin('Flat', color = '#5796c5'),
                                           sliderInput('psample_size2', 'Sample Size', min = 2, max = 50, value = 2, animate = T))))
                        
                       
                      )
                    )
)

server <- function(input, output) {
  
  approx_distr <- reactive({
    drawn_distr <- data_frame(variable = rep(1:input$iteration_in, each = input$sample_size),
                              value = rnorm(input$iteration_in*input$sample_size, mean = input$mean_in, sd = input$sd_in))
    drawn_distr %>%
      group_by(variable) %>%
      summarise(MEAN = mean(value),
                SD = sd(value))

})
  
  output$test_name <- renderPrint({
    if (input$selected_test == 'anova'){
    cat('ANOVA')}
    else if (input$selected_test == 't_test'){
      cat('Pairwise t-test')
    }
  })
  
  output$test_name2 <- renderPrint({
    if (input$selected_test2 == 'anova'){
      cat('ANOVA')}
    else if (input$selected_test2 == 't_test'){
      cat('Pairwise t-test')
    }
  })
  
  selected_type1 <- reactive({
    test_data %>%
      filter(diff == 'no') %>%
      filter(test == input$selected_test & samples == input$psample_size)
  })
  
  selected_type2 <- reactive({
    test_data %>%
      filter(diff == 'yes') %>%
      filter(test == input$selected_test2 & samples == input$psample_size2)
  })
  
  output$distribution <- renderPlot({
    
    purrr::reduce2(.x = approx_distr()$MEAN,
                   .y = approx_distr()$SD,
                   .init = ggplot(data.frame(x_lim = c(as.numeric(input$mean_in) - 5*as.numeric(input$sd_in), as.numeric(input$mean_in) + 5*as.numeric(input$sd_in))), aes(x_lim))+
                     stat_function(fun = dnorm, n = 300, args = list(mean = input$mean_in, sd = input$sd_in)) +
                     labs(y = '', x = '') +
                     scale_y_continuous(limits = c(0,1)) +
                     theme_bw()+
                     theme(panel.grid.minor = element_blank(),
                           panel.grid.major.x = element_blank()),
                   function(prev, .x, .y) {
                     prev + stat_function(fun = dnorm,
                                          n = 300,
                                          args = list(mean = .x,
                                                      sd = .y),
                                          alpha = 0.4, linetype = '22', colour = '#dd3e54')
                   })
  })
  
  output$type1_plot <- renderPlot({
  ggplot(selected_type1(),aes(x = diff_mean, y = sd, fill = 1-correctness)) +
    geom_raster() +
    scale_fill_gradient(low = '#6be585', high = '#dd3e54', limits = c(0,0.3), breaks = c(0,0.3),
                        guide = guide_colorbar(title = '', ticks = FALSE,
                                               nbin = 100)) +
    labs(x = expression(Delta~bar(x)),y = expression(sigma))+
    theme_bw() +
    theme(panel.grid = element_blank())
  })
  
  output$type2_plot <- renderPlot({
    ggplot(selected_type2(),aes(x = diff_mean, y = sd, fill = 1-correctness)) +
      geom_raster() +
      scale_fill_gradient(low = '#6be585', high = '#dd3e54', limits = c(0,1), breaks = c(0,0.5,1),
                          guide = guide_colorbar(title = '', ticks = FALSE,
                                                 nbin = 100)) +
      labs(x = expression(Delta~bar(x)),y = expression(sigma))+
      theme_bw() +
      theme(panel.grid = element_blank())
  })
}
shinyApp(ui = ui, server = server)

