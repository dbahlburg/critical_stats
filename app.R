#Weshalb p-values kritisch betrachtet werden sollten...
library(ggplot2)
library(shiny)
library(dplyr)
library(shinydashboard)
library(gridExtra)
library(shinyWidgets)

#approx_distr <- data_frame(approx_mean = numeric(), approx_sd = numeric())

#Effekt von Stichprobengroesse auf Praezision der Verteilungsbeschreibung

ui <- dashboardPage(skin = 'blue',
                    
                    dashboardHeader(title = 'Kritische Statistik'),
                    
                    dashboardSidebar(width = '240px',
                                     sidebarMenu(
                                       menuItem('Reconstructing distributions', tabName = 'precision',icon = icon('beer')),
                                       menuItem('Sensitivity of p-values', tabName = 'p_values', icon = icon('beer'))
                                     )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = 'precision',
                                box(status = 'primary',
                                    title = 'Reconstructing a known distribution',
                                    width = 12,
                                    solidHeader = T,
                                    plotOutput('distribution'),
                                    chooseSliderSkin('Flat', color = '#5796c5'),
                                    sliderInput('sample_size','Sample Size',min = 0, 
                                                max = 100,value = 2, animate = T),
                                    div(style = 'display:inline-block', textInputAddon(inputId = "mean_in", label = "Mean", value = 0, addon = NULL),
                                        verbatimTextOutput(outputId = "mean_in")),
                                    div(style = 'display:inline-block', textInputAddon(inputId = "sd_in", label = "Standard Deviation", value = 1, addon = NULL),
                                        verbatimTextOutput(outputId = "sd_in"))
                                )
                        ),
                        tabItem(tabName = 'p_values',
                                box(status = 'primary',
                                    title = 'How many samples do you need for a reliable p-value',
                                    width = 12,
                                    solidHeader = T,
                                    column(1,
                                           dropdown(pickerInput(
                                             label = 'select test',
                                             inputId = "selected_test", 
                                             choices = c('ANOVA','pairwise t-test'), 
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
                                    plotOutput('p_plots'),
                                    chooseSliderSkin('Flat', color = '#5796c5'),
                                    sliderInput('psample_size', 'Sample Size', min = 2, max = 50, value = 2, animate = T))))
                      )
                    )
)

server <- function(input, output) {
  
  approx_distr <- reactive({
    samp <- rnorm(input$sample_size, mean = input$mean_in, sd = input$sd_in)
    approx_distr <- data.frame(m = mean(samp), s = sd(samp))
    approx_distr
    #for (i in 1:10){
    #  samps <- rnorm(input$sample_size, mean = input$mean_in, sd = input$sd_in)
    #  mean_samps <- mean(samps)
    #  sd_samps <- sd(samps)
    #  approx_distr <- rbind(approx_distr, c(mean_samps, sd_samps))
    #}
    #approx_extremes <- data.frame(min_sd = min(approx_distr$approx_sd), max_sd = max(approx_distr$approx_sd), mean_mean = mean(approx_distr$approx_mean))
  })
  
  
  
  output$distribution <- renderPlot({
    ggplot(data.frame(x_lim = c(as.numeric(input$mean_in) - 3*as.numeric(input$sd_in), as.numeric(input$mean_in) + 3*as.numeric(input$sd_in))), aes(x_lim)) +
      stat_function(fun = dnorm, n = 300, args = list(mean = as.numeric(input$mean_in), sd = as.numeric(input$sd_in))) +
      stat_function(fun = dnorm, n = 300, args = list(mean = approx_distr()$m, sd = approx_distr()$s)) +
      labs(y = '', x = '') +
      scale_y_continuous(limits = c(0,0.5)) +
      theme_classic()
  })
  
}
shinyApp(ui = ui, server = server)



















