#Weshalb p-values kritisch betrachtet werden sollten...
library(ggplot2)
library(shiny)
library(dplyr)
library(shinydashboard)
library(gridExtra)
library(shinyWidgets)


#Effekt von Stichprobengroesse auf Praezision der Verteilungsbeschreibung

ui <- dashboardPage(skin = 'blue',
                    
                    dashboardHeader(title = 'Kritische Statistik'),
                    
                    dashboardSidebar(width = '190px',
                                     sidebarMenu(
                                       menuItem('Reconstruction', tabName = 'precision',icon = icon('beer')),
                                       menuItem('Experimental Data', tabName = 'experiment', icon = icon('flask'))
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
                                                  max = 100,value = 2, animate = T)
                                  )
                        )
                      )
                    )
)

server <- function(input, output) {
  
  selected_experiment <- reactive({
    flow_summary %>%
      filter(origin == 'experiment') %>%
      filter(stat_or_exp_no == input$exp_no)
  })
 
  output$plots <- renderPlot({
    p2 <- flow_plot(input_data = selected_station(), field_or_exp = 'field', bac_or_phyto = bac_vec,log_scale = input$logswitch2)
    p <- grid.arrange(p1,p2, ncol=1)
    print(p)
  })

}
shinyApp(ui = ui, server = server)



















