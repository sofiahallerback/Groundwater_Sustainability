
library(shiny)
library(shinythemes)

shinyUI(fluidPage( theme = shinytheme("cerulean"),
                  h3("Groundwater Sustainablity in the Central Valley"),
                  
                  fluidRow(
                    column(width = 8,
                           #h3("Precipitation and Groundwater level"),
                           h4("Precipitation ratio compared to average"),
                           plotOutput("number2.plot", height="150",width="780"),
                           h4("Groundwater model output"),
                           textOutput("text1"),
                           plotOutput("number1.plot", height="300",width="800")
                    ), # column
                    column(width = 4,
                           h4("Freshwater Supply/Demand"),
                           h5("Efficency Scenario - 2"),
                         #  textOutput("text1"),
                           plotOutput("number3.plot", height="600",width="300") # # column
                  ) # column
                  
                  ), # fluidRow           
                  
                  hr(), # line          
                  
       fluidRow(column(width = 4, #style = "background-color:lightgrey;",
                       h4("Time Period:"),
                      sliderInput("time", "Choose the time period",
                                  1981, 2099, c(2050), step = 1),
                      radioButtons("unit", "Choose the unit system",
                                         choices = list("km^3","Million-acre-feet"), selected = "Million-acre-feet")
                     # sliderInput("gwRR", "Choose Groundwater recharge rate [Million-acre feet]",
                      #            0, 10, c(3.5), step = 0.5)
                      #sliderInput("swRR", "Choose Surfacewater recharge rate",
                      #            0, 10, c(5), step = 0.5),
                  ), # column
                column(width = 4, offset = 0,
                       h3("Efficency Senario - 1"),
                    h5("Supply"),
                      # sliderInput("gwS", "Groundwater Supply (%)",
                      #            -100, 100, c(0), step = 0.5),
                      sliderInput("swS", "Surfacewater Supply (%)",
                                  -100, 100, c(0), step = 0.5),
                      sliderInput("rrS", "Recycle and Reuse Supply (%)",
                                  -100, 100, c(0), step = 0.5),
                    h5("Demand"),
                    sliderInput("argD", "Efficiency in Agricultural Demand (%)",
                                -100, 100, c(0), step = 0.5),
                    sliderInput("urbanD", "Efficiency in Urban Demand (%)",
                                -100, 100, c(0), step = 0.5)
                ), # column
               column(width = 4, offset = 0,
                      h3("Efficency Senario - 2"),
                      h5("Supply"),
                      # sliderInput("gwS", "Groundwater Supply (%)",
                      #            -100, 100, c(0), step = 0.5),
                      sliderInput("swS2", "Surfacewater Supply (%)",
                                  -100, 100, c(10), step = 0.5),
                      sliderInput("rrS2", "Recycle and Reuse Supply (%)",
                                  -100, 100, c(10), step = 0.5),
                      h5("Demand"),
                      sliderInput("argD2", "Efficiency in Agricultural Demand (%)",
                                  -100, 100, c(10), step = 0.5),
                      sliderInput("urbanD2", "Efficiency in Urban Demand (%)",
                                  -100, 100, c(10), step = 0.5)
                      # sliderInput("envD", "Efficiency in Environmental Releases Demand (%)",
                      #            -100, 100, c(0), step = 0.5),
                      # sliderInput("wsD", "Efficiency in Wild and Scenic Demand (%)",
                      #            -100, 100, c(0), step = 0.5),
                    
                   ) # column
                      
                  ) # fluidrow
      # hr() # line  
#em("Massoud, E., Purdy, A.J., Miro, M., Hallerback, S., Famiglietti, J., Vrugt, J.
     #     'Groundwater Sustainability in the Central Valley: An empirical method to assess the supply-demand gap in California water resource management.'")


)#fluidpage
)#server
