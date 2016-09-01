# Groundwater sustainablity in the Central Valley
# Project based on paper:
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Quantifying groundwater sustainability in California’s Central Valley –
# An  empirical method to estimate and project groundwater depletion and recharge
# Elias C. Massoud, Adam J. Purdy, Michelle Miro, Sofia Hallerbäck, 
# Jasper A. Vrugt, and James 4 Famiglietti
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Shiny app designed by Sofia Hallerbäck and Elias Massoud

# START UI
####################################################################################################
# 0.  Load Packages
####################################################################################################

library(shiny)
library(shinythemes)

####################################################################################################
# 1.  The Shiny App structure
####################################################################################################

shinyUI(fluidPage(theme = shinytheme("cerulean"),
                  h3("Groundwater Sustainablity in the Central Valley"),
                  
                  fluidRow(
                    column(width = 8,
                           
                           em("
A major concern for California water resource management is 
the sustainability of the Central Valley groundwater aquifer. 
In this app you can investigate how 
groundwater levels have changed since 1980, and use the Groundwater 
Depletion Model [Massoud et al., In prep], to assess how groundwater availability 
in the future changes under different user-defined water management scenarios. 
The default case presented is the \"business as usual\" scenario, 
where no management changes are made. 
Using the slide bars on the right, 
you can create different management scenarios and test the impact on future groundwater levels.
                      "),
                           
                           h3("Groundwater in the Central Valley"),
                           textOutput("text1"),
                           plotOutput("number1.plot", height="300",width="800"),
                           
                           em("
Precipitation is the independent variable used to drive the model.  
Precipitation data are empirically related to each supply and demand variable in 
Central Valley water management portfolio (and thus the 
amount of groundwater supply) for each year. Additionally, precipitation also 
impacts the recharge of the aquifer. Shown below is precipitation for each 
year of the study used to drive the model (1 indicates an average year, 
less than 1 is a dry year (red), and greater than 1 is a wet year (blue))."),
                           
                           h4("Precipitation ratio compared to average"),
                           plotOutput("number2.plot", height="150",width="740"),
                           
                           em("
During wet years, greater precipitation increases surface water availability and 
reduces the need for groundwater.  Furthermore increased precipitation recharges 
to the aquifer thus groundwater levels increase. During dry years the opposite occurs, 
especially in the case of consecutive dry years occur, reduced precipitation decreases
surface water availability and increases reliance on groundwater to meet demands therefore 
groundwater levels decreases. Shown below are the model simulated changes in
groundwater for each year; in blue are the wet years and in red the dry years, from precipitation observations presented above.
                              "),
                           
                           h4("Simulated Changes in Groundwater Levels"),
                           textOutput("text2"),
                           plotOutput("number5.plot", height="200",width="700"), # # column
                           
                           em("Shown below are the observed changes in groundwater levels, 
USGS (1981-2003) and GRACE (2004-2013). Again, blue are from wet years and red dry years defined from precipitation.
"),
                           
                          h4("Observed Changes in Groundwater Levels"),
                          textOutput("text3"),
                          plotOutput("number6.plot", height="150",width="700") # # column
                         
                         
                  ), # column
                  column(width = 4,
                         h4("1. Choose time period"),
                         sliderInput("time", "",
                                     1981, 2099, c(2050), step = 1),
                         h4("2. Chose unit system"),
                         radioButtons("unit", "",
                                      choices = list("km^3","Million-acre-feet"), selected = "km^3"),
                         # sliderInput("gwRR", "Choose Groundwater recharge rate [Million-acre feet]",
                         #            0, 10, c(3.5), step = 0.5)
                         #sliderInput("swRR", "Choose Surfacewater recharge rate",
                         #            0, 10, c(5), step = 0.5),
                  
                  h4("3. Try different management scenarios"),
                  h5("Water Demand"),
                  sliderInput("argD2", "Change in Agricultural Demand (%)",
                              -50, 50, c(0), step = 1),
                  sliderInput("urbanD2", "Change in Urban Demand (%)",
                              -200, 200, c(0), step = 1),
                  h5("Water Supply"),
                  # sliderInput("gwS", "Groundwater Supply (%)",
                  #            -100, 100, c(0), step = 0.5),
                  sliderInput("swS2", "Change in use of Surface Water Supply (%)",
                              -100, 100, c(0), step = 1),
                  sliderInput("rrS2", "Change in use of Recycled and Reused Supply (%)",
                              -500, 500, c(0), step = 1)

                  # sliderInput("envD", "Efficiency in Environmental Releases Demand (%)",
                  #            -100, 100, c(0), step = 0.5),
                  # sliderInput("wsD", "Efficiency in Wild and Scenic Demand (%)",
                  #            -100, 100, c(0), step = 0.5),

                  
              ) # column
        ), # fluidRow  
        column(width = 8,
               hr(), # line 
        em("California has a complex and storied history of water management. 
The combination of dams lining the Sierra Nevada and a statewide water conveyance 
infrastructure provide a steady renewable supply of surface water to meet the needs 
of the more arid Central and Southern parts of the state. However, in recent years, 
statewide urban, agricultural, and environmental demands have exceeded the natural renewable supply. 
To date, this gap between limited surface water supply and an increasing statewide
water demand has been met primarily by the extraction of groundwater. However, as 
various studies have shown, an overreliance on this resource can lead to highly 
unsustainable rates of groundwater depletion, particularly in semi-arid regions 
with variable precipitation (McGuire 2009, Rodell et al. 2009,
                     Wada et al. 2010, Famiglietti et al. 2011; Richey et al., 2015a)."),
        hr(), # line 
        em(" Simulations are based on the empirical model presented in the paper:\n
            Quantifying groundwater sustainability in California’s Central Valley –\n
           An  empirical method to estimate and project groundwater depletion and recharge\n
            (Elias C. Massoud, Adam J. Purdy, Michelle Miro, Sofia Hallerbäck, \n
            Jasper A. Vrugt, and James Famiglietti)"),
          hr() # line       

                      
                
                      
   ) # fluidrow
      # hr() # line  
#em("Massoud, E., Purdy, A.J., Miro, M., Hallerback, S., Famiglietti, J., Vrugt, J.
     #     'Groundwater Sustainability in the Central Valley: An empirical method to assess the supply-demand gap in California water resource management.'")


)#fluidpage
)#server
