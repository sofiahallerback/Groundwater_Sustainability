# Shiny App Server

# 1.  Load Packages
library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(animation)
library(devtools)

#root.dir <- "./"

# 2.1  Input data
# Year 1, Precip 2, Obsdata GWL 3

Data_1981_2013 <- read.delim("Data_1981_2013.txt", header=FALSE) #paste0(root.dir,
# Modeled GWL
GW_level_1981_2013 <- read.csv("GW_level_1981_2013.csv", sep="") #paste0(root.dir,
# Year 1, Future Precip 6
#futurePP <- read.table("~/Desktop/GWproject/Data/futurePP.txt", quote="\"", comment.char="")
CAPP_2011_2099 <- read.delim("CAPP_2011_2099.txt", header=FALSE) #paste0(root.dir, 
#futurePPdf <- data.frame(years = futurePP[,1], precip = futurePP[,6])
# Supply Demand realtionship past, 1981-2014
#SupplyDemand_past <- read.csv("~/Desktop/GWproject/Data/SupplyDemand_past.csv")


# 2.2 Create commun data frame
dataframe <- data.frame(year = c(Data_1981_2013$V1,2014:2099 ), # futurePP[15:50,1]), 
                        precip = c(Data_1981_2013$V2, CAPP_2011_2099[4:89,1]), #futurePP[15:50,6]), 
                        GWL_model = c(GW_level_1981_2013$x, rep(NA,86)),
                        GWL_model_senario1 = c(rep(NA,119)),
                        GWL_model_senario2 = c(rep(NA,119)),
                        past_future = c(rep("Past (Prism)",33),rep("Predicted\n(Cal-Adapt)", 86)),
                        past_future2 = c(rep("Model - Calibration period",33),rep("Model - Predicted", 86)))

dataframe$wetdry <- cut(dataframe$precip, breaks = c(-Inf, 1, Inf), labels= c("dry", "wet"))

obsData <- data.frame(year = c(Data_1981_2013$V1, 2014:2099), depletion = c(Data_1981_2013$V3, rep(NA,86)),
                      Source = c(rep("Observation source: USGS",23), rep("Observation source: GRACE",10), rep(NA,86))) #USGS = c(Data_1981_2013$V3[1:23],rep(NA,10)), GRACE = c(rep(NA,23),Data_1981_2013$V3[24:33])

gwRR = 3.5 # groundwater recharge rate


# 3. Evaulate the server
shinyServer(function(input, output) {
  gtReactive <- reactive({
  })

  output$text1 <- renderText({ 
    paste("[", input$unit, "]")
  })

  # 3.1 Groundwater level plot
  output$number1.plot <- renderPlot({
    
    # Change the plotted time period
    #time_start <- input$time[1]
    time_end <- input$time # [2]
    # Change the simulated time period
    sim_start = 2013
    sim_end = max(2013, time_end)
    
    if (time_end >2013){
    # Simulation range
    dataframe_sim <- dataframe  %>% filter(year < time_end) #%>% filter(year > sim_start-1)
    
    n = sim_end - sim_start
    
    # Input the option values
    # Senario 1
    gwRR = gwRR # "Choose Groundwater recharge rate", 0, 10, c(5), step = 0.5),
    #swRR = input$swRR # "Choose Surfacewater recharge rate",0, 10, c(5), step = 0.5),
    #h4("Efficenty Senario - Supply"),
    #gwS = input$gwS #", "Groundwater Supply",-100, 100, c(0), step = 0.5),
    swS = input$swS #", "Surfacewater Supply", -100, 100, c(0), step = 0.5),
    rrS = input$rrS #", "Recycle and Reuse Supply",-100, 100, c(0), step = 0.5),
    #h4("Efficenty Senario - Demand"),
    argD = input$argD #", "Agricultural Demand",-100, 100, c(0), step = 0.5),
    urbanD = input$urbanD #", "Urban Demand",-100, 100, c(0), step = 0.5),
    #envD = input$envD #, "Environmental Releases Demand",-100, 100, c(0), step = 0.5),
    #wsD = input$wsD #", "Wild and Scenic Demand", -100, 100, c(0), step = 0.5),
    
    # Senario 2
    swS2 = input$swS2 #", "Surfacewater Supply", -100, 100, c(0), step = 0.5),
    rrS2 = input$rrS2 #", "Recycle and Reuse Supply",-100, 100, c(0), step = 0.5),
    argD2 = input$argD2 #", "Agricultural Demand",-100, 100, c(0), step = 0.5),
    urbanD2 = input$urbanD2 #", "Urban Demand",-100, 100, c(0), step = 0.5),
    
    
    #### Prealocate supply and demand vectors
    # Senario 1
    GW_S = rep(NA,n)
    RR_S = rep(NA,n)
    SW_S = rep(NA,n)
    ARG_D = rep(NA,n)
    URBAN_D = rep(NA,n)
    ENV_D = rep(NA,n)
    WS_D = rep(NA,n)
    #GW_level = c(obsData$depletion[33], rep(NA,n-1)) # 
   # GW_level = c(obsData$depletion[min(time_start,sim_start)-1981+1],rep(NA,n-1)) # 
   # GW_level2 = c(obsData$depletion[min(time_start,sim_start)-1981+1],rep(NA,n-1)) # 
   #if(time_start <2013){
   #   GW_level = c(obsData$depletion[time_start-1981+1],rep(NA,n-1)) # 
   #   GW_level2 = c(obsData$depletion[time_start-1981+1],rep(NA,n-1)) 
   # } else {
      GW_level = c(dataframe$GWL_model[33], rep(NA,n-1))
      GW_level2 =c(dataframe$GWL_model[33], rep(NA,n-1))
   # }
    
    # Model stucture
    # Senario 1
    for(i in c(2:n)){
      
      ## Relationships
      #Extra.x(1:6) = [-8.1833;14.8316;-0.4073;0.5748;9.7157;12.8645]; % Supply
      #Extra.x(7:14) = [5.8565;2.5411;1.2198;-0.3031;-0.1198;1.2778;-5.8313;24.7550]; % Demand
      # Supply
      GW_S[i] = -8.1833* dataframe_sim$precip[i] + 19.7 #Old value: 14.8316
      RR_S[i] = -0.4073* dataframe_sim$precip[i] + 0.5748
      SW_S[i] = 9.7157* dataframe_sim$precip[i] + 12.8645
      
      # Demand
      WS_D[i] = 5.8565*dataframe_sim$precip[i] + 2.5411
      ENV_D[i] = 1.2198*dataframe_sim$precip[i] + -0.3031
      URBAN_D[i] = -0.1198*dataframe_sim$precip[i] + 1.2778
      ARG_D[i] = -5.8313*dataframe_sim$precip[i] + 24.7550
      
      # Apply efficiency scenarios
      #GW_S[i] = GW_S[i] * (100 - gwS)/100
      RR_S[i] = RR_S[i] * (100 + rrS)/100
      SW_S[i] = SW_S[i] * (100 + swS)/100
      ARG_D[i] = ARG_D[i] * (100 - argD)/100
      URBAN_D[i] = URBAN_D[i] * (100 - urbanD)/100
      # ENV_D[i] = ENV_D[i] * (100 - envD)/100
      # WS_D[i] = WS_D[i] * (100 - wsD)/100
      
      ## Scale supply to demand
      Supply = GW_S[i]+ RR_S[i] + SW_S[i]
      Demand = ARG_D[i] + URBAN_D[i]+ENV_D[i]+WS_D[i] 
      scale_SD = Demand/Supply
      
      GW_S[i] = scale_SD*GW_S[i]
      RR_S[i] = scale_SD*RR_S[i]
      SW_S[i] = scale_SD*SW_S[i]
      
      ## Get recharge value
      GW_RR1 = gwRR * dataframe_sim$precip[i] # How much recharge from precip
      GW_RR2 = -0.5172* dataframe_sim$precip[i] + 5.6198 # How much recharge from antropogenic
      GW_RR = GW_RR1 + GW_RR2
      
      ## Net Groundwater level
      GW_level[i] = GW_level[i-1] + GW_RR - GW_S[i]
    }
    
    
    # Model stucture
    # Senario 2
    for(i in c(2:n)){
      
      ## Relationships
      #Extra.x(1:6) = [-8.1833;14.8316;-0.4073;0.5748;9.7157;12.8645]; % Supply
      #Extra.x(7:14) = [5.8565;2.5411;1.2198;-0.3031;-0.1198;1.2778;-5.8313;24.7550]; % Demand
      # Supply
      GW_S[i] = -8.1833* dataframe_sim$precip[i] + 19.7 #Old value: 14.8316
      RR_S[i] = -0.4073* dataframe_sim$precip[i] + 0.5748
      SW_S[i] = 9.7157* dataframe_sim$precip[i] + 12.8645
      
      # Demand
      WS_D[i] = 5.8565*dataframe_sim$precip[i] + 2.5411
      ENV_D[i] = 1.2198*dataframe_sim$precip[i] + -0.3031
      URBAN_D[i] = -0.1198*dataframe_sim$precip[i] + 1.2778
      ARG_D[i] = -5.8313*dataframe_sim$precip[i] + 24.7550
      
      # Apply efficiency scenarios
      #GW_S[i] = GW_S[i] * (100 - gwS)/100
      RR_S[i] = RR_S[i] * (100 + rrS2)/100
      SW_S[i] = SW_S[i] * (100 + swS2)/100
      ARG_D[i] = ARG_D[i] * (100 - argD2)/100
      URBAN_D[i] = URBAN_D[i] * (100 - urbanD2)/100
      # ENV_D[i] = ENV_D[i] * (100 - envD)/100
      # WS_D[i] = WS_D[i] * (100 - wsD)/100
      
      ## Scale supply to demand
      Supply = GW_S[i]+ RR_S[i] + SW_S[i]
      Demand = ARG_D[i] + URBAN_D[i]+ENV_D[i]+WS_D[i] 
      scale_SD = Demand/Supply
      
      GW_S[i] = scale_SD*GW_S[i]
      RR_S[i] = scale_SD*RR_S[i]
      SW_S[i] = scale_SD*SW_S[i]
      
      ## Get recharge value
      GW_RR1 = gwRR * dataframe_sim$precip[i] # How much recharge from precip
      GW_RR2 = -0.5172* dataframe_sim$precip[i] + 5.6198 # How much recharge from antropogenic
      GW_RR = GW_RR1 + GW_RR2
      
      ## Net Groundwater level
      GW_level2[i] = GW_level2[i-1] + GW_RR - GW_S[i]
    }
    
    
    # Save the simulated GWL
    dataframe$GWL_model_senario1[(sim_start-1981+1):(sim_end-1981)] <- GW_level
    dataframe$GWL_model_senario2[(sim_start-1981+1):(sim_end-1981)] <- GW_level2
    }
    
    # plotting range
    dataframe_new <- dataframe %>% filter(year < time_end) # %>% filter(year > time_start-1)
    obsData <- obsData  %>% filter(year < time_end) #%>% filter(year > time_start-1)
    
    # Change unit
    if (input$unit == "km^3"){
      dataframe_new$GWL_model_senario1 <- dataframe_new$GWL_model_senario1 *1.23348
      dataframe_new$GWL_model_senario2 <- dataframe_new$GWL_model_senario2 *1.23348
      dataframe_new$GWL_model <- dataframe_new$GWL_model *1.23348
      obsData$depletion <- obsData$depletion *1.23348
    }
    
    # If choosed to plot observed data
    #  if (!input$obsSource){obsData$depletion <- rep(NA,(time_end - time_start))}   
    
    # Create the Goundwater level plot
    ggplot() + # y=gtReactive()
      geom_area(data = dataframe_new, aes(x=year,y=GWL_model), fill = "white", alpha = 0.5) +
      geom_ribbon(data = dataframe_new, 
                  aes(x=year,ymax=GWL_model,
                   ymin = min(GWL_model, GWL_model_senario1, GWL_model_senario2, na.rm =TRUE)), 
                  fill = "blue", alpha = 0.3) +
      geom_ribbon(data = dataframe_new, 
                  aes(x=year,ymax=GWL_model_senario1,
                      ymin = min(GWL_model, GWL_model_senario1, GWL_model_senario2, na.rm =TRUE)), 
                  fill = "blue", alpha = 0.3) +
      geom_ribbon(data = dataframe_new, 
                  aes(x=year,ymax=GWL_model_senario2,
                      ymin = min(GWL_model, GWL_model_senario1, GWL_model_senario2, na.rm =TRUE)), 
                  fill = "grey", alpha = 0.3) +
      geom_line(data = dataframe_new, aes(x=year,y=GWL_model, colour ="Model\nfitted with\nobservations"),  size=1.5)+ #, color = "darkblue" color = "black",
      geom_line(data = dataframe_new, aes(x=year,y=GWL_model_senario2, colour = "Senario 2"),  size=1.5)+ #color ="darkred",
      geom_line(data = dataframe_new, aes(x=year,y=GWL_model_senario1, colour = "Senario 1"),  size=1.5)+ # color = "purple",
      geom_point(data = obsData %>% filter(Source == "Observation source: GRACE"), aes(x=as.numeric(year),y= as.numeric(depletion), colour = "Observation\n(source GRACE)"), size = 3) +
      geom_point(data = obsData %>% filter(Source == "Observation source: USGS"), aes(x=as.numeric(year),y= as.numeric(depletion), colour = "Observation\n(source USGS)"), size = 3) +
      scale_color_manual("",
                         breaks = c("Model\nfitted with\nobservations", "Senario 2", "Senario 1", "Observation\n(source USGS)", "Observation\n(source GRACE)"),
                          values = c( "black", "darkgrey", "darkblue", "cornflowerblue", "darkblue"))+ #, guide = guide_legend(nrow = 1)
      labs(x= "Time", y = "Groundwater level", color = "")+ #title = "Groundwater level - model output and observations"
      theme_bw()+
      scale_x_continuous(breaks = c(1980,1990,2000,2010,2020,2030,2040,2050,2060,2070,2080,2090)) +
      #guide_legend(nrow = 2) +
      theme(legend.position="right", 
            #panel.grid.major = element_blank(),
            #panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_text(size= 15),
            axis.title = element_text(size= 15),
            legend.text = element_text(size= 12)
            #panel.background = element_rect(fill = "blue")
            ) 
  })
  
  # 3.2 Precipitation plot
  output$number2.plot <- renderPlot({
    # Create long form subset of the data
    # Create long form subset of the data
    #time_start <- input$time[1]
    time_end <- input$time #[2]
    
    dataframe_new <- dataframe  %>%
      filter(year < time_end) #%>% filter(year > time_start-1)
    
    ggplot(dataframe_new,aes(x=year,y=precip, colour = past_future, fill = wetdry))+ # y=gtReactive(), 
      #geom_area(size=1, colour= "blue", fill = "blue", alpha = 0.5)+
      #geom_line(size=1, colour= "black")+
      geom_bar(stat="identity")+
      #geom_ribbon(aes(ymin = 1, ymax = "precip"), fill = "blue") +
      #geom_text(aes(label = "average"), position = c(0,1)) +
      scale_colour_manual(values = c("black", "grey")) +
      scale_fill_manual(values = c("red", "blue")) +
      labs(x= "Time", y = "Precip anomaly",  color = "",fill="")+ #, title= "Precipitation ratio compared to average"
      theme_bw()+
      scale_x_continuous(breaks = c(1980,1990,2000,2010,2020,2030,2040,2050,2060,2070,2080,2090)) +
      theme(legend.position="right", panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_text(size= 15),
            axis.title = element_text(size= 15),
            legend.text = element_text(size= 12)) +
      geom_hline(yintercept = 1, color = "black") +
     guides(fill = guide_legend(order=1, colour=NA),
            colour = guide_legend(order=2, override.aes=aes(fill=NA)))
    
  }) # renderplot
  
    output$number3.plot <- renderPlot({
    
      # Change the plotted time period
      #time_start <- input$time[1]
      time_end <- input$time # [2]
      # Change the simulated time period
      sim_start = 2013
      sim_end = max(2013, time_end)
      
      if (time_end >2013){
        # Simulation range
        dataframe_sim <- dataframe  %>% filter(year < time_end) %>% filter(year > sim_start-1)
        
        n = sim_end - sim_start
        
        # Input the option values
        # Senario 1
        gwRR = gwRR # "Choose Groundwater recharge rate", 0, 10, c(5), step = 0.5),
        #swRR = input$swRR # "Choose Surfacewater recharge rate",0, 10, c(5), step = 0.5),
        #h4("Efficenty Senario - Supply"),
        #gwS = input$gwS #", "Groundwater Supply",-100, 100, c(0), step = 0.5),
        swS = input$swS #", "Surfacewater Supply", -100, 100, c(0), step = 0.5),
        rrS = input$rrS #", "Recycle and Reuse Supply",-100, 100, c(0), step = 0.5),
        #h4("Efficenty Senario - Demand"),
        argD = input$argD #", "Agricultural Demand",-100, 100, c(0), step = 0.5),
        urbanD = input$urbanD #", "Urban Demand",-100, 100, c(0), step = 0.5),
        #envD = input$envD #, "Environmental Releases Demand",-100, 100, c(0), step = 0.5),
        #wsD = input$wsD #", "Wild and Scenic Demand", -100, 100, c(0), step = 0.5),
        
        # Senario 2
        swS2 = input$swS2 #", "Surfacewater Supply", -100, 100, c(0), step = 0.5),
        rrS2 = input$rrS2 #", "Recycle and Reuse Supply",-100, 100, c(0), step = 0.5),
        argD2 = input$argD2 #", "Agricultural Demand",-100, 100, c(0), step = 0.5),
        urbanD2 = input$urbanD2 #", "Urban Demand",-100, 100, c(0), step = 0.5),
        
        
        #### Prealocate supply and demand vectors
        # Senario 1
        GW_S = rep(NA,n)
        RR_S = rep(NA,n)
        SW_S = rep(NA,n)
        ARG_D = rep(NA,n)
        URBAN_D = rep(NA,n)
        ENV_D = rep(NA,n)
        WS_D = rep(NA,n)
        #GW_level = c(obsData$depletion[33], rep(NA,n-1)) # 
        # GW_level = c(obsData$depletion[min(time_start,sim_start)-1981+1],rep(NA,n-1)) # 
        # GW_level2 = c(obsData$depletion[min(time_start,sim_start)-1981+1],rep(NA,n-1)) # 
        #if(time_start <2013){
        #   GW_level = c(obsData$depletion[time_start-1981+1],rep(NA,n-1)) # 
        #   GW_level2 = c(obsData$depletion[time_start-1981+1],rep(NA,n-1)) 
        # } else {
        GW_level = c(dataframe$GWL_model[33], rep(NA,n-1))
        GW_level2 =c(dataframe$GWL_model[33], rep(NA,n-1))
        # }
        
        # Model stucture
        # Senario 1
        for(i in c(2:n)){
          
          ## Relationships
          #Extra.x(1:6) = [-8.1833;14.8316;-0.4073;0.5748;9.7157;12.8645]; % Supply
          #Extra.x(7:14) = [5.8565;2.5411;1.2198;-0.3031;-0.1198;1.2778;-5.8313;24.7550]; % Demand
          # Supply
          GW_S[i] = -8.1833* dataframe_sim$precip[i] + 19.7 #Old value: 14.8316
          RR_S[i] = -0.4073* dataframe_sim$precip[i] + 0.5748
          SW_S[i] = 9.7157* dataframe_sim$precip[i] + 12.8645
          
          # Demand
          WS_D[i] = 5.8565*dataframe_sim$precip[i] + 2.5411
          ENV_D[i] = 1.2198*dataframe_sim$precip[i] + -0.3031
          URBAN_D[i] = -0.1198*dataframe_sim$precip[i] + 1.2778
          ARG_D[i] = -5.8313*dataframe_sim$precip[i] + 24.7550
          
          # Apply efficiency scenarios
          #GW_S[i] = GW_S[i] * (100 - gwS)/100
          RR_S[i] = RR_S[i] * (100 + rrS)/100
          SW_S[i] = SW_S[i] * (100 + swS)/100
          ARG_D[i] = ARG_D[i] * (100 - argD)/100
          URBAN_D[i] = URBAN_D[i] * (100 - urbanD)/100
          # ENV_D[i] = ENV_D[i] * (100 - envD)/100
          # WS_D[i] = WS_D[i] * (100 - wsD)/100
          
          ## Scale supply to demand
          Supply = GW_S[i]+ RR_S[i] + SW_S[i]
          Demand = ARG_D[i] + URBAN_D[i]+ENV_D[i]+WS_D[i] 
          scale_SD = Demand/Supply
          
          GW_S[i] = scale_SD*GW_S[i]
          RR_S[i] = scale_SD*RR_S[i]
          SW_S[i] = scale_SD*SW_S[i]
          
          ## Get recharge value
          GW_RR1 = gwRR * dataframe_sim$precip[i] # How much recharge from precip
          GW_RR2 = -0.5172* dataframe_sim$precip[i] + 5.6198 # How much recharge from antropogenic
          GW_RR = GW_RR1 + GW_RR2
          
          ## Net Groundwater level
          GW_level[i] = GW_level[i-1] + GW_RR - GW_S[i]
        }
        
        
        # Model stucture
        # Senario 2
        for(i in c(2:n)){
          
          ## Relationships
          #Extra.x(1:6) = [-8.1833;14.8316;-0.4073;0.5748;9.7157;12.8645]; % Supply
          #Extra.x(7:14) = [5.8565;2.5411;1.2198;-0.3031;-0.1198;1.2778;-5.8313;24.7550]; % Demand
          # Supply
          GW_S[i] = -8.1833* dataframe_sim$precip[i] + 19.7 #Old value: 14.8316
          RR_S[i] = -0.4073* dataframe_sim$precip[i] + 0.5748
          SW_S[i] = 9.7157* dataframe_sim$precip[i] + 12.8645
          
          # Demand
          WS_D[i] = 5.8565*dataframe_sim$precip[i] + 2.5411
          ENV_D[i] = 1.2198*dataframe_sim$precip[i] + -0.3031
          URBAN_D[i] = -0.1198*dataframe_sim$precip[i] + 1.2778
          ARG_D[i] = -5.8313*dataframe_sim$precip[i] + 24.7550
          
          # Apply efficiency scenarios
          #GW_S[i] = GW_S[i] * (100 - gwS)/100
          RR_S[i] = RR_S[i] * (100 + rrS2)/100
          SW_S[i] = SW_S[i] * (100 + swS2)/100
          ARG_D[i] = ARG_D[i] * (100 - argD2)/100
          URBAN_D[i] = URBAN_D[i] * (100 - urbanD2)/100
          # ENV_D[i] = ENV_D[i] * (100 - envD)/100
          # WS_D[i] = WS_D[i] * (100 - wsD)/100
          
          ## Scale supply to demand
          Supply = GW_S[i]+ RR_S[i] + SW_S[i]
          Demand = ARG_D[i] + URBAN_D[i]+ENV_D[i]+WS_D[i] 
          scale_SD = Demand/Supply
          
          GW_S[i] = scale_SD*GW_S[i]
          RR_S[i] = scale_SD*RR_S[i]
          SW_S[i] = scale_SD*SW_S[i]
          
          ## Get recharge value
          GW_RR1 = gwRR * dataframe_sim$precip[i] # How much recharge from precip
          GW_RR2 = -0.5172* dataframe_sim$precip[i] + 5.6198 # How much recharge from antropogenic
          GW_RR = GW_RR1 + GW_RR2
          
          ## Net Groundwater level
          GW_level2[i] = GW_level2[i-1] + GW_RR - GW_S[i]
        }
        
        
        # Save the simulated GWL
        dataframe$GWL_model_senario1[(sim_start-1981+1):(sim_end-1981)] <- GW_level
        dataframe$GWL_model_senario2[(sim_start-1981+1):(sim_end-1981)] <- GW_level2
      }
      
      # plotting range
      dataframe_new <- dataframe %>% filter(year < time_end) # %>% filter(year > time_start-1)
      obsData <- obsData  %>% filter(year < time_end) #%>% filter(year > time_start-1)
      
      
      ### Demand and supply valiables
      
      scale =1
   #   if (input$unit == "km^3"){
   #     scale =1.23348
   #   }
      
      Supply <- data.frame(Groundwater = -GW_S*scale, 
                           RecycleReuse = -RR_S*scale,
                           Surfacewater = -SW_S*scale,
                           year = dataframe_sim$year,
                           sd = rep("Supply", length(GW_S)))
                           
     Demand <- data.frame(Agricutrure = ARG_D*scale,
                          Urban = URBAN_D*scale,
                          EnvironmentalReleases =ENV_D*scale,
                          WildandScenicDemand = WS_D*scale,
                          year = dataframe_sim$year,
                          sd = rep("Demand", length(GW_S)))
      
      Supplymelt <- melt(Supply, id = c("year", "sd"))
      
      Demandmelt <- melt(Demand, id = c("year", "sd"))
      
      #SupplyDemand <- rbind(Supplymelt, Demandmelt)
      
      #SupplyDemand_now <- rbind(SupplyDemand, SupplyDemand_past)
      
      ggplot()+ # y=gtReactive(), #SupplyDemand,aes(x=year,y=value, fill = variable)
        #geom_area(size=1, colour= "blue", fill = "blue", alpha = 0.5)+
        #geom_line(size=1, colour= "black")+
        geom_bar(data =Supplymelt,aes(x=year,y=value, fill = variable), stat="identity")+
        geom_bar(data =Demandmelt,aes(x=year,y=value, fill = variable), stat="identity")+
 #, s
       # 
     #   facet_grid(sd~., scales = "free_y") +
        #geom_ribbon(aes(ymin = 1, ymax = "precip"), fill = "blue") +
        #geom_text(aes(label = "average"), position = c(0,1)) +
        #scale_colour_manual(values = c("black", "grey")) +
        #scale_fill_manual(values = c("red", "blue")) +
        labs(x= "Time", y = "",fill="")+ #, title= "Precipitation ratio compared to average"
        theme_bw()+
        coord_flip()+
      #  scale_x_continuous(breaks = c(1980,1990,2000,2010,2020,2030,2040,2050,2060,2070,2080,2090)) +
       theme(legend.position="bottom", panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              #axis.text = element_text(size= 15),
             legend.direction = "vertical",
              axis.title = element_text(size= 15),
              legend.text = element_text(size= 10)) 
 #       geom_hline(yintercept = 1, color = "black") +
 #       guides(fill = guide_legend(order=1, colour=NA),
 #              colour = guide_legend(order=2, override.aes=aes(fill=NA)))

      
      }) # renderplot
    
}) # shiny server
