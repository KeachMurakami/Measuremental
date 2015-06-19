library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(magrittr)
library(tidyr)
library(data.table)
library(rCharts)
library(RCurl)

shinyServer(function(input, output) {
  
  output$contents <- renderChart2({
    Basis <- input$Basis 
    Chl0 <-
      input$file1$datapath %>%
      ChlAnalyze
    Chl_ab <-
      Chl0 %>%
      melt(id.vars = c("Treatment", "PlantNo", "FW_g", "LA_cm2")) %>%
      filter(variable == "ab_ratio") %>%
      mutate(perArea = value, perFW = value) %>%
      select(-value)
    
    Chl0 %>%
      melt(id.vars = c("Treatment", "PlantNo", "FW_g", "LA_cm2")) %>%
      mutate(perArea = value * 0.1 / LA_cm2,
             perFW = value / FW_g) %>%
      select(-value) %>%
      filter(variable != "ab_ratio") %>%
      rbind(., Chl_ab) %>%
      dcast(formula = Treatment + PlantNo + FW_g + LA_cm2 ~ variable) %>%
      dTable(sPaginationType = input$pagination)
  })
  
  output$distPlot <- renderPlot({
    Basis <- input$Basis 
    Chl0 <-
      input$file1$datapath %>%
      ChlAnalyze
    Chl_ab <-
      Chl0 %>%
      melt(id.vars = c("Treatment", "PlantNo", "FW_g", "LA_cm2")) %>%
      filter(variable == "ab_ratio") %>%
      mutate(perArea = value, perFW = value) %>%
      select(-value)
    
    Chl0 %>%
      melt(id.vars = c("Treatment", "PlantNo", "FW_g", "LA_cm2")) %>%
      mutate(perArea = value * 0.1 / LA_cm2,
             perFW = value / FW_g) %>%
      select(-value) %>%
      filter(variable != "ab_ratio") %>%
      rbind(., Chl_ab) %>%
      select_("Treatment", "variable", Basis) %>%
      set_names(c("Treatment", "variable", "value")) %>%
      summariser(labels = 1:2) %>%      
      ggplot(aes(x = MainTrtm, y = ave, fill = MainTrtm)) +
      theme_bw(20) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = ave - SE, ymax = ave + SE), width = rel(.5)) +
      geom_text(aes(y = ave / 2, label = Tukey)) +
      facet_wrap(~ Trtms, scale = "free") %>%
    return
  })

  output$downloadData <- downloadHandler(
    filename = "Chl_shiny.csv",
    content = function(file) {
      Basis <- input$Basis 
      Chl0 <-
        input$file1$datapath %>%
        ChlAnalyze
      Chl_ab <-
        Chl0 %>%
        melt(id.vars = c("Treatment", "PlantNo", "FW_g", "LA_cm2")) %>%
        filter(variable == "ab_ratio") %>%
        mutate(perArea = value, perFW = value) %>%
        select(-value)
      
      Chl0 %>%
        melt(id.vars = c("Treatment", "PlantNo", "FW_g", "LA_cm2")) %>%
        mutate(perArea = value * 0.1 / LA_cm2,
               perFW = value / FW_g) %>%
        select(-value) %>%
        filter(variable != "ab_ratio") %>%
        rbind(., Chl_ab) %>%
        write.csv(., file)
    })

  })