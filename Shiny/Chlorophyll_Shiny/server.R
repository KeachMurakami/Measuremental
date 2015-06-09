library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(magrittr)
library(tidyr)
library(data.table)
library(rCharts)

ChlAnalyze <-
  function(file){
    
    df <-
      read.csv(file, stringsAsFactors = FALSE) %>%
      na.omit
      
    if(length(unique(df$PlantNo)) < dim(df)[1]){
      infoChl <-
        df[duplicated(df[, c("Treatment", "PlantNo")]), ] %>%
        select(Treatment, PlantNo, FW_g, LA_cm2)
    } else {
      infoChl <-
        df %>%
        select(Treatment, PlantNo, FW_g, LA_cm2)
    }
    
    dataChl <-
      df %>%
      mutate(Aa = A663.8 - A750.0,
             Ab = A646.8 - A750.0,
             Ac = A480.0 - A750.0
      ) %>%
      select(-A663.8, -A646.8, -A480.0, -A750.0)
    
    chlM <-
      (as.matrix(dataChl[, c("Aa", "Ab")]) %*% Porra) %>%
      as.data.frame %>%
      mutate(ab_ratio = V1 / V2) %>%
      set_names(c("chl_aM", "chl_bM", "ab_ratio"))
    
    chlW <-
      (as.matrix(dataChl[, c("Aa", "Ab", "Ac")]) %*% Wellburn) %>%
      as.data.frame %>%
      mutate(V3 = (V3 - 1.12 * V1 - 34.07 * V2) / 245) %>%
      set_names(c("chl_aW", "chl_bW", "carotenoids_W"))
    
      cbind(dataChl[, c("Treatment", "PlantNo")], chlM, chlW) %>%
      group_by(Treatment, PlantNo) %>%
      summarise_each(funs = "mean", chl_aM:carotenoids_W) %>%
      ungroup %>%
      merge(., infoChl, by = c("Treatment", "PlantNo")) %>%
      return
  }

  # for calc based on mol
  Porra <- matrix(c(13.43, -3.47, -5.38, 22.90), 2, 2)
  # for calc based on weight
  Wellburn <- matrix(c(12, -3.11, 0, -4.88, 20.78, 0, 0, 0, 1000), 3, 3)

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