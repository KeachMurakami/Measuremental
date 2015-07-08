library(mclust) # 混合分布を扱うパッケージ mclust (Model-Based Clustering)の読み込み
library(RCurl) # webからのソースコード読み込みに使用

# 関数を読み込み
eval(parse(text = getURL("https://raw.githubusercontent.com/KeachMurakami/Sources/master/standard_funs.R", ssl.verifypeer = FALSE)))
eval(parse(text = getURL("https://raw.githubusercontent.com/KeachMurakami/Sources/master/summariser.R", ssl.verifypeer = FALSE)))


Grouper <-
  function(Plants, Group, Mode = "EII", file){
    csv_data <-
      file %>%
      read.csv %>%
      select(-1)
    
    #   csv_data <-
    #     gs_url(file) %>%
    #     gs_read %>%
    #     select(-1)
    mc <-
      csv_data %>%
      Mclust(., G = Plants, modelNames = Mode)
    # 混合分布モデルクラスタリングの実行。
    # クラスタ数はスライドバーでインプット
    
    results <-
      csv_data %>%
      mutate(Levels = LETTERS[mc$classification],
             PlantNo = 1:(dim(csv_data)[1]))
    # クラスタリング結果の抽出
    
    Results <-
      lapply(1:Plants, function(i){
        temp <-
          results %>%
          filter(Levels == LETTERS[i])
        Selected <- sample(dim(temp)[1], Group, replace = F)
        temp[Selected, ] %>%
          mutate(Groups = letters[1:Group]) %>%
          return
      }) %>%
      rbind_all
    
    bind_rows(Results, mutate(results, Groups = "x")) %>%
    arrange(Groups, PlantNo) %>%
    return
  }


shinyServer(function(input, output) {

  GroupedData <-
    reactive({ Grouper(input$plants, input$groups, file = input$file1$datapath) })

  output$message1 <- renderText({
    "<b>Means ± SDs</b> of parameters of each groups. <br>"
  })
  output$message2 <- renderText({
    "<b>Respective data point and SDs</b> were shown. <br> If any significant differences among the groups, <b>large *</b> will apear (according to Tukey's HSD test)."
  })

  output$Grouped <- renderTable({
    GroupedData() %>%
      select(PlantNo, Groups) %>%
      filter(Groups != "x") %>%
      arrange(Groups, PlantNo) %>%
      return
  })
  
  output$Stats <- renderTable({
    data1 <- GroupedData()
        data1 %>%
        select(-PlantNo, -Levels) %>%
        filter(Groups != "x") %>%
        summariser(labels = (dim(data1)[2] - 2)) %>%
        arrange(variable) %>%
        select(-SE) %>%
        mutate(Tukey = str_replace_all(Tukey, "a", ""), 
               Tukey = str_replace_all(Tukey, "b", "*")) %>%
        set_names(c("Groups", "variable", "value", "SD", "n", "Tukey")) %>%
        mutate(value = paste0(sprintf("%.1f", value), "±", sprintf("%.2f", SD))) %>%
        dcast(formula = variable ~ Groups, value.var = c("value")) %>%
        return
  })
  
  output$RawData <- renderChart2({
    GroupedData() %>%
      select(-PlantNo, -Levels) %>%
      arrange(Groups) %>%
      dTable(sPaginationType = input$pagination) %>%
      return
  })
  
  output$barPlot <- renderPlot({
    data2 <- GroupedData()
    stats <-
      data2 %>%
      select(-PlantNo, - Levels) %>%
      filter(Groups != "x") %>%
      summariser(labels = (dim(data2)[2] - 2)) %>%
      mutate(Tukey = str_replace_all(Tukey, "a", ""), 
             Tukey = str_replace_all(Tukey, "b", "*")) %>%
      set_names(c("Groups", "variable", "value", "SD", "SE", "n", "Tukey"))
    
    GroupedData() %>%
      select(-PlantNo) %>%
      filter(Groups != "x") %>%
      melt(id.vars = c("Levels", "Groups")) %>%
      ggplot(aes(x = Groups, y = value, fill = Groups)) +
      theme_bw(base_size = 20) +
      geom_point(position = position_jitter(width = 0.01), size = 5, col = "black", shape = 21, alpha = .7) +
      facet_grid(variable ~ ., scale = "free") +
      guides(fill = FALSE) +
      geom_text(data = stats, aes(y = value, label = Tukey), size = 20) +
      geom_errorbar(data = stats, aes(ymin = value - SD, ymax = value + SD), width = 0.1) %>%
      return
    })
  
})