library(mclust) #混合分布を扱うパッケージ mclust (Model-Based Clustering)の読み込み
library(RCurl)

# read in my function
eval(parse(text = getURL("https://raw.githubusercontent.com/KeachMurakami/Sources/master/standard_funs.R", ssl.verifypeer = FALSE)))
eval(parse(text = getURL("https://raw.githubusercontent.com/KeachMurakami/Sources/master/summariser.R", ssl.verifypeer = FALSE)))


Grouper <-function(Plants, Group, Mode = "EII", file){#file = "https://docs.google.com/spreadsheets/d/1hONdsPvBbvWO6sCVILCGTJ1UDgX1xB3iwuRUArPbTNQ"){
  csv_data <-
    input$file1$datapath %>%
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
    mutate(Groups = LETTERS[mc$classification])
  # クラスタリング結果の抽出
  
  Results <-
    lapply(1:Plants, function(i){
      temp <-
        results %>%
        mutate(PlantNo = 1:(dim(results)[1])) %>%
        filter(Groups == LETTERS[i])
      Selected <- sample(dim(temp)[1], Group, replace = F)
      temp[Selected, ] %>%
        mutate(Groups = letters[1:Group]) %>%
        return
    }) %>%
    rbind_all
  
  Stats <-
    Results %>%
    select(-PlantNo) %>%
    summariser(labels = (dim(Results)[2] - 1)) %>%
    arrange(variable) %>%
    select(-SE)
  
  Figs <-
    Stats %>%
    ggplot(aes(x = Groups, y = ave, fill = Groups)) +
    theme_bw(base_size = 20) +
    geom_bar(stat = "identity", col = "Black") +
    geom_errorbar(aes(ymin = ave, ymax = ave + SD), width = 0.3) +
    facet_grid(variable ~ ., scale = "free") +
    geom_text(aes(y = ave / 2, label = Tukey), size = 5) +
    guides(fill = FALSE)

  
  Grouping <-
    Results %>%
    arrange(Groups, PlantNo)
  
  Aves <-
    Stats %>%
    dcast(formula = variable ~ Groups, value.var = c("ave")) 
  SDs <-
    Stats %>%
    dcast(formula = variable ~ Groups, value.var = c("SD")) 
  
  Data <-
    bind_rows(mutate(Aves, Par = "ave"), mutate(SDs, Par = "SD"))
  
  return(list(Groups = Grouping, Stats = Stats, Figs = Figs, Data = Data))
  #   pa <-
  #     mc$parameters 
  #     pa$mean #平均ベクトル
  #     pa$pro  #混合分布の各分布の線形係数
  #     pa$variance$sigma #分散共分散行列
  #     pa$variance$sigma #分散共分散行列
}

shinyServer(function(input, output) {

  output$message1 <- renderText({
    "<b>Means + SDs</b> were shown with <b>Tukey's HSD test</b>."
  })
  
  output$Grouped <- renderTable({
    table1 <-
      Grouper(input$plants, input$groups) %>%
      .$Groups
    
    table1 %>%
      select(-(1:(dim(table1)[2] - 2))) %>%
      return
  })
  
  output$Data <- renderChart2({
    Grouper(input$plants, input$groups) %>%
      .$Data %>%
      dTable(sPaginationType = input$pagination) %>%
      return
  })

  output$Stats <- renderChart2({
  Grouper(input$plants, input$groups) %>%
    .$Stats %>%
    dTable(sPaginationType = input$pagination) %>%
    return
  })
  
  output$barPlot <- renderPlot({
    Grouper(input$plants, input$groups) %>%
      .$Figs %>%
      return
  })
  
})