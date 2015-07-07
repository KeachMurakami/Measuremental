library(mclust) #混合分布を扱うパッケージ mclust (Model-Based Clustering)の読み込み
library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(magrittr)
library(tidyr)
library(reshape2)
library(data.table)
library(rCharts)
library(RCurl)
library(googleVis)

# read in my function
eval(parse(text = getURL("https://raw.githubusercontent.com/KeachMurakami/Sources/master/summariser.R", ssl.verifypeer = FALSE)))
mean2 <- function(x) mean(x, na.rm = T)
sd2 <- function(x) sd(x, na.rm = TRUE)
se <- function(x) sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x)))
length2 <- function(x) sum(!is.na(x))


Grouper <-function(file, Plants, Group, mode = "EII"){
  library(mclust) #混合分布を扱うパッケージ mclust (Model-Based Clustering)の読み込み

  mc <-
    file %>%
    read.csv %>%
    select(-1) %>%
    Mclust(., G = Plants, modelNames = mode)
  #混合分布モデルクラスタリングの実行。クラスタ数はスライドバーでインプット
  
  results <-
    file %>%
    read.csv %>%
    mutate(Groups = LETTERS[mc$classification]) #クラスタリング結果の抽出
  
  Results <-
    lapply(1:Plants, function(i){
      temp <-
        results %>%
        filter(Groups == LETTERS[i])
      Selected <- sample(dim(temp)[1], Group, replace = F)
      temp[Selected, ] %>%
        mutate(Groups = letters[1:Group],
               ID = Selected) %>%
        return
    }) %>%
    rbind_all
  
  Stats <-
    Results %>%
    select(-ID) %>%
    summariser(labels = (dim(Results)[2] - 1)) %>%
    arrange(variable) %>%
    select(-SE)
  
  Figs <-
    Stats %>%
    ggplot(aes(x = Groups, y = ave, fill = Groups)) +
    geom_bar(stat = "identity", col = "Black") +
    geom_errorbar(aes(ymin = ave, ymax = ave + SD), width = 0.3) +
    facet_grid(variable ~ ., scale = "free") +
    geom_text(aes(y = ave / 2, label = Tukey), size = 5)
  
  Grouping <-
    Results %>%
    arrange(Groups, ID)
  
  Aves <-
    Stats %>%
    dcast(formula = variable ~ Groups, value.var = c("ave")) 
  SDs <-
    Stats %>%
    dcast(formula = variable ~ Groups, value.var = c("SD")) 
  
  Data <-
    bind_rows(mutate(Aves, Par = "ave"), mutate(SDs, Par = "SD"))
  
  return(list(Groups = Grouping, Stats = Stats, Figs = Figs, Data = Data))
}
  #   pa <-
  #     mc$parameters 
  #     pa$mean #平均ベクトル
  #     pa$pro  #混合分布の各分布の線形係数
eval(parse(text = getURL("https://raw.githubusercontent.com/KeachMurakami/Sources/master/summariser.R", ssl.verifypeer = FALSE)))
  #     pa$variance$sigma #分散共分散行列
  #     pa$variance$sigma #分散共分散行列

shinyServer(function(input, output) {

  output$Grouped <- renderChart2({
    Grouper(input$file1$datapath, input$plants, input$groups) %>%
      .$Groups %>%
      dTable(sPaginationType = input$pagination) %>%
      return
  })
  
  output$Data <- renderChart2({
    Grouper(input$file1$datapath, input$plants, input$groups) %>%
      .$Data %>%
      dTable(sPaginationType = input$pagination) %>%
      return
  })

  output$Stats <- renderChart2({
  Grouper(input$file1$datapath, input$plants, input$groups) %>%
    .$Stats %>%
    dTable(sPaginationType = input$pagination) %>%
    return
  })
  
  output$distPlot <- renderPlot({
    Grouper(input$file1$datapath, input$plants, input$groups) %>%
      .$Figs %>%
      return
  })
  
})