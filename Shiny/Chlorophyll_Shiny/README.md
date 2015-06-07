**Chlorophyll_Shiny**は生葉からのクロロフィル抽出データを解析するツールです。手法の詳細については、[Porra et al. 1989](http://www.sciencedirect.com/science/article/pii/S0005272889803470)を参照されたい。  

   
server.Rまたはui.Rを起動し、Rstudioのエディターパネル右上の**Run app**をクリックしてください。  
**Choose and upload info.file**をクリックし、測定情報ファイル (example.xlsxを参照) アップロードします。  
****をクリックし、測定に用いたロガーのIDを選択します。  
**Apply Changes**をクリックすると、分析を行います。  

修正/加筆必要箇所  
* 単位を正確に表示  
* 乾物重ベース、総Chlベースなどの切り替えを実装 

今後の予定
* レポートファイルを出力できるように修正 (優先; html or csv?)  