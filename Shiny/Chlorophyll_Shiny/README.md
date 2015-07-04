**Chlorophyll_Shiny**は生葉からのクロロフィル抽出データを解析するツールです。

手法の詳細については、[Porra et al. 1989](http://www.sciencedirect.com/science/article/pii/S0005272889803470)を参照されたし。  

server.Rまたはui.Rを起動し、Rstudioのエディターパネル右上の**Run app**をクリックしてください。  
**Choose and upload info.file**をクリックし、測定情報ファイル (example.csvを参照) アップロードします。  
**Display based on**から、面積or新鮮重ベースでの計算を選択します。  
**Apply Changes**をクリックすると、分析を行います。    
**Download the table**をクリックすると、分析結果のcsvファイルをダウンロードできます。

特記  
* 図中のMの場合はmmolでの、Wの場合はgでの表示  

修正/加筆必要箇所  
* 単位を正確に表示  
* 乾物重ベース、総Chlベースなどの切り替えを実装 

今後の予定
* レポートファイルを出力できるように修正 (優先; html or csv?; 一応実装したものの、いまいち)  
