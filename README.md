
# 大學心理系專題研究: 空間記憶測驗-前測

## 指導教授 
  楊政達

## 參與人員
  陳彥含、馮書軒、蔡旨軒、林賀軒、李哲維

## 空間記憶能力遊戲簡介:  
  玩家將被要求記憶接下來看到的1～6個立體圖形，每個圖形呈現5秒的時間，結束後將出現一個立體圖形，玩家必須判斷該立體圖形是否曾經出現在前面所記憶的立體圖形中。
  遊戲的題目設計參考教育部課綱中數學幾何與立體圖形的課程安排，題目中所出現的圖形類別包含: 三角柱體、四角柱體、五角柱體、六角柱體、圓柱、三角錐體、四角錐體、五角錐體、六角錐體、圓錐，共十種類別，皆為國小五年級之數理課程或生活中常見之立體圖形類別。各類別之下，會依照其邊角關係再細分十種立體圖形，以四角錐之類別為例，此類別下將會有底面為正方形、長方形、菱形、梯形、平行四邊形等，邊角與立體高度不一的十種立體圖形。在此架構下，我們能夠透過操弄題目的需記憶之立體圖數量、需記憶之立體圖類別組合、需判別之立體圖形與記憶立體圖之關係，以調整遊戲的難易度。然而，上述三種變項間的交互作用卻屬於未知，因此我們將進行前測以蒐集各題型(三種變項之組合方式)之正確率與反應時間，分析並將各題型之難度等級排序為1～6個等級。

## 研究方法
  本研究透過教育部課綱規劃出各圖形種類與參數，依所設之參數使用Blender建模工具生成立體圖形，並使用Unity開發遊戲。由於VR器材的數量限制與最適使用時限，實驗將利用筆電或電腦進行題目的前測驗，目的在於將題型的規則的難度分為六個難易等級，結果將用於遊戲過程中玩家的晉級程度，事後也能用於分析小學生之空間記憶常模。前測預計將於寒假期間收受試者，將招募15名受試者，每一位受試者將參與2小時的試題測驗，過程將會有120~150題不等的題目。
## 難度分析:
  遊戲過程中，會將玩家在各題目表現的正確率與反應時間記錄下來，事後進行多變量分析將各題分為六個等級。前測資料分析以R語言實作k-平均演算法(k-means clustering)進行聚類分析(clustering)。更進一步的，本研究根據上述之分群結果進行難度與各變項之間的ANOVA分析。

## 前測分析結果:
  透過反應時間與正確率，我們可以將這些題目分成十個難度，越靠近右下方者為正確率較高且反應時間較短，是難易度較簡單的題目群，靠近左上者為正確率較低且反應時間較長，是難易度較困難的題目群。
  根據分群結果進行難度與各變項之間的ANOVA分析，發現難度與數量(p=0.014)及難度與大類相似度(p=0.017)呈顯著，符合預期假設，而次類相似度則不成顯著(p=0.175)。

## 討論
  建模的過程中我們注意到三維空間之立體圖形的辨識較二維平面圖形困難，三維空間需要以多角度的動態觀察去記憶一個立體圖形，而非一個固定角度，特別是當兩個圖形於同一個類別之下(如三角錐體之中的等腰鈍角三角錐與正三角錐)，若僅以單一角度觀察將難以辨別。因此在遊戲操作上我們希望玩家能在虛擬空間中把玩各立體圖形，以記憶圖形。然而，有一項研究探討虛擬實境與現實操作之作業時間，發現虛擬實境在操作上耗時顯著高於現實的操作，而考慮到遊戲題目皆有時間限制，因此在這一系列的遊戲中，要給予玩家多大的操作自由度才能達到足夠的遊戲體驗與作業效率，將是一個需要不斷迭代的過程。

## 心得
  在進實驗室的第一個月，由於我們團隊在unity的開發技術不足，加上團隊間需要以英文溝通，容易出現文化上與溝通上不協調的狀況，因此剛開始就陷入停滯的狀態。之後做了一些人員調動，有了多媒體背景與電機背景的夥伴加入，每個角色都能更專注於擅長的領域，使這個計畫團隊漸入佳境。在這個團隊裡，有幸能與不同背景的夥伴合作，讓我能與不同領域的人事物學習，開拓視野，收穫甚多。

## 參考資料
Po-Han Lin & Steven J. Luck (2009) The influence of similarity on visual working memory representations, Visual Cognition,
17:3,356-372, DOI: 10.1080/13506280701766313
周瑜, 刘俊涛, 白翔. 形状匹配方法研究与展望. 自动化学报, 2012, 38(6):889−910 DOI 10.3724/SP.J.1004.2012.00889
教育部，民國108，十二年國民基本教育課程綱要。

