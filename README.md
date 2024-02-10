---
title: 再生可能エネルギーの「出力制御」記事のデータ集計
author: "小宮山亮磨 @ryomakom"
---

朝日新聞は2024年2月10日朝刊で、再生可能エネルギー、つまり太陽光と風力の発電量を意図的に減らしているケース（これを「出力制御」というらしい）が2023年に急増していることを報じた。デジタル版は[こちら](https://digital.asahi.com/articles/ASS296K84S10TIPE026.html)。電力各社が公表しているデータをどう加工して集計したかを、以下で説明する。

データは各社のウェブサイトの「需給実績」というところにあって、火力や水力など方式ごとの発電量や、再エネの出力制御量が1時間ごとに示されている。つまり1日あたり24行、年間だと約9千行ある。電力各社について過去数年分のデータをまとめるのは、excelだとちょっと骨が折れる（ので、旧知の先輩である安田朋起記者がデータ報道チームの私に声をかけた）。

北海道電力：https://www.hepco.co.jp/network/renewable_energy/fixedprice_purchase/supply_demand_results.html

東北電力：https://setsuden.nw.tohoku-epco.co.jp/download.html

中部電力：https://powergrid.chuden.co.jp/denkiyoho/

北陸電力：https://www.rikuden.co.jp/nw_jyukyudata/area_jisseki.html

関西電力：https://www.kansai-td.co.jp/denkiyoho/area-performance.html

中国電力：https://www.energia.co.jp/nw/service/retailer/data/area/

四国電力：https://www.yonden.co.jp/nw/renewable_energy/data/supply_demand.html

九州電力：https://www.kyuden.co.jp/td_service_wheeling_rule-document_disclosure.html

面倒なのは、データ公表の目的は各社同じのはずなのに、データの形式が微妙にちがっていること。ファイルがcsvだったりexcelだったり、はたまたtxtファイルだったりするだけでなく、電力量の単位がメガワット時で共通かと思いきや、四電だけは「万kW」だったりするので注意が必要（というか、正しくは「万kw時」のはず。四電の担当者が電力量の数え方をご存じないとは思いませんが…）。

あと、会社ごとにファイルをひとつにまとまっておらず、月ごとだったり四半期ごとだったりにバラバラになっているのも難点。それからファイル冒頭の但し書きとか、列の並び方とか、時刻の表記方法とかが会社ごとにちょっとずつ違うので、これを共通の形式に書き換えてまとめるのが一苦労。たぶん3時間くらいの作業だった。

逆に言えば、そこまでやってしまえば、後は単なる集計作業なので非常に楽。[熱中症を調べたとき](https://ryomakom.github.io/heat_stroke/)のようないわゆる統計分析的なことは、まったくやっていません。

というわけで、以下がコードです。

## パッケージ読み込み

いつものtidyverseと、時間データを操るためのlubridateを呼び出す。

```{r setup, results=FALSE}
library(tidyverse)
library(lubridate)
```

## データ加工

最初に北海道電力。
```{r hokkaido, message=FALSE}
# 列の名前を横文字に変えておくための下準備
new_col_names_hokkaido <- c("date","time","area_demand","atomic","thermal","hydro","geothermal","biomass","solar_generate","solar_restraint","wind_generate","wind_restraint","pumped_hydro","interconnect")

# フォルダ内のCSVファイルのリストを取得
file_hokkaido <- list.files(path = "data/hokkaido", pattern = "\\.csv$", full.names = TRUE)

# 各ファイルを読み込み、tibbleに格納（Shift-JISを使用、最初の行をスキップ）
# その後、最初の14列の列名を変更
hokkaido_list <- lapply(file_hokkaido, function(file) {
  df <- read_csv(file, locale = locale(encoding = "Shift-JIS"), skip = 3) %>%
    mutate_all(as.character)
  colnames(df)[1:14] <- new_col_names_hokkaido
  return(df)
})

# すべてのデータを一つのtibbleに結合
hokkaido <- bind_rows(hokkaido_list) %>%
  fill(date, .direction = "down") %>% # 日付が空欄になっているところを埋める
  mutate(datetime=ymd_h(str_c(date," ",str_sub(time,-3,-2)))) %>%
  select(datetime,everything()) %>% 
  select(-date,-time) %>% 
  mutate(across(-c(1), as.numeric)) %>%   # 1列目以外を実数形式に変換
  mutate(year=year(datetime),month=month(datetime),fiscalyear=ifelse(month>=4,year,year-1)) %>% # 「年」と「年度」の列を作る
  mutate(area="hokkaido") %>%
  select(area,datetime,year,fiscalyear,month,everything()) %>% 
  filter(!is.na(area_demand)) %>% # 空っぽのデータを削除
  arrange(datetime)
```

次に東北電力。

```{r tohoku, message=FALSE}
# 列名変更の準備
new_col_names_tohoku <- c("datetime","area_demand","hydro","thermal","atomic","solar_generate","solar_restraint","wind_generate","wind_restraint","geothermal","biomass","pumped_hydro","interconnect")

# フォルダ内のCSVファイルのリストを取得
file_tohoku <- list.files(path = "data/tohoku", pattern = "\\.txt$", full.names = TRUE)

# 各ファイルを読み込み、tibbleに格納（Shift-JISを使用、最初の行をスキップ）
# その後、最初の13列の列名を変更
tohoku_list <- lapply(file_tohoku, function(file) {
  df <- read_csv(file, locale = locale(encoding = "Shift-JIS")) %>%
    mutate_all(as.character)
  colnames(df)[1:13] <- new_col_names_tohoku
  return(df)
})

# すべてのデータを一つのtibbleに結合
tohoku <- bind_rows(tohoku_list) %>%
  mutate(across(-1, as.numeric)) %>%   # 1列目以外を実数形式に変換
  mutate(datetime=ymd_hm(datetime)) %>% 
  mutate(year=year(datetime),month=month(datetime),fiscalyear=ifelse(month>=4,year,year-1)) %>%
  mutate(area="tohoku") %>% 
  select(area,datetime,year,fiscalyear,month,everything())
```

中部電力。

```{r chubu, message=FALSE}
new_col_names_chubu <- c("date","time","area_demand","atomic","thermal","hydro","geothermal","biomass","solar_generate","solar_restraint","wind_generate","wind_restraint","pumped_hydro","interconnect")

# フォルダ内のCSVファイルのリストを取得
file_chubu <- list.files(path = "data/chubu", pattern = "\\.csv$", full.names = TRUE)

# 各ファイルを読み込み、tibbleに格納（Shift-JISを使用、最初の行をスキップ）
# その後、最初の14列の列名を変更
chubu_list <- lapply(file_chubu, function(file) {
  df <- read_csv(file, locale = locale(encoding = "Shift-JIS"), skip = 4) %>%
    mutate_all(as.character)
  colnames(df)[1:14] <- new_col_names_chubu
  return(df)
})

# すべてのデータを一つのtibbleに結合
chubu <- bind_rows(chubu_list) %>%
  mutate(across(-c(1,2), as.numeric)) %>%   # 1,2列目以外を実数形式に変換
  mutate(datetime=ymd_hms(str_c(date," ",time))) %>% 
  mutate(year=year(date),month=month(date),fiscalyear=ifelse(month>=4,year,year-1)) %>%
  select(-date,-time) %>%
  mutate(area="chubu") %>% 
  select(area,datetime,year,fiscalyear,month,everything()) %>% 
  arrange(datetime)
```

北陸電力。

```{r hokuriku, message=FALSE}
new_col_names_hokuriku <- c("date","time","area_demand","atomic","thermal","hydro","geothermal","biomass","solar_generate","solar_restraint","wind_generate","wind_restraint","pumped_hydro","interconnect")

# フォルダ内のCSVファイルのリストを取得
file_hokuriku <- list.files(path = "data/hokuriku", pattern = "\\.csv$", full.names = TRUE)

# 各ファイルを読み込み、tibbleに格納（Shift-JISを使用、最初の行をスキップ）
# その後、最初の14列の列名を変更
hokuriku_list <- lapply(file_hokuriku, function(file) {
  df <- read_csv(file, locale = locale(encoding = "Shift-JIS"), skip = 5) %>%
    mutate_all(as.character)
  colnames(df)[1:14] <- new_col_names_hokuriku
  return(df)
})

# すべてのデータを一つのtibbleに結合
hokuriku <- bind_rows(hokuriku_list) %>%
  mutate(time=ifelse(str_sub(time,-5,-1)=="00:00",time,str_c(time,":00"))) %>% 
  filter(date!="DATE") %>% 
  mutate(across(-c(1,2), as.numeric)) %>%   # 1,2列目以外を実数形式に変換
  mutate(datetime=ymd_hms(str_c(date," ",time))) %>% 
  mutate(year=year(date),month=month(date),fiscalyear=ifelse(month>=4,year,year-1)) %>%
  select(-date,-time) %>%
  mutate(area="hokuriku") %>%
  select(area,datetime,year,fiscalyear,month,everything()) %>% 
  arrange(datetime)
```

関西電力。

```{r kansai, message=FALSE}
new_col_names_kansai <- c("datetime","area_demand","atomic","thermal","hydro","geothermal","biomass","solar_generate","solar_restraint","wind_generate","wind_restraint","pumped_hydro","interconnect")

# フォルダ内のCSVファイルのリストを取得
file_kansai <- list.files(path = "data/kansai", pattern = "\\.csv$", full.names = TRUE)

# 各ファイルを読み込み、tibbleに格納（Shift-JISを使用、最初の行をスキップ）
# その後、最初の13列の列名を変更
kansai_list <- lapply(file_kansai, function(file) {
  df <- read_csv(file, locale = locale(encoding = "Shift-JIS"), skip = 1) %>%
    mutate_all(as.character)
  colnames(df)[1:13] <- new_col_names_kansai
  return(df)
})


# すべてのデータを一つのtibbleに結合
kansai <- bind_rows(kansai_list) %>%
  filter(!is.na(area_demand)) %>% 
  mutate(across(-1, as.numeric)) %>%   # 1列目以外を実数形式に変換
  mutate(datetime=ymd_hm(datetime)) %>% 
  mutate(year=year(datetime),month=month(datetime),fiscalyear=ifelse(month>=4,year,year-1)) %>%
  mutate(area="kansai") %>% 
  select(area,datetime,year,fiscalyear,month,everything())
```

中国電力。

```{r chugoku, message=FALSE}
new_col_names_chugoku <- c("date","time","area_demand","atomic","thermal","hydro","geothermal","biomass","solar_generate","solar_restraint","wind_generate","wind_restraint","pumped_hydro","interconnect")


# フォルダ内のCSVファイルのリストを取得
file_chugoku <- list.files(path = "data/chugoku", pattern = "\\.csv$", full.names = TRUE)

# 各ファイルを読み込み、tibbleに格納（Shift-JISを使用、最初の行をスキップ）
# その後、最初の14列の列名を変更
chugoku_list <- lapply(file_chugoku, function(file) {
  df <- read_csv(file, locale = locale(encoding = "Shift-JIS"), skip = 4) %>%
    mutate_all(as.character)
  colnames(df)[1:14] <- new_col_names_chugoku
  return(df)
})

# すべてのデータを一つのtibbleに結合
chugoku <- bind_rows(chugoku_list) %>%
  mutate(across(everything(), ~ ifelse(. == "－", 0, .))) %>%
  mutate(across(-c(1,2), as.numeric)) %>%   # 1,2列目以外を実数形式に変換
  mutate(datetime=ymd_hms(str_c(date," ",time))) %>% 
  mutate(year=year(date),month=month(date),fiscalyear=ifelse(month>=4,year,year-1)) %>%
  select(-date,-time) %>%
  mutate(area="chugoku") %>%
  select(area,datetime,year,fiscalyear,month,everything()) %>%
  filter(!is.na(area_demand)) %>% 
  arrange(datetime)
```

四国電力。

```{r shikoku, message=FALSE}
new_col_names_shikoku <- c("date","time","area_demand","atomic","thermal","hydro","geothermal","biomass","solar_generate","solar_restraint","wind_generate","wind_restraint","pumped_hydro","interconnect")


# フォルダ内のCSVファイルのリストを取得
file_shikoku <- list.files(path = "data/shikoku", pattern = "\\.csv$", full.names = TRUE)

# 各ファイルを読み込み、tibbleに格納（Shift-JISを使用、最初の行をスキップ）
# その後、最初の14列の列名を変更
shikoku_list <- lapply(file_shikoku, function(file) {
  df <- read_csv(file, locale = locale(encoding = "UTF-8"), skip = 8) %>%
    mutate_all(as.character)
  colnames(df)[1:14] <- new_col_names_shikoku
  return(df)
})

# すべてのデータを一つのtibbleに結合
shikoku <- bind_rows(shikoku_list) %>%
  filter(!is.na(area_demand)) %>% 
  mutate(across(everything(), ~ ifelse(. == "－", 0, .))) %>%
  mutate(across(-c(1,2), as.numeric)) %>%   # 1,2列目以外を実数形式に変換
  mutate(across(3:last_col(), ~ .x * 10)) %>% # 3列目以降に10をかける。他社と電力量の単位をそろえるため
  mutate(datetime=ymd_hms(str_c(date," ",time))) %>% 
  mutate(year=year(date),month=month(date),fiscalyear=ifelse(month>=4,year,year-1)) %>%
  select(-date,-time) %>%
  mutate(area="shikoku") %>%
  select(area,datetime,year,fiscalyear,month,everything()) %>% 
  arrange(datetime)
```

九州電力。

```{r kyushu, message=FALSE}
new_col_names_kyushu <- c("datetime","area_demand","atomic","thermal","hydro","geothermal","biomass","solar_generate","solar_restraint","wind_generate","wind_restraint","pumped_hydro","interconnect")


# フォルダ内のCSVファイルのリストを取得
file_kyushu <- list.files(path = "data/kyushu", pattern = "\\.csv$", full.names = TRUE)

# 各ファイルを読み込み、tibbleに格納（Shift-JISを使用、最初の行をスキップ）
# その後、最初の13列の列名を変更
kyushu_list <- lapply(file_kyushu, function(file) {
  df <- read_csv(file, locale = locale(encoding = "Shift-JIS"), skip = 1) %>%
    mutate_all(as.character)
  colnames(df)[1:13] <- new_col_names_kyushu
  return(df)
})

# すべてのデータを一つのtibbleに結合
kyushu <- bind_rows(kyushu_list) %>%
  mutate(across(-1, as.numeric)) %>%   # 1列目以外を実数形式に変換
  mutate(datetime=ymd_hm(datetime)) %>% 
  mutate(year=year(datetime),month=month(datetime),fiscalyear=ifelse(month>=4,year,year-1)) %>%
  mutate(area="kyushu") %>% 
  filter(!is.na(area_demand)) %>% 
  select(area,datetime,year,fiscalyear,month,everything())
```

各社のデータを合体させる。

```{r all, message=FALSE}
elec <- bind_rows(chubu,chugoku,hokkaido,hokuriku,kansai,kyushu,shikoku,tohoku) %>% 
  select(-...18,-...19)
```

## 分析作業

上に書いたように、分析とはいってもとても単純なもので、出力制御の量を月ごとだったり年ごとだったりに足し合わせたり、あるいは発電量に占める制御量の割合をはじいたりするだけ。それをグラフにしたりcsvファイルにしたりして、同僚に送るという。

集計でミスがなかったかどうかは、経産省の[資料](https://www.meti.go.jp/shingikai/enecho/shoene_shinene/shin_energy/keito_wg/pdf/048_01_00.pdf)にあったグラフや数値と照らし合わせることで確認した。


```{r analysis}
r_rate <- elec %>%
  group_by(year,month,area) %>%
  summarize(generate=sum(solar_generate)+sum(wind_generate),
            restraint=sum(solar_restraint)+sum(wind_restraint)) %>%
  mutate(date=as.Date(str_c(year,"-",month,"-1")),
         restraint_rate=restraint/(generate+restraint)) %>%
  ggplot(aes(x=date,y=restraint_rate*100,color=area)) +
  geom_line() +
  labs(title="各電力の制御率",y="制御率(%)")

r_volume_year <- elec %>%
  group_by(year,area) %>%
  summarize(restraint=sum(solar_restraint)+sum(wind_restraint)) %>%
  ggplot(aes(year,restraint/1000,fill=area)) +
  geom_bar(stat="identity") +
  labs(title="「年別」に見た制御量の変遷",x="年",y="制御量（GWh）")

r_volume_fiscalyear <- elec %>%
  group_by(fiscalyear,area) %>%
  summarize(restraint=sum(solar_restraint)+sum(wind_restraint)) %>%
  ggplot(aes(fiscalyear,restraint/1000,fill=area)) +
  geom_bar(stat="identity") +
  labs(title="「年度別」に見た制御量の変遷",x="年度",y="制御量（GWh）")


r_volume_month <- elec %>%
  group_by(area, year, fiscalyear, month) %>%
  summarize(generate = sum(solar_generate) + sum(wind_generate),
            restraint = sum(solar_restraint) + sum(wind_restraint),
            .groups = 'drop') %>%
  mutate(date = as.Date(str_c(year, "-", month, "-1")),
         area = factor(area,
                       levels = c("kyushu", "shikoku", "chugoku", "kansai", "hokuriku", "chubu", "tohoku", "hokkaido"))) %>%
  ggplot(aes(date, restraint/1000, fill = area)) +
  geom_bar(stat = "identity") +
  labs(title="「月別」に見た制御量の変遷",x="月",y="制御量（GWh）")


# グラフ表示

r_rate
r_volume_year
r_volume_fiscalyear
r_volume_month

# 同僚にわたすcsvファイル作り

elec %>%
  group_by(area,year,fiscalyear,month) %>%
  summarize(generate=sum(solar_generate)+sum(wind_generate),
            restraint=sum(solar_restraint)+sum(wind_restraint)) %>%
  write_excel_csv("data/実績と制御量.csv")

elec %>%
  group_by(year,month,area) %>%
  summarize(generate=sum(solar_generate)+sum(wind_generate),
            restraint=sum(solar_restraint)+sum(wind_restraint)) %>%
  mutate(restraint_rate=restraint/(generate+restraint)) %>% 
  arrange(area) %>% 
  write_excel_csv("data/月ごと制御率.csv")

elec %>%
  group_by(year,area) %>%
  summarize(generate=sum(solar_generate)+sum(wind_generate),
            restraint=sum(solar_restraint)+sum(wind_restraint)) %>%
  mutate(restraint_rate=restraint/(generate+restraint)) %>%
  arrange(area) %>% 
  write_excel_csv("data/年ごと制御率.csv")

elec %>%
  group_by(fiscalyear,area) %>%
  summarize(generate=sum(solar_generate)+sum(wind_generate),
            restraint=sum(solar_restraint)+sum(wind_restraint)) %>%
  mutate(restraint_rate=restraint/(generate+restraint)) %>%
  arrange(area) %>% 
  write_excel_csv("data/年度ごと制御率.csv")

elec %>%
  mutate(day=day(datetime)) %>%
  group_by(area,year,month,day) %>%
  summarize(restraint=sum(solar_restraint)+sum(wind_restraint)) %>%
  mutate(制御あり=ifelse(restraint>0,1,0)) %>%
  group_by(area,year,month) %>%
  summarize(制御があった日数=sum(制御あり)) %>%
  write_excel_csv("data/制御があった日数.csv")

```

以上。
