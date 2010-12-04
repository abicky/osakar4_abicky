# 関数を使ったバージョン
# twilogFuncs.R に関数が定義されている

source("init.R")
source("twilogFuncs.R")

#----------#
#  諸情報  #
#----------#
cat("諸情報\n")

# ツイート日時を抽出．lapply だとリストで返ってくるので扱いにくい．
dates <- sapply(tweets, function(x) x@created)
# 日時オブジェクト (POSIXct) に変換．sapply だと属性が失われるので structure で属性を付け加える
dates <- structure(dates, class = c("POSIXt", "POSIXct"), tzone = "Asia/Tokyo")
# dates を出力するときのフォーマット
# (取得したツイートの日数の差が1年以上なら"年/月/日"，そうでなければ"月/日")
form <- ifelse(difftime(dates[1], dates[length(dates)]) > 365, "%y/%m/%d", "%m/%d")
# 文字列に変換
days <- format(dates, form)
# 最も古いツイートから最新のツイートまでの日付（daysと同じ表示形式）
alldays <- format(seq(as.Date(dates[length(dates)]), as.Date(dates[1]), by = "days"), format = form)
# days の因子．水準として alldays を指定
fdays <- factor(days, levels = alldays, order = TRUE)
# 水準ごとのカウント（水準にしないとつぶやかない日が考慮されない）
dtable <- table(fdays)
# ツイートごとのつぶやき文字数
tnchar <- sapply(tweets, function(x) nchar(x@text))
# 日付ごとのつぶやき文字数
dnchar <- tapply(tnchar, fdays, sum)
dnchar <- ifelse(is.na(dnchar), 0, dnchar)

# 実は"／"が正しく表示されない・・・
data <- data.frame("総つぶやき数" = length(tweets),
                   "つぶやいた日数" = length(unique(days)),
                   "つぶやかなかった日数" = sum(dtable == 0),
                   #"つぶやかなかった日数" = length(setdiff(alldays, days)),
                   "一日の平均つぶやき数" = round(mean(dtable), 1),
                   "一日の最高つぶやき数" = max(dtable),
                   "つぶやき文字数" = sum(tnchar),
                   "つぶやき文字数／件" = round(mean(tnchar), 1),
                   "つぶやき文字／日" = round(mean(dnchar)),
                   "コミュニケーション率" = round(sum(grepl("(?<!\\w)@\\w+(?!@)", sapply(tweets, function(x) x@text), perl = TRUE)) / length(tweets), 3),
                   "フォロワー／フォロー比率" = round(user@followersCount / user@friendsCount, 2),
                   "フォロー／フォロワー比率" = round(user@friendsCount / user@followersCount, 2))
rownames(data) <- screenName

print(data)
readline("Press Enter to continue")



#----------------------#
#  日ごとのつぶやき数  #
#----------------------#
cat("日ごとのつぶやき数\n")

# プロットする時のカラー
color = "red"

# 最近30日間
cat("最近30日間\n")
plotTwilog(dates, type = "days", col = color, n = 30, breaks = 15)
readline("Press Enter to continue")

# 最近90日間
cat("最近90日間\n")
plotTwilog(dates, type = "days", col = color, n = 90, breaks = 15)
readline("Press Enter to continue")

# 全期間
cat("全期間\n")
plotTwilog(dates, type = "days", col = color, breaks = 15)
readline("Press Enter to continue")



#----------------------#
#  月ごとのつぶやき数  #
#----------------------#
cat("月ごとのつぶやき数\n")

color <- "yellow3"
plotTwilog(dates, type = "months", col = color)
readline("Press Enter to continue")



#------------------------#
#  曜日ごとのつぶやき数  #
#------------------------#
cat("曜日ごとのつぶやき数\n")

color = "blue"
plotTwilog(dates, type = "weekdays", col = color, levs = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
readline("Press Enter to continue")



#------------------------#
#  時間ごとのつぶやき数  #
#------------------------#
cat("時間ごとのつぶやき数\n")

color = "chocolate1"
plotTwilog(dates, type = "times", col = color, levs = sprintf("%2d", 0:23))
readline("Press Enter to continue")



#----------------------#
#  合計つぶやき数推移  #
#----------------------#
cat("合計つぶやき数推移\n")

cumsums <- cumsum(table(fdays))
yrange <- c(0, cumsums[length(cumsums)])

# 最近30日間
cat("最近30日間\n")
y <- cumsums[-(1:(length(cumsums) - 30))]
plotTrans(y, fdays, yrange, n = 30)
readline("Press Enter to continue")    

# 最近90日間
cat("最近90日間\n")
y <- cumsums[-(1:(length(cumsums) - 90))]
plotTrans(y, fdays, yrange, n = 90)
readline("Press Enter to continue")    

# 全期間
cat("全期間\n")
y <- cumsums
plotTrans(y, fdays, yrange)
