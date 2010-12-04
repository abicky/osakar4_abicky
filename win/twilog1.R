# 関数使わないバージョン
# 置換を使えど修正が大変だった・・・

source("init.R")

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
color <- "red"

# 最近30日間
cat("最近30日間\n")
n <- 30
fdays30 <- fdays
# つぶやき日数がn日よりも多ければデータを削る
if (length(levels(fdays)) > n) {
    fdays30 <- fdays[!(fdays %in% levels(fdays)[1:(length(levels(fdays)) - n)]), drop = TRUE]
}

# plot.factor
cat("plot using plot.factor\n")
plot(fdays30, xlab = "", ylab = "", col = color, border = color, space = 0.7)
# same as
# barplot(table(fdays30), xlab = "", ylab = "", col = color, border = color, space= 0.7)
readline("Press Enter to continue")

# ggplot2
cat("plot using ggplot2\n")
c <- ggplot(mapping = aes(fdays30))
c <- c + geom_bar(fill = color, alpha = 0.7, width = 0.7) + xlab("") + ylab("")
c <- c + scale_x_discrete(breaks = levels(fdays30)[seq(1, length(levels(fdays30)), len = 15)])
print(c)
readline("Press Enter to continue")


# 最近90日間
cat("最近90日間\n")
n <- 90
fdays90 <- fdays
if (length(levels(fdays)) > n) {
    fdays90 <- fdays[!(fdays %in% levels(fdays)[1:(length(levels(fdays)) - n)]), drop = TRUE]
}

# plot.factor
cat("plot using plot.factor\n")
plot(fdays90, xlab = "", ylab = "", col = color, border = color, space = 0.7)
readline("Press Enter to continue")

# ggplot2
cat("plot using ggplot2\n")
c <- ggplot(mapping = aes(fdays90))
c <- c + geom_bar(fill = color, alpha = 0.7, width = 0.7) + xlab("") + ylab("")
c <- c + scale_x_discrete(breaks = levels(fdays90)[seq(1, length(levels(fdays90)), len = 15)])
print(c)
readline("Press Enter to continue")


# 全期間
cat("全期間\n")

# plot.factor
cat("plot using plot.factor\n")
plot(fdays, xlab = "", ylab = "", col = color, border = color, space = 0.7)
readline("Press Enter to continue")

# ggplot2
cat("plot using ggplot2\n")
c <- ggplot(mapping = aes(fdays))
c <- c + geom_bar(fill = color, alpha = 0.7, width = 0.7) + xlab("") + ylab("")
# メモリを15個に絞る
c <- c + scale_x_discrete(breaks = levels(fdays)[seq(1, length(levels(fdays)), len = 15)])
print(c)
readline("Press Enter to continue")



#----------------------#
#  月ごとのつぶやき数  #
#----------------------#
cat("月ごとのつぶやき数\n")

color <- "yellow3"
months <- format(dates, "%Y/%m")
fmonths <- factor(months, levels = rev(unique(months)), order = TRUE)

# plot.factor
cat("plot using plot.factor\n")
plot(fmonths, xlab = "", ylab = "", col = color, border = color, space=0.7)
readline("Press Enter to continue")

# ggplot2
cat("plot using ggplot2\n")
c <- ggplot(mapping = aes(fmonths))
c <- c + geom_bar(fill = color, alpha = 0.7, width = 0.7) + xlab("") + ylab("")
print(c)
readline("Press Enter to continue")



#------------------------#
#  曜日ごとのつぶやき数  #
#------------------------#
cat("曜日ごとのつぶやき数\n")

color <- "blue"
wdays <- format(dates, "%a")
fwdays <- factor(wdays, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), order = TRUE)

# plot.factor
cat("plot using plot.factor\n")
plot(fwdays, xlab = "", ylab = "", col = color, border = color, space=0.7)
readline("Press Enter to continue")

# ggplot2
cat("plot using ggplot2\n")
c <- ggplot(mapping = aes(fwdays))
c <- c + geom_bar(fill = color, alpha = 0.7, width = 0.7) + xlab("") + ylab("")
print(c)
readline("Press Enter to continue")



#------------------------#
#  時間ごとのつぶやき数  #
#------------------------#
cat("時間ごとのつぶやき数\n")

color <- "chocolate1"
times <- format(dates, "%H")
times <- sub("^0", " ", times)
ftimes <- factor(times, levels = sprintf("%2d", 0:23), order = TRUE)

# plot.factor
cat("plot using plot.factor\n")
plot(ftimes, xlab = "", ylab = "", col = color, border = color, space=0.7)
readline("Press Enter to continue")

# ggplot2
cat("plot using ggplot2\n")
c <- ggplot(data.frame(), aes(ftimes))
c <- c + geom_bar(fill = color, alpha = 0.7, width = 0.7) + xlab("") + ylab("")
print(c)
readline("Press Enter to continue")



#----------------------#
#  合計つぶやき数推移  #
#----------------------#
cat("合計つぶやき数推移\n")

color <- "green"
# 合計つぶやき数推移
cumsums <- cumsum(table(fdays))
# 縦軸の範囲
yrange <- c(0, cumsums[length(cumsums)])

# 最近30日間
cat("最近30日間\n")
y <- cumsums[-(1:(length(cumsums) - 30))]

# plot.default
cat("plot using plot.default\n")
plot(y , ylim = yrange, col = color, type = "l", xaxt = "n", xlab = "", ylab = "")
axis(1, label = names(y), at = 1:min(30, length(cumsums)))
readline("Press Enter to continue")

# ggplot2
cat("plot using ggplot2\n")
c <- ggplot(mapping = aes(as.Date(levels(fdays30), format = form), y))
c <- c + geom_line(color = color) + xlab("") + ylab("") + ylim(yrange)
c <- c + scale_x_date(format = form)
print(c)
readline("Press Enter to continue")



# 最近90日間
cat("最近90日間\n")
y <- cumsums[-(1:(length(cumsums) - 90))]

# plot.default
cat("plot using plot.default\n")
# plot.default の場合，日付の表記を指定する方法がわからなかった・・・
plot(y , ylim = yrange, col = color, type = "l", xaxt = "n", xlab = "", ylab = "")
axis(1, label = names(y), at = 1:min(90, length(cumsums)))
readline("Press Enter to continue")

# ggplot2
cat("plot using ggplot2\n")
c <- ggplot(mapping = aes(as.Date(levels(fdays90), format = form), y))
c <- c + geom_line(color = color) + xlab("") + ylab("") + ylim(yrange)
# 目盛りのフォーマットを指定
c <- c + scale_x_date(format = form)
print(c)
readline("Press Enter to continue")


# 全期間
cat("全期間\n")
y <- cumsums

# plot.default
cat("plot using plot.default\n")
plot(y, ylim = yrange, col = color, type = "l", xaxt = "n", xlab = "", ylab = "")
axis(1, label = names(y), at = 1:length(cumsums))
readline("Press Enter to continue")

# ggplot2
cat("plot using ggplot2\n")
c <- ggplot(mapping = aes(as.Date(levels(fdays), format = form), y))
c <- c + geom_line(color = color) + xlab("") + ylab("") + ylim(yrange)
c <- c + scale_x_date(format = form)
print(c)
