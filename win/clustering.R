library(RMeCab)
useOAuth <- TRUE
source("init.R")

# 取得するツイート数／人
n <- 200

if (!file.exists("tweetVec.RData")) {
    # フォローさんだけを対象にする twitter.R の関数
    ids <- getFriendsIDs(screenName)
    
    tweetVec <- sapply(ids, function(id) {
        print(id)
        # twitteR の status クラスを作成 twitter.R の関数
        tweets <- sapply(getTweets(id, n = n), buildStatus)
        # 全てのツイートを1つの文字列に結合
        paste(sapply(tweets, function(x) iconv(x@text, "utf-8", "cp932")), collapse = " ")
    })
    names(tweetVec) <- ids
    save(tweetVec, file = "tweetVec.RData")
} else {
    load("tweetVec.RData")
}

# 自分のツイートを最初に追加
tweetVec <- c(paste(sapply(tweets[1:n], function(x) x@text), collapse = " "), tweetVec)
names(tweetVec)[1] <- screenName

# URLを削除
tweetVec <- gsub("http:\\/\\/[\\w.#&%@\\/\\-\\?=]*", " ", tweetVec, perl = TRUE)

# ハッシュタグ（# の手前に英数字またはアンダースコアがないもの）を削除
# 別に先読み・後読みを使わなくてもいい
tweetVec <- gsub("(?<!\\w)#\\w+", " ", tweetVec, perl = TRUE)
# ※Osaka.Rの後に知ったけど#の後に数字しか続かない場合はハッシュタグとみなされないらしい・・・

# ユーザ名（@ の手前に英数字またはアンダースコアがなくて後ろにアットマークのないもの）を削除
# 別に先読み・後読みを使わなくてもいい
tweetVec <- gsub("(?<!\\w)@\\w+(?!@)", " ", tweetVec, perl = TRUE)

# 本来ならUnicode正規化とかもしたい

# 抽出するのは自立語
pos <- c("感動詞","形容詞","接続詞","動詞","副詞","名詞","連体詞")

#tweetMat <- docMatrixDF(tweetVec, pos, minFreq = 2)
tweetMat <- docMatrixDF(tweetVec, pos)

# 記号のみの形態素を除くための正規表現（何故か数字のみも抽出される）
#regexp <- "^[_`~!@#$%^&*()+-={}|\\;:'\"<>?,./\\[\\]]+$"
# ストップワードも定義したかった・・・
#stopWords <- c("する", "なる", "ある", "思う", "できる", "使う")
#tweetMat <- tweetMat[!grepl(regexp, rownames(tweetMat), perl = TRUE), ]

# IDF値
idf <- globalIDF(tweetMat)
tweetMat <- tweetMat * idf
# 正規化
tweetMat <- t(t(tweetMat) * mynorm(tweetMat))

# LSI．フォロー数が少なすぎて意味がなかった
# svdMats <- svd(tweetMat)
# 特異値が1未満のランクを削除
# index <- svdMats$d >= 1
# D <- docsvd$d[index]
# U <- docsvd$u[,index]
# V <- docsvd$v[,index]
# lsi <- t(t(U) * D) %*% t(V)


#-----------------------------------#
# 階層的クラスタリング (完全連結法) #
#-----------------------------------#

# 各ツイート（ユーザの）の距離行列を作成
d <- dist(t(tweetMat))
# 完全連結法
hc <- hclust(d)
# デンドログラムを表示
plot(hc)
# クラスタ数が5になるところでカット
hclabel <- cutree(hc, k = 5)
# クラスタの分布
print(table(hclabel))


#------------------------#
# single-path clustering #
#------------------------#

# アカウント名やIDを格納
users <- names(tweetVec)
# 類似度計算用の関数 (コサイン類似度)
sim <- function(x, y) {
    sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
}
# 閾値
th <- 0.1
# 結果を格納するための変数
clusters <- list(centroid = list(), user = list())
set.seed(1000)
# ツイート（アカウント）をランダムに抽出
index <- sample(1:ncol(tweetMat))
clusters$user[[1]] <- users[index[1]]
clusters$centroid[[1]] <- tweetMat[,index[1]]
for (i in index[-1]) {
    x <- tweetMat[,i]
    # 零ベクトルは飛ばす
    if (all(x == 0)) next
    # 既存のクラスタ（のセントロイド）との類似度
    sims <- sapply(clusters$centroid, sim, x)
    if (max(sims) < th) {
        target <- length(clusters$user) + 1
        clusters$user[[target]] <- users[i]
        clusters$centroid[[target]] <- x
    } else {
        target <- which.max(sims)
        clusters$user[[target]] <- c(clusters$user[[target]], users[i])
        # セントロイドを更新
        clusters$centroid[[target]] <- clusters$centroid[[target]] + (x - clusters$centroid[[target]]) / length(clusters$user[[target]])
    }
}

# クラスタの分布
print(sapply(clusters$user, length))


#---------#
# k-means #
#---------#

# クラスタ数5でクラスタリング
km <- kmeans(t(tweetMat), 5)
# クラスタの分布
print(km$size)



#-------------------------------------------#
# おまけ ～自分と類似度の高いユーザを探る～ #
#-------------------------------------------#

me <- tweetMat[,1]
others <- as.data.frame(tweetMat[,-1])
# さぁ一体誰でしょう？ (IDを出力)
print(users[which.max(sapply(others, sim, me)) + 1])


# IDじゃ誰かわからないのでアイコンをプロット
id <- users[which.max(sapply(others, sim, me)) + 1]
# twitter.R の関数
who <- getUsers(as.integer(id))
imgUri <- who$profile_image_url
if (grepl("png", substr(imgUri, nchar(imgUri) -2, nchar(imgUri)), ignore.case = T)) {
    library(png); library(pixmap)
    png <- getURLContent(imgUri)
    img <- readPNG(png)
     plot(pixmapRGB(img))
} else {
    library(ReadImages)
    jpg <- file("tmp.jpg", "wb")
    writeBin(as.vector(getURLContent(imgUri)), jpg)
    close(jpg)
    # 自分の環境では Internal error になりましたが，おまけの部分なので許してください・・・
    jpg <- read.jpeg("tmp.jpg")
    plot(jpg)
}
