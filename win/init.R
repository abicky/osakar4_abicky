library(twitteR)
library(ggplot2)

# 非公開ユーザの場合はアンコメント
#useOAuth <- TRUE
if (!exists("useOAuth")) {
    useOAuth <- FALSE
}

# %aの表記を英語にする
Sys.setlocale("LC_TIME", "ENG")
# 公式RTを除くので実際は3200より少なくなる
screenName <- "Twitterのアカウント名"
if (useOAuth) {
    source("twitter.R")
    source("json.R")
    # authenticated user
    # アクセストークンなど
    auser <- c(
               token = "Twitterアカウントのアクセストークン",
               secret = "Twitterアカウントのアクセスシークレット"
               )
    # コンシューマキーなど
    key.consumer <- "アプリのコンシューマキー"
    secret.consumer <- "アプリのコンシューマシークレット"

    user <- buildUser(getUsers(screenName))
    if (!file.exists("tweets.RData")) {
        tweets <- sapply(getTweets(screenName, n = 3200), buildStatus)
        # UTF-8から俗に言うShift_JISというわけのわからない文字コードに変換
        for(i in 1:length(tweets)) {
            tweets[[i]]@text <- iconv(tweets[[i]]@text, "utf-8", "cp932")
        }        
        save(tweets, file = "tweets.RData")        
    } else {
        load("tweets.RData")
    }
} else {
    user <- getUser(screenName)
    if (!file.exists("tweets.RData")) {
        tweets <- userTimeline(screenName, n = 3200)
        # UTF-8から俗に言うShift_JISというわけのわからない文字コードに変換
        for(i in 1:length(tweets)) {
            tweets[[i]]@text <- iconv(tweets[[i]]@text, "utf-8", "cp932")
        }        
        save(tweets, file = "tweets.RData")
    } else {
        load("tweets.RData")
    }
}
