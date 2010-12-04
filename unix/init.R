library(twitteR)
library(ggplot2)

# 非公開ユーザの場合はアンコメント
#useOAuth <- TRUE
if (!exists("useOAuth")) {
    useOAuth <- FALSE
}

# %aの表記を英語にする
Sys.setlocale("LC_TIME", "en_US.utf-8")
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
        save(tweets, file = "tweets.RData")
    } else {
        load("tweets.RData")
    }
} else {
    user <- getUser(screenName)
    if (!file.exists("tweets.RData")) {
        tweets <- userTimeline(screenName, n = 3200)
        save(tweets, file = "tweets.RData")
    } else {
        load("tweets.RData")
    }
}
