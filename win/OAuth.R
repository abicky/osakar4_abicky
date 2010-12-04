# {{{
OAuth.R.License <- 
'Copyright (c) 2010 OZAKI Toru (twittoru)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.'
# }}}
# ŠeŽ©‚Åtoken,secret‚Ç‚à‚Í—pˆÓ‚µ‚Ädefvars/templates‹ß‚­‚É‚ ‚éuser‚Æapp•Ï”‚ð•ÏX‚µ‚Ä‚­‚¾‚³‚¢
#{{{library
library(digest)
library(scrapeR)
#}}}
#{{{defvars
uri.post <- "http://api.twitter.com/1/statuses/update.xml"

#templates
# user
user <- list(
    token  = 'access_token',
    secret = 'access_token_secret'
)

# app
key.consumer    <- 'consumer_token'
secret.consumer <- 'consumer_secret'

#}}}
#{{{miscellaneous

generateRandomString <- function() {
    # Return:
    #  The 64-length random alphabet as string.
    #{{{ other version:
    #      generate 64 random ``alphanumeric'' characters
    #generateRandomString <- function() {
    #    strls <- "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    #    index <- runif(64, 1,63)
    #    paste(strsplit(strls, "")[[1]][index], sep="", collapse="")
    #}
    #}}}
    paste(letters[runif(20, 1, 27)], sep="", collapse="")
}

generateOauthHeader <- function(params) {
    # Arg:
    #  params: A data.frame which has "oauth_xxx" as $keys and $values.
    #
    # Return:
    #  The Authorization header i.e. "OAuth oauth_nonce="sldfjiwouer", ..."    
    params     <- params[params$key != "status", ]
    len  <- length(params[, 1])
    JoinParams <- function(x) {
        paste(as.character(params[x, ]$key),
              "=", '"',
              uriEncode(as.character(params[x, ]$value)),
              '"', sep="")
    }
    paste("OAuth", paste(sapply(1:len, JoinParams), collapse=", "))
}

makeParams <- function() {
    # Return:
    #   The data.frame which has only required parameters.
    nonce     <- generateRandomString()
    timestamp <- as.integer(Sys.time())
    keys      <- c("oauth_consumer_key",
                   "oauth_signature_method",
                   "oauth_timestamp",
                   "oauth_nonce",
                   "oauth_version")
    values    <- c(key.consumer, "HMAC-SHA1", timestamp, nonce, "1.0")
    data.frame(key=keys, value=values)
}

makeParamsWithTweet <- function(tweet, user) {
    # Args:
    #  tweet : `What's happening?'
    #  user  : A list with $key & $value as access_key_secret & access_token.
    #
    # Return:
    #  The param data.frame within oauth_signature and status.
    status    <- uriEncode(tweet[1])
    token     <- user$token
    secret    <- user$secret
    params    <- rbind(makeParams(),
                       data.frame(key=c("oauth_token", "status"),
                                  value=c(token, status)))
    signature <- signForOauth(uri.post, "post", params, secret)
    rbind(params, data.frame(key="oauth_signature", value=signature))
}


makeParamsForReading <- function(uri, user) {
    # Args:
    #  uri  : API url i.e. http://api.twitter.com/1/statuses/user_timeline.json
    #  user : A list with $key & $value as access_key_secret & access_token.
    #
    # Return:
    #  The param data.frame within oauth_signature.
    token     <- user$token
    secret    <- user$secret
    params    <- rbind(makeParams(), data.frame(key="oauth_token", value=token))
    signature <- signForOauth(uri, "get", params, secret)
    rbind(params, data.frame(key="oauth_signature", value=signature))
}

signForOauth <- function(uri, request_method, params, secret) {
    # Args:
    #  uri            : API url i.e. http://api.twitter.com/1/statuses/user_timeline.format
    #  request-method : GET or POST
    #  params         : A data.frame which has "oauth_xxx" as $keys and $values.
    #  secret         : access_key_secret, or "" when called for requesttoken.
    #
    # Return:
    #  The "oauth_signature" as string.
    key <- makeHmacKey(secret.consumer, secret)
    msg <- generateSignatureBaseString(uri, request_method, params)
    base64(makeHmacSha1(key, msg))[1]
}

generateSignatureBaseString <- function(uri, method, params) {
    # Args:
    #  uri    : request URI
    #  method : GET or POST
    #  params : A data.frame which has "oauth_xxx" as $keys and $values.
    #
    # Return:
    #  Signature base string. See OAuth Core 1.0 Appendix A.5.1.
    len        <- length(params[,1])
    JoinParams <- function(x) {
        paste(params[x, ]$key, params[x, ]$value, sep="=")
    }
    params.string  <- uriEncode(paste(sort(sapply(1:len, JoinParams)), collapse="&"))
    uri            <- uriEncode(uri)
    request_method <- toupper(method)
    paste(c(request_method, uri, params.string), collapse="&")
}

uriEncode <- function (noncoded) {
    # Arg:
    #  noncoded: non-URI-encoded string
    #
    # Return:
    #  URI-encoded and touppered string.
    #  noncoding symbol characters is only "_-.~", but utils::URLencode is not.
    OK <- "[^-ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.~]"
    x <- strsplit(noncoded, "")[[1L]]
    z <- grep(OK, x)
    if (length(z)) {
        y <- sapply(x[z], function(x) paste("%", toupper(as.character(charToRaw(x))), 
        sep = "", collapse = ""))
        x[z] <- y
    }
    paste(x, collapse = "")
}

makeHmacKey <- function(secret.consumer, secret.access=""){
    # Args:
    #  secret.consumer : consumer_key_secret
    #  secret.access   : access_key_secret or "" when getting request token.
    #
    # Return:
    #  "secret.consumer&secret.access" URL-encoded
    paste(uriEncode(secret.consumer), uriEncode(secret.access), sep="&")
}

makeHmacSha1 <- function(key ,msg) {
    # Args:
    #  key : HMAC key
    #  msg : HMAC text
    #
    # Return:
    #  HMAC (non-hex)digest.
    hashlength <- 20
    innerpad   <- rawToBits(as.raw(rep(0x36 ,64)))
    outerpad   <- rawToBits(as.raw(rep(0x5C ,64)))
    zero       <- rep(0 ,64)
    HexdigestToDigest <- function(digest) {
        as.raw(strtoi(substring(digest, (1:hashlength)*2-1, (1:hashlength)*2), 16))
    }
    if(length(strsplit(key, "")[[1]]) >= 64) {
        key.digested <- digest(key,serialize=FALSE,algo="sha1")
        key <- intToUtf8(strtoi(HexdigestToDigest(key.digested), 16))
    }
    key <- rawToBits(as.raw(append(utf8ToInt(key),zero)[1:64]))
    mac <- function(pad, text) {
        HexdigestToDigest(digest(append(packBits(xor(key,pad)), text),
                                 serialize=FALSE,algo="sha1"))
    }
    mac(outerpad, mac(innerpad, charToRaw(msg)))
}
#}}}
#{{{examples
twitterUpdateStatus<- function(tweet, user, verbose=FALSE) {
    params <- makeParamsWithTweet(tweet, user)
    curlPerform(url        = uri.post,
                verbose    = verbose,
                httpheader = c(Expect        = "",
                               Authorization = generateOauthHeader(params)),
                postfields = paste("status=", uriEncode(tweet), sep=""))
}
#   
#   twitterCheckLimitrate <- function(user) {
#       uri.limit <- "http://api.twitter.com/1/account/rate_limit_status.xml"
#       params  <- makeParamsForReading(uri.limit, user, verbose=FALSE)
#       curlPerform(url        = uri.limit,
#                   verbose    = verbose,
#                   httpheader = c(Expect        = "",
#                                  Authorization = generateOauthHeader(params)))
#   }
#   twitterGetRequesttoken <- function() {
#       res       <- basicTextGatherer()
#       uri       <- "http://twitter.com/oauth/request_token"
#       params    <- makeParams()
#       signature <- signForOauth(uri, "get", params, "")
#       params    <- rbind(params, data.frame(key="oauth_signature", value=signature))
#       curlPerform(url           = uri,
#                   verbose       = TRUE,
#                   writefunction = res$update,
#                   httpheader    = c(Expect        = "",
#                                     Authorization = generateOauthHeader(params)))
#       res$value()
#   }
#   twitterGetAccesstoken <- function(token, secret, pin) {
#       res    <- basicTextGatherer()
#       uri    <- "http://twitter.com/oauth/access_token"
#       params <- rbind(makeParams(), data.frame(key=c("oauth_token", "oauth_verifier"), value=c(token, pin)))
#       key    <- makeHmacKey(secret.consumer, secret)
#       msg    <- generateSignatureBaseString(uri, "get", params)
#       sign   <- base64(makeHmacSha1(key, msg))[1]
#       rbind(params, data.frame(key="oauth_signature", value=sign))
#       curlPerform(url           = uri,
#                   verbose       = TRUE,
#                   writefunction = res$update,
#                   httpheader    = c(Expect        = "",
#                                     Authorization = generateOauthHeader(params)))
#       res$value()
#   }
#}}}
