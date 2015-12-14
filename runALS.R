source("ALS.R")
source("rateMovies.R")
require(data.table)
require(dplyr)
require(caret)
require(stringr)

#**************************************************
# Matrix Factorizaionの実行
#  args
#  - ratings : user_id, movie_id, ratingを保持するdata.table
#  - conf  : Matrix Factorizationの設定
#  - cv  : cross validation
#  returns
#  - X, Y  : 基底の数
#  - rmse  : 正則化項の係数lambda(cv>0の時のみ)
#**************************************************
exec.MF <- function(ratings, conf, cv=0) {
  if (cv > 0) {
    # cross validation
    folds <- createFolds(1:smry$n, cv)
    is_train <- lapply(folds, function(ind, n) !(1:n %in% ind), n = smry$n)
    
    rmse <- rep(0, cv)
    for(t in 1:cv) {
      train <- ratings[is_train[[t]], ]
      test <- ratings[!is_train[[t]], ]
      
      users <- unique(train$user_id)
      items <- unique(train$movie_id)
      # test data から training dataに含まれないidを除外
      test <- test %>% 
        filter(user_id %in% users) %>% 
        filter(movie_id %in% items)
      
      # training
      start.gl <- proc.time()
      res <- fit(train, users, items, conf$K, conf$lmd, conf$epochs)
      rmse[t] <- compute.rmse(test, res$X, res$Y)
      end.gl <- proc.time()
      message(sprintf("cv: %d / rmse: %f / time: %.2f", 
              t, (end.gl-start.gl)["elapsed"], rmse[t]))
    }
    return(list(X=res$X, Y=res$Y, rmse=mean(rmse)))
  } else {
    users <- unique(ratings$user_id)
    items <- unique(ratings$movie_id)
    start.gl <- proc.time()
    res <- fit(ratings, users, items, conf$K, conf$lmd, conf$epochs)
    end.gl <- proc.time()
    message(sprintf("done. / time: %.2f", (end.gl-start.gl)["elapsed"]))
    return(list(X=res$X, Y=res$Y))
  }
}

setwd("/home/rstudio/movie-recommendation/")
set.seed(123)

# movielens のロード
ratings <- fread("ml-1m/ratings.dat", sep=":")[, c(1,3,5,7), with=FALSE]
setnames(ratings, c("user_id", "movie_id", "rating", "timestamp"))
movies.dat <- readLines("ml-1m/movies.dat")
movies <- data.frame("movie_id"=character(length(movies.dat)), 
                     "title"=character(length(movies.dat)), 
                     "Genre"=character(length(movies.dat)), stringsAsFactors=FALSE)
for (i in 1:length(movies.dat)) {
  movies[i,] <- unlist(str_split(movies.dat[i], "::"))
}
movies$movie_id <- as.integer(movies$movie_id)

# 評価データの作成
rateMovies(movies)
my.ratings <- fread("myRatings.txt", sep=":")[, c(1,3,5,7), with=FALSE]
setnames(my.ratings, c("user_id", "movie_id", "rating", "timestamp"))

# データ数の確認
smry <- ratings %>% 
  dplyr::summarise(n=n(), 
            n_user=n_distinct(user_id), 
            n_movie=n_distinct(movie_id))
message(sprintf("n_ratings: %d / n_user: %d / n_movie: %d ", 
        smry$n, smry$n_user, smry$n_movie))

# Matrix Factorizaionの設定
#  - K    : 基底の数
#  - lmd  : 正則化項の係数lambda
#  - epochs : 学習回数
conf <- data.frame(K=30, lmd=0.1, epochs=15)
res <- exec.MF(ratings, conf)
#load("XY.data")   # 学習済みモデルのロード

# 評価値の予測
rank <- pred(my.ratings, res$Y)
print(data.table(movies) %>% 
        dplyr::filter(movie_id %in% 
                        res$Y$movie_id[order(rank, decreasing=T)[1:10]]))

