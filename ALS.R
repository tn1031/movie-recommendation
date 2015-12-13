require(data.table)
require(dplyr)
require(foreach)
require(iterators)
require(doParallel)
require(MASS)

#**************************************************
# 行列X, Yの更新
#  args
#  - ratings : user_id, movie_id, ratingを保持する(user_id/movie_idでフィルタ済み)
#  - K  : 基底の数
#  - lmd : 正則化項の係数
#  returns
#  - X/Y : 更新したX/Y(matrix)
#**************************************************
update.Mat <- function(ratings, K, lmd) {
  # 正則化付き最小二乗法
  m <- as.matrix(ratings[, 5:(K+4)])
  term1 <- t(m) %*% m + lmd*diag(K)
  term2 <- Map(function(i) ratings$rating[i]*m[i,], c(1:nrow(m)))
  term2 <- apply(matrix(unlist(term2), ncol=K, byrow=TRUE), 2, sum)
  
  return(t(solve(term1) %*% term2))
}

#**************************************************
# Matrix FactorizationのALSによる学習
#  args
#  - ratings : user_id, movie_id, ratingを保持する
#  - users : ユニークなuser_idの列
#  - items : ユニークなmovie_idの列
#  - K  : 基底の数
#  - lmd : 正則化項の係数
#  - epochs : イテレーション回数
#  returns
#  - X, Y : ratingsを分解した行列(data.table)
#**************************************************
fit <- function(ratings, users, items, K=10, lmd=1.0, epochs=10) {
  # ユーザ行列X, アイテム行列Yの初期化
  X <- matrix(rexp(length(users)*K, 1), c(length(users), K))
  X <- data.table(cbind(users, X))
  setnames(X, c("user_id", paste("xx", c(1:K), sep="")))
  Y <- matrix(rexp(length(items)*K, 1), c(length(items), K))
  Y <- data.table(cbind(items, Y))
  setnames(Y, c("movie_id", paste("yy", c(1:K), sep="")))
  
  # クラスタの準備
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  on.exit(stopCluster(cl))
  for(epoch in 1:epochs) {
    start.local <- proc.time()
    
    # ユーザ行列Xの更新
    # user_id 毎に正則化付き最小二乗法を解く
    joined <- inner_join(ratings, Y, by="movie_id")
    X <- foreach(u = icount(length(users)), 
                 .combine=function(x,y)rbindlist(list(x,y)), 
                 .export="update.Mat",
                 .packages = c("data.table", "dplyr")) %dopar% ({
                   data.table(users[u], 
                              update.Mat(data.frame(joined) %>% 
                                           filter(user_id==users[u]), K, lmd))
                 })
    setnames(X, c("user_id", paste("xx", c(1:K), sep="")))
    
    # アイテム行列Yの更新
    # movie_id 毎に正則化付き最小二乗法を解く
    joined <- inner_join(ratings, X, by="user_id")
    Y <- foreach(i = icount(length(items)), 
                 .combine=function(x,y)rbindlist(list(x,y)), 
                 .export="update.Mat",
                 .packages = c("data.table", "dplyr")) %dopar% ({
                   data.table(items[i], 
                              update.Mat(data.frame(joined) %>% 
                                           filter(movie_id==items[i]), K, lmd))
                 })
    setnames(Y, c("movie_id", paste("yy", c(1:K), sep="")))
    
    end.local <- proc.time()
    message(sprintf("epoch: %d / time: %.2f", 
            epoch, (end.local-start.local)["elapsed"]))
    
  }
  save(X, Y, file="XY.data")
  return(list(X=X, Y=Y))
}

#**************************************************
# RMSEの計算
#  args
#  - ratings : user_id, movie_id, ratingを保持する
#  - X, Y : ratingsを分解した行列(data.table)
#  returns
#  - rmse : ratingと X %*% t(Y) の誤差
#**************************************************
compute.rmse <- function(ratings, X, Y) {
  # ratingsのuser_id, movie_idをkeyにX,Yの各要素をjoin
  joined <- data.frame(
    inner_join(
      inner_join(ratings, Y, by="movie_id"), 
      X, by="user_id"))
  
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  on.exit(stopCluster(cl))
  # rmse = sqrt(mean(pow(r-x*y, 2)))
  rmse <- sqrt(
    foreach(i = icount(nrow(joined)), 
            .combine=sum, 
            .packages = c("data.table", "dplyr")) %dopar% ({
              (joined[i,]$rating - 
                 as.numeric(joined[i,] %>% dplyr::select(contains("xx"))) 
               %*% as.numeric(joined[i,] %>% dplyr::select(contains("yy")))
              ) ** 2
            }) / nrow(joined))
  return(rmse)
}

#**************************************************
# movie評価値の予測(recommend)
#  args
#  - my.ratings : user_id, movie_id, ratingを保持する
#  - Y : ratingsを分解した行列(item側のみ)
#  returns
#  - r : movie評価値の予測
#**************************************************
pred <- function(my.ratings, Y) {
  # ratingの行ベクトル
  r <- matrix(0, 1, nrow(Y))
  for(i in 1:nrow(my.ratings)) {
    r[1, which(Y$movie_id==my.ratings[i,]$movie_id)] <- my.ratings[i,]$rating
  }
  # アイテム行列
  y <- as.matrix(Y %>% dplyr::select(-movie_id))

  # 予測(ベクトル) = r * y * (t(y) * y)^(-1) t(y)
  ry <- r %*% y
  yty.inv <- ginv(t(y) %*% y)
  return(ry %*% yty.inv %*% t(y))
}

# 稼働確認用
test.ALS <- function(train, test, users, items, K=10, lmd=1.0, epochs=10) {
  # ユーザ行列X, アイテム行列Yの初期化
  X <- matrix(rexp(length(users)*K, 1), c(length(users), K))
  X <- data.table(cbind(users, X))
  setnames(X, c("user_id", paste("xx", c(1:K), sep="")))
  Y <- matrix(rexp(length(items)*K, 1), c(length(items), K))
  Y <- data.table(cbind(items, Y))
  setnames(Y, c("movie_id", paste("yy", c(1:K), sep="")))
  rmse.seq <- c()
  
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  on.exit(stopCluster(cl))
  for(epoch in 1:epochs) {
    start.local <- proc.time()
    joined <- inner_join(train, Y, by="movie_id")
    X <- foreach(u = icount(length(users)), 
                 .combine=function(x,y)rbindlist(list(x,y)), 
                 .export="update.Mat",
                 .packages = c("data.table", "dplyr")) %dopar% ({
                   data.table(users[u], 
                              update.Mat(data.frame(joined) %>% 
                                           filter(user_id==users[u]), K, lmd))
                 })
    setnames(X, c("user_id", paste("xx", c(1:K), sep="")))
    
    joined <- inner_join(train, X, by="user_id")
    Y <- foreach(i = icount(length(items)), 
                 .combine=function(x,y)rbindlist(list(x,y)), 
                 .export="update.Mat",
                 .packages = c("data.table", "dplyr")) %dopar% ({
                   data.table(items[i], 
                              update.Mat(data.frame(joined) %>% 
                                           filter(movie_id==items[i]), K, lmd))
                 })
    setnames(Y, c("movie_id", paste("yy", c(1:K), sep="")))
    end.local <- proc.time()
    message(sprintf("epoch: %d / time: %.2f", 
            epoch, (end.local-start.local)["elapsed"]))
    
    joined <- data.frame(
      inner_join(
        inner_join(test, Y, by="movie_id"), 
        X, by="user_id"))
    rmse <- sqrt(
      foreach(i = icount(nrow(joined)), 
              .combine=sum, 
              .packages = c("data.table", "dplyr")) %dopar% ({
                (joined[i,]$rating - 
                   as.numeric(joined[i,] %>% dplyr::select(contains("xx"))) 
                 %*% as.numeric(joined[i,] %>% dplyr::select(contains("yy")))
                ) ** 2
              }) / nrow(joined))
    rmse.seq <- c(rmse.seq, rmse)
    print(rmse)
  }
  save(X, Y, file="XY.data")
  return(list(X=X, Y=Y, rmse=rmse.seq))
}
