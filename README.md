# movie-recommendation
Matrix Factorizationを利用したレコメンドシステム

アルゴリズムにALSを採用

### データのロード
movielensのrating.datをロード


	ratings <- fread("ml-1m/ratings.dat", sep=":")[, c(1,3,5,7), with=FALSE]
    setnames(ratings, c("user_id", "movie_id", "rating", "timestamp"))

### Matrix Factorizationの実行
rating.datを学習データとしてユーザ行列Xとアイテム行列Yに分解  
confはMatrix Factorizationの設定(基底の数、正則化の強さ、イテレーション回数)

	exec.MF(ratings, conf)

### 評価データの作成
movies.dat中の映画から評価データを作成し、myRatings.txtに保存する

	rateMovies(movies)

### 評価値の予測
数個の映画に対する評価をインプットに、評価がついていない映画に対する評価値を予測  
値の大きさが関心の強さを表す

	pred(my.ratings, Y)
