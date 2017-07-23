##--  이진화 변환
ra_movies_b <- binarize(ra_movies, minRating=1)
	# 1이상은 1, 나머지는 0(2진화)
	# 결과는 FALSE(NA)/TRUE(숫자) 이진화 되어 저장됨
	
	
