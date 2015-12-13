require(dplyr)

rateMovies <- function(movies, n=10) {
  movies.sub <- dplyr::sample_n(movies, n)
  lines <- c()
  now <- as.numeric(Sys.Date())
  
  print("Please rate the following movie (1-5 (best), or 0 if not seen):")
  for(i in 1:n) {
    rate <- readline(paste(movies.sub$title[i], ": ", sep=""))
    if(rate>0 && rate <= 5) lines <- c(lines, paste(0, movies.sub$movie_id[i], rate, now, sep="::"))
  }
  if(length(lines)==0) {
    print("No rating provided. A rating file was not prepared.")
  } else {
    write.table(lines, "myRatings.txt", quote=F, row.names=F, col.names=F, append=F)
  }
}