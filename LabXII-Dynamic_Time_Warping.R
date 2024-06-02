library(dtw)

time <- 1:10
series1 <- c(1, 7, 4, 8, 2, 9, 6, 5, 2, 0)
series2 <- c(1, 2, 8, 5, 5, 1, 9, 4, 6, 5)

dtw_result <- dtw(series1, series2, keep.internals = TRUE)

dtw_distance <- dtw_result$distance
cat("DTW Distance:", dtw_distance, "\n")

warp_path <- dtw_result$index1
warp_path <- cbind(warp_path, dtw_result$index2)
print("Warp Path:")
print(warp_path)

plot(dtw_result, type = "twoway", main = "DTW Alignment")
