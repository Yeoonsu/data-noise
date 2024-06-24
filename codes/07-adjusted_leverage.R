# 필요한 패키지 로드
library(MASS)
library(matlib)
library(caret)
library(timeSeries)
library(fGarch)
library(fitdistrplus)
library(pracma)
library(BBmisc)

options(warn=-1)
options(repos = c(CRAN = "http://cran.rstudio.com"))

# 경로 설정
from <- "C:/Users/yeon1/Leverage_Eventlog/Leverage_Ko/pca"
to <- "C:/Users/yeon1/Leverage_Eventlog/Leverage_Ko/leverage2"

setwd(from)
fn <- list.files(getwd())

# 레버리지 계산 함수
fun_leverage <- function(x) {
  A <- ginv(t(x) %*% x)
  H_part1 <- x %*% A  
  h_diag <- colSums(t(H_part1) * t(x))
  h_diag
}

dat_with_leverage <- data.frame()
result <- matrix(NA, nrow = length(fn), ncol = 32)
dp <- 5

for (j in 1:length(fn)) {
  print(paste("Starting to calculate adjusted leverage for log :", fn[j]))
  preprocess_start <- Sys.time()
  
  dat <- data.frame(read.csv(fn[j], header = TRUE, stringsAsFactors = TRUE))
  
  # 레버리지 계산
  x2 <- dat[, -(1:2)]
  x <- as.matrix(x2)
  h_diag <- fun_leverage(x)
  
  length <- apply(x, 1, sum)
  z_norm <- (length - mean(length)) / sd(length)
  sigmoid_leng <- 1 / (1 + exp(-z_norm))
  
  if (-2.2822 + max(length) ^ 0.3422 < 0 | length(unique(length)) == 1) {
    h_diag2 <- h_diag
  } else {
    h_diag2 <- h_diag * (1 - sigmoid_leng) ^ (-2.2822 + max(length) ^ 0.3422)
  }
  
  print(paste("Evaluating leverage for log :", fn[j]))
  
  total_case <- unique(dat$Case)
  act <- rep(0, nrow(dat))
  act[which(is.element(total_case, unique(dat[which(dat$label == 1), 'Case'])))] <- 1
  
  act <- factor(act, levels = c(0, 1))
  
  pred1 <- rep(0, nrow(dat))
  pred1[order(h_diag, decreasing = TRUE)[1:sum(act == 1)]] <- 1
  pred1 <- factor(pred1)
  
  pred2 <- rep(0, nrow(dat))
  pred2[order(h_diag2, decreasing = TRUE)[1:sum(act == 1)]] <- 1
  pred2 <- factor(pred2)
  
  cat1_Fs <- numeric()
  cat1_index <- 0 
  cat2_Fs <- numeric()
  cat2_index <- 0 
  
  for (i in seq(0, 1, 0.0001)) {
    pred3 <- rep(0, nrow(dat))
    cat1_index <- cat1_index + 1
    pred3[which(h_diag >= i)] <- 1
    pred3 <- factor(pred3)
    
    if (length(levels(act)) < 2) {
      print("Error: 'act' must have at least 2 factor levels")
      break
    }
    
    cat1_cm <- confusionMatrix(pred3, act, positive = '1')
    cat1_cm1 <- as.vector(cat1_cm[4])[[1]]
    cat1_Fs[cat1_index] <- cat1_cm1[7]
  
    pred4 <- rep(0, nrow(dat))
    cat2_index <- cat2_index + 1
    pred4[which(h_diag2 >= i)] <- 1
    pred4 <- as.factor(pred4)
    cat2_cm <- confusionMatrix(pred4, act, positive = '1')
    cat2_cm1 <- as.vector(cat2_cm[4])[[1]]
    cat2_Fs[cat2_index] <- cat2_cm1[7]
  } 

  opt1 = seq(0,1,0.0001)[which(cat1_Fs == max(cat1_Fs, na.rm = TRUE))]
  pred3 <- rep(0, nrow(dat))
  pred3[h_diag >= min(opt1)] <- 1
  pred3 <- as.factor(pred3)
  
  opt2 = seq(0,1,0.0001)[which(cat2_Fs == max(cat2_Fs, na.rm = TRUE))]
  pred4 <- rep(0, nrow(dat))
  pred4[h_diag2 >= min(opt2)] <- 1
  pred4 <- as.factor(pred4)
  
  pred5 <- rep(0, nrow(dat))
  fit = try(tryCatch(fitdist(h_diag, distr = "gamma", method = 'mle'), error = function(e) NULL))
  if (is.null(fit)) {
    fit.gamma <- fitdist(h_diag, distr = "gamma", method = 'mme')
  } else {
    fit.gamma <- fitdist(h_diag, distr = "gamma", method = 'mle')
  }
  pred5[order(h_diag, decreasing = TRUE)[1:sum(pgamma(h_diag, shape = fit.gamma$estimate[1], rate = fit.gamma$estimate[2], lower.tail = FALSE) < 0.10)]] <- 1
  pred5 <- as.factor(pred5)
  
  pred6 <- rep(0, nrow(dat))
  fit = try(tryCatch(fitdist(h_diag2, distr = "gamma", method = 'mle'), error = function(e) NULL))
  if (is.null(fit)) {
    fit.gamma <- fitdist(h_diag2, distr = "gamma", method = 'mme')
  } else {
    fit.gamma <- fitdist(h_diag2, distr = "gamma", method = 'mle')
  }
  pred6[order(h_diag2, decreasing = TRUE)[1:sum(pgamma(h_diag2, shape = fit.gamma$estimate[1], rate = fit.gamma$estimate[2], lower.tail = FALSE) < 0.10)]] <- 1
  pred6 <- as.factor(pred6)  

  pred7 <- rep(0, nrow(dat))
  pred7[order(h_diag, decreasing = TRUE)[1:sum(h_diag > mean(h_diag) + sd(h_diag))]] <- 1
  pred7 <- as.factor(pred7)

  pred8 <- rep(0, nrow(dat))
  pred8[order(h_diag2, decreasing = TRUE)[1:sum(h_diag2 > mean(h_diag2) + sd(h_diag2))]] <- 1
  pred8 <- as.factor(pred8)
  
  x = h_diag
  x = sort(x)
  y = ecdf(x)
  y = y(x)

  tol = 1e-1
  t = try(tryCatch(smooth.spline(x, y, tol = tol), error = function(e) NULL))
  
  while (is.null(t)) {
    tol = tol / dp
    t = try(tryCatch(smooth.spline(x, y, tol = tol), error = function(e) NULL))
  }
  
  model <- smooth.spline(x, y, tol = tol)
  Y1 <- predict(model, x = seq(min(x), max(x), tol), deriv = 1)
  point = findpeaks(Y1$y)
  
  while (is.null(point)) {
    tol = tol / dp
    t = try(tryCatch(smooth.spline(x, y, tol = tol), error = function(e) NULL))
    while (is.null(t)) {
      tol = tol / dp
      t = try(tryCatch(smooth.spline(x, y, tol = tol), error = function(e) NULL))
    }
    model <- smooth.spline(x, y, tol = tol)
    Y1 <- predict(model, x = seq(min(x), max(x), tol), deriv = 1)
    point = findpeaks(Y1$y)
  }
  
  Y2 = Y1
  while (nrow(point) < 4) {
    tol_save = tol
    tol = tol / dp
    t = try(tryCatch(smooth.spline(x, y, tol = tol), error = function(e) NULL))
    if (is.null(t)) {
      tol = tol_save
      break
    }
    model <- smooth.spline(x, y, tol = tol)
    Y1 <- predict(model, x = seq(min(x), max(x), tol), deriv = 1)
    point = findpeaks(Y1$y)
    while (is.null(point)) {
      tol = tol / dp
      model <- smooth.spline(x, y, tol = tol)
      Y1 <- predict(model, x = seq(min(x), max(x), tol), deriv = 1)
      point = findpeaks(Y1$y)
    }
  }
  
  model <- smooth.spline(x, y, tol = tol)
  Y1 <- predict(model, x = seq(min(x), max(x), tol), deriv = 1)
  point = findpeaks(Y1$y)

  point2 = findpeaks(Y2$y)

  if (nrow(point) == 1) {
    spike_x = c(Y1$x[point[, 3]])
    c1 = point[, 3]
  } else {
    spike_x = c(Y1$x[point[which(point[, 1] > quantile(point[, 1], 0.95)), 3]])
    c1 = quantile(point[, 1], 0.95)
  }

  if (nrow(point2) == 1) {
    spike_x2 = c(Y2$x[point2[, 3]])
    c2 = point2[, 3]
  } else {
    spike_x2 = c(Y2$x[point2[which(point2[, 1] > quantile(point2[, 1], 0.95)), 3]])
    c2 = quantile(point2[, 1], 0.95)
  }

  if (c1 - c2 > 500) {
    spike_x = spike_x2
  }

  thres1 = min(spike_x)
  pred9 <- rep(0, nrow(dat))
  pred9[h_diag > thres1] <- 1
  pred9 <- as.factor(pred9)

  x = h_diag2
  x = sort(x)
  y = ecdf(x)
  y = y(x)

  tol = 1e-1
  t = try(tryCatch(smooth.spline(x, y, tol = tol), error = function(e) NULL))

  while (is.null(t)) {
    tol = tol / dp
    t = try(tryCatch(smooth.spline(x, y, tol = tol), error = function(e) NULL))
  }
  
  model <- smooth.spline(x, y, tol = tol)
  Y1 <- predict(model, x = seq(min(x), max(x), tol), deriv = 1)
  point = findpeaks(Y1$y)

  while (is.null(point)) {
    tol = tol / dp
    t = try(tryCatch(smooth.spline(x, y, tol = tol), error = function(e) NULL))
    while (is.null(t)) {
      tol = tol / dp
      t = try(tryCatch(smooth.spline(x, y, tol = tol), error = function(e) NULL))
    }
    model <- smooth.spline(x, y, tol = tol)
    Y1 <- predict(model, x = seq(min(x), max(x), tol), deriv = 1)
    point = findpeaks(Y1$y)
  }

  Y2 = Y1
  while (nrow(point) < 4) {
    tol_save = tol
    tol = tol / dp
    t = try(tryCatch(smooth.spline(x, y, tol = tol), error = function(e) NULL))
    if (is.null(t)) {
      tol = tol_save
      break
    }
    model <- smooth.spline(x, y, tol = tol)
    Y1 <- predict(model, x = seq(min(x), max(x), tol), deriv = 1)
    point = findpeaks(Y1$y)
    while (is.null(point)) {
      tol = tol / dp
      model <- smooth.spline(x, y, tol = tol)
      Y1 <- predict(model, x = seq(min(x), max(x), tol), deriv = 1)
      point = findpeaks(Y1$y)
    }
  }

  model <- smooth.spline(x, y, tol = tol)
  Y1 <- predict(model, x = seq(min(x), max(x), tol), deriv = 1)
  point = findpeaks(Y1$y)

  point2 = findpeaks(Y2$y)

  if (nrow(point) == 1) {
    spike_x = c(Y1$x[point[, 3]])
    c1 = point[, 3]
  } else {
    spike_x = c(Y1$x[point[which(point[, 1] > quantile(point[, 1], 0.95)), 3]])
    c1 = quantile(point[, 1], 0.95)
  }

  if (nrow(point2) == 1) {
    spike_x2 = c(Y2$x[point2[, 3]])
    c2 = point2[, 3]
  } else {
    spike_x2 = c(Y2$x[point2[which(point2[, 1] > quantile(point2[, 1], 0.95)), 3]])
    c2 = quantile(point2[, 1], 0.95)
  }

  if (c1 - c2 > 500) {
    spike_x = spike_x2
  }
  thres2 = min(spike_x)

  pred10 <- rep(0, nrow(dat))
  pred10[h_diag2 > thres2] <- 1
  pred10 <- as.factor(pred10)

  cm1 <- confusionMatrix(pred1, act, positive = '1')
  cm1 <- as.vector(cm1[4])[[1]]

  cm2 <- confusionMatrix(pred2, act, positive = '1')
  cm2 <- as.vector(cm2[4])[[1]]

  cm3 <- confusionMatrix(pred3, act, positive = '1')
  cm3 <- as.vector(cm3[4])[[1]]

  cm4 <- confusionMatrix(pred4, act, positive = '1')
  cm4 <- as.vector(cm4[4])[[1]]

  cm5 <- confusionMatrix(pred5, act, positive = '1')
  cm5 <- as.vector(cm5[4])[[1]]

  cm6 <- confusionMatrix(pred6, act, positive = '1')
  cm6 <- as.vector(cm6[4])[[1]]

  cm7 <- confusionMatrix(pred7, act, positive = '1')
  cm7 <- as.vector(cm7[4])[[1]]

  cm8 <- confusionMatrix(pred8, act, positive = '1')
  cm8 <- as.vector(cm8[4])[[1]]

  cm9 <- confusionMatrix(pred9, act, positive = '1')
  cm9 <- as.vector(cm9[4])[[1]]

  cm10 <- confusionMatrix(pred10, act, positive = '1')
  cm10 <- as.vector(cm10[4])[[1]]

  precision1 <- cm1[5]
  Recall1 <- cm1[6]
  Fs1 <- cm1[7]

  precision2 <- cm2[5]
  Recall2 <- cm2[6]
  Fs2 <- cm2[7]

  precision3 <- cm3[5]
  Recall3 <- cm3[6]
  Fs3 <- cm3[7]

  precision4 <- cm4[5]
  Recall4 <- cm4[6]
  Fs4 <- cm4[7]

  precision5 <- cm5[5]
  Recall5 <- cm5[6]
  Fs5 <- cm5[7]

  precision6 <- cm6[5]
  Recall6 <- cm6[6]
  Fs6 <- cm6[7]

  precision7 <- cm7[5]
  Recall7 <- cm7[6]
  Fs7 <- cm7[7]

  precision8 <- cm8[5]
  Recall8 <- cm8[6]
  Fs8 <- cm8[7]

  precision9 <- cm9[5]
  Recall9 <- cm9[6]
  Fs9 <- cm9[7]

  precision10 <- cm10[5]
  Recall10 <- cm10[6]
  Fs10 <- cm10[7]


  result[j,]= c(strsplit(fn[j], split='-')[[1]][1] ,
                strsplit((strsplit(fn[j], split='-')[[1]][3]), split=".", fixed = TRUE)[[1]][1],
                precision1, Recall1, Fs1, precision2, Recall2, Fs2,
                precision3, Recall3, Fs3, precision4, Recall4, Fs4,
                precision5, Recall5, Fs5, precision6, Recall6, Fs6,
                precision7, Recall7, Fs7, precision8, Recall8, Fs8,
                precision9, Recall9, Fs9, precision10, Recall10, Fs10)

  cat("Evaluation finished")
  cat_dat_with_leverage = data.frame(cbind(dataset = rep(fn[j],nrow(dat)), caseid=unique(dat[,1]),
                                       length= length,
                                       label = as.character(act),
                                       leverage1 =h_diag , leverage2=h_diag2))
  dat_with_leverage = rbind(dat_with_leverage, cat_dat_with_leverage)

  # 결과 저장
  output_file_path <- paste("C:/Users/yeon1/Leverage_Eventlog/Leverage_Ko/result_table/result_", fn[j], sep = "")
  output_file <- file(output_file_path, open = "wt")
  write.csv(dat_with_leverage, output_file, row.names = FALSE)
  close(output_file)
  
  preprocess_end <- Sys.time()
  print(preprocess_end - preprocess_start)
  
  # dat_with_leverage 초기화
  dat_with_leverage <- data.frame()

}