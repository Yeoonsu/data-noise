library(data.table)
library(ggplot2)

input_dir <- "C:/Users/yeon1/Leverage_Eventlog/Leverage_Ko/preprocessed"
output_dir <- "C:/Users/yeon1/Leverage_Eventlog/Leverage_Ko/pca"

file_list <- list.files(input_dir, pattern = "*.csv", full.names = TRUE)


for (file_path in file_list) {
  # 파일 이름 추출
  file_name <- basename(file_path)
  
  # 파일 읽기
  data <- fread(file_path)
  
  # 중복된 행 제거
  data <- unique(data)

  str(data)
  # 데이터 구조 확인

  # Case와 label 컬럼 추출
  case_labels <- data[, .(Case, label)]

  # PCA 적용할 데이터 추출 (V00~ 컬럼만 선택)
  data_to_reduce <- data[, grep("^V", colnames(data)), with = FALSE]

  # 상수 또는 0으로만 이루어진 열 제거
  data_to_reduce <- data_to_reduce[, lapply(.SD, function(x) if (var(x) != 0) x else NULL)]

  # PCA 적용 및 시간 측정
  set.seed(123)
  pca_time <- system.time({
    pca_result <- prcomp(data_to_reduce, center = TRUE, scale. = TRUE)
  })

  # 소요 시간 출력
  print(pca_time)

  # PCA 결과 저장
  pca_data <- as.data.table(pca_result$x[, 1:2]) # 첫 두 개의 주성분만 선택
  colnames(pca_data) <- c("Dim1", "Dim2")

  # Case와 label 정보를 포함한 PCA 결과 저장
  final_data <- cbind(case_labels, pca_data)

  # 결과 저장 경로 설정
  output_file_path <- file.path(output_dir, paste0("pca_", file_name))

  # 결과 저장
  write.csv(final_data, output_file_path, row.names = FALSE)}
