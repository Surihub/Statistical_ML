# 데이터셋 로드
data(mtcars)

# trainset과 testset 분리
set.seed(123)
sample_size <- floor(0.7 * nrow(mtcars))
trainset <- mtcars[sample(1:nrow(mtcars), sample_size), ]
testset <- mtcars[-sample(1:nrow(mtcars), sample_size), ]

# 선형회귀 모델
lm_model <- lm(mpg ~ wt + disp + hp, data=trainset)
lm_pred <- predict(lm_model, newdata=testset)
mse_lm <- mean((lm_pred - testset$mpg)^2)

# Nearest Neighbor 모델 (연속형 변수 예측)
library(class)
k_values <- c(1, 5, 15)
mse_knn <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  knn_model <- knn(train = trainset[, c("wt", "disp", "hp")], 
                   test = testset[, c("wt", "disp", "hp")], 
                   cl = trainset$mpg, 
                   k = k_values[i])
  mse_knn[i] <- mean((as.numeric(knn_model)-testset$mpg)^2)
}

# 결과 출력
cat("선형회귀 모델의 MSE:", mse_lm, "\n")
for (i in seq_along(k_values)) {
  cat(paste0("Nearest Neighbor 모델 (k=", k_values[i], ")의 MSE:", mse_knn[i], "\n"))
}

