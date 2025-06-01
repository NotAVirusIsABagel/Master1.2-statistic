#用來做ppt的無備註

B <- 1000
n <- length(fish)
boot_means_fish <- numeric(B)
boot_means_copepod <- numeric(B)

for (i in 1:B) {
  sample_indices <- floor(runif(n, min = 1, max = n + 1))
  boot_means_fish[i] <- mean(fish[sample_indices])
  boot_means_copepod[i] <- mean(copepod[sample_indices])
}

se_boot_fish <- sd(boot_means_fish)
se_boot_copepod <- sd(boot_means_copepod)

print("Bootstrap estimates:")
print(paste("Fish: SE =", se_boot_fish))
print(paste("Copepod: SE =", se_boot_copepod))

#選項2：quick way that is not allowed QQ
B <- 1000  # bootstrap 次數
n <- length(fish)  # fish 資料集的長度
boot_means_fish <- numeric(B)  # 用來存儲每次 bootstrap 之後的平均值

for (i in 1:B) {
  # 使用 runif() 生成隨機索引，並將它們縮放到 1 到 n 的範圍
  sample_indices <- floor(runif(n, min = 1, max = n + 1))  # 生成隨機索引
  boot_means_fish[i] <- mean(fish[sample_indices])  # 計算 bootstrap 樣本的平均值
}

se_boot_fish <- sd(boot_means_fish)  # 計算標準誤

cat("Bootstrap estimates:\n")
cat("Fish: SE =", se_boot_fish, "\n")

#gemini

# 假設你的資料是 fish_density
fish_density <- c(12, 15, 18, 20, 22, 25, 28, 30, 32, 35)
n <- length(fish_density)
n_bootstrap <- 1000
fish_bootstrap_means <- numeric(n_bootstrap)

for (i in 1:n_bootstrap) {
  # 使用 runif 生成隨機索引
  indices <- floor(runif(n, 1, n + 1)) # 生成 1 到 n 之間的整數
  
  # 使用生成的索引選擇資料點
  bootstrap_sample <- fish_density[indices]
  
  # 計算 bootstrap 樣本的平均值
  fish_bootstrap_means[i] <- mean(bootstrap_sample)
}

# 剩下的程式碼與之前相同，計算平均值、標準差、繪製直方圖等
fish_mean_bootstrap <- mean(fish_bootstrap_means)
fish_se_bootstrap <- sd(fish_bootstrap_means)

print(paste("Fish Mean (Bootstrap):", fish_mean_bootstrap))
print(paste("Fish SE (Bootstrap):", fish_se_bootstrap))

hist(fish_bootstrap_means, breaks = 30, main = "Bootstrap Means - Fish Density", xlab = "Mean", ylab = "Frequency")

#一開始的中文備註

#會跑很久的方法
#for: loop that will run `1000(B)` times ( number of resampling iterations.)

#floor(runif()): 
#  生成了 n 個在 [1, n+1) 範圍內的隨機數字，這些數字是浮點數。n 是樣本的數量，這裡的 min = 1 和 max = n+1 是為了確保隨機數生成在 1 到 n 的範圍內，但由於是浮點數，我們需要使用 floor() 函數來取整，將其轉換為整數索引。
 # 使用 floor() 來確保索引是整數，並將生成的浮點數轉換為對應的索引（1 到 n），然後用這些索引來選擇重抽樣的元素。
  #
  #boot_means_fish[i] <- mean(fish[sample_indices]) 和 boot_means_copepod[i] <- mean(copepod[sample_indices])：
#  對於每次重抽樣，計算重抽樣樣本的均值。
 # 根據上面的 sample_indices，它從 fish 和 copepod 這兩個資料集選擇樣本，並計算均值。

  for (i in 1:B) { 
    sample_indices <- floor(runif(n, min = 1, max = n+1))  
    boot_means_fish[i] <- mean(fish[sample_indices])
    boot_means_copepod[i] <- mean(copepod[sample_indices])
  }
  
  se_boot_fish <- sd(boot_means_fish)
  se_boot_copepod <- sd(boot_means_copepod)
  
  cat("Bootstrap estimates:\n")
  cat("Fish: SE =", se_boot_fish, "\n")
  cat("Copepod: SE =", se_boot_copepod, "\n")


##跑得出來ㄟ

set.seed(123)  # 確保結果可重現

# 產生隨機數據
n <- 100
fish <- rnorm(n, mean = 50, sd = 10)  # 魚的密度
copepod <- rnorm(n, mean = 30, sd = 5)  # 橈足類密度

# 1. 正規理論計算均值與標準誤
mean_fish <- mean(fish)
se_fish <- sd(fish) / sqrt(n)

mean_copepod <- mean(copepod)
se_copepod <- sd(copepod) / sqrt(n)

cat("Fish Mean:", mean_fish, "SE:", se_fish, "\n")
cat("Copepod Mean:", mean_copepod, "SE:", se_copepod, "\n")

# 2. Bootstrap 計算均值的標準誤
B <- 1000  # bootstrap 次數
boot_means_fish <- numeric(B)
boot_means_copepod <- numeric(B)

for (i in 1:B) {
  sample_indices <- floor(runif(n, min = 1, max = n+1))  # 手動重抽樣
  boot_means_fish[i] <- mean(fish[sample_indices])
  boot_means_copepod[i] <- mean(copepod[sample_indices])
}

se_boot_fish <- sd(boot_means_fish)
se_boot_copepod <- sd(boot_means_copepod)

cat("Bootstrapped Fish SE:", se_boot_fish, "\n")
cat("Bootstrapped Copepod SE:", se_boot_copepod, "\n")

# 繪製 Bootstrapped Means 直方圖
hist(boot_means_fish, breaks = 30, col = "skyblue", main = "Bootstrap Fish Means")
hist(boot_means_copepod, breaks = 30, col = "lightgreen", main = "Bootstrap Copepod Means")

# 3. Bootstrap 計算中位數的標準誤
boot_medians_fish <- numeric(B)
boot_medians_copepod <- numeric(B)

for (i in 1:B) {
  sample_indices <- floor(runif(n, min = 1, max = n+1))
  boot_medians_fish[i] <- median(fish[sample_indices])
  boot_medians_copepod[i] <- median(copepod[sample_indices])
}

se_median_fish <- sd(boot_medians_fish)
se_median_copepod <- sd(boot_medians_copepod)

cat("Bootstrapped Fish SE (Median):", se_median_fish, "\n")
cat("Bootstrapped Copepod SE (Median):", se_median_copepod, "\n")

# 繪製 Bootstrapped Medians 直方圖
hist(boot_medians_fish, breaks = 30, col = "skyblue", main = "Bootstrap Fish Medians")
hist(boot_medians_copepod, breaks = 30, col = "lightgreen", main = "Bootstrap Copepod Medians")

# 4. 線性回歸與 Bootstrap 估計標準誤
reg <- lm(fish ~ copepod)
b0_hat <- coef(reg)[1]
b1_hat <- coef(reg)[2]

boot_b0 <- numeric(B)
boot_b1 <- numeric(B)

for (i in 1:B) {
  sample_indices <- floor(runif(n, min = 1, max = n+1))
  boot_reg <- lm(fish[sample_indices] ~ copepod[sample_indices])
  boot_b0[i] <- coef(boot_reg)[1]
  boot_b1[i] <- coef(boot_reg)[2]
}

se_b0 <- sd(boot_b0)
se_b1 <- sd(boot_b1)

cat("Regression b0:", b0_hat, "SE:", se_b0, "\n")
cat("Regression b1:", b1_hat, "SE:", se_b1, "\n")

# 繪製回歸線
plot(copepod, fish, main = "Fish vs. Copepod", xlab = "Copepod Density", ylab = "Fish Density", pch = 16)
abline(reg, col = "red", lwd = 2)

# 繪製 Bootstrapped b0 和 b1 直方圖
hist(boot_b0, breaks = 30, col = "orange", main = "Bootstrap b0")
hist(boot_b1, breaks = 30, col = "purple", main = "Bootstrap b1")


#claude

# Bootstrap Analysis for Fish and Copepod Data

# First, we'll generate some example data
# In a real scenario, you would import your actual data
set.seed(123)
n <- 100  # Number of observations
copepod <- rnorm(n, mean=50, sd=10)  # Simulated copepod densities
fish <- 2 + 0.5 * copepod + rnorm(n, mean=0, sd=5)  # Simulated fish densities with relationship to copepod

# 1. Compute mean and SE using normal theory and bootstrap
# Normal theory calculations
fish_mean <- mean(fish)
fish_se_normal <- sd(fish) / sqrt(length(fish))

copepod_mean <- mean(copepod)
copepod_se_normal <- sd(copepod) / sqrt(length(copepod))

# Bootstrap calculations for mean
B <- 1000  # Number of bootstrap samples

# Initialize vectors to store bootstrap means
bootstrap_fish_means <- numeric(B)
bootstrap_copepod_means <- numeric(B)

# Bootstrap for means
for (i in 1:B) {
  # Generate bootstrap indices (sampling with replacement)
  indices <- ceiling(runif(n) * n)
  
  # Calculate mean on bootstrapped sample
  bootstrap_fish_means[i] <- mean(fish[indices])
  bootstrap_copepod_means[i] <- mean(copepod[indices])
}

# Calculate bootstrap SE for means
fish_se_bootstrap <- sd(bootstrap_fish_means)
copepod_se_bootstrap <- sd(bootstrap_copepod_means)

# Plot histograms of bootstrapped means
par(mfrow=c(1,2))  # Set up a 1x2 plotting area

hist(bootstrap_fish_means, main="Bootstrap Distribution of Fish Mean", 
     xlab="Mean Fish Density", col="lightblue", breaks=30)
abline(v=fish_mean, col="red", lwd=2)
text(x=fish_mean, y=5, labels=paste("Mean =", round(fish_mean, 2)), pos=4)

hist(bootstrap_copepod_means, main="Bootstrap Distribution of Copepod Mean", 
     xlab="Mean Copepod Density", col="lightgreen", breaks=30)
abline(v=copepod_mean, col="red", lwd=2)
text(x=copepod_mean, y=5, labels=paste("Mean =", round(copepod_mean, 2)), pos=4)

# 2. Compute median and bootstrapped SE(median)
fish_median <- median(fish)
copepod_median <- median(copepod)

# Initialize vectors to store bootstrap medians
bootstrap_fish_medians <- numeric(B)
bootstrap_copepod_medians <- numeric(B)

# Bootstrap for medians
for (i in 1:B) {
  # Generate bootstrap indices
  indices <- ceiling(runif(n) * n)
  
  # Calculate median on bootstrapped sample
  bootstrap_fish_medians[i] <- median(fish[indices])
  bootstrap_copepod_medians[i] <- median(copepod[indices])
}

# Calculate bootstrap SE for medians
fish_median_se_bootstrap <- sd(bootstrap_fish_medians)
copepod_median_se_bootstrap <- sd(bootstrap_copepod_medians)

# Plot histograms of bootstrapped medians
par(mfrow=c(1,2))  # Set up a 1x2 plotting area

hist(bootstrap_fish_medians, main="Bootstrap Distribution of Fish Median", 
     xlab="Median Fish Density", col="lightblue", breaks=30)
abline(v=fish_median, col="red", lwd=2)
text(x=fish_median, y=5, labels=paste("Median =", round(fish_median, 2)), pos=4)

hist(bootstrap_copepod_medians, main="Bootstrap Distribution of Copepod Median", 
     xlab="Median Copepod Density", col="lightgreen", breaks=30)
abline(v=copepod_median, col="red", lwd=2)
text(x=copepod_median, y=5, labels=paste("Median =", round(copepod_median, 2)), pos=4)

# 3. Regression analysis with bootstrap
# Calculate regression coefficients (normal approach)
X <- cbind(1, copepod)  # Design matrix with intercept
beta <- solve(t(X) %*% X) %*% t(X) %*% fish
b0 <- beta[1]  # Intercept
b1 <- beta[2]  # Slope

# Plot the regression
par(mfrow=c(1,1))  # Reset to single plot
plot(copepod, fish, pch=16, col="blue", 
     main="Fish vs Copepod Density with Regression Line",
     xlab="Copepod Density", ylab="Fish Density")
abline(b0, b1, col="red", lwd=2)
legend("topleft", legend=c(paste("Intercept =", round(b0, 3)), 
                           paste("Slope =", round(b1, 3))),
       bty="n")

# Bootstrap for regression coefficients
bootstrap_b0 <- numeric(B)
bootstrap_b1 <- numeric(B)

for (i in 1:B) {
  # Generate bootstrap indices for pairs
  indices <- ceiling(runif(n) * n)
  
  # Get bootstrap samples
  bootstrap_fish <- fish[indices]
  bootstrap_copepod <- copepod[indices]
  
  # Calculate regression coefficients on bootstrapped sample
  X_bootstrap <- cbind(1, bootstrap_copepod)
  beta_bootstrap <- solve(t(X_bootstrap) %*% X_bootstrap) %*% t(X_bootstrap) %*% bootstrap_fish
  
  bootstrap_b0[i] <- beta_bootstrap[1]
  bootstrap_b1[i] <- beta_bootstrap[2]
}

# Calculate bootstrap SE for regression coefficients
b0_se_bootstrap <- sd(bootstrap_b0)
b1_se_bootstrap <- sd(bootstrap_b1)

# Plot histograms of bootstrapped regression coefficients
par(mfrow=c(1,2))  # Set up a 1x2 plotting area

hist(bootstrap_b0, main="Bootstrap Distribution of Intercept (b0)", 
     xlab="Intercept", col="orange", breaks=30)
abline(v=b0, col="red", lwd=2)
text(x=b0, y=5, labels=paste("b0 =", round(b0, 3)), pos=4)

hist(bootstrap_b1, main="Bootstrap Distribution of Slope (b1)", 
     xlab="Slope", col="purple", breaks=30)
abline(v=b1, col="red", lwd=2)
text(x=b1, y=5, labels=paste("b1 =", round(b1, 3)), pos=4)

# Print summary results
cat("\nNormal Theory Results:\n")
cat("Fish Mean:", round(fish_mean, 3), "SE:", round(fish_se_normal, 3), "\n")
cat("Copepod Mean:", round(copepod_mean, 3), "SE:", round(copepod_se_normal, 3), "\n\n")

cat("Bootstrap Results for Mean:\n")
cat("Fish Mean SE (Bootstrap):", round(fish_se_bootstrap, 3), "\n")
cat("Copepod Mean SE (Bootstrap):", round(copepod_se_bootstrap, 3), "\n\n")

cat("Median Results:\n")
cat("Fish Median:", round(fish_median, 3), "SE (Bootstrap):", round(fish_median_se_bootstrap, 3), "\n")
cat("Copepod Median:", round(copepod_median, 3), "SE (Bootstrap):", round(copepod_median_se_bootstrap, 3), "\n\n")

cat("Regression Results:\n")
cat("Intercept (b0):", round(b0, 3), "SE (Bootstrap):", round(b0_se_bootstrap, 3), "\n")
cat("Slope (b1):", round(b1, 3), "SE (Bootstrap):", round(b1_se_bootstrap, 3), "\n")

#ChatGPT:
  # 產生隨機數據
  n <- 100
fish <- rnorm(n, mean = 50, sd = 10)  # 魚的密度
copepod <- rnorm(n, mean = 30, sd = 5)  # 橈足類密度

# 1. 正規理論計算均值與標準誤
mean_fish <- mean(fish)
se_fish <- sd(fish) / sqrt(n)

mean_copepod <- mean(copepod)
se_copepod <- sd(copepod) / sqrt(n)

cat("Fish Mean:", mean_fish, "SE:", se_fish, "\n")
cat("Copepod Mean:", mean_copepod, "SE:", se_copepod, "\n")

#2. Compute the median and bootstrapped SE(median) for the fish and copepod densities. 
#Plot the histogram of bootstrapped medians with bootstrap 1000 times. 

# 2. Bootstrap 計算均值的標準誤
B <- 1000  # bootstrap 次數
boot_means_fish <- numeric(B)
boot_means_copepod <- numeric(B)

for (i in 1:B) {
  sample_indices <- floor(runif(n, min = 1, max = n+1))  # 手動重抽樣
  boot_means_fish[i] <- mean(fish[sample_indices])
  boot_means_copepod[i] <- mean(copepod[sample_indices])
}

se_boot_fish <- sd(boot_means_fish)
se_boot_copepod <- sd(boot_means_copepod)

cat("Bootstrapped Fish SE:", se_boot_fish, "\n")
cat("Bootstrapped Copepod SE:", se_boot_copepod, "\n")

# 繪製 Bootstrapped Means 直方圖
hist(boot_means_fish, breaks = 30, col = "skyblue", main = "Bootstrap Fish Means")
hist(boot_means_copepod, breaks = 30, col = "lightgreen", main = "Bootstrap Copepod Means")

# 3. Bootstrap 計算中位數的標準誤
boot_medians_fish <- numeric(B)
boot_medians_copepod <- numeric(B)

for (i in 1:B) {
  sample_indices <- floor(runif(n, min = 1, max = n+1))
  boot_medians_fish[i] <- median(fish[sample_indices])
  boot_medians_copepod[i] <- median(copepod[sample_indices])
}

se_median_fish <- sd(boot_medians_fish)
se_median_copepod <- sd(boot_medians_copepod)

cat("Bootstrapped Fish SE (Median):", se_median_fish, "\n")
cat("Bootstrapped Copepod SE (Median):", se_median_copepod, "\n")


#最後一題，應該是不會用到了

```{r}
print(paste("Normal theory estimates:"))
print(paste("Fish: Mean =", mean_fish, "SE =", se_fish))
print(paste("Copepod: Mean =", mean_copepod, "SE =", se_copepod))

print("Bootstrap estimates:")
print(paste("Fish: SE =", se_boot_fish))
print(paste("Copepod: SE =", se_boot_copepod))

print(paste("Jacknife estimates:"))
print(paste("Fish: Mean =", round(fish_jackknife$mean, 5), "Jackknife SE =", round(fish_jackknife$se, 8)))
print(paste("Copepod: Mean =", round(copepod_jackknife$mean, 5), "Jackknife SE =", round(copepod_jackknife$se, 8)))


```


```{r}
# Normal theory estimates (using mean and SE)
normal_fish <- mean(fish)
normal_se_fish <- sd(fish) / sqrt(length(fish))
normal_copepod <- mean(copepod)
normal_se_copepod <- sd(copepod) / sqrt(length(copepod))

# Compare estimates for Q1 (mean) and Q2 (standard error)
print("Comparison of Normal Theory, Bootstrap, and Jackknife:")
print(paste("Normal Fish: Mean =", mean_fish, "SE =", se_fish))
print(paste("Normal Copepod: Mean =", mean_copepod, "SE =", se_copepod))
print(paste("Jackknife Fish: Mean =", round(fish_jackknife$mean, 2), "SE =", round(fish_jackknife$se, 2)))
print(paste("Jackknife Copepod: Mean =", round(copepod_jackknife$mean, 2), "SE =", round(copepod_jackknife$se, 2)))

# For bootstrap, you would need the bootstrapped estimates (boot_b0, boot_b1, etc.) 
# and compare the values. You can create similar comparison statements for bootstrapped results.

```
gemini 刪掉
```{r}
# Data
fish_density <- c(12, 15, 18, 20, 22, 25, 28, 30, 32, 35)
copepod_density <- c(5, 7, 9, 11, 13, 15, 17, 19, 21, 23)
n <- length(fish_density)

# 1. Jackknife Mean and SE(mean)

# Fish Jackknife
fish_jack_means <- numeric(n)
for (i in 1:n) {
  fish_jack_means[i] <- mean(fish_density[-i])
}
fish_jack_mean <- mean(fish_jack_means)
fish_jack_se <- sqrt(((n - 1) / n) * sum((fish_jack_means - fish_jack_mean)^2))

print(paste("Fish Jackknife Mean:", fish_jack_mean))
print(paste("Fish Jackknife SE:", fish_jack_se))

# Copepod Jackknife
copepod_jack_means <- numeric(n)
for (i in 1:n) {
  copepod_jack_means[i] <- mean(copepod_density[-i])
}
copepod_jack_mean <- mean(copepod_jack_means)
copepod_jack_se <- sqrt(((n - 1) / n) * sum((copepod_jack_means - copepod_jack_mean)^2))

print(paste("Copepod Jackknife Mean:", copepod_jack_mean))
print(paste("Copepod Jackknife SE:", copepod_jack_se))

# Plotting Histograms of Jackknife Means
par(mfrow = c(1, 2))
hist(fish_jack_means, breaks = 30, main = "Jackknife Means - Fish Density", xlab = "Mean", ylab = "Frequency")
hist(copepod_jack_means, breaks = 30, main = "Jackknife Means - Copepod Density", xlab = "Mean", ylab = "Frequency")

# 2. Jackknife Regression Coefficients and SE

jack_b0 <- numeric(n)
jack_b1 <- numeric(n)

for (i in 1:n) {
  X_jack <- copepod_density[-i]
  Y_jack <- fish_density[-i]
  
  X_jack_mean <- mean(X_jack)
  Y_jack_mean <- mean(Y_jack)
  
  jack_b1[i] <- sum((X_jack - X_jack_mean) * (Y_jack - Y_jack_mean)) / sum((X_jack - X_jack_mean)^2)
  jack_b0[i] <- Y_jack_mean - jack_b1[i] * X_jack_mean
}

jack_b0_mean <- mean(jack_b0)
jack_b1_mean <- mean(jack_b1)

jack_b0_se <- sqrt(((n - 1) / n) * sum((jack_b0 - jack_b0_mean)^2))
jack_b1_se <- sqrt(((n - 1) / n) * sum((jack_b1 - jack_b1_mean)^2))

print(paste("Jackknife b0:", jack_b0_mean))
print(paste("Jackknife SE(b0):", jack_b0_se))
print(paste("Jackknife b1:", jack_b1_mean))
print(paste("Jackknife SE(b1):", jack_b1_se))

# Plotting Histograms of Jackknife b0 and b1
par(mfrow = c(1, 2))
hist(jack_b0, breaks = 30, main = "Jackknife b0", xlab = "b0", ylab = "Frequency")
hist(jack_b1, breaks = 30, main = "Jackknife b1", xlab = "b1", ylab = "Frequency")

# 3. Comparison of Estimates

# Normal Theory (from previous code)
fish_mean_normal <- mean(fish_density)
fish_se_normal <- sd(fish_density) / sqrt(length(fish_density))
copepod_mean_normal <- mean(copepod_density)
copepod_se_normal <- sd(copepod_density) / sqrt(length(copepod_density))

X <- copepod_density
Y <- fish_density
X_mean <- mean(X)
Y_mean <- mean(Y)

b1 <- sum((X - X_mean) * (Y - Y_mean)) / sum((X - X_mean)^2)
b0 <- Y_mean - b1 * X_mean

# Bootstrap (from previous code, using runif for sampling)
n_bootstrap <- 1000
fish_bootstrap_means <- numeric(n_bootstrap)
copepod_bootstrap_means <- numeric(n_bootstrap)

bootstrap_b0 <- numeric(n_bootstrap)
bootstrap_b1 <- numeric(n_bootstrap)

for (i in 1:n_bootstrap) {
  indices <- floor(runif(n, 1, n + 1))
  
  fish_bootstrap_means[i] <- mean(fish_density[indices])
  copepod_bootstrap_means[i] <- mean(copepod_density[indices])
  
  X_boot <- copepod_density[indices]
  Y_boot <- fish_density[indices]
  
  X_boot_mean <- mean(X_boot)
  Y_boot_mean <- mean(Y_boot)
  
  bootstrap_b1[i] <- sum((X_boot - X_boot_mean) * (Y_boot - Y_boot_mean)) / sum((X_boot - X_boot_mean)^2)
  bootstrap_b0[i] <- Y_boot_mean - bootstrap_b1[i] * X_boot_mean
}

fish_mean_bootstrap <- mean(fish_bootstrap_means)
fish_se_bootstrap <- sd(fish_bootstrap_means)
copepod_mean_bootstrap <- mean(copepod_bootstrap_means)
copepod_se_bootstrap <- sd(copepod_bootstrap_means)

se_b0_bootstrap <- sd(bootstrap_b0)
se_b1_bootstrap <- sd(bootstrap_b1)

# Comparison Table
comparison_table <- data.frame(
  Method = c("Normal Theory", "Bootstrap", "Jackknife"),
  Fish_Mean = c(fish_mean_normal, fish_mean_bootstrap, fish_jack_mean),
  Fish_SE = c(fish_se_normal, fish_se_bootstrap, fish_jack_se),
  Copepod_Mean = c(copepod_mean_normal, copepod_mean_bootstrap, copepod_jack_mean),
  Copepod_SE = c(copepod_se_normal, copepod_se_bootstrap, copepod_jack_se),
  b0 = c(b0, mean(bootstrap_b0), jack_b0_mean),
  SE_b0 = c(NA, se_b0_bootstrap, jack_b0_se),
  b1 = c(b1, mean(bootstrap_b1), jack_b1_mean),
  SE_b1 = c(NA, se_b1_bootstrap, jack_b1_se)
)

print(comparison_table)

library(readxl)

# Read the Excel file
data <- read_excel("C:/Users/User/Desktop/Master 1.2/R/HW/enviANDdensity.xls")

# Extract the required columns and rename them for convenience
fish <- data[["FishDensity (ind./1000m3)"]]
copepod <- data[["CopepodDensity ind./m3"]]

-----
  
# Jackknife function to compute mean and standard error
jackknife_mean <- function(data) {
  
  n <- length(data)  
  jackknife_means <- numeric(n)  
  
  # Loop through each data point, leaving one out at a time
  for (i in 1:n) {
    jackknife_means[i] <- mean(data[-i]) #Compute mean after leaving out the i-th data point
  }
  
  jackknife_mean <- mean(jackknife_means)  # Compute the mean of all leave-one-out means
  jackknife_se <- sqrt((n - 1) / n * sum((jackknife_means - jackknife_mean)^2))  
  return(list(means = jackknife_means, mean = jackknife_mean, se = jackknife_se))
}

# Calculate for fish and copepod densities
fish_jackknife <- jackknife_mean(fish)
copepod_jackknife <- jackknife_mean(copepod)

# Print results
print(paste("Fish: Jackknife Mean =", round(fish_jackknife$mean, 2), "Jackknife SE =", round(fish_jackknife$se, 2)))
print(paste("Copepod: Jackknife Mean =", round(copepod_jackknife$mean, 2), "Jackknife SE =", round(copepod_jackknife$se, 2)))

# Plot histograms of Jackknife means
par(mfrow=c(1,2))  # Set up a 1x2 plotting area
hist(fish_jackknife$means, main="Jackknife Means of Fish", xlab="Mean Fish Density", col="lightblue", breaks=30)
hist(copepod_jackknife$means, main="Jackknife Means of Copepod", xlab="Mean Copepod Density", col="lightgreen", breaks=30)
