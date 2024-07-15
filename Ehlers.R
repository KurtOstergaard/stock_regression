# Ehlers cycle 11.2 Stochasic


# Automatic Gain Control (fast attack - slow decay AGC)
AGC <- function (loCutoff = 10, hiCutoff = 48, slope = 1.5) {   
  accSlope = -slope # acceptableSlope = 1.5 dB
  ratio = 10 ^ (accSlope / 20)
  if ((hiCutoff - loCutoff) > 0)
    factor <-  ratio ^ (2 / (hiCutoff - loCutoff));
  return (factor)
}

# Autocorrelation Periodogram (code 8.3)
autocorrStoch <- function (x, period1 = 10, period2 = 48, avgLength = 3) {
  # high pass filter
  alpha1 <- (cos(sqrt(2) * pi / period2) + sin(sqrt(2) * pi / period2) - 1) / 
    cos(sqrt(2) * pi / period2)
  hp <- (1 - alpha1 / 2) ^ 2 * (x - 2 * lag(x) + lag(x, 2))
  hp <- hp[-c(1, 2)]
  hp <- stats::filter(hp, (1 - alpha1), method = "recursive")
  hp <- c(NA, NA, hp)
  hp <- xts(hp, order.by = index(x))
  # super smoother
  a1 <- exp(-sqrt(2) * pi / period1)
  b1 <- 2 * a1 * cos(sqrt(2) * pi / period1)
  c2 <- b1
  c3 <- -a1 * a1
  c1 <- 1 - c2 - c3
  filt <- c1 * (hp + lag(hp)) / 2
  leadNAs <- sum(is.na(filt))
  filt <- filt[-c(1: leadNAs)]
  filt <- stats::filter(filt, c(c2, c3), method = "recursive")
  filt <- c(rep(NA, leadNAs), filt)
  filt <- xts(filt, order.by = index(x))
#   return(filt)
# }

  # Pearson correlation for each value of lag
  autocorr <- matrix(0, period2, length(filt))
  for (lag in 2: period2) {
    # Set the average length as M
    if (avgLength == 0) M <- lag
    else M <- avgLength
    autocorr[lag, ] <- runCor(filt, lag(filt, lag), M)
  }
  autocorr[is.na(autocorr)] <- 0
  # Discrete Fourier transform
  # Correlate autocorrelation values with cosine and sine of each period of interest
  # The sum of the squares of each value represents relative power at each period
  cosinePart <- sinePart <- sqSum <- R <- Pwr <- matrix(0, period2, length(filt))
  for (period in period1: period2) {
    for (N in 2: period2) {
      cosinePart[period, ] = cosinePart[period, ] + autocorr[N, ] * cos(2 * N * pi / period)
      sinePart[period, ] = sinePart[period, ] + autocorr[N, ] * sin(2 * N * pi / period)
    }
    sqSum[period, ] = cosinePart[period, ] ^ 2 + sinePart[period, ] ^ 2
    R[period, ] <- EMA(sqSum[period, ] ^ 2, ratio = 0.2)
  }
  R[is.na(R)] <- 0
  # Normalising Power
  K <- AGC(period1, period2, 1.5)
  maxPwr <- rep(0, length(filt))
  for(period in period1: period2) {
    for (i in 1: length(filt)) {
      if (R[period, i] >= maxPwr[i]) maxPwr[i] <- R[period, i]
      # else maxPwr[i] <- K * maxPwr[i]
    }
  }
  for(period in 2: period2) {
    Pwr[period, ] <- R[period, ] / maxPwr
  }
  # Compute the dominant cycle using the Center of Gravity of the spectrum
  Spx <- Sp <- rep(0, length(filter))
  for(period in period1: period2) {
    Spx <- Spx + period * Pwr[period, ] * (Pwr[period, ] >= 0.5)
    Sp <- Sp + Pwr[period, ] * (Pwr[period, ] >= 0.5)
  }
  dominantCycle <- Spx / Sp
  dominantCycle[is.nan(dominantCycle)] <- 0
 # heatmap(Pwr, Rowv = NA, Colv = NA, na.rm = TRUE, labCol = "", 
  # add.expr = lines(dominantCycle, col = 'blue'))
  return()
  
  mstoc <- (filt - runMin(filt, dominantCycle)) / (runMax(filt, dominantCycle) - runMin(filt, dominantCycle))
  mstoc <- c1 * (mstoc + lag(mstoc)) / 2
  leadNAs <- sum(is.na(mstoc))
  mstoc <- mstoc[-c(1: leadNAs)]
  mstoc <- stats::filter(mstoc, c(c2, c3), method = "recursive")
  mstoc <- c(rep(NA, leadNAs), mstoc)
  return(xts(mstoc, order.by = index(x)))
}

# Read the CSV file
test_file <- read_csv("AGQ.csv", col_names = TRUE, show_col_types = FALSE)
tfG <- test_file %>%
  select(time:close, Fisher, Fish1:length)

tfG$Ehlers <- autocorrStoch(tfG$close, 10, 48, 3)


########### new code below, from Claude 3.5
library(dplyr)
library(TTR)

# Automatic Gain Control (fast attack - slow decay AGC)
AGC <- function (loCutoff = 10, hiCutoff = 48, slope = 1.5) {   
  accSlope = -slope # acceptableSlope = 1.5 dB
  ratio = 10 ^ (accSlope / 20)
  if ((hiCutoff - loCutoff) > 0)
    factor <-  ratio ^ (2 / (hiCutoff - loCutoff));
  return (factor)
}

# Autocorrelation Periodogram (code 8.3)
autocorrStoch <- function (x, period1 = 10, period2 = 48, avgLength = 3) {
  n <- length(x)
  
  # Ensure period2 is not larger than the data length
  period2 <- min(period2, n - 1)
  
  # high pass filter
  alpha1 <- (cos(sqrt(2) * pi / period2) + sin(sqrt(2) * pi / period2) - 1) / 
    cos(sqrt(2) * pi / period2)
  hp <- (1 - alpha1 / 2) ^ 2 * (x - 2 * lag(x) + lag(x, 2))
  hp <- hp[-c(1, 2)]
  hp <- stats::filter(hp, (1 - alpha1), method = "recursive")
  hp <- c(NA, NA, hp)
  
  # super smoother
  a1 <- exp(-sqrt(2) * pi / period1)
  b1 <- 2 * a1 * cos(sqrt(2) * pi / period1)
  c2 <- b1
  c3 <- -a1 * a1
  c1 <- 1 - c2 - c3
  filt <- c1 * (hp + lag(hp)) / 2
  leadNAs <- sum(is.na(filt))
  filt <- filt[-c(1:leadNAs)]
  filt <- stats::filter(filt, c(c2, c3), method = "recursive")
  filt <- c(rep(NA, leadNAs), filt)
  
  # Pearson correlation for each value of lag
  autocorr <- matrix(0, period2, length(filt))
  for (lag in 2:period2) {
    # Set the average length as M
    if (avgLength == 0) M <- lag
    else M <- min(avgLength, n - lag)  # Ensure M is not larger than available data
    
    # Use na.rm = TRUE in runCor to handle NA values
    autocorr[lag, ] <- runCor(filt, lag(filt, lag), n = M, use = "pairwise.complete.obs")
  }
  autocorr[is.na(autocorr)] <- 0
  
  # Discrete Fourier transform
  cosinePart <- sinePart <- sqSum <- R <- Pwr <- matrix(0, period2, length(filt))
  for (period in period1:period2) {
    for (N in 2:period2) {
      cosinePart[period, ] = cosinePart[period, ] + autocorr[N, ] * cos(2 * N * pi / period)
      sinePart[period, ] = sinePart[period, ] + autocorr[N, ] * sin(2 * N * pi / period)
    }
    sqSum[period, ] = cosinePart[period, ] ^ 2 + sinePart[period, ] ^ 2
    R[period, ] <- EMA(sqSum[period, ] ^ 2, n = min(5, n))  # Ensure n is not larger than data length
  }
  R[is.na(R)] <- 0
  
  # Normalising Power
  K <- AGC(period1, period2, 1.5)
  maxPwr <- rep(0, length(filt))
  for(period in period1:period2) {
    for (i in 1:length(filt)) {
      if (R[period, i] >= maxPwr[i]) maxPwr[i] <- R[period, i]
      # else maxPwr[i] <- K * maxPwr[i]
    }
  }
  for(period in 2:period2) {
    Pwr[period, ] <- R[period, ] / maxPwr
  }
  
  # Compute the dominant cycle using the Center of Gravity of the spectrum
  Spx <- Sp <- rep(0, length(filt))
  for(period in period1:period2) {
    Spx <- Spx + period * Pwr[period, ] * (Pwr[period, ] >= 0.5)
    Sp <- Sp + Pwr[period, ] * (Pwr[period, ] >= 0.5)
  }
  dominantCycle <- Spx / Sp
  dominantCycle[is.nan(dominantCycle)] <- 0
  
  # Ensure dominantCycle is not larger than the data length
  dominantCycle <- pmin(dominantCycle, n)
  
  mstoc <- (filt - runMin(filt, n = pmin(round(dominantCycle), n))) / 
    (runMax(filt, n = pmin(round(dominantCycle), n)) - runMin(filt, n = pmin(round(dominantCycle), n)))
  mstoc <- c1 * (mstoc + lag(mstoc)) / 2
  leadNAs <- sum(is.na(mstoc))
  mstoc <- mstoc[-c(1:leadNAs)]
  mstoc <- stats::filter(mstoc, c(c2, c3), method = "recursive")
  mstoc <- c(rep(NA, leadNAs), mstoc)
  return(mstoc)
}

library(readr)
library(dplyr)
library(TTR)

# Read the CSV file
test_file <- read_csv("AGQ.csv", col_names = TRUE, show_col_types = FALSE)
tfG <- test_file %>%
  select(time:close, Fisher, Fish1:length)

tfG$Ehlers <- autocorrStoch(tfG$close, 10, 48, 3)


