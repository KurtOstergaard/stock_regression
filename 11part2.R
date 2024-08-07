library(tidyverse)

# Read the CSV file
DG_file <- read_csv("DG.csv", col_names = TRUE, show_col_types = FALSE) |>
  filter(time > "2011-01-01",
         time < "2012-11-01") |>
  select(time:Volume, Hurst, EBSI)

# Define variables
count <- 0
Sx <- 0
Sy <- 0
Sxx <- 0
Syy <- 0
Sxy <- 0
Period <- 0
Sp <- 0
Spx <- 0
MaxPwr <- 0
DominantCycle <- 0

# Define arrays
Corr <- rep(0, 48)
CosinePart <- rep(0, 48)
SinePart <- rep(0, 48)
SqSum <- rep(0, 48)
R <- matrix(0, nrow = 48, ncol = 2)
Pwr <- rep(0, 48)

# Highpass filter cyclic components whose periods are shorter than 48 bars
highPassFilter <- function(x) {
  alpha1 <- (cos(sqrt(2)*pi/48)+sin(sqrt(2)*pi/48)-1)/cos(sqrt(2)*pi/48)
  HP <- (1-alpha1/2)*(1-alpha1/2)*(x-2*lag(x)+lag(x,2))
  HP <- HP[-c(1,2)]
  HP <- filter(HP, c(2*(1-alpha1), -1*(1-alpha1)*(1-alpha1)), method="recursive")
  HP <- c(NA, NA, HP)
  HP <- xts(HP, order.by=index(x))
  colnames(HP) = 'highPassFilter'
  return(HP)
}

# Smooth with a Super Smoother Filter from equation 3-3
superSmoother <- function(x) {
  a1 <- exp(-sqrt(2)*pi/10)
  b1 <- 2*a1*cos(sqrt(2)*pi/10)
  c2 <- b1
  c3 <- -a1*a1
  c1 <- 1-c2-c3
  filt <- c1*(x+lag(x))/2
  leadNAs <- sum(is.na(filt))
  filt <- filt[-c(1:leadNAs)]
  filt <- filter(filt, c(c2, c3), method="recursive")
  filt <- c(rep(NA,leadNAs), filt)
  filt <- xts(filt, order.by=index(x))
  colnames(filt) = 'superSmoother'
  return(filt)
}


# Remove initial NAs from HP
HP <- na.omit(HP)



# Remove initial NAs from Filt
Filt <- na.omit(Filt)

# Pearson correlation for each value of lag
for (Lag in 0:48) {
  M <- ifelse(AvgLength == 0, Lag, AvgLength)
  Sx <- Sy <- Sxx <- Syy <- Sxy <- 0
  for (count in 0:(M - 1)) {
    X <- Filt[count + 1]
    Y <- Filt[Lag + count + 1]
    Sx <- Sx + X
    Sy <- Sy + Y
    Sxx <- Sxx + X * X
    Sxy <- Sxy + X * Y
    Syy <- Syy + Y * Y
  }
  if (!is.na((M * Sxx - Sx * Sx) * (M * Syy - Sy * Sy)) && (M * Sxx - Sx * Sx) * (M * Syy - Sy * Sy) > 0) {
    Corr[Lag + 1] <- (M * Sxy - Sx * Sy) / sqrt((M * Sxx - Sx * Sx) * (M * Syy - Sy * Sy))
  }
}

for (Period in 10:48) {
  CosinePart[Period] <- 0
  SinePart[Period] <- 0
  for (N in 3:48) {
    CosinePart[Period] <- CosinePart[Period] + Corr[N + 1] * cos(360 * N / Period)
    SinePart[Period] <- SinePart[Period] + Corr[N + 1] * sin(360 * N / Period)
  }
  SqSum[Period] <- CosinePart[Period] * CosinePart[Period] + SinePart[Period] * SinePart[Period]
}

for (Period in 10:48) {
  R[Period, 2] <- R[Period, 1]
  R[Period, 1] <- 0.2 * SqSum[Period] * SqSum[Period] + 0.8 * R[Period, 2]
}

# Find Maximum Power Level for Normalization
MaxPwr <- 0.995 * MaxPwr
for (Period in 10:48) {
  if (!is.na(R[Period, 1]) && R[Period, 1] > MaxPwr) {
    MaxPwr <- R[Period, 1]
  }
}

for (Period in 3:48) {
  Pwr[Period] <- R[Period, 1] / MaxPwr
}

# Compute the dominant cycle using the CG of the spectrum
Spx <- 0
Sp <- 0
for (Period in 10:48) {
  if (!is.na(Pwr[Period]) && Pwr[Period] >= 0.5) {
    Spx <- Spx + Period * Pwr[Period]
    Sp <- Sp + Pwr[Period]
  }
}
if (Sp != 0) {
  DominantCycle <- Spx / Sp
}
if (DominantCycle < 10) {
  DominantCycle <- 10
}
if (DominantCycle > 48) {
  DominantCycle <- 48
}

# Ensure DominantCycle is an integer for indexing
DominantCycle <- round(DominantCycle)

# Stochastic Computation starts here
if (length(Filt) >= DominantCycle) {
  HighestC <- Filt[1:DominantCycle]
  LowestC <- Filt[1:DominantCycle]
  for (count in 1:DominantCycle) {
    if (Filt[count] > HighestC) {
      HighestC <- Filt[count]
    }
    if (Filt[count] < LowestC) {
      LowestC <- Filt[count]
    }
  }
  Stoc <- (Filt - LowestC) / (HighestC - LowestC)
  AdaptiveStochastic <- rep(NA, length(Stoc))
  if (length(Stoc) >= 3) {
    AdaptiveStochastic[3:length(Stoc)] <- c1 * (Stoc[3:length(Stoc)] + Stoc[2:(length(Stoc) - 1)]) / 2 + c2 * AdaptiveStochastic[2:(length(Stoc) - 1)] + c3 * AdaptiveStochastic[1:(length(Stoc) - 2)]
  }
  
  # Plotting the results
  plot(AdaptiveStochastic, type = "l", col = "blue", main = "Adaptive Stochastic", ylab = "Value")
  abline(h = 0.7, col = "red")
  abline(h = 0.3, col = "green")
} else {
  print("Not enough data points to compute DominantCycle")
}

