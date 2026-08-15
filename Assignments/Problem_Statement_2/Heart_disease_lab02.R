heart_disease_uci <- read.csv("C:/Users/91976/Desktop/Assignments Semester 7/Programming with R/heart_disease_uci.csv")
View(heart_disease_uci)
heart_dirty <- heart_disease_uci
# Check the actual blood pressure values
head(heart_dirty$trestbps)
# Introduce errors
heart_dirty$trestbps[c(5, 20)] <- -120
heart_dirty$trestbps[c(10, 30)] <- NA
heart_dirty$trestbps[c(15, 25)] <- 350
heart_dirty$trestbps[1:30]
clean_bp <- function(bp) {
  if (is.na(bp)) {
    return(NA)
  } else if (bp < 0) {
    return(NA)
  } else if (bp > 250) {
    return(250)
  } else {
    return(bp)
  }
}
clean_bp(-120)   # NA
clean_bp(140)    # 140
clean_bp(350)    # 250
clean_bp(NA)
heart_clean <- heart_dirty
# Apply the cleaning function
heart_clean$trestbps <- sapply(heart_clean$trestbps, clean_bp)
# View the cleaned values
heart_clean$trestbps[1:30]
safe_mean_bp <- function(x) {
  tryCatch({
    mean(x, na.rm = TRUE)
  },
  warning = function(w) {
    message("Warning: ", w$message)
    return(NA)
  },
  error = function(e) {
    message("Error: ", e$message)
    return(NA)
  })
}
safe_mean_bp(heart_clean$trestbps)
safe_ratio <- function(chol, bp) {
  tryCatch({
    if (is.na(bp) || bp == 0) {
      stop("Invalid denominator: BP is zero or NA")
    }
    chol / bp
  },
  error = function(e) {
    message("Ratio calculation failed: ", e$message)
    return(NA)
  })
}
safe_ratio(200, 120)
safe_ratio(200, 0)
safe_ratio(200, NA)
heart_clean$chol_bp_ratio <- mapply(
  safe_ratio,
  heart_clean$chol,
  heart_clean$trestbps
)
head(heart_clean$chol_bp_ratio)
loop_clean <- heart_dirty$trestbps
for (i in seq_along(loop_clean)) {
  if (is.na(loop_clean[i])) {
    next
  } else if (loop_clean[i] < 0) {
    loop_clean[i] <- NA
  } else if (loop_clean[i] > 250) {
    loop_clean[i] <- 250
  }
}
loop_clean[1:30]
vector_clean <- heart_dirty$trestbps
vector_clean[vector_clean < 0] <- NA
vector_clean[vector_clean > 250] <- 250
vector_clean[1:30]
install.packages("microbenchmark")
library(microbenchmark)
microbenchmark(
  loop = {
    temp <- heart_dirty$trestbps
    for (i in seq_along(temp)) {
      if (!is.na(temp[i])) {
        if (temp[i] < 0) temp[i] <- NA
        if (temp[i] > 250) temp[i] <- 250
      }
    }
  },
  vector = {
    temp <- heart_dirty$trestbps
    temp[temp < 0] <- NA
    temp[temp > 250] <- 250
  },
  times = 100
)
microbenchmark(
  loop = {
    temp <- heart_dirty$trestbps
    for (i in seq_along(temp)) {
      if (is.na(temp[i])) {
        next
      } else if (temp[i] < 0) {
        temp[i] <- NA
      } else if (temp[i] > 250) {
        temp[i] <- 250
      }
    }
  },
  vector = {
    temp <- heart_dirty$trestbps
    temp[temp < 0] <- NA
    temp[temp > 250] <- 250
  },
  times = 100
)
sum(is.na(vector_clean))
min(vector_clean, na.rm = TRUE)
max(vector_clean, na.rm = TRUE)
mean(vector_clean, na.rm = TRUE)
median(vector_clean, na.rm = TRUE)
any(vector_clean < 0, na.rm = TRUE)
any(vector_clean > 250, na.rm = TRUE)
heart_clean$trestbps <- vector_clean
write.csv(
  heart_clean,
  "cleaned_heart_data.csv",
  row.names = FALSE
)
View(heart_clean)
getwd()